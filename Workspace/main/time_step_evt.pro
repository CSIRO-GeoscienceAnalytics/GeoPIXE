pro time_step_evt, file, time_count, station=station, output=output, $
         spectra=spectra, time_station=time_station, $
         progress=do_progress
;
;   Read an .evt file.
;   Write total spectra out as a .spec file.
;   Using vector calls.
;
;   Use 'output' file name or create one.
;   Return spectra pointer to 'spectra', or just write file.
;
;   Station number starts at '0'.
;   Bit numbers start at '0'; top bit in ADC value is '12'.
;
;   Mask points to a pointer array, each of which points to
;   a struct containing a pointer to a 'q' mask array.
;   Other keyword arguments are also taken from this array.
;
common c_spec_last, last

if n_params() lt 2 then begin
    if n_elements(pmask) gt 0 then begin
       time_count = (*(*pmask)[0]).xstep
       if n_params() lt 1 then begin
         print,'time_step_evt: missing filename.'
         return
       endif
    endif else begin
       print,'time_step_evt: missing arguments.'
       return
    endelse
endif
if strlen(file) lt 1 then begin
    if n_elements(last) ge 1 then begin
       path = extract_path(last)
    endif else begin
       path = 'g:\nmp'
    endelse

    file = file_requester(/read, path=path, $
       title='Select EVT file to read', filter='*.evt', $
       /fix_filter)
endif else begin
    path = extract_path( file)
    if path eq file then begin
       file = file_requester(/read, path=path, file=file, $
         title='Select EVT file to read', filter='*.evt', $
         /fix_filter)
    endif
endelse
if strlen(file) lt 1 then begin
    print,'time_step_evt: no file name.'
    return
endif
last = file

spectra = 0
if n_elements(station) lt 1 then station=0
if n_elements(time_count) lt 1 then time_count=1
if n_elements(time_station) lt 1 then time_station = 2
if n_elements(do_progress) lt 1 then do_progress=0
xcompress = 1

if (time_count lt 1) then begin
    print,'time_step_evt: time_count zero.'
    return
endif
ste_mask = 'C000'xU
stx_mask = '3000'xU
sty_mask = '3000'xU

mdx_mask = 'C000'xU
mdy_mask = 'C000'xU

e_mask = '1FFF'xU
x_mask = '0FFF'xU
y_mask = '0FFF'xU

ste_offset = -14
stx_offset = -12
sty_offset = -12

mdx_offset = -14
mdy_offset = -14
correct_mdx = '8000'xU
correct_mdy = '4000'xU

null_spectrum = define(/spectrum)

on_ioerror, finish
close,1
s = strip_file_ext( file) + '.evt'
n = 1024
openr, 1, s, /binary, bufsiz=n*15

cancel = 0
if do_progress then begin
    progress, tlb=progress_tlb, title='Sort X step EVT file', $             ; put up a progress bar
			pars=['Events','Found 1','Found 4','Blocks','Found 2','Bad X','Size','Found 3','Bad Y']

    p = { unit:1, value:[0L,0L,0L,0L,0l,0l,n,0L,0L]}
    iprogress = 0L
    nprogress = 100000L
endif

event_array = replicate( { e:0U, x:0U, y:0U}, n)
buffer = assoc( 1, event_array)

bad_x = 0L
bad_y = 0L
bad_e = 0L
min_e = 100000L
min_x = 100000L
min_y = 100000L
max_e = 0L
max_x = 0L
max_y = 0L
i = 0L
processed = 0L
xcount = 0L
xpos = 0
found = lonarr(4)

n_steps_max = 20000
spece = lonarr(n_steps_max,4)

while ~ EOF(1) do begin
    ev = buffer[i]
    if do_progress then begin
       iprogress = iprogress + n
       if iprogress ge nprogress then begin
         p.value = [processed,found[0],found[3],i,found[1],bad_x,n,found[2],bad_y]
         progress, /update, progress_tlb, p, cancel=cancel, skip=skip
         if skip then goto, finish
         if cancel then begin
          close, 1
          return
         endif
         iprogress = 0L
       endif
    endif
    i = i+1

;   The data uses the bottom bits for ADC data (13 bits e, 12 bits
;   for x,y), and the top bits to tag the ADC station and to identify X,Y.
;   Various data integrity checks are made (the te,tx,ty vectors), and the
;   ADC data is accessed via bit masks.

    en = ev.e
    xn = ev.x
    yn = ev.y
    ste = ishft( en AND ste_mask, ste_offset)
    stx = ishft( xn AND stx_mask, stx_offset)
    sty = ishft( yn AND sty_mask, sty_offset)

    e = en AND e_mask
    y = yn AND y_mask

;   Find list of events with correct mdx, mdy bits,
;   and those with same station for e,x,y.

    te = (en eq '2020'x)
    tx = ((xn AND mdx_mask) ne correct_mdx) or (stx ne ste)
    ty = ((yn AND mdy_mask) ne correct_mdy) or (sty ne ste)
    bad = tx or ty
    q = where( (tx eq 1) and (te eq 0), count)
    bad_x = bad_x + count
    q = where( (ty eq 1) and (te eq 0), count)
    bad_y = bad_y + count

;   Find those events which are not back-fill values ('2020'x)

    q = where( te eq 0, count)
    processed = processed + count

;   Find any station 3 events, and see if X needs advancing

    q = where( (ste eq time_station) and (te eq 0) and (bad eq 0), count3)
    x = findgen(n_elements(q)) / float(time_count)

    q = where( (bad eq 0) and (te eq 0), count)

    if count gt 0 then begin

;     Collect sum time signal for 'time_station'
;     and total counts in time interval for others

       xj = uint(xpos + congrid( x, count))
       xpos = xpos + fix(x[count3-1])
       if xpos ge n_steps_max then goto, finish

       stn = ste[q]
       ej = e[q]
       yj = y[q]

       err = time_accumulate(ej,xj,stn,count, time_station, $
              spece, n_steps_max, found, 4)
       if err then goto, finish

       min_e = min( [min_e, ej])
       min_x = min( [min_x, xj])
       min_y = min( [min_y, yj])
       max_e = max( [max_e, ej])
       max_x = max( [max_x, xj])
       max_y = max( [max_y, yj])
    endif

;   Backfill the event 'e' vector with a unique value, to help
;   identify the partial last block read.
;   THIS ONLY SEEMS TO WORK ON THE MAC. PC FILLS WITH ZERO.

    ev[*].e = '2020'x
endwhile

finish:
  print, ' processed = ', processed
  print, ' found(stn) = ',found
  print, ' bad X = ',bad_x
  print, ' bad Y = ',bad_y
  print, ' min e,x,y = ',min_e,min_x,min_y
  print, ' max e,x,y = ',max_e,max_x,max_y
    if do_progress then begin
       p.value = [processed,found[0],found[3],i,found[1],bad_x,n,found[2],bad_y]
       progress, /update, progress_tlb, p
    endif
close, 1
on_ioerror, null

if n_elements(output) lt 1 then begin
    output = strip_file_ext( file) + '-time.spec'
endif
t = 'EVT sorting complete. Save Time Spectra: '+output
if do_progress then begin
    progress, /complete, progress_tlb, t
endif

q = where( found gt 0)       ; list of stations found
if q[0] eq -1 then begin
    print,'time_step_evt: no data found.'
    return
endif
siz = xpos < n_steps_max
n = n_elements(q)
p = ptrarr(n)
for i=0L,n-1 do begin
       spec = null_spectrum
       spec.file = output
       spec.source = file
       spec.label = strcompress(file + ' ' + string(q[i]) )
       spec.cal.order = 1
       spec.cal.poly[0] = 0.0
       spec.cal.poly[1] = float(time_count)
       spec.cal.units = 'Time Pulses'
       spec.station = q[i]             ; this is the station #

       spec.size = siz
       spec.data = ptr_new( spece[0:siz-1,q[i]] )
       p[i] = ptr_new( spec, /no_copy)
endfor
if n_elements(output) lt 1 then begin
    output = strip_file_ext( file) + '-time.spec'
endif

write_spec, p, output

pp = ptr_new( p, /no_copy)

if arg_present(spectra) eq 0 then begin
    free_spectra, pp
endif else begin
    spectra = pp
endelse

done:
if do_progress then progress, /ending, progress_tlb
return
end
