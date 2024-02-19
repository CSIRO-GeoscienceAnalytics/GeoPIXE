pro time_amp_evt, file, path=path, mask=qmask, station=channeli, $
          progress=do_progress, events=events, device=obji, $
          group=group, xrange=xrangei, yrange=yrangei, time_amp=time_amp
;
;   Read an .evt 'file'.
;   Write total amplitude-time arrays out as a .et file.
;
;   Optional 'qmask' points to a 'q' mask array.
;
;   Return time_amp pointer to 'time_amp'
;
;   Stop at 'events' events, if set.

COMPILE_OPT STRICTARR
common c_spec_last, last

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'time_amp_EVT',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       close, 1
       return
    endif
endif

time_amp_present = arg_present(time_amp)
define_devices
if n_elements(obji) lt 1 then obji=obj_new('MAIA_DEVICE')
obj = obji
if obj_valid(obj) eq 0 then goto, bad_obj

if n_elements(group) lt 1 then group=0L
j = 0
evt_file = file

if strlen(file[0]) lt 1 then goto, bad_file
last = file[0]
path = extract_path( file[0])

if n_elements(events) lt 1 then events=100000LL
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(xrangei) lt 1 then xrangei=4096
if n_elements(yrangei) lt 1 then yrangei=4096
if n_elements(channeli) lt 1 then begin
    channel = -1
endif else begin
    channel = channeli
endelse

if n_elements(qmask) lt 1 then begin
    use_q = 0
    ecompress = 1L
    xcompress = 1L
    ycompress = 1L
    nx = xrangei
    ny = yrangei
endif else begin
    use_q = 1
    if ptr_valid(qmask.q) eq 0 then begin
       print,'time_amp_evt: q used, but q pointer bad.'
       use_q = 0
    endif
    q = *qmask.q

    ecompress = 1L
    xcompress = 1L
    ycompress = 1L
;   ecompress = (*p).ecompress
;   xcompress = fix( (*p).xcompress)
;   ycompress = fix( (*p).ycompress)
    nx = qmask.nx
    ny = qmask.ny

    npx = long(nx)*long(ny)
    mask = bytarr(nx,ny)
    mask[q] = 1
endelse

bad_xy = 0LL
bad_e = 0LL
processed = 0LL
found = 0LL
e2 = 0US
t2 = 0US
x2 = 0US
y2 = 0US
ste2 = 0US
multiple2 = 0L
cancel = 0
etitle = ''
tot = 0L

j = 0L
nj = n_elements(file)
first = 1

;-----------------------------------------------------------------------------------------

loop_file:
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, next

    device_specific, obj,1, nx,ny, n_guide,progress_file,progress_size=progress_size, first=first, error=err, $
         ecompress=ecompress		;, group=group
    if err then goto, finish
    nprogress = ((100000L / n_guide) > 1L) < 500L
    first = 0

    if j eq 0 then begin
       if do_progress then begin
         progress, tlb=progress_tlb, title='Sort XY EVT file', $           ; put up a progress bar
		 		pars=['Events','Found','','Blocks','','Bad XY','Size','','']

         iprogress = 0L
		case progress_file of
			0: begin
				p = { unit:1, value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			1: begin
				p = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			2: begin
				p = { unit:0, current:0L, size:long(xrange)*yrange, file:evt_file[0], value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			3: begin
				p = { unit:0, current:0L, size:progress_size, file:evt_file[0], value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			else:
		endcase
       endif
    endif

    i = 0L
    n = 0L
    while ~ EOF(1) do begin
       sbad_xy = 0L

;     Note that in /raw_xy mode, ecompress is ignored. For Lund data, apply below ...

       read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress,  $
          ecompress=ecompress, title=etitle, multiple=multiple, $
          total_bad_xy=sbad_xy, total_processed=processed, $
          processed=count1, valid=good, error=err, raw_xy=1-use_q, $
          station_e=ste, file=evt_file[j], time=tot
       if err then goto, next
       bad_xy = bad_xy + sbad_xy
       if good eq 0 then goto, cont

       if do_progress then begin
         iprogress = iprogress + 1
         if iprogress ge nprogress then begin
		case progress_file of
			0: begin
				p.value = [processed,found,0LL,i,0LL,bad_xy,n,0LL]
				end
			1: begin
				p.value = [processed,found,0LL,i,0LL,bad_xy,n,0LL]
				p.current = j
				p.file = evt_file[j]
				end
			2: begin
				p.value = [processed,found,0LL,i,0LL,bad_xy,n,0LL]
				p.current = i
				end
			3: begin
				p.value = [processed,found,0LL,i,0LL,bad_xy,n,0LL]
				p.current = i
				end
			else:
		endcase
          progress, /update, progress_tlb, p, cancel=cancel, skip=skip
          if skip then goto, finish
          if cancel then begin
              close, 1
              return
          endif
          iprogress = 0L
         endif
       endif

       if good gt 0 then begin
         if multiple[0] eq -1 then multiple = replicate(1L, n)

;         q = where( ((x1 ge 0) and (x1 lt nx)) and $
;                 ((y1 ge 0) and (y1 lt ny)), good)
		  q = lindgen(good)
         if good eq 0 then goto, cont
         e = e[q]
         tot = tot[q]
         x1 = x1[q]
         y1 = y1[q]
         ste = ste[q]
         multiple = multiple[q]

         if use_q then begin           ; use masks

         q = where( mask[x1,y1] eq 1, good)
         if good eq 0 then goto, cont

         e2 = [e2,e[q]]
         t2 = [t2,tot[q]]
         x2 = [x2,x1[q]]
         y2 = [y2,y1[q]]
         ste2 = [ste2,ste[q]]
         multiple2 = [multiple2,multiple[q]]

         endif else begin                   ; all E-T

         e2 = [e2, e]
         t2 = [t2, tot]
         ste2 = [ste2, ste]
         x2 = [x2, x1]
         y2 = [y2, y1]
         multiple2 = [multiple2,multiple]

         endelse
         found = found + long64(total(multiple))
       endif

       if events gt 0 then if found ge events then begin
         print,'time_amp_evt: requested event count exceeded; stop.'
         goto, finish
       endif

cont:
       i = i+1
    endwhile

next:
    j = j+1
    if j lt nj then goto, loop_file

;-----------------------------------------------------------------------------------------

finish:
    print, ' processed = ', processed
    print, ' valid = ',found
    print, ' bad XY = ',bad_xy
    if do_progress and (n_elements(p) gt 0) then begin
       p.value = [processed,found,0LL,i,0LL,bad_xy,n,0LL,0]
       if progress_file then p.current=j
       progress, /update, progress_tlb, p
    endif
close, 1
on_ioerror, null

t = 'EVT sorting complete. Save time_amp ...'
if do_progress then begin
    progress, /complete, progress_tlb, t
endif

if n_elements(e2) gt 1 then begin
    e2 = e2[1:*]
    t2 = t2[1:*]
    x2 = x2[1:*]
    y2 = y2[1:*]
    ste2 = ste2[1:*]
    multiple2 = multiple2[1:*]
endif

;even = 1
;odd = 0
;if even then begin
;    q = where( 2*(y2/2) eq y2)
;    if q[0] ne -1 then begin
;       e2 = e2[q]
;       t2 = t2[q]
;       x2 = x2[q]
;       y2 = y2[q]
;       ste2 = ste2[q]
;       multiple2 = multiple2[q]
;    endif
;endif

    time_amp = ptr_new( {e:e2, t:t2, ste:ste2, multiple:multiple2, x:x2, y:y2 } )

if time_amp_present eq 0 then begin
    if ptr_valid(time_amp) then ptr_free, time_amp
endif

done:
    if do_progress then progress, /ending, progress_tlb
    return

bad_file:
    warning,'time_amp_evt', 'EVT file "' + evt_file[j] + '", not found.'
    return
bad_obj:
	warning, 'time_amp_evt', 'Bad device object for: '+device
    return
end
