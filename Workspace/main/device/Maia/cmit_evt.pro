pro cmit_evt, filei, xrange=xrange, yrange=yrange, $
          charge=charge, cal_a=cal_a, cal_b=cal_b, xcompress=xcompress, ycompress=ycompress, $
          channel=channel, output=outputi, events=events, detector=detector, $
          scanx=scanx, scany=scany, sample=sample, grain=grain, comment=comment, $
          progress=do_progress, device=devicei, group=group, throttle = throttlef
;
;   Convert an .evt file to .xvt.
;
;   'throttlef' - use the throttle file to throttle/reduce the output file count-rates
;
;   Sort events from station 'channel', for 'detector' type of data (0=PIXE, 1=PIGE, ...).
;
;   If 'events' set, stop at this number of events.
;
;   'xrange', 'yrange' are the X,Y ranges in the original data.
;   Use 'xcompress', 'ycompress' to compress these by an integral factor.
;
;   'cal_a', 'cal_b', are the energy calibration (mandatory).
;   All units are in 'keV'.
;   If these are vectors, then they will be indexed with the 'ste' channel returned.
;
;   Use filename 'output', if specified.
;
;   'scanx', 'scany' are optional scan sizes (microns).
;   'sample', 'grain', 'comment' are optional strings.
;
;   'Channel' gives the station/ADC numbers, which start at '0'.
;   This can be a vector of channels to sort, or -1 meaning all.
;
; Multiple detectors:
;   This is supported by passing a vector of channels to all detectors in 'channel',
;   and passing 'cal_a' and 'cal_b' as vectors over ALL channels, not just those in 'channel'.
;
;   Bit numbers start at '0'; top bit in ADC value is '12'.

COMPILE_OPT STRICTARR
common c_evt_last, last
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_geopixe_adcs, geopixe_max_adcs
common c_seed, seed
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8
if n_elements(seed) lt 1 then seed=1L

maia = 0              ; output in new Maia 32 format

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'cmit_evt',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

define_devices
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

x1 = 0

file = strtrim(filei,2)
if strlen(file[0]) lt 1 then begin
    if n_elements(last) ge 1 then begin
       path = extract_path(last)
    endif else begin
       path = 'c:\nmp'
    endelse

    file = file_requester(/read, path=path, $
       title='Select EVT file to convert', filter='*'+obj->extension(), $
       /fix_filter)
endif else begin
    path = extract_path( file[0])
    if path eq file[0] then begin
       file = file_requester(/read, path=path, file=file[0], $
         title='Select EVT file to convert', filter='*'+obj->extension(), $
         /fix_filter)
    endif
endelse
if strlen(file[0]) lt 1 then goto, bad_file
last = file[0]

if n_elements(channel) lt 1 then channel = 0L
if n_elements(xrange) lt 1 then xrange = 256
if n_elements(yrange) lt 1 then yrange = 256
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(cal_a) lt 1 then cal_a = 1.0
if n_elements(cal_b) lt 1 then cal_b = 0.0
if n_elements(xcompress) lt 1 then xcompress = 1
if n_elements(ycompress) lt 1 then ycompress = 1
if n_elements(events) lt 1 then events=0L
if n_elements(scanx) lt 1 then scanx = 0.0
if n_elements(scany) lt 1 then scany = 0.0
if n_elements(sample) lt 1 then sample = '?'
if n_elements(grain) lt 1 then grain = '?'
if n_elements(comment) lt 1 then comment = '?'
if n_elements(detector) lt 1 then detector=0L
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(do_progress) lt 1 then do_progress=0

output = strip_file_ext(outputi) + '.xvt'

charge = float(charge)

xcompress = (xcompress > 1)
ycompress = (ycompress > 1)
xrange2 = fix( xrange / xcompress)
yrange2 = fix( yrange / ycompress)

bad_xy = 0L
clipped = 0L
found = lonarr(geopixe_max_adcs)
processed = 0L
valid = 0L
cancel = 0
etitle = ''

throttle = get_throttle(throttlef, do_throttle=do_throttle)

print,' Process ADC(s) ',channel+1

n_det = max(channel+1)
if n_det lt 1 then n_det=geopixe_max_adcs
if channel[0] eq -1 then channel=indgen(n_det)
if (n_det gt 1) and (n_det gt n_elements(cal_a)) and (n_elements(cal_a) ne 1) then goto, bad_channels

if maia eq 0 then begin
    file2 = strip_file_ext(file[0]) + '.xvt'
    close, 2
    on_ioerror, bad_file2
    openw, 2, file2
endif
version = -2L
if maia eq 0 then writeu, 2, version         ; version

hcal = replicate( {b:0.0, a:0.0, q:0.0}, n_det)
if n_elements(cal_a) gt 1 then begin
    hcal[*].b = cal_b[0:n_det-1]
    hcal[*].a = cal_a[0:n_det-1]
endif else begin
    hcal[*].b = cal_b
    hcal[*].a = cal_a
endelse
flux = fltarr(xrange,yrange)
flux[*,*] = charge/(float(xrange)*float(yrange))

head = { n_x:     long(xrange), $          ; LONG       X pixels
       n_y:   long(yrange), $          ; LONG     Y pixels
       n_energy: 4096L, $              ; LONG     Energy channels
       n_mca:       long(n_det), $          ; LONG   Number of active detectors
       cal:   hcal, $               ; FLOAT       Energy Cal (n_mca * [b,a,q])
       xsize:       float(scanx), $          ; FLOAT       X size (micron)
       ysize:       float(scany), $          ; FLOAT       Y size (micron)
       flux:     flux, $                 ; FLOAT     flux/charge per pixel (n_x * n_y)
       fluence:  float(charge) }          ; FLOAT     total charge

head_length = 4*4L + n_det*3*4L + 2*4L + xrange*yrange*4L + 4L  ; bytes

;   Don't do byte swap - assume PC to PC

on_ioerror, bad_write
if maia eq 0 then writeu, 2, head_length       ; length in bytes
if maia eq 0 then writeu, 2, head

resolution = 180.           		         ; resolution (eV)
tau = 0.70                      			 ; shaping time (uS)
threshold = 40.                 		     ; threshold (channel)
delta_max = tos(threshold,tau,2000)          ; maximum of delta distribution

j = 0L
nj = n_elements(file)
evt_file = file
first = 1
x0 = -1
y0 = -1

loop_file:
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, next

    if maia then begin
       file2 = strip_file_ext(file[0])
       if do_throttle then file2=file2 + '-throttle'
       file2 = file2 + '.' + str_tidy(j)
       close, 2
       on_ioerror, bad_file2
       openw, 2, file2
       on_ioerror, next
    endif

    device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, first=first, error=err
    if err then goto, finish
    nprogress = ((100000L / n_guide) > 1L) < 500L
    first = 0

    n_out_max = 10*n_buffer
    if maia then begin
       out = bytarr(n_out_max)             ; byte stream format
    endif else begin
       out = uintarr(5, n_out_max)          ; extra word for time
    endelse

    if j eq 0 then begin
       if do_progress then begin
         progress, tlb=progress_tlb, title='Convert XY EVT file', $   ; put up a progress bar
          		pars=['Events','Valid','Blocks','Bad XY','Size','Clipped']

         iprogress = 0L
		case progress_file of
			0: begin
				p = { unit:1, value:[0LL,0LL,0LL,0LL,n_guide,0LL]}
				end
			1: begin
				p = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0LL,1000L,0LL]}
				end
			2: begin
				p = { unit:0, current:0L, size:xrange*yrange, file:evt_file[0], value:[0LL,0LL,0LL,0LL,1000L,0LL]}
				end
			3: begin
				p = { unit:0, current:0L, size:progress_size, file:evt_file[0], value:[0LL,0LL,0LL,0LL,1000L,0LL]}
				end
			else:
		endcase
       endif
    endif

    i = 0L
    while ~ EOF(1) do begin

       read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress,  $
          total_bad_xy=bad_xy, total_processed=processed, time=time, $
          processed=count1, valid=good, multiple=multiple, $
          station_e=ste, error=err
       if err then goto, next
       if good eq 0 then goto, cont

;   Manufacture 'time over threshold' data (ns) ...
;
;      Time = uint(300. * tos_real( e, threshold=threshold, tau=tau, resolution=resolution, cal_a=hcal[ste].a))

       clipped = clipped + (n-good)
       valid = valid + good
       if do_progress then begin
         iprogress = iprogress + 1
         if iprogress ge nprogress then begin
		case progress_file of
			0: begin
				p.value = [processed,valid,i,bad_xy,n,clipped]
				r = 0.5
				end
			1: begin
				p.value = [processed,valid,i,bad_xy,n,clipped]
				p.current = j
				p.file = evt_file[j]
				r = float(j)/nj
				end
			2: begin
				p.value = [processed,valid,i,bad_xy,n,clipped]
				p.current = i
				r = float(i)/(xrange*yrange)
				end
			3: begin
				p.value = [processed,valid,i,bad_xy,n,clipped]
				p.current = i
				r = float(i)/progress_size
				end
			else:
		endcase
          progress, /update, progress_tlb, p, cancel=cancel, skip=skip
          if skip then goto, finish
          if cancel then begin
              close, 1
              close, 2
              return
          endif
          iprogress = 0L
         endif
       endif

       if do_throttle then begin
         q = throttle_q( e, -1L, good, throttle=throttle, tbuff=tbuff)
         if q[0] ne -1 then begin
             e = e[q]
             x1 = x1[q]
             y1 = y1[q]
             time = time[q]
          ste = ste[q]
          good = n_elements(q)
         endif
       endif

       n_out = 0L
       err = cmit_event_build( ste, e, x1,y1, multiple,good, out,n_out_max,n_out, $
                 x0,y0, time=time, maia=maia)
       if err then begin
         warning,'cmit_evt','Bad return from Build.'
         goto, finish
       endif

       if n_out lt 1 then goto, next
       if n_out ge n_out_max then warning,'cmit_evt','OUT buffer full.'

;     Build pile-up lines for one detector at same XY coordinate ...

       do_pu = 0
       q = where( (ste lt 3) and (ste eq ste[0]) and (x1 eq x1[0]) and (y1 eq y1[0]), cp)
       if do_pu and (cp ge 5) then begin
         x1p = x1[q]
         y1p = y1[q]
         step = ste[q]
         ep = e[q]
         multiplep = multiple[q]

         np = cp/5
         q1 = long(cp*randomu(seed, np))       ; select 20% at random for pile-up
         q2 = long(cp*randomu(seed, np))

         x1p = x1p[q1]
         y1p = y1p[q1]
         step = step[q1]
         multiplep = multiplep[q1] * multiplep[q2]

;      Want an exponential delta distribution, with a time-constant of
;      inverse count rate, assume to be 10 KHz --> 100 us, but only
;      want those that produce a true pile-up, over only ~2-3 us. Hence,
;      can approximate delta distribution to a linear slope, and
;`       truncate it at a few us. In fact, for a 100 us time constant >>
;      tau we can assume a uniform delta distribution with little bias.

         delta = delta_max * randomu(seed,np)

         Timep = uint(300.* tos_pu( ep[q1], ep[q2], threshold, tau, $
                        delta, /short, twin=twin, peak=peak))

;      Want NOT sum energy, but maximum of amp_fun for first pulse.

         eps = peak                    ; ~sum energy

         q = where( (eps lt 4095) and (twin eq 0), goodp)
         if goodp gt 0 then begin
          x1p = x1p[q]
          y1p = y1p[q]
          eps = eps[q]
          step = step[q]
          multiplep = multiplep[q]

          err = cmit_event_build( step, eps,  x1p,y1p, multiplep,goodp, out,n_out_max,n_out, $
                 time=Timep, maia=maia)
          if err then begin
              warning,'CMIT_xstep_evt','Bad return from Build.'
              goto, finish
          endif

          if n_out lt 1 then goto, next
          if n_out ge n_out_max then warning,'CMIT_xstep_evt','OUT buffer full.'
         endif
       endif

       on_ioerror, bad_write
       if maia then begin
         writeu, 2, out[0:n_out-1]
       endif else begin
         writeu, 2, out[*,0:n_out-1]
       endelse

       if events gt 0 then if processed gt events then begin
         print,'cmit_evt: requested event count exceeded; stop.'
         goto, finish
       endif

cont:
       i = i+1
    endwhile

next:
    j = j+1
    if j lt nj then goto, loop_file

finish:
    print, ' processed = ', processed
    print, ' valid events = ', valid
    print, ' bad event triplets = ', bad_xy
    print, ' clipped events = ', clipped
    if n_elements(p) gt 0 then begin
       if do_progress then begin
         p.value = [processed,valid,i,bad_xy,n,clipped]
         progress, /update, progress_tlb, p
       endif
    endif
close, 1
close, 2
on_ioerror, null
t = 'EVT sorting complete.'
if do_progress then begin
    progress, /complete, progress_tlb, t
endif

if do_progress then progress, /ending, progress_tlb
if obj_valid(obj) then obj_destroy, obj
return

bad_channels:
    warning, 'cmit_evt', 'Channel array does not match Cal_a.'
    close, 1
    close, 2
    return
bad_file:
    warning, 'cmit_evt', 'EVT File not found.'
    return
bad_file2:
    warning, 'cmit_evt', 'XVT File not openned.'
    close, 1
    return
bad_obj:
	warning, 'cmit_evt', 'Bad device object for: '+device
    close, 1
    return
bad_write:
    close, 1
    close, 2
    warning, 'cmit_evt', 'bad write.'
    return
end
