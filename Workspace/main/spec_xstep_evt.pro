pro spec_xstep_evt, filei, xstep_count, mask=pmaski, station=channeli, output=output, $
         spectra=spectra, step_events=step_events, events=events, $
         step_toggle=step_toggle, toggle_bit=toggle_bit, toggle_station=toggle_station, $
         progress=do_progress, device=devicei, ystep=ystep, devpars=devpars, $
         group=group, xrange=xrangei, yrange=yrangei, do_tot=do_tot, $
         pileup=pileup, by_detector=by_detector, by_odd=by_odd, by_even=by_even, $
		 detector_select=by_detector_select
          
;   Read an .evt file.
;   Write total spectra out as a .spec file.
;   Using vector calls.
;
;   If /step_events, then use xstep_count as an event count to step on.
;   If /step_toggle, then step on change in toggle_bit of station toggle_station.
;   Stop at 'events' events, if set.
;
;   /ystep         Y step mode, else X step mode
;
;   Use 'output' file name or create one.
;   Return spectra pointer to 'spectra', or just write file.
;
;   channel number starts at '0'.
;   Bit numbers start at '0'; top bit in ADC value is '12'.
;
;   Mask points to a pointer array, each of which points to
;   a struct containing a pointer to a 'q' mask array.
;   Other keyword arguments are also taken from this array.
;
;   If do_tot, add the time-over-threshold data to another spectrum
;   'pileup' optional file of pileup limits (for device 16?), for
;   use when in full spectra mode.
;
;	/by_detector will use one mask region only, but produce
;				a spectrum for every detector in an array.
;		detector_select=n use region 'n'
;		/by_odd		only do /by_detector for odd rows
;		/by_even	only do /by_detector for even rows


COMPILE_OPT STRICTARR
common c_spec_last, last
common c_om_2, lmf_cal
common c_om_3b, c_per_pulse, lmf_charge
common c_om_5, lmf_live
common c_seed, seed
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

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
       warning,'Spec_xstep_EVT',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       close, 1
       return
    endif
endif

spectra_present = arg_present(spectra)
spectra = ptr_new(0)

define_devices
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

if n_elements(group) lt 1 then group=0L
if n_params() lt 2 then begin
    if n_elements(pmaski) gt 0 then begin
       xstep_count = (*(*pmaski)[0]).xstep
       if n_params() lt 1 then begin
         print,'spec_xstep_evt: missing filename.'
         return
       endif
    endif else begin
       print,'spec_xstep_evt: missing arguments.'
       return
    endelse
endif
if n_elements(filei) lt 1 then filei=''
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then begin
    if n_elements(last) ge 1 then begin
       path = extract_path(last)
    endif else begin
       path = 'g:\nmp'
    endelse

    file = file_requester(/read, path=path, $
       title='Select SPEC file to read', filter='*'+ obj->extension(), /fix_filter)
endif else begin
    path = extract_path( file[0])
    if path eq file[0] then begin
       file = file_requester(/read, path=path, file=file, $
         title='Select SPEC file to read', filter='*'+ obj->extension(), /fix_filter)
    endif
endelse
if strlen(file[0]) lt 1 then goto, bad_file
last = file[0]

if n_elements(ystep) lt 1 then ystep = 0L
if n_elements(xstep_count) lt 1 then xstep_count=0
if n_elements(step_events) lt 1 then step_events = 0
if n_elements(events) lt 1 then events=0L
if n_elements(step_toggle) lt 1 then step_toggle = 0L
if n_elements(toggle_station) lt 1 then toggle_station = 0L
if n_elements(toggle_bit) lt 1 then toggle_bit = 12
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(xrangei) lt 1 then xrangei=256
if n_elements(yrangei) lt 1 then yrangei=256
if n_elements(do_tot) lt 1 then do_tot=0
if n_elements(pileup) lt 1 then pileup=''
if n_elements(by_detector) lt 1 then by_detector=0
if n_elements(by_detector_select) lt 1 then by_detector_select=0
if n_elements(by_odd) lt 1 then by_odd=0
if n_elements(by_even) lt 1 then by_even=0
if by_odd then by_even=0
if by_detector or by_odd or by_even then begin
	warning,'spec_xstep_evt','"/by_detector, /by_odd, /by_even" keywords ignored.'
endif

if n_elements(pmaski) lt 1 then begin
    use_mask = 0
    update_charge = 1
    nq = 1
    show_back = 0
    if n_elements(output) lt 1 then begin
       output = strip_file_ext( file[0]) + 'i.spec'
    endif
    ecompress = 1L
    charge = 0.0
    if n_elements(channeli) lt 1 then channeli=-1
    channel = channeli

	if n_elements(devpars) gt 0 then obj->set_options, devpars
    do_pileup = 0
    do_throttle = 0
	if obj->pileup() then begin
	    pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
	endif
endif else begin
	pmask = pmaski
	if by_detector then begin
		if by_detector_select ge n_elements(*pmaski) then begin
	       gprint, output=cluster_debug, 'spec_xstep_evt: "by_detector_select" used, too large index.'
	       return
		endif
		pmask = ptr_new( (*pmaski)[by_detector_select])
	endif
    use_mask = 1
    update_charge = 0
    q = where( ptr_valid( *pmask ) eq 1)     ; only valid pointers
    if q[0] eq -1 then begin
       print,'spec_xstep_evt: mask used, but all pointers bad.'
       return
    endif
    nq = n_elements(q)
    pq = ptrarr(nq)
    for i=0L,nq-1 do begin
       pq[i] = (*(*pmask)[q[i]]).q     ; an array of pointers to 'q' arrays
    endfor
    p = (*pmask)[q[0]]
    if (*p).xstep_on ne 1 then begin
       print,'spec_xstep_evt: mask used, but xstep_on=0.'
       return
    endif

    remap = 0
    if n_elements(channeli) lt 1 then begin
       channel = (*p).channel
    endif else begin
       if channeli lt 0 then begin		; use channeli = -1 to select valid
         if (*p).array then begin       ; detectors in an array
          channel = *(*p).pactive
          remap = 1
         endif else begin
          channel = (*p).channel
         endelse
       endif else begin
         channel = channeli
       endelse
    endelse
    ecompress = (*p).ecompress
    nx = (*p).nx
    ny = (*p).ny
    xcompress = fix( (*p).xcompress)
    ycompress = fix( (*p).ycompress)
    events = (*p).events

    do_pileup = 0
    do_throttle = 0
	if obj->pileup() then begin
		pileup_limit = get_pileup((*p).pileup, do_pileup=do_pileup)
	endif
	if obj->throttle() then begin
		throttle_factor = get_throttle((*p).throttle, do_throttle=do_throttle)
	endif

    if remap then begin                   ; build lookup table to remap energy
       c = indgen(8192)
       c0 = fltarr(8192,geopixe_max_adcs)
       for n=0L,n_elements(channel)-1 do begin
         e = (*(*p).pcal)[n].poly[1] * c + (*(*p).pcal)[n].poly[0]
         x = (e - (*(*p).pcal)[0].poly[0]) / (*(*p).pcal)[0].poly[1]
         c0[*,channel[n]] = x
       endfor
    endif

    ystep = (*p).ystep
    xstep_count = (*p).xstep
    step_events = (*p).step_events
    step_toggle = (*p).step_toggle
    toggle_bit = (*p).toggle_bit
    toggle_station = (*p).toggle_station

    npx = long(nx)*long(ny)
    mask = bytarr(nx,ny,nq)
    for i=0L,nq-1L do begin
       mask[ *(pq[i]) + npx*i ] = 1B
    endfor

    original_xsize = (*p).original_xsize
    original_ysize = (*p).original_ysize
;   if (original_xsize ne 0) and (original_ysize ne 0) and $
;      ((original_xsize ne nx) or (original_ysize ne ny)) then begin
;     mask = congrid( mask, original_xsize, original_ysize, nq)
;     nx = original_xsize
;     ny = original_ysize
;   endif

    if (((*p).scaled_x ne 1.0) and ((*p).scaled_x gt 0.001)) or $
         (((*p).scaled_y ne 1.0) and ((*p).scaled_y gt 0.001)) then begin
       if (*p).scaled_x lt 0.001 then (*p).scaled_x=1.0
       if (*p).scaled_y lt 0.001 then (*p).scaled_y=1.0

       nx = nx/(*p).scaled_x
       ny = ny/(*p).scaled_y
       mask = congrid( mask, nx, ny, nq)
    endif
    show_back = (*p).show_back

    if n_elements(output) lt 1 then begin
       output = strip_file_ext( file) + '-q1.spec'
    endif
endelse
if (step_toggle eq 0) and (xstep_count lt 1) then begin
    print,'spec_xstep_evt: xstep_count zero.'
    return
endif

null_spectrum = define(/spectrum)

min_e = 10000
min_x = 10000
min_y = 10000
min_t = 10000
max_e = 0
max_x = 0
max_y = 0
max_t = 0
processed = 0LL
valid = 0LL
bad_xy = 0LL
bad_e = 0LL
clipped = 0LL
pileup_losses = 0LL

if use_mask then begin
	n_spectra_channels = 4096
    found = lon64arr(max([nq,geopixe_max_adcs]))
    spece = lonarr(n_spectra_channels,nq)

    xcompress = (xcompress > 1)
    ycompress = (ycompress > 1)
    xrange = nx * xcompress
    yrange = ny * ycompress
    xrange2 = fix( nx)
    yrange2 = fix( ny)
endif else begin
	n_spectra_channels = 16384					; must be this value for Maia
    found = lon64arr(geopixe_max_adcs)
    spece = lonarr(n_spectra_channels,geopixe_max_adcs)
    specx = lonarr(n_spectra_channels,geopixe_max_adcs)
    specy = lonarr(n_spectra_channels,geopixe_max_adcs)
    if do_tot then spect = lonarr(n_spectra_channels,geopixe_max_adcs)
    xrange = xrangei
    yrange = yrangei
endelse
nnpu64 = lon64arr(geopixe_max_adcs)
nn64 = lon64arr(geopixe_max_adcs)
nnpu = lonarr(geopixe_max_adcs)
nn = lonarr(geopixe_max_adcs)

sfound = lonarr(n_elements(found))
cancel = 0
etitle = ''
direction = 'X'
if ystep then direction = 'Y'

j = 0L
nj = n_elements(file)
;evt_file = strip_file_ext( file) + obj->extension()
evt_file = file
first = 1

;-----------------------------------------------------------------------------------------

loop_file:
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, next

    device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, ystep=ystep, first=first, error=err, $
         charge=charge, ecompress=ecompress, flux=flux
    if err then goto, finish
    nprogress = ((100000L / n_guide) > 1L) < 500L
    first = 0

    if j eq 0 then begin
       if do_progress then begin
         progress, tlb=progress_tlb, title='Sort XY EVT file', $           ; put up a progress bar
		 		pars=['Events','Found 1','Found 4','Blocks','Found 2','Bad XY',direction,'Found 3','']

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
		veto = 0US

;		Call read_buffer: 
;			'n' returns number of events returned (arrays e,tot,x1,y1,veto,ste).
;			'veto' =1 flags bad/rejected/pseudo events, within the 'n'.
;			'good' returns number of good (veto=0) events.
;
;		Take care with vectors that some devices do not set, such as veto, ste, tot
;		The test in read_buffer will detect if they are not set-up. But after that we need to
;		clear them for each buffer. The easiest way to ensure that is to set them to "0L" here
;		(for veto) or in 'read_buffer' (for ste, t, multiple).

       read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress,  $
          ecompress=ecompress, title=etitle, file=evt_file[j], multiple=multiple, error=err, $
          flux=flux, time=tot, station_e=ste, veto=veto, $
          total_processed=processed, processed=count1, valid=good, total_bad_xy=sbad_xy, raw_xy=1-use_mask, $
          step_count=xstep_count, step_events=step_events, step_toggle=step_toggle, toggle_bit=toggle_bit, toggle_adc=toggle_station, ystep=ystep
                    
       if err then goto, next
	   if (n eq 0) or (good eq 0) then goto, cont
       bad_xy = bad_xy + sbad_xy

		if n_elements(veto) ne n then veto = uintarr(n)
		if do_pileup then begin
			pu = uint( (tot lt pileup_limit[0,e]) or (tot gt pileup_limit[1,e]))
			qp = where( pu eq 1, nqp)
			pileup_losses = pileup_losses + nqp
		endif else pu = uintarr(n)
		if do_throttle then begin
			multiple = temporary(multiple) * throttle_factor[e]
		endif

       if do_progress then begin
         iprogress = iprogress + 1
         if iprogress ge nprogress then begin
          t = x1[0]
          if ystep then t=y1[0]
          case progress_file of
              0: begin
                 p.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
                 end
              1: begin
                 p.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
                 p.current = j
                 p.file = evt_file[j]
                 end
              2: begin
                 p.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
                 p.current = i
                 end
              3: begin
                 p.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
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

	   q = where( veto eq 0, good)
	   valid = valid + good
	   
    if good gt 0 then begin

      if use_mask then begin             ; use masks

			 if remap then begin
			 	R = randomu( seed, n)-0.5       	    					; +- 0.5 random dither
				e = uint(round(c0[temporary(e),ste]+R) > 0)  		 		; remap to cal for first detector
             endif

       sfound[*] = 0
       err = spec_accumulate3( e,x1,y1,pu,veto,n, spece,n_spectra_channels,sfound, mask,nx,ny,nq, $
                    multiple=multiple)
       found = found + sfound

       if err ne 0 then begin
         print,'spec_xstep_evt: error (',err,') return from spec_accumulate'
         goto, finish
       endif

      endif else begin                  ; total spectrum

       sfound[*] = 0
       err = hist_accumulate4( e,tot,x1,y1,ste,pu,veto,n, spece,specx,specy,n_spectra_channels,geopixe_max_adcs,sfound, $
                    nnpu,nn, multiple=multiple, tot=tot, spect=spect )
       found = found + sfound
	   nnpu64[*] = nnpu64 + nnpu
	   nn64[*] = nn64 + nn
	   nnpu[*] = 0
	   nn[*] = 0

       if err ne 0 then begin
         print,'spec_xstep_evt: error (',err,') return from hist_accumulate'
         goto, finish
       endif

       min_e = min( [min_e, min(e / ecompress)])
       min_x = min( [min_x, min(x1)])
       min_y = min( [min_y, min(y1)])
       max_e = max( [max_e, max(e / ecompress)])
       max_x = max( [max_x, max(x1)])
       max_y = max( [max_y, max(y1)])
       min_t = min( [min_t, min(tot)])
       max_t = max( [max_t, max(tot)])
      endelse
    endif
    if events gt 0 then if processed gt events then begin
       print,'spec_xstep_evt: requested event count exceeded; stop.'
       goto, finish
    endif

cont:
    i = i+1
endwhile

next:
    j = j+1
if j lt nj then goto, loop_file

finish:
    if do_throttle then print,'    Used THROTTLE file ',throttle
    if do_pileup then print,'  Used PILEUP file ',pileup
  print, ' processed = ', processed
  print, ' found(stn) = ',found
    print, ' bad XY = ',bad_xy
    print, ' pileup losses = ',pileup_losses
    print, ' total pileup fraction = ', float(pileup_losses) / processed
  if use_mask eq 0 then begin
      print, ' min e,x,y = ',min_e,min_x,min_y
      print, ' max e,x,y = ',max_e,max_x,max_y
  endif
    t = 0
    if n_elements(x1) gt 0 then begin
        t = max([x1])
       if ystep then t=max([y1])
    endif
    if do_progress then begin
       p.value = [processed,found[0],found[3],i,found[1],bad_xy,t,found[2],0]
         case progress_file of
          1: begin
              p.current = j
              end
          2: begin
              p.current = i
              end
          else:
         endcase
       progress, /update, progress_tlb, p
    endif
close, 1
on_ioerror, null

t = 'EVT sorting complete. Save Spectra: '+output
if do_progress then begin
    progress, /complete, progress_tlb, t
endif

if use_mask eq 0 then begin        ; total spectra
    q = where( found gt 0, n)        ; list of channels found
    if q[0] eq -1 then begin
       warning,'spec_xstep_evt','No data found in list-mode file.'
       goto, done
    endif
    axis = ['E','X','Y','T']
    siz = [max_e,max_x,max_y,max_t]
;    ex = fix(alog( siz>1)/alog(2) + 1.00001)
;    siz = 2L^ex < 8192
	siz = (siz > 100) < n_spectra_channels
	if n eq 0 then begin
		q = 0					; save at least one spectrum
		n = 1
	endif

    live = 1.0
    if (obj->name() eq 'OM_DAQ_DEVICE') and (suppress eq 0) then begin
	   old_charge = charge
       charge = lmf_charge
       dead = 0.0
       if (n_elements(group) ge 1) then begin
       		charge = OM_charge_select(group, charge, live=lmf_live, dead=dead)
       endif
	   if charge lt 1.e-10 then charge=old_charge
       live = 1.0-dead
    endif

; For many synchrotron formats, we get flux initially (device_specific) or from
; each pixel record. Where DT is available, this flux is DT corrected before
; this point.

	if n_elements(flux) gt 0 then begin
       tflx = total(flux)
       if tflx gt 0.001 then begin
;         if (device eq 17) or (device eq 14) or (device eq 15) or (device eq 10) or (device eq 8) then begin
;          c = charge
;			if aps_count_to_charge gt 1.e-20 then c=aps_count_to_charge*tflx
;			head = {charge:c, fluence:tflx, conversion:aps_count_to_charge, use_flux:0, error:0}
;			if suppress eq 0 then head = APS_charge_select( group, head)
;			if head.error eq 0 then begin
;				charge = head.charge
;				aps_count_to_charge = head.conversion
;			endif
;         endif
       endif
    endif

    ns = 2
    if do_tot then ns=3
    p = ptrarr((ns+1)*n)
    for i=0L,n-1 do begin
       for j=0L,ns do begin
         spec = null_spectrum
         spec.file = output
         spec.source = evt_file[0]
         spec.source2 = evt_file[nj-1]
		 spec.pileup = pileup
		 spec.throttle = ''
         spec.DevObj = clone_device_object(obj)
         tag = string( q[i] + 1 + adc_offset_device(obj) )
         spec.label = strcompress(file[0] + ' ' + tag + '/' + axis[j])
         spec.cal.order = 1
         spec.cal.poly[0] = 0.0
         spec.cal.poly[1] = 1.0
         spec.cal.units = 'channel'
         spec.ecompress = ecompress
         spec.station = q[i]+1                   ; this is the station #
         spec.comment = etitle
         spec.charge = charge
         spec.deadtime_correction = 1./live
         spec.array = 0
         spec.multiplicity = 1

         spec.ystep = ystep
         spec.xstep_on = 1
         spec.xstep = xstep_count
         spec.step_events = step_events
         spec.step_toggle = step_toggle
         spec.toggle_bit = toggle_bit
         spec.toggle_station = toggle_station
         spec.events = events                  ; terminate events, NOT total events

         spec.type = 0                        ; 0=counts, 1=conc (traverse)
         spec.channel = q[i]
         spec.show_back = show_back

         spec.size = siz[j]
         case j of
          0: spec.data = ptr_new( spece[*,q[i]] )
          1: spec.data = ptr_new( specx[*,q[i]] )
          2: spec.data = ptr_new( specy[*,q[i]] )
          3: spec.data = ptr_new( spect[*,q[i]] )
         endcase
         p[(ns+1)*i+j] = ptr_new( spec, /no_copy)
       endfor
    endfor
endif else begin              ; region energy spectra
    p = ptrarr(nq)
    centre = [0,4,2,0,4,4,0,0]        ; which is centre handle
    for i=0L,nq-1 do begin
       siz = max( where(spece[*,i] gt 0) ) + 1
;       ex = fix(alog(siz>1)/alog(2) + 1.1)
;       siz = 2^ex < 8192
	   siz = (siz > 100) < n_spectra_channels

       pm = (*pmask)[i]
       spec = null_spectrum
	   spec.file = output
       spec.source = evt_file[0]
       spec.source2 = evt_file[nj-1]
       spec.pileup = (*pm).pileup
       spec.throttle = (*pm).throttle
	   spec.DevObj = clone_device_object(obj)
       spec.label = strcompress('Region ' + str_tidy(i) + ', ' + (*pm).el_shown)
       spec.station = channel[0]+1

       pm = (*pmask)[i]
       type = (*pm).analyze_type[0]
       pmk = (*pm).pmark[0]
       spec.x = (*pmk).x[centre[type]]                       ; centre
       spec.y = (*pmk).y[centre[type]]
       shape = {type:-1, X:0.0, Y:0.0 }

       if (*pm).analyze_type[1] eq 0 then begin             ; spectrum from an include shape
         shape.type = (*pm).analyze_type[0]
         case shape.type of
          1: begin                   ; Box
              tx = abs( (*pmk).x[1] - (*pmk).x[0] )
              ty = abs( (*pmk).y[1] - (*pmk).y[0] )
              sx = float(tx) * 1000.0 * float((*pm).scanx) / float((*pm).nx)
              sy = float(ty) * 1000.0 * float((*pm).scany) / float((*pm).ny)
              sx = sqrt( sx*sx + sy*sy)
              shape.x = sx
              tx = abs( (*pmk).x[2] - (*pmk).x[1] )
              ty = abs( (*pmk).y[2] - (*pmk).y[1] )
              sx = float(tx) * 1000.0 * float((*pm).scanx) / float((*pm).nx)
              sy = float(ty) * 1000.0 * float((*pm).scany) / float((*pm).ny)
              sy = sqrt( sx*sx + sy*sy)
              shape.y = sy
              end
          2:begin                    ; Circle
              tx = abs( (*pmk).x[1] - (*pmk).x[0] )
              ty = abs( (*pmk).y[1] - (*pmk).y[0] )
              sx = float(tx) * 1000.0 * float((*pm).scanx) / float((*pm).nx)
              sy = float(ty) * 1000.0 * float((*pm).scany) / float((*pm).ny)
              sx = sqrt( sx*sx + sy*sy)
              shape.x = sx
              shape.y = sx
              end
          5: begin                   ; Ellipse
              tx = sqrt( ((*pmk).x[1]-(*pmk).x[0])*((*pmk).x[1]-(*pmk).x[0]) + ((*pmk).y[1]-(*pmk).y[0])*((*pmk).y[1]-(*pmk).y[0]) )
              ty = sqrt( ((*pmk).x[2]-(*pmk).x[3])*((*pmk).x[2]-(*pmk).x[3]) + ((*pmk).y[2]-(*pmk).y[3])*((*pmk).y[2]-(*pmk).y[3]) )
              sx = float(tx) * 1000.0 * float((*pm).scanx) / float((*pm).nx)
              sy = float(ty) * 1000.0 * float((*pm).scany) / float((*pm).ny)
              shape.x = sx
              shape.y = sy
              end
          else:
         endcase
       endif
       spec.shape = shape

       spec.cal.order = 1
       spec.cal.poly[0] = 0.0
       spec.cal.poly[1] = 1.0
       spec.cal.units = 'channel'
       if (*pm).channel eq channel[0] then begin
         spec.cal.order = 1
         spec.cal.units = 'keV'
         spec.cal.poly[0] = (*pm).cal_b
         spec.cal.poly[1] = (*pm).cal_a
       endif else begin
         if (*pm).array eq 1 then begin
          if ptr_valid( (*pm).pactive) then begin
              qt = where( channel[0] eq *(*pm).pactive)
              if qt[0] ne -1 then begin
                 spec.cal.order = 1
                 spec.cal.units = 'keV'
                 spec.cal.poly[0:1] = (*(*pm).pcal)[qt[0]].poly[0:1]
              endif
          endif
         endif
       endelse
       spec.ecompress = ecompress

       spec.charge = (*pm).charge
       spec.sample = (*pm).sample
       spec.grain = (*pm).grain
       if lenchr((*pm).comment) gt 0 then begin
         spec.comment = (*pm).comment
       endif else begin
         spec.comment = etitle
       endelse

       spec.history[0] = 'Extracted from '+direction+' STEP EVT using region ' + str_tidy(i)
       spec.n_history = 1
       if step_events then begin
         spec.history[1] = 'Stepped '+direction+' after ' + string(xstep_count) + 'events in channel ' + string(channel+1)
         spec.n_history = 2
       endif else if step_toggle then begin
         spec.history[1] = 'Stepped '+direction+' using toggle bit ' + string(toggle_bit) + ' on channel ' + string(toggle_station)
         spec.n_history = 2
       endif else begin
         spec.history[1] = 'Stepped '+direction+' after ' + string(xstep_count) + 'pulses in channel ' + string(toggle_station)
         spec.n_history = 2
       endelse

       spec.scan.x = (*pm).scanx
       spec.scan.y = (*pm).scany

       spec.matrix.file = (*pm).matrix
       spec.events = events                 ; terminate events, NOT total events

       spec.ystep = ystep
       spec.xstep_on = (*pm).xstep_on
       spec.xstep = xstep_count
       spec.step_events = step_events
       spec.step_toggle = step_toggle
       spec.toggle_bit = toggle_bit
       spec.toggle_station = toggle_station

       spec.type = 0                     ; 0=counts, 1=conc (traverse)
       spec.channel = channel[0]
       spec.detector = -1
       spec.multiplicity = n_elements(channel) > 1
       if (*pm).array eq 1 then begin
         spec.array = 1
         spec.pactive = ptr_new(channel)
         q = where( channel[0] eq *(*pm).pactive)
         if q[0] ne -1 then begin
          spec.detector = (*pm).detector
         endif
       endif else begin
         if(*pm).channel eq channel[0] then begin
          spec.detector = (*pm).detector
         endif
       endelse
       spec.xcompress = (*pm).xcompress
       spec.ycompress = (*pm).ycompress
       spec.nx = (*pm).nx
       spec.ny = (*pm).ny
       spec.show_back = show_back
       print,' Spec ',i,',  x,y=',spec.x,spec.y,' charge=',spec.charge,' multiplicity=',spec.multiplicity

       spec.size = siz
       spec.data = ptr_new( spece[0:siz-1,i] )

       spec.log = 1
       spec.showfit = 1
       spec.show = 0
       if i eq 0 then spec.show=1

       p[i] = ptr_new( spec, /no_copy)
    endfor
endelse

write_spec, p, output

pp = ptr_new( p, /no_copy)

if spectra_present eq 0 then begin
    free_spectra, pp
    if ptr_valid(spectra) then ptr_free, spectra
endif else begin
    spectra = pp
endelse

done:
	if obj_valid(obj) then obj_destroy, obj
	progress, /ending, progress_tlb
	return

bad_file:
	warning,'spec_xstep_evt','EVT file not found.'
	return
bad_obj:
	warning, 'spec_xstep_evt', 'Bad device object for: '+device
	return
end
