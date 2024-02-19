pro da_xstep_evt2, filei, matrix=matrix_file, xstep_count, xrange=xrange, yrange=yrange, charge=charge, $
          cal_a=cal_a, cal_b=cal_b, xcompress=xcompress, ycompress=ycompress, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          throttle=throttle, pileup=pileup, linearize=linearize, suppress=suppress, $
          progress=do_progress, scanx=scanx, scany=scany, group=group, device=devicei, $
          
          sample=sample, grain=grain, comment=comment, devpars=devpars, ic=flux_ic, $
          pv_list=pv_list, flatten=flatten, $
          
          images=images, xorigin=xorigin, yorigin=yorigin, $
          xoffset=xoffset, yoffset=yoffset, x_sub_range=x_sub_range, y_sub_range=y_sub_range, $

		  step_events=step_events, ystep=ystep, $
		  step_toggle=step_toggle, toggle_bit=toggle_bit, step_station=step_station

;   Read an .evt file.
;   Analyze using the Dynamic Matrix 'matrix'.
;   Sort events from station 'channel', for 'detector' type of data (0=PIXE, 1=PIGE, ...).
;
;   Modes:
;     /step_toggle use changes in 'toggle_bit' on 'step_station' to
;               indicate the advance of the X pixel.
;
;     /step_events use 'xstep_count' events on station 'channel' to
;               advance to X pixel.
;
;     else        use 'xstep_count' events on station 'step_station' to
;               advance to X pixel.
;
;     /ystep      Y step mode, else X step mode
;
;   Bit numbers start at '0'; top bit in ADC value is '12'.
;
;   If 'events' set, stop at this number of events.
;
;   file		list-mode file(s) to process.
;   matrix_file	load Dynamic Analysis Matrix from file 'matrix_file'
;   detector	type of data (0=PIXE, 1=PIGE, ...).
;   device		list-mode device object name
;   devpars		device dependent options parameter struct
;   charge		charge/flux for whole image
;   xrange		the full X ranges in the original data
;   yrange		full Y range
;   xcompress	compress these by an integral factor.
;   ycompress
;   channel		list of active detectors.
;
;   cal_a,cal_b	 the energy calibration (mandatory).
;   			These must be in 'keV' to match the DA matrix file.
;   			If these are vectors, then they will be indexed with the 'ste' channel returned.
;
;   output		Output file, Write elemental images out as a .dai DA image file.
;   images		if this arg is present, return ptr to images here
;
;   group		group_leader for progress
;   /do_progress pop-up a progress bar
;   /suppress	suppress pop-ups when in Batch mode for subsequent image sorts
;
;   scanx,scany  are optional scan sizes (microns).
;   xorigin, yorigin are optional scan origin (mm)
;   
;   xoffset, yoffset	pixel offset for origin of image to sort (not compressed)
;   x_sub_range, y_sub_range  pixel range for a sub-region scan (not compressed)
;   
;   sample,grain,comment are optional strings.
;   throttle 	gives name of throttle factors file.
;   pileup 		give name of pileup Time-over-threshold limits file.
;	linearize 	give the name of a linearization function file.
;   events		If set, stop at this number of events.
;
;   Channel 	gives the station/ADC numbers, which start at '0'.
;   			This can be a vector of channels to sort, or -1 meaning all.
;
; Multiple detectors:
;   This is supported by passing a vector of channels to all detectors in 'channel',
;   and passing 'cal_a' and 'cal_b' as vectors over ALL channels, not just those in 'channel'.

common c_evt_last, last
common c_om_3b, c_per_pulse, lmf_charge
common c_om_4, lmf_size
common c_om_5, lmf_live
common aps_4, aps_count_to_charge
common c_geopixe_adcs, geopixe_max_adcs
common c_null_image_1, max_image_cal
common c_seed, seed
common c_debug_warnings, enable_warning_popup
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8
if n_elements(seed) lt 1 then seed=1L
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
       warning,'DA_xstep_EVT',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

images = 0L
define_devices
if n_elements(devicei) lt 1 then devicei='MPSYS_DEVICE'
device = devicei
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj
x1 = 0

if n_elements(step_toggle) lt 1 then step_toggle = 0L
if ((step_toggle eq 1) and (n_params() lt 2)) or $
       ((step_toggle eq 0) and (n_params() lt 3)) then begin
    print,'da_xstep_evt: missing arguments'
    return
endif
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then begin
    if n_elements(last) ge 1 then begin
       path = extract_path(last)
    endif else begin
       path = 'g:\nmp'
    endelse

    file = file_requester(/read, path=path, $
       title='Select EVT file to sort', filter='*'+ obj->exctension(), $
       /fix_filter)
endif else begin
    path = extract_path( file[0])
    if path eq file[0] then begin
       file = file_requester(/read, path=path, file=file[0], $
         title='Select EVT file to sort', filter='*'+ obj->exctension(), $
         /fix_filter)
    endif
endelse
if strlen(file[0]) lt 1 then goto, bad_file
last = file[0]

if n_elements(channeli) lt 1 then channeli = 0L
if n_elements(ystep) lt 1 then ystep = 0L
if n_elements(step_events) lt 1 then step_events = 0L
if step_toggle eq 1 then begin
    if n_elements(step_station) lt 1 then step_station = 0L
endif else begin
    if step_events then begin
       if n_elements(step_station) lt 1 then step_station = channeli[0]
    endif else begin
       if n_elements(step_station) lt 1 then step_station = 2L     ; old data default
    endelse
endelse
if n_elements(xrange) lt 1 then xrange = 256
if n_elements(yrange) lt 1 then yrange = 256
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(xcompress) lt 1 then xcompress = 1
if n_elements(ycompress) lt 1 then ycompress = 1
if n_elements(events) lt 1 then events=0L
if n_elements(toggle_bit) lt 1 then toggle_bit = 12
if n_elements(scanx) lt 1 then scanx = 0.0
if n_elements(scany) lt 1 then scany = 0.0
if n_elements(xorigin) lt 1 then xorigin = 0.0
if n_elements(yorigin) lt 1 then yorigin = 0.0
if n_elements(xoffset) eq 0 then xoffset = 0L
if n_elements(yoffset) eq 0 then yoffset = 0L
if n_elements(x_sub_range) eq 0 then x_sub_range = 0L
if n_elements(y_sub_range) eq 0 then y_sub_range = 0L
if n_elements(sample) lt 1 then sample = '?'
if n_elements(grain) lt 1 then grain = '?'
if n_elements(comment) lt 1 then comment = '?'
if n_elements(throttle) lt 1 then throttle = ''
if n_elements(pileup) lt 1 then pileup = ''
if n_elements(detector) lt 1 then detector=0L
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(suppress) lt 1 then suppress=0
if x_sub_range eq 0 then x_sub_range = xrange
if y_sub_range eq 0 then y_sub_range = yrange

output = strip_file_ext(outputi) + '.dai'

channel = channeli									; this is new

if (n_elements(matrix_file) lt 1) then goto, bad_matrix
mpda = extract_extension( matrix_file) eq 'mpdam'

matrix = read_da( matrix_file, e_beam=0.0, phases=phase_dai, pcorr=pcorr, error=err)
if err then goto, bad_matrix

if mpda then begin
	da_matrix = matrix.matrix
	n_comp = n_elements( da_matrix[0,0,*])
	
;	Normalize the matrix rows for Back to complement the scaling in the poverlay calculation (analyze_image) ...
;	This is now done on 'read_da" for MP DA files ...
;	if n_comp gt 1 then begin
;		for i=0,n_comp-1 do begin
;			da_matrix[*,0,i] = da_matrix[*,0,i] * matrix.charge[i] / mean(matrix.charge)
;		endfor
;	endif

	print, 'Use Multi-phase Matrix File="',matrix_file
	print,' Sort using DA matrix: n_el = ', matrix.n_el
	mat_inv_yield = 1. / matrix.yield
	qy = where(matrix.yield lt 1.0e-5, nqy)
	if nqy ne 0 then begin
		mat_inv_yield[qy] = 0.0			; note: zero is tested in 'da_accumulate8'
		print,' Clip: ',nqy,' zero yields.'
	endif
endif else begin
	print, 'Use Matrix File="',matrix_file,'"'
	da_matrix = matrix.matrix
endelse
if n_elements(cal_a) lt 1 then cal_a = matrix.cal_orig.a
if n_elements(cal_b) lt 1 then cal_b = matrix.cal_orig.b

if step_toggle then xstep_count = 0L
charge = float(charge)

xcompress = (xcompress > 1)
ycompress = (ycompress > 1)

; 'xrange, yrange' are the full scan area, not compressed.
; 'xrange2, yrange2' are these compressed. These will become the 'original_size' values.
; 'xrange3, yrange3' are either a sub-region size or a cluster stripe or a bit of both.
;
; In sub-region mode:
;	'x_sub_region, y_sub_region' is the un-compressed sub-region size.
;	'xoffset, yoffset' is the un-compresssed offset.

xrange2 = long( xrange / xcompress)
yrange2 = long( yrange / ycompress)
xrange2e = (xrange2+1)/2
yrange2e = (yrange2+1)/2
xrange3 = long( xrange < x_sub_range)
xr = xrange3
xrange3 = xrange3 / xcompress
xrange3e = (xrange3+1)/2
yrange3 = yrange  < y_sub_range
yr = yrange3
yrange3 = yrange3 / ycompress
yrange3e = (yrange3+1)/2

yrange3 = long( yrange < y_sub_range)
yr = yrange3
yrange3 = yrange3 / ycompress
yrange3e = (yrange3+1)/2
xoffset3 = xoffset / xcompress
yoffset3 = yoffset / ycompress

; For multi-phase DA, we need to read in, offset and compress, the phase maps
; If phase image is a sub-region, only accept a sub-region exactly like the current one.
; If not, then need to allow for the phase image to have been compressed.

if mpda then begin
	pdai = read_geopixe_image( phase_dai, /silent, error=err)
	if err then goto, bad_mpdai
	if n_comp ne (*pdai).n_el then goto, bad_mpnum

	if (*pdai).sub_region then begin
		if (*pdai).x_sub_range ne x_sub_range then goto, bad_mpsub
		if (*pdai).y_sub_range ne y_sub_range then goto, bad_mpsub
		if (*pdai).xoffset ne xoffset then goto, bad_mpsub
		if (*pdai).yoffset ne yoffset then goto, bad_mpsub
		phase = smart_congrid( *(*pdai).image, xrange3, yrange3, (*pdai).n_el)
	endif else begin
		xoff = xoffset
		yoff = yoffset
		xran = xr
		yran = yr
		if (*pdai).xcompress gt 1 then begin
			xoff = xoff / (*pdai).xcompress
			xran = xran / (*pdai).xcompress
		endif
		if (*pdai).ycompress gt 1 then begin
			yoff = yoff / (*pdai).ycompress
			yran = yran / (*pdai).ycompress
		endif	
		print,'mpda: Yoff, Yran; Yrange3 set=',yoff, yran, yrange3	
		phase = smart_congrid( (*(*pdai).image)[ xoff:xoff+xran-1, yoff:yoff+yran-1, *], xrange3, yrange3, (*pdai).n_el)
	endelse
	if n_comp ne (*pdai).n_el then goto, bad_mpnum
	
;	mpda_table = build_DA_phase_weights( matrix, phase, pcorr, q=qmpe, compress=mcompress, error=error)		;@3-16
;	if error then goto, cleanup
	
	free_images, pdai
endif

bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
processed = 0LL
valid = 0LL
cancel = 0
etitle = ''
direction = 'X'
if ystep then direction = 'Y'
min_x = 10000
min_y = 10000
max_x = 0
max_y = 0

do_linear = 0
do_pileup = 0
do_throttle = 0
if obj->linear() then begin
	flinear = get_linearize(linearize, do_linear=do_linear, multi=multilinear, max=8191)
endif
if obj->pileup() then begin
	pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
endif
if obj->throttle() then begin
	throttle_factor = get_throttle(throttle, do_throttle=do_throttle)
endif

print,' Sort ADC(s) ',channel+1,' using DA matrix: n_el = ', matrix.n_el
if step_toggle then begin
    print,'    Advance ',direction,' using toggle bit ',toggle_bit,' in station ',step_station+1
endif else begin
    print,'    Advance ',direction,' after ',xstep_count,' counts in station ',step_station+1
endelse
gprint, output=cluster_debug,'xrange3,yrange3,matrix.n_el,n_attributes=',xrange3,yrange3,matrix.n_el,0

image = fltarr(xrange3,yrange3, matrix.n_el)
flux = fltarr(xrange3,yrange3)
dead_fraction = fltarr(xrange3,yrange3)
pileup_loss_map = fltarr(xrange3,yrange3)
nnpu = lonarr(xrange3,yrange3)
nn = lonarr(xrange3,yrange3)
count_rate_map = fltarr(xrange3,yrange3)

image_error = fltarr(xrange3e,yrange3e, matrix.n_el)
if mpda then begin
	invy = fltarr(xrange3e,yrange3e, matrix.n_el)
	fny = fltarr(xrange3e,yrange3e, matrix.n_el)
endif

;	'n_det' is the number of detectors selected, in array mode.
 
n_det = n_elements(channel)
array = 0
if n_det gt 1 then array=1
if channel[0] eq -1 then begin
    n_det = n_elements(cal_a)
    channel = indgen(n_det)							; all channels
    if n_det gt 1 then array=1
endif
nmax = max([channel,n_det-1])
print,'max_det=',nmax+1
print,'channel=',channel

j = 0L
nj = n_elements(file)
;evt_file = strip_file_ext( file) + obj->exctension()
evt_file = file
first = 1
random_subset = 0             ; warning: count is only ~50000

loop_file:
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, next

    device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, $
              flux=flux, dead_fraction=dead_fraction, ystep=ystep, first=first, $
              suppress=suppress, error=err
    if err then goto, finish
    nprogress = ((100000L / n_guide) > 1L) < 500L
    first = 0

    if j eq 0 then begin
       if do_progress then begin
         t = 'Clipped'
         if do_pileup then t = 'Pileup'
         progress, tlb=progress_tlb, title='Sort '+direction+' Step XY EVT file', $   ; put up a progress bar
		 		pars=['Events','Valid','Blocks','Bad XY',direction,t]

         iprogress = 0L
         case progress_file of
          0: begin
              p = { unit:1, value:[0LL,0LL,0LL,0LL,0LL,0LL]}
              end
          1: begin
              p = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
              end
          2: begin
              p = { unit:0, current:0L, size:xrange*yrange, file:evt_file[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
              end
          3: begin
              p = { unit:0, current:0L, size:progress_size, file:evt_file[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
              end
          else:
         endcase
       endif
    endif

    i = 0L
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
			ecompress=matrix.ecompress, title=etitle, step_events=step_events, $
			total_bad_xy=sbad_xy, total_processed=processed, step_count=xstep_count, $
			step_toggle=step_toggle, toggle_bit=toggle_bit, toggle_adc=step_station, $
			processed=count1, valid=good, ystep=ystep, multiple=multiple, time=tot, veto=veto, $
			station_e=ste, flux=flux, dead_fraction=dead_fraction, file=evt_file[j], error=err, $
			xoffset=xoffset, yoffset=yoffset

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

;     Translate event energy to DA matrix column.

       if n_elements(cal_a) eq 1 then begin
         energy = cal_a * float(e) + cal_b
       endif else begin
         energy = cal_a[ste] * float(e) + cal_b[ste]
       endelse

       col = uint( (energy - matrix.cal.b)/matrix.cal.a + 0.5)     ; - 1.0)     ; VAX - PC channel error

       if array then begin
         if n_elements(hist) lt 1 then begin
          hist = histogram( ste, max=nmax, min=0)
         endif else begin
          hist = histogram( ste, max=nmax, min=0, input=hist)
         endelse
       endif

       if ystep then begin
         q1 = where( ((col lt 0) or (col ge matrix.size)) or $
              ((x1 lt 0) and (x1 ge xrange3)), nq1)
       endif else begin
         q1 = where( ((col lt 0) or (col ge matrix.size)) or $
              ((y1 lt 0) or (y1 ge yrange3)), nq1)
       endelse

       clipped = clipped + nq1
		if nq1 gt 0 then veto[q1] = 1
		q = where( veto eq 0, count2)

       if do_progress then begin
         iprogress = iprogress + 1
         if iprogress ge nprogress then begin
          t = x1[0]
          if ystep then t=y1[0]
;		          			Events	Valid	Blocks	Bad XY	X/Y	clipped/pileup
          case progress_file of
              0: begin
                 p.value = [processed,good,i,bad_xy,t,clipped]
                 end
              1: begin
                 p.value = [processed,good,i,bad_xy,t,clipped]
                 p.current = j
                 p.file = evt_file[j]
                 end
              2: begin
                 p.value = [processed,good,i,bad_xy,t,clipped]
                 p.current = i
                 end
              3: begin
                 p.value = [processed,good,i,bad_xy,t,clipped]
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

       if count2 gt 0 then begin
       
			if random_subset then begin								; so this limits how small fraction can be.
				seed = i+1											; To force a known seed and random sequence
				nr = long(float(count2) * accept_fraction) > 1
				q2 = long(float(count2) * randomu(seed,nr) < (count2-1))
				qr = q[q2]
				q1 = where( veto[qr] eq 0, nq1)
				veto[*] = 1
				if nq1 gt 0 then veto[qr[q1]] = 0 
				q = where( veto eq 0, count2)
			endif
			valid = valid + count2

;      Pass the event arrays (x2,y2,col2) and the 'image' array
;      and the DA 'matrix' to the DLL routine 'da_accumulate'.
;      This will accumulate the DA values in 'col2' into
;      'image' at coords 'x2,y2', for each element.

;      For detector arrays, divide accumulated DA increments here (in da_accumulate2)
;      by the number of detector elements. This does assume that all detectors sample
;      the same sensivity and count rate.

			min_x = min( [min_x, min(x1[q]) + xoffset3])
			min_y = min( [min_y, min(y1[q]) + yoffset3])
			max_x = max( [max_x, max(x1[q]) + xoffset3])
			max_y = max( [max_y, max(y1[q]) + yoffset3])

			if mpda then begin
				err = da_accumulate10( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el, nnpu,nn, $			;@3-16
							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, phase, n_comp, $
							mat_inv_yield, invy, fny, multiple=multiple)
;				err = da_accumulate8( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el, nnpu,nn, $			;@3-16
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, mpda_table, $
;							mat_inv_yield, invy, fny, multiple=multiple, q=qmpe, compress=mcompress)
;				err = da_accumulate7( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el, nnpu,nn, $
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, mpda_table, $
;							multiple=multiple, q=qmpe, compress=mcompress)
;				err = da_accumulate6( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3, nnpu,nn, $
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, phase, n_comp, $
;							multiple=multiple)
			endif else begin
				err = da_accumulate5( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3, nnpu,nn, $
							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, $
							multiple=multiple)
			endelse
         if err ne 0 then begin
          print,'da_xstep_evt: error (',err,') return from da_accumulate3'
          goto, finish
         endif
       endif
       if events gt 0 then if processed gt events then begin
         print,'da_xstep_evt: requested event count exceeded; stop.'
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
    t = max([x1])
    if ystep then t=max([y1])
    print, ' final '+direction+' = ', t
    print, ' valid events = ', valid
    print, ' bad event triplets = ', bad_xy
    print, ' clipped to image,matrix bounds, or not station ',channel+1,' = ', clipped
    print, ' pileup losses = ',pileup_losses
    if n_elements(flux) gt 1 then print,' found FLUX array'
    if n_elements(dead_fraction) gt 1 then print,' found DEAD_FRACTION array'
	print, ' X range = ', min_x, max_x
	print, ' Y range = ', min_y, max_y
	if mpda then print,' used PHASE and YIELD arrays for Multiphase DA'
    if n_elements(p) gt 0 then begin
       if do_progress then begin
         p.value = [processed,valid,i,bad_xy,t,(do_pileup ? pileup_losses: clipped)]
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
    endif
close, 1
on_ioerror, null
t = 'EVT sorting complete. Save Images: '+strip_file_ext(output)+'.dai'
if do_progress then begin
    progress, /complete, progress_tlb, t
endif

;--------------------------------------------------------------------------
;
;   Write the image results file ...

null_image = define(/image)
img = null_image
nxy = long(xrange3) * long(yrange3)
ninf = 0L
for i=0L,matrix.n_el-1 do begin
	q = where(finite(image[*,*,i]) eq 0, nq)
	if nq gt 0 then begin
		image[q + nxy*i] = 0.0
		ninf = ninf + nq
	endif
endfor
if ninf gt 0 then print,'Killed ',nq,' non finite pixels.'

print,'da_xstep_evt: write image file - ',strip_file_ext(output)+'.dai'

; For detector arrays, need to scale down result, depending on the number of
; detectors actually used to sort the EVT. Without an array, just count n_det.
; With an array, use the rGamma yield-ratio arrays.

;if matrix.array.on then begin
;	rG = fltarr(matrix.n_el)
;	for i=0L,n_det-1 do begin
;		if channel[i] lt matrix.array.n_det then begin
;			rG = rG + matrix.array.rGamma[channel[i],*]
;		endif
;	endfor
;	xd = 1./rG
;endif else begin
;	xd = replicate(1./float(n_det), matrix.n_el)
;endelse

xd = 1. / multiplicity_scale( channel, matrix, multiplicity=multiplicity)		;@3-16
xd2 = xd*xd
for i=0L,matrix.n_el-1 do begin
	image[*,*,i] = image[*,*,i] * xd[i]
	image_error[*,*,i] = image_error[*,*,i] * xd2[i]
endfor

xstep_on = 1L
type = 0L                ; 0=ppm.uC data, 1=mineral fractions
;units = ['keV','MeV','MeV']

img.source = evt_file[0]
img.source2 = evt_file[nj-1]
img.throttle = throttle
img.pileup = pileup
img.DevObj = obj
img.sample = sample
img.grain = grain
if lenchr(comment) gt 0 then begin
    img.comment = comment
endif else if n_elements(etitle) gt 0 then begin
    img.comment = etitle
endif

ic = 0
if channel[0] ge 0 then ic=channel[0]
img.cal.poly = [cal_b[ic], cal_a[ic]]
img.cal.order = 1
img.cal.units = 'keV'
if array eq 0 then begin
    img.array = 0
    n_active = 1
    img.pactive = ptr_new( channel[0])
endif else begin
    img.array = 1
    n_active = n_elements(channel)
    img.pactive = ptr_new( channel)

    poly = fltarr(max_image_cal+1)
    cal0 = {order:1, units:'keV', poly:poly}
    cal = replicate(cal0, n_active)
    for i=0L,n_active-1 do begin
       cal[i].poly[0] = cal_b[channel[i]]
       cal[i].poly[1] = cal_a[channel[i]]
    endfor
    img.pcal = ptr_new( cal )
endelse
img.ecompress = matrix.ecompress

live = 1.0
if (obj->name() eq 'OM_DAQ_DEVICE') and (suppress eq 0) then begin
    charge = lmf_charge
    dead = 0.0
    if n_elements(group) ge 1 then charge = OM_charge_select(group,charge, live=lmf_live, dead=dead)
    live = 1.0-dead
    img.charge = charge
    scanx = float(lmf_size[0])
    scany = float(lmf_size[1])
    img.scan.x = lmf_size[0]*0.001
    img.scan.y = lmf_size[1]*0.001
endif else begin
    img.scan.x = scanx*0.001
    img.scan.y = scany*0.001
endelse

img.processed = processed
img.valid = valid
img.bad_xy = bad_xy
img.clipped = clipped

img.matrix.label = matrix.label
if matrix.label eq '' then matrix.label=matrix_file
img.matrix.file = matrix.file
img.matrix.charge = matrix.charge[0]
img.matrix.mdl = ptr_new(matrix.mdl)

img.n_el = matrix.n_el
img.el = ptr_new(matrix.el)

img.xsize = xrange3
img.ysize = yrange3
img.original_xsize = xrange2
img.original_ysize = yrange2
img.xcompress = xcompress
img.ycompress = ycompress

sub_region = 0
if (xoffset ne 0) or (yoffset ne 0) or (x_sub_range ne xrange) or $
				(y_sub_range ne yrange) then sub_region=1
img.sub_region = sub_region
img.xoffset = xoffset
img.yoffset = yoffset
img.x_sub_range = x_sub_range
img.y_sub_range = y_sub_range

valid = 0
if (max_x gt min_x) and (max_y gt min_y) then valid=1
img.bounds.valid = valid and obj->use_bounds()
img.bounds.xmin = min_x
img.bounds.xmax = max_x
img.bounds.ymin = min_y
img.bounds.ymax = max_y

img.ystep = ystep                ; Y step mode ?
img.xstep_on = xstep_on               ; X or Y step mode used
img.xstep = xstep_count
img.step_events = step_events
img.step_toggle = step_toggle
img.toggle_bit = toggle_bit
img.toggle_station = step_station

img.events = events                  ; terminate events, NOT total events

img.type = type
img.channel = channel[0]
img.detector = detector
if n_elements(hist) gt 0 then img.hist = ptr_new(hist, /no_copy)

; Correct for flux/current variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, so the correction here must
; preserve the average.

if n_elements(flux) gt 1 then begin
    total_flux = total(flux)
	use_flux = 1
    if total_flux gt 0.001 then begin
		flux = flux_repair( flux, /median)
		total_flux = total(flux)
		
		charge = total_flux * flux_ic.conversion
		aps_count_to_charge = flux_ic.conversion
		use_flux = flatten

		if use_flux then begin
			avf = total_flux / (float(xrange2)*float(yrange2))
			q = where(flux gt 0.1*avf)
			scale = flux
			scale[*] = 1.0
			tf = total(flux[q])
			scale[q] = ((flux[q] * n_elements(q) / tf) > 0.1) < 10.
			
			for i=0L,matrix.n_el-1 do begin
				image[*,*,i] = image[*,*,i] / scale
			endfor
		endif
       img.has_flux = 1
       img.flux = ptr_new(flux)
    endif
endif
if random_subset then charge = charge*accept_fraction
img.charge = charge

; Correct for dead-time variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, and the flux is corrected for DT losses
; already, and this is used above to correct the image, so we don't need any
; more correction here.

if n_elements(dead_fraction) gt 1 then begin
;    tdead = total(dead_fraction)/n_elements(dead_fraction)
;    if tdead gt 0.001 then begin
;       q = where((dead_fraction gt 0.0) and (dead_fraction lt 0.95))
;       scale = (1.0 - dead_fraction)
;       t = scale
;       scale[*] = 1.0
;       mt = mean(t)
;       scale[q] = t[q] / mt
;
;       for i=0L,matrix.n_el-1 do begin
;         image[*,*,i] = image[*,*,i] / scale
;       endfor
;     img.has_flux = 1           ; save dead in Image later?
;     img.flux = ptr_new(flux)
;    endif
    live = 1.0 - mean(dead_fraction)     ; save in Image later?
endif

if mpda then begin
	print,'da_xstep_evt: save Phases, n_comp =', n_comp
	img.has_phase = 1
	img.phase = ptr_new( phase, /no_copy)
	
	print,'da_xstep_evt: save Yields, n_el =', img.n_el
	img.has_yield = 1
	yield = finite_image( fny / invy, /mean)				; note leave /0 in this ???
	img.yield = ptr_new( yield, /no_copy)
endif

img.has_errors = 1
img.image = ptr_new( image, /no_copy)
img.error = ptr_new( image_error, /no_copy)

img.file = output
pimg = ptr_new(img, /no_copy)

write_geopixe_image, pimg, output, /no_display

if arg_present(images) eq 0 then begin
    free_images, pimg
endif else begin
    images = pimg
endelse

cleanup:
	print, 'da_xstep_evt: finish time = ',systime()
    if do_progress  then progress, /ending, progress_tlb
	free_DA, matrix
    return

bad_matrix:
	warning, 'da_xstep_evt', 'Bad DA matrix.'
    goto, cleanup
bad_mpdam:
    warning, 'da_xstep_evt', 'Bad multiphase DA correction file read.'
    goto, cleanup
bad_mpdai:
    warning, 'da_xstep_evt', 'Bad phase map DAI file read.'
    goto, cleanup
bad_mpsub:
    warning, 'da_xstep_evt', ['Phase map sub-region size and offset', $
											'do not match the present settings.']
    goto, cleanup
bad_mpnum:
    warning, 'da_xstep_evt', 'Number of phases in DAI does not match number of DA matrices.'
    goto, cleanup
bad_mpnum2:
    warning,  'da_xstep_evt', 'Number of elements in one DA does not match number in first matrix.'
    goto, cleanup
bad_mpsize:
    warning,  'da_xstep_evt', 'Inconsistent DA matrix sizes.'
    goto, cleanup
bad_file:
	warning, 'da_xstep_evt', 'EVT File not found.'
	return
bad_obj:
	warning, 'da_xstep_evt', 'Bad device object for: '+device
    return
end
