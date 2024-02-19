pro cut_xstep_evt, filei, pcuts, xstep_count, step_events=step_events, xrange=xrange, yrange=yrange, $
          charge=charge, cal_a=cal_a, cal_b=cal_b, ecompress=ecompress, $
          xcompress=xcompress, ycompress=ycompress, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          step_toggle=step_toggle, toggle_bit=toggle_bit, step_station=step_station, $
          scanx=scanx, scany=scany, sample=sample, grain=grain, comment=comment, $
          progress=do_progress, images=images, device=devicei, ystep=ystep, group=group, $
         throttle=throttle, pileup=pileup, stim_mean=stim_mean, suppress=suppress

;   Read an .evt file.
;   Analyze using cuts array pointed to by 'pcuts'.
;   Sort events from station 'channel', for 'detector' type of data (0=PIXE, 1=PIGE, ...).
;
;   Assume units for cal are keV for PIXE, MeV for PIGE.
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
;   If 'events' set, stop at this number of events.
;
;   'xrange', 'yrange' are the X,Y ranges in the original data.
;   Use 'xcompress', 'ycompress' to compress these by an integral factor.
;
;   'cal_a', 'cal_b', are the energy calibration (mandatory).
;   The units are "keV" always, as in DA_EVT (must match CUTs to these units).
;   If these are vectors, then they will be indexed with the 'ste' channel returned.
;
;   Write elemental images out as a .dai DA image file.
;   Use filename 'output', if specified.
;   Return images pointer to 'images', or just write file.
;
;   'scanx', 'scany' are optional scan sizes (microns).
;   'sample', 'grain', 'comment' are optional strings.
;   'throttle' gives name of throttle factors file.
;   'pileup' give name of pileup Time-over-threshold limits file.
;
;   Assume that x,y are centred on 2048
;   Station numbers start at '0'.
;   Bit numbers start at '0'; top bit in ADC value is '12'.
;
;   'Channel' gives the station/ADC numbers, which start at '0'.
;   This can be a vector of channels to sort, or -1 meaning all.
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
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
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
       warning,'Cut_xstep_EVT',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

images = 0L
define_devices
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

if n_elements(step_toggle) lt 1 then step_toggle = 0L
if ((step_toggle eq 1) and (n_params() lt 2)) or $
       ((step_toggle eq 0) and (n_params() lt 3)) then begin
    print,'cut_xstep_evt: missing arguments'
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
       title='Select EVT file to sort', filter='*'+ obj->extension(), $
       /fix_filter) 
endif else begin
    path = extract_path( file[0])
    if path eq file[0] then begin
       file = file_requester(/read, path=path, file=file, $
         title='Select EVT file to sort', filter='*'+ obj->extension(), $
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
       if n_elements(step_station) lt 1 then step_station = 0L
    endif else begin
       if n_elements(step_station) lt 1 then step_station = 2L     ; old data default
    endelse
endelse
if n_elements(xrange) lt 1 then xrange = 256
if n_elements(yrange) lt 1 then yrange = 256
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(cal_a) lt 1 then cal_a = 1.0
if n_elements(cal_b) lt 1 then cal_b = 0.0
if n_elements(ecompress) lt 1 then ecompress = 1
if n_elements(xcompress) lt 1 then xcompress = 1
if n_elements(ycompress) lt 1 then ycompress = 1
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(events) lt 1 then events=0L
if n_elements(toggle_bit) lt 1 then toggle_bit = 12
if n_elements(scanx) lt 1 then scanx = 0.0
if n_elements(scany) lt 1 then scany = 0.0
if n_elements(sample) lt 1 then sample = '?'
if n_elements(grain) lt 1 then grain = '?'
if n_elements(comment) lt 1 then comment = '?'
if n_elements(throttle) lt 1 then throttle = ''
if n_elements(pileup) lt 1 then pileup = ''
if n_elements(detector) lt 1 then detector=1L
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(stim_mean) lt 1 then stim_mean = 0L
if n_elements(suppress) lt 1 then suppress=0

output = strip_file_ext(outputi) + '.dai'

channel = channeli
if ptr_valid(pcuts) eq 0 then goto, bad_cuts
s = size(*pcuts,/structure)
if (s.type_name ne 'STRUCT') then goto, bad_cuts

r = [0.001,1.0,1000.,1000000.,1.0]
q = where( strupcase(((*pcuts).units)[0]) eq ['EV','KEV','MEV','GEV','ENERGY'])
scale = 1.0
if q[0] ne -1 then scale = r[q[0]]          ; conversion to "keV"

n_el = n_elements(*pcuts)
cut = fltarr(6,n_el)
q = sort( (*pcuts).e[2])

for i=0L,5 do cut[i,*] = scale * (*pcuts)[q].e[i]
dleft = (*pcuts)[q].dleft
dright = (*pcuts)[q].dright
type = uint((*pcuts)[q].type)
el = (*pcuts)[q].el

if step_toggle then xstep_count = 0L
charge = float(charge)
xcompress = (xcompress > 1)
ycompress = (ycompress > 1)
xrange2 = fix( xrange / xcompress)
yrange2 = fix( yrange / ycompress)
xrange2e = (xrange2+1)/2
yrange2e = (yrange2+1)/2

bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
processed = 0LL
valid = 0LL
cancel = 0
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

print,' Sort ADC ',channel+1,' using cuts'
if step_toggle then begin
    print,'    Advance ',direction,' using toggle bit ',toggle_bit,' in station ',step_station+1
endif else begin
    print,'    Advance ',direction,' after ',xstep_count,' counts in station ',step_station+1
endelse

image = fltarr(xrange2,yrange2,n_el)
image_error = fltarr(xrange2e,yrange2e,n_el)
flux = fltarr(xrange2,yrange2)
dead_fraction = fltarr(xrange2,yrange2)
nnpu = lonarr(xrange2,yrange2)
nn = lonarr(xrange2,yrange2)

if stim_mean then begin
	image_count = lonarr(xrange2,yrange2,n_el)
	image_error_count = lonarr(xrange2e,yrange2e,n_el)
endif else begin
	image_count = 0L
	image_error_count = 0L
endelse

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
;evt_file = strip_file_ext( file) + obj->extension()
evt_file = file
first = 1

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
         progress, tlb=progress_tlb, title='Sort XStep XY EVT file', $             ; put up a progress bar
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
			ecompress=ecompress, title=etitle, step_events=step_events, $
			total_bad_xy=sbad_xy, total_processed=processed, step_count=xstep_count, $
			step_toggle=step_toggle, toggle_bit=toggle_bit, toggle_adc=step_station, $
			processed=count1, valid=good, ystep=ystep, multiple=multiple, time=tot, veto=veto, $
			station_e=ste, flux=flux, dead_fraction=dead_fraction, file=evt_file[j], error=err
          
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

		if array then begin
			if n_elements(hist) lt 1 then begin
				hist = histogram( ste, max=nmax, min=0)
			endif else begin
				hist = histogram( ste, max=nmax, min=0, input=hist)
			endelse
		endif

;     Reject those events with x,y outside the pixel range
;     'count2' is the total remaining good events.

		if ystep then begin
			q1 = where( (x1 lt 0) or (x1 ge xrange2), nq1)
		endif else begin
			q1 = where( (y1 lt 0) or (y1 ge yrange2), nq1)
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
         valid = valid + count2

;      Pass the event arrays (x2,y2,energy2) and the 'image' array
;      and the cuts 'cuts' to the DLL routine 'cut_accumulate3'.
;      This will accumulate the cuts into
;      'image' at coords 'x2,y2', for each element cut.

		stim_mean = 0

         min_x = min( [min_x, min(x1[q])])
         min_y = min( [min_y, min(y1[q])])
         max_x = max( [max_x, max(x1[q])])
         max_y = max( [max_y, max(y1[q])])

         err = cut_accumulate5( x1,y1,energy,pu,veto, n,count2, image, xrange2,yrange2, nnpu,nn, $
         			image_error,xrange2e,yrange2e, cut,type,dleft,dright, n_el, multiple=multiple, $
					stim_mean, image_count, image_error_count)

           if err ne 0 then begin
             print,'cut_evt: error (',err,') return from cut_accumulate'
          goto, finish
         endif
       endif
       if events gt 0 then if processed gt events then begin
         print,'cut_xstep_evt: requested event count exceeded; stop.'
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
    print, ' clipped to image bounds, or not station ',channel+1,' = ', clipped
    print, ' pileup losses = ',pileup_losses
    if n_elements(flux) gt 1 then print,' found FLUX array'
    if n_elements(dead_fraction) gt 1 then print,' found DEAD_FRACTION array'
    if n_elements(p) gt 0 then begin
       if do_progress then begin
         p.value = [processed,valid,i,bad_xy,t,(do_pileup ? pileup_losses: clipped)]
         progress, /update, progress_tlb, p
       endif
    endif
close, 1
on_ioerror, null
t = 'EVT sorting complete. Save Images ... '
if do_progress then begin
    progress, /complete, progress_tlb, t
endif

;--------------------------------------------------------------------------
;
;   Write the image results file ...

null_image = define(/image)
img = null_image

print,'cut_xstep_evt: write image file - ',output

; For STIM mean energy mode, the total number of additions is stored in Error.
; Use it now to scale sum of energies back to a mean.

type = 2L                ; 0=ppm.uC data, 1=mineral fractions, 2=counts, 3=energy
if stim_mean then begin
	type = 3L
	image = image / image_count
	q = where( finite(image) eq 0)
	if q[0] ne -1 then image[q] = 0.0
	image_error = image_error / image_error_count
	q = where( finite(image_error) eq 0)
	if q[0] ne -1 then image_error[q] = 0.0

	timage = image
	timage_error = image_error
	image = fltarr(xrange2,yrange2,2*n_el)
	image_error = fltarr(xrange2e,yrange2e,2*n_el)
	image[*,*,0:n_el-1] = timage
	image[*,*,n_el:2*n_el-1] = image_count
	image_error[*,*,0:n_el-1] = timage_error
	image_error[*,*,n_el:2*n_el-1] = image_error_count
	n_el = 2*n_el
	el = [el, 'n('+strtrim(el,2)+')']
endif

; For detector arrays, need to scale down result, depending on the number of
; detectors actually used to sort the EVT.

xd = 1./float(n_det)
xd2 = xd*xd
image = image * xd
image_error = image_error * xd2

xstep_on = 1L
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
img.ecompress = ecompress

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

img.matrix.label = 'Cuts'
img.matrix.file = (*pcuts)[0].file
img.matrix.charge = 1.0
img.matrix.mdl = ptr_new(fltarr(n_el))

img.n_el = n_el
img.el = ptr_new(el)

img.xsize = xrange2
img.ysize = yrange2
img.original_xsize = xrange2
img.original_ysize = yrange2
img.xcompress = xcompress
img.ycompress = ycompress
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
img.mode = 1 + stim_mean
img.channel = channel[0]
img.detector = detector
if n_elements(hist) gt 0 then img.hist = ptr_new(hist, /no_copy)

; Correct for flux/current variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, so the correction here must
; preserve the average.

if n_elements(flux) gt 1 then begin
	total_flux = total(flux)
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
img.charge = charge

; Correct for dead-time variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, and the flux is corrected for DT losses
; already, and this is used above to correct the image, so we don't need any
; more correction here.

if n_elements(dead_fraction) gt 1 then begin
;    tdead = total(dead_fraction)/n_elements(dead_fraction)
;    if tdead gt 0.001 then begin
;       q = where((dead_fraction gt 0.0) and (dead_fraction lt 0.9))
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

if do_progress then progress, /ending, progress_tlb
return

bad_cuts:
	warning, 'cut_xstep_evt', 'Bad CUTs data.'
	return
bad_file:
	warning, 'cut_xstep_evt', 'EVT file not found.'
	return
bad_obj:
	warning, 'cut_xstep_evt', 'Bad device object for: '+device
    return
end
