pro cut_evt, filei, pcuts, files=files, cuts=cuts, all=all, xstep_count, step_events=step_events, xrange=xrange, yrange=yrange, $
          charge=charge, cal_a=cal_a, cal_b=cal_b, ecompress=ecompress, $
          xcompress=xcompress, ycompress=ycompress, linearize=linearize, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          step_toggle=step_toggle, toggle_bit=toggle_bit, step_station=step_station, $
          scanx=scanx, scany=scany, sample=sample, grain=grain, comment=comment, $
		  facility=facility, endstation=endstation, $
          progress=do_progress, images=images, device=devicei, group=group, $
         throttle=throttle, pileup=pileup, stim_mean=stim_mean, suppress=suppress, $
         xorigin=xorigin, yorigin=yorigin, ic=flux_ic, flatten=flatten, $
          xoffset=xoffset, yoffset=yoffset, x_sub_range=x_sub_range, y_sub_range=y_sub_range, $ 
          pv_list=pv_list, cluster_index=cluster_index, cluster_total=cluster_total, $
          cluster_result=cluster_result, cluster_debug=cluster_debug, devpars=devpars

;   file		list-mode file(s) to process (or specify using "files=" keyword).
;   pcuts		Analyze using this using CUTs pointed to by 'pcuts' (or use "cuts" keyword)
;	/all		Use full energy range single cut. Do not use 'pcuts' or 'cuts'.
;   detector	type of data (0=PIXE, 1=PIGE, ...).
;   device		list-mode device/format type
;   devpars		device dependent options parameter struct
;   charge		charge/flux for whole image (some devices will pop-up a
;   			requester to set PVs and charge conversion)
;   xrange		the X ranges in the original data
;   yrange		Y range
;   xcompress compress these by an integral factor.
;   ycompress
;
;   cal_a,cal_b	 the energy calibration (mandatory).
;   			These must be in 'keV' to match the CUTs file.
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
;   xoffset, yoffset	pixel offset for origin of image to sort
;   x_sub_range, y_sub_range  pixel range for a sub-region scan (no compressed)
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

COMPILE_OPT STRICTARR
common c_evt_last, last
common c_om_3b, c_per_pulse, lmf_charge
common c_om_4, lmf_size
common c_om_5, lmf_live
common aps_4, aps_count_to_charge
common c_maia_6, maia_y_min
common c_geopixe_adcs, geopixe_max_adcs
common c_null_image_1, max_image_cal
common c_seed, seed
common c_debug_warnings, enable_warning_popup
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8
if n_elements(seed) lt 1 then seed=1L
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=0.

if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(suppress) lt 1 then suppress=0
if n_elements(cluster_total) lt 1 then cluster_total=0
if n_elements(cluster_index) lt 1 then cluster_index=0
if n_elements(cluster_debug) lt 1 then cluster_debug=-1		; stdout

gprint, active=2								; enable gprint diagnostics with level at least this
												; set to active=2 normally, =1 for most diagnostics
cluster_result = 'null'
if cluster_total gt 0 then begin
	startupp, /error							; , /database
	cluster = 1
	suppress = 1
	do_progress = 0
	enable_warning_popup = 0
endif else cluster=0
gprint,level=2, output=cluster_debug, '================================================================================================'
gprint,level=2, output=cluster_debug, 'CUT_EVT: start time = ',systime()

;cluster = 1
;cluster_total = 3										; for debugging
;cluster_index = 1
;openw, cluster_debug, 'cut_evt_debug.txt', /get_lun

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
       s2 = ['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c]
       if cluster then begin
       		worker_error, stringify( {command:'CUT_EVT', error:s2 })
       		gprint, output=cluster_debug, 'CUT_EVT', s2 
       endif else begin
	       warning,'CUT_EVT', s2, /error
       endelse
       MESSAGE, /RESET
       return
    endif
endif

images = 0L
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei

define_devices
obj = new_device_object( device, options=devpars, error=err)
if err then goto, bad_obj

list = obj->options_legend()
gprint, output=cluster_debug, list

;if n_params() lt 2 then begin
;    gprint, output=cluster_debug,'cut_evt: missing arguments'
;    return
;endif
if n_elements(files) gt 0 then filei=files
file = strtrim(filei,2)
if n_elements(channeli) lt 1 then channeli = 0L
if n_elements(xrange) lt 1 then xrange = 256
if n_elements(yrange) lt 1 then yrange = 256
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(cal_a) lt 1 then cal_a = 1.0
if n_elements(cal_b) lt 1 then cal_b = 0.0
if n_elements(ecompress) lt 1 then ecompress = 1
if n_elements(xcompress) lt 1 then xcompress = 1
if n_elements(ycompress) lt 1 then ycompress = 1
if n_elements(events) lt 1 then events=0L
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
if n_elements(facility) lt 1 then facility = ''
if n_elements(endstation) lt 1 then endstation = ''
if n_elements(throttle) lt 1 then throttle = ''
if n_elements(pileup) lt 1 then pileup = ''
if n_elements(detector) lt 1 then detector=1L
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(stim_mean) lt 1 then stim_mean = 0L
if n_elements(flux_ic) lt 1 then flux_ic = {mode:0, pv:'', val:0.0, unit:0.0, conversion:1., use_dwell:0, dwell:1.0}
if n_elements(flatten) lt 1 then flatten=1
if n_elements(pv_list) lt 1 then pv_list=['']
if x_sub_range eq 0 then x_sub_range = xrange
if y_sub_range eq 0 then y_sub_range = yrange

evt_file = file
output = strip_file_ext(strip_non_print(outputi,/no_tab)) + '.dai'

build_ylut = 1
if cluster then begin
	build_ylut=0
;	q1 = indgen(cluster_total)
;	q1 = congrid( q1, n_elements(file), /center)
;	q2 = where(q1 eq cluster_index, nq)
	q2 = obj->cluster_files( file, cluster_total, cluster_index, nq=nq)

	if nq lt 1 then goto, bad_cluster
	evt_file = file[q2]
	charge = charge / float(cluster_total)
	
	gprint,level=2, output=cluster_debug, 'Cluster #',cluster_index,' files=', evt_file
	output = output + '.' + str_tidy(cluster_index)
endif
gprint,level=2, output=cluster_debug, 'Output=',output
cluster_result = output

channel = channeli									; this is new

if n_elements(all) ne 0 then begin					; use /all whole spectrum single CUT
	if all ne 1 then goto, bad_all
	dleft = 0.0
	dright = 0.0
	type = 0US
	el = "NN"
	cut = [0.,0.,-3.,100.,0.,0.]
	n_el = 1
endif else begin
	all = 0
	if n_elements(cuts) ne 0 then pcuts=cuts
	if ptr_good(pcuts,/struct) eq 0 then goto, bad_cuts
	
	r = [0.001,1.0,1000.,1000000.,1.0]
	q = where( strupcase(((*pcuts).units)[0]) eq ['EV','KEV','MEV','GEV','ENERGY'])
	scale = 1.0
	if q[0] ne -1 then scale = r[q[0]]         		; conversion to "keV"
	
	n_el = n_elements(*pcuts)
	cut = fltarr(6,n_el)
	q = sort( (*pcuts).e[2])
	
	for i=0L,5 do cut[i,*] = scale * (*pcuts)[q].e[i]
	dleft = (*pcuts)[q].dleft
	dright = (*pcuts)[q].dright
	type = uint((*pcuts)[q].type)
	el = (*pcuts)[q].el
endelse

charge = float(charge)
xcompress = (xcompress > 1)
ycompress = (ycompress > 1)

processed = 0LL
valid = 0LL
bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
cancel = 0
etitle = ''
min_x = 100000L
min_y = 100000L
max_x = 0L
max_y = 0L

; Devices with numeric file extensions can have a YLUT to make sorting for region spectra quicker.
; These are built here, but not for special incremental modes that must be sorted fully, such as the
; Maia 'Correct Y Encoder' mode. These are built during DA_EVT processing below.
; A YLUT is only built for non-cluster scans. For a cluster scan, this routine is only called for a
; stripe by each node. Hence, the YLUT must be built prior to this.

if obj->ylut() then begin
	if build_ylut then begin
		if obj->incremental_ylut() eq 0 then begin
			ylut = obj->build_ylut( evt_file, output=output, error=err)
			if err then goto, bad_ylut_build
			build_ylut = 0
			gprint,level=2, output=cluster_debug,'Build YLUT success, YLUT=',ylut		
		endif else begin	
			nylut = max(long(extract_extension(evt_file)))+1
			ylut = lonarr(nylut)
			err = 1
		endelse
	endif else begin
		ylut = obj->get_ylut( evt_file[0], output=output, /strip, error=err)
		if err then goto, bad_ylut_get
		gprint,level=2, output=cluster_debug,'Get YLUT=',ylut		
	endelse
endif else build_ylut=0

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
xrange3 = long( (xrange < x_sub_range) / xcompress)
xrange3e = (xrange3+1)/2
yrange3 = ( yrange  < y_sub_range ) / ycompress
yrange3e = (yrange3+1)/2

; In cluster mode, only build a stripe image for each node, based on the range of Y seen in
; the evt_file[] array. Note that "yoffset" is compressed in read_buffer elsewhere.
; N.B. range method returns a very large 'max' for the last file, so clip to (yrange-1).

if cluster and obj->ylut() then begin
	if obj->incremental_ylut() and err then goto, bad_ylut_ymode

	range = obj->range_ylut( evt_file, error=err)
	if err eq 0 then begin
		yoffset = ((yoffset > range.min) > 0) < (yrange-1)
		yoffset3 = yoffset / ycompress
		yrange3 = (range.max > 1) < (yrange-1)
		yrange3 = yrange3 / ycompress
		yrange3 = ((yrange3 - yoffset3 + 1) > 1)			;  < (yrange-yoffset)
		yrange3 = ( yrange3  < y_sub_range )
		yrange3e = (yrange3+1)/2
		gprint,level=2, output=cluster_debug,'Y offset, range3 set=',yoffset, yrange3	
	endif else begin
		gprint,level=2, output=cluster_debug,'Failed to find valid YLUT to reduce Y image range.'
	endelse
endif else begin
	yrange3 = long( (yrange < y_sub_range) / ycompress)
	yrange3e = (yrange3+1)/2
	yoffset3 = yoffset / ycompress
endelse
xoffset3 = xoffset / xcompress

do_linear = 0
do_pileup = 0
do_throttle = 0
do_attributes = 0
if obj->linear() then begin
	flinear = get_linearize(linearize, do_linear=do_linear, multi=multilinear, max=8191)
	if do_linear then gprint,level=2, output=cluster_debug, 'Linearize file =',linearize
endif
if obj->pileup() then begin
	pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
	if do_pileup then gprint,level=2, output=cluster_debug, 'Pileup file =',pileup
endif
if obj->throttle() then begin
	throttle_factor = get_throttle(throttle, do_throttle=do_throttle)
	if do_throttle then gprint,level=2, output=cluster_debug, 'Throttle file =',throttle
endif
attributes = obj->get_attribute_list()
if attributes[0] ne '' then begin
	do_attributes = 1
	n_attributes = n_elements(attributes)
endif else n_attributes=0

gprint,level=2, output=cluster_debug,' Sort using CUTs: n_el = ', n_el
gprint, output=cluster_debug,'xrange3,yrange3,n_el,n_attributes=',xrange3,yrange3,n_el,n_attributes

image = fltarr(xrange3,yrange3, n_el + n_attributes)
image_error = fltarr(xrange3e,yrange3e, n_el + n_attributes)
flux = fltarr(xrange3,yrange3, n_attributes+1)
dead_fraction = fltarr(xrange3,yrange3)
pileup_loss_map = fltarr(xrange3,yrange3)
nnpu = lonarr(xrange3,yrange3)
nn = lonarr(xrange3,yrange3)
count_rate_map = fltarr(xrange3,yrange3)

if stim_mean then begin
	image_count = lonarr(xrange3,yrange3,n_el)
	image_error_count = lonarr(xrange3e,yrange3e,n_el)
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
gprint, output=cluster_debug,'max_det=',nmax+1
gprint, output=cluster_debug,'channel=',channel

j = 0L
nj = n_elements(evt_file)
last_time = systime(/seconds)
first = 1
random_subset = 0

loop_file:
	gprint,level=1, output=cluster_debug, 'File loop, evt_file[j] =', evt_file[j]
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, null									; was next

	if build_ylut and obj->ylut() then begin
		jy = long2(extract_extension(evt_file[j])) > 0
	endif
	firsty = 1
    on_ioerror, next									; was next
	
    device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, $
                 flux=flux, dead_fraction=dead_fraction, first=first, $
                 suppress=suppress, ic=flux_ic, error=err, x_coords=x_coords, $
				 y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units, $
				 beam_energy=beam_energy
				 
    if err then goto, finish
    if first then nprogress = ((100000L / n_guide) > 1L) < 500L

    if j eq 0 then begin
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
       if do_progress then begin
        t = 'Clipped'
        if do_pileup then t = 'Pileup'
         progress, tlb=progress_tlb, title='Sort XY EVT file', $           ; put up a progress bar
		 	pars=['Events','Valid','Blocks','Bad XY','Size',t]

         iprogress = 0L
       endif
       if cluster then begin
	   		if worker_progress(0.0) eq 0 then begin
				close, 1
				return
			endif
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

       read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress, $
			station_e=ste, time=tot, veto=veto, ecompress=ecompress, multiple=multiple, $
			xoffset=xoffset, yoffset=yoffset, title=etitle, file=evt_file[j], error=err, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			total_processed=processed, processed=count1, valid=good, total_bad_xy=sbad_xy
       
		if err then goto, next
		if (n eq 0) or (good eq 0) then goto, cont
		bad_xy = bad_xy + sbad_xy

         if build_ylut and obj->ylut() and firsty then begin		 ; Maia Y lookup table
         	ylut[jy] = (jy eq 0) ? 0 : maia_y_min
         	firsty = 0
         endif

		if n_elements(veto) ne n then veto = uintarr(n)
		if do_pileup then begin
			pu = uint( (tot lt pileup_limit[0,e]) or (tot gt pileup_limit[1,e]))
			qp = where( pu eq 1, nqp)
			pileup_losses = pileup_losses + nqp
		endif else pu = uintarr(n)
		if do_throttle then begin
			multiple = temporary(multiple) * throttle_factor[e]
		endif

		if do_linear then begin
			if multilinear then begin
				e = uint(flinear[temporary(e),ste])			; linearize (individual tables)
			endif else begin									; dither not needed here (as in spec_evt)
				e = uint(flinear[temporary(e)])   			; linearize (single table)
			endelse
		endif
			 
;     Translate event energy to DA matrix column.

		if n_elements(cal_a) eq 1 then begin
			energy = float( cal_a * float(e) + cal_b)
		endif else begin
			energy = float(cal_a[ste] * float(e) + cal_b[ste])
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

		q1 = where( (veto eq 0) and (((x1 lt 0) or (x1 ge xrange3)) or $
									((y1 lt 0) or (y1 ge yrange3))), nq1)
		clipped = clipped + nq1
		if nq1 gt 0 then veto[q1] = 1
		q = where( veto eq 0, count2)

;		          		Events	Valid	Blocks	Bad XY	Size	Clipped/pileup
          case progress_file of
              0: begin
                 p.value = [processed,good,i,bad_xy,n,clipped]
				 r = 0.5
                 end
              1: begin
                 p.value = [processed,good,i,bad_xy,n,clipped]
                 p.current = j
                 p.file = evt_file[j]
				 r = float(j)/nj
                 end
              2: begin
                 p.value = [processed,good,i,bad_xy,n,clipped]
                 p.current = i
				 r = float(i)/(xrange*yrange)
                 end
              3: begin
                 p.value = [processed,good,i,bad_xy,n,clipped]
                 p.current = i
				 r = float(i)/progress_size
                 end
              else:
		endcase
		if do_progress then begin
		  	iprogress = iprogress + 1
			if iprogress ge nprogress then begin
				time = systime(/seconds)
				progress, /update, progress_tlb, p, cancel=cancel, skip=skip
				t = systime(/seconds)
				if skip then goto, finish
				if cancel then begin
					close, 1
					return
				endif
				ft = t - last_time
				dt = t - time
				if dt gt 0.2*ft then begin
					nprogress = nprogress*2
					if cluster_debug gt 0 then gprint, output=cluster_debug,'cut_evt: Extend progress period to ',nprogress
				endif
				iprogress = 0L
				last_time = time
			endif
		endif
		if cluster then begin
			if worker_progress(r) eq 0 then begin
				close, 1
				return
			endif
		endif

       if count2 gt 0 then begin
         valid = valid + count2

;      Pass the event arrays (x2,y2,energy2) and the 'image' array
;      and the cuts 'cuts' to the DLL routine 'cut_accumulate3'.
;      This will accumulate the cuts into
;      'image' at coords 'x2,y2', for each element cut.

		min_x = min( [min_x, min(x1[q]) + xoffset3])
		min_y = min( [min_y, min(y1[q]) + yoffset3])
		max_x = max( [max_x, max(x1[q]) + xoffset3])
		max_y = max( [max_y, max(y1[q]) + yoffset3])

         err = cut_accumulate5( x1,y1,energy,pu,veto, n,count2, image, xrange3,yrange3, nnpu,nn, $
         			image_error,xrange3e,yrange3e, cut,type,dleft,dright, n_el, multiple=multiple, $
					stim_mean, image_count, image_error_count)
         if err ne 0 then begin
          gprint,level=2, output=cluster_debug,'cut_evt: error (',err,') return from cut_accumulate5'
          goto, finish
         endif
       endif
       if events gt 0 then if processed gt events then begin
         gprint,level=2, output=cluster_debug,'cut_evt: requested event count exceeded; stop.'
         goto, finish
       endif

cont:
       i = i+1
    endwhile

next:
    j = j+1
    first = 0
	gprint, level=1, output=cluster_debug,'da_evt: next file ...'
	if j lt nj then goto, loop_file

finish:
	gprint,level=2, output=cluster_debug,'cut_evt: finished.'
    if do_throttle then gprint,level=2, output=cluster_debug,'    Used THROTTLE file ',throttle
    if do_pileup then gprint,level=2, output=cluster_debug,'  Used PILEUP file ',pileup
    if do_linear then gprint,level=2, output=cluster_debug,'  Used LINEARIZE file ',linearize
    gprint,level=2, output=cluster_debug, ' processed = ', processed
    gprint,level=2, output=cluster_debug, ' valid events = ',valid
    gprint,level=2, output=cluster_debug, ' bad event triplets = ',bad_xy
    gprint,level=2, output=cluster_debug, ' clipped to image bounds, or not station 0 = ',clipped
    gprint,level=2, output=cluster_debug, ' pileup losses = ',pileup_losses
    gprint,level=2, output=cluster_debug, ' pileup fraction = ', float(pileup_losses) / processed
    gprint,level=2, output=cluster_debug, ' X range = ', min_x, max_x
    gprint,level=2, output=cluster_debug, ' Y range = ', min_y, max_y
    if n_elements(flux) gt 1 then gprint,level=2, output=cluster_debug,' found FLUX array'
    if n_elements(dead_fraction) gt 1 then gprint,level=2, output=cluster_debug,' found DEAD_FRACTION array'
    gprint,level=2, output=cluster_debug,' Hist=',hist

    if n_elements(p) gt 0 then begin
       if do_progress then begin
         p.value = [processed,valid,i,bad_xy,n,(do_pileup ? pileup_losses: clipped)]
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
	if cluster then begin
		if worker_progress(1.) eq 0 then return
	endif

	if obj->ylut() then obj->write_ylut, ylut, file[0], output=output
	
;--------------------------------------------------------------------------
;
;   Write the image results file ...

null_image = define(/image)
img = null_image

gprint,level=2, output=cluster_debug,'cut_evt: write image file - ',output

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
	image = fltarr(xrange3,yrange3,2*n_el)
	image_error = fltarr(xrange3e,yrange3e,2*n_el)
	image[*,*,0:n_el-1] = timage
	image[*,*,n_el:2*n_el-1] = image_count
	image_error[*,*,0:n_el-1] = timage_error
	image_error[*,*,n_el:2*n_el-1] = image_error_count
	n_el = 2*n_el
	el = [el, 'n('+strtrim(el,2)+')']
endif

; For detector arrays, need to scale down result, depending on the number of
; detectors actually used to sort the EVT.

; Just leave in total detector counts units for Cuts ...
;xd = 1./float(n_det)
;xd2 = xd*xd
;image = image * xd
;image_error = image_error * xd2

xstep_on = 0L
xstep_count = 0L
step_events = 0L
step_toggle = 0L
toggle_bit = 0L
step_station = 0L
;units = ['keV','MeV','MeV']

img.source = evt_file[0]
img.source2 = evt_file[nj-1]
img.throttle = throttle
img.pileup = pileup
img.linearize = linearize
img.DevObj = obj
img.sample = sample
img.grain = grain
if lenchr(comment) gt 0 then begin
    img.comment = comment
endif else if n_elements(etitle) gt 0 then begin
    img.comment = etitle
endif
img.facility = facility
img.endstation = endstation

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
    old_charge = charge
    charge = lmf_charge
    dead = 0.0
    if n_elements(group) ge 1 then charge = OM_charge_select(group,charge, live=lmf_live, dead=dead)
    live = 1.0-dead
    if charge lt 1.e-10 then charge=old_charge
    img.charge = charge
    scanx = float(lmf_size[0])
    scany = float(lmf_size[1])
    img.scan.x = lmf_size[0]*0.001
    img.scan.y = lmf_size[1]*0.001
endif else begin
    img.scan.x = scanx*0.001
    img.scan.y = scany*0.001
endelse
img.scan.origin.x = xorigin
img.scan.origin.y = yorigin

if n_elements(x_coords) gt 0 then img.px_coords = ptr_new(x_coords)
if n_elements(y_coords) gt 0 then img.py_coords = ptr_new(y_coords)
img.x_coord_units = x_coord_units
img.y_coord_units = y_coord_units
 
img.processed = processed
img.valid = valid
img.bad_xy = bad_xy
img.clipped = clipped

img.matrix.label = 'Cuts'
img.matrix.file = all ? '' : (*pcuts)[0].file
img.matrix.charge = 1.0
img.matrix.mdl = ptr_new(fltarr(n_el))

img.n_el = n_el
img.el = ptr_new(el)

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

; If this is a cluster node image, then it will probably contain just an image stripe.
; Both 'image' and 'flux' need to be rebuilt when node parts are recombined.
; The full image size is still contained in 'original_xsize', 'original_ysize'.
; This is done in 'evt_start_image_increment'.

img.xstep_on = xstep_on
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

; Repair flux array for glitches and missing values, etc.
; Note: this assumes scan order (x=0). This should be moved into device dependent
; method later.
; Later will need to do dwell per pixel correction AFTER this repair, as it will
; add a little jitter per pixel that the repair might try to remove.

; If 'flux' has been multiply counted, use 'pass-count' method to scale back.

if n_elements(flux) gt 1 then begin
	flux = flux / float( obj->pass_count())
	raw_flux = flux[*,*,0]
	total_flux = total(flux[*,*,0])
;	if total_flux gt 0.001 then begin
;		flux[*,*,0] = flux_repair( raw_flux, X=0, /median)
;	endif
endif

;	For some devices 'dead_fraction' is counts weighted and will need 
;	to be normalized to a 'weight' and not dwell.

weight = obj->get_dead_weight(error=err_weight)
mode = -1

if (err_weight eq 0) and (n_elements(weight) gt 2) then begin

;	Check weight mode:
;		0	weight is incoming count estimate, or sum dwell
;		1	weight is outgoing raw count weight
;		2	Dead time is not implemented (yet) or disabled

	mode = obj->get_dead_weight_mode()
	if mode eq 2 then begin
		dead_fraction[*] = 0.0
	endif else begin
		q1 = where( (weight gt 0.), nq1)
		if nq1 gt 0 then begin
			gprint,level=2, output=cluster_debug,'cut_evt: normalize dead-fraction to weight'
	
			dead_fraction[q1] = dead_fraction[q1] / weight[q1]
			if mode eq 1 then begin
				gprint,level=2, output=cluster_debug,'cut_evt: Correct from OCR to ICR weights (mode=1).'
				dead_fraction[q1] = dead_fraction[q1] / (1 + dead_fraction[q1])
			endif
		endif
	endelse
endif

;	Retrieve a dwell map from the device if it exists.
;	If it exists, that means returned 'dead_fraction' is actually dead-time.
;	Dwell will be used to normalize the dead_fraction and count-rates.

dwell = obj->get_dwell(error=err_dwell)

if (err_dwell eq 0) and (n_elements(dwell) gt 2) then begin
	q = where( (dwell gt 0.), nq)
	if nq gt 0 then begin
	gprint,level=2, output=cluster_debug,'cut_evt: Dwell-time, ave: = ', mean(dwell)

		if mode le -1 then begin
			dead_fraction[q] = dead_fraction[q] / dwell[q]
		endif
		gprint,level=2, output=cluster_debug,'cut_evt: dead-fraction, ave: = ', mean(dead_fraction)

		count_rate_map[q] = 1000. * float(nn[q]) / dwell[q]
		gprint,level=2, output=cluster_debug,'cut_evt: count-rate, ave: = ', mean(count_rate_map)
		img.has_rates = 1
		img.count_rate_map = ptr_new( count_rate_map, /no_copy)

		img.has_dwell = 1
		img.dwell_map = ptr_new( dwell, /no_copy)	
	endif else begin
		gprint,level=2, output=cluster_debug,'cut_evt: Dwell-time, zero !'
	endelse
endif

; Test dead_fraction for magnitude.

if n_elements(dead_fraction) gt 1 then begin
	if n_elements(flux[*,*,0]) eq n_elements(dead_fraction) then begin
		tdead = total(dead_fraction)/n_elements(dead_fraction)
    	if tdead gt 0.001 then begin
			q = where(dead_fraction gt 0.95, nq)
			if nq gt 0 then begin
				gprint,level=2, output=cluster_debug,'cut_evt','Extreme dead-time fraction for '+str_tidy(nq)+' pixels. Ignore these.'
				dead_fraction[q] = 0.95
			endif
		endif
	endif
endif

; Combine PU losses map with dead-time map.

DT = dead_fraction
if do_pileup and (n_elements(dead_fraction) gt 1) then begin
	if (n_elements(flux[*,*,0]) eq n_elements(pileup_loss_map)) then begin
		q = where( nn gt 0, nq)
		if nq gt 0 then begin
			pileup_loss_map[q] = float(nnpu[q]) / float(nn[q])
			q = where(pileup_loss_map gt 0.95, nq)
			if nq gt 0 then begin
				gprint,level=2, output=cluster_debug,'cut_evt','Extreme pileup-loss fraction for '+str_tidy(nq)+' pixels. Clip these to 0.9.'
				pileup_loss_map[q] = 0.95
			endif
		endif	
	   	gprint,level=2, output=cluster_debug,'cut_evt: pile-up losses, ave: = ', mean(pileup_loss_map)

		if (n_elements(dead_fraction) gt 1) then begin
			gprint,level=2, output=cluster_debug,'cut_evt: dead-time losses, ave: = ', mean(dead_fraction)
			if (n_elements(flux[*,*,0]) eq n_elements(dead_fraction)) then begin
				dead_fraction = 1. - (1. - dead_fraction)*(1. - pileup_loss_map)
			endif
		endif
		img.has_pileup = 1
		img.pileup_map = ptr_new( pileup_loss_map, /no_copy)
	endif
endif
 
; Correct flux for dead-time variation. This is only needed if the charge used in the sort
; is not already 'live charge' corrected, and the flux is not corrected for DT losses
; already. Only set dead_fraction in device object if flux needs DT correction.
; Note: We don't correct extra planes of 'flux' as these do not have this dead-time.
; But we do normalize these to raw_flux.

if n_elements(dead_fraction) gt 1 then begin
	if n_elements(flux[*,*,0]) eq n_elements(dead_fraction) then begin
		tdead = total(dead_fraction)/n_elements(dead_fraction)
    	if tdead gt 0.00001 then begin
			gprint,level=2, output=cluster_debug,'cut_evt: correct flux array for DT+PU losses, ave: = ', mean(dead_fraction)

	    	flux[*,*,0] = flux[*,*,0] * (1. - dead_fraction)

			img.has_dead = 1
			img.dead_fraction = ptr_new( DT, /no_copy)
		endif
	endif
endif

img.energy = beam_energy

; Add extra atribute planes to image[] array and element list ...

gprint,level=2, output=cluster_debug,'cut_evt: n_el, n_attributes =', img.n_el, n_attributes
if do_attributes then begin
	image[*,*,img.n_el:img.n_el+n_attributes-1] = flux[*,*,1:n_attributes]
	if img.has_errors then begin
		error[*,*,img.n_el:img.n_el+n_attributes-1] = smart_congrid( sqrt( flux[*,*,1:n_attributes]), xrange3e,yrange3e,n_attributes)
	endif
	img.n_attributes = n_attributes
	img.el = ptr_new( [ (*img.el)[0:img.n_el-1], attributes ])
	img.n_el = img.n_el + n_attributes
endif

img.has_errors = 1
img.image = ptr_new( image, /no_copy)
img.error = ptr_new( image_error, /no_copy)

img.file = output
pimg = ptr_new(img, /no_copy)

; Correct for flux/current variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, so the correction here must
; preserve the average.

if n_elements(flux) gt 1 then begin
	aps_count_to_charge = flux_ic.conversion
	gprint,level=2, output=cluster_debug,'cut_evt: Correct images for flux, if /flatten set.'

	image_correct_flux, pimg, flux, flux_IC, pv_list, charge=charge, flatten=flatten, $
							raw_flux = raw_flux, random_subset=random_subset, accept_fraction=accept_fraction
endif

gprint,level=2, output=cluster_debug,'cut_evt: Write images ...'
write_geopixe_image, pimg, output, /no_display, cluster=cluster
gprint,level=2, output=cluster_debug,'cut_evt: Image write completed, update image display ...'

if arg_present(images) eq 0 then begin
    free_images, pimg
endif else begin
    images = pimg
endelse
cluster_result = output

cleanup:
	gprint,level=2, output=cluster_debug, 'CUT_EVT: finish time = ',systime()
	if do_progress and (cluster eq 0) then progress, /ending, progress_tlb
	return

bad_cuts:
	warning, output=cluster_debug, 'cut_evt', 'Bad CUTs data.'
    goto, cleanup

bad_file:
	warning, output=cluster_debug, 'cut_evt', 'EVT file not found.'
    goto, cleanup

bad_obj:
	warning, output=cluster_debug,'cut_evt', 'Bad device object for: '+device
    goto, cleanup

bad_ylut_ymode:
    warning, output=cluster_debug, 'cut_evt', ['Cannot use "cluster" mode first with "Correct Y Encoder" mode.', $
    				'Do a normal (non-cluster) sort first to build a valid YLUT file.']
    goto, cleanup

bad_ylut_build:
    warning, output=cluster_debug, 'cut_evt', 'Error building Y Lookup Table.'
    goto, cleanup

bad_ylut_get:
    warning, output=cluster_debug, 'cut_evt', 'Error reading Y Lookup Table.'
    goto, cleanup

bad_all:
    warning, output=cluster_debug, 'cut_evt', 'All mode "0" not known.'
    goto, cleanup

bad_cluster:
    warning, output=cluster_debug, 'cut_evt', 'No EVT files for this node.'
    goto, cleanup
end
