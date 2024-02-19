pro spec_evt, filei, files=files, mask=pmaski, station=channeli, output=outputi, spectra=spectra, $
          progress=do_progress, events=events, device=devicei, $
          group=group, xrange=xrangei, yrange=yrangei, do_tot=do_tot, $
          pileup=pileup, by_detector=by_detector, linearize=linearize, $
          by_odd=by_odd, by_even=by_even, suppress=suppress, $
          ic=flux_ic, pv_list=pv_list, throttle=throttle_file, devpars=devpars, $
          cluster_index=cluster_index, cluster_total=cluster_total, $
          cluster_result=cluster_result, cluster_debug=cluster_debug, $
          mfile=mask_file, detector_select=by_detector_select, $
		  start_mask=start_mask, stop_mask=stop_mask

;   Read a list-mode 'file' of type 'device'.
;   Write total spectra out as a .spec file.
;
;   filei		list-mode file(s) to process (or specify using "files=" keyword).
;   pmask		points to a pointer array, each of which points to a struct
;				containing a pointer to a 'q' mask array to select regions.
;				Return spectra for these regions, else spectra for each detector.
;				Also contains information about scan (channels, XY size, device, compress, ...)
;				Always pass full pmask array (use start_mask, stop_mask to select a range)
;	mask_file	File-name of regions file to collect pmask from (as alterntive to 'pmask'
;				when using cluster processing to save memory)
;
;	start_mask	first region to process (use these to limit number of masks,)
;	stop_mask	last region to process	(and the memory needed for them ...)
;
;	channel		vector to select detector channels, -1 for All. Using 'pmask', it gets this info
;				from the image 'pactive' array, and remaps energy calibrations.
;   events		stop at this many events, if set.
;   device		list-mode device object name
;   devpars		device dependent options parameter struct
;   pileup		optional file of pileup limits (for device 16?), for
;   			use when in full spectra mode, or override in mask mode.
;	linearize	give the name of a linearization function file, or override in mask mode.
;   throttle 	gives name of throttle factors file, or override in mask mode.
;	group		group_leader for progress and pop-up windows.
;   /do_tot		add the time-over-threshold data to another spectrum
;	/by_detector will use one mask region only, but produce
;				a spectrum for every detector in an array.
;		detector_select=n use region 'n'
;		/by_odd		only do /by_detector for odd rows
;		/by_even	only do /by_detector for even rows
;	/do_progress put up a progress bar.
;	
;   output		file name for output, or create one.
;   spectra		return spectra pointer here, or just write file.

COMPILE_OPT STRICTARR
common c_spec_last, last
common c_om_2, lmf_cal
common c_om_3b, c_per_pulse, lmf_charge
common c_om_5, lmf_live
common aps_4, aps_count_to_charge
common c_nsls_9, nsls_debug
common c_seed, seed
common c_geopixe_adcs, geopixe_max_adcs
common c_debug_warnings, enable_warning_popup
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=32
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=0.
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
if n_elements(seed) lt 1 then seed=1L

if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(suppress) lt 1 then suppress=0
if n_elements(cluster_total) lt 1 then cluster_total=0
if n_elements(cluster_index) lt 1 then cluster_index=0
if n_elements(cluster_debug) lt 1 then cluster_debug=-1		; stdout

;cluster_total = 10								; to debug cluster mode

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
gprint,level=2, output=cluster_debug, 'SPEC_EVT: start time = ',systime()

toc, lun=cluster_debug					; assume that a "tic" has been done previously (will use unit declared with tic)

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
       		worker_error, stringify( {command:'SPEC_EVT', error:s2 })
       		gprint, level=2, output=cluster_debug, 'SPEC_EVT', s2 
       endif else begin
	       warning,'SPEC_EVT', s2, /error
       endelse
       MESSAGE, /RESET
       return
    endif
endif

spectra_present = arg_present(spectra)
spectra = ptr_new(0)

define_devices
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei

if n_elements(group) lt 1 then group=0L
if n_elements(files) gt 0 then filei=files
if n_elements(filei) lt 1 then filei=''
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then goto, bad_file
last = file[0]

if n_elements(events) lt 1 then events=0L
if n_elements(xrangei) lt 1 then xrangei=4096
if n_elements(yrangei) lt 1 then yrangei=4096
if n_elements(do_tot) lt 1 then do_tot=0
if n_elements(pileup) lt 1 then pileup=''
if n_elements(linearize) lt 1 then linearize=''
if n_elements(throttle_file) lt 1 then throttle_file = ''
if n_elements(by_detector) lt 1 then by_detector=0
if n_elements(by_detector_select) lt 1 then by_detector_select=0
if n_elements(by_odd) lt 1 then by_odd=0
if n_elements(by_even) lt 1 then by_even=0
if by_odd then by_even=0

if n_elements(flux_ic) lt 1 then flux_ic = {mode:0, pv:'', val:0.0, unit:0.0, conversion:1., use_dwell:0, dwell:1.0}
if n_elements(pv_list) lt 1 then pv_list=['']

free_mask = 0
if n_elements(pmaski) lt 1 then begin				; do we have a mask 'mfile' instead?
	if n_elements(mask_file) eq 1 then begin
		p = read_regions( mask_file)
		if ptr_valid( p[0]) eq 0 then goto, bad_region
		pmaski = ptr_new( p, /no_copy)
		free_mask = 1
	endif
endif

; A master mask layer is used routinely now, for all cases, and is not optional
; (indeed, the master mask is needed for the new 'spec_accumulate4')

if n_elements(pmaski) lt 1 then begin				; normal full spectra extraction
    if n_elements(outputi) lt 1 then begin
       outputi = strip_file_ext( file[0]) + 'i.spec'
    endif
endif else begin									; spectra from region masks
    if n_elements(outputi) lt 1 then begin
    	if by_detector then begin
			outputi = strip_file_ext( file[0]) + '-q1-detector.spec'
		endif else begin
			outputi = strip_file_ext( file[0]) + '-q1.spec'
		endelse
    endif
endelse
output = outputi
if extract_path(output) eq output then begin		; 'output' is a path
	if n_elements(pmaski) lt 1 then begin			; normal full spectra extraction
		output = output + strip_path( strip_file_ext(file[0])) + 'i.spec'
	endif else begin								; spectra from region masks
		output = output + strip_path( strip_file_ext(file[0])) + '.spec'
	endelse
endif

evt_file = file
obj = new_device_object( device, options=devpars, error=err)
if err then goto, bad_obj

if cluster then begin
;	q1 = indgen(cluster_total)
;	q1 = congrid( q1, n_elements(file), /center)
;	q2 = where(q1 eq cluster_index, nq)
	q2 = obj->cluster_files( file, cluster_total, cluster_index, nq=nq2)

	if nq2 lt 1 then goto, bad_cluster
	evt_file = file[q2]

	gprint,level=2, output=cluster_debug, 'Cluster #',cluster_index,' files=', evt_file
	output = output + '.' + str_tidy(cluster_index)
endif
gprint,level=2, output=cluster_debug, 'Output="',output,'"'

if n_elements(pmaski) lt 1 then begin		; normal full spectra extraction
    use_mask = 0
    nqmask = 1
    show_back = 0
    ecompress = 1L
    charge = 0.0
    if n_elements(channeli) lt 1 then channeli=-1
    channel = channeli
    xoffset = 0L
    yoffset = 0L

    do_linear = 0
    do_pileup = 0
    do_throttle = 0
	if obj->pileup() then begin
	    pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
		if do_pileup then gprint,level=2, output=cluster_debug, 'Pileup file =',pileup
	endif
	if obj->linear() then begin
		flinear = get_linearize(linearize, do_linear=do_linear, multi=multilinear, max=8191)
		flinx = findgen( n_elements(flinear))
		if do_linear then gprint,level=2, output=cluster_debug, 'Linearize file =',linearize
	endif
	if obj->throttle() then begin
		throttle_factor = get_throttle(throttle_file, do_throttle=do_throttle)
		if do_throttle then gprint,level=2, output=cluster_debug, 'Throttle file =',throttle_file
	endif
    
endif else begin							; spectra from region masks
;	pointer_display, pmaski, unit=cluster_debug
	pmask = pmaski
	if by_detector then begin
		if by_detector_select ge n_elements(*pmaski) then begin
	       gprint, output=cluster_debug, 'spec_evt: "by_detector_select" used, too large index.'
	       return
		endif
		pmask = ptr_new( (*pmaski)[by_detector_select])
	endif
    use_mask = 1
    qmask = where( ptr_valid( *pmask ) eq 1, nqmask)     ; only valid pointers
    if qmask[0] eq -1 then begin
       gprint, output=cluster_debug, 'spec_evt: mask used, but all pointers bad.'
       return
    endif

	if (n_elements(start_mask) eq 1) or (n_elements(stop_mask) eq 1) then begin
		if n_elements(start_mask) eq 0 then start_mask=0
		if n_elements(stop_mask) eq 0 then stop_mask=max(qmask)

		q = where( (qmask ge start_mask) and (qmask le stop_mask), nqmask)
	    if nqmask eq 0 then begin
	       gprint, output=cluster_debug, 'spec_evt: mask used, but all outside start/stop index range.'
	       return
	    endif
		qmask = qmask[q]
	endif

    pq = ptrarr(nqmask)
    for i=0L,nqmask-1 do begin
       pq[i] = (*(*pmask)[qmask[i]]).q          ; an array of pointers to 'q' arrays
	   if ptr_good(pq[i]) eq 0 then pq[i] = ptr_new(-1)
    endfor
    p = (*pmask)[qmask[0]]						; first region
    if (*p).xstep_on eq 1 then begin
       gprint, output=cluster_debug,'spec_evt: mask used, but xstep_on=1.'
       return
    endif

	flux_ic.mode = (*p).IC.mode
	flux_ic.pv = (*p).IC.pv.name
	flux_ic.val = (*p).IC.pv.val
	flux_ic.unit = (*p).IC.pv.unit
	flux_ic.conversion = (*p).IC.conversion
	
    remap = 0
    if n_elements(channeli) lt 1 then begin
       channel = (*p).channel
    endif else begin									; use channeli = -1 to select all valid
       if channeli lt 0 then begin						; detectors in an array
       		if by_detector then begin
				channel = indgen(geopixe_max_adcs)
       		endif else begin
		         if (*p).array then begin
			          channel = *(*p).pactive
			          remap = 1
		         endif else begin
			          channel = (*p).channel
		         endelse
			endelse
       endif else begin
    	     channel = channeli
       endelse
    endelse
	nmax = max([channel,geopixe_max_adcs-1])
	
    ecompress = (*p).ecompress					; *p is the first region pointer
    nx = (*p).nx								; was (*p).xsize (compressed xrange3) in image used to create region
    ny = (*p).ny								;	  (*p).ysize (compressed yrange3)
	xoffset = (*p).xoffset						; was (*p).xoffset (not compressed) in image
	yoffset = (*p).yoffset						;	  (*p).yoffset (not compressed) 
    xcompress = fix( (*p).xcompress > 1)
    ycompress = fix( (*p).ycompress > 1)
	xrange = (*p).original_xsize * xcompress	; scale size after compress back to full scan size
	yrange = (*p).original_ysize * ycompress	; needed for Flip axis in device
    events = (*p).events

;	In cluster mode, only build a stripe image for each node, based on the range of Y seen in
;	the evt_file[] array. Note that the events are offset and compressed in read_buffer.
;	N.B. range method returns a very large 'max' for the last file, so clip to (yrange-1).
;	But we may need an extra offset (dyoffset3) if cluster stripe Y offset is larger than 
;	original yoffset.

	if cluster and obj->ylut() then begin
		ylut = obj->get_ylut( evt_file[0], output=output, /strip, error=err)
		if err then goto, bad_ylut_get								; 'get_ylut' needed to populate YLUT
																	; for 'range_ylut' below
		range = obj->range_ylut( evt_file, error=err)
		if err eq 0 then begin
			yoffset3 = ((yoffset > range.min) > 0) < (yrange-1)
			dyoffset3 = (yoffset3 - yoffset) > 0					; extra Y offset, uncompressed
			yoffset3 = yoffset3 / ycompress
			dyoffset3 = dyoffset3 / ycompress						; extra Y offset, compressed

			yrange3 = (range.max > 1) < (yrange-1)
			yrange3 = yrange3 / ycompress
			yrange3 = ((yrange3 - yoffset3 + 1) > 1)	
			yrange3 = long( yrange3  < ny )
	
			gprint,level=2, output=cluster_debug,'Y offset, Y range3 set=',yoffset, yrange3	
		endif else begin
			yrange3 = ny
			yoffset3 = yoffset / ycompress
			dyoffset3 = 0L		

			gprint,level=2, output=cluster_debug,'EVT_file ='+evt_file[0]
			gprint,level=2, output=cluster_debug,'Failed to find valid YLUT to reduce Y image range.'
		endelse
	endif else begin
		yrange3 = ny
		yoffset3 = yoffset / ycompress
		dyoffset3 = 0L		
	endelse
	gprint,level=2, output=cluster_debug, 'Y offset, Y range3, dYoffset3 set=',yoffset, yrange3, dyoffset3

    do_linear = 0
    do_pileup = 0
    do_throttle = 0
	if obj->pileup() then begin
		if pileup eq '' then pileup=(*p).pileup
		gprint,level=2, output=cluster_debug, 'Test Pileup file =',pileup
		pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
		if do_pileup then begin
			gprint,level=2, output=cluster_debug, 'Use Pileup file =',pileup
			gprint,level=2, output=cluster_debug, '  pileup table [220:240] =',pileup_limit[220:240]
		endif
	endif
	if obj->linear() then begin
		if linearize eq '' then linearize=(*p).linearize
		gprint,level=2, output=cluster_debug, 'Test Linearize file =',linearize
		flinear = get_linearize( linearize, do_linear=do_linear, multi=multilinear, max=8191)
		flinx = findgen( n_elements(flinear))
		if do_linear then gprint,level=2, output=cluster_debug, 'Use Linearize file =',linearize
	endif
	if obj->throttle() then begin
		if throttle_file eq '' then throttle_file=(*p).throttle
		gprint,level=2, output=cluster_debug, 'Test Throttle file =',throttle_file
		throttle_factor = get_throttle(throttle_file, do_throttle=do_throttle)
		if do_throttle then gprint,level=2, output=cluster_debug, 'Use Throttle file =',throttle_file
	endif
	toc, lun=cluster_debug

    if remap then begin                  			; build lookup table to remap energy
       c = indgen(8192)								; this assumes channel & pcal in pactive order
       c0 = fltarr(8192,geopixe_max_adcs)
       for n=0L,n_elements(channel)-1 do begin
         e = (*(*p).pcal)[n].poly[1] * c + (*(*p).pcal)[n].poly[0]
         x = (e - (*(*p).pcal)[0].poly[0]) / (*(*p).pcal)[0].poly[1]
         c0[*,channel[n]] = x
       endfor
    endif
	gprint,level=2, output=cluster_debug, 'Done remap'

	gprint,level=2, output=cluster_debug, 'nx,ny, dyoffset3,yrange3=', nx,ny, dyoffset3,yrange3

    npx = long(nx)*long(ny)
    nqx = long(nqmask)*long(nx)
    mask0 = bytarr(nx,ny)
    mask = bytarr(nqmask,nx,yrange3)							; new order, only for 'stripe'
;	if master_mask then begin
;		pqmast= (*(*pmaster)[0]).q 
		master = bytarr(nx,yrange3)								; master mask
;	endif
    all_bad = 1
    for i=0L,nqmask-1 do begin
    	if (*(pq[i]))[0] ne -1 then begin
			mask0[*] = 0
			t = *(pq[i])
			qt = where( t lt n_elements(mask0), nqt)			; clip q to within mask0
			if nqt gt 0 then begin
				mask0[ t[qt] ] = 1B
				mask[ i, *,0:(yrange3-1) < (ny-dyoffset3-1)] = mask0[ *, dyoffset3:(dyoffset3+yrange3-1) < (ny-1)]
;				if master_mask then begin
					master[*,0:(yrange3-1) < (ny-dyoffset3-1)] OR= mask0[ *, dyoffset3:(dyoffset3+yrange3-1) < (ny-1)]
;				endif
			endif
			all_bad = 0
		endif
    endfor
	mask0 = 0
	if all_bad then goto, bad_mask
	gprint,level=2, output=cluster_debug, 'Mask test OK'
	
    original_xsize = (*p).original_xsize
    original_ysize = (*p).original_ysize
;	if (original_xsize ne 0) and (original_ysize ne 0) and $
;				((original_xsize ne nx) or (original_ysize ne ny)) then begin
;		mask = congrid( mask, original_xsize, original_ysize, nqmask)
;		nx = original_xsize
;		ny = original_ysize
;	endif

    if (((*p).scaled_x ne 1.0) and ((*p).scaled_x gt 0.001)) or $
			(((*p).scaled_y ne 1.0) and ((*p).scaled_y gt 0.001)) then begin
       if (*p).scaled_x lt 0.001 then (*p).scaled_x=1.0
       if (*p).scaled_y lt 0.001 then (*p).scaled_y=1.0

       nx = nx/(*p).scaled_x
       ny = ny/(*p).scaled_y
       mask = congrid( mask, nqmask, nx, ny)
    endif
    show_back = (*p).show_back
    gprint, output=cluster_debug,'spec_evt: mask - nqmask=',nqmask,', nx,ny=',nx,ny,', compress x,y=',xcompress,ycompress
endelse

null_spectrum = define(/spectrum)

min_e = 50000L
min_x = 50000L
min_y = 50000L
min_t = 50000L
max_e = 0L
max_x = 0L
max_y = 0L
max_t = 0L
processed = 0LL
valid = 0LL
bad_xy = 0LL
bad_e = 0LL
clipped = 0LL
pileup_losses = 0LL

if use_mask then begin
	n_spectra_channels = 16384					; increased from 4k for FalconX
    found = lon64arr(max([nqmask,geopixe_max_adcs]))
    spece = lonarr(n_spectra_channels,(by_detector ? geopixe_max_adcs: nqmask))
    xrange2 = nx
    yrange2 = ny
;   flux = fltarr(xrange2,yrange2)				; *** new (22/4/10) *** to fix Maia borders
;   dead_fraction = fltarr(xrange2,yrange2)		; not used in spec mode
endif else begin
	n_spectra_channels = 16384					; must be this value for Maia
    xrange = xrangei							; (see test in device_specific)
    yrange = yrangei
    found = lon64arr(geopixe_max_adcs)
    spece = lonarr(n_spectra_channels,geopixe_max_adcs)
    specx = lonarr(n_spectra_channels,geopixe_max_adcs)
    specy = lonarr(n_spectra_channels,geopixe_max_adcs)
    if do_tot then spect = lonarr(n_spectra_channels,geopixe_max_adcs)
endelse
flux = 0.0
dead_fraction = fltarr(geopixe_max_adcs)
pileup_loss_det = fltarr(geopixe_max_adcs)
nnpu64 = lon64arr(geopixe_max_adcs)
nn64 = lon64arr(geopixe_max_adcs)
nnpu = lonarr(geopixe_max_adcs)
nn = lonarr(geopixe_max_adcs)

sfound = lonarr(n_elements(found))
cancel = 0
etitle = ''
tot = 0L
ntest1 = 0LL
ntest2 = 0LL
support_even_odd = 0
report_even_odd = 1

j = 0L
nj = n_elements(evt_file)
ndj = (nj/20) > 1
njc = ndj
first = 1
last_time = systime(/seconds)

;-----------------------------------------------------------------------------------------

loop_file:
	njc += 1
	if njc ge ndj then begin
		gprint,level=2, output=cluster_debug, 'File loop, evt_file[j] = ', evt_file[j]
		toc, lun=cluster_debug
		njc = 0L
	endif
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
	if dfile(1, 5481, dt=dt) ne -3175 then begin
		warning, output=cluster_debug,'spec_evt',['Platform ERROR 71','Please consult CSIRO.','Delta = '+string(dt)], /error
;		probe_time, 1
		exit, /no_confirm
	endif
    on_ioerror, next		
	
    device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, first=first, $
         charge=charge, flux=flux, dead_fraction=dead_fraction, ecompress=ecompress, $
         suppress=suppress, ic=flux_ic, error=err, beam_energy=beam_energy
    if err then goto, finish
	if first then nprogress = ((100000L / n_guide) > 1L) < 500L
    first = 0

	if j eq 0 then begin
		case progress_file of
			0: begin
				pr = { unit:1, value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			1: begin
				pr = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			2: begin
				pr = { unit:0, current:0L, size:long(xrange)*yrange, file:evt_file[0], value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			3: begin
				pr = { unit:0, current:0L, size:progress_size, file:evt_file[0], value:[0LL,0LL,0LL,0L,0LL,0LL,n_guide,0LL,0L]}
				end
			else:
		endcase
		if do_progress then begin
			progress, tlb=progress_tlb, title='Sort XY EVT file', $           ; put up a progress bar
				pars=['Events','Found 1','Found 4','Blocks','Found 2','Bad XY','Size','Found 3','']

			iprogress = 0L
		endif
    endif
	if cluster then begin
		if worker_progress(0.0) eq 0 then begin
			close, 1
			return
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

		read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress, $
			station_e=ste, time=tot, veto=veto, ecompress=ecompress, multiple=multiple, $
			xoffset=xoffset, yoffset=yoffset, title=title, file=evt_file[j], error=err, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			total_processed=processed, processed=count1, valid=good, total_bad_xy=sbad_xy, raw_xy=1-use_mask, $
			by_odd=by_odd, by_even=by_even, support_even_odd=support_even_odd
		
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

		case progress_file of
              0: begin
                 pr.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
				 r = 0.5
                 end
              1: begin
                 pr.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
                 pr.current = j
                 pr.file = evt_file[j]
				 r = float(j)/nj
                 end
              2: begin
                 pr.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
                 pr.current = i
				 r = float(i)/(xrange*yrange)
                 end
              3: begin
                 pr.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
                 pr.current = i
				 r = float(i)/progress_size
                 end
              else:
		endcase
		if do_progress then begin
		  	iprogress = iprogress + 1
			if iprogress ge nprogress then begin
				time = systime(/seconds)
				progress, /update, progress_tlb, pr, cancel=cancel, skip=skip
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
					gprint, output=cluster_debug,'spec_evt: Extend progress period to ',nprogress
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
       
		q = where( veto eq 0, good)
		valid = valid + good
	   
    	if use_mask then begin            ; use masks

;			Note that these dithers will not work for formats that use "Multiple"

			if do_linear then begin
			 	R = randomu( seed, n)-0.501      							; +- 0.5 random dither
				if multilinear then begin
					e = uint(round(flinear[temporary(e),ste]+R) > 0)		; linearize (individual tables)
				endif else begin
;				 	e = uint(round(flinear[temporary(e)+R]) > 0)  			; linearize (single table)
					e = uint(round(interpol( flinear, flinx, temporary(e)+R)))
				endelse
			endif
			 
			if remap then begin
				R = randomu( seed, n)-0.5       	    					; +- 0.5 random dither
				e = uint(round(c0[temporary(e),ste]+R) > 0)  		 		; remap to cal for first detector
			endif

;			Why do we have this code, for devices that don't support even_odd selection (e.g. Maia)?
;			The device should just get this code added to it?

			if support_even_odd eq 0 then begin
				if report_even_odd and (by_odd or by_even) and (ycompress ne 1) then begin
					warning,'spec_evt',['"Even" or "Odd" row selection does not work', $
								'with Y compression for this event data device.', $
								'Use a Y compression of 1 for this device.']
					report_even_odd = 0
				endif
				if by_odd or by_even then begin									; only include odd or even lines
					yodd = by_odd ? 0 : 1
					q1 = where( (2*(y1/2) eq y1-yodd) and (veto eq 0), nq1)		; only makes sense for no ycompress?
					if nq1 gt 0 then veto[q1] = 1
					q = where( veto eq 0, good)
					if good eq 0 then goto, cont
				endif
			endif

             min_e = min( [min_e, min(e / ecompress)])
             min_x = min( [min_x, min(x1)])										; do these need some offset correction?
             min_y = min( [min_y, min(y1)])										; (see da_evt)
             min_t = min( [min_t, min(tot)])
             max_e = max( [max_e, max(e / ecompress)])
             max_x = max( [max_x, max(x1)])
             max_y = max( [max_y, max(y1)])
			 max_t = max( [max_t, max(tot)])

             sfound[*] = 0
             if by_detector then begin
	             err = spec_det_accumulate4( e,x1,y1,ste,pu,veto,n, spece,n_spectra_channels,geopixe_max_adcs,sfound, $
				 			nnpu,nn, mask,nqmask,nx,yrange3,dyoffset3, multiple=multiple )

	             nnpu64[*] = nnpu64 + nnpu
	             nn64[*] = nn64 + nn
				 nnpu[*] = 0
				 nn[*] = 0
			 endif else begin
	             err = spec_accumulate4( e,x1,y1,pu,veto,n, spece,n_spectra_channels,sfound, mask,nqmask,nx,yrange3,dyoffset3, $
                        multiple=multiple, master=master )
			 endelse
             found = found + sfound

             if err ne 0 then begin
              gprint, output=cluster_debug,'spec_evt: error (',err,') return from one spec accumulate routine'
              goto, finish
             endif

         endif else begin                   ; total spectrum

;	         if (obj->name() eq 'MAIA_DEVICE') or (obj->name() eq 'MAIA_NMP_DEVICE') then begin    ; fudge to show Maia negative X,Y at top of spectrum
;				 q1 = where(x1 ge 16*1024)
;				 if q1[0] ne -1 then x1 = uint( (xrange + (long(x1) -32*1024L)) > 0)
;				 q1 = where(y1 ge 16*1024)
;				 if q1[0] ne -1 then y1 = uint( (yrange + (long(y1) -32*1024L)) > 0)
;	         endif

			 if do_linear then begin
				 R = randomu( seed, n)-0.501									; +- 0.5 random dither
				 if multilinear then begin
				 	e = uint(round(flinear[temporary(e),ste]+R) > 0)			; linearize (individual tables)
				 endif else begin
				 	e = uint(round(flinear[temporary(e)]+R) > 0) 				; linearize (single table)
				 endelse
			 endif
			 
             sfound[*] = 0
             err = hist_accumulate4( e,tot,x1,y1,ste,pu,veto,n, spece,specx,specy,n_spectra_channels,geopixe_max_adcs, $
             			sfound, nnpu, nn, multiple=multiple, spect=spect )
             found = found + sfound
             nnpu64[*] = nnpu64 + nnpu
             nn64[*] = nn64 + nn
			 nnpu[*] = 0
			 nn[*] = 0
			 
             if err ne 0 then begin
              gprint, output=cluster_debug,'spec_evt: error (',err,') return from hist_accumulate'
              goto, finish
             endif

             min_e = min( [min_e, min(e / ecompress)])
             min_x = min( [min_x, min(x1)])
             min_y = min( [min_y, min(y1)])
             min_t = min( [min_t, min(tot)])
             max_e = max( [max_e, max(e / ecompress)])
             max_x = max( [max_x, max(x1)])
             max_y = max( [max_y, max(y1)])
			 max_t = max( [max_t, max(tot)])
		endelse
       if events gt 0 then if processed gt events then begin
         gprint, output=cluster_debug,'spec_evt: requested event count exceeded; stop.'
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
    if do_throttle then gprint, output=cluster_debug,'    Used THROTTLE file ',throttle_file
    if do_pileup then gprint, output=cluster_debug,'  Used PILEUP file ',pileup
    if do_linear then gprint, output=cluster_debug,'  Used LINEARIZE file ',linearize
    gprint, level=2, output=cluster_debug, ' found(stn) = ',found
;    gprint, level=2, output=cluster_debug, ' ntest1 = ', ntest1, '   ',ttest1
;    gprint, level=2, output=cluster_debug, ' ntest2 = ', ntest2, '   ',ttest2
    gprint, level=2, output=cluster_debug, ' processed = ', processed
    gprint,level=2, output=cluster_debug, ' valid events = ',valid
    gprint, level=2, output=cluster_debug, ' bad XY = ',bad_xy
    gprint, level=2, output=cluster_debug, ' total pileup losses = ',pileup_losses
    gprint,level=2, output=cluster_debug, ' total pileup fraction = ', float(pileup_losses) / processed
    gprint, level=2, output=cluster_debug, ' total flux count = ',total(flux)
	gprint, level=2, output=cluster_debug, ' min e,x,y = ',min_e,min_x,min_y
	gprint, level=2, output=cluster_debug, ' max e,x,y = ',max_e,max_x,max_y
    if do_progress and (n_elements(pr) gt 0) then begin
       pr.value = [processed,found[0],found[3],i,found[1],bad_xy,n,found[2],0]
         case progress_file of
          1: begin
              pr.current = j
              end
          2: begin
              pr.current = i
              end
          else:
         endcase
		 progress, /update, progress_tlb, pr
	endif
	close, 1
	on_ioerror, null

t = 'EVT sorting complete. Save Spectra ...'
toc, lun=cluster_debug
if do_progress then begin
    progress, /complete, progress_tlb, t
endif
if cluster then begin
	if worker_progress(100.) eq 0 then return
endif
gprint, output=cluster_debug, 'Debug = ',nsls_debug

;	If 'flux' has been multiply counted, use 'pass-count' method to scale back.

	flux = flux / float( obj->pass_count())

;	Sort out dead-time and pileup corrections to flux, for "live" flux ...

if (use_mask eq 0) or by_detector then begin

    live = 1.0
    if (obj->name() eq 'OM_DAQ_DEVICE') then begin				;and (suppress eq 0) then begin
	   old_charge = charge
       charge = lmf_charge
       dead = 0.0
       if (n_elements(group) ge 1) then begin
       		charge = OM_charge_select(group, charge, live=lmf_live, dead=dead)
       endif
	   if charge lt 1.e-10 then charge=old_charge
       live = 1.0-dead
    endif

;	For some devices 'dead_fraction' is counts weighted and will need 
;	to be normalized to a 'weight' and not dwell.

	weight = obj->get_dead_weight(error=err_weight)
	mode = -1
	
	if (err_weight eq 0) then begin

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
			gprint,level=2, output=cluster_debug,'spec_evt: normalize dead-fraction to weight'
	
			dead_fraction[q1] = dead_fraction[q1] / weight[q1]
			if mode eq 1 then begin
				gprint,level=2, output=cluster_debug,'spec_evt: Correct from OCR to ICR weights (mode=1).'
				dead_fraction[q1] = dead_fraction[q1] / (1 + dead_fraction[q1])
			endif
		endif
	endelse
endif

; If dead_fraction has been accumulated as time, then need to normalize here ...

	t = obj->get_total_time()
	if (t gt 0.) and (mode le -1) then begin
		print,'spec_evt: Normalize "dead_fraction" by total time (ms) or norm factor =',t
		dead_fraction = (dead_fraction / t) > 0.
	endif
	
; For many synchrotron formats, we get flux initially (device_specific) or from
; each pixel record. Where dead_fraction is available, the flux needs to be corrected
; for dead-time losses, either in the device (and then dead_fraction should return zero)
; or here.

	DT = dead_fraction
	rel_dt_corr = DT
	rel_dt_corr[*] = 1.0
	qt = where(dead_fraction gt 0.95, nqt)
	if nqt gt 0 then begin
		gprint,level=2, output=cluster_debug,'spec_evt','Extreme dead-time fraction for '+str_tidy(nqt)+' detectors.'
		dead_fraction[qt] = 0.95
	endif

;	Combine PU losses with dead-time per detector. If do_pileup not enabled, then nnpu64 will be zero. If device
;	does not support 'dead_fraction' then they will be zero and ignored. If 'flux' is not zero, then synchrotron XRF
;	data will be assumed and a conversion 'charge' will be applied.

	qnz = where( nn64 gt 0, nqnz)			; active detector channels
	if nqnz gt 0 then begin
		pileup_loss_det[qnz] = float(nnpu64[qnz]) / float(nn64[qnz])
	endif
	if do_pileup then begin
		qt = where(pileup_loss_det gt 0.95, nqt)
		if nqt gt 0 then begin
			gprint,level=2, output=cluster_debug,'spec_evt','Extreme PU fraction for '+str_tidy(nqt)+' detectors.'
			pileup_loss_det[qt] = 0.95
		endif
	
	   	gprint,level=2, output=cluster_debug,'spec_evt: dead-time losses, ave: = ', mean(dead_fraction)
	   	gprint,level=2, output=cluster_debug,'spec_evt: pile-up losses, ave: = ', mean(pileup_loss_det)

		dead_fraction = 1. - (1. - dead_fraction)*(1. - pileup_loss_det)
	endif
 
;	For spectra files and fitting, we adopt a single average live charge/flux.
;	Hence, we pass on a rel_dt_corr vector to give the relative DT correction per detector.

 	if nqnz gt 0 then begin
		if mean(dead_fraction[qnz]) gt 0.00001 then begin
	    	gprint,level=2, output=cluster_debug,'spec_evt: correct flux for DT + PU losses, ave: = ', mean(dead_fraction[qnz])
			live = 1.0 - dead_fraction[qnz]
	
			rel_dt_corr[qnz] = 1./live
			rel_dt_corr[qnz] = rel_dt_corr[qnz] * mean(live)					;@9-21 (if we don't do this, worry about pixe_fit just saving live charge)
;			rel_dt_corr[qnz] = rel_dt_corr[qnz] / mean(rel_dt_corr[qnz])		;@9-21 (should not use mean(1/live) here and not below)

		    flux = flux * mean(live)											;@9-21 (if we don't do this, worry about pixe_fit just saving live charge)
		endif
    endif
	gprint,level=2, output=cluster_debug,'spec_evt: average "live" fraction = '+str_tidy(mean(live))
	gprint,level=2, output=cluster_debug,'spec_evt: average "live" flux = '+str_tidy(flux)

	if flux gt 1.0e-6 then begin
;		if (flux_ic.mode ne 0) then charge = flux * flux_ic.conversion
		charge = flux * ((flux_ic.mode eq 0) ? 1.0 : flux_ic.conversion)
		aps_count_to_charge = flux_ic.conversion
	endif
endif

;	Save spectra. Make distinctions between 3 cases:
;		2. Total spectra across all detectors, which needs DT,PU correction by detector.
;		1. Region spectra, all active detectors summed together, already with "live" flux corrected flux
;		3. Region #select spectra for all detectors separately, which needs DT,PU correction by detector.

if use_mask eq 0 then begin										; total spectra ----------------------------------
    q = where( found gt 0, n)
;    if (cluster eq 0) and (n eq 0) then begin
;       warning, output=cluster_debug,'spec_evt',['No data found in list-mode file.','Debug = '+string(nsls_debug)]
;       goto, cleanup
;    endif
    if n_elements(output) lt 1 then begin
       output = strip_file_ext( file) + 'e.spec'
    endif
    axis = ['E','X','Y','T']
    siz = ([max_e,max_x,max_y,max_t]+1) > 10
;    ex = fix(alog( siz>1)/alog(2) + 1.0002)
;    siz = 2L^ex
	siz = (siz > 100) < n_spectra_channels
	if n eq 0 then begin
		q = 0													; save at least one spectrum
		n = 1
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
         spec.linearize = linearize
         spec.throttle = throttle_file
         spec.DevObj = clone_device_object(obj)
         tag = string( q[i] + 1 + adc_offset_device(obj) )
         spec.label = strcompress(file[0] + ' ' + tag + '/' + axis[j])
         spec.cal.order = 1
         spec.cal.poly[0] = 0.0              			  ; These set after read in
         spec.cal.poly[1] = 1.0              			  ; Spectrum_load
         spec.cal.units = 'channel'
         spec.ecompress = ecompress
         spec.detector = -1
         spec.comment = etitle
         spec.array = 0
         spec.multiplicity = 1

         spec.charge = charge
		 spec.energy = beam_energy
         spec.IC_total = flux
         spec.deadtime_correction = rel_dt_corr[q[i]]

		 spec.IC.mode = flux_ic.mode
		 spec.IC.conversion = flux_IC.conversion
		 spec.IC.pv.name = flux_ic.pv
		 spec.IC.pv.val = flux_ic.val
		 spec.IC.pv.unit = flux_ic.unit
		 spec.dwell.on = flux_ic.use_dwell
		 spec.dwell.val = flux_ic.dwell
		 spec.plist = ptr_new(pv_list)

         spec.type = 0                   			     ; 0=counts, 1=conc (traverse)
         spec.channel = q[i]
         spec.station = q[i]+1
         spec.show_back = show_back

		spec.processed = processed
		spec.valid = valid
		spec.bad_xy = bad_xy
;		spec.clipped = clipped

         spec.size = siz[j]
         case j of
          0: spec.data = ptr_new( spece[0:siz[j]-1,q[i]] )
          1: spec.data = ptr_new( specx[0:siz[j]-1,q[i]] )
          2: spec.data = ptr_new( specy[0:siz[j]-1,q[i]] )
          3: spec.data = ptr_new( spect[0:siz[j]-1,q[i]] )
         endcase
         p[(ns+1)*i+j] = ptr_new( spec, /no_copy)
       endfor
    endfor
endif else begin											; region energy spectra --------------------------
	if by_detector then begin
		qmask = where( found gt 0, nqmask)					; found detectors
		if nqmask eq 0 then goto, bad_zero
	endif
	p = ptrarr(nqmask)
	centre = [0,4,2,4, 4,4,0,0, 4,4,0,0]     				; which is centre handle
	for i=0L,nqmask-1 do begin
		qi = by_detector ?  qmask[i] : i					; qi = found detector index or index into qmask[]
		siz = max( where(spece[*,qi] gt 0) > 0 ) + 1
;		ex = fix(alog(siz>1)/alog(2) + 1.1)
;		siz = 2^ex < 8192
		siz = (siz > 100) < n_spectra_channels

		pm = by_detector ?  (*pmask)[0] : (*pmask)[qmask[qi]]											;@5-20  qi --> qmask[qi]
		spec = null_spectrum
		spec.file = output
		spec.source = evt_file[0]
		spec.source2 = evt_file[nj-1]
		spec.pileup = (*pm).pileup
		spec.linearize = (*pm).linearize
		spec.throttle = (*pm).throttle
		spec.DevObj = clone_device_object(obj)
		if by_detector then begin
			spec.label = strcompress('Detector ' + str_tidy(qi))
		endif else begin
			spec.label = strcompress('Region ' + str_tidy(qmask[qi]) + ', ' + (*pm).el_shown)			;@5-20  qi --> qmask[qi]
		endelse
		spec.station = (by_detector ? qi : channel[0]) + 1

		type = (*pm).analyze_type[0]
		pmk = (*pm).pmark[0]
		if ptr_good(pmk) then begin
			spec.x = (*pmk).x[centre[type]]                       ; centre
			spec.y = (*pmk).y[centre[type]]
		endif
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
		if by_detector then begin
			if (*pm).array eq 1 then begin
				if ptr_valid( (*pm).pactive) then begin
					qt = where( qi eq *(*pm).pactive)
					if qt[0] ne -1 then begin
						spec.cal.order = 1
						spec.cal.units = 'keV'
						spec.cal.poly[0:1] = (*(*pm).pcal)[qt[0]].poly[0:1]
					endif
				endif
			endif else if (*pm).channel eq qi then begin
				spec.cal.order = 1
				spec.cal.units = 'keV'
				spec.cal.poly[0] = (*pm).cal_b
				spec.cal.poly[1] = (*pm).cal_a
			endif
		endif else begin
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
		endelse
		spec.ecompress = ecompress

		spec.charge = (*pm).charge			; live charge from region
		spec.IC_total = (*pm).IC_total		; total "live" flux count for this region
		spec.energy = beam_energy
		spec.deadtime_correction = by_detector ? rel_dt_corr[qi] : 1.0
		spec.IC.mode = (*pm).ic.mode
		spec.IC.conversion = (*pm).IC.conversion
		spec.IC.pv.name = (*pm).ic.pv.name
		spec.IC.pv.val = (*pm).ic.pv.val
		spec.IC.pv.unit = (*pm).ic.pv.unit
		spec.dwell.on = (*pm).dwell.on
		spec.dwell.val = (*pm).dwell.val
		spec.plist = (*pm).plist
 
    	spec.sample = (*pm).sample
		spec.grain = (*pm).grain
		if lenchr((*pm).comment) gt 0 then begin
			spec.comment = (*pm).comment
		endif else begin
			spec.comment = etitle
		endelse

		spec.history[0] = 'Extracted from EVT using region ' + str_tidy(by_detector ? 0 : qmask[qi])			;@5-20  qi --> qmask[qi]
		spec.n_history = 1

		spec.scan.x = (*pm).scanx
		spec.scan.y = (*pm).scany

		spec.matrix.file = (*pm).matrix
		spec.events = events                 ; terminate events, NOT total events

		spec.type = 0                     ; 0=counts, 1=conc (traverse)
		spec.channel = by_detector ? qi : channel[0]															;@5-20  i --> qi
		spec.detector = -1

		if by_detector then begin
			spec.detector = (*pm).detector
			spec.multiplicity = 1
			spec.array = 0
		endif else begin
			spec.multiplicity = n_elements(channel) > 1
			if (*pm).array eq 1 then begin
				spec.array = 1
				spec.pactive = ptr_new(channel)
				q1 = where( channel[0] eq *(*pm).pactive)
				if q1[0] ne -1 then begin
					spec.detector = (*pm).detector
				endif
			endif else begin
				spec.array = 0
				if(*pm).channel eq channel[0] then begin			; ???
					spec.detector = (*pm).detector
				endif
			endelse
		endelse
		spec.xcompress = (*pm).xcompress
		spec.ycompress = (*pm).ycompress
		spec.nx = (*pm).nx
		spec.ny = (*pm).ny
		spec.show_back = show_back
		gprint, output=cluster_debug,' Spec ',qi,',  x,y=',spec.x,spec.y,' charge=',spec.charge,' multiplicity=',spec.multiplicity

		spec.processed = processed
		spec.valid = valid
		spec.bad_xy = bad_xy
;		spec.clipped = clipped
		gprint, output=cluster_debug,level=2,'   tot=',total(spece[0:siz-1,qi]),'  ',spec.label

		spec.size = siz
		spec.data = ptr_new( spece[0:siz-1,qi] )

		spec.log = 1
		spec.showfit = 1
		spec.show = 0
		if qi eq 0 then spec.show=1

    	p[i] = ptr_new( spec, /no_copy)
	endfor
endelse

;	Save a copy of the detector histograms ...

if (use_mask eq 0) or by_detector then begin
	(*p[0]).max_detectors = geopixe_max_adcs
	(*p[0]).has_pileup = 1
	(*p[0]).pileup_loss_det = ptr_new( pileup_loss_det, /no_copy)
	
	(*p[0]).has_dead = 1
	(*p[0]).deadtime_det = ptr_new( DT, /no_copy)
endif

gprint, output=cluster_debug,level=2,'write spec file=',output
write_spec, p, output

pp = ptr_new( p, /no_copy)

if spectra_present eq 0 then begin
    free_spectra, pp
    if ptr_valid(spectra) then ptr_free, spectra
endif else begin
    spectra = pp
endelse
cluster_result = output

cleanup:
	if free_mask then begin
		for i=0,n_elements( *pmaski)-1 do begin
			free_region_entry, pmaski, i
		endfor
		ptr_free, pmaski
	endif
;	if free_master then begin
;		free_region_entry, pmaster, 0
;		ptr_free, pmaster
;	endif
	if obj_valid(obj) then obj_destroy, obj
    if do_progress and (cluster eq 0) then progress, /ending, progress_tlb
	gprint,level=2, output=cluster_debug, 'SPEC_EVT: end time = ',systime()
	toc, lun=cluster_debug
    return
    
bad_file:
	warning, output=cluster_debug,'spec_evt', 'EVT file not found.'
	return
bad_region:
	warning, output=cluster_debug,'spec_evt', 'Region file not found = '+mask_file
	return
bad_master:
	warning, output=cluster_debug,'spec_evt', 'Master mask Region file not found.'
	return
bad_obj:
	warning, output=cluster_debug,'spec_evt', 'Bad device object for: '+device
    goto, cleanup
bad_mask:
	warning, output=cluster_debug,'spec_evt', 'All region pixel selection masks are Zero.'
    goto, cleanup
bad_cluster:
    warning, output=cluster_debug, 'spec_evt', 'No EVT files for this node.'
    goto, cleanup
bad_ylut_get:
    warning, output=cluster_debug, 'spec_evt', ['Error reading Y Lookup Table.','','Check that there is a YLUT file in the Output directory.']
    goto, cleanup

bad_zero:
    warning, output=cluster_debug, 'spec_evt', 'No spectra data found.'
    goto, cleanup
end
