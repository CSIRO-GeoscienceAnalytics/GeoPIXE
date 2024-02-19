pro da_evt, filei, files=files, matrix=matrix_file, xrange=xrange, yrange=yrange, charge=charge, $
          cal_a=cal_a, cal_b=cal_b, xcompress=xcompress, ycompress=ycompress, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          throttle=throttle, pileup=pileup, linearize=linearize, suppress=suppress, $
          progress=do_progress, scanx=scanx, scany=scany, group=group, device=devicei, $
          
          sample=sample, grain=grain, comment=comment, devpars=devpars, ic=flux_ic, $
		  facility=facility, endstation=endstation, translate_file=translate_file, $
          pv_list=pv_list, flatten=flatten, mpdam_string=mpdam_string, proxy_axis=proxy_axis, $
          
          images=images, xorigin=xorigin, yorigin=yorigin, $
          xoffset=xoffset, yoffset=yoffset, x_sub_range=x_sub_range, y_sub_range=y_sub_range, $ 
          cluster_index=cluster_index, cluster_total=cluster_total, $
          cluster_result=cluster_result, cluster_debug=cluster_debug
          
;   file		list-mode file(s) to process (or specify using "files=" keyword).
;   matrix_file	load Dynamic Analysis Matrix from file 'matrix_file'
;	mpdam_string	if 'matrix_file' is a MPDA (.mpdam) file, 'mp_string' can contain the full struct
;				as read by 'read_mpdam()'. Else, 'read_mpdam' will be called.
;				This enables testing all paths before calling 'da_evt'.
;
;   detector	type of data (0=PIXE, 1=PIGE, ...).
;   device		list-mode device object
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
;	translate_file	file for table of energies (keV) for each proxy-axis (XANES energy) bin.
;	proxy_axis	axis to use as index into 'translate_file' list (e.g. for Line XANES)
;				(0=None, 1=X, 2=Y)	
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
common c_debug_dummy, dummy_write
common c_debug_warnings, enable_warning_popup
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
if n_elements(dummy_write) lt 1 then dummy_write=0
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384
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
gprint,level=2, output=cluster_debug, 'DA_EVT: start time = ',systime()

;cluster = 1
;cluster_total = 4										; for debugging
;cluster_index = 1										; but missing 'worker_progress' call ...
;openw, cluster_debug, 'da_evt_debug.txt', /get_lun

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
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
       		worker_error, stringify( {command:'DA_EVT', error:s2 })
       		gprint,level=2, output=cluster_debug, 'DA_EVT', s2
       endif else begin
	       warning,'DA_EVT', s2, /error
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

;if n_params() lt 1 then begin
;    gprint, output=cluster_debug, 'da_evt: missing arguments'
;    return
;endif
if n_elements(files) gt 0 then filei=files
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then goto, bad_file
if n_elements(channeli) lt 1 then channeli = -1L
if n_elements(xrange) lt 1 then xrange = 1024
if n_elements(yrange) lt 1 then yrange = 1024
if n_elements(charge) lt 1 then charge = 0.0
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
if n_elements(detector) lt 1 then detector=0L
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(flux_ic) lt 1 then flux_ic = {mode:0, pv:'', val:0.0, unit:0.0, conversion:0., use_dwell:0, dwell:1.0}
if n_elements(flatten) lt 1 then flatten=1
if n_elements(pv_list) lt 1 then pv_list=['']
if n_elements(translate_file) lt 1 then translate_file=''
if n_elements(proxy_axis) lt 1 then proxy_axis=0
if x_sub_range eq 0 then x_sub_range = xrange
if y_sub_range eq 0 then y_sub_range = yrange

; Line XANES mode will use the 'proxy_axis' (1=X, 2=Y) as an index into an energy table
; provided by 'translate_file'.

if (translate_file eq '') then proxy_axis=0
line_xanes = (proxy_axis ge 1)

evt_file = file
output = strip_file_ext(strip_non_print(outputi,/no_tab)) + '.dai'

build_ylut = 1
if cluster then begin
	build_ylut = 0
;	q1 = indgen(cluster_total)
;	q1 = congrid( q1, n_elements(file), /center)
;	q2 = where(q1 eq cluster_index, nq)
	q2 = obj->cluster_files( file, cluster_total, cluster_index, nq=nq)

	if nq lt 1 then goto, bad_cluster
	evt_file = file[q2]
	charge = charge * float(nq) / float(cluster_total)	

	gprint,level=2, output=cluster_debug, 'Cluster #',cluster_index,' files=', evt_file
	output = output + '.' + str_tidy(cluster_index)
endif
gprint,level=2, output=cluster_debug, 'Output="',output,'"'

toc, lun=cluster_debug					; assume that a "tic" has been done previously (will use unit declared with tic)

channel = channeli			

if (n_elements(matrix_file) lt 1) then goto, bad_matrix

;	In order to test all files in mpdam (and its referenced .correct DA matrix files), these are
;	done in 'evt_start' and put in an updated mpdam struct, stringified into string 'mpdam_string'.
;	This way all file-names passed here are good, but passed in 'mpdam_string' argument to 'da_evt'.

da_string = matrix_file
if extract_extension( matrix_file) eq 'mpdam' then begin
	if mpdam_string ne '' then begin
		da_string = mpdam_string
		gprint,level=2, output=cluster_debug, 'mpdam_string: '+mpdam_string
	endif
endif

matrix = read_da( da_string, phases=phase_dai, pcorr=pcorr, mpda=mpda, eDA=eDA, error=err)
if err then goto, bad_matrix

if mpda then begin
	help, output=s, matrix
	gprint,level=2, output=cluster_debug, 'Matrix: '+s
	da_matrix = matrix.matrix
	n_comp = n_elements( da_matrix[0,0,*])

	gprint,level=2, output=cluster_debug, 'Use Multi-phase Matrix File="',matrix_file
	gprint,level=2, output=cluster_debug,' Sort using DA matrix: n_el = ', matrix.n_el
	mat_inv_yield = 1. / matrix.yield
	qy = where( finite(matrix.yield) eq 0, nqy)
	if nqy ne 0 then begin
		mat_inv_yield[qy] = 0.0			; note: zero is tested in 'da_accumulate8'
		gprint,level=2, output=cluster_debug,' Clip: ',nqy,' zero yields.'
	endif
	line_xanes = 0
	n_energy = 1
endif else begin
	gprint,level=2, output=cluster_debug, 'Use Matrix File="',matrix_file,'"'

	if ptr_good( matrix.pmore) then begin
		n_energy = n_elements(*matrix.pmore)

		n_planes = n_elements((*(*matrix.pmore)[0]).matrix[0,*])
		da_matrix = fltarr( matrix.size, n_planes, n_energy)	
		for i=0,n_energy-1 do begin
			pm = (*matrix.pmore)[i]
			da_matrix[*,0:n_planes-1,i] = (*pm).matrix[*,0:n_planes-1]
		endfor

;	Because the DA matrix stack may have combined "elastic" and "Compton" to form "scatter",
;	which changes the number of elements and planes, we assign 'matrix' to the one of these
;	with highest beam energy, so the element lists, etc. are correct.

		qe = reverse(sort(eDA))
		t = *(*matrix.pmore)[qe[0]]
		matrix = t
	endif else begin
		if line_xanes then goto, bad_matrix2
		n_energy = 1
		eDA = matrix.e_beam
		n_planes = n_elements( matrix.matrix[0,*])
		da_matrix = matrix.matrix
	endelse
endelse
gprint,level=2, output=cluster_debug, 'DA: Matrix energies=',n_elements(eDA)

if line_xanes then begin

;	Build a translation lookup table from pixel on proxy_axis to eDA matrix index ...
;	It assumes that the 'translation_table' contains all energies for the pixels along the proxy axis.
;	'e_lookup' is a lookup table from these energies into the eDA table, which is usually shorter.
;	Do not use compress on the proxy axis, as it will upset the e_lookup.

	case proxy_axis of
		1: begin
			zrange = xrange
			xcompress = 1
			end
		2: begin
			zrange = yrange
			ycompress = 1
			end
		else: goto, bad_proxy
	endcase

	translate_energy = get_angle_energies( translate_file, do_xanes=do_xanes)
	if (do_xanes eq 0) or (n_elements(translate_energy) lt zrange) then goto, bad_table
	
	e_lookup = uintarr(zrange)
	for j=0,zrange-1 do begin
		e_lookup[j] = binary_search( eDA, translate_energy[j])
	endfor
endif

if n_elements(cal_a) lt 1 then cal_a = matrix.cal_orig.a
if n_elements(cal_b) lt 1 then cal_b = matrix.cal_orig.b

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
; Note that "YLUT" refers to the slowest axis, which is sometimes not actually "Y".

if obj->ylut() then begin
	if build_ylut then begin
		if obj->incremental_ylut() eq 0 then begin
			ylut = obj->build_ylut( evt_file, output=output, error=err)
			if err then goto, bad_ylut_build
			build_ylut = 0
			gprint,level=2, output=cluster_debug,'Build YLUT success.'		;, YLUT=',ylut		
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
xrange2e = (xrange2+1)/2
xrange3 = long( xrange < x_sub_range)
xr = xrange3
xrange3 = xrange3 / xcompress
xrange3e = (xrange3+1)/2

yrange2 = long( yrange / ycompress)
yrange2e = (yrange2+1)/2
yrange3 = yrange  < y_sub_range
yr = yrange3
yrange3 = yrange3 / ycompress
yrange3e = (yrange3+1)/2

; In cluster mode, only build a stripe image for each node, based on the range of Y seen in
; the evt_file[] array. Note that "yoffset" is compressed in read_buffer elsewhere.
; N.B. range method returns a very large 'max' for the last file, so clip to (yrange-1).

; N.B. The constraint of YLUT table Y values all being zero or above, means that
; the negative Y offset sort MUST be done with NO cluster mode.

if cluster and obj->ylut() then begin
;	if obj->incremental_ylut() and err then goto, bad_ylut_ymode

	range = obj->range_ylut( evt_file, error=err)
	if err eq 0 then begin
		yoffset = ((yoffset > range.min) > 0) < (yrange-1)
		yrange3 = ((range.max > 1) < (yrange-1)) + 1
		yr = yrange3 - yoffset
		yoffset3 = yoffset / ycompress
		yrange3 = yrange3 / ycompress
		yrange3 = ((yrange3 - yoffset3) > 1)				;  < (yrange-yoffset)
		yrange3 = ( yrange3  < y_sub_range )

		gprint,level=2, output=cluster_debug,'Y offset, Y range3 set=',yoffset, yrange3	
	endif else begin
		gprint,level=2, output=cluster_debug,'Failed to find valid YLUT to reduce Y image range.'
	endelse
endif else begin
	yrange3 = long( yrange < y_sub_range)
	yr = yrange3
	yrange3 = yrange3 / ycompress
	yoffset3 = yoffset / ycompress
endelse
xoffset3 = xoffset / xcompress
yrange3e = (yrange3+1)/2

; For multi-phase DA, we need to read in, offset and compress, the phase maps
; If phase image is a sub-region, only accept a sub-region exactly like the current one.
; If not, then need to allow for the phase image to have been compressed.

if mpda then begin
	pdai = read_geopixe_image( phase_dai, /silent, error=err)
	if err then goto, bad_mpdai
	if n_comp ne (*pdai).n_el then goto, bad_mpnum

	sphase = strip_path( (*pdai).source)
	sraw = strip_path( file[0])
	if obj->multi_files() then begin
		k = locate( obj->multi_char(), sphase)
		if k ge 0 then sphase = strmid( sphase,0,k)
		k = locate( obj->multi_char(), sraw)
		if k ge 0 then sraw = strmid( sraw,0,k)
	endif
	if sphase ne sraw then goto, bad_phase

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
		gprint,level=2, output=cluster_debug,'mpda: Yoff, Yran; Yrange3 set=',yoff, yran, yrange3	
		phase = smart_congrid( (*(*pdai).image)[ xoff:xoff+xran-1, yoff:yoff+yran-1, *], xrange3, yrange3, (*pdai).n_el)
	endelse
	if n_comp ne (*pdai).n_el then goto, bad_mpnum
	
;	mpda_table = build_DA_phase_weights( matrix, phase, pcorr, q=qmpe, compress=mcompress, error=error)		;@3-16
;	if error then goto, cleanup
	
	free_images, pdai
endif

If line_xanes then begin
	z_coords = eDA[ e_lookup[ yoffset3 + indgen(yrange3)] ]
	z_coords_units = 'keV'
	gprint,level=2, output=cluster_debug,'z_coords:',z_coords
endif

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
 
; Note: matrix.n_el may change later when read_da is called with a valid beam_energy.
; (happens if beam_energy is less than 10 keV as "Compton, elastic" combined to "scatter" in DA)
; This means that the size of 'image' and 'image_error' here may be larger than saved later.
; This is only OK (e.g. to pass to Fortran) because n_el is LAST index in arrays.

gprint, output=cluster_debug,'xrange3,yrange3,matrix.n_el,n_attributes=',xrange3,yrange3,matrix.n_el,n_attributes
toc, lun=cluster_debug

image = fltarr(xrange3,yrange3, matrix.n_el + n_attributes)
flux = fltarr(xrange3,yrange3, n_attributes+1)
dead_fraction = fltarr(xrange3,yrange3)
pileup_loss_map = fltarr(xrange3,yrange3)
nnpu = lonarr(xrange3,yrange3)
nn = lonarr(xrange3,yrange3)
count_rate_map = fltarr(xrange3,yrange3)

image_error = fltarr(xrange3e,yrange3e, matrix.n_el + n_attributes)
if mpda then begin
	invy = fltarr(xrange3e,yrange3e, matrix.n_el)
	fny = fltarr(xrange3e,yrange3e, matrix.n_el)
;	phase_weight = fltarr(xrange3,yrange3, n_comp)			;@3-16
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
gprint, output=cluster_debug,'max_det=',nmax+1
gprint, output=cluster_debug,'channel=',channel

n = 0L
j = 0L
nj = n_elements(evt_file)
ndj = (nj/20) > 1
njc = 0L
first = 1
last_time = systime(/seconds)
ntest1 = 0LL
ntest2 = 0LL
random_subset = 0			; 1								; edit for manual random subset
accept_fraction = 1.		; 0.005
if random_subset then warning,'da_evt','Random subset of "'+str_tidy(accept_fraction)+'" selected.'

loop_file:
	njc += 1
	if njc ge ndj then begin
		gprint,level=2, output=cluster_debug, 'File loop, evt_file[j] =', evt_file[j]
		njc = 0L
		toc, lun=cluster_debug
	endif
	on_ioerror, bad_file
	close,1
	openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, null									; was next

	if build_ylut and obj->ylut() then begin
		jy = long2(extract_extension(evt_file[j])) > 0
	endif
	firsty = 1
	on_ioerror, next									; was next

	device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, first=first, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			suppress=suppress, ic=flux_ic, error=err, x_coords=x_coords, $
			y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units

	if err then goto, finish
	if first then begin
		nprogress = ((100000L / n_guide) > 1L) < 500L

		if (mpda eq 0) and (line_xanes eq 0) then begin
;			free_DA, matrix
;			matrix = read_da(matrix_file, e_beam=beam_energy, error=err)
;			if err then goto, bad_matrix
;			gprint,level=2, output=cluster_debug, 'Use Matrix File="'+matrix_file+'", energy='+str_tidy(beam_energy)
;			gprint,level=2, output=cluster_debug,' Sort using DA matrix: n_el = ', matrix.n_el
;			da_matrix = matrix.matrix

			iDA = binary_search( eDA, beam_energy)
			if abs(eDA[iDA]-beam_energy) gt 0.001 then gprint,level=2, output=cluster_debug, 'Poor energy match: eDA['+str_tidy(iDA)+']='+str_tidy(eDA[iDA])+', beam energy='+str_tidy(beam_energy)
		endif else iDA=0
	endif

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
			station_e=ste, time=tot, veto=veto, ecompress=matrix.ecompress, multiple=multiple, $
			xoffset=xoffset, yoffset=yoffset, title=etitle, file=evt_file[j], error=err, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			total_processed=processed, processed=count1, valid=good, total_bad_xy=sbad_xy
		
		if err then goto, next
		if (n eq 0) or (good eq 0) then goto, cont
		bad_xy = bad_xy + sbad_xy
		
		if build_ylut and obj->ylut() and firsty then begin			; Maia Y lookup table
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
				e = uint(flinear[temporary(e),ste])					; linearize (individual tables)
			endif else begin										; dither not needed here (as in spec_evt)
				e = uint(flinear[temporary(e)])   					; linearize (single table)
			endelse
		endif

;		Translate event energy to DA matrix column.

		if n_elements(cal_a) eq 1 then begin
			energy = float( cal_a * float(e) + cal_b)
		endif else begin
			energy = float( cal_a[ste] * float(e) + cal_b[ste])
		endelse

		col = uint( (energy - matrix.cal.b)/matrix.cal.a + 0.5)

		if array then begin
			if n_elements(hist) lt 1 then begin
				hist = histogram( ste, max=nmax, min=0)
			endif else begin
				hist = histogram( ste, max=nmax, min=0, input=hist)
			endelse
		endif

;		Translate from proxy_axis to nearest 'eDA' energy ...
;		Note if proxy axis is Y, then remember the Y offset to absolute Y axis index.

		if line_xanes then begin
			z1 = (proxy_axis eq 2) ? e_lookup[y1+yoffset3] : e_lookup[x1+xoffset3] 
		endif else begin
			z1 = x1
			z1[*] = iDA												; else just use beam_energy index
		endelse

;     Reject those events with col outside the matrix, outside the pixel range
;     'count2' is the total remaining good events.

		q1 = where( (veto eq 0) and (((col lt 0) or (col ge matrix.size)) or $
										((x1 lt 0) or (x1 ge xrange3)) or  $
										((y1 lt 0) or (y1 ge yrange3)) or $
										((z1 lt 0) or (z1 ge n_energy))), nq1)
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
					gprint, level=2, output=cluster_debug,'da_evt: Extend progress period to ',nprogress
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
																	; warning: count is only ~50000
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
;      and the DA 'matrix' to the DLL routine 'da_accumulate2'.
;      This will accumulate the DA values in 'col2' into
;      'image' at coords 'x2,y2', for each element.

			min_x = min( [min_x, min(x1[q]) + xoffset3])
			min_y = min( [min_y, min(y1[q]) + yoffset3])
			max_x = max( [max_x, max(x1[q]) + xoffset3])
			max_y = max( [max_y, max(y1[q]) + yoffset3])
	
			if mpda then begin
				err = da_accumulate10( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el+n_attributes, nnpu,nn, $		;@3-16
					image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, phase, n_comp, $
					mat_inv_yield, invy, fny, multiple=multiple)
;				err = da_accumulate9( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el+n_attributes, nnpu,nn, $		;@3-16
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, mpda_table, $
;							mat_inv_yield, invy, fny, multiple=multiple, q=qmpe, compress=mcompress, phase_weight)
;				err = da_accumulate8( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el+n_attributes, nnpu,nn, $
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, mpda_table, $
;							mat_inv_yield, invy, fny, multiple=multiple, q=qmpe, compress=mcompress)
;				err = da_accumulate7( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3,matrix.n_el+n_attributes, nnpu,nn, $
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, mpda_table, $
;							multiple=multiple, q=qmpe, compress=mcompress)
;				err = da_accumulate6( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3, nnpu,nn, $
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, phase, n_comp, $
;							multiple=multiple)
			endif else begin
				err = da_accumulate11( x1,y1,col,pu,veto,z1, n,count2, image,xrange3,yrange3, nnpu,nn, $
							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, n_planes, n_energy, $
							multiple=multiple)
;				err = da_accumulate5( x1,y1,col,pu,veto, n,count2, image,xrange3,yrange3, nnpu,nn, $
;							image_error,xrange3e,yrange3e, matrix.n_el, da_matrix, matrix.size, $
;							multiple=multiple)
			endelse
			if err ne 0 then begin
				gprint,level=2, output=cluster_debug,'da_evt: error (',err,') return from da_accumulate'
				goto, finish
			endif
		endif
		if events gt 0 then if processed gt events then begin
			gprint,level=2, output=cluster_debug,'da_evt: requested event count exceeded; stop.'
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
	gprint,level=2, output=cluster_debug,'da_evt: finished.'
	if do_throttle then gprint,level=2, output=cluster_debug,'    Used THROTTLE file ',throttle
	if do_pileup then gprint,level=2, output=cluster_debug,'  Used PILEUP file ',pileup
	if do_linear then gprint,level=2, output=cluster_debug,'  Used LINEARIZE file ',linearize
;    gprint, level=2, output=cluster_debug, ' ntest1 = ', ntest1, '   ',ttest1
;    gprint, level=2, output=cluster_debug, ' ntest2 = ', ntest2, '   ',ttest2
	gprint,level=2, output=cluster_debug, ' processed = ', processed
	gprint,level=2, output=cluster_debug, ' valid events = ',valid
	gprint,level=2, output=cluster_debug, ' bad event triplets = ',bad_xy
	gprint,level=2, output=cluster_debug, ' clipped to image,matrix bounds, or not station = ', clipped
	gprint,level=2, output=cluster_debug, ' pileup losses = ',pileup_losses
	gprint,level=2, output=cluster_debug, ' pileup fraction = ', float(pileup_losses) / processed
	gprint,level=2, output=cluster_debug, ' X range = ', min_x, max_x
	gprint,level=2, output=cluster_debug, ' Y range = ', min_y, max_y
	if n_elements(flux) gt 1 then gprint,level=2, output=cluster_debug,' found FLUX array'
	if n_elements(dead_fraction) gt 1 then gprint,level=2, output=cluster_debug,' found DEAD_FRACTION array'
	if mpda then gprint,level=2, output=cluster_debug,' used PHASE and YIELD arrays for Multiphase DA'
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
	toc, lun=cluster_debug

	close, 1
	on_ioerror, null
	t = 'EVT sorting complete. Save Images: '+strip_file_ext(output)+'.dai'
	if do_progress then begin
		progress, /complete, progress_tlb, t
	endif
	if cluster then begin
		if worker_progress(1.) eq 0 then return
	endif

	if build_ylut and obj->ylut() then obj->write_ylut, ylut, file[0], output=output

;--------------------------------------------------------------------------
;   Write the image results file ...

null_image = define(/image)
img = null_image
nxy = long(xrange3) * long(yrange3)
ninf = 0LL
for i=0L,matrix.n_el-1 do begin
	image[*,*,i] = finite_image( image[*,*,i], /zero, ninf=nq)
	ninf = ninf + nq
	image_error[*,*,i] = finite_image( image_error[*,*,i], /zero)
endfor
if ninf gt 0 then gprint, level=2, output=cluster_debug,'Killed ',nq,' non finite pixels.'

gprint,level=2, output=cluster_debug, 'da_evt: write image file - ' + output

; For detector arrays, need to scale down result, depending on the number of
; detectors actually used to sort the EVT. Without an array, just count n_det.
; With an array, use the rGamma yield-ratio arrays.

xd = 1. / multiplicity_scale( channel, matrix, multiplicity=multiplicity)		;@3-16
xd2 = xd*xd
for i=0L,matrix.n_el-1 do begin
	image[*,*,i] = image[*,*,i] * xd[i]
	image_error[*,*,i] = image_error[*,*,i] * xd2[i]
endfor

xstep_on = 0L
xstep_count = 0L
step_events = 0L
step_toggle = 0L
toggle_bit = 0L
step_station = 0L
type = 0L               ; 0=ppm.uC data, 1=mineral fractions
						; units = ['keV','MeV','MeV']

img.source = evt_file[0]
img.source2 = evt_file[n_elements(evt_file)-1]
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
img.ecompress = matrix.ecompress

live = 1.0
if (obj->name() eq 'OM_DAQ_DEVICE') and (suppress eq 0) then begin
    old_charge = charge
    charge = lmf_charge
    dead = 0.0
    if (n_elements(group) ge 1) then charge = OM_charge_select(group, charge, live=lmf_live, dead=dead)
    live = 1.0-dead
    if charge lt 1.e-10 then charge=old_charge
    img.charge = charge
    scanx = float(lmf_size[0])
    scany = float(lmf_size[1])
    img.scan.x = lmf_size[0]*0.001			; this will break sub-range windows
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
if line_XANES then begin
	case proxy_axis of
		1: begin
			img.px_coords = ptr_new(z_coords)
			img.x_coord_units = z_coords_units
			end
		2: begin
			img.py_coords = ptr_new(z_coords)
			img.y_coord_units = z_coords_units
			end
	endcase
endif

img.processed = processed
img.valid = valid
img.bad_xy = bad_xy
img.clipped = clipped

img.matrix.label = matrix.label
if matrix.label eq '' then img.matrix.label=matrix_file
img.matrix.file = matrix_file
img.matrix.charge = matrix.charge[0]
img.matrix.mdl = ptr_new(matrix.mdl)

img.n_el = matrix.n_el
img.el = ptr_new(matrix.el)
img.energy_proxy_axis = proxy_axis
img.energies_file = translate_file

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

img.events = events                  ; events up to 'terminate', NOT necessarily total events

img.type = type
img.mode = mpda ? 3 : 0				; DA or MPDA mode
img.channel = channel[0]
img.detector = detector
if n_elements(hist) gt 0 then img.hist = ptr_new(hist, /no_copy)

; Repair flux array for glitches and missing values, etc.
; Note: this assumes scan order (x=0). This should be moved into device dependent
; method later.
; Later will need to do dwell per pixel correction AFTER this repair, as it will
; add a little jitter per pixel that the repair might try to remove.

; If 'flux' has been multiply counted, use 'pass-count' method to scale back.
; This assumes all detectors together in each cluster stripe (make sure device object
; ensures this, e.g. "FalconX (merge) device").

if n_elements(flux) gt 1 then begin
	flux = flux / float( obj->pass_count())
	raw_flux = flux[*,*,0]
	total_flux = total(flux[*,*,0])
;	if total_flux gt 0.001 then begin
;		flux[*,*,0] = flux_repair( raw_flux, X=0, /median)
;	endif
endif

;	For some devices, the dead_fraction, flux and dwell maps my be noisy.
;	The dwell and weight get smoothed in the method calls below.
;	Here we need to explicitly request smoothing of flux and dead-fraction.
;	The smoothing width is controlled in the Device panels.

if n_elements(flux) gt 1 then begin
	raw_flux = obj->smooth_flux(raw_flux)
	flux[*,*,0] = raw_flux
endif
if n_elements(dead_fraction) gt 1 then begin
	dead_fraction = obj->smooth_weight(dead_fraction)
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
			gprint,level=2, output=cluster_debug,'da_evt: normalize dead-fraction to weight'
	
			dead_fraction[q1] = dead_fraction[q1] / weight[q1]
			if mode eq 1 then begin
				gprint,level=2, output=cluster_debug,'da_evt: Correct from OCR to ICR weights (mode=1).'
				dead_fraction[q1] = dead_fraction[q1] / (1 + dead_fraction[q1])
			endif
		endif
	endelse
endif

;	Retrieve a dwell map from the device if it exists.
;	If it exists, that means returned 'dead_fraction' is actually dead-time.
;	Dwell will be used to normalize the dead_fraction and count-rates.

dwell = obj->get_dwell(error=err_dwell)
mean_dwell = 0.

if (err_dwell eq 0) and (n_elements(dwell) gt 2) then begin
	q = where( (dwell gt 0.), nq)
	if nq gt 0 then begin
		mean_dwell = mean(dwell[q])
		gprint,level=2, output=cluster_debug,'da_evt: Dwell-time, ave: = ', mean_dwell, ' ms'

		if mode le -1 then begin
			dead_fraction[q] = dead_fraction[q] / dwell[q]
		endif
		gprint,level=2, output=cluster_debug,'da_evt: dead-fraction, ave: = ', mean(dead_fraction[q])

		count_rate_map[q] = 1000. * float(nn[q]) / dwell[q]
		gprint,level=2, output=cluster_debug,'da_evt: count-rate, ave: = ', mean(count_rate_map[q])
		img.has_rates = 1
		img.count_rate_map = ptr_new( count_rate_map, /no_copy)

		img.has_dwell = 1
		img.dwell_map = ptr_new( dwell, /no_copy)	
	endif else begin
		gprint,level=2, output=cluster_debug,'da_evt: Dwell-time, zero !'
	endelse
endif

; Test dead_fraction for magnitude.

if n_elements(dead_fraction) gt 1 then begin
	if n_elements(flux[*,*,0]) eq n_elements(dead_fraction) then begin
		tdead = total(dead_fraction)/n_elements(dead_fraction)
    	if tdead gt 0.001 then begin
			q = where(dead_fraction gt 0.95, nq)
			if nq gt 0 then begin
				gprint,level=2, output=cluster_debug,'da_evt: Extreme dead-time fraction for '+str_tidy(nq)+' pixels. Clip these to 0.95.'
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
				gprint,level=2, output=cluster_debug,'da_evt','Extreme pileup-loss fraction for '+str_tidy(nq)+' pixels. Clip these to 0.95.'
				pileup_loss_map[q] = 0.95
			endif
		endif	
	   	gprint,level=2, output=cluster_debug,'da_evt: pile-up losses, ave: = ', mean(pileup_loss_map)

		if (n_elements(dead_fraction) gt 1) then begin
			gprint,level=2, output=cluster_debug,'da_evt: dead-time losses, ave: = ', mean(dead_fraction)
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
			gprint,level=2, output=cluster_debug,'da_evt: correct flux array for DT+PU losses, ave: = ', mean(dead_fraction)

	    	flux[*,*,0] = flux[*,*,0] * (1. - dead_fraction)

			img.has_dead = 1
			img.dead_fraction = ptr_new( DT, /no_copy)			; save DT excluding PU effects
		endif
	endif
endif

img.energy = beam_energy
img.has_errors = 1

; Add extra attribute planes to image[] array and element list ...

gprint,level=2, output=cluster_debug,'da_evt: n_el, n_attributes =', img.n_el, n_attributes
if do_attributes then begin
	image[*,*,img.n_el:img.n_el+n_attributes-1] = flux[*,*,1:n_attributes]
	if img.has_errors then begin
		image_error[*,*,img.n_el:img.n_el+n_attributes-1] = smart_congrid( sqrt( flux[*,*,1:n_attributes]), xrange3e,yrange3e,n_attributes)
	endif
	img.n_attributes = n_attributes
	img.el = ptr_new( [ (*img.el)[0:img.n_el-1], attributes ])
	img.n_el = img.n_el + n_attributes
endif

if mpda then begin
	gprint,level=2, output=cluster_debug,'da_evt: save Phases, n_comp =', n_comp
	img.has_phase = 1
	img.phase = ptr_new( phase, /no_copy)									;@3-16

;	Test new 'phase_weight' as phase for now ...
;	phase_weight = finite_image( phase_weight, /zero, /norm, ninf=ninf)		;@3-16
;	if ninf gt 0 then gprint,level=2, output=cluster_debug,'da_evt: phase infinite pixels =', ninf
;	img.phase = ptr_new( phase_weight, /no_copy)	

;	Save phase weighted inverse yields as 'yield' map.

	gprint,level=2, output=cluster_debug,'da_evt: save Yields, n_el =', img.n_el
	img.has_yield = 1
	yield = finite_image( fny / invy, /mean, ninf=ninf)				; note leave /0 in this ???
	img.yield = ptr_new( yield, /no_copy)
	if ninf ne 0 then gprint,level=2, output=cluster_debug,'da_evt: *** Warning: Yields have '+str_tidy(ninf)+' NaN pixels. ***'
endif

img.image = ptr_new( image, /no_copy)
img.error = ptr_new( image_error, /no_copy)

img.file = output
pimg = ptr_new(img, /no_copy)

; Correct for flux/current variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, so the correction here must
; preserve the average.

if n_elements(flux) gt 1 then begin
	aps_count_to_charge = flux_ic.conversion
	if flatten then gprint,level=2, output=cluster_debug,'da_evt: Correct images for flux ...'

	image_correct_flux, pimg, flux, flux_IC, pv_list, charge=charge, flatten=flatten, $
							raw_flux=raw_flux, random_subset=random_subset, accept_fraction=accept_fraction
endif
gprint,level=2, output=cluster_debug,'da_evt: pimg struct:'
;pointer_display, unit=cluster_debug, pimg

if dummy_write then begin
	gprint,level=2, output=cluster_debug,'da_evt: Dummy save of images to file ',output
endif else begin
	gprint,level=2, output=cluster_debug,'da_evt: Write images ...'
	write_geopixe_image, pimg, output, /no_display, cluster=cluster, error=err, no_delete=0
	if err eq 0 then gprint,level=2, output=cluster_debug,'da_evt: Image write completed.'
endelse

if arg_present(images) eq 0 then begin
    free_images, pimg
endif else begin
    images = pimg
endelse
cluster_result = output

cleanup:
	gprint,level=2, output=cluster_debug, 'DA_EVT: finish time = ',systime()
    if do_progress and (cluster eq 0) then progress, /ending, progress_tlb
	free_DA, matrix
	toc, lun=cluster_debug
    return

bad_matrix:
    warning, output=cluster_debug, 'da_evt', 'Bad DA matrix.'
    goto, cleanup

bad_matrix2:
    warning, output=cluster_debug, 'da_evt', 'Matrix does not seem to be a XANES stack series.'
    goto, cleanup

bad_table:
    warning, output=cluster_debug, 'da_evt', 'Missing or short energy Translation table.'
    goto, cleanup

bad_proxy:
    warning, output=cluster_debug, 'da_evt', 'Illegal proxy axis.'
    goto, cleanup

bad_mpdam:
    warning, output=cluster_debug, 'da_evt', 'Bad multiphase DA correction file read.'
    goto, cleanup

bad_mpdai:
    warning, output=cluster_debug, 'da_evt', 'Bad phase map DAI file read.'
    goto, cleanup

bad_phase:
	warning, 'da_evt',['Phase maps do not appear to be those for this data-set.', '', $
				'Input data source:  '+ sraw, $
				'Phase map source:  '+ sphase, '', $
				'Generate relevant phase maps and add to "mpdam".', $
				'Click on "New" to edit mpdam details.']
	goto, cleanup
bad_mpsub:
    warning, output=cluster_debug, 'da_evt', ['Phase map sub-region size and offset', $
												'do not match the present settings.']
    goto, cleanup

bad_mpnum:
    warning, output=cluster_debug, 'da_evt', 'Number of phases in DAI does not match number of DA matrices.'
    goto, cleanup

bad_mpnum2:
    warning, output=cluster_debug, 'da_evt', 'Number of elements in one DA does not match number in first matrix.'
    goto, cleanup

bad_mpsize:
    warning, output=cluster_debug, 'da_evt', 'Inconsistent DA matrix sizes.'
    goto, cleanup

bad_file:
    warning, output=cluster_debug, 'da_evt', 'EVT file not found.'
    goto, cleanup

bad_obj:
	warning, output=cluster_debug,'da_evt', 'Bad device object for: '+device
    goto, cleanup

bad_ylut_ymode:
    warning, output=cluster_debug, 'da_evt', ['Cannot use "cluster" mode first with "Correct Y Encoder" mode.', $
    				'Do a normal (non-cluster) sort first to build a valid YLUT file.']
    goto, cleanup

bad_ylut_build:
    warning, output=cluster_debug, 'da_evt', 'Error building Y Lookup Table.'
    goto, cleanup

bad_ylut_get:
    warning, output=cluster_debug, 'da_evt', 'Error reading Y Lookup Table.'
    goto, cleanup

bad_cluster:
	warning, output=cluster_debug, 'da_evt', 'No EVT files for this node.'
    goto, cleanup
end
