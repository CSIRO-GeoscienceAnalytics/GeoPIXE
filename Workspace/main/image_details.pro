pro add_history_item, detail, itemi, value, json=json

; Add an 'item' with a 'value' to 'detail'
;	'item' is a name identifier.
;	'value' is any value/data-type
;
; json=0	'detail' is assumed to be a string array
; json=1	'detail' is assumed to be a dictionay destined to be converted to
;			JSON.
; if 'value' missing, then skip this in /json case

	if n_elements(json) eq 0 then json=0

	if json then begin
		if n_elements(value) eq 0 then return
		if typename(value) eq 'STRING' then begin
			if value[0] eq '' then return
		endif

		item = strtrim(itemi,2)
		if typename(detail) ne 'ORDEREDHASH' then begin
			detail = orderedhash( item, value)
		endif else begin
			detail = detail + orderedhash( item, value)
		endelse
	endif else begin
		if n_elements(detail) eq 0 then detail=''
		item = itemi + ':  '
		if detail[0] eq '' then begin
			detail = [ item + str_tidy(value)]
		endif else begin
			detail = [ detail, item + str_tidy(value)]
		endelse
	endelse
	return
end

;----------------------------------------------------------------------------------------------

function image_details, pimg, show=showi, index=index, brief=brief, stats=stats, json=json

; Return a string array of image metadata for image 'pimg'.
; 
; showi		for a selected element index
; index		return a vector of line index to be used to identify click events on rows
;			(only used/correct in JSON=0 case)
; /brief	only main details
; /stats	also output image statistics
; /json		outout in JSON form for metadata dump

COMPILE_OPT STRICTARR
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
		warning,'image_geopixe_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, detail
	endif
endif

define_devices
if n_elements(showi) lt 1 then showi=0
if n_elements(brief) lt 1 then brief=0
if n_elements(stats) lt 1 then stats=0
if n_elements(json) lt 1 then json=0

bigpixels = stats ? 1.2e+9 : 3.0e+4

detail = ''
index = [0]
if ptr_good(pimg, /struct) eq 0 then return, detail
p = pimg
obj = (*p).DevObj
if obj_valid(obj) eq 0 then return,''

xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found
show = (showi > 0) < (n_el - 1)

mpda = extract_extension( (*p).matrix.file) eq 'mpdam'

;------------------------------------------------------------
; Details string array:
; 
;	'index' refers to an identifier that is used in 'Image_history_event'/'Image_history_List' when 
;	a line is double-clicked. This is used to enable changing certain parameters.

add_history_item, detail, 'GeoPIXE Image DAI file', json=json
add_history_item, detail, '   File', (*p).file, json=json
add_history_item, detail, '   Version', (*p).version, json=json
index = [0,0,0]

ionbeam = obj->ionbeam()
charge_gain_unit_lists, ic_vals, ic_units, ic_vunits, ionbeam=ionbeam
on_off = ['Off','On']
modes = ['DA matrix:', 'Cuts (ROI):', 'STIM Cuts (mean energy):', 'Multiphase DA (MPDA):', 'XYE (compressed E) data cube:']

image_axes_units, p, xunits=xunits, yunits=yunits

add_history_item, detail, 'Data file(s)', json=json
add_history_item, detail, '   Source', (*p).source, json=json
add_history_item, detail, '   Title', obj->title(), json=json
index = [index,0,0,0]

if (xanes eq 0) and (brief eq 0) then begin
	add_history_item, detail, '   Valid', (*p).valid, json=json
	add_history_item, detail, '   Bad XY', (*p).bad_xy, json=json
	add_history_item, detail, '   Clipped', (*p).clipped, json=json
	index = [index, 10,10,10]
endif
if (*p).facility ne '' then begin
	add_history_item, detail, '   Facility', (*p).facility, json=json
	if (*p).endstation ne '' then begin
		add_history_item, detail, '   Endstation', (*p).endstation, json=json
		index = [index, 11]
	endif
	index = [index, 11]
endif

if obj->linear() then begin
	if (*p).linearize ne '' then begin
		add_history_item, detail, '   Linearize', (*p).linearize, json=json
		index = [index, 20]
	endif
endif
if obj->pileup() then begin
	if (*p).throttle ne '' then begin
		add_history_item, detail, '   Pileup', (*p).pileup, json=json
		index = [index, 30]
	endif
endif
if obj->Throttle() then begin
	if (*p).throttle ne '' then begin
		add_history_item, detail, '   Throttle', (*p).throttle, json=json
		index = [index, 40]
	endif
endif

add_history_item, detail, 'Image', json=json
if json then begin
	add_history_item, detail, 'X pixels', (*p).xsize, json=json
	add_history_item, detail, 'Y pixels', (*p).ysize, json=json
	add_history_item, detail, 'X compress', (*p).xcompress, json=json
	add_history_item, detail, 'Y compress', (*p).ycompress, json=json
	if xanes then begin
		if ((*p).stack_type eq 1) then begin
			add_history_item, detail, '   Z compress', (*p).zcompress, json=json
		endif
	endif
endif else begin
	add_history_item, detail, '   Size', str_tidy((*p).xsize) + ' x ' + str_tidy((*p).ysize) + ' pixels', json=json
	s = str_tidy((*p).xcompress) + ' x ' + str_tidy((*p).ycompress)
	if xanes then begin
		if ((*p).stack_type eq 1) then s = s + ' x ' + str_tidy((*p).zcompress)
	endif
	add_history_item, detail, '   Compression', s, json=json
	index = [index, 50,50,50]
endelse

npixels = long((*p).xsize)*long((*p).ysize)

r = image_absolute( p, error=err)
if err ne 0 then return, detail
ox = r.absolute.org.x
oy = r.absolute.org.y

sub_region = 0
if xanes eq 0 then begin
	sub_region = (*p).sub_region
	if (*p).sub_region then begin
		dx = (*p).scan.x / ((*p).original_xsize * (*p).xcompress)
		dy = (*p).scan.y / ((*p).original_ysize * (*p).ycompress)
		add_history_item, detail, '   Sub-region', json=json
		if json then begin
			add_history_item, detail, 'X sub_range', (*p).x_sub_range, json=json
			add_history_item, detail, 'Y sub_range', (*p).y_sub_range, json=json
			add_history_item, detail, 'Xsize sub_range', (*p).x_sub_range*dx, json=json
			add_history_item, detail, 'Ysize sub_range', (*p).y_sub_range*dy, json=json
		endif else begin
			add_history_item, detail, '         Sub-range', str_tidy((*p).x_sub_range) + ' x ' + str_tidy((*p).y_sub_range) + ' (uncompressed pixels)', json=json
			add_history_item, detail, '         Size', str_tidy((*p).x_sub_range*dx) + ' x ' + str_tidy((*p).y_sub_range*dy) + ' (mm)', json=json
			index = [index, 80,80,80]
		endelse
	endif
endif
if ((*p).xoffset ne 0) or ((*p).yoffset ne 0) or (sub_region ne 0)then begin
	if json then begin
		add_history_item, detail, 'X offset', (*p).xoffset, json=json
		add_history_item, detail, 'Y offset', (*p).yoffset, json=json
		add_history_item, detail, 'Xorigin sub_range', ox, json=json
		add_history_item, detail, 'Yorigin sub_range', oy, json=json
	endif else begin
		add_history_item, detail, '         Offset: ' + str_tidy((*p).xoffset) + ' x ' + str_tidy((*p).yoffset) + ' (uncompressed pixels)', json=json
		add_history_item, detail, '         New origin: ' + str_tidy(ox) + ' x ' + str_tidy(oy) + ' ('+yunits+')', json=json
		index = [index, 80,80]
	endelse
endif

if xanes then begin
	if json then begin
		if (*p).stack_type eq 1 then begin
			add_history_item, detail, 'Z angles', (*p).zsize, json=json
		endif else begin
			add_history_item, detail, 'Z energies', (*p).zsize, json=json
		endelse
	endif else begin
		if (*p).stack_type eq 1 then begin
			add_history_item, detail, '   Tomography image stack', json=json
			s = z_found ? '(angles loaded)' : '(no angles table)'
			add_history_item, detail, '       Angle planes', str_tidy((*p).zsize) + ' ' + s, json=json
		endif else begin
			s = z_found ? '(energies loaded)' : '(no energies table)'
			add_history_item, detail, '       Energy planes', str_tidy((*p).zsize) + ' ' + s, json=json
		endelse
		index = [index, 60]
	endelse
endif

if json then begin
	add_history_item, detail, 'Original X pixels', (*p).original_xsize*(*p).xcompress, json=json
	add_history_item, detail, 'Original Y pixels', (*p).original_ysize*(*p).ycompress, json=json
	add_history_item, detail, 'Original N pixels', long((*p).original_xsize*(*p).xcompress) * long((*p).original_ysize*(*p).ycompress), json=json
	add_history_item, detail, 'X size', (*p).scan.x, json=json
	add_history_item, detail, 'Y size', (*p).scan.y, json=json
	if xanes then begin
		add_history_item, detail, 'Z planes', (*p).original_zsize*(*p).zcompress, json=json
		add_history_item, detail, 'Z size', (*p).scan.z * 1000., json=json
	endif
endif else begin
	sorg = str_tidy((*p).original_xsize*(*p).xcompress) + ' x ' + str_tidy((*p).original_ysize*(*p).ycompress)
	spix = ' pixels (' + pixels_legend( float((*p).original_xsize*(*p).xcompress) * float((*p).original_ysize*(*p).ycompress)) + ')'
	sorg2 = str_tidy((*p).scan.x) + ' ('+xunits+')' + ' x ' + str_tidy((*p).scan.y) + ' ('+yunits+')'
	if xanes then begin
		spix = spix + ' x ' + str_tidy((*p).original_zsize*(*p).zcompress)
		sorg2 = sorg2 + ' x ' + str_tidy((*p).scan.z)
	endif
	add_history_item, detail, '   Original', sorg + spix, json=json
	add_history_item, detail, '         Size', sorg2, json=json
	index = [index, 70, 80]
endelse

if ((*p).scan.origin.x ne 0.) or ((*p).scan.origin.y ne 0.) then begin
	if json then begin
		add_history_item, detail, 'X origin', (*p).scan.origin.x, json=json
		add_history_item, detail, 'Y origin', (*p).scan.origin.y, json=json
		if xanes then add_history_item, detail, 'Z origin', (*p).scan.origin.z, json=json
	endif else begin
		s = str_tidy((*p).scan.origin.x) + ' ('+xunits+')' + ', ' + str_tidy((*p).scan.origin.y) + ' ('+yunits+')'
		if xanes then s=s+', ' + str_tidy((*p).scan.origin.y)
		add_history_item, detail, '         Origin', s, json=json
		index = [index, 90]
	endelse
endif

if ((*p).bounds.xmin ne 0) or ((*p).bounds.xmax ne 0) or ((*p).bounds.ymin ne 0) or ((*p).bounds.ymax ne 0) then begin
	if json then begin
		add_history_item, detail, 'X min', (*p).bounds.xmin, json=json
		add_history_item, detail, 'X max', (*p).bounds.xmax, json=json
		add_history_item, detail, 'Y min', (*p).bounds.ymin, json=json
		add_history_item, detail, 'Y max', (*p).bounds.ymax, json=json
	endif else begin
		if ((*p).scan.origin.x ne 0.) or ((*p).scan.origin.y ne 0.) then begin
			add_history_item, detail, '   Bounds X', str_tidy((*p).bounds.xmin) + ' to ' + str_tidy((*p).bounds.xmax) + ',  ' + $
						'Y: ' + str_tidy((*p).bounds.ymin) + ' to ' + str_tidy((*p).bounds.ymax), json=json
			index = [index, 90]
		endif
	endelse
endif

if ((*p).scaled_x ne 1.0) or ((*p).scaled_y ne 1.0) then begin
	if json then begin
		add_history_item, detail, 'X scaled', (*p).scaled_x, json=json
		add_history_item, detail, 'Y scaled', (*p).scaled_y, json=json
	endif else begin
		add_history_item, detail, '   Scaled X', str_tidy((*p).scaled_x) + ', Y:' + str_tidy((*p).scaled_y) + ' (post compress)', json=json
		index = [index, 100]
	endelse
endif

if (*p).dwell.on then begin
	add_history_item, detail, '   Dwell', (*p).dwell.val, json=json
	index = [index, 110]
endif

if ptr_good((*p).pactive) then begin
	active = (*(*p).pactive) + adc_offset_device(obj)+1
	if json then begin
		if n_elements(active) gt 1 then begin
			add_history_item, detail, 'Detectors', active, json=json
			add_history_item, detail, 'Multiplicity', n_elements(*(*p).pactive), json=json
		endif else begin
			add_history_item, detail, 'Detector', active[0], json=json
			index = [index, 120]
		endelse
	endif else begin
		if n_elements(active) gt 1 then begin
			s = strjoin(str_tidy( active[0:26 < (n_elements(active)-1) ]),', ')
			if n_elements(active) gt 27 then s=s+' ...'
			add_history_item, detail, '   Detectors', s, json=json
			add_history_item, detail, '   Detector array (active detector multiplicity)', str_tidy(n_elements(*(*p).pactive)), json=json
			index = [index, 120,120]
		endif else begin
			add_history_item, detail, '   Detector', str_tidy( active[0]), json=json
			index = [index, 120]
		endelse
	endelse
endif else begin
	add_history_item, detail, '   Detector', (*p).channel[0] + adc_offset_device(obj) + 1, json=json
	index = [index, 120]
endelse
if xanes eq 0 then begin
	if (*p).energy gt 1. then begin
		add_history_item, detail, '   Energy', (*p).energy, json=json
		index = [index, 125]
	endif
endif

if ptr_good((*p).px_coords) then begin
	if json then begin
		add_history_item, detail, '   Xarray points', *(*p).px_coords, json=json
		add_history_item, detail, '   Xarray min', min(*(*p).px_coords), json=json
		add_history_item, detail, '   Xarray max', max(*(*p).px_coords), json=json
	endif else begin
		add_history_item, detail, '   X axis position array', str_tidy(n_elements(*(*p).px_coords)) + '), units: ' + (*p).x_coord_units, json=json
		add_history_item, detail, '   X coord range', str_tidy(min(*(*p).px_coords),places=4) + ' to ' + str_tidy(max(*(*p).px_coords),places=4) + ' ' + (*p).x_coord_units, json=json
		index = [index, 130,130]
	endelse
endif
if ptr_good((*p).py_coords) then begin
	if json then begin
		add_history_item, detail, '   Yarray points', *(*p).py_coords, json=json
		add_history_item, detail, '   Yarray min', min(*(*p).py_coords), json=json
		add_history_item, detail, '   Yarray max', max(*(*p).py_coords), json=json
	endif else begin
		add_history_item, detail, '   Y axis position array', str_tidy(n_elements(*(*p).py_coords)) + '), units: ' + (*p).y_coord_units, json=json
		add_history_item, detail, '   Y coord range', str_tidy(min(*(*p).py_coords),places=4) + ' to ' + str_tidy(max(*(*p).py_coords),places=4) + ' ' + (*p).y_coord_units, json=json
		index = [index, 140,140]
	endelse
endif

add_history_item, detail, modes[(*p).mode], json=json
index = [index, 150]
if (*p).matrix.file ne '' then begin
	add_history_item, detail, '   Matrix file', (*p).matrix.file, json=json
	index = [index, 150]
endif
if ((*p).mode eq 4) then begin
	if json then begin
		add_history_item, detail, 'Compressed_energies', *(*p).el, json=json
	endif else begin
		add_history_item, detail, '   Compressed energies', str_tidy(n_elements(*(*p).el)-2)+'): ' + strjoin( (*(*p).el)[0:12], ', ') + '...', json=json
		index = [index, 152]
	endelse
endif else if (((*p).matrix.charge lt 1.0e-6) or ((*p).matrix.charge gt 1.0e+6)) then begin
	if json then begin
		add_history_item, detail, 'Matrix warning', str_tidy((*p).matrix.charge) + ' uC). Consider reconstructing this DA matrix.', json=json
	endif else begin
		add_history_item, detail, '   A very strange matrix charge found', str_tidy((*p).matrix.charge) + ' uC). Consider reconstructing this DA matrix.', json=json
		index = [index, 151]
	endelse
endif

if xanes eq 0 then begin
	if ptr_good( (*p).matrix.mdl) eq 0 then begin
		if ((*p).mode eq 0) then begin
			add_history_item, detail, 'MDL warning', '*** No matrix MDLs found. Regenerate images.', json=json
			index = [index, 160]
		endif
	endif else begin
		sum = total( *(*p).matrix.mdl)
		nm = n_elements( *(*p).matrix.mdl)
		q = where( finite( *(*p).matrix.mdl) eq 0, nq)
		if ((*p).mode eq 0) then begin
			if json then begin
				add_history_item, detail, 'Elements', el, json=json
			endif else begin
				add_history_item, detail, '   Elements', strjoin(el,' '), json=json
				index = [index, 160]
			endelse
			if nq gt nm/2 then begin
				add_history_item, detail, 'MDL warning', '*** Matrix MDLs found but some not finite. Regenerate images with valid charge.', json=json
				index = [index, 160]
			endif else begin
				if abs(sum) lt 1.0e-3 then begin
					add_history_item, detail, 'MDL warning', '*** Matrix MDLs found but all zero. Regenerate images.', json=json
					index = [index, 160]
				endif
			endelse
		endif
	endelse
	if (*p).has_phase then Begin
		sp = ptr_good((*p).phase) ? str_tidy(n_elements( (*(*p).phase)[0,0,*])) : '?'
		add_history_item, detail, '   Phase maps available', sp, json=json
		index = [index, 165]
	endif
endif

unit = ''
if (*p).IC.mode eq 1 then begin
	add_history_item, detail, 'Ion chamber', json=json
	l = locate('time', strlowcase((*p).IC.PV.name))
	ival = find_charge_val_unit_index( (*p).IC.PV.val, (*p).IC.PV.unit, iunit=iunit, time=(l ge 0))
	unit = ic_units[iunit]
	if json then begin
		add_history_item, detail, 'IC PV', (*p).IC.PV.name, json=json
		add_history_item, detail, 'IC Sensitivity', (*p).IC.PV.val, json=json
		add_history_item, detail, 'IC Units', unit, json=json
	endif else begin
		ic = '   PV: ' + (*p).IC.PV.name + ', sensitivity = ' + str_tidy((*p).IC.PV.val) + ' (' + unit + ')'
		ic = (*p).IC.PV.name + ', sensitivity = ' + str_tidy((*p).IC.PV.val) + ' (' + unit + ')'
		add_history_item, detail, '   PV', ic, json=json
		index = [index, 170,170]
	endelse
endif

image_flux_charge, p, xanes=xanes, charge=charge, conv=conv, flux=flux, error=error
c = str_tidy(charge)
if conv ne 0.0 then begin
	c = c + ', Total live flux: ' + str_tidy(charge/conv)
	c = c + ' (conversion: ' + str_tidy(conv) + ')'
endif

;	Photon per second: for normal PIXE, SXRF, where yields are in counts/uC
;	we use the charge/time times 6.242e+12. For Maia Mapper, where yields are 
;	in counts/ms, charge equals 'conv' * total dwell (ms). Then we need the 
;	total model spectrum counts per second, scaled by 'conv' for ph/s.

if (*p).has_dwell then begin
	total_dwell = total( *(*p).dwell_map) / 1000.
	ok = (flux ne 0.) ? (abs(1-conv) gt 0.1) : 1
	if (total_dwell gt 0.0) and ok then Begin
		if (unit eq 'ms') then Begin
;			phs = charge * total_beam_per_second / flux
;			c = c + ', p/s: ' + str_tidy(phs)
		endif else begin
			phs = charge * 6.242e+12 / total_dwell
			c = c + ', p/s: ' + str_tidy(phs)
		endelse
	endif
endif
if json then begin
	add_history_item, detail, 'Charge', charge, json=json
	add_history_item, detail, 'Total live flux', charge/conv, json=json
	add_history_item, detail, 'Conversion', conv, json=json
	if (unit ne 'ms') then add_history_item, detail, 'Photon rate', phs, json=json
endif else begin
	add_history_item, detail, '   Charge', c, json=json
	index = [index, 170]
endelse

if (brief eq 0) and (npixels lt bigpixels) then begin
	add_history_item, detail, 'Per pixel', json=json
	index = [index, 111]
	if xanes eq 0 then begin
		if (*p).has_flux and ptr_good( (*p).raw_flux) then Begin
			q1 = qsample( (*p).raw_flux, nq1, min=1.)
			q2 = qsample( (*p).raw_flux, nq2, /positive, min=1., veto_low=1., veto_high=1.)
			if nq2 gt 0 then begin
				m = moment( (*(*p).raw_flux)[q2])
				av = m[0]
				sd = 100. * stddev( (*(*p).raw_flux)[q1])/av
				if json then begin
					add_history_item, detail, '   Raw Flux mean', av, json=json
					add_history_item, detail, '   Raw Flux Std.Dev (%)', sd, json=json
				endif else begin
					add_history_item, detail, '   Raw Flux mean', str_tidy(av) + ', std. dev. (%) = ' + str_tidy(sd), json=json
					index = [index, 111]
				endelse
			endif
		endif
	endif
	if (*p).has_dwell then Begin
		q1 = qsample( (*p).dwell_map, nq1, min=0.001)
;		q2 = qsample( (*p).dwell_map, nq2, /positive, min=0.001, veto_low=0., veto_high=3.)
		q2 = qsample( (*p).dwell_map, nq2, /positive, min=0.001, veto_low=0.2, veto_high=0.2)
		if nq2 gt 0 then begin
			m = moment( (*(*p).dwell_map)[q2])
			av = m[0]

;			window,1
;			wset, 1
;			!y.title='Frequency'
;			!x.title='Dwell time'
;			!p.title='Dwell Time Histogram'
;			h = histogram( (*(*p).dwell_map)[q2],nbins=300,locations=x,min=0.95*av,max=1.05*av)
;
;;			!p.background=spec_colour('black')
;;			!p.color=spec_colour('white')
;;			plot,x,h,ystyle=1,xstyle=1,charsize=1.5,/nodata,charthick=1.4
;;			oplot,x,h,color=spec_colour('green'),thick=1.5
;
;			!p.background=spec_colour('white')
;			!p.color=spec_colour('black')
;			plot,x,h,ystyle=1,xstyle=1,charsize=3.5,/nodata,charthick=2.5,thick=5
;			oplot,x,h,color=spec_colour('green'),thick=5

			sd = 100. * stddev( (*(*p).dwell_map)[q1])/av
			if json then begin
				add_history_item, detail, '   Dwell map mean (ms)', av, json=json
				add_history_item, detail, '   Dwell map Std.Dev (%)', sd, json=json
			endif else begin
				add_history_item, detail, '   Dwell map mean (ms)', str_tidy(av) + ', std. dev. (%) = ' + str_tidy(sd), json=json
				index = [index, 112]
			endelse
		endif
	endif
	if (*p).has_dead then Begin
		q1 = qsample( (*p).dead_fraction, nq1, min=0.0001)
		q2 = qsample( (*p).dead_fraction, nq2, /positive, min=0.0001, veto_low=0., veto_high=3.)
		if nq2 gt 0 then begin
			m = moment( (*(*p).dead_fraction)[q2])
			av = m[0]
			sd = 100. * stddev( (*(*p).dead_fraction)[q1])/av
			av = av / (float((*p).xcompress)*float((*p).ycompress))
			if json then begin
				add_history_item, detail, '   Dead-time fraction mean', av, json=json
				add_history_item, detail, '   Dead-time fraction Std.Dev (%)', sd, json=json
			endif else begin
				add_history_item, detail, '   Dead-time fraction mean', str_tidy(av) + ', std. dev. (%) = ' + str_tidy(sd), json=json
				index = [index, 113]
			endelse
		endif
	endif
	if (*p).has_pileup then Begin
		q1 = qsample( (*p).pileup_map, nq1, min=0.0001)
		q2 = qsample( (*p).pileup_map, nq2, /positive, min=0.0001, veto_low=0., veto_high=3.)
		if nq2 gt 0 then begin
			m = moment( (*(*p).pileup_map)[q2])
			av = m[0]
			sd = 100. * stddev( (*(*p).pileup_map)[q1])/av
			if json then begin
				add_history_item, detail, '   Pile-up losses mean', av, json=json
				add_history_item, detail, '   Pile-up losses Std.Dev (%)', sd, json=json
			endif else begin
				add_history_item, detail, '   Pile-up losses mean', str_tidy(av) + ', std. dev. (%) = ' + str_tidy(sd), json=json
				index = [index, 114]
			endelse
		endif
	endif
	if (*p).has_rates then Begin
		q1 = qsample( (*p).count_rate_map, nq1, min=1.)
		q2 = qsample( (*p).count_rate_map, nq2, /positive, min=1., veto_low=0., veto_high=3.)
		if nq2 gt 0 then begin
			m = moment( (*(*p).count_rate_map)[q2])
			av = m[0]

;			window,0
;			wset,0
;			!y.title='Frequency'
;			!x.title='Count Rate'
;			!p.title='Count Rate Histogram'
;			h = histogram( 1.34*(*(*p).count_rate_map)[q1],nbins=300,locations=x)
;
;;			!p.background=spec_colour('black')
;;			!p.color=spec_colour('white')
;;			plot,x,h,/ylog,ystyle=1,xstyle=1,yrange=[1,8.0e+6],xrange=[0,3.3e+6],charsize=1.5,/nodata,charthick=1.4
;;			oplot,x,h,color=spec_colour('green'),thick=1.5
;
;			!p.background=spec_colour('white')
;			!p.color=spec_colour('black')
;			plot,x,h,/ylog,ystyle=1,xstyle=1,yrange=[1,8.0e+6],xrange=[0,3.3e+6],charsize=3.5,/nodata,charthick=2.5,thick=5
;			oplot,x,h,color=spec_colour('green'),thick=5

			sd = 100. * stddev( (*(*p).count_rate_map)[q1])/av
			if json then begin
				add_history_item, detail, '   Count-rates mean', av, json=json
				add_history_item, detail, '   Count-rates Std.Dev (%)', sd, json=json
			endif else begin
				add_history_item, detail, '   Count-rates mean', str_tidy(av) + ', std. dev. (%) = ' + str_tidy(sd), json=json
				index = [index, 115]
			endelse
		endif
	endif
endif

; Device objects specific history info here ...

s = obj->options_legend()
if s[0] ne '' then begin
	if json then begin
		add_history_item, detail, 'Device', strtrim(s,2), json=json
	endif else begin
		detail = [detail,s]
		index = [index, replicate(500, n_elements(s))]	
	endelse
endif

add_history_item, detail, 'Notes', json=json
add_history_item, detail, '   Sample', (*p).sample, json=json
add_history_item, detail, '   Grain', (*p).grain, json=json
add_history_item, detail, '   Comment', (*p).comment, json=json
index = [index, 45,45,45,45]

if (brief eq 0) then begin
	add_history_item, detail, '--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------', json=json
	if xanes then begin
		if (*p).stack_type eq 1 then begin
			add_history_item, detail, 'Tomography stack frame,stack element', el_xanes, json=json
		endif else if (*p).stack_type eq 2 then begin
			add_history_item, detail, 'XANES stack frame (not E collapsed), XANES element', el_xanes, json=json
		endif else begin
			add_history_item, detail, 'XANES stack frame, XANES element', el_xanes, json=json
		endelse
		index = [index, 800,800]
	endif else begin
		add_history_item, detail, 'Element image', json=json
		index = [index, 800,800]
	endelse
	
	if xanes then begin
		i = show
		s = ((*p).stack_type eq 1) ? '   Angle = ' : '   Energy = '
		if ptr_good((*p).pz_coords) then s = s + str_tidy((*(*p).pz_coords)[i])
		if ptr_good((*p).pz_label) then if n_elements(*(*p).pz_label) gt i then begin
			s = s + ', Source= '+(*(*p).pz_label)[i]
		endif
		add_history_item, detail, '   XANES energies file', (*p).energies_file, json=json
		index = [index, 900,900]
	endif else begin
		i = show
		add_history_item, detail, '   Element', el[i], json=json
		index = [index, 950,950]
	endelse
	if ptr_valid( (*p).history) then begin
		show = show < (n_elements( *(*p).history) - 1)
		if ptr_valid( (*(*p).history)[show]) then begin
			s = *(*(*p).history)[show]
			if json then begin
				add_history_item, detail, '   History', s, json=json
			endif else begin
				detail = [detail, '       ' + s]
				index = [index, 1000+indgen(n_elements(s))]
			endelse
		endif
	endif
endif

if json then detail = string(detail, /implied_print)
return, detail
end
