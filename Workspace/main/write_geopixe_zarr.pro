pro write_geopixe_zarr, p, dir

;	Write a ZARR organization dir tree for Cloud applications
;
;	Write the images given by pointer 'p' to dir 'dir'
;
;	'p' is a pointer pointing to image structs, containing the image details
;	and data.

COMPILE_OPT STRICTARR
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
		warning,'write_geopixe_zarr',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

; Maintain consistency with 'write_geopixe_image' for write in DAI format
; Remember to check for XANES specific code and exit for now ...

zarr_version = 3									; .zarr version number
err = 1

pver = python_version( revision=rev)
if float2(rev) lt 3.4 then goto, bad_python			; zarr only works with python 3.4+

if ptr_good(p) eq 0 then goto, bad_ptr
if n_elements(dir) lt 1 then dir= strip_file_ext( (*p).file) + '.zarr'

xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found
if xanes then begin
	gprint,'write_geopixe_zarr: No XANES export support yet.'
	return
endif

if n_el lt 1 then begin
	gprint,'write_geopixe_zarr: No elements to write.'
	return
endif

if ptr_valid( (*p).error) eq 0 then (*p).has_errors=0
if (*p).has_preview eq 0 then ptr_free, (*p).preview
if (ptr_valid( (*p).preview) eq 0) then begin
	scale = max([float((*p).xsize)/200, float((*p).ysize)/200]) 
	nxp = long((*p).xsize/scale)
	nyp = long((*p).ysize/scale) > 2
	border = max([nxp,nyp]/50) > 1
	preview = smart_congrid( (*(*p).image)[*,*,q[1<(nq-1)]]>0, nxp, nyp)
	if n_el gt 2 then begin
		for i=2,(4<nq)-1 do begin
			preview = preview + smart_congrid( (*(*p).image)[*,*,q[i]]>0, nxp, nyp)
		endfor
	endif
	trimx = border < nxp/5
	trimy = border < nyp/5
	if trimx gt 0 then begin
		preview[0:trimx-1,*] = 0
		preview[nxp-trimx:nxp-1,*] = 0
	endif
	if trimy gt 0 then begin
		preview[*,0:trimy-1] = 0
		preview[*,nyp-trimy:nyp-1] = 0
	endif
	(*p).preview = ptr_new(preview, /no_copy)
	(*p).has_preview = 1
endif

; Note: Due to python problems with numpy variables in JSON serialize, all attributes passed
;		as strings only.
;
; As requested by Sam Bradley, structure flattened, and all attributes are now
; output at root level.

gprint,'write_geopixe_zarr: import zarr, numcodecs ...'
zarr = python.import('zarr')
numcodecs = python.import('numcodecs')

gprint,'write_geopixe_zarr: open output dir ['+dir+']...'
root = zarr.open( dir, mode='w')
python.root = root

gprint,'write_geopixe_zarr: write attributes ...'
root.attrs['version.zarr'] = str_tidy(zarr_version)	
root.attrs['version.dai'] = str_tidy((*p).version)
	
root.attrs['source'] = (*p).source
root.attrs['source2'] = (*p).source2
root.attrs['throttle'] = (*p).throttle
root.attrs['pileup'] = (*p).pileup
root.attrs['linearize'] = (*p).linearize
root.attrs['sample'] = (*p).sample
root.attrs['grain'] = (*p).grain
root.attrs['comment'] = (*p).comment
root.attrs['facility'] = (*p).facility
root.attrs['endstation'] = (*p).endstation
root.attrs['energy'] = str_tidy((*p).energy)
root.attrs['n_el'] = str_tidy((*p).n_el)
root.attrs['n_attributes'] = str_tidy((*p).n_attributes)
;root.attrs['element'] = [*(*p).el]

gprint,'write_geopixe_zarr: create els ...'
python.nel = n_elements(*(*p).el)
python.els = *(*p).el
python.run, 'element = root.create_dataset( "element", shape=nel, dtype="str")'
python.run, 'element[:] = els'
python.run, 'element.attrs["_ARRAY_DIMENSIONS"] = ["element"]'

;device = root.create_group('device')
root.attrs['device.flipx'] = str_tidy(0)						; need to get options from device later

;method = root.create_group('method')
root.attrs['method.type'] = str_tidy((*p).type)
root.attrs['method.mode'] = str_tidy((*p).mode)
root.attrs['method.corrected'] = str_tidy((*p).corrected)
root.attrs['method.flatten'] = str_tidy((*p).flatten)

;matrix = method.create_group('matrix')
root.attrs['matrix.label'] = (*p).matrix.label
root.attrs['matrix.file'] = (*p).matrix.file
root.attrs['matrix.charge'] = str_tidy((*p).matrix.charge)

;if ptr_good((*p).matrix.mdl) then begin
;	root.attrs['matrix.mdl'] = str_tidy([*(*p).matrix.mdl])
;endif

gprint,'write_geopixe_zarr: create matrix ...'
python.nmdl = n_elements(*(*p).matrix.mdl)
python.temp = *(*p).matrix.mdl
python.run, 'mdl = root.create_dataset( "mdl", shape=nmdl, dtype="f4")'
python.run, 'mdl[:] = temp'
python.run, 'mdl.attrs["_ARRAY_DIMENSIONS"] = ["element"]'
python.run, 'mdl.attrs["long_name"] = "Detection limits"
python.run, 'mdl.attrs["units"] = "ppm"

;scanning = root.create_group('scanning')
root.attrs['scanning.xcompress'] = str_tidy((*p).xcompress)
root.attrs['scanning.ycompress'] = str_tidy((*p).ycompress)
root.attrs['scanning.original_xsize'] = str_tidy((*p).original_xsize)
root.attrs['scanning.original_ysize'] = str_tidy((*p).original_ysize)
root.attrs['scanning.xsize'] = str_tidy((*p).xsize)
root.attrs['scanning.ysize'] = str_tidy((*p).ysize)
root.attrs['scanning.scale_x'] = str_tidy((*p).scaled_x)
root.attrs['scanning.scaled_y'] = str_tidy((*p).scaled_y)
root.attrs['scanning.sub_region'] = str_tidy((*p).sub_region)
root.attrs['scanning.xoffset'] = str_tidy((*p).xoffset)
root.attrs['scanning.yoffset'] = str_tidy((*p).yoffset)
root.attrs['scanning.x_sub_range'] = str_tidy((*p).x_sub_range)
root.attrs['scanning.y_sub_range'] = str_tidy((*p).y_sub_range)

;scan = scanning.create_group('scan')
root.attrs['scanning.scan.x'] = str_tidy((*p).scan.x)
root.attrs['scanning.scan.y'] = str_tidy((*p).scan.y)
;origin = scan.create_group('origin')
root.attrs['scanning.scan.origin.x'] = str_tidy((*p).scan.origin.x)
root.attrs['scanning.scan.origin.y'] = str_tidy((*p).scan.origin.y)

;dwell = scanning.create_group('dwell')
root.attrs['scanning.dwell.on'] = str_tidy((*p).dwell.on)
root.attrs['scanning.dwell.val'] = str_tidy((*p).dwell.val)

;bounds = scanning.create_group('bounds')
root.attrs['scanning.bounds.valid'] = str_tidy((*p).bounds.valid)
root.attrs['scanning.bounds.xmin'] = str_tidy((*p).bounds.xmin)
root.attrs['scanning.bounds.xmax'] = str_tidy((*p).bounds.xmax)
root.attrs['scanning.bounds.ymin'] = str_tidy((*p).bounds.ymin)
root.attrs['scanning.bounds.ymax'] = str_tidy((*p).bounds.ymax)

;detect = root.create_group('detector')
root.attrs['detect.channel'] = str_tidy((*p).channel)
root.attrs['detect.detector'] = str_tidy((*p).detector)
root.attrs['detect.ecompress'] = str_tidy((*p).ecompress)
root.attrs['detect.array'] = str_tidy((*p).array)

gprint,'write_geopixe_zarr: create pactive ...'
if ptr_good((*p).pactive) then begin
	python.nactive = n_elements(*(*p).pactive)
	python.temp = *(*p).pactive
	python.run, 'pactive = root.create_dataset( "pactive", shape=nactive, dtype="u1")'
	python.run, 'pactive[:] = temp'
	python.run, 'pactive.attrs["_ARRAY_DIMENSIONS"] = ["chan_active"]'
	python.run, 'pactive.attrs["long_name"] = "Active detector lookup"
endif

;cali = detect.create_group('cal')
root.attrs['detect.cali.order'] = str_tidy((*p).cal.order)
root.attrs['detect.cali.units'] = str_tidy((*p).cal.units)
root.attrs['detect.cali.poly'] = str_tidy([(*p).cal.poly])

gprint,'write_geopixe_zarr: create pcal ...'
if ptr_good((*p).pcal) then begin
	nc = n_elements(*(*p).pcal)
	pcal = root.create_dataset('pcal', shape=[nc], dtype='object', object_codec=numcodecs.JSON())
	for i=0,nc-1 do begin
		r = pcal.__setitem__(i, hash('a',str_tidy((*(*p).pcal)[i].poly[1]), 'b',str_tidy((*(*p).pcal)[i].poly[0]), 'order',str_tidy((*(*p).pcal)[i].order), 'units',str_tidy((*(*p).pcal)[i].units)))
	endfor
	pcal.attrs['_ARRAY_DIMENSIONS'] = ['chan_active']
	pcal.attrs["long_name"] = "Active Detector cals"
endif

;norma = root.create_group('norm')
root.attrs['norma.charge'] = str_tidy((*p).channel)

;ic = norma.create_group('ic')
root.attrs['norma.ic.mode'] = str_tidy((*p).IC.mode)
root.attrs['norma.ic.conversion'] = str_tidy((*p).IC.conversion)
;pv = ic.create_group('pv')
root.attrs['norma.ic.pv.name'] = (*p).IC.pv.name
root.attrs['norma.ic.pv.val'] = str_tidy((*p).IC.pv.val)
root.attrs['norma.ic.pv.unit'] = str_tidy((*p).IC.pv.unit)

;stats = root.create_group('stats')
root.attrs['stats.processed'] = str_tidy((*p).processed)
root.attrs['stats.valid'] = str_tidy((*p).valid)
root.attrs['stats.bad_xy'] = str_tidy((*p).bad_xy)
root.attrs['stats.clipped'] = str_tidy((*p).clipped)
root.attrs['stats.has_flux'] = str_tidy((*p).has_flux)
root.attrs['stats.has_dwell'] = str_tidy((*p).has_dwell)
root.attrs['stats.has_dead'] = str_tidy((*p).has_dead)
root.attrs['stats.has_pileup'] = str_tidy((*p).has_pileup)
root.attrs['stats.has_rates'] = str_tidy((*p).has_rates)

gprint,'write_geopixe_zarr: create hist ...'
if ptr_good((*p).hist) then begin
	python.nh = n_elements(*(*p).hist)
	python.temp = *(*p).hist
	python.run, 'hist = root.create_dataset( "hist", shape=nh, dtype="i4")'
	python.run, 'hist[:] = temp'
	python.run, 'hist.attrs["_ARRAY_DIMENSIONS"] = ["channel"]'
	python.run, 'hist.attrs["long_name"] = "Detector counts histogram"
	python.run, 'hist.attrs["units"] = "Counts"
endif

python.nx = (*p).xsize
python.ny = (*p).ysize

if (*p).has_flux then begin
	gprint,'write_geopixe_zarr: create flux ...'
	python.temp = *(*p).flux
	python.run, 'flux = root.create_dataset( "flux", shape=(ny,nx), order="C", chunks=(1000,1000), dtype="f4")'
	python.run, 'flux[:,:] = temp'
	python.run, 'flux.attrs["_ARRAY_DIMENSIONS"] = ["y","x"]'
	python.run, 'flux.attrs["long_name"] = "Flux map"
;	python.run, 'flux.attrs["units"] = "fraction"
	python.temp = *(*p).raw_flux
	python.run, 'raw_flux = root.create_dataset( "raw_flux", shape=(ny,nx), order="C", chunks=(1000,1000), dtype="f4")'
	python.run, 'raw_flux[:,:] = temp'
	python.run, 'raw_flux.attrs["_ARRAY_DIMENSIONS"] = ["y","x"]'
	python.run, 'raw_flux.attrs["coordinates"] = "yc xc"'
	python.run, 'raw_flux.attrs["long_name"] = "Raw flux map"
;	python.run, 'raw_flux.attrs["units"] = "fraction"
endif
if (*p).has_dead then begin
	gprint,'write_geopixe_zarr: create dead ...'
	python.temp = *(*p).dead_fraction
	python.run, 'dead_fraction = root.create_dataset( "dead_fraction", shape=(ny,nx), order="C", chunks=(1000,1000), dtype="f4")'
	python.run, 'dead_fraction[:,:] = temp'
	python.run, 'dead_fraction.attrs["_ARRAY_DIMENSIONS"] = ["y","x"]'
	python.run, 'dead_fraction.attrs["coordinates"] = "yc xc"'
	python.run, 'dead_fraction.attrs["long_name"] = "Dead time maps"
	python.run, 'dead_fraction.attrs["units"] = "fraction"
endif
if (*p).has_dwell then begin
	gprint,'write_geopixe_zarr: create dwell ...'
	python.temp = *(*p).dwell_map
	python.run, 'dwell_map = root.create_dataset( "dwell_map", shape=(ny,nx), order="C", chunks=(1000,1000), dtype="f4")'
	python.run, 'dwell_map[:,:] = temp'
	python.run, 'dwell_map.attrs["_ARRAY_DIMENSIONS"] = ["y","x"]'
	python.run, 'dwell_map.attrs["coordinates"] = "yc xc"'
	python.run, 'dwell_map.attrs["long_name"] = "Dwell time maps"
	python.run, 'dwell_map.attrs["units"] = "ms"
endif
if (*p).has_pileup then begin
	gprint,'write_geopixe_zarr: create pileup ...'
	python.temp = *(*p).pileup_map
	python.run, 'pileup_map = root.create_dataset( "pileup_map", shape=(ny,nx), order="C", chunks=(1000,1000), dtype="f4")'
	python.run, 'pileup_map[:,:] = temp'
	python.run, 'pileup_map.attrs["_ARRAY_DIMENSIONS"] = ["y","x"]'
	python.run, 'pileup_map.attrs["coordinates"] = "yc xc"'
	python.run, 'pileup_map.attrs["long_name"] = "Pileup proportion maps"
	python.run, 'pileup_map.attrs["units"] = "fraction"
endif
if (*p).has_rates then begin
	gprint,'write_geopixe_zarr: create rates ...'
	python.temp = *(*p).count_rate_map
	python.run, 'count_rate_map = root.create_dataset( "count_rate_map", shape=(ny,nx), order="C", chunks=(1000,1000), dtype="f4")'
	python.run, 'count_rate_map[:,:] = temp'
	python.run, 'count_rate_map.attrs["_ARRAY_DIMENSIONS"] = ["y","x"]'
	python.run, 'count_rate_map.attrs["coordinates"] = "yc xc"'
	python.run, 'count_rate_map.attrs["long_name"] = "count rate maps"
	python.run, 'count_rate_map.attrs["units"] = "counts/s"
endif

;display = root.create_group('display')
root.attrs['display.has_preview'] = str_tidy((*p).has_preview)

;python.display = display
if (*p).has_preview then begin
	gprint,'write_geopixe_zarr: create preview ...'
	python.npx = n_elements((*(*p).preview)[*,0])
	python.npy = n_elements((*(*p).preview)[0,*])
	python.temp = *(*p).preview
	python.run, 'preview = root.create_dataset( "preview", shape=(npy,npx), order="C", dtype="f4")'
	python.run, 'preview[:,:] = temp'
	python.run, 'preview.attrs["_ARRAY_DIMENSIONS"] = ["y_preview","x_preview"]'
endif

if ptr_good((*p).options) then begin
	gprint,'write_geopixe_zarr: create options ...'
	nc = n_elements(*(*p).options)
	options = root.create_dataset('options', shape=[nc], dtype='object', object_codec=numcodecs.JSON())
	for i=0,nc-1 do begin
		r = options.__setitem__(i, hash('min',str_tidy((*(*p).options)[i].min), 'max',str_tidy((*(*p).options)[i].max), 'bottom',str_tidy((*(*p).options)[i].bottom), 'top',str_tidy((*(*p).options)[i].top), 'interp',str_tidy((*(*p).options)[i].interp), 'log',str_tidy((*(*p).options)[i].log)))
	endfor
	options.attrs['_ARRAY_DIMENSIONS'] = ['element']
	options.attrs["long_name"] = "Image display parameters"
endif
if ptr_good((*p).escale) then begin
	gprint,'write_geopixe_zarr: create escale ...'
	nc = n_elements(*(*p).escale)
	escale = root.create_dataset('escale', shape=[nc], dtype='object', object_codec=numcodecs.JSON())
	for i=0,nc-1 do begin
		r = escale.__setitem__(i, hash('min',str_tidy((*(*p).escale)[i].min), 'max',str_tidy((*(*p).escale)[i].max), 'bottom',str_tidy((*(*p).escale)[i].bottom), 'top',str_tidy((*(*p).escale)[i].top), 'interp',str_tidy((*(*p).escale)[i].interp), 'log',str_tidy((*(*p).escale)[i].log)))
	endfor
	escale.attrs['_ARRAY_DIMENSIONS'] = ['element']
	escale.attrs["long_name"] = "Variance display parameters"
endif

;maps = root.create_group('maps')
root.attrs['maps.has_phase'] = str_tidy((*p).has_phase)
root.attrs['maps.has_yield'] = str_tidy((*p).has_yield)
root.attrs['maps.has_errors'] = str_tidy((*p).has_errors)

nxe = ((*p).xsize+1)/2
nye = ((*p).ysize+1)/2

;python.maps = maps
python.nxe = nxe
python.nye = nye

if (*p).has_yield then begin
	gprint,'write_geopixe_zarr: create yield ...'
	nz = n_elements( (*(*p).yield)[0,0,*])
	python.nz = nz
	python.run, 'yields = root.create_dataset( "yield", shape=(nz,nye,nxe), order="C", chunks=(1,500,500), dtype="f4")'
	for i=0,nz-1 do begin
		python.temp = (*(*p).yield)[*,*,i]
		python.i = i
		python.run, 'yields[i,:,:] = temp'			; note: Could not use variable "yield" in python
	endfor
	python.run, 'yields.attrs["_ARRAY_DIMENSIONS"] = ["element","y2","x2"]'
	python.run, 'yields.attrs["long_name"] = "Yield maps"
	python.run, 'yields.attrs["units"] = "counts/ppm.uC"
endif

if (*p).has_errors then begin
	gprint,'write_geopixe_zarr: create errors ...'
	nz = n_elements( (*(*p).error)[0,0,*])
	python.nz = nz
	python.run, 'error = root.create_dataset( "error", shape=(nz,nye,nxe), order="C", chunks=(1,500,500), dtype="f4")'
	for i=0,nz-1 do begin
		python.temp = (*(*p).error)[*,*,i]
		python.i = i
		python.run, 'error[i,:,:] = temp'
	endfor
	python.run, 'error.attrs["_ARRAY_DIMENSIONS"] = ["element","y2","x2"]'
	python.run, 'error.attrs["long_name"] = "Error maps"
	python.run, 'error.attrs["units"] = "variance"
endif

if (*p).has_phase then begin
	gprint,'write_geopixe_zarr: create phase ...'
	nz = n_elements( (*(*p).phase)[0,0,*])
	python.nz = nz
	python.run, 'phase = root.create_dataset( "phase", shape=(nz,ny,nx), order="C", chunks=(1,1000,1000), dtype="f4")'
	for i=0,nz-1 do begin
		python.temp = (*(*p).phase)[*,*,i]
		python.i = i
		python.run, 'phase[i,:,:] = temp'
	endfor
	python.run, 'phase.attrs["_ARRAY_DIMENSIONS"] = ["phase","y","x"]'
	python.run, 'phase.attrs["coordinates"] = "phase yc xc"'
	python.run, 'phase.attrs["long_name"] = "Phase maps"
	python.run, 'phase.attrs["units"] = "fraction"
endif

gprint,'write_geopixe_zarr: create image ...'
nz = n_elements( (*(*p).image)[0,0,*])
python.nz = nz
python.run, 'image = root.create_dataset( "image", shape=(nz,ny,nx), order="C", chunks=(1,1000,1000), dtype="f4")'
for i=0,nz-1 do begin
	python.temp = (*(*p).image)[*,*,i]
	python.i = i
	python.run, 'image[i,:,:] = temp'
endfor
python.run, 'image.attrs["_ARRAY_DIMENSIONS"] = ["element","y","x"]'
python.run, 'image.attrs["coordinates"] = "element yc xc"'
python.run, 'image.attrs["long_name"] = "Concentration images"
python.run, 'image.attrs["units"] = "ppm uC"

nx = (*p).xsize
ny = (*p).ysize
x = findgen(nx)
y = findgen(ny)
munits = 'pixels'
scx = (*p).scaled_x lt 0.01 ? 1.0 : (*p).scaled_x 
scy = (*p).scaled_y lt 0.01 ? 1.0 : (*p).scaled_y

if ((*p).scan.x gt 0.0001) and ((*p).scan.y gt 0.0001) then begin
	munits = 'mm'
	mx = float(x) * float((*p).scan.x) / float((*p).original_xsize * scx)
	my = float(y) * float((*p).scan.y) / float((*p).original_ysize * scy)
endif

gprint,'write_geopixe_zarr: create coords ...'
python.run, 'xc = root.create_dataset( "xc", shape=(nx), order="C", chunks=(1000), dtype="f4")'
python.temp = mx
python.munits = munits
python.run, 'xc[:] = temp'
python.run, 'xc.attrs["_ARRAY_DIMENSIONS"] = ["x"]'
python.run, 'xc.attrs["long_name"] = "X stage coordinate"
python.run, 'xc.attrs["units"] = munits
	
python.run, 'yc = root.create_dataset( "yc", shape=(ny), order="C", chunks=(1000), dtype="f4")'
python.temp = my
python.run, 'yc[:] = temp'
python.run, 'yc.attrs["_ARRAY_DIMENSIONS"] = ["y"]'
python.run, 'yc.attrs["long_name"] = "Y stage coordinate"
python.run, 'yc.attrs["units"] = munits

finish:
	gprint,'write_geopixe_zarr: cleanup ...'
	python.temp = 0
	if obj_valid(maps) then obj_destroy, maps
	if obj_valid(display) then obj_destroy, display
	if obj_valid(stats) then obj_destroy, stats
	if obj_valid(pv) then obj_destroy, pv
	if obj_valid(ic) then obj_destroy, ic
	if obj_valid(norma) then obj_destroy, norma
	if obj_valid(cali) then obj_destroy, cali
	if obj_valid(detect) then obj_destroy, detect
	if obj_valid(bounds) then obj_destroy, bounds
	if obj_valid(dwell) then obj_destroy, dwell
	if obj_valid(origin) then obj_destroy, origin
	if obj_valid(scan) then obj_destroy, scan
	if obj_valid(scanning) then obj_destroy, scanning
	if obj_valid(matrix) then obj_destroy, matrix
	if obj_valid(method) then obj_destroy, method
	if obj_valid(device) then obj_destroy, device
	if obj_valid(root) then obj_destroy, root
	gprint,'write_geopixe_zarr: All done.'
	return

bad_python:
	gprint,'write_geopixe_zarr: Unsupported version of Python used.'
	goto, finish
bad_open:
	gprint,'write_geopixe_zarr: Error opening file: "'+dir[0]+'"'
	goto, finish
bad_io:
	gprint,'write_geopixe_zarr: I/O error'
	goto, finish
bad_obj_io:
	gprint,'write_geopixe_zarr: Object specific I/O error'
	goto, finish
bad_flux_io:
	gprint,'write_geopixe_zarr: No "raw flux" map found. Abort Image Save.'
	goto, finish
bad_ptr:
	gprint,'write_geopixe_zarr: bad image pointer.'
	goto, finish
bad_file:
	gprint,'write_geopixe_zarr: no file name supplied'
	goto, usage

usage:
	print,'write_geopixe_zarr: Usage: write_geopixe_zarr, p, dir'
	print,'		where "p" is pointer to image struct'
	print,'		and "dir" is the name of the ZARR dir tree to create.'
	goto, finish
end
