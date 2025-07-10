function read_geopixe_image, file, header=header, version=version, error=err, $
			xanes=xanes, ignore=ignore, silent=silent, $
			memory_compress_applied=memory_compress_applied
;
;	Read the images file 'file'
;
;	Return 'p', a pointer pointing to an
;	image struct, containing the images details
;	and data. See "define.pro" for details.
;
;	/header		just read header information, not images.
;	/xanes		read a XANES image stack file
;	/ignore		ignore nulls issue in dai read.

err = 1
if n_elements(silent) lt 1 then silent=0
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
		if (silent eq 0) then begin
			warning,'read_geopixe_image',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c,'','Try using "Load (ignore nulls)".'], /error
		endif
		MESSAGE, /RESET
		goto, bad_io
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_null_image_1, max_image_cal
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384

if n_elements(file) lt 1 then goto, bad_file
if n_elements(header) lt 1 then header=0
if n_elements(ignore) lt 1 then ignore=0
if n_elements(xanes) lt 1 then begin
	xanes = (extract_extension(strlowcase(file)) eq 'xan') ? 1 : 0
	temp = strip_file_ext(file)
	if xanes eq 0 then xanes = (extract_extension(strlowcase(temp)) eq 'xan') ? 1 : 0
endif
image_routines			; make sure these are compiled.

p = 0
on_ioerror, bad_io
close, 1
;if strlowcase(extract_extension(file)) ne 'dai' then goto, bad_file
;s = strip_file_ext( file) + '.dai'

; If use /compress then image files are about 10% smaller,
; but saving takes longer.
; Need /compress here and in write_geopixe_image.pro.
; Note that using /compress here seems to work with files that
; have not used compression (no /compress in write_geopixe_image.pro).

on_ioerror, bad_open
openr, 1, file, /XDR			;, /compress
on_ioerror, bad_io

if xanes then begin
	image = define(/stack)
endif else begin
	image = define(/image)
endelse
image.file = file

;------- basic image file data ----------------------------------

; Remember to check for XANES specific code, or Image only code ...

valid = [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26, $
				-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-46,-47,-48,-49, $
				-50,-51,-52,-53,-54,-55,-56,-57]
version = 0L
readu,1, version
q = where( version eq valid)
if q[0] eq -1 then goto, bad_version
image.version = version

s = ''
readu,1, s
image.source = s

x = 0.0
readu,1, x
image.charge = x

xsize = 0L
ysize = 0L
readu,1, xsize,ysize

if version le -15 then begin
	xesize = 0L
	yesize = 0L
	readu,1, xesize,yesize
endif else if version le -14 then begin
	xesize = (xsize+1)/2
	yesize = (ysize+1)/2
endif else begin
	xesize = xsize/2
	yesize = ysize/2
endelse
print,'version 0: xsize=',xsize,' ysize=',ysize,' charge=',image.charge

xsize1 = xsize
ysize1 = ysize
xesize1 = xesize
yesize1 = yesize
memory_compress_applied = 0
nsq = 1LL

big = !version.os_family eq 'unix' ? 16 : 4 						; 5.3			; 4.2
if (header eq 0) and (!version.memory_bits eq 32) then begin
	while (xsize/nsq)*(ysize/nsq) gt big*1000000LL do nsq=nsq+1
	if nsq ge 2 then begin	
		xsize1 = long(xsize/nsq)
		ysize1 = long(ysize/nsq)
		xesize1 = long(xesize/nsq)
		yesize1 = long(yesize/nsq)
		memory_compress_applied = nsq
		if silent eq 0 then begin
			warning,'read_geopixe_image',['Unable to allocate enought memory for large images in 32-bit O/S.', $
					'Limit Windows to 4M pixels and other O/S to 16M pixels.', '', $
					'Re-compressing images to fit in 32-bit memory ...']
		endif
	endif
	print,'!version.memory_bits = 32: xsize1=',xsize1,' ysize1=',ysize1,' nsq=',nsq
endif

image.xsize = xsize1
image.ysize = ysize1
original_xsize = xsize1
original_ysize = ysize1
sub_region = 0
xoffset = 0L
yoffset = 0L
x_sub_range = xsize
y_sub_range = ysize

if version le -36 then begin
	readu,1, xoffset, yoffset
endif
image.xoffset = xoffset
image.yoffset = yoffset

if (xanes eq 0) or (version le -51) then begin
	if (xoffset ne 0) or (yoffset ne 0) then sub_region=1
	if version le -43 then begin
		readu,1, sub_region
		readu,1, x_sub_range, y_sub_range
	endif
	image.sub_region = sub_region
	image.x_sub_range = x_sub_range
	image.y_sub_range = y_sub_range
endif

cal_a = 0.0
cal_b = 0.0
cal_units = 'keV'
readu,1, cal_a, cal_b
if cal_a lt 0.002 then cal_units='MeV'
if version le -17 then readu,1, cal_units
image.cal.poly[1] = cal_a
image.cal.poly[0] = cal_b
image.cal.units = cal_units
image.cal.order = 1

ecompress = 1L
if version le -14 then readu,1, ecompress
image.ecompress = ecompress

if version le -44 then begin
	processed = 0LL
	valid = 0LL
	bad_xy = 0LL
	clipped = 0LL
endif else begin
	processed = 0L
	valid = 0L
	bad_xy = 0L
	clipped = 0L
endelse
if (xanes eq 0) or (version le -51) then begin
	readu,1, processed, valid, bad_xy, clipped
	image.processed = processed
	image.valid = valid
	image.bad_xy = bad_xy
	image.clipped = clipped
endif

scanx = 0.0
scany = 0.0
scanz = 0.0
ox = 0.0
oy = 0.0
oz = 0.0
nxc = 0L
nyc = 0L
nzc = 0L
nz2 = 0L
sx = ''
sy = ''
sz = ''
readu,1, scanx, scany
image.scan.x = scanx
image.scan.y = scany
if version le -28 then begin
	readu,1,ox,oy
	image.scan.origin.x = ox
	image.scan.origin.y = oy
endif
if version le -31 then begin
	readu,1, nxc, nyc
	print,'version -31: X coords=',nxc,' Y coords=',nyc
	if nxc gt 1 then begin
		x_coords = dblarr(nxc)
		readu,1, x_coords
		image.px_coords = ptr_new(x_coords)
	endif
	if nyc gt 1 then begin
		y_coords = dblarr(nyc)
		readu,1, y_coords
		image.py_coords = ptr_new(y_coords)
	endif
endif
if version le -32 then begin
	readu,1,sx,sy
	image.x_coord_units = sx
	image.y_coord_units = sy
	print,'version -32: X units=',sx,' Y units=',sy
endif
if xanes then begin
	readu,1, nzc
	print,'xanes: Z coords=',nzc
	if nzc gt 1 then begin
		z_coords = fltarr(nzc)
		readu,1, z_coords
		image.pz_coords = ptr_new(z_coords)
	endif
	readu,1,sz
	image.z_coord_units = sz
	print,'xanes: Z units=',sz

	readu,1, nz2
	if nz2 gt 1 then begin
		z_labels = strarr(nz2)
		readu,1, z_labels
		image.pz_label = ptr_new(z_labels)
	endif
	
	if version le -49 then begin
		readu,1, scanz, oz
		image.scan.z = scanz
		image.scan.origin.z = oz
	endif
endif

readu,1, s
image.sample = s
readu,1, s
image.grain = s
readu,1, s
image.comment = s
if version le -56 then begin
	readu,1, s
	image.facility = s
	readu,1, s
	image.endstation = s
endif

readu,1, s
image.matrix.label = s
print,'read_geopixe_image: matrix label=',s

n_el = 0L
el_xanes = ''
ixanes = 0
stack_type = 0
energy_proxy_axis = 0
readu,1, n_el
if xanes then begin
	image.zsize = n_el
	original_zsize = image.zsize
	readu,1, el_xanes, ixanes
	image.el = el_xanes
	image.ixanes = ixanes
	if version le -49 then begin
		readu,1, s
		image.energies_file = s
		print,'read_geopixe_image: energies_file=',s
	endif
	if version le -53 then begin
		readu,1, stack_type
		image.stack_type = stack_type
;		print,'read_geopixe_image: stack_type=',stack_type
	endif
endif else begin
	image.n_el = n_el
	el = strarr(image.n_el)
	readu,1, el
	image.el = ptr_new(el, /no_copy)
	if version le -57 then begin
		readu,1, energy_proxy_axis
		image.energy_proxy_axis = energy_proxy_axis
		readu,1, s
		image.energies_file = s
		print,'read_geopixe_image: energies_file=',s
	endif
endelse

readu,1, x
image.matrix.charge = x

; Have disabled this fiddle to be able to load old RT images. 
; New RT images should have all this set correctly anyway.
 
if xanes eq 0 then begin
	null_matrix = ((image.matrix.label eq '') or (image.matrix.charge eq 0.0))
	if ignore or NOT null_matrix then begin
		mdl = fltarr(image.n_el)
		readu,1, mdl
		image.matrix.mdl = ptr_new(mdl, /no_copy)
	endif else begin
		if silent eq 0 then begin
			if image.n_el lt 100 then begin				; fudge to avoid data cube case with no matrix
				warning,'read_geopixe_image',['Null matrix label or zero matrix charge encountered, skip MDL read.', $
						'If this does not work, try "Load (ignore nulls)".']
			endif
		endif
	endelse
endif

img = 0
if (header eq 0) or (version ge -7) then img = fltarr(xsize1,ysize1,n_el)
error = 0

; With version -8, image is read after all the version parameters

frame = fltarr(xsize,ysize)
if (version ge -7) then begin
	for i=0L,n_el-1 do begin
		readu,1, frame
		img[*,*,i] = rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
	endfor
endif

ystep = 0L
xstep_on = 0L
xcompress = long(nsq)
ycompress = long(nsq)
zcompress = 1
xstep = 0L
step_events = 0L
events = 0L
step_toggle = 0L
toggle_bit = 12L
toggle_station = 2L
type = 0L
channel = 0L
detector = 0L
corrected = 0L
has_errors = 0L
scaled_x = 1.0
scaled_y = 1.0
show_back = 0L
device = 0L
device_name = 'MPSYS_DEVICE'
source2 = ''
throttle = ''
pileup = ''
linearize = ''
array = 0L
active = 0L
n_active = 1L
bounds =  { valid: 0, xmin: 0, xmax: 0, ymin: 0, ymax: 0 }
has_flux = 0L
has_preview = 0L
has_dead = 0L
has_dwell = 0L
has_pileup = 0L
has_rates = 0L
has_phase = 0L
has_yield = 0L
encoder_y_correct = 0
x_margin = 0
clear_x = 0
clear_y = 0
energy = 0.0
n_attributes = 0L

; version -2 stuff ...

if version le -2 then begin
	if version le -16 then readu,1, ystep
	readu,1, xstep_on
	readu,1, xcompress, ycompress
	if xanes and (version le -53) then begin
		readu,1, zcompress
		image.zcompress = zcompress
	endif
	readu,1, xstep
	image.ystep = ystep
	image.xstep_on = xstep_on
	image.xcompress = xcompress * nsq
	image.ycompress = ycompress * nsq
	image.xstep = xstep
	print,'version -2: xstep_on=',xstep_on,' xstep=',xstep,' xcompress=',xcompress
endif

if xanes eq 0 then begin
	if version gt -43 then begin
		image.x_sub_range = x_sub_range * xcompress
		image.y_sub_range = y_sub_range * ycompress
	endif
endif

if xanes eq 0 then begin
	if (silent eq 0) and null_matrix and ((ystep gt 2) or (xstep_on gt 2) or (xcompress gt 100) or (ycompress gt 100)) then begin
		warning,'read_geopixe_image',['Illegal parameters encountered,','after null matrix details were skipped.', $
					'','To correct this, try loading image using "ignore nulls" option.', $
					'and then save the image again.']
	endif
endif

; version -3 stuff ...

if version le -3 then begin
	readu,1,s
	image.matrix.file = s
	print,'version -3: matrix.file=',s
endif

; version -4 stuff ...

if version le -4 then begin
	readu,1,step_events
	image.step_events = step_events
	print,'version -4: step_events=',step_events
endif

; version -5 stuff ...

if version le -5 then begin
	readu,1,events
	image.events = events
	print,'version -5: events=',events
endif

; version -6 stuff ...

if version le -6 then begin
	readu,1, step_toggle, toggle_bit, toggle_station
	image.step_toggle = step_toggle
	image.toggle_bit = toggle_bit
	image.toggle_station = toggle_station
	print,'version -6: step_toggle=',step_toggle, toggle_bit, toggle_station
endif

; version -7 stuff ...

if version le -7 then begin
	readu,1, type
	image.type = type
	print,'version -7: type=',type
endif

; version -22 stuff ...

mode = 0L
ext = extract_extension( image.matrix.file)
if strlowcase(ext) eq 'cuts' then mode=1L

if version le -22 then begin
	readu,1, mode
	print,'version -22: mode=',mode
endif
image.mode = mode

; version -8 stuff ...

if version le -8 then begin
	readu,1, channel, detector
	image.channel = channel < (geopixe_max_adcs-1)
	image.detector = detector
	if image.detector eq 7 then image.IC.mode=2
	print,'version -8: channel=',channel, ' detector=',detector
endif

; version -9 stuff ...

if (xanes eq 0) and (version le -9) then begin
	readu,1, corrected
	image.corrected = corrected
	print,'version -9: corrected=',corrected
endif

; version -10 - the error image details

if version le -10 then begin
	readu,1, has_errors
	image.has_errors = has_errors
	print,'version -10: has_errors=',has_errors
	if (silent eq 0) and has_errors and ((xesize le 0) or (yesize le 0)) then begin
		warning,'read_geopixe_image',['Inconsistency between error array and size.', $
				'Image file corrupt.','','You will need to re-sort the list-mode data.']
		goto, error
	endif
	if (header eq 0) and (has_errors eq 1) then error = fltarr(xesize1,yesize1,n_el)
endif

; version -11 - the scaled image details

if version le -11 then begin
	readu,1, original_xsize, original_ysize
	if xanes and (version le -54) then begin
		readu,1, original_zsize
	endif
	readu,1, scaled_x, scaled_y
	readu,1, show_back
	image.scaled_x = scaled_x
	image.scaled_y = scaled_y
	image.show_back = show_back
	print,'version -10: scaled_x,y=',scaled_x, scaled_y
	print,'		original xsize, ysize=', original_xsize, original_ysize
endif
image.original_xsize = original_xsize / nsq
image.original_ysize = original_ysize / nsq
if xanes then image.original_zsize = original_zsize

if xanes eq 0 then begin
	if version gt -43 then begin
		if (image.x_sub_range/image.xcompress ne image.original_xsize) or $
						(image.y_sub_range/image.ycompress ne image.original_ysize) then begin
			image.sub_region = 1
		endif
	endif
endif

; Before version -34 and Object Devices, a fixed device number was read/written.
; Now save the Object Device_name, and lookup equivalent device
; number (which depend on device order on local system) on read.

if version le -12 then begin
	if version le -34 then begin
		readu,1, device_name
		print,'version -34: device_name=',device_name
	endif else begin
		readu,1, device
		print,'version -12: device=',device
		old = device_index_from_old_index( device, name=name, silent=silent, error=err2)
		if err2 then begin
			print, '    Error converting old device index.'
			goto, error
		endif 
		device_name = name
	endelse
endif
obj = obj_new(device_name)
if obj_valid(obj) eq 0 then begin
	warning,'read_geopixe_image','Failed to create device object for: '+device_name
endif
image.DevObj = obj
print,'all versions: device_name=',device_name

; version -13

if version le -13 then begin
	readu,1, source2
	image.source2 = source2
	print,'version -13: source2=',source2
endif

; version -20 - throttle factors file, for device 16 (Maia)

if version le -20 then begin
	readu,1, throttle
	image.throttle = throttle
	print,'version -20: throttle=',throttle
endif

; version -21 - pileup factors file, for device 16 (Maia)

if version le -21 then begin
	readu,1, pileup
	image.pileup = pileup
	print,'version -21: pileup=',pileup
endif

; version -24 - bounds of image, for device 21 (UniDAQ, Wakasa)

if version le -24 then begin
	readu,1, bounds
	image.bounds = bounds
	print,'version -24: bounds=',bounds
endif

; version -25 - linearize file-name, for device 16 (Maia)

if version le -25 then begin
	readu,1, linearize
	image.linearize = linearize
	print,'version -25: linearize=',linearize
endif

; version -29 - Y encoder and border pars, for device 16 (Maia)
; Old version. Read and manually set Maia parameter in Object. 
; New version (below in file), uses read_method in object.

if (version le -29) and (version gt -35) then begin
	readu,1, encoder_y_correct, x_margin
	readu,1, clear_x, clear_y
	print,'version -29: Maia encoder XY correct=',encoder_y_correct,' X margin=',x_margin
endif

; version -18 - array detector stuff

if version le -18 then begin
	readu,1, array
	image.array = array
	print,'version -18: array=',array
	if array eq 1 then begin
		readu,1, n_active
		active = lonarr(n_active)
		readu,1, active
		print,'version -18: active=',active
		image.pactive = ptr_new( active)

		poly = fltarr(max_image_cal+1)
		cal0 = {order:1, units:'keV', poly:poly}
		cal = replicate(cal0, n_active)
		readu,1, cal
		image.pcal = ptr_new( cal)

		if (xanes eq 0) and (version le -19) then begin
			nhist = 0L
			readu,1, nhist
			if nhist gt 0 then begin
				hist = lonarr(nhist)
				readu,1, hist
				image.hist = ptr_new( hist, /no_copy)
			endif
		endif
	endif
endif

image.flatten = 1
got_IC = 0
if version le -30 then begin
	IC_mode = 0
	image.IC.mode = 0
	dwell = { on:	0, $					; dwell used (convert IC rate to count in a pixel)
			val:	0.0}					; dwell value (ms)
	flatten = 1
	np = 0L

	readu,1, IC_mode
	image.IC.mode = IC_mode					; 0:direct charge, 1:IC with PV, 2:Ion Chamber no PV)
	if IC_mode ne 0 then begin
		IC = {	mode:	0, $				; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
				conversion:		1.0, $		; IC count to charge conversion
				pv: {	name:	'', $		; Epics PV name string
						val:	0.0, $		; PV value multipler
						unit:	0.0}}		; PV units, range value

		readu,1, IC
		image.IC = IC
		got_IC = 1
		print,'version -30: Image IC parameters= PV: ',IC.pv.name,' = ',IC.pv.val*IC.pv.unit
	endif
	readu,1, np
	if np gt 0 then begin
		s = strarr(np)
		readu, 1, s
		if ptr_valid(image.plist) then ptr_free, image.plist
		image.plist = ptr_new(s)
	endif	
	readu,1, dwell
	image.dwell = dwell
	readu,1, flatten
	image.flatten = flatten
	print,'version -30: Image dwell = ',dwell.val
endif

if version le -38 then begin
	dummy = {a:1.0, b:0.0}						; was deadtime_cal, now obsolete (in Device now)
	readu,1, dummy
endif

if (xanes eq 0) and (version le -33) then begin
	readu,1, energy
	image.energy = energy
	print,'version -33: beam energy=',energy
endif
if (xanes eq 0) and (version le -42) then begin
	readu,1, n_attributes
	image.n_attributes = n_attributes
	print,'version -42: attribute planes added=',n_attributes
endif

if version le -27 then begin					; preview image
	readu,1, has_preview
	image.has_preview = has_preview
	if has_preview then begin
		nxp = 0L
		nyp = 0L
		readu,1, nxp, nyp
		preview = fltarr(nxp, nyp)
		readu,1, preview
	endif
endif

;...............New place for object specific parameter read ....................................

if version le -55 then begin					; new device data place

; Now all device specific options are stored and managed by the Object,
; and not explicitly known by GeoPIXE. This includes the Maia options.

	obj->set_versioning, 1
	obj->read_options, 1, error=err2
	if err2 then goto, bad_obj_io	

	if header then goto, skip_for_header
endif

;...............Various planes of attribute data ....................................

; From here, it is OK to skip if /header ...

if version le -26 then begin					; flux details
	readu,1, has_flux
	image.has_flux = has_flux
	if has_flux then begin
		print,'version -26: Read flux ...'
		if (version le -50) and xanes then begin
			n_flux = n_el
			if (version le -52) and xanes then readu,1, n_flux
			flux = fltarr(xsize1,ysize1,n_flux)
			for i=0L,n_flux-1 do begin
				print,i
				readu,1, frame
				if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
					flux[*,*,i] = frame
				endif else begin
					flux[*,*,i] = nsq*nsq* rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
				endelse
			endfor
		endif else begin
			flux = fltarr(xsize1,ysize1)
			if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
				readu,1, flux
			endif else begin
				readu,1, frame
				flux[*,*] = nsq*nsq* rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
			endelse
		endelse
		
		if (xanes eq 0) and (version le -37) then begin			; raw flux 
			raw_flux = fltarr(xsize1,ysize1)
			if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
				readu,1, raw_flux
			endif else begin
				readu,1, frame
				raw_flux[*,*] = nsq*nsq* rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
			endelse
		endif else begin
			raw_flux = flux
		endelse
	endif
	
;	Check consistency of flux, IC.conversion and charge ...

	if has_flux then begin
;		if image.IC.conversion eq 0.0 then image.IC.conversion = 1.0
		total_flux = total(flux)
		print,'version -26: total(flux)=',total_flux,', conv=',image.IC.conversion,', flux*conv=',total_flux*image.IC.conversion,', charge=',image.charge
		if total_flux gt 1.0e-10 then begin
			if got_IC then begin
				if image.charge eq 0.0 then begin
					image.charge = total_flux * image.IC.conversion
					print,'                 charge zero, set it to flux * conv = ',image.charge
				endif else begin
					if (abs(image.IC.conversion - image.charge / total_flux) gt 1.0e-9) then begin
						image.IC.conversion = image.charge / total_flux
						print,'                 set IC.conversion = image.charge / total_flux = ',image.IC.conversion
					endif
				endelse
			endif
			print,'             total(flux)=',total_flux,', conv=',image.IC.conversion,', flux*conv=',total_flux*image.IC.conversion,', charge=',image.charge
		endif
	endif
endif

if version le -39 then begin					; dead_fraction details
	readu,1, has_dead
	image.has_dead = has_dead
	if has_dead then begin
		print,'version -39: Read dead_fraction ...'
		dead_fraction = fltarr(xsize1,ysize1)
		if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
			readu,1, dead_fraction
		endif else begin
			readu,1, frame
			dead_fraction[*,*] = rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
		endelse
	endif
endif

if version le -40 then begin					; dwell map details
	readu,1, has_dwell
	image.has_dwell = has_dwell
	if has_dwell then begin
		print,'version -40: Read dwell map ...'
		dwell_map = fltarr(xsize1,ysize1)
		if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
			readu,1, dwell_map
		endif else begin
			readu,1, frame
			dwell_map[*,*] = nsq*nsq* rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
		endelse
	endif
endif

if version le -41 then begin					; pileup map details
	readu,1, has_pileup
	image.has_pileup = has_pileup
	if has_pileup then begin
		print,'version -41: Read pileup map ...'
		pileup_map = fltarr(xsize1,ysize1)
		if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
			readu,1, pileup_map
		endif else begin
			readu,1, frame
			pileup_map[*,*] = rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
		endelse
	endif
endif

if (xanes eq 0) or (version le -50) then begin
	if (version le -45) then begin				; count-rate map details
		readu,1, has_rates
		image.has_rates = has_rates
		if has_rates then begin
			print,'version -45: Read count-rate map ...'
			count_rate_map = fltarr(xsize1,ysize1)
			if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
				readu,1, count_rate_map
			endif else begin
				readu,1, frame
				count_rate_map[*,*] = rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
			endelse
		endif
	endif
endif

;-------------- old place for object specific parameter read -----------------

; Now all device specific options are stored and managed by the Object,
; and not explicitly known by GeoPIXE. This includes the Maia options.

if (version le -35) and (version gt -55) then begin			; general device object internal parameters
	if obj->name() eq 'MAIA_DEVICE' then begin
		use_version = 0
		if version le -39 then use_version=1
		dev_skip = 0
	endif else begin
		use_version = 1						; 'use_version' only varied for Maia device
		dev_skip = 0
		if version gt -46 then dev_skip = 1
	endelse
	if dev_skip eq 0 then begin
		obj->set_versioning, use_version
		obj->read_options, 1, error=err2
		obj->set_versioning, 1
		if err2 then goto, bad_obj_io
	endif
	
endif else if (version le -29) and (version gt -35) then begin		; old Maia internal parameters
	devpars = obj->get_options()
	if tag_present('ENCODER_Y_CORRECT',devpars) then devpars.encoder_y_correct = encoder_y_correct
	if tag_present('X_MARGIN',devpars) then devpars.x_margin = x_margin
	if tag_present('CLEAR',devpars) then devpars.clear.x = clear_x
	if tag_present('CLEAR',devpars) then devpars.clear.y = clear_y
	obj->set_options, devpars
endif

;----------------------------------------------------------------

if header then goto, skip_for_header

if (xanes eq 0) or (version le -50) then begin
	if (header eq 0) and (version le -47) then begin			; phase map details
		readu,1, has_phase
		image.has_phase = has_phase
		if has_phase then begin
			n_comp = 0L
			readu,1, n_comp
			if n_comp gt 0 then begin
				print,'version -47: Read phase map, n_comp =',n_comp
				phase = fltarr(xsize1,ysize1,n_comp)
				for i=0L,n_comp-1 do begin
					print,i
					readu,1, frame
					if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
						phase[*,*,i] = frame
					endif else begin
						phase[*,*,i] = rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
					endelse
				endfor
			endif else has_phase=0
		endif
	endif

	if (header eq 0) and (version le -48) then begin			; yield map details
		readu,1, has_yield
		image.has_yield = has_yield
		if has_yield then begin
			n_yield = 0L
			frame2 = fltarr(xesize,yesize)
			readu,1, n_yield
			if n_yield gt 0 then begin
				print,'version -48: Read yield map, n_yield =',n_yield
				yield = fltarr(xesize1,yesize1,n_yield)
				for i=0L,n_yield-1 do begin
					print,i
					readu,1, frame2
					if (xesize1 eq xesize) and (yesize1 eq yesize) then begin
						yield[*,*,i] = frame2
					endif else begin
						yield[*,*,i] = rebin(frame[0:nsq*xesize1-1,0:nsq*yesize1-1],xesize1,yesize1)
					endelse
				endfor
			endif else has_yield=0
		endif
	endif
endif

; With version -8 and beyond, image is read after all the header parameters

if (header eq 0) and (version le -8) then begin
	print,'version -8: Read images ...'
	for i=0L,n_el-1 do begin
		print,i
		readu,1, frame
		if (xsize1 eq xsize) and (ysize1 eq ysize) then begin
			img[*,*,i] = frame
		endif else begin
			img[*,*,i] = nsq*nsq* rebin(frame[0:nsq*xsize1-1,0:nsq*ysize1-1],xsize1,ysize1)
		endelse
	endfor
endif

sz = size(img)
ninf = 0
if sz[0] eq 3 then begin
	nxy = long(xsize) * long(ysize)
	ninf = 0L
	for i=0L,n_el-1 do begin
		q = where(finite(img[*,*,i]) eq 0, nq)
		if nq gt 0 then begin
			img[q + nxy*i] = 0.0
			ninf = ninf + nq
		endif
	endfor
endif
if ninf gt 0 then print,'Killed ',ninf,' non finite pixels.'

; version -10 - the error image (scaled down by 2)

;if (header eq 0) and (has_errors eq 1) then readu,1, error
if (header eq 0) and (has_errors eq 1) then begin
	if n_elements(frame2) eq 0 then frame2 = fltarr(xesize,yesize)
	for i=0L,n_el-1 do begin
		print,i
		readu,1, frame2
		if (xesize1 eq xesize) and (yesize1 eq yesize) then begin
			error[*,*,i] = frame2
		endif else begin
			error[*,*,i] = rebin(frame2[0:nsq*xesize1-1,0:nsq*yesize1-1],xesize1,yesize1)
		endelse
	endfor
endif

q = where(finite(error) eq 0)
if q[0] ne -1 then error[q]=0.0

;.............. skip to here on header read ......................

skip_for_header:

	opt = define( /options_image)
	options = replicate( opt, n_el)	
	escale = options

	history = ptrarr( n_el)

	if header then goto, wrap_up

;------- extra display info data --------------------------------

on_ioerror, wrap_up

bot = fltarr(n_el)
top = fltarr(n_el)
log = intarr(n_el)
readu,1, bot, top
for i=0L,n_el-1 do begin
	options[i].bottom = bot[i]
	options[i].top = top[i]
endfor
if version le -23 then begin
	readu,1, log
	for i=0L,n_el-1 do begin
		options[i].log = log[i]
	endfor
endif
if has_errors eq 1 then begin
	readu,1, bot, top
	for i=0L,n_el-1 do begin
		escale[i].bottom = bot[i]
		escale[i].top = top[i]
	endfor
	if version le -23 then begin
		readu,1, log
		for i=0L,n_el-1 do begin
			escale[i].log = log[i]
		endfor
	endif
endif

n = 0L
for i=0L,n_el-1 do begin
	readu,1, n
;	print,' read ',n,' history records for element ',(*image.el)[i]
	if n gt 0 then begin
		s = strarr(n)
		readu,1, s
;		print,' history = ', s
		history[i] = ptr_new(s, /no_copy)
	endif
endfor

;------- make pointers and finish -------------------------------

wrap_up:
	image.options = ptr_new( options, /no_copy)
	image.escale = ptr_new( escale, /no_copy)
	image.history = ptr_new( history, /no_copy)
	image.image = ptr_new( img, /no_copy)
	if has_errors then image.error = ptr_new( error, /no_copy)
	if has_flux then begin
		image.flux = ptr_new( flux, /no_copy)
		if xanes eq 0 then image.raw_flux = ptr_new( raw_flux, /no_copy)
	endif
	if has_preview then image.preview = ptr_new( preview, /no_copy)
	if has_dead then image.dead_fraction = ptr_new( dead_fraction, /no_copy)
	if has_dwell then image.dwell_map = ptr_new( dwell_map, /no_copy)
	if has_pileup then image.pileup_map = ptr_new( pileup_map, /no_copy)
	if has_rates then image.count_rate_map = ptr_new( count_rate_map, /no_copy)
	if has_phase then image.phase = ptr_new( phase, /no_copy)
	if has_yield then image.yield = ptr_new( yield, /no_copy)

	p = ptr_new( image, /no_copy)
	err = 0
	if header then goto, finish
	
	set_image_minmax, p, (*p).image, (*p).options
	if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
		set_image_minmax, p, (*p).error, (*p).escale
	endif

finish:
	close,1
	return, p

bad_extra:
	print,'read_geopixe_image: no extra data'
	goto, finish
bad_io:
	print,'read_geopixe_image: I/O error'
	goto, error
bad_open:
	print,'read_geopixe_image: file open failed'
	goto, error
bad_obj_io:
	print,'read_geopixe_image: Object parameters I/O error'
	goto, error
bad_file:
	print,'read_geopixe_image: bad file type or no file name supplied'
	goto, usage
bad_version:
	print,'read_geopixe_image: bad version number'
	goto, error

usage:
;	print,'read_geopixe_image: Usage: p = read_geopixe_image(file)'
;	print,'		where "p" is pointer to image struct'
;	print,'		and "file" is the name of the input file'
	goto, error

error:
	p = 0
	err = 1
	goto, finish
end
