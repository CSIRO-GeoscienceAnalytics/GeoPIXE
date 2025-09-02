;
; GeoPIXE Device Object draft for AS MEX (HDF5) scan/map data
; 
; Initialization:
; When all device classes are detected and loaded into GeoPIXE (all files of the
; form "xxx_device__define.sav" or "xxx_device__define.pro" in the "interface"
; sub-directory of the GeoPIXE main directory, excluding 'BASE_DEVICE"), an 
; Object reference is created for each using the IDL call "Obj_New('xxx_device')".
; This executes the "Init" method in the class.
; 
; init()			initialize the class definition and also defines the main 
; 					object parameter struct in the "xxx_device__define" routine.
; 					DO NOT RUN THIS METHOD (it is done by IDL).
; cleanup			this is run when an object is destroyed (DO NOT RUN THIS METHOD)
; 
; Reading list-mode data to produce images and spectra:
; All device object classes need to implement these methods, with a full parameter
; list (even if some parameters are not used).
;
; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.
; read_buffer()		called repeatedly to read buffers from the data file, process
;					these to extract X,Y,E triplet data, tagged by detector channel,
;					compress X,Y,E if needed, and optionally detect other
;					information (e.g. flux/charge, energy tokens). 
; get_header_info()	interrogate the data files (usually prior to starting processing)
; 					for various details, such as scan size (physical size and/or
; 					pixel count), title, energy cal for detectors, etc.
; flux_scan()		scan the raw data-files for details of available ion chamber 
; 					specifications (e.g. EPICS PVs) to provide for user selection, and 
; 					select one to use, and the pre-amp sensitivity value and units.
;
; The above 3 methods (plus the init and cleanup methods) are the minimum set needed 
; to be written for a new device class.
; 
; Import of various device-specific spectra formats:
; The "Spectrum Display" window "File->Import" menu can provide access to various routines
; for reading local device-specific spectrum data formats. It also provides access for
; scanning the list-mode data to extract all spectrum information (this is handled internally
; in GeoPIXE. The two routines providing this access are:
; 
; get_import_list()	returns a vector of structs specifiying various properties of the
; 					spectrum import (or list-mode spectrum extract) routines.
; 					(look at these routines below for details of these structs and various
; 					examples).
; import_spec()		calls the selected external import routine to read local spectral data.
; 
; Base-device methods available:
; Some general purpose methods are provided by the BASE_DEVICE master-class, so you don't
; need to write these for each device class (call from your class code using e.g.
; "self->big_endian()"). These are:
;
; name()				return name of device (e.g. "as_mex_h5_DEVICE").
; title()				return title string for this device (used in Sort EVT droplist)
; extension()			return raw data file extension (if fixed, else '')
; multi_files()			1=data organized in multiple files, else=0
; multi_char()			character used to separate run name/number from numeric data-file series
; big_endian()			1=flags data stored in raw data in Big Endian byte order, else=0
; vax_float()			1=flags VAX D-floating variables as part of data header
; start_adc()			# of the first detector ADC.
; pileup()				1=flags the use of a pileup rejection file for this device
; throttle()			1=f;ags the use of a Throttle mechanism for this device
; linear()				1=flags a linearization correction table used for this device
; ylut()				1=flags that this device can use and generate a lookup table of 
;						first Y for each member of a multi-file data series to speed up
;						certain operations (e.g. spectra extract using EVT button on Image Regions window).
; use_bounds()			1=flags that this device may have a border of pixels that contain
;						no data and no beam charge/flux that should be excluded from pixel count.
;
; The values for these setting are set-up in the device Init method, in a call to
; the BASE_DEVICE super-class Init method. 
; 
; Other options that can be flagged are the use of pileup, throttle, linearization, and the
; use of a Y lookup table, which can be flagged this way:
;
;	self.pileup.use = 1
;	self.throttle.use = 1
;	self.linear.use = 1
;	self.header.scan.use_ylut = 1
;
; Device specific parameters:
; If the device has some device specific parameters that need to be set-up for
; processing and read/written along with image and region data, etc. then use this
; facility to define widgets to gather info about parameter options and manage them.
; If you don't need them, do not define these methods. Then a default (no action)
; method will be used in the "BASE_DEVICE" master-class. See the code in the Maia_device
; as an example.
; 
; These options are set-up in widgets that appear in the Sort EVT options box on the
; Sort tab. The parameters live in the class 'self' struct and are handled by GeoPIXE
; using these methods:
;
; render_options	Draws widgets needed in supplied parent base (on Sort tab).
; read_options		Called when images and regions are read from disk to read
;					the device specific options into the object self struct.
; write_options		Called when images and regions are written to disk to write
;					the device specific options from the object self struct.
; options_legend()	Returns a formatted string array to be added to the
;					image history list in the Image History window.
;
; To use these widgets and methods, enable the options by setting the following in
; your class 'init' method:
; 
; 	self.options.scan.on = 1			; enable options wdigets
; 	self.options.scan.ysize = 100		; Y size of sort options box, when open
; 	
; If you need to set or get your options parameters from your class, you can define these
; methods (see examples in Maia_device). They are not essential and are NOT called
; from GeoPIXE:
; 
; set_options		Explicitly pass these options parameters into the object.
;					This is only used to transfer old version file data in
;					and should be avoided normally.
; get_options()		Explicitly get options parameters from object.
;					Avoid using this.		
;
; The following two are used with Options widgets, but are handled by the Base super-class:
; 
; show_sort_options()  Flags that this device has sort options to display.
; get_sort_ysize()	   Returns number of Y pixels needed for device options fields.
;
; The code in the render_options for creating options widgets for the Maia device,
; which also calls some OnRealize routines and an event handler, can be used as a model
; for new device options fields.
;
;----------------------------------------------------------------------------------------------------

pro as_mex_h5_DEVICE::cleanup

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::cleanup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	self->BASE_DEVICE::cleanup
    return
end

;------------------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written from/to DISK using the "read_options",
; "write_options" methods. Keep a local copy of device parameters and set them using
; "set_options". The keywords here are only used internally in this device.
;
;-------------------------------------------------------------------------------
; These routines are associated with the rendering of Sort Options widgets
; in the Sort EVT window Sort tab options box.
; 
; Render options widgets in Sort Options box in Sort EVT window.
; Parent is the framed container box. Its child must be a base that
; all options widgets are attached to. This child is target of destroy
; when switching devices.

pro as_mex_h5_device::render_options, parent

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device::render_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

case !version.os_family of
	'MacOS': begin
		source_xsize = 45
		axes_xsize = 365
		end
	'unix': begin
		source_xsize = 45
		axes_xsize = 365
		end
	else: begin
		source_xsize = 45
		axes_xsize = 305
		end
endcase

; Call super-class to cleanup old display and set Y size of box first ...

self->BASE_DEVICE::render_options, parent

; The following will appear in the Device box on the Sort tab of the Sort EVT window, and elsewhere where
; the device specific parameters are selected (e.g. spectra Import) ...

as_mex_h5mode_base = widget_base( parent, /column,  space=3, xpad=0, ypad=0, /base_align_center, $
		event_func='as_mex_h5_device_sort_option_event', uvalue=self, uname='obj-ref-here')
lab = widget_label( as_mex_h5mode_base, value='AS MEX H5 Option Parameters')

; Check-boxes to smooth the reference time-stamp

as_mex_h5fbase = widget_base( as_mex_h5mode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( as_mex_h5fbase, value='Smooth:')
smooth_ref_ts_check = cw_bgroup2( as_mex_h5fbase, ['Reference time-stamp'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='as_mex_h5-smooth-ref-ts', set_value=self.sort_options.smooth_ref_ts, /nonexclusive, $
					uvalue=['Check box to smooth out errors in the Reference time-stamp (based on "x_ts").'])

error = 0
add_widget_vector, self.sort_id.smooth_ref_ts, smooth_ref_ts_check, error=err & error=error or err
if error then begin
	warning,'as_mex_h5_device::render_options','Error adding device object widget ID vectors.'
endif
return
end

;------------------------------------------------------------------------------------------

function as_mex_h5_device_sort_option_event, event

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device_sort_option_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	if widget_info( event.handler, /valid) eq 0L then begin
;		print,'as_mex_h5_device_sort_option_event: event.handler not valid.'
		return, 0
	endif
	uname = widget_info( event.handler, /uname)
	if uname ne 'obj-ref-here' then begin
		print,'as_mex_h5_device_sort_option_event: Object base not found.'
		return, 0
	endif
	widget_control, event.handler, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'as_mex_h5_device_sort_option_event: Device Object ref not found.'
		return, 0
	endif

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		return, event						; pass context help up the line ...
		end
	else: begin
		
		uname = widget_info( event.id, /uname)
		case uname of
			'as_mex_h5-smooth-ref-ts': begin
				obj->set_options, smooth_ref_ts = event.select
				end
		endcase		
		end
endcase
return, 0L
end

;------------------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written from/to DISK using the "read_options",
; "write_options" methods. Keep a local copy of device parameters and set them using
; "set_options". The keywords here are only used internally in AS MEX H5 device.

pro as_mex_h5_device::set_options, p, smooth_ref_ts=smooth_ref_ts

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device::set_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(p) eq 1 then begin
		if size(p, /tname) eq 'STRUCT' then begin
			opt = p
		endif else if ptr_good(p,/struct) then begin
			opt = *p
		endif else return
	endif

	if n_elements(opt) eq 1 then begin
		if tag_present('VERSION',opt) then self.sort_options.version = opt.version
		if tag_present('SMOOTH_REF_TS',opt) then self.sort_options.smooth_ref_ts = opt.smooth_ref_ts
	endif else begin
		if n_elements(smooth_ref_ts) eq 1 then self.sort_options.smooth_ref_ts = smooth_ref_ts
	endelse
	
	; Set value of widgets. There may be multiple sort option panels attached to this object, so
	; we use 'widget_control_vector' to set all of them.
	
	widget_control_vector, self.sort_id.smooth_ref_ts, set_combobox_select = self.sort_options.smooth_ref_ts
	return
end

;-------------------------------------------------------------------

;	Returns internal self.sort_options struct.
;	Will get values of options widgets (e.g. text strings) and
;	update internal values in self struct.

function as_mex_h5_device::get_options, error=error

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device::get_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 0
	
	return, self.sort_options
end

;-------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written FROM DISK using the "read_options",
; "write_options" methods. This is used to embed this data in the GeoPIXE image
; and spectra files.
;
;	options		return current option values

pro as_mex_h5_device::read_options, unit, options=options, error=error

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device::read_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

; Get current defaults in this device

	options = self->get_options()
	version = 0L
	smooth_ref_ts = options.smooth_ref_ts
	
; Read options parameters from the file. Note this device will always use versioning, so
; we do not test for it. Note this 'version' is to cater for different options data written to
; output files (e.g. image and region). Use conditional statements on 'version' for these.
; Do Note confuse this version with the listmode version in options.

	on_ioerror, bad_io
	readu, unit, version

; Use 'version' for conditional read statements here ....
		
	if version le -2 then begin
		readu, unit, smooth_ref_ts
	endif

; Write back these options parameters to the device ...

	options.smooth_ref_ts = smooth_ref_ts

	self->set_options, options
	error = 0
	return
	
bad_io:
	error = 1
	return
end

;-------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written FROM DISK using the "read_options",
; "write_options" methods.
;
; write_options writes device parameters to disk. It is assumes that they have been used
; (e.g. in sort) and are being saved in an image file. Hence, the widgets are not read
; again here first. If 'options' passed use these instead.

pro as_mex_h5_device::write_options, unit, options=p, error=error

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device::write_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(p) eq 0 then options = self.sort_options
	if ptr_valid(p) then begin
		if size(*p,/tname) eq 'STRUCT' then begin
			options = *p
		endif else begin
			options = self.sort_options
		endelse
	endif else begin
		if size(p,/tname) eq 'STRUCT' then begin
			options = p
		endif else begin
			options = self.sort_options
		endelse
	endelse
	
;	This is the version of the options write data. Do not confuse this with the listmode
;	version

	version = -2L	
	on_ioerror, bad_io
	writeu, unit, version

;	Write any new options parameters here. If you add to these change the version number
;	(e.g. decrement it) and add conditional code on 'version' for the matching read method.

	writeu, unit, options.smooth_ref_ts
	error = 0
	return
	
bad_io:
	error = 1
	return
end

;-------------------------------------------------------------------------------

; Return a string to display (e.g. in Image History window) to show the
; state of this device's options. If 'p' present, then show this parameter set.

function as_mex_h5_device::options_legend, p

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_device::read_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif

	if n_elements(p) ge 1 then begin
		if size(p, /tname) eq 'STRUCT' then options = p
		if ptr_good(p, /struct) then options = *p
	endif
	if n_elements(options) eq 0 then options = self.sort_options

	on_off = ['Off','On']

	list = ['as_mex_h5:' ] 
	list = [list, '   Smooth reference time-stamp ("x_ts"): ' + str_tidy(options.smooth_ref_ts) ]

	return, list
end

;-------------------------------------------------------------------
;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

function as_mex_h5_device::get_header_info, file, output=output, silent=silent, error=error

; file		a raw data file to look for associated header, metadata
; output	if present, this is a file on the output path, if some metadata is
;			located in that path. 

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_mex_1, reference_ts, smooth_ref_time

	if n_elements(silent) lt 1 then silent=0
	error = 1
	smooth_ref_time = self.sort_options.smooth_ref_ts			; pass self variable for smooth to 'read_as_mex_h5_header'

	if H5F_is_hdf5(file) eq 0 then begin
		warning,'as_mex_h5_DEVICE::get_header_info','Not a valid HDF5 file: '+file
		self.header.error = 1
		return, self.header
	endif
	
	mp = get_as_mex_h5_header( file, error=error)
	if error then begin
		self.header.error = 1
		return, self.header
	endif

	self.header.error = 0

	self->save_header_data, mp						; save raw device data 'mp' in self
	self->update_header_info, error=error			; update self.header using saved 'mp'
	if error then return,0
	return, self.header
end

;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

pro as_mex_h5_DEVICE::update_header_info, error=error

; Update header from old 'mp', mapping XYZ according to source details.

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::update_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	error = 1
	pmp = self.old_mp
	if ptr_good(pmp) eq 0 then return
	
	if (*pmp).nx gt 1 then self.header.scan.on = 1
	self.header.scan.dwell = (*pmp).dwell
	self.header.scan.x_pixels = (*pmp).nx
	self.header.scan.y_pixels = (*pmp).ny
	self.header.scan.x_on = 1
	self.header.scan.y_on = 1
	nx = (*pmp).nx 
	ny = (*pmp).ny 
	self.header.scan.x_mm = max((*pmp).axisx) - min((*pmp).axisx)
	self.header.scan.y_mm = max((*pmp).axisy) - min((*pmp).axisy)
	self.header.scan.x = min((*pmp).axisx)
	self.header.scan.y = min((*pmp).axisy)
	self.header.detector[*] = 7								; SXRF default

	if n_elements((*pmp).axisx) gt 1 then begin
		if ptr_valid( self.header.px_coords) then ptr_free, self.header.px_coords 
		self.header.px_coords = ptr_new( (*pmp).axisx)
		self.header.x_coord_units = 'mm'
	endif
	if n_elements((*pmp).axisx) gt 1 then begin
		if ptr_valid( self.header.py_coords) then ptr_free, self.header.py_coords 
		self.header.py_coords = ptr_new( (*pmp).axisy)
		self.header.y_coord_units = 'mm'
	endif
	n = n_elements((*pmp).cal)
	self.header.cal[0:n-1] = (*pmp).cal

	self.header.IC_name = (*pmp).pv_names[0]				; PV name for ion chamber
;	self.header.sensitivity = (*pmp).IC_sensitivity			; IC preamp sensitivity (relative to 1.0 = nA/V)

	self.header.energy = (*pmp).energy
;	self.header.title = (*pmp).comment
	self.header.sample = (*pmp).sample
;	self.header.grain = (*pmp).grain
	
;	self.header.metadata.sample_type = (*pmp).sample_type
;	self.header.metadata.sample_serial = (*pmp).sample_serial
;	self.header.metadata.detector_identity = (*pmp).detector_identity
	self.header.metadata.facility = (*pmp).facility
	self.header.metadata.endstation = (*pmp).endstation

	self.header.error = 0
	error = 0
	return
end

;-------------------------------------------------------------------

pro as_mex_h5_DEVICE::check_pv_list, plist

; Check the PV list pointed to by 'plist'. Add any local default PVs
; not present to this list.

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::check_pv_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	check_plist_as_mex_h5, plist
	return
end

;-------------------------------------------------------------------

; Scan raw data files for device specific flux IC PV information
 
pro as_mex_h5_DEVICE::flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
			image_mode=image_mode, group=group, suppress=suppress, $
			no_pv=no_pv, use_dwell=use_dwell, error=error

; Scan raw data files for flux IC PV information
; 
; Input:
; 	unit		I/O unit open
; 	evt_file	file-name of opened file
; 	group		group-leader parent window to pass to any pop-up windows
;
;	/Image_mode	scan data for PVs for an image, with dwell
;	/Suppress	suppress pop-ups

; Return:
; 	PV_list		string list of all PV's found that may be used to measure flux/IC count
; 	IC_name		PV selected by user from list
; 	IC_val		pre-amp sensitivity value
; 	IC_vunit	pre-amp sensitivity unit multipler
; 	dwell		dwell-time in a pixel (ms), if needed
; 	no_pv		flags absence of any PVs
;	use_dwell	flags need to use dwell in flux count measure

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::flux_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_pnc_hdf_1, pnc_hdf_x_range, pnc_hdf_y_range, pnc_hdf_x, pnc_hdf_y
common c_pnc_hdf_2, pnc_hdf_e, pnc_hdf_ste, pnc_hdf_chans, pnc_hdf_dets, pnc_hdf_x1, pnc_hdf_y1
common c_pnc_hdf_3, med, pnc_hdf_data
common c_pnc_hdf_4, pnc_hdf_IC
common c_pnc_hdf_5, pnc_hdf_IC_value_index, pnc_hdf_flux_scale
common c_pnc_hdf_6, max_det
common c_pnc_hdf_7, pnc_hdf_dead
common c_pnc_hdf_8, hdf5_id, pnc_hdf_read_line, hdf5_spectra, spectra_id
common c_pnc_hdf_9, pnc_hdf_debug

	if n_elements(first) lt 1 then first = 1
	if n_elements(suppress) lt 1 then suppress = 0
	if n_elements(image_mode) lt 1 then image_mode = 1
	if n_elements(pnc_hdf_debug) lt 1 then pnc_hdf_debug = 0
	
	PV_list = 'none'
	check_plist_as_mex_h5, PV_list
	IC_name = ''
	IC_val = 0.
	IC_vunit = 0.
	dwell = 0.
	no_pv = 1
	use_dwell = 0
	error = 1
	
	mp = read_as_mex_h5_header( unit, error=error)
	if error then return

	dwell = mp.dwell
	use_dwell = 1
	PV_list = mp.pv_names
	no_pv = 0
	IC_val = 1.					; no preferred gain/units for PV in header
	IC_vunit = 1.
	IC_name = PV_list[0]		; no preferred PV for ion chamber in header, use first
	error = 0
	return

bad_io:
	warning,'as_mex_h5_DEVICE::flux_scan','HDF file I/O error.'
	error = 1
	return
end

;-------------------------------------------------------------------

; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.

function as_mex_h5_DEVICE::read_setup, unit, xrange, yrange, first=first, $
			n_guide,progress_file, charge=charge, ecompress=ecompress, $
			flux=flux, dead_fraction=dead_fraction, progress_size=progress_size, $
			suppress=suppress, ic=flux_ic, x_coords=x_coords, $
			y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units, $
			beam_energy=beam_energy

;   Device specific list-mode (event-by-event) data file read set-up routine.
;   Remember, channel starts at 0 (-1 means any channel/ADC).
;
; input:
;   unit		read unit number
;   xrange		X size of scan (pixels)
;   yrange		Y size of scan (pixels)
;   ecompress	desired E axis compression (needs to match DA energy calibration)
;   ic			struct containing ion chamber settings for flux measurement. Has the form:
;   			{mode:0, pv:'', val:0.0, unit:0.0, conversion:1., use_dwell:0, dwell:1.0}
;   			where	mode	0=no flux IC (ion beam, no IC data), 1=use IC PV, 2=use conversion only (no PV)
;   					pv		user selected EPICS PV string to be used for flux IC
;   					val		pre-amp sensitivity value
;   					unit	pre-amp sensitivity unit (scale)
;   				conversion	conversion from flux count to charge (uC)
;   				use_dwell	use the dwell time with flux count-rate to build flux count per pixel
;   				dwell		dwell time in a pixel (ms)
;
;	/first		for first file in multi-file data-set
;   /ystep		Y step mode, else X
;   /suppress	suppress any pop-up windows
;
; return:
; 	n_guide		an event number to guide reporting in the progress bar
;   progress_file provide progress by (1) file advance/ (2) spectra # rather than (0) records/events
;   charge		charge for whole scan (uC), if available from file
;   beam_energy	beam energy, if it's available (MeV=ionbeam, keV=synchrotron)
;
;	x_coords	vector of physical coordinates for X pixels (if available from data files)
;	x_coord_units units string for X coords
;	y_coords	vector of physical coordinates for y pixels
;	y_coord_units units string for Y coords
;	
;   flux		flux array (matches image dimensions for scan), accumulated here or in read_buffer
;   dead_fraction DT loss-fraction array (matches image dimensions for scan), accumulated here or in read_buffer
;
;   error		read_setup() returns=1 to flag an error to abort data processing

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_pnc_hdf_1, pnc_hdf_x_range, pnc_hdf_y_range, pnc_hdf_x, pnc_hdf_y
common c_pnc_hdf_2, pnc_hdf_e, pnc_hdf_ste, pnc_hdf_chans, pnc_hdf_dets, pnc_hdf_x1, pnc_hdf_y1
common c_pnc_hdf_3, med, pnc_hdf_data
common c_pnc_hdf_4, pnc_hdf_IC
common c_pnc_hdf_5, pnc_hdf_IC_value_index, pnc_hdf_flux_scale
common c_pnc_hdf_6, max_det
common c_pnc_hdf_7, pnc_hdf_dead
common c_pnc_hdf_8, hdf5_id, pnc_hdf_read_line, hdf5_spectra, spectra_id
common c_pnc_hdf_9, pnc_hdf_debug
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_maia_13, maia_dwell
common c_maia_11, maia_hw_scaler, maia_fixed_dwell
common c_pnc_cat_11, i_spectra, q_spectra, name_spectra
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_petra_1, min_x, step_x, min_y, pixel_x, pixel_y
common c_petra_3, n_sequence_buffer, p06_file_id
common c_petra_4, n_petra_channel, i_petra_channel
common c_mex_1, reference_ts, smooth_ref_time

	n_guide = 1000000L
	progress_file = 3					; progress indicator will be rough (see updated below)
	stat = fstat(unit)						
	
;	Data is organized as spectra (4096) for each of detector channel (4), for each sequence step (41496),
;	for each energy (1). The energy dimension is ignored for now.
;	The sequence steps have an 'x' and 'y' each, which do not necessarily follow a perfect rectangle.
;
;	Have ignored energy axis for now. Will need this for XANES maps. Will also need to test if E axis
;	is actually the fast axis.
;
;	It is assumed below that the various parameters are not necessarily sampled at the same rate. Hence,
;	the various time-stamps ('_ts' vectors) may have different steps and lengths. The logic is to first
;	find the average step size in 'x', then determine an equi-spaced X grid for pixels based on this step
;	and the starting value. Then determine the effective pixel-x time-stamp for each pixel. This becomes
;	the 'reference_ts' (stored in common) that all other parameters are sampled to.
;
;	The main routines where all this is applied is 'read_as_mex_h5_header' and 'as_mex_h5_DEVICE::read_setup'.

	smooth_ref_time = self.sort_options.smooth_ref_ts			; pass self variable for smooth to 'read_as_mex_h5_header'

	self.spectrum_mode = 1
	if (n_elements(flux) ge 2) then self.spectrum_mode=0

	file_id = H5F_OPEN(stat.name)
	p06_file_id = file_id				; save to use in 'read_buffer'
	version = '1.0.0'
	timebase = 1000L					; for ms?
	i_buffer = 0L

	nm = H5G_get_num_objs( file_id)
	name7 = strarr(nm)
	for j=0,nm-1 do begin
		name7[j] = H5G_get_obj_name_by_idx(file_id, j)
	endfor

;	Get sequence numbers 
	
	q = where( name7 eq 'position', nq)
	if nq eq 0 then begin
		warning,'as_mex_h5_DEVICE::read_setup','No "position" dataset found.'
		goto, bad_io
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'position')
		sequence = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

;	Need to read the header for each HDF5 file. This is not automatic if a sort is started (da_evt)
;	without selecting new data files.

	if first then begin
		head = read_as_mex_h5_header( unit, error=err32)
		if err32 eq 0 then begin
			self->save_header_data, head							; save raw device data 'mp' in self
			self->update_header_info, error=error					; update self.header using saved 'mp'
			
			beam_energy = self.header.energy
		endif
	endif

;	Get x,y for each sequence step (read by 'read_as_mex_h5_header'). This would allow multiple files,
;	each continuing a sequence to be processed in future.
;	It is assumed that the sequence steps parallel the 'x' steps.

	nseq = n_elements(pixel_x)
	n_sequence_buffer = nseq

	x = pixel_x[ (sequence-1)>0]
	y = pixel_y[ (sequence-1)>0]

	nx = max(pixel_x) + 1
	ny = max(pixel_y) + 1

;	Get dwell (from reference time-stamps on x pixels, stored in 'reference_ts')

	t = reference_ts - shift(reference_ts,1)
	t[0] = t[1]											; fix wrap
	dwell_array = t 									; dwell time (s)
	
	h = histogram( dwell_array *300./(max(dwell_array)>0.001), /NaN, locations=tx)
	q2 = reverse(sort(h))
	q3 = where( tx[q2] ne 0.0, nq3)						; to avoid missed pixels with zero dwell
	common_dwell = 0.0
	if nq3 ne 0 then common_dwell = tx[q2[q3[0]]] *(max(dwell_array)>0.001)/300.

	if self.spectrum_mode then begin
		maia_dwell = common_dwell
	endif else begin
		maia_dwell = flux
		maia_dwell[*] = 0.0
		maia_dwell[x,y] = t * 1000.								; ms
	endelse

;	Get flux

	nsls_flux_scale = flux_ic.val * flux_ic.unit
;	if (self.spectrum_mode eq 0)  then begin
;		nsls_flux_scale = nsls_flux_scale * flux_ic.dwell*0.001			; Scale Epics scaler rates by dwell only for maps.
;	endif																; This assumes PV values are RATES not counts.

	found = 0
	q = where( name7 eq flux_ic.pv, nq)
	if nq ne 0 then begin
		rec_id = H5D_OPEN(file_id, flux_ic.pv)
		i0 = H5D_read(rec_id)
		H5D_close, rec_id
		found = 1
	endif
	if not found then begin
		warning,'as_mex_h5_DEVICE::read_setup','No flux data found for selected PV "'+flux_ic.pv+'".'
;		goto, bad_io
	endif

	q = where( name7 eq flux_ic.pv + '_ts', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "'+flux_ic.pv+'_ts" dataset found.'
		goto, bad_io
	endif else begin
		rec_id = H5D_OPEN(file_id, flux_ic.pv + '_ts')
		i0_ts = H5D_read(rec_id)
		H5D_close, rec_id
	endelse
	flux_x_ts = interpol( i0, i0_ts, reference_ts)				; effective 'flux' at time of effective 'x' time-stamps

	if (self.spectrum_mode eq 0)  then begin
		flux[x,y] = nsls_flux_scale * flux_x_ts * t				; assume i0 is a rate, so scale by times 't'
	endif else begin
		flux = total( nsls_flux_scale * flux_x_ts * t)
	endelse

;	Get histgram/spectra data size ...

	q = where( name7 eq 'spectrum', nq)
	if nq eq 0 then begin
		warning,'as_mex_h5_DEVICE::read_setup','No "spectrum" dataset found.'
		goto, bad_io
	endif
	
;	Spectra data
;	dims over: channels in each spectrum, detector channel, sequence steps, energy steps (XANES maps)
;	(Ignore XANES map case for now)

	spectra_id = H5D_OPEN(file_id, 'spectrum')
	dspace = H5D_get_space(spectra_id)
	dims = H5S_get_simple_extent_dims(dspace)
	H5S_close, dspace

	if long(nx)*long(ny) ne dims[2] then begin
;		warning,'as_mex_h5_DEVICE::read_setup','Sequence count does not match "nx*ny".'
;		goto, bad_io
	endif

;	Energy dimension is ignore for now. Later shoild step both detector channel and energy
;	on each call to 'read_buffer' method.

	n_petra_channel = dims[1]			; step through detector channel
	i_petra_channel = 0
	progress_size = n_petra_channel

  	pnc_hdf_x_range = nx
  	pnc_hdf_y_range = ny
  	pnc_hdf_dets = dims[1]
  	pnc_hdf_chans = dims[0]
  	pnc_hdf_x = 0						; not used
	pnc_hdf_y = 0

;	Set up various vectors to use to construct X,Y,E events in 'read_buffer'

	ramp = indgen(pnc_hdf_chans)
	pnc_hdf_e = uintarr( pnc_hdf_chans, nseq)
	pnc_hdf_x1 = pnc_hdf_e
	pnc_hdf_y1 = pnc_hdf_e
	pnc_hdf_ste = pnc_hdf_e
	for j=0L,nseq-1 do begin
		pnc_hdf_x1[*,j] = x[j]
		pnc_hdf_y1[*,j] = y[j]
		pnc_hdf_e[*,j] = ramp
	endfor

	pnc_hdf_e = reform( pnc_hdf_e, pnc_hdf_chans*nseq)
	pnc_hdf_ste = reform( pnc_hdf_ste, pnc_hdf_chans*nseq)
	pnc_hdf_x1 = reform( pnc_hdf_x1, pnc_hdf_chans*nseq)
	pnc_hdf_y1 = reform( pnc_hdf_y1, pnc_hdf_chans*nseq)

	return, 0						; hdf5_id will be closed in read_buffer later ...
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

; read_buffer()		called repeatedly to read buffers from the data file, process
;					these to extract X,Y,E triplet data, tagged by detector channel, ste,
;					compress X,Y,E if needed, and optionally detect other
;					information (e.g. flux/charge, energy tokens). 

function as_mex_h5_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
		station_e=ste, time=tot, veto=veto, ecompress=ecompress, multiple=multiple, $
		xoffset=xoffset, yoffset=yoffset, title=title, file=file, error=error, $
		flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
		total_processed=processed, processed=count1, valid=good, total_bad_xy=bad_xy, raw_xy=raw_xy, $
		by_odd=by_odd, by_even=by_even, support_even_odd=support_even_odd

;   Device specific list-mode (event-by-event) data file reading routine.
;   Remember, channel starts at 0 (-1 means any channel/ADC).
;
; input:
;   unit		read unit number
;   channel_on	desired ADC channel(s) (these start at zero, after an optional offset)
;   xcompress	desired X axis compression
;   ycompress	desired Y axis compression
;   ecompress	desired E axis compression (needs to match DA energy calibration)
;   xoffset		offset X by this (i.e. subtract this)
;   yoffset		offset Y by this
;   /raw_xy		suppresses X,Y compression and offset
;   flux		for some flux is an array that comes in to be updated with pixel flux
;   dead_fraction for some this is an array that comes in to be updated with pixel dead_fraction
;   file		filename passed for multi-file XY checking
;				
; return:
;   e			energy vector (uintarr) returned
;   x1			X vector (uintarr) return
;   y1			Y vector (uintarr) return
;   ste			ADC number vector (uintarr) for each returned event (less offset, starting at 0)
;	n			number of (x,y,e) triplets returned
;   beam_energy	beam energy, if it's available (MeV=ionbeam, keV=synchrotron)
;   valid		number of good events (same as 'n'), or zero
;   count1		number of events processes in this buffer
;   bad_xy		increment passed in value of total event with bad X,Y codes
;   processed	increment total number of events processed.
;   title		run title
;   error		error=1 flags an error to abort
;
; Optional (not used here):
;   tot			(optional) Time-over-threshold vector (uintarr), for some DAQs (e.g. Maia)
;	veto		(optional) vector (uintarr) indicates a vetoed event (use this as events are rejected)
;   multiple	(optional) if this has same dimensions as e, then it indicates multiple
;          		events with the same x1,y1,e.

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_pnc_hdf_1, pnc_hdf_x_range, pnc_hdf_y_range, pnc_hdf_x, pnc_hdf_y
common c_pnc_hdf_2, pnc_hdf_e, pnc_hdf_ste, pnc_hdf_chans, pnc_hdf_dets, pnc_hdf_x1, pnc_hdf_y1
common c_pnc_hdf_3, med, pnc_hdf_data
common c_pnc_hdf_4, pnc_hdf_IC
common c_pnc_hdf_5, pnc_hdf_IC_value_index, pnc_hdf_flux_scale
common c_pnc_hdf_6, max_det
common c_pnc_hdf_7, pnc_hdf_dead
common c_pnc_hdf_8, hdf5_id, pnc_hdf_read_line, hdf5_spectra, spectra_id
common c_pnc_hdf_9, pnc_hdf_debug
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_maia_13, maia_dwell
common c_maia_11, maia_hw_scaler, maia_fixed_dwell
common c_pnc_cat_11, i_spectra, q_spectra, name_spectra
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_petra_1, min_x, step_x, min_y, pixel_x, pixel_y
common c_petra_2, dwell_array
common c_petra_3, n_sequence_buffer, p06_file_id
common c_petra_4, n_petra_channel, i_petra_channel
common c_mex_1, reference_ts, smooth_ref_time

	n = 0L
	good = 0L
	if i_petra_channel ge n_petra_channel then goto, bad_io

	i_buffer = i_buffer+1
	
	on_ioerror, bad_io
	nc = n_elements(channel_on)
	file_id = p06_file_id					; remember to close 'file_id' after last "channelxx"

;	Data is organized as spectra (4096) for each of detector channel (4), for each sequence step (41496),
;	for each energy (1). The energy dimension is ignored for now.
;	The sequence steps have an 'x' and 'y' each, which do not necessarily follow a perfect rectangle.
;
;	Have ignored energy axis for now. Will need this for XANES maps. Will also need to test if E axis
;	is actually the fast axis.
;
;	It is assumed below that the various parameters are not necessarily sampled at the same rate. Hence,
;	the various time-stamps ('_ts' vectors) may have different steps and lengths. The logic is to first
;	find the average step size in 'x', then determine an equi-spaced X grid for pixels based on this step
;	and the starting value. Then determine the effective pixel-x time-stamp for each pixel. This becomes
;	the 'reference_ts' (stored in common) that all other parameters are sampled to.
;
;	The main routines where all this is applied is 'read_as_mex_h5_header' and 'as_mex_h5_DEVICE::read_setup'.
;
;	Energy dimension is ignore for now. Later should step both detector channel and energy
;	on each call to 'read_buffer' method.

	dataspace_id = H5D_get_space(spectra_id)
	start = [0, i_petra_channel, 0, 0]
	count = [pnc_hdf_chans, 1, n_sequence_buffer, 1]
	H5S_select_hyperslab, dataspace_id, start, count, /RESET

	mem_space_id = H5S_create_simple(count)
	hdf5_spectra = H5D_read(spectra_id, FILE_SPACE=dataspace_id, MEMORY_SPACE=mem_space_id)
	H5S_close, mem_space_id
	H5S_close, dataspace_id

;	Define E, X, Y event vectors
;	Return events for a single Y line in each buffer, cycle through Y and detectors.

	e = pnc_hdf_e
	x1 = pnc_hdf_x1
	y1 = pnc_hdf_y1
	ste = pnc_hdf_ste
	ste[*] = i_petra_channel++	

;	Spectum count per pixel/channel used to set a 'multiplicity' for each 'event'.

	multiple = long( reform(hdf5_spectra, pnc_hdf_chans*n_sequence_buffer))

	q = where((multiple gt 0) and channel_on[ste], count)
	if count gt 0 then begin
		e = e[q]
		ste = ste[q]
		x1 = x1[q]
		y1 = y1[q]
		multiple = multiple[q]
		good = n_elements(q)
	endif else begin
		good = 0L
	endelse

	if good gt 0 then begin
		if raw_xy eq 0 then begin
			if xcompress ne 1 then x1 = x1 / uint(xcompress)
			if ycompress ne 1 then y1 = y1 / uint(ycompress)
			if ecompress ne 1 then e = e / uint(ecompress)
		endif
	endif else begin
		x1 = 0US
		y1 = 0US
		e = 0US
		ste = 0US
		multiple = -1L
	endelse
	n = good

    processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function as_mex_h5_DEVICE::get_dwell, error=error

; Return the internal dwell (ms) image array

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::get_dwell',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_maia_13, maia_dwell

	error = 0
	return, maia_dwell
end

;-------------------------------------------------------------------

function as_mex_h5_DEVICE::import_spec, name, file, group=group

; Import spectra of various local types. This does not include extraction of
; spectra from list-mode data, which is handled elsewhere.

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

case name of
;	'as_mex_h5_spec': begin						; 41
;		p2 = 0L
;		if H5F_is_hdf5( file) eq 0 then begin 
;			warning,'as_mex_h5_DEVICE::get_import_list','Not a valid HDF5 file: '+file
;			goto, as_mex_h5_evt_done
;		endif
;		file_id = H5F_open(file)
;		hdf5_id = file_id
;
;		version = '2.1.0'
;		timebase = 1000L			; for ms
;
;		base_id = H5G_open(file_id,'xrmmap')
;		n = H5A_get_num_attrs( base_id)
;		for i=0,n-1 do begin
;			attr_id = H5A_open_idx(base_id, i)
;			case H5A_get_name( attr_id) of
;				'VERSION': begin
;;					attr = H5A_read( attr_id)
;;					version = attr
;					end
;				else:
;			endcase
;			H5A_close, attr_id
;		endfor
;		H5G_close, base_id
;
;		nm = H5G_get_nmembers(file_id,'xrmmap')	
;		name = strarr(nm)
;		for i=0,nm-1 do begin
;			name[i] = H5G_get_member_name(file_id,'xrmmap',i)
;		endfor
;
;		q = where( name eq 'scalars', nq)
;		if nq eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/scalars" group found.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		nm = H5G_get_nmembers(file_id,'xrmmap/scalars')	
;		headings = strarr(nm)
;		for i=0,nm-1 do begin
;			headings[i] = H5G_get_member_name(file_id,'xrmmap/scalars',i)
;		endfor
;		pv_names = headings
;
;		q = where( name eq 'mca1', nq)
;		if nq eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/mca1" dataset found.'
;			goto, as_mex_h5_evt_done
;		endif
;	
;		nm = H5G_get_nmembers(hdf5_id,'xrmmap/mca1')	
;		name2 = strarr(nm)
;		for i=0,nm-1 do begin
;			name2[i] = H5G_get_member_name(hdf5_id,'xrmmap/mca1',i)
;		endfor
;	
;		q = where( name2 eq 'realtime', nq)
;		if nq eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/mca1/realtime" data found.'
;			goto, as_mex_h5_evt_done
;		endif
;		det_id = H5D_OPEN(hdf5_id,'xrmmap/mca1/realtime')
;		dspace = H5D_get_space(det_id)
;		dims = H5S_get_simple_extent_dims(dspace)
;		H5S_close, dspace
;	
;		nx = dims[0]
;		ny = dims[1]
;	
;		det = H5D_read(det_id) / float(timebase)		; for ms ???
;		dwell = det[*,*]
;		H5D_close, det_id
;
;		q = where( (strmid(name,0,3) eq 'mca') and (name ne 'mcasum'), n_det)
;		if nq eq 0 then begin
;			warning,'as_mex_h5_DEVICE::get_import_list','No "xrmmap/mca" datasets found in group.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_det)
;	
;		q2 = where( name eq 'config', nq2)
;		if nq2 eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/config" group found.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		nm = H5G_get_nmembers(file_id,'xrmmap/config')	
;		name2 = strarr(nm)
;		for i=0,nm-1 do begin
;			name2[i] = H5G_get_member_name(file_id,'xrmmap/config',i)
;		endfor
;
;		q3 = where( name2 eq 'mca_calib', nq3)
;		if nq3 eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/config/mca_calib" group found.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		nm = H5G_get_nmembers(file_id,'xrmmap/config/mca_calib')	
;		name3 = strarr(nm)
;		for i=0,nm-1 do begin
;			name3[i] = H5G_get_member_name(file_id,'xrmmap/config/mca_calib',i)
;		endfor
;
;		q4 = where( name3 eq 'offset', nq4)
;		if nq4 eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/config/mca_calib/offset" data found.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		axis_id = H5D_OPEN(file_id,'xrmmap/config/mca_calib/offset')
;		doff = H5D_read(axis_id)
;		H5D_close, axis_id			
;
;		q4 = where( name3 eq 'slope', nq4)
;		if nq4 eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/config/mca_calib/slope" data found.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		axis_id = H5D_OPEN(file_id,'xrmmap/config/mca_calib/slope')
;		dslope = H5D_read(axis_id)
;		H5D_close, axis_id			
;
;		for i=0,n_det-1 do begin
;			cal[i].a = dslope[i]
;			cal[i].b = doff[i]
;			if cal[i].a gt 1.0e-10 then begin
;				cal[i].on = 1
;				cal[i].units = 'keV'
;			endif
;		endfor
;
;		q = where( name eq 'positions', nq)
;		if nq eq 0 then begin
;			warning,'as_mex_h5_DEVICE::read_setup','No "xrmmap/positions" group found.'
;			goto, as_mex_h5_evt_done
;		endif else begin
;			nm = H5G_get_nmembers(file_id,'xrmmap/positions')	
;			name2 = strarr(nm)
;			for i=0,nm-1 do begin
;				name2[i] = H5G_get_member_name(file_id,'xrmmap/positions',i)
;			endfor
;	
;			q = where( name2 eq 'pos', nq)
;			if nq eq 0 then begin
;				warning,'as_mex_h5_DEVICE::import_spec','No "xrmmap/positions/pos" data found.'
;				goto, as_mex_h5_evt_done
;			endif else begin
;				axis_id = H5D_OPEN(file_id,'xrmmap/positions/pos')
;				pos = H5D_read(axis_id)
;				axisx = reform(pos[0,*,0])					; assumes was mm
;				use_x = 1
;				axisy = reform(pos[1,0,*])					; assumes was mm
;				use_y = 1
;				H5D_close, axis_id			
;			endelse
;		endelse
;
;		q = where( (strmid(name,0,3) eq 'mca') and (name ne 'mcasum'), n_det)
;		if nq eq 0 then begin
;			warning,'as_mex_h5_DEVICE::import_spec','No "xrmmap/mca" datasets found in group.'
;			goto, as_mex_h5_evt_done
;		endif
;
;		h = histogram( dwell, /NaN, locations=x)
;		q2 = reverse(sort(h))
;		maia_fixed_dwell = x[q2[0]]
;
;		p2 = ptrarr(n_det)
;		for i=0,n_det-1 do begin
;			spec_id = H5D_open(file_id,'xrmmap/'+name[q[i]]+'/counts')
;			space = H5D_get_space(spec_id)
;			dims = H5S_get_simple_extent_dims(space)
;			spec1 = H5D_read(spec_id)
;			spec2 = fltarr(dims[0])
;			for j=0,dims[0]-1 do begin
;				spec2[j] = total( spec1[j,*,*])
;			endfor
;
;			spec = define(/spectrum)
;			spec.size = dims[0]
;			spec.data = ptr_new(spec2, /no_copy)
;			spec.label = file + ' [' + name[q[i]] + ']'
;			spec.station = i+1
;			spec.channel = i
;			spec.detector = 7
;			spec.cal.order = 1
;			spec.cal.poly[0] = cal[i].b
;			spec.cal.poly[1] = cal[i].a
;			spec.cal.units = cal[i].units
;;			spec.comment = 
;			spec.dwell = {on:1, val: maia_fixed_dwell}
;;			spec.px_coords = ptr_new(axisx)
;;			spec.x_coords_units = 'mm'
;			p2[i] = ptr_new(spec)
;			
;			H5S_close, space
;			H5D_close, spec_id
;		endfor
;
;as_mex_h5_evt_done:
;		H5F_close, file_id
;		end

	'as_mex_h5_evt': begin							; 9
		warning,'as_mex_h5_DEVICE::import_spec',['"as_mex_h5_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
		end
	endcase
	return, p2
end

;-------------------------------------------------------------------

function as_mex_h5_DEVICE::get_import_list, error=error

; Return a vector of import specification structs that can be used in the
; Import window ("File->Import" menu of Spectrum Display window).

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

; Details of spectrum import struct ... (the first 4 items are essential):
;opt = { name:			'', $			; unique name for import
;		title:			'', $			; description for import list
;		in_ext:			'', $			; input file extension
;		request:		'', $			; title for file requester
;		
;		preview:		0, $			; allow spectrum preview
;		raw:			0, $			; flags use of separate Raw data path '(*pstate).dpath'
;		spec_evt:		0, $			; uses a call to spec_evt to extract from EVT data
;		device_name:	'', $			; associated device object name
;		multifile:		0, $			; denotes data in a series of more than one file
;		separate:		'.', $			; char between file and run #
;		use_linear:		0, $			; request linearization file
;		use_pileup:		0, $			; request pileup file
;		use_throttle:	0, $			; request throttle file
;		use_IC:			0, $			; pop-up the flux_select PV selection panel
;		use_tot:		0, $			; collect ToT data too
;		xr:				200, $			; default X range
;		yr:				200 $			; default Y range

	error = 1

;	opt_41 = define(/import)			; MEX new HDF5 image file
;		opt_41.name =		'as_mex_h5_spec'		; unique name of import
;		opt_41.title =		'Extract E from MEX map HDF5 file'
;		opt_41.in_ext =		'.h5'		; input file extension
;		opt_41.request =	'Select MEX map HDF file to scan for all spectra'
;		opt_41.spec_evt =	0			; uses direct HDF5 reads for spectra
;		opt_41.use_IC =		1			; pop-up the flux_select PV selection panel
;		opt_41.IC_mode = 	1			; default to using PV for IC
	
	opt_39 = define(/import)			; MEX new HDF5 file read as a list-mode
		opt_39.name =		'as_mex_h5_evt'	; unique name of import
		opt_39.title =		'Extract E,X,Y from MEX map HDF5 file as list-mode'
		opt_39.in_ext =		'.hdf5'		; input file extension
		opt_39.request =	'Select MEX HDF file to scan for all spectra, X,Y'
		opt_39.preview =	0			; allow spectrum preview
		opt_39.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_39.multifile =	0			; denotes data in a series of more than one file
		opt_39.separate =	''			; char between file and run #
		opt_39.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_39.use_IC =		1			; pop-up the flux_select PV selection panel
		opt_39.IC_mode = 	1			; default to using PV for IC
	
;	opt = [opt_41, opt_39]
	opt = [ opt_39]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function as_mex_h5_DEVICE::init

COMPILE_OPT STRICTARR
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
		warning,'as_mex_h5_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

; Sort Option items appear as Scan setup options in the Sort EVT "Scan" tab

	self.options.scan.on = 1		; scan sort options availability for this class
									; these must be set-up in a render_options method
	self.options.scan.ysize = 75	; Y size of sort options box, when open

; Set default Sort Options local parameters ...

	self.sort_options.smooth_ref_ts = 1							; Enable smoothing of Ref time-stamp as default

; Initial heap allocation for sort_id widget vector pointers ...

	self.sort_id.smooth_ref_ts = ptr_new(/allocate_heap)		; Smooth Ref checkbox ID array pointer
	
;	Pass core device parameters to BASE DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"as_mex_h5_DEVICE" --> as_mex_h5_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'as_mex_H5_DEVICE', $	; unique name for this device object
		title = 'AS MEX map HDF5 file', $
		ext = '.hdf5', $		; a fixed file extension for raw data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; list-mode data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 0, $		; start detector ADC #'s at 0
		use_bounds = 0, $		; not confine charge/flux within bounded area
		array_default = 0, $	; a detect array by default
		synchrotron = 1, $		; synchrotron data
		ionbeam = 0)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro as_mex_h5_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('as_mex_h5_DEVICE')

COMPILE_OPT STRICTARR

maia = { as_mex_h5_DEVICE,  $

		INHERITS BASE_DEVICE, $			; mandatory base device parameters

;		Note we add 'version' to devpars so that it gets retained in 'da_evt' and 'spec_evt'
;		when a new obj instance is used. Do not confuse this version with the version used in
;		write and read of device options, which is defined in the 'write_options' method.

		sort_options : {sort_options_devicespec_as_mex_h5, $	; Sort EVT window Sort options panel
			version:		0.0,  $								; Listmode version number
			smooth_ref_ts:	1},$								; smooth reference time-stamp

		sort_id: {sort_id_as_mex_h5, $							; pointers to vector of sort widget IDs
			smooth_ref_ts:	ptr_new()} $						; smooth check-box ID array pointer
		}														; see the base_device super-class for details.
	return
end
