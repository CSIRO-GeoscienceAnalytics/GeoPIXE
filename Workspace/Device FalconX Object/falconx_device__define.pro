;
; GeoPIXE Device Object for FalconX listmode data
; 
; FalconX (SI-Torro XIA) is a FPGA-based pulse processor and data acquisition 
; system developed by Southern Innovation (Melbourne) and XIA. 
; Data is written by a Unix front-end processor in big-endian byte order.
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
;
; The above 3 methods (plus the init and cleanup methods) are the minimum set needed 
; to be written for a new device class.
;
; flux_scan()		scan the raw data-files for details of available ion chamber 
; 					specifications (e.g. EPICS PVs) to provide for user selection, and 
; 					select one to use, and the pre-amp sensitivity value and units.
; trim_evt_files()	trim the list of files to only include files needed for the Y range
; 					seen in the region mask arrays (uses the information in the Y lookup table; 
; 					used with EVT button on Image Regions window).
;					Y LUT for FalconX is a list of first Y value for each blog data file.
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
; name()				return name of device (e.g. "NSLS_MCA_DEVICE").
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
; method will be used in the "BASE_DEVICE" master-class. See the code in the FalconX_device
; as an example.
; 
; These options are set-up in widgets that appear in the Sort EVT options box on the
; Device tab. The parameters live in the class 'self' struct and are handled by GeoPIXE
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
; methods (see examples in FalconX_device). They are not essential and are NOT called
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
; The code in the render_options for creating options widgets for the FalconX device,
; which also calls some OnRealize routines and an event handler, can be used as a model
; for new device options fields.
;
;----------------------------------------------------------------------------------------------------

pro falconx_device::cleanup

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
		warning,'falconx_device::cleanup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_valid( self.sort_id.clear_x) then ptr_free, self.sort_id.clear_x
	if ptr_valid( self.sort_id.clear_y) then ptr_free, self.sort_id.clear_y
	if ptr_valid( self.sort_id.clear_z) then ptr_free, self.sort_id.clear_z
	if ptr_valid( self.sort_id.source_x) then ptr_free, self.sort_id.source_x
	if ptr_valid( self.sort_id.source_y) then ptr_free, self.sort_id.source_y
	if ptr_valid( self.sort_id.source_z) then ptr_free, self.sort_id.source_z
	if ptr_valid( self.sort_id.flip_icr_raw) then ptr_free, self.sort_id.flip_icr_raw
	
	self->BASE_DEVICE::cleanup
    return
end

;-------------------------------------------------------------------------------
; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written from/to DISK using the "read_options",
; "write_options" methods. Keep a local copy of device parameters and set them using
; "set_options". 
;
;-------------------------------------------------------------------------------
; These routines are associated with the rendering of Sort Options widgets
; in the Sort EVT window Sort tab options box.
; 
; Render options widgets in Sort Options box in Sort EVT window.
; Parent is the framed container box. Its child must be a base that
; all options widgets are attached to. This child is target of destroy
; when switching devices.

pro falconx_device::render_options, parent

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
		warning,'falconx_device::render_options',['IDL run-time error caught.', '', $
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

falconxmode_base = widget_base( parent, /column,  space=3, xpad=0, ypad=0, /base_align_center, $
		event_func='falconx_device_sort_option_event', uvalue=self, uname='obj-ref-here')
lab = widget_label( falconxmode_base, value='FalconX Option Parameters')

falconxtbase = widget_base( falconxmode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( falconxtbase, value='Slowest motor axis (for YLUT):')
falconx_slow = widget_combobox( falconxtbase, value=str_tidy(indgen(6)), uname='falconx-slow', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_slow', sensitive=1, $
					uvalue='Select the source motor axis that changes most slowly. If "cluster" mode is used, then the slowest axis provides the index into the "Y lookup table" (YLUT), used to divide the problem for parallel processing. ' + $
						'Note: Must equal "Y axis" to use "cluster" mode.',xsize=source_xsize)

falconxs0base = widget_base( falconxmode_base, /column, /base_align_center, xpad=1, ypad=1, space=3, xsize=axes_xsize, /frame)
lab = widget_label( falconxs0base, value='Select Motor Axis Indices for XYZ')
falconxsbase = widget_base( falconxs0base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( falconxsbase, value='X axis:')
falconx_xsource = widget_combobox( falconxsbase, value=str_tidy(indgen(6)), uname='falconx-x-source', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_x_source', sensitive=1, $
					uvalue='Select the source motor axis to use as the GeoPIXE "X" axis.',xsize=source_xsize)
lab = widget_label( falconxsbase, value='  Y axis:')
falconx_ysource = widget_combobox( falconxsbase, value=str_tidy(indgen(6)), uname='falconx-y-source', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_y_source', sensitive=1, $
					uvalue='Select the source motor axis to use as the GeoPIXE "Y" axis. Note: To use "cluster" mode, "Y" must be the "slow" axis. This is needed for the YLUT scheme to work at present.',xsize=source_xsize)
lab = widget_label( falconxsbase, value='  Z axis:')
falconx_zsource = widget_combobox( falconxsbase, value=str_tidy(indgen(6)), uname='falconx-z-source', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_z_source', sensitive=1, $
					uvalue='Select the source motor axis to use as the GeoPIXE "Z" axis, which may represent Energy or Tomo Theta.',xsize=source_xsize)

; Droplists to both enable and set the size of XYZ border margins. This replaces both the X margin droplist
; above and the check-boxes below.

falconxcbase = widget_base( falconxmode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( falconxcbase, value='Clear Axis Border X:')
falconx_clearX = widget_combobox( falconxcbase, value=str_tidy(indgen(20)), uname='falconx-clear-x', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_clear_x', sensitive=1, $
					uvalue='Clear both borders along X axis.',xsize=source_xsize)
lab = widget_label( falconxcbase, value='  Y:')
falconx_clearY = widget_combobox( falconxcbase, value=str_tidy(indgen(20)), uname='falconx-clear-y', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_clear_y', sensitive=1, $
					uvalue='Clear both borders along Y axis.',xsize=source_xsize)
lab = widget_label( falconxcbase, value='  Z:')
falconx_clearZ = widget_combobox( falconxcbase, value=str_tidy(indgen(20)), uname='falconx-clear-z', /tracking, $
					notify_realize='OnRealize_falconx_device_sort_option_clear_z', sensitive=1, $
					uvalue='Clear both borders along Z axis.',xsize=source_xsize)

; Check-boxes to flip ICR and RAW values on input

falconxfbase = widget_base( falconxmode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( falconxfbase, value='Swap:')
flip_icr_raw_check = cw_bgroup2( falconxfbase, ['ICR/RAW smart'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='falconx-flip-icr-raw', set_value=self.sort_options.flip.icr_raw, /nonexclusive, $
					uvalue=['Check box to flip (smart swap) the values for ICR and RAW during processing, if RAW is larger.'])

error = 0
add_widget_vector, self.sort_id.slow, falconx_slow, error=err & error=error or err
add_widget_vector, self.sort_id.clear_X, falconx_clearX, error=err & error=error or err
add_widget_vector, self.sort_id.clear_Y, falconx_clearY, error=err & error=error or err
add_widget_vector, self.sort_id.clear_Z, falconx_clearZ, error=err & error=error or err
add_widget_vector, self.sort_id.source_x, falconx_xsource, error=err & error=error or err
add_widget_vector, self.sort_id.source_y, falconx_ysource, error=err & error=error or err
add_widget_vector, self.sort_id.source_z, falconx_zsource, error=err & error=error or err
add_widget_vector, self.sort_id.flip_icr_raw, flip_icr_raw_check, error=err & error=error or err
if error then begin
	warning,'falconx_device::render_options','Error adding device object widget ID vectors.'
endif
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_clear_x, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_clear_x',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_clear_x: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_clear_x: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.clear.x
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_clear_y, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_clear_y',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_clear_y: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_clear_y: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.clear.y
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_clear_z, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_clear_z',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_clear_z: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_clear_z: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.clear.z
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_slow, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_slow',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_slow: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_slow: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.slow_axis
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_x_source, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_x_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_x_source: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_x_source: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.source.x
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_y_source, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_y_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_y_source: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_y_source: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.source.y
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_falconx_device_sort_option_z_source, wWidget

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
		warning,'OnRealize_falconx_device_sort_option_z_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_z_source: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_falconx_device_sort_option_z_source: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.source.z
	return
end

;------------------------------------------------------------------------------------------

function falconx_device_sort_option_event, event

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
		warning,'falconx_device_sort_option_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	if widget_info( event.handler, /valid) eq 0L then begin
;		print,'falconx_device_sort_option_event: event.handler not valid.'
		return, 0
	endif
	uname = widget_info( event.handler, /uname)
	if uname ne 'obj-ref-here' then begin
		print,'falconx_device_sort_option_event: Object base not found.'
		return, 0
	endif
	widget_control, event.handler, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'falconx_device_sort_option_event: Device Object ref not found.'
		return, 0
	endif

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		return, event						; pass context help up the line ...
		end
	else: begin
		
		uname = widget_info( event.id, /uname)
		case uname of
			'falconx-slow': begin
				obj->set_options, slowest = event.index
				end
			'falconx-clear-x': begin
				obj->set_options, clear_x = event.index
				end
			'falconx-clear-y': begin
				obj->set_options, clear_y = event.index
				end
			'falconx-clear-z': begin
				obj->set_options, clear_z = event.index
				end
			'falconx-x-source': begin
				obj->set_options, source_x = event.index
				obj->update_header_info, error=error
				obj->update_notify
				end
			'falconx-y-source': begin
				obj->set_options, source_y = event.index
				obj->update_header_info, error=error
				obj->update_notify
				end
			'falconx-z-source': begin
				obj->set_options, source_z = event.index
				obj->update_header_info, error=error
				obj->update_notify
				end
			'falconx-flip-icr-raw': begin
				obj->set_options, flip_icr_raw = event.select
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
; "set_options". The keywords here are only used internally in FalconX device.

pro falconx_device::set_options, p, $
				clear_x=clear_x, clear_y=clear_y, clear_z=clear_z, $			; x_margin=x_margin
				source_x=source_x, source_y=source_y, source_z=source_z, $
				slowest=slowest, flip_icr_raw=flip_icr_raw

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
		warning,'falconx_device::set_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(p) eq 1 then begin
		if size(p, /tname) eq 'STRUCT' then begin
			falconx = p
		endif else if ptr_good(p,/struct) then begin
			falconx = *p
		endif else return
	endif

	if n_elements(falconx) eq 1 then begin
		if tag_present('SLOW_AXIS',falconx) then self.sort_options.slow_axis = falconx.slow_axis
		if tag_present('CLEAR',falconx) then begin
			if tag_present('X',falconx.clear) then self.sort_options.clear.x = falconx.clear.x
			if tag_present('Y',falconx.clear) then self.sort_options.clear.y = falconx.clear.y
			if tag_present('Z',falconx.clear) then self.sort_options.clear.z = falconx.clear.z
		endif
		if tag_present('SOURCE',falconx) then begin
			if tag_present('X',falconx.source) then self.sort_options.source.x = falconx.source.x
			if tag_present('Y',falconx.source) then self.sort_options.source.y = falconx.source.y
			if tag_present('Z',falconx.source) then self.sort_options.source.z = falconx.source.z
		endif
		if tag_present('FLIP',falconx) then begin
			if tag_present('ICR_RAW',falconx.flip) then self.sort_options.flip.icr_raw = falconx.flip.icr_raw
		endif
		if tag_present('VERSION',falconx) then self.sort_options.version = falconx.version
	endif else begin
		if n_elements(slowest) eq 1 then self.sort_options.slow_axis = slowest
		if n_elements(clear_x) eq 1 then self.sort_options.clear.x = clear_x
		if n_elements(clear_y) eq 1 then self.sort_options.clear.y = clear_y
		if n_elements(clear_z) eq 1 then self.sort_options.clear.z = clear_z
		if n_elements(source_x) eq 1 then self.sort_options.source.x = source_x
		if n_elements(source_y) eq 1 then self.sort_options.source.y = source_y
		if n_elements(source_z) eq 1 then self.sort_options.source.z = source_z
		if n_elements(flip_icr_raw) eq 1 then self.sort_options.flip.icr_raw = flip_icr_raw
	endelse
	
	; Set value of widgets. There may be multiple sort option panels attached to this object, so
	; we use 'widget_control_vector' to set all of them.
	
	widget_control_vector, self.sort_id.slow, set_combobox_select = self.sort_options.slow_axis
	widget_control_vector, self.sort_id.clear_x, set_combobox_select = self.sort_options.clear.x
	widget_control_vector, self.sort_id.clear_y, set_combobox_select = self.sort_options.clear.y
	widget_control_vector, self.sort_id.clear_z, set_combobox_select = self.sort_options.clear.z
	widget_control_vector, self.sort_id.source_x, set_combobox_select = str_tidy(self.sort_options.source.x)
	widget_control_vector, self.sort_id.source_y, set_combobox_select = str_tidy(self.sort_options.source.y)
	widget_control_vector, self.sort_id.source_z, set_combobox_select = str_tidy(self.sort_options.source.z)
	widget_control_vector, self.sort_id.flip_icr_raw, set_combobox_select = self.sort_options.flip.icr_raw
	return
end

;-------------------------------------------------------------------

;	Returns internal self.sort_options struct.
;	Will get values of options widgets (e.g. text strings) and
;	update internal values in self struct.

function falconx_device::get_options, error=error

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
		warning,'falconx_device::get_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 0
	
;	widget_control_vector, self.sort_id.deadtime_cal_a, /first, get_value=s, error=err
;	if err eq 0 then self.sort_options.deadtime_cal.a = float2(s)
;	
;	widget_control_vector, self.sort_id.deadtime_cal_b, /first, get_value=s, error=err
;	if err eq 0 then self.sort_options.deadtime_cal.b = float2(s)
	
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

pro falconx_device::read_options, unit, options=options, error=error

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
		warning,'falconx_device::read_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

; Get current defaults in this device

	options = self->get_options()
	clear_x = options.clear.x
	clear_y = options.clear.y
	clear_z = options.clear.z
	source = options.source
	slow_axis = options.slow_axis
	si_version = options.version
	flip_icr_raw = options.flip.icr_raw
	version = 0L
	
; Read options parameters from the file

	on_ioerror, bad_io
	if self.use_version then readu, unit, version

	if version eq 803 then begin				; correction to work around an error
		si_version = version					; with the original header write
		version = -1
	endif else begin
		q = where( version eq [-1,-2,-3], nq)
		if nq eq 0 then goto, bad_io

		if version le -2 then readu, unit, si_version
	endelse

	readu, unit, clear_x, clear_y
	
	clear_x = clip(clear_x,0,19)
	clear_y = clip(clear_y,0,19)
	
	if self.use_version then begin
		if version le -1 then begin
			readu, unit, source
		endif
		if version le -1 then begin
			readu, unit, clear_z
		endif
		if version le -1 then begin
			readu, unit, slow_axis
		endif
		if version le -3 then begin
			readu, unit, flip_icr_raw
		endif
	endif
	
	clear_z = clip(clear_z,0,19)
	slow_axis = clip(slow_axis,0,2)
	
; Write back these options parameters to the device ...

	options.clear.x = clear_x
	options.clear.y = clear_y
	options.clear.z = clear_z
	options.slow_axis = slow_axis
	options.version = si_version
	if (source.x ne 0) or (source.y ne 0) or (source.z ne 0) then begin	; reject all zeroes
		options.source = source
	endif
	options.flip.icr_raw = flip_icr_raw
	
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

pro falconx_device::write_options, unit, options=p, error=error

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
		warning,'falconx_device::write_options',['IDL run-time error caught.', '', $
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
	
	version = -3L	
	on_ioerror, bad_io
	writeu, unit, version
	writeu, unit, options.version
	
	writeu, unit, options.clear.x, options.clear.y
	writeu, unit, options.source
	writeu, unit, options.clear.z
	writeu, unit, options.slow_axis
	writeu, unit, options.flip.icr_raw
	error = 0
	return
	
bad_io:
	error = 1
	return
end

;-------------------------------------------------------------------

; Is the flip (swap) ICR-RAW on?
; Use in tests: if obj->flip_icr_raw() 

function falconx_device::flip_icr_raw

COMPILE_OPT STRICTARR
		
	return, self.sort_options.flip.icr_raw
end

;-------------------------------------------------------------------------------

; Return a string to display (e.g. in Image History window) to show the
; state of this device's options. If 'p' present, then show this parameter set.

function falconx_device::options_legend, p

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
		warning,'falconx_device::read_options',['IDL run-time error caught.', '', $
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

	list = ['FalconX:' ] 
	list = [list, '   Slowest axis used for YLUT: ' + str_tidy(options.slow_axis) ]
	list = [list, '   Axes: X: ' + str_tidy(options.source.x) + ', Y: ' + str_tidy(options.source.y) + ', Z: ' + str_tidy(options.source.z) ]
	list = [list, '   Clear margins X: ' + str_tidy(options.clear.x) + ', Y: ' + str_tidy(options.clear.y) + ', Z: ' + str_tidy(options.clear.z) ]
	list = [list, '   Flip (swap) ICR-RAW: ' + str_tidy(options.flip.icr_raw) ]

	return, list
end

;-------------------------------------------------------------------
;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

function falconx_device::get_header_info, filei, output=output, silent=silent, error=error

; file		a raw data file to look for associated header, metadata
; output	if present, this is a file on the output path, if some metadata is
;			located in that path (e.g. FalconX Y LUT). 
; /silent	suppress any pop-ups.

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1;
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'falconx_device::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	;print,'FalconX device:: get_header_info: file=',filei

	if n_elements(silent) lt 1 then silent=0
	error = 1
	self.header.error = 1
	file = filei

	t = strip_file_ext(file)
	k = locate_last('_',t)
	f = strmid(t,0,k+1) + '0.' + extract_extension(file)
	if file_test(f) then file=f

;	mp = get_falconx_header( file, silent=silent, error=error)
	mp = get_falconx_header( file, error=error)
	if error then return,0

;	Y lookup table, stored with output, or on blog raw data path ...
;	The Y LUT contains a list of the first Y value in each raw data file in the multi-file set.
;	This does not need to be redone in 'update_header_info' later, so we do it here.

	if n_elements(output) gt 0 then begin
;		t = strip_file_ext(output)									; these lines are redundant as there
;		k = locate_last('_',t)										; is no _0 appended to 'output'
;		f = strmid(t,0,k+1) + '0.' + extract_extension(file)
;		if file_test(f) then output=f

		retry = 0
		ylut = get_falconx_ylut(file, output=output, error=err, $	; stored now with output
					embed_detector=self.embed_detector)
		if err or (n_elements(ylut) lt 1) then retry=1				; keep a single row as valid
	endif else retry=1
	if retry then begin
		ylut = get_falconx_ylut( file, error=err, $					; if not, try blog path
					embed_detector=self.embed_detector)
	endif	
	if err eq 0 then begin					
		*self.header.scan.pYlut = ylut	
	endif

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

pro falconx_device::update_header_info, error=error

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
		warning,'falconx_device::update_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	error = 1
	pinfo = self.old_mp
	if ptr_good(pinfo) eq 0 then return
	
	self.sort_options.version = (*pinfo).version
	self.sort_options.njson = (*pinfo).njson

	case self.sort_options.source.x of
		0: begin
			self.header.scan.x_pixels = (*pinfo).scan_dim1_extent
			self.header.scan.x_mm = (*pinfo).scan_dim1_size
			self.header.scan.x = (*pinfo).scan_dim1_origin
			self.header.scan.x_on = (*pinfo).scan_dim1_extent gt 1
			self.header.scan.x_name = (*pinfo).scan_dim1_name
			end
		1: begin
			self.header.scan.x_pixels = (*pinfo).scan_dim2_extent
			self.header.scan.x_mm = (*pinfo).scan_dim2_size
			self.header.scan.x = (*pinfo).scan_dim2_origin
			self.header.scan.x_on = (*pinfo).scan_dim2_extent gt 1
			self.header.scan.x_name = (*pinfo).scan_dim2_name
			end
		2: begin
			self.header.scan.x_pixels = (*pinfo).scan_dim3_extent
			self.header.scan.x_mm = (*pinfo).scan_dim3_size
			self.header.scan.x = (*pinfo).scan_dim3_origin
			self.header.scan.x_on = (*pinfo).scan_dim3_extent gt 1
			self.header.scan.x_name = (*pinfo).scan_dim3_name
			end
		3: begin
			self.header.scan.x_pixels = (*pinfo).scan_dim4_extent
			self.header.scan.x_mm = (*pinfo).scan_dim4_size
			self.header.scan.x = (*pinfo).scan_dim4_origin
			self.header.scan.x_on = (*pinfo).scan_dim4_extent gt 1
			self.header.scan.x_name = (*pinfo).scan_dim4_name
			end
		4: begin
			self.header.scan.x_pixels = (*pinfo).scan_dim5_extent
			self.header.scan.x_mm = (*pinfo).scan_dim5_size
			self.header.scan.x = (*pinfo).scan_dim5_origin
			self.header.scan.x_on = (*pinfo).scan_dim5_extent gt 1
			self.header.scan.x_name = (*pinfo).scan_dim5_name
			end
		5: begin
			self.header.scan.x_pixels = (*pinfo).scan_dim6_extent
			self.header.scan.x_mm = (*pinfo).scan_dim6_size
			self.header.scan.x = (*pinfo).scan_dim6_origin
			self.header.scan.x_on = (*pinfo).scan_dim6_extent gt 1
			self.header.scan.x_name = (*pinfo).scan_dim6_name
			end
		else:
	endcase
	case self.sort_options.source.y of
		0: begin
			self.header.scan.y_pixels = (*pinfo).scan_dim1_extent
			self.header.scan.y_mm = (*pinfo).scan_dim1_size
			self.header.scan.y = (*pinfo).scan_dim1_origin
			self.header.scan.y_on = (*pinfo).scan_dim1_extent gt 1
			self.header.scan.y_name = (*pinfo).scan_dim1_name
			end
		1: begin
			self.header.scan.y_pixels = (*pinfo).scan_dim2_extent
			self.header.scan.y_mm = (*pinfo).scan_dim2_size
			self.header.scan.y = (*pinfo).scan_dim2_origin
			self.header.scan.y_on = (*pinfo).scan_dim2_extent gt 1
			self.header.scan.y_name = (*pinfo).scan_dim2_name
			end
		2: begin
			self.header.scan.y_pixels = (*pinfo).scan_dim3_extent
			self.header.scan.y_mm = (*pinfo).scan_dim3_size
			self.header.scan.y = (*pinfo).scan_dim3_origin
			self.header.scan.y_on = (*pinfo).scan_dim3_extent gt 1
			self.header.scan.y_name = (*pinfo).scan_dim3_name
			end
		3: begin
			self.header.scan.y_pixels = (*pinfo).scan_dim4_extent
			self.header.scan.y_mm = (*pinfo).scan_dim4_size
			self.header.scan.y = (*pinfo).scan_dim4_origin
			self.header.scan.y_on = (*pinfo).scan_dim4_extent gt 1
			self.header.scan.y_name = (*pinfo).scan_dim4_name
			end
		4: begin
			self.header.scan.y_pixels = (*pinfo).scan_dim5_extent
			self.header.scan.y_mm = (*pinfo).scan_dim5_size
			self.header.scan.y = (*pinfo).scan_dim5_origin
			self.header.scan.y_on = (*pinfo).scan_dim5_extent gt 1
			self.header.scan.y_name = (*pinfo).scan_dim5_name
			end
		5: begin
			self.header.scan.y_pixels = (*pinfo).scan_dim6_extent
			self.header.scan.y_mm = (*pinfo).scan_dim6_size
			self.header.scan.y = (*pinfo).scan_dim6_origin
			self.header.scan.y_on = (*pinfo).scan_dim6_extent gt 1
			self.header.scan.y_name = (*pinfo).scan_dim6_name
			end
		else:
	endcase
	case self.sort_options.source.z of
		0: begin
			self.header.scan.z_pixels = (*pinfo).scan_dim1_extent
			self.header.scan.z_mm = (*pinfo).scan_dim1_size
			self.header.scan.z = (*pinfo).scan_dim1_origin
			self.header.scan.z_on = (*pinfo).scan_dim1_extent gt 1
			self.header.scan.z_name = (*pinfo).scan_dim1_name
			end
		1: begin
			self.header.scan.z_pixels = (*pinfo).scan_dim2_extent
			self.header.scan.z_mm = (*pinfo).scan_dim2_size
			self.header.scan.z = (*pinfo).scan_dim2_origin
			self.header.scan.z_on = (*pinfo).scan_dim2_extent gt 1
			self.header.scan.z_name = (*pinfo).scan_dim2_name
			end
		2: begin
			self.header.scan.z_pixels = (*pinfo).scan_dim3_extent
			self.header.scan.z_mm = (*pinfo).scan_dim3_size
			self.header.scan.z = (*pinfo).scan_dim3_origin
			self.header.scan.z_on = (*pinfo).scan_dim3_extent gt 1
			self.header.scan.z_name = (*pinfo).scan_dim3_name
			end
		3: begin
			self.header.scan.z_pixels = (*pinfo).scan_dim4_extent
			self.header.scan.z_mm = (*pinfo).scan_dim4_size
			self.header.scan.z = (*pinfo).scan_dim4_origin
			self.header.scan.z_on = (*pinfo).scan_dim4_extent gt 1
			self.header.scan.z_name = (*pinfo).scan_dim4_name
			end
		4: begin
			self.header.scan.z_pixels = (*pinfo).scan_dim5_extent
			self.header.scan.z_mm = (*pinfo).scan_dim5_size
			self.header.scan.z = (*pinfo).scan_dim5_origin
			self.header.scan.z_on = (*pinfo).scan_dim5_extent gt 1
			self.header.scan.z_name = (*pinfo).scan_dim5_name
			end
		5: begin
			self.header.scan.z_pixels = (*pinfo).scan_dim6_extent
			self.header.scan.z_mm = (*pinfo).scan_dim6_size
			self.header.scan.z = (*pinfo).scan_dim6_origin
			self.header.scan.z_on = (*pinfo).scan_dim6_extent gt 1
			self.header.scan.z_name = (*pinfo).scan_dim6_name
			end
		else:
	endcase
	if self.header.scan.x_on or self.header.scan.y_on or self.header.scan.z_on then self.header.scan.on = 1

	self.header.scan.dwell = (*pinfo).scan_dwell
	self.header.sample = (*pinfo).sample_name
	self.header.grain = (*pinfo).scan_region
	comment = '#' + str_tidy((*pinfo).run) + ' - ' + (*pinfo).run_start_time
	if strlen((*pinfo).sample_info) gt 0 then comment = (*pinfo).sample_info+ ', ' + (*pinfo).run_start_time
	self.header.title = comment

	self.header.energy = (*pinfo).beam_energy
	if self.header.energy gt 500. then self.header.energy = self.header.energy/1000.
	
	self.header.metadata.sample_type = (*pinfo).sample_type
	self.header.metadata.sample_serial = (*pinfo).sample_serial
	self.header.metadata.detector_identity = (*pinfo).detector_identity
;	self.header.metadata.facility = (*pinfo).facility
;	self.header.metadata.endstation = (*pinfo).endstation

	self.header.sensitivity = (*pinfo).IC_sensitivity
	self.header.IC_name = (*pinfo).IC_name
	self.header.detector[*] = 7
;	self.header.cal = (*pinfo).cal
;	?? channel = (*pinfo).channel

	self.header.error = 0
	error = 0
	return
end

;-------------------------------------------------------------------

; This method is called to see if there are extra data/detector planes that should be
; accumulated. It returns their name strings. If these are returned, then
; the 'read_buffer' method should accumulate these as extra planes of the 'flux'
; array if the flux array is passed with 3 dimensions.

function falconx_device::get_attribute_list, error=error

; error=1 and return '' if no list

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
		warning,'falconx_device::get_attribute_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 0
	return, ['Flux0','Flux1']
end

;-------------------------------------------------------------------

; Scan raw data files for device specific flux IC PV information
 
pro falconx_device::flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
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
; 	IC_name		PV selected by user from list (if suppress=0)
; 	IC_val		pre-amp sensitivity value (if suppress=0)
; 	IC_vunit	pre-amp sensitivity unit multipler (if suppress=0)
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
		warning,'falconx_device::flux_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_falconx_2, falconx_swap,length,skip,x0,y0,z0,u0,v0,w0
common c_falconx_3, monitor_array, n_monitor, falconx_IC_name
common c_falconx_4, n_times, time_last
;common c_falconx_5, xanes_energies, do_xanes
common c_falconx_7, falconx_energy_pv
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_falconx_17, falconx_x_range, falconx_y_range, falconx_z_range, falconx_3D
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
if n_elements(falconx_IC_name) lt 1 then falconx_IC_name=''

	if n_elements(first) lt 1 then first = 1
	if n_elements(suppress) lt 1 then suppress = 0
	if n_elements(image_mode) lt 1 then image_mode = 1
	if n_elements(nsls_debug) lt 1 then nsls_debug = 0
	
	PV_list = 'none'
	IC_name = ''
	IC_val = 0.
	IC_vunit = 0.
	dwell = 0.
	no_pv = 0
	use_dwell = 0
	error = 1
	
	if first then begin
		if image_mode then begin
			dwell = self.header.scan.dwell
			use_dwell = 1
		endif
		
		IC_name = self.header.IC_name
		PV_list = IC_name
		check_plist_falconx, PV_list	
	
		IC_sensitivity = self.header.sensitivity
		if IC_sensitivity ne 0.0 then begin
			IC_val = charge_gain_units( IC_sensitivity, units=IC_vunit)
		endif
	endif

	error = 0
	return
end

;-------------------------------------------------------------------

pro falconx_device::check_pv_list, plist

; Check the PV list pointed to by 'plist'. Add any FalconX default PVs
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
		warning,'falconx_device::check_pv_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	check_plist_falconx, plist
	return
end

;-------------------------------------------------------------------

; Indicates that this device support a Y lookup table that matches the Y value
; at the start of each raw EVT list-mode data-file. 

function falconx_device::ylut

COMPILE_OPT STRICTARR
		
	ok = self.header.scan.use_ylut
	if ok eq 0 then return, ok
	ok3 = 1
	
;	If the cluster stripes and Y windowing both mixed up in 'DA_evt', then use this ...

	if self.sort_options.source.y ne self.sort_options.slow_axis then ok3=0

	return, ok and ok3
end

;-------------------------------------------------------------------

; Build a Y lookup table file
 
function falconx_device::build_ylut, files, output=output, error=error, force=force

; Build a Y lookup table file that lists the first Y value
; for the list-mode data files given by all files matching 'files' file-name root dot *.
; Use the 'output' file path by default, else base it on the input 'files'.
; Test whether any existing YLUT file is valid for all 'files' listed, else re-build it.

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
		warning,'falconx_device::build_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
if n_elements(force) eq 0 then force=0

	if force then goto, build
	ylut = self->get_ylut( files, output=output, /strip, error=error)
	if error then goto, build
	return, ylut
	
build:	
	ylut = build_falconx_ylut( files[0], slow_axis=self.sort_options.slow_axis, output=output, error=error, $
				embed_detector=self.embed_detector)
	
	if error eq 0 then begin
		*self.header.scan.pYlut = ylut
	endif
	return, ylut
end

;-------------------------------------------------------------------

; Build a Y lookup table file
 
pro falconx_device::delete_ylut, files, output=output, error=error

; Delete a Y lookup table file that lists the first Y value.
; Use the 'output' file path by default, else base it on the input 'files'.
; Test whether any existing YLUT file is valid for all 'files' listed, else re-build it.

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
		warning,'falconx_device::delete_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	delete_falconx_ylut, files[0], output=output, error=error
	return
end

;-------------------------------------------------------------------

function falconx_device::get_ylut, files, strip=strip, output=output, error=error

; Read a Y lookup table file that lists the first Y value for each list-mode data file.
; Use the 'output' file path by default, else base it on the input 'files'.
; Check that 'ylut' spans all of 'files', else return 'error'.

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
		warning,'falconx_device::get_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif

	ylut = get_falconx_ylut( files, strip=strip, output=output, error=error, $
					embed_detector=self.embed_detector)
	
	nf = n_elements(files)
	fs1 = strip_file_ext(files)
	l = locate_last('_',fs1)
	ext = fs1
	for i=0,nf-1 do ext[i] = strmid(fs1[i],l[i]+1)
	q = where( inumeric(ext), nq)
	if nq gt 0 then begin
		inum = long( ext[q])
		if max(inum) ge n_elements(ylut) then error=1
	endif

	if error eq 0 then begin
		*self.header.scan.pYlut = ylut
	endif
	return, ylut
end

;-------------------------------------------------------------------

function falconx_device::range_ylut, files, error=error

; Return the range of Y values sampled by this group of 'files'.
; But keep this within range in YLUT.

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
		warning,'falconx_device::range_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif

	error = 1
	if n_elements(files) lt 1 then return, 0L
	nylut = n_elements(*self.header.scan.pYlut)
	if nylut lt 1 then return, 0L
	
	nf = n_elements(files)
	fs1 = strip_file_ext(files)
	l = locate_last('_',fs1)
	num = fs1
	for i=0,nf-1 do num[i] = strmid(fs1[i],l[i]+1)
	q = where( inumeric(num), nq)
	if nq eq 0 then return, 0L
	
	num = long2( num[q])
	q1 = where( (num ge 0) and (num lt nylut), nq1)
	if nq1 eq 0 then return, 0L
	
	ymin = min((*self.header.scan.pYlut)[num[q1]])
	ymax = max(num[q1]) + 1
	if ymax ge nylut then begin
		ymax = 100000L
	endif else begin
		ymax = (*self.header.scan.pYlut)[ymax]
	endelse
	error = 0	
	
	return, {min:ymin, max:ymax}
end

;-------------------------------------------------------------------

; Write a Y lookup table file
 
pro falconx_device::write_ylut, ylut, file, output=output, strip=strip, error=error

; Write out a Y lookup table that lists the first Y value
; in each segment file, indexed by segment file number (extension).
; Use the 'output' file path by default, else base it on the input 'file'.
; If output has a 'nn' after DAI, then append this too.
; /strip remove trailing index after ".ylut" for cluster output files.

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
		warning,'falconx_device::write_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	write_falconx_ylut, ylut, file, output=output, strip=strip, error=error
	return
end

;-------------------------------------------------------------------

; Trim EVT file list for Y lookup table
 
function falconx_device::trim_evt_files, files, mask=pmask, pYlut=pYlut, $
								yoffset=yoffset, yrange=yrange

; Trim the list of 'files' to only include files needed for the Y range
; and offset selected, or as seen in the region mask arrays.
;
; yoffset	Y offset (not compressed)
; yrange	Y range (not compressed)
; pmask		ptr to region mask ptr array
; pYlut		pointer to Y LUT for blog data files
;			Y LUT for FalconX is a list of first Y value for each blog data file

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
		warning,'falconx_device::trim_evt_files',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif

;	Take care as yoffset in 'pmask' is post-compress, while 'yoffset' argument is pre-compressed.

if n_elements(pYlut) lt 1 then goto, bad
if ptr_valid(pYlut) eq 0 then goto, bad
if ptr_good(pmask) then begin
    q = where( ptr_valid( *pmask ) eq 1, nq)
    if nq eq 0 then goto, bad	
	p = (*pmask)[0]
	ny = (*p).ny
	ny0 = ny * (*p).ycompress							; original (pre-compress) scan dimensions (pixels)
	yoffset1 = (*p).yoffset								; Y offset of original scan if sub-region
	yoffset = yoffset1 * (*p).ycompress
	top0 = ny0 + yoffset-1
	top = ny + yoffset1-1
	use_mask = 1
endif else begin
	if n_elements(yrange) eq 0 then goto, bad
	if n_elements(yoffset) eq 0 then yoffset=0L
	ny0 = yrange
	yoffset1 = yoffset									; no compress in this mode?
	top0 = ny0 + yoffset-1
	top = top0
	use_mask = 0
endelse

    n = n_elements(files)
    if n eq 0 then return, ''
    if n eq 1 then goto, bad
    
;	mask of all Y values (uncompressed, no Y offset) spanned by 'files'

	yfiles = bytarr(top0+1)								; flags Y values used in files
	itag = lonarr(top0+1)								; itag[] equals file # for each Y
	np = n_elements(*pYlut)
	nf = n_elements(files)
	fs1 = strip_file_ext(files)
	l = locate_last('_',fs1)
	ext = fs1
	for i=0,nf-1 do ext[i] = strmid(fs1[i],l[i]+1)
	jlast = long(ext[n-1])
	if (jlast eq 0) and (n gt 1) then goto, bad_table	; catch zero as last entry in table' error
	if (jlast lt 0) or (jlast ge np) then goto, bad_index
	ny1 = (*pYlut)[jlast]
	if (ny1 lt 0) then goto, bad_index
	if ny1 le top0 then begin
		yfiles[ny1:top0] = 1
		itag[ny1:top0] = n-1							; extend last to top
	endif
	for i=n-2,0,-1 do begin
		j = long(ext[i])
		ny1 = (*pYlut)[j]
		ny2 = (*pYlut)[jlast] < top0
		if ny2 ge ny1 then begin
			yfiles[ny1:ny2] = 1
			itag[ny1:ny2] = i							; index to files[]
		endif
		jlast = j
    endfor

	if use_mask then begin

;		mask of Y values spanned by region masks or by yoffset,yrange
    
		yfiles2 = congrid( yfiles, top+1, /center)		; resize to compressed 'ny'
		itag2 = congrid( itag, top+1, /center)
		
	    ymask = bytarr(top+1)							; flags Y used in regions
	    for i=0L,nq-1 do begin
	       pq = (*(*pmask)[q[i]]).q						; pointer to 'q' array
	       q_to_xy, *pq, (*p).nx, x,y
	       y = y + yoffset1
	       
	       q2 = where(y le (top), nq2)
	       if nq2 gt 0 then ymask[y[q2]] = 1			; 'y' values used in region
	    endfor
	endif else begin

;		mask of Y values spanned by region masks or by yoffset,yrange
    
		yfiles2 = yfiles								; flags Y present
		itag2 = itag									; index to file
		
	    ymask = bytarr(top+1)							; flags Y used in regions
	    y1 = yoffset1 < top
	    y2 = top
	    ymask[y1:y2] = 1								; 'y' values used in region
	endelse
	
	fmask = bytarr(n)
	q = where( yfiles2 and (ymask eq 1), nq)
	if nq eq 0 then goto, bad_files
	for i=0L,nq-1 do begin
		fmask[ itag2[q[i]] : itag2[ (q[i]+1) < top ] ] = 1	; flags file indices needed
	endfor
    
	q = where(fmask eq 1, nq)
	if nq eq 0 then goto, bad_files
    
	return, files[q]
    
bad_files:
	warning,'falconx_device::trim_evt_files','No FalconX files contain the selected region(s).'
	return, ''
bad_index:
	warning,'falconx_device::trim_evt_files',['Bad file YLUT index.','Data files exceed YLUT table.','', $
			'Delete the old YLUT file, and it will be regenerated to include missing data files.']
	return, ''
bad_table:
	warning,'falconx_device::trim_evt_files',['Bad YLUT lookup table for files: '+files[0]+' ...', $
			'Delete the old YLUT file, and it will be regenerated.']
	return, ''
bad:
	return, files
end

;-------------------------------------------------------------------

function falconx_device::get_dwell, error=error

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
		warning,'falconx_device::get_dwell',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_falconx_13, falconx_dwell

	error = 0
	return, falconx_dwell / float( self->pass_count())
end

;-------------------------------------------------------------------

function falconx_device::get_dead_weight, error=error

; Return the internal dead-time weight image array.
; If none found (zero), then return error to not use weights.

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
		warning,'falconx_device::get_dead_weight',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_falconx_21, falconx_weight

	error = 0
	if total(falconx_weight) lt 0.1 then error=1		; no raw counts found

	return, falconx_weight
end

;-------------------------------------------------------------------

function falconx_device::get_dead_weight_mode, error=error

; Return the internal dead-time weight mode
;		0	weight is incoming count estimate
;		1	weight is outgoing raw count weight

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
		warning,'falconx_device::get_dead_weight_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_falconx_21, falconx_weight

	mode = 1

	error = 0
	return, mode
end

;-------------------------------------------------------------------

;	For some devices, the accumulated flux may be multiply counted.
;	This method is used to normalize these back to true per pixel flux.

function falconx_device::pass_count

COMPILE_OPT STRICTARR
common c_falconx_23, falconx_det_flag

	if self.embed_detector eq 1 then begin					; merge device mode (det # from file-name)
		return, total(falconx_det_flag) > 1
	endif else begin
		return, 1
	endelse
end

;-------------------------------------------------------------------

; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.

function falconx_device::read_setup, unit, xrange,yrange, first=first, $
			n_guide,progress_file, charge=charge, ecompress=ecompress, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			suppress=suppress, ic=flux_ic, x_coords=x_coords, $
			y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units, $
			zrange=zrange, z_coords=z_coords, z_coord_units=z_coord_units

;   Device specific list-mode (event-by-event) data file read set-up routine.
;   Remember, channel starts at 0 (-1 means any channel/ADC).
;
; input:
;   unit		read unit number
;   xrange		full X size of scan (pixels)
;   yrange		full Y size of scan (pixels)
;   zrange		full Z size of scan (pixels)
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
;   /suppress	suppress any pop-up windows
;
; return:
; 	n_guide		an event number to guide reporting in the progress bar
;   progress_file provide progress by (1) file advance/ (2) spectra # rather than (0) records/events
;   charge		charge for whole scan (uC), if available from file
;   beam_energy	beam energy, if it's available from header (MeV=ionbeam, keV=synchrotron)
;
;	x_coords	vector of physical coordinates for X pixels (if available from data files)
;	x_coord_units units string for X coords
;	y_coords	vector of physical coordinates for y pixels
;	y_coord_units units string for Y coords
;	z_coords	vector of physical coordinates for Z pixels
;	z_coord_units units string for Z coords
;	
;   flux		flux array (matches image dimensions for scan), accumulated here or in read_buffer
;   			extra planes are for added 'attributes', as defined by 'get_attributes()' method.
;   			'flux' is scalar for spectrum mode.
;   dead_fraction DT loss-fraction array (matches image dimensions for scan, matches n_detectors in spectrum mode),
;   			accumulated here or in read_buffer
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
		warning,'falconx_device::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_falconx_2, falconx_swap,length,skip,x0,y0,z0,u0,v0,w0
common c_falconx_3, monitor_array, n_monitor, falconx_IC_name
common c_falconx_11, falconx_hw_scaler, falconx_fixed_dwell
common c_falconx_12, falconx_flux_mode
common c_falconx_13, falconx_dwell
common c_falconx_21, falconx_weight
common c_falconx_22, falconx_det
common c_falconx_23, falconx_det_flag
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_falconx_17, falconx_x_range, falconx_y_range, falconx_z_range, falconx_3D
common c_falconx_18, fx3_last
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
if n_elements(xanes) lt 1 then xanes = 0
falconx_3D = 0
if n_elements(zrange) eq 0 then zrange=1
if zrange gt 1 then falconx_3D=1
	x_coord_units = ''
	y_coord_units = ''
	z_coord_units = ''

	if n_elements(falconx_det_flag) eq 0 then begin
		falconx_det_flag = intarr(384)
	endif
	if first then falconx_det_flag[*] = 0

		if first then begin
			err = init_fx()
			if err then goto, bad_io

			gprint,level=2, 'FalconX device: flip.icr_raw = ', self.sort_options.flip.icr_raw

			self.spectrum_mode = 1
			if (n_elements(flux) ge 2) then self.spectrum_mode=0
			
;	Hardware counters "FalconX:scaler.FC0" and "FalconX.scaler.FC1" are already counts in a pixel. The scale needed
;	for these is just sensitivity val*unit. For Epics PV's in c/s we need to scale by dwell time (s).

			falconx_IC_name = flux_ic.pv
			falconx_hw_scaler = 0
			falconx_flux_mode = 0
			if strmid(falconx_IC_name,0,14) eq 'FalconX:scaler' then begin
				falconx_hw_scaler = 1
				falconx_flux_mode = 1
				if falconx_IC_name eq 'FalconX:scaler.FC1' then falconx_flux_mode=2
				if falconx_IC_name eq 'FalconX:scaler.FC2' then falconx_flux_mode=3
				if falconx_IC_name eq 'FalconX:scaler.FC3' then falconx_flux_mode=4
			endif
			nsls_flux_scale = flux_ic.val * flux_ic.unit
			if (falconx_hw_scaler eq 0) and (self.spectrum_mode eq 0) and (xanes eq 0) then begin
				nsls_flux_scale = nsls_flux_scale * flux_ic.dwell*0.001		; scale by dwell only for maps
			endif
			falconx_fixed_dwell = flux_ic.dwell
			gprint,level=2,' falconx_device::read_setup: falconx_IC_name = ',falconx_IC_name,', nsls_flux_scale = ', nsls_flux_scale

;	Set-up an internal buffer to hold a dwell array, and a total elapsed time

			if self.spectrum_mode then begin
				falconx_dwell = flux[0]
				falconx_weight = dead_fraction							; dead-time weight array (by detector #)
			endif else begin
				falconx_dwell = flux[*,*,0]								; to replicate size and type as flux
				falconx_weight = dead_fraction							; dead-time weight array, same as dead_fraction
			endelse														; should dwell be zeroed here, or elsewhere?
			falconx_dwell[*] = 0.0										; zero only for 'first'
			falconx_weight[*] = 0.0										
			self.dwell_total = 0.0
			fx3_last = 0.
			
			falconx_swap = (big_endian() ne self->big_endian())

;	# buffers to read before Progress = 100,000/n_guide > 1. N_buffer MUST be divisable by 4 for Fortran code.
;	Take care increasing n_buffer, as the falconx_tt sum in spectrum mode will get large. Absolute limit is about 2M bytes.

			n_buffer = 500000L				; ~500K byte buffer (0.5 MBytes)
;			n_buffer = 7300L				; test
			n_guide = 50000L				; progress every 10th buffer (5 Mb)
			nsls_IC = 0.0					; in c_nsls_1

			x0 = 0							; in c_falconx_2
			y0 = 0
			z0 = 0
			u0 = 0
			v0 = 0
			w0 = 0
			
			falconx_x_range = long(xrange)
			falconx_y_range = long(yrange)
			falconx_z_range = long(zrange)
			
			event_array = bytarr(n_buffer)
		endif

		if self.embed_detector eq 1 then begin						; merge device mode
			stat = fstat(unit)
			name = strip_path(strip_file_ext(stat.name))
			falconx_det = 0
			l = locate_last( '_', name)								; "_" before sequence number
			if l gt 0 then begin
				m = locate_last( '_', strmid(name,0,l))				; "_" before detector number
				if m gt 0 then begin
					falconx_det = long2(strmid(name,m+1,l-m-1))		; detector number
				endif
			endif
		endif

;	Need to read the header for each silist file in case it has a JSON string.

		head = read_falconx_header( unit, /no_scan, /silent, error=err32)
		if err32 eq 0 then begin
			self->save_header_data, head							; save raw device data 'mp' in self
			self->update_header_info, error=error					; update self.header using saved 'mp'
			
			gprint,level=2, 'FalconX device: flip.icr_raw = ', self.sort_options.flip.icr_raw

			point_lun, unit, self.sort_options.njson				; skip over JSON header string
			beam_energy = self.header.energy
		endif
		
		ibranch = 0							; in c_sandia_6
		tag = 0								; in c_sandia_7
		length = 0L							; in c_falconx_2
		skip = 0L							; in c_falconx_2
		i_buffer = 0L

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

; read_buffer()		called repeatedly to read buffers from the data file, process
;					these to extract X,Y,E,T data, tagged by detector channel,
;					compress X,Y,E if needed, and optionally detect other
;					information (e.g. flux/charge, energy tokens). 

function falconx_device::read_buffer, unit, x1,y1,e, channel_oni,n, xcompress,ycompress, z1=z1, $
		station_e=ste, time=t, veto=veto, ecompress=ecompress, multiple=multiple, $
		xoffset=xoffset, yoffset=yoffset, title=title, file=file, error=error, $
		flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
		total_processed=processed, processed=count1, valid=good, total_bad_xy=bad_xy, raw_xy=raw_xy, $
		by_odd=by_odd, by_even=by_even, support_even_odd=support_even_odd, $
		zcompress=zcompress

;   Device specific list-mode (event-by-event) data file reading routine.
;   Remember, channel starts at 0 (-1 means any/all channel/ADCs).
;
; input:
;   unit		read unit number
;   file		filename passed for multi-file XY checking
;   channel_on	ADC channel(s) on/off flag vector
;   xcompress	desired X axis compression
;   ycompress	desired Y axis compression
;   zcompress	desired Z axis compression
;   ecompress	desired E axis compression (needs to match DA energy calibration)
;   xoffset		offset X by this (i.e. subtract this) - in pre-compress units
;   yoffset		offset Y by this
;   /raw_xy		suppresses X,Y compression and offset
;   flux		optional array that comes in to be updated with pixel flux
;   			2D: extra planes are for added 'attributes', as defined by 'get_attributes()' method.
;   			3D: multiple planes for all Z pixels.
;   dead_fraction for some this is an array that comes in to be updated with pixel dead_fraction.
;   			3D: multiple planes for all Z pixels; 2D: 1 plane only.
;				If flux is already DT corrected, "live" flux, then set dead-fraction zero.
;			
;	/by_odd		only for odd rows
;	/by_even	only for even rows
;
; return:
;   e			energy vector (uintarr) returned
;   t			Time-over-threshold, for some DAQs (e.g. FalconX)
;   x1			X vector (uintarr) return
;   y1			Y vector (uintarr) return
;   z1			Z vector (uintarr) return
;   ste			ADC number vector (uintarr) for each returned event (less offset, starting at 0)
;	veto		(optional) vector (uintarr) indicates a vetoed event (use this as events are rejected)
;	n			number of (x,y,e,t,ste) events returned (may include veto=1 pseudo events for BT, FC0, FC1)
;   multiple	if this has same dimensions as e, then it indicates multiple
;          		events with the same x1,y1,e,t.
;   beam_energy	beam energy, if it's available from monitor records (MeV=ionbeam, keV=synchrotron)
;   good		number of good events, or zero
;   count1		number of events processes in this buffer
;   bad_xy		increment passed in value of total event with bad X,Y codes
;   processed	increment total number of events processed.
;   title		run title
;   error		error=1 flags an error to abort
;
;	support_even_odd	=1 for FalconX device (it supports selecting only even or odd Y rows)

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
		warning,'falconx_device::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_falconx_2, falconx_swap,length,skip,x0,y0,z0,u0,v0,w0
common c_falconx_3, monitor_array, n_monitor, falconx_IC_name
common c_falconx_6, falconx_y_min
common c_falconx_13, falconx_dwell
common c_falconx_15, falconx_fx, falconx_tags, falconx_pseudo
common c_falconx_16, falconx_0, falconx_1, falconx_2, falconx_e, falconx_t, falconx_ste, falconx_veto
common c_falconx_19, falconx_3, falconx_4, falconx_5
common c_falconx_21, falconx_weight
common c_falconx_22, falconx_det
common c_falconx_23, falconx_det_flag
common c_falconx_11, falconx_hw_scaler, falconx_fixed_dwell
common c_falconx_12, falconx_flux_mode
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_falconx_17, falconx_x_range, falconx_y_range, falconx_z_range, falconx_3D
common c_falconx_18, fx3_last
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_nsls_9, nsls_debug
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc

		if n_elements(xanes) lt 1 then xanes = 0
		if n_elements(zcompress) lt 1 then zcompress=1
		support_even_odd = 1
		if n_elements(by_odd) lt 1 then by_odd=0
		if n_elements(by_even) lt 1 then by_even=0
		if by_odd then by_even=0

		channel_on = channel_oni
		if self.embed_detector eq 1 then begin					; merge device mode (det # from file-name)
			channel_on[*] = 1									; so accept all det# in file (probably all #0)
		endif													; actual det# selected by 'falconx_det' below

;		Find the number of active detectors selected 'nqc'

		nc = n_elements( channel_on)							; channel on/off flag for each channel
;		qc = where( channel_on eq 1, nqc)

		on_ioerror, bad_io
		read_event_buffer, unit, self, n_actual, bytes=1
		if n_actual eq 0 then begin
			gprint,level=2,'falconx_device::read_buffer: bad n_actual'
			goto, bad_io
		endif
		i_buffer = i_buffer+1
		
		if n_elements(falconx_0) eq 0 then begin				; just assign these 
			n_events = n_buffer/4								; vectors first time
			falconx_e = uintarr(n_events, /nozero)
			falconx_t = uintarr(n_events, /nozero)				; e,t per event
			falconx_0 = intarr(n_events, /nozero)				; x,y,z per event (unlike many devices, we will
			falconx_1 = intarr(n_events, /nozero)				; accept negative x,y,z,u,v,w)
			falconx_2 = intarr(n_events, /nozero)
			falconx_3 = intarr(n_events, /nozero)
			falconx_4 = intarr(n_events, /nozero)
			falconx_5 = intarr(n_events, /nozero)
			falconx_ste = uintarr(n_events, /nozero)			; 'station', i.e. detector #
			falconx_veto = uintarr(n_events, /nozero)			; flag a rejected event
			falconx_tags = uintarr(n_events, /nozero)			; can be used to retrieve debug event info
			falconx_pseudo = uintarr(n_events, /nozero)			; pseudo event (i.e. select, FC0, FC1, dwell, DT, raw in records added as events)
			n_fx = 6
			falconx_fx = fltarr(n_fx,n_events, /nozero)			; selected flux (Epics or h/w), FC0, FC1, dwell, DT, raw for each event
		endif else begin
			n_events = n_elements(falconx_e)
			n_fx = n_elements(falconx_fx[*,0])
		endelse
		n = 0L
		nm = 0L
		good = 0L
		debug = 0L
		xcompress = xcompress > 1
		ycompress = ycompress > 1
		zcompress = zcompress > 1
		gprint, level=1, 'falconx_device::read_buffer: falconx_swap = ', falconx_swap
;		y0 = fix( y0 > yoffset)

;		Veto=0	real events returned in e,t,x1,y1,z1,ste vectors.
;		Veto=1	pseudo events (flux and dwell) values.
;		'n' returns high water mark in vectors, total number of events.
;		NOTE: for encoder_y_mode=1 to work falconx_x_range must be actual scan X range.
;		Note that x0,y0,z0 keep current place of axis 0,1,2 (not really x,y,z).
;		Note: do not pass 'tick', will use default from header read.
		
		if self.embed_detector eq 1 then begin					; merge device mode (det # from file-name)
			if channel_oni[falconx_det] eq 0 then begin
				return, 0
			endif else begin
				falconx_det_flag[falconx_det] = 1				; this detector sighted (needed for norm later)
			endelse
		endif

		err = FalconX_events4( event_array,n_actual, channel_on,nc, falconx_e,falconx_t,falconx_0,falconx_1,falconx_2, $
				falconx_3,falconx_4,falconx_5, falconx_ste,falconx_veto,falconx_tags,n_events,n, $
				falconx_fx,n_fx, falconx_flux_mode, self.sort_options.flip.icr_raw, x0,y0,z0,u0,v0,w0, ibranch,falconx_swap, $
				tag,length,skip, bad_xy,debug, self.sort_options.version )

		nsls_debug = debug
		if err ne 0 then begin
			gprint,level=2,'falconx_device::read_buffer: error (',err,') return from FalconX_events4'
			if bad_xy then gprint,level=2,'		... and bad xy'
			goto, bad_io
		endif

		if n eq 0 then begin
;			gprint,level=2,'falconx_device::read_buffer: Zero "n" return from FalconX_events4'
			return, 0
		endif

;		Do the redirection from axes 012345 to XYZ here. This is quite free, except for the YLUT action.
;		The code in da_evt, etc. is really geared towards YLUT working with real Y (Yoffset, Yrange3, etc.).
;		Hence, for now, we can ONLY have YLUT (falconx_y_min) working with Y. This means for cluster processing
;		we can only have full 3D modes such as: EXY, XEY, theta-XY, X-theta-Y (theta-YX will not work).
		
		case self.sort_options.source.x of
			0: x1 = falconx_0[0:n-1]
			1: x1 = falconx_1[0:n-1]
			2: x1 = falconx_2[0:n-1]
			3: x1 = falconx_3[0:n-1]
			4: x1 = falconx_4[0:n-1]
			5: x1 = falconx_5[0:n-1]
		endcase
		case self.sort_options.source.y of
			0: y1 = falconx_0[0:n-1]
			1: y1 = falconx_1[0:n-1]
			2: y1 = falconx_2[0:n-1]
			3: y1 = falconx_3[0:n-1]
			4: y1 = falconx_4[0:n-1]
			5: y1 = falconx_5[0:n-1]
		endcase
		case self.sort_options.source.z of
			0: z1 = falconx_0[0:n-1]
			1: z1 = falconx_1[0:n-1]
			2: z1 = falconx_2[0:n-1]
			3: z1 = falconx_3[0:n-1]
			4: z1 = falconx_4[0:n-1]
			5: z1 = falconx_5[0:n-1]
		endcase
		case self.sort_options.slow_axis of						; min value for slow axis for "YLUT"
			0: falconx_y_min = min(falconx_0[0:n-1])
			1: falconx_y_min = min(falconx_1[0:n-1])
			2: falconx_y_min = min(falconx_2[0:n-1])
			3: falconx_y_min = min(falconx_3[0:n-1])
			4: falconx_y_min = min(falconx_4[0:n-1])
			5: falconx_y_min = min(falconx_5[0:n-1])
		endcase

		if self.embed_detector eq 1 then begin					; merge device mode (det # from file-name)
			falconx_ste[0:n-1] = falconx_det
		endif

		e = falconx_e[0:n-1]
		t = falconx_t[0:n-1]
		ste = falconx_ste[0:n-1]
		veto = falconx_veto[0:n-1]		
		
;		n		high water mark in event vector buffers
;		good	number of valid events, so far

		q = where( veto eq 0, good, complement=qveto, ncomplement=n_veto)	; good (and pseudo) events
		falconx_pseudo[*] = 0
		if n_veto gt 0 then falconx_pseudo[qveto] = 1
		
;		qt = where( y1 ge 1, nqt)
;		if nqt gt 0 then begin
;		if good gt 0 then begin
;			print,'debug ...'
;		endif

;		To select only odd rows, veto even ones, and vica versa).
;		But take care to skip the pseudo events (veto=1).

		if good gt 0 then begin
			if support_even_odd and (by_odd or by_even) then begin		; only include odd or even lines
				yodd = by_odd ? 0 : 1
				q1 = where( (2*(y1/2) eq y1-yodd) and (veto eq 0), nq1)
				if nq1 gt 0 then veto[q1] = 1
			endif
		endif
		q = where( veto eq 0, good)

;		Compress all, not just good events, so that pseudo events get corrected too ...
		
		xoffset2 = xoffset
		yoffset2 = yoffset
		zoffset2 = 0
		if raw_xy eq 0 then begin
			e = e / uint(ecompress)
			x1 = x1 / xcompress
			y1 = y1 / ycompress
			z1 = z1 / zcompress
			xoffset2 = xoffset2 / xcompress
			yoffset2 = yoffset2 / ycompress
			zoffset2 = zoffset2 / zcompress
		endif

		xrange2 = long( falconx_x_range / xcompress)	; full image range, compressed
		yrange2 = long( falconx_y_range / ycompress)
		zrange2 = long( falconx_z_range / zcompress)

		if (self.spectrum_mode eq 0) and (xanes eq 0) then begin				; images

;			This scales the count with compression for PV rates. H/w counters use the accumulation approach.
;			Ignore border pixel data for mapping (where flux is not scalar) ...
;			Offset increment into 'flux' by (compressed) offset for this stripe.
;			Combine all detectors, so divide DTcal by the number active 'nc'.

			xmin = self.sort_options.clear.x + (xoffset2 < 0)					; optionally clear border pixels
			ymin = self.sort_options.clear.y + (yoffset2 < 0)					; offsets can be negative to recover offset scans
			zmin = falconx_3D ? (self.sort_options.clear.z + (zoffset2 < 0)) : 0

;			For a detector array selected, need to divide accumulated DT by 'nqc'.
;			This approximation may not be good for multiple passes on a pixel where each has very different count
;			rate and dead time. This is not done as yet because the FalconX implementation only allows a single
;			detector channel in a file series, so we can only do one detector at a time at present.

;			dta = 1./float(nqc > 1)
			
;			Test border, and offset all, not just good events, so that pseudo events get offset ...
		
			zbad = (falconx_3D eq 0) ? 0 : ((z1 lt zmin) or (z1 gt (zrange2-1-(zmin>0))))
			q1 = where( (x1 lt xmin) or (y1 lt ymin) or (x1 gt (xrange2-1-(xmin>0))) or (y1 gt (yrange2-1-(ymin>0))) or zbad, nq1)
			if nq1 gt 0 then begin
				veto[q1] = 1						; veto border events
				falconx_pseudo[q1] = 0				; also do not include flux, etc. for border
			endif
			q = where( veto eq 0, good)
			
			x1 = uint(x1 - xoffset2)
			y1 = uint(y1 - yoffset2)
			z1 = uint(z1 - zoffset2)
			
;			Accumulate flux (including attributes), dead_fraction and dwell ...
;			Do not ignore veto 'events' (these may be pseudo events). Use flux_mode to select action:
;				0	Epics PV as a rate, so set the flux in a pixel to falconx_fx[0,*], if it is not zero
;						scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
;				1,2	H/W flux counter used and accumulated in falconx_fx[0,*] in Fortran, so accumulate falconx_fx[0,*] 
;						here in flux[*,*,0] scaled by: nsls_flux_scale
;				
;					Use FC0 in falconx_fx[1,*] to accumulate into flux[*,*,1]
;					Use FC1 in falconx_fx[2,*] to accumulate into flux[*,*,2]
;			
;				* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
;
;			Dwell:
;				Use fx[3,*] to set Dwell in maia_dwell (ms)
;
;			Dead_fraction:
;				Use fx[4,*] to set dead-time weighted by raw OCR count (fx[5,*]) in dead_fraction.
;				This is then dead_fraction weighted by dwell time (ms, fx[3,*]) and by OCR weight (fx[5,*]).
;				Later (in da_evt) norm this using both the dwell (ms) and weight arrays.
;				NOTE: This does NOT have the 1/nqc weighting applied for a detector array.
;
;			Need to make use of 'self.sort_options.flip.icr_raw' in the dtfx routines ...

		 	if falconx_3D then begin
				falconx_accumulate_dtfx_3D, /image, x1,y1,z1,ste,veto,falconx_fx,n,n_fx, falconx_flux_mode, nsls_flux_scale, $
		 				n_elements(flux[*,0,0]),n_elements(flux[0,*,0]),n_elements(flux[0,0,*]), dead_fraction, pseudo=falconx_pseudo, $
		 				flux=flux, weight=falconx_weight, dwell=falconx_dwell, xcompress=xcompress,ycompress=ycompress, error=err	;dta
		 	endif else begin
				falconx_accumulate_dtfx, /image, x1,y1,ste,veto,falconx_fx,n,n_fx, falconx_flux_mode, nsls_flux_scale, $
		 				n_elements(flux[*,0,0]),n_elements(flux[0,*,0]),n_elements(flux[0,0,*]), dead_fraction, pseudo=falconx_pseudo, $
		 				flux=flux, weight=falconx_weight, dwell=falconx_dwell, xcompress=xcompress,ycompress=ycompress, error=err	;dta
		 	endelse
		endif else begin										; spectra

;			Flux needs to be summed for h/w FC counters (flux_mode >0), and set times for Epics (flux_mode=0).
;	
;			Accumulate 'dt' into dead_fraction (across detectors). This time use the veto array.
;			Later (in spec_evt) norm this using the get_total_time method, which returns total time (ms).

;			Offset all, not just good events, so that pseudo events get offset ...
		
			if raw_xy eq 0 then begin
				x1 = uint(x1 - xoffset2)			; assumes offsets are zero in total spectrum mode
				y1 = uint(y1 - yoffset2)			; offsets used in region spectra extract mode
				z1 = uint(z1 - zoffset2)
			endif else begin
				x1 = uint(x1)		
				y1 = uint(y1)		
				z1 = uint(z1)
			endelse
;			dta = 1.
			
;			Dwell:
;				Use maia_fx[3,*] (if not zero) to accumulate total Dwell in self.dwell_total.
;
;			Dead_fraction:
;				Accumulate 't' into dead_fraction (detector array), using cal to make it in ms units.
;				Later (in spec_evt) norm this using the get_total_time() method, which returns the total dwell (ms).
;
;			Remember to NOT skip veto events, some will be pseudo events with valid BT, FC0, FC1
;
;			Need to make use of 'self.sort_options.flip.icr_raw' in the dtfx routines ...

			if falconx_hw_scaler then begin								; flux is NOT suitable for Regions
				flux = flux + nsls_flux_scale * total(falconx_fx[0,0:n-1])
			endif 	;else begin
;				dt = ((falconx_last_time - time_last) > 0) * 1000.		; dt (ms)
;				flux = flux + nsls_flux_scale * falconx_last_flux * dt * 0.001	
;			endelse
			dt = total(falconx_fx[3,0:n-1])								; dwell time (ms)

			falconx_accumulate_dtfx, /spectrum, x1,y1,ste,veto,falconx_fx,n,n_fx, falconx_flux_mode, nsls_flux_scale, $
 					n_elements(dead_fraction[*,0]),1,1, dead_fraction, weight=falconx_weight, error=err	;dta

			self.dwell_total = self.dwell_total + dt					; ms
		endelse

       processed = processed + good

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function falconx_device::import_spec, name, file, group=group

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
		warning,'falconx_device::get_import_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'falconx_evt': begin							; 9
			warning,'falconx_device::import_spec',['"falconx_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function falconx_device::get_import_list, error=error

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
		warning,'falconx_device::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_39 = define(/import)			; FalconX 384 list-mode
		opt_39.name =		'falconx_evt'	; unique name of import
		opt_39.title =		'Extract from FalconX - listmode files'
		opt_39.in_ext =		'.silist'		; input file extension
		opt_39.request =	'FalconX data scan for all spectra [Enter FIRST file or range]'
		opt_39.preview =	0			; allow spectrum preview
		opt_39.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_39.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_39.multifile =	1			; denotes data in a series of more than one file
		opt_39.separate =	'_'			; char between file and run #
		opt_39.use_pileup =	0			; request pileup file
		opt_39.use_throttle = 0			; request throttle file
		opt_39.use_IC =		1			; pop-up the flux_select PV selection panel
		opt_39.IC_mode = 	1			; default to using PV for IC
		opt_39.use_tot =	0			; collect ToT data too
		opt_39.xr =			16384		; default X range (this value forces header read)
		opt_39.yr =			16384		; default Y range
	
	opt = [opt_39]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function falconx_device::init

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
		warning,'falconx_device::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	self.header.scan.use_ylut = 1	; FalconX device can use a Y lookup table
	self.header.scan.pYlut = ptr_new(/allocate_heap)	; allocate heap for Y LUT
	
; Sort Option items appear as device setup options in the Sort EVT "Device" tab

	self.options.scan.on = 1		; scan sort options available for this class
									; these must be set-up in the render_options method
	self.options.scan.ysize = 200	; Y size of sort options box, when open

; Set default Sort Options local parameters ...

	self.sort_options.clear.x = 0					; clear X margins by default
	self.sort_options.clear.y = 0					; clear Y margins by default
	self.sort_options.clear.z = 0					; clear Z margins by default
	self.sort_options.source.x = 0					; axis to use for X
	self.sort_options.source.y = 1					; axis to use for Y
	self.sort_options.source.z = 2					; axis to use for Z
	self.sort_options.slow_axis = 1					; Y is slow axis by default (can't change this yet)
	self.sort_options.flip.icr_raw = 1				; Enable smart swap of ICR, RAW by default

; Initial heap allocation for sort_id widget vector pointers ...

	self.sort_id.slow = ptr_new(/allocate_heap)				; Slowest axis droplist ID array pointer
	self.sort_id.clear_x = ptr_new(/allocate_heap)			; Clear X border droplist ID array pointer
	self.sort_id.clear_y = ptr_new(/allocate_heap)			; Clear X border droplist ID array pointer
	self.sort_id.clear_z = ptr_new(/allocate_heap)			; Clear X border droplist ID array pointer
	self.sort_id.source_x = ptr_new(/allocate_heap)			; axis to use for X ID ptrarr
	self.sort_id.source_y = ptr_new(/allocate_heap)			; axis to use for Y ID ptrarr
	self.sort_id.source_z = ptr_new(/allocate_heap)			; axis to use for Z ID ptrarr
	self.sort_id.flip_icr_raw = ptr_new(/allocate_heap)		; Flip ICR-RAW droplist ID array pointer
	
;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"falconx_DEVICE" --> falconx_DEVICE__define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'FALCONX_DEVICE', $	; unique name for this device object
		title = 'FalconX - XIA data acquisition', $
		ext = '.silist', $		; fixed file extension for event data
		multi_files = 1, $		; multiple segment files per run
		multi_char = '_', $		; separates run from segment number in file name
		big_endian = 0, $		; FalconX data written with Linux byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 0, $		; start detector ADC #'s at 0
		use_bounds = 0, $		; not confine charge/flux within bounded area
		synchrotron = 1, $		; both synchrotron data
		ionbeam = 0, $			; and ion-beam data
		use_cluster = 1)		; uses cluster parallel processing
	return, i
end

;-------------------------------------------------------------------

pro falconx_device__define

; Define FalconX device object internal data structure.
; This adds some internal device parameters, local to FalconX in 'sort_options',
; and device specific sort widgets options to be displayed in the 'Scan' tab
; of the Sort window and elsewhere in 'sort_id'.
; Only called using obj = obj_new('falconx_DEVICE')

COMPILE_OPT STRICTARR

falconx = { falconx_DEVICE,  $

		INHERITS BASE_DEVICE, $								; mandatory base device parameters

;		Note we add 'version' to devpars so that it gets retained in 'da_evt' and 'spec_evt'
;		when a new obj instance is used.

		sort_options : {sort_options_devicespec_falconx, $	; Sort EVT window Sort options panel
				clear: {sort_options_clear_devicespec_falconx, $
						x:			0, $					; width clear X borders in sort
						y:			0, $					; width clear Y borders in sort
						z:			0}, $					; width clear Z borders in sort
				source: {options_source_devicespec_falconx, $
						x:			0, $					; X axis source (0,1,2,3,4,5 PA data)
						y:			1, $					; Y axis source (0,1,2,3,4,5 PA data)
						z:			2 }, $					; Z axis source (0,1,2,3,4,5 PA data)
				flip: {sort_options_flip_devicespec_falconx, $
						ICR_RAW:	0}, $					; Enable smart swap of ICR and RAW values on input from silist file
				slow_axis:	1, $							; slow axis for YLUT  (LATER?)
				njson:		0L, $							; bytes in JSON header to skip
				version:	0L }, $							; Listmode version number
											
		sort_id: {sort_id_falconx, $							; pointers to vector of sort widget IDs
				slow:						ptr_new(), $	; Slowest axis droplist ID array pointer
				flip_icr_raw:				ptr_new(), $	; flip ICR and RAW values check-box ID array pointer
				clear_x:					ptr_new(), $	; Clear X border droplist ID array pointer
				clear_y:					ptr_new(), $	; Clear Y border droplist ID array pointer
				clear_z:					ptr_new(), $	; Clear Z border droplist ID array pointer
				source_x:					ptr_new(), $	; Re-direction droplist(s) ID array pointer
				source_y:					ptr_new(), $	; Re-direction droplist(s) ID array pointer
				source_z:					ptr_new()} $	; Re-direction droplist(s) ID array pointer
	}
	return
end
