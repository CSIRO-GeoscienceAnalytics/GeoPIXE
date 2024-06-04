;
; GeoPIXE Device Object for Maia detector array data
; 
; Maia is a 384 (96) channel detector array and data acquisition and scanning
; system developed by CSIRO-BNL for high definition, high-throughput
; X-ray fluorescence imaging. Data is written by a Unix front-end processor in 
; big-endian byte order.
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
;					Y LUT for Maia is a list of first Y value for each blog data file.
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
; method will be used in the "BASE_DEVICE" master-class. See the code in the Maia_device
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

pro maia_device::cleanup

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
		warning,'maia_device::cleanup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_valid( self.sort_id.yenable) then ptr_free, self.sort_id.yenable
;	if ptr_valid( self.sort_id.xmargin) then ptr_free, self.sort_id.xmargin
;	if ptr_valid( self.sort_id.clear_border) then ptr_free, self.sort_id.clear_border
	if ptr_valid( self.sort_id.clear_x) then ptr_free, self.sort_id.clear_x
	if ptr_valid( self.sort_id.clear_y) then ptr_free, self.sort_id.clear_y
	if ptr_valid( self.sort_id.clear_z) then ptr_free, self.sort_id.clear_z
	if ptr_valid( self.sort_id.flip_x) then ptr_free, self.sort_id.flip_x
	if ptr_valid( self.sort_id.flip_y) then ptr_free, self.sort_id.flip_y
	if ptr_valid( self.sort_id.skew_x) then ptr_free, self.sort_id.skew_x
	if ptr_valid( self.sort_id.deadtime_cal_a) then ptr_free, self.sort_id.deadtime_cal_a
	if ptr_valid( self.sort_id.deadtime_cal_b) then ptr_free, self.sort_id.deadtime_cal_b
	if ptr_valid( self.sort_id.source_x) then ptr_free, self.sort_id.source_x
	if ptr_valid( self.sort_id.source_y) then ptr_free, self.sort_id.source_y
	if ptr_valid( self.sort_id.source_z) then ptr_free, self.sort_id.source_z
	
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

pro maia_device::render_options, parent, axes_only=axes_only

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
		warning,'maia_device::render_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(axes_only) eq 0 then axes_only=0

case !version.os_family of
	'MacOS': begin
		xmargin_xsize = 45
		encoder_xsize = 220
		deadtime_xsize = 70
		source_xsize = 45
		axes_xsize = 365
		axes_only_ysize = 75
		end
	'unix': begin
		xmargin_xsize = 45
		encoder_xsize = 220
		deadtime_xsize = 70
		source_xsize = 45
		axes_xsize = 365
		axes_only_ysize = 75
		end
	else: begin
		xmargin_xsize = 45
		encoder_xsize = 220
		deadtime_xsize = 70
		source_xsize = 45
		axes_xsize = 305
		axes_only_ysize = 80
		end
endcase

if axes_only then self.options.scan.ysize = axes_only_ysize

; Call super-class to cleanup old display and set Y size of box first ...

self->BASE_DEVICE::render_options, parent

; The following will appear in the Device box on the Sort tab of the Sort EVT window, and elsewhere where
; the device specific parameters are selected (e.g. spectra Import) ...

maiamode_base = widget_base( parent, /column,  space=3, xpad=0, ypad=0, /base_align_center, $
		event_func='maia_device_sort_option_event', uvalue=self, uname='obj-ref-here')
lab = widget_label( maiamode_base, value='Maia Option Parameters')

; A droplist (combobox) of sorting options. The uvalue contains a help string to be displayed to explain this widget.

if axes_only eq 0 then begin
	maiaybase = widget_base( maiamode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)

	maia_yenable = widget_combobox( maiaybase, value=['Off','Encoder axis 1 correction','Filter XY +-16 glitches','Encoder axes 1&2 correction'], uname='encoder-xy-correction', /tracking, $
					notify_realize='OnRealize_maia_device_sort_option_xy_correction',xsize=encoder_xsize,  $
					uvalue='a) Enable correction of encoder noise on axis 1 to keep within scan line (axis 1 only increments within the axis 0 border margin); ' + $
							'b) Enable filtering of XY +-16 glitches; c) Enable correction of encoder noise on axes 1&2 (forces axes 1&2 to only change within borders). ')

	maiadbase = widget_base( maiamode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( maiadbase, value='DT cal A:')
	maia_deadtime_cal_a = widget_text( maiadbase, value=str_tidy(self.sort_options.deadtime_cal.a), uname='maia-deadtime-cal-a', /tracking, /editable, $
					uvalue='Calibration (gain, ns) of Maia time-over-threshold (T).', scr_xsize=deadtime_xsize)
	lab = widget_label( maiadbase, value='    B:')
	maia_deadtime_cal_b = widget_text( maiadbase, value=str_tidy(self.sort_options.deadtime_cal.b), uname='maia-deadtime-cal-b', /tracking, /editable, $
					uvalue='Calibration (offset, ns) of Maia time-over-threshold (T).', scr_xsize=deadtime_xsize)
	
	maiatbase = widget_base( maiamode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( maiatbase, value='Slowest motor axis (for YLUT):')
	maia_slow = widget_combobox( maiatbase, value=str_tidy(indgen(3)), uname='maia-slow', /tracking, $
					notify_realize='OnRealize_maia_device_sort_option_slow', sensitive=1, $
					uvalue='Select the source motor axis that changes most slowly. If "cluster" mode is used, then the slowest axis provides the index into the "Y lookup table" (YLUT), used to divide the problem for parallel processing. ' + $
							'Note: Must equal "Y axis" to use "cluster" mode.',xsize=source_xsize)
endif

maias0base = widget_base( maiamode_base, /column, /base_align_center, xpad=1, ypad=1, space=3, xsize=axes_xsize, /frame)
lab = widget_label( maias0base, value='Select Motor Axis Indices for XYZ')
maiasbase = widget_base( maias0base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( maiasbase, value='X axis:')
maia_xsource = widget_combobox( maiasbase, value=str_tidy(indgen(3)), uname='maia-x-source', /tracking, $
					notify_realize='OnRealize_maia_device_sort_option_x_source', sensitive=1, $
					uvalue='Select the source motor axis to use as the GeoPIXE "X" axis.',xsize=source_xsize)
lab = widget_label( maiasbase, value='  Y axis:')
maia_ysource = widget_combobox( maiasbase, value=str_tidy(indgen(3)), uname='maia-y-source', /tracking, $
					notify_realize='OnRealize_maia_device_sort_option_y_source', sensitive=1, $
					uvalue='Select the source motor axis to use as the GeoPIXE "Y" axis. Note: To use "cluster" mode, "Y" must be the "slow" axis. This is needed for the YLUT scheme to work at present.',xsize=source_xsize)
lab = widget_label( maiasbase, value='  Z axis:')
maia_zsource = widget_combobox( maiasbase, value=str_tidy(indgen(3)), uname='maia-z-source', /tracking, $
					notify_realize='OnRealize_maia_device_sort_option_z_source', sensitive=1, $
					uvalue='Select the source motor axis to use as the GeoPIXE "Z" axis, which may represent Energy or Tomo Theta.',xsize=source_xsize)

; Droplists to both enable and set the size of XYZ border margins. This replaces both the X margin droplist
; above and the check-boxes below.

if axes_only eq 0 then begin
	maiacbase = widget_base( maiamode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( maiacbase, value='Clear Axis Border X:')
	maia_clearX = widget_combobox( maiacbase, value=str_tidy(indgen(20)), uname='maia-clear-x', /tracking, $
						notify_realize='OnRealize_maia_device_sort_option_clear_x', sensitive=1, $
						uvalue='Clear both borders along X axis.',xsize=source_xsize)
	lab = widget_label( maiacbase, value='  Y:')
	maia_clearY = widget_combobox( maiacbase, value=str_tidy(indgen(20)), uname='maia-clear-y', /tracking, $
						notify_realize='OnRealize_maia_device_sort_option_clear_y', sensitive=1, $
						uvalue='Clear both borders along Y axis.',xsize=source_xsize)
	lab = widget_label( maiacbase, value='  Z:')
	maia_clearZ = widget_combobox( maiacbase, value=str_tidy(indgen(20)), uname='maia-clear-z', /tracking, $
						notify_realize='OnRealize_maia_device_sort_option_clear_z', sensitive=1, $
						uvalue='Clear both borders along Z axis.',xsize=source_xsize)

; Skew control for X axis

	maiaskbase = widget_base( maiamode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( maiaskbase, value='Skew (correct) X Axis:')
	maia_skewX = widget_combobox( maiaskbase, value=str_tidy(indgen(50)), uname='maia-skew-x', /tracking, $
		notify_realize='OnRealize_maia_device_sort_option_skew_x', sensitive=1, $
		uvalue='Correct skew in X by one for this many Y steps.',xsize=source_xsize*1.5)
endif

; Check-boxes to flip (mirror) X,Y axes

maiafbase = widget_base( maiamode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( maiafbase, value='Flip (mirror) Axis:')
flipx_check = cw_bgroup2( maiafbase, ['X'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='maia-flip-x', set_value=self.sort_options.flip.x, /nonexclusive, $
					uvalue=['Check box to flip (mirror) the X axis during processing.'])
flipy_check = cw_bgroup2( maiafbase, ['Y'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='maia-flip-y', set_value=self.sort_options.flip.y, /nonexclusive, $
					uvalue=['Check box to flip (mirror) the Y axis during processing.'])

error = 0
if axes_only eq 0 then begin
	add_widget_vector, self.sort_id.yenable, maia_yenable, error=err & error=error or err
	add_widget_vector, self.sort_id.slow, maia_slow, error=err & error=error or err
	add_widget_vector, self.sort_id.clear_X, maia_clearX, error=err & error=error or err
	add_widget_vector, self.sort_id.clear_Y, maia_clearY, error=err & error=error or err
	add_widget_vector, self.sort_id.clear_Z, maia_clearZ, error=err & error=error or err
	add_widget_vector, self.sort_id.skew_X, maia_skewX, error=err & error=error or err
	add_widget_vector, self.sort_id.deadtime_cal_a, maia_deadtime_cal_a, error=err & error=error or err
	add_widget_vector, self.sort_id.deadtime_cal_b, maia_deadtime_cal_b, error=err & error=error or err
endif
add_widget_vector, self.sort_id.flip_X, flipx_check, error=err & error=error or err
add_widget_vector, self.sort_id.flip_Y, flipy_check, error=err & error=error or err
add_widget_vector, self.sort_id.source_x, maia_xsource, error=err & error=error or err
add_widget_vector, self.sort_id.source_y, maia_ysource, error=err & error=error or err
add_widget_vector, self.sort_id.source_z, maia_zsource, error=err & error=error or err
if error then begin
	warning,'maia_device::render_options','Error adding device object widget ID vectors.'
endif
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_clear_x, wWidget

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
		warning,'OnRealize_maia_device_sort_option_clear_x',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_clear_x: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_clear_x: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.clear.x
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_clear_y, wWidget

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
		warning,'OnRealize_maia_device_sort_option_clear_y',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_clear_y: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_clear_y: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.clear.y
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_skew_x, wWidget

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
			warning,'OnRealize_maia_device_sort_option_clear_x',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_skew_x: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_skew_x: Device Object ref not found.'
		return
	endif

	options = obj->get_options()

	widget_control, wWidget, set_combobox_select=options.skew.x
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_clear_z, wWidget

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
		warning,'OnRealize_maia_device_sort_option_clear_z',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_clear_z: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_clear_z: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.clear.z
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_slow, wWidget

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
		warning,'OnRealize_maia_device_sort_option_slow',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_slow: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_slow: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.slow_axis
	return
end

;------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_xy_correction, wWidget

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
		warning,'OnRealize_maia_device_sort_option_xy_correction',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_xy_correction: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_xy_correction: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.encoder_y_correct
	return
end

;------------------------------------------------------------------------------------------
;
;pro OnRealize_maia_device_sort_option_x_margin, wWidget
;
;COMPILE_OPT STRICTARR
;common c_errors_1, catch_errors_on
;if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
;if catch_errors_on then begin
;	Catch, ErrorNo
;	if (ErrorNo ne 0) then begin
;		Catch, /cancel
;		on_error, 1
;		help, calls = s
;		n = n_elements(s)
;		c = 'Call stack: '
;		if n gt 2 then c = [c, s[1:n-2]]
;		warning,'OnRealize_maia_device_sort_option_x_margin',['IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!error_state.msg,'',c], /error
;		MESSAGE, /RESET
;		return
;	endif
;endif
;
;	ObjBase = find_id( wWidget, uname='obj-ref-here')
;	if widget_info(ObjBase, /valid) eq 0L then begin
;		print,'OnRealize_maia_device_sort_option_x_margin: Object base not found.'
;		return
;	endif
;	widget_control, ObjBase, get_uvalue=obj
;	if obj_valid(obj) eq 0L then begin
;		print,'OnRealize_maia_device_sort_option_x_margin: Device Object ref not found.'
;		return
;	endif
;
;	options = obj->get_options()
;	
;	widget_control, wWidget, set_combobox_select=(options.x_margin - 1) > 0
;	return
;end
;
;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_x_source, wWidget

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
		warning,'OnRealize_maia_device_sort_option_x_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_x_source: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_x_source: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.source.x
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_y_source, wWidget

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
		warning,'OnRealize_maia_device_sort_option_y_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_y_source: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_y_source: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.source.y
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_device_sort_option_z_source, wWidget

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
		warning,'OnRealize_maia_device_sort_option_z_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_z_source: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_maia_device_sort_option_z_source: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.source.z
	return
end

;------------------------------------------------------------------------------------------

function maia_device_sort_option_event, event

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
		warning,'maia_device_sort_option_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	if widget_info( event.handler, /valid) eq 0L then begin
;		print,'maia_device_sort_option_event: event.handler not valid.'
		return, 0
	endif
	uname = widget_info( event.handler, /uname)
	if uname ne 'obj-ref-here' then begin
		print,'maia_device_sort_option_event: Object base not found.'
		return, 0
	endif
	widget_control, event.handler, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'maia_device_sort_option_event: Device Object ref not found.'
		return, 0
	endif

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		return, event						; pass context help up the line ...
		end
	else: begin
		
		uname = widget_info( event.id, /uname)
		case uname of
			'encoder-xy-correction': begin
				obj->set_options, encoder_y_correct = event.index
				end
;			'maia-x-margin': begin
;				obj->set_options, x_margin = event.index + 1
;				end
;			'maia-options': begin
;				case event.value of
;					0: begin
;						obj->set_options, clear_x = event.select
;						end
;					1: begin
;						obj->set_options, clear_y = event.select
;						end
;				endcase
;				end
			'maia-slow': begin
				obj->set_options, slowest = event.index
				end
			'maia-clear-x': begin
				obj->set_options, clear_x = event.index
				end
			'maia-clear-y': begin
				obj->set_options, clear_y = event.index
				end
			'maia-clear-z': begin
				obj->set_options, clear_z = event.index
				end
			'maia-flip-x': begin
				obj->set_options, flip_x = event.select
				end
			'maia-flip-y': begin
				obj->set_options, flip_y = event.select
				end
			'maia-skew-x': begin
				obj->set_options, skew_x = event.index
				end
			'maia-deadtime-cal-a': begin
				widget_control, event.id, get_value=s
				obj->set_options, dt_cala = float2(s)
				end
			'maia-deadtime-cal-b': begin
				widget_control, event.id, get_value=s
				obj->set_options, dt_calb = float2(s)
				end
			'maia-x-source': begin
				obj->set_options, source_x = event.index
				obj->update_header_info, error=error
				obj->update_notify
				end
			'maia-y-source': begin
				obj->set_options, source_y = event.index
				obj->update_header_info, error=error
				obj->update_notify
				end
			'maia-z-source': begin
				obj->set_options, source_z = event.index
				obj->update_header_info, error=error
				obj->update_notify
				end
		endcase		
		end
endcase
return, 0L
end

;------------------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window, the spectrum Import selector and Maia Setup. 
; These are rendered in this class, using the method "render_options" and the parameters
; read/written from/to DISK using the "read_options", "write_options" methods. 
; Keep a local copy of device parameters and set them using "set_options". 
; The keywords here are only used internally in Maia device.

; Note: apart from the parameter 'p', all others are local/specific to the Maia device.
; Hence, these should NOT be used in 'general' device code, as they will throw errors
; in 'base_device'.

pro maia_device::set_options, p, encoder_y_correct=encoder_y_correct, $
				clear_x=clear_x, clear_y=clear_y, clear_z=clear_z, $			; x_margin=x_margin
				deadtime_cal=deadtime_cal, dt_cala=dt_cala, dt_calb=dt_calb, $
				source_x=source_x, source_y=source_y, source_z=source_z, $
				slowest=slowest, flip_x=flip_x, flip_y=flip_y, skew_x=skew_x

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
		warning,'maia_device::set_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(p) eq 1 then begin
		if size(p, /tname) eq 'STRUCT' then begin
			maia = p
		endif else if ptr_good(p,/struct) then begin
			maia = *p
		endif else return
	endif

	if n_elements(maia) eq 1 then begin
		if tag_present('ENCODER_Y_CORRECT',maia) then self.sort_options.encoder_y_correct = maia.encoder_y_correct
;		if tag_present('X_MARGIN',maia) then self.sort_options.x_margin = maia.x_margin
		if tag_present('SLOW_AXIS',maia) then self.sort_options.slow_axis = maia.slow_axis
		if tag_present('CLEAR',maia) then begin
			if tag_present('X',maia.clear) then self.sort_options.clear.x = maia.clear.x
			if tag_present('Y',maia.clear) then self.sort_options.clear.y = maia.clear.y
			if tag_present('Z',maia.clear) then self.sort_options.clear.z = maia.clear.z
		endif
		if tag_present('FLIP',maia) then begin
			if tag_present('X',maia.flip) then self.sort_options.flip.x = maia.flip.x
			if tag_present('Y',maia.flip) then self.sort_options.flip.y = maia.flip.y
		endif
		if tag_present('SKEW',maia) then begin
			if tag_present('X',maia.skew) then self.sort_options.skew.x = maia.skew.x
		endif
		if tag_present('DEADTIME_CAL',maia) then self.sort_options.deadtime_cal = maia.deadtime_cal
		if tag_present('SOURCE',maia) then begin
			if tag_present('X',maia.source) then self.sort_options.source.x = maia.source.x
			if tag_present('Y',maia.source) then self.sort_options.source.y = maia.source.y
			if tag_present('Z',maia.source) then self.sort_options.source.z = maia.source.z
		endif
	endif else begin
		if n_elements(encoder_y_correct) eq 1 then self.sort_options.encoder_y_correct = encoder_y_correct
;		if n_elements(x_margin) eq 1 then self.sort_options.x_margin = x_margin
		if n_elements(slowest) eq 1 then self.sort_options.slow_axis = slowest
		if n_elements(clear_x) eq 1 then self.sort_options.clear.x = clear_x
		if n_elements(clear_y) eq 1 then self.sort_options.clear.y = clear_y
		if n_elements(clear_z) eq 1 then self.sort_options.clear.z = clear_z
		if n_elements(flip_x) eq 1 then self.sort_options.flip.x = flip_x
		if n_elements(flip_y) eq 1 then self.sort_options.flip.y = flip_y
		if n_elements(skew_x) eq 1 then self.sort_options.skew.x = skew_x
		if n_elements(deadtime_cal) eq 1 then begin
			if deadtime_cal.a gt 0.0 then begin
				self.sort_options.deadtime_cal = deadtime_cal
			endif
		endif
		if n_elements(dt_cala) eq 1 then begin
			if dt_cala gt 0.0 then begin
				self.sort_options.deadtime_cal.a = dt_cala
			endif
		endif
		if n_elements(dt_calb) eq 1 then self.sort_options.deadtime_cal.b = dt_calb
		if n_elements(source_x) eq 1 then self.sort_options.source.x = source_x
		if n_elements(source_y) eq 1 then self.sort_options.source.y = source_y
		if n_elements(source_z) eq 1 then self.sort_options.source.z = source_z
	endelse
	
	; Set value of widgets. There may be multiple sort option panels attached to this object, so
	; we use 'widget_control_vector' to set all of them.
	
	widget_control_vector, self.sort_id.yenable, set_combobox_select = self.sort_options.encoder_y_correct
;	widget_control_vector, self.sort_id.xmargin, set_combobox_select = self.sort_options.x_margin -1
;	widget_control_vector, self.sort_id.clear_border, set_value = [self.sort_options.clear.x,self.sort_options.clear.y]
	widget_control_vector, self.sort_id.slow, set_combobox_select = self.sort_options.slow_axis
	widget_control_vector, self.sort_id.clear_x, set_combobox_select = self.sort_options.clear.x
	widget_control_vector, self.sort_id.clear_y, set_combobox_select = self.sort_options.clear.y
	widget_control_vector, self.sort_id.clear_z, set_combobox_select = self.sort_options.clear.z
	widget_control_vector, self.sort_id.flip_x, set_value = self.sort_options.flip.x
	widget_control_vector, self.sort_id.flip_y, set_value = self.sort_options.flip.y
	widget_control_vector, self.sort_id.skew_x, set_combobox_select = str_tidy(self.sort_options.skew.x)
	widget_control_vector, self.sort_id.deadtime_cal_a, set_value = str_tidy(self.sort_options.deadtime_cal.a)
	widget_control_vector, self.sort_id.deadtime_cal_b, set_value = str_tidy(self.sort_options.deadtime_cal.b)
	widget_control_vector, self.sort_id.source_x, set_combobox_select = str_tidy(self.sort_options.source.x)
	widget_control_vector, self.sort_id.source_y, set_combobox_select = str_tidy(self.sort_options.source.y)
	widget_control_vector, self.sort_id.source_z, set_combobox_select = str_tidy(self.sort_options.source.z)
	return
end

;-------------------------------------------------------------------

;	Returns internal self.sort_options struct.
;	Will get values of options widgets (e.g. text strings) and
;	update internal values in self struct.

function maia_device::get_options, error=error

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
		warning,'maia_device::get_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 0
	
	widget_control_vector, self.sort_id.deadtime_cal_a, /first, get_value=s, error=err
	if err eq 0 then self.sort_options.deadtime_cal.a = float2(s)
	
	widget_control_vector, self.sort_id.deadtime_cal_b, /first, get_value=s, error=err
	if err eq 0 then self.sort_options.deadtime_cal.b = float2(s)
	
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

pro maia_device::read_options, unit, options=options, error=error

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
		warning,'maia_device::read_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

; Get current defaults in this device

	options = self->get_options()
	encoder_y_correct = options.encoder_y_correct
	x_margin = 0
	clear_x = options.clear.x
	clear_y = options.clear.y
	clear_z = options.clear.z
	flip_x = options.flip.x
	flip_y = options.flip.y
	skew_x = options.skew.x
	deadtime_cal = options.deadtime_cal
	source = options.source
	slow_axis = options.slow_axis
	version = 0L
	
; Read options parameters from the file

	on_ioerror, bad_io
	if self.use_version then readu, unit, version
	
	readu, unit, encoder_y_correct, x_margin
	readu, unit, clear_x, clear_y
	
	clear_x = (version le -4) ? clear_x : x_margin
	clear_x = clip(clear_x,0,19)
	clear_y = clip(clear_y,0,19)
	encoder_y_correct = clip(encoder_y_correct,0,3)
	
	if self.use_version then begin
		if version le -2 then begin
			readu, unit, deadtime_cal
		endif
		if version le -3 then begin
			readu, unit, source
		endif
		if version le -4 then begin
			readu, unit, clear_z
		endif
		if version le -5 then begin
			readu, unit, slow_axis
		endif
		if version le -6 then begin
			readu, unit, flip_x, flip_y
		endif
		if version le -7 then begin
			readu, unit, skew_x
		endif
	endif
	
	clear_z = clip(clear_z,0,19)
	slow_axis = clip(slow_axis,0,2)
	
; Write back these options parameters to the device ...

	options.encoder_y_correct = encoder_y_correct
;	options.x_margin = x_margin
	options.clear.x = clear_x
	options.clear.y = clear_y
	options.clear.z = clear_z
	options.flip.x = flip_x
	options.flip.y = flip_y
	options.skew.x = skew_x
	options.slow_axis = slow_axis
	if deadtime_cal.a gt 2. then begin									; reject 0. or 1. dummies
		options.deadtime_cal = deadtime_cal
	endif
	if (source.x ne 0) or (source.y ne 0) or (source.z ne 0) then begin	; reject all zeroes
		options.source = source
	endif
	
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

pro maia_device::write_options, unit, options=p, error=error

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
		warning,'maia_device::write_options',['IDL run-time error caught.', '', $
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
	
	version = -7L
	x_margin = 0
	
	on_ioerror, bad_io
	writeu, unit, version
	
	writeu, unit, options.encoder_y_correct, x_margin
	writeu, unit, options.clear.x, options.clear.y
	writeu, unit, options.deadtime_cal
	writeu, unit, options.source
	writeu, unit, options.clear.z
	writeu, unit, options.slow_axis
	writeu, unit, options.flip.x, options.flip.y
	writeu, unit, options.skew.x
	error = 0
	return
	
bad_io:
	error = 1
	return
end

;-------------------------------------------------------------------

; Is the flip (mirror) X axis on?
; Use in tests: if obj->flipX() 

function maia_device::flipX

COMPILE_OPT STRICTARR
		
	return, self.sort_options.flip.x
end

;-------------------------------------------------------------------

; Is the flip (mirror) Y axis on?
; Use in tests: if obj->flipY() 

function maia_device::flipY

COMPILE_OPT STRICTARR
		
	return, self.sort_options.flip.y
end

;-------------------------------------------------------------------

; Is the Skew (correctr) X axis on?
; Use in tests: if obj->skewX() ne 0

function maia_device::skewX

	COMPILE_OPT STRICTARR

	return, self.sort_options.skew.x
end

;-------------------------------------------------------------------------------

; Return a string to display (e.g. in Image History window) to show the
; state of this device's options. If 'p' present, then show this parameter set.

function maia_device::options_legend, p

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
		warning,'maia_device::options_legend',['IDL run-time error caught.', '', $
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

	list = ['Maia:' ] 
	list = [list, '   DT Cal A: ' + str_tidy(options.deadtime_cal.a) + ', B: ' + str_tidy(options.deadtime_cal.b) ]
	list = [list, '   Slowest axis used for YLUT: ' + str_tidy(options.slow_axis) ]
	list = [list, '   Axes: X: ' + str_tidy(options.source.x) + ', Y: ' + str_tidy(options.source.y) + ', Z: ' + str_tidy(options.source.z) ]
	if options.encoder_y_correct eq 1 then begin
		list = [list, '   Encoder axis 1 correction active' ]
	endif
	if options.encoder_y_correct eq 2 then begin
		list = [list, '   Encoder +/-16 glitch correction active' ]
	endif
	if options.encoder_y_correct eq 3 then begin
		list = [list, '   Encoder axes 1,2 correction active' ]
	endif
	list = [list, '   Clear margins X: ' + str_tidy(options.clear.x) + ', Y: ' + str_tidy(options.clear.y) + ', Z: ' + str_tidy(options.clear.z) ]
	list = [list, '   Flip (mirror) axis X: ' + str_tidy(options.flip.x) + ', Y: ' + str_tidy(options.flip.y)  ]
	list = [list, '   Skew (correct) axis X: ' + str_tidy(options.skew.x)  ]

	return, list
end

;-------------------------------------------------------------------
;-------------------------------------------------------------------

; This method should be phased, as the deadtime cal is internally handled in
; device objects. It is still used in 'analyze_image' in the Image region analysis.

function maia_device::get_deadtime_cal, error=error

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
		warning,'maia_device::get_deadtime_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 0
	t = self->get_options( error=error)
	
	return, t.deadtime_cal
end

;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

function maia_device::get_header_info, filei, output=output, silent=silent, error=error

; file		a raw data file to look for associated header, metadata
; output	if present, this is a file on the output path, if some metadata is
;			located in that path (e.g. Maia Y LUT). 
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
		warning,'maia_device::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	if n_elements(silent) lt 1 then silent=0
	error = 1
	self.header.error = 1
	file = filei

	t = strip_file_ext(file)						; look for segment ".0" file
	f = t + '.0'
	gprint,level=2,'maia_device::get_header_info: f = ',f,' OK=',file_test(f)
	if file_test(f) then file=f

;	mp = get_maia_32_header( file, silent=silent, error=error)
	mp = get_maia_32_header( file, error=error)
	if error then return,0

;	Y lookup table, stored with output, or on blog raw data path ...
;	The Y LUT contains a list of the first Y value in each raw data file in the multi-file set.
;	This does not need to be redone in 'update_header_info' later, so we do it here.

	if n_elements(output) gt 0 then begin
		retry = 0
		ylut = get_maia_ylut(file, output=output, error=err)	; stored now with output
		if err or (n_elements(ylut) lt 1) then retry=1	
	endif else retry=1
	if retry then begin
		ylut = get_maia_ylut( file, error=err)					; if not, try blog path
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

; A sub-class method will freeze changes to options.

pro maia_device::change_options, flag, error=error

COMPILE_OPT STRICTARR
		
	self.no_change_options = ((flag) ? 0 : 1)
	error = 0
	return
end

;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

pro maia_device::update_header_info, error=error

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
		warning,'maia_device::update_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	error = 1
	pmp = self.old_mp
	if ptr_good(pmp) eq 0 then return
	
	if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
	self.header.scan.dwell = (*pmp).dwell

	case self.sort_options.source.x of
		0: begin
			self.header.scan.x_pixels = (*pmp).scan.xrange
			self.header.scan.x_mm = (*pmp).scan.xsize * 0.001
			self.header.scan.x = (*pmp).scan.xorigin
			self.header.scan.x_on = (*pmp).scan.xon
			self.header.scan.x_name = (*pmp).scan.xname
			end
		1: begin
			self.header.scan.x_pixels = (*pmp).scan.yrange
			self.header.scan.x_mm = (*pmp).scan.ysize * 0.001
			self.header.scan.x = (*pmp).scan.yorigin
			self.header.scan.x_on = (*pmp).scan.yon
			self.header.scan.x_name = (*pmp).scan.yname
			end
		2: begin
			self.header.scan.x_pixels = (*pmp).scan.zrange
			self.header.scan.x_mm = (*pmp).scan.zsize * 0.001
			self.header.scan.x = (*pmp).scan.zorigin
			self.header.scan.x_on = (*pmp).scan.zon
			self.header.scan.x_name = (*pmp).scan.zname
			end
		else:
	endcase
	case self.sort_options.source.y of
		0: begin
			self.header.scan.y_pixels = (*pmp).scan.xrange
			self.header.scan.y_mm = (*pmp).scan.xsize * 0.001
			self.header.scan.y = (*pmp).scan.xorigin
			self.header.scan.y_on = (*pmp).scan.xon
			self.header.scan.y_name = (*pmp).scan.xname
			end
		1: begin
			self.header.scan.y_pixels = (*pmp).scan.yrange
			self.header.scan.y_mm = (*pmp).scan.ysize * 0.001
			self.header.scan.y = (*pmp).scan.yorigin
			self.header.scan.y_on = (*pmp).scan.yon
			self.header.scan.y_name = (*pmp).scan.yname
			end
		2: begin
			self.header.scan.y_pixels = (*pmp).scan.zrange
			self.header.scan.y_mm = (*pmp).scan.zsize * 0.001
			self.header.scan.y = (*pmp).scan.zorigin
			self.header.scan.y_on = (*pmp).scan.zon
			self.header.scan.y_name = (*pmp).scan.zname
			end
		else:
	endcase
	case self.sort_options.source.z of
		0: begin
			self.header.scan.z_pixels = (*pmp).scan.xrange
			self.header.scan.z_mm = (*pmp).scan.xsize * 0.001
			self.header.scan.z = (*pmp).scan.xorigin
			self.header.scan.z_on = (*pmp).scan.xon
			self.header.scan.z_name = (*pmp).scan.xname
			end
		1: begin
			self.header.scan.z_pixels = (*pmp).scan.yrange
			self.header.scan.z_mm = (*pmp).scan.ysize * 0.001
			self.header.scan.z = (*pmp).scan.yorigin
			self.header.scan.z_on = (*pmp).scan.yon
			self.header.scan.z_name = (*pmp).scan.yname
			end
		2: begin
			self.header.scan.z_pixels = (*pmp).scan.zrange
			self.header.scan.z_mm = (*pmp).scan.zsize * 0.001
			self.header.scan.z = (*pmp).scan.zorigin
			self.header.scan.z_on = (*pmp).scan.zon
			self.header.scan.z_name = (*pmp).scan.zname
			end
		else:
	endcase

	self.header.energy = (*pmp).energy
	self.header.deadtime_cal = (*pmp).deadtime_cal
	self.header.title = (*pmp).comment
	self.header.sample = (*pmp).sample
	self.header.grain = (*pmp).grain
	
	self.header.metadata.sample_type = (*pmp).sample_type
	self.header.metadata.sample_serial = (*pmp).sample_serial
	self.header.metadata.detector_identity = (*pmp).detector_identity
	self.header.metadata.facility = (*pmp).facility
	self.header.metadata.endstation = (*pmp).endstation

	self.header.pileup.found = (*pmp).pileup.found
	self.header.pileup.on = (*pmp).pileup.on
	self.header.pileup.file = (*pmp).pileup.file
	self.header.throttle.found = (*pmp).throttle.found
	self.header.throttle.on = (*pmp).throttle.on
	self.header.throttle.file = (*pmp).throttle.file

	if (self.header.pileup.on eq 0) or (self.header.pileup.found eq 0) then self.header.pileup.file = ''
	if (self.header.throttle.on eq 0) or (self.header.throttle.found eq 0) then self.header.throttle.file = ''

	self.header.sensitivity = (*pmp).IC_sensitivity
	self.header.IC_name = (*pmp).IC_name
	self.header.cal = (*pmp).cal
	self.header.detector[*] = -1

;	A header read and update called from 'setup' method (or table EVT) should not change any sort options, which are set
;	by the user from the 'Device' panels in Sort EVT, etc.

	if self.no_change_options eq 0 then begin
;		if (abs((*pmp).energy-24.1) lt 0.002) then begin
			if ((strmid((*pmp).facility,0,6) eq 'MM.Mel') and ((*pmp).endstation eq '1')) or $
						((strmid((*pmp).facility,0,6) eq 'MM.Per') and ((*pmp).endstation eq '2')) then begin
				self.sort_options.flip.x = 1
				print,'Maia device: force flip.x ON'
			endif
			if ((strmid((*pmp).facility,0,6) eq 'MM.Mel') and ((*pmp).endstation eq '2')) or $
						((strmid((*pmp).facility,0,6) eq 'MM.Per') and ((*pmp).endstation eq '1')) then begin
				self.sort_options.flip.x = 0
				print,'Maia device: force flip.x OFF'
			endif
;		endif
	endif
	self.header.error = 0
	error = 0
	return
end

;-------------------------------------------------------------------

; This method is called after the 'get_header_info()' method
; has been called (e.g. when raw data files are first referenced) to update any
; internal device parameters if they also occur in the header.

pro maia_device::update_device_from_header, error=error

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
		warning,'maia_device::update_device_from_header',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

;	Deadtime cal is one Device parameter set in both header and device sort_options ...

	self->set_options, deadtime_cal=self.header.deadtime_cal

	error = 0
	return
end

;-------------------------------------------------------------------

; This method is called to see if there are extra data/detector planes that should be
; accumulated. It returns their name strings. If these are returned, then
; the 'read_buffer' method should accumulate these as extra planes of the 'flux'
; array if the flux array is passed with 3 dimensions.

function maia_device::get_attribute_list, error=error

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
		warning,'maia_device::get_attribute_list',['IDL run-time error caught.', '', $
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
 
pro maia_device::flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
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
		warning,'maia_device::flux_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_maia_2, maia_swap,length,skip,x0,y0,z0
common c_maia_3, monitor_array, n_monitor, maia_IC_name
common c_maia_4, n_times, time_last
;common c_maia_5, xanes_energies, do_xanes
common c_maia_7, maia_energy_pv
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_maia_17, maia_x_range, maia_y_range, maia_z_range, maia_3D
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
if n_elements(maia_IC_name) lt 1 then maia_IC_name=''

	if n_elements(first) lt 1 then first = 1
	if n_elements(suppress) lt 1 then suppress = 0
	if n_elements(image_mode) lt 1 then image_mode = 1
	if n_elements(nsls_debug) lt 1 then nsls_debug = 0
	
	PV_list = 'none'
	IC_name = ''
	IC_val = 0.
	IC_vunit = 0.
	dwell = 0.
	no_pv = 1
	use_dwell = 0
	error = 1
	
	on_ioerror, bad_io
	if first then begin
		err = init_maia_32()
		if err then goto, bad_io

		head = read_maia_32_header( unit, error=err32)

		if image_mode then begin
			dwell = head.dwell
			use_dwell = 1
		endif
		
		if err32 eq 0 then begin
			maia = maia_defaults(source='maia_device::flux_scan', /any)		; read "Maia.conf" default PVs

;			Ion chamber PV selection options, or ring current, etc.

			nq = 0
			name = head.monitor.name
			val = head.monitor.val
			check_plist_maia, name				; what about extend 'val' too, or NOT check_plist yet?
			if (n_elements(name) ge 1) and (maia.epics.npic ge 1) then begin
				q = where_tokens( *maia.epics.pic, name, nq)
				PV_list = name[q]
				no_pv = 0
			endif
		endif

;		Use /suppress to just read header values and set some defaults

		ionbeam = self->ionbeam()
		charge_gain_unit_lists, vals, units, vunits, ionbeam=ionbeam

		if suppress then begin

			deadtime_cal = head.deadtime_cal	; deadtime cal read from blog DA_info (44) record
			print,'maia_device::flux_scan: deadtime_cal = ',deadtime_cal
			
			if (abs(deadtime_cal.a - 0.1) gt 0.1) and (abs(deadtime_cal.a - 0.0) gt 0.1) then begin
				self->set_options, deadtime_cal=deadtime_cal
			endif

			if head.IC_sensitivity ne 0.0 then begin
				IC_val = charge_gain_units( head.IC_sensitivity, units=IC_vunit)
				print,'Maia:flux_scan: found sensitivity = ',head.IC_sensitivity
			endif
			if head.IC_name ne '' then begin
				IC_name = head.IC_name
			endif
			
		endif else begin
			maia_IC_name = ''					; in c_maia_3
			nsls_flux_scale = 1.				; in c_nsls_?

;				This section is ~nearly common between device_specific (10, 14, 16)
;				and aps_to_list()

			if err32 eq 0 then begin
;				a1_val = 0.0
;				a1_unit = 1.0

;				Ion chamber PV selection options, or ring current, etc.

;				nq = 0
;				name = head.monitor.name
;				check_plist_maia, name
;				if (n_elements(name) ge 1) and (maia.epics.npic ge 1) then begin
;					q = where_tokens( *maia.epics.pic, name, nq)
;				endIF
	
;				Ion chamber preamp sensitivity value
;				Perhaps these should be in maia defaults read too?

				q2 = where_tokens( ['sens_num','sensitivity'], name, nq2)
	
;				Ion chamber preamp sensitivity units

				q3 = where_tokens( ['sens_unit','sensitivity'], name, nq3)
	
;				Live-time, dead-time handled elsewhere ...

				nq4 = 0
				if nq4 ne 0 then begin					; deadtime disabled in pop-up below ...
					times = name[q4]
				endif else begin
					times = ['none found']
				endelse
			
;				Need to disable dwell in pop-up in spec_evt mode.
;				Leave it UNDEFINED if not in map mode.

				if (nq ne 0) then begin
					if (nq2 ne 0) and (nq3 ne 0) then begin
						select = generic_flux_select( group, name[q], name[q2], name[q3], times, $
									error=dud, dwell=dwell, dead=0)
					endif else begin
						select = generic_flux_select( group, name[q], string(vals), units, times, $
									error=dud, dwell=dwell, dead=0)
					endelse
				endif else dud=1
				
				if dud then begin
					maia_IC_name = ''			; in c_maia_3
					nsls_flux_scale = 1.		; in c_nsls_?
				endif else begin
					maia_IC_name = name[q[select.flux]]
					IC_name = maia_IC_name
					if (nq2 ne 0) and (nq3 ne 0) then begin
						nsls_flux_scale = charge_sensitivity( float(val[q2[select.sense_num]]), val[q3[select.sense_unit]])
;						a1_val = float(val[q2[select.sense_num]])
;						u = val[q3[select.sense_unit]]
;						q2b = where( strlowcase(u) eq strlowcase(units))
;						a1_unit = 1.
;						if q2b[0] ne -1 then a1_unit = vunit[q2b]
					endif else begin
						nsls_flux_scale = charge_sensitivity( float(vals[select.sense_num]), units[select.sense_unit]) 
;						a1_val = vals[ select.sense_num ]
;						a1_unit = vunit[ select.sense_unit ]
					endelse
;					IC_val = a1_val
;					IC_vunit = a1_unit
;					nsls_flux_scale = a1_val * a1_unit							; scaling of counts for nA/V units
					IC_val = charge_gain_units( nsls_flux_scale, units=IC_vunit)
	
					if image_mode then begin
						nsls_flux_scale = nsls_flux_scale * select.dwell*0.001			; scale by dwell only for maps
					endif
				endelse
			endif
			print,' maia_device::flux_scan: maia_IC_name = ',maia_IC_name,', nsls_flux_scale = ', nsls_flux_scale
		endelse
	endif
	error = 0
	return

bad_io:
	warning,'maia_device::flux_scan',['blog file I/O error','file = '+evt_file]
	error=1
	return
end

;-------------------------------------------------------------------

function maia_device::incremental_ylut

; Indicates that a Y encoder mode is set that necessitates the building of a
; Y lookup table file that lists the first Y value for each list-mode data file
; incrementally during a DA_EVT scan, rather than using the build_ylut method.

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
		warning,'maia_device::incremental_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif

;	If the Y encoder correction upsets cluster processing, and
;	if the correction axis matches the 'slow' axis, then return
;	true here to suppress YLUT usage.
;
;	New Fortran code works with YLUTs now, so return FALSE.

	bad = 0
;	case self.sort_options.encoder_y_correct of
;		1: begin
;			if self.sort_options.slow_axis eq 1 then bad=1
;			end
;		3: begin
;			if self.sort_options.slow_axis eq 1 then bad=1
;			if self.sort_options.slow_axis eq 2 then bad=1
;			end
;		else:
;	endcase

	return, bad
end

;-------------------------------------------------------------------

pro maia_device::check_pv_list, plist

; Check the PV list pointed to by 'plist'. Add any Maia default PVs
; not present to this list. The ones corresponding to the 'get_attributes'
; must appear first in the list.

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
		warning,'maia_device::check_pv_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	check_plist_maia, plist
	return
end

;-------------------------------------------------------------------

; Indicates that this device support a Y lookup table that matches the Y value
; at the start of each raw EVT list-mode data-file. 

function maia_device::ylut

COMPILE_OPT STRICTARR
		
	ok = self.header.scan.use_ylut
	if ok eq 0 then return, ok
	ok2 = 1
	ok3 = 1
	
;	If the Y encoder correction upsets cluster processing, then use this ...
;	If absolute Y values available from Fortran even in stripe mode, then don't ...

	case self.sort_options.encoder_y_correct of
		1: begin
			if self.sort_options.slow_axis eq 1 then ok2=0
			end
		3: begin
			if self.sort_options.slow_axis eq 1 then ok2=0
			if self.sort_options.slow_axis eq 2 then ok2=0
			end
		else:
	endcase
	
;	If the cluster stripes and Y windowing both mixed up in 'DA_evt', then use this ...

	if self.sort_options.source.y ne self.sort_options.slow_axis then ok3=0

;	Cannot have pop-ups in driver. This gets annoying ...
;	
;	if (ok2 eq 0) then begin
;		warning,'maia_device::ylut',['"incremental Y mode" selected using Y encoder correction.', $
;				'Hence, no YLUT test can be performed, and "cluster" mode cannot be used.']
;	endif
;	if (ok3 eq 0) then begin
;		warning,'maia_device::ylut',['Selected Y axis does not match "slow" axis. ', $
;				'Hence, no YLUT test can be performed, and "cluster" mode cannot be used.']
;	endif

;	return, ok and ok2 and ok3
	return, ok and ok3
end

;-------------------------------------------------------------------

; Build a Y lookup table file
 
function maia_device::build_ylut, files, output=output, error=error, force=force

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
		warning,'maia_device::build_ylut',['IDL run-time error caught.', '', $
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
	ylut = build_maia_ylut( files[0], slow_axis=self.sort_options.slow_axis, $
				output=output, error=error)
	
	if error eq 0 then begin
		*self.header.scan.pYlut = ylut
	endif
	return, ylut
end

;-------------------------------------------------------------------

; Build a Y lookup table file
 
pro maia_device::delete_ylut, files, output=output, error=error

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
		warning,'maia_device::delete_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	delete_maia_ylut, files[0], output=output, error=error
	return
end

;-------------------------------------------------------------------

function maia_device::get_ylut, files, strip=strip, output=output, error=error

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
		warning,'maia_device::get_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif

	ylut = get_maia_ylut( files, strip=strip, output=output, error=error)
	
	num = extract_extension( files)
	q = where( inumeric(num), nq)
	if nq gt 0 then begin
		inum = long( num[q])
		if max(inum) ge n_elements(ylut) then error=1
	endif

	if error eq 0 then begin
		*self.header.scan.pYlut = ylut
	endif
	return, ylut
end

;-------------------------------------------------------------------

function maia_device::range_ylut, files, error=error

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
		warning,'maia_device::range_ylut',['IDL run-time error caught.', '', $
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
	
	num = extract_extension( files)
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
 
pro maia_device::write_ylut, ylut, file, output=output, strip=strip, error=error

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
		warning,'maia_device::write_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	write_maia_ylut, ylut, file, output=output, strip=strip, error=error
	return
end

;-------------------------------------------------------------------

; Trim EVT file list for Y lookup table
 
function maia_device::trim_evt_files, filesi, mask=pmask, pYlut=pYlut, $
								yoffset=yoffset, yrange=yrange

; Trim the list of 'files' to only include files needed for the Y range
; and offset selected, or as seen in the region mask arrays.
;
; yoffset	Y offset (not compressed)
; yrange	Y range (not compressed)
; pmask		ptr to region mask ptr array
; pYlut		pointer to Y LUT for blog data files
;			Y LUT for Maia is a list of first Y value for each blog data file

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
		warning,'maia_device::trim_evt_files',['IDL run-time error caught.', '', $
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

    n = n_elements(filesi)
    if n eq 0 then return, ''
	files = filesi
;    if n eq 1 then goto, bad
    
;	mask of all Y values (uncompressed, no Y offset) spanned by 'files'

	yfiles = bytarr(top0+1)								; flags Y values used in files
	itag = lonarr(top0+1)								; itag[] equals file # for each Y
	np = n_elements(*pYlut)
	jlast = long(extract_extension(files[n-1]))
	if (jlast eq 0) and (n gt 1) then goto, bad_table	; catch zero as last entry in table' error
	if (jlast lt 0) then goto, bad_index
	if (jlast ge np) then begin
		warning,'maia_device::trim_evt_files',['Bad file YLUT index.','Data files exceed YLUT table.','', $
				'Delete the old YLUT file, and it will be regenerated to include missing data files.', $
				'','Continue ignoring missing data files for now ...']
		qf = where( long(extract_extension(files)) lt np, nqf)
		if nqf ne 0 then begin
			files = files[qf]
			n = nqf
			jlast = long(extract_extension(files[n-1]))
		endif
	endif
	ny1 = (*pYlut)[jlast]
	if (ny1 lt 0) then goto, bad_index
	if ny1 le top0 then begin
		yfiles[ny1:top0] = 1
		itag[ny1:top0] = n-1							; extend last to top
	endif
	for i=n-2,0,-1 do begin
		j = long(extract_extension(files[i]))
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
    
		yfiles2 = congrid( yfiles, top+1, /center)	; resize to compressed 'ny'
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
    
		yfiles2 = yfiles								; same, as not compressed?
		itag2 = itag
		
	    ymask = bytarr(top+1)							; flags Y used in regions
	    y1 = yoffset1 < top
	    y2 = top
	    ymask[y1:y2] = 1								; 'y' values used in region
	endelse
	
	fmask = bytarr(n)
	q = where( yfiles2 and (ymask eq 1), nq)
	if nq eq 0 then goto, bad_files
	for i=0L,nq-1 do begin
;		fmask[ itag2[q[i]] : itag2[ (q[i]+1) < top ] ] = 1	; flags file indices needed
		fmask[ itag2[q[i]] : (itag2[ (q[i]+1) < top ] + 1) < (n-1) ] = 1	; flags file indices needed
	endfor
    
	q = where(fmask eq 1, nq)
	if nq eq 0 then goto, bad_files
    
	return, files[q]
    
bad_files:
	warning,'maia_device::trim_evt_files','No Maia files contain the selected region(s).'
	return, ''
bad_index:
	warning,'maia_device::trim_evt_files',['Bad file YLUT index.','Data files exceed YLUT table.','', $
			'Delete the old YLUT file, and it will be regenerated to include missing data files.']
	return, ''
bad_table:
	warning,'maia_device::trim_evt_files',['Bad YLUT lookup table for files: '+files[0]+' ...', $
			'Delete the old YLUT file, and it will be regenerated.']
	return, ''
bad:
	return, files
end

;-------------------------------------------------------------------

; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.

function maia_device::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'maia_device::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_maia_2, maia_swap,length,skip,x0,y0,z0
common c_maia_3, monitor_array, n_monitor, maia_IC_name
common c_maia_4, n_times, time_last
common c_maia_7, maia_energy_pv
common c_maia_8, maia_last_energy, maia_last_flux
common c_maia_11, maia_hw_scaler, maia_fixed_dwell
common c_maia_12, maia_flux_mode
common c_maia_13, maia_dwell
common c_maia_19, maia_weight
common c_maia_14, maia_last_time, maia_first_time
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_maia_17, maia_x_range, maia_y_range, maia_z_range, maia_3D
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
if n_elements(xanes) lt 1 then xanes = 0
maia_3D = 0
if n_elements(zrange) eq 0 then zrange=1
if zrange gt 1 then maia_3D=1
x_coord_units = ''
y_coord_units = ''
z_coord_units = ''

		if first then begin
			err = init_maia_32()
			if err then goto, bad_io
;print,'Setup: first ...'

			gprint,level=2, 'Maia device: flip.X = ', self.sort_options.flip.x
			gprint,level=2, 'Maia device: skew.X = ', self.sort_options.skew.x

			maia = maia_defaults(source='maia_device::read_setup', /any)		; read "Maia.conf" default PVs
			maia_energy_pv = maia.epics.energy
			if maia_energy_pv eq '' then maia_energy_pv='ENERGY'
			gprint,level=2,' maia_device::read_setup: maia_energy_pv = '+maia_energy_pv
			
			self.spectrum_mode = 1
			if (n_elements(flux) ge 2) then self.spectrum_mode=0
			
;	Hardware counters "Maia:scaler.FC0" and "Maia.scaler.FC1" are already counts in a pixel. The scale needed
;	for these is just sensitivity val*unit.  The proxy hardware scaler "Maia.dwell.time" is also integral
;	for a pixel and so sets the H/W flag, but uses a scale of 1. For Epics PV's in c/s we need to scale by
;	dwell time (s).

			maia_IC_name = flux_ic.pv
			maia_hw_scaler = 0
			maia_flux_mode = 0
			nsls_flux_scale = 1.
			if maia_IC_name eq 'Maia:scaler.FC0' then begin
				maia_hw_scaler = 1
				maia_flux_mode = 1
				nsls_flux_scale = flux_ic.val * flux_ic.unit
			endif else if maia_IC_name eq 'Maia:scaler.FC1' then begin
				maia_hw_scaler = 1
				maia_flux_mode = 2
				nsls_flux_scale = flux_ic.val * flux_ic.unit
			endif else if maia_IC_name eq 'Maia:dwell.time' then begin
				maia_hw_scaler = 1
				maia_flux_mode = 3
				nsls_flux_scale = 1.
			endif

			if (maia_hw_scaler eq 0) and (self.spectrum_mode eq 0) and (xanes eq 0) then begin
				nsls_flux_scale = nsls_flux_scale * flux_ic.dwell*0.001		; scale Epics scaler rates by dwell only for maps
			endif
			maia_fixed_dwell = flux_ic.dwell
			gprint,level=2,' maia_device::read_setup: maia_IC_name = '+maia_IC_name+', nsls_flux_scale = '+str_tidy(nsls_flux_scale)

;	Set-up an internal buffer to hold a dwell array, and a total elapsed time

			if self.spectrum_mode then begin
				maia_dwell = flux[0]
				maia_weight = dead_fraction								; dead-time weight array (by detector #)
			endif else begin
				maia_dwell = flux[*,*,0]								; to replicate size and type as flux
				maia_weight = dead_fraction								; dead-time weight array, same as dead_fraction
			endelse
			maia_dwell[*] = 0.0											; zero only for 'first'
			maia_weight[*] = 0.0										
			self.dwell_total = 0.0
			
			maia_swap = (big_endian() ne self->big_endian())

;	# buffers to read before Progress = 100,000/n_guide > 1. N_buffer MUST be divisable by 4 for Fortran code.
;	Take care increasing n_buffer, as the maia_tt sum in spectrum mode will get large. Absolute limit is about 2M bytes.

;			n_buffer = 500000L				; ~500K byte buffer (0.5 MBytes)
;			n_guide = 50000L				; progress every 10th buffer (5 Mb)
			n_buffer = 20000000L			; 20M byte buffer (20 MBytes) for more efficient data handling poor latency drives
			n_guide = 20000000L				; progress every buffer (20 Mb)
			n_monitor = n_buffer			; monitor PV string return
			time_last = 9.0d+10				; set initially larger than any time
			nsls_IC = 0.0					; in c_nsls_1

			x0 = 0L							; in c_maia_2
			y0 = 0L
			z0 = 0L
			
;	For spec_evt scan for files, 'xrange' set to 16384 in 'spectrum_new_load' to force header read here.
;	For DA_evt scans, this header call is not used. Instead, Monitor records are scanned in 'read_buffer'.
;	self.no_change_options below prevents any changes to device sort options.

;			head = read_maia_32_header( unit, /no_scan, error=err32)
;			head = read_maia_32_header( unit, error=err32)

			stat = fstat( unit)
			file = stat.name
			head = get_maia_32_header( file, error=err32)		; needs to find .0 blog file regardless
			if err32 eq 0 then begin
				self->change_options, 0
				self->save_header_data, head			; save raw device data 'mp' in self
				self->update_header_info, error=error	; update self.header using saved 'mp'
				self->change_options, 1
			
				gprint,level=2, 'Maia device: flip.X = ', self.sort_options.flip.x
				gprint,level=2, 'Maia device: skew.X = ', self.sort_options.skew.x

				if xrange eq 16384 then begin
					if head.scan.xrange gt 1 then begin
						xrange = head.scan.xrange
					endif else if head.limits.x.max gt 1 then begin
						xrange = head.limits.x.max
					endif
				endif
				beam_energy = head.energy
			endif
			maia_x_range = long(xrange)
			maia_y_range = long(yrange)
			maia_z_range = long(zrange)
			
			event_array = bytarr(n_buffer)
			monitor_array = bytarr(n_monitor)

			maia_last_energy = 0.0			; in c_maia_8
			maia_last_flux = 0.0			; in c_maia_8
			maia_first_time = 0.0D+0		; in c_maia_14
			maia_last_time = 0.0D+0			; in c_maia_14
			
			if self.sort_options.encoder_y_correct eq 1 then begin			; axis 1 encoder mode
				if zrange ge 2 then begin
					if self.sort_options.clear.z lt 1 then begin
						warning,'maia_device::read_setup','Need to set a Z axis border clear for this encoder correction mode.'
					endif
				endif else begin
					if self.sort_options.clear.x lt 1 then begin
						warning,'maia_device::read_setup','Need to set a X axis border clear for this encoder correction mode.'
					endif
				endelse
			endif
			if self.sort_options.encoder_y_correct eq 3 then begin			; axes 1,2 encoder mode
				if zrange ge 2 then begin
					if self.sort_options.clear.z lt 1 then begin
						warning,'maia_device::read_setup','Need to set a Z axis border clear for this encoder correction mode.'
					endif
					if self.sort_options.clear.x lt 1 then begin
						warning,'maia_device::read_setup','Need to set a X axis border clear for this encoder correction mode.'
					endif
				endif else begin
					warning,'maia_device::read_setup','This encoder correction mode does not make sense for a 2D sort.'
				endelse
			endif
		endif
		
		ibranch = 0							; in c_sandia_6
		tag = 0								; in c_sandia_7
		length = 0L							; in c_maia_2
		skip = 0L							; in c_maia_2
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

function maia_device::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, z1=z1, $
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
;				'dead_fraction' returns dead-fraction, unless 'dwell' is also returned from device
;				(via 'get_dwell' method), then 'dead_fraction' returns dead-time per pixel.
;			
;	/by_odd		only for odd rows
;	/by_even	only for even rows
;
; return:
;   e			energy vector (uintarr) returned
;   t			Time-over-threshold, for some DAQs (e.g. Maia)
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
;	support_even_odd	=1 for Maia device (it supports selecting only even or odd Y rows)

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
		warning,'maia_device::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_maia_2, maia_swap,length,skip,x0,y0,z0
common c_maia_3, monitor_array, n_monitor, maia_IC_name
common c_maia_4, n_times, time_last
common c_maia_6, maia_y_min
common c_maia_7, maia_energy_pv
common c_maia_8, maia_last_energy, maia_last_flux
common c_maia_13, maia_dwell
common c_maia_19, maia_weight
common c_maia_14, maia_last_time, maia_first_time
common c_maia_15, maia_fx, maia_tags, maia_pseudo
common c_maia_16, maia_0, maia_1, maia_2, maia_e, maia_t, maia_ste, maia_veto
common c_maia_11, maia_hw_scaler, maia_fixed_dwell
common c_maia_12, maia_flux_mode
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_maia_17, maia_x_range, maia_y_range, maia_z_range, maia_3D
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_nsls_9, nsls_debug
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc

		if n_elements(xanes) lt 1 then xanes = 0				; 'xanes' not passed, so always zero
		if n_elements(zcompress) lt 1 then zcompress=1
		support_even_odd = 1
		if n_elements(by_odd) lt 1 then by_odd=0
		if n_elements(by_even) lt 1 then by_even=0
		if by_odd then by_even=0
		nc = n_elements(channel_on)								; channel on/off flag for each channel

		on_ioerror, bad_io
		read_event_buffer, unit, self, n_actual, bytes=1
;		print,'	Buffer: i_buffer = ',i_buffer, ' n_actual=',n_actual
		if n_actual eq 0 then begin
			gprint,level=2,'maia_device::read_buffer: bad n_actual'
			goto, bad_io
		endif
		i_buffer = i_buffer+1
		
		if n_elements(x1) eq 0 then begin						; just assign these 
			n_events = n_buffer/4								; vectors first time
			maia_e = uintarr(n_events, /nozero)
			maia_t = uintarr(n_events, /nozero)					; e,t per event
			maia_0 = lonarr(n_events, /nozero)
			maia_1 = lonarr(n_events, /nozero)					; x,y,z per event
			maia_2 = lonarr(n_events, /nozero)
			maia_ste = uintarr(n_events, /nozero)				; 'station', i.e. detector #
			maia_veto = uintarr(n_events, /nozero)				; flag a rejected event
			maia_tags = uintarr(n_events, /nozero)				; can be used to retrieve debug event info
			maia_pseudo = uintarr(n_events, /nozero)			; pseudo event (i.e. BT, FC0, FC1 in records added as events)
			n_fx = 4
			maia_fx = fltarr(n_fx,n_events, /nozero)			; selected flux (Epics or h/w), FC0, FC1, BT for each event
		endif else begin
			n_events = n_elements(maia_e)
			n_fx = n_elements(maia_fx[*,0])
		endelse
		
		width = 0
		height = 0
		clear_0 = 0
		clear_1 = 0
		case self.sort_options.source.z of
			0: begin
				clear_0 = self.sort_options.clear.z
				width = maia_z_range
				end
			1: begin
				clear_1 = self.sort_options.clear.z
				height = maia_z_range
				end
			else:
		endcase
		case self.sort_options.source.y of
			0: begin
				clear_0 = self.sort_options.clear.y
				width = maia_y_range
				end
			1: begin
				clear_1 = self.sort_options.clear.y
				height = maia_y_range
				end
			else:
		endcase
		case self.sort_options.source.x of
			0: begin
				clear_0 = self.sort_options.clear.x
				width = maia_x_range
				end
			1: begin
				clear_1 = self.sort_options.clear.x
				height = maia_x_range
				end
			else:
		endcase
		n = 0L
		nm = 0L
		good = 0L
		debug = 0L
		xcompress = xcompress > 1
		ycompress = ycompress > 1
		zcompress = zcompress > 1
;		gprint, level=1, 'maia_device::read_buffer: maia_swap = ', maia_swap
;		y0 = fix( y0 > yoffset)

;		Veto=0	real events returned in e,t,x1,y1,z1,ste vectors.
;		Veto=1	pseudo events (flux and dwell) values.
;		'n' returns high water mark in vectors, total number of events.
;		NOTE: for encoder_y_mode=1 to work maia_x_range must be actual scan X range.
;		Note that x0,y0,z0 keep current place of axis 0,1,2 (not necessarily x,y,z).
		
		err = maia_384_events6( event_array,n_actual, channel_on,nc, $
				maia_e,maia_t, maia_0,maia_1,maia_2, maia_ste, $
				maia_veto,maia_tags,n_events,n, $
				maia_fx,n_fx, maia_flux_mode, x0,y0,z0, monitor_array,n_monitor,nm, $
				maia_energy_pv, maia_last_energy, maia_IC_name, maia_last_flux, maia_last_time, maia_first_time, $
				self.sort_options.encoder_y_correct, clear_0,clear_1, width,height, $
				ibranch,maia_swap, tag,length,skip, bad_xy, debug )

		nsls_debug = debug
		if err ne 0 then begin
			gprint,level=2,'maia_device::read_buffer: error (',err,') return from maia_384_events6'
			if (bad_xy gt 0) then gprint,level=2,'		... and bad xy'
			goto, bad_io
		endif

;		gprint,level=2,'maia_device::read_buffer: PVs: energy, flux, time = ', maia_last_energy, maia_last_flux, maia_last_time

		if maia_last_energy gt 0.1 then begin
			beam_energy = maia_last_energy
			if beam_energy gt 500. then beam_energy = beam_energy/1000.
		endif
		if (maia_first_time ne 0.0D+0) and (maia_first_time lt time_last) then time_last = maia_first_time
		
		if n eq 0 then begin
			gprint,level=2,'maia_device::read_buffer: Zero "n" return from maia_384_events6'
			return, 0
		endif

;		Do the redirection from axes 012 to XYZ here. This is quite free, except for the YLUT action.
;		The code in da_evt, etc. is really geared towards YLUT working with real Y (Yoffset, Yrange3, etc.).
;		Hence, for now, we can ONLY have YLUT (maia_y_min) working with Y. This means for cluster processing
;		we can only have full 3D modes such as: EXY, XEY, theta-XY, X-theta-Y (theta-YX will not work).
		
;		if (maia_z_range gt 1) then begin
;			if (self.header.scan.z_on eq 0) then begin
;				gprint,level=2,'maia_device::read_buffer: Z axis requested, but Z axis not found to be "ON".'
;				goto, bad_io
;			endif
;		endif
		case self.sort_options.source.x of
			0: x1 = maia_0[0:n-1]
			1: x1 = maia_1[0:n-1]
			2: x1 = maia_2[0:n-1]
		endcase
		case self.sort_options.source.y of
			0: y1 = maia_0[0:n-1]
			1: y1 = maia_1[0:n-1]
			2: y1 = maia_2[0:n-1]
		endcase
		case self.sort_options.source.z of
			0: z1 = maia_0[0:n-1]
			1: z1 = maia_1[0:n-1]
			2: z1 = maia_2[0:n-1]
		endcase
;		print,'raw X1,y1=',x1[0],y1[0]

		if (self.sort_options.skew.x gt 0) then begin					; will this work with Cluster stripes?
			x1 = x1 - long( y1 / float(self.sort_options.skew.x) )
		endif
		if self.sort_options.flip.x then begin
			x1 = width - x1
		endif
		if self.sort_options.flip.y then begin					; will this work with Cluster stripes?
			y1 = height - y1
		endif
;		print,'flipped X1,y1=',x1[0],y1[0]

		case self.sort_options.slow_axis of						; min value for slow axis for "YLUT"
			0: maia_y_min = min(maia_0[0:n-1])
			1: maia_y_min = min(maia_1[0:n-1])
			2: maia_y_min = min(maia_2[0:n-1])
		endcase

		if self.abs_xy then begin								; use abs(x), abs(y)
			q = where( x1 ge 32*1024L, nq)						; this is selected in the Maia_abs device sub-class
			if nq gt 0 then x1[q] = uint(64*1024L - x1[q])
			q = where( y1 ge 32*1024L, nq)					
			if nq gt 0 then y1[q] = uint(64*1024L - y1[q])
			q = where( z1 ge 32*1024L, nq)					
			if nq gt 0 then z1[q] = uint(64*1024L - z1[q])
		endif

;;		qt1 = where( (x1 gt 32*1024L-1000) , nt1)
;		qt2 = where( ((x1 ge 7322) and (x1 le 7324)) and (y1 eq 2000), nt2)
;		if nt2 gt 0 then begin
;			print,'debug ...'
;;			ttx = 1.
;			qt = where( maia_veto[qt2] eq 1,nt4)
;			qt4 = qt2[qt]
;		endif
		e = maia_e[0:n-1]
		t = maia_t[0:n-1]
		ste = maia_ste[0:n-1]
		veto = maia_veto[0:n-1]		
		
;		n		high water mark in event vector buffers
;		good	number of valid events, so far (veto=0)

		q = where( veto eq 0, good, complement=qveto, ncomplement=n_veto)	; good (and pseudo) events
		maia_pseudo[*] = 0
		if n_veto gt 0 then maia_pseudo[qveto] = 1
		
		qt1 = where( (x1 ge 0) and (y1 ge 0), nt1)			; for debugging only?
		if nt1 gt 0 then begin
			qt2 = where( maia_pseudo eq 1,nt2)				; all pseudo events
			qt = where( maia_pseudo[qt1] eq 1,nt4)			; peusdo events w/ positive x1,y1
			qt4 = qt1[qt]
		endif

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
;		print,'		Buffer: n, good, n_veto, X1, Y1, pseudo_veto: ', n, good, n_veto, x1[0],y1[0],maia_pseudo[0]

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

		xrange2 = long( maia_x_range / xcompress)
		yrange2 = long( maia_y_range / ycompress)
		zrange2 = long( maia_z_range / zcompress)

		if (self.spectrum_mode eq 0) and (xanes eq 0) then begin				; images

;			This scales the count with compression for PV rates. H/w counters use the accumulation approach.
;			Ignore border pixel data for mapping (where flux is not scalar) ...
;			Offset increment into 'flux' by (compressed) offset for this stripe.
;			Combine all detectors, so divide DTcal by the number active 'nqo'.
;			Do NOT divide by average dwell, as the divide by dwell will happen in 'da_evt' later.

			xmin = self.sort_options.clear.x + (xoffset2 < 0)					; optionally clear border pixels
			ymin = self.sort_options.clear.y + (yoffset2 < 0)
			qo = where( channel_on eq 1, nqo)
			zmin = maia_3D ? (self.sort_options.clear.z + (zoffset2 < 0)) : 0
;			dta = 1.0e-6 * self.sort_options.deadtime_cal.a / (float(nqo) * maia_fixed_dwell)	; this duplicate /dwell norm in 'da_avt'
;			dtb = 1.0e-6 * self.sort_options.deadtime_cal.b / (float(nqo) * maia_fixed_dwell)			
			dta = 1.0e-6 * self.sort_options.deadtime_cal.a / float(nqo)
			dtb = 1.0e-6 * self.sort_options.deadtime_cal.b / float(nqo)			

;			Test border, and offset all, not just good events, so that pseudo events get offset ...
		
			zbad = (maia_3D eq 0) ? 0 : ((z1 lt zmin) or (z1 gt (zrange2-1-zmin)))
			q1 = where( (x1 lt xmin) or (y1 lt ymin) or (x1 gt (xrange2-1-xmin)) or (y1 gt (yrange2-1-ymin)) or zbad, nq1)
			if nq1 gt 0 then begin
				veto[q1] = 1						; veto border events
				maia_pseudo[q1] = 0					; also do not include flux, etc. for border
			endif
			q = where( veto eq 0, good)
			
			x1 = uint(x1 - xoffset2)
			y1 = uint(y1 - yoffset2)
			z1 = uint(z1 - zoffset2)
			
;			Accumulate flux (including attributes), dead_fraction and dwell ...
;			Do not ignore veto 'events' (these may be pseudo events). Use flux_mode to select action:
;				0	Epics PV as a rate, so set the flux in a pixel to maia_fx[0,*], if it is not zero
;						scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
;				1,2	H/W flux counter used and accumulated in maia_fx[0,*] in Fortran, so accumulate maia_fx[0,*] 
;						here in flux[*,*,0] scaled by: nsls_flux_scale
;				
;					Use FC0 in maia_fx[1,*] to accumulate into flux[*,*,1]
;					Use FC1 in maia_fx[2,*] to accumulate into flux[*,*,2]
;			
;				* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
;
;			Dwell:
;				Use maia_fx[3,*] (if not zero) to set Dwell in maia_dwell (can pass by ref, better than self.dwell)
;
;			Dead_fraction:
;				Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
;				Use fx[4,*] to set DT in dead_fraction, weighted by 1/nqo in imaging case (see above).
;				Later (in da_evt) norm this using the get_dwell() method, which returns the dwell (ms) array.
		
		 	if maia_3D then begin
				maia_accumulate_dtfx2_3D, /image, t,x1,y1,z1,ste,veto,maia_fx,n,n_fx, maia_flux_mode, nsls_flux_scale, $
		 				n_elements(flux[*,0,0]),n_elements(flux[0,*,0]),n_elements(flux[0,0,*]), dta,dtb, dead_fraction, pseudo=maia_pseudo, $
		 				flux=flux, weight=maia_weight, dwell=maia_dwell, xcompress=xcompress,ycompress=ycompress, error=err
		 	endif else begin
				maia_accumulate_dtfx2, /image, t,x1,y1,ste,veto,maia_fx,n,n_fx, maia_flux_mode, nsls_flux_scale, $
		 				n_elements(flux[*,0,0]),n_elements(flux[0,*,0]),n_elements(flux[0,0,*]), dta,dtb, dead_fraction, pseudo=maia_pseudo, $
		 				flux=flux, weight=maia_weight, dwell=maia_dwell, xcompress=xcompress,ycompress=ycompress, error=err
		 	endelse
		endif else begin										; spectra

;			Flux needs to be summed for h/w FC counters (flux_mode >0), and set times for Epics (flux_mode=0).
;			Set the time-stamp 'time_PV' for pseudo events to Monitor time.
;	
;			Accumulate 't' into dead_fraction (across detectors), using cal to make it in ms units.
;			This time use the veto array.
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
;			print,'offset X1,y1=',x1[0],y1[0]
			
;			Detectors returned individually, so do not divide by the number active.
	
			dta = 1.0e-6 * self.sort_options.deadtime_cal.a 		; ms
			dtb = 1.0e-6 * self.sort_options.deadtime_cal.b	
	
;			Dwell:
;				Use maia_fx[3,*] (if not zero) to accumulate total Dwell in self.dwell_total.
;
;			Dead_fraction:
;				Accumulate 't' into dead_fraction (detector array), using cal to make it in ms units.
;				Later (in spec_evt) norm this using the get_total_time() method, which returns the total dwell (ms).
;
;			Remember to NOT skip veto events, some will be pseudo events with valid BT, FC0, FC1

			if maia_hw_scaler then begin							; flux is NOT suitable for Regions
				flux = flux + nsls_flux_scale * total(maia_fx[0,0:n-1])
				dt = total(maia_fx[3,0:n-1])						; dwell time (ms)
			endif else begin
				dt = 0.
				if time_last gt 0. then dt = ((maia_last_time - time_last) > 0) * 1000.		; dt (ms)
				flux = flux + nsls_flux_scale * maia_last_flux * dt * 0.001	
			endelse

			maia_accumulate_dtfx2, /spectrum, t,x1,y1,ste,veto,maia_fx,n,n_fx, maia_flux_mode, nsls_flux_scale, $
 				n_elements(dead_fraction[*,0]),1,1, dta,dtb, dead_fraction, weight=maia_weight, error=err
 				
			self.dwell_total = self.dwell_total + dt				; ms
			time_last = maia_last_time
		endelse

       processed = processed + good

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function maia_device::get_dead_weight, error=error

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
		warning,'maia_device::get_dead_weight',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_maia_19, maia_weight

	error = 0
	if total(maia_weight) lt 0.1 then error=1		; no raw counts found

	return, maia_weight
end

;-------------------------------------------------------------------

function maia_device::get_dead_weight_mode, error=error

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
		warning,'maia_device::get_dead_weight_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_maia_19, maia_weight

	mode = 1

	error = 0
	return, mode
end

;-------------------------------------------------------------------

function maia_device::get_dwell, error=error

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
		warning,'maia_device::get_dwell',['IDL run-time error caught.', '', $
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

function maia_device::import_spec, name, file, group=group

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
		warning,'maia_device::get_import_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'maia_evt': begin							; 9
			warning,'maia_device::import_spec',['"maia_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
		'maia_old_evt': begin						; 9
			warning,'maia_device::import_spec',['"maia_old_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function maia_device::get_import_list, error=error

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
		warning,'maia_device::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_39 = define(/import)			; Maia 384 list-mode
		opt_39.name =		'maia_evt'	; unique name of import
		opt_39.title =		'Extract from Maia-384/96 - corrected blog files'
		opt_39.in_ext =		''			; input file extension
		opt_39.request =	'Maia blog data scan for all spectra [Enter FIRST file or range]'
		opt_39.preview =	0			; allow spectrum preview
		opt_39.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_39.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_39.multifile =	1			; denotes data in a series of more than one file
		opt_39.separate =	'.'			; char between file and run #
		opt_39.use_pileup =	1			; request pileup file
		opt_39.use_throttle = 1			; request throttle file
		opt_39.use_IC =		1			; pop-up the flux_select PV selection panel
		opt_39.IC_mode = 	1			; default to using PV for IC
		opt_39.use_tot =	1			; collect ToT data too
		opt_39.xr =			16384		; default X range (this value forces header read)
		opt_39.yr =			16384		; default Y range
	
	opt_40 = define(/import)			; Maia 96 list-mode
		opt_40.name =		'maia_old_evt'		; unique name of import
		opt_40.title =		'Extract from Maia-384/96 - specifiy re-linearization'
		opt_40.in_ext =		''			; input file extension
		opt_40.request =	'Maia blog data scan for all spectra [Enter FIRST file or range]'
		opt_40.preview =	0			; allow spectrum preview
		opt_40.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_40.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_40.multifile =	1			; denotes data in a series of more than one file
		opt_40.separate =	'.'			; char between file and run #
		opt_40.use_linear =	1			; request linearization file
		opt_40.use_pileup =	1			; request pileup file
		opt_40.use_throttle = 1			; request throttle file
		opt_40.use_IC =		1			; pop-up the flux_select PV selection panel
		opt_40.IC_mode = 	1			; default to using PV for IC
		opt_40.use_tot =	1			; collect ToT data too
		opt_40.xr =			16384		; default X range (this value forces header read)
		opt_40.yr =			16384		; default Y range

	opt = [opt_39, opt_40]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function maia_device::init

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
		warning,'maia_device::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	self.pileup.use = 1				; Uses pileup curve		Maia specfic items
	self.throttle.use = 1			; Uses Throttle vectors
	self.linear.use = 1				; May use Linearization correction
	
	self.header.scan.use_ylut = 1	; Maia device can use a Y lookup table
	self.header.scan.pYlut = ptr_new(/allocate_heap)	; allocate heap for Y LUT
	
; Sort Option items appear as device setup options in the Sort EVT "Device" tab

	self.options.scan.on = 1		; scan sort options available for this class
									; these must be set-up in the render_options method
;	self.options.scan.ysize = 227	; Y size of sort options box, when open
	self.options.scan.ysize = 247	; Y size of sort options box, when open (w/ skew)

; Set default Sort Options local parameters ...

;	self.sort_options.x_margin = 2					; set default X margin
	self.sort_options.clear.x = 0					; no clear X margins by default
	self.sort_options.clear.y = 0					; no clear Y margins by default
	self.sort_options.clear.z = 0					; no clear Z margins by default
	self.sort_options.flip.x = 0					; no flip X axis by default
	self.sort_options.flip.y = 0					; no flip Y axis by default
	self.sort_options.skew.x = 0					; no skew X axis by default
	self.sort_options.deadtime_cal.a = 0.0			; deadtime calibration (slope)
	self.sort_options.deadtime_cal.b = 0.0			; deadtime cal (offset)
	self.sort_options.source.x = 0					; axis to use for X
	self.sort_options.source.y = 1					; axis to use for Y
	self.sort_options.source.z = 2					; axis to use for Z
	self.sort_options.slow_axis = 1					; Y is slow axis by default (can't change this yet)

; Initial heap allocation for sort_id widget vector pointers ...

	self.sort_id.yenable = ptr_new(/allocate_heap)			; XY correct mode ID array pointer
;	self.sort_id.xmargin = ptr_new(/allocate_heap)			; X margin droplist ID array pointer
;	self.sort_id.clear_border = ptr_new(/allocate_heap)		; Clear X,Y border check-box ID array pointer
	self.sort_id.slow = ptr_new(/allocate_heap)				; Slowest axis droplist ID array pointer
	self.sort_id.clear_x = ptr_new(/allocate_heap)			; Clear X border droplist ID array pointer
	self.sort_id.clear_y = ptr_new(/allocate_heap)			; Clear X border droplist ID array pointer
	self.sort_id.clear_z = ptr_new(/allocate_heap)			; Clear X border droplist ID array pointer
	self.sort_id.flip_x = ptr_new(/allocate_heap)			; Flip X axis check-box ID array pointer
	self.sort_id.flip_y = ptr_new(/allocate_heap)			; Flip Y axis check-box ID array pointer
	self.sort_id.skew_x = ptr_new(/allocate_heap)			; Skew along X axis check-box ID array pointer
	self.sort_id.deadtime_cal_a = ptr_new(/allocate_heap)	; Deadtime Cal A text ID array pointer
	self.sort_id.deadtime_cal_b = ptr_new(/allocate_heap)	; Deadtime Cal B text ID array pointer
	self.sort_id.source_x = ptr_new(/allocate_heap)			; axis to use for X ID ptrarr
	self.sort_id.source_y = ptr_new(/allocate_heap)			; axis to use for Y ID ptrarr
	self.sort_id.source_z = ptr_new(/allocate_heap)			; axis to use for Z ID ptrarr
	
;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"MAIA_DEVICE" --> MAIA_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'MAIA_DEVICE', $	; unique name for this device object
		title = 'Maia 384 (Sync) - HYMOD data acquisition', $
		ext = '', $				; not a fixed file extension for blog data
		multi_files = 1, $		; multiple segment files per run
		multi_char = '.', $		; separates run from segment number in file name
		big_endian = 1, $		; blog data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 0, $		; start detector ADC #'s at 0
		use_bounds = 0, $		; not confine charge/flux within bounded area
		synchrotron = 1, $		; synchrotron data
		ionbeam = 0, $			; but not ion-beam data (use MAIA_NMP_DEVICE for that)
		array_default = 1, $	; a detect array by default
		use_cluster = 1)		; uses cluster parallel processing
	return, i
end

;-------------------------------------------------------------------

pro maia_device__define

; Define Maia device object internal data structure.
; This adds some internal device parameters, local to Maia in 'sort_options',
; and device specific sort widgets options to be displayed in the 'Scan' tab
; of the Sort window and elsewhere in 'sort_id'.
; Only called using obj = obj_new('MAIA_DEVICE')

COMPILE_OPT STRICTARR

maia = { MAIA_DEVICE,  $

		INHERITS BASE_DEVICE, $								; mandatory base device parameters

		sort_options : {sort_options_devicespec_maia, $		; Sort EVT window Sort options panel
				encoder_y_correct:	0, $					; XY sort correction mode (0=ff, 1=encoder Y correct, 2=Filter XY +- glitches)
;				x_margin:			2, $					; default X margin pixels in Y_mode=1
				clear: {sort_options_clear_devicespec_maia, $
						x:			0, $					; width clear X borders in sort
						y:			0, $					; width clear Y borders in sort
						z:			0}, $					; width clear Z borders in sort
				flip: {sort_options_flip_devicespec_maia, $
						x:			0, $					; flip X axis in sort
						y:			0}, $					; flip Y axis in sort
				deadtime_cal: {sort_options_deadtime_maia, $
						a:			1.0, $					; deadtime calibration (slope)
						b:			0.0 }, $				; deadtime cal (offset)
				source: {options_source_devicespec, $
						x:			0, $					; X axis source (0,1,2 PA data)
						y:			1, $					; Y axis source (0,1,2 PA data)
						z:			2 }, $					; Z axis source (0,1,2 PA data)
				slow_axis:	1, $							; slow axis for YLUT  (LATER?)
				skew: {sort_options_skew_devicespec_maia, $
						x:			0 }}, $					; correct a skew offset in X (divide Y by this for correction)
											
		sort_id: {sort_id_maia, $							; pointers to vector of sort widget IDs
				yenable:					ptr_new(), $	; XY correct mode ID array pointer
;				xmargin:					ptr_new(), $	; X margin droplist ID array pointer
;				clear_border:				ptr_new(), $	; Clear X,Y border check-box ID array pointer
				slow:						ptr_new(), $	; Slowest axis droplist ID array pointer
				clear_x:					ptr_new(), $	; Clear X border droplist ID array pointer
				clear_y:					ptr_new(), $	; Clear Y border droplist ID array pointer
				clear_z:					ptr_new(), $	; Clear Z border droplist ID array pointer
				flip_x:						ptr_new(), $	; Flip X axis check-box ID array pointer
				flip_y:						ptr_new(), $	; Flip Y axis check-box ID array pointer
				skew_x:						ptr_new(), $	; Skew X axis check-box ID array pointer
				deadtime_cal_a:				ptr_new(), $	; Deadtime Cal A text ID array pointer
				deadtime_cal_b:				ptr_new(), $	; Deadtime Cal B text ID array pointer
				source_x:					ptr_new(), $	; Re-direction droplist(s) ID array pointer
				source_y:					ptr_new(), $	; Re-direction droplist(s) ID array pointer
				source_z:					ptr_new()}, $	; Re-direction droplist(s) ID array pointer

		abs_xy:		0,  $									; flags use of abs(X), abs(Y) pixel addresses
		no_change_options:		0  $						; flags freeze options (i.e. to set flip.x)
	}
	return
end
