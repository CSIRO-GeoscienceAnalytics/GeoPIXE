;
; GeoPIXE Device Object for DAQ detector array data
; 
; DAQ is a 36 channel data acquisition system (32 via Scepter, 4 via NIM ADC interface)
; system developed by CSIRO-BNL for general purpose data acquisition.
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
;					Y LUT for DAQ is a list of first Y value for each blog data file.
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
; method will be used in the "BASE_DEVICE" master-class. See the code in the DAQ_device
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
; methods (see examples in DAQ_device). They are not essential and are NOT called
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
; The code in the render_options for creating options widgets for the DAQ device,
; which also calls some OnRealize routines and an event handler, can be used as a model
; for new device options fields.
;
;----------------------------------------------------------------------------------------------------

pro daq_device::cleanup

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
		warning,'daq_device::cleanup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

;	if ptr_valid( self.dwell) then ptr_free, self.sort_id.dwell

	if ptr_valid( self.sort_id.x_axis) then ptr_free, self.sort_id.x_axis
	if ptr_valid( self.sort_id.y_axis) then ptr_free, self.sort_id.y_axis
	if ptr_valid( self.sort_id.clear_border) then ptr_free, self.sort_id.clear_border
	if ptr_valid( self.sort_id.deadtime_cal_a) then ptr_free, self.sort_id.deadtime_cal_a
	if ptr_valid( self.sort_id.deadtime_cal_b) then ptr_free, self.sort_id.deadtime_cal_b
	if ptr_valid( self.sort_id.deadtime_nim) then ptr_free, self.sort_id.deadtime_nim
	
	self->BASE_DEVICE::cleanup
    return
end

;------------------------------------------------------------------------------------------

; Render options widgets in Sort Options box in Sort EVT window.
; Parent is the framed container box. Its child must be a base that
; all options widgets are attached to. This child is target of destroy
; when switching devices.

pro daq_device::render_options, parent

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
			warning,'daq_device::render_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	case !version.os_family of
		'MacOS': begin
			deadtime_xsize = 107
			axis_xsize = 100
		end
		'unix': begin
			deadtime_xsize = 107
			axis_xsize = 100
		end
		else: begin
			deadtime_xsize = 107
			axis_xsize = 100
		end
	endcase
	axes = ['0  DAC X','1  DAC Y','2  Stage X','3  Stage Y','4  Stage Z','5  Stage A']

	; Call super-class to cleanup old display and set Y size of box first ...

	self->BASE_DEVICE::render_options, parent

	; The following will appear in the Options box on the Sort tab of the Sort EVT window ...

	daqmode_base = widget_base( parent, /column,  space=1, xpad=0, ypad=0, /base_align_center, $
		event_func='daq_device_sort_option_event', uvalue=self, uname='obj-ref-here')
	lab = widget_label( daqmode_base, value='DAQ Option Parameters')

	; Droplists (combobox) of X,Y axis index used to select default X,Y axes. These show as 0,1,2,3,...
	; The uvalue contains a help string to be displayed to explain this widget.

	DAQ_xyaxis_base = widget_base( daqmode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( DAQ_xyaxis_base, value='Axis  X:')
	DAQ_xaxis = widget_combobox( DAQ_xyaxis_base, value=axes, uname='daq-x-axis', /tracking, $
		notify_realize='OnRealize_daq_device_sort_option_x_axis', sensitive=1, $
		uvalue='Select the default Axis to use for X.',xsize=axis_xsize)

	lab = widget_label( DAQ_xyaxis_base, value='    Axis  Y:')
	DAQ_yaxis = widget_combobox( DAQ_xyaxis_base, value=axes, uname='daq-y-axis', /tracking, $
		notify_realize='OnRealize_daq_device_sort_option_y_axis', sensitive=1, $
		uvalue='Select the default Axis to use for Y.',xsize=axis_xsize)

	; Check-boxes for optional X and Y margin clears. the width of X margin is set by the X margin droplist.
	; The uvalue contains a help string to be displayed to explain this widget.

	daq_clear_border = cw_bgroup2( daqmode_base, ['Clear X borders','Clear Y borders'], /row, set_value=[self.sort_options.clear.x, self.sort_options.clear.y], sensitive=1, $
		/return_index, uname='daq-options',/ nonexclusive, /tracking, ypad=0, $
		uvalue=['Optionally clear X border pixels. The width of the cleared area is set by the "X margin" droplist.','Optionally clear a single row of Y border pixels.'])

	daqdbase = widget_base( daqmode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( daqdbase, value='DT cal A:')
	daq_deadtime_cal_a = widget_text( daqdbase, value=str_tidy(self.sort_options.deadtime_cal.a), uname='daq-deadtime-cal-a', /tracking, /editable, $
		uvalue='Calibration (gain, ns) of SCEPTER time-over-threshold (T).', scr_xsize=deadtime_xsize)
	lab = widget_label( daqdbase, value='    B:')
	daq_deadtime_cal_b = widget_text( daqdbase, value=str_tidy(self.sort_options.deadtime_cal.b), uname='daq-deadtime-cal-b', /tracking, /editable, $
		uvalue='Calibration (offset, ns) of SCEPTER time-over-threshold (T).', scr_xsize=deadtime_xsize)

	daqdbase2 = widget_base( daqmode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( daqdbase2, value='DT cal NIM:')
	daq_deadtime_nim = widget_text( daqdbase2, value=str_tidy(self.sort_options.deadtime_nim), uname='daq-deadtime-nim', /tracking, /editable, $
		uvalue='Calibration (gain, ns) of NIM dead-time (T).', scr_xsize=deadtime_xsize)

	error = 0
	add_widget_vector, self.sort_id.x_axis, DAQ_xaxis, error=err & error=error or err
	add_widget_vector, self.sort_id.y_axis, DAQ_yaxis, error=err & error=error or err
	add_widget_vector, self.sort_id.clear_border, daq_clear_border, error=err & error=error or err
	add_widget_vector, self.sort_id.deadtime_cal_a, daq_deadtime_cal_a, error=err & error=error or err
	add_widget_vector, self.sort_id.deadtime_cal_b, daq_deadtime_cal_b, error=err & error=error or err
	add_widget_vector, self.sort_id.deadtime_nim, daq_deadtime_nim, error=err & error=error or err
	if error then begin
		warning,'daq_device::render_options','Error adding device object widget ID vectors.'
	endif
	return
end

;-------------------------------------------------------------------------------
; These routines are associated with the rendering of Sort Options widgets
; in the Sort EVT window Sort tab options box.
;------------------------------------------------------------------------------------------

pro OnRealize_daq_device_sort_option_x_axis, wWidget

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
			warning,'OnRealize_daq_device_sort_option_x_axis',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_daq_device_sort_option_x_axis: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_daq_device_sort_option_x_axis: Device Object ref not found.'
		return
	endif

	options = obj->get_options()

	widget_control, wWidget, set_combobox_select=options.axis.x
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_daq_device_sort_option_y_axis, wWidget

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
			warning,'OnRealize_daq_device_sort_option_y_axis',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_daq_device_sort_option_y_axis: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_daq_device_sort_option_y_axis: Device Object ref not found.'
		return
	endif

	options = obj->get_options()

	widget_control, wWidget, set_combobox_select=options.axis.y
	return
end

;------------------------------------------------------------------------------------------

function daq_device_sort_option_event, event

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
			warning,'daq_device_sort_option_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0L
		endif
	endif

	if widget_info( event.handler, /valid) eq 0L then begin
		;		print,'daq_device_sort_option_event: event.handler not valid.'
		return, 0
	endif
	uname = widget_info( event.handler, /uname)
	if uname ne 'obj-ref-here' then begin
		print,'daq_device_sort_option_event: Object base not found.'
		return, 0
	endif
	widget_control, event.handler, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'daq_device_sort_option_event: Device Object ref not found.'
		return, 0
	endif

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			return, event						; pass context help up the line ...
		end
		else: begin

			uname = widget_info( event.id, /uname)
			case uname of
				'daq-x-axis': begin
					obj->set_options, x_axis = event.index
					obj->update_header_info, error=error
					obj->update_notify
				end
				'daq-y-axis': begin
					obj->set_options, y_axis = event.index
					obj->update_header_info, error=error
					obj->update_notify
				end
				'daq-options': begin
					case event.value of
						0: begin
							obj->set_options, clear_x = event.select
						end
						1: begin
							obj->set_options, clear_y = event.select
						end
					endcase
				end
				'daq-deadtime-cal-a': begin
					widget_control, event.id, get_value=s
					obj->set_options, dt_cala = float2(s)
				end
				'daq-deadtime-cal-b': begin
					widget_control, event.id, get_value=s
					obj->set_options, dt_calb = float2(s)
				end
				'daq-deadtime-cal-nim': begin
					widget_control, event.id, get_value=s
					obj->set_options, deadtime_nim = float2(s)
				end
			endcase
		end
	endcase
	return, 0L
end

;-------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written FROM DISK using the "read_options",
; "write_options" methods.
;
;	options		return current option values

pro daq_device::read_options, unit, options=options, error=error

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
		warning,'daq_device::read_options',['IDL run-time error caught.', '', $
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
	deadtime_cal = options.deadtime_cal
	deadtime_nim = options.deadtime_nim
	axis_x = options.axis.x
	axis_y = options.axis.y
	version = 0L
	
; Read options parameters from the file

	on_ioerror, bad_io
	if self.use_version then readu, unit, version
	
	if self.use_version then begin
		if version le -1 then begin
			readu, unit, clear_x, clear_y
			readu, unit, deadtime_cal

			clear_x = clip(clear_x,0,1)
			clear_y = clip(clear_y,0,1)
		endif

		if version le -2 then begin
			readu, unit, axis_x, axis_y

			axis_x = clip(axis_x,0,5)
			axis_y = clip(axis_y,0,5)
		endif

;		if version le -3 then begin
;			readu, unit, deadtime_nim
;		endif
	endif
	
; Write back these options arameters to the device ...

	options.clear.x = clear_x
	options.clear.y = clear_y
	options.deadtime_cal = deadtime_cal
	options.deadtime_nim = deadtime_nim
	options.axis.x = axis_x
	options.axis.y = axis_y
	
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

pro daq_device::write_options, unit, options=p, error=error

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
		warning,'daq_device::write_options',['IDL run-time error caught.', '', $
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
	
	version = -2L
;	version = -3L
	
	on_ioerror, bad_io
	writeu, unit, version
	
	writeu, unit, options.clear.x, options.clear.y
	writeu, unit, options.deadtime_cal
	writeu, unit, options.axis.x, options.axis.y
;	writeu, unit, options.deadtime_nim
	error = 0
	return
	
bad_io:
	error = 1
	return
end

;-------------------------------------------------------------------------------

; Return a string to display (e.g. in Image History window) to show the
; state of this device's options. If 'p' present, then show this parameter set.

function daq_device::options_legend, p

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
		warning,'daq_device::read_options',['IDL run-time error caught.', '', $
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

	list = ['DAQ:' ] 
	list = [list, '   Clear margins X: ' + on_off[options.clear.x] + ', Y: ' + on_off[options.clear.y] ]
	list = [list, '   DT Cal A: ' + str_tidy(options.deadtime_cal.a) + ', B: ' + str_tidy(options.deadtime_cal.b) ]
	list = [list, '   DT Cal NIM: ' + str_tidy(options.deadtime_nim) ]

	return, list
end

;------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written from/to DISK using the "read_options",
; "write_options" methods. Keep a local copy of device parameters and set them using
; "set_options". 

pro daq_device::set_options, p, clear_x=clear_x, clear_y=clear_y, x_axis=x_axis, y_axis=y_axis, $
				deadtime_cal=deadtime_cal, dt_cala=dt_cala, dt_calb=dt_calb, deadtime_nim=dt_cal_nim

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
		warning,'daq_device::set_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(p) eq 1 then begin
		if size(p, /tname) eq 'STRUCT' then begin
			daq = p
		endif else if ptr_good(p,/struct) then begin
			daq = *p
		endif else return
	endif

	if size(daq, /tname) eq 'STRUCT' then begin
		if tag_present('AXIS',daq) then self.sort_options.axis.x = daq.axis.x
		if tag_present('AXIS',daq) then self.sort_options.axis.y = daq.axis.y
		if tag_present('CLEAR',daq) then self.sort_options.clear.x = daq.clear.x
		if tag_present('CLEAR',daq) then self.sort_options.clear.y = daq.clear.y
		if tag_present('DEADTIME_CAL',daq) then self.sort_options.deadtime_cal = daq.deadtime_cal
		if tag_present('deadtime_nim',daq) then self.sort_options.deadtime_nim = daq.deadtime_nim
	endif else begin
		if n_elements(x_axis) eq 1 then self.sort_options.axis.x = x_axis
		if n_elements(y_axis) eq 1 then self.sort_options.axis.y = y_axis
		if n_elements(clear_x) eq 1 then self.sort_options.clear.x = clear_x
		if n_elements(clear_y) eq 1 then self.sort_options.clear.y = clear_y
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
		if n_elements(dt_cal_nim) eq 1 then self.sort_options.deadtime_nim = dt_cal_nim
	endelse
	
	; Set value of widgets. There may be multiple sort option panels attached to this object, so
	; we use 'widget_control_vector' to set all of them.
	
	widget_control_vector, self.sort_id.x_axis, set_combobox_select = self.sort_options.axis.x
	widget_control_vector, self.sort_id.y_axis, set_combobox_select = self.sort_options.axis.y
	widget_control_vector, self.sort_id.clear_border, set_value = [self.sort_options.clear.x,self.sort_options.clear.y]
	widget_control_vector, self.sort_id.deadtime_cal_a, set_value = str_tidy(self.sort_options.deadtime_cal.a)
	widget_control_vector, self.sort_id.deadtime_cal_b, set_value = str_tidy(self.sort_options.deadtime_cal.b)
	widget_control_vector, self.sort_id.deadtime_nim, set_value = str_tidy(self.sort_options.deadtime_nim)
	return
end

;-------------------------------------------------------------------

function daq_device::get_options, error=error

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
		warning,'daq_device::get_options',['IDL run-time error caught.', '', $
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

	widget_control_vector, self.sort_id.deadtime_nim, /first, get_value=s, error=err
	if err eq 0 then self.sort_options.deadtime_nim = float2(s)
	return, self.sort_options
end

;-------------------------------------------------------------------

; This method is a local one for the DAQ device, and returns the
; selected X axis for imaging.

function daq_device::get_Xaxis, error=error

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
    warning,'daq_device::get_Xaxis',['IDL run-time error caught.', '', $
        'Error:  '+strtrim(!error_state.name,2), $
        !error_state.msg,'',c], /error
    MESSAGE, /RESET
    return, 0
  endif
endif

  error = 0
  t = self.sort_options.axis.x
  
  return, t
end

;-------------------------------------------------------------------

; This method is a local one for the DAQ device, and returns the
; selected X axis for imaging.

function daq_device::get_Yaxis, error=error

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
    warning,'daq_device::get_Yaxis',['IDL run-time error caught.', '', $
        'Error:  '+strtrim(!error_state.name,2), $
        !error_state.msg,'',c], /error
    MESSAGE, /RESET
    return, 0
  endif
endif

  error = 0
  t = self.sort_options.axis.y
  
  return, t
end

;-------------------------------------------------------------------
;-------------------------------------------------------------------

; This method is not really used anymore from GeoPIXE, as the deadtime cal is
; internally handled in device objects. 
; However, is still used in DAQ control.

function daq_device::get_deadtime_cal, more=more, error=error

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
    warning,'daq_device::get_deadtime_cal',['IDL run-time error caught.', '', $
        'Error:  '+strtrim(!error_state.name,2), $
        !error_state.msg,'',c], /error
    MESSAGE, /RESET
    return, 0
  endif
endif
  
	error = 0
	pars = self->get_options()
	more = pars.deadtime_nim
	return, pars.deadtime_cal
end

;-------------------------------------------------------------------
;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

function daq_device::get_header_info, file, output=output, silent=silent, error=error

; file		a raw data file to look for associated header, metadata
; output	if present, this is a file on the output path, if some metadata is
;			located in that path (e.g. DAQ Y LUT). 
; /silent	suppress any pop-ups.

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
		warning,'daq_device::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	if n_elements(silent) lt 1 then silent=0
	error = 1
	self.header.error = 1

	mp = get_daq_32_header( file, silent=silent, error=error)
	if error then return,0

;	Y lookup table, stored with output, or on blog raw data path ...
;	The Y LUT contains a list of the first Y value in each raw data file in the multi-file set.

	if n_elements(output) gt 0 then begin
		retry = 0
		ylut = get_daq_ylut(file, output=output, error=err)				; stored now with output
		if err or (n_elements(ylut) le 1) then retry=1	
	endif else retry=1
	if retry then begin
		ylut = get_daq_ylut( file, error=err)							; if not, try blog path
	endif	
	if err eq 0 then begin					
		*self.header.scan.pYlut = ylut	
	endif
	
	if mp.scan.order ne '' then begin
		str = strsplit(mp.scan.order,' ',/extract)
		if n_elements(str) ge 2 then begin
			self->set_options, x_axis=clip(fix2(str[0]),0,5), y_axis=clip(fix2(str[1]),0,5)
		endif
	endif

	self->save_header_data, mp						; save raw device data 'mp' in self
	self->update_header_info, error=error			; update self.header using saved 'mp'
	self.header.error = 0
	return, self.header
end

;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the
; contents of the header. Of particular importance are the scan sizes in pixels.

pro daq_device::update_header_info, error=error

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
			warning,'daq_device::update_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	error = 1
	pmp = self.old_mp
	if ptr_good(pmp) eq 0 then return

	case self.sort_options.axis.x of
		0: begin
			self.header.scan.x_pixels = (*pmp).scan.xrange
			self.header.scan.x_mm = (*pmp).scan.xsize * 0.001
;			self.header.scan.x = (*pmp).scan.xorigin
			if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
;			self.header.scan.x_name = (*pmp).scan.xname
			end
		1: begin
			self.header.scan.x_pixels = (*pmp).scan.yrange
			self.header.scan.x_mm = (*pmp).scan.ysize * 0.001
;			self.header.scan.x = (*pmp).scan.yorigin
			if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
;			self.header.scan.x_name = (*pmp).scan.yname
			end
		2: begin
			self.header.scan.x_pixels = (*pmp).scan.zrange
			self.header.scan.x_mm = (*pmp).scan.zsize * 0.001
;			self.header.scan.x = (*pmp).scan.zorigin
			if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
;			self.header.scan.x_name = (*pmp).scan.zname
			end
		3: begin
			self.header.scan.x_pixels = (*pmp).scan.urange
			self.header.scan.x_mm = (*pmp).scan.usize * 0.001
;			self.header.scan.x = (*pmp).scan.uorigin
			if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
;			self.header.scan.x_name = (*pmp).scan.uname
			end
		4: begin
			self.header.scan.x_pixels = (*pmp).scan.vrange
			self.header.scan.x_mm = (*pmp).scan.vsize * 0.001
;			self.header.scan.x = (*pmp).scan.vorigin
			if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
;			self.header.scan.x_name = (*pmp).scan.vname
			end
		5: begin
			self.header.scan.x_pixels = (*pmp).scan.wrange
			self.header.scan.x_mm = (*pmp).scan.wsize * 0.001
;			self.header.scan.x = (*pmp).scan.worigin
			if (*pmp).scan.xrange gt 0 then self.header.scan.on = 1
;			self.header.scan.x_name = (*pmp).scan.wname
			end
		else:
	endcase

	case self.sort_options.axis.y of
		0: begin
			self.header.scan.y_pixels = (*pmp).scan.xrange
			self.header.scan.y_mm = (*pmp).scan.xsize * 0.001
;			self.header.scan.y = (*pmp).scan.xorigin
			if (*pmp).scan.yrange gt 0 then self.header.scan.on = 1
;			self.header.scan.y_name = (*pmp).scan.xname
			end
		1: begin
			self.header.scan.y_pixels = (*pmp).scan.yrange
			self.header.scan.y_mm = (*pmp).scan.ysize * 0.001
;			self.header.scan.y = (*pmp).scan.yorigin
			if (*pmp).scan.yrange gt 0 then self.header.scan.on = 1
;			self.header.scan.y_name = (*pmp).scan.yname
			end
		2: begin
			self.header.scan.y_pixels = (*pmp).scan.zrange
			self.header.scan.y_mm = (*pmp).scan.zsize * 0.001
;			self.header.scan.y = (*pmp).scan.zorigin
			if (*pmp).scan.yrange gt 0 then self.header.scan.on = 1
;			self.header.scan.y_name = (*pmp).scan.zname
			end
		3: begin
			self.header.scan.y_pixels = (*pmp).scan.urange
			self.header.scan.y_mm = (*pmp).scan.usize * 0.001
;			self.header.scan.y = (*pmp).scan.uorigin
			if (*pmp).scan.yrange gt 0 then self.header.scan.on = 1
;			self.header.scan.y_name = (*pmp).scan.uname
			end
		4: begin
			self.header.scan.y_pixels = (*pmp).scan.vrange
			self.header.scan.y_mm = (*pmp).scan.vsize * 0.001
;			self.header.scan.y = (*pmp).scan.vorigin
			if (*pmp).scan.yrange gt 0 then self.header.scan.on = 1
;			self.header.scan.y_name = (*pmp).scan.vname
			end
		5: begin
			self.header.scan.y_pixels = (*pmp).scan.wrange
			self.header.scan.y_mm = (*pmp).scan.wsize * 0.001
			self.header.scan.y = (*pmp).scan.worigin
			if (*pmp).scan.yrange gt 0 then self.header.scan.on = 1
;			self.header.scan.y_name = (*pmp).scan.wname
			end
		else:
	endcase

;	Take care here as this 'origin' is the 6D x,y,z,u,v,w axes, where actually
;	the physical stage XYZ is always ZUV.

	self.header.scan.x = (*pmp).scan.zorigin
	self.header.scan.y = (*pmp).scan.uorigin

	self.header.title = (*pmp).comment
	self.header.sample = (*pmp).sample
	self.header.grain = (*pmp).grain

	self.header.metadata.sample_type = (*pmp).sample_type
	self.header.metadata.sample_serial = (*pmp).sample_serial
	self.header.metadata.detector_identity = (*pmp).detector_identity

	self.header.pileup.found = (*pmp).pileup.found
	self.header.pileup.on = (*pmp).pileup.on
	self.header.pileup.file = (*pmp).pileup.file
;	self.header.throttle.found = (*pmp).throttle.found
;	self.header.throttle.on = (*pmp).throttle.on
;	self.header.throttle.file = (*pmp).throttle.file

;	if (self.header.pileup.on eq 0) or (self.header.pileup.found eq 0) then self.header.pileup.file = ''
;	if (self.header.throttle.on eq 0) or (self.header.throttle.found eq 0) then self.header.throttle.file = ''

;	if ((*pmp).clock gt 0.) then self.clock = (*pmp).clock

	if ((*pmp).IC_sensitivity gt 0.) then self.header.sensitivity = (*pmp).IC_sensitivity * 1.e-6
	self.header.IC_name = (*pmp).IC_name
	self.header.detector[*] = -1

	self.header.scan.dwell = (*pmp).dwell
	self.header.energy = (*pmp).energy
	self.header.deadtime_cal = (*pmp).deadtime_cal

;	self.header.charge = (*pmp).charge
;	t = self.header.cal
;	struct_assign, (*pmp).cal, t
;	self.header.cal = t

	self.header.error = 0
	error = 0
	return
end

;-------------------------------------------------------------------

; This method is called after the 'get_header_info()' method
; has been called (e.g. when raw data files are first referenced) to update any
; internal device parameters if they also occur in the header.

pro daq_device::update_device_from_header, error=error

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
		warning,'daq_device::update_device_from_header',['IDL run-time error caught.', '', $
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

function daq_device::get_attribute_list, error=error

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
		warning,'daq_device::get_attribute_list',['IDL run-time error caught.', '', $
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
 
pro daq_device::flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
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
		warning,'daq_device::flux_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_daq_2, daq_swap,length,skip,x0,y0,z0,u0,v0,w0
common c_daq_3, monitor_array, n_monitor, daq_IC_name
common c_daq_4, n_times, time_last
;common c_daq_5, xanes_energies, do_xanes
common c_daq_7, daq_energy_pv
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_nsls_1, nsls_x_range, nsls_y_range, nsls_x, nsls_y
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
if n_elements(daq_IC_name) lt 1 then daq_IC_name=''

	if n_elements(first) lt 1 then first = 1
	if n_elements(suppress) lt 1 then suppress = 0
	if n_elements(image_mode) lt 1 then image_mode = 1
	if n_elements(nsls_debug) lt 1 then nsls_debug = 0
	
	PV_list = ''
	IC_name = 'DAQ:scaler.FC0'
	dwell = 0.
	no_pv = 1
	use_dwell = 0
	IC_val = 1.
	IC_vunit = 0.
	error = 1
	
	if self.header.sensitivity ne 0.0 then begin
		IC_val = charge_gain_units( self.header.sensitivity, units=IC_vunit)
		print,'DAQ:flux_scan: found sensitivity = ', self.header.sensitivity
	endif
	nsls_flux_scale = self.header.sensitivity
	error = 0
	return

;	on_ioerror, bad_io
;	if first then begin
;		err = init_daq_32()
;		if err then goto, bad_io
;
;		head = read_maia_32_header( unit, error=err32)
;
;		if image_mode then begin
;			dwell = head.dwell
;			use_dwell = 1
;		endif
;		
;		if err32 eq 0 then begin
;			daq = daq_defaults(source='daq_device::flux_scan', /any)		; read "DAQ.conf" default PVs
;
;;			Ion chamber PV selection options, or ring current, etc.
;
;			nq = 0
;			name = head.monitor.name
;			check_plist_daq, name
;;			val = head.monitor.val
;			if (n_elements(name) ge 1) and (daq.epics.npic ge 1) then begin
;				q = where_tokens( *daq.epics.pic, name, nq)
;				PV_list = name[q]
;				no_pv = 0
;			endif
;		endif
;
;;		Use /suppress to read header values and set some defaults
;
;		if suppress then begin
;
;			deadtime_cal = head.deadtime_cal	; deadtime cal read from blog DA_info (44) record
;			print,'daq_device::flux_scan: deadtime_cal = ',deadtime_cal
;			
;			if (abs(deadtime_cal.a - 0.1) gt 0.1) and (abs(deadtime_cal.a - 0.0) gt 0.1) then begin
;				self->set_options, deadtime_cal=deadtime_cal
;			endif
;			
;		endif else begin
;			daq_IC_name = ''					; in c_daq_3
;			nsls_flux_scale = 1.				; in c_nsls_?
;
;;				This section is ~nearly common between device_specific (10, 14, 16)
;;				and aps_to_list()
;
;			if err32 eq 0 then begin
;;				daq = daq_defaults(source='daq_device::flux_scan', /any)		; read "DAQ.conf" default PVs
;
;				a1_val = 0.0
;				a1_unit = 1.0
;				vals = [1., 2., 5., 10., 20., 50., 100., 200., 500.]
;				units = ['pA/V', 'nA/V', 'uA/V', 'mA/V']
;				vunit = [0.001, 1.0, 1000.0, 1000000.0]
;
;;				Ion chamber PV selection options, or ring current, etc.
;
;				val = head.monitor.val
;;				nq = 0
;;				name = head.monitor.name
;;				check_plist_daq, name
;;				if (n_elements(name) ge 1) and (daq.epics.npic ge 1) then begin
;;					q = where_tokens( *daq.epics.pic, name, nq)
;;				endIF
;	
;;				Ion chamber preamp sensitivity value
;
;				q2 = where_tokens( ['sens_num.VAL','sensitivity'], name, nq2)
;	
;;				Ion chamber preamp sensitivity units
;
;				q3 = where_tokens( ['sens_unit.VAL','sensitivity'], name, nq3)
;	
;;				Live-time, dead-time handled elsewhere ...
;
;				nq4 = 0
;				if nq4 ne 0 then begin					; deadtime disabled in pop-up below ...
;					times = name[q4]
;				endif else begin
;					times = ['none found']
;				endelse
;			
;;				Need to disable dwell in pop-up in spec_evt mode.
;;				Leave it UNDEFINED if not in map mode.
;
;				if (q[0] ne -1) then begin
;					if (q2[0] ne -1) and (q3[0] ne -1) then begin
;						select = generic_flux_select( group, name[q], name[q2], name[q3], times, $
;									error=dud, dwell=dwell, dead=0)
;					endif else begin
;						select = generic_flux_select( group, name[q], string(vals), units, times, $
;									error=dud, dwell=dwell, dead=0)
;					endelse
;;					PV_list = name[q]
;;					no_pv = 0
;				endif else dud=1
;				
;				if dud then begin
;					daq_IC_name = ''			; in c_daq_3
;					nsls_flux_scale = 1.		; in c_nsls_?
;				endif else begin
;					daq_IC_name = name[q[select.flux]]
;					IC_name = daq_IC_name
;					if (q2[0] ne -1) and (q3[0] ne -1) then begin
;						a1_val = float(val[q2[select.sense_num]])
;						u = val[q3[select.sense_unit]]
;						q2b = where( strlowcase(u) eq strlowcase(units))
;						a1_unit = 1.
;						if q2b[0] ne -1 then a1_unit = vunit[q2b]
;					endif else begin
;						a1_val = vals[ select.sense_num ]
;						a1_unit = vunit[ select.sense_unit ]
;					endelse
;					IC_val = a1_val
;					IC_vunit = a1_unit
;					nsls_flux_scale = a1_val * a1_unit							; scaling of counts for nA/V units
;	
;					if image_mode then begin
;						nsls_flux_scale = nsls_flux_scale * select.dwell*0.001			; scale by dwell only for maps
;					endif
;				endelse
;			endif
;			print,' daq_device::flux_scan: daq_IC_name = ',daq_IC_name,', nsls_flux_scale = ', nsls_flux_scale
;		endelse
;	endif
;	error = 0
;	return

bad_io:
	warning,'daq_device::flux_scan','blog file I/O error.'
	error=1
	return
end

;-------------------------------------------------------------------

pro daq_device::check_pv_list, plist

; Check the PV list pointed to by 'plist'. Add any DAQ default PVs
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
		warning,'daq_device::check_pv_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	check_plist_daq, plist
	return
end

;-------------------------------------------------------------------

; Build a Y lookup table file
 
function daq_device::build_ylut, file, output=output, error=error, force=force

; Build a Y lookup table file that lists the first Y value
; for each list-mode data file.
; Use the 'output' file path by default, else base it on the input 'file'.

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
		warning,'daq_device::build_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
if n_elements(force) eq 0 then force=0

	if force then goto, build
	ylut = self->get_ylut( file[0], output=output, /strip, error=error)
	if error eq 0 then return, ylut
	
build:	
	ylut = build_daq_ylut( file[0], output=output, error=error)
	
	if error eq 0 then begin
		*self.header.scan.pYlut = ylut
	endif
	return, ylut
end

;-------------------------------------------------------------------

function daq_device::get_ylut, file, strip=strip, output=output, error=error

; Read a Y lookup table file that lists the first Y value
; for each list-mode data file.
; Use the 'output' file path by default, else base it on the input 'file'.

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
		warning,'daq_device::get_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif

	ylut = get_daq_ylut( file, strip=strip, output=output, error=error)
	
	if error eq 0 then begin
		*self.header.scan.pYlut = ylut
	endif
	return, ylut
end

;-------------------------------------------------------------------

function daq_device::get_dwell, error=error

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
		warning,'daq_device::get_dwell',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_daq_13, daq_dwell

	error = 0
	return, daq_dwell

;	if ptr_good( self.dwell) then begin
;		error = 0
;		return, *self.dwell
;	endif
	
	error = 1
	return, 0.0
end

;-------------------------------------------------------------------

function daq_device::range_ylut, files, error=error

; Return the range of Y values sampled by this group of 'files'.

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
		warning,'daq_device::range_ylut',['IDL run-time error caught.', '', $
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
 
pro daq_device::write_ylut, ylut, file, output=output, strip=strip, error=error

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
		warning,'daq_device::write_ylut',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	write_daq_ylut, ylut, file, output=output, strip=strip, error=error
	return
end

;-------------------------------------------------------------------

; Trim EVT file list for Y lookup table
 
function daq_device::trim_evt_files, files, mask=pmask, pYlut=pYlut, $
								yoffset=yoffset, yrange=yrange

; Trim the list of 'files' to only include files needed for the Y range
; and offset selected, or as seen in the region mask arrays.
;
; yoffset	Y offset (not compressed)
; yrange	Y range (not compressed)
; pmask		ptr to region mask ptr array
; pYlut		pointer to Y LUT for blog data files
;			Y LUT for DAQ is a list of first Y value for each blog data file

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
		warning,'daq_device::trim_evt_files',['IDL run-time error caught.', '', $
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
	jlast = long(extract_extension(files[n-1]))
	if (jlast eq 0) and (n gt 1) then goto, bad_table	; catch zero as last entry in table' error
	if (jlast lt 0) or (jlast ge np) then goto, bad_index
	ny1 = (*pYlut)[jlast]
	if (ny1 lt 0) then goto, bad_index
	if ny1 le top0 then begin
		yfiles[ny1:top0] = 1
		itag[ny1:top0] = n-1								; extend last to top
	endif
	for i=n-2,0,-1 do begin
		j = long(extract_extension(files[i]))
		ny1 = (*pYlut)[j]
		ny2 = (*pYlut)[jlast] < top0
		if ny2 ge ny1 then begin
			yfiles[ny1:ny2] = 1
			itag[ny1:ny2] = i			; index to files[]
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
		fmask[ itag2[q[i]] : itag2[ (q[i]+1) < top ] ] = 1	; flags file indices needed
	endfor
    
	q = where(fmask eq 1, nq)
	if nq eq 0 then goto, bad_files
    
	return, files[q]
    
bad_files:
	warning,'daq_device::trim_evt_files','No DAQ files contain the selected region(s).'
	return, ''
bad_index:
	warning,'daq_device::trim_evt_files',['Bad file index.','','Check that data files exist,', $
			'and names are valid DAQ format.','Or, no files contain these region Y values.']
	return, ''
bad_table:
	warning,'daq_device::trim_evt_files',['Bad YLUT lookup table for files: '+files[0]+' ...', $
			'Delete the old YLUT file, and it will be regenerated.']
	return, ''
bad:
	return, files
end

;-------------------------------------------------------------------

; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.

function daq_device::read_setup, unit, xrange,yrange, first=first, $
			n_guide,progress_file, charge=charge, ecompress=ecompress, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			suppress=suppress, ic=flux_ic, x_coords=x_coords, $
			y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units

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
;	
;   flux		flux array (matches image dimensions for scan), accumulated here or in read_buffer
;   			extra planes are for added 'attributes', as defined by 'get_attributes()' method.
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
		warning,'daq_device::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_daq_2, daq_swap,length,skip,x0,y0,z0,u0,v0,w0
common c_daq_3, daq_IC_name
common c_daq_11, daq_hw_scaler, daq_fixed_dwell
common c_daq_12, daq_flux_mode
common c_daq_13, daq_dwell
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_nsls_1, nsls_x_range, nsls_y_range, nsls_x, nsls_y
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc

		if first then begin
			err = init_daq_32()
			if err then goto, bad_io

			daq = daq_defaults(source='daq_device::read_setup', /any)	; read "DAQ.conf" default PVs
;			daq_energy_pv = daq.epics.energy
;			if daq_energy_pv eq '' then daq_energy_pv='ENERGY'
;			gprint,level=2,' device_specific: daq_energy_pv = ',daq_energy_pv
			
			self.spectrum_mode = 1
			if (n_elements(flux) ge 2) then self.spectrum_mode=0
			
;	Hardware counters "DAQ:scaler.FC0" and "DAQ.scaler.FC1" are already counts in a pixel. The scale needed
;	for these is just sensitivity val*unit. For Epics PV's in c/s we need to scale by dwell time (s).

			daq_IC_name = flux_ic.pv
			daq_hw_scaler = 0
			daq_flux_mode = 0
			if strmid(daq_IC_name,0,strlen('DAQ:scaler')) eq 'DAQ:scaler' then begin
				daq_hw_scaler = 1
				daq_flux_mode = 1
				if daq_IC_name eq 'DAQ:scaler.FC1' then daq_flux_mode=2
			endif
			nsls_flux_scale = flux_ic.val * flux_ic.unit
			if (daq_hw_scaler eq 0) and (self.spectrum_mode eq 0) then begin
				nsls_flux_scale = nsls_flux_scale * flux_ic.dwell*0.001		; scale by dwell only for maps
			endif
			daq_fixed_dwell = flux_ic.dwell
			gprint,level=2,' daq_device::read_setup: daq_IC_name = ',daq_IC_name,', nsls_flux_scale = ', nsls_flux_scale

;	Set-up an internal buffer to hold a dwell array, and a total elapsed time

			if self.spectrum_mode then begin
				daq_dwell = flux[0]
			endif else begin
				daq_dwell = flux[*,*,0]									; to replicate size and type as flux
			endelse														; should dwell be zeroed here, or elsewhere?
			daq_dwell[*] = 0.0											; zero only for 'first'
			self.dwell_total = 0.0
			
			daq_swap = (big_endian() ne self->big_endian())

;	# buffers to read before Progress = 100,000/n_guide > 1
;	n_buffer MUST be divisable by 4 for Fortran code
;	Take care increasing n_buffer, as the daq_tt sum in spectrum mode will
;	get large. Absolute limit is about 2M bytes.

			n_buffer = 500000L				; ~500K byte buffer (0.5 MBytes)
			n_guide = 50000L				; progress every 10th buffer (5 Mb)
			nsls_IC = 0.0					; in c_nsls_1

			x0 = 0							; in c_daq_2
			y0 = 0
			z0 = 0
			u0 = 0
			v0 = 0
			w0 = 0
			
;	For spec_evt scan for files, 'xrange' set to 16384 in 'spectrum_new_load' to force header read here.
;	For DA_evt scans, this header call is not used. Instead, Monitor records are scanned in 'read_buffer'.

;			head = read_daq_32_header( unit, /no_scan, error=err32)
;			if err32 eq 0 then begin
;				if xrange eq 16384 then begin
;					if head.scan.xrange gt 1 then begin
;						xrange = head.scan.xrange
;					endif else if head.limits.x.max gt 1 then begin
;						xrange = head.limits.x.max
;					endif
;				endif
;;				beam_energy = head.energy
;			endif
			nsls_x_range = long(xrange)
			nsls_y_range = long(yrange)
			
			event_array = bytarr(n_buffer)
		endif
		
		ibranch = 0							; in c_sandia_6
		tag = 0								; in c_sandia_7
		length = 0L							; in c_daq_2
		skip = 0L							; in c_daq_2
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

function daq_device::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
		station_e=ste, time=t, veto=veto, ecompress=ecompress, multiple=multiple, $
		xoffset=xoffset, yoffset=yoffset, title=title, file=file, error=error, $
		flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
		total_processed=processed, processed=count1, valid=good, total_bad_xy=bad_xy, raw_xy=raw_xy, $
		by_odd=by_odd, by_even=by_even, support_even_odd=support_even_odd

;   Device specific list-mode (event-by-event) data file reading routine.
;   Remember, channel starts at 0 (-1 means any channel/ADC).
;
; input:
;   unit		read unit number
;   file		filename passed for multi-file XY checking
;   channel_on	desired ADC channel(s) (these start at zero, after an optional offset)
;   xcompress	desired X axis compression
;   ycompress	desired Y axis compression
;   ecompress	desired E axis compression (needs to match DA energy calibration)
;   xoffset		offset X by this (i.e. subtract this) - in pre-compress units
;   yoffset		offset Y by this
;   /raw_xy		suppresses X,Y compression and offset
;   flux		optional array that comes in to be updated with pixel flux
;   			extra planes are for added 'attributes', as defined by 'get_attributes()' method.
;   dead_fraction for some this is an array that comes in to be updated with pixel dead_fraction
;				If flux is already DT corrected, "live" flux, then set dead-fraction zero.
;			
;	/by_odd		only for odd rows
;	/by_even	only for even rows
;
; return:
;   e			energy vector (uintarr) returned
;   t			Time-over-threshold, for some DAQ channels (e.g. 0-31)
;   x1			X vector (uintarr) return
;   y1			Y vector (uintarr) return
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
		warning,'daq_device::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_daq_2, daq_swap,length,skip,x0,y0,z0,u0,v0,w0
common c_daq_3, daq_IC_name
common c_maia_6, maia_y_min
common c_daq_13, daq_dwell
common c_daq_15, daq_fx, daq_tags, daq_pseudo
common c_daq_16, daq_0, daq_1, daq_2, daq_3, daq_4, daq_5, daq_e, daq_t, daq_ste, daq_veto
;common c_daq_18, daq_time
common c_daq_11, daq_hw_scaler, daq_fixed_dwell
common c_daq_12, daq_flux_mode
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_nsls_1, nsls_x_range, nsls_y_range, nsls_x, nsls_y
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_nsls_9, nsls_debug
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc

	support_even_odd = 1
	if n_elements(by_odd) lt 1 then by_odd=0
	if n_elements(by_even) lt 1 then by_even=0
	if by_odd then by_even=0

	on_ioerror, bad_io
	nc = n_elements(channel_on)

	read_event_buffer, unit, self, n_actual, bytes=1
;	print,'i_buffer = ',i_buffer, ' n_actual=',n_actual
	if n_actual eq 0 then begin
		gprint,level=2,'daq_device::read_buffer: bad n_actual'
		goto, bad_io
	endif
	i_buffer = i_buffer+1
	
	if n_elements(daq_0) eq 0 then begin					; just assign these 
		n_events = n_buffer/4								; vectors first time
		daq_e = uintarr(n_events, /nozero)
		daq_t = uintarr(n_events, /nozero)					; e,t per event
		daq_0 = intarr(n_events, /nozero)					; x,y,z per event (unlike many devices, we will
		daq_1 = intarr(n_events, /nozero)					; accept negative x,y,z,u,v,w)
		daq_2 = intarr(n_events, /nozero)
		daq_3 = intarr(n_events, /nozero)
		daq_4 = intarr(n_events, /nozero)
		daq_5 = intarr(n_events, /nozero)
;		daq_time = ulonarr(n_events, /nozero)				; time stamp per event
		daq_ste = uintarr(n_events, /nozero)				; 'station', i.e. detector #
		daq_veto = uintarr(n_events, /nozero)				; flag a rejected event
		daq_tags = uintarr(n_events, /nozero)				; can be used to retrieve debug event info
		daq_pseudo = uintarr(n_events, /nozero)				; pseudo event (i.e. BT, FC0, FC1 in records added as events)
		n_fx = 4
		daq_fx = fltarr(n_fx,n_events, /nozero)				; selected flux (Epics or h/w), FC0, FC1, BT for each event
	endif else begin
		n_events = n_elements(daq_e)
		n_fx = n_elements(daq_fx[*,0])
	endelse
	n = 0L
	nt = 0L
	debug = 0L
	gprint, level=1, 'daq_device::read_buffer: daq_swap = ', daq_swap
;	nsls_y = fix( nsls_y > yoffset)

;	Veto=0	real events returned in e,t,x1,y1,ste vectors.
;	Veto=1	pseudo events (flux and dwell) values.
;	'n' returns high water mark in vectors, total number of events.
;	daq_x_axis, daq_y_axis selects axes indices (0-5) from: 
;			DAC dX, dY, and stage sX, sY, sZ, sA
		
	err = daq_36_events( event_array,n_actual, channel_on,nc, $
			daq_e,daq_t,daq_0,daq_1,daq_2,daq_3,daq_4,daq_5,daq_ste,daq_veto,daq_tags,n_events,n, $
			daq_fx,n_fx, daq_flux_mode, x0,y0,z0,u0,v0,w0, $
			ibranch,daq_swap, tag,length,skip, bad_xy, debug )

	nsls_debug = debug
	if err ne 0 then begin
		gprint,level=2,'daq_device::read_buffer: error (',err,') return from daq_384_events4'
		if bad_xy then gprint,level=2,'		... and bad xy'
		goto, bad_io
	endif

;	beam_energy = daq_last_energy
;	if beam_energy gt 500. then beam_energy = beam_energy/1000.
;	if (daq_first_time ne 0.0D+0) and (daq_first_time lt time_last) then time_last = daq_first_time
		
	if n eq 0 then begin
		gprint,level=2,'daq_device::read_buffer: Zero "n" return from daq_384_events4'
		return, 0
	endif

;	Do the redirection from axes 012345 to XYZ here. This is quite free, except for the YLUT action.
;	The code in da_evt, etc. is really geared towards YLUT working with real Y (Yoffset, Yrange3, etc.).
;	Hence, for now, we can ONLY have YLUT (mdaq2_y_min) working with Y. This means for cluster processing
;	we can only have full 3D modes such as: EXY, XEY, theta-XY, X-theta-Y (theta-YX will not work).

	case self.sort_options.axis.x of
		0: x1 = daq_0[0:n-1]
		1: x1 = daq_1[0:n-1]
		2: x1 = daq_2[0:n-1]
		3: x1 = daq_3[0:n-1]
		4: x1 = daq_4[0:n-1]
		5: x1 = daq_5[0:n-1]
	endcase
	case self.sort_options.axis.y of
		0: y1 = daq_0[0:n-1]
		1: y1 = daq_1[0:n-1]
		2: y1 = daq_2[0:n-1]
		3: y1 = daq_3[0:n-1]
		4: y1 = daq_4[0:n-1]
		5: y1 = daq_5[0:n-1]
	endcase
;	case self.sort_options.slow_axis of						; min value for slow axis for "YLUT"
;		0: daq_y_min = min(daq_0[0:n-1])
;		1: daq_y_min = min(daq_1[0:n-1])
;		2: daq_y_min = min(daq_2[0:n-1])
;		3: daq_y_min = min(daq_3[0:n-1])
;		4: daq_y_min = min(daq_4[0:n-1])
;		5: daq_y_min = min(daq_5[0:n-1])
;	endcase
	daq_y_min = 0

	e = daq_e[0:n-1]
	t = daq_t[0:n-1]
;	time = daq_time[0:n-1]
	ste = daq_ste[0:n-1]
	veto = daq_veto[0:n-1]
		
;	n		high water mark in event vector buffers
;	good	number of valid events, so far

	q = where( veto eq 0, good, complement=qveto, ncomplement=n_veto)	; good (and pseudo) events
	daq_pseudo[*] = 0
	if n_veto gt 0 then daq_pseudo[qveto] = 1

;	To select only odd rows, veto even ones, and vica versa).
;	But take care to ignore the pseudo events (veto=1).

	if good gt 0 then begin
		if support_even_odd and (by_odd or by_even) then begin		; only include odd or even lines
			yodd = by_odd ? 0 : 1
			q1 = where( (2*(y1/2) eq y1-yodd) and (veto eq 0), nq1)
			if nq1 gt 0 then veto[q1] = 1
		endif
	endif
	q = where( veto eq 0, good)

;	Compress all, not just good events, so that pseudo events get corrected too ...
		
	xoffset2 = xoffset
	yoffset2 = yoffset
	if raw_xy eq 0 then begin
		e = e / uint(ecompress)
		x1 = x1 / xcompress
		y1 = y1 / ycompress
		xoffset2 = xoffset2 / xcompress
		yoffset2 = yoffset2 / ycompress
	endif

	xrange2 = long( nsls_x_range / xcompress)
	yrange2 = long( nsls_y_range / ycompress)

	if (self.spectrum_mode eq 0) then begin						; images

;		This scales the count with compression for PV rates. H/w counters use the accumulation approach.
;		Ignore border pixel data for mapping (where flux is not scalar) ...
;		Offset increment into 'flux' by (compressed) offset for this stripe.
;		Combine all detectors, so divide DTcal by the number active 'nqo'.
;		Do NOT divide by average dwell, as the divide by dwell will happen in 'da_evt' later.

		xmin = (self.sort_options.clear.x ? 1 : 0)				; optionally clear border pixels
		ymin = (self.sort_options.clear.y ? 1 : 0)
		qo = where( channel_on eq 1, nqo)
		dta = 1.0e-6 * self.sort_options.deadtime_cal.a / float(nqo)
		dtb = 1.0e-6 * self.sort_options.deadtime_cal.b / float(nqo)			
		dtc = 1.0e-6 * self.sort_options.deadtime_nim / float(nqo)	

;		Test border, and offset all, not just good events, so that pseudo events get offset ...
		
		q1 = where( (x1 lt xmin) or (y1 lt ymin) or (x1 gt (xrange2-1-xmin)) or (y1 gt (yrange2-1-ymin)), nq1)
		if nq1 gt 0 then begin
			veto[q1] = 1						; veto border events
			daq_pseudo[q1] = 0					; also do not include flux, etc. for border
		endif
		q = where( veto eq 0, good)
		
		x1 = uint(x1 - xoffset2)
		y1 = uint(y1 - yoffset2)
			
;		Accumulate flux (including attributes), dead_fraction and dwell ...
;		Do not ignore veto 'events' (these may be pseudo events). Use flux_mode to select action:
;			0	was Epics PV as a rate, so set the flux in a pixel to daq_fx[0,*], if it is not zero
;					scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
;			1	H/W flux counter used and accumulated in daq_fx[0,*] in Fortran, so accumulate daq_fx[0,*] 
;					here in flux[*,*,0] scaled by: nsls_flux_scale
;			
;				Use FC in daq_fx[1,*] to accumulate into flux[*,*,1]
;		
;		Dwell:
;			Use daq_fx[3,*] (if not zero) to set Dwell in daq_dwell (can pass by ref, better than self.dwell)
;
;			Dead_fraction:
;			Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
;			Use fx[4,*] to set DT in dead_fraction, weighted by 1/nqo in imaging case (see above).
;			Later (in da_evt) norm this using the dead_fraction_norm(/image) method, which returns dwell (ms) array.
		 				
	 	daq_accumulate_dtfx, /image, t,x1,y1,ste,veto,daq_fx,n,n_fx, daq_flux_mode, nsls_flux_scale, $
	 				n_elements(flux[*,0,0]),n_elements(flux[0,*,0]),n_elements(flux[0,0,*]), dta,dtb,dtc, dead_fraction, pseudo=daq_pseudo, $
	 				flux=flux, dwell=daq_dwell, xcompress=xcompress,ycompress=ycompress, error=err
		 				
	endif else begin										; spectra

;		Flux needs to be summed for h/w FC counters (flux_mode >0), and set times for Epics (flux_mode=0).
;		Set the time-stamp 'time_PV' for pseudo events to Monitor time.
;	
;		Accumulate 't' into dead_fraction (across detectors), using cal to make it in ms units.
;		This time use the veto array.
;		Later (in spec_evt) norm this using the get_total_time method, which returns total time (ms).

;		Offset all, not just good events, so that pseudo events get offset ...
		
		if raw_xy eq 0 then begin
			x1 = uint(x1 - xoffset2)			; assumes offsets are zero in total spectrum mode
			y1 = uint(y1 - yoffset2)			; offsets used in region spectra extract mode
		endif else begin
			x1 = uint(x1)
			y1 = uint(y1)
		endelse
			
;		Detectors returned individually, so do not divide by the number active.
	
		dta = 1.0e-6 * self.sort_options.deadtime_cal.a 		; ms
		dtb = 1.0e-6 * self.sort_options.deadtime_cal.b	
		dtc = 1.0e-6 * self.sort_options.deadtime_nim
	
;		Dwell:
;			Use maia_fx[3,*] (if not zero) to accumulate total Dwell in self.dwell_total.
;
;		Dead_fraction:
;			Accumulate 't' into dead_fraction (detector array), using cal to make it in ms units.
;			Later (in spec_evt) norm this using the get_total_time() method, which returns the total dwell (ms).
;
;		Remember to NOT skip veto events, some will be pseudo events with valid BT, FC0, FC1

		if daq_hw_scaler then begin								; flux is NOT suitable for Regions
			flux = flux + nsls_flux_scale * total(daq_fx[0,0:n-1])
			dt = total(daq_fx[3,0:n-1])							; time (ms)
		endif else begin
;			dt = ((daq_last_time - time_last) > 0) * 1000.		; dt (ms)
;			flux = flux + nsls_flux_scale * daq_last_flux * dt * 0.001
			dt = 0.	
		endelse

		daq_accumulate_dtfx, /spectrum, t,x1,y1,ste,veto,daq_fx,n,n_fx, daq_flux_mode, nsls_flux_scale, $
			n_elements(dead_fraction[*,0]),1,1, dta,dtb,dtc, dead_fraction, error=err
			
		self.dwell_total = self.dwell_total + dt				; ms
;		time_last = daq_last_time
	endelse

;	Scale 't' for NIM channels so that we can use the same 'cal.a' for all channels beyond this point ...
;	Why? All DT has been done here already. What use is there for NIM T beyond here?

;	qnim = where( ste[q] ge 32, nqnim)
;	if nqnim gt 0 then begin
;		dt_scale_nim = dtc / dta
;		max16 = (64. * 1024.) - 1.1 
;		t[q[qnim]] = uint( clip(t[q[qnim]] * dt_scale_nim, 0, max16))
;	endif
	processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function daq_device::import_spec, name, file, group=group

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
		warning,'daq_device::get_import_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'daq_evt': begin							; 9
			warning,'daq_device::import_spec',['"daq_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function daq_device::get_import_list, error=error

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
		warning,'daq_device::get_import_list',['IDL run-time error caught.', '', $
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
;		device_name:	'', $			; associated device object name (filled in below)
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
	
	opt_39 = define(/import)			; DAQ 36 list-mode
		opt_39.name =		'daq_evt'	; unique name of import
		opt_39.title =		'Extract from DAQ-36 - blog files'
		opt_39.in_ext =		''			; input file extension
		opt_39.request =	'DAQ blog data scan for all spectra [Enter FIRST file or range]'
		opt_39.preview =	0			; allow spectrum preview
		opt_39.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_39.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_39.multifile =	1			; denotes data in a series of more than one file
		opt_39.separate =	'.'			; char between file and run #
		opt_39.use_pileup =	1			; request pileup file
		opt_39.use_throttle = 0			; request throttle file
		opt_39.use_IC =		1			; pop-up the flux_select PV selection panel (assume charge collected)
		opt_39.IC_mode = 	1			; default to using PV for IC
		opt_39.use_tot =	1			; collect ToT data too
		opt_39.xr =			16384		; default X range (this value forces header read)
		opt_39.yr =			16384		; default Y range
	
	opt = [opt_39]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function daq_device::init

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
		warning,'daq_device::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	self.pileup.use = 1									; DAQ specfic items
;	self.throttle.use = 1
;	self.linear.use = 1
	self.header.sensitivity = 0.1e-6					; default sensitivity is 0.1 pC/count
	
;	self.header.scan.use_ylut = 1						; DAQ device can use a Y lookup table
;	self.header.scan.pYlut = ptr_new(/allocate_heap)	; allocate heap for Y LUT
	
; Sort Option items appear as Scan setup options in the Sort EVT "Scan" tab

	self.options.scan.on = 1		; scan sort options available for this class
									; these must be set-up in the render_options method
;	self.options.scan.ysize = 100	; Y size of sort options box, when open (no NIM DT)
	self.options.scan.ysize = 136	; Y size of sort options box, when open

; Set default Sort Options local parameters ...

	self.sort_options.axis.x = 0					; set default X axis index
	self.sort_options.axis.y = 1					; set default Y axis index
;	self.sort_options.clear.x = 1					; clear X margins by default
;	self.sort_options.clear.y = 1					; clear Y margins by default
	self.sort_options.deadtime_cal.a = 0.0			; deadtime calibration (slope)
	self.sort_options.deadtime_cal.b = 0.0			; deadtime cal (offset)
	self.sort_options.deadtime_nim = 100.0		; deadtime cal (offset)

; Initial heap allocation for sort_id widget vector pointers ...

	self.sort_id.x_axis = ptr_new(/allocate_heap)				; Axis X ID array pointer
	self.sort_id.y_axis = ptr_new(/allocate_heap)				; Axis Y ID array pointer
	self.sort_id.clear_border = ptr_new(/allocate_heap)			; Clear X,Y border check-box ID array pointer
	self.sort_id.deadtime_cal_a = ptr_new(/allocate_heap)		; Deadtime Cal A text ID array pointer
	self.sort_id.deadtime_cal_b = ptr_new(/allocate_heap)		; Deadtime Cal B text ID array pointer
	self.sort_id.deadtime_nim = ptr_new(/allocate_heap)		; Deadtime Cal NIM text ID array pointer
	
;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"DAQ_DEVICE" --> DAQ_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'DAQ_DEVICE', $	; unique name for this device object
		title = 'CSIRO DAQ 36 - HYMOD data acquisition.', $
		ext = '', $				; not a fixed file extension for blog data
		multi_files = 1, $		; multiple segment files per run
		multi_char = '.', $		; separates run from segment number in file name
		big_endian = 1, $		; blog data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 0, $		; start detector ADC #'s at 0
		use_bounds = 0, $		; not confine charge/flux within bounded area
		synchrotron = 0, $		; both synchrotron data
		ionbeam = 1, $			; and ion-beam data
		use_cluster = 0)		; uses cluster parallel processing
	return, i
end

;-------------------------------------------------------------------

pro daq_device__define

; Define DAQ device object internal data structure.
; This adds some internal device parameters, local to DAQ in 'sort_options',
; and device specific sort widgets options to be displayed in the 'Scan' tab
; of the Sort window in 'sort_id'.
; Only called using obj = obj_new('DAQ_DEVICE')

COMPILE_OPT STRICTARR

daq = {DAQ_DEVICE,  $

		INHERITS BASE_DEVICE, $								; mandatory base device parameters

		sort_options : {sort_options_daq32, $				; Sort EVT window Sort options panel
				clear: {clear_devicespec,	x:	0, $		; flags clear X borders in sort
											y:	0}, $		; flags clear Y borders in sort
				axis: {axis_devicespec,		x:	0, $		; X axis index (0-5)
											y:	1}, $		; Y axis index
				deadtime_cal: {sort_options_deadtime_devicespec, $
											a:	0.0, $		; deadtime calibration (slope) for SCEPTER channels (0-31)
											b:	0.0 }, $	; deadtime cal (offset)
				deadtime_nim:	0.0 }, $					; deadtime cal for NIM channels (32-35)

		sort_id: {sort_id_daq32, $							; pointers to vector of sort widget IDs
				x_axis:						ptr_new(), $	; X axis index widget ID array pointer
				y_axis:						ptr_new(), $	; Y axis index widget ID array pointer
				clear_border:				ptr_new(), $	; Clear X,Y border check-box ID array pointer
				deadtime_cal_a:				ptr_new(), $	; Deadtime Cal A text ID array pointer
				deadtime_cal_b:				ptr_new(), $	; Deadtime Cal A text ID array pointer
				deadtime_nim:				ptr_new()} $	; Deadtime Cal NIM text ID array pointer
				
;		dwell: 				ptr_new() $						; pointer to dwell image array	
	}
	return
end
