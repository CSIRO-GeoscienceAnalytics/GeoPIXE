;
; GeoPIXE Device Object for Fastcom MPA3 system data as used at Sandia
;
; MPA3 is a 4-16 channel data acquisition and scanning
; system developed by FastComTec and adapted by Sandia for general nuclear microprobe imaging.
; Data is written by a PC processor in little-endian byte order.
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
; use of a Y lookup table, which can be flagged this way (not used in Fastcom_MPA3 as yet):
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

pro FASTCOM_MPA3_DEVICE::cleanup

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
		warning,'FASTCOM_MPA3_DEVICE::cleanup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_valid( self.sort_id.x_adc) then ptr_free, self.sort_id.x_adc
	if ptr_valid( self.sort_id.y_adc) then ptr_free, self.sort_id.y_adc

	self->BASE_DEVICE::cleanup
    return
end

;-------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window and elsewhere. These are rendered in this class, using the method
; "render_options" and the parameters read/written FROM DISK using the "read_options",
; "write_options" methods.
;
;	options		return current option values

pro fastcom_mpa3_device::read_options, unit, options=options, error=error

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
		warning,'fastcom_mpa3_device::read_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

; Get current defaults in this device

	options = self->get_options()
	x_adc = options.x_adc
	y_adc = options.y_adc
	version = 0L
	
; Read options parameters from the file

	on_ioerror, bad_io
	if self.use_version then readu, unit, version
	
	if self.use_version then begin
		if version le -1 then begin
			readu, unit, x_adc, y_adc
		endif
	endif
	
; Write back these options arameters to the device ...

	options.x_adc = x_adc
	options.y_adc = y_adc
	
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

pro fastcom_mpa3_device::write_options, unit, options=p, error=error

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
		warning,'fastcom_mpa3_device::write_options',['IDL run-time error caught.', '', $
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
	
	version = -1L
	
	on_ioerror, bad_io
	writeu, unit, version
	
	writeu, unit, options.x_adc, options.y_adc
	error = 0
	return
	
bad_io:
	error = 1
	return
end

;-------------------------------------------------------------------------------

; Return a string to display (e.g. in Image History window) to show the
; state of this device's options. If 'p' present, then show this parameter set.

function fastcom_mpa3_device::options_legend, p

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
		warning,'fastcom_mpa3_device::read_options',['IDL run-time error caught.', '', $
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

	list = ['Fastcom_MPA3:' ] 
	list = [list, '   X ADC: ' + str_tidy(options.x_adc) + ', Y ADC: ' + str_tidy(options.y_adc) ]

	return, list
end

;-------------------------------------------------------------------------------
; These routines are associated with the rendering of Sort Options widgets
; in the Sort EVT window Sort tab options box.
;-------------------------------------------------------------------------------

pro OnRealize_fastcom_mpa3_device_sort_option_x_adc, wWidget

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
		warning,'OnRealize_fastcom_mpa3_device_sort_option_x_adc',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_fastcom_mpa3_device_sort_option_x_adc: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_fastcom_mpa3_device_sort_option_x_adc: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.x_adc
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_fastcom_mpa3_device_sort_option_y_adc, wWidget

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
		warning,'OnRealize_fastcom_mpa3_device_sort_option_y_adc',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ObjBase = find_id( wWidget, uname='obj-ref-here')
	if widget_info(ObjBase, /valid) eq 0L then begin
		print,'OnRealize_fastcom_mpa3_device_sort_option_y_adc: Object base not found.'
		return
	endif
	widget_control, ObjBase, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'OnRealize_fastcom_mpa3_device_sort_option_y_adc: Device Object ref not found.'
		return
	endif

	options = obj->get_options()
	
	widget_control, wWidget, set_combobox_select=options.y_adc
	return
end

;------------------------------------------------------------------------------------------

function fastcom_mpa3_device_sort_option_event, event

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
		warning,'fastcom_mpa3_device_sort_option_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	if widget_info( event.handler, /valid) eq 0L then begin
;		print,'fastcom_mpa3_device_sort_option_event: event.handler not valid.'
		return, 0
	endif
	uname = widget_info( event.handler, /uname)
	if uname ne 'obj-ref-here' then begin
		print,'fastcom_mpa3_device_sort_option_event: Object base not found.'
		return, 0
	endif
	widget_control, event.handler, get_uvalue=obj
	if obj_valid(obj) eq 0L then begin
		print,'fastcom_mpa3_device_sort_option_event: Device Object ref not found.'
		return, 0
	endif

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		return, event						; pass context help up the line ...
		end
	else: begin
		
		uname = widget_info( event.id, /uname)
		case uname of
			'Fastcom_MPA3-x-adc': begin
				obj->set_options, x_adc = event.index
				end
			'Fastcom_MPA3-y-adc': begin
				obj->set_options, y_adc = event.index
				end
		endcase		
		end
endcase
return, 0L
end

;------------------------------------------------------------------------------------------

; Render options widgets in Sort Options box in Sort EVT window.
; Parent is the framed container box. Its child must be a base that
; all options widgets are attached to. This child is target of destroy
; when switching devices.

pro fastcom_mpa3_device::render_options, parent

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
		warning,'fastcom_mpa3_device::render_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

case !version.os_family of
	'MacOS': begin
		xadc_xsize = 50
		end
	'unix': begin
		xadc_xsize = 50
		end
	else: begin
		xadc_xsize = 50
		end
endcase

; Call super-class to cleanup old display and set Y size of box first ...

self->BASE_DEVICE::render_options, parent

; The following will appear in the Options box on the Sort tab of the Sort EVT window, and elsewhere where
; the device specific parameters are selected (e.g. spectra Import) ...

Fastcom_MPA3mode_base = widget_base( parent, /column,  space=2, xpad=1, ypad=1, /base_align_center, $
							event_func='fastcom_mpa3_device_sort_option_event', uvalue=self, uname='obj-ref-here')
lab = widget_label( Fastcom_MPA3mode_base, value='Fastcom_MPA3 Option Parameters')

; Droplists (combobox) of X,Y ADC used to select default X,Y ADC #. These show as 1,2,3,4,... for internal values 0,1,2,3,...
; The uvalue contains a help string to be displayed to explain this widget.

Fastcom_MPA3xybase = widget_base( Fastcom_MPA3mode_base, /row, /base_align_center, xpad=0, ypad=0, space=5)
lab = widget_label( Fastcom_MPA3xybase, value='ADC  X:')
Fastcom_MPA3_xadc = widget_combobox( Fastcom_MPA3xybase, value=str_tidy(indgen(10)+1), uname='Fastcom_MPA3-x-adc', /tracking, $
					notify_realize='OnRealize_fastcom_mpa3_device_sort_option_x_adc', sensitive=1, $
					uvalue='Select the default ADC # to use for X.',xsize=xadc_xsize)

lab = widget_label( Fastcom_MPA3xybase, value='       ADC  Y:')
Fastcom_MPA3_yadc = widget_combobox( Fastcom_MPA3xybase, value=str_tidy(indgen(10)+1), uname='Fastcom_MPA3-y-adc', /tracking, $
					notify_realize='OnRealize_fastcom_mpa3_device_sort_option_y_adc', sensitive=1, $
					uvalue='Select the default ADC # to use for Y.',xsize=xadc_xsize)

error = 0
add_widget_vector, self.sort_id.x_adc, Fastcom_MPA3_xadc, error=err & error=error or err
add_widget_vector, self.sort_id.y_adc, Fastcom_MPA3_yadc, error=err & error=error or err
if error then begin
	warning,'fastcom_mpa3_device::render_options','Error adding device object widget ID vectors.'
endif
return
end

;------------------------------------------------------------------------------

; The "options" are widgets and parameters associated with the Sort tab
; of the Sort EVT window. These are rendered in this class, using the method
; "render_options" and the parameters read/written from/to DISK using the "read_options",
; "write_options" methods. Keep a local copy of device parameters and set them using
; "set_options". 

pro fastcom_mpa3_device::set_options, p, x_adc=x_adc, y_adc=y_adc

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
		warning,'fastcom_mpa3_device::set_options',['IDL run-time error caught.', '', $
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
		if tag_present('X_ADC',maia) then self.sort_options.x_adc = maia.x_adc
		if tag_present('Y_ADC',maia) then self.sort_options.y_adc = maia.y_adc
	endif else begin
		if n_elements(x_adc) eq 1 then self.sort_options.x_adc = x_adc
		if n_elements(y_adc) eq 1 then self.sort_options.y_adc = y_adc
	endelse
	
	; Set value of widgets. There may be multiple sort option panels attached to this object, so
	; we use 'widget_control_vector' to set all of them.
	
	widget_control_vector, self.sort_id.x_adc, set_combobox_select = self.sort_options.x_adc
	widget_control_vector, self.sort_id.y_adc, set_combobox_select = self.sort_options.y_adc
	return
end

;-------------------------------------------------------------------

function fastcom_mpa3_device::get_options, error=error

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
		warning,'fastcom_mpa3_device::get_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 0
	
	return, self.sort_options
end

;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

function FASTCOM_MPA3_DEVICE::get_header_info, file, output=output, silent=silent, error=error

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
		warning,'FASTCOM_MPA3_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_sandia_4, sync, TimerEvent, RTCmask, Dummymask, maxADCs, mpa_x_adc, mpa_y_adc

	if n_elements(silent) lt 1 then silent=0
	error = 1

	mp = get_mpa3_header( file, error=error)
	if error then return,0

	n = min([geopixe_max_adcs,16])
	self.header.cal[0:n-1].on = mp.cal[0:n-1].use
	self.header.cal[0:n-1].a = mp.cal[0:n-1].poly[1]
	self.header.cal[0:n-1].b = mp.cal[0:n-1].poly[0]
	self.header.cal[0:n-1].units = mp.cal[0:n-1].unit
	self.header.detector[0:n-1] = 0
	
	self.header.scan.on = 1
	self.header.scan.x_pixels = mp.range[self.sort_options.x_adc]
	self.header.scan.y_pixels = mp.range[self.sort_options.y_adc]
	
	self.header.error = 0
	return, self.header
end

;-------------------------------------------------------------------

function FASTCOM_MPA3_DEVICE::read_setup, unit, xrange,yrange, first=first, $
			n_guide,progress_file, charge=charge, ecompress=ecompress, $
			flux=flux, dead_fraction=dead_fraction, $
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
		warning,'FASTCOM_MPA3_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_sandia_1, evt_data, evt_trailer
common c_sandia_4, sync, TimerEvent, RTCmask, Dummymask, maxADCs, mpa_x_adc, mpa_y_adc
common c_sandia_5, ADCEnabled, ADCpntr, RTadc, rt, ADCindex, j_mpa, n_actual
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

;		This assumes that the file has been opened already. It reads over the header
;		records and defines ADCs in use. File is finally positioned for data read in
;		"read_buffer".

		on_ioerror, bad_io
		head = read_mpa3_header( unit, error=error)
		if error then goto, bad_io

		ADCEnabled = head.ADCEnabled
		ADCpntr = head.ADCpntr
		maxADCs = head.n_ADCs
		ADCindex = uintarr(16)
		adc = uintarr(maxADCs)
		tag = uintarr(maxADCs)

		minx = uint(0)
		maxx = uint(xrange-1)
		miny = uint(0)
		maxy = uint(yrange-1)
		RTadc = lonarr(16)
		rt = 0L

		n_buffer = 100000L				; ~100K x 4 byte buffers (0.4 MBytes)
;		n_buffer = 20L
		stat = fstat(unit)
		n_buffer = n_buffer < ((stat.size-head.bytes) / 8)
		n_guide = n_buffer
		i_buffer = 0L
		j_mpa = 0L
		ibranch = 1L
		hword = 0US
		lword = 0US
		rtcrec = 0US
		dummyrec = 0US
		adcs = 0L
		k_adc = 0L
		print,' n_buffer = ',n_buffer
		print,' MPA3 X,Y ADCs = ',self.sort_options.x_adc, self.sort_options.y_adc

		on_ioerror, bad_io
		stat = fstat(unit)
		close, unit
		openr, unit, stat.name

		tmp = bytarr(head.bytes)
		readu, unit, tmp				; skip over header

		event_array = uintarr( 2, n_buffer)

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function FASTCOM_MPA3_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
       ecompress=ecompress, total_bad_xy=bad_xy, total_processed=processed, $
       station_e=ste, title=title, multiple=multiple, $
       processed=count1, valid=good, raw_xy=raw_xy, time=tot, $
       flux=flux, dead_fraction=dead_fraction, file=file, $
       xoffset=xoffset, yoffset=yoffset, error=error, beam_energy=beam_energy

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
;   t			(optional) Time-over-threshold vector (uintarr), for some DAQs (e.g. Maia)
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
		warning,'FASTCOM_MPA3_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_sandia_1, evt_data, evt_trailer
common c_sandia_2, evt_xrange, evt_yrange
common c_sandia_3, sandia_list
common c_sandia_4, sync, TimerEvent, RTCmask, Dummymask, maxADCs, mpa_x_adc, mpa_y_adc
common c_sandia_5, ADCEnabled, ADCpntr, RTadc, rt, ADCindex, j_mpa, n_actual
common c_sandia_6, ibranch, hword,lword, RTCrec, DummyRec, adcs
common c_sandia_7, adc, tag, k_adc
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

	on_ioerror, bad_io
	nc = n_elements(channel_on)
	
       read_event_buffer, unit, self, n_actual, bytes=4
       if n_actual eq 0 then goto, bad_io
       i_buffer = i_buffer+1

       e = uintarr(2*n_buffer, /nozero)
       x1 = uintarr(2*n_buffer, /nozero)
       y1 = uintarr(2*n_buffer, /nozero)
       ste = uintarr(2*n_buffer, /nozero)
       n = 0L

       err = mpa3_events( event_array,j_mpa,ibranch,n_buffer,n_actual, $
          e,x1,y1,ste, n,2*n_buffer, self.sort_options.x_adc,self.sort_options.y_adc, $
          hword,lword, rtcrec,dummyrec, ADCindex, ADCpntr, $
          adc,tag, k_adc,maxADCs, adcs, bad_xy, rt, rtadc)

       if err ne 0 then begin
         print,'read_buffer: error (',err,') return from mpa3_events'
         goto, bad_io
       endif

       good = n

       if good gt 0 then begin
         qc = where((ste[0:good-1] ge 0) and (ste[0:good-1] lt geopixe_max_adcs), nqc)
         good = nqc
         if nqc gt 0 then begin
;          q = where( (channel_on[ste[qc]] eq 1) and $
;                    (x1[qc] ne 0) and (y1[qc] ne 0), nq)
          q = where( (channel_on[ste[qc]] eq 1) , nq)
          good = nq
          if nq gt 0 then begin
              if raw_xy eq 0 then begin
                 e = e[qc[q]] / uint(ecompress)
                 x1 = x1[qc[q]] / uint(xcompress)
                 y1 = y1[qc[q]] / uint(ycompress)
              endif else begin
                 e = e[qc[q]]
                 x1 = x1[qc[q]]
                 y1 = y1[qc[q]]
              endelse
              ste = ste[qc[q]]
          endif
         endif
       endif

       if good eq 0 then begin
         x1 = 0US
         y1 = 0US
         e = 0US
         ste = 0US
       endif
	   n = good

       processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function FASTCOM_MPA3_DEVICE::import_spec, name, file, group=group

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
		warning,'FASTCOM_MPA3_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'fastcom_mpa3_evt': begin						; 9
			warning,'FASTCOM_MPA3_DEVICE::import_spec',['"fastcom_mpa3_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function FASTCOM_MPA3_DEVICE::get_import_list, error=error

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
		warning,'FASTCOM_MPA3_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

; Details of spectrum import struct ...
;opt = { name:			'', $			; unique name for import
;		title:			'', $			; description for import list
;		in_ext:			'', $			; input file extension
;		request:		'', $			; title for requester
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
	
	opt_22 = define(/import)			; Sandia MPA3 LST
		opt_22.name =		'fastcom_mpa3_evt'		; unique name of import
		opt_22.title =		'Extract from Sandia Fastcom MPA3 LST'
		opt_22.in_ext =		'.lst'		; input file extension
		opt_22.request =	'Select Fastcom MPA3 file to sort for all spectra'
		opt_22.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_22.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_22.use_IC =		1			; pop-up the flux_select PV selection panel
		opt_22.IC_mode = 	0			; default to ion-beam charge mode (pop-up open to access ADC #s for XY)

	opt = [opt_22]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function FASTCOM_MPA3_DEVICE::init

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
		warning,'FASTCOM_MPA3_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

; This is the old common block. Once 'mpa_x_adc' has been replaced in code by 'self.sort_options.x_adc',
; and same for Y, then this can be removed.

common c_sandia_4, sync, TimerEvent, RTCmask, Dummymask, maxADCs, mpa_x_adc, mpa_y_adc
if n_elements(mpa_x_adc) lt 1 then mpa_x_adc = 0S
if n_elements(mpa_y_adc) lt 1 then mpa_y_adc = 2S			;	1S for recent emails

; Sort Option items appear as device setup options in the Sort EVT "Device" tab

	self.options.scan.on = 1		; scan sort options available for this class
									; these must be set-up in the render_options method
	self.options.scan.ysize = 60	; Y size of sort options box, when open

; Set default Sort Options local parameters ...

	self.sort_options.x_adc = 0								; select default X ADC #
	self.sort_options.y_adc = 2								; select default Y ADC #

; Initial heap allocation for sort_id widget vector pointers ...

	self.sort_id.x_adc = ptr_new(/allocate_heap)			; X ADC ID array pointer
	self.sort_id.y_adc = ptr_new(/allocate_heap)			; Y ADC ID array pointer

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"FASTCOM_MPA3_DEVICE" --> FASTCOM_MPA3_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'FASTCOM_MPA3_DEVICE', $	; unique name for this device object
		title = 'Fastcom MPA3 - PC data acquisiion', $
		ext = '.lst', $			; fixed file extension for raw data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; blog data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 1, $		; start detector ADC #'s at 1
		synchrotron = 0, $		; synchrotron data
		ionbeam = 1)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro FASTCOM_MPA3_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('FASTCOM_MPA3_DEVICE')

COMPILE_OPT STRICTARR

maia = {FASTCOM_MPA3_DEVICE,  $

		INHERITS BASE_DEVICE, $		; mandatory base device parameters
		
			sort_options : {sort_options_mpa3, $				; Sort EVT window Sort options panel
					x_adc:						0, $			; X ADC default (0 here for #1 on widget)
					y_adc:						2}, $			; Y ADC default (2 here for #3 on widget)

		sort_id: {sort_id_mpa3, $								; pointers to vector of sort widget IDs
					x_adc:					ptr_new(), $		; X ADC widget ID array pointer
					y_adc:					ptr_new()} $		; Y ADC widget ID array pointer
		}
	return
end
