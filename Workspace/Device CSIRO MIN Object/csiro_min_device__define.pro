;
; GeoPIXE Device Object for CSIRO Minerals BIN data
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
; 					seen in the region mask arrays (used with EVT button on Image Regions window).
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

pro CSIRO_MIN_DEVICE::cleanup

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
		warning,'CSIRO_MIN_DEVICE::cleanup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	self->BASE_DEVICE::cleanup
    return
end

;-------------------------------------------------------------------

; This method is called when list-mode files are accessed (e.g. when a 
; file is selected in the Sort EVT window) to read details of the data
; and/or experiment/scan set-up. It fills in as many header details as can be
; found in the data. See the Base_device super-class definition for the 
; contents of the header. Of particular importance are the scan sizes in pixels.

function CSIRO_MIN_DEVICE::get_header_info, file, output=output, silent=silent, error=error

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
		warning,'CSIRO_MIN_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	if n_elements(silent) lt 1 then silent=0
	error = 1

	 close, 2
     on_ioerror, bad_io
     openr, 2, strip_file_ext(file)+'.bin'

	 event_header1 = { $
		version:			0US, $			; version
		width:				0L, $			; width
		height:				0L, $			; height
		SpectraPerPixel:	0US, $			; spectra per pixel
		ChannelsPerSpec:	0US, $			; channels per spectrum
		BytesPerChannel:	0US, $			; bytes per channels
		Gain:				0.0, $			; gain (eV per channel)
		Offset:				0.0	$			; offset (eV)
	 }

	 readu, 2, event_header1
	 swap_bytes, event_header1, big_endian_data=self->big_endian()
	 close, 2

     n = min([geopixe_max_adcs,event_header1.SpectraPerPixel])

     self.header.cal[0:n-1].on = 1
     self.header.cal[0:n-1].a = 0.001 * event_header1.gain
     self.header.cal[0:n-1].b = 0.001 * event_header1.offset
     self.header.cal[0:n-1].units = 'keV'
     self.header.detector[0:n-1] = 0          		; (SEM = 8 later)

     self.header.scan.on = 1
     self.header.scan.x_pixels = event_header1.width
     self.header.scan.y_pixels = event_header1.height
     error = 0
	
	self.header.error = 0
	return, self.header
	
bad_io:
	self.header.error = 1
	return, self.header
end

;-------------------------------------------------------------------

function CSIRO_MIN_DEVICE::read_setup, unit, xrange,yrange, first=first, $
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
;   					pv		user selected tag string (EPICS PV or other tag) to be used for flux IC
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
		warning,'CSIRO_MIN_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_nsls_1, nsls_x_range, nsls_y_range, nsls_x, nsls_y
common c_nsls_2, nsls_e, nsls_ste, nsls_chans, nsls_dets, nsls_x1, nsls_y1
common c_nsls_3, med, nsls_data
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_nsls_6, max_det
common c_nsls_7, nsls_dead
common c_nsls_8, mca_id, sdsSPECTRA, nsls_read_line, mca_spectra
common c_nsls_9, nsls_debug

	event_header1 = { $
		version:			0US, $			; version
		width:				0L, $			; width
		height:				0L, $			; height
		SpectraPerPixel:	0US, $			; spectra per pixel
		ChannelsPerSpec:	0US, $			; channels per spectrum
		BytesPerChannel:	0US, $			; bytes per channels
		Gain:				0.0, $			; gain (eV per channel)
		Offset:				0.0	$			; offset (eV)
	}

	readu, unit, event_header1
	swap_bytes, event_header1, big_endian_data=self->big_endian()

	nsls_x_range = event_header1.width
	nsls_y_range = event_header1.height
	nsls_dets = event_header1.SpectraPerPixel
	nsls_chans = event_header1.ChannelsPerSpec

	case event_header1.BytesPerChannel of
		1: begin
			nsls_data = bytarr( event_header1.SpectraPerPixel, event_header1.ChannelsPerSpec, /nozero)
			end
		2: begin
			nsls_data = uintarr( event_header1.SpectraPerPixel, event_header1.ChannelsPerSpec, /nozero)
			end
		4: begin
			nsls_data = ulonarr( event_header1.SpectraPerPixel, event_header1.ChannelsPerSpec, /nozero)
			end
	endcase

	n_guide = 1000000L				; i.e. Will update each 10th pixel

	if first then begin
		nsls_x = 0
		nsls_y = 0
	endif

	ramp = indgen(nsls_chans)
	nsls_e = uintarr( nsls_dets, nsls_chans)
	nsls_ste = nsls_e
	for i=0L,nsls_dets-1 do begin
		nsls_e[i,*] = ramp
		nsls_ste[i,ramp] = i
	endfor

	nsls_e = reform( nsls_e, nsls_chans*nsls_dets)
	nsls_ste = reform( nsls_ste, nsls_chans*nsls_dets)
	nsls_x1 = uintarr( nsls_chans*nsls_dets)
	nsls_y1 = uintarr( nsls_chans*nsls_dets)
	return, 0

bad_io:
	return, 1
end

;-------------------------------------------------------------------

function CSIRO_MIN_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
;   e			energy array returned
;   x1			X array return
;   y1			Y array return
;   n			number of (x,y,e) triplets returned
;   multiple	if this has same dimensions as e, then it indicates multiple
;          		events with the same x1,y1,e.
;   ste			ADC number for each returned event (less offset, starting at 0)
;   tot			Time-over-threshold, for some DAQs (e.g. Maia)
;   beam_energy	beam energy, if it's available (MeV=ionbeam, keV=synchrotron)
;   valid		number of good events (same as 'n'), or zero
;   count1		number of events processes in this buffer
;   bad_xy		increment passed in value of total event with bad X,Y codes
;   processed	increment total number of events processed.
;   title		run title
;   error		error=1 flags an error to abort

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
		warning,'CSIRO_MIN_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_nsls_1, nsls_x_range, nsls_y_range, nsls_x, nsls_y
common c_nsls_2, nsls_e, nsls_ste, nsls_chans, nsls_dets, nsls_x1, nsls_y1
common c_nsls_3, med, nsls_data
common c_nsls_4, nsls_IC
common c_nsls_5, nsls_IC_value_index, nsls_flux_scale
common c_nsls_6, max_det
common c_nsls_7, nsls_dead
common c_nsls_8, mca_id, sdsSPECTRA, nsls_read_line, mca_spectra
common c_nsls_9, nsls_debug

	on_ioerror, bad_io
	nc = n_elements(channel_on)
	
	   readu, unit, nsls_data
	   swap_bytes, nsls_data, big_endian_data=self->big_endian()

       multiple = long( reform(nsls_data, nsls_chans*nsls_dets))
       e = nsls_e
       ste = nsls_ste
       x1 = nsls_x1
       y1 = nsls_y1
       x1[*] = uint(nsls_x)
       y1[*] = uint(nsls_y)

       nsls_x = nsls_x+1
       if nsls_x ge nsls_x_range then begin
         nsls_x = 0
         nsls_y = nsls_y+1
       endif

       q = where((multiple gt 0) and channel_on[ste], n)
       if n gt 0 then begin
         e = e[q]
         ste = ste[q]
         x1 = x1[q]
         y1 = y1[q]
         multiple = multiple[q]
         good = n
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
         n = 0L
       endelse

       processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function CSIRO_MIN_DEVICE::import_spec, name, file, group=group

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
		warning,'CSIRO_MIN_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'csiro_min_evt': begin						; 45
			warning,'CSIRO_MIN_DEVICE::import_spec',['"csiro_min_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function CSIRO_MIN_DEVICE::get_import_list, error=error

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
		warning,'CSIRO_MIN_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_45 = define(/import)			; CSIRO Minerals BIN
		opt_45.name =		'csiro_min_evt'		; unique name of import
		opt_45.title =		'Extract from CSIRO Minerals BIN spectra files'
		opt_45.in_ext =		'.bin'		; input file extension
		opt_45.request =	'Select minerals BIN file to scan for all spectra'
		opt_45.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_45.spec_evt =	1			; uses a call to spec_evt to extract from EVT data

	opt = [opt_45]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function CSIRO_MIN_DEVICE::init

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
		warning,'CSIRO_MIN_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"CSIRO_MIN_DEVICE" --> CSIRO_MIN_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'CSIRO_MIN_DEVICE', $	; unique name for this device object
		title = 'CSIRO minerals - EMP image data file', $
		ext = '.bin', $			; a fixed file extension for data
		multi_files = 0, $		; not multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; data written with Linux byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 1, $		; start detector ADC #'s at 1
		synchrotron = 0, $		; synchrotron/XRF data
		ionbeam = 1)			; ion-beam (EMP) data
	return, i
end

;-------------------------------------------------------------------

pro CSIRO_MIN_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('CSIRO_MIN_DEVICE')

COMPILE_OPT STRICTARR

maia = {CSIRO_MIN_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
