;
; GeoPIXE Device Object for NAC XSYS system data
;
; MPsys is a general Camac data acquisition and scanning
; system developed by NAC (iThemba Labs) for general nuclear microprobe imaging.
; Data is written by a Linux processor in little-endian byte order.
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

pro NAC_XSYS_DEVICE::cleanup

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
		warning,'NAC_XSYS_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function NAC_XSYS_DEVICE::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'NAC_XSYS_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_nac_1, buffer_id_mask, event_id_mask,event_type_mask
common c_nac_2, header_skip, low_byte
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_mpsys_3, mdx_offset,mdy_offset, correct_mdx,correct_mdy
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_mpsys_5c, xlast,xmax,xcount, toggle_last
common c_mpsys_6, shuffle

		e_mask = '1FFF'xU
		x_mask = '0FFF'xU
		y_mask = '0FFF'xU

		buffer_id_mask = 'FC00'xU
		event_id_mask = 'C000'xU
		event_type_mask = '00FF'xU
		header_skip = '03FF'xU
		low_byte = '00FF'xU

		n_buffer = 16*1024L - 256		; ~32K byte buffers
		n_guide = n_buffer/5
		i_buffer = 0L

		event_array = uintarr( n_buffer)
;		buffer = assoc( unit, event_array)

	return, 0
end

;-------------------------------------------------------------------

function NAC_XSYS_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
		warning,'NAC_XSYS_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_nac_1, buffer_id_mask, event_id_mask,event_type_mask
common c_nac_2, header_skip, low_byte
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_mpsys_3, mdx_offset,mdy_offset, correct_mdx,correct_mdy
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_mpsys_5c, xlast,xmax,xcount, toggle_last
common c_mpsys_6, shuffle

	on_ioerror, bad_io
	nc = n_elements(channel_on)
	
       read_event_buffer, unit, self, n_actual, bytes=2			; regular 2-byte repeat format
       if n_actual eq 0 then goto, bad_io						; byte swaps done in 'read_event_buffer'
       i_buffer = i_buffer+1

       n = n_buffer
       e = uintarr(n_buffer, /nozero)
       x = uintarr(n_buffer, /nozero)
       y = uintarr(n_buffer, /nozero)
       ste = uintarr(n_buffer, /nozero)

;     The data uses the bottom bits for ADC data (13 bits e, 12 bits
;     for x,y).

       i = 0L
       buffer_length = (event_array[i] / 2) < n_actual
       buffer_id = event_array[i+1] and buffer_id_mask
       skip = (event_array[i+1] and header_skip) / 2
;     print, ' Buffer Length = ', buffer_length
;     print, ' Buffer ID = ', buffer_id, format='(a,z9)'
;     print, ' Header skip = ', skip

       if buffer_id ne 'E800'xu then begin
         print,'Unknown buffer ID = ',buffer_id, format='(a,z9)'
         good = 0L
         n = 0L
         return, 1
       endif
       i = i+skip
       title_start = -1L
       title_end = -1L
       run = -1L
       n = 0L

       err = xsys_events( event_array,i,n,buffer_length, channel_on,nc,e,x,y,ste,  $
          run, title_start,title_end, bad_xy)

       if err ne 0 then begin
         print,'read_buffer: error (',err,') return from xsys_events'
         goto, bad_io
       endif

       if run ge 0 then run_num=run
       if (title_start ge 0) and (title_end ge title_start) then begin
         s = word_string( event_array[title_start:title_end])
         l1 = locate('BEGIN=',s)
         l2 = locate('MAXEVZ=',s)
         if (l1 ge 0) and (l2 ge l1+10) then begin
          title = strmid(s,l1+6,l2-l1-9)
         endif
       endif

       good = long(n)
       processed = processed + good

       if good gt 0 then begin
         if raw_xy eq 0 then begin
          x1 = x[0:n-1] / uint(xcompress)
          y1 = y[0:n-1] / uint(ycompress)
          e = e[0:n-1] / uint(ecompress)
         endif else begin
          x1 = x[0:n-1]
          y1 = y[0:n-1]
          e = e[0:n-1]
         endelse
         ste = ste[0:n-1]
       endif else begin
         x1 = 0US
         y1 = 0US
         e = 0US
         ste = 0
       endelse
	return, 0
	
bad_io:
	good = 0L
	n = 0L
	return, 1
end

;-------------------------------------------------------------------

; Import spectra of various local types. This does not include extraction of
; spectra from list-mode data, which is handled elsewhere.

function NAC_XSYS_DEVICE::import_spec, name, file, group=group

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
		warning,'NAC_XSYS_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'nac_xsys_dat': begin						; 8
			get_nac_spec, p, file, group=group
			end
		'nac_root_ascii': begin						; 
			get_root_ascii_spec, p, file, group=group
			end
		'nac_xsys_evt': begin						; 12
			warning,'NAC_XSYS_DEVICE::import_spec',['"nac_xsys_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function NAC_XSYS_DEVICE::get_import_list, error=error

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
		warning,'NAC_XSYS_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_8 = define(/import)				; NAC spectrum DAT
		opt_8.name =		'nac_xsys_dat'		; unique name of import
		opt_8.title =		'iThemba XSYS VAX/VMS spectrum'
		opt_8.in_ext =		'.dat'		; file extension
		opt_8.request =		'Select NAC XSYS Spectrum to Load'

	opt_99 = define(/import)			; ROOT ascii
		opt_99.name =		'nac_root_ascii'		; unique name of import
		opt_99.title =		'iThemba ROOT ascii spectrum'
		opt_99.in_ext =		'.txt'		; file extension
		opt_99.request =	'Select ROOT ASCII Spectrum to Load'

	opt_12 = define(/import)			; NAC spectrum DAT
		opt_12.name =		'nac_xsys_evt'		; unique name of import
		opt_12.title =		'Extract from XSYS VAX/VMS EVT'
		opt_12.in_ext =		'.evt'		; input file extension
		opt_12.request =	'Select NAC XSYS EVT file to sort for all spectra'
		opt_12.spec_evt =	1			; uses a call to spec_evt to extract from EVT data

	opt = [opt_8, opt_12, opt_99]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function NAC_XSYS_DEVICE::init

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
		warning,'NAC_XSYS_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

; Sort Option items appear as Scan setup options in the Sort EVT "Scan" tab

	self.options.scan.on = 0		; scan sort options availability for this class
									; these must be set-up in a render_options method
;	self.options.scan.ysize = 75	; Y size of sort options box, when open

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"NAC_XSYS_DEVICE" --> NAC_XSYS_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'NAC_XSYS_DEVICE', $	; unique name for this device object
		title = 'iThemba XSYS - VAX data acquisition', $
		ext = '.evt', $			; not a fixed file extension for blog data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; blog data written with Unix byte order
		vax_float = 1, $		; VAX D-Floating point
		start_adc = 1, $		; start detector ADC #'s
		synchrotron = 0, $		; synchrotron data
		ionbeam = 1)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro NAC_XSYS_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('NAC_XSYS_DEVICE')

COMPILE_OPT STRICTARR

maia = {NAC_XSYS_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
