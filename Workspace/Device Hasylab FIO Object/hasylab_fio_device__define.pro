;
; GeoPIXE Device Object for Hasylab FIO format data (basic only, needs work ...)
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

pro HASYLAB_FIO_DEVICE::cleanup

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
		warning,'HASYLAB_FIO_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function HASYLAB_FIO_DEVICE::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'HASYLAB_FIO_DEVICE::read_setup',['IDL run-time error caught.', '', $
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

		n_guide = 50000L				; i.e. Will update each 2nd pixel
		spectrum_mode = 0
		if n_elements(flux) eq 1 then spectrum_mode = 1

		on_ioerror, bad_io
		stat = fstat(unit)
 		close, unit
 		get_hasylab_fio, p, stat.name
 		if ptr_valid(p[0]) eq 0 then goto, bad_io

		max_det = n_elements(p)
		if max_det lt 1 then goto, bad_io
		max_size = 0
		for i=0L,max_det-1 do begin
			max_size = max([max_size,(*p[i]).size])
		endfor
		if max_size lt 1 then goto, bad_io

;		The variable data is the counts in each mca channel, so an array
;		2048 x number of detector elements. 

		nsls_data = fltarr(max_size,max_det)
		dtcorr = fltarr(max_det)
		I0 = fltarr(max_det)
		for i=0L,max_det-1 do begin
			dtcorr[i] = (*p[i]).deadtime_correction
			I0[i] = (*p[i]).IC_total
			n = (*p[i]).size
			if n ge 1 then nsls_data[0:n-1,i] = *((*p[i]).data)
		endfor

		nsls_flux_scale = 1.0

;		If 'flux_scan' method implemented, then we'll have a sensivity selection ...
;		nsls_flux_scale = flux_ic.val * flux_ic.unit

		nsls_x_range = long(xrange)
		nsls_y_range = long(yrange)
		nsls_dets = n_elements(nsls_data[0,*])
		nsls_chans = n_elements(nsls_data[*,0])
		nsls_IC = I0 * nsls_flux_scale
		nsls_dead = 0. > (1. - 1./(dtcorr > 1.))
		q = where(finite(nsls_dead) eq 0)
		if q[0] ne -1 then nsls_dead[q]=0.0

		if first then begin
			nsls_x = 0
			nsls_y = 0
			if spectrum_mode then self.dwell_total = 0.0
		endif

		ramp = indgen(nsls_chans)
		nsls_e = uintarr( nsls_chans, nsls_dets)
		nsls_ste = nsls_e
		for i=0L,nsls_dets-1 do begin
			nsls_e[*,i] = ramp
			nsls_ste[ramp,i] = i
		endfor

		nsls_e = reform( nsls_e, nsls_chans*nsls_dets)
		nsls_ste = reform( nsls_ste, nsls_chans*nsls_dets)
		nsls_x1 = uintarr( nsls_chans*nsls_dets)
		nsls_y1 = uintarr( nsls_chans*nsls_dets)

		openr, unit, stat.name

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function HASYLAB_FIO_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
		warning,'HASYLAB_FIO_DEVICE::read_buffer',['IDL run-time error caught.', '', $
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
	
		multiple = long( reform(nsls_data, nsls_chans*nsls_dets))
		e = nsls_e
		ste = nsls_ste
		x1 = nsls_x1
		y1 = nsls_y1
		x1[*] = uint(nsls_x)
		y1[*] = uint(nsls_y)

		spectrum_mode = 0
		if n_elements(flux) eq 1 then spectrum_mode = 1
		 
		if spectrum_mode then begin
			dead_fraction[0:n_elements(nsls_dead)-1] = dead_fraction[0:n_elements(nsls_dead)-1] + nsls_dead
			self.dwell_total = self.dwell_total + 1.				; norm factor (init this in 'setup' for /first)
		endif else begin
			dt = mean(nsls_dead)
			if (nsls_x ge 0) and (nsls_y ge 0) and $
						(nsls_x lt nsls_x_range) and (nsls_y lt nsls_y_range) then begin
				dead_fraction[nsls_x/xcompress,nsls_y/ycompress] = dt
			endif
		endelse

		if spectrum_mode then begin
			if ((nsls_x_range*nsls_y_range gt 0) and (nsls_x ge 0) and (nsls_y ge 0) and $
						(nsls_x lt nsls_x_range) and (nsls_y lt nsls_y_range)) or $
						(nsls_x_range*nsls_y_range eq 0) then begin
				flux = flux + float(nsls_IC)
			endif
		endif else begin
			if (nsls_x ge 0) and (nsls_y ge 0) and $
						(nsls_x lt nsls_x_range) and (nsls_y lt nsls_y_range) then begin
				flux[nsls_x/xcompress,nsls_y/ycompress] = flux[nsls_x/xcompress,nsls_y/ycompress] + float(nsls_IC)
			endif
		endelse

       nsls_x = nsls_x+1
       if nsls_x ge nsls_x_range then begin
         nsls_x = 0
         nsls_y = nsls_y+1
       endif

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
      close, unit
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

; Import spectra of various local types. This does not include extraction of
; spectra from list-mode data, which is handled elsewhere.

function HASYLAB_FIO_DEVICE::import_spec, name, file, group=group

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
		warning,'HASYLAB_FIO_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'hasylab_fio_fio': begin					; 37
			get_hasylab_fio, p, file
			end
		'hasylab_fio_evt': begin					; 38
			warning,'HASYLAB_FIO_DEVICE::import_spec',['"hasylab_fio_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function HASYLAB_FIO_DEVICE::get_import_list, error=error

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
		warning,'HASYLAB_FIO_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_37 = define(/import)			; selected Hasylab FIO files
		opt_37.name =		'hasylab_fio_fio'		; unique name of import
		opt_37.title =		'Hasylab FIO spectra files'
		opt_37.in_ext =		'.fio'		; file extension
		opt_37.request =	'Select Hasylab FIO spectra to Load'
	
	opt_38 = define(/import)			; Extract from range of Hasylab FIO image spectra
		opt_38.name =		'hasylab_fio_evt'		; unique name of import
		opt_38.title =		'Extract from Hasylab FIO image spectra'
		opt_38.in_ext =		'.fio'		; input file extension
		opt_38.request =	'Hasylab FIO files to scan for all spectra [Enter FIRST file]'
		opt_38.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_38.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_38.multifile =	1			; denotes data in a series of more than one file
		opt_38.separate =	'_'			; char between file and run #
		opt_38.use_IC =		1			; pop-up the flux_select PV selection panel
		opt_38.xr =			200			; default X range
		opt_38.yr =			200			; default Y range

	opt = [opt_37, opt_38]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function HASYLAB_FIO_DEVICE::init

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
		warning,'HASYLAB_FIO_DEVICE::init',['IDL run-time error caught.', '', $
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
;	"HASYLAB_FIO_DEVICE" --> HASYLAB_FIO_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'HASYLAB_FIO_DEVICE', $	; unique name for this device object
		title = 'Hasylab FIO - ASCII pixel data', $
		ext = '.fio', $			; file extension for raw data
		multi_files = 1, $		; multiple segment files per run
		multi_char = '_', $		; separates run from segment number in file name
		big_endian = 0, $		; blog data written with PC byte order
		vax_float = 0, $		; VAX D-Floating point
		start_adc = 1, $		; start detector ADC #'s
		synchrotron = 1, $		; synchrotron data
		ionbeam = 0)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro HASYLAB_FIO_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('HASYLAB_FIO_DEVICE')

COMPILE_OPT STRICTARR

maia = {HASYLAB_FIO_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
