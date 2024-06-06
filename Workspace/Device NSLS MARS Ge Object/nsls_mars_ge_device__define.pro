;
; GeoPIXE Device Object for Simple 2 long word MARS Ge event data
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
; name()				return name of device (e.g. "NSLS_MARS_GE_DEVICE").
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

pro NSLS_MARS_GE_DEVICE::cleanup

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
		warning,'NSLS_MARS_GE_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

;function NSLS_MARS_GE_DEVICE::get_header_info, file, output=output, silent=silent, error=error
;
;; file		a raw data file to look for associated header, metadata
;; output	if present, this is a file on the output path, if some metadata is
;;			located in that path. 
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
;		warning,'NSLS_MARS_GE_DEVICE::get_header_info',['IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!error_state.msg,'',c], /error
;		MESSAGE, /RESET
;		return, 0
;	endif
;endif
;common c_geopixe_adcs, geopixe_max_adcs
;
;	if n_elements(silent) lt 1 then silent=0
;	error = 1
;	
;	
;	self.header.error = 0
;	return, self.header
;end

;-------------------------------------------------------------------

; Scan raw data files for device specific flux IC PV information
 
;pro NSLS_MARS_GE_DEVICE::flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
;			image_mode=image_mode, group=group, suppress=suppress, $
;			no_pv=no_pv, use_dwell=use_dwell, error=error
;
;; Scan raw data files for flux IC PV information
;; 
;; Input:
;; 	unit		I/O unit open
;; 	evt_file	file-name of opened file
;; 	group		group-leader parent window to pass to any pop-up windows
;;
;;	/Image_mode	scan data for PVs for an image, with dwell
;;	/Suppress	suppress pop-ups
;
;; Return:
;; 	PV_list		string list of all PV's found that may be used to measure flux/IC count
;; 	IC_name		PV selected by user from list
;; 	IC_val		pre-amp sensitivity value
;; 	IC_vunit	pre-amp sensitivity unit multipler
;; 	dwell		dwell-time in a pixel (ms), if needed
;; 	no_pv		flags absence of any PVs
;;	use_dwell	flags need to use dwell in flux count measure
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
;		warning,'NSLS_MARS_GE_DEVICE::flux_scan',['IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!error_state.msg,'',c], /error
;		MESSAGE, /RESET
;		return
;	endif
;endif
;common c_geopixe_adcs, geopixe_max_adcs
;common c_hs_pixe_1, hs_pixe_x_range, hs_pixe_y_range, hs_pixe_x, hs_pixe_y
;common c_hs_pixe_2, hs_pixe_e, hs_pixe_ste, hs_pixe_chans, hs_pixe_dets, hs_pixe_x1, hs_pixe_y1
;common c_hs_pixe_3, med, hs_pixe_data
;common c_hs_pixe_4, hs_pixe_IC
;common c_hs_pixe_5, hs_pixe_IC_value_index, hs_pixe_flux_scale
;common c_hs_pixe_6, max_det
;common c_hs_pixe_7, hs_pixe_dead
;common c_hs_pixe_8, hdf5_id, sdsSPECTRA, hs_pixe_read_line, hdf5_spectra, spectra_id
;common c_hs_pixe_9, hs_pixe_debug
;
;	if n_elements(first) lt 1 then first = 1
;	if n_elements(suppress) lt 1 then suppress = 0
;	if n_elements(image_mode) lt 1 then image_mode = 1
;	if n_elements(hs_pixe_debug) lt 1 then hs_pixe_debug = 0
;	
;	PV_list = 'none'
;	IC_name = ''
;	IC_val = 1.
;	IC_vunit = 0.
;	dwell = 0.
;	no_pv = 1
;	use_dwell = 0
;	error = 1
;	
;	on_ioerror, bad_io
;
;	error = 0
;	return
;
;bad_io:
;	warning,'NSLS_MARS_GE_DEVICE::flux_scan','HDF file I/O error.'
;	error = 1
;	return
;end

;-------------------------------------------------------------------

; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.

function NSLS_MARS_GE_DEVICE::read_setup, unit, xrange, yrange, first=first, $
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
		warning,'NSLS_MARS_GE_DEVICE::read_setup',['IDL run-time error caught.', '', $
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
		self.spectrum_mode = 1
		if (n_elements(flux) ge 2) then self.spectrum_mode=0
		nsls_flux_scale = 1.0
		maia_swap = (big_endian() ne self->big_endian())

;	# buffers to read before Progress = 100,000/n_guide > 1. N_buffer MUST be divisable by 4 for Fortran code.
;	Take care increasing n_buffer, as the maia_tt sum in spectrum mode will get large. Absolute limit is about 2M bytes.

		n_buffer = 500000L				; ~500K byte buffer (2 MBytes)
		n_guide = 125000L				; progress every 4th buffer (8 Mb)
		n_monitor = n_buffer			; monitor PV string return
		nsls_IC = 0.0					; in c_nsls_1

		x0 = 0L							; in c_maia_2
		y0 = 0L
		z0 = 0L
		progress_file = 0
		i_buffer = 0L

		maia_x_range = long(xrange)
		maia_y_range = long(yrange)
		maia_z_range = long(zrange)
			
		event_array = lonarr(n_buffer)
	endif
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

; read_buffer()		called repeatedly to read buffers from the data file, process
;					these to extract X,Y,E triplet data, tagged by detector channel, ste,
;					compress X,Y,E if needed, and optionally detect other
;					information (e.g. flux/charge, energy tokens). 

function NSLS_MARS_GE_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
		station_e=ste, time=tot, veto=veto, ecompress=ecompress, multiple=multiple, $
		xoffset=xoffset, yoffset=yoffset, title=title, file=file, error=error, $
		flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
		total_processed=processed, processed=count1, valid=good, total_bad_xy=bad_xy, raw_xy=raw_xy, $
		by_odd=by_odd, by_even=by_even, support_even_odd=support_even_odd

;       ecompress=ecompress, total_bad_xy=bad_xy, total_processed=processed, $
;       station_e=ste, title=title, multiple=multiple, $
;       processed=count1, valid=good, raw_xy=raw_xy, time=tot, $
;       flux=flux, dead_fraction=dead_fraction, file=file, $
;       xoffset=xoffset, yoffset=yoffset, error=error, beam_energy=beam_energy

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
;   tot			Time-over-threshold vector (uintarr)
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
		warning,'NSLS_MARS_GE_DEVICE::read_buffer',['IDL run-time error caught.', '', $
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

@mars_listmode.def

	if n_elements(xanes) lt 1 then xanes = 0
	nc = n_elements(channel_on)								; channel on/off flag for each channel

	on_ioerror, bad_io
	read_event_buffer, unit, self, n_actual, bytes=4
;	print,'i_buffer = ',i_buffer, ' n_actual=',n_actual
	if n_actual eq 0 then begin
		gprint,level=2,'maia_device::read_buffer: bad n_actual'
		goto, bad_io
	endif
	i_buffer = i_buffer+1

	if n_elements(x1) eq 0 then begin						; just assign these 
		n_events = n_buffer									; vectors first time
		maia_e = uintarr(n_events, /nozero)
		maia_t = uintarr(n_events, /nozero)					; e,t per event
		maia_0 = lonarr(n_events, /nozero)
		maia_1 = lonarr(n_events, /nozero)					; x,y,z per event
		maia_2 = lonarr(n_events, /nozero)
		maia_ste = uintarr(n_events, /nozero)				; 'station', i.e. detector #
;		maia_veto = uintarr(n_events, /nozero)				; flag a rejected event
;		maia_tags = uintarr(n_events, /nozero)				; can be used to retrieve debug event info
;		maia_pseudo = uintarr(n_events, /nozero)			; pseudo event (i.e. BT, FC0, FC1 in records added as events)
;		n_fx = 4
;		maia_fx = fltarr(n_fx,n_events, /nozero)			; selected flux (Epics or h/w), FC0, FC1, BT for each event
	endif else begin
		n_events = n_elements(maia_e)
;		n_fx = n_elements(maia_fx[*,0])
	endelse

	n = n_actual / 2
	good = 0L
	debug = 0L
	xcompress = 1
	ycompress = 1
	zcompress = 1
	gprint, level=1, 'NSLS_MARS_GE_DEVICE::read_buffer: maia_swap = ', maia_swap

	if n eq 0 then begin
		gprint,level=2,'NSLS_MARS_GE_DEVICE::read_buffer: Zero "n" return from maia_384_events6'
		return, 0
	endif

	d = ulong(event_array,0,2,n)
	maia_e = reform( ishft( d[0,0:n-1] and e_mask4, e_offset4))
	maia_t = reform( ishft( d[0,0:n-1] and t_mask4, t_offset4))
	maia_ste = reform( ishft( d[0,0:n-1] and adr_mask4, adr_offset4))

	e = uint(maia_e[0:n-1])
	tot = uint(maia_t[0:n-1])
	ste = uint(maia_ste[0:n-1])
	x1 = maia_0[0:n-1]
	y1 = maia_1[0:n-1]
	z1 = maia_2[0:n-1]
	good = n

;	n		high water mark in event vector buffers
;	good	number of valid events, so far

;	Compress all, not just good events, so that pseudo events get corrected too ...
		
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

		x1 = uint(x1 - xoffset2)
		y1 = uint(y1 - yoffset2)
		z1 = uint(z1 - zoffset2)
			
	endif else begin										; spectra
		if raw_xy eq 0 then begin
			x1 = uint(x1 - xoffset2)			; assumes offsets are zero in total spectrum mode
			y1 = uint(y1 - yoffset2)			; offsets used in region spectra extract mode
			z1 = uint(z1 - zoffset2)
		endif else begin
			x1 = uint(x1)		
			y1 = uint(y1)		
			z1 = uint(z1)
		endelse
			
;		self.dwell_total = self.dwell_total + dt				; ms
;		time_last = maia_last_time
	endelse

	processed = processed + good

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function NSLS_MARS_GE_DEVICE::import_spec, name, file, group=group

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
		warning,'NSLS_MARS_GE_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'nsls_mars_ge_bin': begin							; 9
			warning,'NSLS_MARS_GE_DEVICE::import_spec',['"maia_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function NSLS_MARS_GE_DEVICE::get_import_list, error=error

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
		warning,'NSLS_MARS_GE_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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

	opt_39 = define(/import)			; Maia 384 list-mode
		opt_39.name =		'nsls_mars_ge_bin'	; unique name of import
		opt_39.title =		'Extract from NSLS MARS Ge BIN files'
		opt_39.in_ext =		'.bin'			; input file extension
		opt_39.request =	'NSLS MARS Ge data scan for all spectra
		opt_39.preview =	0			; allow spectrum preview
		opt_39.raw =		0			; flags use of separate Raw data path '(*pstate).dpath'
		opt_39.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_39.multifile =	0			; denotes data in a series of more than one file
		opt_39.separate =	''			; char between file and run #
		opt_39.use_pileup =	0			; request pileup file
		opt_39.use_throttle = 0			; request throttle file
		opt_39.use_IC =		0			; pop-up the flux_select PV selection panel
		opt_39.IC_mode = 	0			; default to using PV for IC
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

function NSLS_MARS_GE_DEVICE::init

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
		warning,'NSLS_MARS_GE_DEVICE::init',['IDL run-time error caught.', '', $
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

;	Pass core device parameters to BASE DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"NSLS_MARS_GE_DEVICE" --> NSLS_MARS_GE_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'NSLS_MARS_GE_DEVICE', $	; unique name for this device object
		title = 'NSLS MARS Ge BIN data file', $
		ext = '.bin', $			; a fixed file extension for raw data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 1, $		; 1=list-mode data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 0, $		; start detector ADC #'s at 1
		use_bounds = 0, $		; not confine charge/flux within bounded area
		synchrotron = 1, $		; synchrotron data
		array_default = 1, $	; a detector array by default
		ionbeam = 0)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro NSLS_MARS_GE_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('NSLS_MARS_GE_DEVICE')

COMPILE_OPT STRICTARR

maia = {NSLS_MARS_GE_DEVICE,  $

		INHERITS BASE_DEVICE $			; mandatory base device parameters
		}								; see the base_device super-class for details.
	return
end
