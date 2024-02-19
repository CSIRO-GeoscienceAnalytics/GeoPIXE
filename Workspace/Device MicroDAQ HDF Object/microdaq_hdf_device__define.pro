;
; GeoPIXE Device Object for UM/CSIRO LabVIEW MicroDAQ HDF data
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
; name()				return name of device (e.g. "MicroDAQ_HDF_DEVICE").
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

pro MicroDAQ_HDF_DEVICE::cleanup

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
		warning,'MicroDAQ_HDF_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function MicroDAQ_HDF_DEVICE::get_header_info, file, output=output, silent=silent, error=error

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
		warning,'MicroDAQ_HDF_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	if n_elements(silent) lt 1 then silent=0
	error = 1
	
	if H5F_IS_HDF5(strip_file_ext(file)+'.hdf') eq 0 then begin
		warning,'MicroDAQ_HDF_DEVICE::get_header_info','Not a valid HDF5 file: '+strip_file_ext(file)+'.hdf'
		self.header.error = 1
		return, self.header
	endif
	
	fid = h5f_open( strip_file_ext(file)+'.hdf')
	
	
	mca_id = HDF_SD_START(strip_file_ext(file)+'.hdf', /read)

;  get the indexes to the data
	offset_id = HDF_SD_NAMETOINDEX(mca_id, 'OFFSET')
	slope_id = HDF_SD_NAMETOINDEX(mca_id, 'SLOPE')
	quad_id = HDF_SD_NAMETOINDEX(mca_id, 'QUAD')
	units_id = HDF_SD_NAMETOINDEX(mca_id, 'UNITS')
	i0_id = HDF_SD_NAMETOINDEX(mca_id, 'i0')

;  select the SDs
	sdsOFFSET = HDF_SD_SELECT(mca_id, offset_id)
	sdsSLOPE = HDF_SD_SELECT(mca_id, slope_id)
	sdsQUAD = HDF_SD_SELECT(mca_id, quad_id)
	sdsUNITS = HDF_SD_SELECT(mca_id, units_id)
	sdsI0 = HDF_SD_SELECT(mca_id, i0_id)

;  get the mca calibration info
	hdf_sd_getdata, sdsOFFSET, OFFSET
	hdf_sd_getdata, sdsSLOPE, SLOPE
	hdf_sd_getdata, sdsQUAD, QUAD
	hdf_sd_getdata, sdsUNITS, UNITS

	n = min([geopixe_max_adcs,n_elements(OFFSET),32])
	self.header.cal[0:n-1].on = 1
	self.header.cal[0:n-1].a = SLOPE
	self.header.cal[0:n-1].b = OFFSET
	self.header.cal[0:n-1].units = 'keV'
	self.header.detector[0:n-1] = 7          ; SXRF

;  read the HDF file again to figure out how many point there are
	title_id = HDF_SD_ATTRFIND(mca_id,'Image title')
	ncols_id = HDF_SD_ATTRFIND(mca_id,'Number of columns')
	nrows_id = HDF_SD_ATTRFIND(mca_id,'Number of rows')
	time_id  = HDF_SD_ATTRFIND(mca_id, 'Preset Real Time (msec)')
	start1_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 1 Start')
	stop1_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 1 Stop')
	start2_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 2 Start')
	stop2_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 2 Stop')

	HDF_SD_ATTRINFO, mca_id, title_id, DATA=title
	HDF_SD_ATTRINFO, mca_id, ncols_id, DATA=ncols
	HDF_SD_ATTRINFO, mca_id, nrows_id, DATA=nrows
	HDF_SD_ATTRINFO, mca_id, time_id, DATA=ntime
	HDF_SD_ATTRINFO, mca_id, start1_id, DATA=start1
	HDF_SD_ATTRINFO, mca_id, stop1_id, DATA=stop1
	HDF_SD_ATTRINFO, mca_id, start2_id, DATA=start2
	HDF_SD_ATTRINFO, mca_id, stop2_id, DATA=stop2
	HDF_SD_END, mca_id

	self.header.title = title
	self.header.scan.x_pixels = fix(ncols)
	self.header.scan.y_pixels = fix(nrows)
	self.header.scan.on = 1
	self.header.scan.x_mm = abs(stop1-start1)
	self.header.scan.y_mm = abs(stop2-start2)
	error = 0
	
	self.header.error = 0
	return, self.header
end

;-------------------------------------------------------------------

; Scan raw data files for device specific flux IC PV information
 
pro MicroDAQ_HDF_DEVICE::flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
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
		warning,'MicroDAQ_HDF_DEVICE::flux_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
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

	if n_elements(first) lt 1 then first = 1
	if n_elements(suppress) lt 1 then suppress = 0
	if n_elements(image_mode) lt 1 then image_mode = 1
	if n_elements(nsls_debug) lt 1 then nsls_debug = 0
	
	PV_list = 'none'
	IC_name = ''
	IC_val = 1.
	IC_vunit = 0.
	dwell = 0.
	no_pv = 1
	use_dwell = 0
	error = 1
	
	on_ioerror, bad_io

	error = 0
	return

bad_io:
	warning,'MicroDAQ_HDF_DEVICE::flux_scan','HDF file I/O error.'
	error = 1
	return
end

;-------------------------------------------------------------------

; read_setup()		will be called after each data file that is opened to setup
;					internal device parameters needed for reading data buffers,
;					such as buffer size and device-specific buffer organization.

function MicroDAQ_HDF_DEVICE::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'MicroDAQ_HDF_DEVICE::read_setup',['IDL run-time error caught.', '', $
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
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

		n_guide = 50000L
		progress_file = 2				; special case, advance by spec #

		stat = fstat(unit)
 		close, unit
	    mca_id = HDF_SD_START(stat.name, /read)
	    HDF_SD_FILEINFO, mca_id, NumSDS, attributes
	    if NumSDS lt 1 then goto, bad_io

		i_buffer = 0L
		openr, unit, stat.name

    ;	get the indexes to the data
	    offset_id = HDF_SD_NAMETOINDEX(mca_id, 'OFFSET')
	    slope_id = HDF_SD_NAMETOINDEX(mca_id, 'SLOPE')
	    quad_id = HDF_SD_NAMETOINDEX(mca_id, 'QUAD')
	    units_id = HDF_SD_NAMETOINDEX(mca_id, 'UNITS')
	    i0_id = HDF_SD_NAMETOINDEX(mca_id, 'i0')
	    spectra_id = HDF_SD_NAMETOINDEX(mca_id, 'mca data')

    ;	select the SDs
	    sdsOFFSET = HDF_SD_SELECT(mca_id, offset_id)
	    sdsSLOPE = HDF_SD_SELECT(mca_id, slope_id)
	    sdsQUAD = HDF_SD_SELECT(mca_id, quad_id)
	    sdsUNITS = HDF_SD_SELECT(mca_id, units_id)
	    sdsI0 = HDF_SD_SELECT(mca_id, i0_id)
	    sdsSPECTRA = HDF_SD_SELECT(mca_id, spectra_id)

    ;	get the mca calibration info
	    hdf_sd_getdata, sdsOFFSET, OFFSET
	    hdf_sd_getdata, sdsSLOPE, SLOPE
	    hdf_sd_getdata, sdsQUAD, QUAD
	    hdf_sd_getdata, sdsUNITS, UNITS
	    hdf_sd_getdata, sdsI0, I0

		n = min([geopixe_max_adcs,n_elements(OFFSET),32])
		if n ne 1 then begin
			warning,'Device_Specific',['More than one detector.','Check format of pixel spectra data']
		endif

    ;	read the HDF file again to figure out how many point there are
	    title_id = HDF_SD_ATTRFIND(mca_id,'Image title')
	    ncols_id = HDF_SD_ATTRFIND(mca_id,'Number of columns')
	    nrows_id = HDF_SD_ATTRFIND(mca_id,'Number of rows')
	    time_id  = HDF_SD_ATTRFIND(mca_id, 'Preset Real Time (msec)')
	    start1_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 1 Start')
	    stop1_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 1 Stop')
	    start2_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 2 Start')
	    stop2_id  = HDF_SD_ATTRFIND(mca_id, 'Motor 2 Stop')

	    HDF_SD_ATTRINFO, mca_id, title_id, DATA=title
		HDF_SD_ATTRINFO, mca_id, ncols_id, DATA=ncols
	    HDF_SD_ATTRINFO, mca_id, nrows_id, DATA=nrows
	    HDF_SD_ATTRINFO, mca_id, time_id, DATA=ntime
	    HDF_SD_ATTRINFO, mca_id, start1_id, DATA=start1
	    HDF_SD_ATTRINFO, mca_id, stop1_id, DATA=stop1
	    HDF_SD_ATTRINFO, mca_id, start2_id, DATA=start2
	    HDF_SD_ATTRINFO, mca_id, stop2_id, DATA=stop2

		nsls_x_range = long(ncols[0])
		nsls_y_range = long(nrows[0])
		nsls_dets = n
		nsls_chans = 2048
		nsls_x = 0
		nsls_y = 0

		nsls_flux_scale = 1.0
		flux2 = reform(I0, nCOLS,nROWS) * nsls_flux_scale

		if (n_elements(flux) gt 1) and (n_elements(flux2) gt 1) then begin
			nx2 = n_elements(flux[*,0])
			ny2 = n_elements(flux[0,*])
			nx = nCOLS
			ny = nROWS
			sx = nx/nx2
			sy = ny/ny2
			nx3 = sx*nx2
			ny3 = sy*ny2
			flux[*,*] = rebin( flux2[0:nx3-1,0:ny3-1], nx2,ny2)
		endif

;	Is the flux array dead-time corrected ("live" flux)? If so, leave dead_fraction zero.
;	If not, we'll need to gather dead_fraction here or in read_buffer().

		nsls_dead = 0.0
;		nsls_dead = (time.real_time - time.live_time)/time.real_time
;		q = where(finite(nsls_dead) eq 0)
;		if q[0] ne -1 then nsls_dead[q]=0.0

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

;	To overcome compression quirk, read the entire image array here ...
;	This will take up a lot of memory (approaching 1 Gb) for individual detectors.

		s = nsls_x_range * nsls_x_range * 2048L * 4L /1000000L
		if s gt 300 then begin
			warning,'MicroDAQ_HDF_DEVICE::read_setup','array in memory will be ',s,'Mbytes in size ...'
		endif
        nsls_read_line = 0
    	hdf_sd_getdata, sdsSPECTRA, mca_spectra, start=[0,0], count=[nsls_x_range*nsls_y_range,2048]

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

; read_buffer()		called repeatedly to read buffers from the data file, process
;					these to extract X,Y,E triplet data, tagged by detector channel, ste,
;					compress X,Y,E if needed, and optionally detect other
;					information (e.g. flux/charge, energy tokens). 

function MicroDAQ_HDF_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
;   file		filename passed for multi-file XY checking
;   channel_on	desired ADC channel(s) (these start at zero, after an optional offset)
;   xcompress	desired X axis compression
;   ycompress	desired Y axis compression
;   ecompress	desired E axis compression (needs to match DA energy calibration)
;   xoffset		offset X by this (i.e. subtract this)
;   yoffset		offset Y by this
;   /raw_xy		suppresses X,Y compression and offset
;   flux		for some flux is an array that comes in to be updated with pixel flux
;   dead_fraction for some this is an array that comes in to be updated with pixel dead_fraction
;				If flux is already DT corrected, "live" flux, then set dead-fraction zero.
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
		warning,'MicroDAQ_HDF_DEVICE::read_buffer',['IDL run-time error caught.', '', $
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
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

	on_ioerror, bad_io
	nc = n_elements(channel_on)

	   if (nsls_x ge nsls_x_range-1) and (nsls_y ge nsls_y_range-1) then begin
;		    print,'x,y=',nsls_x,nsls_y
			HDF_SD_ENDACCESS, sdsSPECTRA
		    HDF_SD_END, mca_id
		    skip_lun, unit, /EOF
		    goto, bad_io
	   endif

	   read_by_line = 0
		if read_by_line and nsls_read_line then begin
	    	hdf_sd_getdata, sdsSPECTRA, mca_spectra, start=[i_buffer,0], count=[nsls_x_range,2048]
			nsls_read_line = 0
		endif

; Only seem to have it set-up for one detector?
; What about dead_fraction?

	   if read_by_line then begin
;	       multiple = long( reform(mca_spectra[nsls_x,*], nsls_chans*nsls_dets))
	       multiple = long( reform(mca_spectra[nsls_x,*], nsls_chans))
	   endif else begin
	       multiple = long( reform(mca_spectra[nsls_x + nsls_y*nsls_x_range,*], nsls_chans))
	   endelse

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
         nsls_read_line = 1
       endif
       i_buffer = i_buffer+1

       q = where((multiple gt 0) and channel_on[ste], count)
       if count gt 0 then begin
         e = e[q]
         ste = ste[q]
         x1 = x1[q]
         y1 = y1[q]
         multiple = multiple[q]
         good = count
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

function MicroDAQ_HDF_DEVICE::import_spec, name, file, group=group

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
		warning,'MicroDAQ_HDF_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'nsls_hdf_evt': begin						; 41
			warning,'MicroDAQ_HDF_DEVICE::import_spec',['"nsls_hdf_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function MicroDAQ_HDF_DEVICE::get_import_list, error=error

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
		warning,'MicroDAQ_HDF_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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

	opt_41 = define(/import)			; NSLS HDF image file
		opt_41.name =		'nsls_hdf_evt'		; unique name of import
		opt_41.title =		'Extract from NSLS image HDF file'
		opt_41.in_ext =		'.hdf'		; input file extension
		opt_41.request =	'Select NSLS HDF file to scan for all spectra'
		opt_41.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_41.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
	;	opt_41.use_IC =		1			; pop-up the flux_select PV selection panel
	
	opt = [opt_41]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function MicroDAQ_HDF_DEVICE::init

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
		warning,'MicroDAQ_HDF_DEVICE::init',['IDL run-time error caught.', '', $
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
;	"MicroDAQ_HDF_DEVICE" --> NSLS_HDF_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'MICRODAQ_HDF_DEVICE', $	; unique name for this device object
		title = 'MicroDAQ HDF - SXRF image data file', $
		ext = '.hdf', $			; a fixed file extension for raw data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; list-mode data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 1, $		; start detector ADC #'s at 1
		use_bounds = 0, $		; not confine charge/flux within bounded area
		synchrotron = 1, $		; synchrotron data
		ionbeam = 0)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro MicroDAQ_HDF_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('MicroDAQ_HDF_DEVICE')

COMPILE_OPT STRICTARR

maia = {MicroDAQ_HDF_DEVICE,  $

		INHERITS BASE_DEVICE $			; mandatory base device parameters
		}								; see the base_device super-class for details.
	return
end
