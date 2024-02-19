;
; GeoPIXE Device Object for Sandia BD12 Rontec 12 detector array system data
;
; Sandia BD12 is a data acquisition and scanning system developed
; by Rontec for Sandia for general nuclear microprobe imaging based on
; a custom 12-element annular array detector.

pro SANDIA_BD12_DEVICE::cleanup

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
		warning,'SANDIA_BD12_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function SANDIA_BD12_DEVICE::get_header_info, file, output=output, silent=silent, error=error

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
		warning,'SANDIA_BD12_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	if n_elements(silent) lt 1 then silent=0
	error = 1

     mp = get_bd12_header( file, error=error)
     if error then return,0

     n = min([geopixe_max_adcs,12])

;      self.header.cal[0:n-1].on = mp.ADCenabled[0:n-1]

     self.header.cal[0:n-1].on = mp.cal[0:n-1].use
     self.header.cal[0:n-1].a = 0.001 * mp.cal[0:n-1].poly[1]
     self.header.cal[0:n-1].b = 0.001 * mp.cal[0:n-1].poly[0]
     self.header.cal[0:n-1].units = 'keV'

     self.header.scan.on = 1
     self.header.scan.x_pixels = mp.stepsX
     self.header.scan.y_pixels = mp.stepsY
	
	self.header.error = 0
	return, self.header
end

;-------------------------------------------------------------------

function SANDIA_BD12_DEVICE::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'SANDIA_BD12_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_bd12, x_bd12, y_bd12
common c_bd12_2, xinc_bd12, yinc_bd12
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_nac_1, buffer_id_mask, event_id_mask,event_type_mask
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

;		This assumes that the file has been opened already. It reads over the header
;		records and defines ADCs in use. File is finally positioned for data read in
;		"read_buffer".

		head = read_bd12_header( unit, error=error)
		if error then goto, bad_io

		xinc_bd12 = head.incX > 1
		yinc_bd12 = head.incY > 1

		e_mask = '1FFF'xU
		ste_offset = -12
		event_id_mask = 'C000'xU

		n_buffer = 128*1024L			; 256K byte buffers
		n_guide = n_buffer/2
		i_buffer = 0L

		x_bd12 = 0US
		y_bd12 = 0US

		event_array = uintarr( n_buffer)

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function SANDIA_BD12_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
		warning,'SANDIA_BD12_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_bd12, x_bd12, y_bd12
common c_bd12_2, xinc_bd12, yinc_bd12
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_nac_1, buffer_id_mask, event_id_mask,event_type_mask
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

	on_ioerror, bad_io
	nc = n_elements(channel_on)
	
       read_event_buffer, unit, self, n, bytes=2
       if n le 0 then goto, bad_io
       i_buffer = i_buffer+1

       e = uintarr( n_buffer, /nozero)
       ste = uintarr( n_buffer, /nozero)
       x1 = uintarr( n_buffer, /nozero)
       y1 = uintarr( n_buffer, /nozero)

       err = bd12_events( event_array,n_buffer, x_bd12,y_bd12, $
                   channel_on,nc, e,x1,y1,n,ste)

       good = n
       if err ne 0 then begin
         print,'read_buffer: error (',err,') return from bd12_events'
         goto, bad_io
       endif

       if good gt 0 then begin
         e = e[0:n-1]
         x1 = x1[0:n-1] / uint(xinc_bd12)
         y1 = y1[0:n-1] / uint(yinc_bd12)
         ste = ste[0:n-1]

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
       endelse

       processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function SANDIA_BD12_DEVICE::import_spec, name, file, group=group

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
		warning,'SANDIA_BD12_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'sandia_bd12_evt': begin						; 9
			warning,'SANDIA_BD12_DEVICE::import_spec',['"sandia_bd12_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function SANDIA_BD12_DEVICE::get_import_list, error=error

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
		warning,'SANDIA_BD12_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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

	opt_24 = define(/import)			; Sandia Rontec BD12
		opt_24.name =		'sandia_bd12_evt'		; unique name of import
		opt_24.title =		'Extract from Sandia BD12'
		opt_24.in_ext =		'.bd12'		; input file extension
		opt_24.request =	'Select Sandia BD12 file to scan for all spectra'
		opt_24.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_24.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
	
	opt = [opt_24]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function SANDIA_BD12_DEVICE::init

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
		warning,'SANDIA_BD12_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"SANDIA_BD12_DEVICE" --> SANDIA_BD12_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'SANDIA_BD12_DEVICE', $	; unique name for this device object
		title = 'Sandia BD12 - PC data acquisition', $
		ext = '.bd12', $			; not a fixed file extension for blog data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; data written with Linux byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 1, $		; start detector ADC #'s at 0
		array_default = 1, $	; a detect array by default
		synchrotron = 0, $		; synchrotron data
		ionbeam = 1)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro SANDIA_BD12_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('SANDIA_BD12_DEVICE')

COMPILE_OPT STRICTARR

maia = {SANDIA_BD12_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
