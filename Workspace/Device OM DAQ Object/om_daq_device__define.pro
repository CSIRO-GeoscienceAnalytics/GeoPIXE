;
; GeoPIXE Device Object for OM DAQ system data
; 
; Based on the OM DAQ SDK guide and header info in "Dqp_DataIO.h"
; (see Oxford Microbeams for further information).
;
; OM DAQ is a 8-channel PC data acquisition and scanning
; system developed by Oxford Microbeams for general nuclear microprobe imaging.
; Data is written by a PC processor in little-endian byte order.

pro OM_DAQ_DEVICE::cleanup

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
		warning,'OM_DAQ_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function OM_DAQ_DEVICE::get_header_info, file, output=output, silent=silent, error=error

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
		warning,'OM_DAQ_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
    common c_geopixe_adcs, geopixe_max_adcs

	if n_elements(silent) lt 1 then silent=0
	error = 1

         mp = get_om_header( file, error=error)
         if error then return, 0

         n = min([geopixe_max_adcs, 8])
         self.header.scan.on = 1
         self.header.scan.x_pixels = mp.n_x
         self.header.scan.y_pixels = mp.n_y
         self.header.scan.x_mm = mp.x_size * 0.001
         self.header.scan.y_mm = mp.y_size * 0.001
         self.header.title = mp.title
         self.header.detector[0:n-1] = 0

         q = where(mp.cal[1,0:n-1] gt 1.0e-6)
         if q[0] ne -1 then begin
          self.header.cal[q].on = 1
          self.header.cal[q].a = reform([mp.cal[1,q]])
          self.header.cal[q].b = reform([mp.cal[0,q]])
          self.header.cal[q].units = 'channel'
          q2 = where( abs(mp.cal[1,q] - 1.0) gt 0.001)
          if q2[0] ne -1 then begin
              self.header.cal[q[q2]].units = 'keV'
          endif
         endif

	self.header.error = 0
	return, self.header
end

;-------------------------------------------------------------------

function OM_DAQ_DEVICE::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'OM_DAQ_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_om_1, lmf_block
common c_om_2, lmf_cal
common c_om_3b, c_per_pulse, lmf_charge
common c_om_4, lmf_size
common c_om_5, lmf_live
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset

; Note that this call to read_om_header defines the e_mask, ste_mask values
; depending on LMF version, and it ignores 'device'.

	head  = read_om_header( device, unit, error=error)
	if error then return, 1

	case head.LMF_version of
		0: begin
			n = (head.block_size - 5*4)/4
			lmf_data = replicate( {x:0ub, y:0ub, e:0u}, n)
			lmf_block = { charge:0L, elapsed:0L, live:0L, live0:0L, live1:0L, data:lmf_data}
			end
		1: begin
			n = (head.block_size - 5*4)/4
			lmf_data = replicate( {x:0ub, y:0ub, e:0u}, n)
			lmf_block = { charge:0L, elapsed:0L, live:0L, live0:0L, live1:0L, data:lmf_data}
			end
		2: begin
			n = (head.block_size - 5*4)/6
			lmf_data = replicate( {x:0u, y:0u, e:0u}, n)
			lmf_block = { charge:0L, elapsed:0L, live:0L, live0:0L, live1:0L, data:lmf_data}
			end
		3: begin
			n = (head.block_size - 37*4)/8
			lmf_data = replicate( {x:0u, y:0u, e:0UL}, n)
			lmf_block = { charge:0L, elapsed:0L, live:0L, live0:0L, live1:0L, timing:lon64arr(2,8), data:lmf_data}
			end
		4: begin
			n = (head.block_size - 37*4)/8
			lmf_data = replicate( {xycount:0UL, e:0UL}, n)
			if head.block_size - 37*4 gt 8*n then begin
				fill = bytarr( head.block_size - 37*4 - 8*n)
				lmf_block = { charge:0L, elapsed:0L, live:0L, live0:0L, live1:0L, timing:lon64arr(2,8), data:lmf_data, fill:fill}
			endif else begin
				lmf_block = { charge:0L, elapsed:0L, live:0L, live0:0L, live1:0L, timing:lon64arr(2,8), data:lmf_data}
			endelse
			end
	endcase

	n_guide = head.block_size/4		; ~50K x 4 x 6 byte buffers (1.2 Mbyte)
	lmf_charge = -1.0
	
	return, 0
end

;-------------------------------------------------------------------

function OM_DAQ_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
		warning,'OM_DAQ_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_om_1, lmf_block
common c_om_2, lmf_cal
common c_om_3b, c_per_pulse, lmf_charge
common c_om_4, lmf_size
common c_om_5, lmf_live
common c_om_6, LMF_version, count_offset, count_mask, lmf_pixels
common c_om_7, LMF_x, LMF_y
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset

	on_ioerror, bad_io
	nc = n_elements(channel_on)

	readu, unit, LMF_block
	swap_bytes, LMF_block, big_endian_data=self->big_endian()

	lmf_live = {elapsed:float(LMF_block.elapsed)*1.0e-5, live:float(LMF_block.live)*1.0e-5, live0:float(LMF_block.live0)*1.0e-5, live1:float(LMF_block.live1)*1.0e-5}

;	help, LMF_block[0], n_elements(LMF_block.data), lmf_live[0], lmf_charge, /str
	
	if lmf_charge eq -1.0 then begin
		lmf_charge = 0.0
	endif else begin
		if LMF_block.charge eq 0 then begin
			print, 'Zero Charge for elapsed time = ', float(LMF_block.elapsed)*1.0e-5
		endif else begin
			lmf_charge = c_per_pulse * float( LMF_block.charge)
		endelse
	endelse
;	print,'LMF_charge = ',lmf_charge
	
	e = uint( LMF_block.data.e and e_mask)
	ste = uint( ishft( LMF_block.data.e and ste_mask, ste_offset))
	q = where( e ne e_mask)
	n = 0L

	if q[0] ne -1 then begin
		if LMF_version ge 3 then begin
			multiple = long( ishft( LMF_block.data.e and count_mask, count_offset) +1)
		endif
		if LMF_version le 3 then begin
			x1 = uint( LMF_block.data[q].x)
			y1 = uint( LMF_block.data[q].y)
		endif else begin
			c = ulong( LMF_block.data[q].xycount)
			y1 = LMF_x[c]
			x1 = LMF_y[c]
		endelse
		e = e[q]
		ste = ste[q]
		n = n_elements(q)
	endif
	good = long(n)

;	qt = where(c ne 0, nqt)
;	if nqt gt 0 then begin
;		print,'non zero "c" ...'
;	endif

	if good gt 0 then begin
		qc = where((ste[0:good-1] ge 0) and (ste[0:good-1] lt geopixe_max_adcs), nqc)
		good = nqc
		if nqc gt 0 then begin
			q = where( channel_on[ste[qc]] eq 1, nq)
			good = nq
			if nq gt 0 then begin
				ste = ste[qc[q]]
				if raw_xy eq 0 then begin
					e = e[qc[q]] / uint(xcompress)
					x1 = x1[qc[q]] / uint(xcompress)
					y1 = y1[qc[q]] / uint(ycompress)
				endif else begin
					e = e[qc[q]]
					x1 = x1[qc[q]]
					y1 = y1[qc[q]]
				endelse
			endif
		endif
	endif else begin
		x1 = 0US
		y1 = 0US
		e = 0US
		ste = 0US
	endelse
	n = good

	processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function OM_DAQ_DEVICE::import_spec, name, file, group=group

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
		warning,'OM_DAQ_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'om_daq_rpt': begin						; 19
			get_ansto_rpt, p, file
			end
		'om_daq_evt': begin						; 18
			warning,'OM_DAQ_DEVICE::import_spec',['"om_daq_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function OM_DAQ_DEVICE::get_import_list, error=error

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
		warning,'OM_DAQ_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_19 = define(/import)			; ANSTO RPT spec
		opt_19.name =		'om_daq_rpt'		; unique name of import
		opt_19.title =		'Extract from OM DAQ/PC LMF'
		opt_19.in_ext =		'.rpt'		; file extension
		opt_19.request =	'Select RPT spectrum file to load'
		opt_19.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'

	opt_18 = define(/import)			; OM DAQ EVT
		opt_18.name =		'om_daq_evt'		; unique name of import
		opt_18.title =		'Extract from OM DAQ/PC LMF'
		opt_18.in_ext =		'.lmf'		; input file extension
		opt_18.request =	'Select LMF file to sort for all spectra'
		opt_18.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_18.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
	
	opt = [opt_18]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function OM_DAQ_DEVICE::init

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
		warning,'OM_DAQ_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"OM_DAQ_DEVICE" --> OM_DAQ_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'OM_DAQ_DEVICE', $	; unique name for this device object
		title = 'OM DAQ - PC data acquisition', $
		ext = '.lmf', $			; not a fixed file extension for blog data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 0, $		; data written with Linux byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 0, $		; start detector ADC #'s at 0
		synchrotron = 0, $		; synchrotron data
		ionbeam = 1)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro OM_DAQ_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('OM_DAQ_DEVICE')

COMPILE_OPT STRICTARR

maia = {OM_DAQ_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
