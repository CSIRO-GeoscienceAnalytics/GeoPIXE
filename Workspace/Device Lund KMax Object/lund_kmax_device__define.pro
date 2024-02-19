;
; GeoPIXE Device Object for Lund-Kmax system data
; 
; KMax Macintosh based data acquisition and scanning
; system developed by Lund for general nuclear microprobe imaging.
; Data is written by a Mac front-end processor in big-endian byte order.

pro lund_kmax_device::cleanup

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
		warning,'lund_kmax_device::cleanup',['IDL run-time error caught.', '', $
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

function lund_kmax_device::read_setup, unit, xrange,yrange, first=first, $
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
		warning,'lund_kmax_device::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_lund_1, bufstart
common c_lund_2, lund_format, lund_offset
common c_lund_3, lund_size
common c_lund_4, lund_pixe_event

	on_ioerror, bad_io
;		event_header1 = bytarr(362)		; These seem to have sizes out by 8 bytes
;		eventsize1 = intarr(185)

		event_header1 = bytarr(354)		; To adjust start of sizes array
		event_size = intarr(32)
		event_header2 = intarr(157)
		lastword1 = bytarr(2)
		bufstart = lonarr(2)
		n_guide = 75000L				; ~50K x 4 x 6 byte buffers (1.2 Mbyte)

		readu, unit, event_header1, event_size, event_header2, lastword1

;		readu, unit, event_header1, event_size, lastword1
;		swap_bytes, event_header1, big_endian_data=self->big_endian()
;		swap_bytes, event_header2, big_endian_data=self->big_endian()
;		swap_bytes, lastword1, big_endian_data=self->big_endian()

		swap_bytes, event_size, big_endian_data=self->big_endian()

;		lund_format = {x:0L, y:0L, stim:0L, PIXE:0L, BS:0L, charge:0L}
;		lund_size = 6					; for event type 7

		lund_size = event_size			; size for all event types (PIXE = 7)
		lund_offset = 2					; offset over x,y to data ADCs
		lund_pixe_event = 7				; ID of a PIXE event (actually ALL ADC data)

	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function lund_kmax_device::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
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
		warning,'lund_kmax_device::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_lund_1, bufstart
common c_lund_2, lund_format, lund_offset
common c_lund_3, lund_size
common c_lund_4, lund_pixe_event

	on_ioerror, bad_io
	nc = n_elements(channel_on)

   readu, unit, bufstart
   swap_bytes, bufstart, big_endian_data=self->big_endian()

   bufsize = bufstart[1]
   buftype = bufstart[0]
   evtsize = lund_size[buftype-1]
   n = bufsize
   good = 0L
   n = 0L
   if (bufsize le 0) or (evtsize lt 1) then begin
     error = 1
     return, 1
   endif

   buffer = lonarr( evtsize, bufsize, /nozero)

   readu, unit, buffer
   swap_bytes, buffer, big_endian_data=self->big_endian()

   if buftype ne lund_pixe_event then return,1        ; PIXE event buffer

   if lund_offset ge geopixe_max_adcs then return,1
   qc = where( channel_on eq 1, nqc)
   if nqc eq 0 then return,1
   start = 1
   good = 0L
   nqc = min([nqc,evtsize-lund_offset])

   for i=0L,nqc-1 do begin
     j = lund_offset + qc[i]                    ; Lund ADC #
     q = where( buffer[j,*] ne 0, nq)

     if nq ne 0 then begin
      if start then begin
          ste = uint(replicate(qc[i],nq))

          if raw_xy eq 0 then begin
             x1 = reform( uint(buffer[0,q] / uint(xcompress)))
             y1 = reform( uint(buffer[1,q] / uint(ycompress)))
             e = reform( uint(buffer[j,q] / uint(ecompress)))
          endif else begin
             x1 = reform( uint(buffer[0,q]))
             y1 = reform( uint(buffer[1,q]))
             e = reform( uint(buffer[j,q]))
          endelse
          start = 0
      endif else begin
          ste = [ste, uint(replicate(qc[i],nq))]

          if raw_xy eq 0 then begin
             x1 = [x1, reform( uint(buffer[0,q] / uint(xcompress))) ]
             y1 = [y1, reform( uint(buffer[1,q] / uint(ycompress))) ]
             e = [e, reform( uint(buffer[j,q] / uint(ecompress))) ]
          endif else begin
             x1 = [x1, reform( uint(buffer[0,q])) ]
             y1 = [y1, reform( uint(buffer[1,q])) ]
             e = [e, reform( uint(buffer[j,q])) ]
          endelse
      endelse
      good = good + nq
     endif
   endfor
   n = good

   processed = processed + good
	return, 0
	
bad_io:
	return, 1
end

;-------------------------------------------------------------------

function lund_kmax_device::import_spec, name, file, group=group

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
		warning,'lund_kmax_device::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'lund_kmax_hist': begin						; 7
			get_lund_spec, p, file, group=group
			end
		'lund_kmax_ascii': begin					; 32
			get_lund_ascii_spec, p, file
			end
		'lund_kmax_evt': begin						; 11
			warning,'lund_kmax_device::import_spec',['"lund_kmax_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
end

;-------------------------------------------------------------------

function lund_kmax_device::get_import_list, error=error

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
		warning,'lund_kmax_device::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_7 = define(/import)				; Lund HIST
		opt_7.name =		'lund_kmax_hist'		; unique name of import
		opt_7.title =		'Lund KMax/Mac HIST file'
		opt_7.in_ext =		'.hist'		; file extension
		opt_7.request =		'Select Lund HIST Spectrum to Load'

	opt_32 = define(/import)			; ASCII spec, with energy
		opt_32.name =		'lund_kmax_ascii'		; unique name of import
		opt_32.title =		'Lund KMax/Mac ASCII file'
		opt_32.in_ext =		'.txt'		; file extension
		opt_32.request =	'Select Lund KMax ASCII file(s) to load'

	opt_11 = define(/import)			; Lund list-mode
		opt_11.name =		'lund_kmax_evt'		; unique name of import
		opt_11.title =		'Extract from KMax/Mac List Mode'
		opt_11.in_ext =		''			; input file extension
		opt_11.request =	'Select first Lund KMax list-mode file'
		opt_11.raw =		1			; flags use of separate Raw data path '(*pstate).dpath'
		opt_11.spec_evt =	1			; uses a call to spec_evt to extract from EVT data
		opt_11.multifile =	1			; denotes data in a series of more than one file
		opt_11.separate =	'%'			; char between file and run #

	opt = [opt_7, opt_32, opt_11]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function lund_kmax_device::init

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
		warning,'lund_kmax_device::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"LUND_KMAX_DEVICE" --> MAIA_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'LUND_KMAX_DEVICE', $	; unique name for this device object
		title = 'Lund KMax - MAC data acquisition', $
		ext = '', $				; not a fixed file extension for data
		multi_files = 1, $		; multiple segment files per run
		multi_char = '%', $		; separates run from segment number in file name
		big_endian = 1, $		; blog data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 3, $		; start detector ADC #'s at 0
		synchrotron = 0, $		; synchrotron data
		ionbeam = 1 $			; ion-beam data
		 )
	return, i
end

;-------------------------------------------------------------------

pro lund_kmax_device__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('LUND_KMAX_DEVICE')

COMPILE_OPT STRICTARR

maia = {LUND_KMAX_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
