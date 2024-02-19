;
; GeoPIXE Device Object for MPsys system data
;
; MPsys is a 4-channel data acquisition and scanning
; system developed by MARC for general nuclear microprobe imaging.
; Data is written by a Unix processor in big-endian byte order.

pro MPSYS_UNIX_DEVICE::cleanup

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
		warning,'MPSYS_UNIX_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function MPSYS_UNIX_DEVICE::get_header_info, file, output=output, silent=silent, error=error

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
		warning,'MPSYS_UNIX_DEVICE::get_header_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	if n_elements(silent) lt 1 then silent=0
	error = 1

	mp = get_mp( file, error=error)
	if error then return,0

	self.header.title = mp.comment
		t = self.header.scan
		struct_assign, mp.scan, t
	self.header.scan = t
	self.header.charge = mp.charge
	self.header.detector = mp.detector
		t = self.header.cal
		struct_assign, mp.cal, t
	self.header.cal = t
	
	self.header.error = 0
	return, self.header
end

;-------------------------------------------------------------------

function MPSYS_UNIX_DEVICE::read_setup, unit, xrange,yrange, first=first, $
			n_guide,progress_file, ystep=ystep, charge=charge, ecompress=ecompress, $
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
		warning,'MPSYS_UNIX_DEVICE::read_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_mpsys_3, mdx_offset,mdy_offset, correct_mdx,correct_mdy
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_mpsys_5c, xlast,xmax,xcount, toggle_last
common c_mpsys_6, shuffle
common c_mpsys_7, mpsys_xrange, mpsys_yrange
if n_elements(ystep) lt 1 then ystep = 0L

;		Assume that x,y are centred on 2048

		mpsys_xrange = xrange
		mpsys_yrange = yrange
		minx = uint(2048 - xrange/2)
		miny = uint(2048 - yrange/2)
		maxx = uint(minx + xrange-1)
		maxy = uint(miny + yrange-1)
		xmax = uint(xrange-1)
		if ystep then xmax = uint(yrange-1)
		xlast = 0US
		xcount = 0L
		toggle_last = 0US

		n_buffer = 50000L				; ~50K x 2 x 3 byte buffers (0.3 MBytes)
		n_guide = n_buffer
		i_buffer = 0L

		event_array = uintarr( 3, n_buffer, /nozero)

	return, 0
end

;-------------------------------------------------------------------

function MPSYS_UNIX_DEVICE::read_buffer, unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
       ecompress=ecompress, total_bad_xy=bad_xy, total_processed=processed, $
       station_e=ste, title=title, step_events=step_events, multiple=multiple, $
       step_toggle=step_toggle, step_count=step_count, toggle_bit=toggle_bit, toggle_adc=toggle_adc, $
       processed=count1, valid=good, raw_xy=raw_xy, ystep=ystep, time=tot, $
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
;   /step_toggle advance step by toggle bit			; these are for X step flagged by
;   toggle_bit	bit # for toggle bit (starts at 0)	; toggling bit in E data
;   toggle_adc	ADC # for toggle bit
;   /ystep		Y step mode, else X
;   /step_events advance step by event count		; toggle by set # events in toggle_adc	
;   step_count	count for step_event				; # events to use with step_events
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
;   valid		number of good events, or zero
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
		warning,'MPSYS_UNIX_DEVICE::read_buffer',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1
	endif
endif
common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_1b, maxx,maxy
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_mpsys_3, mdx_offset,mdy_offset, correct_mdx,correct_mdy
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer
common c_mpsys_5c, xlast,xmax,xcount, toggle_last
common c_mpsys_6, shuffle
common c_mpsys_7, mpsys_xrange, mpsys_yrange
if n_elements(ystep) lt 1 then ystep = 0L
if n_elements(step_toggle) lt 1 then step_toggle=0
if n_elements(step_count) lt 1 then step_count=0
if n_elements(step_events) lt 1 then step_events=0
if n_elements(toggle_adc) lt 1 then toggle_adc=0
if n_elements(toggle_bit) lt 1 then toggle_bit = 12

	on_ioerror, bad_io
	xstep = (step_toggle eq 1) or (step_events eq 1) or (step_count gt 0)
	nc = n_elements(channel_on)

    read_event_buffer, unit, self, n_actual, bytes=6
    if n_actual le 0 then goto, bad_io
    i_buffer = i_buffer+1

    e = uintarr(n_buffer, /nozero)
    x = uintarr(n_buffer, /nozero)
    y = uintarr(n_buffer, /nozero)
    ste = uintarr(n_buffer, /nozero)
    n = 0L

    err = mpsys_events( event_array,n_buffer,channel_on,nc, e,x,y,n,ste, bad_xy, $
                 step_toggle,step_events,step_count, toggle_adc,toggle_bit, $
                 ystep=ystep )

    if err ne 0 then begin
       print,'read_buffer: error (',err,') return from mpsys_events'
       goto, bad_io
    endif

    good = long(n)
    processed = processed + good
    if (good eq 0) and (xstep ne 0) and (xlast gt xmax) then return, 1

    if good gt 0 then begin
       if raw_xy then begin
         x1 = x[0:n-1]
         y1 = y[0:n-1]
         e = e[0:n-1]
       endif else begin
         if (xstep eq 0) then begin
          x1 = ((x[0:n-1] - minx) > 0) / uint(xcompress)
          y1 = ((y[0:n-1] - miny) > 0) / uint(ycompress)
         endif else begin
          if ystep then begin
              x1 = ((x[0:n-1] - minx) > 0) / uint(ycompress)
              y1 = y[0:n-1] / uint(xcompress)
          endif else begin
              x1 = x[0:n-1] / uint(xcompress)
              y1 = ((y[0:n-1] - miny) > 0) / uint(ycompress)
          endelse
         endelse
         e = e[0:n-1] / uint(ecompress)
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
	return, 1
end

;-------------------------------------------------------------------

function MPSYS_UNIX_DEVICE::import_spec, name, file, group=group

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
		warning,'MPSYS_UNIX_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs

	case name of
		'mpsys_unix_img': begin						; 10
			spec_img, file, p=p, /once, /MPsys_unix
;			file = strip_file_ext(file) + 'i.spec'
			index = 0
			goto, header
			end
		'mpsys_unix_mu_ascii': begin				; 6
			get_mu_spec, p, file
			end
		'mpsys_rontec_txt': begin				; 17
			get_rontec_spec, p, file
			end
		'mpsys_unix_evt': begin						; 13
			warning,'MPSYS_UNIX_DEVICE::import_spec',['"mpsys_unix_evt" spectrum import.','This import should use "spec_evt" elsewhere.']
			end
	endcase
	return, p
	
header:
	mp = self->get_header_info(file, /silent, error=error)

	if (error eq 0) then begin
		done = intarr(geopixe_max_adcs)
		for j=0L,n_elements(p)-1 do begin
			adc = clip((*p[j]).station-1,0,geopixe_max_adcs-1)
			(*p[j]).detector = mp.detector[adc]
			if (done[adc] eq 0) and mp.cal[adc].on then begin
				(*p[j]).cal.order = 1
				(*p[j]).cal.units = mp.cal[adc].units
				(*p[j]).cal.poly[0] = mp.cal[adc].b
				(*p[j]).cal.poly[1] = mp.cal[adc].a
			endif
			(*p[j]).comment = mp.title
			if (mp.charge gt 1.0e-6) and ( ((*self.import_list)[index]).use_ic eq 0) then (*p[i]).charge = mp.charge
			done[adc] = 1
		endfor
	endif
	
	return, p
end

;-------------------------------------------------------------------

function MPSYS_UNIX_DEVICE::get_import_list, error=error

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
		warning,'MPSYS_UNIX_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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
	
	opt_10 = define(/import)				; MPsys/Unix IMG
		opt_10.name =		'mpsys_unix_img'		; unique name of import
		opt_10.title =		'MPsys/Unix IMG file'
		opt_10.in_ext =		'.img'			; file extension
		opt_10.request =	'Select IMG to convert and load'
	
	opt_6 = define(/import)					; MU TXT
		opt_6.name =		'mpsys_unix_mu_ascii' ; unique name of import
		opt_6.title =		'MARC ascii spectrum'
		opt_6.in_ext =		'.txt'			; file extension
		opt_6.request =		'Select MU Spectrum to Load'
	
	opt_13 = define(/import)				; MPsys/Unix EVT
		opt_13.name =		'mpsys_unix_evt'		; unique name of import
		opt_13.title =		'Extract from MPsys/Unix EVT'
		opt_13.in_ext =		'.evt'			; input file extension
		opt_13.request =	'Select EVT file to sort for all spectra'
		opt_13.spec_evt =	1				; uses a call to spec_evt to extract from EVT data
	
	opt_17 = define(/import)			; Rontec ASCII spec
		opt_17.name =		'mpsys_rontec_txt'		; unique name of import
		opt_17.title =		'Rontec ASCII spectrum'
		opt_17.in_ext =		'.txt'		; file extension
		opt_17.request =	'Select Rontec ASCII spectrum to load'
	
	opt = [opt_10, opt_6, opt_13, opt_17]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function MPSYS_UNIX_DEVICE::init

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
		warning,'MPSYS_UNIX_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"MPSYS_UNIX_DEVICE" --> MPSYS_UNIX_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'MPSYS_UNIX_DEVICE', $	; unique name for this device object
		title = 'MARC MPsys - Unix data acquisition', $
		ext = '.evt', $			; not a fixed file extension for blog data
		multi_files = 0, $		; multiple segment files per run
		multi_char = '', $		; separates run from segment number in file name
		big_endian = 1, $		; data written with Unix byte order
		vax_float = 0, $		; not VAX floating point
		start_adc = 1, $		; start detector ADC #'s at 0
		synchrotron = 0, $		; synchrotron data
		ionbeam = 1)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro MPSYS_UNIX_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('MPSYS_UNIX_DEVICE')

COMPILE_OPT STRICTARR

maia = {MPSYS_UNIX_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
