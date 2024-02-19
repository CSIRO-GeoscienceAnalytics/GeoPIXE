;
; GeoPIXE Generic Device Object
;
; Generic data reads for GeoPIXE native spectra files, and
; generic ASCII files.

pro GENERIC_DEVICE::cleanup

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
		warning,'GENERIC_DEVICE::cleanup',['IDL run-time error caught.', '', $
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

function GENERIC_DEVICE::import_spec, name, file, group=group

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
		warning,'GENERIC_DEVICE::get_import_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	case name of
		'generic_geopixe_spec': begin				; 0
			p = read_spec( file)
			end
		'generic_geopixe_trav': begin				; 2
			p = read_spec( file)
			end
		'generic_old_geopixe_ospec': begin			; 3
			get_old_spec, p, file
			end
		'generic_spec_fit': begin					; 5
			get_old_spec, p, file, /fit
			end
		'generic_old_geopixe_spec': begin			; 15
			get_csiro_dat, p, file, group=group
			end
		'generic_old_geopixe_spec_fit': begin		; 16
			get_csiro_dat, p, file, /fit, group=group
			end
		'generic_ascii': begin						; 4
			get_ascii_spec, p, file
			end
		'generic_ascii_channel': begin				; 36
			get_ascii_spec, p, file, /channel
			end
		'generic_ascii_energy': begin				; 14
			get_ascii_spec, p, file, /energy
			end
		'generic_ascii_channel_energy': begin		; 30
			get_ascii_spec, p, file, /energy, /channel
			end
		'generic_delta_ascii': begin	
			get_delta_ascii_spec, p, file
			end
	endcase
	
	return, p
end

;-------------------------------------------------------------------

function GENERIC_DEVICE::get_import_list, error=error

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
		warning,'GENERIC_DEVICE::get_import_list',['IDL run-time error caught.', '', $
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

; The GENERIC_DEVICE is a special case, just to handle these import spectra
; options. DO NOT SET the flag 'spec_evt' in any of these.

	opt_0 = define(/import)				; native GeoPIXE SPEC files
		opt_0.name =		'generic_geopixe_spec' 	; unique name of import
		opt_0.title =		'GeoPIXE native SPEC file'
		opt_0.in_ext	=	'.spec'		; file extension
		opt_0.request =		'Select Spectrum to Load'
		opt_0.preview =		1			; allow spectrum preview
	
	opt_2 = define(/import)				; TRAV
		opt_2.name =		'generic_geopixe_trav' 	; unique name of import
		opt_2.title =		'GeoPIXE native TRAV file'
		opt_2.in_ext =		'.trav'		; file extension
		opt_2.request =		'Select Traverse to Load'
		opt_2.preview =		1			; allow spectrum preview
	
	opt_4 = define(/import)				; ASCII spec
		opt_4.name =		'generic_ascii' 	; unique name of import
		opt_4.title =		'ASCII spectrum (Data only)'
		opt_4.in_ext =		''			; file extension
		opt_4.request =		'Select ASCII Spectrum to Load'
	
	opt_36 = define(/import)			; ASCII Spec (with channel column)
		opt_36.name =		'generic_ascii_channel' 	; unique name of import
		opt_36.title =		'ASCII spectrum (Channel, Data)'
		opt_36.in_ext =		''			; file extension
		opt_36.request =	'Select ASCII Spec (with channel column) to Load'
	
	opt_14 = define(/import)			; ASCII spec, with energy
		opt_14.name =		'generic_ascii_energy' 	; unique name of import
		opt_14.title =		'ASCII spectrum (Energy, Data)'
		opt_14.in_ext =		'.csv'		; file extension
		opt_14.request =	'Select ASCII Spec (with energy column) to Load'
	
	opt_30 = define(/import)			; ASCII spec, with channel, energy
		opt_30.name =		'generic_ascii_channel_energy' 	; unique name of import
		opt_30.title =		'ASCII spectrum (Channel, Energy, data)'
		opt_30.in_ext =		''			; file extension
		opt_30.request =	'Select ASCII Spec (w/ channel, energy cols) to Load'
	
	opt_15 = define(/import)			; CSIRO old GeoPIXE spectra
		opt_15.name =		'generic_old_geopixe_spec' 	; unique name of import
		opt_15.title =		'old GeoPIXE raw spectrum file'
		opt_15.in_ext =		''			; file extension
		opt_15.request =	'Select old GeoPIXE spectrum to Load'
	
	opt_16 = define(/import)			; old GeoPIXE fit
		opt_16.name =		'generic_old_geopixe_spec_fit' 	; unique name of import
		opt_16.title =		'Old GeoPIXE fitted spectrum DAT file'
		opt_16.in_ext =		'.dat'		; file extension
		opt_16.request =	'Select old GeoPIXE spectrum to Load'
	
	opt_91 = define(/import)								; ASCII spec
		opt_91.name =		'generic_delta_ascii' 			; unique name of import
		opt_91.title =		'Delta pXRF CSV spectrum'
		opt_91.in_ext =		'.csv'							; file extension
		opt_91.request =	'Select CSV Spectrum file to Load'

;	opt_3 = define(/import)				; old GeoPIXE spec
;		opt_3.name =		'generic_old_geopixe_ospec' 	; unique name of import
;		opt_3.title =		'Old GeoPIXE OSPEC file'
;		opt_3.in_ext =		'.ospec'	; file extension
;		opt_3.request =		'Select Old Spectrum to Load'
;	
;	opt_5 = define(/import)				; old spec, fit
;		opt_5.name =		'generic_spec_fit' 	; unique name of import
;		opt_5.title =		'Old GeoPIXE OPEC file w/ fit'
;		opt_5.in_ext =		'.ospec'	; file extension
;		opt_5.request =		'Select Old Fitted Spectrum to Load'	
	
	opt = [opt_4, opt_36, opt_14, opt_30, opt_91, opt_0, opt_2, opt_15, opt_16]
	for i=0L,n_elements(opt)-1 do opt[i].device_name = self.name

	self.import_list = ptr_new(opt)
	error = 0
	return, opt
end

;-------------------------------------------------------------------

function GENERIC_DEVICE::init

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
		warning,'GENERIC_DEVICE::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

;	Pass core device parameters to base DEVICE superclass
;	Note that 'name' must match the object's file-name:
;	"GENERIC_DEVICE" --> GENERIC_DEVICE_define.sav
;	and 'name' must contain the string "_DEVICE".
	 
	i = self->BASE_DEVICE::init(  $
		name = 'GENERIC_DEVICE', $	; unique name for this device object
		title = 'Generic data and native GeoPIXE', $
		synchrotron = 1, $		; synchrotron data
		ionbeam = 1)			; ion-beam data
	return, i
end

;-------------------------------------------------------------------

pro GENERIC_DEVICE__define

; Define Maia device object internal data structure.
; Only called using obj = obj_new('GENERIC_DEVICE')

COMPILE_OPT STRICTARR

maia = {GENERIC_DEVICE,  $

		INHERITS BASE_DEVICE $		; mandatory base device parameters
		}
	return
end
