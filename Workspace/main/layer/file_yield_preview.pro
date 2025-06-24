pro file_yield_preview, file, preview=preview

; Provide preview of Yield file details for 'file_requester' from 'file'.
; Return preview data in 'preview' struct.

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'file_yield_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
define_devices

	preview = 0L
	if n_elements(file) lt 1 then return
	
	p = read_yield( file, error=error)

	if error ne 0 then return
	if ptr_valid(p) eq 0 then return

;------------------------------------------------------------
; Details string array:

	list = ['File: ' + strip_path(file),'Title: '+ (*p).title,'']
	if ((*p).z1 eq 0) and ((*p).a1 eq 0) then begin
		list = [list, (*p).beam.continuum ? 'Continuum source' : 'Monochromatic source', $
			'Energy: ' + str_tidy((*p).beam.energy) + '  (Beam for "mono" and maximum for "contuinuum")', '']
	endif else begin
		list = [list, 'Beam: Z1 = ' + str_tidy((*p).z1) + ', A1 = ' + str_tidy((*p).a1)]
		list = [list, 'Energy: ' + str_tidy((*p).e_beam), '']
	endelse
	list = [list, 'Theta: ' + str_tidy((*p).theta) + ', Phi: ' + str_tidy((*p).phi)]
	list = [list, 'Alpha: ' + str_tidy((*p).alpha) + ', Beta: ' + str_tidy((*p).beta),'']

	if (*p).array eq 1 then begin
		list = [list, 'Array detector: ' + str_tidy((*p).detector_file), '']
	endif

	list = [list, 'Number of layers: '  + str_tidy((*p).n_layers)] 
	for i=0,(*p).n_layers-1 do begin
		list = [list, '    ' + (*p).layers[i].name] 
		units = (*p).microns[i] ? '(microns)' : '(mg/cm^2)'
		list = [list, '        Thickness: ' + str_tidy((*p).thick[i]) + ' ' + units] 
	endfor

;------------------------------------------------------------

	ptr_free, p

	preview = {details:list}
	return
end