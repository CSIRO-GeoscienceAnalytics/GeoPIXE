pro file_source_preview, file, preview=preview

; Provide preview of Source/Pink beam details for 'file_requester' from 'file'.
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
		warning,'file_source_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
define_devices

	preview = 0L
	if n_elements(file) lt 1 then return
	
	case extract_extension(file) of
		'source': begin
			p = ptr_new( read_source( file, error=error))
			end
		'pink': begin
			p = ptr_new( read_pink( file, error=error))
			end
		else: return
	endcase

	if error ne 0 then return
	if ptr_valid(p) eq 0 then return

;------------------------------------------------------------
; Details string array:

	list = ['File: ' + strip_path(file),'Title: '+ (*p).title]
	list = [list, (*p).continuum ? 'Continuum source' : 'Monochromatic source', $
			'Model: ' + str_tidy((*p).model) + '  (model 1: Lab Source, 2: Pink Beam)', $
			'Energy: ' + str_tidy((*p).energy) + '  (Beam for "mono" and maximum for "contuinuum")', '']
			
;	Lab source
	if ((*p).model eq 1) then begin
		list = [list, 'Volts: '  + str_tidy((*p).modata.volts)] 
		list = [list, 'Power: '  + str_tidy((*p).modata.power)] 
		list = [list, 'Anode: '  + (*p).modata.anode.name] 
		list = [list, '    Formula: '  + (*p).modata.anode.formula, ''] 
 	endif

;	Pink beam
	if ((*p).model eq 2) then begin
		list = [list, 'Frontend spectrum: '  + (*p).fe_spectrum_file] 

		list = [list, 'Number of mirrors: '  + str_tidy((*p).n_mirrors)] 
		for i=0,(*p).n_mirrors-1 do begin
			list = [list, '    ' + (*p).mirrors[i].title + ' - ' + (*p).mirrors[i].file] 
		endfor
	endif

	if (*p).poly.mode eq 1 then begin
		list = [list, 'Polycapillary: '  + str_tidy((*p).poly.model)]
		list = [list, '    Gain: '  + str_tidy((*p).poly.gain)]
		list = [list, '    Energy: '  + str_tidy((*p).poly.energy)]
	endif

;------------------------------------------------------------

	ptr_free, p

	preview = {details:list}
	return
end