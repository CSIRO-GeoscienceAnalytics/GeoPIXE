function read_correct, F, silent=silent, mode=mode, error=error

;	Read yield correction parameters for Correct_Yield
;
; mode=	1 	for image correction matrix data
;		0	for simple composition correction matrix

COMPILE_OPT STRICTARR
ErrorNo = 0
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
		warning,'Read_correct',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

error = 1
if n_params() lt 1 then return, 0
if lenchr(F) lt 1 then return, 0
if n_elements(silent) eq 0 then silent=0
if n_elements(mode) eq 0 then mode=1

valid = [-1,-2]					; valid versions

	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	version = 0L
	readu,1, version
	q = where( version eq valid)
	if q[0] eq -1 then return, 0

	n_comp = 0L
	max_comp = 0L
	readu,1, n_comp, max_comp
	if (n_comp lt 0) or (n_comp gt max_comp) or (n_comp gt 32) then goto, bad_io

	comp = strarr(n_comp)
	minerals = strarr(n_comp)
	original = ''
	files = strarr(n_comp)
	R = fltarr(n_comp,n_comp)
	readu,1, comp
	readu,1, minerals
	readu,1, original
	rest = original
	if version le -2 then begin
		readu,1, rest
	endif
	readu,1, files
	readu,1, R
	close_file,1

;	Test that these files are accessible. If not look nearby ...

	path = extract_path(F[0])
	
	if mode then begin
		title = 'Select "Original" DA matrix file' 
		t = file_requester( /read, filter = '*.'+extract_extension(original), path=path, file=original, $
						title=title, fix_filter=0, /translate, updir=3, /skip_if_exists)
		if t[0] ne '' then begin
			da = read_da( t, error=error)
			if error then begin
				warning,'read_correct','error reading the "Original" DA matrix file: '+t[0]
				goto, cont
			endif
			path = extract_path(t[0])
		endif
		original = t[0]
	endif else original=''
	
	title = mode ? 'Select "Rest" DA matrix file' : 'Select "Rest" yield file'
	ext = mode ? 'dam*' : 'yield'
	t = file_requester( /read, filter = '*.'+ext, path=path, file=rest, $
					title=title, /fix_filter, /translate, updir=3, /skip_if_exists)
	if t[0] ne '' then begin
		if mode then begin
			da = read_da( t, error=error)
			if error then begin
				warning,'read_correct','error reading the "Rest" DA matrix file: '+t[0]
				goto, cont
			endif
		endif
		path = extract_path(t[0])
	endif
	rest = t[0]

	for j=0,n_elements(files)-1 do begin
		t = files[j]
		title = mode ? 'Select "'+minerals[j]+'" DA matrix file' : 'Select "'+minerals[j]+'" yield file'
		ext = mode ? 'dam*' : 'yield'
		t = file_requester( /read, filter = '*.'+ext, path=path, file=t, $
						title=title, /fix_filter, /translate, updir=3, /skip_if_exists)
		if t[0] ne '' then begin
			if mode then begin
				da = read_da( t, error=error)
				if error then begin
					warning,'read_correct','error reading the "'+minerals[j]+'" DA matrix file: '+t[0]
					goto, cont
				endif
			endif 
			path = extract_path(t[0])
		endif
		files[j] = t[0]
	endfor
	
	; NOTE: This struct also occurs in main Correct_Yield routine, and in read_correct.pro.

cont:
	pars = {	$
		n_comp:		n_comp, $				; number of components
		max_comp:	max_comp, $				; maximum number of components
		comp:		comp, $					; component elements array
		minerals:	minerals, $				; mineral name strings array
		original:	original, $				; original DA matrix file name
		rest:		rest, $					; "rest" DA matrix file name
		files:		files, $				; DA matrix file names for minerals
		R:			R, $					; end-member conc table (wt%)
		current_mode:	0, $				; current correction mode
		pyield:		ptr_new(), $			; pointer to end-mmeber yields
		plast:		ptr_new(), $			; pointer to pixel yields, last iteration
		pdensity:	ptr_new(), $			; pointer to end-member densities
		pscale:		ptr_new() $				; pointer to absorb scaling, last iteration
		}
	p = ptr_new( pars, /no_copy)
	error = 0
	return, p

bad_io:
	print,'Read_correct: bad region I/O'
	return, 0L
end

