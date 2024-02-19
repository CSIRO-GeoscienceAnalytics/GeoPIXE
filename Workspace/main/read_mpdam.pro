
;	Read multi-phase DA file, which points to phase DAI file
;	and a correct file. Also read and return 'correct' data.
	
function read_old_mpdam, F, silent=silent, error=error

;	Read old text version of the .mpdam file.

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
		warning,'read_old_mpdam',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif
if n_params() lt 1 then return, 0L
if lenchr(F) lt 1 then return, 0L
if n_elements(silent) eq 0 then silent=0

error = 1
valid = [-1]						; valid versions

	on_ioerror, bad_io
	openr, unit, F, /get_lun

	line = ''
	readf,unit, line
	version = fix2(line)
	q = where( version eq valid)
	if q[0] eq -1 then return, 0L

	readf,unit, line
	phases = line					; multi-phase DAI file-name

	readf,unit, line
	correct = line					; .correct file-name
	close_file, unit
		
	error = 0
	return, { file:F[0], phases:phases, correct:correct}

bad_io:
	print,'read_old_mpdam: bad old MPDAM I/O'
	return, 0L
end

;------------------------------------------------------------------------------------------------

function read_mpdam, F, silent=silent, error=error

	;	Read multi-phase DA file, which points to phase DAI file
	;	and a correct file. Also read and return 'correct' data.

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
			warning,'read_mpdam',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0L
		endif
	endif
	if n_params() lt 1 then return, 0L
	if lenchr(F) lt 1 then return, 0L
	if n_elements(silent) eq 0 then silent=0

	error = 1
	valid = [-1]						; valid versions

	on_ioerror, bad_io
	openr, unit, F, /get_lun, /xdr
	
	version = 0
	readu, unit, version
	q = where( version eq valid)
	if q[0] eq -1 then begin
		close_file, unit
		mpdam = read_old_mpdam( F, silent=silent, error=error)
		phases = mpdam.phases
		correct = mpdam.correct
		goto, cont
	endif

	phases = ''
	readu, unit, phases				; multi-phase DAI file-name

	correct = ''
	readu, unit, correct			; .correct file-name
	close_file, unit

cont:
	path = extract_path(F[0])
	if silent eq 0 then begin
		t = file_requester( /read, filter = '*.'+extract_extension(phases), path=path, file=phases, $
					title='Select Phase map DA image file', fix_filter=1, $
					/translate, updir=3, /skip_if_exists)
		if t[0] eq '' then goto, bad_io
		phases = t[0]
	endif

	if silent eq 0 then begin
		t = file_requester( /read, filter = '*.'+extract_extension(correct), path=path, file=correct, $
					title='Select Phase Correction matrix file', fix_filter=1, $
					/translate, updir=3, /skip_if_exists)
		if t[0] eq '' then goto, bad_io
		correct = t[0]
	endif

	pcorr = read_correct( correct, silent=silent, error=error)
	if error then return, 0L

	error = 0
	return, { file:F[0], phases:phases, correct:correct, pcorr:pcorr}

	bad_io:
	print,'read_mpdam: bad MPDAM I/O'
	return, 0L
end
