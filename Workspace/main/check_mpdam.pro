function check_mpdam, file, DevObj, raw=raw, stringed=mpdam_string, phase_good=good, original=original, $
				verify=verify, error=err

;	In order to test all files in mpdam (and its referenced .correct file and all referenced DA matrix files),
;	these are done here for 'evt_start' and put in an updated mpdam struct, stringified into string 'stringed'.
;	This way all file-names passed on are good, but passed in 'stringed' argument to 'da_evt', etc.,
;	which will work in a cluster mode sub-process where files must be found.
;
;	Also checks whether the phase maps are relevant for this 'raw' data source.
;
;	Inputs:
;		file	MPDAM file name
;		DevObj	device object
;		raw		raw data file name
;
;	Return:
;		mpda	1 (OK), 0 (not an mpdam file, or insufficient valid info to do MPDA)
;		stringed full mpdam struct stringified.
;		good	phase map source matches the 'raw' data source.
;		original original normal DA matrix file-name.
;		err		error reading a file (0 means all files good and it's ready for MPDA).

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
		warning,'check_mpdam',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

	mpda = 0
	good = 0
	cancel = 0
	err = 1
	if n_elements( file) eq 0 then goto, finish
	if n_elements( DevObj) eq 0 then goto, finish
	if n_elements( raw) eq 0 then goto, finish
	if obj_valid( DevObj) eq 0 then goto, finish
	if n_elements( verify) eq 0 then verify=1

	if extract_extension( file) eq 'mpdam' then begin
		mpdam = read_mpdam( file, silent=(verify eq 0), error=err)
		if err then begin
			warning,'check_mpdam',['Bad read from MPDAM file: ', file]
			goto, finish
		endif

		if arg_present(mpdam_string) then mpdam_string = stringify( mpdam, /embed_ptr)
		if arg_present(original) then original = (*mpdam.pcorr).original

		if ptr_valid( mpdam.pcorr) then ptr_free, mpdam.pcorr

;		Check that the raw data source for the phase maps matches that for the
;		input raw data.

		pdai = read_geopixe_image( mpdam.phases, /header, /silent, error=err)
		if err then begin
			warning,'check_mpdam',['Bad read of Phase DAI file: ', mpdam.phases]
			goto, finish
		endif
		sphase = strip_path( (*pdai).source)
		free_images, pdai

		sraw = strip_path( raw)
		if DevObj->multi_files() then begin
			k = locate( DevObj->multi_char(), sphase)
			if k ge 0 then sphase = strmid( sphase,0,k)
			k = locate( DevObj->multi_char(), sraw)
			if k ge 0 then sraw = strmid( sraw,0,k)
		endif
		good = sphase eq sraw				; phases and raw use same source

		if good eq 0 then begin
			warning, timeout=10, 'check_mpdam',['Phase maps do not appear to be those for this data-set.', '', $
					'Input data source:  '+ sraw, $
					'Phase map source:  '+ sphase, '', $
					'New phase maps will now be generated for this data-set.', $
					'If normal DA images are not found, they will be done first.'], cancel=cancel
			if cancel then goto, finish
;			goto, finish
		endif

		err = 0
		mpda = 1
	endif else begin
		err = 0
	endelse

finish:
	return, mpda
end
