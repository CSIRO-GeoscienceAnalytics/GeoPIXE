pro geopixe_do_DA, str, obj, raw=evt_files, stringed=mpdam_string, path=path, original=original, $
					cluster=cluster, mpdam=mpdam, error=error

; If a MPDA sort is missing phase, do normal DA, correct images and project phase maps.
; Then modify 'str' argument list to continue with MPDA sorting after return (see "evt_start" and
; "geopixe_do_command").
;
; Note: only set returned 'mpdam', if it changes.

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
		warning,'geopixe_do_DA',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
error = 1

	if obj_valid( obj) eq 0 then goto, bad_obj
	if n_elements(cluster) eq 0 then cluster = obj->cluster()
	resolve_routine, 'geopixe_do_commands'

	mpda_matrix = unstringify( mpdam_string)
  	prefs = geopixe_defaults( error=err, source='geopixe_do_DA')
	cluster_prefs = prefs.cluster
	keep_str = str

	T = strip_file_ext( evt_files[0])
	if obj->multi_files() and (obj->multi_char() ne '.') then begin
		T = strip_file_m( T, ending=obj->multi_char() + ((adc_offset_device(obj) eq -1) ? '0' : '1'))
	endif
	ofile = fix_path(path) + strip_path(T,/keep) + '.dai'

;	Check whether a normal DA image result exists. If not do Normal DA ...

	if file_test( ofile) eq 0 then begin
		geopixe_do_replace, 'matrix', str, original
		geopixe_do_replace, 'mpdam_string', str, ''
		geopixe_do_replace, 'output', str, ofile
		args = strjoin( str, ', ')

		if cluster then begin
			cluster_client, title = 'GeoPIXE GCF imaging (normal DA)', $
			;			group_leader = group, $
				subtitle = strip_path(ofile), $
				args = args, $
				n_files = n_elements(evt_files), $
				presult = presults, $
				prefs = cluster_prefs, $
				error = error
			if error then goto, bad_cluster
		
			if ptr_good(presults) then begin
				cluster_merge_images, presults, images=pp, error=error
				if error then goto, bad_merge
			endif else goto, bad_results
		
		endif else begin
			geopixe_execute, args, error=error, /progress
			if error then goto, bad_execute

			pp = read_geopixe_image( ofile, error=error)
			if error then goto, bad_load
		endelse
	endif else begin
		pp = read_geopixe_image( ofile, error=error)
		if error then goto, bad_load

;		Need to check original DA file again, in case it got changed in loaded DAI image ...

		title = 'Select "Original" DA matrix file' 
		t = file_requester( /read, filter = '*.'+extract_extension(original), path=extract_path(original), file=(*pp).matrix.file, $
						title=title, fix_filter=0, /translate, updir=3, /skip_if_exists)
		if t[0] eq '' then goto, bad_original
		original = t[0]
	endelse

;	Check that we have a normal (non MPDA), non-corrected DAI file ...

	if (*pp).mode ne 0 then goto, bad_mode					; non DA
	if (*pp).type ne 0 then goto, bad_type					; e.g. Phase maps?
	if (*pp).corrected ne 0 then goto, bad_correct2			; already yield corrected

;	Correct images ...

	ps = mpda_matrix.pcorr
	correct = mpda_matrix.correct
	(*ps).original= original								; in case this has changed

	n_iter = 3
	for i=0,n_iter-1 do begin
		correct_yields_do_correct, ps, pp, path=path, /progress, error=error
		if error then goto, bad_correct
	endfor

;	Project phases ...

	correct_yields_do_phases, ps, pp, /progress, error=error
	if error then goto, bad_phases

	phases = strip_file_ext(ofile) + '-phases.dai'
	write_geopixe_image, pp, phases, error=error
	free_images, pp
	if error then goto, bad_phase_save

;	Save a new MPDAM file for this run ...

	mpdam = strip_file_ext(ofile) + '.mpdam'
	write_mpdam, mpdam, phases, correct, error=error
	if error then goto, bad_mpdam_save

;	Now able to continue with full MPDA processing. First update mpdam_string struct ...
		
	mpda_matrix.file = mpdam
	mpda_matrix.phases = phases
;	mpda_matrix.correct = correct

;	Make sure temporary data is cleared ...
;	Remember that ps is the same pointer as mpda_matrix.pcorr

	if ptr_valid( (*ps).pyield) then ptr_free, (*ps).pyield
	if ptr_valid( (*ps).plast) then ptr_free, (*ps).plast
	if ptr_valid( (*ps).pdensity) then ptr_free, (*ps).pdensity
	if ptr_valid( (*ps).pscale) then ptr_free, (*ps).pscale

	mpdam_string = stringify( mpda_matrix, /embed_ptr)

	str = keep_str
	geopixe_do_replace, 'matrix', str, mpdam
	geopixe_do_replace, 'mpdam_string', str, mpdam_string
	error = 0
	return

bad_obj:
	warning, 'geopixe_do_DA', 'Bad Device object.'
	return
bad_cluster:
	warning, 'geopixe_do_DA', 'Bad cluster run. Error returned.'
	return
bad_results:
	warning, 'geopixe_do_DA', 'Bad results pointer.'
	return
bad_merge:
	warning, 'geopixe_do_DA', 'Bad merge of stripe data.'
	return
bad_execute:
	warning, 'geopixe_do_DA', 'Bad execute. Error returned.'
	return
bad_load:
	warning, 'geopixe_do_DA', 'Bad load of DA images after Normal DA.'
	return
bad_correct:
	warning, 'geopixe_do_DA', 'Error performing Yield Correction of Normal DA images.'
	return
bad_phases:
	warning, 'geopixe_do_DA', 'Error projecting onto end-member phases.'
	return
bad_phase_save:
	warning, 'geopixe_do_DA', 'Error writing Phase images.'
	return
bad_mpdam_save:
	warning, 'geopixe_do_DA', 'Error writing MPDAM file.'
	return
bad_mode:
	warning, 'geopixe_do_DA', "Loaded Normal DA image file, but it's NOT a DA mode file."
	return
bad_type:
	warning, 'geopixe_do_DA', ["Loaded Normal DA image file, but it's NOT a Conc type file.", $
				"Perhaps it's a phase fraction file?"]
	return
bad_correct2:
	warning, 'geopixe_do_DA', "Loaded Normal DA image is already corrected!"
	return
bad_original:
	warning, 'geopixe_do_DA', "Can't find the original DA matrix file: "+original
	return
end
