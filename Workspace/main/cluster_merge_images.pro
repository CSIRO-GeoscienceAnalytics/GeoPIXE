pro cluster_merge_images, presults, images=pp, input_file1=evt_file, input_file2=evt2_file, output_file=output_file, $
				path=path, flatten=flatten, error=error

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
		warning,'cluster_merge_images',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(flatten) eq 0 then flatten=1
	if n_elements(output_file) eq 0 then output_file = strip_file_ext( (*presults)[0], /double) + '.dai'
	if n_elements(path) eq 0 then path = extract_path( output_file)
	
	error = 1
	if ptr_good(presults) eq 0 then return
	do_free = arg_present( pp) eq 0

	q = where( (*presults ne '') and (*presults ne 'null'), nq)
	if nq ne 0 then begin 
		pp = read_geopixe_image( (*presults)[q[0]], error=error)
		if error eq 0 then begin
			if n_elements(evt_file) eq 0 then evt_file=(*pp).source	
			file2 = (*pp).source2	

			evt_start_image_increment2, pp, /init, path=path
			if nq ge 2 then begin
				for i=1L,nq-1 do begin
					p2 = read_geopixe_image( (*presults)[q[i]], error=err)
					if err eq 0 then begin
						file2 = (*p2).source2	
	
						evt_start_image_increment2, pp, p2, path=path
					endif else begin
						warning,'cluster_merge_images','Failed to open DAI file: ' + (*presults)[q[i]]
					endelse
					free_images, p2
				endfor
			endif
			if n_elements(evt2_file) eq 0 then evt2_file=file2
				
			if flatten then evt_start_image_increment2, pp, /flatten, path=path			; final global flatten

			set_image_minmax, pp, (*pp).image, (*pp).options
			if (*pp).has_errors then begin
				set_image_minmax, pp, (*pp).error, (*pp).escale
			endif
				
;			(*pp).file = strip_file_ext( (*pp).file, /double) + '.dai'
	
			if n_elements(evt_file) gt 0 then (*pp).source = evt_file					; write file names back in
			if n_elements(evt2_file) gt 0 then (*pp).source2 = evt2_file				; in case these have been
			if n_elements(output_file) gt 0 then (*pp).file = output_file				; remapped in cluster node

;			(*pp).linearize = linearize		
;			(*pp).throttle = throttle
;			(*pp).pileup = pileup
;			(*pp).matrix.file = file
	
			write_geopixe_image, pp, (*pp).file
			if do_free then free_images, pp
			error = 0
		endif else begin
			warning,'cluster_merge_images','Failed to open first DAI file: ' + (*presults)[q[0]]
		endelse
	endif
	return
end

