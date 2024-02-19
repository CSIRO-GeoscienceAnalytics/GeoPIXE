pro cluster_merge_spectra, presults, spectra=pp, error=error

;	Results is an array containing the result strings from each slave.
;	This is an array of 'xxx.SPEC.nn' file names of resulting spectra.
;	Read each spectrum file and combine spectra into one. 
;	Don't combine charge,IC for regions (already done) - use /region.

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
		warning,'cluster_merge_spectra',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	error = 1
	if ptr_good(presults) eq 0 then return

	q = where( (*presults ne '') and (*presults ne 'null'), nq)
	if nq ne 0 then begin 
		p1 = read_spec( (*presults)[q[0]], error=error)
		if error eq 0 then begin
			if ptr_good(pp) eq 0 then pp = ptr_new(p1, /no_copy)
			if nq gt 1 then begin
				for j=1L,nq-1 do begin
					p2 = read_spec( (*presults)[q[j]], error=err)
					pp2 = ptr_new(p2, /no_copy)
					if err eq 0 then begin
						spectrum_load_spectrum_increment, pp, pp2, /free, /region
					endif
				endfor
			endif
			if ptr_valid(pp) then p1 = *pp		
			for j=0L,n_elements(p1)-1 do begin
				(*p1[j]).file = strip_file_ext( (*p1[0]).file, /double) + '.spec'
			endfor
			error = 0
		endif else begin
			warning,'cluster_merge_spectra','Failed to open first SPEC file: ' + (*presults)[q[0]]
		endelse
	endif
	return
end

;-----------------------------------------------------------------

