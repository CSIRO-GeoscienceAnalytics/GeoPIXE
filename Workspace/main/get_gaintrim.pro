function get_gaintrim, gfile, energy=energy, time=time, error=error

; Read a gaintrim 'gfile'
; Return a 2-element vector of b,a for each detector channel.

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
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
	       warning,'get_gaintrim',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return, g
	    endif
	endif

	g = fltarr(2,384)
	found = 0
	energy = 0
	time = 0
	error = 1

	if extract_extension( gfile) eq 'spec' then goto, old_gaintrim
	if lenchr(gfile) eq 0 then goto, bad_file_name

	line = ''
	on_ioerror, bad_gaintrim
	openr, lun, gfile, /get_lun
	while NOT EOF(lun) do begin
		readf, lun, line
		i = locate('#', line)
		if (i eq -1) and (lenchr(line) gt 0) then begin
			s = strsplit( line, '[] ', /extract)
			ns = n_elements(s)
			if ns ge 5 then begin
				found = 1
				n = fix2(s[1])
				if (n lt 0) or (n ge 384) then goto, bad_detector
				g[0,n] = float2(s[ns-2])
				g[1,n] = float2(s[ns-1])
;				print,' Detector: ',n,' Trim = ', g[*,n]
			endif
			q = where( s eq '.tcoeff', nq)
			if nq ge 1 then time=1
			q = where( s eq '.ecoeff', nq)
			if nq ge 1 then energy=1
		endif
	endwhile
	on_ioerror, null
	close_file, lun
	if found eq 0 then warning,'get_gaintrim','No good detectors found.'
	error = 0
	goto, finish

old_gaintrim:
	p = read_spec( gfile)
	if ptr_valid(p[0]) then begin
		found = 0
		obj = (*p[0]).DevObj
		for i=0L,n_elements(p)-1 do begin
			n = strlen((*p[i]).label)
			lab = strmid( (*p[i]).label, n-2,2)
			if strupcase(lab) eq '/E' then begin
				j = fix((*p[i]).station + adc_offset_device(obj))
				found = 1
				energy = 1
				g[0,j] = (*p[i]).cal.poly[0] 
				g[1,j] = (*p[i]).cal.poly[1] 
			endif else if strupcase(lab) eq '/T' then begin
				j = fix((*p[i]).station + adc_offset_device(obj))
				found = 1
				time = 1
				g[0,j] = (*p[i]).cal.poly[0] 
				g[1,j] = (*p[i]).cal.poly[1] 
			endif
;			print,' Detector: ',j,' Trim = ', g[*,j]
		endfor
	endif else goto, bad_gaintrim
	if found eq 0 then warning,'get_gaintrim','No good detectors found.'
	error = 0
	goto, finish

bad_gaintrim:
	warning,'get_gaintrim',['error reading gaintrim file:',gfile]
	goto, finish
bad_file_name:
	warning,'get_gaintrim','Bad gaintrim file name.'
	goto, finish
bad_detector:
	warning,'get_gaintrim','Bad detector number.'
	goto, finish

finish:
	close_file, lun
	return, g
	end
