function read_falconx_segments, segments, n_buffer=n_buffer, state=state, $
					select=select, unit=unit, veto_progress=veto_progress, tick=tick2, $
					skip=skipi, header=json, version=versioni, accept=accept, njson=njson

;	unit			read the already open unit as a single segment file
COMPILE_OPT STRICTARR

ErrorNo = 0
common c_falconx_20, version, tick
common c_errors_1, catch_errors_on
if n_elements(tick) eq 0 then tick = 4.0e-6
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
       warning,'read_falconx_segments',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pr
       return, 0L
    endif
endif
if n_elements(versioni) lt 1 then versioni=0
if versioni gt 0 then version=versioni
tick2 = tick

pr = ptrarr(100000L)
npr = 100000L
ipr = 0L
by_unit = 0
if n_elements(n_buffer) lt 1 then n_buffer = 50000LL
if n_elements(state) lt 1 then state = {x:0, y:0, enable:replicate(1B,16), time:0 }
if n_elements(segments) lt 1 then return, 0L
if n_elements(select) lt 1 then select=-1L
if n_elements(skipi) lt 1 then skipi=0LL
if n_elements(njson) lt 1 then njson=0L
if n_elements(version) lt 1 then version=0					; 9999999L
if n_elements(veto_progress) lt 1 then veto_progress=0
if n_elements(accept) lt 1 then accept=indgen(1000)
if n_elements(unit) ge 1 then begin
	stat = fstat(unit)
	if stat.open then begin
		segments = stat.name
		lun = unit
		by_unit = 1
	endif
endif

	cancel = 0
	skipbutton = 0
	skip = skipi
	i = 0
	ns = (select eq -1 ) ? n_elements(segments) : 1
	do_progress = 0
	if ns gt 1 then	begin
		progress, tlb=progress_tlb, title='Read falconx Segments'
		do_progress = 1
	endif else do_progress=0
	first = 1
	for i=0L,ns-1 do begin
		if by_unit eq 0 then begin
			on_ioerror, bad_open
			if first then begin
				openr, lun, segments[i], /get_lun
			endif else begin
				openr, lun, segments[i]
			endelse
			if select eq -1 then begin
				json = read_falconx_json( lun, n_header=njson, error=err)
				if err then return,0
				if njson gt 0 then begin
					ver = falconx_version( json, tick=tick)
					tick2 = tick
					if ver gt 0 then begin
						version = ver
					endif else begin
						warning,'read_falconx_segments','JSON found, but Version zero!'
						return, 0
					endelse
				endif else begin
					if version eq 0 then begin
						gprint,'read_falconx_segments: Version zero, so look for 0 segment file.'
						file2 = file
						t = strip_file_ext(file2)
						k = locate_last('_',t)
						f = strmid(t,0,k+1) + '0.' + extract_extension(file2)
						if file_test(f) and (f ne file2) then begin
							file2 = f
							json2 = read_falconx_json( file=file2, n_header=njson2, error=err)
							ver = falconx_version( json2, tick=tick)
							tick2 = tick
							if ver gt 0 then begin
								version = ver
							endif else begin
								warning,'read_falconx_segments','Version zero. JSON not found in first segment.'
								return, 0
							endelse
						endif else begin
							warning,'read_falconx_segments','Version zero. Error reading first segment?'
							return, 0
						endelse
					endif
				endelse
			endif
		endif
		versioni = version			; copy back new version to caller ...
		on_ioerror, bad_io
		if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:i, size:ns}, cancel=cancel, skip=skipbutton
		if cancel or skipbutton then goto, done

		pr2 = read_falconx( lun, n_buffer=n_buffer, n_actual=n_actual, skip=skip, njson=njson, version=version, $
					select=select, accept=accept, state=state, progressbar=(not do_progress) and (not veto_progress))
		skip = 0LL
		if ptr_valid(pr2[0]) then begin
			npr2 = n_elements(pr2)
			for j=0L,npr2-1 do (*pr2[j]).file = segments[i]
			if ipr+npr2 ge npr then begin
				pr = [pr, ptrarr(npr2+100000L)]
				npr = npr + npr2+100000L
			endif
			pr[ipr:ipr+npr2-1] = pr2
			ipr = ipr+npr2
		endif
		if by_unit eq 0 then close, lun
	endfor
	if ptr_valid(pr[0]) and (ipr gt 0) then begin
		for j=0L,ipr-1 do (*pr[j]).index = j
	endif

;	print, 'Read_falconx_segments: Total bytes read = ',nt
	if do_progress then progress, /complete, progress_tlb, 'Read falconx Segments completed.'

done:
	if by_unit eq 0 then begin
;		print,'read_falconx_segments: close unit=',lun
		close_file, lun
	endif else begin
		point_lun, lun, 0
	endelse
	if do_progress then progress, /ending, progress_tlb

	if npr gt ipr then ptr_free, pr[ipr:npr-1]
	p = (ipr gt 0) ? pr[0:ipr-1] : 0L
	if n_elements(p) eq 1 then p=p[0]
	return, p

bad_open:
	warning,'read_falconx_segments','error opening segment file(s): '+segments[i]
	goto, done
bad_io:
	warning,'read_falconx_segments','error reading segment file(s): '+segments[i<(ns-1)]
	goto, done
end