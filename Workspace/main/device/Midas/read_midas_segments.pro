function read_midas_segments, segments, n_buffer=n_buffer, state=state, $
					select=select, unit=unit, veto_progress=veto_progress, $
					skip=skip, type=type

;	unit			read the already open unit as a single segment file
COMPILE_OPT STRICTARR

ErrorNo = 0
common c_errors_1, catch_errors_on
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
       warning,'read_midas_segments',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pr
       return, 0L
    endif
endif

pr = ptrarr(100000L)
npr = 100000L
ipr = 0L
by_unit = 0
if n_elements(n_buffer) lt 1 then n_buffer = 200000LL
if n_elements(state) lt 1 then state = {x:0, y:0, enable:replicate(1B,16), time:0 }
if n_elements(segments) lt 1 then return, 0L
if n_elements(select) lt 1 then select=-1L
if n_elements(skip) lt 1 then skip=0
if n_elements(type) lt 1 then type=0
if n_elements(veto_progress) lt 1 then veto_progress=0
if n_elements(unit) ge 1 then begin
	stat = fstat(unit)
	if stat.open then begin
		segments = stat.name
		lun = unit
		by_unit = 1
	endif
endif

	cancel = 0
	skipb = 0
	i = 0
	ns = (select eq -1 ) ? n_elements(segments) : 1
	do_progress = 0
	if ns gt 1 then	begin
		progress, tlb=progress_tlb, title='Read midas Segments'
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
		endif
		on_ioerror, bad_io
		if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:i, size:ns}, cancel=cancel, skip=skipb
		if cancel or skipb then goto, done

		pr2 = read_midas( lun, n_buffer=n_buffer, n_actual=n_actual, skip=skip, type=type, $
					select=select, state=state, progressbar=(not do_progress) and (not veto_progress))
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

;	print, 'Read_midas_segments: Total bytes read = ',nt
	if do_progress then progress, /complete, progress_tlb, 'Read midas Segments completed.'

done:
	if by_unit eq 0 then begin
;		print,'read_midas_segments: close unit=',lun
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
	warning,'read_midas_segments','error opening segment file(s): '+segments[i]
	goto, done
bad_io:
	warning,'read_midas_segments','error reading segment file(s): '+segments[i<(ns-1)]
	goto, done
end