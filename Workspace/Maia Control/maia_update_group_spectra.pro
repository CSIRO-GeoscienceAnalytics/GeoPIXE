pro maia_update_group_spectra, pstate, update=update, error=error

; Triggered by a timer event on the "Group" button, uses shared memory
; mapped directly onto Group local spectra for use with new spectra data records.
; In this case shared memory pdat[] is an array of pointers to vectors, one for 
; each Group spectrum.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
error = 1
update = 0

	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'maia_update_group_spectra',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	tin = systime(/seconds)
	
;	par array:	0	n_buffers
;				1	free
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	pf	0	% busy				pl	0	blog port		pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval
;		3	flux1
;		4	flux2

	err_mask = '00000001'x		; accumulator error bit (ERR)
	err_offset = 0
	ofv_mask = '00000002'x		; accumulator overflow bit (OFV)
	ofv_offset = -1
	mtc_mask = '00F80000'x		; accumulator missing trigger count bit (MTC)
	mtc_offset = -19
	
	if (*(*pstate).pmaia).number.spectra eq 0 then return
	pshrmem = (*pstate).pshrmem_spectra
	if (*pshrmem).error then return

	(*pshrmem).loop = 0
	pf = (*pshrmem).pfloat[0]
	pl = (*pshrmem).plong[0]
	pd = (*pshrmem).pdat[0]
	valid = 0
	
;	In this case shared memory pdat[] is an array of pointers to vectors, one for 
;	each Group spectrum. These are accumulated directly in backgnd, and don't need to be here.

;	Should copy one/all of the 'duration', 'flux' values into the spectrum struct somewhere?

	for i=0L,(*pshrmem).n_buffers-1 do begin
		valid = valid OR (*(*pshrmem).pvalid[i])[0]
		(*(*pshrmem).pvalid[i])[0] = 0
	endfor
	
	error_bits = (*pl)[1]
	ERR = 0
	OFV = 0
	MTC = 0
	if error_bits ne 0 then begin
		ERR = uint( ishft(error_bits and err_mask, err_offset))
		OFV = uint( ishft(error_bits and ofv_mask, ofv_offset))
		MTC = uint( ishft(error_bits and mtc_mask, mtc_offset))
		print,'Region spectra accumulator error found:'
		if ERR ne 0 then print,'		ERR bit set (error)'
		if OFV ne 0 then print,'		OFV bit set (overflow)'
		if MTC ne 0 then print,'		MTC set, missing trigger count = ',MTC
	endif
	
	t = systime(/seconds)
	t_interval = t - (*pstate).time.update.spectra
	(*pstate).time.update.spectra = t

	percent = 100. * (t - tin) / t_interval
	(*pstate).activity.process.spectra = percent + (*pf)[0]
	(*pstate).activity.buffers.spectra = (*pf)[1]
	if percent gt 10. then print,'Update Spectra: percent=',percent,'  busy=',(*pf)[0],' lost=',(*pf)[1],' time_group=',t_interval

	t_interval2 = t - (*pstate).time.display.spectra
	if t_interval2 gt (*pstate).time_spectra_update then begin
		(*pstate).time.display.spectra = t
		update = 1
	endif
	error = 0
	return
end
