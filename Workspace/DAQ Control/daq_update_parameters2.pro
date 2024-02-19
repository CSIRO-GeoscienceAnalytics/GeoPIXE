pro daq_update_parameters2, pstate, update=update, error=error

; Request faster variables from DAQ socket and update the DAQ
; parameters struct and rates display.
; Updated in shared memory by "daq_client_parameters" process.
; 
; No checking on the number of returned parameters is made for speed.
; If the number is wrong, the error catch below will intercept it.

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		if n_elements(v) gt 0 then c = [c,'value returned = '+string(v)]
		warning,'daq_update_parameters2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','Possible bad number of values', $
				'returned from "socket_command_get".'], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

tin = systime(/seconds)
t_interval = 0.5
error = 0
update = 0
busy = 0.0

if ptr_valid(pstate) eq 0 then goto, fin
ps = (*pstate).psocket
if ptr_valid(ps) eq 0 then goto, fin
if (*(*pstate).psocket).open eq 0 then goto, fin

; '(*pm).control' and 'data' tables are in CSV layout file order.
; 'data.index' returns the detector number for each member of these tables. Use this to
; index returned data 'v' in detector order to give CSV table order for assigns.
; 'data.hermes' returns the Hermes/Scepter chip number for each member of these tables.
 
pm = (*pstate).pdaq
pl = (*pstate).playout
pr = (*pstate).preadout
data = (*pl).data
n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32
ref = (*pl).ref[ (*pl).start + indgen((*pl).N) ]	; index back from CSV table to detector order
pspecE = *(*pstate).ppspec3				; pointer arrays for E and T spectra
pspecT = *(*pstate).ppspec4

; Update DAQ status ----------------------------------------

pshrmem = (*pstate).pshrmem_pars
if (*pshrmem).error then goto, fin
pdat = (*pshrmem).pdat[0]

(*pm).control = (*pdat).control
(*pm).stage.velocity = (*pdat).stage.velocity

; Update stage pixel position ----------------------------------------

(*pm).run.position = (*pdat).run.position
(*pm).run.pixel = (*pdat).run.pixel
(*pm).run.rate = (*pdat).control.status.blog_rate
(*pm).run.discard = (*pdat).control.status.discard

;	pf	0	% busy				pl	0	blog port			pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval				2	run number			pb1	*	group dir name
;									3	segment #
;									4	run time (sec)
;									5	# blocks written
;									6	bytes/sec
;									7	discard mode

err_mask = '00000001'x		; accumulator error bit (ERR)
err_offset = 0
ofv_mask = '00000002'x		; accumulator overflow bit (OFV)
ofv_offset = -1
mtc_mask = '00F80000'x		; accumulator missing trigger count bit (MTC)
mtc_offset = -19

if (*pstate).enable_activity then begin
	pshrmem = (*pstate).pshrmem_activity
	if (*pshrmem).error eq 0 then begin
		(*pshrmem).loop = 0
		pf = (*pshrmem).pfloat[0]
		pl = (*pshrmem).plong[0]
		pd0 = (*pshrmem).pdat[0]
		pd1 = (*pshrmem).pdat[1]
		pb1 = (*pshrmem).pbyte[1]
		n_detectors = (*pshrmem).buffer_size < (*pm).n_detectors
		t_interval = (*pf)[2]
		busy = (*pf)[0]
		
		if t_interval gt 0.001 then begin
			(*(*pstate).prates).detectors[0:n_detectors-1] = (*pd0)[0:n_detectors-1] / t_interval
		endif
;		print,'     run=',(*pl)[2],'  Segment=',(*pl)[3], '  bytes/s=',rates_string((*pl)[6],/bytes),'  discard=',(*pl)[7],'  t_interval=',(*pf)[2]
		(*pm).run.number = (*pl)[2]
		(*pm).run.segment = (*pl)[3]
;		(*pm).run.discard = (*pl)[7]		; too slow for updates
		
		(*pspecE[0]).file = str_tidy((*pm).run.number)
		(*pspecT[0]).file = str_tidy((*pm).run.number)
		
		s = string(*pb1)
		r = unstringify(s)
		if tag_present('group',r) then (*pm).run.group = r.group.current
		if tag_present('project',r) then (*pm).run.project = r.project.current

		(*pm).run.error.bits = (*pl)[1]
		(*pm).run.error.ERR = 0
		(*pm).run.error.OFV = 0
		(*pm).run.error.MTC = 0
		if (*pm).run.error.bits ne 0 then begin
			(*pm).run.error.ERR = uint( ishft((*pm).run.error.bits and err_mask, err_offset))
			(*pm).run.error.OFV = uint( ishft((*pm).run.error.bits and ofv_mask, ofv_offset))
			(*pm).run.error.MTC = uint( ishft((*pm).run.error.bits and mtc_mask, mtc_offset))
			print,'Activity accumulator error found:'
			if (*pm).run.error.ERR ne 0 then print,'		ERR bit set (error)'
			if (*pm).run.error.OFV ne 0 then print,'		OFV bit set (overflow)'
			if (*pm).run.error.MTC ne 0 then print,'		MTC set, missing trigger count = ',(*pm).run.error.MTC
		endif
	endif	
	if (*pr).activity then update = 1
endif

fin:
	t = systime(/seconds)
	t_interval = t - (*pstate).time.update.activity
	(*pstate).time.update.activity = t

	percent = 100. * (t - tin) / t_interval
	(*pstate).activity.process.activity = percent + busy

	if percent gt 10. then (*pstate).time_daq2 = (2.*(*pstate).time_daq2) < 10. 
	if percent lt 2. then (*pstate).time_daq2 = (0.5*(*pstate).time_daq2) > 0.5
	if percent gt 10. then print,'Update Activity: percent=',percent,' busy=',busy,' time_daq2=',(*pstate).time_daq2
	return

bad_n:
	warning,'daq_update_parameters2','Bad number of vals returned from "socket_command_get".'
	error = 1
	goto, fin
end
