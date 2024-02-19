pro daq_update_parameters3, pstate, update=update, error=error

; Request slow variables from DAQ socket and update the DAQ
; chart struct and chart display. Also update DA rGamma array here and
; pileup limits, because they're large.
; Fast update of parameters to shared memory --> written by "daq_client_parameters_slow".
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
		warning,'daq_update_parameters3',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','Possible bad number of values', $
				'returned from "socket_command_get".'], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

tin = systime(/seconds)
error = 1
update = 0

if ptr_valid(pstate) eq 0 then goto, fin
ps = (*pstate).psocket
if ptr_valid(ps) eq 0 then goto, fin

; '(*pm).control' and 'data' tables are in CSV layout file order.
; 'data.index' returns the detector number for each member of these tables. Use this to
; index returned data 'v' in detector order to give CSV table order for assigns.
; 'data.hermes' returns the Hermes/Scepter chip number for each member of these tables.
 
pm = (*pstate).pdaq
pl = (*pstate).playout
pr = (*pstate).preadout
pc = (*pstate).pchart
data = (*pl).data
n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32
ref = (*pl).ref[ (*pl).start + indgen((*pl).N) ]	; index back from CSV table to detector order

; Check whether we need to extend the log ptr array ...
; This code to extend ptr array and initialize struct also in DAQ_chart_add
; and DAQ_launch after 'state' set-up.

nlog = (*pc).n
nmax = n_elements( (*pc).p)
if nlog ge nmax then begin
	print,'daq_update_parameters3: chart buffer full, extend it ...'
	p1 = ptrarr(10,/allocate_heap)
	for k=0L,10-1 do *p1[k] = {name:'', index:0, val:0.0}
	pnew = [(*pc).p, replicate(ptr_new({n:0L,time:0.0d+0,p:p1}),1000)]
	*pc = {n:nlog, file:(*pc).file, init:(*pc).init, p:pnew}
endif
(*(*pc).p[nlog]).time = systime(/seconds)
if nlog eq 0 then (*(*pc).p[0]).n = 0

; If DAQ disabled in DAQ.conf and /debug mode used, then generate some
; random chart entries ...

if (*pstate).debug and ((*pstate).default.daq.enable eq 0) then begin
	seed = *(*pstate).pseed
	nlog = nlog+1
	(*pc).n = nlog
	error = 0
	update = 1
	return
endif

if (*(*pstate).psocket).open eq 0 then goto, fin

;------------------------------------------------------------------------------------
; These use the slow 10 sec update period here, but don't log to Chart
; Also read once when DAQ_Launch is opened.

; Cal coeffs, written to spectra structs in daq_launch Notify event code
; Don't read here, as it upsets online Gain-Trim work. Done once on DAQ_Launch (initial) open and Hymod apply.

;daq_launch_read_cal, ps, pm, pl

; Update DA rGamma array here cos it's large ... (rather than in 'daq_launch_read_da')

;daq_launch_read_da_rGamma, ps, pm

;------------------------------------------------------------------------------------
nlog = nlog+1
(*pc).n = nlog
error = 0
update = 1

fin:
	t = systime(/seconds)
	if t-tin gt 2.0 then print,'daq_update_parameters3: time = ',t-tin
	return

bad_n:
	warning,'daq_update_parameters3','Bad number of vals returned from "socket_command_get".'
	error = 1
	goto, fin
end