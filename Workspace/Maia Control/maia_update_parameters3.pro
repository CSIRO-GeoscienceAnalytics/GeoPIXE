pro maia_update_parameters3, pstate, update=update, error=error

; Request ASIC parameter and slow variables from Maia client shared memory
; written by "maia_client_parameters_slow", 
; and update the Maia parameters struct and table. Also update DA rGamma 
; array here and pileup limits, because they're large.
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
		warning,'maia_update_parameters3',['IDL run-time error caught.', '', $
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
 
pm = (*pstate).pmaia
pl = (*pstate).playout
pr = (*pstate).preadout
pc = (*pstate).pchart
data = (*pl).data
n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32
ref = (*pl).ref[ (*pl).start + indgen((*pl).N) ]	; index back from CSV table to detector order
errors = 0

pshrmem = (*pstate).pshrmem_pars
if (*pshrmem).error then goto, fin
pdat = (*pshrmem).pdat[0]
plong = (*pshrmem).plong[0]
pf = (*pshrmem).pfloat[0]

; Check whether we need to extend the log ptr array ...
; This code to extend ptr array and initialize struct also in Maia_chart_add
; and Maia_launch after 'state' set-up.

nlog = (*pc).n
nmax = n_elements( (*pc).p)
if nlog ge nmax then begin
	print,'maia_update_parameters3: chart buffer full, extend it ...'
	p1 = ptrarr(10,/allocate_heap)
	for k=0L,10-1 do *p1[k] = {name:'', index:0, val:0.0}
	pnew = [(*pc).p, replicate(ptr_new({n:0L,time:0.0d+0,p:p1}),1000)]
	*pc = {n:nlog, file:(*pc).file, init:(*pc).init, time0:0.0d+0, p:pnew}
endif
(*(*pc).p[nlog]).time = systime(/seconds)
if nlog eq 0 then (*(*pc).p[0]).n = 0

; If Maia disabled in Maia.conf and /debug mode used, then generate some
; random chart entries ...

if (*pstate).debug and ((*pstate).default.maia.enable eq 0) then begin
	seed = *(*pstate).pseed
	maia_chart_add, pc, 'Detector temp', -25. + randomn( seed, 1)
	maia_chart_add, pc, 'HERMES temp', 15. + randomn( seed, 1)
	maia_chart_add, pc, 'Water temp', -3. + randomn( seed, 1)
	maia_chart_add, pc, 'Bias monitor', 145. + randomn( seed, 1)
	maia_chart_add, pc, 'Detector leakage', 0.5 + 0.2*randomn( seed, 1)
	maia_chart_add, pc, 'Link rate', 300000. + 50000.*randomn( seed, 1)
	maia_chart_add, pc, 'SCEPTER rates', 30000. + 10000.*randomn( seed, 12)
	*(*pstate).pseed = seed

	nlog = nlog+1
	(*pc).n = nlog
	error = 0
	update = 1
	return
endif

if (*(*pstate).psocket).open eq 0 then goto, fin

; Store logs in structures {name:'name text',val:value} pointed to by
; an array of pointers pointed to by (*(*pc).p)[nlog] where nlog is given by (*pc).n
; The current values are handled by maia_client_parameters and maia_client_parameters_slow processes

	(*pm).channel = (*(*pshrmem).pdat).channel
	(*pm).control = (*(*pshrmem).pdat).control

; Temperatures (more added below) -----------------------------------------------

	maia_chart_add, pc, 'Detector temp', (*pm).control.temp.detector
	maia_chart_add, pc, 'Mosfet temp', (*pm).control.temp.mosfet
	maia_chart_add, pc, 'FPGA temp', (*pm).control.temp.fpga
	maia_chart_add, pc, 'HERMES temp', (*pm).control.temp.hermes
	maia_chart_add, pc, 'Water temp', (*pm).control.temp.water
	maia_chart_add, pc, 'Coldtrap temp', (*pm).control.temp.coldtrap

; Update detector monitor ----------------------------------------
 
	maia_chart_add, pc, 'Bias control', (*pm).control.bias
	maia_chart_add, pc, 'Bias monitor', (*pm).control.bias_monitor
	maia_chart_add, pc, 'Detector guard', (*pm).control.guard 
	maia_chart_add, pc, 'Peltier control', (*pm).control.peltier
	maia_chart_add, pc, 'Detector leakage', (*pm).control.leakage 
	maia_chart_add, pc, 'BP interlock', (*pm).control.interlock

; Update Maia rates ----------------------------------------

	maia_chart_add, pc, 'Link rate', (*pm).control.status.link_rate
	maia_chart_add, pc, 'Event rate', (*pm).control.status.event_rate
	maia_chart_add, pc, 'Link errors', (*pm).control.status.link_erate

	v = socket_command_get( ps, 'rate', class='status.scepter', chip=-1, error=err)
	if (err eq 0) then begin
		(*pstate).activity.chip = v
		maia_chart_add, pc, 'SCEPTER rates', v
	endif

	time = systime(/seconds)
	rtim = 1./(3600.)

	maia_chart_add, pc, 'Main uptime (h)', (time-(*pm).control.status.main_uptime)*rtim
	maia_chart_add, pc, 'Link uptime (h)', (time-(*pm).control.status.link_uptime)*rtim
	maia_chart_add, pc, 'Bias Peltier interlock uptime (h)', (time-(*pm).control.status.bpinterlock_uptime)*rtim

; Temperatures (extra ones) -----------------------------------------------------------

	maia_chart_add, pc, 'HYMOD FPGA temp', (*pm).control.temp.hymod_fpga
	maia_chart_add, pc, 'HYMOD CPU temp', (*pm).control.temp.hymod_cpu

;------------------------------------------------------------------------------------
; These use the slow 10 sec update period here, but don't log to Chart
; Also read once when Maia_Launch is opened.

; Throttle factors, written to shared memory in maia_update_ET_spectra

	(*pm).throttle.factors = (*(*pshrmem).pdat).throttle.factors

; Update DA rGamma array here cos it's large ... (rather than in 'maia_launch_read_da')

	(*pm).DA.on =  (*(*pshrmem).pdat).DA.on
	if (*pm).DA.on and ((*pm).version.software ge 5411) then begin
		if ptr_valid((*pm).DA.parray) eq 0 then (*pm).DA.parray = ptr_new(/allocate_heap)

		n_detectors = (*ps).n_detectors
		(*pm).DA.N =  (*(*pshrmem).pdat).DA.N
		if (*pm).DA.N gt 0 then begin
			*(*pm).DA.parray = {On:1, n_det:n_detectors, rGamma:(*(*pshrmem).pdat).DA.rGamma}
		endif else begin
			*(*pm).DA.parray = {On:0, n_det:n_detectors}
		endelse
	endif
	
; Update pileup limits table ...

	*(*pm).pileup.limits.plow = (*(*pshrmem).pdat).pileup.limits.low
	*(*pm).pileup.limits.phigh = (*(*pshrmem).pdat).pileup.limits.high

;------------------------------------------------------------------------------------
nlog = nlog+1
(*pc).n = nlog
error = 0
update = 1
errors = (*plong)[2]
busy = (*pf)[3]
dtback = (*pf)[5]

fin:
	t = systime(/seconds)
	;print,'maia_update_parameters3: time = ',t-tin,' busy%=',busy,' dt(back)=',dtback
	if (t-tin gt 2.0) or (errors gt 0) then print,'maia_update_parameters3: errors=',errors,' time=', t-tin
	return

bad_n:
	warning,'maia_update_parameters3','Bad number of vals returned from "socket_command_get".'
	error = 1
	goto, fin
end