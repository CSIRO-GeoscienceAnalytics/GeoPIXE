pro maia_update_parameters, pstate, update=update, error=error

; Request ASIC parameter and  variables from Maia client shared memory 
; as written by "maia_client_parameters",
; and update the Maia parameters struct and table.
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
		warning,'maia_update_parameters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','Possible bad number of values', $
				'returned from "socket_command_get".'], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

@maia_scratch.def

tin = systime(/seconds)
t_interval = 1.0
error = 0
update = 0
busy = 0.
busy_slow=0.
dtback = 0.
errors = 0

if ptr_valid(pstate) eq 0 then goto, fin
ps = (*pstate).psocket

; '(*pm).control' and 'data' tables are in CSV layout file order.
; 'data.index' returns the detector number for each member of these tables. Use this to
; index returned data 'v' in detector order to give CSV table order for assigns.
; 'data.hermes' returns the Hermes/Scepter chip number for each member of these tables.
 
pm = (*pstate).pmaia
pl = (*pstate).playout
pr = (*pstate).preadout
pimage = (*pstate).pimage
data = (*pl).data
ref = (*pl).ref[ (*pl).start + indgen((*pl).N) ]	; index back from CSV table to detector order

if ptr_valid(ps) eq 0 then goto, fin
if (*(*pstate).psocket).open eq 0 then goto, fin
n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32

; Update ROI hack in scratch from (*pm).ROI.rate

;socket_command_set, ps, 'value', string( (*pm).ROI.rate * (*pm).control.status.event_rate), class='scratch.datum', chip=scratch_ROI_rate

; Update Maia status from shared memory ----------------------------------

pshrmem = (*pstate).pshrmem_pars
if (*pshrmem).error then goto, fin
pf = (*pshrmem).pfloat
pdat = (*pshrmem).pdat
plong = (*pshrmem).plong

s = string((*pdat).identity.dam)								; DAM identity string
if s ne (*pm).identity.dam then begin
	warning,'maia_update_parameters',['Maia detector DAM Identity has changed.', $
		'Previously: "'+(*pm).identity.dam+'", Now: "'+s+'"' , '', $
		'You will need to RESTART Maia_Control to inherit correct layouts, etc.', $
		'And load correct parameters for the new Maia detector array.']
	(*pm).identity.dam = s
	(*pm).file = 'Maia_' + (*pm).identity.dam + '.parameters.csv'
endif

s = string((*pdat).status.blog_error)							; blog error string
(*pm).status.blog_error = s

(*pm).control = (*pdat).control									; copy from shared memory
(*pm).channel = (*pdat).channel

maia_launch_update_deadtime, pstate, pm							; check "auto" to set DTcalA

socket_command_mode, ddm_down = 1 - clip((*pm).control.status.link, 0,1)

(*pstate).led[1] = clip((*pm).control.status.link+1, 0, 2)		; LEDs value (0=off, 1=red, 2=green)
(*pstate).led[2] = clip((*pm).control.status.blog+1, 0, 2)
(*pstate).led[4] = clip((*pm).control.interlock+1, 0, 2)
(*pstate).led[5] = ((*pm).control.peltier lt -0.02) ? 1 : 0		; replaces "Maia ready" with "Bakeout"

; Check debug state ----- (see bake above) -----------------------------------

	pulser_on = 0
	synth_on = 0
	debug_ean_mode = 0
	debug_aux_mode = 0
	bake_on = (*pm).control.status.bake and ((*pm).control.peltier lt -0.02)
	
	synth_on = (*pdat).enable.synth
	pulser_on = (*pdat).enable.pulser
	q = where( (*pdat).enable.EAN eq 1, nq)
	debug_ean_mode = (nq ne 0)
	q = where( (*pdat).enable.LOCK eq 1, nq)
	debug_aux_mode = (nq ne 0)
	q = where( (*pdat).channel.hermes.eblk ne 0, nq)
	debug_eblk = nq ne 0
	
	*(*pstate).pHYMOD_DEBUG = {pulser:pulser_on, synth:synth_on, hermes:debug_ean_mode, scepter:debug_aux_mode, eblk:debug_eblk, bake:bake_on}

;	This reads enables, info, DA ...

	maia_launch_read_enable, ps, pm, pr, pimage, pl, disable=disable, pshrmem = (*pstate).pshrmem_pars
	
	if n_elements(disable) gt 0 then *(*pstate).pdisable = disable
	update = 1
	busy = (*pf)[0]
	busy_slow = (*pf)[3] 
	dtback = (*pf)[2]
	errors = (*plong)[1]

;	'busy' (from (*pf)[0]) is the busy percent of the 'maia_client_parameters' process.
;	'busy_slow' (from (*pf)[3]) is the busy percent of the 'maia_client_parameters_slow' process.
;	'percent' is the busy percent of this foreground routine.
;	Note that all these may be using different processors in a multicore system.

fin:
;	if n_elements(disable) gt 0 then *(*pstate).pdisable = disable[data.index]		; offline debug

	t = systime(/seconds)
	t_interval = t - (*pstate).time.update.maia
	(*pstate).time.update.maia = t
	
	percent = ((t_interval gt 0.) ? 100. * (t-tin)/t_interval : 1.)
	(*pstate).activity.process.maia = percent + busy + busy_slow
	
	if percent gt 10. then (*pstate).time_maia = (2.*(*pstate).time_maia) < 100. 
	if percent lt 4. then (*pstate).time_maia = (0.5*(*pstate).time_maia) > 2. 
	if percent gt 5. then print,'Update Maia pars: percent=', percent,' dt=',t-tin, ' Busy%=',busy,' dt(back)=',dtback,' time_maia=',(*pstate).time_maia
	if errors gt 0 then print,'Update Maia pars: errors=',errors,' percent=', percent,' dt=',t-tin, ' Busy%=',busy,' dt(back)=',dtback
	return

bad_n:
	warning,'maia_update_parameters','Bad number of vals returned from "socket_command_get".'
	error = 1
	goto, fin
end
