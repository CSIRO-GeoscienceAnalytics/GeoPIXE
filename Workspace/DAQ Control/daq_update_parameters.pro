pro daq_update_parameters, pstate, update=update, error=error

; Request ASIC parameter and slow variables from DAQ socket and update the DAQ
; parameters struct and table.
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
		warning,'daq_update_parameters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','Possible bad number of values', $
				'returned from "socket_command_get".'], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

tin = systime(/seconds)
t_interval = 1.0
error = 0
update = 0

if ptr_valid(pstate) eq 0 then goto, fin
ps = (*pstate).psocket
psm = (*pstate).pmsocket
if ptr_valid(ps) eq 0 then goto, fin
if ptr_valid(psm) eq 0 then goto, fin
if (*ps).open eq 0 then begin
	(*pstate).led[0] = 1
	goto, fin
endif
if (*psm).open eq 0 then begin
	(*pstate).led[4] = 1
endif

; '(*pm).control' and 'data' tables are in CSV layout file order.
; 'data.index' returns the detector number for each member of these tables. Use this to
; index returned data 'v' in detector order to give CSV table order for assigns.
; 'data.hermes' returns the Hermes/Scepter chip number for each member of these tables.
 
pm = (*pstate).pdaq
pl = (*pstate).playout
pr = (*pstate).preadout
pimage = (*pstate).pimage
data = (*pl).data
n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32
ref = (*pl).ref[ (*pl).start + indgen((*pl).N) ]	; index back from CSV table to detector order
obj = (*pstate).DevObj

; Update DAQ status ----------------------------------------

pshrmem = (*pstate).pshrmem_pars
if (*pshrmem).error then goto, fin
pdat = (*pshrmem).pdat[0]

(*pm).scan.iX = clip( obj->get_Xaxis(), 0,5)
(*pm).scan.iY = clip( obj->get_Yaxis(), 0,5)
(*pm).scan.X = (*pdat).scan.extent[(*pm).scan.iX]
(*pm).scan.Y = (*pdat).scan.extent[(*pm).scan.iY]
(*pm).scan.origin.X = (*pdat).scan.origin[(*pm).scan.iX]
(*pm).scan.origin.Y = (*pdat).scan.origin[(*pm).scan.iY]
(*pm).scan.pitch.X = (*pdat).scan.pitch[(*pm).scan.iX]
(*pm).scan.pitch.Y = (*pdat).scan.pitch[(*pm).scan.iY]
(*pm).scan.info = string( (*pdat).scan.info)
;pointer_display, (*pm).scan

(*pm).control = (*pdat).control

(*pstate).led[1] = clip((*pm).control.status.blog+1, 0, 2)

v  = socket_command_get( ps, 'enable', class='blog', error=err)
if err eq 0 then (*pstate).led[2] = clip(v[0]+1, 0, 2)

interlock = 1
if (*pm).control.temp.cpu gt 60. then interlock = 0
if (*pm).control.temp.fpga gt 60. then interlock = 0
if (*pm).control.temp.board gt 60. then interlock = 0
if (*pm).control.temp.eth0 gt 70. then interlock = 0
if (*pm).control.temp.eth1 gt 70. then interlock = 0
;(*pstate).led[4] = clip(interlock+1, 0, 2)		; LED 4 now used for slave maia open

; Check assigned axes -----------------------------------

v  = socket_command_get( ps, 'source', class='stepdir.chan', chip=[0,1], error=err)
if err eq 0 then begin
	(*pm).DevObj->set_options,  x_axis=clip(v[0], 0, 4), y_axis=clip(v[1], 0, 4)
endif

; Check debug state ----- (see bake above) -----------------------------------

pulser_on = (*pdat).enable.pulser
debug_aux_mode = ((*pdat).enable.LOCK[0] ne 0)	
synth_on = (*pdat).enable.synth
	
*(*pstate).pHYMOD_DEBUG = {pulser:pulser_on, synth:synth_on, scepter:debug_aux_mode}

; Update local copy of certain global parameters ----------------------------------------

;daq_launch_read_info, ps, pm, pimage						; done in read enable now

daq_launch_read_enable, ps, pm, pr, pimage, pl, disable=disable, pshrmem=pshrmem
daq_launch_update_deadtime, pstate, pm						; check "auto" to set DTcalA

if n_elements(disable) gt 0 then *(*pstate).pdisable = disable
update = 1

fin:
	t = systime(/seconds)
	t_interval = t - (*pstate).time.update.daq
	(*pstate).time.update.daq = t
	
	percent = (t_interval gt 0.) ? 100. * (t-tin)/t_interval : 1.
	(*pstate).activity.process.daq = percent
	
	if percent gt 10. then (*pstate).time_daq = (2.*(*pstate).time_daq) < 30. 
	if percent lt 2. then (*pstate).time_daq = (0.5*(*pstate).time_daq) > 2. 
	if percent gt 10. then print,'Percent DAQ = ', percent, ' time_daq =',(*pstate).time_daq
	return

bad_n:
	warning,'daq_update_parameters','Bad number of vals returned from "socket_command_get".'
	error = 1
	goto, fin
end
