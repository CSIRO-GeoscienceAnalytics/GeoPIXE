pro maia_update_ET_spectra, pstate, update=update, error=error

; Triggered by a timer event on the "Spectra" button, scan spectra
; shared memory for new spectra data records to increment into local spectra.
; Use this for 2D spectra map [n_channels,n_detectors] storage used for ET2/ET3/ET4 records.
; In this case shared memory pdat[] is a pointer to a single 2D array, with each row
; for each Detector spectrum.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
error = 1
update = 0

;	par array:	0	n_buffers
;				1	free
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	  incoming (from maia_launch ...):     reset kill
;	  outgoing (to maia_launch ...):                 kill running          buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;
;	pf	0	% busy				pl	0	blog port		pb	*	blog server ip
;		1	% buffers lost			1					pb4	*	throttle spectrum
;		2	ROI fraction			2	ROI enable
;									3,4	ROI limits

	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'maia_update_ET_spectra',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	tin = systime(/seconds)
		
	pm = (*pstate).pmaia
	
	pshrmem = (*pstate).pshrmem_ET_spectra
	if (*pshrmem).error then return

	(*pshrmem).loop = 0
	pf = (*pshrmem).pfloat[0]
	pl = (*pshrmem).plong[0]
	pd = (*pshrmem).pdat[0]
	dim = (*pshrmem).buffer_size
	n_detectors = dim[1]
	n_channels = dim[0]
	valid = 0
	if (*pstate).enable_activity eq 0 then begin
		tot = fltarr(n_detectors)
		sum = fltarr(n_detectors)
	endif

;	A quick and dirty to get the count-rate between ROI limits
	
	(*pl)[2] = 1					; ROI enable
	(*pl)[3:4] = [560,800]			; ROI limits
	(*pm).ROI.rate = (*pf)[2]
	if (*pm).ROI.rate gt 1. then print, 'ROI rate fraction > 1 ... ', (*pm).ROI.rate

; In this case shared memory pdat[] is a pointer to a single 2D array, with each row
; for each Detector E spectrum. Need to copy this into local spectrum memory here.
	
	for i=0L,n_detectors-1 do begin
		y = (*pd)[*,i]
		if (*pm).throttle.on then begin
			n = min([n_elements(y),n_elements((*pm).throttle.factors)])
			y[0:n-1] = y[0:n-1] * (*pm).throttle.factors[0:n-1]
		endif
		*(*(*(*pstate).ppspec3)[i]).data = y
		if (*pstate).enable_activity eq 0 then begin
			sum[i] = total((*pd)[*,i])
			tot[i] = sum[i] - (*pstate).rate_last[i]
		endif
	endfor
	valid = (*(*pshrmem).pvalid[0])[0]
	(*(*pshrmem).pvalid[0])[0] = 0
	
; Same for the T spectra. Don't need to for ET2D because it uses the 
; shared mmemory array directly for display.

	pshrmem4 = (*pstate).pshrmem_ET_spectraT
	if (*pshrmem4).error then return

	(*pshrmem4).loop = 0
	pd4 = (*pshrmem4).pdat[0]
	pb4 = (*pshrmem4).pbyte[0]
	dim4 = (*pshrmem4).buffer_size
	n_detectors4 = dim4[1]
	n_channels4 = dim4[0]
	
	for i=0L,n_detectors4-1 do begin
		*(*(*(*pstate).ppspec4)[i]).data = (*pd4)[*,i]
	endfor
	
; Copy throttle factors to shared memory for use in blog_client_et2_spectra process
; The throttle factors are read from Kandinski in maia_update_parameters3
; Cal coeffs are written to spectra structs in Maia_launch Timer event code.

	(*pb4)[*] = (*pm).throttle.factors > 1			; copy throttle factors to backgnd process
	
; For now, determine rates here from ET data. Correct them here for lost
; records. Later will use activity data from kandinski.

	t = systime(/seconds)
	t_interval = t - (*pstate).time.update.ET_spectra
	(*pstate).time.update.ET_spectra = t

; % Percent reported in Maia Launch is sum of et_spectra process on (*pf)[0] and
; time spent here updating spectra arrays. Lost records are noted by et_spectra
; process as (*pf)[1].

	percent = 100. * (t - tin) / t_interval
	(*pstate).activity.process.ET_spectra = percent	+ (*pf)[0]			; % busy
	(*pstate).activity.buffers.ET_spectra = (*pf)[1]					; % records lost
	;print,'Maia_update_et_spectra: local percent=',percent,'  back busy%=',(*pf)[0]
	
; Correct rates for the lost records ...

	if (*pstate).enable_activity eq 0 then begin
		(*(*pstate).prates).detectors = tot / t_interval
		(*(*pstate).prates).detectors = 100. * (*(*pstate).prates).detectors / ((100. - (*pstate).activity.buffers.ET_spectra) > 1.)
	endif
	
	t_interval2 = t - (*pstate).time.display.ET_spectra
	if t_interval2 gt (*pstate).time_ET_spectra_update then begin
		update = 1
		(*pstate).time.display.ET_spectra = t
		if (*pstate).enable_activity eq 0 then (*pstate).rate_last = sum
		busy = launch_total_activity( (*pstate).activity.display.ET_spectra) + $
						launch_total_activity( (*pstate).activity.display.ET_spectraT)
		dt = (*pstate).time_ET_spectra_max - (*pstate).time_ET_spectra_update
		if busy gt 20 then begin
			(*pstate).time_ET_spectra_update = ((*pstate).time_ET_spectra_update + dt*0.15) < (*pstate).time_ET_spectra_max
		endif else if busy gt 40 then begin
			(*pstate).time_ET_spectra_update = ((*pstate).time_ET_spectra_update + dt*0.4) < (*pstate).time_ET_spectra_max
		endif else if busy gt 80 then begin
			(*pstate).time_ET_spectra_update = (*pstate).time_ET_spectra_max
		endif
		dt = (*pstate).time_ET_spectra_update - (*pstate).time_ET_spectra_min
		if busy lt 20 then begin
			(*pstate).time_ET_spectra_update = ((*pstate).time_ET_spectra_update - dt*0.15) > (*pstate).time_ET_spectra_min
		endif else if busy lt 5 then begin
			(*pstate).time_ET_spectra_update = ((*pstate).time_ET_spectra_update - dt*0.4) > (*pstate).time_ET_spectra_min
		endif
	endif
	if percent gt 10. then print,'Update ET Spectra: percent=',percent,'  busy=',(*pf)[0],' lost=',(*pf)[1],' time_et=',t_interval
	error = 0
	return
end
