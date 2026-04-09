pro geopixe_execute, workerParam, olun=olun, progress=progress, error=error, $
			workerIndex=workerIndex, totalWorkers=totalWorkers, Result=strResult, $
			spectra=spectra_arg, images=images_arg, log=log

; Use 'execute' to execute a (potentially very long) command-line string
; (this requires a full IDL license, as NV mode dopes not support 'execute').
;
; If worker arguments are present, pass these as arguments to divide problem into
; 'stripes'.
;
; /progress	show a progress bar (passes /progress to command, which must support this).

	if n_elements(log) lt 1 then log=0

	strResult = ''
	error = 1
	if n_elements(progress) eq 0 then progress=0
	tic

	if log then gprint,level=1, output=olun, 'GeoPIXE_execute - processing ...'

	t = workerParam
	t = hide_embedded(t, ',')   								; hide ',' or '=' within
	t = hide_embedded(t, '=')  									; containers ({}, [], ...)

	str = strsplit(t, ',', /extract)
	ns = n_elements(str)

	if ns lt 1 then begin
		if log then gprint,level=2, output=olun, string(t, ns, $
			format='(%"GeoPIXE Worker - strsplit failed! (\"%s\" => %d)")')
		goto, cleanup
	endif

  	pos_args = where(strpos(str, '=') lt 0, npos)				; command, positional args

	if npos lt 1 then begin
    	if log then gprint,level=2, output=olun, string(str, npos, $
			format='(%"GeoPIXE Worker - where/strpos failed! (\"%s\" => %d)")')
		goto, cleanup
	endif

	command = str[pos_args[0]]   								; command name

	if log then gprint,level=1, output=olun, string(command, $
		format='(%" - function name = \"%s\"")')

	if npos gt 1 then begin
		if log then gprint,level=1, output=olun, ' - positional args ...'

		for i = 1, npos-1 do begin
;			if log then gprint,level=2, output=olun, string(i, pos_args[i], str[pos_args[i]], $
;				format='(%"   - i=%d, pos_args[i]=%d, str[]=\"%s\"")')

			s1 = hide_embedded(str[pos_args[i]], ',', /unhide)

;			if log then gprint,level=2, output=olun, i, s1, format='(3x,"> ",I," = ",A)'
			if log then gprint,level=1, output=olun, i, format='(3x,"> ",I)'

			x = unstringify(s1, error=errstat)
			if log then pointer_display, x, unit=olun

			if errstat eq 0 then begin
				var = 'pos_'+strtrim(string(i),2)				; var name
				com = var + ' = x' 								; execute to create local copy
																; of each positional argument
				status = execute(com)

;				if log then gprint,level=1, output=olun, string(com, status, $
;					format='(%"   - execute(\"%s\") => %d)")')

				if status eq 0 then begin
					if log then gprint,level=2, output=olun, 'Error executing: ' + com
					goto, cleanup
				endif

				command = command + ', ' + var

			endif else begin
				if log then gprint,level=2, output=olun, 'Error unstringing: ' + s1
				goto, cleanup
			endelse
		endfor
	endif

	key_args = where(strpos(str, '=') ge 0, nkey)				; keyword args

	if nkey gt 0 then begin
		if log then gprint,level=1, output=olun, ' - keyword args ...'

		for i = 0,nkey-1 do begin

			s = strsplit(str[key_args[i]], '=', /extract)
			ns = n_elements(s)

			if ns eq 2 then begin
				s1 = hide_embedded(s[1], ',', /unhide)
				s1 = hide_embedded(s1, '=', /unhide)
				
;				if log then gprint,level=2, output=olun, s[0]+'_arg', s1, format='(3x,"> ",A," = ",A)'
				if log then gprint,level=2, output=olun, s[0]+'_arg =', format='(3x,"> ",A)'
				
				x = unstringify(s1, error=errstat)
				if log then pointer_display, x, unit=olun

	        	if errstat eq 0 then begin
					com = s[0] + '_arg = x'     				; execute to create local copy
																; of each keyword argument
					status = execute(com)

;					if log then gprint,level=1, output=olun, string(com, status, $
;						format='(%"   - execute(\"%s\") => %d)")')

					if status eq 0 then begin
						if log then gprint,level=2, output=olun, 'Error executing: ' + com
						goto, cleanup
					endif

					command += ', ' + s[0] + '=' + s[0] + '_arg'

				endif else begin
					if log then gprint,level=2, output=olun, 'Error unstringing: ' + s1
					goto, cleanup
				endelse
			endif
		endfor
	endif

	if n_elements(totalWorkers) ge 1 then begin
		if n_elements(workerIndex) eq 0 then workerIndex=0

		command += ', cluster_index=' + strtrim(string(workerIndex), 2)
		command += ', cluster_total=' + strtrim(string(totalWorkers), 2)
		command += ', cluster_result=strResult'
		if log then begin
			command += ', cluster_debug=' + strtrim(string(olun), 2)
		endif else begin
			command += ', cluster_debug=' + strtrim(string(-2), 2)
		endelse
	endif
	if progress then command += ', /progress'

	if log then gprint,level=2, output=olun, string(command, format='(%" - final execute string is \"%s\"")')
	if log then toc, lun=olun else toc

;	Any "toc" commands in this execution, will continue to print to olun unit.

	status = execute(command)
	if log then toc, lun=olun else toc

	if status eq 0 then begin
		if log then gprint,level=2, output=olun,' '
		if log then gprint,level=2, output=olun,'Error state struct on completion of Execute ... '
		if log then help, !error_state, output=s
		if log then gprint,level=2, output=olun, s

		if log then gprint,level=2, output=olun,' '
		if log then gprint,level=2, output=olun, string(command, !error_state.msg, format='(%"execute(%s) failed: %s")')
		warning, 'geopixe_parallel', string(command, !error_state.msg, format='(%"execute(%s) failed: %s")')
	endif else begin
		MESSAGE, /RESET											; clear EOF in execute file reads
	endelse

	if n_elements(totalWorkers) ge 1 then begin
		if log then gprint,level=2, output=olun,' '
		if log then gprint,level=2, output=olun,'Results string= ',strResult
	endif
	error = 0
	return

cleanup:
	error = 1
	return
end
