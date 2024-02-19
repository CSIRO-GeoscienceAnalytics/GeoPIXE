;pro socket_command_set, ps, commandi, vali, chip=ichip, channel=ichannel, error=error, $
;					class=tag, n_channels=n_channels, n_chips=n_chips, queue=queue, next=next, $
;					quiet=quiet, report=report, get=pget, index=index, fail_on_retry=fail_on_retry
;
;	SET a Maia scommand variable via the socket
;		ps			pointer to socket port parameters
;					or pointer to script storage (with /queue set)
;		class		tag name or parameter class
;		chip		chip to control (0-11; -1 for ALL), can be a vector of indices
;		command		parameter to set
;		channel		channel(s) on chip (0-31; -1 for ALL), can be a vector of indices
;		val			value to set ichip.ichannel to. If vector, number must match total
;					of chip.channel combinations
;		/quiet		Do not report errors (store them for /report later).
;
;;		/queue		use the command queue (fill the queue by default)
;		/next		send the next line in the queue
;		get=pget	queue a "SET" command instead, and save 'p' as a ptr to
;					the return location following the get.
;		index=index	the index into the pget array for returned value(s)
;		/fail_on_retry  force an error return after socket retry, regardless of success.
;		
; Example calls:
; 
;	build a command of the form:
;		(multiple=1)
;			class[chip].command[channel]=value		; both chip and channel can be vectors
;			class[chip].command=value				; chip can be a vector
;		(multiple=0)
;			class.command[channel]=value			; channel can be a vector
;			class.command=value						; all scalar registers
;
;	where	'value'		is a number
;			'chip'		is a string of the form: 1,2,3,  3-4,  *
;			'channel'	is a string of the form: 1,2,3,  3-4,  *
;
; Example calls (here "set" means the full procedure name "socket_command_set"):
; 		set, ps, 'LOCK', 0, class='scepter', chip=-1
;		set, ps, 'GAIN', 3, class='hermes', chip=2
;		set, ps, 'DAC', 1234, class='dac', chip=1, channel=[1,3]
;		set, ps, 'INIT', 1, class='maia'
;
;					for equal numbers of chip and channel:
;		set, ps, 'ELK', 1, class='hermes', chip=[3,11], channel=[13,31]
;					take this to mean chip=3,channel=13 and chip=11,channel=31
;
;					if only one of chip or channel, then use mutiple like this:
;		set, ps, 'ELK', 1, class='hermes', chip=[3,11], channel=[13]
;					take this to mean chip=3,11, channel=13
;		set, ps, 'ELK', 1, class='hermes', chip=[-1], channel=[13,31]
;					take this to mean chip=* ,channel=13,31
;
;					for vector 'value', number must match:
;		set, ps, 'ELK', [1,0], class='hermes', chip=[3,11], channel=[13,31]
;					take this to mean chip=3,channel=13,value=1 and chip=11,channel=31,value=0

;----------------------------------------------------------------------------------------------

pro socket_command_set_item, ps, line, error=error, debug=debug, quiet=quiet, prefix=prefix, fail_on_retry=fail_on_retry

; send command string and monitor return

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
		warning,'socket_command_set_item',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_socket_io, ddm_down, ddm_error_reported
if n_elements(ddm_down) lt 1 then ddm_down=0
if n_elements(ddm_error_reported) lt 1 then ddm_error_reported=0

if n_elements(debug) lt 1 then debug=0
if n_elements(quiet) lt 1 then quiet=0
if n_elements(prefix) lt 1 then prefix=0
if n_elements(line) lt 1 then line='<null>'
if n_elements(fail_on_retry) lt 1 then fail_on_retry=0

start:
	error = 1
	if (*ps).open eq 0 then return
	scom = line
	if prefix then begin
		scom = (*ps).token + ' set ' + line
		print, 'Write Maia: ' + scom
	endif
	on_ioerror, bad_write
	printf, (*ps).unit, scom
	send = strsplit( scom, ' 	', /extract)
	
	sret = ''
	on_ioerror, bad_read
	readf, (*ps).unit, sret
	on_ioerror, null
	str = strsplit( sret, ' 	', /extract)
	if n_elements(str) lt 2 then goto,  bad_read2
	if str[0] ne send[0] then goto, bad_read3			; check token
	funny_error = 0
	if n_elements(str) ge 3 then begin
;		if ((str[1] eq 'set') and (str[2] eq 'error')) then funny_error=1
		if (str[2] eq 'error') then funny_error=1
	endif
	if (str[1] eq 'error') or funny_error then begin
		if quiet then begin
			(*ps).last_error = 1
			(*ps).last_command = scom
			(*ps).last_message = strjoin(str[2:*],' ')
		endif else begin
			goto, bad_read4
		endelse
	endif
	error = 0
	(*ps).attempts = 0
	return
	
bad_write:
	warning, timeout=10.,'socket_command_set_item',['bad write to Kandinski socket.', 'Client = '+(*ps).client, $
			'Command: '+scom, '', 'Retry socket open ...'], cancel=cancel
	if cancel then return
	goto, retry
bad_read:
	warning, timeout=10.,'socket_command_set_item',['Bad read from Kandinski socket.', 'Client = '+(*ps).client, $
			'Command: '+scom, '', 'Retry socket open ...'], cancel=cancel
	if cancel then return
	goto, retry
bad_read2:
	warning,'socket_command_set_item',['Bad number of words returned from socket.', 'Client = '+(*ps).client, $
			'Command: '+scom]
	return
bad_read3:
	warning,'socket_command_set_item',['Bad token returned.', 'Client = '+(*ps).client, $
			'Command: '+scom, ' Abort.']
	return
bad_read4:
	if ddm_error_reported then return
	if strjoin(str[2:*],' ') eq '"DDM communication error (token)"' then ddm_error_reported = 1
	if strjoin(str[2:*],' ') eq '"DDM communication error (exch)"' then ddm_error_reported = 1
	warning,'socket_command_set_item',['error in socket command: '+scom, strjoin(str[2:*],' '), ' Abort.'], /error
	return
	
retry:
	socket_retry, ps, error=error
	if fail_on_retry then begin
		log_warning,'socket_command_set_item','Retry returned, "fail_on_retry" set, so abort.'
		error = 1						; Force error return, as the command failed to be sent (possibly).
		return							; This is to stop re-sending 'newrun'.
	endif
	log_warning,'socket_command_set_item','Retry returned, "fail_on_retry" not set, error='+str_tidy(error)
	if error eq 0 then goto, start
	return
end

;----------------------------------------------------------------------------------------------

pro socket_command_set, ps, commandi, vali, chip=ichipi, channel=ichanneli, error=error, $
					class=tag, n_channels=n_channels, n_chips=n_chips, queue=queue, next=next, $
					quiet=quiet, report=report, get=pget, index=index, fail_on_retry=fail_on_retry

COMPILE_OPT STRICTARR
error = 1
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
		warning,'socket_command_set',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

if ptr_valid(ps) eq 0 then goto, bad_ptr
version = (*ps).version
max_channels = (*ps).n_detectors
if n_elements(fail_on_retry) lt 1 then fail_on_retry=0
if n_elements(report) lt 1 then report=0
if report then begin
	if (*ps).last_error then begin
		l = strlen((*ps).last_command)
		s = strmid((*ps).last_command,0,(l < 50)) + ' ...'
		warning,'socket_command_set',['error(s) in Maia command: '+s, (*ps).last_message]
		(*ps).last_error = 0
	endif
	return
endif
if n_elements(quiet) lt 1 then quiet=0
if quiet eq 0 then (*ps).last_error=0

if n_elements(queue) lt 1 then queue=0
get = 0
if n_elements(pget) ge 1 then begin
	if ptr_good(pget) then get = 1
endif
if n_elements(index) lt 1 then index=0L
if n_elements(next) lt 1 then next=0
if next then goto, do_script

; if (*ps).open eq 0 then return
if n_elements(tag) eq 0 then goto, bad
if get eq 0 then begin
	if n_elements(vali) eq 0 then goto, bad
endif
if n_elements(n_channels) lt 1 then n_channels = 32
if n_elements(n_chips) lt 1 then n_chips = 2 > (max_channels/n_channels)
multiple = 1  &  vector = 1
if n_elements(ichipi) lt 1 then multiple=0 else ichip=ichipi
if n_elements(ichanneli) lt 1 then vector=0 else ichannel=ichanneli

(*ps).token = str_tidy(((uint((*ps).token)+1) mod 1000US))

start:
	command = strlowcase(commandi)
	if get eq 0 then begin
		val = str_escape( vali)													; escape special chars
		if size(val,/tname) eq 'BYTE' then val=fix(val)							; prefix them with "\"
		if size(val,/tname) eq 'STRING' then begin
			for k=0,n_elements(val)-1 do if val[k] eq '' then val[k]='""'
		endif

		nv = n_elements(val)
		sv = strarr(nv)
		for i=0,nv-1 do begin													; check for embedded strings
			sv[i] = strtrim(string(val[i]),2)									; enclosed in ""
			bf = byte(sv[i])													; (except first, last char)						
			bb = byte('"')														; if present:	
			qf = where( bf eq bb[0], nqf)										;	replace with single '
			if nqf gt 0 then begin												;	enclose all in ""								
				qf2 = where( (qf gt 0) and (qf lt n_elements(bf)-1), nqf2)		; (workaround Kandinsky issue)
				if nqf2 gt 0 then begin
					bf[qf[qf2]] = byte("'")
					bf = [byte('"'),bf,byte('"')]
					sv[i] = string(bf)
				endif
			endif
		endfor
	endif
	n = n_elements(ichip)
	n2 = n_elements(ichannel)
	
; If we have selected all chips/channels individually and the other
; just selects one channel/chip or all (-1). Then set it to all (-1).

	if multiple and (n eq n_chips) and (n2 eq 1) then begin
		ramp = indgen(n_chips)
		q = where( (ichip[sort(ichip)] xor ramp) ne 0, nq)
		if nq eq 0 then ichip=-1
	endif
	if vector and (n2 eq n_channels) and (n eq 1) then begin
		ramp2 = indgen(n_channels)
		q = where( (ichannel[sort(ichannel)] xor ramp2) ne 0, nq)
		if nq eq 0 then ichannel=-1
	endif

; Use multiple=1 below for classes of ASIC, etc. that have multiple 'chips'

case version of
	else: class=tag
endcase

set = ' set '
if get then set = ' get '

n = n_elements(ichip)
n2 = n_elements(ichannel)

if (n gt 1) and (n2 gt 1) then begin
	if n ne n2 then begin
		warning,'socket_command_set',['"chip" and "channel" count do not match.','Abort Maia Set.']
		return
	endif
	if (nv gt 1) and (nv ne n) then begin
		warning,'socket_command_set',['"value" and "channel" count do not match.','Abort Maia Set.']
		return
	endif
	for i=0L,n-1 do begin
		if multiple then begin
			schip = socket_index_string(ichip[i])
			class2 = class + '[' + schip + ']'
		endif else class2=class
		if vector then begin
			schannel = socket_index_string(ichannel[i])
			command2 = command + '[' + schannel + ']'
		endif else command2=command
		
		if get eq 0 then begin
			sf = (nv eq 1) ? sv[0] : sv[i]
			if lenchr(class2) gt 0 then begin
				scom = (*ps).token + ' set ' + class2 + '.' + command2 + ' ' + strjoin(sf,' ')
			endif else begin
				scom = (*ps).token + ' set ' + command2 + ' ' + strjoin(sf,' ')
			endelse
		endif else begin
			if lenchr(class2) gt 0 then begin
				scom = (*ps).token + ' get ' + class2 + '.' + command2
			endif else begin
				scom = (*ps).token + ' get ' + command2
			endelse
		endelse
		print, 'Write Maia: ',strmid(scom,0,strlen(scom)<80)
		if strlen(scom) gt 65000 then begin
			mess = strjoin(['Maia command string is >65000 bytes long.', 'It may be truncated by "Kandinski".'], ' ')
			if quiet then begin
				(*ps).last_error = 1
				(*ps).last_command = scom
				(*ps).last_message = mess
			endif else begin
				warning,'socket_command_set', mess
			endelse
		endif
		if queue then begin
			(*(*ps).ps)[ (*ps).current] = scom
			if get then begin
				(*(*ps).pval)[ (*ps).current] = pget
				(*(*ps).pindex)[ (*ps).current] = index
			endif else (*(*ps).pval)[ (*ps).current] = ptr_new()
			(*ps).last = (*ps).current
			(*ps).current = (*ps).current+1
			if (*ps).current ge n_elements(*(*ps).ps)-10 then begin
				*(*ps).ps = [*(*ps).ps, strarr(100)]
				*(*ps).pval = [*(*ps).pval, ptrarr(100)]
				*(*ps).pindex = [*(*ps).pindex, lonarr(100)]
			endif
			error = 0
		endif else begin
			socket_command_set_item, ps, scom, error=error, debug=debug, quiet=quiet, fail_on_retry=fail_on_retry
			if error then break
		endelse
	endfor
endif else begin
	if multiple then begin
		schip = socket_index_string(ichip, nrun=n_chips)
		class = class + '[' + schip + ']'
	endif
	schannel = ''
	if vector then begin
		schannel = socket_index_string(ichannel, nrun=n_channels)
		command = command + '[' + schannel + ']'
	endif

	if get eq 0 then begin
		if lenchr(class) gt 0 then begin
			scom = (*ps).token + ' set ' + class + '.' + strlowcase(command) + ' ' + strjoin(sv,' ')
		endif else begin
			scom = (*ps).token + ' set ' + strlowcase(command) + ' ' + strjoin(sv,' ')
		endelse
	endif else begin
		if lenchr(class) gt 0 then begin
			scom = (*ps).token + ' get ' + class + '.' + strlowcase(command)
		endif else begin
			scom = (*ps).token + ' get ' + strlowcase(command)
		endelse
	endelse
	if strlen(scom) gt 60000 then begin
		mess = strjoin(['Maia command string is >60000 bytes long.', 'It may be truncated by "Kandinski".'], ' ')
		if quiet then begin
			(*ps).last_error = 1
			(*ps).last_command = scom
			(*ps).last_message = mess
		endif else begin
			warning,'socket_command_set', mess
		endelse
	endif
	if queue then begin
		(*(*ps).ps)[ (*ps).current] = scom
		if get then begin
			(*(*ps).pval)[ (*ps).current] = pget
			(*(*ps).pindex)[ (*ps).current] = index
		endif else (*(*ps).pval)[ (*ps).current] = ptr_new()
		(*ps).last = (*ps).current
		(*ps).current = (*ps).current+1
		if (*ps).current ge n_elements(*(*ps).ps)-10 then begin
			*(*ps).ps = [*(*ps).ps, strarr(100)]
			*(*ps).pval = [*(*ps).pval, ptrarr(100)]
			*(*ps).pindex = [*(*ps).pindex, lonarr(100)]
		endif
		error = 0
	endif else begin
		print, 'Write Maia: ',strmid(scom,0,strlen(scom)<80)
		socket_command_set_item, ps, scom, error=error, debug=debug, quiet=quiet, fail_on_retry=fail_on_retry
	endelse
endelse
return

do_script:
	if next then begin
		if (*ps).current gt (*ps).last then begin
			error = 0
			return
		endif
		scom = (*(*ps).ps)[(*ps).current]
		print, 'Write Maia: ', scom
		current = (*ps).current
		(*ps).current = (*ps).current + 1
		s = strsplit(scom,' ',/extract)
		ns = n_elements(s)
		if s[0] eq 'sleep' then begin
			(*ps).time = float(s[1])
			error = 0
			return
		endif else if s[1] eq 'get' then begin
			(*ps).time = 0.1
			v = socket_command_get_item( ps, scom, sjoin=sjoin, debug=debug, error=error)
			if error eq 0 then begin
				nv = n_elements(v)
				indx = (*(*ps).pindex)[current]
				pv =  (*(*ps).pval)[current]
				(*pv)[indx:indx + nv-1] = v
			endif
			return
		endif else begin
			(*ps).time = 0.1
		endelse
		if ns ge 3 then begin
			if s[2] eq 'blog.newrun' then fail_on_retry=1
		endif
		socket_command_set_item, ps, scom, error=error, debug=debug, quiet=quiet, fail_on_retry=fail_on_retry
		if fail_on_retry and error then return
		if next then goto, do_script
	endif
	return

bad_ptr:
	warning,'socket_command_set','bad port pointer.'
	return
bad:
	warning,'socket_command_set','missing arguments.'
	return
end