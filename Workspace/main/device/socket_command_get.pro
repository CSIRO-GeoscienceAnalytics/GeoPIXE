;function socket_command_get, ps, commandi, chip=ichip, channel=ichannel, error=error, $
;					class=tag, quiet=quiet, string=vstring, float=vfloat, long=vlong, $
;					n_chips=n_chips, n_channels=n_channels, debug=debug, double=vdouble
;
;	GET a response to a SET from a Maia socket
;		ps			pointer to socket port parameters
;		class		tag name or parameter class
;		chip		chip to control (0-11; -1 for ALL), can be a vector of indices
;		command		parameter to set
;		channel		channel(s) on chip (0-31; -1 for ALL), can be a vector of indices
;		quiet		do not pop-up error report
;		
;	returns
;		values of class[chips].command[channels]
;		error		0 no error, else error code
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
; Example calls:
; 		r = get( ps, 'LOCK', class='scepter', chip=-1)
;		r = get( ps, 'GAIN', class='hermes', chip=2)
;		r = get( ps, 'DAC', class='dac', chip=1, channel=[1,3])
;		r = get( ps, 'INIT', class='maia')
;
;					for equal numbers of chip and channel:
;		r = get( ps, 'ELK', class='hermes', chip=[3,11], channel=[13,31])
;					take this to mean chip=3,channel=13 and chip=11,channel=31
;
;					if only one of chip or channel, then use mutiple like this:
;		r = get( ps, 'ELK', class='hermes', chip=[3,11], channel=[13])
;					take this to mean chip=3,11, channel=13
;		r = get( ps, 'ELK', class='hermes', chip=[-1], channel=[13,31])
;					take this to mean chip=* ,channel=13,31
;
;----------------------------------------------------------------------------------

function socket_command_get_item, ps, scom, sjoin=sjoin, debug=debug, error=error, $
			string=vstring, float=vfloat, long=vlong, double=vdouble, l64=l64, ul64=ul64, quiet=quiet

; send command string and monitor return

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
		warning,'socket_command_get_item',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_socket_io, ddm_down, ddm_error_reported
if n_elements(ddm_down) lt 1 then ddm_down=0
if n_elements(ddm_error_reported) lt 1 then ddm_error_reported=0

if n_elements(sjoin) lt 1 then sjoin=0
if n_elements(debug) lt 1 then debug=0
if n_elements(vstring) lt 1 then vstring=0
if n_elements(vfloat) lt 1 then vfloat=0
if n_elements(vdouble) lt 1 then vdouble=0
if n_elements(vlong) lt 1 then vlong=0
if n_elements(l64) lt 1 then l64=0
if n_elements(ul64) lt 1 then ul64=0
if n_elements(quiet) lt 1 then quiet=0

start:
	if (*ps).open eq 0 then return, 0
	
	on_ioerror, bad_write
	printf, (*ps).unit, scom
	
	sret = ''
	on_ioerror, bad_read
	readf, (*ps).unit, sret
	t = fstat((*ps).unit)
	if t.size ne 0 then warning,'socket_command_get','Unit left with non-zero size='+str_tidy(t.size)
	if t.transfer_count eq 0 then warning,'socket_command_get','Unit left with zero transfer_count.'
	on_ioerror, null
	if debug then print, '   returned: ',sret
	
	; This form is needed for FILE name in da.info ...'
	str = str_break( sret, /keep)

	; This was needed for ...
	;	str = str_break( sret)

	send = strsplit( scom, ' 	', /extract)
	if n_elements(str) lt 3 then goto,  bad_read2
	if str[0] ne send[0] then goto, bad_read3				; check token
	if (str[1] eq 'error') then goto, bad_read4
	if n_elements(str) ge 3 then begin
		if ((str[1] eq 'get') and (str[2] eq 'error')) then goto, bad_read4
	endif
	if vstring then begin
		v = str_escape( str[2:*], /strip, join=sjoin)
	endif else if vfloat then begin
		v = float2(str[2:*])
	endif else if vdouble then begin
		v = double2(str[2:*])
	endif else if vlong then begin
		v = long2(str[2:*])
	endif else if l64 then begin
		v = long2_64(str[2:*])
	endif else if ul64 then begin
		v = ulong2_64(str[2:*])
	endif else begin
		if locate('"',str[2]) ge 0 then begin
			v = str_escape( str[2:*], /strip, join=sjoin)
		endif else if locate('.',str[2]) ge 0 then begin
			v = float2(str[2:*])
		endif else begin
			v = long2(str[2:*])
		endelse
	endelse
	error = 0
	(*ps).attempts = 0
	return, v
	
bad_read2:
	warning,'socket_command_get',['Bad number of words returned from Kandinski/Klee socket.','Client = '+(*ps).client]
	return,0
bad_read3:
	n1 = strlen(scom)
	s1 = strmid(scom,0,n1<40)
	n2 = strlen(sret)
	s2 = strmid(sret,0,n2<40)
	warning,'socket_command_get',['Bad token returned from Kandinski/Klee.','Client = '+(*ps).client, $
		'Sent token: "'+send[0]+'", Returned token: "'+str[0]+'"', $
		'Sent command: "'+s1+'"', 'Returned: "'+s2+'"']
	readf, (*ps).unit, sret
	return,0
bad_read4:
	if ddm_error_reported then return, 0
	if strjoin(str[2:*],' ') eq '"DDM communication error (token)"' then ddm_error_reported = 1
	if strjoin(str[2:*],' ') eq '"DDM communication error (exch)"' then ddm_error_reported = 1
	print,'Error in socket_command_get_vals:',['error in socket command to Kandinski/Klee:','Client = '+(*ps).client, scom, strjoin(str[2:*],' ')]
	if quiet eq 0 then warning,'socket_command_get',['error in socket command to Kandinski/Klee:','Client = '+(*ps).client, scom, strjoin(str[2:*],' ')]
	return,0

bad_write:
	warning, timeout=10.,'socket_command_get',['Bad write to Kandinski socket.','Client = '+(*ps).client, $
			'Command: '+scom, '','Retry socket open ...'], cancel=cancel
	if cancel then return, 0
	goto, retry
bad_read:
	warning, timeout=10.,'socket_command_get',['Bad read from Kandinski socket.','Client = '+(*ps).client, $
			'Command: '+scom, '','Retry socket open ...'], cancel=cancel
	if cancel then return, 0
	goto, retry
	
retry:
	socket_retry, ps, error=error
	if error eq 0 then goto, start
	return, 0
end

;----------------------------------------------------------------------------------

function socket_command_get_vals, scom, sret, sjoin=sjoin, error=error, $
			string=vstring, float=vfloat, long=vlong, l64=l64, ul64=ul64, double=vdouble, quiet=quiet, client=client

; send command string and monitor return

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
		warning,'socket_command_get_vals',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_socket_io, ddm_down, ddm_error_reported
if n_elements(ddm_down) lt 1 then ddm_down=0
if n_elements(ddm_error_reported) lt 1 then ddm_error_reported=0

if n_elements(sjoin) lt 1 then sjoin=0
if n_elements(vstring) lt 1 then vstring=0
if n_elements(vfloat) lt 1 then vfloat=0
if n_elements(vdouble) lt 1 then vdouble=0
if n_elements(vlong) lt 1 then vlong=0
if n_elements(l64) lt 1 then l64=0
if n_elements(ul64) lt 1 then ul64=0
if n_elements(quiet) lt 1 then quiet=0
if n_elements(client) lt 1 then client=''

; This form is needed for FILE name in da.info ...'
	str = str_break( sret, /keep)

; This was needed for ...
;	str = str_break( sret)

	send = strsplit( scom, ' 	', /extract)
	if n_elements(str) lt 3 then goto,  bad_read2
	if str[0] ne send[0] then goto, bad_read3				; check token
	if (str[1] eq 'error') then goto, bad_read4	
	if n_elements(str) ge 3 then begin
		if ((str[1] eq 'get') and (str[2] eq 'error')) then goto, bad_read4	
	endif
	if vstring then begin
		v = str_escape( str[2:*], /strip, join=sjoin)
	endif else if vfloat then begin
		v = float2(str[2:*])
	endif else if vdouble then begin
		v = double2(str[2:*])
	endif else if vlong then begin
		v = long2(str[2:*])
	endif else if l64 then begin
		v = long2_64(str[2:*])
	endif else if ul64 then begin
		v = ulong2_64(str[2:*])
	endif else begin
		if locate('"',str[2]) ge 0 then begin
			v = str_escape( str[2:*], /strip, join=sjoin)
		endif else if locate('.',str[2]) ge 0 then begin
			v = float2(str[2:*])	
		endif else begin
			v = long2(str[2:*])	
		endelse
	endelse
	error = 0
	return, v
	
bad_read2:
	warning,'socket_command_get_vals',['Bad number of words returned from Kandinski/Klee socket.','Client = '+client]
	return,0
bad_read3:
	n1 = strlen(scom)
	s1 = strmid(scom,0,n1<40)
	n2 = strlen(sret)
	s2 = strmid(sret,0,n2<40)
	warning,'socket_command_get_vals',['Bad token returned from Kandinski/Klee.','Client = '+client, $
										'Sent token: "'+send[0]+'", Returned token: "'+str[0]+'"', $
										'Sent command: "'+s1+'"', 'Returned: "'+s2+'"']
	return,0
bad_read4:
	if ddm_error_reported then return, 0
	if strjoin(str[2:*],' ') eq '"DDM communication error (token)"' then ddm_error_reported = 1
	if strjoin(str[2:*],' ') eq '"DDM communication error (exch)"' then ddm_error_reported = 1
	print,'Error in socket_command_get_vals:',['error in socket command to Kandinski/Klee:','Client = '+client, scom, strjoin(str[2:*],' ')]
	if quiet eq 0 then warning,'socket_command_get_vals',['error in socket command to Kandinski/Klee:','Client = '+client, scom, strjoin(str[2:*],' ')]
	return,0
end

;----------------------------------------------------------------------------------

function socket_command_get, ps, commandi, chip=ichipi, channel=ichanneli, error=error, $
					class=tag, quiet=quiet, string=vstring, float=vfloat, long=vlong, l64=l64, ul64=ul64, $
					n_chips=n_chips, n_channels=n_channels, debug=debug, double=vdouble

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
		warning,'socket_command_get',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
if ptr_valid(ps) eq 0 then goto, bad_ptr
if (*ps).open eq 0 then return, 0
version = (*ps).version
max_channels = (*ps).n_detectors
if n_elements(tag) eq 0 then tag=''
if n_elements(n_channels) lt 1 then n_channels = 32
if n_elements(n_chips) lt 1 then begin
	if max_channels lt 1 then warning,'socket_command_get','zero "max_channels".'
	n_chips = max_channels/n_channels
endif
multiple = 1  &  vector = 1
if n_elements(ichipi) lt 1 then multiple=0 else ichip=ichipi
if n_elements(ichanneli) lt 1 then vector=0 else ichannel=ichanneli
if n_elements(quiet) lt 1 then quiet=0
if n_elements(vstring) lt 1 then vstring=0
if n_elements(vfloat) lt 1 then vfloat=0
if n_elements(vdouble) lt 1 then vdouble=0
if n_elements(vlong) lt 1 then vlong=0
if n_elements(l64) lt 1 then l64=0
if n_elements(ul64) lt 1 then ul64=0
if n_elements(debug) lt 1 then debug=0

(*ps).token = str_tidy(((uint((*ps).token)+1) mod 1000US))

start:
	command = strlowcase(commandi)
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

n = n_elements(ichip)
n2 = n_elements(ichannel)
first = 1
val = 0
sjoin = (multiple eq 0) and (vector eq 0)
many = (multiple eq 1) or (vector eq 1)

if (n gt 1) and (n2 gt 1) then begin
	if n ne n2 then begin
		warning,'socket_command_get',['"chip" and "channel" count do not match.','Abort Maia Get.']
		return,0
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
		
		if lenchr(class2) gt 0 then begin
			scom = (*ps).token + ' get ' + class2 + '.' + command2
		endif else begin
			scom = (*ps).token + ' get ' + command2
		endelse
		if debug then print, 'Read Maia: ',scom
		
		v = socket_command_get_item( ps, scom, sjoin=sjoin, error=error, debug=debug, $
							string=vstring, float=vfloat, long=vlong, l64=l64, ul64=ul64, double=vdouble, quiet=quiet)
		
		if error eq 0 then begin
			if first then begin
				first = 0
				val = v
			endif else val=[val,v]
		endif
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

	if lenchr(class) gt 0 then begin
		scom = (*ps).token + ' get ' + class + '.' + command
	endif else begin
		scom = (*ps).token + ' get ' + command
	endelse
	if debug then print, 'Read Maia: ',scom
	if  n_elements(byte(scom)) gt 1020 then warning,'socket_command_get','Maia command string is >1020 bytes long.'

	val = socket_command_get_item( ps, scom, sjoin=sjoin, error=error, debug=debug, $
							string=vstring, float=vfloat, long=vlong, l64=l64, ul64=ul64, double=vdouble, quiet=quiet)
endelse
error = 0
if n_elements(val) eq 1 then val=val[0]
return, val

bad_ptr:
	warning,'socket_command_get','bad port pointer.'
	return,0
bad:
	warning,'socket_command_get','missing arguments.'
	return,0
end