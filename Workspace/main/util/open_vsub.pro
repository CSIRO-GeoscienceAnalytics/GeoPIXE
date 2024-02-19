
function get_vsub, vsub, key, template=template, error=error

; Get the latest 'key' value from the vsub object 'vsub'.
; If 'template' is a struct, reform val to match this template.
; Note: the key-value call will wait until vsub/keys are synchronized.

COMPILE_OPT STRICTARR
error = 1
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'get_vsub',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
if typename(vsub) ne 'PYTHON' then return, 0

	r = get_vsub_server_active( vsub, error=error)
	if (r eq 0) or error then return, 0

;	print,'get_vsub: ', key
	error = 1
	q = where( vsub.keys() eq key, nq)
	if nq eq 0 then return, 0

	val = vsub[key]

	if is_a_hash( val) then begin
		s0 = hash_to_struct( val)

		if (size( template, /tname) eq 'STRUCT') and (size( s0, /tname) eq 'STRUCT') then begin
			val = template
			struct_assign, s0, val									; do relaxed struct assign
		endif else val=s0
	endif

;	print,'get_vsub: return.'
	error = 0
	return, val
end

;--------------------------------------------------------------------------------------------------------

pro set_vsub, vsub, key, val, error=error

; Set a 'key' value in the vsub object 'vsub'. NOT IMPLEMENTED YET

COMPILE_OPT STRICTARR
error = 1
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'set_vsub',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if typename(vsub) ne 'PYTHON' then return

	if vsub.server_active eq 0 then return
	q = where( vsub.keys() eq key, nq)
	if nq eq 0 then return

	vsub[key] = val

	error = 0
	return
end

;--------------------------------------------------------------------------------------------------------

function get_vsub_server_active, vsub, error=error

; Get the latest 'server_active' status.

COMPILE_OPT STRICTARR
error = 1
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'get_vsub_server_active',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
if typename(vsub) ne 'PYTHON' then return, 0

;	r = vsub.server_active
	r = vsub.is_usable()

	error = 0
	return, r
end

;--------------------------------------------------------------------------------------------------------

function req_vsub_persist, vsub, req, val, seq=seq, message=message, error=error, errno=errno

; Send a command to the REQ endpoint of this 'vsub' VSUB object,
; and pass the value 'val'. If OK return error=0 and 'res'.
; If error return error=1 and 'err in 'messsage'
;
; If an error occurs, flag this in a warning pop-up, which disappears after 10s.
; Then try again and continue 'persisting'.

COMPILE_OPT STRICTARR
ErrorNo = 0
errno = 0
error = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'req_vsub_persist',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif

start:
	r = req_vsub( vsub, req, val, seq=seq, message=message, timeout=10., error=error, errno=errno)
	if error eq 0 then return, r

	log_message, vsub, type='ERROR', 'req_vsub_persist, Error in REQ "'+req+'", VAL="'+str_tidy(val)+'"; '+message+' Wait and try again ...'
	warning,timeout=10., 'req_vsub_persist', ['Error in REQ "'+req+'", VAL="'+str_tidy(val)+'"', $
							message,'','Wait and try again ...'], cancel=cancel
	if cancel then begin
		error = 1
		message = 'Cancel: Abort loop on REQ VSUB: REQ "'+req+'", VAL="'+str_tidy(val)+'"'
		return, 0
	endif
	goto, start
end

;--------------------------------------------------------------------------------------------------------

function req_vsub, vsub, req, val, seq=seq, message=message, timeout=timeout1, notimeout=notimeout, error=error, errno=errno

; Send a command to the REQ endpoint of this 'vsub' VSUB object,
; and pass the value 'val'. If OK return error=0 and 'res'.
; If error return error=1 and 'err in 'messsage', error code in 'errno'
;
; This has a default timeout, but is generaly called after a 'get_vsub', which would
; wait for the vsub to be ready. So it should not timeout.

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
errno = 0
message = ''
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'req_vsub',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
common c_vsub_1, vsub_seqno
common c_vsub_2, cmd
if n_elements(vsub_seqno) eq 0 then vsub_seqno=1
if n_elements(timeout1) eq 0 then timeout1=10.0
if n_elements(notimeout) eq 0 then notimeout=0
if typename(vsub) ne 'PYTHON' then return, 0
timeout = timeout1
if notimeout then timeout = !Null

	if n_elements(req) eq 0 then begin
		warning,'req_vsub','Missing "req" command.'
		return, ''
	endif
	if n_elements(val) eq 0 then val = 'none'
;	print,'req_vsub: send command = ',req

	if n_elements(cmd) eq 0 then begin
		cmd_class = python.import('mmlib.MMcmd')
		cmd = cmd_class.MMcmd( mmcl=vsub.mmcl, req=req, val=val)
	endif else begin
		cmd.req = req
		cmd.val = val
		if python.hasattr(cmd,'rep') then cmd.rep = ''
		if python.hasattr(cmd,'res') then cmd.res = ''
		if python.hasattr(cmd,'err') then cmd.err = ''
		if python.hasattr(cmd,'code') then cmd.code = 0
	endelse

	seq = vsub_seqno
	r = vsub.request_reply( cmd, seq=vsub_seqno++, timeout=timeout)

	error = 0
	if isa(r) eq 0 then begin				; n_elements() returns wrong!  ;@10-18
		print, 'req_vsub: Error: Null return (timeout?).'
		message = 'Null return (timeout?)'
		error = 1
	endif else begin
		if python.hasattr(r,'rep') then begin
			error = (r.rep eq 'ERROR') or (r.rep ne 'OK')
			if error then begin
				message = 'Error inferred from reply: '+r.rep
				print, 'req_vsub: Error inferred from reply: '+r.rep
				if python.hasattr(r,'val') then begin
					if is_a_hash(r.val) then begin
						s = hash_to_struct(r.val)
						if tag_present('error',s) then message = s.error
					endif
				endif
			endif
		endif else error=1
		if python.hasattr(r,'err') then begin
			message = r.err
			if message ne '' then print, 'req_vsub: "err" attr return: '+r.err
		endif
		if python.hasattr(r,'code') then begin
			errno = r.code
			if errno ne 0 then print, 'req_vsub: "code" attr return: '+str_tidy(r.code)
		endif
	endelse

	return, python.hasattr(r,'res') ? r.res : (python.hasattr(r,'rep') ? r.rep : 'ERROR')
end

;--------------------------------------------------------------------------------------------------------

pro close_vsub, vsub, error=error

; Close the VSUB Python object

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'close_vsub',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if typename(vsub) ne 'PYTHON' then return

;	r = vsub.__del__()
	r = vsub.stop()
	r = vsub.closeall()
	obj_destroy, vsub

	vsub = 0
	error = 0
	return
end

;--------------------------------------------------------------------------------------------------------

function open_vsub, kvs, endpoints, comms=comms, mmcl=mmcl, subscribe=subscribe, ztap=use_ztap, error=error

; Open a VSUB 'endpoint' and return Python object for it 'vsub'.
; This only deals with the asynchronous access to published parameter,s.
;
; Use the KVS entries:
;	(kvs[endpoints])["SUB"]	for the SUB port
;	(kvs[endpoints])["REQ"]	for the REQ port

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
vsub = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'open_vsub',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
if n_elements(endpoints) eq 0 then endpoints = 'MM.test.endpoints'
;if n_elements(mmcl) eq 0 then mmcl = 'test.a.b.c.x'
;if n_elements(subscribe) eq 0 then subscribe = 'test.a.b'
if n_elements(subscribe) eq 0 then subscribe = ''
if n_elements(use_ztap) eq 0 then use_ztap=0
if typename(kvs) ne 'PYTHON' then return, 0

	error = 0
	if n_elements(comms) eq 0 then comms = open_comms(error=error, ztap=use_ztap)
	if error then return, 0L

	error = 1
	print,'Open VSUB: Lookup KVS for endpoints SUB, REQ ...'
	if exists_kvs( kvs, endpoints, error=err) then begin
		ends = get_kvs( kvs, endpoints, error=err)
		if err then begin
			warning,timeout=10.,'open_vsub',['Endpoints: '+endpoints,'Not found in KVS.']
			return, 0
		endif
		sub_endpoint = ends.sub
		req_endpoint = tag_present('req',ends) ? ends.req : ''
		print,'	Connect to endpoints SUB=',sub_endpoint,', REQ=',req_endpoint
	endif else begin
		warning,timeout=10.,'open_vsub',['Endpoints: '+endpoints,'Not found in KVS.']
		return, 0
	endelse

	if use_ztap then begin
;		vsub_class = python.import('test_vsub')
;		vsub = vsub_class.make_vsub( kvs, comms=comms, endpoints=endpoints, mmcl=mmcl, subscribe=subscribe, debug=use_ztap)

		vsub_class = python.import('mmlib.MMvsub')
		if req_endpoint eq '' then begin
			vsub = vsub_class.MMvsub( subaddr=sub_endpoint, mmcl=mmcl, comms=comms, $
						timeout=30.0, keepalive=1.0, snapshot_timeout=30.0)
		endif else begin
			vsub = vsub_class.MMvsub( subaddr=sub_endpoint, reqaddr=req_endpoint, mmcl=mmcl, comms=comms, $
						timeout=30.0, keepalive=1.0, snapshot_timeout=30.0)
		endelse
		r = vsub.subscribe( subscribe)
	endif else begin
		vsub_class = python.import('mmlib.MMvsub')
		if req_endpoint eq '' then begin
			vsub = vsub_class.MMvsub( subaddr=sub_endpoint, mmcl=mmcl, comms=comms, $
						timeout=30.0, keepalive=1.0, snapshot_timeout=30.0)
		endif else begin
			vsub = vsub_class.MMvsub( subaddr=sub_endpoint, reqaddr=req_endpoint, mmcl=mmcl, comms=comms, $
						timeout=30.0, keepalive=1.0, snapshot_timeout=30.0)
		endelse
		r = vsub.subscribe( subscribe)
	endelse

	print,'	VSUB open succesful.'
	error = 0
	return, vsub
end
