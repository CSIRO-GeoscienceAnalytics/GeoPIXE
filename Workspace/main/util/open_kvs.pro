
pro set_kvs, kvs, key, f, val, hash=ishash, list=islist, huge=huge, lower=lower, error=error

; Set a 'key' to a value 'f' in the key-value store 'kvs'
; The value 'f' can be a struct, in which case it is converted to Hash.
; the value is converted to a Redis string JSON entry within the KVS class.
;
; If /hash, then set a Redis 'Hash' datatype entry in KVS and either:
;	w/ val		in the KVS entry 'key' set the hash item 'f' to 'val'
;	no val		set the KVS entry 'key' to the hash 'f'
;
; If /list, then will set an entire List key. To 'append' to an existing List in KVS
; use 'append_kvs' below.
;
; /huge indicates setting a large KVS data value, so use a long timeout temporarily
;
;	set_kvs, kvs, key, value
;	set_kvs, kvs, key, /hash, hash
;	set_kvs, kvs, key, /hash, item, value
;	set_kvs, kvs, key, /list, list
;
; /lower to force lowercase keys in KVS hash
;
;	Take care as struct val has lost case, and so will not replicate a mixed case hash
;	in KVS! i.e. Can't 'get_kvs' and then 'set_kvs' and keep mixed case!
;	To fiddle a KVS hash key's contents, manipulate it as a hash only.

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
		warning,'set_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		if timeout_changed then kvs.timeout = told
		error = 1
		return
	endif
endif
if n_elements(ishash) eq 0 then ishash=0
if n_elements(islist) eq 0 then islist=0
if n_elements(huge) eq 0 then huge=0
if n_elements(lower) eq 0 then lower=0
if (ishash eq 0) and (n_elements(val) ne 0) then return
if typename(kvs) ne 'PYTHON' then return

	timeout_changed = 0
	if huge then begin
		told = kvs.timeout
		kvs.timeout = 100. * told
		timeout_changed = 1
	endif

	if islist then begin
		if var_type(f) ne 110 then begin						; not a LIST
			v = list( f, /extract)
		endif else v=f

		for i=0,n_elements(v)-1 do begin
			old = kvs.lpush(key, v[i])
		endfor

	endif else if ishash then begin
		if n_elements( val) eq 0 then begin
			if var_type(f) ne 111 then begin					; not a HASH
				if size( val, /tname) eq 'STRUCT' then begin	; a Struct
					v = struct_to_hash(f, lower=lower)			; use /lower to force lowercase keys
				endif else v=hash(f)
			endif else v=f

			old = kvs.hsetall( key, v)

		endif else begin										; set item within Hash
			if size( val, /tname) eq 'STRUCT' then begin
				v2 = struct_to_hash(val, lower=lower)			; use /lower to force lowercase keys
			endif else v2=val

			old = kvs.hset( key, f, v2)
		endelse
	endif else begin
		if size(f, /tname) eq 'STRUCT' then begin
			v = struct_to_hash(f, lower=lower)					; use /lower to force lowercase keys
		endif else v=f

		kvs[key] = v
	endelse

done:
	if timeout_changed then kvs.timeout = told
	error = 0
	return
end

;-------------------------------------------------------------------------------------------------------

pro append_kvs, kvs, key, val, error=error

; Append a new 'value' to a 'key' that is of Redis type List in the key-value store 'kvs'.
; This only works for Redis List type 'keys'. For a string/JSON use 'set_kvs'.
;
; The new 'val' does have to be a string (e.g. JSON or plain string), or
; if a structure, it is converted to JSON. It only makes sense for a struct containing
; strings and items easily tested as equal (e.g. not floating point).
;
; Ignores adding a 'val' that is already in the key's list (case-sensitive test).

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
		warning,'append_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if typename(kvs) ne 'PYTHON' then return

	if size(val, /tname) eq 'STRUCT' then begin
		v = json_serialize(val)
	endif else v=val

	lold = kvs.lget(key)
	if n_elements(lold) gt 0 then begin
		q = where( v eq lold, nq)
		if nq gt 0 then goto, done
	endif

	r = kvs.lpush(key,v)

done:
	error = 0
	return
end

;-------------------------------------------------------------------------------------------------------

function exchange_kvs, kvs, key, val, template=template, error=error

; Exchange a 'key' to a new 'value' in the key-value store 'kvs'
; and return the old value.
; If 'template' is a struct, reform val to match this template.

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
		warning,'exchange_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
if typename(kvs) ne 'PYTHON' then return, 0

	if size(val, /tname) eq 'STRUCT' then begin
		v = hash (val)
	endif else v=val

	v2 = kvs.exchange(key, v)

	if is_a_hash( v2) then begin
		s0 = hash_to_struct( v2)

		if size( template, /tname) eq 'STRUCT' then begin
			v2 = template
			struct_assign, s0, v2									; do relaxed struct assign
		endif else v2=s0
	endif

	error = 0
	return, v2
end

;-------------------------------------------------------------------------------------------------------

pro check_kvs, kvs, error=error

; Check that the key-value store 'kvs' is working and ZMQ ports are functioning

COMPILE_OPT STRICTARR
common c_check_kvs, check_seq
if n_elements(check_seq) eq 0 then check_seq=0

	error = 1
	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin				; catch on 'exists' test
		Catch, /cancel
		on_error, 1

		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		print,['check_kvs','IDL run-time error caught.', '', $
				'Failed to write "test.idl.'+str_tidy(check_seq)+'" key ...  Retry.','', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c]
		MESSAGE, /RESET
		error = 1
		return
	endif
	if typename(kvs) ne 'PYTHON' then return

;	print,'check_kvs: enter ...'

	error = 0
	key = 'test.idl.' + str_tidy(check_seq++)
	kvs[key] = 'test'

	if check_seq ge 10 then check_seq=0

;	print, fred.joe
;	print,'check_kvs: "test.idl.'+str_tidy(check_seq)+'" return.'
	return
end

;-------------------------------------------------------------------------------------------------------

pro delete_kvs, kvs, key, error=error

; Delete a 'key' value from the key-value store 'kvs'

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
			warning,'delete_kvs',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			error = 1
			return
		endif
	endif
	if typename(kvs) ne 'PYTHON' then return

	if exists_kvs(kvs, key, error=err) eq 0 then begin
		error = err
		return
	endif

	r = kvs.delete(key)

	error = 0
	return
end

;-------------------------------------------------------------------------------------------------------

function exists_kvs, kvs, key, error=error

; Test a 'key' for existence in the key-value store 'kvs'

COMPILE_OPT STRICTARR

	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin				; catch on 'exists' test
		Catch, /cancel
		on_error, 1
		print,'exists_kvs: "exists" test timeout error ...  Continue.'
		MESSAGE, /RESET
		error = 1
		return, 0
	endif

;	print,'exists_kvs: enter ...'

	error = 1
	if typename(kvs) ne 'PYTHON' then return, 0

	error = 0
	r = kvs.exists(key)

;	print,'exists_kvs: return.'
	return, r
end

;-------------------------------------------------------------------------------------------------------

function get_kvs, kvs, key, f, list=islist, hash=ishash, template=template, huge=huge, error=error

; Get a 'key' value from the key-value store 'kvs'
; If 'template' is a struct, reform val to match this template.
;
; /list indicates that it's a special Redis List type, else it's assumed to be string JSON.
; /hash indicates that it's a special Redis Hash type. Return item 'f' or whole hash.
; /huge indicates reading a large KVS data value, so use a long timeout temporarily

	COMPILE_OPT STRICTARR
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
			warning,'get_kvs',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			error = 1
			if timeout_changed then kvs.timeout = told
			return, 0
		endif
	endif
	if n_elements(islist) lt 1 then islist=0
	if n_elements(ishash) eq 0 then ishash=0
	if n_elements(huge) eq 0 then huge=0
	if typename(kvs) ne 'PYTHON' then return, 0
	error = 1

;	print,'get_kvs: key =', key

	timeout_changed = 0
	if exists_kvs(kvs, key, error=err) eq 0 then begin
		return, 0
	endif
	if huge then begin
		told = kvs.timeout
		kvs.timeout = 20. * told
		timeout_changed = 1
	endif

	error = 1
	if islist then begin
		val = kvs.lget(key)
	endif else if ishash then begin
		if n_elements(f) eq 0 then begin
			val = kvs.hgetall(key)
		endif else begin
			val = kvs.hget(key,f)
		endelse
	endif else begin
		val = kvs[key]
	endelse
	if timeout_changed then kvs.timeout = told

	if is_a_hash( val) then begin
		s0 = hash_to_struct( val)

		if size( template, /tname) eq 'STRUCT' then begin
			val = template
			struct_assign, s0, val							; do relaxed struct assign
		endif else val=s0
	endif

;	print,'get_kvs: return val =', val
	error = 0
	return, val
end

;-------------------------------------------------------------------------------------------------------

function query_kvs, kvs, pattern, error=error

; Get a list of all keys value from the key-value store 'kvs' that match 'pattern'

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
			warning,'query_kvs',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			error = 1
			return, ''
		endif
	endif
	if typename(kvs) ne 'PYTHON' then return, ''

	keys = kvs.search(pattern)

	error = 0
	return, keys
end

;--------------------------------------------------------------------------------------------------------

pro close_kvs, kvs, error=error

; Close the key-value store object

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
		warning,'close_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
common c_zmq_ztap, ztap, ztap_done

	if typename(kvs) ne 'PYTHON' then return

;	r = kvs.stop()
;	r = kvs.closeall()
	obj_destroy, kvs
	if n_elements(ztap) gt 0 then obj_destroy, ztap

	kvs = 0
	error = 0
	return
end

;--------------------------------------------------------------------------------------------------------

function open_kvs, endpoint, comms=comms, ztap=use_ztap, perth=perth, error=error

; Open the key-value store at 'endpoint' and return Python object for it 'kvs'

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
		warning,'open_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0L
	endif
endif
common c_zmq_ztap, ztap, ztap_done
if n_elements(use_ztap) eq 0 then use_ztap=0
if n_elements(ztap_done) eq 0 then ztap_done=0
if n_elements(perth) eq 0 then perth=0
if n_elements(endpoint) eq 0 then endpoint=''

if endpoint eq '' then begin
	if perth then begin
;		endpoint = 'tcp://mr-05-per.it.csiro.au:29320'
		endpoint = 'tcp://mm-per-1-kf.it.csiro.au:29320'
	endif else begin
;		endpoint = 'tcp://mr-04-mel.it.csiro.au:29320'
		endpoint = 'tcp://mm-mel-1-cl.it.csiro.au:29320'
	endelse
endif
if endpoint eq 'null' then return, 0

	error = 0
	if n_elements(comms) eq 0 then comms = open_comms(error=error, ztap=use_ztap)
	if error then return, 0L

	error = 1
	if use_ztap then begin
;		kvclass = python.import('test_vsub')
;		kvs = kvclass.make_kvs( comms=comms, debug=use_ztap)

		kvclass = python.import('mmkvs.MMkvs')
		kvs = kvclass.MMkvs( endpoint, comms=comms, timeout=20.0)

		if ztap_done eq 0 then begin
			ztap_class = python.import('mmlib.MMztap')
			ztap = ztap_class.MMztap('tcp://*:29001', comms=comms)
			ztap_done = 1
		endif
	endif else begin
		kvclass = python.import('mmkvs.MMkvs')
		kvs = kvclass.MMkvs( endpoint, comms=comms, timeout=20.0)
	endelse

	error = 0
	return, kvs
end
