
pro close_comms, old, error=error

; Close the comms used by MM KVS and ZMQ
; Note this applies to whole process, so don't do this in IDLDE mode.

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
		warning,'close_comms',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

;	print,'Close MMcomms ...'
	if n_elements(old) eq 0 then begin
		comms = python.import('mmlib.MMcomms')
		r = (comms.MMcomms.instance()).stop()
	endif else begin
		if typename(old) ne 'PYTHON' then return
		r = old.stop()
		r = old.closeall()
		obj_destroy, old
	endelse

	error = 0
	return
end

;--------------------------------------------------------------------------------------------------------

;	Save comms in common for use in implcit 'log_message' in 'Warning' and 'Alarm_popup'

pro save_comms_in_common, comms

	common c_logging_comms, logging_comms
	
	if n_elements(comms) lt 1 then comms=0
	logging_comms = comms
	return
end

;--------------------------------------------------------------------------------------------------------

function open_comms, ztap=use_ztap, source=source, server=server, error=error

; Open a 'comms' Python object to use for ZMQ messaging

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
		warning,'open_comms',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
common c_zmq_ztap, ztap, ztap_done
common c_zmq_logging, logging_setup
if n_elements(use_ztap) eq 0 then use_ztap=0
if n_elements(logging_setup) eq 0 then logging_setup=0
if n_elements(ztap_done) eq 0 then ztap_done=0
if n_elements(source) eq 0 then source='unknown'
if n_elements(server) eq 0 then server='mm-mel-1-cl.it.csiro.au'

	error = 1
	Cclass = python.import('mmlib.MMcomms')
	Lclass = python.import('mmlib.MMlog')

;	Note that this Logging step must come before any comms objects are created ...
;	This sets up logging for whole IDL session, and all programs!

	if (logging_setup eq 0) then begin
		if use_ztap then begin
			DEBUG = 10
			r = Lclass.MMlog.basic_config(level=DEBUG, server=server)
			logging_setup = 1
		endif else begin
;			Lclass.MMlog.LOG_LEVEL = Lclass.MMlog.INFO	; does not work with python 2.7.13 and above
			Lclass.MMlog.LOG_LEVEL = Lclass.INFO
			Lclass.MMlog.LOG_DEST = "syslog"
			Lclass.MMlog.LOG_SERVER = server
			Lclass.MMlog.LOG_IDENT = source
			r = Lclass.MMlog.basic_config()
			logging_setup = 1
		endelse
	endif

	comms = Cclass.MMcomms()

	error = 0
	return, comms
end

