function spawn_bridge_object, path, client, args=args, error=error

; Create an IDL-Bridge object and set it up ...

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'spawn_bridge_object',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, obj
    endif

endif
if n_elements(debug) eq 0 then debug=0
error = 1

	obj = OBJ_NEW('IDL_IDLBridge')				; , CALLBACK='client_callback') executed on completion
;	obj->SetProperty, USERDATA=state			; could save a 'state' in 'obj' to be used in callback?

	obj->SetVar, "path", path
	obj->Execute, "cd, path"

	if n_elements(args) ge 1 then begin
		s = stringify( args)					; SetVar does not accept Struct variables!
		obj->SetVar, "sargs", s
		obj->Execute, client + ", args=sargs", /nowait
	endif else begin
		obj->Execute, client, /nowait
	endelse

	wait, 0.2
	status = obj->Status(error=msg)
	if status ne 1 then begin
		help, output=s, args
		warning,'spawn_bridge_object',['Failed to launch client process: '+client, '', $
				'Status = '+str_tidy(status)+' (0:idle, 1:executing, 2:completed, 3:Error halted, 4:Aborted)', $
				'Error = '+msg, '', 'Args = ',s] 
		return, obj
	endif

	error = 0
	return, obj

;	obj->Abort						; Use elsewhere to force abort of blog_client loop?
;	obj_destroy, obj				; Use on exit to force process exit (done in Maia-Launch)

end
