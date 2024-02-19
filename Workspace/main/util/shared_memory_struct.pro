function shared_memory_struct, prefix=prefixi, template=template, error=error, $
					n_long=n_long, n_byte=n_byte, n_float=n_float, init=init

; Open a shared memory buffer for Blog Client data using a template struct
;
; Use 'template' struct definition to define the shared memory structure.
;	prefix		text prefix for shared memory variable/array names (maximum of 12 characters)
;
;	If need to change the details of the struct, uncomment the line
;	'shared_memory_unmap' in the blog client programs, run them, stop using stop ppar,
;	then exit all processes that use the memory. It should then delete. Else reboot.
;
;	/init		initialize ppar flags
;
; Returns a pointer to a struct containing:
;	ppar			pointer to 16 long parameters for shared memory control
;	pdat			pointer to template shared struct
;	plong			pointer to general purpose Long array (16 or n_long)
;	pfloat			pointer to general purpose Float array (16 or n_float)
;	pbyte			pointer to general purpose byte array (256 or n_byte)
;	error			flags an error (0 = OK)
;
;	ppar array:	0	
;				1	
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-15	free

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if n_elements(prefixi) lt 1 then prefixi='blog_client_'
if n_elements(n_long) lt 1 then n_long=16
if n_elements(n_byte) lt 1 then n_byte=256
if n_elements(n_float) lt 1 then n_float=16
if n_elements(init) lt 1 then init=0
if n_elements(template) eq 0 then goto, bad
if size(template,/tname) ne 'STRUCT' then goto, bad

	prefix = prefixi[0]
	print,'shared_memory_struct: map buffers for prefix = '+prefix+' (#chars='+strtrim(string(n_elements(byte(prefix))),2)+') ...'
	n_pars = 16
	par_name = prefix + 'pars'

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		if !Error_state.name eq 'IDL_M_SHM_REDEFSEG' then begin
			print,'shared_memory_struct: "shmmap" IDL_M_SHM_REDEFSEG error on "'+par_name+'" - assume it is already established.'
			goto, skip1
		endif
		
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'shared_memory_struct',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			'Message:  '+!Error_state.msg, 'Code:  '+string(!Error_state.code), 'Sys Msg:  '+!Error_state.sys_msg, $
			'Sys Code:  '+strjoin(string(!Error_state.sys_code),','), 'Sys Code Type:  '+!Error_state.sys_code_type,'',c], /error
		MESSAGE, /RESET
		return, {error:1}
	endif

	shmmap, par_name, /long, dimension=n_pars
	
skip1:
	ppar = ptr_new( shmvar(par_name), /no_copy)

	;  outgoing (to blog_client):      reset kill
	;  incoming (from blog_client):              kill running  magic
	;                   0           1    2    3    4    5        6
	
	init_par = init
	if (*ppar)[6] ne 173 then init_par=1					; test if already set-up

	if init_par then begin
		print,'shared_memory_struct: Memory not set-up, so clear/set ppar ...'
		(*ppar)[*] = 0
		(*ppar)[6] = 173
	endif
	
	dat_name = prefix + 'dat_'
	long_name = prefix + 'long_'
	float_name = prefix + 'float_'
	byte_name = prefix + 'byte_'

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		if !Error_state.name eq 'IDL_M_SHM_REDEFSEG' then begin
			print,'shared_memory_struct: "shmmap" IDL_M_SHM_REDEFSEG error on "'+name+'" - assume it is already established.'
			goto, skip
		endif
		
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'shared_memory_struct',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			'Message:  '+!Error_state.msg, 'Code:  '+string(!Error_state.code), 'Sys Msg:  '+!Error_state.sys_msg, $
			'Sys Code:  '+strjoin(string(!Error_state.sys_code),','), 'Sys Code Type:  '+!Error_state.sys_code_type,'',c], /error
		MESSAGE, /RESET
		return, {error:1}
	endif

	name = dat_name
	shmmap, name, template=template
	name = long_name
	shmmap, name, /long, dimension=n_long
	name = byte_name
	shmmap, name, /byte, dimension=n_byte
	name = float_name
	shmmap, name, /float, dimension=n_float

skip:
	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'shared_memory_struct',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return, {error:1}
	    endif
	endif

	pdat = ptr_new( shmvar(dat_name), /no_copy)
	plong = ptr_new( shmvar(long_name), /no_copy)
	pbyte = ptr_new( shmvar(byte_name), /no_copy)
	pfloat = ptr_new( shmvar(float_name), /no_copy)
	error = 0

done:
	pshrmem = ptr_new({ppar:ppar, pdat:pdat, plong:plong, pbyte:pbyte, pfloat:pfloat, error:error})
	return, pshrmem

bad:
	warning,'shared_memory_struct','bad parameters.'
	error = 1
	ppar = 0L
	pdat = 0L
	plong = 0L
	pbyte = 0L
	goto, done
end
