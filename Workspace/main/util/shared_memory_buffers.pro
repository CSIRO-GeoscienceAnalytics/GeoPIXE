function shared_memory_buffers, prefix=prefixi, error=error, output=olun, $
			n_buffers=n_buffers, buffer_size=buffer_size, floatd=floatd, byted=byted, $
			n_long=n_long, n_float=n_float, n_byte=n_byte, destroy=destroy, init=init

; Open shared memory buffers for Blog Client data
;
; If 'buffer_size' is present, then it sets up the ppar data,
; else it returns the n_buffers and buffer_size found in the ppar shared memory.
;
; If 'buffer_size' is used, also supply 'n_buffers' for # of buffers of this size,
; this is first call to create or attach to shared memory,
; else 'n_buffers' returns the # buffers for an existing shared memory block.
;
;	buffer_size	length of buffer, vector to specify length of each dimension
;	prefix		text prefix for shared memory variable/array names (maximum of 12 characters)
;	/floatd		for float shared memory 'pdat' (default long)
;	/byted		for byte data
;
;	/init		initialize ppar flags
;	
;	If need to change size (or data type) of buffers, uncomment the line
;	'shared_memory_unmap' in the blog client programs, run them, stop using stop ppar,
;	then exit all processes that use the memory. It should then delete. Else reboot.
;
; Returns a pointer to a struct containing:
;	loop			int to be used as a loop counter
;	n_buffers		# buffers
;	buffer_size		size of buffers (up to 3 dimensions)
;	ppar			pointer to 16 long parameters for shared memory control
;	pdat			pointers (n_buffer) to data of type (typ), buffer_size long
;	pfill			pointers (n_buffer) to flag showing # entries in pdat
;	pvalid			pointers (n_buffer) to flag pdat is available for read
;	plong			pointers (n_buffer) to general purpose Long array (16 or n_long)
;	pfloat			pointers (n_buffer) to general purpose Float array (16 or n_float)
;	pbyte			pointers (n_buffer) to general purpose byte array (256 or n_byte)
;	error			flags an error (0 = OK)
;
;	ppar array:	0	n_buffers
;				1	free
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if n_elements(prefixi) lt 1 then prefixi='blog_client_'
if n_elements(buffer_size) ge 1 then new=1 else new=0
if new and (n_elements(n_buffers) lt 1) then goto, bad
if n_elements(floatd) lt 1 then floatd=0
if n_elements(byted) lt 1 then byted=0
if n_elements(n_float) lt 1 then n_float=16
if n_elements(n_long) lt 1 then n_long=16
if n_elements(n_byte) lt 1 then n_byte=256
if n_elements(destroy) lt 1 then destroy=0
if n_elements(init) lt 1 then init=0
if n_elements(olun) lt 1 then olun=0

	prefix = prefixi[0]
	gprint,level=2, output=olun,'shared_memory_buffers: map buffers for prefix = "'+prefix+'" (#chars='+strtrim(string(n_elements(byte(prefix))),2)+') ...'
	n_pars = 16
	par_name = prefix + 'pars'

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		if !Error_state.name eq 'IDL_M_SHM_REDEFSEG' then begin
			gprint,level=2, output=olun,'shared_memory_buffers: "shmmap" IDL_M_SHM_REDEFSEG error on "'+par_name+'" - assume it is already established.'
			goto, skip1
		endif
		
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'shared_memory_buffers',['IDL run-time error caught.', '', $
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
	;  incoming (from blog_client):              kill running
	;                   0           1    2    3    4    5    6    7    8    9   10   11   12   13-15
	;		                      free						free free free free free free free dimensions

	if new or init then begin
		gprint,level=1, output=olun, '	set-up new shared memory? Test ...'
		n = n_elements(buffer_size) > 1
		bufsiz = (n ge 3) ? buffer_size[0:2] : [buffer_size,lonarr(3-n)]

		init_par = init
		if (*ppar)[13] ne buffer_size[0] then init_par=1		; test if already set-up
		if (*ppar)[0] ne n_buffers then init_par=1
		
		if init_par then begin
			gprint,level=2, output=olun,'shared_memory_buffers: Memory not set-up, so clear/set ppar ...'
			(*ppar)[0:15] = [n_buffers,     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  bufsiz ]
		endif
	endif else begin
		gprint,level=1, output=olun, '	Look for existing shared memory ...'
		n_buffers = (*ppar)[0]
		buffer_size = (*ppar)[13]
		if (*ppar)[14] ne 0 then buffer_size = [buffer_size, (*ppar)[14]]
		if (*ppar)[15] ne 0 then buffer_size = [buffer_size, (*ppar)[15]]
	endelse
	gprint,level=2, output=olun,n_buffers, ' buffers, each = ',buffer_size

	if n_buffers eq 0 then begin
		gprint,level=2, output=olun,'Zero n_buffers. Ignore DAT.'
		error = 1
		pdat = 0
		plong = 0
		pfloat = 0
		pbyte = 0
		pfill = 0
		pvalid = 0
		goto, done
	endif

	dat_name = prefix + 'dat_'+ str_tidy(indgen(n_buffers))
	long_name = prefix + 'long_'+ str_tidy(indgen(n_buffers))
	float_name = prefix + 'float_'+ str_tidy(indgen(n_buffers))
	byte_name = prefix + 'byte_'+ str_tidy(indgen(n_buffers))
	fill_name = prefix + 'fill_'+ str_tidy(indgen(n_buffers))
	valid_name = prefix + 'valid_'+ str_tidy(indgen(n_buffers))

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		if !Error_state.name eq 'IDL_M_SHM_REDEFSEG' then begin
			gprint,level=2, output=olun,'shared_memory_buffers: "shmmap" IDL_M_SHM_REDEFSEG error on "'+name+'" - assume it is already established.'
			goto, skip
		endif
		
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'shared_memory_buffers',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			'Message:  '+!Error_state.msg, 'Code:  '+string(!Error_state.code), 'Sys Msg:  '+!Error_state.sys_msg, $
			'Sys Code:  '+strjoin(string(!Error_state.sys_code),','), 'Sys Code Type:  '+!Error_state.sys_code_type,'',c], /error
		MESSAGE, /RESET
		return, {error:1}
	endif

;	Set these up in reverse order in case n_buffers has been extended. This will
;	establish the new maps and then skip when it encounters an existing one.

	for i=n_buffers-1,0,-1 do begin
		name = dat_name[i]
		if floatd then begin
			shmmap, name, /float, dimension=buffer_size
		endif else if byted then begin
			shmmap, name, /byte, dimension=buffer_size
		endif else begin
			shmmap, name, /long, dimension=buffer_size
		endelse
		name = long_name[i]
		shmmap, name, /long, dimension=n_long
		name = float_name[i]
		shmmap, name, /float, dimension=n_float
		name = byte_name[i]
		shmmap, name, /byte, dimension=n_byte
		name = fill_name[i]
		shmmap, name, /long, dimension=1
		name = valid_name[i]
		shmmap, name, /long, dimension=1
	endfor

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
	       warning,'shared_memory_buffers',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return, {error:1}
	    endif
	endif

	first = 1
	for i=0L,n_buffers-1 do begin
		if first then begin
			pdat = ptr_new( shmvar(dat_name[i]), /no_copy)
			plong = ptr_new( shmvar(long_name[i]), /no_copy)
			pfloat = ptr_new( shmvar(float_name[i]), /no_copy)
			pbyte = ptr_new( shmvar(byte_name[i]), /no_copy)
			pfill = ptr_new( shmvar(fill_name[i]), /no_copy)
			pvalid = ptr_new( shmvar(valid_name[i]), /no_copy)
			first = 0
		endif else begin
			pdat = [pdat, ptr_new( shmvar(dat_name[i]), /no_copy)]
			plong = [plong, ptr_new( shmvar(long_name[i]), /no_copy)]
			pfloat = [pfloat, ptr_new( shmvar(float_name[i]), /no_copy)]
			pbyte = [pbyte, ptr_new( shmvar(byte_name[i]), /no_copy)]
			pfill = [pfill, ptr_new( shmvar(fill_name[i]), /no_copy)]
			pvalid = [pvalid, ptr_new( shmvar(valid_name[i]), /no_copy)]
		endelse
	endfor
	error = 0

done:
	pshrmem = ptr_new({loop:0, n_buffers:n_buffers, buffer_size:buffer_size, $
					ppar:ppar, pdat:pdat, pfill:pfill, pvalid:pvalid, $
					plong:plong, pfloat:pfloat, pbyte:pbyte, error:error})
	return, pshrmem

bad:
	warning,'shared_memory_buffers','bad parameters.'
	error = 1
	pdat = 0L
	pfill = 0L
	plong = 0L
	pfloat = 0L
	pbyte = 0L
	pvalid = 0L
	goto, done
end
