pro shared_memory_unmap, prefix=prefix, n_buffers=n_buffers

COMPILE_OPT STRICTARR
if prefix eq '' then return
if n_buffers eq 0 then return

par_name = prefix + 'pars'
dat_name = prefix + 'dat_'+ str_tidy(indgen(n_buffers))
long_name = prefix + 'long_'+ str_tidy(indgen(n_buffers))
float_name = prefix + 'float_'+ str_tidy(indgen(n_buffers))
fill_name = prefix + 'fill_'+ str_tidy(indgen(n_buffers))
valid_name = prefix + 'valid_'+ str_tidy(indgen(n_buffers))

for i=0L,n_buffers-1 do begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		print,'shared_memory_buffers: "shmunmap" error on "'+name
		goto, skip1
	endif
	name = dat_name[i]
	shmunmap, name				; use these to remove mapping
skip1:	
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		print,'shared_memory_buffers: "shmunmap" error on "'+name
		goto, skip2
	endif
	name = fill_name[i]
	shmunmap, name				; must stop all processes
skip2:	
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		print,'shared_memory_buffers: "shmunmap" error on "'+name
		goto, skip3
	endif
	name = valid_name[i]
	shmunmap, name				; use only to redefine memory sizes
skip3:	
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		print,'shared_memory_buffers: "shmunmap" error on "'+name
		goto, skip4
	endif
	name = long_name[i]
	shmunmap, name				; 
skip4:	
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		print,'shared_memory_buffers: "shmunmap" error on "'+name
		goto, skip5
	endif
	name = float_name[i]
	shmunmap, name
skip5:				; 
endfor

Catch, ErrorNo2
if (ErrorNo2 ne 0) then begin
	Catch, /cancel
	print,'shared_memory_buffers: "shmunmap" error on "'+name
	return
endif
name = par_name
shmunmap, name

return
end