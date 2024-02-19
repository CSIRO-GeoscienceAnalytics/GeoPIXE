function allocate_sems, n_buffers, prefix=prefix, error=error, $
						init=init

; Create semaphores for n_buffers shared memory buffers
; /init	for first process that sets up the semaphores

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if n_elements(prefix) lt 1 then prefix='blog_client_'
if n_elements(n_buffers) lt 1 then goto, bad
if n_elements(init) lt 1 then init=0

	sem_name = prefix + 'sem_'+ str_tidy(indgen(n_buffers))
	for i=0L,n_buffers-1 do begin
		if not sem_create(sem_name[i]) then goto, bad_sem
		sem_release, sem_name[i]
	endfor
	if init then begin
		for i=0L,n_buffers-1 do begin
			lock = sem_lock(sem_name[i])
			if lock eq 0 then goto, bad_sem2
			sem_release, sem_name[i]
		endfor
	endif

	error = 0
	print,'allocate_sems: Allocated SEMs for '+prefix
	return, sem_name

bad:
	warning,'allocate_sems','error creating semaphore'
	return,''
bad_sem:
	warning,'allocate_sems','error creating semaphore'
	error = 2
	return,sem_name
bad_sem2:
	warning,'allocate_sems',['Initial semaphore locked out.', $
				'All SEMs now flagged for deletion.','Now shutdown all processes.']
	error = 3
	return,sem_name
end
