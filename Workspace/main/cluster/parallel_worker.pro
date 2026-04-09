; Worker parallel processing routines.
;
; These call low-level routines for cluster processing or multi-core
; support on the worker node side in each cluster node/core.
;
; The client routines, called from the GUI code on the master node
; are found elsewhere in "parallel_client.pro".

pro worker_init, n, p, log=log

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_worker_1, c_worker_olun, c_worker_psh
common c_worker_2, c_worker_log
if n_elements(log) eq 0 then log=0

	c_worker_olun = n							; debug output logical unit
	c_worker_log = log							; enable loggin
	c_worker_psh = p							; pointer to shared memory struct
	return
end

;----------------------------------------------------------------------------------

function worker_progress, f

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_worker_1, c_worker_olun, c_worker_psh

;return,1

	psh = c_worker_psh							; pointer to shared memory struct
	pf = (*psh).pfloat[0]						; progress fraction return
	ppar = (*psh).ppar

	(*pf)[0] = f
	
	if (*ppar)[3] eq 1 then return,0
	return, 1
end

;----------------------------------------------------------------------------------

pro worker_debug, s

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_worker_1, c_worker_olun, c_worker_psh
common c_worker_2, c_worker_log

	if c_worker_log then printf, c_worker_olun, s
	return
end

;----------------------------------------------------------------------------------

pro worker_error, s

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_worker_1, c_worker_olun, c_worker_psh
common c_worker_2, c_worker_log

	if c_worker_log then printf, c_worker_olun, s
	return
end

;----------------------------------------------------------------------------------

pro parallel_worker
end
