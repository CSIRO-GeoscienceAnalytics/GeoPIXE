;
; Client user-interface parallel processing interface routines.
; 
; These call low-level routines for cluster processing or multi-core
; support on the client side.
;
; The worker routines, called from the excuting code in each node/core
; are found elsewhere in "parallel_worker.pro".
; 
; See notes in Change Log 8 Apr, 2016.
;
; Config:
;	Tests pointers to shared memory in common. If OK, just use them.
;	Loop through all nodes to make sure all are setup.
; Init:
;	Spawn background processes, link to logical units to enable closure.
;	Use /noshell for Linux, in which case command and args must be a string vector.
; Start:
;	NOTE: No modification to any shared memory before here!
;	If we get here, “busy” must not be set, so initialize memory.
;	Clear (*ppar)[2] to indicate wait for command string.
;	Upload command data.
;	Sets (*ppar)[2]=1 to indicate command data is ready.
;	Return good status, even if command string is null.
; Monitor:
;	Checks for (*ppar)[4]=1 (all processes) to indicate all done.
; Results:
;	Reads back all result data.
; Release:
;	Sets (*ppar)[3]=1 kill flags to stop backgrpund process.
; Cancel:
;	Destroy Bridge object connected to backgropund processes to kill them.
;	Frees shared memory using 'unmap'.
;
;	 This is designed to connect to the background process worker: geopixe_parallel
;
; Geopixe_parallel:
;	Sets (*ppar)[5]=1 to indicate up and running.
;	Waits for (*ppar)[2] to  be set to indicate command data loaded.
;	Clears (*ppar)[2] once data has been read.
;	Sets (*ppar)[4]=1 to indicate results have been uploaded.
;	Clears (*pb1)[0] on exit, if null command string.
;	Clears (*ppar)[5] on exit.
;
;	It uses the 'shared_memory_buffers' routine for shared memory:
;	
; Shared_memory_buffers:
;	NOTE: Must not set pars if shared memory already setup; just connect.
;	Use (*ppar)[0] (n_buffers) and (*ppar)[13] (buffer_size[0]) as indicators of setup. 
;	If they’re not OK, then set all *ppar.

pro parallel_config, type=type, nodes=nodes, error=error

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun
common c_parallel_3, c_parallel_n_buff, c_parallel_bufsize
common c_parallel_5, c_parallel_segment

	if n_elements(type) eq 0 then type='cores'
	if n_elements(nodes) eq 0 then nodes=3
	c_parallel_nodes = nodes			
	c_parallel_type = type
	if c_parallel_nodes eq 0 then warning, 'parallel_config','Zero number of nodes.'
	c_parallel_n_buff = 2
	c_parallel_bufsize = 10000000L	
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
			warning,'parallel_config',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
				
	case c_parallel_type of
;		'CWS': begin
;		
;			g = geopixe_defaults()
;			Cluster_Config, type=type, ServiceUrl=g.cws.ServiceUrl, name=g.cws.name, user=g.cws.user, $
;					password=g.cws.password, OSType=g.cws.OSType, Nodestr=g.cws.Nodestr, $
;					NumNodes=nodes, LocalPaths=*g.cws.pLocalPaths, NodePaths=*g.cws.pNodePaths
;			end
			
		'cores': begin
			c_parallel_pshr = ptrarr(c_parallel_nodes)
			c_parallel_segment = strarr(c_parallel_nodes)
			
			for i=0,c_parallel_nodes-1 do begin
				sindex = strcompress(string(i),/remove_all)
				str = strsplit( systime(), ' ', /extract)
				st = replace( ':', '_', strjoin( str[2:3], '_')) + '_'
				user = get_login_info()
				prefix = trim_user( user.user_name) + '_' + sindex + '_' + st
				prefix = strcompress( prefix, /remove_all)

				print,'Process ',i,' Shared memory segment prefix= "'+prefix+'"'
				c_parallel_segment[i] = prefix
				
				n_buffers = c_parallel_n_buff
				buffer_size = c_parallel_bufsize
				pshrmem = shared_memory_buffers( prefix=prefix, n_buffers=n_buffers, $
										buffer_size=buffer_size, /byted, /init, error=error)
				if error then goto, bad_shrmem
				c_parallel_pshr[i] = pshrmem
				
				ppar = (*pshrmem).ppar			; memory for handshaking pars
				(*ppar)[2:5] = 0				; clear args ready flag,  process running flag, etc.
			endfor
			end
			
		else: warning,'parallel_config','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase
	error = 0
	return
	
bad_shrmem:
	warning,'parallel_config','error allocating shared memory for background processes.'
	error = 1
	return
end

;-------------------------------------------------------------------------------

pro parallel_init, progress=do_progress

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun
common c_parallel_5, c_parallel_segment
common c_parallel_6, c_parallel_obj
if n_elements(do_progress) eq 0 then do_progress=0

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
			warning,'parallel_init',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	timeout_processes = 120.

	case c_parallel_type of
;		'CWS': begin
;			Cluster_Init
;			end
			
		'cores': begin
			print,'Spawn background processes ...'
			c_parallel_obj = objarr(c_parallel_nodes)
			if do_progress then begin
				progress, tlb=progress_tlb, title='Spawn background processes'
				progress, /update, progress_tlb, {unit:0, value:0, current:0L, size:c_parallel_nodes}, cancel=cancel
			endif
			
			for i=0,c_parallel_nodes-1 do begin
				print,'     Process ',i,' ...'
				args = {workerIndex:i, totalWorkers:c_parallel_nodes, prefix:c_parallel_segment[i]}

				c_parallel_obj[i] = spawn_bridge_object( geopixe_root, 'geopixe_parallel', args=args)

				if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:i+1, size:c_parallel_nodes}, cancel=cancel
			endfor
			if do_progress then progress, /complete, progress_tlb, 'Launched. Wait for children to run ...'
			
			err = 0
			for i=0,c_parallel_nodes-1 do begin
				print,'Wait for child ',i,' to run ...'
				t = 0.
				pshr = c_parallel_pshr[i]
				ppar = (*pshr).ppar
				while (*ppar)[5] eq 0 do begin
					wait, 0.5
					t = t+0.5
					if t gt timeout_processes then begin
						warning,'parallel_init','Timeout: Child '+str_tidy(i)+' not yet running, abort.'
;						(*pshr).error = 1
						err = 1
						break
					endif
				endwhile
			endfor

			if n_elements(progress_tlb) gt 0 then begin
				if widget_info( progress_tlb, /valid) then progress, /ending, progress_tlb
			endif
			end
			
		else: warning,'parallel_init','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return
end

;-------------------------------------------------------------------------------

function parallel_start, args

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun
common c_parallel_3, c_parallel_n_buff, c_parallel_bufsize
status = 0

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
			warning,'parallel_start',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0
		endif
	endif

	case c_parallel_type of
;		'CWS': begin
;			status = Cluster_Start(args)
;			end
			
		'cores': begin
			for i=0,c_parallel_nodes-1 do begin
				pshr = c_parallel_pshr[i]
				if ptr_good( pshr, /struct) then begin
					print,'Process ',i,' Load args into shared memory and start ...'
					ppar = (*pshr).ppar						; memory for handshaking pars
					pb0 = (*pshr).pdat[0]					; memory for execute command string
					pb1 = (*pshr).pdat[1]					; memory for "results" return string
					pf = (*pshr).pfloat[0]					; memory for floats

					(*pb0)[0:c_parallel_bufsize-1] = 0B		; clear command buffer
					(*pb1)[0:c_parallel_bufsize-1] = 0B		; clear results return
					(*pf)[0] = 0.0							; progress fraction return
					
					b = byte(args)
					nb = n_elements(b)
					if nb ge c_parallel_bufsize then begin
						warning,'parallel_start','"args" array exceeds buffer length.'
					endif
					n = nb < c_parallel_bufsize
					if n eq 0 then begin
						 warning,'parallel_start','Zero length "args" encountered.'
					endif
					(*pb0)[0:n-1] = b[0:n-1]				; load args
					(*ppar)[2] = 1							; start processing
				endif else begin
					 warning,'parallel_start','Bad shared memory pointer for node '+str_tidy(i)
				endelse
			endfor
			status = 1
			end
			
		else: warning,'parallel_start','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return, status
end

;-------------------------------------------------------------------------------

function parallel_progress

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun

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
			warning,'parallel_progress',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, -1.0
		endif
	endif

	case c_parallel_type of
;		'CWS': begin
;			prog = Cluster_Progress()
;			end
			
		'cores': begin
			sum = 0.
			for i=0,c_parallel_nodes-1 do begin
				pshr = c_parallel_pshr[i]
				pfloat = (*pshr).pfloat
				pf = pfloat[0]
				sum = sum +(*pf)[0]
			endfor
			prog = 100. * sum / float(c_parallel_nodes)
			end
			
		else: warning,'parallel_progress','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return, prog
end

;-------------------------------------------------------------------------------

function parallel_monitor, pending=still_underway

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun
common c_parallel_6, c_parallel_obj

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
			warning,'parallel_monitor',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, -1
		endif
	endif
	still_underway = 0

	case c_parallel_type of
;		'CWS': begin
;			done = Cluster_Monitor()
;			end
			
		'cores': begin
			done = 1
			for i=0,c_parallel_nodes-1 do begin
				pshr = c_parallel_pshr[i]
				ppar = (*pshr).ppar

;				This causes some problems. status=3 seems to pop-up occasionally/temporarily at end?
;				Status = 0:idle, 1:executing, 2:completed, 3:Error halted, 4:Aborted
;
				status = (c_parallel_obj[i])->status(error=msg)	; check obj status
				if (status eq 3) or (status eq 4) then begin
					print,'parallel_monitor (error): process=',i,' status=',status,' msg=',msg
					print,'    (Status = 0:idle, 1:executing, 2:completed, 3:Error halted, 4:Aborted)'
;					done = -1									; error halted or aborted
				endif

				if (*ppar)[4] eq 0 then begin
					if (done eq 1) then done = 0				; check client process shared memory flag for finished
					still_underway++
				endif
			endfor
			end
			
		else: warning,'parallel_monitor','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return, done
end

;-------------------------------------------------------------------------------

function parallel_results, results

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun

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
			warning,'parallel_results',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0
		endif
	endif

	case c_parallel_type of
;		'CWS': begin
;			status = Cluster_Results( results)
;			end
			
		'cores': begin
			first = 1
			for i=0,c_parallel_nodes-1 do begin
				pshr = c_parallel_pshr[i]
				ppar = (*pshr).ppar
				pb1 = (*pshr).pdat[1]
				if first then begin
					results = string((*pb1))
					first = 0
				endif else begin
					results = [results, string((*pb1))]
				endelse
			endfor
			status = 1
			end
			
		else: warning,'parallel_results','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return, status
end

;-------------------------------------------------------------------------------

pro parallel_release

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun

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
			warning,'parallel_release',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	case c_parallel_type of
;		'CWS': begin
;			Cluster_Release
;			end
			
		'cores': begin
			for i=0,c_parallel_nodes-1 do begin
				if n_elements(c_parallel_pshr) eq 0 then return
				pshr = c_parallel_pshr[i]
				if ptr_valid(pshr) then begin
					ppar = (*pshr).ppar
					(*ppar)[3] = 1								; kill flag
				endif
			endfor
			end
			
		else: warning,'parallel_release','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return
end

;-------------------------------------------------------------------------------

pro parallel_cancel

COMPILE_OPT STRICTARR
common c_parallel_1, c_parallel_type, c_parallel_nodes
common c_parallel_2, c_parallel_pshr, c_parallel_lun
common c_parallel_3, c_parallel_n_buff, c_parallel_bufsize
common c_parallel_5, c_parallel_segment
common c_parallel_6, c_parallel_obj

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
			warning,'parallel_cancel',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	case c_parallel_type of
;		'CWS': begin
;			Cluster_Cancel
;			end
			
		'cores': begin
			for i=0,c_parallel_nodes-1 do begin

				obj_destroy, c_parallel_obj[i]

;				Warning: This marks the shared memory as pending delete.
;				It cannot be used again, even for a fresh process.
;				So new segment names must be unique next time.

				shared_memory_unmap, prefix=c_parallel_segment[i], n_buffers=c_parallel_n_buff
			endfor
			end
			
		else: warning,'parallel_cancel','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return
end

;-------------------------------------------------------------------------------

pro parallel_reset

;	Called as a last resort to clear busy flags to free up access to shared memory
;	if a 'lock out' situation develops. Link this to a "reset" menu item.

	COMPILE_OPT STRICTARR
	common c_parallel_1, c_parallel_type, c_parallel_nodes
	common c_parallel_2, c_parallel_pshr, c_parallel_lun
	common c_parallel_3, c_parallel_n_buff, c_parallel_bufsize
	;common c_parallel_4, c_parallel_prefix

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
			warning,'parallel_reset',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	case c_parallel_type of
;		'CWS': begin
;			Cluster_Cancel
;			end

		'cores': begin
			for i=0,c_parallel_nodes-1 do begin
				if n_elements(c_parallel_pshr) eq 0 then return
				pshr = c_parallel_pshr[i]
				if ptr_valid(pshr) then begin
					ppar = (*pshr).ppar
					(*ppar)[1:5]  = 0							; clear wait flags, etc.
				endif
			endfor
			end

		else: warning,'parallel_reset','unknown cluster type="'+string(c_parallel_type)+'"'
	endcase

	return
end

;-------------------------------------------------------------------------------

pro parallel_client
end
