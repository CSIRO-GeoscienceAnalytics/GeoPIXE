;
;	Cluster Client
;	
;	Launches jobs across cluster to perform some batch processing (e.g. da_evt)
;	Monitors progress using a progress bar, and collects results after all are done.
;	
;	Is a blocking Widget program.
;	
;		cluster_client, $
;				args=args, $
;				n_files = n_files, $
;				group = group, $
;				presult = presult, $
;				error = error
;
;	args		batch command with arguments to run (string scaler)
;	group		group leader for window
;	presult		pointer to final assembled resulting data to return
;	/test		test mode, without cluster calls
	
pro cluster_client_event, event

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
			warning,'cluster_client_event',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			goto, finish
		endif
	endif
	
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	
	case tag_names( event,/structure) of
		'WIDGET_KILL_REQUEST': goto, kill
			
		'WIDGET_TIMER': begin
			if (*pstate).test then begin
				(*pstate).temp = (*pstate).temp + 10.		; test
				prg = (*pstate).temp						; test
			endif else begin
				prg = parallel_Progress()
			endelse
			
			if prg < 0.0 then begin
				clstat = -1
				goto, done
			endif
			
			if (*pstate).test then begin
				clstat = 0									; test
				if prg ge 100. then clstat=1				; test
			endif else begin
			    clstat = parallel_Monitor( pending=pending)
			endelse
			if clstat ne 0 then begin
				goto, done
			endif
		    
			cluster_client_update, event.top, prg/100., pending=pending
		
			widget_control, event.top, timer=(*pstate).delay
			end
			
		else: begin	
			uname = widget_info(event.id, /uname)
			case uname of
				'cancel': begin
					goto, kill
					end
				'skip': begin
					if (*pstate).test then begin
						clstat = 0	
					endif else begin
					    clstat = parallel_Monitor()
					endelse
					goto, done
					end
				else:
			endcase
			end
	endcase

finish:
	return

done:
	if clstat lt 0 then begin
		warning,'cluster_client','Cluster job failed.'
		goto, kill
	endif
	
	if (*pstate).test then begin
;		results = ['C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440-test.dai.0','C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440-test.dai.1','C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440-test.dai.2']
;		results = ['C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440i.spec.0','C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440i.spec.1','C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440i.spec.2']
		results = ['C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440-test-q2.spec.0','C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440-test-q2.spec.1','C:\software\Data\CSIRO\Maia-96\Mar-2009\Analysis\440\440-test-q2.spec.2']
		status = 1										; test
	endif else begin
		status = parallel_Results(results)
	endelse
	if status eq 0 then begin
		warning,'cluster_client','Failed to fetch cluster job results!'
		goto, kill
	endif

	cluster_client_update, event.top, 1.0

;	Results is an array containing the result strings from each slave.
;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).

	*(*pstate).presult = results
	*(*pstate).perror = 0	
	
kill:
	widget_control, (*pstate).title, set_value='Cleanup ...'
	if (*pstate).test eq 0 then begin
		parallel_Release
		parallel_cancel
	endif
	print,'Kill cluster_client ...'
	widget_control, event.top, /destroy
	return
end

;------------------------------------------------------------------------------------------

pro cluster_client_update, tlb, v, pending=pending

	COMPILE_OPT STRICTARR
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
			warning,'cluster_client_update',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			cancel = 1
			goto, finish
		endif
	endif
	
	cancel = 0
	if n_elements(tlb) lt 1 then return
	if n_elements(v) lt 1 then return
	if n_elements(pending) lt 1 then pending=1
	if widget_info( tlb,/valid_id) eq 0 then return

	child = widget_info( tlb, /child)
	widget_control, child, get_uvalue=pstate

	(*pstate).fraction = v < 1.
	
	message = 'Processing ('+(*pstate).subtitle+') ['+str_tidy(pending)+' pending] ...'
	if (*pstate).fraction gt 0.99 then message = 'Gather results ['+str_tidy(pending)+' pending] ...'
	widget_control, (*pstate).title, set_value=message
	
	dt = float( systime(/seconds) - (*pstate).t0)
	fullt = dt / ((*pstate).fraction > 0.01)
	remain = fullt - dt
	widget_control, (*pstate).percent, set_value=string(100.0*(*pstate).fraction, format='(F5.0,"%")')
	widget_control, (*pstate).time, set_value=string(remain, format='(F7.0)')

	cluster_client_draw, pstate

finish:
	return
end

;------------------------------------------------------------------------------------------

pro cluster_client_draw, pstate

	COMPILE_OPT STRICTARR
	
	wset, (*pstate).wid
	right = (fix((*pstate).fraction * (*pstate).width) + 1) < ((*pstate).width-1)
	
	polyfill, [0,right,right,0,0],[0,0,(*pstate).height-1,(*pstate).height-1,0], /device, color=spec_colour('green')
	end

;------------------------------------------------------------------------------------------

pro OnRealize_cluster_client, wWidget

	COMPILE_OPT STRICTARR
	
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	widget_control, wWidget, get_value=wid
	(*pstate).wid = wid
	(*pstate).draw = wWidget
end

;------------------------------------------------------------------------------------------

pro cluster_client, group_leader=group, args=args, presult=presult, title=title, $
						test=test, prefs=prefs, n_files=n_files, subtitle=subtitle, error=error

;	if 'n_files' not supplied it will spawn max processes, some of which will return 'null'
;	as a result. These will be skipped in the stripe merge.

	COMPILE_OPT STRICTARR
	cluster_running = 0
	tlb = 0L
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
			warning,'cluster_client',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			if cluster_running eq 0 then begin
				if widget_info( tlb, /valid) then widget_control, tlb, /destroy
				if test eq 0 then begin
					parallel_release
					parallel_cancel
				endif
			endif
			return
		endif
	endif
	
	error=1
	presult = ptr_new(/allocate_heap)
	if n_elements(group) lt 1 then group=0L
	if n_elements(args) lt 1 then begin
		warning,'cluster_client','No "args" supplied, return.'
		return
	endif
	if n_elements(title) lt 1 then title = 'GeoPIXE Cluster Execution'
	if n_elements(subtitle) lt 1 then subtitle = ''
	
	if n_elements( prefs) eq 0 then begin
	  	g = geopixe_defaults( error=err, source='cluster_client')
	  	prefs = g.cluster
	endif
	parallel_client										; compile these ...
	
	; Change default test value to '0' for real cluster ...
	
	if n_elements(test) lt 1 then test=0				; test=1 for testing without Cluster code
	
	if test eq 0 then begin
		nodes = prefs.nodes
		if n_elements(n_files) gt 0 then begin
			nodes = (nodes < n_files) > 1
			print,'cluster_client: process n_files = ',n_files
		endif
		print,'cluster_client: Spawn processes for nodes = ',nodes
		parallel_Config, type=prefs.type, nodes=nodes, error=error
		if error then begin
				warning,'cluster_client','"parallel_config" failed, return.'
				return
		endif
		parallel_Init, /progress
	endif
	
	width = 480
	device, get_screen_size=screen
	xoff = screen[0]/2 - width/2
	yoff = screen[1]/2 - 100
	
	case !version.os_family of
		'MacOS': begin
			retain = 2
			end
		'unix': begin
			retain = 2
			end
		else: begin
			retain = 1
	 		end
	endcase
	
	tlb = widget_base( /column, title=title[0], /TLB_KILL_REQUEST_EVENTS, $
						group_leader=group, uname='cluster_client_TLB', $
						/base_align_center, xoffset=xoff, yoffset=yoff, floating=(widget_info(group,/valid_id) eq 1))
	tbase = widget_base( tlb, /row, /base_align_center)
	
	title = widget_label( tbase, value='Configure cluster ...', /align_left, xsize=width-32)
	percent = widget_label( tbase, value='   0.% ', xsize=30)
	
	width2 = width-5
	height = 25
	draw = widget_draw( tlb, xsize=width2, ysize=height, retain=retain, notify_realize='OnRealize_cluster_client')
	
	base2 = widget_base( tlb, /row, /base_align_center, xpad=0, ypad=0, space=2, /align_right)
	
	lab = widget_label( base2, value='Remaining (sec):')
	time = widget_label( base2, value='0',xsize=0.14*width)
	
	bbase = widget_base( tlb, /row, space=30, /align_center)
	cancel = widget_button( bbase, value='Skip Remaining', uname='skip')
	skip = widget_button( bbase, value='Cancel', uname='cancel')
	
	perror = ptr_new(1)
	
	state = {	draw:		draw, $				; draw widget ID
				title:		title, $			; title label ID
				cancel:		cancel, $			; cancel button ID
				time:		time, $				; bytes label ID
				percent: 	percent, $			; percent label ID
				prefs:		prefs, $			; cluster preferences
	
				wid:		0, $				; draw window id
				width:		width2, $			; draw width
				height:		height, $			; draw height
				fraction:	0.0, $				; fraction done
				delay:		1.0, $				; update delay
				subtitle:	subtitle, $			; sub-title text
				presult:	presult, $			; returned data pointer
				temp:		0.0, $				; temporary for testing
				test:		test, $				; flags test mode
				perror:		perror, $			; error flag
				t0:			systime(/seconds) $	; starting time (seconds)
			}
	
	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	
	widget_control, tlb, /realize, timer=1.0
	
	if test eq 0 then begin
		if parallel_Start(args) eq 0 then begin
			warning,'cluster_client',['Cluster_Start failed','GeoPIXE command:',strmid(args,0,200)+' ...']
			widget_control, tlb, /destroy
			if test eq 0 then begin
				parallel_release
				parallel_cancel
			endif
			return
		endif
	endif
	
;	if test then begin
;		xmanager, 'cluster_client', tlb, /no_block		; immediate return to enable debug
;	endif else begin
		xmanager, 'cluster_client', tlb					; manage cluster progress bar with blocking
;	endelse
	cluster_running = 1
	error = *perror
	return
end


