;
;	Put up a progress bar.
;	Respond to notify events, and display progress through a file.
;
;	Pass struct of form:
;
;		{ unit:1, value:lon64arr(), current:0L, size:0L, file:string }
;
;	where	unit	I/O unit number, or zero if no unit (multifile mode).
;			value	array of parameters to display.
;			current	current position, if unit=0
;			size	maximum position, if unit=0
;			file	filename in multifile mode (unit=0)
;
;	Use:
;		progress, tlb=prog_tlb, title='', pars=['par1','par2','par3']	; any number of pars
;
;		where:
;			'par1', etc. are titles of parameters to show
;			'title' is a title for the window.
;
; normal file pointer mode:
;		Uses file pointer to show progress against total number of bytes:
;
;		progress, /update, prog_tlb, {unit:n, value:lon64arr() }
;
; multifile mode:
;		Uses the file number input and total number of files to show progress
;
;		progress, /update, prog_tlb, {unit:0, value:lon64arr(), file:'', current:0L, size:0L}
;
; change running title:
;		progress, /running, prog_tlb, title=''
;		
; finish:
;		progress, /complete, prog_tlb, 'title message'
;
;		progress, /ending, prog_tlb
;
pro progress_event, event

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
		warning,'Progress_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case tag_names( event,/structure) of
	'WIDGET_KILL_REQUEST': begin
		print,'Kill progress ...'
		goto, kill
		end
	'WIDGET_TIMER': begin
		if lenchr((*pstate).program) gt 0 then begin
			program = (*pstate).program

			call_procedure, program, progress_tlb=event.top

;			cancel_notify, event.top
			widget_control, event.top, /destroy
			return
		endif
		end
	else:
endcase

;	The events below usually do not get received. It was added chasing the file_requester use of progress.
;	In nornal operation, the "progress, /update" call will use 'widget_event' directly to get cancel button presses.

uname = widget_info( event.id, /uname)
case uname of
	'cancel': begin
		print, 'cancel ...'
		if ptr_valid((*pstate).pcancel) then *(*pstate).pcancel = 1
		widget_control, (*pstate).title, set_value='Operation Cancelled'
		wait, 0.8
		end
	'skip': begin
		print, 'skip ...'
		if ptr_valid((*pstate).pcancel) then *(*pstate).pcancel = 1
		widget_control, (*pstate).title, set_value='Skip remaining'
		wait, 0.8
		end
	else: begin
		print, 'progress (unknown) ...'
		help, event
		end
endcase

finish:
	return

kill:
	if ptr_valid((*pstate).pcancel) then *(*pstate).pcancel = 1
	widget_control, event.top, /destroy
	return
end

;------------------------------------------------------------------------------------------

pro progress_end, tlb

if n_elements(tlb) lt 1 then return
if widget_info( tlb,/valid_id) eq 0 then return

	child = widget_info( tlb, /child)
	widget_control, child, get_uvalue=pstate

	widget_control, (*pstate).title, set_value='Operation Complete'
;	wait, 0.3

;	print,'Kill progress ...'
	widget_control, tlb, /destroy
;	heap_gc, /verbose
return
end

;------------------------------------------------------------------------------------------

pro progress_complete, tlb, title

if n_elements(tlb) lt 1 then return
if n_elements(title) lt 1 then title='Cleanup ...'
if widget_info( tlb,/valid_id) eq 0 then return

	child = widget_info( tlb, /child)
	widget_control, child, get_uvalue=pstate

	widget_control, (*pstate).title, set_value=title[0]
	widget_control, (*pstate).bytes, set_value=string((*pstate).size)
	widget_control, (*pstate).total, set_value=string((*pstate).size)
	widget_control, (*pstate).percent, set_value=' 100% '
	widget_control, (*pstate).time, set_value=' 0.0 '

	(*pstate).fraction = 1.0
	progress_draw, pstate
return
end

;------------------------------------------------------------------------------------------

pro progress_running, tlb, title

if n_elements(tlb) lt 1 then return
if n_elements(title) lt 1 then title='Continue processing ...'
if widget_info( tlb,/valid_id) eq 0 then return

	child = widget_info( tlb, /child)
	widget_control, child, get_uvalue=pstate

	widget_control, (*pstate).title, set_value=title[0]
return
end

;------------------------------------------------------------------------------------------

pro progress_update, tlb, v, cancel=cancel, skip=skip

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
		warning,'Progress_update',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		cancel = 1
		goto, finish
	endif
endif

cancel = 0
skip = 0
if n_elements(tlb) lt 1 then return
if n_elements(v) lt 1 then return
if widget_info( tlb,/valid_id) eq 0 then return

	child = widget_info( tlb, /child)
	widget_control, child, get_uvalue=pstate
	message = ''

;	Do not use /save_hourglass. This upsets the pointer return.
;	The problem only appears with the RT /hourglass.

	cancel_event = widget_event( (*pstate).cancel, /nowait)

	if tag_names( cancel_event, /structure_name) eq 'WIDGET_BUTTON' then begin
		print,'progress: cancel from widget_event'
		message = 'Operation Cancelled'
		cancel = 1
	endif

	skip_event = widget_event( (*pstate).skip, /nowait)
	if tag_names( skip_event, /structure_name) eq 'WIDGET_BUTTON' then begin
		print,'progress: skip from widget_event'
		message = 'Skip Rest of File'
		skip = 1
	endif

	if cancel or skip then begin
		widget_control, (*pstate).title, set_value=message
		wait, 0.5
		return
	endif

	nv = tag_present('value',v) ? n_elements(v.value) : 0
	n = min( [nv, (*pstate).n] )
	if v.unit ge 1 then begin
		a = fstat( v.unit)

		if a.open eq 0 then begin
			widget_control, (*pstate).title, set_value='File Closed'
			widget_control, (*pstate).bytes, set_value=' '
			widget_control, (*pstate).total, set_value=' '
			current = (*pstate).size
		endif else begin
			file = a.name
			nf = strlen(file)
			if nf gt 60 then file = strmid( file, nf-60,60)
			widget_control, (*pstate).title, set_value=file
			widget_control, (*pstate).bytes, set_value=' '+strtrim(string(a.cur_ptr),2)+' '
			widget_control, (*pstate).total, set_value=' '+strtrim(string(a.size),2)+' '
			(*pstate).size = a.size
			current = a.cur_ptr
		endelse
	endif else begin
		(*pstate).size = 10LL
		current = 0LL
		file = 'Processing ...'
		vnames = tag_names(v)
		q = where(vnames eq 'SIZE')
		if q[0] ne -1 then (*pstate).size = v.size
		q = where(vnames eq 'CURRENT')
		if q[0] ne -1 then current = v.current
		q = where(vnames eq 'FILE')
		if q[0] ne -1 then file = v.file

		nf = strlen(file)
		if nf gt 60 then file = strmid( file, nf-60,60)
		widget_control, (*pstate).title, set_value=file
		widget_control, (*pstate).bytes, set_value=' '+strtrim(string(current),2)+' '
		widget_control, (*pstate).total, set_value=' '+strtrim(string((*pstate).size),2)+' '
	endelse
	dt = float( systime(/seconds) - (*pstate).t0)
	(*pstate).fraction = float(current) / float((*pstate).size)
	fullt = dt / (*pstate).fraction
	remain = fullt - dt
	widget_control, (*pstate).percent, set_value=' '+string(100.0*(*pstate).fraction, format='(F6.1,"% ")')+' '
	widget_control, (*pstate).time, set_value=' '+string(remain, format='(F7.0)')+' '

	progress_draw, pstate

	if n gt 0 then begin
		for i=0L,n-1 do begin
			widget_control, (*pstate).par[i], set_value=' '+strtrim(string(v.value[i]),2)+' '
		endfor
	endif

finish:
	return
end

;------------------------------------------------------------------------------------------

pro progress_draw, pstate

wset, (*pstate).wid
right = (fix((*pstate).fraction * (*pstate).width) + 1) < ((*pstate).width-1)

polyfill, [0,right,right,0,0],[0,0,(*pstate).height-1,(*pstate).height-1,0], /device, color=spec_colour('green')
end

;------------------------------------------------------------------------------------------

pro OnRealize_Progress, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid
(*pstate).draw = wWidget
end

;------------------------------------------------------------------------------------------

pro progress, tlbi, vi, group_leader=group, title=stitle, pars=pars, $
		program=program, top=top, tlb=tlb, _extra=extra, pcancel=pcancel,  $
		running=running, ending=ending, update=update, complete=complete, cancel=cancel, skip=skip

;	Put up a progress bar.
;	Respond to notify events, and display progress through a file.
;
; /update		update contents of progress bar
;	tlb			progress TLB
;	v			values struct to update
;	cancel		returns 'cancel=1' if bar cancelled
;	skip		returns 'skip=1' if 'skip remaining'
;
; /running		change the running title
; 	title		new running title
; 	
; /complete		complete operations, bar --> 100%
;	title		display changed title
;
; /ending		finished, close progress bar and exit
;
; else			set-up Progress bar
;	group		window parent
;	title		window title
;	top			where to place top of window, else centre
;	pars		counter pars array of titles
;	program		optional: program name to run, then exit

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
		warning,'Progress',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		tlb = 0L
		return
	endif
endif

if n_elements(update) lt 1 then update=0
if n_elements(complete) lt 1 then complete=0
if n_elements(ending) lt 1 then ending=0
if n_elements(running) lt 1 then running=0

if update then begin
	progress_update, tlbi, vi, cancel=cancel, skip=skip
	return
endif

if complete then begin
	progress_complete, tlbi, title
	return
endif

if ending then begin
	progress_end, tlbi
	return
endif

if running then begin
	progress_running, tlbi, stitle
	return
endif

;..................................................................................

if n_elements(group) lt 1 then group=0L
if n_elements(stitle) lt 1 then stitle='Sort progress'
if n_elements(program) lt 1 then program=''
if n_elements(top) lt 1 then top=0
n = n_elements(pars)
device, get_screen_size=screen

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
		retain = 2
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		width = 600
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
		retain = 2
;@2		widget_control, default_font='6x12'				; set font for all windows
		width = 600
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
		retain = 1
;		widget_control, default_font='Arial*14'			; set font for all windows
		width = 480
 		end
  endcase
  xoff = screen[0]/2 - width/2
  yoff = top ? 0 : (screen[1]/2 - 100)


tlb = widget_base( /column, title=stitle[0], /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='progress_TLB', $
					/base_align_center, xoffset=xoff, yoffset=yoff)
tbase = widget_base( tlb, /row, /base_align_center, space=5,xpad=1,ypad=1)

title = widget_label( tbase, value='Start Processing ...', /align_left, xsize=width-80)
percent = widget_label( tbase, value='   0.% ', xsize=60)

width2 = width-5
height = 25
draw = widget_draw( tlb, xsize=width2, ysize=height, retain=retain, notify_realize='OnRealize_Progress')

base2 = widget_base( tlb, /row, /base_align_center, xpad=0, ypad=0, space=2)

lbytes = widget_label( base2, value='Current:')
bytes = widget_label( base2, value='0', scr_xsize=0.2*width)
gap = widget_label( base2, value='   ')
ltotal = widget_label( base2, value='Total:')
total = widget_label( base2, value='0', scr_xsize=0.2*width)
gap = widget_label( base2, value='   ')
ltime = widget_label( base2, value='Remaining (sec):')
time = widget_label( base2, value='0', scr_xsize=0.14*width)

par = 0L
if n gt 0 then begin
	base3 = widget_base( tlb, column=3, /frame, /base_align_right, xpad=2, ypad=2, space=5)

	par = lonarr(n)
	for i=0L,n-1 do begin
		base = widget_base( base3, /row, /base_align_right, xpad=0, ypad=0, space=0)
		lab = widget_label( base, scr_xsize=0.3*(width/3)-5, value=pars[i]+':')
		par[i] = widget_label( base, scr_xsize=0.7*(width/3)-6, value='0000000000',/align_right)
	endfor
endif

bbase = widget_base( tlb, /row, space=10, /align_center)
skip = widget_button( bbase, value='Skip Remaining', uname='skip')
cancel = widget_button( bbase, value='Cancel', uname='cancel')

state = {	draw:	draw, $				; draw widget ID
			title:	title, $			; title label ID
			cancel:	cancel, $			; cancel button ID
			skip:	skip, $				; skip button ID
			par:	par, $				; parameter label IDs
			bytes:	bytes, $			; bytes label ID
			total:	total, $			; bytes label ID
			time:	time, $				; bytes label ID
			percent: percent, $			; percent label ID
			wid:	0, $				; draw window id
			width:	width2, $			; draw width
			height:	height, $			; draw height
			n:		n, $				; number of pars
			fraction:	0.0, $			; fraction done
			size:		0LL, $			; file size
			pcancel: (ptr_valid(pcancel) ? pcancel : 0L), $
			program:	program, $		; program to run, then exit
			t0:	systime(/seconds) $		; starting time (seconds)
		}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)

if lenchr(program) gt 0 then begin
	widget_control, tlb, /realize, timer=0.1
	xmanager, 'progress', tlb, /no_block
	return
endif

widget_control, tlb, /realize
xmanager, 'progress', tlb, /no_block
return
end


