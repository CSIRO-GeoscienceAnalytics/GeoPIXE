;
pro select_periodic_event, event

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
		warning,'select_periodic',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr

case tag_names( event,/structure) of
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request select_periodic ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'periodic-table': begin
		if event.alt eq 0 then begin
			if event.select eq 1 then begin
				(*pstate).zon[event.z] = event.shell
				(*pstate).zon_id[event.z] = event.zid
				(*pstate).zalt[event.z] = 0
			endif else begin
				(*pstate).zon[event.z] = 0
				(*pstate).zalt[event.z] = 0
			endelse
		endif else begin
			if event.select eq 1 then begin
				(*pstate).zalt[event.z] = event.shell
				(*pstate).zalt_id[event.z] = event.zid
				(*pstate).zon[event.z] = 0
			endif else begin
				(*pstate).zon[event.z] = 0
				(*pstate).zalt[event.z] = 0
			endelse
		endelse

		zon = intarr((*pstate).n_states-1)
		zalt = intarr((*pstate).n_states-1)
		for i=1L,(*pstate).n_states-1 do begin
			q = where( (*pstate).zon eq i)
			if q[0] ne -1 then zon[i-1] = q[0]

			q = where( (*pstate).zalt eq i)
			if q[0] ne -1 then begin
				if zon[i-1] ne q[0] then zalt[i-1] = q[0]
			endif
		endfor

		*p = {ZON:zon, ZALT:zalt }
;		print,*p

		notify, 'select-periodic', p, from=event.top
		end

	'close-button': begin
		print,'Close Select Periodic ...'
		goto, kill
		end

	else:
endcase

finish:
	return

bad_state:
	warning,'select_periodic_event',['STATE variable has become ill-defined.','Abort Select Periodic.'],/error
	goto, kill
bad_ptr:
	warning,'select_periodic_event',['Parameter structure variable has become ill-defined.','Abort Select Periodic.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
return
end

;------------------------------------------------------------------------------------------

pro select_periodic, group_leader=group, TLB=tlb, xoffset=xoffset, yoffset=yoffset, $
				zon=zon, zstate=zstate

COMPILE_OPT STRICTARR

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'select_periodic',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(zon) lt 1 then zon=0
if n_elements(zstate) lt 1 then zstate=0

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
black = spec_colour('black')
white = spec_colour('white')
;									select:   0	   1	 2	   3	 4	   5	 6	  7
;if n_elements(colours) lt 1 then colours = [grey,green,orange,red,white,dgreen,blue,violet]

;									select:   0	   1	 2	   3
if n_elements(colours) lt 1 then colours = [grey,green,white,orange]

if !version.os_family eq 'MacOS' then begin
	xw = 400
	yh = 250
	xsize_help = 400
endif else begin
	xw = 400
	yh = 250
	xsize_help = 400
endelse

if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = ((screen[0]-34)/2 - xw) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((screen[1]-28)/2 - yh) > 0
endif

; 	top-level base

tlb = widget_base( /column, title='select from Periodic Table', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, _extra=extra, uname='select_periodic_TLB', /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

; set-up periodic table

n = min( [n_elements(zon), n_elements(zstate)])
if n ge 1 then begin
	zon = zon[0:n-1]
	zstate = zstate[0:n-1]
	zalt = zon
	zastate = zstate
	q = where( (zstate eq 1) and ((zon ge 3) and (zon le 94)))
	if q[0] ne -1 then begin
		zon = zon[q]
		zstate = zstate[q]
	endif else begin
		zon = 0
		zstate = 0
	endelse
	q = where( (zastate eq 3) and ((zalt ge 3) and (zalt le 94)))
	if q[0] ne -1 then begin
		zalt = zalt[q]
		zastate = zastate[q]-2
	endif else begin
		zalt = 0
		zastate = 0
	endelse
endif else begin
	zon = 0
	zstate = 0
	zalt = 0
	zastate = 0
endelse

n_state = 2
ptable = periodic_table( tbase, uname='periodic-table', n_states=n_states, n_alt_states=n_states, $
			z_on=zon, z_state=zstate, /no_tiny, colours=colours, z_alt=zalt, z_astate=zastate, $
			/start_Li, /right, legend=[' ','1',' ','2'])

bbase = widget_base( tlb, /row, /base_align_center, ypad=1, space=2)
lab = widget_label( bbase,value='Left mouse click to select "1", right click to select "2". Click again to clear.       ')
button = widget_button( bbase, value=' Close ', uname='close-button')

ion = intarr(104)
ion[zon] = zstate

state = {	$
		p:				ptr_new(/allocate_heap), $	; return notify data
		n_states:		n_states, $					; number of states
		ptable:			ptable, $					; ID of periodic table
		zon:			ion, $						; vector of all Z, giving ON state
		zalt:			intarr(104), $				; vector of all Z, giving ALT state
		zon_id:			lonarr(104), $				; widget ID on Z buttons
		zalt_id:		lonarr(104) $				; widget ID of Z buttons
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

xmanager, 'select_periodic', tlb, /no_block

return
end
