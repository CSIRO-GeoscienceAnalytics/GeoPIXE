pro detector_layout_event, event

COMPILE_OPT STRICTARR

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
	   warning,'Detector_layout',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   goto, kill
	endif
endif

if widget_info( event.id, /valid) eq 0 then return
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if size(pstate,/tname) ne 'POINTER' then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

pd = (*pstate).data
if size(pd,/tname) ne 'POINTER' then goto, bad_ptr
if size(*pd,/tname) ne 'STRUCT' then goto, bad_ptr

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					print,'Fit Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end

			'detector-get': begin
				if ptr_valid( event.pointer) then begin
					p = event.pointer
					print,'Received: "detector-get" notify'
					(*pd).data[*].bad = 2
					(*pd).data[*].fwhm = 0.0
					for k=0L,n_elements((*p).enable)-1 do begin
						q = where( k eq (*pd).data.index - (*pd).start)
						i = q[0]
						if i ne -1 then begin
							if (*p).enable[k] then begin
;								print,'i=',i,'  k =',k,'  fwhm=', (*p).fwhm[k]
								(*pd).data[i].FWHM = (*p).FWHM[k]
								(*pd).data[i].bad = 0
							endif else begin
								(*pd).data[i].bad = 2
							endelse
						endif
					endfor
					for i=0L,(*pd).n-1 do begin
						if (*pd).data[i].bad le 1 then begin
							(*pd).data[i].bad = ((*pd).data[i].FWHM gt (*pd).Threshold) ? 1 : 0
						endif
						widget_control, (*pstate).layout, set_value={select:i, value:(*pd).data[i].bad}
					endfor
				endif
				end

			'detector-setup-load': begin
				(*pstate).file = (*pd).file
			 	detector_layout_rebuild, pstate, event.top
				end

			'evt-load': begin
				(*(*pstate).pnotify).pdata = pd
				notify, 'detector-load', (*pstate).pnotify, from=(*pstate).base
				end
			else:
		endcase
		end
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s
		prompt = 'Colours indicate: (1) YELLOW: excessive FWHM; (2) VIOLET: disabled; (3) RED: warning. ' + $
							'Click detector to select it in "Sort EVT".'
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				widget_control, (*pstate).help, set_value=prompt
			endelse
		endif else begin
			i = s
			k = (*(*pstate).data).data[i].index
			s = 'Detector['+str_tidy(i)+'] = '+str_tidy(k)+', FWHM = '+str_tidy((*(*pstate).data).data[i].fwhm)+ $
							' eV, Bad flag = '+str_tidy((*(*pstate).data).data[i].bad)+' [0=good, 1=poor FWHM, 2=dead, 3=fit error]'
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				widget_control, (*pstate).help, set_value=prompt
			endelse
		endelse
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request Fit_Setup ...'
		goto, kill
		end
	else:
endcase

if widget_info( event.id, /valid) eq 0 then return
uname = widget_info( event.id, /uname)
case uname of

	'load-button': begin
		file = find_file2( (*pstate).file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /read, filter = ['*.detector.csv','*.csv'], $
				/must_exist, path=path, group=event.top, $
				title='Select the source detector array layout file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.csv'
			detector_layout_load, pstate, F, error=error
			if not error then begin
				(*pstate).file = F
			 	detector_layout_rebuild, pstate, event.top
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).file_id, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).file_id, set_value=F, set_text_select=[n-1,0]

				(*(*pstate).pnotify).pdata = pd
				notify, 'detector-load', (*pstate).pnotify, from=(*pstate).base
			endif else widget_control, (*pstate).file_id, set_value='error in file'
		endif
		end

	'detector-file': begin
		widget_control, event.id, get_value=F
	   	if F ne '' then begin
			F = strip_file_ext(F) + '.csv'
			detector_layout_load, pstate, F, error=error
			if not error then begin
				(*pstate).file = F
			 	detector_layout_rebuild, pstate, event.top
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).file_id, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).file_id, set_value=F, set_text_select=[n-1,0]

				(*(*pstate).pnotify).pdata = pd
				notify, 'detector-load', (*pstate).pnotify, from=(*pstate).base
			endif else widget_control, (*pstate).file_id, set_value='error in file'
		endif
		end

	'save-button': begin
		file = find_file2( (*pstate).file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /write, filter = '*.csv', $
;		F = file_requester( /write, filter = '*.detector.csv', $
				path=path, group=event.top, $
				title='Save the detector array layout parameters to a file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.csv'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).file_id, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).file_id, set_value=F, set_text_select=[n-1,0]
			(*pstate).file = F
			detector_layout_save, pstate, F
		endif
		end

	'ARRAY': begin
;		print, 'Detector=',event.detector, '  select=',event.select, '  alt=',event.alt
		(*(*pstate).pnotify).n = event.detector
		(*(*pstate).pnotify).pdata = pd
		notify, 'detector-select', (*pstate).pnotify, from=(*pstate).base
		end

	'options': begin
		case event.value of
			0: begin
				(*pd).reorient = ((*pd).reorient + 1) mod (*pd).symmetry
				widget_control, (*pstate).options, set_value=[((*pd).reorient ne 0), (*pd).veto, (*pd).mirrorX, (*pd).mirrorY]
			 	detector_layout_rebuild, pstate, event.top
				end
			1: begin
				(*pd).veto = event.select
				end
			2: begin
				(*pd).MirrorX = event.select
			 	detector_layout_rebuild, pstate, event.top
				end
			3: begin
				(*pd).MirrorY = event.select
			 	detector_layout_rebuild, pstate, event.top
				end
			else:
		endcase

		(*(*pstate).pnotify).pdata = pd
		notify, 'detector-load', (*pstate).pnotify, from=(*pstate).base
		end

	'fwhm-threshold': begin
		(*pd).threshold = event.value
		sel = indgen((*pd).n)
		val = intarr((*pd).n)
		for i=0L,(*pd).n-1 do begin
			if (*pd).data[i].bad le 1 then begin
				if (*pd).data[i].fwhm gt (*pd).threshold then begin
					(*pd).data[i].bad = 1
				endif else begin
					(*pd).data[i].bad = 0
				endelse
				val[i] = (*pd).data[i].bad
			endif
		endfor
		widget_control, (*pstate).layout, set_value={select:sel, value:val}

		if (*pd).veto then begin
			(*(*pstate).pnotify).pdata = pd
			notify, 'detector-fwhm', (*pstate).pnotify, from=(*pstate).base
		endif
		end
	else:
endcase

finish:
	return

bad_state:
	warning,'detector_layout_event',['STATE variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill
bad_ptr:
	warning,'detector_layout_event',['Parameter structure variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill

kill:
	if ptr_valid(pstate) eq 0 then goto, die
	if n_elements((*pstate)) lt 1 then goto, die
;	if size((*pstate),/tname) ne 'STRUCT' then goto, die
	ptr_free, pstate

;   cancel_notify, event.top

die:
	widget_control, event.top, /destroy
	return
end

;------------------------------------------------------------------------------------------

pro detector_layout_save, pstate, F

	p = (*pstate).data

	write_detector_layout, F, data=p, error=error
	return
end


;------------------------------------------------------------------------------------------

pro detector_layout_rebuild, pstate, tlb

	old_tlb = (*pstate).base
	geom = widget_info( old_tlb, /geometry)
	(*pstate).xoffset = geom.xoffset
	(*pstate).yoffset = geom.yoffset

	widget_control, old_tlb, /destroy

	detector_layout_build, pstate, tlb, group=(*pstate).group, xoffset=(*pstate).xoffset, yoffset=(*pstate).yoffset

	redirect_notify, from=old_tlb, to=tlb
	return
end

;------------------------------------------------------------------------------------------

pro detector_layout_load, pstate, F, error=error

if n_elements(F) lt 1 then return
if strlen(F) lt 1 then return

	d = read_detector_layout(F, error=error)
	if not error then begin
		*(*pstate).data = d
		(*pstate).file = F
		*(*pstate).path = extract_path(F)

		widget_control, (*pstate).options, set_value=[(*(*pstate).data).reorient,(*(*pstate).data).veto]
	endif
	return
end

;------------------------------------------------------------------------------------------

pro detector_layout_build, pstate, tlb, group_leader=group, xoffset=xoffset, yoffset=yoffset

if n_elements(group) lt 1 then group=0L
if n_elements(pstate) lt 1 then return
if ptr_valid(pstate) eq 0 then return

pd = (*pstate).data
if n_elements(pd) lt 1 then return
if ptr_valid(pd) eq 0 then return

for i=0L,(*pd).n-1 do begin
	if (*pd).data[i].bad le 1 then begin
		(*pd).data[i].bad = ((*pd).data[i].FWHM gt (*pd).Threshold) ? 1 : 0
	endif
endfor

; state button does not use Font at the moment ...

case !version.os_family of
	'MacOS': begin
	   fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
	   csize = 1.2
	   help_xsize = 300
	   help_xsize2 = 600
	   end
	'unix': begin
	   fnt1 = 'timr08'             ;'6x12'
	   csize = 1.2
	   help_xsize = 300
	   help_xsize2 = 600
	   end
	else: begin
	   fnt1 = 'Helvetica*8'       ;'ARIAL*12'
	   csize = 1.3
	   help_xsize = 300
	   help_xsize2 = 600
	   end
endcase

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

;	  select: 0     1     2     3
colours = [grey,yellow,violet,red]

w = 0
h = 0
xoff = 0
yoff = 0
if widget_info( Group, /valid) then begin
	geom = widget_info( Group, /geometry)
	w = geom.scr_xsize
	h = geom.scr_ysize
	xoff = geom.xoffset
	yoff = geom.yoffset
endif
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = ((xoff + w) < (screen[0]-34 - 237)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((yoff - 250) < (screen[1]-28 - 342)) > 0
endif

; This base will become the first child of the parent TLB top-level base

TLB = Widget_Base( GROUP_LEADER=Group, UNAME='Array_TLB', /TLB_KILL_REQUEST_EVENTS, xpad=2, ypad=0,  $
	   /ALIGN_CENTER ,/BASE_ALIGN_CENTER ,TITLE='Array detector Layout' ,SPACE=1,  $
	   /COLUMN, xoffset=xoffset, yoffset=yoffset)

base = widget_base( tlb, /column, /base_align_center, ypad=0, xpad=0, space=1)

sbase = widget_base( base, /row, /base_align_center, ypad=2, xpad=0, space=5)
lab = widget_label( sbase, value='Layout:')
array_file = widget_text( sbase, value=(*pstate).file, uname='detector-file', /tracking, /editable, $
			  uvalue='Enter a file-name for source detector array layout details, or use the "Load" button.',scr_xsize=200)
load_button = widget_button( sbase, value='Load', uname='load-button', /tracking, $
			  uvalue='Load detector array layout parameters from a previous array file.', scr_xsize=38)
save_button = widget_button( sbase, value='Save', uname='save-button', /tracking, $
			  uvalue='Save detector array layout parameters to an array file.', scr_xsize=38)


base2 = detector_mimic( base, data=pd, uname='ARRAY', uvalue='Click on a detector pad to select it in connected windows, such as "Sort EVT".', tracking=1, $
				xsize_min=help_xsize, xsize_max=help_xsize2, csize=csize, colours=colours)


vbase = widget_base( base, /row, /base_align_center, ypad=0, xpad=0, space=5)

options = cw_bgroup2( vbase, ['Re-orient detector array','Veto poor FWHM','Mirror in X','Mirror in Y'], row=2, set_value=[(*(*pstate).data).reorient ne 0,(*(*pstate).data).veto,(*(*pstate).data).mirrorX,(*(*pstate).data).mirrorY], $
				/return_index, uname='options',/ nonexclusive, /tracking, $
				uvalue=['Re-orient the detector array by its symmetry step angle (360/symmetry).', $
				'Veto or disable poor FWHM that exceed the "Threshold" value. This will affect the "Enable" check-marks in "Sort EVT".', $
				'Mirror the detector layout in original X direction, in the Horizontal direction, before reorienting.','Mirror the detector layout in original Y direction, in the Vertical direction, before reorienting.'], ypad=0)

tbase = widget_base( base, /row, /base_align_center, ypad=0, xpad=0, space=5)

lab = widget_label( tbase, value='FWHM Thresold:')
threshold_slider = cw_fslider2( tbase, format='(F8.3)', minimum=100.0, maximum=1000.0, layout=1, $
				value=(*(*pstate).data).threshold, uname='fwhm-threshold', xsize=160, /tracking, /edit, /drag, scroll=10.0, $
				uvalue='Adjust the threshold FWHM value. Use "Veto" above to disable excessive FWHM in "Sort EVT".')

help = widget_text( base, scr_xsize=help_xsize>340, ysize=3, /wrap, uname='HELP', /tracking, $
		  uvalue='Help window. Displays info about widgets. Move the mouse pointer over widgets to get information about them.',frame=0)

(*pstate).base = tlb
(*pstate).file_id = array_file
(*pstate).options = options
(*pstate).layout = base2
(*pstate).help = help
(*pstate).xoffset = xoffset
(*pstate).yoffset = yoffset
widget_control, base, set_uvalue=pstate

widget_control, tlb, /realize

n = lenchr((*pstate).file)
k = lenchr(strip_path((*pstate).file))
widget_control, (*pstate).file_id, set_value=(*pstate).file, set_text_select=[n-k,k]
widget_control, (*pstate).file_id, set_value=(*pstate).file, set_text_select=[n-1,0]

xmanager, 'detector_layout', tlb, /no_block
return
end

;------------------------------------------------------------------------------------------

pro detector_layout, file, group_leader=group, TLB=tlb, xoffset=xoffset, yoffset=yoffset, $
				data=data, path=path

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
	   warning,'Detector_layout',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=ptr_new('')
startupp, error=catch_errors_on, /colours
register_notify

; Initial layout read for now ...

pd = bad_pars_struct( data, make_pars=make_pd)
if make_pd then begin
	d = read_detector_layout(file, error=error)
	if error then begin
		warning,'detector_layout','Failed to read "'+file+'" to initialize layout.'
		return
	endif
	*pd = d
endif
if n_elements(file) lt 1 then file = (*pd).file

state = { base:		0L, $						; base (first child of TLB)
		data:		pd, $						; pointer to detector data
		pnotify:	ptr_new({n:0, pdata:pd}), $	; struct to pass in Notify
		file:		file, $						; file-name
		path:		path, $						; pointer to current path string
		group:		group, $					; group leader ID
		layout:		0L, $						; ID of layout
		xoffset:	0, $						; window X offset
		yoffset:	0, $						; window Y offset
		file_id:	0L, $						; ID of file widget text
		options:	0L, $						; options check-marks
		help:       0L $	              		; help widget
	   }
pstate = ptr_new(state, /no_copy)

detector_layout_build, pstate, TLB, group_leader=group, xoffset=xoffset, yoffset=yoffset

register_notify, tlb, ['detector-get','evt-load','detector-setup-load'], from=group
return
end
