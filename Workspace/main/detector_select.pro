pro detector_select_event, event

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
common aps_4, aps_count_to_charge
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=1.
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
		warning,'detector_select_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return						; goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good(pstate, /struct) eq 0 then goto, bad_state
p = (*pstate).p
if ptr_good(p, /struct) eq 0 then goto, finish
play = (*p).playout
if ptr_good(play,/struct) eq 0 then begin
	warning,'detector_select_event','No detector layout found for selected array.'
	return
endif
n_detectors = (*play).n

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		widget_control, event.id, get_uvalue=s0
		if size(s0,/tname) eq 'STRING' then begin
			s = s0
		endif else if size(s0,/tname) eq 'STRUCT' then begin
			s = s0.help
		endif else s=''
		if event.enter eq 1 then begin
			widget_control, (*pstate).help, set_value=s
		endif else begin
			widget_control, (*pstate).help, set_value='Select detector channels (Grey: Off, Green: On, Violet: not found). Select by various classes uising the droplist below. Click again to deselect a channel or class group.'
		endelse
		goto, finish
		end
		
	'NOTIFY': begin
		case event.tag of
			(*pstate).watch: begin
				if ptr_good( event.pointer) eq 0 then break
				n = *event.pointer
				csv_index = (*play).ref
				widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0, alt:0}
				for i=0L,n_detectors-1 do begin
					j = csv_index[i]
					q = where( n eq i, nq)
					sel = (nq ge 1)
					(*(*p).pselect)[i] = sel
					widget_control, (*pstate).detector, set_value={select:j, value:sel, alt:0}
				endfor
				end
			else:
		endcase
		end
		
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'detector-select-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				case !version.os_family of
					'MacOS': begin
						xoff = 0
						yoff = 0
						end
					'unix': begin
						xoff = 0
						yoff = 0
						end
					else: begin
						xoff = 0
						yoff = 0
						end
				endcase
				widget_control, event.id, scr_xsize=(*pstate).scr_xsize+xoff, scr_ysize=(*pstate).scr_ysize+yoff
				end
			else:
		endcase
		end

	'array-mode': begin
		n = n_elements(*(*p).detector_list)
		if n lt 1 then goto, finish
		(*p).detector_mode = event.index < (n-1)
		detector_update, /array, list=list, title=title, present=(*(*p).detector_list)[(*p).detector_mode], new=i, file=f
		*(*p).detector_list = list
		(*p).detector_mode = i
		widget_control, (*pstate).detector, set_value=title, set_combobox_select=i
		detector = read_detector( f, error=error)
		if error then begin
			warning, 'detector_select_event','Error reading Detectors file '+f, /error
			goto, finish
		endif else begin
			*(*p).pdetector = *detector
			d = read_detector_layout((*(*p).pdetector).layout, error=error)
			if error eq 0 then begin
				play = (*p).playout
				*play = d
				if ptr_good(play,/struct) eq 0 then begin
					warning,'detector_select_event','No detector layout found for selected array.'
					return
				endif
				(*play).data.bad = 0
				n_detectors = (*play).n
				n = n_detectors + ((*play).start > 0)
				if n_elements(*(*p).pactive) lt n_detectors then *(*p).pactive = indgen(n_detectors)
				*(*p).pselect = bytarr(n)				
				if n_elements(*(*p).pshow) gt 0 then begin
					q = where( (*(*p).pshow ge 0) and (*(*p).pshow lt n), nq)
					if nq ge 1 then (*(*p).pselect)[ (*(*p).pshow)[q] ] = 1
				endif else begin
					if n_elements(*(*p).pactive) gt 0 then begin
						q = where( (*(*p).pactive ge 0) and (*(*p).pactive lt n), nq)
						if nq ge 1 then (*(*p).pselect)[ (*(*p).pactive)[q] ] = 1
					endif
				endelse
				detector_select_rebuild, pstate, event.top
			endif
		endelse
		goto, update
		end

	'select-mode': begin
		(*p).class = event.index
		end
		
	'clear-select': begin
		(*(*p).pselect)[*] = 0 
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0}
		goto, update
		end
		
	'save-select': begin
		F = file_requester( /write, filter = ['*.select.csv'], path=(*pstate).path, group=event.top, $
					title='Save selected detectors in a .select.csv file', /fix_filter)
		if F[0] ne '' then begin
			F = strip_file_ext(F[0], /double)+'.select.csv'
			on_ioerror, finish
			openw, lun, F[0], /get_lun
			on_ioerror, bad_save
			q = where( *(*p).pselect ne 0, nq)
			if nq eq 0 then begin
				warning,'detector_select','No detectors currently selected.'
				goto, bad_save
			endif
			printf, lun, '# Selected detector channels.'
			printf, lun, '# Saved by "detector_select".'	
			for i=0,nq-1 do begin
				printf, lun, q[i]
			endfor
bad_save:
			close_file, lun
		endif
		end
		
	'get-select': begin
		(*(*p).pselect)[*] = 0 
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0}
		
		F = file_requester( /read, filter = ['*.select.csv','*.spec'], path=(*pstate).path, group=event.top, $
					title='Set selected detectors from a .SPEC or .select.csv file', fix_filter=0)
		if F[0] ne '' then begin
			(*pstate).path = extract_path(F[0])
			ext = extract_extension(F[0])
			case strupcase(ext) of
			'SPEC': begin
				pp = read_spec(F)
				if ptr_valid(pp[0]) eq 0 then goto, finish
				npp = n_elements(pp)
				if (*pp[0]).array then begin
					if ptr_good( (*pp[0]).pactive) then begin
						n = *(*pp[0]).pactive
					endif else begin
						for j=0L,npp-1 do begin
							free_spectrum, pp[j]
						endfor
						goto, finish
					endelse
				endif else begin
					n = intarr(npp)
					for j=0L,npp-1 do begin
						if ptr_good(pp[j]) then n[j] = (*pp[j]).station + adc_offset_device((*pp[j]).DevObj)
					endfor
				endelse
	
				for j=0L,npp-1 do begin
					free_spectrum, pp[j]
				endfor
				end
			'CSV': begin
				n = get_select( F[0], error=err)
				if err or (n[0] eq -1) then goto, finish
				end
			endcase
	
			csv_index = (*play).ref
			for i=0L,n_detectors-1 do begin
				j = csv_index[i]
				q = where( n eq i, nq)
				sel = (nq ge 1)
				(*(*p).pselect)[i] = sel
				widget_control, (*pstate).detector, set_value={select:j, value:sel, alt:0}
			endfor
		endif
		end
		
	'apply': begin
		q = where( *(*p).pselect eq 1)
		*(*pstate).pnotify = q
		print, q
		notify, 'array-select', (*pstate).pnotify, from=event.top
		end
		
	'detector': begin
		
		; Select pads by all members of group that event.index belongs to ...
		
		det_index = (*play).data.index
		csv_index = (*play).ref
		if event.alt eq 1 then goto, finish
		
		case (*p).class of
			0: begin			; one only
				sel = (*(*p).pselect)[det_index]
				q = where( sel eq 1, nq)
				on = 1 - sel[event.index]
				if nq gt 0 then begin
					widget_control, (*pstate).detector, set_value={select:q, value:0, alt:0}
				endif
				(*(*p).pselect)[*] = 0 
				(*(*p).pselect)[ det_index[event.index] ] = on 
				widget_control, (*pstate).detector, set_value={select:event.index, value:on, alt:0}
				end
			1: begin			; Individual
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				(*(*p).pselect)[det_index[event.index]] = sel 
				widget_control, (*pstate).detector, set_value={select:event.index, value:sel, alt:0}
				end
			2: begin			; radial
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				q = where( (*play).data.radial eq (*play).data[event.index].radial, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*(*p).pselect)[det_index[q[j]]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:0}
					endfor
				endif
				end
			3: begin			; column
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				q = where( (*play).data.column eq (*play).data[event.index].column, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*(*p).pselect)[det_index[q[j]]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:0}
					endfor
				endif
				end
			4: begin			; row
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				q = where( (*play).data.row eq (*play).data[event.index].row, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*(*p).pselect)[det_index[q[j]]] = sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:0}
					endfor
				endif
				end
			5: begin			; chip
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				q = where( (*play).data.hermes eq (*play).data[event.index].hermes, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*(*p).pselect)[det_index[q[j]]] = sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:0}
					endfor
				endif
				end
			6: begin			; quadrant
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				q = where( (*play).data.quadrant eq (*play).data[event.index].quadrant, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*(*p).pselect)[det_index[q[j]]] = sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:0}
					endfor
				endif
				end
			7: begin			; All
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				for i=0L,n_detectors-1 do begin
					(*(*p).pselect)[det_index[i]] = sel 
					widget_control, (*pstate).detector, set_value={select:i, value:sel, alt:0}
				endfor
				end
			8: begin			; odd
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				for i=1L,n_detectors-1,2 do begin
					j = csv_index[i]
					(*(*p).pselect)[i] = sel 
					widget_control, (*pstate).detector, set_value={select:j, value:sel, alt:0}
				endfor
				end
			9: begin			; even
				sel = 1 - (*(*p).pselect)[det_index[event.index]]
				for i=0L,n_detectors-1,2 do begin
					j = csv_index[i]
					(*(*p).pselect)[i] = sel 
					widget_control, (*pstate).detector, set_value={select:j, value:sel, alt:0}
				endfor
				end
		endcase
		goto, update
		end
		
	else:
endcase

finish:
	widget_control, hourglass=0
	return

update:
	detector_select_veto_missing, pstate
	goto, finish
	
done:
	goto, kill

bad_state:
	warning,'maia_setup_event',['STATE variable has become ill-defined.','Abort Maia setup.'],/error
	goto, kill

kill:

		
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-------------------------------------------------------------------------------------------------------------

pro OnRealize_detector_select_detector_mode, wWidget

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	n = n_elements(*(*p).detector_list)
	if (n eq 1) and ( (*(*p).detector_list)[0] eq '') then goto, done
	if n lt 1 then goto, done
	(*p).detector_mode = (*p).detector_mode < (n-1)
	widget_control, wWidget, set_combobox_select=(*p).detector_mode
endif

done:
end

;-------------------------------------------------------------------------------------------------------------

pro OnRealize_detector_select_class_mode, wWidget

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	(*p).class = (*p).class < 9
	widget_control, wWidget, set_combobox_select=(*p).class
endif

done:
end

;------------------------------------------------------------------------------------------
; Veto detector pads that do not occur (not active) in any spectra ...

pro detector_select_veto_missing, pstate

p = (*pstate).p
if ptr_good(p,/struct) eq 0 then return
play = (*p).playout
if ptr_good(play,/struct) eq 0 then begin
	warning,'detector_select_veto_missing','No detector layout found for selected array.'
	return
endif
n_detectors = (*play).n

	n = n_detectors + ((*play).start > 0)
	mask = bytarr(n)
	mask[*] = 2
	
	q1 = where( *(*p).pactive lt n, nq1)			; detectors available in select window
	if nq1 ge 1 then begin							; within detector # range of this array
		mask[ (*(*p).pactive)[q1] ] = 0
	endif

	q3 = (*play).start + where( mask[(*play).start:*] ne 0, nq3)	; colour missing detectors
	if nq3 ne 0 then begin							; any missing detectors cannot be selected
		 (*(*p).pselect)[q3] = 0
		widget_control, (*pstate).detector, set_value={select:(*play).ref[q3], value:1, alt:1}
	endif
	return
end

;------------------------------------------------------------------------------------------

pro detector_select_rebuild, pstate, tlb

	old_tlb = (*pstate).base
	geom = widget_info( old_tlb, /geometry)
	(*pstate).xoffset = geom.xoffset
	(*pstate).yoffset = geom.yoffset

	widget_control, old_tlb, /destroy

	detector_select_build, pstate, tlb, group=(*pstate).group, xoffset=(*pstate).xoffset, yoffset=(*pstate).yoffset

	redirect_notify, from=old_tlb, to=tlb
	return
end

;------------------------------------------------------------------------------------------

pro detector_select_build, pstate, tlb, group_leader=group, xoffset=xoffset, yoffset=yoffset

if n_elements(group) lt 1 then group=0L
if n_elements(pstate) lt 1 then return
if ptr_valid(pstate) eq 0 then return

p = (*pstate).p
if ptr_good(p,/struct) eq 0 then return

detector_update,  list=detector_list, title=detector_title, /array

n = n_elements(detector_list)
if (n eq 0) or ((n eq 1) and (detector_list[0] eq '')) then begin
	warning,'detector_select_build','No detector arrays found.'
	return
endif

play = (*p).playout
if ptr_good(play,/struct) eq 0 then begin
	warning,'detector_select_build','No detector layout found for selected array.'
	return
endif
n_detectors = (*play).n

(*play).data.bad = 0					; ??

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
pink = spec_colour('pink')
black = spec_colour('black')
white = spec_colour('white')

;	  select: 0     1     2     3
colours = [grey,green,violet,violet]			; for detector mimic
colours2 = [green, yellow, red]					; for warning "buttons"

case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		help_xsize = 380
		space1 = 1
		space2 = 1
		space5 = 5
		space10 = 10

		right_xsize2 = (n_detectors eq 384) ? 705 : 300
		right_ysize2 = (n_detectors eq 384) ? 690 : 450
		text_xsize = 120
		text_xsize3 = 10
		button_xsize = 50
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		help_xsize = 380
		space1 = 1
		space2 = 1
		space5 = 5
		space10 = 10

		right_xsize2 = (n_detectors eq 384) ? 705 : 300
		right_ysize2 = (n_detectors eq 384) ? 690 : 450
		text_xsize = 120
		text_xsize3 = 10
		button_xsize = 50
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		help_xsize = 380
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10

		right_xsize2 = (n_detectors eq 384) ? 705 : 300
		right_ysize2 = (n_detectors eq 384) ? 690 : 450
		text_xsize = 120
		text_xsize3 = 10
		button_xsize = 50
		end
endcase

; 	top-level base

tlb = widget_base( /column, title='Array Detector Channel Selection', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='detector_select_TLB', xoffset=xoffset, yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

array_base = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
label = widget_label( array_base, value='Select Array detector:')
array_mode = widget_combobox( array_base, value=detector_title, uname='array-mode', xsize=160, /tracking, $
					notify_realize='OnRealize_detector_select_detector_mode', $
					uvalue='Select an available array detector parameter set. ')

detector = detector_mimic( tbase, data=play, uname='detector', uvalue='Select detectors(s) by ' + $
					'clicking on individual detector pads, or members of groups of pads or Classes. Select a grouping class using the ' + $
					'"Detector Selection Class" droplist. If the "Detector Selection Class" mode is not in "Individual" all members of a class ' + $
					'will be selected together. Click again to deselect.', /tracking,  $
					xsize_min=right_xsize2, xsize_max=right_xsize2, ysize_max=right_ysize2, csize=csize, colours=colours)

class_base = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=2)
label = widget_label( class_base, value='Selection:')
class_mode = widget_combobox(class_base, uname='select-mode', scr_xsize=120, $
			NOTIFY_REALIZE='OnRealize_detector_select_class_mode', $
			value=['One only','Individual','Radial','Column','Row','Chip','Quadrant','All','Odd','Even'], /tracking, $
					uvalue='Select the grouping of detector pads selected or deselected on a left mouse button click.')

button = widget_button( class_base, value='Clear', uname='clear-select', /tracking, uvalue='Clear all detector selections.')
button = widget_button( class_base, value='Get', uname='get-select', /tracking, uvalue='Set detector selections based on detectors present in a .SPEC file or a .select.csv table.')
button = widget_button( class_base, value='Save', uname='save-select', /tracking, uvalue='Save detector selections to a .select.csv table file.')
label = widget_label( class_base, value=' ', scr_xsize=text_xsize3)
button = widget_button( class_base, value='Apply', uname='apply', /tracking, uvalue='Apply the detector selection to the parent window.')

help = widget_text( tbase, scr_xsize=help_xsize, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Help text ...', frame=0)

(*pstate).base = tlb
(*pstate).array_mode = array_mode
(*pstate).detector = detector
(*pstate).class_mode = class_mode
(*pstate).help = help
(*pstate).xoffset = xoffset
(*pstate).yoffset = yoffset
widget_control, tbase, set_uvalue=pstate

widget_control, tlb, /realize
xmanager, 'detector_select', tlb, /no_block

; Show detector pads that are selected (shown in select table) and active (they occur in spectra) ...

widget_control, (*pstate).detector, set_value={select: (*play).ref[indgen(n_detectors)+(*play).start], value:(*(*p).pselect)[(*play).start:*], alt:0}

; Veto detector pads that do not occur (not active) in any spectra ...

detector_select_veto_missing, pstate
return
end

;------------------------------------------------------------------------------------------

; file	suggest a detector file to use, if not found use first (note spectra do not have this).

pro detector_select, group=group, pars=p, TLB=tlb, active=active, select=show, $
			path=path, watch=watch, file=file

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
ErrorNo = 0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'detector_select',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0
if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(file) lt 1 then file=''

case !version.os_family of
	'MacOS': begin
		xw = 405
		yh = 625
		end
	'unix': begin
		xw = 405
		yh = 625
		end
	else: begin
		xw = 405
		yh = 625
		end
endcase

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
	xoffset = ((xoff - xw) < (screen[0] - xw)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((yoff - yh+h) < (screen[1] - yh)) > 0
endif

detector_update,  list=detector_list, title=detector_title, /array

n = n_elements(detector_list)
if (n eq 0) or ((n eq 1) and (detector_list[0] eq '')) then begin
	warning,'detector_select','No detector arrays found.'
	return
endif
qi = where( detector_list eq strip_path(file), nqi)
id = (nqi ge 1) ? qi[0] : 0

p = bad_pars_struct( p, make_pars=make_p)

if make_p then begin
	detector_update, present=detector_list[id], new=i, file=f
	detector = read_detector( f, error=error)
	if error then begin
		warning, 'detector_select','Read error in Detectors file: '+detector_list[0], /error
		return
	endif else begin
		d = read_detector_layout( (*detector).layout, error=error)
		if error then begin
			warning, 'detector_select','Read error in Layout file: '+(*detector).layout, /error
			return
		endif
		n_detectors = d.n
	endelse

	pars = {	$
			detector_mode:		0, $								; detector droplist index
			class:				0, $								; class selection
			detector_list:		ptr_new(/allocate_heap), $			; pointer to list of detector file names
			pdetector:			ptr_new(detector), $				; pointer to detector struct (kept in parent)
			playout:			ptr_new(d), $						; pointer to detector layout struct (kept in parent)
			pselect:			ptr_new(bytarr(n_detectors+d.start)), $	; pointer to detector channel selection array
			pactive:			ptr_new(indgen(n_detectors)), $		; pointer to active detectors, if specified on entry
			pshow:				ptr_new(/allocate_heap) $			; pointer to shown detectors, if specified on entry
	}
	*p = pars
endif
*(*p).detector_list = detector_list
play = (*p).playout
n_detectors = (*play).n
n = n_detectors + ((*play).start > 0)
if n_elements(active) gt 0 then begin
	*(*p).pactive = active
endif else begin
	active = indgen(n_detectors)
	*(*p).pactive = active
endelse
if n_elements(show) gt 0 then begin
	*(*p).pshow = show
	(*(*p).pselect)[ (show > 0) < (n-1) ] = 1
endif else begin
	*(*p).pselect = bytarr(n)
	(*(*p).pselect)[ (active > 0) < (n-1) ] = 1
endelse
notify_watch = n_elements(watch) gt 0 ? watch: 'none'

state = { $
		p:					p, $						; pointer to pars struct (kept in parent)
		pnotify: 			ptr_new(/allocate_heap), $	; pointer to notify buffer 
		path:				path, $						; path default
		watch:				notify_watch, $				; select notify string to watch for
		
		array_mode:			0L, $						; array select widget ID
		detector:			0L, $						; detector mimic ID
		class_mode:			0L, $						; selection class droplist ID
		tracking:			1, $						; tracking enabled for help
		xoffset:			xoffset, $					; X offset
		yoffset:			yoffset, $					; Y offset
		base:				0L, $						; TLB
		group:				group, $					; parent ID
		help:				0L $						; help text field ID
	}
pstate = ptr_new(state, /no_copy)
	
detector_select_build, pstate, TLB, group_leader=group, xoffset=xoffset, yoffset=yoffset

register_notify, tlb, ['path'], from=group
if n_elements(watch) gt 0 then begin
	register_notify, tlb, [watch], from=group
endif
return
end
	