
; IDL Event Callback Procedures
; Spectrum_display_eventcb
;
; NOTE:
;1. If the Spectrum_display.pro file is regenerated, without generating this
;   eventcb file, then the Spectrum_TLB pro is missing the following line.
;   This needs to be added by hand:
;
;   resolve_routine, 'spectrum_display_eventcb'
;
;2. The generated Spectrum_display.pro does no use 'no_block' in the call
;   to xmanager.  Add '/no_block' to xmanager line.
;
;3. The lines in the event handler for spectrum_display_event for draw
;   widget button/motion events have been changed by hand.
;
;----------------------------------------------------------------------------
; NOTE ABOUT COMPOUND WIDGETS
;
; The GUI Builder generated Spectrum_display.pro file assumes this to be an
; application, not a compound widget. To modify the generated spectrum_CW.pro
; file, to make it a compound widget (if needed), follow these steps:
;
;1. Change the first line (of stub at end of file) to a function call:
;
;   function Spectrum_display, parent, Uvalue=uvalue, _EXTRA=_VWBExtra
;
;2. These arguments are passed on to the top-level base:
;
;   tlb = widget_base( parent, uvalue=uvalue, ...
;
;3. Note that we don't have a group-leader argument now, either in the
;   first line or in the Spectrum_TLB = widget_base() call.
;
;4. Comment out the xmanager call in Spectrum_TLB.
;
;5. Add the following to the tlb definition widget_base call:
;
;   event_func='spectrum_TLB_event'
;
;6. Comment out the widget_control, /realize call. This is done in
;   the calling definition routine.
;
;7. NOTE: The whole issue of a CW passing on an event, perhaps with
;   modification, has not been addressed.
;
;-----------------------------------------------------------------
; BUTTON_EVENTS Callback Procedure.
;
;   {WIDGET_DRAW, ID:0L, TOP:0L, HANDLER:0L, TYPE: 0, X:0, Y:0,
;       PRESS:0B, RELEASE:0B, CLICKS:0}
;
;   TYPE returns a value that describes the type of draw widget
;       interaction that generated an event: 0 - Button Press, 1 -
;       Button Release, 2 - Motion, 3 - Viewport Moved, 4 -
;       Visibility Changed (Expose)
;--------------------------------------------------------------------

pro OnButton_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;if ptr_valid( (*pstate).p) then begin
    Spectrum_Select, group_leader=event.top, spectrum=(*pstate).p, TLB=tlb, $
         highlight = {on:(*pstate).highlight_on, highlight:(*pstate).highlight}, $
         realtime=(*pstate).realtime, show=(*pstate).pshow, update_notify=(*pstate).update_notify, $
         layout=(*pstate).layout
    register_notify, event.top, $
         [(*pstate).update_notify, $	; display par changes
         'spectrum-highlight', $		; highlight one spectrum
         'image-region-select', $		; region select from image region
		 'select-highlight' $			; row select from spectrum-select
         ], from=tlb
;endif
end
;
;-----------------------------------------------------------------

pro OnButton_Spectrum_Analyze, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ((*pstate).mark_set eq 4) or ((*pstate).mark_set eq 1) then begin

    results = analyze_cut( pstate, error=error)

    if error eq 0 then begin
       no_results = 0
       if n_elements( *(*pstate).pcut) eq 0 then no_results=1
       if no_results eq 0 then if ptr_valid( (*(*pstate).pcut)[0]) eq 0 then no_results=1
       if no_results then begin
         *(*pstate).pcut = ptr_new( results, /no_copy)
       endif else begin
         *(*pstate).pcut = [ *(*pstate).pcut, ptr_new( results, /no_copy)]
       endelse
       notify, 'cut-results', from=event.top
    endif
endif
end
;
;-----------------------------------------------------------------

pro OnButton_Identify, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

identify2, group_leader=event.top, TLB=tlb
register_notify, event.top, ['mark-e','mark-element','mark-fit'], from=tlb

(*pstate).mark_set = 0
widget_control, (*pstate).marker, set_combobox_select=0
wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]
plot_markers, pstate
end
;
;-----------------------------------------------------------------

pro OnButton_Full, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]
    (*pstate).elow = (*p).emin > 0.01
    (*pstate).ehigh = (*p).emax
    (*pstate).ylow = 0.0

    (*pstate).vlow = 0
    (*pstate).vhigh = (*pstate).view-1
    widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Rescale, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

rescale_spectra, pstate, mode=1

draw_spectra, pstate
notify, 'spectra-changed', (*pstate).p, from=event.top
end

;-----------------------------------------------------------------

pro OnButton_Up, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]
    (*pstate).yhigh = (*pstate).yhigh/1.5

	(*pstate).scale_mode = 0
	widget_control, (*pstate).rescale, set_combobox_select=(*pstate).scale_mode

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Down, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]
    (*pstate).yhigh = (*pstate).yhigh*1.5

	(*pstate).scale_mode = 0
	widget_control, (*pstate).rescale, set_combobox_select=(*pstate).scale_mode

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Shrink, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]

    vmid = 0.5*float((*pstate).vlow + (*pstate).vhigh)
    emid = (*pstate).a * vmid + (*pstate).b
    de = ((*pstate).ehigh - (*pstate).elow)*1.5

	if de lt ((*p).emax-(*p).emin) then begin
		(*pstate).elow = emid - 0.5*de
		(*pstate).ehigh = emid + 0.5*de
		if (*pstate).elow lt (*p).emin then begin
			(*pstate).ehigh = (*pstate).ehigh + ((*p).emin-(*pstate).elow)
			(*pstate).elow = (*p).emin > 0.01
		endif
		if (*pstate).ehigh gt (*p).emax then begin
			(*pstate).elow = ((*pstate).elow - ((*pstate).ehigh-(*p).emax)) > 0.01
			(*pstate).ehigh = (*p).emax
		endif
	endif else begin
		(*pstate).elow = (*p).emin > 0.01
		(*pstate).ehigh = (*p).emax
	endelse
	
	(*pstate).a = ((*pstate).ehigh - (*pstate).elow) / $
			(float((*pstate).width*(1.+(*pstate).position[0])+1) * $
			(!x.window[1]-!x.window[0]))
	(*pstate).b = (*pstate).elow - (*pstate).a * !x.window[0] * float((*pstate).width*(1.+(*pstate).position[0])+1) + $
			float((*pstate).xoffset * (*pstate).a)

    vmid = (emid - (*pstate).b) / (*pstate).a

	(*pstate).vlow = vmid - float((*pstate).view+1)/2
	if (*pstate).vlow lt 0 then begin
		(*pstate).vlow = 0
	endif
	if (*pstate).vlow ge (*pstate).width-(*pstate).view then begin
		(*pstate).vlow = (*pstate).width-(*pstate).view
	endif
    (*pstate).vhigh = (*pstate).vlow + (*pstate).view-1
    widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Expand, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]

    vmid = 0.5*float((*pstate).vlow + (*pstate).vhigh)
    emid = (*pstate).a * vmid + (*pstate).b
    de = ((*pstate).ehigh - (*pstate).elow)/1.5

	if de lt ((*p).emax-(*p).emin) then begin
		(*pstate).elow = emid - 0.5*de
		(*pstate).ehigh = emid + 0.5*de
		if (*pstate).elow lt (*p).emin then begin
			(*pstate).ehigh = (*pstate).ehigh + ((*p).emin-(*pstate).elow)
			(*pstate).elow = (*p).emin > 0.01
		endif
		if (*pstate).ehigh gt (*p).emax then begin
			(*pstate).elow = ((*pstate).elow - ((*pstate).ehigh-(*p).emax)) > 0.01
			(*pstate).ehigh = (*p).emax
		endif
	endif else begin
		(*pstate).elow = (*p).emin > 0.01
		(*pstate).ehigh = (*p).emax
	endelse
	
	(*pstate).a = ((*pstate).ehigh - (*pstate).elow) / $
			(float((*pstate).width*(1.+(*pstate).position[0])+1) * $
			(!x.window[1]-!x.window[0]))
	(*pstate).b = (*pstate).elow - (*pstate).a * !x.window[0] * float((*pstate).width*(1.+(*pstate).position[0])+1) + $
			float((*pstate).xoffset * (*pstate).a)

	vmid = (emid - (*pstate).b) / (*pstate).a

	(*pstate).vlow = vmid - float((*pstate).view+1)/2
	if (*pstate).vlow lt 0 then begin
		(*pstate).vlow = 0
	endif
	if (*pstate).vlow ge (*pstate).width-(*pstate).view then begin
		(*pstate).vlow = (*pstate).width-(*pstate).view
	endif
	(*pstate).vhigh = (*pstate).vlow + (*pstate).view-1
    widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Widen, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]
    e1 = (*p).emin > ((*pstate).cmark[0,3] * (*p).cal.poly[1] + (*p).cal.poly[0])
    e2 = (*p).emax < ((*pstate).cmark[1,3] * (*p).cal.poly[1] + (*p).cal.poly[0])
    emid = 0.5*(e1 + e2)
    de = (e2-e1) * (*pstate).width/(*pstate).view
    if de lt 0.1 then goto, done

	if de lt ((*p).emax-(*p).emin) then begin
		(*pstate).elow = emid - 0.5*de
		(*pstate).ehigh = emid + 0.5*de
		if (*pstate).elow lt (*p).emin then begin
			(*pstate).ehigh = (*pstate).ehigh + ((*p).emin-(*pstate).elow)
			(*pstate).elow = (*p).emin > 0.01
		endif
		if (*pstate).ehigh gt (*p).emax then begin
			(*pstate).elow = ((*pstate).elow - ((*pstate).ehigh-(*p).emax)) > 0.01
			(*pstate).ehigh = (*p).emax
		endif
	endif else begin
		(*pstate).elow = (*p).emin > 0.01
		(*pstate).ehigh = (*p).emax
	endelse
	
	(*pstate).a = ((*pstate).ehigh - (*pstate).elow) / $
			(float((*pstate).width*(1.+(*pstate).position[0])+1) * $
			(!x.window[1]-!x.window[0]))
	(*pstate).b = (*pstate).elow - (*pstate).a * !x.window[0] * float((*pstate).width*(1.+(*pstate).position[0])+1) + $
			float((*pstate).xoffset * (*pstate).a)

	vmid = (emid - (*pstate).b) / (*pstate).a

	(*pstate).vlow = vmid - float((*pstate).view+1)/2
	if (*pstate).vlow lt 0 then begin
		(*pstate).vlow = 0
	endif
	if (*pstate).vlow ge (*pstate).width-(*pstate).view then begin
		(*pstate).vlow = (*pstate).width-(*pstate).view
	endif
	(*pstate).vhigh = (*pstate).vlow + (*pstate).view-1
    widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]
    print,'Vlow=',(*pstate).vlow,' Vhigh=',(*pstate).vhigh

    draw_spectra, pstate
	notify, 'spectra-changed', (*pstate).p, from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro OnButton_Zoom, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    print,'ZOOM button'
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Log, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
    j = current_plot( pstate)
    p = (*(*pstate).p)[j]
    log = ((*pstate).log eq 0) ? 1 : 0
    (*pstate).log = log

	for i=0L,n_elements(*(*pstate).p)-1 do begin
		p = (*(*pstate).p)[i]
		if (*(*pstate).pshow)[i] eq 1 then (*p).log = log
	endfor

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Clear, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	for i=0L,n_elements(*(*pstate).p)-1 do begin
		p = (*(*pstate).p)[i]
		(*(*p).data)[*] = 0
	endfor
	*(*pstate).pregions = 0L

    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top
    s = (*pstate).update_notify + '-cleared'
    notify, s, (*pstate).p, from=event.top
endif
end

;-----------------------------------------------------------------

pro OnButton_Show_errors, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.value of
	0: begin
		(*pstate).show_errors = event.select
		end
	else:
endcase
draw_spectra, pstate
end

;-----------------------------------------------------------------

pro OnButton_Show_Diff, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.value of
	0: begin
		(*pstate).show_diff = event.select
		end
	else:
endcase
draw_spectra, pstate
end

;-----------------------------------------------------------------

pro OnDestroy_Spectrum, wWidget

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
       warning,'OnDestroy_Spectrum',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

free_spectrum_state, pstate
end

;-----------------------------------------------------------------

pro OnKill_Spectrum, Event

cancel_notify, event.top

widget_control, event.top, /destroy
;heap_gc, /verbose
end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:from }
;
;   TAG   string showing the notification name, as registered.
;   POINTER   pointer passed as a general argument (can be null).
;   FROM  id of widget sending the notify (or 0).
;-----------------------------------------------------------------

pro OnNotify_Spectrum, Event

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
       warning,'OnNotify_spectrum',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;print,'spectrum_display:  ', event.tag, '  '

case event.tag of

    'image-clone': begin                ; returned from 'image' clone

       (*pstate).pimage = ptr_new()
       p2 = event.pointer                   ; pointer to snapshot of image 'state'
       if ptr_valid(p2) eq 0 then goto, done
       p = (*p2).p
       if ptr_valid(p) eq 0 then goto, done
       (*pstate).pimage = p

       if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
       (*pstate).matrix_file = ''
       (*pstate).matrix = ptr_new()
       end

    'images': begin                        ; returned from 'Sort EVT' via 'image'

       (*pstate).pimage = ptr_new()
       p = event.pointer                  ; pointer to image
       if ptr_valid(p) eq 0 then goto, done
       (*pstate).pimage = p

       if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
       (*pstate).matrix_file = ''
       (*pstate).matrix = ptr_new()
       end

    'correct-image-pileup': begin            ; subtract pileup from images

       Spectrum_Correct_Pileup_Image, Event
       return
       end

    'detector-select': begin
		if ptr_valid( event.pointer) eq 0 then goto, done
		if ptr_valid( (*pstate).p) eq 0 then goto, done
		i = (*event.pointer).detector
		lab = 'Detector #'+str_tidy(i)+' '
		n = -1
		for j=0L,n_elements( *(*pstate).p)-1 do begin
			label = (*(*(*pstate).p)[j]).label
			if locate( lab, label) ge 0 then begin
				n = j
				break
			endif
		endfor
		if n lt 0 then goto, done
		
		if (*pstate).highlight_on then begin
			(*pstate).highlight = n
		endif else begin
;			goto, done
			show_one_spectrum, pstate, n
		endelse
		draw_spectra, pstate
		*(*pstate).pselect = n
		notify, 'select-update', (*pstate).pselect, from=event.top
		end
		 
	'image-regions': begin
		if ptr_good( event.pointer) eq 0 then goto, done
		*(*pstate).pregions = *(event.pointer)						; pointer to all regions (0L if don't match spectra anymore)
		end

	'image-region-select': begin
		if ptr_valid( event.pointer) eq 0 then goto, done
		if ptr_good( (*pstate).p) eq 0 then goto, done
		if (*(*(*pstate).p)[0]).type eq 1 then goto, done			; not for traverses
		n = (*event.pointer).sel.top
		if (n lt 0) or (n ge n_elements(*(*pstate).p)) then goto, done
		pregion = (*event.pointer).pregion							; pointer to selected region
		if ptr_valid( pregion) eq 0 then goto, done

		file1 = strip_file_ext( (*pregion).region_file)				; name of regions file (blank after "update")
		file2 = strip_file_ext( (*(*(*pstate).p)[0]).file)			; name of regions spectrum file
;   	if file1 eq file2 then begin
		show_one_spectrum, pstate, n
;   	endif
		error = 0

;		Note below that we load all matrix energies (if a XANES series matrix is used. The correct
;		energy matrix is selected in append_DA_fit.

		if (((*pregion).detector eq 0) or ((*pregion).detector eq 1) or ((*pregion).detector eq 7)) and $
					(strlowcase(extract_extension((*pregion).matrix)) ne 'cuts') and $
					((*(*(*pstate).p)[n]).detector ne -1) then begin     ; only for DA PIXE,PIGE

			if ((*pregion).matrix ne '') and ((*pregion).matrix ne (*pstate).matrix_file) then begin
				(*pstate).matrix_file = (*pregion).matrix 
				file = (*pregion).matrix
				ext = extract_extension(file)
				debug = 0
				file = file_requester( /read, /must_exist, filter='*.'+ext, group=event.top, $
							title='Select original '+ext+' file to append fit', file=file, fix_filter=0, $
							path=*(*pstate).path, /translate, updir=3, /skip_if_exists, debug=debug)
				if file ne '' then begin
					matrix = read_da( file, error=error)
					if error eq 0 then begin
						Free_DA, (*pstate).matrix
						(*pstate).matrix_file = (*pregion).matrix
						(*pstate).matrix = ptr_new( matrix, /no_copy)
					endif
				endif
			endif

			if (file1 eq file2) and (error eq 0) then begin
				append_DA_fit, (*(*pstate).p)[n], (*pstate).matrix, pregion, (*pregion).charge, (*(*(*pstate).p)[n]).multiplicity
			endif
		endif
		if ptr_valid((*pstate).p) then begin
			j = current_plot( pstate)
			p = (*((*pstate).p))[j]
			if ptr_valid(p) then begin
				(*(*pstate).pf).pspec = p
				(*(*pstate).pf).pall = (*pstate).p
				notify, 'spectrum-fit', (*pstate).pf, from=event.top
			endif
		endif
		draw_spectra, pstate
		notify, 'spectra-changed', (*pstate).p, from=event.top
		end

	'image-region-delete': begin
		if ptr_good( event.pointer) eq 0 then goto, done
		if ptr_valid((*pstate).p) eq 0 then goto, done
		if ptr_valid((*(*pstate).p)[0]) eq 0 then goto, done
		if (*(*(*pstate).p)[0]).type eq 1 then goto, done			; not for traverses
		Spectrum_Delete_Select, Event, select=(*event.pointer).select, keep_zero=(*event.pointer).keep
		draw_spectra, pstate
		notify, 'spectra-changed', (*pstate).p, from=event.top
;		notify, 'image-region-select', ...
		end

	'image-spectrum-throttle': begin
       if ptr_valid( event.pointer) eq 0 then goto, done
       if ptr_valid( (*pstate).p) eq 0 then goto, done
       if (*(*(*pstate).p)[0]).type eq 1 then goto, done        ; not for traverses
       n = current_plot( pstate)
       pregion = (*event.pointer).pregion
       if ptr_valid( pregion) eq 0 then goto, done

       file1 = strip_file_ext( (*pregion).region_file)
       file2 = strip_file_ext( (*(*(*pstate).p)[0]).file)
       show_one_spectrum, pstate, n

         if (*pregion).matrix ne (*pstate).matrix_file then begin
          file = (*pregion).matrix
          ext = extract_extension(file)
          matrix = read_da( file, error=error)
          if error then begin
              path = *(*pstate).path
              file = path + strip_file_ext( strip_path( (*pregion).matrix)) + '.'+ext
              matrix = read_da( file, error=error)
          endif
    ;;;         if error eq 0 then if matrix.n_pure lt 1 then error=1

          if error then begin
              file = file_requester( /read, /must_exist, filter = '*.'+ext, group=event.top, $
                   			title='Select original '+ext+' file to calc throttle', file=file, fix_filter=0)
              matrix = read_da( file, error=error)
              if error then begin
                 goto, done
              endif
          endif
          if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
          (*pstate).matrix_file = (*pregion).matrix
          (*pstate).matrix = ptr_new( matrix, /no_copy)
         endif

         append_DA_fit, (*(*pstate).p)[n], (*pstate).matrix, pregion, (*pregion).charge, $
         						(*(*(*pstate).p)[n]).multiplicity, /throttle
		 throttle_spectrum, (*(*pstate).p)[n], group=event.top, view=(*pstate).cmark[0:1,3]

       if ptr_valid((*pstate).p) then begin
         j = current_plot( pstate)
         p = (*((*pstate).p))[j]
         if ptr_valid(p) then begin
          (*(*pstate).pf).pspec = p
          (*(*pstate).pf).pall = (*pstate).p
          notify, 'spectrum-fit', (*pstate).pf, from=event.top
         endif
       endif
      draw_spectra, pstate
       notify, 'spectra-changed', (*pstate).p, from=event.top
       end

    'results-select': begin              ; this is passed on to PIXE Fit
       end

    'snapshot': begin
       name = 'spectrum_display.snap'
       on_ioerror, done
       openw, lun, name, /xdr, /get_lun
       geom = widget_info( event.top, /geometry)
       on_ioerror, snap_done
       writeu, lun, geom.xoffset>0, geom.yoffset>0, geom.scr_xsize>400, geom.scr_ysize>100
snap_done:
       close_file, lun
       on_ioerror, null
       end

    'adjust-values': begin ; returned from 'adjust'

       spectrum_background, pstate, event.pointer
       end

	(*pstate).update_notify: begin  ; 'spectrum-display' returned from 'spectrum_select' or parent

		init_spectra, (*pstate).p, (*pstate).width, (*pstate).view, negative=(*pstate).show_negative
		draw_spectra, pstate

		n = n_elements(*(*pstate).p)
		if ptr_valid((*pstate).p) and (n ge 1) then begin
			for i=0L,n-1 do begin
				if ptr_valid((*(*pstate).p)[i]) then (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
			endfor
			j = current_plot( pstate)
			p = (*(*pstate).p)[j]
			if ptr_valid(p) then begin
				(*(*pstate).pf).pspec = p
				(*(*pstate).pf).pall = (*pstate).p
				notify, 'spectrum-fit', (*pstate).pf, from=event.top
			endif
			notify, 'spectra-changed', (*pstate).p, from=event.top		; **** new Nov 19, 2011
		endif
		end

    'fit-display': begin     ; returned from 'pixe_fit' after fit or set-up change

       init_spectra, (*pstate).p, (*pstate).width, (*pstate).view, negative=(*pstate).show_negative
       draw_spectra, pstate
       notify, 'spectra-changed', (*pstate).p, from=event.top
       end

    'new-results': begin     ; returned from 'pixe_fit' after valid fit
       end

    'cut-select': begin         ; returned from 'cut_setup' to draw a cut

		i = (*event.pointer).top
		n = n_elements( *(*pstate).pcut)
		sets = [4,1,4]
		if (i ge 0) and (i lt n) then begin
			p = (*(*pstate).pcut)[i]
			set = sets[ (*p).type]
			if (*pstate).mark_set ne set then begin
				(*pstate).mark_set = set
				widget_control, (*pstate).marker, set_combobox_select=set
			endif else begin
				if (*pstate).pix2_valid eq 0 then begin
					wset,(*pstate).pix2
					device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).pix]
					(*pstate).pix2_valid = 1
				endif
			endelse
			wset, (*pstate).wid2
			device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]
			k = current_plot( pstate)
			if ptr_valid((*pstate).p) eq 0 then goto, done
			ps = (*((*pstate).p))[k]
			if ptr_valid(ps) eq 0 then goto, done
			offset = 0
			if set eq 4 then offset=2
			units = ['EV','KEV','MEV']
			escale = [0.001,1.0,1000.0]
			scale = 1.0
			qc = where( strupcase((*p).units) eq units)
			qs = where( strupcase((*ps).cal.units) eq units)
			if (qc[0] ne -1) and (qs[0] ne -1) then begin
				scale = escale[qc[0]] / escale[qs[0]]
			endif
			for j=offset,offset+(*pstate).nmark[set]-1 do begin
				e = (*p).e[j] * scale
				a = (*ps).cal.poly[1]
				b = (*ps).cal.poly[0]
				c = (e-b)/a
				(*pstate).cmark[j-offset,set] = clip( c, 0, ((*ps).size-1) )
			endfor
			plot_markers, pstate
		endif
		end

    'spectrum-highlight': begin    ; returned from 'spectrum_select' or parent

       (*pstate).highlight_on = (*event.pointer).on
       (*pstate).highlight = (*event.pointer).highlight
       init_spectra, (*pstate).p, (*pstate).width, (*pstate).view, negative=(*pstate).show_negative
       draw_spectra, pstate
       end

    'spectra': begin   ; returned from parent, other spectrum, or image_table

	   if (*pstate).realtime then goto, done
       if ptr_valid( event.pointer) then begin
         if ptr_valid( (*event.pointer)[0]) then begin
          if ptr_valid((*pstate).p) then begin
              if ptr_valid( (*(*pstate).p)[0]) then begin
                 if (*(*(*pstate).p)[0]).orphan eq 1 then begin
                   (*pstate).local = 1
                   (*(*(*pstate).p)[0]).orphan = 0
                 endif
                 if ((*pstate).p ne event.pointer) and (*pstate).local then free_spectra, (*pstate).p
              endif
          endif
          (*pstate).p = event.pointer						; !!!pspec
          *(*pstate).pshow = replicate(1,n_elements(*event.pointer))
		  for i=0L,n_elements(*(*pstate).pshow)-1 do (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
          (*pstate).file = (*(*(*pstate).p)[0]).file
          if (*(*(*pstate).p)[0]).orphan eq 1 then begin
              (*pstate).local = 1
              (*(*(*pstate).p)[0]).orphan = 0
          endif else begin
              (*pstate).local = 0
          endelse
          init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
          draw_spectra, pstate

          (*pstate).vlow = 0
          (*pstate).vhigh = (*pstate).view-1
          widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]

          notify, 'spectra-changed', (*pstate).p, from=event.top
          j = current_plot( pstate)
          p = (*((*pstate).p))[j]
          if ptr_valid(p) then begin
              (*(*pstate).pf).pspec = p
              (*(*pstate).pf).pall = (*pstate).p
              notify, 'spectrum-fit', (*pstate).pf, from=event.top
          endif
         endif
       endif
       end

    'path': begin
       if ptr_valid( event.pointer) eq 0 then goto, done
       *(*pstate).path = (*event.pointer)
	   path = build_output_path( *(*pstate).dpath, *(*pstate).path, (*pstate).root, /set)
       end

    'dpath': begin
       if ptr_valid( event.pointer) eq 0 then goto, done
       *(*pstate).dpath = (*event.pointer)
	   path = build_output_path( *(*pstate).dpath, *(*pstate).path, (*pstate).root, /set)
       end

    'mark-e': begin       ; returned from 'identify'

       if (*pstate).mark_set ne 0 then begin
         (*pstate).mark_set = 0
         widget_control, (*pstate).marker, set_combobox_select=0
         wset, (*pstate).wid2
         device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]
         ey_to_pixel, pstate, (*(event.pointer)).e,0.0, px,py
         (*pstate).xmark[0,0] = px
         plot_markers, pstate
       endif else begin
         if (*pstate).pix2_valid eq 0 then begin
          wset,(*pstate).pix2
          device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).pix]
          (*pstate).pix2_valid = 1
         endif
         wset, (*pstate).wid2
         device,copy=[(*pstate).xmark[(*pstate).id,(*pstate).mark_set],0,1,(*pstate).height, $
                 (*pstate).xmark[(*pstate).id,(*pstate).mark_set],0, (*pstate).pix2]
         ey_to_pixel, pstate, (*(event.pointer)).e,0.0, px,py
         (*pstate).xmark[(*pstate).id,(*pstate).mark_set] = px
         plot_markers, pstate, moving=(*pstate).id
       endelse
       end

    'mark-element': begin  ; returned from 'identify'

       wset, (*pstate).wid2
       device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]
       (*pstate).mark_set = (*pstate).element_set
       widget_control, (*pstate).marker, set_combobox_select=(*pstate).element_set

       z = (*(event.pointer)).z
       e = *((*(event.pointer)).e)
       rel = *((*(event.pointer)).rel)
       shell = *((*(event.pointer)).shell)
       (*pstate).n_lines = n_elements(e) < (*pstate).max_lines
       (*pstate).e[0:(*pstate).n_lines-1] = e[0:(*pstate).n_lines-1]
       (*pstate).rel[0:(*pstate).n_lines-1] = rel[0:(*pstate).n_lines-1]
       (*pstate).shell[0:(*pstate).n_lines-1] = shell[0:(*pstate).n_lines-1]

       plot_element, pstate

       wset, (*pstate).wid1
       polyfill, [0,71,71,0,0],[0,0,14,14,0],/device,color=!p.background
       s = string(z)
       s = strtrim(s,2)
       xyouts,0.01,0.01,/norm, s, color=spec_colour('orange')
       widget_control, (*pstate).help, set_value='Mark element '+element_name(z)+'  Z = '+s
       end

    'cal-ab': begin       ; returned from 'cal'

       j = current_plot( pstate)
       p = (*(*pstate).p)[j]
       new = 0
       if abs(  ((*p).cal.poly[1] - (*event.pointer).poly[1])/(*event.pointer).poly[1]  ) gt 0.2 then new=1
       (*p).cal.units = (*event.pointer).units
       (*p).cal.order = (*event.pointer).order
       (*p).cal.poly = (*event.pointer).poly

       init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, new=new, negative=(*pstate).show_negative
       draw_spectra, pstate

       if (*(*(*pstate).p)[0]).orphan eq 1 then begin
         (*pstate).local = 1
         (*(*(*pstate).p)[0]).orphan = 0
       endif
       notify, 'spectra-changed', (*pstate).p, from=event.top
       end

    'cal-ab-all': begin       ; returned from 'cal'

       j = current_plot( pstate)
       p = (*(*pstate).p)[j]
       new = 0
       if abs(  ((*p).cal.poly[1] - (*event.pointer).poly[1])/(*event.pointer).poly[1]  ) gt 0.2 then new=1

       for i=0L,n_elements(*(*pstate).p)-1 do begin
         p = (*(*pstate).p)[i]
         (*p).cal.units = (*event.pointer).units
         (*p).cal.order = (*event.pointer).order
         (*p).cal.poly = (*event.pointer).poly
       endfor

       init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, new=new, negative=(*pstate).show_negative
       draw_spectra, pstate

       if (*(*(*pstate).p)[0]).orphan eq 1 then begin
         (*pstate).local = 1
         (*(*(*pstate).p)[0]).orphan = 0
       endif
       notify, 'spectra-changed', (*pstate).p, from=event.top
       end

    'cal-ab-ra': begin       ; returned from 'cal' to re-assign peaks to new marked E'

       j = current_plot( pstate)
       p = (*(*pstate).p)[j]
       new = 0
       if abs(  ((*p).cal.poly[1] - (*event.pointer).poly[1])/(*event.pointer).poly[1]  ) gt 0.2 then new=1
	   xlow = (*event.pointer).x[0]						; channel for peak1 in main spectrum
	   xhigh = (*event.pointer).x[1]					; channel for peak2 in main spectrum
	   elow = (*event.pointer).e[0]						; new energy for peak1
	   ehigh = (*event.pointer).e[1]					; new energy for peak2
	   
       e1 = (*p).cal.poly[1] * xlow + (*p).cal.poly[0]
       e2 = (*p).cal.poly[1] * xhigh + (*p).cal.poly[0]

	   if (n_elements(*(*pstate).p) gt 1) and (ehigh gt elow) then begin
	   		for i=0L,n_elements(*(*pstate).p)-1 do begin
				p = (*(*pstate).p)[i]
				if ((*p).cal.poly[1] gt 0.0001) then begin
					x1 = (e1 - (*p).cal.poly[0]) / (*p).cal.poly[1] 
					x2 = (e2 - (*p).cal.poly[0]) / (*p).cal.poly[1]
					a = (ehigh - elow) / (x2 - x1)
					b = elow - a*x1 
					(*p).cal.units = (*event.pointer).units
					(*p).cal.order = 1
					(*p).cal.poly = [b,a]
				endif else begin
					(*p).cal.units = (*event.pointer).units
					(*p).cal.order = (*event.pointer).order
					(*p).cal.poly = (*event.pointer).poly
				endelse
			endfor
	   endif
		
       init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, new=new, negative=(*pstate).show_negative
       draw_spectra, pstate

       if (*(*(*pstate).p)[0]).orphan eq 1 then begin
         (*pstate).local = 1
         (*(*(*pstate).p)[0]).orphan = 0
       endif
       notify, 'spectra-changed', (*pstate).p, from=event.top
       end

    'cal-ab-ra-ab': begin       ; returned from 'cal' to re-assign peaks to new marked E'

       j = current_plot( pstate)
       p = (*(*pstate).p)[j]
       new = 0
       if abs(  ((*p).cal.poly[1] - (*event.pointer).poly[1])/(*event.pointer).poly[1]  ) gt 0.2 then new=1
	   xlow = 0																	; channel for peak1 in main spectrum
	   xhigh = 1000																; channel for peak2 in main spectrum
	   elow = xlow * (*event.pointer).poly[1] + (*event.pointer).poly[0]		; new energy for peak1
	   ehigh = xhigh * (*event.pointer).poly[1] + (*event.pointer).poly[0]		; new energy for peak2
	   
       e1 = (*p).cal.poly[1] * xlow + (*p).cal.poly[0]
       e2 = (*p).cal.poly[1] * xhigh + (*p).cal.poly[0]

	   if (n_elements(*(*pstate).p) gt 1) and (ehigh gt elow) then begin
	   		for i=0L,n_elements(*(*pstate).p)-1 do begin
				p = (*(*pstate).p)[i]
				if ((*p).cal.poly[1] gt 0.0001) then begin
					x1 = (e1 - (*p).cal.poly[0]) / (*p).cal.poly[1] 
					x2 = (e2 - (*p).cal.poly[0]) / (*p).cal.poly[1]
					a = (ehigh - elow) / (x2 - x1)
					b = elow - a*x1 
					(*p).cal.units = (*event.pointer).units
					(*p).cal.order = 1
					(*p).cal.poly = [b,a]
				endif else begin
					(*p).cal.units = (*event.pointer).units
					(*p).cal.order = (*event.pointer).order
					(*p).cal.poly = (*event.pointer).poly
				endelse
			endfor
	   endif
		
       init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, new=new, negative=(*pstate).show_negative
       draw_spectra, pstate

       if (*(*(*pstate).p)[0]).orphan eq 1 then begin
         (*pstate).local = 1
         (*(*(*pstate).p)[0]).orphan = 0
       endif
       notify, 'spectra-changed', (*pstate).p, from=event.top
       end

	'wizard-action': begin
		if ptr_valid( event.pointer) then begin
			if (*event.pointer).window eq 'Spectrum Display' then begin
				case (*event.pointer).command of
					'open-test': begin
;						print,'*** Wizard Spectrum Display: test if window is open ...'
						pw = (*pstate).pwiz
						*pw = *event.pointer
						(*pw).top = event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'import-spectra': begin
						print,'*** Wizard Spectrum Display: import spectra and write ...'
						pw = event.pointer
						pd = (*pw).pdata
						
						err = 0
						pileup='' & throttle='' & linear='' & output='' 
						charge_mode=-1 & flux_pv='' & sensitivity=0.0 & conv=0.0 & charge=0.0
						
						if tag_present('BLOG', *pd) eq 0 then err=1
						if tag_present('OPT', *pd) eq 0 then err=1
						if tag_present('PILEUP', *pd) then pileup = (*pd).pileup
						if tag_present('THROTTLE', *pd) then throttle = (*pd).throttle
						if tag_present('LINEAR', *pd) then linear = (*pd).linear
						if tag_present('OUTPUT', *pd) then *(*pstate).path = (*pd).output
						if tag_present('CHARGE_MODE', *pd) then charge_mode = (*pd).charge_mode
						if tag_present('FLUX_SCALER', *pd) then flux_pv = (*pd).flux_scaler
						if tag_present('SENSITIVITY', *pd) then sensitivity = (*pd).sensitivity
						if tag_present('CONV', *pd) then conv = (*pd).conv
						if tag_present('CHARGE', *pd) then charge = (*pd).charge
						if err eq 0 then begin
							verify = 0
							if tag_present('VERIFY', *pd) then verify=(*pd).verify
							obj = obj_new( (*pd).opt.device_name)
							if obj_valid(obj) eq 0 then err = 1
							
							if err eq 0 then begin
								widget_control, hourglass=1
								flux_select, group_leader=event.top, device=obj, conv=conv, flux_pv=flux_pv, $
										pars=(*pstate).pflux, path=*(*pstate).path, sensitivity=sensitivity, /silent, $
										dpath=extract_path((*pd).blog), evt_file=(*pd).blog, charge_mode=charge_mode
			
								Spectrum_Load_do, pstate, (*pd).blog, '', device=obj, opt=(*pd).opt, group=event.top, throttle=throttle, pileup=pileup, $
										linearize=linearize, sensitivity=sensitivity, Q=charge, verify=verify, file_return=sret, error=err
								widget_control, hourglass=0
								obj_destroy, obj
							endif
						endif
						(*pw).error = err
						if tag_present('CHARGE', *pd) then (*pd).charge = charge
						if tag_present('PNEW', *pd) then begin
							if n_elements(sret) gt 0 then *(*pd).pnew = sret
						endif
						notify, 'wizard-return', pw
						end

					'energy-cal': begin
						print,'*** Wizard Spectrum Display: set ALL energy cals ...'
						pw = event.pointer
						pd = (*pw).pdata
						
						Spectrum_Menu_Get_Cal, Event, (*pd).file, error=err
						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'sum-spectra': begin
						print,'*** Wizard Spectrum Display: sum spectra w/ cal re-map ...'
						pw = event.pointer
						pd = (*pw).pdata
						
						Spectrum_Add, Event, /map_cal
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'save-spectra': begin
						print,'*** Wizard Spectrum Display: save spectra to file ...'
						pw = event.pointer
						pd = (*pw).pdata
						
						Spectrum_Save, Event, (*pd).file
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'sum-selected-spectra': begin
						print,'*** Wizard Spectrum Display: sum selected detector spectra and write ...'
						pw = event.pointer
						pd = (*pw).pdata
						select = (*pd).select
						label = (*pd).label
						file = *(*pd).poutput
						err = 1

						n = get_select( select, error=err)
						p = *(*pstate).p
						np = n_elements(p)
						if err or (n[0] eq -1) or (np lt 1) then begin
							if err or (n[0] eq -1) then warning,'OnNotify_spectrum','"'+label+'" select file not found = '+select
							if (np lt 1) then warning,'OnNotify_spectrum','No spectra loaded to sum.'
							err = 1
						endif else begin
							(*(*pstate).pshow)[*] = 0
							for i=0,np-1 do begin
								if (*p[i]).array and ptr_good((*p[i]).pactive) then begin
									mask = bytarr( 1000)
									mask[ *(*p[i]).pactive] = 1
								endif else begin
									mask = bytarr( 1000)
									mask[ (*p[i]).station + adc_offset_device( (*p[i]).DevObj) ] = 1
								endelse
								q = where( mask[n] eq 1, nq)
								if nq ge 1 then (*(*pstate).pshow)[i] = 1 
							endfor
							err = 0
							good = where( *(*pstate).pshow eq 1, ng)
							if ng eq 0 then begin
								warning,'OnNotify_spectrum',['No detector spectra selected in =',select]
								err = 1
							endif else begin	
								Spectrum_Add, Event, /map_cal, /delete, /skip_uncal
								p = *(*pstate).p
								np = n_elements(p)
								(*(p)[0]).label = label
								write_spec, p, file, error=err
								if err then warning,'OnNotify_spectrum','Failed to write spectrum = '+file

								*(*pstate).pshow = replicate(1, np)	
								for i=0L,np-1 do (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
								
								init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
								draw_spectra, pstate
								notify, 'spectra-changed', (*pstate).p, from=event.top
								j = current_plot( pstate)
								p = (*((*pstate).p))[j]
								if ptr_valid(p) then begin
									(*(*pstate).pf).pspec = p
									(*(*pstate).pf).pall = (*pstate).p
									notify, 'spectrum-fit', (*pstate).pf, from=event.top
								endif
							endelse
						endelse

						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'load-spectra': begin
						print,'*** Wizard Spectrum Display: load spectra ...'
						pw = event.pointer
						nfiles = n_elements( *(*pw).pdata)
						file0 = (*(*pw).pdata)[0]
						p = read_spec( file0, error=err)
						if err or (ptr_good(p[0]) eq 0) then begin
							warning,'OnNotify_spectrum','Failed to read file = '+file0
							err = 1
						endif else begin
							if nfiles ge 2 then begin
								for k=1,nfiles-1 do begin
									file = (*(*pw).pdata)[k]
									p1 = read_spec( file, error=err)
									if err or (ptr_good(p1[0]) eq 0) then begin
										warning,'OnNotify_spectrum','Failed to append file = '+file
										err = 1
									endif else begin
										p = [p,p1]
									endelse
								endfor
							endif
							free_spectra, (*pstate).p
							(*pstate).p = ptr_new(p)
							np = n_elements(p)
							*(*pstate).pshow = replicate(1, np)	
							for i=0L,np-1 do (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
	
							init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
							draw_spectra, pstate
							notify, 'spectra-changed', (*pstate).p, from=event.top
							j = current_plot( pstate)
							p = (*((*pstate).p))[j]
							if ptr_valid(p) then begin
								(*(*pstate).pf).pspec = p
								(*(*pstate).pf).pall = (*pstate).p
								notify, 'spectrum-fit', (*pstate).pf, from=event.top
							endif
							err = 0
						endelse

						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'select-spectra': begin
						print,'*** Wizard Spectrum Display: select detector spectra and write ...'
						pw = event.pointer
						pd = (*pw).pdata
						select = (*pd).select
						file = *(*pd).poutput
						err = 1

						p = *(*pstate).p
						np = n_elements(p)
						n = replicate(-1,np)
						for j=0L,np-1 do begin
							if ptr_good(p[j]) then n[j] = (*p[j]).station + adc_offset_device((*p[j]).DevObj)
						endfor
						n_sel = get_select( select, error=err)
						if err or (n_sel[0] eq -1) then begin
							warning, 'OnNotify_spectrum',['Error reading .select.csv file:',select], /error
							err = 1
						endif else begin
							for j=0,np-1 do begin
								q = where( n[j] eq n_sel, nq)
								if nq eq 0 then n[j]=-1 
							endfor
							q = where( n ge 0, nq)
							if nq eq 0 then begin
								warning,'OnNotify_spectrum',['No detector spectra selected using:',select]
								err = 1
							endif else begin	
								if strlen(file) gt 0 then write_spec, p[q], file
								for j=0L,np-1 do begin
									if n[j] eq -1 then free_spectrum, p[j]
								endfor
								*(*pstate).p = p[q]
								p = *(*pstate).p
								np = n_elements(p)
								*(*pstate).pshow = replicate(1, np)	
								for i=0L,np-1 do (*p[i]).show = (*(*pstate).pshow)[i]
								
								init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
								draw_spectra, pstate
								notify, 'spectra-changed', (*pstate).p, from=event.top
								j = current_plot( pstate)
								p1 = p[j]
								if ptr_valid(p1) then begin
									(*(*pstate).pf).pspec = p1
									(*(*pstate).pf).pall = (*pstate).p
									notify, 'spectrum-fit', (*pstate).pf, from=event.top
								endif
							endelse
							
						endelse
						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'outer-inner-ratio': begin
						print,'*** Wizard Spectrum Display: outer-inner ratio ...'
						pw = event.pointer
						err = 1
						d = ''
						nlast = (n_elements( *(*pstate).presults)-1) > 0
						if nlast lt 1 then begin
							warning,'OnNotify_spectrum','Bad number of fit results found.'
						endif else begin
							pouter = (*(*pstate).presults)[nlast-1]
							pinner = (*(*pstate).presults)[nlast]
							el = (*pw).qual1
							shell_menu = ['','','L','M']
							els = (*pouter).el.name
							el_code, strcompress(els,/remove_all), t,z,shell
							q = where( z gt 0)
							if q[0] ne -1 then els[q] = element_name( z[q] ) + shell_menu[ shell[q]]
							nel = (where( el eq els, nq))[0]
							if nq lt 1 then begin
								warning,'OnNotify_spectrum','Element '+el+' not found in fit results.'
							endif else begin
								d = [(*pouter).area[nel],(*pouter).aerror[nel],(*pinner).area[nel],(*pinner).aerror[nel]]
								err = 0
							endelse
						endelse

						(*pw).error = err
						*(*pw).pdata = d
						notify, 'wizard-return', pw
						end	
					else: begin
						warning,'spectrum_display: Notify',['Unknown wizard command: '+(*event.pointer).command, $
								'Make sure GeoPIXE version is compatible with Wizard.']
					endelse
				endcase
			endif
		endif
		end
    else: begin
;       print,'OnNotify_Spectrum: unknown tag = ',event.tag
       end
endcase

done:
	close_file, lun
	return
end

;----------------------------------------------------------------------------

pro OnRealize_Help, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Legend, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid1 = wid
(*pstate).draw1 = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Spectrum, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
wset,wid
(*pstate).wid2 = wid
(*pstate).draw2 = wWidget

case !version.os_family of
    'MacOS': begin
       (*pstate).dw =              -16
       (*pstate).draw_ysize_off =       79
       (*pstate).scr_xsize_off =     100
       (*pstate).scr_ysize_off =     79
       end
    'unix': begin
       (*pstate).dw =              -16
       (*pstate).draw_ysize_off =       80
       (*pstate).scr_xsize_off =     92
       (*pstate).scr_ysize_off =     65
       end
    else: begin
       (*pstate).dw =              -16
       (*pstate).draw_ysize_off =    87	;79
       (*pstate).scr_xsize_off =     100
       (*pstate).scr_ysize_off =     87	;79
       end
endcase

window, /free, xsize=(*pstate).width*(1.+(*pstate).position[0])+1, $
              ysize=(*pstate).height, /pixmap
(*pstate).pix = !d.window
load_spec_colours
window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix2 = !d.window
load_spec_colours

draw_spectra, pstate
widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]
end

;-----------------------------------------------------------------

pro OnRealize_Marker, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).marker = wWidget
(*pstate).mark_set = 3
widget_control, (*pstate).marker, set_combobox_select=(*pstate).mark_set
end

;-----------------------------------------------------------------

pro OnRealize_Rescale, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).rescale = wWidget
(*pstate).scale_mode = 1
widget_control, (*pstate).rescale, set_combobox_select=(*pstate).scale_mode
end

;-----------------------------------------------------------------
; Droplist Select Item Callback Procedure.
;
;   {WIDGET_combobox, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L }
;
;   INDEX returns the index of the selected item. This can be used to
;       index the array of names originally used to set the widget's
;       value.
;-----------------------------------------------------------------

pro OnSelect_Marker, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = event.index
plot_markers, pstate
case event.index of
    0: begin
       notify, 'identify-line', from=event.top
       end
    5: begin
       notify, 'identify-element', from=event.top
       end
    else:
endcase
end

;-----------------------------------------------------------------

pro OnSelect_Rescale, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).scale_mode = event.index
draw_spectra, pstate

notify, 'spectra-changed', (*pstate).p, from=event.top
end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_Spectrum, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case !version.os_family of
    'MacOS': begin
       draw_trim = 15
       scr_trim = 21
       help_trim = 90
       x_limit = (*pstate).chart ? 460: 570
       y_trim = 0
       y_trim2 = 0
       end
    'unix': begin
       draw_trim = 0
       scr_trim = 30		; 15
       help_trim = 90
       x_limit = (*pstate).chart ? 460: 570
       y_trim = 11
       y_trim2 = 55
       end
    else: begin
       draw_trim = 0
       scr_trim = 21		; 15
       help_trim = 90
       x_limit = (*pstate).chart ? 460: 570
       y_trim = 0
       y_trim2 = 0
       end
endcase

if (event.x eq (*pstate).size_event_x) and (event.y eq (*pstate).size_event_y) then return
(*pstate).size_event_x = event.x
(*pstate).size_event_y = event.y

geom = widget_info( event.top, /geometry)

width   = (*pstate).width
height  =    (event.y - (*pstate).draw_ysize_off - y_trim2) > 205   ; 127
w     =    (event.x - (*pstate).scr_xsize_off) > x_limit
h     =    height + (*pstate).draw_ysize_off - (*pstate).scr_ysize_off

widget_control, (*pstate).draw2, $
         draw_xsize=width+draw_trim, draw_ysize=height+draw_trim, $
         scr_xsize=w+scr_trim, scr_ysize=h+scr_trim

widget_control, (*pstate).draw1, scr_ysize=h+scr_trim, ysize=h+draw_trim-y_trim
(*pstate).height = height
;(*pstate).view = w + (*pstate).dw
(*pstate).view = w + scr_trim

widget_control, (*pstate).help, scr_xsize=w+help_trim

if (*pstate).pix ge 0 then wdelete, (*pstate).pix
window, /free, xsize=(*pstate).width*(1.+(*pstate).position[0])+1, $
              ysize=(*pstate).height, /pixmap
(*pstate).pix = !d.window
load_spec_colours
if (*pstate).pix2 ge 0 then wdelete, (*pstate).pix2
window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix2 = !d.window
load_spec_colours

draw_spectra, pstate
end

;-----------------------------------------------------------------

pro OnTimer_Spectrum, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) lt 1 then goto, done
if ptr_valid(pstate) eq 0 then goto, done
if size(*pstate,/tname) ne 'STRUCT' then goto, done

start_identify = (*pstate).start_identify
start_PIXE = (*pstate).start_PIXE

if start_identify then onButton_Identify, event
if start_PIXE then Spectrum_Fit_Setup, event
return

done:
return
end
;
;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_Spectrum, Event

COMPILE_OPT STRICTARR
widget_control, event.id, get_uvalue=message
if n_elements(message) lt 1 then return
if size(message,/tname) ne 'STRING' then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if event.enter eq 1 then begin
    widget_control, (*pstate).help, set_value=message
endif else begin
    widget_control, (*pstate).help, set_value=(*pstate).file
endelse
end
;
;-----------------------------------------------------------------
; VIEWPORT_EVENTS Callback Procedure.
;
;   {WIDGET_DRAW, ID:0L, TOP:0L, HANDLER:0L, TYPE: 0, X:0, Y:0,
;       PRESS:0B, RELEASE:0B, CLICKS:0}
;
;   TYPE returns a value that describes the type of draw widget
;       interaction that generated an event: 0 - Button Press, 1 -
;       Button Release, 2 - Motion, 3 - Viewport Moved, 4 -
;       Visibility Changed (Expose)
;
;-----------------------------------------------------------------

pro OnViewport_Spectrum, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).vlow = event.x
(*pstate).vhigh = (*pstate).vlow + (*pstate).view-1

if (*pstate).scale_mode ne 0 then draw_spectra, pstate
return
end

;-----------------------------------------------------------------

pro PostCreate_Draw_Base, wWidget, spectrum=p, path=path, plugins=plugins, test=test, $
          start_identify=start_identify, start_pixe=start_pixe, image=pimg, $
          dpath=dpath, realtime=realtime, ppercent=ppercent, update_notify=update_notify, $
          show_negatives=show_negatives, ehigh=ehigh, layout=layout, highlight=highlight, $
          chart=chart, _EXTRA=_VWBExtra_

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
       warning,'PostCreate_draw_base',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=path
if n_elements(test) lt 1 then test=0
if n_elements(chart) lt 1 then chart=0
if n_elements(realtime) lt 1 then realtime=0
if n_elements(show_negatives) lt 1 then show_negatives=0
if n_elements(pimg) lt 1 then pimg=ptr_new()
if n_elements(start_identify) lt 1 then start_identify=0
if n_elements(start_PIXE) lt 1 then start_PIXE=0
if n_elements(update_notify) eq 0 then update_notify='spectrum-display'
if n_elements(ehigh) lt 1 then ehigh = 2000.0
;if n_elements(ehigh) lt 1 then ehigh = (realtime ? float((*p[0]).size) : 200.0)
if n_elements(layout) lt 1 then layout=''
if n_elements(highlight) lt 1 then highlight=0

pos = [0.023,0.07,0.995,0.995]
w = 2400
v = 621
h = 285
off = 1.3
max_h = 303
vlow = 32L
vhigh = long(vlow + v-1)
vyoff = 0

; There are max_set's of normal integer pixel markers, and then a set of floating energy
; element line markers, which uses index 'element_set'. These do not use the xmark array,
; but instead use 'e', 'rel' arrays.

max_set = 5                   		 ; don't include last one, used for elements
number_markers = [1,6,2,2,2,1]       ; keep elements last
element_set = 5
max_markers = 10
max_lines = 50

if n_elements(p) lt 1 then begin
    p=0
    show = 1
    local = 1
endif
if ptr_valid(p[0]) eq 0 then begin
    local = 1
    pp = ptr_new()             		  ; null pointer
    show = 1
endif else begin
    local = 0
    pp = ptr_new(p)              	   ; pointer to spectrum pointer array
	np = n_elements(p)
	show = replicate(1,np)
	if realtime and (np gt 36) then show[16:np-1]=0
    init_spectra, pp, w,v, /new
endelse

loadct,0, bottom=0, ncolors=16       ; to initialize the common needed by load_spec_colours
load_spec_colours

root = ptr_new(/allocate_heap)
path = build_output_path( dpath, path, root, /set)

state = {	p:			pp, $				; pointer to spectrum pointer array
			pshow:		ptr_new(show), $	; pointer to 'show' array
         	file:		'', $				; file name
			path:		ptr_new(path), $	; current path
			dpath:		ptr_new(dpath), $	; current raw data path
			root:		root, $				; file path root
			local:		local, $			; flags that pointer array is local
			matrix:		ptr_new(), $		; pointer DA matrix passed from Image_Table
			matrix_file: '', $				; matrix file name
			pimage:		pimg, $				; pointer to current image
			update_notify: update_notify, $	; notify 'spectrum-display' string
			layout:		layout, $			; detector layout file (if supplied, else blank)

			plugins:	plugins, $			; pointer to list of user plugins
			plugin_menus: 0L, $				; plugin menus base menu widget ID
			plugin_menus_root: 0L, $		; parent widget for plugin menus base ID
			pwiz:		ptr_new(/alloc), $	; pointer for Wizard return
			
			test:		test, $				; enable test features
			realtime:	realtime, $			; realtime mode flag
			chart:		chart, $			; chart mode
			ppercent:	ppercent, $			; ptr to % time (for draw)
			last_time:	systime(/seconds), $ ; last draw time
		 
		 	pe:			ptr_new( {e:0.0, units:''}), $		; pointer to a float for identify-e
			px:			ptr_new( {low:0, high:1}), $		; pointer to markers, for cal
			pv:			ptr_new( {low:0.0, high:0.0}), $	; pointer to view channel range for fit_setup
			pf:			ptr_new( {pspec:ptr_new(), pall:ptr_new()}), $ ; pointer to spectrum-fit pointers

			pevt:		ptr_new(/allocate_heap), $		; pointer to EVT parameters
			pfit:		ptr_new(/allocate_heap), $		; pointer to PIXE_Fit parameters
			pfitg:		ptr_new(/allocate_heap), $		; pointer to PIGE_Fit parameters
			presults:	ptr_new(/allocate_heap), $		; pointer to PIXE_Fit results pointer array
			player:		ptr_new(/allocate_heap), $		; pointer to Layer PIXE setup parameters (passed to fit_setup)
			playerg:	ptr_new(/allocate_heap), $		; pointer to Layer PIGE setup parameters (passed to fit_setup)
			pcut:		ptr_new(/allocate_heap), $		; pointer to Cut_Setup parameters
			pfilter:	ptr_new(/allocate_heap), $		; pointer to Filter_Setup parameters
			pdetector:	ptr_new(/allocate_heap), $		; pointer to Detector_Setup parameters
			pexport:	ptr_new(/allocate_heap), $		; pointer to plot_spectrum_select options
			pselect:	ptr_new(/allocate_heap), $		; pointer to spectrum_select options
			pileup:		ptr_new(), $					; pointer to pileup data
			pflux:		ptr_new(/allocate_heap), $		; pointer to IC, flux details
			pdepth:		ptr_new(/allocate_heap), $		; pointer to depth profile yield calc details
			pregions:	ptr_new(/alloc), $				; pointer to 'image-regions'
			
			tlb:		0L, $				; TLB base ID
			wid1:		0L, $				; draw 1 window id
			wid2:		0L, $				; draw 2 window id
			draw1:		0L, $				; draw 1 widget ID
			draw2:		0L, $				; draw 2 widget ID
			pix:		0L, $				; pixmap window id
			pix2:		0L, $				; pixmap for marker moves
			pix2_valid:	0, $				; flags pix2 up to date
			marker:		0L, $				; marker droplist ID
			help:		0L, $				; help text widget ID
			rescale:	0L, $				; scale mode droplist ID

			element_set: element_set, $		; which marker set is for element lines
			n_lines:	0, $				; number of lines
			max_lines:	max_lines, $		; maximum number of lines
			e:			fltarr(max_lines), $	; element line energies
			rel:		fltarr(max_lines), $	; element line relative intensities
			shell:		intarr(max_lines), $	; shell code (K=0, L=1, M=2)

			max_set:	max_set, $				; number of normal marker sets
			max_markers: max_markers, $			; maximum number of markers
			mark_set:	0, $					; current set of markers
			id:			0, $					; current marker id
			nmark:		number_markers, $		; number of markers in each set
			xmark:		intarr(max_markers,max_set), $   ; pixel x marker position
			cmark:		fltarr(max_markers,max_set), $   ; channel x marker position

			scale_mode:	1, $				; Y scale display mode

			fitfunction: 'error_function', $ ; curve fit function

			tlb_width:	0, $				; tlb scr_xsize (initial)
			tlb_height:	0, $				; tlb scr_ysize (initial)
			spectrum_width: 0, $			; spectrum scr_xsize (initial)
			spectrum_height: 0, $			; spectrum scr_ysize (initial)
			dw:			0, $				; width difference/border
			width:		w, $				; width of pixmap     (pixels)
			height:		h-vyoff, $          ; height of pixmap       (pixels)
			view:		v, $				; X view width      (pixels)
			max_h:		max_h, $			; max scroll height   (pixels)
			off:		off, $				; offset factor
			position:	pos, $				; !p.position to use    (norm)
			xoffset:	pos[0]*w, $			; offset between pixmap,wid2
			vlow:		vlow, $				; viewport low         (pixels)
			vhigh:		vhigh, $			; viewport high       (pixels)
			vyoff:		vyoff, $			; offset for axis copy
			draw_ysize_off: 0, $			; offsets for draw resize
			scr_xsize_off: 0, $
			scr_ysize_off: 0, $
			size_event_x:	0, $			; previous resize X
			size_event_y:	0, $			; previous resize Y

			highlight_on: highlight, $		; hightlight one spectrum only, others grey
			highlight:	0, $				; hightlight this one
			show_negative: show_negatives, $	; show negative counts
			show_errors: 0, $				; force showing error bars in spectra
			show_diff:	0, $				; showing diff between data and first overlay (back) in spectra

			start_identify: start_identify, $ ; start-up attached windows
			start_pixe: start_pixe, $

			log:		(show_negatives eq 0), $	; Y scale log/lin
			ylow:		0.0, $				; Y axis ylow
			yhigh:		100.0, $			; Y axis yhigh
			elow:		0.001, $			; X axis elow
			ehigh:		float(ehigh), $		; Y axis ehigh
			cal_a:		0.0, $				; current spectrum cal
			cal_b:		0.0, $				; (not used anymore?, use cal in spec struct)
			cal_units:	'', $				;
			a:			1., $				; pixel to energy	(e/pixel)
			b:			0., $				;					(energy)
			ya:			1., $				; pixel to counts	(c/pixel)
			yb:			0. }				;					(counts)

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
end

;-----------------------------------------------------------------

pro PostCreate_Spectrum, wWidget, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_draw_view=[(*pstate).vlow,0]
end

;-----------------------------------------------------------------

pro Spectrum_Add, Event, map_cal=map, delete=delete, skip_uncal=skip_uncal

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(map) lt 1 then map=0
if n_elements(delete) lt 1 then delete=1
if n_elements(skip_uncal) lt 1 then skip_uncal=0

if ptr_valid( (*pstate).p) eq 0 then return
p = *((*pstate).p)
if n_elements(p) le 1 then return
if ptr_good( p[0], /struct) eq 0 then return
DevObj = (*p[0]).DevObj
if obj_valid(DevObj) eq 0 then return

;	Deal with simple cases:
;		1.	All spectra are single (no array), from different channels (e.g. i.spec).
;		2.	Spectra may be arrays, but combine unique detectors (different pactive lists)
;		3.	All spectra are single from same detector (e.g. regions from non-array detector)
;		4.	All spectra are array, with same pactive list (e.g. regions from array detector)
;
;		For 3,4 need to add charge, IC, average DT_corr
;		For 1,2 need to incorporate all detectors in the combined pactive list; charge, IC don't change.

n = current_plot( pstate)
ok = intarr(n_elements(p))
ok[*] = 1
dev_off = adc_offset_device( DevObj)

singles_3 = (*p[n]).array eq 0
det_3 = ((*p[n]).station + dev_off) > 0
array_4 = (*p[n]).array eq 1
nm = det_3
if array_4 then nm = nm > max( *(*p[n]).pactive)
 
for j=0L,n_elements(p)-1 do begin
    if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) and (j ne n) then begin
		if singles_3 then begin
			if (*p[j]).array eq 1 then singles_3 = 0
			if det_3 ne ((*p[j]).station + dev_off) then singles_3 = 0
		endif
		if array_4 then begin
			if (*p[j]).array then begin
				if vector_equal( *(*p[n]).pactive, *(*p[j]).pactive) eq 0 then array_4=0
			endif else array_4=0
		endif
		if (*p[j]).array eq 1 then begin
			nm = nm > max(*(*p[j]).pactive)
		endif else begin
			nm = nm > ((*p[j]).station + dev_off)
		endelse
	endif
endfor
mask = bytarr(nm+1)
if (*p[n]).array eq 1 then begin
	mask[ *(*p[n]).pactive] = 1
endif else begin
	mask[ (*p[n]).station + dev_off] = 1
endelse
double_up = 0
print, 'Spectrum_Add: n=',n,' nm=',nm,', singles_3, array_4 = ',singles_3, array_4

data = fltarr( 16*1024L)
siz = (*p[n]).size < 16*1024L
data[0:siz-1] = (*(*p[n]).data)[0:siz-1]
if (*p[n]).has_errors then begin
	errors = fltarr( 16*1024L)
	errors[0:siz-1] = (*(*p[n]).error)[0:siz-1]
endif
if (*p[n]).has_mdl then begin
	mdl = fltarr( 16*1024L)
	mdl[0:siz-1] = (*(*p[n]).mdl)[0:siz-1]
endif
IC = (*p[n]).IC_total
charge = (*p[n]).charge
energy = (*p[n]).energy
processed = (*p[n]).processed
valid = (*p[n]).valid
bad_xy = (*p[n]).bad_xy
clipped = (*p[n]).clipped

use_charge = 1										; weight by charge?
DT = (*p[n]).deadtime_correction * (*p[n]).charge
qt = (*p[n]).charge
dtt = (*p[n]).deadtime_correction * qt
if (*p[n]).charge eq 0. then begin
	if (*p[n]).IC_total gt 0. then begin
		use_charge = 0								; weight by IC count
		charge = (*p[n]).IC_total
		DT = (*p[n]).deadtime_correction * charge
		qt = (*p[n]).IC_total
		dtt = (*p[n]).deadtime_correction * qt
	endif else begin
		warning,'',['Both charge and IC count are zero.', $
					'which makes it difficult to weight DT contrubutions.', $
					'Will use a uniform weighting in this case.']
		use_charge = 2								; uniform weights
		charge = 1.
		DT = (*p[n]).deadtime_correction
		qt = 1.
		dtt = (*p[n]).deadtime_correction 
	endelse
endif

for j=0L,n_elements(p)-1 do begin
    if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) and (j ne n) then begin

;		Need to allow for 'map_spec' returning a longer array (if spectrum moves up).

		pspec = p[j]
		if map then begin
			t = map_spec( (*pspec).data, (*pspec).cal, (*p[n]).cal, /quiet, error=err)
			if err then begin
				if skip_uncal then continue
				warning,'Spectrum_Add',['Spectrum found uncalibrated, label:',(*pspec).label,'Skip this spectrum.', $
						'','If this spectrum is needed, CANCEL, reload, delete or calibrate this spectrum,', $
						'and repeat the spectrum Add.'], cancel=cancel
				if cancel then return
				continue
			endif
			if (*pspec).has_errors then t2 = map_spec( *(*pspec).error, (*pspec).cal, (*p[n]).cal, /quiet, error=err)
			if (*pspec).has_mdl then t3 = map_spec( *(*pspec).mdl, (*pspec).cal, (*p[n]).cal, /quiet, error=err)
		endif else begin
			t = *(*pspec).data
			if (*pspec).has_errors then t2 = *(*pspec).error
			if (*pspec).has_mdl then t3 = *(*pspec).mdl
		endelse

		nd = n_elements(t) < 16*1024L
		data[0:nd-1] = data[0:nd-1] + t[0:nd-1]
		siz = siz > nd
		
		if (*p[j]).has_errors then begin
			errors[0:nd-1] = 1./(1./errors[0:nd-1] + 1./t2[0:nd-1])
		endif
		if (*p[j]).has_mdl then begin
			mdl[0:nd-1] = 1./(1./mdl[0:nd-1] + 1./t3[0:nd-1])
		endif
       
		case use_charge of
			1: w = (*p[j]).charge
			0: w = (*p[j]).IC_total
			2: w = 1.
		endcase

;		For single_3 or array_4 combine charge, IC, DT_corr
;		else, add spectra #'s from separate detectors, update 'pactive', 'multiplicity' and 'array'.

		if singles_3 or array_4 then begin
			DT = DT + (*p[j]).deadtime_correction * w
			charge = charge + w

			energy = energy > (*p[j]).energy 										; these add lines also in routine:
																					; "spectrum_load_spectra_increment"
			processed = processed + (*p[j]).processed 
			valid = valid + (*p[j]).valid 
			bad_xy = bad_xy + (*p[j]).bad_xy 
			clipped = clipped + (*p[j]).clipped 
		endif else begin
			if (*p[j]).array then begin
				q = where( mask[ *(*p[j]).pactive] eq 1, nq)
				if nq gt 0 then double_up = 1
				mask[ *(*p[j]).pactive] = 1
			endif else begin
				if mask[(*p[j]).station + dev_off] eq 1 then double_up = 1
				mask[(*p[j]).station + dev_off] = 1
			endelse
			dtt = dtt + (*p[j]).deadtime_correction * w
			qt = qt + w
		endelse
		
	endif
endfor

done:
	if double_up then begin
		warning,'Spectrum_Add',['Adding spectra for detector arrays, but with different detector lists.', $
					'Some detector numbers were summed twice.', 'Derived concentrations may not be consistent.', '', $
				'Use "Add" for the following cases (for consistent charge, flux, multipicity values):', $
				'1. All spectra are single detectors, from different detector channels (e.g. "import").', $
				'2. Spectra may be arrays, but combine unique detectors (different detector lists).', $
				'3. All spectra are single from same detector (e.g. regions from a single detector).', $
				'4. All spectra are array, with same detector list (e.g. regions from array detector).']
	endif
	
	while (total(data[siz/2:siz-1]) eq 0.) and (siz gt 20) do begin
		siz = siz / 2
	endwhile
	*(*p[n]).data = data[0:siz-1]
	(*p[n]).size = siz
	if (*p[n]).has_errors then begin
		*(*p[n]).error = errors[0:siz-1]
	endif
	if (*p[n]).has_errors then begin
		*(*p[n]).mdl = mdl[0:siz-1]
	endif
	
	if singles_3 or array_4 then begin
		(*p[n]).IC_total = IC
		(*p[n]).deadtime_correction = DT / charge
		(*p[n]).charge = charge
		(*p[n]).energy = energy
		(*p[n]).processed = processed
		(*p[n]).valid = valid
		(*p[n]).bad_xy = bad_xy
		(*p[n]).clipped = clipped
	endif else begin
		(*p[n]).deadtime_correction = dtt / qt
		q = where(mask eq 1, nq)
		if nq ne 0 then begin
			(*p[n]).array = 0
			if nq gt 1 then begin
				if ptr_valid((*p[n]).pactive) eq 0 then (*p[n]).pactive = ptr_new(/allocate)
				*(*p[n]).pactive = q
				(*p[n]).array = 1
			endif
			(*p[n]).multiplicity = nq
			(*p[n]).channel = q[0]
			(*p[n]).station = q[0] - dev_off
		endif else begin
			warning,'Spectrum_Add','No valid detectors?'
		endelse
	endelse
	
	if (*p[n]).n_fit gt 0 then begin
	    for i=0L,(*p[n]).n_fit-1 do begin
	       if ptr_valid( (*p[n]).fit[i]) then free_spectrum, (*p[n]).fit[i]
	    endfor
	(*p[n]).n_fit = 0
	endif

   	if delete then begin
		for j=0L,n_elements(p)-1 do begin
;		    if ((*(*pstate).pshow)[j] or ((*p[j]).size eq 0)) and (j ne n) then begin
			if (j ne n) then begin
				free_spectrum, p[j]
		   		ok[j] = 0
			endif
		endfor
		n = 0
    endif

	q = where( ok eq 1, nq)
	if nq ge 1 then *(*pstate).p = (*(*pstate).p)[q] else *(*pstate).p = ptr_new()

	*(*pstate).pshow = 1
	if nq gt 1 then *(*pstate).pshow = [*(*pstate).pshow,replicate(0,nq-1)]
	for i=0L,nq-1 do (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
	
	init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
	draw_spectra, pstate
	
	p = (*((*pstate).p))[n]
	if ptr_valid(p) then begin
	    (*(*pstate).pf).pspec = p
	    (*(*pstate).pf).pall = (*pstate).p
	    notify, 'spectrum-fit', (*pstate).pf, from=event.top
	endif
	notify, 'spectra-changed', (*pstate).p, from=event.top
	return
end

;-----------------------------------------------------------------

pro Spectrum_Blog_Browser, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

; This loads and runs 'blog_browser.sav' in runtime

restore, geopixe_root+'blog_browser.sav'
blog_browse, group=event.top

end
;
;-----------------------------------------------------------------

pro Spectrum_Clear_Mark, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).xmark[*,*] = 0
(*pstate).cmark[*,*] = 0
(*pstate).id = -1
end

;-----------------------------------------------------------------

pro Spectrum_Compress, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

if (*pstate).highlight_on then begin
	j1 = (*pstate).highlight
	j2 = (*pstate).highlight
endif else begin
	j1 = 0L
	j2 = n_elements(p)-1
endelse

for j=j1,j2 do begin
    if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
       q = where( finite(*(*p[j]).data) eq 0)
       if q[0] ne -1 then (*(*p[j]).data)[q] = 0
       (*(*p[j]).data) = smooth( *(*p[j]).data, 2)
       n = (*p[j]).size/2
       scale = 1.0
       if (*p[j]).type ne 1 then scale=2.0
       *(*p[j]).data = scale * smart_congrid( *(*p[j]).data, n)
       (*p[j]).ecompress = ((*p[j]).ecompress > 1) * 2

       if (*p[j]).has_errors then begin
         q = where( finite(*(*p[j]).error) eq 0)
         if q[0] ne -1 then (*(*p[j]).error)[q] = 0
         (*(*p[j]).error) = smooth( (*(*p[j]).error),2)/sqrt(2.)
         *(*p[j]).error = smart_congrid( *(*p[j]).error, n)
       endif
       if (*p[j]).has_mdl then begin
         q = where( finite(*(*p[j]).mdl) eq 0)
         if q[0] ne -1 then (*(*p[j]).mdl)[q] = 0
         (*(*p[j]).mdl) = smooth( (*(*p[j]).mdl),2)/sqrt(2.)
         *(*p[j]).mdl = smart_congrid( *(*p[j]).mdl, n)
       endif
       (*p[j]).size = n
       (*p[j]).cal.poly[0] = (*p[j]).cal.poly[0] + 0.5 * (*p[j]).cal.poly[1]
       (*p[j]).cal.poly[1] = 2.0 * (*p[j]).cal.poly[1]
    endif
endfor
draw_spectra, pstate

n = current_plot( pstate)
p = (*((*pstate).p))[n]
if ptr_valid(p) then begin
    (*(*pstate).pf).pspec = p
    (*(*pstate).pf).pall = (*pstate).p
    notify, 'spectrum-fit', (*pstate).pf, from=event.top
endif
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Correct_Pileup_Image, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)                           ; pointer to spectra
if ptr_valid( p[0]) eq 0 then goto, done
pimg = (*pstate).pimage                         ; pointer to images
if ptr_valid( pimg) eq 0 then goto, done
popt = (*pimg).options

if (*pimg).mode ne 0 then begin
    warning,'Spectrum_Correct_Pileup_Image','Image Pileup correction only available using a DA matrix image'
    goto, done
endif

if strip_path((*pstate).matrix_file) ne strip_path((*pimg).matrix.file) then begin
    file = (*pimg).matrix.file
    ext = extract_extension(file)
    matrix = read_da( file, error=error)
    if error then begin
       path = *(*pstate).path
       file = path + strip_path( (*pimg).matrix.file)
       matrix = read_da( file, error=error)
       if error then begin
         file = file_requester( /read, /must_exist, filter = '*.'+ext, group=event.top, $
              			title='Select original '+ext+' file for Images', file=file, fix_filter=0)
         matrix = read_da( file, error=error)
         if error then begin
          goto, done
         endif
       endif
    endif
    if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
    (*pstate).matrix_file = file
    (*pstate).matrix = ptr_new( matrix, /no_copy)
endif
if ptr_valid( (*pstate).matrix) eq 0 then begin
    warning,'Spectrum_Correct_Pileup_Image','DA matrix file not found'
    (*pstate).matrix_file = ''
    goto, done
endif
if (*(*pstate).matrix).n_pure eq 0 then begin
    warning,'Spectrum_Correct_Pileup_Image',['Need new DA matrix file with pure element spectra.','Regenerate your DA matrix file.']
    goto, done
endif

j = current_plot( pstate)
pspec = p[j]
if (abs((*pspec).pileup_A4-1.0) lt 0.001) or ((*pspec).pileup_A4 eq 0.0) or  $
              ((*pspec).pileup_ratio lt 1.0e-20) then begin
    warning,'Spectrum_Correct_Pileup_Image',['Spectrum does not have a valid determination of pileup strength','', $
         'Generate a pileup spectrum using the Spectrum Display "Display->Pileup->using Images" menu.', $
         'Then fit the spectrum in PIXE Fit, using the "using Images" pileup mode.']
    goto, done
endif

    pileup_ratio = (*pspec).pileup_ratio * (*pspec).pileup_A4
    siz = (*pspec).size
    ca = (*pspec).cal.poly[1]
    cb = (*pspec).cal.poly[0]

    n_el = (*pimg).n_el
    el = *(*pimg).el
    shells = ['K','L','M']
    e = 0.0
    i = -1
    for k=0L,n_el-1 do begin
       name = el[k]
       n = lenchr(name)
       last = extract(name,n-1,n-1)
       shell = where(last eq shells)
       if shell[0] eq -1 then begin
         shell = 0
       endif else begin
         name = extract(name,0,n-2)
       endelse
       shell = shell[0]+1
       z = atomic_number(name)
       if z gt 0 then begin
         e = [e, e_line( z, major_line( z, shell))]
         i = [i,k]
       endif
    endfor
    if n_elements(i) le 1 then goto, done
    i = i[1:*]
    e = e[1:*]
    y = (*(*pspec).data)[ (e-cb)/ca ]
    q = reverse(sort(y))
    q2 = where( y[q] ge 0.2*y[q[0]] )          ; identify dominant elements for
                                  ; element_select pop-up
    old = intarr(n_el)
    old[i[q[q2]]] = 1
    select = element_select( event.top, el, old_select=old, path=*(*pstate).path)
    iq = where(select eq 1)
    if iq [0] eq -1 then goto, done
    nq = (n_elements(iq) < 6) < (*(*pstate).matrix).n_pure
    if nq lt 1 then goto, done

;   Then take all pair-wise combinations (plus binomial factors).

    ca2 = (*(*pstate).matrix).cal.a
    cb2 = (*(*pstate).matrix).cal.b
    siz2 = n_elements((*(*pstate).matrix).pure[*,0]) < siz
    pileup = fltarr(siz)
    pure1 = fltarr(siz)
    pure2 = fltarr(siz)
;   widget_control, /hour

    nt = nq*(nq+1)/2
    cancel = 0
    progress, tlb=progress_tlb, title='Correct Images for Pile-up'
    kt = 1

    for j1=0,nq-1 do begin
       for j2=0,j1 do begin
         i1 = iq[j1]                       ; indices to images
         i2 = iq[j2]
         im1 = where( el[i1] eq (*(*pstate).matrix).el )   ; indices to matrix rows
         im2 = where( el[i2] eq (*(*pstate).matrix).el )
         im1 = im1[0]
         im2 = im2[0]
         if (im1 eq -1) then warning,'Spectrum_Correct_Pileup_Image','Major element ' + el[i1] + ' is missing from DA matrix.'
         if (im2 eq -1) then warning,'Spectrum_Correct_Pileup_Image','Major element ' + el[i2] + ' is missing from DA matrix.'
         progress, /update, progress_tlb, {unit:0, value:0, current:kt, size:nt}, cancel=cancel
         if cancel then goto, done
         kt = kt+1

         binomial = (i1 eq i2) ? 1.0 : 2.0

;      image1 = (*(*pimg).image)[*,*,i1] > 0.0
;      image2 = (*(*pimg).image)[*,*,i2] > 0.0
         image1 = (*(*pimg).image)[*,*,i1]
         image2 = (*(*pimg).image)[*,*,i2]
         intensity = pileup_ratio * binomial * (image1 * image2) *  $
                 (*(*pstate).matrix).yield[im1] * (*(*pstate).matrix).yield[im2]

         pure1[0:siz2-1] = (*(*pstate).matrix).pure[*,im1]
         if siz gt siz2 then pure1[siz2:siz-1] = 0.0
         pure2[0:siz2-1] = (*(*pstate).matrix).pure[*,im2]
         if siz gt siz2 then pure2[siz2:siz-1] = 0.0

         cross = convol_self( pure1, ca2,cb2, cross=pure2)

         contribute = reform( (*(*pstate).matrix).matrix ## cross[0:siz2-1] )

;      Note: to put these contributions back in images, you need to map matrix index
;      back to image index.

         for k=0L,(*(*pstate).matrix).n_el-1 do begin
          k2 = where( (*(*pstate).matrix).el[k] eq el)
          k2 = k2[0]
          if k2 ne -1 then begin
              (*(*pimg).image)[*,*,k2] = (*(*pimg).image)[*,*,k2] - contribute[k]*intensity
          endif
         endfor
       endfor
    endfor
    progress, /complete, progress_tlb, 'Finished, notify Image ...'

	set_image_minmax, pimg
    notify, 'image-display', from=event.top

done:
    progress, /ending, progress_tlb
end

;-----------------------------------------------------------------

pro Spectrum_Cut_Setup, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 4
widget_control, (*pstate).marker, set_combobox_select=4
plot_markers, pstate

Cut_Setup, group_leader=event.top, path=*(*pstate).path, pcut=(*pstate).pcut, TLB=tlb

register_notify, event.top, $
         ['path', $               ; new path
         'cut-select' $             ; show a cut on spectrum
         ], from=tlb
end
;  
;-----------------------------------------------------------------

pro Spectrum_DA_Load, Event, da_type=da_type

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
       warning,'Spectrum_DA_load',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

if n_elements(da_type) lt 1 then da_type=0
ext = ['*.dam','*.damg']
name = detector_types()

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

F = file_requester( filter='*.dam*', dialog_parent=event.top, fix_filter=0, $
		title='Select DA matrix to Load as Spectra', path=*(*pstate).path)
;		title='Select '+name[da_type]+' DA matrix to Load as Spectra', path=*(*pstate).path)
if F ne '' then begin
    widget_control, /hourglass

    p = read_da_as_spec( F)
    if ptr_valid(p[0]) then begin
       if ptr_valid((*pstate).p) then begin
         if (*(*(*pstate).p)[0]).orphan eq 1 then begin
          (*pstate).local = 1
          (*(*(*pstate).p)[0]).orphan = 0
         endif
         if (*pstate).local and ((*(*pstate).p)[0] ne p[0]) then free_spectra, (*pstate).p
       endif
       (*pstate).p = ptr_new( p)						; !!!pspec
	   *(*pstate).pshow = replicate(1,n_elements(p))
	   for i=0L,n_elements(*(*pstate).pshow)-1 do (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
       (*pstate).local = 1
       (*(*(*pstate).p)[0]).orphan = 0
       (*pstate).file = F
       *(*pstate).path = extract_path( F)

       init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
       draw_spectra, pstate
       (*pstate).vlow = 0
       (*pstate).vhigh = (*pstate).view-1
       widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]
       notify, 'path', (*pstate).path, from=event.top
       notify, 'spectra-changed', (*pstate).p, from=event.top
    endif
endif

done:
end

;-----------------------------------------------------------------

pro Spectrum_Delete_Select, Event, select=sel, keep_zero=keep_zero

; The 'sel' will be region #, so search label for a match

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(sel) eq 0 then return
if n_elements(keep_zero) eq 0 then keep_zero=0

	if ptr_good( (*pstate).p) eq 0 then return
	p = (*pstate).p
	np = n_elements(*p)
	if np eq 0 then return
	if ptr_good( (*p)[0], /struct) eq 0 then return
	mask = intarr(np)
	
	nq = n_elements(sel)
	q = lonarr(nq)

	index = lonarr(np)
	for j=0,np-1 do begin
		label = (*(*p)[j]).label
		s = strsplit( label, ' ,', /extract, count=ns)
		if (ns ge 2) and (s[0] eq 'Region') then begin
			index[j] = fix2(s[1])
		endif
	endfor

	for j=0,nq-1 do begin						; find each 'sel' in index list
		qt = where( sel[j] eq index, nqt)
		q[j] = qt[0]
	endfor
	qt = where( q ge 0, nqt)
	if nqt gt 0 then begin
		q = q[qt]
		nq = nqt
	endif else return

	mask[*] = 1
	mask[q] = 0

;	NOTE: Do not pause execution in debug between the free and the *(*pstate).p
;	as notifies go elsewhere and use this pointer array

	qc = where( mask eq 0, nqc)				; delete these
	if nqc eq 0 then return
	for j=0,nqc-1 do begin
		i = qc[j]
		if keep_zero and (index[i] eq 0) then begin
			mask[j] = 1
			continue
		endif
		if ptr_valid( (*p)[i]) then free_spectrum, (*p)[i]
	endfor

	q = where( mask eq 1, nq)				; keep these
	if nq ge 1 then begin
		if keep_zero then begin				; make #0 the sum of the others
			*(*(*p)[0]).data = 0
			(*(*p)[0]).charge = 0.0
			(*(*p)[0]).IC_total = 0.0
			for j=0,nq-1 do begin
				i = q[j]
				if index[i] eq 0 then continue
				*(*(*p)[0]).data += *(*(*p)[i]).data
				(*(*p)[0]).charge += (*(*p)[i]).charge
				(*(*p)[0]).IC_total += (*(*p)[i]).IC_total
			endfor
		endif

		pq = (*p)[q]
		s = (*(*pstate).pshow)[q]
	endif else begin
		pq = ptr_new()
		s = 0
	endelse

	*(*pstate).p = pq
	*(*pstate).pshow = s
	return
end

;-----------------------------------------------------------------

pro Spectrum_depth_Calc, Event


COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

depth_ratio_yields, pdat=(*pstate).pdepth, group_leader=event.top, path=*(*pstate).path

return
end

;-----------------------------------------------------------------
;
pro Spectrum_Do_Curve_Fit, Event

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
       warning,'Spectrum_do_curve_fit',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
n = current_plot( pstate)
ps = p[n]
if ptr_good( ps) eq 0 then goto, done
pd = (*ps).data
if ptr_good( pd) eq 0 then goto, done

mark = fix( (*pstate).cmark[0:5,1] + 0.5)   		  ; "1" is the X0-X5 marker set
err = check_X0X5_markers(mark)
if err then begin
    warning,'Spectrum_Do_Curve_Fit','Markers bad ['+string(mark)+'].'
    goto, done
endif

y = (*pd)[mark[0]:mark[5]]
x = findgen(mark[5]-mark[0]+1) + mark[0]
x = (*ps).cal.poly[1] * x + (*ps).cal.poly[0]
w = 1./(y > 1.0)
if (*ps).has_errors then begin
	err = (*(*ps).error)[mark[0]:mark[5]]
	w = 1./ ( err^2 > 1.0)
endif
curvefit_initial, (*pstate).fitfunction, pd, ps, mark, a

r = curvefit( x,y,w, a,sigma, chisq=chisq, function_name=(*pstate).fitfunction, iter=iter, tol=1.0e-3)

print,'a=',a
print,'s=',sigma
print,'chisq=',chisq,'  iterations=',iter,' reduced chisq=',chisq/(n_elements(x)-n_elements(a))

x = 0.1 * findgen(10*(mark[5]-mark[0]+1)) + mark[0]
x = (*ps).cal.poly[1] * x + (*ps).cal.poly[0]
call_procedure, (*pstate).fitfunction, x,a,r

null_spectrum = define(/spectrum)
spec = null_spectrum
spec.source = (*ps).source
spec.label = (*pstate).fitfunction + ' CurveFit to ' + (*ps).label
spec.cal.poly[0] = x[0]
spec.cal.poly[1] = (*ps).cal.poly[1] * 0.1	
spec.cal.units = (*ps).cal.units
spec.cal.order = 1
spec.comment = 'CurveFit ' + str_tidy(a[3], places=-2) + ' ' + (*ps).cal.units
(*ps).comment = str_tidy(a[3], places=-2) + ' ' + (*ps).cal.units

spec.size = n_elements(r)
spec.data = ptr_new(r * 0.1, /no_copy)					; to match cal.poly[1] scaling above  @10-22

if ptr_valid( (*ps).fit[0] ) then free_spectrum, (*ps).fit[0]
(*ps).fit[0] = ptr_new(spec, /no_copy)
if (*ps).n_fit lt 1 then (*ps).n_fit = 1
draw_spectra, pstate
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Duplicate, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

if (*pstate).highlight_on then begin
	j1 = (*pstate).highlight
	j2 = (*pstate).highlight
endif else begin
	j1 = 0L
	j2 = n_elements(p)-1
endelse

for j=j1,j2 do begin
    if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
		pj = (*((*pstate).p))[j]
		copy_pointer_data, pj, pdest, /init
		*((*pstate).p) = [*((*pstate).p), pdest]
		*(*pstate).pshow =[*((*pstate).pshow), 1] 
	endif
endfor

draw_spectra, pstate
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Edit_Filters, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

Filter_Setup, group_leader=event.top, path=*(*pstate).path, pars=(*pstate).pfilter, TLB=tlb

register_notify, event.top, $
         ['path' $                 ; new path
         ], from=tlb
end
;
;-----------------------------------------------------------------

pro Spectrum_Edit_Detectors, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

Detector_Setup, group_leader=event.top, path=*(*pstate).path, pars=(*pstate).pdetector, TLB=tlb

register_notify, event.top, $
         ['path' $                 ; new path
         ], from=tlb
end
;
;-----------------------------------------------------------------

pro Spectrum_Error_Function, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

resolve_routine, 'error_function'
(*pstate).fitfunction = 'error_function'
end

;-----------------------------------------------------------------

pro Spectrum_Gauss_Function, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

resolve_routine, 'gauss_function'
(*pstate).fitfunction = 'gauss_function'
end

;-----------------------------------------------------------------

pro Spectrum_EVT, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

EVT, group_leader=event.top, TLB=tlb, pars=(*pstate).pevt, path=*(*pstate).path, /spectra

register_notify, event.top, $
          ['images', $       ; pass on notify of new images loaded
          'path', $     ; new path
          'spectra' $     ; new spectra loaded
          ], from=tlb
end

;-----------------------------------------------------------------

pro Spectrum_Exit, Event

OnKill_Spectrum, event
end

;-----------------------------------------------------------------

pro Spectrum_Export, Event, fits=fits

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(fits) lt 1 then fits=0

file = find_file2( (*pstate).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
file = strip_path( strip_file_ext( (*pstate).file))
if strmid( file, strlen(file)-1,1) eq 'i' then file = strmid(file,0,strlen(file)-1)
    file = file + '.csv'
    filter = '*.csv'
    title = 'Export spectra as CSV ascii'

F = file_requester( /write, filter=filter, path=path, $
          	title=title, file=file, group=event.top, /fix_filter)
if F ne '' then begin
    F = strip_file_ext( F) + '.csv'
    widget_control, /hourglass

    if ptr_valid((*pstate).p) then begin
       if ptr_valid((*(*pstate).p)[0]) then begin
         export_csv, (*pstate).p, F, fits=fits
       endif else goto, nothing
    endif else goto, nothing
endif

done:
    return
nothing:
    warning,'Spectrum_Export','Nothing to export.'
    goto, done
end

;-----------------------------------------------------------------

pro Spectrum_Fit_Setup, Event, gamma=gamma, tweek=tweek

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(gamma) lt 1 then gamma=0
if n_elements(tweek) lt 1 then tweek=1
pspec = 0L
pall = 0L
view = [3.0,38.0]
if ptr_valid((*pstate).p) then begin
    j = current_plot( pstate)
    pspec = (*((*pstate).p))[j]
    pall = (*pstate).p
    cal_ab, (*pspec).cal, a,b,u, error=error
    if error eq 0 then view = [(*pstate).cmark[0,3],(*pstate).cmark[1,3]] * a + b
endif

if gamma then begin
    fit_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).pfitg, path=*(*pstate).path, $
         view=view, pspec=pspec, pall=pall, presults=(*pstate).presults, $
         layer_pars=(*pstate).playerg, /gamma, tweek=tweek, test=(*pstate).test
endif else begin
    fit_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).pfit, path=*(*pstate).path, $
         view=view, pspec=pspec, pall=pall, presults=(*pstate).presults, $
         layer_pars=(*pstate).player, tweek=tweek, test=(*pstate).test
endelse

register_notify, event.top, $
          ['fit-display', $   ; pass on result of fit for display
          'new-results', $    ; means that presults has new data to pass on
          'path' $         ; new path
          ], from=tlb
end

;-----------------------------------------------------------------

pro Spectrum_Fit_Results, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

fit_results, group_leader=event.top, TLB=tlb, path=*(*pstate).path, presults=(*pstate).presults, $
         layer_pars=(*pstate).player

register_notify, event.top, $
          ['path', $            ; new path
          'image-region-select', $	; new Region row selected --> result-properties
          'results-select' $       ; row selected in fit results (pass to fit setup for refit)
          ], from=tlb
end

;-----------------------------------------------------------------

pro Spectrum_Fold, Event, bit12=bit12, bit11=bit11

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(bit12) lt 1 then bit12=0
if n_elements(bit11) lt 1 then bit11=0
if (bit12 eq 0) and (bit11 eq 0) then bit12=1

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done
n = current_plot( pstate)

siz1 = (*p[n]).size
if siz1 gt 4096 and bit12 then begin
	siz2 = siz1/2
	spec = (*(*p[n]).data)[0:siz2-1]
	top = (*(*p[n]).data)[siz2:2*siz2-1]
	ptr_free, (*p[n]).data
	spec = spec + top
	(*p[n]).data = ptr_new( spec[0:siz2-1], /no_copy)
	(*p[n]).size = siz2

	if (*p[n]).has_errors then begin
	    spec = *(*p[n]).error
	    ptr_free, (*p[n]).error
	    spec[0:siz2-1] = 1./(1./(spec[0:siz2-1]) + 1./(spec[siz2:siz1-1]))
	    (*p[n]).error = ptr_new( spec, /no_copy)
	endif
	if (*p[n]).has_mdl then begin
	    spec = *(*p[n]).mdl
	    ptr_free, (*p[n]).mdl
	    spec[0:siz2-1] = 1./(1./(spec[0:siz2-1]) + 1./(spec[siz2:siz1-1]))
	    (*p[n]).mdl = ptr_new( spec, /no_copy)
	endif
	siz1 = siz2
endif
if siz1 gt 2048 and bit11 then begin
	siz2 = siz1/2
	spec = (*(*p[n]).data)[0:siz2-1]
	top = (*(*p[n]).data)[siz2:2*siz2-1]
	ptr_free, (*p[n]).data
	spec = spec + top
	(*p[n]).data = ptr_new( spec[0:siz2-1], /no_copy)
	(*p[n]).size = siz2

	if (*p[n]).has_errors then begin
	    spec = *(*p[n]).error
	    ptr_free, (*p[n]).error
	    spec[0:siz2-1] = 1./(1./(spec[0:siz2-1]) + 1./(spec[siz2:siz1-1]))
	    (*p[n]).error = ptr_new( spec, /no_copy)
	endif
	if (*p[n]).has_mdl then begin
	    spec = *(*p[n]).mdl
	    ptr_free, (*p[n]).mdl
	    spec[0:siz2-1] = 1./(1./(spec[0:siz2-1]) + 1./(spec[siz2:siz1-1]))
	    (*p[n]).mdl = ptr_new( spec, /no_copy)
	endif
endif

if (*p[n]).n_fit gt 0 then begin
    for i=0L,(*p[n]).n_fit-1 do begin
       if ptr_valid( (*p[n]).fit[i]) then ptr_free, (*p[n]).fit[i]
    endfor
(*p[n]).n_fit = 0
endif
init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
draw_spectra, pstate

p = (*((*pstate).p))[n]
if ptr_valid(p) then begin
    (*(*pstate).pf).pspec = p
    (*(*pstate).pf).pall = (*pstate).p
    notify, 'spectrum-fit', (*pstate).pf, from=event.top
endif
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Correct_Throttle, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

F = file_requester( /read, /must_exist, filter = '*.txt', $
			title='Select Throttle file to apply', path=*(*pstate).path, group=event.top, fix_filter=0)
if F[0] eq '' then goto, done

throttle = get_throttle( F[0], do_throttle=do_throttle)
if do_throttle eq 0 then goto, done

for i=0L,n_elements(p)-1 do begin
	(*p[i]).throttle = F[0]
	siz = min( [n_elements(throttle),(*p[i]).size])
	(*(*p[i]).data)[0:siz-1] = (*(*p[i]).data)[0:siz-1] * throttle[0:siz-1]

	if (*p[i]).has_errors then begin
		(*(*p[i]).error)[0:siz-1] = (*(*p[i]).error)[0:siz-1] * sqrt(throttle[0:siz-1])
	endif

	if (*p[i]).n_fit gt 0 then begin
		for j=0L,(*p[i]).n_fit-1 do begin
			if ptr_valid( (*p[i]).fit[j]) then begin
				free_spectrum, (*p[i]).fit[j]
	       		ptr_free, (*p[i]).fit[j]
			endif
		endfor
	(*p[i]).n_fit = 0
	endif
endfor
init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative
draw_spectra, pstate

n = current_plot( pstate)
p = (*((*pstate).p))[n]
if ptr_valid(p) then begin
    (*(*pstate).pf).pspec = p
    (*(*pstate).pf).pall = (*pstate).p
    notify, 'spectrum-fit', (*pstate).pf, from=event.top
endif
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Throttle, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done
n = current_plot( pstate)
if size(p[n],/tname) ne 'POINTER' then goto, done
if size(*p[n],/tname) ne 'STRUCT' then goto, done

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 3
widget_control, (*pstate).marker, set_combobox_select=3
plot_markers, pstate

cal_ab, (*p[n]).cal, a,b,u, error=error
;if error then goto, done
eview = ((*pstate).cmark[0:1,3]*a + b) > 0.32

throttle3_select, p[n], group_leader=event.top, tlb=tlb, view=eview, $
						update_notify=(*pstate).update_notify

register_notify, event.top, $
		[(*pstate).update_notify $		; pass on notify of new spectrum overlay
;		'path', $     					; new path
;		'spectra' $						; new spectra loaded
		], from=tlb

;draw_spectra, pstate

done:
end

;-----------------------------------------------------------------

pro Spectrum_GIF, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

file = strip_file_ext((*pstate).file) + '.gif'
file = file_requester( /write, filter='*.gif', path=*(*pstate).path, $
         file=file, title='Save spectrum as GIF', group=event.top, /fix_filter)
if strlen(file) ge 1 then begin
    wset, (*pstate).pix
    img = tvrd(true=3)
    img = color_quan(img,3,r,g,b,colors=16)

    write_gif, file, img, r,g,b
endif
end

;-----------------------------------------------------------------

pro Spectrum_Image, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

gimage, group_leader=event.top, path=*(*pstate).path, TLB=tlb
register_notify, event.top, $
          ['images', $       ; pass on notify of new images loaded
          'path', $     ; new path
          'spectra' $     ; new spectra loaded
          ], from=tlb
end

;-----------------------------------------------------------------

pro Spectrum_Import_energies, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	select = import_select(title='Select device for XANES energies',/device_only)
	if select.error then return
	obj = obj_new( select.device)
	if obj_valid( obj) eq 0 then return
	
	F = file_requester( /read, title='Select data path to scan for files', path=*(*pstate).dpath, $
									group=event.top, /dir )
	widget_control, /hourglass
	
	p = scan_dir_evt( F[0], obj, ppath=(*pstate).path, proot=(*pstate).root, error=error)
	if error eq 0 then begin
		np = n_elements(p)
		energy = fltarr(np)
		for i=0,np-1 do energy[i] = (*p[i]).energy

		F2 = file_requester( /write, filter='*.csv', fix_filter=0, multiple=0, $
		         title='Select XANES energies file to write', path=*(*pstate).path, group=event.top )
		if F2[0] ne '' then begin
			on_ioerror, bad_file
			openw, lun, F2[0], /get_lun
			printf, lun, '# XANES energies imported from data dir = '+F[0]
			for i=0,n_elements(energy)-1 do begin
				printf, lun, energy[i]
			endfor
			close_file, lun
			return
bad_file:	
			warning,'Spectrum_Import_energies','Failed to write XANES energies file.'
			close_file, lun
		endif
	endif	
	obj_destroy, obj
	return
end

;-----------------------------------------------------------------

pro Spectrum_Import, Event

COMPILE_OPT STRICTARR

select = import_select( event.top)		;, /debug)
if n_elements(select) eq 0 then return

if select.error eq 0 then begin
	spectrum_load, event, opt=select.opt, append=select.append
endif
end

;-----------------------------------------------------------------

pro Spectrum_Load, Event, opt=opt, append=append

; The 'opt' struct contains the details of the desired load.
; 'obj' is a device object defined in 'prep' and passed to 'do' ...

COMPILE_OPT STRICTARR
common c_spectrum_import_obj, import_obj

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

Spectrum_Load_prep, pstate, F1, F2, device=import_obj, opt=opt, group=event.top, throttle=throttle, $
		pileup=pileup, linearize=linearize, sensitivity=sensitivity, append=append
if f1[0] eq '' then return

tic
Spectrum_Load_do, pstate, F1, F2, device=import_obj, opt=opt, group=event.top, throttle=throttle, $
		pileup=pileup, linearize=linearize, sensitivity=sensitivity, append=append

*(*pstate).pregions = 0L
return
end

;-----------------------------------------------------------------

function Spectrum_Load_Plugins, error=error

;	Load any spectrum plugins ...

	COMPILE_OPT STRICTARR
	error = 1
	ErrorNo = 0
	common c_working_dir, geopixe_root
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
	       warning,'Spectrum_Load_Plugins',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return, 0L
	    endif
	endif
	
	add_plugins = 0
	report_name = ''
	plugin_path = geopixe_root+'plugins'+slash()
	plugins = ptr_new()
	plugin_list = find_file2(plugin_path+'*_spectrum_plugin.sav')
	plugin_good = bytarr( n_elements(plugin_list))
	
	if plugin_list[0] eq '' then begin
		plugin_title = ['-- none --']
		print,'Spectrum: no plugins found.'
	endif else begin
		nf = n_elements(plugin_list)
		print,'spectrum:  process ',nf,' plugin files ...'
		plugin_title = strarr(nf)

		for i=0L,nf-1 do begin
			print,'Spectrum: restore plugin: ', plugin_list[i]
			restore, plugin_list[i], /verbose
			plugin_list[i] = strip_path( plugin_list[i])
			plugin_list[i] = strip_file_ext( plugin_list[i])
			report_name = plugin_list[i]

			Catch, ErrorNo
			if (ErrorNo ne 0) then begin
				Catch, /cancel
					warning,'Spectrum_Load_Plugins',['Errors detected in plugin: '+report_name, $
						'Check plugin SAV files.','','Make sure version is less than or', $
						'equal to current IDL session,', $
						'and at least v7.0 for VM mode.']
	
;				if catch_errors_on then begin
;					Catch, ErrorNo
;					if (ErrorNo ne 0) then begin
;						Catch, /cancel
;						on_error, 1
;						help, calls = s
;						n = n_elements(s)
;						c = 'Call stack: '
;						if n gt 2 then c = [c, s[1:n-2]]
;						warning,'Spectrum_Load_Plugins',['IDL run-time error caught.', '', $
;							'Error:  '+strtrim(!error_state.name,2), $
;							!Error_state.msg,'',c,'','Check plugins for errors.'], /error
;						MESSAGE, /RESET
;						goto, poor
;					endif
;				endif else on_error,0
				goto, poor
			endif

			call_procedure, plugin_list[i], title=title
			plugin_good[i] = 1
			Catch, /cancel
			
			if n_elements(title) lt 1 then begin
				plugin_title[i] = 'No "title" return'
			endif else begin
				plugin_title[i] = title
			endelse
			print,'Spectrum: register plugin: ', plugin_title[i]
			continue
poor:
			print,'Spectrum: bad plugin: ', report_name
		endfor
		q = where( plugin_good eq 1, nq)
		if nq eq 0 then return, 0L

		add_plugins = 1		
		plugins = ptr_new( {list:plugin_list[q], title:plugin_title[q]})
		error = 0
	endelse

	if catch_errors_on eq 0 then Catch, /cancel
	return, plugins
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Identify, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 0
widget_control, (*pstate).marker, set_combobox_select=0
plot_markers, pstate
notify, 'identify-line', from=event.top
end

;-----------------------------------------------------------------

pro Spectrum_Mark_X0X5, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 1
widget_control, (*pstate).marker, set_combobox_select=1
plot_markers, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Cal, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 2
widget_control, (*pstate).marker, set_combobox_select=2
plot_markers, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Cut, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 4
widget_control, (*pstate).marker, set_combobox_select=4
plot_markers, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Mark_View, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = 3
widget_control, (*pstate).marker, set_combobox_select=3
plot_markers, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Element, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]

(*pstate).mark_set = (*pstate).element_set
widget_control, (*pstate).marker, set_combobox_select=(*pstate).element_set
plot_markers, pstate
notify, 'identify-element', from=event.top
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Pileup, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done
widget_control, /hour

j = current_plot( pstate)
;for j=0L,n_elements(p)-1 do begin
    ca = (*p[j]).cal.poly[1]
    if (abs(ca-1.0) gt 0.001) and (*(*pstate).pshow)[j] and ((*p[j]).size gt 3) then begin
       cb = (*p[j]).cal.poly[0]
       siz = (*p[j]).size

       pileup = convol_self( *(*p[j]).data, ca,cb, low=siz/100.)

       q = where( (*(*p[j]).data)[0:siz-1] gt 0.5)
       if (q[0] ne -1) and (n_elements(q) gt 10) then begin
         r = smooth(pileup[q],5) / smooth((*(*p[j]).data)[q],5)
         pileup = pileup / max(r)
       endif
       temp = 0

       sfit = define(/spectrum)
       sfit.source = (*p[j]).source
       sfit.label = 'Pileup'
       sfit.cal.poly[0] = cb
       sfit.cal.poly[1] = ca
       sfit.cal.units = (*p[j]).cal.units
       sfit.cal.order = 1
       sfit.comment = 'Calculated pileup spectrum'
       sfit.size = n_elements(pileup)

       sfit.data = ptr_new( pileup, /no_copy)

       if (*p[j]).n_fit gt 0 then begin
         for i=0L,(*p[j]).n_fit-1 do begin
          pf = (*p[j]).fit[i]
          if (*pf).label eq 'Pileup' then begin
              ptr_free, pf
              (*p[j]).fit[i] = ptr_new( sfit, /no_copy)
              goto, next
          endif
         endfor
         (*p[j]).fit[(*p[j]).n_fit] = ptr_new( sfit, /no_copy)
         (*p[j]).n_fit = (*p[j]).n_fit + 1
next:
       endif else begin
         (*p[j]).fit[0] = ptr_new( sfit, /no_copy)
         (*p[j]).n_fit = 1
       endelse
    endif
;endfor
draw_spectra, pstate

done:
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Pileup_Image, Event

; Determine a pileup spectrum based on products of images, pixel-by-pixel,
; and using the 'pure' element spectra.

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	if ptr_valid( (*pstate).p) eq 0 then goto, done
	p = *((*pstate).p)									; pointer to spectra
	if ptr_valid( p[0]) eq 0 then goto, done
	pimg = (*pstate).pimage								; pointer to images
	if ptr_valid( pimg) eq 0 then goto, done
	pregions = (*pstate).pregions						; pointer to regions, if valid (else 0L)

	if (*pimg).mode ne 0 then begin
		warning,'Spectrum_Mark_Pileup_Image','Image Pileup calculation only available using a DA matrix image'
		goto, done
	endif

	if strip_path((*pstate).matrix_file) ne strip_path((*pimg).matrix.file) then begin
		file = (*pimg).matrix.file
		ext = extract_extension(file)
		matrix = read_da( file, error=error)
		if error then begin
			path = *(*pstate).path
			file = path + strip_path( (*pimg).matrix.file)
			matrix = read_da( file, error=error)
			if error then begin
				file = file_requester( /read, /must_exist, filter = '*.'+ext, group=event.top, $
							title='Select original '+ext+' file to use', file=file, fix_filter=0)
				matrix = read_da( file, error=error)
				if error then begin
					goto, done
				endif
			endif
		endif
		if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
		(*pstate).matrix_file = file
		(*pstate).matrix = ptr_new( matrix, /no_copy)
	endif
	if ptr_valid( (*pstate).matrix) eq 0 then begin
		warning,'Spectrum_Mark_Pileup_Image','DA matrix file not found'
		(*pstate).matrix_file = ''
		goto, done
	endif
	if (*(*pstate).matrix).n_pure eq 0 then begin
		warning,'Spectrum_Mark_Pileup_Image',['Need new DA matrix file with pure element spectra.','Regenerate your DA matrix file.']
		goto, done
	endif
	if ptr_valid( (*pregions)[0] ) then begin
		print,'Spectrum_Mark_Pileup_Image: Matching regions found. Use region q.'
		good = 1
	endif else begin
		print,'Spectrum_Mark_Pileup_Image: Matching regions NOT found. Use full images.'
		nxy = long((*pimg).xsize) * long((*pimg).ysize)
		good = 0
	endelse

    j = current_plot( pstate)
    ca = (*p[j]).cal.poly[1]
    if (abs(ca-1.0) lt 0.1) or ((*p[j]).size lt 3) then goto, done
    cb = (*p[j]).cal.poly[0]
    siz = (*p[j]).size

;	Find any connected region #, to focus on these selected pixels only ...

	if good then begin
		s = strsplit( (*p[j]).label, ' ,', /extract, count=ns)
		if (s[0] eq 'Region') then begin
			spec_index = fix2(s[1])
			n_regions = n_elements(*pregions)
			index = intarr( n_regions)
			for k=0,n_regions-1 do index[k] = (*(*pregions)[k]).index
			q = where( spec_index eq index, nq)
			if nq ge 1 then begin
				ireg = q[0]
			endif else good=0
		endif else good=0
	endif

;	Find major line energies for element (and 'elastic') and sort these to select
;	the main elements to pre-select.

    n_el = (*pimg).n_el
    el = *(*pimg).el
	nxy = long((*pimg).xsize) * long((*pimg).ysize)
    shells = ['K','L','M']
    e = 0.0
    i = -1
    for k=0L,n_el-1 do begin
		name = el[k]
		n = lenchr(name)
		last = extract(name,n-1,n-1)
		shell = where(last eq shells)
		if shell[0] eq -1 then begin
			shell = 0
		endif else begin
			name = extract(name,0,n-2)
		endelse
		shell = shell[0]+1
		z = atomic_number(name)
		if z gt 0 then begin
			e = [e, e_line( z, major_line( z, shell))]
			i = [i,k]
		endif else if (name eq 'elastic') or (name eq 'Compton') then begin
			q1 = reverse( sort( (*(*pstate).matrix).pure[*,k]))
			e = [e, cb + ca*q1[0]]
			i = [i,k]
		endif
    endfor
    if n_elements(i) le 1 then goto, done
    i = i[1:*]
    e = e[1:*]
    y = (*(*p[j]).data)[ (e-cb)/ca ]
    q = reverse(sort(y))
    q2 = where( y[q] ge 0.05*y[q[0]] )

    old = intarr(n_el)
    old[i[q[q2]]] = 1
	if good then title='Region model:' else title='Whole map model:'
    select = element_select( event.top, el, old_select=old, path=*(*pstate).path, $
					title=title+' Strong elements (<10)')
    iq = where(select eq 1)
    if iq [0] eq -1 then goto, done
    nq = (n_elements(iq) < 10) < (*(*pstate).matrix).n_pure
    if nq lt 1 then goto, done

;   Then take all pair-wise combinations of images (plus binomial factors).
;	Matching spectra based on pair-wise convolution of 'pure' spectra.

    ca2 = (*(*pstate).matrix).cal.a
    cb2 = (*(*pstate).matrix).cal.b

	ca2 = ca2 * (1. - 0.2/100.)			; fudge for pileup deficit (need this back from Fit-Setup)
	cb2 = cb2 * (1. - 0.2/100.)			; this value corrected/assumed in 'pixe_fit' to apply real sum_deficit

    siz2 = n_elements((*(*pstate).matrix).pure[*,0]) < siz
    pileup = fltarr(siz)
    pileup2 = fltarr(siz)
    pure1 = fltarr(siz)
    pure2 = fltarr(siz)
;   widget_control, /hour
    nt = nq*(nq+1)/2
    cancel = 0
    progress, tlb=progress_tlb, title='Calculate Image Pile-up Spectrum'
    kt = 1

	for j1=0,nq-1 do begin
		for j2=0,j1 do begin
			i1 = iq[j1]                   							; indices to images
			i2 = iq[j2]
			im1 = where( el[i1] eq (*(*pstate).matrix).el )		; indices to matrix rows
			im2 = where( el[i2] eq (*(*pstate).matrix).el )
			im1 = im1[0]
			im2 = im2[0]
			if (im1 eq -1) then warning,'Spectrum_Mark_Pileup_Image','Major element ' + el[i1] + ' is missing from DA matrix.'
			if (im2 eq -1) then warning,'Spectrum_Mark_Pileup_Image','Major element ' + el[i2] + ' is missing from DA matrix.'
			progress, /update, progress_tlb, {unit:0, value:0, current:kt, size:nt}, cancel=cancel
			if cancel then goto, done
			kt = kt+1

        	if (im1 ge 0) and (im2 ge 0) then begin
				binomial = (i1 eq i2) ? 1.0 : 2.0

				if good then begin
					pq = (*(*(*pstate).pregions)[ireg]).q
				endif else begin
					pq = ptr_new(lindgen(nxy))
				endelse
				image1 = (*(*pimg).image)[*pq + nxy*i1] > 0.0			; need to select 'q' for regions here
				image2 = (*(*pimg).image)[*pq + nxy*i2] > 0.0
				intensity = total( image1 * image2) *  $
						(*(*pstate).matrix).yield[im1] *  (*(*pstate).matrix).yield[im2]

    			print,'    Pileup: ',el[i1],' + ',el[i2],'  intensity=',intensity

				pure1[0:siz2-1] = (*(*pstate).matrix).pure[0:siz2-1,im1]
				if siz gt siz2 then pure1[siz2:siz-1] = 0.0
				pure2[0:siz2-1] = (*(*pstate).matrix).pure[0:siz2-1,im2]
				if siz gt siz2 then pure2[siz2:siz-1] = 0.0

    			cross = binomial * intensity *  convol_self( pure1, ca2,cb2, cross=pure2)
				pileup = pileup + cross
			endif
		endfor
    endfor
	if good eq 0 then ptr_free, pq

    progress, /complete, progress_tlb, 'Finished, overlay pile-up spectrum ...'

;	Remap pileup onto spectrum E cal, which accounts for eneregy deficit.

	pileup2 = map_spec( pileup, {order:1,poly:[cb2,ca2],units:'keV'}, {order:1,poly:[cb,ca],units:'keV'}, error=err)

;   Now rescale pileup2 to match spectrum ...

    q = where( (*(*p[j]).data)[0:siz-1] gt 0.5)
    if (q[0] ne -1) and (n_elements(q) gt 10) then begin
       r = smooth(pileup2[q],3) / smooth((*(*p[j]).data)[q],3)
       pu_factor = 1.0 / max(r)
       pileup2 = pileup2 * pu_factor
    endif
    temp = 0
    print,'    Inferred pileup_ratio = ',pu_factor

    sfit = define(/spectrum)
    sfit.source = (*p[j]).source
    sfit.label = 'Image Pileup'
    sfit.cal.poly[0] = cb
    sfit.cal.poly[1] = ca
    sfit.cal.units = (*p[j]).cal.units
    sfit.cal.order = 1
    sfit.comment = 'Pileup spectrum from Images'
    sfit.size = n_elements(pileup2)
    sfit.pileup_ratio = pu_factor
    (*p[j]).pileup_ratio = pu_factor
    sfit.pileup_A4 = 1.0
    (*p[j]).pileup_A4 = 1.0

    sfit.data = ptr_new( pileup2, /no_copy)

    if (*p[j]).n_fit gt 0 then begin
       for i=0L,(*p[j]).n_fit-1 do begin
         pf = (*p[j]).fit[i]
         if (*pf).label eq 'Image Pileup' then begin
          ptr_free, pf
          (*p[j]).fit[i] = ptr_new( sfit, /no_copy)
          goto, next
         endif
       endfor
       (*p[j]).fit[(*p[j]).n_fit] = ptr_new( sfit, /no_copy)
       (*p[j]).n_fit = (*p[j]).n_fit + 1
next:
    endif else begin
       (*p[j]).fit[0] = ptr_new( sfit, /no_copy)
       (*p[j]).n_fit = 1
    endelse

    draw_spectra, pstate

done:
    progress, /ending, progress_tlb
end

;-----------------------------------------------------------------

pro Spectrum_Mark_Escapes, Event, si=si

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(si) lt 1 then si=0
if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

if si then begin
    detector = make_detector('Si')
endif else begin
    detector = make_detector('Ge')
endelse

widget_control, /hour
for j=0L,n_elements(p)-1 do begin
    ca = (*p[j]).cal.poly[1]
    if (abs(ca-1.0) gt 0.1) and (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
       cb = (*p[j]).cal.poly[0]
       siz = (*p[j]).size < 8191

       escapes = fltarr(siz)
       e = indgen(siz) * ca + cb

       esc_energy = escape_energy( detector)
       x = (e - esc_energy - cb) / ca
       q = where( (x gt 0) and (x lt siz) and (e lt 70.0))
       if q[0] ne -1 then begin
         escapes[x[q]] = (*(*p[j]).data)[q] * escape_fraction( detector, e[q])
       endif

       esc_energy_beta = escape_energy( detector, /beta)
       if esc_energy_beta gt 0.1 then begin
         x = (e - esc_energy_beta - cb) / ca
         q = where( (x gt 0) and (x lt siz) and (e lt 70.0))
         if q[0] ne -1 then begin
          escapes[x[q]] = escapes[x[q]] + (*(*p[j]).data)[q] * escape_fraction( detector, e[q], /beta)
         endif
       endif

       sfit = define(/spectrum)
       sfit.source = (*p[j]).source
       sfit.label = 'Escapes'
       sfit.cal.poly[0] = cb
       sfit.cal.poly[1] = ca
       sfit.cal.units = (*p[j]).cal.units
       sfit.cal.order = 1
       sfit.comment = 'Calculated escape spectrum'
       sfit.size = siz

       sfit.data = ptr_new( escapes, /no_copy)

       if (*p[j]).n_fit gt 0 then begin
         for i=0L,(*p[j]).n_fit-1 do begin
          pf = (*p[j]).fit[i]
          if (*pf).label eq 'Escapes' then begin
              ptr_free, pf
              (*p[j]).fit[i] = ptr_new( sfit, /no_copy)
              goto, next
          endif
         endfor
         (*p[j]).fit[(*p[j]).n_fit] = ptr_new( sfit, /no_copy)
         (*p[j]).n_fit = (*p[j]).n_fit + 1
next:
       endif else begin
         (*p[j]).fit[0] = ptr_new( sfit, /no_copy)
         (*p[j]).n_fit = 1
       endelse
    endif
endfor
draw_spectra, pstate

done:
end

;-----------------------------------------------------------------

pro Spectrum_Median, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

if (*pstate).highlight_on then begin
	j1 = (*pstate).highlight
	j2 = (*pstate).highlight
endif else begin
	j1 = 0L
	j2 = n_elements(p)-1
endelse

for j=j1,j2 do begin
	if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
		(*(*p[j]).data) = median( (*(*p[j]).data), 3)
	
;		if (*p[j]).has_errors then begin
;			(*(*p[j]).error) = smooth( (*(*p[j]).error),2)/sqrt(2.)
;		endif
	endif
endfor
draw_spectra, pstate

n = current_plot( pstate)
p = (*((*pstate).p))[n]
if ptr_valid(p) then begin
    (*(*pstate).pf).pspec = p
    (*(*pstate).pf).pall = (*pstate).p
    notify, 'spectrum-fit', (*pstate).pf, from=event.top
endif
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Menu_Cal, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

cal, group_leader=event.top, xlow=(*pstate).cmark[0,2], xhigh=(*pstate).cmark[1,2], path=*(*pstate).path, TLB=tlb

register_notify, event.top, [ $
          'cal-ab', $            ; new cal A,B coefficients
          'cal-ab-RA', $         ; set cal A,B coefficients to "re-assign" peaks to new E
          'cal-ab-RA-AB', $      ; set cal A,B coefficients to "re-assign" peaks based on new A,B for first
          'cal-ab-all', $        ; new cal A,B coefficients (for ALL spectra)
          'path'], $             ; new path
           from=tlb

(*pstate).mark_set = 2
widget_control, (*pstate).marker, set_combobox_select=2
wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]
plot_markers, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Menu_History, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

spectrum_history, group_leader=event.top, TLB=tlb, pspectra=(*pstate).p, show=current_plot( pstate), $
          path=*(*pstate).path

;register_notify, event.top, $
;          ['images', $             ; new images loaded
;          'path' $               ; new path
;          ], from=tlb

return
end

;-----------------------------------------------------------------

pro Spectrum_Menu_Get_Cal, Event, F, error=err

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

err = 0
if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

if (*pstate).realtime then begin
	notify, 'spectrum-get-cals', from=event.top						; tell maia-launch to copy *pm Cals to spec
	notify, 'spectra-changed', (*pstate).p, from=event.top			; tell spectrum-select to update Cals in table
	return
endif

path = *(*pstate).path
if n_elements(F) eq 0 then begin
	F = file_requester( /read, filter = '*.spec', $
			/must_exist, path=path, group=event.top, $
			title='Load spectrum calibration from a SPEC file', fix_filter=0)
endif 
if F[0] ne '' then begin
	print,'Get Cals from SPEC file="'+F[0]+'"'
	if *(*pstate).path eq '' then *(*pstate).path = extract_path(F[0])
	pp = read_spec(F[0])
	if size(pp[0],/tname) eq 'POINTER' then begin
		np = n_elements(p)
		npp = n_elements(pp)
		ts = intarr(npp)
		ise = ts
		ist = ise
		for k=0L,npp-1 do begin
			ts[k] = (*pp[k]).station-1	
			s = strsplit( (*pp[k]).label, ' ', /extract)			; split string, only look at last field
			n = n_elements(s)
			ise[k] = (locate('/X',s[n-1]) eq -1) and (locate('/Y',s[n-1]) eq -1)		; could be /E
			ist[k] = (locate('/X',s[n-1]) eq -1) and (locate('/Y',s[n-1]) eq -1) and (locate('/T',s[n-1]) ne -1)	; is /T
		endfor
		for i=0L,np-1 do begin
			if ptr_valid(p[i]) then begin
				s2 = strsplit( (*p[i]).label, ' ', /extract)		; split string, only look at last field
				n2 = n_elements(s2)
				if (locate('/X',s2[n2-1]) eq -1) and (locate('/Y',s2[n2-1]) eq -1) then begin
					ft = (locate('/T',s2[n2-1]) ne -1)
					j = ((*p[i]).station-1) > 0
					q = where( (ts eq j) and (((ise eq 1) and (ist ne 1) and (ft ne 1)) or ((ist eq 1) and (ft eq 1))) )
					if q[0] ne -1 then begin
						cal_ab, (*pp[q[0]]).cal, ca,cb,cu
						(*p[i]).cal.poly[1] = ca > 0.0001
						(*p[i]).cal.poly[0] = cb
						(*p[i]).cal.order = 1
						(*p[i]).cal.units = cu
    ;           		(*p[i]).ecompress = (*pp[q[0]]).ecompress > 1
					endif
				endif
			endif
		endfor
	endif
	for k=0L,n_elements(pp)-1 do begin
		free_spectrum, pp[k]
	endfor

	draw_spectra, pstate
	notify, 'spectra-changed', (*pstate).p, from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Spectrum_Menu_Get_Cal_image, Event, F, error=err

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

err = 0
if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done
np = n_elements(p)

if (*pstate).realtime then begin
	return
endif

path = *(*pstate).path
if n_elements(F) eq 0 then begin
	F = file_requester( /read, filter = '*.dai', $
			/must_exist, path=path, group=event.top, $
			title='Load spectrum calibration from a DAI file', fix_filter=0)
endif 
if F[0] ne '' then begin
	print,'Get Cals from DAI file="'+F[0]+'"'
	pp = read_geopixe_image(F[0], /header)
	if size(pp,/tname) eq 'POINTER' then begin
		if (*pp).array eq 0 then return

		active = *(*pp).pactive
		cal = *(*pp).pcal
		nactive = n_elements(active)
		np = n_elements(p)

		for i=0,np-1 do begin
			channel = (*p[i]).station + adc_offset_device( (*p[i]).DevObj)
			q = where( active eq channel, nq)
			if nq eq 0 then continue

			cal_ab, cal[q[0]], ca,cb,cu
			(*p[i]).cal.poly[1] = ca > 0.0001
			(*p[i]).cal.poly[0] = cb
			(*p[i]).cal.order = 1
			(*p[i]).cal.units = cu
		endfor
	endif
	free_images, pp

	draw_spectra, pstate
	notify, 'spectra-changed', (*pstate).p, from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Spectrum_Menu_Clear_Cal, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	if ptr_valid( (*pstate).p) eq 0 then goto, done
	p = *((*pstate).p)
	if ptr_valid( p[0]) eq 0 then goto, done
	np = n_elements(p)
	
	if size(p[0],/tname) eq 'POINTER' then begin
		for i=0L,np-1 do begin
			if ptr_valid(p[i]) then begin
				if (locate('/X',(*p[i]).label) eq -1) and (locate('/Y',(*p[i]).label) eq -1) then begin
					(*p[i]).cal.poly[1] = 1.0
					(*p[i]).cal.poly[0] = 0.0
					(*p[i]).cal.order = 1
					(*p[i]).cal.units = 'channel'
				endif
			endif
		endfor
	endif
    draw_spectra, pstate
    notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Menu_Identify, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

identify2, group_leader=event.top, TLB=tlb
register_notify, event.top, ['mark-e','mark-element'], from=tlb

(*pstate).mark_set = 0
widget_control, (*pstate).marker, set_combobox_select=0
wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height,0,0,(*pstate).pix]
plot_markers, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Menu_Background, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

adjust, group_leader=event.top, TLB=tlb
register_notify, event.top, ['adjust-values'], from=tlb
end

;-----------------------------------------------------------------

pro Spectrum_Plot_Group, Event, cgm=cgm, wmf=wmf, white=white, separate=separate, png=png, jpeg=jpeg

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(cgm) eq 0 then cgm=0
if n_elements(wmf) eq 0 then wmf=0
if n_elements(png) eq 0 then png=0
if n_elements(jpeg) eq 0 then jpeg=0
if n_elements(white) eq 0 then white=0
if n_elements(separate) eq 0 then separate=0
if ptr_valid( (*pstate).p) eq 0 then goto, done

spec_names = ''
for i=0L,n_elements(*(*pstate).p)-1 do begin
    spec_names = [spec_names,(*(*(*pstate).p)[i]).label]
endfor
if n_elements(spec_names) gt 1 then spec_names=spec_names[1:*]

select = plot_spectrum_select( event.top, spec_names, cgm=cgm, wmf=wmf, png=png, jpeg=jpeg, $
       old_select=*(*pstate).pexport, path=*(*pstate).path, spec_pstate=pstate )
if select.error then goto, done

qselect = where(select.enable eq 1)
if qselect[0] eq -1 then goto, done

name = 'Plot Spectra to '
if select.plot.type eq 'CGM' then begin
    file = strip_file_ext((*pstate).file) + '.cgm'
    file = file_requester( /write, filter='*.cgm', path=*(*pstate).path, $
         file=file, title=name+'CGM', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done

    file = strip_file_ext(file) + '.cgm'
    plot_spectra, pstate, /cgm, file=file, options=select.plot, select=select.enable

endif else if select.plot.type eq 'METAFILE' then begin
    file = strip_file_ext((*pstate).file) + '.wmf'
    file = file_requester( /write, filter='*.wmf', path=*(*pstate).path, $
         file=file, title=name+'WMF', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done

    file = strip_file_ext(file) + '.wmf'
    plot_spectra, pstate, /wmf, file=file, options=select.plot, select=select.enable

endif else if select.plot.type eq 'PNG' then begin
    file = strip_file_ext((*pstate).file) + '.png'
    file = file_requester( /write, filter='*.png', path=*(*pstate).path, $
         file=file, title=name+'PNG', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done

    file = strip_file_ext(file) + '.png'
    plot_spectra, pstate, /png, file=file, options=select.plot, select=select.enable

endif else if select.plot.type eq 'JPEG' then begin
    file = strip_file_ext((*pstate).file) + '.jpg'
    file = file_requester( /write, filter='*.jpg', path=*(*pstate).path, $
         file=file, title=name+'JPEG', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done

    file = strip_file_ext(file) + '.jpg'
    plot_spectra, pstate, /jpeg, file=file, options=select.plot, select=select.enable

endif else if select.plot.type eq 'PS' then begin
    file = strip_file_ext((*pstate).file) + '.eps'
    file = file_requester( /write, filter='*.eps', path=*(*pstate).path, $
         file=file, title=name+'EPS', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done

    file = strip_file_ext(file) + '.eps'
    plot_spectra, pstate, /eps, file=file, options=select.plot, select=select.enable

endif else begin

    plot_spectra, pstate, options=select.plot, select=select.enable
endelse

*(*pstate).pexport = select

done:
end

;-----------------------------------------------------------------

pro Spectrum_Print, Event

end

;-----------------------------------------------------------------

pro Spectrum_Preferences, Event

end

;-----------------------------------------------------------------

pro Spectrum_process_plugin, event

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
       warning,'Spectrum_Process_Plugin',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugin source code.'], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

; Name of routine to execute

widget_control, event.id, get_uvalue=routine
if n_elements(routine) lt 1 then goto, done

; Does title have "*" as first character indicating that it wants to process
; a range of spectra

multi = 0
widget_control, event.id, get_value=val
s = strmid(val,0,1)
if s eq '*' then multi=1

r = routine_info( routine, /parameters)
;pointer_display, r

if r.num_kw_args gt 0 then begin
	q = where( r.kw_args eq 'FIRST', nq)
	first_OK = nq gt 0 
	first = 1
endif else first_OK = 0

mark = (*pstate).cmark            			; pass in ALL marker sets
if multi then begin
   (*p[0]).group = event.top
	call_procedure, routine, p, 0, mark, history=history

	*((*pstate).p) = p						; in case plugin changes spectra array
											; such as combining and reducing number
	*(*pstate).pshow = replicate(1,n_elements(p))
endif else begin
	if (*pstate).highlight_on then begin
		(*p[(*pstate).highlight]).group = event.top
		call_procedure, routine, p, (*pstate).highlight, mark, history=history
	endif else begin
		for j=0L,n_elements(p)-1 do begin
		    if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
			   (*p[j]).group = event.top
		
				if first_ok then begin
					call_procedure, routine, p, j, mark, history=history, first=first
				endif else begin
					call_procedure, routine, p, j, mark, history=history
				endelse
		    endif
		endfor
	endelse
endelse

;	history records not handled yet ...
;	(need to change history in 'define')

notify, 'spectra-changed', (*pstate).p, from=event.top
(*pstate).cmark = mark
draw_spectra, pstate

done:
end

;-----------------------------------------------------------------

pro Spectrum_Plot, Event

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
       warning,'Spectrum_plot',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
j = current_plot( pstate)
p = (*(*pstate).p)[j]

sz = 1.6
!p.charsize=sz
load_spec_colours
xwin = !d.name

if n_elements(cgm) lt 1 then cgm = 0
if n_elements(ps) lt 1 then ps = 0

if cgm then begin
    set_device, 'CGM', file='spectrum.cgm'
    used_printer = 1
endif else if ps then begin
    set_device, 'PS', file='spectrum.ps'
    used_printer = 1
endif else begin
    if new_dialog_printersetup() then begin
       used_printer = 1
    endif else begin
       set_device, xwin
       used_printer = 0
       if !d.name eq xwin then begin
         window,0, xsize=900, ysize=640
       endif
    endelse
endelse

default_plot, thick, athick, csize, cthick, thick_scale=1.2

elow = 1.0
ehigh = 30.
wlo = 0.11
whi = 0.95
base = 0.11
top = 0.92

if (!d.name eq 'CGM') then begin
    wlo = 0.15
    whi = 0.95
    base = 0.20
    top = 0.80
endif

;----------------------------------------------------------------------------

h = top-base

!p.title = (*p).file
erase
polyfill,[wlo,whi,whi,wlo,wlo],[top-h,top-h,top,top,top-h],/norm,color=!p.background

plot_spec, p, elow,ehigh,window=[wlo,top-h,whi,top],/ylog,/noerase

plots,[wlo,whi,whi,wlo,wlo],[top-h,top-h,top,top,top-h],/norm,thick=athick

;x = 0.99*(!x.window[1]-!x.window[0])+!x.window[0]
;y = 0.94*(!y.window[1]-!y.window[0])+!y.window[0]
;xyouts,x,y, (*(*(*pstate).p)[0]).file, charthick=cthick*0.7, alignment=1.0,/norm,charsize=csize*0.8

;----------------------------------------------------------------------------

done:
    if (!d.name ne xwin) then begin
       device,/close
       set_plot, xwin
    endif

    !p.charsize=1.0
end

;-----------------------------------------------------------------

pro Spectrum_Reload_Plugins, Event

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
       warning,'Spectrum_Reload_Plugins',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	
	plugins = Spectrum_Load_Plugins( error=error)
	if error eq 0 then begin
		if ptr_valid( (*pstate).plugins) then ptr_free, (*pstate).plugins
		(*pstate).plugins = plugins

		widget_control, (*pstate).plugin_menus, /destroy
		plugin_menus = Widget_Button( (*pstate).plugin_menus_root, UNAME='W_MENU_33', /menu, VALUE='User Plugins' )		
		(*pstate).plugin_menus = plugin_menus

		for i=0L,n_elements((*plugins).title)-1 do begin
			W_MENU = Widget_Button((*pstate).plugin_menus, UNAME='Plugin', VALUE=(*plugins).title[i], $
						uvalue=(*plugins).list[i] )
		endfor
	endif
	return
end

;-----------------------------------------------------------------

pro Spectrum_Save, Event, F, trav=trav

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
       warning,'Spectrum_save',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

if n_elements(trav) lt 1 then trav=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

file = find_file2( (*pstate).file)
if file eq '' then file=(*(*(*pstate).p)[0]).file
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
file = strip_path( strip_file_ext( file))
if strmid( file, strlen(file)-1,1) eq 'i' then file = strmid(file,0,strlen(file)-1)
if trav then begin
    file = file + '.trav'
    filter = '*.trav'
    title = 'Save as Traverse TRAV file'
endif else begin
    file = file + '.spec'
    filter = '*.spec'
    title = 'Save as spectrum SPEC file'
endelse
spectrum_preview

if n_elements(F) eq 0 then begin
	F = file_requester( /write, filter=filter, path=path, /image, preview_routine='spectrum_preview', $
				title=title, file=file, dialog_parent=event.top, /fix_filter)
endif
if F ne '' then begin
    widget_control, /hourglass
	(*pstate).file = F[0]
	*(*pstate).path = extract_path(F[0])
	
    if ptr_valid((*pstate).p) then begin
       if ptr_valid((*(*pstate).p)[0]) then begin
         write_spec, *(*pstate).p, F, trav=trav
       endif else goto, nothing
    endif else goto, nothing
endif

done:
    return
nothing:
    warning,'Spectrum_Save','Nothing to save!'
    goto, done
end

;-----------------------------------------------------------------

pro Spectrum_Show_Negatives, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).show_negative = (1 - (*pstate).show_negative)
draw_spectra, pstate
end

;-----------------------------------------------------------------

pro Spectrum_Smooth, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, done

if (*pstate).highlight_on then begin
	j1 = (*pstate).highlight
	j2 = (*pstate).highlight
endif else begin
	j1 = 0L
	j2 = n_elements(p)-1
endelse

for j=j1,j2 do begin
	if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
		(*(*p[j]).data) = smooth( (*(*p[j]).data), 2)

		if (*p[j]).has_errors then begin
			(*(*p[j]).error) = smooth( (*(*p[j]).error),2)/sqrt(2.)
		endif
	endif
endfor
draw_spectra, pstate

n = current_plot( pstate)
p = (*((*pstate).p))[n]
if ptr_valid(p) then begin
    (*(*pstate).pf).pspec = p
    (*(*pstate).pf).pall = (*pstate).p
    notify, 'spectrum-fit', (*pstate).pf, from=event.top
endif
notify, 'spectra-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Spectrum_Time_Amp, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

time_amp, path=(*pstate).path, group=event.top, tlb=tlb

register_notify, event.top, $
         ['time-amp-pileup' $     ; pileup results table
         ], from=tlb

done:
end

;-----------------------------------------------------------------

pro Spectrum_Yield_Calc, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

layer_setup, group_leader=event.top, TLB=tlb, path=*(*pstate).path

end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;

pro spectrum_display_eventcb
end
