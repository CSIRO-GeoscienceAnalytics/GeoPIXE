pro OnButton_Spectrum, Event

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
		warning,'OnButton_spectrum',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

possible = ['DOWN','UP','MOTION','SCROLL']
if event.type gt 2 then begin
	print,'OnButton_Spectrum: Found illegal button type'
	return
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case possible[ event.type] of
	'DOWN': begin
		if (*pstate).mark_set ge (*pstate).max_set then goto, finish
		t = 0
		show_now = 0
		if event.x ge max( (*pstate).xmark[*,(*pstate).mark_set]) then begin
			t=1
		endif else if event.x le min( (*pstate).xmark[*,(*pstate).mark_set]) then begin
			t=-1
		endif
		if t eq 1 then begin
			i1 = (*pstate).nmark[(*pstate).mark_set]-1
			i2 = 0
			i3 = -1
		endif else if t eq -1 then begin
			i1 = 0
			i2 = (*pstate).nmark[(*pstate).mark_set]-1
			i3 = 1
		endif else begin
;			if event.x gt (*pstate).width/2 then begin
			if event.x gt 0.5*float((*pstate).vlow + (*pstate).vhigh) then begin		; new test
				i1 = 0
				i2 = (*pstate).nmark[(*pstate).mark_set]-1					; reversed these
				i3 = 1
			endif else begin
				i1 = (*pstate).nmark[(*pstate).mark_set]-1
				i2 = 0
				i3 = -1
			endelse
		endelse

;		Check for near to a marker ...

		(*pstate).id = -1
		for i=i1,i2,i3 do begin
			if near( event.x, (*pstate).xmark[i,(*pstate).mark_set]) then begin
				(*pstate).id = i
				goto, down_1									; near to a marker
			endif
		endfor
		
;		Check for near to data in a spectrum ...

;		help, event,/str
		if ptr_good( (*pstate).p) then begin
			n = n_elements( *(*pstate).p)
			if n ge 1 then begin
				dy = replicate(100000000L,n)
				off = [-2,-1,0,1,2]
				for i=0,n-1 do begin
					p = (*(*pstate).p)[i]
					if (*p).show then begin
						pixel_to_exy, pstate, event.x,event.y, e, ch, counts, select=i
						dy[i] = min( abs((*(*p).data)[ ((ch+off) > 0) < ((*p).size-1)] - counts))
					endif
				endfor
				q = sort( abs(dy))
				delta = (*pstate).log ? counts*0.1 : ((*pstate).yhigh-(*pstate).ylow)*0.02 
				if dy[q[0]] lt delta then begin
					p = (*(*pstate).p)[q[0]]
					pixel_to_exy, pstate, event.x,event.y, e, ch, counts, select=q[0]
					print, 'x,y=', event.x,event.y,', n: ',q[0],',  e,ch,counts,dy=', e,ch,counts, dy[q[0]]
	
					if (*pstate).highlight_on then begin
						(*pstate).highlight = q[0]
						draw_spectra, pstate
						print,'onbutton_spectrum: highlight/select detector =',q[0]
						*(*pstate).pselect = q[0]
						notify, 'select-highlight', (*pstate).pselect, from=event.top
						return
					endif
				endif
			endif
		endif
		
;		Else, set a new marker position ...

		show_now = 1
		if t eq 1 then begin									; above all markers
			(*pstate).id = (*pstate).nmark[(*pstate).mark_set]-1
			goto, down_1
		endif else if t eq -1 then begin						; below all markers
			(*pstate).id = 0
			goto, down_1
		endif else begin
			if event.x gt 0.5*float((*pstate).vlow + (*pstate).vhigh) then begin			; next marker to right
				for i=0L,(*pstate).nmark[(*pstate).mark_set]-1 do begin
					if (*pstate).xmark[i,(*pstate).mark_set] gt event.x then begin
						(*pstate).id = i
						goto, down_1
					endif
				endfor
			endif else begin								; next marker to left
				for i=(*pstate).nmark[(*pstate).mark_set]-1,0,-1	 do begin
					if (*pstate).xmark[i,(*pstate).mark_set] lt event.x then begin
						(*pstate).id = i
						goto, down_1
					endif
				endfor
			endelse
		endelse
		(*pstate).id = 0											; default marker 0
down_1:
		wset,(*pstate).pix2
		device,copy=[(*pstate).xoffset,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).pix]
		plot_markers, pstate, veto=(*pstate).id
		(*pstate).pix2_valid = 1

		if show_now then begin
			wset, (*pstate).wid2
			device,copy=[(*pstate).xmark[(*pstate).id,(*pstate).mark_set],0,1,(*pstate).height, $
					(*pstate).xmark[(*pstate).id,(*pstate).mark_set],0, (*pstate).pix2]
			(*pstate).xmark[(*pstate).id,(*pstate).mark_set] = event.x
			plot_markers, pstate, moving=(*pstate).id
			pixel_to_exy, pstate, event.x,event.y, e,ch,counts
			(*pstate).cmark[(*pstate).id,(*pstate).mark_set] = ch

			case (*pstate).mark_set of
				0: begin								; ident
					(*(*pstate).pe).e = e
					(*(*pstate).pe).units = (*pstate).cal_units
					notify, 'identify-e', (*pstate).pe, from=event.top
					end
				2: begin								; cal
					(*(*pstate).px).low = (*pstate).cmark[0,(*pstate).mark_set]
					(*(*pstate).px).high = (*pstate).cmark[1,(*pstate).mark_set]
					notify, 'cal-x', (*pstate).px, from=event.top
					end
				3: begin								; view
					(*(*pstate).pv).low = (*pstate).cmark[0,(*pstate).mark_set]
					(*(*pstate).pv).high = (*pstate).cmark[1,(*pstate).mark_set]
					notify, 'spectrum-view', (*pstate).pv, from=event.top
					end
				else:
			endcase

			wset, (*pstate).wid1
			polyfill, [0,71,71,0,0],[0,0,14,14,0],/device,color=!p.background
			s = str_tidy(e)
			s2 = str_tidy(ch)
			xyouts,0.01,0.01,/norm, s, color=spec_colour('orange')
			widget_control, (*pstate).help, set_value=strcompress('Marker '+string((*pstate).id)+': Screen X,Y = '+string(event.x)+ $
					','+string(event.y)+'  channel = '+s2+'  E = '+s+'  Counts = '+string(counts))
		endif
		widget_control, event.id, draw_motion_events=1
		end

	'MOTION': begin
		if event.x ne (*pstate).xmark[(*pstate).id,(*pstate).mark_set] then begin
			if (*pstate).id gt 0 then begin
				if event.x lt (*pstate).xmark[(*pstate).id-1,(*pstate).mark_set]+2 then begin
					goto, finish
				endif
			endif
			if (*pstate).id lt (*pstate).nmark[(*pstate).mark_set]-1 then begin
				if event.x gt (*pstate).xmark[(*pstate).id+1,(*pstate).mark_set]-2 then begin
					goto, finish
				endif
			endif
			wset, (*pstate).wid2
			device,copy=[(*pstate).xmark[(*pstate).id,(*pstate).mark_set],0,1,(*pstate).height, $
					(*pstate).xmark[(*pstate).id,(*pstate).mark_set],0, (*pstate).pix2]
			(*pstate).xmark[(*pstate).id,(*pstate).mark_set] = event.x
			plot_markers, pstate, moving=(*pstate).id
			pixel_to_exy, pstate, event.x,event.y, e,ch,counts
			(*pstate).cmark[(*pstate).id,(*pstate).mark_set] = ch

			case (*pstate).mark_set of
				0: begin								; ident
					(*(*pstate).pe).e = e
					(*(*pstate).pe).units = (*pstate).cal_units
					notify, 'identify-e', (*pstate).pe, from=event.top
					end
				2: begin								; cal
					(*(*pstate).px).low = (*pstate).cmark[0,(*pstate).mark_set]
					(*(*pstate).px).high = (*pstate).cmark[1,(*pstate).mark_set]
					notify, 'cal-x', (*pstate).px, from=event.top
					end
				3: begin								; view
					(*(*pstate).pv).low = (*pstate).cmark[0,(*pstate).mark_set]
					(*(*pstate).pv).high = (*pstate).cmark[1,(*pstate).mark_set]
					notify, 'spectrum-view', (*pstate).pv, from=event.top
					end
				else:
			endcase

			wset, (*pstate).wid1
			polyfill, [0,71,71,0,0],[0,0,14,14,0],/device,color=!p.background
			s = str_tidy(e)
			s2 = str_tidy(ch)
			xyouts,0.01,0.01,/norm, s, color=spec_colour('orange')
			widget_control, (*pstate).help, set_value='Marker '+string((*pstate).id)+': Screen X,Y = '+string(event.x)+ $
					','+string(event.y)+'  channel = '+s2+'  E = '+s+'  Counts = '+string(counts)
		endif
		end

	'UP': begin
		widget_control, event.id, draw_motion_events=0
		end
endcase

finish:
return
end
