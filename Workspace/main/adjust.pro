
pro adjust_event, event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
;			'identify-e': begin
;				if ptr_valid( event.pointer) then begin
;					n = bin_search( *(event.pointer), -1,-1)
;					g = widget_info( (*pstate).list,/geometry)
;					nt = n-(g.ysize/2)+1 > 0
;					widget_control, (*pstate).list, set_list_select=n, $
;								set_list_top=nt
;					*((*pstate).pe) = *(event.pointer)
;				endif
;				goto, finish
;				end
			else:
		endcase
		end
	'WIDGET_TRACKING': begin
;		widget_control, event.id, get_uvalue=s
;		if size(s,/tname) eq 'STRING' then begin
;			if event.enter eq 1 then begin
;				widget_control, (*pstate).help, set_value=s
;			endif else begin
;				widget_control, (*pstate).help, set_value=' '
;			endelse
;		endif
;		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill Adjusr ...'
;		cancel_notify, event.top
		widget_control, event.top, /destroy
;		ptr_free, (*pstate).pe
;		heap_gc, /verbose
		return
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of
	'slide1': begin
		(*(*pstate).p).value1 = event.value
		notify, 'adjust-values', (*pstate).p, from=event.top
		end
	'slide2': begin
		(*(*pstate).p).value2 = event.value
		notify, 'adjust-values', (*pstate).p, from=event.top
		end
	'slide3': begin
		(*(*pstate).p).value3 = event.value
		notify, 'adjust-values', (*pstate).p, from=event.top
		end
	'slide4': begin
		(*(*pstate).p).value4 = event.value
		notify, 'adjust-values', (*pstate).p, from=event.top
		end
	'slide5': begin
		(*(*pstate).p).value5 = event.value
		notify, 'adjust-values', (*pstate).p, from=event.top
		end
	else:
endcase

finish:
return
end

;-----------------------------------------------------------


pro adjust, group_leader=group, TLB=tlb, title=title, _extra=extra, value=ival

if n_elements(group) lt 1 then group=0
if n_elements(title) lt 1 then title='Adjust parameters'
if n_elements(ival) lt 1 then begin
	ival = { value1:300.0, value2:3000.0, value3:0.5, value4:10.0, value5:0.05 }
endif

if !version.os_family eq 'MacOS' then begin
	fnt = 'COURIER*BOLD*10'
endif else begin
	fnt = 'COURIER*10'
endelse

tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='Adjust_TLB')
base1 = widget_base( tlb, /column, xpad=0, ypad=0, space=0)

slide1 = cw_fslider2( base1, format='(f6.1)', minimum=0.1, maximum=1500.0, $
				value=300.0, uname='slide1')

slide2 = cw_fslider2( base1, format='(f7.1)', minimum=1.0, maximum=300000.0, $
				value=3000.0, uname='slide2')

slide3 = cw_fslider2( base1, format='(f6.3)', minimum=0.01, maximum=1.0, $
				value=0.5, uname='slide3')

slide4 = cw_fslider2( base1, format='(f6.1)', minimum=0.1, maximum=2000.0, $
				value=10.0, uname='slide4')

slide5 = cw_fslider2( base1, format='(f6.3)', minimum=0.01, maximum=0.2, $
				value=0.05, uname='slide5')

state = { $
			p:		ptr_new( ival ) $	; pointer to values
		}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

;register_notify, tlb, 'adjust-something?', from=group

xmanager, 'adjust', tlb, /no_block

return
end
