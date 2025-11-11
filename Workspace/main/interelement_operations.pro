pro interelement_operations_event, Event

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
       warning,'interelement_operations_event',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
;	   goto, kill
       return
    endif
endif

	widget_control, hourglass=0
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	
	if ptr_good(pstate,/struct) eq 0 then goto, bad_state
	p = (*pstate).p
	changed = 0
	operations = define(/operations)

	case tag_names( event,/structure) of
		'NOTIFY': begin
			case event.tag of
				'images-changed': begin						; returned from other apps
					(*pstate).p = event.pointer				; pointer to other 'image'
					p = (*pstate).p
					if ptr_good(p,/struct) eq 0 then goto, bad_ptr

					widget_control, (*pstate).source, set_value=*(*p).el, set_combobox_select=(*p).modify.source
					widget_control, (*pstate).target, set_value=*(*p).el, set_combobox_select=(*p).modify.target
					widget_control, (*pstate).operation, set_combobox_select=(*p).modify.operation
					widget_control, (*pstate).filter, set_combobox_select=(*p).modify.filter
					widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale,places=2)
					widget_control, (*pstate).strength, set_value=(*p).modify.strength
					(*pstate).last_filter = -1
					goto, update_source
					end
				else:
			endcase
			goto, finish
			end
		'WIDGET_TRACKING': begin
			widget_control, event.id, get_uvalue=s
			if size(s,/tname) eq 'STRING' then begin
				if event.enter eq 1 then begin
					widget_control, (*pstate).help, set_value=s
				endif else begin
;					widget_control, (*pstate).help, set_value='?.
				endelse
			endif
			goto, finish
			end
		'WIDGET_KILL_REQUEST': begin
         	goto, kill
			end
		else:
	endcase

	if ptr_good(p,/struct) eq 0 then goto, bad_ptr
	nx = (*p).xsize
	ny = (*p).ysize
	nxy = long(nx) * long(ny)

	uname = widget_info( event.id, /uname)

	case uname of
		'source': begin
			(*p).modify.source = event.index
			(*pstate).last_filter = -1
			goto, update_source
			end
		'filter': begin
			(*p).modify.filter = event.index
			goto, update_source
			end
		'operation': begin
			(*p).modify.operation = event.index
			op = operations[(*p).modify.operation]		
			case op of
				'Subtract': begin												; subtract
					(*p).modify.scale = 0.0001
					end
				'Add': begin													; add
					(*p).modify.scale = 1.0
					end
				'Multiply': begin												; multiply
					(*p).modify.scale = 1.0
					end
				'Divide': begin													; divide
					(*p).modify.scale = 1.0
					end
				'Correct Y step': begin											; Correct Y step
					(*p).modify.scale = 1.0
					end
				else:
			endcase
			(*p).modify.strength = 1.0
			widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale)
			widget_control, (*pstate).strength, set_value=(*p).modify.strength
			goto, update_display
			end
		'target': begin
			(*p).modify.target = event.index
			goto, update_display
			end
		'scale': begin
			widget_control, (*pstate).scale, get_value=s
			(*p).modify.scale = float2(s)
			goto, update_display
			end
		'strength': begin
			widget_control, (*pstate).scale, get_value=s
			(*p).modify.scale = float2(s)
			(*p).modify.strength = event.value
			goto, update_display
			end
		'auto-button': begin
;			if (*p).modify.operation ne 1 then begin
;				warning,'interelement_operations_event','"Auto" scale setting only works for "subtract" for now.'
;				return
;			endif
			if ptr_good((*p).modify.image) eq 0 then begin
				*(*p).modify.image = (*(*p).image)[*,*,(*p).modify.source]
				interelement_filter, p, error=err
				(*pstate).last_filter = (*p).modify.filter
			endif
			op = operations[(*p).modify.operation]		
			case op of
				'Add': begin													; add
					(*p).modify.scale = mean((*(*p).image)[*,*,(*p).modify.target])/mean(*(*p).modify.image)
					widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale)
					(*p).modify.strength = 1.0
					widget_control, (*pstate).strength, set_value=(*p).modify.strength
					goto, update_display
					end
				'Multiply': begin												; multiply
					(*p).modify.scale = 1/mean(*(*p).modify.image)
					widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale)
					(*p).modify.strength = 1.0
					widget_control, (*pstate).strength, set_value=(*p).modify.strength
					goto, update_display
					end
				'Divide': begin													; divide
					(*p).modify.scale = 1/mean(*(*p).modify.image)
					widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale)
					(*p).modify.strength = 1.0
					widget_control, (*pstate).strength, set_value=(*p).modify.strength
					goto, update_display
					end
				'Correct Y step': begin											; Correct Y step
					(*p).modify.scale = 1/mean(*(*p).modify.image)
					widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale)
					(*p).modify.strength = 1.0
					widget_control, (*pstate).strength, set_value=(*p).modify.strength
					goto, update_display
					end
				else:
			endcase

;			img1 is (filtered) source image
;			img2 is target image
;			only consider significant source pixels - above 50% of source maximum

			img1 = median( *(*p).modify.image, 3)
;			img1 = *(*p).modify.image
			img2 = median( (*(*p).image)[*,*,(*p).modify.target], 3)
			small1 = 0.5 * max( img1)
			small2 = 0.02 * max( img2)
			q = where( (img1 gt small1) and (img2 gt small2), nq)
			if nq eq 0 then goto, finish

;			Now form cummulative histogram of ratio img1/img2, and look
;			for the ratio at which we have accumulated 5% of nq pixels.
;			The 'x' location of that point ignores 5% of low ratio pixels.

			temp = img1
			temp[*] = 0
			temp[q] = img2[q] / img1[q]
			h = histogram( temp[q], binsize=0.002*max(temp[q]), locations=x)
			cdf = total( h, /cumulative) / n_elements(temp[q])
			q2 = where( cdf le 0.05, nq2)
			f = x[q2[nq2-1]]

;			wset,1
;			plot,x,h
;			wset,0
;			plot,x[*],cdf[*],yrange=[0,0.1]

			(*p).modify.scale = f
			(*p).modify.strength = 1.0
			widget_control, (*pstate).scale, set_value=str_tidy((*p).modify.scale)
			widget_control, (*pstate).strength, set_value=(*p).modify.strength
			goto, update_display
			end
		'apply-button': begin
			widget_control, (*pstate).scale, get_value=s
			(*p).modify.scale = float2(s)

;			Note: format of history (e.g. "()") is assumes elsewhere for image processing Get function
;				e.g. "Inter-Element: Subtract Ni (Median filter 3) * 0.04"

			img = interelement_transform( p, error=err)
			if err eq 0 then begin
				(*(*p).image)[*,*,(*p).modify.target] = img
				s = (*pstate).operations[(*p).modify.operation] + ' ' + (*(*p).el)[(*p).modify.source] + ' ('+(*pstate).filters[(*p).modify.filter]+') * ' + str_tidy((*p).modify.scale * (*p).modify.strength)
				add_history, (*p).history, (*p).modify.target, 'Inter-Element: '+s
			endif

			(*p).modify.operation = 0
			widget_control, (*pstate).operation, set_combobox_select=(*p).modify.operation
			changed = 1
			end
		else:
	endcase
	goto, finish

update_display:
	if ptr_good(p,/struct) eq 0 then goto, finish
;	if (*p).modify.operation eq 0 then goto, finish
	if ptr_good((*p).modify.image) eq 0 then goto, finish
	if abs((*p).modify.scale) lt 1.0e-5 then goto, finish
	if abs((*p).modify.strength) lt 1.0e-5 then goto, finish
	changed = 1
	goto, finish

update_source:										; update modified source image
	if (*p).modify.filter eq (*pstate).last_filter then goto, finish
	interelement_filter, p, error=err
	(*pstate).last_filter = (*p).modify.filter
	changed = 1

finish:
	widget_control, hourglass=0
	if ptr_good(p,/struct) and changed then begin
		help, (*p).modify
		Notify, 'image-display', from=event.top
	endif
	return

bad_state:
	warning,'interelement_operations_event',['STATE variable has become ill-defined.','Abort.'],/error
	goto, kill
bad_ptr:
	warning,'interelement_operations_event',['Parameter structure variable has become ill-defined.','Abort.'],/error
	goto, kill

kill:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_good(pstate, /struct) eq 0 then goto, die

	(*p).modify.operation = 0
	if ptr_good(p,/struct) then begin
		help, (*p).modify
		Notify, 'image-display', from=event.top
	endif

	cancel_notify, event.top

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;--------------------------------------------------------------------------------------

;	Transform the element. But if 'i' passed, only do it if 'i' is target.

function interelement_transform, p, i, error=err

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
       warning,'interelement_transform',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, 0L
    endif
endif
	err = 1
	if ptr_good(p) eq 0 then return, 0L
	if tag_present( 'modify', *p) eq 0 then return, 0L				; no modify struct (stack image?)
	if n_elements(i) eq 1 then begin
		if (*p).modify.target ne i then return, 0L					; not this modified element
	endif
	if ptr_good((*p).modify.image) eq 0 then return, 0L				; modify.image not setup yet
	if (*p).modify.operation eq 0 then return, 0L					; err will flag no change needed

	img = (*(*p).image)[*,*,(*p).modify.target]
	operations = define(/operations)
	if (*p).modify.operation ge n_elements(operations) then goto, done
	if (*p).modify.operation lt 0 then goto, done

	op = operations[(*p).modify.operation]

	case op of
		'Subtract': begin												; subtract
			f = (*p).modify.scale * (*p).modify.strength
			img -= f * *(*p).modify.image
			end
		'Add': begin													; add
			f = (*p).modify.scale * (*p).modify.strength
			img += f * *(*p).modify.image
			end
		'Multiply': begin												; multiply
			f = (*p).modify.scale * (*p).modify.strength
			img *= f * (*(*p).modify.image)
			end
		'Divide': begin													; divide
			f = (*p).modify.scale * (*p).modify.strength
			q = where( *(*p).modify.image gt (max(*(*p).modify.image) * 0.01), nq)
			if nq gt 0 then img[q] /= f * (*(*p).modify.image)[q]
			end
		'Correct Y step': begin											; Correct Y step
			f = (*p).modify.strength									; 'dynamic_correction'
			ny = n_elements(img[0,*])
			if ((*p).modify.anchour lt 0) or ((*p).modify.anchour ge ny) then goto, done
			for i=(*p).modify.anchour, ny-1 do begin
				img[*,i] *= f
			endfor
			end
		else:
	endcase
done:
	err = 0
	return, img
end

;--------------------------------------------------------------------------------------

;	Filter the source and clip it to remove negatives

pro interelement_filter, p, error=err

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
       warning,'interelement_filter',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
	err = 1
	if ptr_good(p) eq 0 then return
	widget_control, hourglass=1

;	0			1				2					3					4					5					6						7					7						9					10
;	'None','Median filter 2','Median filter 3','Median filter 5','Median filter 10','Gaussian filter 1.0','Gaussian filter 1.5','Gaussian filter 2.0','Gaussian filter 3.0','Gaussian filter 5.0','Gaussian filter 10.0']

	case (*p).modify.filter of
		0: begin
			*(*p).modify.image = (*(*p).image)[*,*,(*p).modify.source] > 0.
			end
		1: begin
			*(*p).modify.image = median( (*(*p).image)[*,*,(*p).modify.source], 2) > 0.
			end
		2: begin
			*(*p).modify.image = median( (*(*p).image)[*,*,(*p).modify.source], 3) > 0.
			end
		3: begin
			*(*p).modify.image = median( (*(*p).image)[*,*,(*p).modify.source], 5) > 0.
			end
		4: begin
			*(*p).modify.image = median( (*(*p).image)[*,*,(*p).modify.source], 10) > 0.
			end
		5: begin
			kernel = gaussian_kernel( 1.0)
			*(*p).modify.image = convol( (*(*p).image)[*,*,(*p).modify.source], kernel, /edge_truncate) > 0.
			end
		6: begin
			kernel = gaussian_kernel( 1.5)
			*(*p).modify.image = convol( (*(*p).image)[*,*,(*p).modify.source], kernel, /edge_truncate) > 0.
			end
		7: begin
			kernel = gaussian_kernel( 2.0)
			*(*p).modify.image = convol( (*(*p).image)[*,*,(*p).modify.source], kernel, /edge_truncate) > 0.
			end
		8: begin
			kernel = gaussian_kernel( 3.0)
			*(*p).modify.image = convol( (*(*p).image)[*,*,(*p).modify.source], kernel, /edge_truncate) > 0.
			end
		9: begin
			kernel = gaussian_kernel( 5.0)
			*(*p).modify.image = convol( (*(*p).image)[*,*,(*p).modify.source], kernel, /edge_truncate) > 0.
			end
		10: begin
			kernel = gaussian_kernel( 10.0)
			*(*p).modify.image = convol( (*(*p).image)[*,*,(*p).modify.source], kernel, /edge_truncate) > 0.
			end
		else: begin
			*(*p).modify.image = (*(*p).image)[*,*,(*p).modify.source] > 0.
			end
	endcase
	err = 0
	widget_control, hourglass=0
	return
end

;--------------------------------------------------------------------------------------

pro interelement_operations, p, GROUP_LEADER=wGroup, TLB=TLB, xoffset=xoffset, yoffset=yoffset, _EXTRA=_VWBExtra_

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
       warning,'interelement_operations',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

  if n_elements(wGroup) lt 1 then wGroup=0L
  if ptr_good(p) eq 0 then return

case !version.os_family of
    'MacOS': begin
       yw = 200
	   xsize_element = 70
	   xsize_ops = 100
	   xsize_filters = 120
	   xsize_slider = 140
	   xsize_button = 60
	   xsize_help = 3*xsize_element + xsize_ops + xsize_button + 16
	   ysize_help = 3
       end
    'unix': begin
       yw = 200
	   xsize_element = 70
	   xsize_ops = 100
	   xsize_filters = 120
	   xsize_slider = 140
	   xsize_button = 60
	   xsize_help = 3*xsize_element + xsize_ops + xsize_button + 16
	   ysize_help = 3
       end
    else: begin
       yw = 200
	   xsize_element = 70
	   xsize_ops = 100
	   xsize_filters = 120
	   xsize_slider = 140
	   xsize_button = 60
	   xsize_help = 3*xsize_element + xsize_ops + xsize_button + 16
	   ysize_help = 3
       end
endcase

	w = 0
	h = 0
	xoff = 0
	yoff = 0
	if widget_info( wGroup, /valid) then begin
		geom = widget_info( wGroup, /geometry)
		w = geom.scr_xsize
		h = geom.scr_ysize
		xoff = geom.xoffset
		yoff = geom.yoffset
	endif
	screen = get_screen_size()
	if n_elements(xoffset) lt 1 then begin
		screen = get_screen_size()
;		xoffset = ((xoff + w - 410) < (screen[0]-34 - 410)) > 0
		xoffset = 100
	endif
	if n_elements(yoffset) lt 1 then begin
    	screen = get_screen_size()
;		yoffset = ((yoff - yw) < (screen[1]-28 - 159)) > 0
		yoffset = ((yoff + h) < (screen[1]-28 - yw)) > 0
	endif

;	Declared also in 'image_eventcb event':'image-process' and used in 'interelement_filter', interelement_transform:
 
	ops = define(/operations)
	filts = ['None','Median filter 2','Median filter 3','Median filter 5','Median filter 10','Gaussian filter 1.0','Gaussian filter 1.5','Gaussian filter 2.0','Gaussian filter 3.0','Gaussian filter 5.0','Gaussian filter 10.0']
	els = *(*p).el

	TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Interelement-TLB',  $
		/TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, /ALIGN_CENTER, /BASE_ALIGN_CENTER,  $
		TITLE='Interelement Operations', SPACE=1, /COLUMN)

	Top = Widget_Base(TLB, UNAME='Top_base', SPACE=1, /ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER)

	target = widget_combobox(Top, UNAME='target', VALUE=els, /tracking,  $
		xsize=xsize_element, uvalue='"Target" element map. This element map will be modified upon "Apply".')

	operation = widget_combobox(Top, UNAME='operation', VALUE=ops, /tracking,  $
		xsize=xsize_ops, uvalue='Operation (on "Source") to apply to "Target" element map.')

	source = widget_combobox(Top, UNAME='source', VALUE=els, /tracking,  $
		xsize=xsize_element, uvalue='"Source" element map. Operation (on source) will be applied to "Target" map.')

	button = widget_button( Top, value='Auto', uname='auto-button', /tracking, scr_xsize=xsize_button, $
		uvalue='For "Subtract", set the "Scale" automatically to (partially) null cross-over features from source in target image. Watch "Target" image and Association between source and target elements.')

	scale = widget_text( Top, value='0.01', uname='scale', /tracking, /editable, $
		uvalue='"Scale" of "Source" element contribution. Adjust further using the "Strength" slider below.', scr_xsize=xsize_element)

	Bot = Widget_Base(TLB, UNAME='Bot_base', SPACE=1, /ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER)

	filter = widget_combobox(Bot, UNAME='filter', VALUE=filts, /tracking, xsize=xsize_filters,  $
		uvalue='Digital filter to apply to "Source" map before operation. Use this for a noisy source image')

	strength = cw_fslider2( Bot, format='(F6.2)', minimum=0.1, maximum=20.0, layout=1, scroll=0.1, $
				value=1.0, uname='strength', xsize=xsize_slider, /tracking, /edit, /drag, $
				uvalue='Adjust the "Strength" of the source element action on the "Target" element map. Watch "Target" image and Association between "source" and "target" elements.')

	button = widget_button( Bot, value='Apply', uname='apply-button', /tracking, scr_xsize=xsize_button, $
		uvalue='Apply the scaled source contribution to the target element map. This is needed before modified image can be saved.')

	help = widget_text( tlb, scr_xsize=xsize_help, ysize=ysize_help, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', frame=0)

	state = {  $
		p:				p, $				; pointer to image struct
		operations:		ops, $				; list of operations
		filters:		filts, $			; list of filters
		source:			source, $			; source element ID
		target:			target, $			; target element ID
		filter:			filter, $			; source filter ID
		last_filter:	-1, $				; previous filter done
		operation:		operation, $		; operation ID
		scale:			scale, $			; scale ID
		strength:		strength, $			; strength ID
		help:			help}  				; help ID

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize
	
	register_notify, tlb, [  $
			'images-changed' $				; new images pointer
			], from=wGroup
	
	xmanager, 'interelement_operations', tlb, /no_block
	return
end

					