pro ET2d_image_event, event

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
		warning,'ET2d_image_event',['IDL run-time error caught.', '', $
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
p = (*pstate).pimage

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s0
		if size(s0,/tname) eq 'STRING' then begin
			s = s0
		endif else if size(s0,/tname) eq 'STRUCT' then begin
			s = s0.help
		endif else s=''
		if event.enter eq 1 then begin
			widget_control, (*pstate).help, set_value=s
		endif else begin
			widget_control, (*pstate).help, set_value='Energy-Time plot: Left click and drag a box to crop display. Right click for full display. Move cursor for E,T.'
		endelse
		goto, finish
		end
	'NOTIFY': begin
		case event.tag of
			'et2d-display': begin
				percent = 0.0
				display = 1
				if widget_info( *(*pstate).pid, /valid) then begin
					if ptr_good((*pstate).pm) then begin
						if ptr_good( (*(*pstate).pm).pileup.limits.plow) and $
									ptr_good( (*(*pstate).pm).pileup.limits.phigh) then begin
							low = *(*(*pstate).pm).pileup.limits.plow
							high = *(*(*pstate).pm).pileup.limits.phigh
							n = min([n_elements(low),n_elements(high)])
							x = indgen(n)
							q = where( (high gt 0) and (low gt 0), nq)
							if nq gt 0 then begin
								x = [x[q], reverse(x[q])]
								y = [ low[q], reverse(high[q])]  
								widget_control, *(*pstate).pid, set_value={shape:{x:x, y:y}}
								display = 0
							endif
						endif
					endif
					if display then widget_control, *(*pstate).pid, set_value={display:1}
					widget_control, *(*pstate).pid, get_value=per
					percent = percent + per
				endif
				*(*pstate).ppercent = percent
				goto, finish
				end
			else:
		endcase
		end

	'WIDGET_KILL_REQUEST': begin
		print,'Kill request ET 2D ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'ET2d-image-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				w = (event.x - (*pstate).dw) > (*pstate).wmin
				h = (event.y - (*pstate).dh) > (*pstate).hmin
				widget_control, *(*pstate).pid, set_value={width:w, height:h}				
				widget_control, (*pstate).help, scr_xsize=w-3				
				end
			else:
		endcase
		end

	'simple-image': begin
		case tag_names( event,/structure) of
			'SIMPLE_IMAGE_CROP': begin
				print, 'Crop=',event.crop
				if widget_info( *(*pstate).pid, /valid) then begin
					widget_control, *(*pstate).pid, set_value={CROP:event.crop}
				endif
				end
			'SIMPLE_IMAGE_CURSOR': begin
;				print, 'Cursor= ', event.cursor.x, event.cursor.y
				widget_control, (*pstate).help, set_value='Cursor E,T = ' + string((*p).xcompress*event.cursor.x) + ', ' + string((*p).ycompress*event.cursor.y)
				end
			'SIMPLE_IMAGE_TIME': begin
				end
			else:
		endcase
		end
		
	else:
endcase

finish:
	return

done:
	goto, kill

bad_state:
	warning,'ET2d_image_event',['STATE variable has become ill-defined.','Abort Batch Sort.'],/error
	goto, kill

kill:
	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		goto, die
	endif
	widget_control, event.top, /destroy
	widget_control, hourglass=0

	print,'Cleanup ET2D-image ...'

die:
;	heap_gc
	return
end

;--------------------------------------------------------------------------

pro ET2d_image, group_leader=group, tlb=tlb, pimage=pimage, width=width, height=height, $
		ppercent=ppercent, pm=pm, title=title

; A single 'simple_image' image for ET 2D display.

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
	   warning,'ET2d_image',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(width) lt 1 then width=570
if n_elements(height) lt 1 then height=400
if n_elements(ppercent) eq 0 then ppercent=ptr_new(0.0)
if n_elements(pm) eq 0 then pm=ptr_new()
if n_elements(title) eq 0 then title = 'Maia ET 2D Display'

register_notify                           ; notification routines
startupp, /colours						  ; setup IDL
simple_image_routines

pimage = bad_pars_struct( pimage, make_pars=make_pd)
if make_pd then begin
	warning,'ET2d_image',['No "pimage" supplied for image.','Will use a dummy for now.']
	pimage = read_geopixe_image('C:\Software\Demo\CSIRO\c4-x\c4-x-3-m.dai')
;	return
endif

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
	local_group = 0
endif else begin
	group = widget_base(scr_xsize=1, scr_ysize=1)
	local_group = 1
endelse
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = (xoff + w) < (screen[0]- (800) > 0)
	if xoffset lt (xoff + w) then begin
		t = xoff - (600)
		if t ge 0 then xoffset=t
	endif
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff) < (screen[1]-28 - 500)) > 0
endif

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

;	  select: 0     1      2      3
colours = [ grey, green, violet, violet]		; for ??
colours2 = [grey, green, yellow, red]			; for "number buttons"

case !version.os_family of
	'MacOS': begin
		retain=2
		dw = 9
		dh = 31
		xoff = 2
		yoff = 2
		wmin = 300
		hmin = 200
		number_xsize = 22
		end
	'unix': begin
		retain=2
		dw = 9
		dh = 31
		xoff = 2
		yoff = 2
		wmin = 300
		hmin = 200
		number_xsize = 22
		end
	else: begin
		retain=1
		dw = 9
		dh = 31
		xoff = 2
		yoff = 2
		wmin = 300
		hmin = 200
		number_xsize = 22
		end
endcase

tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='ET2d-image-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=0 ,YPAD=0 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=1, ypad=1, space=0, /base_align_center, /align_center)

; make layout in window

id = simple_image( tbase, pimage=pimage, Nimage=0, /tracking, /cursor, /border, /ganged, $
			uname='simple-image', width=width, height=height, /box_select)

help = widget_text( tbase, scr_xsize=width-3, ysize=1, /wrap, uname='help', value='Energy-Time plot: Left click and drag a box to crop display. Right click for full display. Move cursor for E,T.', /tracking, $
					uvalue='Context sensitive help. Move cursor over widgets to see help on them.', frame=0)

done:
	state = { $
		pimage:				pimage, $				; pointer to images
		pm:					pm, $					; pointer to passed Maia 'pm' struct
		pid:				ptr_new(id), $			; pointer to simple_image TLB array
		width:				width, $				; overall requested width
		height:				height, $				; overall requested height
		dw:					dw, $
		dh:					dh, $
		wmin:				wmin, $
		hmin:				hmin, $
		pmaia:				ptr_new(/allocate_heap), $		; heap for Notify to maia
		ppercent:			ppercent, $				; ptr to % time (total of all simple-images)
		help:				help $					; help text ID
		}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
						'et2d-display'], $		; update images
						from=group

xmanager, 'ET2d_image', tlb, /no_block
end

