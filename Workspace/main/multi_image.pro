pro multi_image_event, event

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
		warning,'multi_image_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good(pstate, /struct) eq 0 then goto, bad_state

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		goto, finish
		end
	'NOTIFY': begin
		case event.tag of
			'image-display': begin
				percent = 0.0
				for i=0L,n_elements(*(*pstate).pid)-1 do begin
					if widget_info( (*(*pstate).pid)[i], /valid) then begin
						widget_control, (*(*pstate).pid)[i], set_value={display:1}
						widget_control, (*(*pstate).pid)[i], get_value=per
						percent = percent + per
					endif
				endfor
				*(*pstate).ppercent = percent
				goto, finish
				end
			'image-clone': begin
				p2 = event.pointer
				if ptr_valid(p2) eq 0 then goto, finish
				(*pstate).pimage = (*p2).p
				if ptr_valid((*pstate).pimage) eq 0 then goto, finish
				for i=0L,n_elements(*(*pstate).pid)-1 do begin
					if widget_info( (*(*pstate).pid)[i], /valid) then begin
						widget_control, (*(*pstate).pid)[i], set_value={image:(*pstate).pimage, element:i}
					endif
				endfor
				multi_image_layout, pstate
				goto, finish
				end
			'image-elements': begin
				if ptr_valid((*pstate).pimage) eq 0 then goto, finish
				for i=0L,n_elements(*(*pstate).pid)-1 do begin
					if widget_info( (*(*pstate).pid)[i], /valid) then begin
						widget_control, (*(*pstate).pid)[i], set_value={list:1}
					endif
				endfor
				goto, finish
				end
		
			else:
		endcase
		end

	'WIDGET_KILL_REQUEST': begin
		print,'Kill request multi-image ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'multi-image-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				multi_image_layout, pstate, width=event.x, height=event.y, /resize
				end
			else:
		endcase
		end

	'simple-image': begin
		case tag_names( event,/structure) of
			'SIMPLE_IMAGE_CROP': begin
				print, 'Crop=',event.crop
				for i=1L,n_elements(*(*pstate).pid)-1 do begin
					if widget_info( (*(*pstate).pid)[i], /valid) then begin
						widget_control, (*(*pstate).pid)[i], set_value={CROP:event.crop}
					endif
				endfor
				*(*pstate).pmaia = event.crop
				notify, 'image-crop', (*pstate).pmaia, from=event.top
				end
			'SIMPLE_IMAGE_TIME': begin
				end
			else:
		endcase
		end
		
	'layout-number': begin
		widget_control, event.id, get_uvalue=n
		multi_image_layout, pstate, Nmax=n
		for i=0L,n_elements((*pstate).number)-1 do begin
			widget_control, (*pstate).number[i], set_value={select:0}
		endfor
		widget_control, event.id, set_value={select:1}
		end
		
	'query-button':begin
		geopixe_browser, 'Help/GeoPIXE-Users-Guide.htm', title='GeoPIXE Users Guide', group=event.top, key='Multi Image Window'
		end
	else:
endcase

finish:
	return

done:
	goto, kill

bad_state:
	warning,'multi_image_event',['STATE variable has become ill-defined.','Abort Batch Sort.'],/error
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

	print,'Cleanup Multi-image ...'

die:
;	heap_gc
	return
end

;--------------------------------------------------------------------------

pro multi_image_layout, pstate, Nmax=Nmax, width=width, height=height, resize=resize

;	Redo multi-image layout
;	nMax		number of simple images
;	width		total width
;	height		total height
;	/resize		after resize event

pimage = (*pstate).pimage
if ptr_good( pimage) eq 0 then return
if n_elements(Nmax) lt 1 then Nmax=(*pstate).N
if n_elements(resize) lt 1 then resize=0
if n_elements(width) lt 1 then width=(*pstate).width
if n_elements(height) lt 1 then height=(*pstate).height
xanes_stack_test, pimage, xanes, n_el, el, el_xanes

aspect = float((*pimage).ysize) / float((*pimage).xsize)
r = (float(height)/float(width)) /aspect
N = min([n_el, Nmax])
N0 = N
if resize then begin
	nx = fix( round(sqrt(float(N)/r)))
	ny = fix( round(float(N)/float(nx)))
endif else begin
	nx = fix( ceil(sqrt(float(N)/r)))
	ny = fix( ceil(float(N)/float(nx)))
endelse
w = float(width-(*pstate).dw)/nx
h = float(height-(*pstate).dh)/ny
w2 = 5 + (h-27) / aspect
if w2 gt w then begin
	h = 27 + (w2-5) * aspect
endif else w=w2
if w lt (*pstate).wmin then begin
;	h = 27 + (h-27) * (((*pstate).wmin-5)/(w-5)) 
	w = (*pstate).wmin
	h = 27 + (w-5) * aspect
endif
if ny * h gt height then begin
	ny = fix( floor(height/h))
	nx = N/ny
	if nx * w gt width then begin
		nx = fix( floor(width/w))
	endif
endif
N = min([N, nx*ny])
print,'nx, ny = ',nx,ny
print,'aspect = ', aspect
print,'width, height=', width,height
print,'w, h=', w,h
print,'(w-5)/(h-27)=',(w-5)/(h-27),' xsize/ysize=',float((*pimage).xsize) / float((*pimage).ysize)

border = 0
i = 0
for j=0L,nx-1 do begin
	x = j*w
	for k=0L,ny-1 do begin
		y = k*h
	
		if widget_info( (*(*pstate).pid)[i], /valid) then begin
			widget_control, (*(*pstate).pid)[i], set_value={width:fix(w), height:fix(h)}
			widget_control, (*(*pstate).pid)[i], xoffset=x+(*pstate).xoff, yoffset=y+(*pstate).yoff
		endif else begin
			(*(*pstate).pid)[i] = simple_image( (*pstate).bulletin_base, pimage=pimage, Nimage=i, $
					uname='simple-image', width=fix(w), height=fix(h), xoffset=x+(*pstate).xoff, yoffset=y+(*pstate).yoff, $
					/frame, xpad=0, ypad=0, space=0, box_select=(i eq 0), border=border)
		endelse
		i = i+1
		if (i ge N) then goto, done
	endfor
endfor

done:
	for i=N,n_elements(*(*pstate).pid)-1 do begin
		if widget_info( (*(*pstate).pid)[i], /valid) then begin
			widget_control, (*(*pstate).pid)[i], /destroy
		endif
	endfor
	(*pstate).N = N0
	(*pstate).w = w
	(*pstate).h = h
	(*pstate).width = width
	(*pstate).height = height
	return
end


;--------------------------------------------------------------------------

pro multi_image, group_leader=group, tlb=tlb, pimage=pimage, width=width, height=height, $
		Nmax=Nmax, ppercent=ppercent, title=title, nosav=nosav, debug=debug

; Layout 'simple_image' images on a grid in a bulletin board base in this window.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if n_elements(debug) lt 1 then debug=0
if debug then catch_errors_on = 0             ; disable error CATCHing
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
	   Catch, /cancel
	   on_error, 1
	   help, calls = s
	   n = n_elements(s)
	   c = 'Call stack: '
	   if n gt 2 then c = [c, s[1:n-2]]
	   warning,'multi_image',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(width) lt 1 then width=1200
if n_elements(height) lt 1 then height=900
if n_elements(Nmax) lt 1 then Nmax=12
if n_elements(title) lt 1 then title='Maia Multi-Image Display'
if n_elements(ppercent) eq 0 then ppercent=ptr_new(0.0)
if n_elements(nosav) lt 1 then nosav=0

register_notify                           ; notification routines
startupp, /colours						  ; setup IDL
define_devices
simple_image_routines
image_routines

pimage = bad_pars_struct( pimage, make_pars=make_pd)
if make_pd then begin
	if nosav eq 0 then begin
		warning,'multi_image',['No "pimage" supplied for image.','Will use a dummy for now.']
		pimage = read_geopixe_image('C:\software\Data\CSIRO\c4-x\c4-x-3-m.dai')
	endif else pimage=ptr_new(define(/image))
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
	xoffset = (xoff + w) < (screen[0]- (600) > 0)
	if xoffset lt (xoff + w) then begin
		t = xoff - (600)
		if t ge 0 then xoffset=t
	endif
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff) < (screen[1]-28 - 200)) > 0
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
		dw = 40		;16			; needs to grow with number buttons on left ...
		dh = 18
		xoff = 2
		yoff = 2
		wmin = 187
		number_xsize = 22
		end
	'unix': begin
		retain=2
		dw = 40		;16			; needs to grow with number buttons on left ...
		dh = 18
		xoff = 2
		yoff = 2
		wmin = 187
		number_xsize = 22
		end
	else: begin
		retain=1
		dw = 40		;16			; needs to grow with number buttons on left ...
		dh = 18
		xoff = 2
		yoff = 2
		wmin = 187
		number_xsize = 22
		end
endcase

if ptr_good( pimage) then begin
	xanes_stack_test, pimage, xanes, n_el, el, el_xanes
endif else begin
	n_el = 1
	el = [' ']
endelse

tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='multi-image-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=0 ,YPAD=0 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /row, xpad=1, ypad=1, space=2, /base_align_center, /align_center)

cbase = widget_base( tbase, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
numbers = [2,3,4,6,9,10,12,16,25]
nn = n_elements(numbers)
number = lonarr(nn)
for i=0L, nn-1 do begin
	number[i] = state_button( cbase, value=str_tidy(numbers[i]), uname='layout-number', $
					tracking=0, xsize=number_xsize, ysize=22, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue=numbers[i])
endfor

space = widget_label( cbase, value='', scr_ysize=100)

query_button = Widget_Button(cbase, UNAME='query-button', xsize=15, ysize=20,  $
      /ALIGN_CENTER ,VALUE='?', /tracking_events, uvalue='Jump to the help on this window in the GeoPIXE Users Guide.')

 
bbase = widget_base( tbase)

; make layout in window

xsize = 256
ysize = 256
if ptr_valid(pimages) then begin
	if n_elements( *pimages) gt 0 then begin
		if (*pimages).xsize gt 0 then xsize = (*pimages).xsize
		if (*pimages).ysize gt 0 then ysize = (*pimages).ysize
	endif
endif

r = float(height)/float(width)
N = min([n_el, Nmax])
nx = fix( ceil(sqrt(float(N)/r)))
ny = fix( ceil(float(N)/float(nx)))
w = float(width-dw)/nx
h = float(height-dh)/ny
wx = float(xsize) > 10.
hy = float(ysize) > 10.
w2 = 5 + (h-27) * float(xsize)/float(ysize)
if w2 gt w then begin
	h = 27 + (w-5) * float(ysize)/float(xsize)
endif else w=w2
if w lt wmin then begin
	h = 27 + (h-27) * ((wmin-5)/(w-5)) 
	w = wmin
endif
if ny * h gt height then begin
	ny = fix( floor(height/h))
	nx = N/ny
	if nx * w gt width then begin
		nx = fix( floor(width/w))
	endif
endif
print,'width, height=', width,height
print,'w, h=', w,h
print,'(w-5)/(h-27)=',(w-5)/(h-27),' xsize/ysize=',wx/hy

border = 0
id = lonarr(100)
i = 0
for j=0L,nx-1 do begin
	x = j*w
	for k=0L,ny-1 do begin
		y = k*h
	
		id[i] = simple_image( bbase, pimage=pimage, Nimage=i, $
					uname='simple-image', width=fix(w), height=fix(h), xoffset=x+xoff, yoffset=y+yoff, $
					/frame, xpad=0, ypad=0, space=0, box_select=(i eq 0), border=border)
		i = i+1
		if (i ge N) then goto, done
	endfor
endfor

done:
	state = { $
		pimage:				pimage, $				; pointer to images
		pid:				ptr_new(id), $			; pointer to simple_image TLB array
		width:				width, $				; overall requested width
		height:				height, $				; overall requested height
		N:					N, $					; number of simple images shown
		w:					fix(w), $				; width of each image
		h:					fix(h), $				; height of each image
		dw:					dw, $
		dh:					dh, $
		wmin:				wmin, $
		xoff:				xoff, $					; X offset origin
		yoff:				yoff, $					; Y offset origin
		tracking:			0, $					; no tracking for now
		pmaia:		ptr_new(/allocate_heap), $		; heap for Notify to maia
		ppercent:			ppercent, $				; ptr to % time (total of all simple-images)

		bulletin_base:		bbase, $				; base ID on which simple images are placed
		number:				number, $				; number button IDs
		help:				0L $					; help text ID
		}

child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

q = where(numbers ge N, nq)
if nq gt 0 then j = q[0] else j=n_elements(numbers)-1
widget_control, number[j], set_value={select:1}

register_notify, tlb, ['path', $				; new path
						'image-clone', $		; images loaded elsewhere
						'image-elements', $		; refresh element droplist for new elements
						'image-display'], $		; update images
						from=group

xmanager, 'multi_image', tlb, /no_block
multi_image_layout, pstate, Nmax=N
end

