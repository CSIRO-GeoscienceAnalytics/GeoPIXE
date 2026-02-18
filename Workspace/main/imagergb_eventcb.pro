;
; IDL Event Callback Procedures
; ImageRGB_eventcb
;
;-----------------------------------------------------------------

pro ImageRGB_Compare, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good( p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

if xanes then begin
	ext = '*.xan'
	title = 'Select XANES Image file #2 to Compare'
endif else begin
	ext = '*.dai'
	title = 'Select Image file #2 to Compare'
endelse

F = file_requester( /read, /must_exist, filter=ext, $
         title=title, path=*(*pstate).path, group=event.top, fix_filter=0)
if F ne '' then begin
	ImageRGB_Compare2, pstate, F
endif
end

;-----------------------------------------------------------------

pro ImageRGB_Compare3, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good( p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

if xanes then begin
	ext = '*.xan'
	title = 'Select XANES Image file #3 to Compare'
endif else begin
	ext = '*.dai'
	title = 'Select Image file #3 to Compare'
endelse

F = file_requester( /read, /must_exist, filter=ext, $
         title=title, path=*(*pstate).path, group=event.top, fix_filter=0)
if F ne '' then begin
	ImageRGB_Compare2, pstate, F, /third
endif
end

;-----------------------------------------------------------------

pro ImageRGB_Compare2, pstate, F, third=third, show=sel, error=err

COMPILE_OPT STRICTARR

	err = 1
	p = (*pstate).p
	if ptr_good( p) eq 0 then return
	if n_elements(third) eq 0 then third=0
	palt = (*pstate).palt
	palt2 = (*pstate).palt2
	good_alt = ptr_good(palt)
	good_alt2 = ptr_good(palt2)
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	n_el2 = 0
	n_el3 = 0
	if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
	if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

	widget_control, /hourglass
    if xanes then begin
		palt = read_geopixe_image( F, /XANES)
    endif else begin
    	palt = read_geopixe_image( F)
    endelse
	if ptr_good(palt) eq 0 then begin
		warning,'ImageRGB_Compare2','Failed to load image file.'
		return
	endif

    if ptr_valid(palt) then begin
		if third then begin
			if ptr_valid((*pstate).palt2) then free_images, (*pstate).palt2
    		(*pstate).palt2 = palt
			palt2 = palt
			good_alt2 = 1
			xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3
			elm = el3
			n_elm = n_el3
		endif else begin
			if ptr_valid((*pstate).palt) then free_images, (*pstate).palt
    		(*pstate).palt = palt
			good_alt = 1
			xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
			elm = el2
			n_elm = n_el2
		endelse
       *(*pstate).path = extract_path( F)

		if ptr_valid( (*palt).options ) eq 0 then fix_options, palt
		charge_ratio = (*palt).charge / (*p).charge
		for i=0,n_elm-1 do begin
			q = where( elm[i] eq el, nq)
			if nq ne 0 then begin
				(*(*palt).options)[i].max = charge_ratio * (*(*p).options)[q[0]].max 
			endif
		endfor
		
		(*pstate).Image = 0
		(*pstate).Image2 = 0
		(*pstate).Image3 = 0

		if n_elements(sel) gt 0 then begin
			sel = strtrim(sel, 2)
			q1 = where( *(*(*pstate).p).el eq sel, nq1)
			if nq1 gt 0 then (*pstate).Image = q1[0]
			q2 = where( *(*(*pstate).palt).el eq sel, nq2)
			if nq2 gt 0 then (*pstate).Image2 = n_el + q2[0]
			(*pstate).Image3 = 0
			if (nq1 eq 0) or (nq2 eq 0) then begin
				warning,'ImageRGB_Compare2',['Selected element not found.','Check spelling.', $
					'Remember that elements with L,M lines','need a "L" or "M" suffix.']
			endif else err=0
		endif
		
		s = el
		if good_alt then s = [el,el2+' 2']
		if good_alt2 then s = [s,el3+' 3']
		widget_control, (*pstate).element_id1, set_value=s
		widget_control, (*pstate).element_id2, set_value=s
		widget_control, (*pstate).element_id3, set_value=s
		widget_control, (*pstate).element_id1, set_combobox_select = (*pstate).Image
		widget_control, (*pstate).element_id2, set_combobox_select = (*pstate).Image2
		widget_control, (*pstate).element_id3, set_combobox_select = (*pstate).Image3

    endif else begin
       warning,'imageRGB_Compare2','error reading image file '+F
    endelse

	draw_ImageRGBs, pstate
	return
end

;-----------------------------------------------------------------

pro ImageRGB_Exit, Event

OnKill_ImageRGB, event

end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro ImageRGB_Plot_Results, Event

COMPILE_OPT STRICTARR
;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate

;ImageRGB_plot, group_leader=event.top, TLB=tlb
;register_notify, event.top, 'ImageRGB-?', from=tlb
end

;-----------------------------------------------------------------

pro ImageRGB_Export, Event, cgm=cgm, wmf=wmf, png=png, jpeg=jpeg

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(cgm) eq 0 then cgm=0
if n_elements(wmf) eq 0 then wmf=0
if n_elements(png) eq 0 then png=0
if n_elements(jpeg) eq 0 then jpeg=0
if ptr_good( (*pstate).p) eq 0 then goto, done

p = (*pstate).p
xanes_stack_test, p, xanes, n_el, el_names, el_xanes

; This still refers to the 'image' pstate struct, not available in RGB image pstate
if ptr_valid( (*pstate).pstate_parent) then begin
	pstate2 = (*pstate).pstate_parent
	crop = {x:[0,(*(*pstate2).p).xsize-1], y:[0,(*(*pstate2).p).ysize-1] }

;	Can have corr dots displayed, as long as there is a shape defined too.
;	if (*pstate2).corr_mode eq 0 then begin
	    if (*pstate2).analyze_mode eq 0 then begin            ; include mode only
	       mark_vertices, pstate2, x1,y1, n
	       if n gt 0 then begin
	         x = round(x1)
	         y = round(y1)
	         crop.x = [min(x),max(x)]
	         crop.y = [min(y),max(y)]
	       endif
	    endif
;	endif
endif else crop=0L

select = plot_RGB_image_select( event.top, cgm=cgm, wmf=wmf, png=png, jpeg=jpeg, old_select=*(*pstate).pexport, $
			path=*(*pstate).path, image_pstate=pstate, crop=crop )
if select.error then goto, done

;goto, done     ; use this bypass if testing plot_RGB_image_select without /modal, etc.

name = 'Select File Root for RGB image save as '
file = strip_file_m(strip_file_ext(strip_path((*p).file)), ending=['-m','-x'])
if file eq '' then file = strip_file_ext(strip_path((*p).source))

if select.plot.learn.on then begin
	ImageRGB_Learn, event, /restore, file=select.plot.learn.file, /silent
	warning,/info, 'ImageRGB_Export', ['Save RGB plots using Learn file option,', $
		'for '+str_tidy(n_elements( *(*pstate).plearn))+ ' RGB combinations.','','This will take some time ...']
endif

if select.plot.type eq 'CGM' then begin
    file = file + '.cgm'
    file = file_requester( /write, filter='*.cgm', path=*(*pstate).path, $
         file=file, title=name+'CGM', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file)
	add = ''
	if select.plot.enhance.spots.on then begin
		els = strtrim( strcompress( select.plot.enhance.spots.elements), 2)
		add = '-enhance' + '-'+replace( ' ', '-', els)
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_RGB_images, pstate, /cgm, file=file, options=select.plot, crop=crop

endif else if select.plot.type eq 'METAFILE' then begin
    file = file + '.wmf'
    file = file_requester( /write, filter='*.wmf', path=*(*pstate).path, $
         file=file, title=name+'WMF', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file)
	add = ''
	if select.plot.enhance.spots.on then begin
		els = strtrim( strcompress( select.plot.enhance.spots.elements), 2)
		add = '-enhance' + '-'+replace( ' ', '-', els)
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_RGB_images, pstate, /wmf, file=file, options=select.plot, crop=crop

endif else if select.plot.type eq 'PNG' then begin
    file = file + '.png'
    file = file_requester( /write, filter='*.png', path=*(*pstate).path, $
         file=file, title=name+'PNG', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = ''
	if select.plot.enhance.spots.on then begin
		els = strtrim( strcompress( select.plot.enhance.spots.elements), 2)
		add = '-enhance' + '-'+replace( ' ', '-', els)
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_RGB_images, pstate, /png, file=file, options=select.plot, crop=crop

endif else if select.plot.type eq 'JPEG' then begin
    file = file + '.jpg'
    file = file_requester( /write, filter='*.jpg', path=*(*pstate).path, $
         file=file, title=name+'JPEG', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = ''
	if select.plot.enhance.spots.on then begin
		els = strtrim( strcompress( select.plot.enhance.spots.elements), 2)
		add = '-enhance' + '-'+replace( ' ', '-', els)
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_RGB_images, pstate, /jpeg, file=file, options=select.plot, crop=crop

endif else if select.plot.type eq 'PS' then begin
    file = file + '.eps'
    file = file_requester( /write, filter='*.eps', path=*(*pstate).path, $
         file=file, title=name+'EPS', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = ''
	if select.plot.enhance.spots.on then begin
		els = strtrim( strcompress( select.plot.enhance.spots.elements), 2)
		add = '-enhance' + '-'+replace( ' ', '-', els)
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_RGB_images, pstate, /eps, file=file, options=select.plot, crop=crop

endif else begin

    plot_RGB_images, pstate, options=select.plot, crop=crop
endelse
if  select.plot.type eq 'CGM' then warning,'ImageRGB_Export',['Note that Windows may fail to import CGM files into Office programs.', $
		'This is not a GeoPIXE problem, but a new security setting in the Windows registry.', $
		'See solutions to this problem on the web (e.g. google "Windows Office Powerpoint import CGM problems")', $
		'and in the GeoPIXE "Help" directory.'], /info

*(*pstate).pexport = {plot:select.plot}

done:
end

;-----------------------------------------------------------------

pro ImageRGB_Preferences, Event

end

;-----------------------------------------------------------------

pro ImageRGB_Save_GIF, Event, png=png

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(png) lt 1 then png=0

if png then begin
	ext = '.png'
	title = 'PNG file to Write'
endif else begin
	ext = '.gif'
	title = 'GIF file to Write'
endelse

p = (*pstate).p
if ptr_good( p) eq 0 then return
palt = (*pstate).palt
palt2 = (*pstate).palt2
good_alt = ptr_good(palt)
good_alt2 = ptr_good(palt2)
xanes_stack_test, p, xanes, n_el, el, el_xanes
n_el2 = 0
n_el3 = 0
if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
q = where( (*pstate).image ge sizes, ns)
(*pstate).Image1Alt = ns
q = where( (*pstate).image2 ge sizes, ns)
(*pstate).Image2Alt = ns
q = where( (*pstate).image3 ge sizes, ns)
(*pstate).Image3Alt = ns

case (*pstate).Image1Alt of
	0: begin
		s1 = el[(*pstate).Image - n_el]
		end
	1: begin
		s1 = el2[(*pstate).Image - n_el]
		end
	2: begin
		s1 = el3[(*pstate).Image - (n_el + n_el2)]
		end
endcase
case (*pstate).Image2Alt of
	0: begin
		s2 = el[(*pstate).Image2 - n_el]
		end
	1: begin
		s2 = el2[(*pstate).Image2 - n_el]
		end
	2: begin
		s2 = el3[(*pstate).Image2 - (n_el + n_el2)]
		end
endcase
case (*pstate).Image3Alt of
	0: begin
		s3 = el[(*pstate).Image3 - n_el]
		end
	1: begin
		s3 = el2[(*pstate).Image3 - n_el]
		end
	2: begin
		s3 = el3[(*pstate).Image3 - (n_el + n_el2)]
		end
endcase

file = find_file2( (*p).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
gif_file = strip_path( strip_file_ext( (*p).file)) + '-' + s1+s2+s3 + ext

F = file_requester( /write, filter = '*'+ext, path=path, $
			title=title, file = gif_file, group=event.top, fix_filter=1)
if F ne '' then begin
	F = strip_file_ext(F)+ext
	b = make_RGB_true( pstate)
	temp_pix = 0
	if n_elements(b) gt 1 then begin
		if png then begin
			write_png, F, b
		endif else begin
;			window, /free, xsize=n_elements(b[0,*,0]), ysize=n_elements(b[0,0,*]), /pixmap
;			tpix = !d.window
;			temp_pix = 1
;			tv, b, /true
;			b = color_quan( tvrd(true=1), 1, rcol,gcol,bcol, colors=128)

			b = color_quan( b, 1, rcol,gcol,bcol, colors=128)
			write_gif, F, b, rcol,gcol,bcol
		endelse
	endif else begin
		warning,'ImageRGB_save_GIF','No image data to save.'
	endelse
;	if temp_pix then wdelete, tpix
endif

done:
end

;-----------------------------------------------------------------

pro ImageRGB_Learn, Event, first=first, save=save_it, restore=restore_it, execute=do_it, file=file, silent=silent

; /silent to not prompt for a zoom

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
		warning,'ImageRGB_Learn',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(first) lt 1 then first=0
if n_elements(save_it) lt 1 then save_it=0
if n_elements(restore_it) lt 1 then restore_it=0
if n_elements(do_it) lt 1 then do_it=0
if n_elements(file) lt 1 then file=''
if n_elements(silent) lt 1 then silent=0

	p = (*pstate).p
	if ptr_good( p) eq 0 then return
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	
	pl = (*pstate).plearn
	n = n_elements(*pl)
	if n eq 0 then first=1
	
	if save_it then goto, save_it
	if file ne '' then goto, load_file
	if restore_it then goto, restore_it
	if do_it then goto, do_it

	if ((*pstate).image1alt ge 1) or ((*pstate).image2alt ge 1) or ((*pstate).image3alt ge 1) then begin
		warning,'ImageRGB_Learn','Cannot save the "Compare" element to list.'
		return
	endif

	item = {r:el[(*pstate).image], g:el[(*pstate).image2], b:el[(*pstate).image3], zoom:(*pstate).zoom}
	
	if first then begin
		*pl = item
	endif else begin
		*pl = [*pl,item]
	endelse
	return

save_it:
	if n eq 0 then begin
		warning,'ImageRGB_Learn','No RGB images lists to save.'
		return
	endif
	F = file_requester( /write, filter=['*.rgb.csv'], group=event.top, path=*(*pstate).path, $
				title='Save RGB image display list to a file', /fix_filter)
	if F eq '' then return
	F = strip_file_ext(F,/double) + '.rgb.csv'


	on_ioerror, bad_file
	openw, unit, F[0], /get_lun

	list = *(*pstate).plearn
	for i=0,n-1 do begin
		printf, unit, list[i].r, list[i].g, list[i].b, list[i].zoom, format='(A,",",A,",",A,",",I3)'
	endfor
	close_file, unit
	return

restore_it:
	file = file_requester( /read, filter=['*.rgb.csv'], group=event.top, path=*(*pstate).path, $
				title='Load RGB image display list from a file', /fix_filter)
load_file:
	if file eq '' then return

	on_ioerror, bad_file
	openr, unit, file[0], /get_lun
	(*pstate).display_file = file

	list = replicate( {r:'', g:'', b:'', zoom:0}, 200)
	s = ''
	on_ioerror, cont
	for i=0,199 do begin
		readf, unit, s
		str = strsplit( s, ', 	', /extract, count=ns)
		if ns eq 0 then goto, cont
		if ns lt 4 then begin
			warning,'ImageRGB_Learn','Bad file format.'
			goto, bad_file
		endif
		list[i].r = str[0]
		list[i].g = str[1]
		list[i].b = str[2]
		list[i].zoom = fix2(str[3])
	endfor
cont:
	q = where( list.r ne '', nq)
	if nq eq 0 then goto, bad_file
	list = list[q]
	*(*pstate).plearn = list
	close_file, unit
	return

do_it:
	red = (*pstate).image
	green = (*pstate).image2
	blue = (*pstate).image3
	zm = (*pstate).zoom

	if silent eq 0 then begin
		drop = ['Images at full resolution','Select zoom level for output files']
		help_drop = 'Optionally, select the display zoom (negative = factors of 2 compressed) for the image output file. The default is full resolution (zoom = 0).'
		check = ['Full (zoom = 0)','Half (zoom = -1)','Quarter (zoom = -2)']
		initial_check = 1
		help_drop = 'Optionally, select the display zoom (negative = factors of 2 compressed) for the image output file. The default is full resolution (zoom = 0).'
		map_check = [0,1]
		help_check = 'Select the display zoom (negative = factors of 2 compressed) for the image output file. The default is full resolution (zoom = 0).'
		Help_default = 'Select the display zoom (negative = factors of 2 compressed) for the image output file. The default is full resolution (zoom = 0).'
		r = options_popup( title='Select display zoom for output', check=check, /exclusive, initial_check=initial_check, help_check=help_check, map_check=map_check, $
					help_default=help_default, drop=drop, help_drop=help_drop, error=error)				; , min_xsize=200
		if error then return
		
		use_zoom = r.drop[0]
		if use_zoom then begin
			izoom = [0,-1,-2]
			zoom = izoom[ where( r.check eq 1)]
			zoom = zoom - (*pstate).zoom
			full = 0
		endif else begin
			zoom = 0
			full = 1
		endelse
	endif else begin
		zoom = 0
		full = 1
	endelse

	set_RGB_view, pstate, event.top, zoom=zoom, full=full

	print, 'R', 'G', 'B', 'zoom', format='(3x,A,",",A,",",A,",",A)'
	for i=0,n-1 do begin
		print, (*(*pstate).plearn)[i].r, (*(*pstate).plearn)[i].g, (*(*pstate).plearn)[i].b, (*(*pstate).plearn)[i].zoom, format='(3x,A,",",A,",",A,",",I3)'

		q = where( (*(*pstate).plearn)[i].r eq el, nq)
		if nq eq 0 then continue
		(*pstate).image = q[0]
		q = where( (*(*pstate).plearn)[i].g eq el, nq)
		if nq eq 0 then continue
		(*pstate).image2 = q[0]
		q = where( (*(*pstate).plearn)[i].b eq el, nq)
		if nq eq 0 then continue
		(*pstate).image3 = q[0]

		draw_ImageRGBs, pstate
		ImageRGB_Save_JPEG, Event, /silent
	endfor
	(*pstate).image = red
	(*pstate).image2 = green
	(*pstate).image3 = blue
	draw_ImageRGBs, pstate
	return

bad_file:
	close_file, unit
	return
end

;-----------------------------------------------------------------

pro ImageRGB_Save_JPEG, Event, silent=silent

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(silent) lt 1 then silent=0

p = (*pstate).p
if ptr_good( p) eq 0 then return
palt = (*pstate).palt
palt2 = (*pstate).palt2
good_alt = ptr_good(palt)
good_alt2 = ptr_good(palt2)
xanes_stack_test, p, xanes, n_el, el, el_xanes
n_el2 = 0
n_el3 = 0
if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
q = where( (*pstate).image ge sizes, ns)
(*pstate).Image1Alt = ns
q = where( (*pstate).image2 ge sizes, ns)
(*pstate).Image2Alt = ns
q = where( (*pstate).image3 ge sizes, ns)
(*pstate).Image3Alt = ns

case (*pstate).Image1Alt of
	0: begin
		s1 = el[(*pstate).Image - n_el]
		end
	1: begin
		s1 = el2[(*pstate).Image - n_el]
		end
	2: begin
		s1 = el3[(*pstate).Image - (n_el + n_el2)]
		end
endcase
case (*pstate).Image2Alt of
	0: begin
		s2 = el[(*pstate).Image2 - n_el]
		end
	1: begin
		s2 = el2[(*pstate).Image2 - n_el]
		end
	2: begin
		s2 = el3[(*pstate).Image2 - (n_el + n_el2)]
		end
endcase
case (*pstate).Image3Alt of
	0: begin
		s3 = el[(*pstate).Image3 - n_el]
		end
	1: begin
		s3 = el2[(*pstate).Image3 - n_el]
		end
	2: begin
		s3 = el3[(*pstate).Image3 - (n_el + n_el2)]
		end
endcase

ext = '.jpg'
title = 'JPEG file to Write'

file = find_file2( (*p).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
gif_file = strip_path( strip_file_ext( (*p).file)) + '-' + s1+s2+s3 + ext

if silent then begin
	F = path + gif_file
endif else begin
	F = file_requester( /write, filter='*'+ext, path=path, $
			title=title, file=gif_file, group=event.top, fix_filter=1)
endelse
if F ne '' then begin

	b = make_RGB_true( pstate)
	temp_pix = 0
	if n_elements(b) gt 1 then begin
		write_jpeg, F, b, true=1, quality=100
	endif else begin
		warning,'ImageRGB_save_JPEG','No image data to save.'
	endelse
endif

done:
end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro OnButton_ImageRGB_Full, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then return

set_RGB_view, pstate, event.top, /full
end
;
;-----------------------------------------------------------------

pro OnButton_ImageRGB_Zoom_In, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_RGB_view, pstate, event.top, zoom=+1

done:
end
;
;-----------------------------------------------------------------

pro OnButton_ImageRGB_Zoom_Out, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_RGB_view, pstate, event.top, zoom=-1

done:
end
;
;-----------------------------------------------------------------
; Note: operation here depends on whether this is a clone or not.

pro OnDestroy_ImageRGB, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

	free_ImageRGB_state, pstate
return
end

;-----------------------------------------------------------------

pro OnKill_ImageRGB, Event

notify, 'image-rgb-closed', from=event.top

;cancel_notify, event.top
widget_control, event.top, /destroy
end

;-----------------------------------------------------------------
; Slider Callback Procedure.
;
;   {WIDGET_SLIDER, ID:0L, TOP:0L, HANDLER:0L, VALUE:0,  DRAG:0}
;
;	VALUE returns the new value of the slider.
;	DRAG returns integer 1
;		if the slider event was generated as part of a drag operation,
;		or zero if the event was generated when the user had finished
;		positioning the slider.
;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:from }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or 0).
;-----------------------------------------------------------------

pro OnNotify_ImageRGB, Event

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
		warning,'OnNotify_ImageRGB',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.tag of

	'path': begin
		if ptr_valid( event.pointer) eq 0 then goto, finish
		*(*pstate).path = (*event.pointer)
		end

	'image-display': begin									; returned from 'Image'

;		print,'RGB: image-display ...'
		p = (*pstate).p
		if ptr_good(p) then begin
			palt = (*pstate).palt
			palt2 = (*pstate).palt2
			good_alt = ptr_good(palt)
			good_alt2 = ptr_good(palt2)
			xanes_stack_test, p, xanes, n_el, el, el_xanes
			n_el2 = 0
			n_el3 = 0
			if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
			if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

			(*pstate).image <= (n_el + n_el2 + n_el3-1)
			if good_alt2 eq 0 then (*pstate).image <= (n_el + n_el2-1)
			if good_alt eq 0 then (*pstate).image <= (n_el-1)
			(*pstate).image2 <= (n_el + n_el2 + n_el3-1)
			if good_alt2 eq 0 then (*pstate).image2 <= (n_el + n_el2-1)
			if good_alt eq 0 then (*pstate).image2 <= (n_el-1)
			(*pstate).image3 <= (n_el + n_el2 + n_el3-1)
			if good_alt2 eq 0 then (*pstate).image3 <= (n_el + n_el2-1)
			if good_alt eq 0 then (*pstate).image3 <= (n_el-1)
			sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
	
			q = where( (*pstate).image ge sizes, ns)
			(*pstate).Image1Alt = ns
			q = where( (*pstate).image2 ge sizes, ns)
			(*pstate).Image2Alt = ns
			q = where( (*pstate).image3 ge sizes, ns)
			(*pstate).Image3Alt = ns
	
			case (*pstate).Image1Alt of
				0: begin
					opt = (*p).options
					n = (*pstate).Image
					end
				1: begin
					opt = (*palt).options
					n = (*pstate).Image - n_el
					end
				2: begin
					opt = (*palt2).options
					n = (*pstate).Image - (n_el + n_el2)
					end
			endcase
			widget_control, (*pstate).max_id1, set_value=str_tidy( (*opt)[n].top)
	
			case (*pstate).Image2Alt of
				0: begin
					opt = (*p).options
					n = (*pstate).Image2
					end
				1: begin
					opt = (*palt).options
					n = (*pstate).Image2 - n_el
					end
				2: begin
					opt = (*palt2).options
					n = (*pstate).Image2 - (n_el + n_el2)
					end
			endcase
			widget_control, (*pstate).max_id2, set_value=str_tidy( (*opt)[n].top)
	
			case (*pstate).Image3Alt of
				0: begin
					opt = (*p).options
					n = (*pstate).Image3
					end
				1: begin
					opt = (*palt).options
					n = (*pstate).Image3 - n_el
					end
				2: begin
					opt = (*palt2).options
					n = (*pstate).Image3 - (n_el + n_el2)
					end
			endcase
			widget_control, (*pstate).max_id3, set_value=str_tidy( (*opt)[n].top)
		endif

		draw_ImageRGBs, pstate

		s = legend_RGB_string(pstate)
		widget_control, (*pstate).help, set_value=s
		end

	'images-changed': begin									; returned from other apps

		print,'RGB: images-changed ...'
		p = event.pointer									; pointer to other 'ImageRGB'
		if ptr_valid(p) eq 0 then goto, finish

		(*pstate).p = p
		(*pstate).file = (*p).file

		set_RGB_view, pstate, event.top

		s = legend_RGB_string(pstate)
		widget_control, (*pstate).help, set_value=s
		end

	'image-analyze-mark': begin								; returned from image when shape changes

		print,'RGB: shape-changed ...'
		draw_ImageRGBs, pstate

		s = legend_RGB_string(pstate)
		widget_control, (*pstate).help, set_value=s
		end

	'batch-save': begin										; returned from batch via other apps

		print,'RGB: batch-save ...........................................................'
		first = 1
		if ptr_valid( event.pointer) then begin
			if size(*event.pointer,/tname) eq 'STRUCT' then first=(*event.pointer).first
		endif

		if (*event.pointer).rgb then begin
			if first then begin
				ImageRGB_Learn, Event, /restore
			endif else begin
				ImageRGB_Learn, Event, file=(*pstate).display_file 
			endelse
			ImageRGB_Learn, Event, /execute, /silent
		endif
		end

	'wizard-action': begin
		if ptr_valid( event.pointer) then begin
			if (*event.pointer).window eq 'Image RGB' then begin
				case (*event.pointer).command of
					'open-test': begin
;						print,'*** Wizard Image RGB: test if window is open ...'
						pw = (*pstate).pwiz
						*pw = *event.pointer
						(*pw).top = event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'compare-image': begin
						print,'*** Wizard Image RGB: compare spectra ...'
						pw = event.pointer
						file = *(*pw).pdata
						el = (*pw).qual1
						print,'	Image RGB: compare image= '+file
						ImageRGB_Compare2, pstate, file, show=el, error=err

						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'save-rgb': begin
						print,'*** Wizard Image RGB: save RGB images ...'
						pw = event.pointer
						file = *(*pw).pdata
						ImageRGB_Learn, Event, file=file 
						ImageRGB_Learn, Event, /execute, /silent

						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					else: begin
						warning,'image_RGB: Notify',['Unknown wizard command: '+(*event.pointer).command, $
								'Make sure GeoPIXE version is compatible with Wizard.']
					endelse
				endcase
			endif
		endif
		end

	else: begin
		print,'OnNotify_ImageRGB: unknown tag = ',event.tag
		end
endcase

finish:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	return

die:
	widget_control, event.top, /destroy
	return
	end

;-----------------------------------------------------------------

pro OnRealize_ImageRGB_Help1, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help1 = wWidget
(*pstate).help = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_ImageRGB_Help2, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help2 = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_ImageRGB, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

draw_trim = 0
scr_trim = 15
if !version.os_family eq 'MacOS' then begin
	draw_trim = 15
	scr_trim = 21
endif

(*pstate).dw =		16
(*pstate).dh =		0

widget_control, wWidget, get_value=wid2
wset,wid2
(*pstate).wid2 = wid2
(*pstate).draw2 = wWidget

set_RGB_view, pstate, top, /realize
return
end

;-----------------------------------------------------------------

pro OnRealize_Element1, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).element_id1 = wWidget
p = (*pstate).p
if ptr_good( p) then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	
	s = replicate( '?',  n_el * 2)
	s[0:n_el-1] = el
	widget_control, wWidget, set_value=s
endif
widget_control, wWidget, set_combobox_select = (*pstate).Image
end

;-----------------------------------------------------------------

pro OnRealize_Element2, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).element_id2 = wWidget
p = (*pstate).p
if ptr_good( p) then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes

	s = replicate( '?',  n_el * 2)
	s[0:n_el-1] = el
	widget_control, wWidget, set_value=s
endif
widget_control, wWidget, set_combobox_select = (*pstate).Image2
end

;-----------------------------------------------------------------

pro OnRealize_Element3, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).element_id3 = wWidget
p = (*pstate).p
if ptr_good( p) then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes

	s = replicate( '?',  n_el * 2)
	s[0:n_el-1] = el
	widget_control, wWidget, set_value=s
endif
widget_control, wWidget, set_combobox_select = (*pstate).Image3
end

;-----------------------------------------------------------------

pro OnRealize_ImageRGB_Max1, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).max_id1 = wWidget
p = (*pstate).p
if ptr_good( p) then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	
	(*pstate).Image = (*pstate).Image < (n_el-1)
	i = (*pstate).Image
	opt = (*p).options
	widget_control, wWidget, set_value=str_tidy((*opt)[i].top)
endif
end

;-----------------------------------------------------------------

pro OnRealize_ImageRGB_Max2, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).max_id2 = wWidget
p = (*pstate).p
if ptr_good( p) then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	
	(*pstate).Image2 = (*pstate).Image2 < (n_el-1)
	i = (*pstate).Image2
	opt = (*p).options
	widget_control, wWidget, set_value=str_tidy((*opt)[i].top)
endif
end

;-----------------------------------------------------------------

pro OnRealize_ImageRGB_Max3, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).max_id3 = wWidget
p = (*pstate).p
if ptr_good( p) then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	
	(*pstate).Image3 = (*pstate).Image3 < (n_el-1)
	i = (*pstate).Image3
	opt = (*p).options
	widget_control, wWidget, set_value=str_tidy((*opt)[i].top)
endif
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

pro OnSelect_ImageRGB_Element, Event, n

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good( p) eq 0 then goto, done
palt = (*pstate).palt
palt2 = (*pstate).palt2
good_alt = ptr_good(palt)
good_alt2 = ptr_good(palt2)
if good_alt2 and (good_alt eq 0) then begin
	palt = palt2
	palt2 = ptr_new()
	good_alt = 1
	good_alt2 = 0
endif
xanes_stack_test, p, xanes, n_el, el, el_xanes
n_el2 = 0
n_el3 = 0
if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

i = event.index

i <= (n_el + n_el2 + n_el3-1)
if good_alt2 eq 0 then i <= (n_el + n_el2-1)
if good_alt eq 0 then i <= (n_el-1)
sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]

case n of
	1: begin
		(*pstate).Image = i
		q = where( (*pstate).Image ge sizes, ns)
		(*pstate).Image1Alt = ns
		case (*pstate).Image1Alt of
			0: begin
				i = (*pstate).Image
				opt = (*p).options
				end
			1: begin
				opt = (*palt).options
				i = (*pstate).Image - n_el
				end
			2: begin
				opt = (*palt2).options
				i = (*pstate).Image - (n_el + n_el2)
				end
		endcase
		widget_control, (*pstate).max_id1, set_value=str_tidy( (*opt)[i].top)
		end
	2: begin
		(*pstate).Image2 = i
		q = where( (*pstate).Image2 ge sizes, ns)
		(*pstate).Image2Alt = ns
		case (*pstate).Image2Alt of
			0: begin
				opt = (*p).options
				i = (*pstate).Image2
				end
			1: begin
				opt = (*palt).options
				i = (*pstate).Image2 - n_el
				end
			2: begin
				opt = (*palt2).options
				i = (*pstate).Image2 - (n_el + n_el2)
				end
		endcase
		widget_control, (*pstate).max_id2, set_value=str_tidy( (*opt)[i].top)
		end
	3: begin
		(*pstate).Image3 = i
		q = where( (*pstate).Image3 ge sizes, ns)
		(*pstate).Image3Alt = ns
		case (*pstate).Image3Alt of
			0: begin
				opt = (*p).options
				i = (*pstate).Image3
				end
			1: begin
				opt = (*palt).options
				i = (*pstate).Image3 - n_el
				end
			2: begin
				opt = (*palt2).options
				i = (*pstate).Image3 - (n_el + n_el2)
				end
		endcase
		widget_control, (*pstate).max_id3, set_value=str_tidy( (*opt)[i].top)
		end
	else:
endcase

draw_ImageRGBs, pstate

s = legend_RGB_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------
pro OnSelect_ImageRGB_Max, Event, n

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, event.id, get_value=str
if fnumeric( str) eq 0 then begin
	warning,'OnSelect_ImageRGB_Max','Enter a legal numeric value (0-300).'
	return
endif
val = clip( long(str),0,300)

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
palt = (*pstate).palt
palt2 = (*pstate).palt2
good_alt = ptr_good(palt)
good_alt2 = ptr_good(palt2)
xanes_stack_test, p, xanes, n_el, el, el_xanes
n_el2 = 0
n_el3 = 0
if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
q = where( (*pstate).image ge sizes, ns)
(*pstate).Image1Alt = ns
q = where( (*pstate).image2 ge sizes, ns)
(*pstate).Image2Alt = ns
q = where( (*pstate).image3 ge sizes, ns)
(*pstate).Image3Alt = ns
not_alt = 0

case n of
	1: begin
		case (*pstate).Image1Alt of
			0: begin
				opt = (*p).options
				i = (*pstate).Image
				not_alt = 1
				end
			1: begin
				opt = (*palt).options
				i = (*pstate).Image - n_el
				end
			2: begin
				opt = (*palt2).options
				i = (*pstate).Image - (n_el + n_el2)
				end
		endcase
		(*opt)[i].top = val
		end
	2: begin
		case (*pstate).Image2Alt of
			0: begin
				opt = (*p).options
				i = (*pstate).Image2
				not_alt = 1
				end
			1: begin
				opt = (*palt).options
				i = (*pstate).Image2 - n_el
				end
			2: begin
				opt = (*palt2).options
				i = (*pstate).Image2 - (n_el + n_el2)
				end
		endcase
		(*opt)[i].top = val
		end
	3: begin
		case (*pstate).Image3Alt of
			0: begin
				opt = (*p).options
				i = (*pstate).Image3
				not_alt = 1
				end
			1: begin
				opt = (*palt).options
				i = (*pstate).Image3 - n_el
				end
			2: begin
				opt = (*palt2).options
				i = (*pstate).Image3 - (n_el + n_el2)
				end
		endcase
		(*opt)[i].top = val
		end
	else:
endcase

draw_ImageRGBs, pstate

if not_alt then begin
	notify, 'image-display', from=event.top
endif

s = legend_RGB_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_ImageRGB, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case !version.os_family of
	'MacOS': begin
		draw_trim = 15
		scr_trim = 21
		end
	'unix': begin
		draw_trim = 0
		scr_trim = 15
		end
	else: begin
		draw_trim = 0
		scr_trim = 15
		end
endcase

;w = (event.x - 8)
;h = (event.y - 64)
w = ((event.x - (*pstate).scr_xsize_off) > (256 + scr_trim)) < ((*pstate).width + scr_trim)
h = ((event.y - (*pstate).scr_ysize_off) > (64 + scr_trim)) < ((*pstate).height + scr_trim)
;print,'size: x,y=',event.x,event.y,'  w,h=',w,h

(*pstate).w = w
(*pstate).h = h
map_RGB_help, pstate

w = ((event.x - (*pstate).scr_xsize_off) > (256 + scr_trim)) < ((*pstate).width + scr_trim)
h = ((event.y - (*pstate).scr_ysize_off) > (64 + scr_trim)) < ((*pstate).height + scr_trim)

; Note that setting "draw_xsize=(*pstate).width, draw_ysize=(*pstate).height" is redundant,
; but necessary to keep the scrolling window working ...

widget_control, (*pstate).draw2, scr_xsize=w, scr_ysize=h, $
			draw_xsize=(*pstate).width+draw_trim, draw_ysize=(*pstate).height+draw_trim

(*pstate).w = w
(*pstate).h = h
(*pstate).xview = w - (*pstate).dw
(*pstate).yview = h - (*pstate).dh
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_ImageRGB, Event

COMPILE_OPT STRICTARR
widget_control, event.id, get_uvalue=message
if n_elements(message) lt 1 then return
if size(message,/tname) ne 'STRING' then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if event.enter eq 1 then begin
	widget_control, (*pstate).help, set_value=message
endif else begin
	s = legend_RGB_string(pstate)
	widget_control, (*pstate).help, set_value=s
endelse

done:
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

pro OnViewport_ImageRGB, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).xlow = event.x
(*pstate).ylow = event.y
print,'New View:', (*pstate).xlow, (*pstate).ylow
end

;-----------------------------------------------------------------

pro PostCreate_ImageRGB_Base, wWidget, Help1_Base, Help2_Base, $
					parent=parent, path=path, _EXTRA=_VWBExtra_, $
					pimages=pimages
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
		warning,'Postcreate_ImageRGB_base',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(path) lt 1 then path=''
if n_elements(parent) gt 0 then begin
  	child = widget_info( parent, /child)
	widget_control, child, get_uvalue=pstate_parent
endif else begin
	pstate_parent=ptr_new()
	parent = 0
endelse

w = 256
h = 256
if ptr_valid(pimages) then begin
  	if n_elements( *pimages) gt 0 then begin
		w = (*pimages).xsize
		h = (*pimages).ysize
	endif
endif
vx = 256
vy = 256
tlb = tlb_id( wWidget)

state = {	p:			pimages, $		; pointer to images
			palt:		ptr_new(), $	; pointer to alt compare images
			palt2:		ptr_new(), $	; pointer to alt2 compare images
			path:		ptr_new(path), $	; current path
			file:		'', $			; current file name
			plearn:		ptr_new(/alloc), $	; RGB display learn list
			display_file:	'', $		; RGB output list file name
			pexport:	ptr_new(/allocate_heap), $	; pointer to plot_RGB_image_select options
			pwiz:		ptr_new(/alloc), $		; pointer for Wizard return
			pstate_parent:	pstate_parent, $	; image window (parent) pstate
			tlb:		tlb, $			; top level base
			wid2:		0L, $			; draw 2 window id
			draw2:		0L, $			; draw 2 widget ID
			pix:		0L, $			; pixmap window id
			pix2:		0L, $			; pixmap 2 window id

			element_id1:	0L, $		; element droplist ID
			element_id2:	0L, $		; element droplist ID
			element_id3:	0, $		; element droplist ID
			max_id1:	0L, $			; element max text ID
			max_id2:	0L, $			; element max text ID
			max_id3:	0L, $			; element max text ID
			help1:		0L, $			; help 1 text widget ID
			help2:		0L, $			; help 2 text widget ID
			help:		0L, $			; current help text widget ID
			help1_base:	Help1_Base, $	; base to map for help 1
			help2_base:	Help2_Base, $	; base to map for help 2
			query1:		0L, $			; query (help) button 1 ID
			query2:		0L, $			; query (help) button 2 ID

			Image:		0, $			; current RED image
			Image2:		0, $			; current GREEN image
			Image3:		0, $			; current BLUE image
			Image1Alt:	0, $			; use Alt image compare RED (0:normal, 1:alt, 2:alt2)
			Image2Alt:	0, $			; use Alt image compare GREEN (0:normal, 1:alt, 2:alt2)
			Image3Alt:	0, $			; use Alt image compare BLUE (0:normal, 1:alt, 2:alt2)

			zoom:		0, $			; zoom factor
			left_button:	0, $		; flags left mouse button
			right_button:	0, $		; right mouse

			width:		w, $			; width of pixmap		(pixels)
			height:		h, $			; height of pixmap		(pixels)
			w:			0, $			; scr width of draw
			h:			0, $			; scr height of draw

			xview:		vx, $			; X view width			(pixels)
			yview:		vy, $			; Y view width			(pixels)
			xlow:		0, $			; X view low			(pixels)
			ylow:		0, $			; Y view low			(pixels)

			dw:			0, $			; width difference/border
			dh:			0, $			; height difference/border
			scr_xsize_off:	0, $		; resize offsets (see also map_help
			scr_ysize_off:	0, $
			tlb_width:	0, $			; tlb scr_xsize (initial)
			tlb_height:	0 }				; tlb scr_ysize (initial)


finish:
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
end

;-----------------------------------------------------------------

pro PostCreate_ImageRGB, wWidget, _EXTRA=_VWBExtra_

widget_control, wWidget, set_draw_view=[0,0]
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;

pro ImageRGB_eventcb
end
