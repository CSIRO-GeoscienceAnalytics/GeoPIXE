;
; IDL Event Callback Procedures
; time_amp_eventcb
;
;-----------------------------------------------------------------

pro time_amp_Analyze_Spline, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, done
if ptr_valid((*pstate).pmark) eq 0 then goto, done

qc = make_time_amp_mask( pstate)

if qc[0] ne -1 then begin
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	(*pstate).qc = ptr_new( qc, /no_copy)

	if ptr_valid( (*pstate).pspline) then ptr_free, (*pstate).pspline
	(*pstate).pspline = ptr_new( {elx:(*(*p).el)[(*pstate).time_amp_x], ely:(*(*p).el)[(*pstate).time_amp_y], $
						qc:(*pstate).qc, pmark:(*pstate).pmark }, /no_copy)

	notify, 'time_amp-analyze-spline', (*pstate).pspline, from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro time_amp_Clear_Spline, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

clear_time_amp_all_markers, pstate

notify, 'time_amp-analyze-clear', from=event.top
end

;-----------------------------------------------------------------

pro time_amp_Clone, Event, n

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
		warning,'time_amp_clone',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
obj = (*pstate).DevObj

if n_elements(n) lt 1 then n=1
geom = widget_info( event.top, /geometry)

xoffset = intarr(n)
yoffset = intarr(n)
for i=0L,n-1 do begin
	if (n eq 1) and (i eq 0) then begin
		xoffset[0] = geom.xoffset + 50
		yoffset[0] = geom.yoffset + 50
	endif else if i lt 3 then begin
		xoffset[i] = geom.xoffset + (i+1)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset
	endif else if i lt 7 then begin
		xoffset[i] = geom.xoffset + (i-3)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset + geom.scr_ysize+2
	endif else if i lt 11 then begin
		xoffset[i] = geom.xoffset + (i-7)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset + 2*(geom.scr_ysize+2)
	endif else if i lt 15 then begin
		xoffset[i] = geom.xoffset + (i-11)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset + 3*(geom.scr_ysize+2)
	endif
endfor

path = *(*pstate).path

for i=0L,n-1 do begin
	time_amp, group_leader=event.top, TLB=tlb, path=path, pimages=(*pstate).p, qregion=(*pstate).q, $
				xoffset=xoffset[i], yoffset=yoffset[i], device=obj

	register_notify, event.top, $
 				['images', $					; new images loaded somewhere
		  		'images-changed', $				; pass on notify of images changed
				'path', $						; new path
				'image-region-select', $		; region selected
				'image-analyze-q', $			; new q array
				'image-analyze-all-clear', $	; clear q array
				'time_amp-analyze-clear', $			; pass on notify of qc destined for image
				'time_amp-analyze-spline', $		; pass on notify of qc cleared destined for image
		  		'time_amp-display' $				; pass on notify of time_amp changed (e.g. colours)
				], from=tlb
endfor
end

;-----------------------------------------------------------------

pro time_amp_detector_slider, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
obj = (*pstate).DevObj

(*pstate).detector = event.value-1
val = ['all', adc_list_device(obj)]
widget_control, (*pstate).station_text, set_value=val[(*pstate).detector+1]

draw_time_amps, pstate

s = legend_time_amp_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro time_amp_detector_up, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
obj = (*pstate).DevObj

val = ['all', adc_list_device(obj)]
(*pstate).detector++

widget_control, (*pstate).station_text, set_value=val[(*pstate).detector+1]
widget_control, (*pstate).station_slider, set_value=(*pstate).detector+1

draw_time_amps, pstate

s = legend_time_amp_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro time_amp_detector_text, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
obj = (*pstate).DevObj

widget_control, (*pstate).station_text, get_value=v
val = ['all', adc_list_device(obj)]
if strlowcase(v) eq 'all' then begin
	(*pstate).detector = -1
endif else begin
	set_separators, '# '
	chop_string, v, sub, n_sub
	if n_sub ge 1 then begin
		(*pstate).detector = (fix(sub[0]) > 0) < 383
	endif
endelse
widget_control, (*pstate).station_text, set_value=val[(*pstate).detector+1]
widget_control, (*pstate).station_slider, set_value=(*pstate).detector+1

draw_time_amps, pstate

s = legend_time_amp_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro time_amp_Colours, Event, n

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(n) gt 0 then begin
	loadct, n, bottom=16, ncolors=100
endif else begin
	xloadct, group=event.top, /modal, bottom=16, ncolors=100
endelse

load_spec_colours
draw_time_amps, pstate
notify, 'time_amp-display', from=event.top
end

;-----------------------------------------------------------------

pro time_amp_Exit, Event

print,'time_amp_Exit ...'
OnKill_time_amp, event

end

;-----------------------------------------------------------------

pro time_amp_Extract, Event, DEVICE=DEVICE

COMPILE_OPT STRICTARR
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

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
		warning,'time_amp_Extract',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).events_text, get_value=v
(*pstate).events = long64(v)

obj = (*pstate).DevObj
if n_elements(device) ne 0 then begin
	if obj_valid( obj) then begin
		if obj->name() ne device then begin
			obj = obj_new(device)
		endif
	endif else obj = obj_new(device)
endif

F = file_requester( /read, /must_exist, filter = '*.*', $
			title='Select Maia 384 list files to scan for all spectra [Enter FIRST file]', $
			path=*(*pstate).path, group=event.top, fix_filter=0, /multiple)
if n_elements(F) eq 1 then begin
	F2 = file_requester( /read, /must_exist, filter = '*.*', $
		title='Select Maia 384 list files to scan for all spectra [Enter LAST file]', $
		path=extract_path(F[0]), group=event.top, fix_filter=0)
	if F2[0] ne '' then begin
		files = select_evt_files( F, F2, 1, '.', '')
	endif
endif else begin
   	files = F
	F2 = files[0]
endelse

if F2[0] ne '' then begin
	widget_control, /hourglass
	
	time_amp_evt, files, path=*(*pstate).path, events=(*pstate).events, /progress, device=obj, $
				group=event.top, time_amp=p

	if ptr_valid(p) then begin
		if ptr_valid ((*pstate).p) then ptr_free, (*pstate).p
		(*pstate).p = p
		if ptr_valid((*pstate).sources) then ptr_free, (*pstate).sources
		(*pstate).sources = ptr_new(files)
		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		(*pstate).q = ptr_new()

		draw_time_amps, pstate

		*(*pstate).path = extract_path( F[0])
		notify, 'path', (*pstate).path, from=event.top
	endif
endif

done:
end

;-----------------------------------------------------------------

pro time_amp_get_gaintrim, Event

COMPILE_OPT STRICTARR
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

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
		warning,'time_amp_get_gaintrim',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).events_text, get_value=v
(*pstate).events = long64(v)

obj = (*pstate).DevObj
if n_elements(device) ne 0 then begin
	if obj_valid( obj) then begin
		if obj->name() ne device then begin
			obj = obj_new(device)
		endif
	endif else obj = obj_new(device)
endif

F = file_requester( /read, /must_exist, filter = ['*.gaintrim.time.var','*.spec'], title='Select a Time gaintrim file', $
			path=*(*pstate).path, group=event.top, fix_filter=1)
if F[0] ne '' then begin
	g = get_gaintrim( F[0], energy=energy, time=time, error=error)
	if time eq 0 then begin
		warning,'time_amp_get_gaintrim','Not a TIME gaintrim file.'
		return
	endif
	if error then goto, done

	n = min( [n_elements(g[0,*]), n_elements((*(*pstate).ptrim).T.b)])
	for i=0,n-1 do begin
		(*(*pstate).ptrim0).T[i] = { a:reform(g[1,i]), b:reform(g[0,i]) }
	endfor
	(*(*pstate).ptrim).T[0:n-1] = (*(*pstate).ptrim0).T[0:n-1]
endif

done:
end

;-----------------------------------------------------------------

pro time_amp_save_gaintrim, Event

COMPILE_OPT STRICTARR
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

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
		warning,'time_amp_save_gaintrim',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	
	amin = min((*(*pstate).ptrim0).T.a)
	amax = max((*(*pstate).ptrim0).T.a)
	bmin = min((*(*pstate).ptrim0).T.b)
	bmax = max((*(*pstate).ptrim0).T.b)
	doubt_a = 0
	doubt_b = 0
	if (amin eq 1.) and (amax eq 1.) then doubt_a=1
	if (bmin eq 0.) and (bmax eq 0.) then doubt_b=1
	if doubt_a or doubt_b then begin
		warning,'time_amp_save_gaintrim',['Either the A or B Time gaintrim parameters','are all default values.', '',  $
					'Make sure you loaded the "existing" T gaintrim file before tweaking gaintrim parameters.']
	endif

	F = file_requester( /write, filter = ['*.gaintrim.time.var'], title='Save to a Time gaintrim file', $
				path=*(*pstate).path, group=event.top, fix_filter=1)
	if F[0] ne '' then begin
		openw, lun, F[0], /get_lun
		on_ioerror, bad_gaintrim
	
		printf, lun, '# gaintrim time'
		printf, lun, '# written by "time_amp_save_gaintrim"'
		printf, lun, '# file: '+F[0]
		printf, lun, '# comment: Time-Amplitude tweaks'
		printf, lun, '# '
		printf, lun, 'gaintrim.det[].tcoeff[0] 0'
		printf, lun, 'gaintrim.det[].tcoeff[1] 1'
	
		for i=0,n_elements((*(*pstate).ptrim).T)-1 do begin
			name = 'gaintrim.det[' + str_tidy(i) + '].tcoeff[]'
			if (*(*pstate).ptrim).T[i].a lt 0.001 then (*(*pstate).ptrim).T[i].a = 1.0
			printf, lun, format='(A,1x,F9.4,1x,F9.5)', name, (*(*pstate).ptrim).T[i].b, (*(*pstate).ptrim).T[i].a 
		endfor
		close_file, lun
	endif
	return

bad_gaintrim:
	if time eq 0 then begin
		warning,'time_amp_get_gaintrim','Not a TIME gaintrim file.'
		return
	endif
	return
end

;-----------------------------------------------------------------

pro time_amp_Invert_Colours, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

tvlct, ro,go,bo, /get
rr = reverse(ro[16:115])
gg = reverse(go[16:115])
bb = reverse(bo[16:115])
tvlct, rr,gg,bb, 16

load_spec_colours
draw_time_amps, pstate
notify, 'time_amp-display', from=event.top
end

;-----------------------------------------------------------------

pro time_amp_Linear_Luminance, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

tvlct, rr,gg,bb, /get
r = rr[16:115]
g = gg[16:115]
b = bb[16:115]

s = float(r) + float(2.2*g) + float(b)
if s[0] gt s[99] then begin
	L = (256./100.)*(99. - findgen(100))*4.2
endif else begin
	L = (256./100.)*findgen(100)*4.2
endelse
f = L/s
rr[16:115] = byte(f*r < 255)
gg[16:115] = byte(f*g < 255)
bb[16:115] = byte(f*b < 255)

tvlct, rr[16:115],gg[16:115],bb[16:115], 16

load_spec_colours
draw_time_amps, pstate
notify, 'time_amp-display', from=event.top
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

pro time_amp_Save_GIF, Event, png=png

COMPILE_OPT STRICTARR

warning,'time_amp_save_gif','not implemented yet'
return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(png) lt 1 then png=0
p = (*pstate).p
if ptr_valid(p) eq 0 then return

if png then begin
	ext = '.png'
	title = 'PNG file to Write'
endif else begin
	ext = '.gif'
	title = 'GIF file to Write'
endelse
s = strtrim( (*(*p).el)[(*pstate).time_amp_x],2) + '-' + strtrim( (*(*(*pstate).p).el)[(*pstate).time_amp_y],2)

file = find_file2( (*(*pstate).p).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
gif_file = strip_path( strip_file_ext( (*p).file)) + '-' + s + '-time_amp' + ext

F = file_requester( /write, filter = '*'+ext, path=path, $
			title=title, file = gif_file, group=event.top, fix_filter=1)
if F ne '' then begin
;	widget_control, /hourglass

;	b = make_time_amp_tvb( pstate, (*pstate).time_amp_x, (*pstate).time_amp_y)
	window, /free, xsize=n_elements(b[*,0]), ysize=n_elements(b[0,*]), /pixmap
	tpix = !d.window
	tv, b
	plot_time_amp_spline, pstate, /wide
	xyouts,0.8,0.02,(*(*p).el)[(*pstate).time_amp_x],charsize=1.8,charthick=8.0,color=16,/norm
	xyouts,0.01,0.8,(*(*p).el)[(*pstate).time_amp_y],charsize=1.8,charthick=8.0,color=16,/norm
	xyouts,0.8,0.02,(*(*p).el)[(*pstate).time_amp_x],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm
	xyouts,0.01,0.8,(*(*p).el)[(*pstate).time_amp_y],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm
	b = color_quan( tvrd(true=3), 3, rcol,gcol,bcol, colors=128)
	if n_elements(b) gt 1 then begin
		if png then begin
			write_png, F, b, rcol,gcol,bcol
		endif else begin
			write_gif, F, b, rcol,gcol,bcol
		endelse
	endif else begin
		print,'time_amp_save_GIF:  Nothing to save!  No TV byte data.'
	endelse
	if tpix ne 0 then wdelete, tpix
endif

done:
end

;-----------------------------------------------------------------

pro time_amp_Save_pileup, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_valid(p) eq 0 then return

file = find_file2( (*pstate).file)
if file eq '' then file = strip_file_ext(strip_path((*(*pstate).sources)[0]))
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
save_file = strip_path( strip_file_ext( file)) + '.pileup.var'

F = file_requester( /write, filter = ['*.pileup.var','*.txt'], path=path, $
			title='Pileup filename to write', file = save_file, group=event.top, fix_filter=1)
if F ne '' then begin
	widget_control, /hourglass
	spline_time_amp_vertices, pstate, x,y,n
	mask = bytarr(4096,4096)
	q = polyfillv( x,y, 4096,4096)
	if q[0] eq -1 then goto, done
	mask[*] = 0
	mask[q] = 1

	close, 1
	openw, 1, F
	printf, 1, '# Pileup limits file'
	printf, 1, '# Generated by Time_Amplitude display, ' + systime()
	printf, 1, '# Source: ' + (*(*pstate).sources)[0] + ' ...' 
	printf, 1, '#' 
	for i=0L,4095 do begin
		qi = where(mask[i,*] eq 1)
		if qi[0] ne -1 then begin
			tmin = min(qi)
			tmax = max(qi)
			printf, 1, 'pileup.energy[' + str_tidy(i), '].trange[] ' + str_tidy(tmin) + ' ' + str_tidy(tmax)
			(*pstate).limits[*,i] = [tmin,tmax]
		endif
	endfor
	printf, 1, '# end'
	close_file, 1

	(*pstate).pileup = ptr_new(mask, /no_copy)
	notify, 'time-amp-pileup', (*pstate).pileup, from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro time_amp_overlay_pileup, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

F = file_requester( /read, /must_exist, filter = ['*.pileup.var','*.txt','*.pileup'], $
			title='Select Maia pileup limits file to overlay', $
			path=*(*pstate).path, group=event.top, fix_filter=0)
if n_elements(F) eq 0 then return
if F[0] eq '' then return

pileup_limit = get_pileup(F[0], do_pileup=do_pileup)
if do_pileup eq 0 then return

n = min([4096,n_elements(pileup_limit[0,*])])
(*pstate).limits[*,0:n-1] = pileup_limit[*,0:n-1]

draw_time_amp_pileup, pstate
end

;-----------------------------------------------------------------

pro set_time_amp_pileup, tlb, pileup

COMPILE_OPT STRICTARR

child = widget_info( tlb, /child)
widget_control, child, get_uvalue=pstate

pileup = (*pstate).pileup

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

; Note: operation here depends on whether this is a clone or not.

pro OnDestroy_time_amp, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

print,'OnDestroy_time_amp: Cleanup pixemaps ...'

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

free_time_amp_state, pstate
return
end

;-----------------------------------------------------------------

pro OnKill_time_amp, Event

print,'OnKill_time_amp: destroy widget tree ...'
;cancel_notify, event.top

widget_control, event.top, /destroy
;heap_gc, /verbose
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

pro OnMove_time_amp_Low_X, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).LowX = float(event.value)
draw_time_amps, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_time_amp_High_X, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).HighX = float(event.value)
draw_time_amps, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_time_amp_Low_Y, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).LowY = float(event.value)
draw_time_amps, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_time_amp_High_Y, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).HighY = float(event.value)
draw_time_amps, pstate

done:
end

;-----------------------------------------------------------------

;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:from }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or 0).
;-----------------------------------------------------------------

pro OnNotify_time_amp, Event

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
		warning,'OnNotify_time_amp',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

;print,'OnNotify_time_amp: tag = ',event.tag

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
obj = (*pstate).DevObj

print,'******* time_amp_Evenctcb: notify: tag=',event.tag,' from=',event.from,' to=',event.top

case event.tag of

	'time_amp-display': begin	; returned from 'time_amp_select'

		; print,' Notified of display changed.'
		draw_time_amps, pstate
		end

	'path': begin
		if ptr_valid( event.pointer) eq 0 then goto, finish
		print,'time_amp: new path = ',(*event.pointer)
		*(*pstate).path = (*event.pointer)
		end

	'image-region-select': begin							; returned from 'time_amp_table'

		if ptr_valid( event.pointer) then begin
			p = (*event.pointer).pregion
			if ptr_valid(p) then begin
				if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
				if (*p).mode eq 1 then begin
					if ptr_valid( (*pstate).pmark) then ptr_free, (*pstate).pmark
					if ptr_valid( (*p).pmark[0]) then (*pstate).pmark = ptr_new( *(*p).pmark[0])
					q = where( (*p).elx eq *(*(*pstate).p).el)
					if q[0] ne -1 then begin
						(*pstate).time_amp_x = q[0]
						widget_control, (*pstate).element_idx, set_combobox_select=(*pstate).time_amp_x
					endif
					q = where( (*p).ely eq *(*(*pstate).p).el)
					if q[0] ne -1 then begin
						(*pstate).time_amp_y = q[0]
						widget_control, (*pstate).element_idy, set_combobox_select=(*pstate).time_amp_y
					endif
				endif else begin
					if ptr_valid( (*p).q) then (*pstate).q = ptr_new( *(*p).q)
				endelse
				draw_time_amps, pstate
			endif
		endif
		end

	'time_amp-analyze-clear': begin								; returned from other 'time_amp'

		clear_time_amp_all_markers, pstate
   		end

	'images-changed': begin											; returned from other apps

		pimg = event.pointer									; pointer to new 'images'
		if ptr_valid(pimg) eq 0 then goto, finish

		(*pstate).pimg = pimg
		(*pstate).nx = (*pimg).xsize
		(*pstate).ny = (*pimg).ysize
		(*pstate).file = (*pimg).file
		if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q

;		set_time_amp_view, pstate, event.top
		end



	'image-update-time': begin								; returned from 'image' clone

		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		if ptr_valid( event.pointer) then begin
			pq = event.pointer
			(*pstate).nx = (*pq).nx
			(*pstate).ny = (*pq).ny
			ptr_free, (*pstate).q
			if ptr_valid((*pq).q) then (*pstate).q = ptr_new( *(*pq).q)
				
			time_amp_evt, *(*pstate).sources, path=*(*pstate).path, events=(*pstate).events, /progress, device=obj, $
					group=group, mask={ q:(*pstate).q, nx:(*pstate).nx, ny:(*pstate).ny}, time_amp=pt

			if ptr_valid(pt) then begin
				if ptr_valid ((*pstate).p) then ptr_free, (*pstate).p
				(*pstate).p = pt
				draw_time_amps, pstate
			endif
		endif
		end

	'image-analyze-all-clear': begin						; returned from 'image' clone

		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		draw_time_amps, pstate
		end

	else: begin
	;	print,'OnNotify_time_amp: unknown tag = ',event.tag
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

pro OnRealize_time_amp_Help1, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help1 = wWidget
(*pstate).help = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_time_amp_Help2, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help2 = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_time_amp, wWidget

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

widget_control, wWidget, get_value=wid2
wset,wid2
(*pstate).wid2 = wid2
(*pstate).draw2 = wWidget
print,' wid2 = ', wid2

(*pstate).w = 600
(*pstate).h = 500
widget_control, (*pstate).draw2, scr_xsize=(*pstate).w, scr_ysize=(*pstate).h

(*pstate).dw =		16
(*pstate).dh =		0

window, /free, xsize=(*pstate).w, ysize=(*pstate).h, /pixmap
(*pstate).pix = !d.window
window, /free, xsize=(*pstate).w, ysize=(*pstate).h, /pixmap
(*pstate).pix2 = !d.window

if ptr_valid( (*pstate).p) then begin
	if n_elements( *(*pstate).p) gt 0 then begin
		draw_time_amps, pstate
	endif
endif
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_detector, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).detector_id = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).detector
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_detector_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).station_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_detector_text, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).station_text = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_events, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).events_text = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_Low_X_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Low_X_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_High_X_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).High_X_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_Low_Y_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Low_Y_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_High_Y_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).High_Y_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_Xlog, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Xlog_button = wWidget
widget_control, wWidget, set_button=(*pstate).logX
end

;-----------------------------------------------------------------

pro OnRealize_time_amp_Ylog, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Ylog_button = wWidget
widget_control, wWidget, set_button=(*pstate).logX
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

pro OnSelect_time_amp_detector, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).detector = event.index-1

draw_time_amps, pstate

s = legend_time_amp_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_time_amp_Xlog, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).logx = float(event.select)

;p = (*pstate).pmark
;conc_time_amp_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
;(*p).x = x
;(*p).y = y
draw_time_amps, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_time_amp_Ylog, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).logy = float(event.select)

;p = (*pstate).pmark
;conc_time_amp_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
;(*p).x = x
;(*p).y = y
draw_time_amps, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_time_amp_events, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, event.id, get_value=s
(*pstate).events = long64(s)

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

pro OnSize_time_amp, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (*pstate).skip_resize then return

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

w = ((event.x - (*pstate).scr_xsize_off) > (390 + scr_trim))
h = ((event.y - (*pstate).scr_ysize_off) > (280 + scr_trim))

(*pstate).w = w
(*pstate).h = h
;map_time_amp_help, pstate

w = ((event.x - (*pstate).scr_xsize_off) > (390 + scr_trim))
h = ((event.y - (*pstate).scr_ysize_off) > (280 + scr_trim))

widget_control, (*pstate).draw2, scr_xsize=w, scr_ysize=h

(*pstate).w = w
(*pstate).h = h
(*pstate).xview = w - (*pstate).dw
(*pstate).yview = h - (*pstate).dh

if (*pstate).pix ge 0 then wdelete, (*pstate).pix
window, /free, xsize=(*pstate).w, ysize=(*pstate).h, /pixmap
(*pstate).pix = !d.window

draw_time_amps, pstate

;if (*pstate).pix2 ge 0 then wdelete, (*pstate).pix2
;window, /free, xsize=(*pstate).w, ysize=(*pstate).h, /pixmap
;(*pstate).pix2 = !d.window
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_time_amp, Event

COMPILE_OPT STRICTARR
widget_control, event.id, get_uvalue=message
if n_elements(message) lt 1 then return
if size(message,/tname) ne 'STRING' then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if widget_info( (*pstate).help, /valid) eq 0 then goto, done

if event.enter eq 1 then begin
	widget_control, (*pstate).help, set_value=message
endif else begin
	s = legend_time_amp_string(pstate)
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

pro OnViewport_time_amp, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).xlow = event.x
(*pstate).ylow = event.y
end

;-----------------------------------------------------------------

pro PostCreate_time_amp_Base, wWidget, Help1_Base, Help2_Base, $
					parent=parent, path=path, _EXTRA=_VWBExtra_, $
					qregion=qregion, device=obj

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
		warning,'Postcreate_time_amp_base',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(parent) lt 1 then parent=0
if n_elements(path) lt 1 then path=''
if n_elements(obj) lt 1 then obj=obj_new('MAIA_DEVICE')

w = 400
h = 300
vx = 400
vy = 300

mark = {present:0, x:fltarr(7), y:fltarr(7), cx:fltarr(7), cy:fltarr(7)  }	; time amp curve

state = {	p:			ptr_new(), $	; pointer to E,T,X,Y,ste vector data
			pimg:		ptr_new(), $	; pointer to image data
			file:		'', $			; current image filename
			path:		ptr_new(path), $ ; current path
			sources:	ptr_new(''), $	; source EVT file names
			DevObj:		obj, $			; device object index
			nx:			0, $			; size of associated image
			ny:			0, $			; size of associated image

			q:			qregion, $		; Q region mask array
			qc:			ptr_new(), $	; qc mask array to return to Image
			pspline:	ptr_new(), $	; pointer to struct for notify 'time_amp-analyze-spline'
			px:			ptr_new(), $	; pointer to temp storage of spline points
			py:			ptr_new(), $

			pmark:		ptr_new( mark, /no_copy), $	; spline 10 marker
			id:			0, $			; id of point being moved
			a:			fltarr(3), $	; curve parameters

			wid2:		0L, $			; draw 2 window id
			draw2:		0L, $			; draw 2 widget ID
			pix:		0L, $			; pixmap window id
			pix2:		0L, $			; pixmap 2 window id
			position:	[0.14,0.11,0.97,0.93], $
			ax:			0.0, $			; conversion from pixels to (log) X axis data
			bx:			0.0, $
			ay:			0.0, $			; conversion from pixels to (log) Y axis data
			by:			0.0, $
			maxx:		0.0, $			; maxmimum X scale
			maxy:		0.0, $			; maxmimum Y scale
			xrange:		[0.0,0.0], $	; X range in data units
			yrange:		[0.0,0.0], $	; Y
			oldx:		0.0, $
			oldy:		0.0, $
			skip_resize: 0, $			; flags skip resize (to stop resize event after draw plot)

;			detector_id:	0L, $		; detector droplist ID
			station_text:	0L, $		; channel text ID
			station_slider:	0L, $		; channel slider ID
			help1:		0L, $			; help 1 text widget ID
			help2:		0L, $			; help 2 text widget ID
			help:		0L, $			; current help text widget ID
			help1_base:	Help1_Base, $	; base to map for help 1
			help2_base:	Help2_Base, $	; base to map for help 2
			Low_X_slider:	0L, $		; Low X scale slider ID
			High_X_slider: 0L, $		; High X scale slider ID
			Low_Y_slider:	0L, $		; Low Y scale slider ID
			High_Y_slider: 0L, $		; High Y scale slider ID
			xlog_button:	0L, $		; X log toggle button ID
			ylog_button:	0L, $		; Y log toggle button ID
			events_text:	0L, $		; events text ID

			detector:	-1, $			; current time_amp displayed
			events:		100000LL, $		; events to process
			pileup:		ptr_new(), $	; pointer to XY mask array
			limits:		lonarr(2,4096), $	; pileup limits
			logx:		0, $			; X log scale
			logy:		0, $			; Y log scale
			lowX:		0, $			; low X %
			highX:		100, $			; high X %
			lowY:		0, $			; low Y %
			highY:		100, $			; high Y %
			zoom:		0, $			; zoom factor
			left_button:	0, $		; flags left mouse button
			right_button:	0, $		; right mouse

			ptrim:		ptr_new({T: replicate({a:1.0,b:0.0},384)}), $	; T gain trim values
			ptrim0:		ptr_new({T: replicate({a:1.0,b:0.0},384)}), $	; T gain trim values

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

pro PostCreate_time_amp, wWidget, _EXTRA=_VWBExtra_

widget_control, wWidget, set_draw_view=[0,0]
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;

pro time_amp_eventcb
end
