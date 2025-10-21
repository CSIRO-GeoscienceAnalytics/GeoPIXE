;
; IDL Event Callback Procedures
; image_eventcb
;
;-----------------------------------------------------------------

pro Clear_All_Image_Marks, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

clear_all_markers, pstate

if ptr_valid( (*pstate).pmode) then ptr_free, (*pstate).pmode
(*pstate).pmode = ptr_new( (*pstate).analyze_mode)
notify, 'image-analyze-mode', (*pstate).pmode, from=event.top

if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
(*pstate).ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
notify, 'image-analyze-type', (*pstate).ptype, from=event.top

notify, 'image-analyze-all-clear', from=event.top
end

;-----------------------------------------------------------------

pro Clear_Image_Corr, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
(*pstate).corr_mode = 0
draw_images, pstate

notify, 'image-corr-clear', from=event.top
end

;-----------------------------------------------------------------

pro Image_Analyze, Event, throttle=throttle, get_stats=get_stats, uniform_element=uniform_element, error=err

COMPILE_OPT STRICTARR
if n_elements(uniform_element) lt 1 then uniform_element='nothing'
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).corr_mode = 0
err = 1
if (*pstate).analyze_type[(*pstate).analyze_mode] gt 0 then begin
    analyze_image, pstate, throttle=throttle, get_stats=get_stats, uniform_element=uniform_element, error=err

    if ((*pstate).analyze_type[0] eq 4) or ((*pstate).analyze_type[0] eq 3) or $
    	((*pstate).analyze_type[0] eq 8) or ((*pstate).analyze_type[0] eq 9) then begin
       notify, 'spectra', (*pstate).pline, from=event.top
    endif

    if ptr_valid((*pstate).pq) then ptr_free, (*pstate).pq
    (*pstate).pq = ptr_new( {q:(*pstate).q, nx:(*(*pstate).p).xsize, ny:(*(*pstate).p).ysize} )
    notify, 'image-analyze-q', (*pstate).pq, from=event.top
    notify, 'image-update-time', (*pstate).pq, from=event.top

    if throttle then begin
       notify, 'image-region-throttle', from=event.top
    endif else begin
        notify, 'image-results', from=event.top
    endelse
endif else begin
    warning,'Image_Analyze','Need to select and shape a region first'
endelse
end

;-----------------------------------------------------------------

pro Image_Blog_Browser, Event

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

; This loads and runs 'blog_browser.sav' in runtime

restore, geopixe_root+'blog_browser.sav'
blog_browse, group=event.top
end

;--------------------------------------------------------------------------

;	Build vector of image process operations 'ops' on images
;	Called from Notify 'image-process' and wizard 'wizard_batch_ops'

pro image_build_op, i, el, uv, history, ops

;	Build next operation for element 'i' (elements = 'el') using 'history'.
;	'uv contains the operations list from 'image_process'.
;	Vector 'ops' will be created and added to here.
 
	COMPILE_OPT STRICTARR
	list = uv.list
	for k=0L,n_elements(list)-1 do begin
		j = strsplit( list[k], ' ', count=nj)
		if gnumeric( strmid(list[k], j[nj-1])) then begin
			list[k] = strmid( list[k], 0, j[nj-1]-1)
		endif
	endfor

	s = hide_embedded( history, ' ')
	m = locate('[',s)
	if m ge 0 then s = strmid(s,0,m)
	sub = strsplit( s, ',:()', /extract, count=n_sub)
	sub = strtrim(sub,2)
	hist = strlowcase(sub[0])	

	arg = 0.0
	sub2 = strsplit( sub[n_sub-1], ' ,=', /extract, count=n_sub2)
	if gnumeric(sub2[n_sub2-1]) then arg = float(sub2[n_sub2-1])
	OK = 0

	if hist eq 'plugin' then begin
		op = {mode:1, image:i, name:hist, valid:1, operation:sub[1],  $
					arg1:0.0, filter:'', arg2:'' }
		OK = 1
	endif else if hist eq 'inter-element' then begin
		sq = hide_embedded( sub[3], ' ', /unhide)
		op = {mode:1, image:i, name:hist, valid:1, operation:sub[1],  $
					arg1:arg, filter:sq, arg2:sub[2] }
		OK = 1
	endif else begin

;		Look for other processing commands, as listed in the 'image_process' routine list.
;		"*" indicates an operation that gets applied to all element planes.
;		If one particular element must be displayed to do this, it is put in brackets "[]".
;		Else, this is done for element i=0.

		hist = history
		skip_el = 0
		if strmid(hist,0,1) eq '*' then begin
			l1 = locate('[',hist)
			l2 = locate(']',hist)
			if (l1 ge 0) and (l2 ge 0) and (l2 gt l1+1) then begin
				tag = strmid( hist,l1+1,l2-l1-1)
				if el[i] ne tag then skip_el=1
			endif
		endif

		q = where( (strlowcase(list) eq strlowcase(sub[0])) and (abs(uv.arg-arg) lt 0.01))
		m = q[0]
		if m ne -1 then begin
			op = {mode:1, image:i, name:'process', valid:1, operation:uv.routine[m],  $
						arg1:uv.arg[m], filter:'', arg2:'' }
			OK = (skip_el eq 0)
		endif
	endelse
	if OK then begin
		help, op
		if typevar(ops) ne 'STRUCT' then begin
			ops = op
		endif else begin
			ops = [ops, op ]
		endelse
	endif
	return
end

;-----------------------------------------------------------------

;	Do image process operations 'ops' on images
;	Called from Notify 'image-process' and wizard action 'image-operations'

pro image_do_operations, event, ops

	COMPILE_OPT STRICTARR
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	widget_control, /hourglass
	n_ops = n_elements(ops)

	for j=0L,n_ops-1 do begin
		i = (ops[j].mode eq 1) ? ops[j].image : (*pstate).image
		help, ops[j]

		case strlowcase(ops[j].name) of
			'plugin': begin
				p = (*pstate).p
				call_procedure, ops[j].operation, p, i, history=history
				set_image_minmax, p, (*p).image, (*p).options

				hist = 'Plugin ' + ops[j].operation
				if n_elements(history) gt 0 then hist = hist + ', ' + history
				add_history, (*p).history, i, hist
				end

			'inter-element': begin
;					Note: format of history (e.g. "()") is assumed for image processing Get function
;					and must be consistent in routine doing the processing/adding the history note.
;
;					Declared also in 'interelement_operations' and used in 'interelement_filter', 'interelement_transform'

				operations = define(/operations)
				filters = ['None','Median filter 2','Median filter 3','Median filter 5','Median filter 10','Gaussian filter 1.0','Gaussian filter 1.5','Gaussian filter 2.0','Gaussian filter 3.0','Gaussian filter 5.0','Gaussian filter 10.0']

;					e.g. "Inter-Element: Subtract Ni (Median filter 3) * 0.04"
; 					e.g. ops[j].operation  'Subtract'
;								arg2       'Ni'
;								filter     'Median filter 3'
;								arg1       0.04

				p = (*pstate).p
				k = where( ops[j].arg2 eq *(*p).el, nk)
				if nk eq 0 then begin
					print,'OnNotify_image "image-process": inter-element source "'+ops[j].arg2+'" not found.'
				endif else begin
					(*p).modify.source = k
					*(*p).modify.image = (*(*p).image)[*,*,k]
					qf = where( ops[j].filter eq filters, nqf)
					if nqf eq 0 then begin
						warning, 'OnNotify_image "image-process"',['"inter-element" Filter "'+ops[j].filter,'" unknown, element='+(*(*p).el)[i]]
					endif else begin
						(*p).modify.filter = qf[0]
						interelement_filter, p, error=err
						if err then warning, 'OnNotify_image "image-process"',['"inter-element" Error for Filter "'+ops[j].filter,'", element='+(*(*p).el)[i]]
					endelse
					(*p).modify.target = i
					(*p).modify.scale = ops[j].arg1
					(*p).modify.strength = 1.0
					qo = where( ops[j].operation eq operations, nqo)
					if nqo eq 0 then begin
						warning, 'OnNotify_image "image-process"',['"inter-element" Operation "'+ops[j].operation,'" unknown, element='+(*(*p).el)[i]]
					endif else begin
						(*p).modify.operation = qo[0]
						img = interelement_transform( p, i, error=err)
						if err eq 0 then begin
							(*(*p).image)[*,*,(*p).modify.target] = img
							s = operations[(*p).modify.operation] + ' ' + (*(*p).el)[(*p).modify.source] + ' ('+ops[j].filter+') * ' + str_tidy((*p).modify.scale * (*p).modify.strength)
							add_history, (*p).history, (*p).modify.target, 'Inter-Element: '+s
						endif
					endelse
					(*p).modify.filter = 0
					(*p).modify.operation = 0
					(*p).modify.scale = 1.0
				endelse
				end
			'process': begin
;					Note: format of history (e.g. "*" at start or "[tag]") is assumed for image processing Get function
;					and must be consistent in routine doing the processing/adding the history noe.
;
				call_procedure, ops[j].operation, event, ops[j].arg1, select=i, /silent
				end
			else:
		endcase
	endfor

	return
end

;-----------------------------------------------------------------

pro image_Shape, Event, shape=shape

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(shape) lt 1 then shape=1

clear_mark, pstate
(*pstate).analyze_type[(*pstate).analyze_mode] = shape
wset, (*pstate).wid2
plot_mark, pstate
widget_control, (*pstate).analyze_type_id, set_combobox_select=shape

if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
(*pstate).ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
notify, 'image-analyze-type', (*pstate).ptype, from=event.top
end

;-----------------------------------------------------------------

pro image_Clone, Event, n

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
       warning,'Image_clone',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

path = *(*pstate).path
dpath = *(*pstate).dpath
test = (*pstate).test
debug = (*pstate).debug
xanes = (*pstate).xanes

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

for i=0L,n-1 do begin
    gimage, group_leader=event.top, TLB=tlb, /clone, path=path, dpath=dpath, $
          xoffset=xoffset[i], yoffset=yoffset[i], test=test, debug=debug, xanes=xanes

    register_notify, event.top, $
		['images', $				; new images loaded somewhere
		'image-clone', $			; new images from Image clone
		'spectra', $				; new spectra loaded
		'spectrum-display', $		; new spectra display chaange
		'path', $					; new path
		'dpath', $					; new raw data path
		'mark-e', $					; mark line energy from Identify (pass to setup-filter)
		'correct-image-pileup', $	; subtract pileup from images
		'corr-analyze-clear', $		; pass on notify of qc clear
		'image-corr-q', $			; pass on notify of qc set by corr
		'image-corr-clear', $		; clear corr pixels in other images
		'image-display', $			; image display needs updating (smooth...)
		'image-line', $				; new line results (pass on)
		'image-results', $			; new conc results (pass on)
		'image-update-time', $		; q region vector --> Time Amp
		'image-analyze-type', $		; analyze type changed
		'image-analyze-mode', $		; analyze mode chanhged
		'image-analyze-clear', $	; shape cleared off screen
		'image-analyze-mark', $		; shape has changed
		'image-analyze-q', $		; q region vector has changed
		'image-analyze-all-clear', $  ; clear all marker settings and droplists
		'image-region-clear', $		; clear current marker, just prior to select
		'image-region-select', $	; pass on notify of image_table region-select
		'image-regions', $				; regions pointers, if valid w/ spectra
		'image-region-throttle', $	; pass on notify of image region throttle to image_table
		'image-spectrum-throttle', $	; pass on notify of image_table spectrum throttle to spectrum_display
		'image-kill-regions-all-planes' $ ; image_table clear region in all planes
		], from=tlb
endfor
end

;-----------------------------------------------------------------

pro Image_Corr_Display, Event, energy=energy

COMPILE_OPT STRICTARR
if n_elements(energy) eq 0 then energy=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then return

if energy then begin
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	if xanes then return
	if (*p).energy_proxy_axis eq 0 then return

	copy_pointer_data, p, p2, /init
	
	nx = (*p).xsize
	ny = (*p).ysize
	nel = (*p).n_el
	
	zplane = (*pstate).image							; Cr XE map

; Smooth both non-E and E axes ...

	image = reform((*(*p).image)[*,*,zplane])
	image = smooth( image,3)

; Rearrange and smooth just non-E some more ...

	case (*p).energy_proxy_axis of
		1: begin
			image2 = fltarr(1,ny,nx)
			for i=0,nx-1 do begin
				t = image[i,*]
				image2[0,*,i] = t						; smooth( t, 3)
			endfor
			
			ptr_free, (*p2).image
			(*p2).image = ptr_new( image2, /no_copy)
			
			(*p2).xsize = 1
			(*p2).ysize = ny
			(*p2).original_xsize = 1
			(*p2).original_ysize = ny
			(*p2).n_el = nx
			ptr_free, (*p2).el
			(*p2).el = ptr_new( str_tidy(*(*p).px_coords, places=4) )
			end
		2: begin
			image2 = fltarr(nx,1,ny)
			for i=0,ny-1 do begin
				t = image[*,i]
				image2[*,0,i] = smooth( t, 5)
			endfor
			
			ptr_free, (*p2).image
			(*p2).image = ptr_new( image2, /no_copy)
			
			(*p2).xsize = nx
			(*p2).ysize = 1
			(*p2).original_xsize = nx
			(*p2).original_ysize = 1
			(*p2).n_el = ny
			ptr_free, (*p2).el
			(*p2).el = ptr_new( str_tidy(*(*p).py_coords, places=4) )
			end
	endcase
	
	corr, group_leader=event.top, path=*(*pstate).path, pimages=p2, TLB=tlb, title='Energy Associations'
	
endif else begin
	if (*pstate).xanes then begin
		title='Energy Associations'
	endif else begin
		title='Associations'
	endelse

	corr, group_leader=event.top, path=*(*pstate).path, pimages=(*pstate).p, qregion=(*pstate).q, TLB=tlb, title=title
endelse

register_notify, event.top, $
          ['corr-display',  $			; display altered
          'path', $						; new path
          'corr-analyze-clear', $		; pass on notify of qc destined for image
          'corr-analyze-spline' $		; pass on notify of qc cleared destined for image
          ], from=tlb
end

;-----------------------------------------------------------------

pro Image_interelement_operations, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

interelement_operations, (*pstate).p, group_leader=event.top, TLB=TLB

register_notify, event.top, [ $
		'image-display' $				; image display needs updating 
			], from=tlb
end

;-----------------------------------------------------------------

pro Image_PCA_cluster_Display, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

PCA_cluster, group_leader=event.top, path=*(*pstate).path, pimages=(*pstate).p, qregion=(*pstate).q, TLB=tlb
register_notify, event.top, $
          ['corr-display',  $          	; display altered
          'path', $              				; new path
          'corr-analyze-clear', $        ; pass on notify of qc destined for image
          'corr-analyze-spline' $        ; pass on notify of qc cleared destined for image
          ], from=tlb
end

;-----------------------------------------------------------------

pro Image_dump_binary, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid((*pstate).p) then begin
    if ptr_valid((*(*pstate).p).image) eq 0 then goto, nothing
endif else goto, nothing

title = 'Binary file to dump'
file = find_file2( (*pstate).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
file = strip_file_ext((*(*pstate).p).file[0])
file = strip_path(file) + '.bin'

F = file_requester( /write, filter='*.bin', path=path, $
				title=title, file=file, dialog_parent=event.top, fix_filter=1)
if F ne '' then begin
    widget_control, /hourglass
    *(*pstate).path = extract_path(F[0])
    
	write_geopixe_image, (*pstate).p, F, error=err, /dump
endif

done:
    return

nothing:
    warning,'image_dump_binary','There is no image data to dump!'
    goto, done
end

;-----------------------------------------------------------------

pro Image_Correct_Yield, Event, big=big, small=small

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(big) lt 1 then big=0
if n_elements(small) lt 1 then small=0

Correct_Yield, (*pstate).p, group_leader=event.top, TLB=tlb, $
       pars=(*pstate).pcorrect, path=*(*pstate).path, big=big, small=small

register_notify, event.top, ['image-display', $     ; display changes (correct)
                   'images'], $        ; new images (project)
                   from=tlb
end

;-----------------------------------------------------------------

pro image_Clear, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (*pstate).realtime then begin
	(*(*(*pstate).p).image)[*,*,*] = 0
endif else begin
	free_images, (*pstate).p
endelse
draw_images, pstate
end

;-----------------------------------------------------------------

pro image_Colours, Event, n

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(n) gt 0 then begin
    if n ge 0 then begin
       loadct, n, bottom=16, ncolors=100
    endif else begin
       case n of
         -1: begin
	          loadct, 5, bottom=16, ncolors=100
	          tvlct, r,g,b, /get
	          q1 = indgen(100)
	          q2 = round(one_sig_figure(q1-5)+5)
	          q = where(q1 lt 10)
	          if q[0] ne -1 then q2[q]=0
	          q = where((q1 ge 10) and (q1 lt 20))
	          if q[0] ne -1 then q2[q]=15
	          q = where(q1 ge 90)
	          if q[0] ne -1 then q2[q]=99
	          r2 = r
	          g2 = g
	          b2 = b
	          r2[16+q1] = r[16+q2]
	          g2[16+q1] = g[16+q2]
	          b2[16+q1] = b[16+q2]
	          tvlct, r2,g2,b2
	          end
         else:
       endcase
    endelse
endif else begin
    xloadct, group=event.top, /modal, bottom=16, ncolors=100
endelse

load_spec_colours
draw_images, pstate
notify, 'image-display', from=event.top

;colours, group_leader=event.top, TLB=tlb
;register_notify, event.top, 'colours', from=tlb
end

;-----------------------------------------------------------------

pro Image_Colour_blind, Event, on

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case on of
	0: begin
       loadct, 5, bottom=16, ncolors=100
	   load_spec_colours, red_blind=0
		end
	1: begin
       loadct, 1, bottom=16, ncolors=100
	   load_spec_colours, red_blind=1
		end
endcase
draw_images, pstate
notify, 'image-display', from=event.top
notify, 'spectrum-display', from=event.top
end

;-----------------------------------------------------------------

pro Image_Edit_Filters, Event

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

pro Image_Edit_Detectors, Event

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

pro Image_Export, Event, cgm=cgm, wmf=wmf, all=all, png=png, jpeg=jpeg

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(cgm) eq 0 then cgm=0
if n_elements(wmf) eq 0 then wmf=0
if n_elements(png) eq 0 then png=0
if n_elements(jpeg) eq 0 then jpeg=0
if n_elements(all) eq 0 then all=0
if ptr_good( (*pstate).p) eq 0 then goto, done

p = (*pstate).p
xanes_stack_test, p, xanes, n_el, el_names, el_xanes

if n_elements( *(*pstate).pexport) ge 1 then begin
    old_els = (*(*pstate).pexport).el
    old_enable = (*(*pstate).pexport).el_enable
endif else begin
    old_els = el_names
    old_enable = replicate(1,n_elements(el_names))
endelse

crop = {x:[0,(*(*pstate).p).xsize-1], y:[0,(*(*pstate).p).ysize-1] }
;if (*pstate).corr_mode eq 0 then begin
    if (*pstate).analyze_mode eq 0 then begin            ; include mode only
       mark_vertices, pstate, x1,y1, n
       if n gt 0 then begin
         x = round(x1)
         y = round(y1)
         crop.x = [min(x),max(x)]
         crop.y = [min(y),max(y)]
       endif
    endif
;endif

if all then begin
    select = plot_image_select( event.top, el_names, old_select=*(*pstate).pexport, $
         cgm=cgm, wmf=wmf, png=png, jpeg=jpeg, path=*(*pstate).path, image_pstate=pstate, crop=crop )
endif else begin
    select = plot_image_select( event.top, el_names, just_one=(*pstate).image, $
         cgm=cgm, wmf=wmf, png=png, jpeg=jpeg, old_select=*(*pstate).pexport, path=*(*pstate).path, image_pstate=pstate, $
         crop=crop )
endelse
if select.error then goto, done

;goto, done     ; use this bypass if testing plot_image_select without /modal, etc.

qselect = where(select.el_enable eq 1)
if qselect[0] eq -1 then goto, done

name = 'Select File Root for image save'
if n_elements(qselect) gt 1 then name='Select File Root to Save Images as '

file = strip_file_m(strip_file_ext(strip_path((*pstate).file)))
if file eq '' then file = strip_file_ext(strip_path((*p).source))
widget_control, hourglass=1


if select.plot.type eq 'CGM' then begin
    file = file + '.cgm'
    file = file_requester( /write, filter='*.cgm', path=*(*pstate).path, $
         			file=file, title=name+'CGM', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = '-plot'
	if select.plot.centroids.on then begin
		els = strtrim( select.plot.centroids.element, 2)
		add = '-centroids' + '-'+els
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_images, pstate, /cgm, file=file, options=select.plot, select=select.el_enable, crop=crop

endif else if select.plot.type eq 'METAFILE' then begin
    file = file + '.wmf'
    file = file_requester( /write, filter='*.wmf', path=*(*pstate).path, $
         file=file, title=name+'WMF', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = '-plot'
	if select.plot.centroids.on then begin
		els = strtrim( select.plot.centroids.element, 2)
		add = '-centroids' + '-'+els
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_images, pstate, /wmf, file=file, options=select.plot, select=select.el_enable, crop=crop

endif else if select.plot.type eq 'PNG' then begin
    file = file + '.png'
    file = file_requester( /write, filter='*.png', path=*(*pstate).path, $
         file=file, title=name+'PNG', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = '-plot'
	if select.plot.centroids.on then begin
		els = strtrim( select.plot.centroids.element, 2)
		add = ['-centroids' + '-'+els, add]
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_images, pstate, /png, file=file, options=select.plot, select=select.el_enable, crop=crop

endif else if select.plot.type eq 'JPEG' then begin
    file = file + '.jpg'
    file = file_requester( /write, filter='*.jpg', path=*(*pstate).path, $
         file=file, title=name+'JPEG', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = '-plot'
	if select.plot.centroids.on then begin
		els = strtrim( select.plot.centroids.element, 2)
		add = ['-centroids' + '-'+els, add]
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_images, pstate, /jpeg, file=file, options=select.plot, select=select.el_enable, crop=crop

endif else if select.plot.type eq 'PS' then begin
    file = file + '.eps'
    file = file_requester( /write, filter='*.eps', path=*(*pstate).path, $
         file=file, title=name+'EPS', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
	add = '-plot'
	if select.plot.centroids.on then begin
		els = strtrim( select.plot.centroids.element, 2)
		add = '-centroids' + '-'+els
	endif
	file = strip_file_keys( file, remove=['-plot','-enhance','-centroids'], element=4, add=add)

    plot_images, pstate, /eps, file=file, options=select.plot, select=select.el_enable, crop=crop

endif else begin

    plot_images, pstate, options=select.plot, select=select.el_enable, crop=crop
endelse
widget_control, hourglass=0
if  select.plot.type eq 'CGM' then warning,'Image_Export',['Note that Windows may fail to import CGM files into Office programs.', $
		'This is not a GeoPIXE problem, but a new security setting in Windows.', $
		'See solutions to this problem on the web (e.g. google "Windows Office Powerpoint import CGM problems")', $
		'and in the GeoPIXE "Help" directory.'], /info

if all then begin
    *(*pstate).pexport = select
endif else begin
    *(*pstate).pexport = {el:old_els, el_enable:old_enable, plot:select.plot}
endelse

done:
end

;-----------------------------------------------------------------

pro Image_Export_CSV, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;if n_elements(all) eq 0 then all=0
if ptr_valid( (*pstate).p) eq 0 then goto, done

file = strip_file_ext((*pstate).file) + '.csv'
file = file_requester( /write, filter=['*.csv','*.txt'], path=*(*pstate).path, $
         file=file, title='Export Image - select filename stub', group=event.top, /fix_filter)
if strlen(file) lt 1 then goto, done
file = strip_file_m( file, /element)

widget_control, /hourglass
export_images_csv, pstate, file=file, /first, event=event

done:
end

;-----------------------------------------------------------------

; Export XANES stack energies to a file (in keV units)

pro Image_Export_XANES_energies, event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if ptr_good( (*pstate).p) eq 0 then return
p = (*pstate).p
if ptr_good( (*p).pz_coords) eq 0 then goto, bad_pz
ff = strip_file_ext( (*p).file) + '-energies.csv'

	F = file_requester( /write, filter='*.csv', file=ff, $
	         title='Select XANES energies file to write', path=*(*pstate).path, group=event.top, $
	         fix_filter=0, multiple=0)
	if F[0] ne '' then begin
		energy = *(*p).pz_coords
		on_ioerror, bad_file
		openw, lun, F[0], /get_lun
		printf, lun, '# XANES energies exported from stack file = '+(*p).file
		for i=0,n_elements(energy)-1 do begin
			printf, lun, energy[i]
		endfor
		close_file, lun
	endif
	return
	
bad_pz:
	warning,'Image_Export_XANES_energies','Bad array of stack Z (energy) values.'
	return
bad_file:
	warning,'Image_Export_XANES_energies','File I/O error, file: '+F[0]
	close_file, lun
	return
end

;-----------------------------------------------------------------

pro Image_Export_Zarr_GCF, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if ptr_valid( (*pstate).p) eq 0 then goto, nothing

file = find_file2( (*pstate).file)
if file eq '' then  file=(*(*pstate).p).file
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path

file = file_requester( /read, filter='*.dai', file=file[0], $
	path=path, group=event.top, /translate, updir=4, $
	title='Select the Image file', fix_filter=1, /skip_if_exists)
if file eq '' then goto, done
path = extract_path( file[0])

output = strip_file_ext( file) + '.zarr'

file2 = strip_file_ext(file[0]) + '.gcf'

F = file_requester( /write, filter = '*.gcf', file=file2, $ 
         title='GeoPIXE Command File to write', group=event.top, fix_filter=1)
if file eq '' then goto, done

args = [ 'export_zarr', $
		'files=' + stringify( file), $
		'output=' + stringify( output) ]
geopixe_gen_commands, F, args

done:
    return

nothing:
    warning,'Image_Export_Zarr_GCF','There is no image data to save!'
    goto, done
end

;-----------------------------------------------------------------

pro Image_EVT, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

EVT, group_leader=event.top, TLB=tlb, pars=(*pstate).pevt, path=*(*pstate).path, $
         test=(*pstate).test, dpath=*(*pstate).dpath, pprefs=(*pstate).pprefs, /image

register_notify, event.top, $
		['images', $					; new images loaded
		'path', $						; new path
		'dpath', $						; new raw data path
		'spectra', $					; pass on notify of new spectra loaded
		'batch-operations-open', $		; open Image operations window
		'batch-rgb-open', $				; open RGB Image window
		'batch-filter', $				; digital filter from Batch_Sort
		'batch-save' $					; save images/HTML from Batch_Sort
		], from=tlb
end

;-----------------------------------------------------------------

pro image_Exclude, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ((*pstate).analyze_type[(*pstate).analyze_mode] eq 0) then goto, done

clear_mark, pstate, /both
(*pstate).analyze_mode = 1
wset, (*pstate).wid2
plot_mark, pstate

widget_control, (*pstate).analyze_mode_id, set_combobox_select=1

if ptr_valid( (*pstate).pmode) then ptr_free, (*pstate).pmode
(*pstate).pmode = ptr_new( (*pstate).analyze_mode)
notify, 'image-analyze-mode', (*pstate).pmode, from=event.top

widget_control, (*pstate).analyze_type_id, set_combobox_select=(*pstate).analyze_type[(*pstate).analyze_mode]

if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
(*pstate).ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
notify, 'image-analyze-type', (*pstate).ptype, from=event.top

done:
end

;-----------------------------------------------------------------

pro image_Exit, Event

OnKill_image, event

end

;-----------------------------------------------------------------

pro Image_History_window, Event, stats=stats

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(stats) eq 0 then stats=0

image_history, group_leader=event.top, TLB=tlb, pimages=(*pstate).p, $
			show=(*pstate).image, path=*(*pstate).path, stats=stats

end

;-----------------------------------------------------------------

pro image_Image_Operations, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

image_process, group_leader=event.top, TLB=tlb, plugins=(*pstate).plugins, $
          path=(*pstate).path, get_file=(*pstate).get_file, pimage=(*pstate).p

(*pstate).operations = 1

register_notify, event.top, ['image-process', $
              'image-display', $
              'image-operations-closed', $	; closed the operations window
              'batch-filter', $				; forward to Image Operations
              'done-filter' $				; batch filter done
              ], from=tlb
end

;-----------------------------------------------------------------

pro Image_Image_RGB, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

geom = widget_info( event.top, /geometry)
xoffset = geom.xoffset + 150
yoffset = geom.yoffset + 150

ImageRGB, group_leader=event.top, path=*(*pstate).path, pimages=(*pstate).p, $
		TLB=tlb, xoffset=xoffset, yoffset=yoffset

(*pstate).RGB_open = 1

register_notify, event.top, $
          ['path', $				; new path
		  'image-display', $		; image re-display
		  'image-rgb-closed', $		; closed the RGB window
          'batch-save' $            ; save images/HTML from Batch_Sort
          ], from=tlb
end

;-----------------------------------------------------------------

pro image_Include, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

clear_mark, pstate, /both
(*pstate).analyze_mode = 0
wset, (*pstate).wid2
plot_mark, pstate

widget_control, (*pstate).analyze_mode_id, set_combobox_select=0

if ptr_valid( (*pstate).pmode) then ptr_free, (*pstate).pmode
(*pstate).pmode = ptr_new( (*pstate).analyze_mode)
notify, 'image-analyze-mode', (*pstate).pmode, from=event.top

widget_control, (*pstate).analyze_type_id, set_combobox_select=(*pstate).analyze_type[(*pstate).analyze_mode]

if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
(*pstate).ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
notify, 'image-analyze-type', (*pstate).ptype, from=event.top
end

;-----------------------------------------------------------------

pro Image_import, Event, type=type

COMPILE_OPT STRICTARR
common c_image_load_temp, p
if n_elements(type) lt 1 then type=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

ext = ['*.bmp','*.*','*.pef','*.png','*.tif']
message = ['Select BMP image files to import','Select PNC-CAT image file to import','Select PEF image file to import','Select PNG image file to import','Select TIF image file to import']
multiple = [1,0,0,1,1]

F = file_requester( /read, filter = ext[type], $
         title=message[type], path=*(*pstate).path, group=event.top, /image, $
         fix_filter=0, multiple=multiple[type], preview_routine='image_preview')
if F[0] ne '' then begin
	widget_control, /hourglass
	case type of
		0: begin
			p = read_bmp_images( F)
			end
		1: begin
			p = read_pnc_images( F)
			end
		2: begin
			p = read_pef_images( F)
			end
		3: begin
			p = read_png_images( F)
			end
		4: begin
			p = read_tif_images( F)
			end
		else: begin
			goto, finish
			end
	endcase

    if ptr_valid(p) then begin
       if ptr_valid((*pstate).p) then begin
         if (*(*pstate).p).orphan eq 1 then begin
          (*pstate).local = 1
          (*(*pstate).p).orphan = 0
         endif
         if ((*pstate).p ne p) and ((*pstate).local eq 1) then free_images, (*pstate).p
       endif

       if ((*pstate).clone eq 1) then begin
         (*pstate).local = 0
         (*p).orphan = 1
       endif else begin
         (*pstate).local = 1
         (*p).orphan = 0
       endelse
       (*pstate).p = p
       (*p).undo.ok = 0
       (*pstate).file = F[0]
       *(*pstate).path = extract_path( F[0])
       notify, 'path', (*pstate).path, from=event.top

       set_image_view, pstate, event.top
       pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
       if ptr_valid( opt) then begin
         widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
         widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
         widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
         widget_control, (*pstate).interp_id, set_button = (*opt)[(*pstate).image].interp
       endif

       if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
       (*pstate).pstate = ptr_new( *pstate)
       notify, 'image-clone', (*pstate).pstate, from=event.top
       notify, 'images-changed', (*pstate).p, from=event.top
       *(*pstate).pselect = 1
    endif else begin
       warning,'image_import','error reading image file '+F
    endelse
endif

finish:
end

;-----------------------------------------------------------------

; Import XANES stack energies from a file (assumes keV units)

pro Image_Import_Stack_energies, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if ptr_good( (*pstate).p) eq 0 then return
p = (*pstate).p

	F = file_requester( /read, filter='*.csv', $
	         title='Select file of XANES energies', path=*(*pstate).path, group=event.top, $
	         fix_filter=0, multiple=0)
	if F[0] ne '' then begin
		energy = get_xanes_energies( F[0], do_xanes=OK)
		if OK then begin
			if max(energy) gt 500. then energy = energy/1000.
			if ptr_valid( (*p).pz_coords) then ptr_free, (*p).pz_coords
			(*p).pz_coords = ptr_new( energy, /no_copy)
			(*p).z_coord_units = 'keV'
		endif
		
		set_image_view, pstate, event.top
	endif
	return
end

;-----------------------------------------------------------------

pro Image_import_stack, Event, dir=dir, flatten=flatten

COMPILE_OPT STRICTARR
common c_image_load_temp, p
if n_elements(type) lt 1 then type=0
if n_elements(flatten) lt 1 then flatten=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, /hourglass
p = load_geopixe_image_stack( path=*(*pstate).path, dir=dir, group=event.top, $
							flatten=flatten, error=error)
widget_control, hourglass=0
if error then return

if ptr_valid(p) then begin
	if ptr_valid((*pstate).p) then begin
		if (*(*pstate).p).orphan eq 1 then begin
			(*pstate).local = 1
			(*(*pstate).p).orphan = 0
		endif
		if ((*pstate).p ne p) and ((*pstate).local eq 1) then free_images, (*pstate).p
	endif

	if ((*pstate).clone eq 1) then begin
		(*pstate).local = 0
		(*p).orphan = 1
	endif else begin
		(*pstate).local = 1
		(*p).orphan = 0
	endelse
	(*pstate).p = p
	(*p).undo.ok = 0
	(*pstate).file = (*p).file
	*(*pstate).path = extract_path( (*p).file)
	if dir then begin
		*(*pstate).path = dir_up(*(*pstate).path)
		file = *(*pstate).path + strip_path(strip_file_ext((*pstate).file)) + '.xan'
		(*p).file = file
		(*pstate).file = file
	endif
	notify, 'path', (*pstate).path, from=event.top

	set_image_view, pstate, event.top
	pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
	if ptr_valid( opt) then begin
		widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
		widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
		widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
		widget_control, (*pstate).interp_id, set_button = (*opt)[(*pstate).image].interp
	endif

	if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
	(*pstate).pstate = ptr_new( *pstate)
	notify, 'image-clone', (*pstate).pstate, from=event.top
	notify, 'images-changed', (*pstate).p, from=event.top
	*(*pstate).pselect = 1
endif

finish:
end

;-----------------------------------------------------------------

pro Image_Invert_Colours, Event

COMPILE_OPT STRICTARR
;common Colors, ro,go,bo, rr,gg,bb

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;ro[16:115] = rr[16:115]
;go[16:115] = gg[16:115]
;bo[16:115] = bb[16:115]
tvlct, ro,go,bo, /get
rr = reverse(ro[16:115])
gg = reverse(go[16:115])
bb = reverse(bo[16:115])
tvlct, rr,gg,bb, 16

load_spec_colours
draw_images, pstate
notify, 'image-display', from=event.top
end

;-----------------------------------------------------------------

pro Image_Linear_Luminance, Event

COMPILE_OPT STRICTARR
;common Colors, ro,go,bo, rr,gg,bb

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;ro[16:115] = rr[16:115]
;go[16:115] = gg[16:115]
;bo[16:115] = bb[16:115]
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
draw_images, pstate
notify, 'image-display', from=event.top
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

pro Image_Load, Event, ignore=ignore

COMPILE_OPT STRICTARR
common c_image_load_temp, p
if n_elements(ignore) lt 1 then ignore=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (*pstate).xanes then begin
	ext = '*.xan'
	title = 'Select XANES image stack to read'
	preview = 'image_stack_preview'
	image_stack_preview							; to get it compiled into GeoPIXE
endif else begin
	ext = ['*.dai','*.dai.*']
	title = 'Select image file to read'
	preview = 'image_geopixe_preview'
	image_geopixe_preview						; to get it compiled into GeoPIXE
endelse

F = file_requester( /read, filter=ext, title=title, fix_filter=0, /image, ignore=ignore, $
			path=*(*pstate).path, dialog_parent=event.top, preview_routine=preview)
if F ne '' then begin
	Image_Load2, pstate, F, ignore=ignore, error=err
endif
end

;-----------------------------------------------------------------

pro Image_Load2, pstate, F, ignore=ignore, error=err

COMPILE_OPT STRICTARR
if n_elements(ignore) lt 1 then ignore=0
err = 1

    widget_control, /hourglass
    if (*pstate).xanes then begin
		p = read_geopixe_image( F, /xanes, ignore=ignore)
    endif else begin
    	p = read_geopixe_image( F, ignore=ignore)
    endelse

	if ptr_valid(p) then begin
		if ptr_valid((*pstate).p) then begin
			if (*(*pstate).p).orphan eq 1 then begin
				(*pstate).local = 1
				(*(*pstate).p).orphan = 0
			endif
			if ((*pstate).p ne p) and ((*pstate).local eq 1) then begin
				free_images, (*pstate).p
			endif
		endif

		if ((*pstate).clone eq 1) then begin
			(*pstate).local = 0
			(*p).orphan = 1
		endif else begin
			(*pstate).local = 1
			(*p).orphan = 0
		endelse
		(*pstate).p = p
		(*p).undo.ok = 0
		(*pstate).file = F
		*(*pstate).path = extract_path( F)
		notify, 'path', (*pstate).path, from=(*pstate).tlb

		set_image_view, pstate, (*pstate).tlb
		pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
		if ptr_valid( opt) then begin
			widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
			widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
			widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
			widget_control, (*pstate).interp_id, set_button = (*opt)[(*pstate).image].interp
		endif

		if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
		(*pstate).pstate = ptr_new( *pstate)
		notify, 'image-clone', (*pstate).pstate, from=(*pstate).tlb
		notify, 'images-changed', (*pstate).p, from=(*pstate).tlb
		*(*pstate).pselect = 1
		err = 0
	endif else begin
		warning,'image_load2','error reading image file '+F
		err = 1
	endelse
	return
end

;-----------------------------------------------------------------

function Image_Load_plugins, error=error

	COMPILE_OPT STRICTARR
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
	       warning,'Image_Load_plugins',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return, 0L
	    endif
	endif
	error = 1
	
	add_plugins = 0
	plugin_path = geopixe_root+'plugins'+slash()
	plugins = ptr_new()
    plugin_list = find_file2(plugin_path+'*_image_plugin.sav')
	if plugin_list[0] eq '' then begin
		plugin_title = ['-- none --']
		print,'Image: no plugins found.'
	endif else begin
		nf = n_elements(plugin_list)
		print,'image:  process ',nf,' plugin files ...'
		plugin_title = strarr(nf)

		if catch_errors_on then begin
			Catch, ErrorNo
			if (ErrorNo ne 0) then begin
				Catch, /cancel
				warning,'Image_Load_plugins',['Errors detected in plugins.', $
						'Check plugin SAV files.','','Make sure version is less than or', $
						'equal to current IDL session,', $
						'and at least v6.0 for VM mode.']

				Catch, ErrorNo
				if (ErrorNo ne 0) then begin
					Catch, /cancel
					on_error, 1
					help, calls = s
					n = n_elements(s)
					c = 'Call stack: '
					if n gt 2 then c = [c, s[1:n-2]]
					warning,'Image_Load_plugins',['IDL run-time error caught.', '', $
						'Error:  '+strtrim(!error_state.name,2), $
						!error_state.msg,'',c,'','Check plugins for errors.'], /error
					MESSAGE, /RESET
					return, plugins
				endif
				return, plugins
			endif
		endif

		for i=0L,nf-1 do begin
			print,'Image: restore plugin: ', plugin_list[i]
			restore, plugin_list[i], /verbose
			plugin_list[i] = strip_path( plugin_list[i])
			plugin_list[i] = strip_file_ext( plugin_list[i])
			call_procedure, plugin_list[i], title=title
			if n_elements(title) lt 1 then begin
				plugin_title[i] = 'No "title" return'
			endif else begin
				plugin_title[i] = title
				print,'Image: register valid plugin: ', plugin_title[i]
			endelse
		endfor
		add_plugins = 1
		plugins = ptr_new( {list:plugin_list, title:plugin_title})
		error = 0
	endelse
	return, plugins
end

;-----------------------------------------------------------------

pro image_Plot_Results, Event

COMPILE_OPT STRICTARR
;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate

;image_plot, group_leader=event.top, TLB=tlb
;register_notify, event.top, 'image-?', from=tlb
end

;-----------------------------------------------------------------

pro image_Multi_Image, Event

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
       warning,'Image_Multi_Image',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

path = *(*pstate).path
test = (*pstate).test
debug = (*pstate).debug

multi_image, group_leader=event.top, TLB=tlb, pimage=(*pstate).p, $
				title='GeoPIXE Multi-image Display'

register_notify, event.top, $
		['path', $					; new path
		'image-display' $			; image display updates
		], from=tlb
return
end

;-----------------------------------------------------------------
; Now obsolete - use "geopixe.conf" file and geopixe_defaults() call

;pro image_Prefs, Event
;
;COMPILE_OPT STRICTARR
;common c_working_dir, geopixe_root
;
;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate
;
;prefs = prefs_select( event.top, path=geopixe_root, old_prefs=*(*pstate).pprefs)
;
;if prefs.error eq 0 then begin
;    image_update_prefs, prefs
;    *(*pstate).pprefs = prefs
;    *(*pstate).path = prefs.directory
;	notify, 'path', (*pstate).path, from=event.top
;	if strlen(prefs.data_dir) gt 0 then begin
;		*(*pstate).dpath = prefs.data_dir
;		notify, 'dpath', (*pstate).dpath, from=event.top
;	endif
;	path = build_output_path( *(*pstate).dpath, *(*pstate).path, (*pstate).root, /set)
;endif
;end

;-----------------------------------------------------------------

pro image_process_plugin, event

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
       warning,'Image_Process_Plugin',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugin source code.'], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image) eq 0 then goto, done
i = (*pstate).image
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

widget_control, event.id, get_uvalue=routine
if n_elements(routine) lt 1 then goto, done

call_procedure, routine, p, i, history=history

set_image_view, pstate, event.top, clone=0		;, /clone
set_image_minmax, p, (*p).image, (*p).options
set_image_minmax, p, (*p).error, (*p).escale

if n_elements(history) eq 0 then begin
    widget_control, event.id, get_value=name
    history = name
endif
hist = 'Plugin: ' + routine
if n_elements(history) gt 0 then hist = hist + ', ' + history
add_history, (*p).history, i, hist

draw_images, pstate

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top

done:
end

;-----------------------------------------------------------------

pro Image_Process_Clear_sides, Event, n, silent=silent, no_undo=no_undo, select=ii

	Image_Process_Clear, Event, n, silent=silent, no_undo=no_undo, select=ii, /sides
	return
end

;-----------------------------------------------------------------

pro Image_Process_Clear, Event, n, silent=silent, no_undo=no_undo, select=ii, sides=sides

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0
if n_elements(sides) eq 0 then sides=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image
if ii eq -1 then no_undo=1
if no_undo then (*p).undo.ok = 0
xanes_stack_test, p, xanes, n_el, el, el_xanes

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if ii eq -1 then begin
	i1 = 0
	i2 = n_el-1
endif else begin
	i1 = ii
	i2 = ii
endelse

for i=i1,i2 do begin
	if no_undo eq 0 then image_save_undo, p, i
	
	img = (*pimg)[*,*,i]
	clear_image_border, img, n, sides=sides            		; clear border
	
	(*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Clear border, width = '+ str_tidy(n)
	endif
endfor

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Dilate, Event, n, silent=silent, no_undo=no_undo, select=i

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(i) lt 1 then i = (*pstate).image

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).
;
if no_undo eq 0 then image_save_undo, p, i

img = (*pimg)[*,*,i]
;s = replicate(1,n,n)
s = round_kernel(n)

low = (*opt)[i].bottom * (*opt)[i].max / 100.
high = (*opt)[i].top * (*opt)[i].max / 100

f = replicate(1.0, (*p).xsize,(*p).ysize)

b = img ge 0.5*(low + high)
e = dilate(b,s)
q = where( b xor e)
if q[0] ne -1 then f[q] = 1./0.5

b = img ge 0.1*(low + high)
e = dilate(b,s)
q = where( b xor e)
if q[0] ne -1 then f[q] = 1./0.7

f = smooth2(f,(n/2)>2)					; special smooth with extension
img = img * f

(*pimg)[*,*,i] = img

set_image_minmax, p, pimg, opt

if (*pstate).display_mode eq 0 then begin
    add_history, (*p).history, i, 'Dilate, width = '+ str_tidy(n)
endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Erode, Event, n, silent=silent, no_undo=no_undo, select=i

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(i) lt 1 then i = (*pstate).image

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).
;
if no_undo eq 0 then image_save_undo, p, i

img = (*pimg)[*,*,i]
s = replicate(1,n,n)

low = (*opt)[i].bottom * (*opt)[i].max / 100.
high = (*opt)[i].top * (*opt)[i].max / 100

f = replicate(1.0, (*p).xsize,(*p).ysize)

b = img ge 0.5*(low + high)
e = erode(b,s)
q = where( b xor e)
f[q] = 0.5

b = img ge 0.1*(low + high)
e = erode(b,s)
q = where( b xor e)
f[q] = 0.7

f = smooth2(f,(n/2)>2)
img = img * f

(*pimg)[*,*,i] = img

set_image_minmax, p, pimg, opt

if (*pstate).display_mode eq 0 then begin
    add_history, (*p).history, i, 'Erode, width = '+ str_tidy(n)
endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Median, event, n, silent=silent, no_undo=no_undo, select=ii

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes
if xanes eq 0 then i=ii else i=0L

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if (no_undo eq 0) and (xanes eq 0) then image_save_undo, p, i

process:
	img = median2( (*pimg)[*,*,i], n)
	
	(*pimg)[*,*,i] = img
	
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Median filter, width = '+ str_tidy(n)
	endif
	if xanes then begin
		i = i+1
		if i lt n_el then goto, process
	endif

if (*pstate).display_mode eq 0 then begin      ; smooth errors as well
    (*pstate).display_mode = 1
    check_mode, pstate
    if (*pstate).display_mode eq 1 then begin
       Image_Process_Median, event, (n/2) > 2, /silent, /no_undo
    endif
    (*pstate).display_mode = 0
endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Boxcar, Event, n, silent=silent, no_undo=no_undo, select=ii

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes
if xanes eq 0 then i=ii else i=0L

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if (no_undo eq 0) and (xanes eq 0) then image_save_undo, p, i

process:
	img = smooth2( (*pimg)[*,*,i], n)

;	r = ceil(n/2.)
;	clear_image_border, img, r             ; clear border

	(*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Boxcar smooth, width = '+ str_tidy(n)
	endif
	if xanes then begin
		i = i+1
		if i lt n_el then goto, process
	endif

if (*pstate).display_mode eq 0 then begin      ; smooth errors as well
    (*pstate).display_mode = 1
    check_mode, pstate
    if (*pstate).display_mode eq 1 then begin
       if n ge 3 then begin
         Image_Process_Boxcar, event, (n/2) > 2, /silent, /no_undo, select=ii
       endif else begin
         Image_Process_Gaussian, event, float(n/1.5) > 1.0, /silent, /no_undo, select=ii
       endelse
    endif
    (*pstate).display_mode = 0
endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Clip, Event, right=right, top=top

COMPILE_OPT STRICTARR
; Clip /right or /top of images

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p
opt = (*p).options
xanes_stack_test, p, xanes, n_el, el, el_xanes

if n_elements(right) lt 1 then right=0
if n_elements(top) lt 1 then top=0
if (right eq 0) and (top eq 0) then right=1

if (*pstate).corr_mode ne 0 then begin
    warning,'Image_Process_Clip','Need to select a normal region.'
    goto, done
endif

nx = (*p).xsize
ny = (*p).ysize
mark_vertices, pstate, x1,y1, n
s = 'Clipped -'
if right then begin
    nx = fix(max(x1))+1
    s = s + ' right'
endif
if top then begin
    ny = fix(max(y1))+1
    s = s + ' top'
endif
(*p).charge = (*p).charge * float(nx)*float(ny) / (float((*p).xsize)*float((*p).ysize))
(*p).xsize = nx
(*p).ysize = ny

(*p).undo.ok = 0
img = fltarr(nx,ny,n_el)
for i=0L,n_el-1 do begin
    img[*,*,i] = (*(*p).image)[0:nx-1,0:ny-1,i]
    add_history, (*p).history, i, s
endfor

if ((*p).has_errors eq 1) and ptr_valid((*p).error ) then begin
    opt = (*p).escale
    err = fltarr((nx+1)/2,(ny+1)/2,n_el)
    for i=0L,n_el-1 do begin
       err[*,*,i] = (*(*p).error)[0:(nx+1)/2-1,0:(ny+1)/2-1,i]
    endfor
endif else (*p).has_errors=0

ptr_free, (*p).image
(*p).image = ptr_new( img, /no_copy)
if (*p).has_errors eq 1 then begin
    ptr_free, (*p).error
    (*p).error = ptr_new( err, /no_copy)
endif

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) and ptr_valid((*p).error) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif
set_image_view, pstate, event.top

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
if ((*pstate).clone eq 1) then begin
    (*pstate).local = 0
    (*p).orphan = 1
endif else begin
    (*pstate).local = 1
    (*p).orphan = 0
endelse
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top
*(*pstate).pselect = 1

done:
end

;-----------------------------------------------------------------

pro Image_Process_correct_Y_ripples, Event, n, silent=silent, no_undo=no_undo, select=i

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	p = (*pstate).p
	if ptr_good( p) eq 0 then goto, done
	if n_elements(i) lt 1 then i = (*pstate).image

; Only works for include mode and "Project Y" shape, for now ...

	if ((*pstate).corr_mode ne 0) or ((*pstate).analyze_type[(*pstate).analyze_mode] ne 9) then begin
		warning,'Image_Process_correct_Y_ripples',['Need to define a shape using "Project Y" first,','including the area to correct.']
		return
	endif

	xanes_stack_test, p, xanes, n_el, el, el_xanes
	if xanes then return

	pimg = (*p).image
	opt = (*p).options
	nx = (*p).xsize
	ny = (*p).ysize
	nxy = long(nx)*long(ny)
	if ptr_good( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

	if (no_undo eq 0) and (xanes eq 0) then image_save_undo, p, i

	mark_vertices, pstate, x1,y1, n1
	if n1 le 1 then return

	x = round(x1)
	y = round(y1)
	q = polyfillv( x,y, (*p).xsize,(*p).ysize)
	if q[0] eq -1 then goto, no_points

	if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
	(*pstate).q = ptr_new(q)
	(*pstate).analyze_type[1] = 0						; corr_mode 0, use region q

	q_to_xy, q, nx, x,y

	pm = (*(*pstate).pmark[0])[9]
	z = long(y - min(y))
	nz = max(z)+1
	mx = mean(x)
	rx = max(x)-min(x)+1

;	Accumulate histogram along 'z' to integrate contents of all 'x,y' pixels that map to 'z'
;	also integrate the 'charge_map' the same way (see "analyse_image").

	hist_xy, x,y,z, pimg, nx,ny,n_el, hist,nhist, /clip_zero
	if (nhist[0] eq -1) or (hist[0] eq -1) then goto, no_points
;	hist_xy, x,y,z, (*p).temp.charge_map, nx,ny,1, hcharge,nchist

;	hist[*,i] = hist[*,i] / hcharge
	hist[*,i] = hist[*,i] / rx
	hist0 = median2( hist[*,i] > 0., 3)
	ratio = smooth2( hist0, (n > 3)) / (hist0)

	scale = replicate(1.,nx,ny)
	scale[q] = ratio[z]

;	wset,0
;	plot, (*pimg)[mx,*,i] > 0.
;	oplot, ((*pimg)[mx,*,i]>0.)*scale[mx,*], color=spec_colour('red')

	(*pimg)[*,*,i] = scale * (*pimg)[*,*,i]
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Correct Y ripples, width = '+ str_tidy(n)
	endif

;	if (*pstate).display_mode eq 0 then begin      ; smooth errors as well
;	    (*pstate).display_mode = 1
;	    check_mode, pstate
;	    if (*pstate).display_mode eq 1 then begin
;	       if n ge 3 then begin
;	         Image_Process_Boxcar, event, (n/2) > 2, /silent, /no_undo, select=ii
;	       endif else begin
;	         Image_Process_Gaussian, event, float(n/1.5) > 1.0, /silent, /no_undo, select=ii
;	       endelse
;	    endif
;	    (*pstate).display_mode = 0
;	endif
	
	if silent eq 0 then begin
	    draw_images, pstate
	    notify, 'image-display', from=event.top
	endif
	return

no_points:
	warning,'Image_Process_correct_Y_ripples','No points enclosed.'
	return
done:
	return
end

;-----------------------------------------------------------------

pro Image_Process_Crop, Event, zero=zeroi

; Crop images to Shape bounds
;	/zero	also clear image outside shape (if not a rectangle) -- later

COMPILE_OPT STRICTARR
if n_elements(zeroi) lt 1 then zeroi=1

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p
opt = (*p).options
xanes_stack_test, p, xanes, n_el, el, el_xanes
zero = zeroi

if xanes then begin
    warning,'Image_Process_Crop','Crop not available for 3D stack images.'
    goto, done
endif
if ((*pstate).corr_mode ne 0) or ((*pstate).analyze_mode ne 0) then begin
    warning,'Image_Process_Crop','Need to select a normal "Shape" region and "Include" mode.'
    goto, done
endif
if ((*p).xcompress ge 2  ) or ((*p).ycompress ge 2) then begin
    warning,'Image_Process_Crop',['Cannot use "Crop" on a compresed image,', $
					'because "Crop" creates a "Windowed Sort".']
    goto, done
endif

mark_vertices, pstate, x1,y1, n		; new size and offset

if n le 1 then begin
    warning,'Image_Process_Crop','Need to select a normal "Shape" region.'
    goto, done
endif

mask = bytarr((*p).xsize,(*p).ysize)
nxy = long((*p).xsize) * long((*p).ysize)

x = round(x1)
y = round(y1)
x1 = clip( x, 0, (*p).xsize-1)
y1 = clip( y, 0, (*p).ysize-1)
xoff = min(x1)
nx = max(x1) - min(x1) + 1
yoff = min(y1)
ny = max(y1) - min(y1) + 1

if zero then begin
	q = polyfillv( x,y, (*p).xsize,(*p).ysize)
	if q[0] eq -1 then begin
	    warning,'Image_Process_Crop','Selected "Shape" region includes no points.'
	    goto, done
	endif
	mask[q] = 1
endif else begin
	mask[xoff:nx+xoff-1,yoff:ny+yoff-1] = 1
endelse
q  = where( mask eq 1, nq)
qz = where( mask eq 0, nqz)			; outside of shape area (original coordinates)
if nqz eq 0 then zero=0				; nothing to zero

nex = (nx+1)/2						; offset and size for error (yield) arrays
ney = (ny+1)/2
xeoff = xoff/2
yeoff = yoff/2

(*p).charge = (*p).charge * float(nq) / (float((*p).xsize)*float((*p).ysize))

(*p).sub_region = 1					; set "Windowed sort" mode
(*p).xsize = nx
(*p).ysize = ny
(*p).xoffset = xoff
(*p).yoffset = yoff
(*p).x_sub_range = nx
(*p).y_sub_range = ny

(*p).bounds.xmin = (*p).bounds.xmin > xoff 
(*p).bounds.xmax = (*p).bounds.xmax < (xoff + nx-1) 
(*p).bounds.ymin = (*p).bounds.ymin > yoff 
(*p).bounds.ymax = (*p).bounds.ymax < (yoff + ny-1) 

(*p).undo.ok = 0					; undo buffer not valid anymore
(*p).temp.valid = 0					; force calc of charge and flux maps and total (bounds) charge
(*p).has_preview = 0				; force calc of preview image on write
ptr_free, (*p).preview

img = fltarr(nx,ny,n_el)
for i=0L,n_el-1 do begin
	if zero then (*(*p).image)[qz + nxy*i] = 0
    img[*,*,i] = (*(*p).image)[xoff:xoff+nx-1,yoff:yoff+ny-1,i]
    add_history, (*p).history, i, 'Cropped, to ['+str_tidy(xoff)+','+str_tidy(xoff+nx-1)+'] x ['+str_tidy(yoff)+','+str_tidy(yoff+ny-1)+']'
    if zero then add_history, (*p).history, i, 'Zeroed outside shape'
endfor
ptr_free, (*p).image
(*p).image = ptr_new( img, /no_copy)

if ptr_good((*p).error) eq 0 then (*p).has_errors=0
if ((*p).has_errors eq 1) then begin
    err = fltarr(nex,ney,n_el)
    for i=0L,n_el-1 do begin
       err[*,*,i] = (*(*p).error)[xeoff:xeoff+nex-1,yeoff:yeoff+ney-1,i]
    endfor
    ptr_free, (*p).error
    (*p).error = ptr_new( err, /no_copy)
endif

if ptr_good((*p).yield) eq 0 then (*p).has_yield=0
if ((*p).has_yield eq 1) then begin
	n_yield = n_elements( (*(*p).yield)[0,0,*])
    yield = fltarr(nex,ney,n_yield)
    for i=0L,n_yield-1 do begin
       yield[*,*,i] = (*(*p).yield)[xeoff:xeoff+nex-1,yeoff:yeoff+ney-1,i]
    endfor
    ptr_free, (*p).yield
    (*p).yield = ptr_new( yield, /no_copy)
endif

if ptr_good((*p).phase) eq 0 then (*p).has_phase=0
if ((*p).has_phase eq 1) then begin
	n_phase = n_elements( (*(*p).phase)[0,0,*])
	phase = fltarr(nx,ny,n_phase)
	for i=0L,n_phase-1 do begin
	    phase[*,*,i] = (*(*p).phase)[xoff:xoff+nx-1,yoff:yoff+ny-1,i]
	endfor
	ptr_free, (*p).phase
	(*p).phase = ptr_new( phase, /no_copy)
endif

if (ptr_good((*p).flux) eq 0) or (ptr_good((*p).raw_flux) eq 0) then (*p).has_flux=0
if ((*p).has_flux eq 1) then begin
	if zero then begin
		(*(*p).flux)[qz] = 0
		(*(*p).raw_flux)[qz] = 0
	endif
    flux = fltarr(nx,ny)
    raw_flux = fltarr(nx,ny)
	flux[*,*] = (*(*p).flux)[xoff:xoff+nx-1,yoff:yoff+ny-1]
	raw_flux[*,*] = (*(*p).raw_flux)[xoff:xoff+nx-1,yoff:yoff+ny-1]
    ptr_free, (*p).flux
    ptr_free, (*p).raw_flux
    (*p).flux = ptr_new( flux, /no_copy)
    (*p).raw_flux = ptr_new( raw_flux, /no_copy)
endif

if (ptr_good((*p).dead_fraction) eq 0) then (*p).has_dead=0
if ((*p).has_dead eq 1) then begin
    dead = fltarr(nx,ny)
	dead[*,*] = (*(*p).dead_fraction)[xoff:xoff+nx-1,yoff:yoff+ny-1]
    ptr_free, (*p).dead_fraction
    (*p).dead_fraction = ptr_new( dead, /no_copy)
endif

if (ptr_good((*p).dwell_map) eq 0) then (*p).has_dwell=0
if ((*p).has_dwell eq 1) then begin
    dwell = fltarr(nx,ny)
	dwell[*,*] = (*(*p).dwell_map)[xoff:xoff+nx-1,yoff:yoff+ny-1]
    ptr_free, (*p).dwell_map
    (*p).dwell_map = ptr_new( dwell, /no_copy)
endif

if (ptr_good((*p).pileup_map) eq 0) then (*p).has_pileup=0
if ((*p).has_pileup eq 1) then begin
    pileup = fltarr(nx,ny)
	pileup[*,*] = (*(*p).pileup_map)[xoff:xoff+nx-1,yoff:yoff+ny-1]
    ptr_free, (*p).pileup_map
    (*p).pileup_map = ptr_new( pileup, /no_copy)
endif

if (ptr_good((*p).count_rate_map) eq 0) then (*p).has_rates=0
if ((*p).has_rates eq 1) then begin
    rates = fltarr(nx,ny)
	rates[*,*] = (*(*p).count_rate_map)[xoff:xoff+nx-1,yoff:yoff+ny-1]
    ptr_free, (*p).count_rate_map
    (*p).count_rate_map = ptr_new( rates, /no_copy)
endif

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif
set_image_view, pstate, event.top

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
if ((*pstate).clone eq 1) then begin
    (*pstate).local = 0
    (*p).orphan = 1
endif else begin
    (*pstate).local = 1
    (*p).orphan = 0
endelse
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top
*(*pstate).pselect = 1

done:
end

;-----------------------------------------------------------------

pro Image_Process_Gaussian, Event, w, silent=silent, no_undo=no_undo, select=ii

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes
if xanes eq 0 then i=ii else i=0L

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if (no_undo eq 0) and (xanes eq 0) then image_save_undo, p, i

	kernel = gaussian_kernel(w)
;	n = fix(w + 0.5)
;	n = 2*(n/2) + 1
;	n = n > 3
;	off = n/2
;	kernel = fltarr(n,n)
;
;	sum = 0.0
;	for k=0L,n-1 do begin
;	    for j=0L,n-1 do begin
;	       term = exp( -0.693 * float((k-off)*(k-off) + (j-off)*(j-off)) / (0.25*w*w) )
;	       kernel[k,j] = term
;	       sum = sum + term
;	    endfor
;	endfor
;	kernel = kernel/sum
	
process:
	img = convol( (*pimg)[*,*,i], kernel, /edge_truncate)
		
;	r = ceil(n/2.)
;	clear_image_border, img, r            		   ; clear border

	(*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Gaussian smooth, width = '+ str_tidy(w)
	endif
	if xanes then begin
		i = i+1
		if i lt n_el then goto, process
	endif

if (*pstate).display_mode eq 0 then begin    	  ; smooth errors as well
    (*pstate).display_mode = 1
    check_mode, pstate
    if (*pstate).display_mode eq 1 then begin
       Image_Process_Gaussian, event, (w/2.0) > 1.0, /silent, /no_undo, select=ii
    endif
    (*pstate).display_mode = 0
endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Ghost, Event, f, silent=silent, no_undo=no_undo

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0
if n_elements(all) lt 1 then all=0
if n_elements(f) lt 1 then f=0.1

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

if (*pstate).display_mode ne 0 then goto, done
p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(i) lt 1 then i = (*pstate).image
xanes_stack_test, p, xanes, n_el, el, el_xanes

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

if (*pstate).analyze_mode eq 0 then begin          ; include mode
    if (*pstate).analyze_type[0] ne 0 then goto, done
    mark_vertices, pstate, x,y, nxy
    if nxy eq 0 then begin
    	warning,'Image_Process_Ghost',['Need to drag "Distance" first,', $
    				'from original feature to ghost.']
       goto, done
    endif else begin
       dx = x[1] - x[0]
       dy = y[1] - y[0]
    endelse
endif else begin                        ; exclude mode (multipass)
    goto, done
endelse

order = 3
sign = (f gt 0.) ? 1 : -1
for i=0L,n_el-1 do begin
    img = (*pimg)[*,*,i]

;    img = img + f* shift(img, dx,dy)

	for k=1L,order do begin
	    r = sign * (-1)^k * abs(f)^k
	    img = img - r * shift(img, k*dx,k*dy)
	endfor

;   clear_image_border, img, n                 ; clear border

    (*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt

    if (*pstate).display_mode eq 0 then begin
       add_history, (*p).history, i, 'Correct Ghost, by '+str_tidy(f)+' at '+ str_tidy(dx)+','+str_tidy(dy)
    endif
endfor

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

;   If a line in image is missing (n=0), then replace it with average of lines on each side.
;   If a line is double counted (n=1), then halve it.
;   Use /vertical for vertical missing lines, or add 2 to 'n'.
;	Uses region (or 'select') to select area to examine.

pro Image_Process_missing_lines, Event, n, silent=silent, no_undo=no_undo, vertical=vertical, select=select

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0
if n_elements(vertical) lt 1 then vertical=0
if n gt 1 then begin
    vertical = 1
    n = n-2
endif
if ((n ne 0) and (n ne 1)) then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

if (*pstate).display_mode ne 0 then goto, done
p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
;if n_elements(i) lt 1 then i = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

if n_elements(select) gt 0 then begin
	if typevar(select) eq 'STRUCT' then begin
         lowy = select.ylow
         highy = select.yhigh
         lowx = select.xlow
         highx = select.xhigh
		 goto, fix_row
	endif
endif

if (*pstate).analyze_mode eq 0 then begin          ; include mode
    if (*pstate).analyze_type[0] eq 0 then goto, done
    mark_vertices, pstate, x,y, nxy
    x = round(x)
    y = round(y)
    if nxy eq 0 then begin
       pq = (*pstate).qc
       if ptr_valid(pq) then begin
         q_to_xy, *pq, sx, tx,ty
         lowy = min(ty)
         highy = max(ty)
         lowx = min(tx)
         highx = max(tx)
       endif
    endif else begin
       q = polyfillv( x,y, (*p).xsize,(*p).ysize)
       if q[0] eq -1 then begin
         warning,'Image_Process_Missing_Line',['You need to select a region at', $
          'least one line in height.']
         goto, done
       endif

       if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
       (*pstate).q = ptr_new(q, /no_copy)
       pq = (*pstate).q
       (*pstate).analyze_type[1] = 0
       if ptr_valid(pq) then begin
         lowy = min(y)
         highy = max(y)
         lowx = min(x)
         highx = max(x)
       endif
    endelse
endif else begin                		        ; exclude mode (multipass)
    pq = (*pstate).q
    if ptr_valid(pq) then begin
       q_to_xy, *pq, sx, tx,ty
       lowy = min(ty)
       highy = max(ty)
       lowx = min(tx)
       highx = max(tx)
    endif else begin
       warning,'Image_Process_Missing_Line',['You need to select a region first.', $
         'Then repeat this operation.']
       goto, done
    endelse
endelse
if n_elements(lowx) lt 1 then goto, done
if highy le lowy then begin
    warning,'Image_Process_Missing_Line',['You need to select a region at', $
       'least one line in height.']
    goto, done
endif

;	Larger box surrounding the missing stripe ...

fix_row:
	x0 = [(lowx-1)>0, (highx+1)<(sx-1), (highx+1)<(sx-1), (lowx-1)>0, (lowx-1)>0]
	y0 = [(lowy-20)>0, (lowy-20)>0, (highy+20)<(sy-1), (highy+20)<(sy-1), (lowy-20)>0]
	q0 = polyfillv( x0,y0, sx,sy)
	q1 = veto( *pq, q0)						; exclude the missing box
	q_to_xy, q1, sx, tx,ty					; larger box less 'missing' region

for i=0L,n_el-1 do begin
	print, 'Correct missing rows for ',i, '  ', el[i]

    img = (*pimg)[*,*,i]

    case n of
		0: begin                     		 ; replace with linear interpolarion

;			xout = (lowx) + indgen( (highx-1)-(lowx)+1)
;			yout = (lowy) + indgen( (highy-1)-(lowy)+1)
;			TRIANGULATE, tx, tx, Triangles, CONNECTIVITY=connect, tolerance=0.001
;			r = griddata( tx,ty, img[q1], /grid, xout=xout,yout=yout, /linear, triangles=Triangles)
;			r = griddata( tx,ty, img[q1], /grid, xout=xout,yout=yout, /min_curv)
;			r = min_curve_surf( img[q1], tx,ty, gs=[1,1], bounds=[lowx,lowy,highx-1,highy-1])
;			r = min_curve_surf( img[q1], tx,ty, xout=xout, yout=yout)
;			r = Krig2D( img[q1], tx,ty, gs=[1,1], bounds=[lowx,lowy,highx-1,highy-1])

			r = tri_surf( img[q1], tx,ty, /linear, gs=[1,1], bounds=[lowx,lowy,highx-1,highy-1])
			img[*pq] = r

;			if vertical then begin			; replace with average across
;				highx = highx > (lowx+2)
;				below = img[(lowx)>0, lowy>0:highy<(sy-1)]
;				above = img[(highx)<(sx-1), lowy>0:highy<(sy-1)]
;				for j=lowx+1,highx-1 do begin
;					img[j,lowy>0:highy<(sy-1)] = 0.5 * (below+above)
;				endfor
;			endif else begin
;				highy = highy > (lowy+2)
;				below = img[lowx>0:highx<(sx-1),(lowy)>0]
;				above = img[lowx>0:highx<(sx-1),(highy)<(sy-1)]
;				for j=lowy+1,highy-1 do begin
;					img[lowx>0:highx<(sx-1),j] = 0.5 * (below+above)
;				endfor
;			endelse
			end
		1: begin                     		 ; halve
			if vertical then begin
				highx = highx > (lowx+2)
				for j=lowx+1,highx-1 do begin
					img[j,lowy>0:highy<(sy-1)] = 0.5 * img[j,lowy>0:highy<(sy-1)]
				endfor
			endif else begin
				highy = highy > (lowy+2)
				for j=lowy+1,highy-1 do begin
					img[lowx>0:highx<(sx-1),j] = 0.5 * img[lowx>0:highx<(sx-1),j]
				endfor
			endelse
			end
		else:
	endcase

    (*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt

    mess = ['Eliminated selected missing rows','Halved selected double counted rows']
    if (*pstate).display_mode eq 0 then begin
       add_history, (*p).history, i, mess[n]
    endif
endfor

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Roberts, Event, n, silent=silent, no_undo=no_undo, select=ii

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes
if xanes eq 0 then i=ii else i=0L

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if (no_undo eq 0) and (xanes eq 0) then image_save_undo, p, i

process:
	img = roberts( (*pimg)[*,*,i])

;	r = ceil(n/2.)
;	clear_image_border, img, r               ; clear border

	(*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Roberts edge enhancement'
	endif
	if xanes then begin
		i = i+1
		if i lt n_el then goto, process
	endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Shift_all, Event, n, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR

Image_Process_Shift, Event, n, /all, _EXTRA=_VWBExtra_
return

end

;-----------------------------------------------------------------

pro Image_Process_Shift_even_columns, Event, n, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR

Image_Process_Shift_rows, Event, n, /Y, /even, _EXTRA=_VWBExtra_
return

end

;-----------------------------------------------------------------

pro Image_Process_Shift_odd_columns, Event, n, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR

Image_Process_Shift_rows, Event, n, /Y, /odd, _EXTRA=_VWBExtra_
return

end

;-----------------------------------------------------------------

pro Image_Process_Shift_even_rows, Event, n, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR

Image_Process_Shift_rows, Event, n, /X, /even, _EXTRA=_VWBExtra_
return

end

;-----------------------------------------------------------------

pro Image_Process_Shift_odd_rows, Event, n, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR

Image_Process_Shift_rows, Event, n, /X, /odd, _EXTRA=_VWBExtra_
return

end

;-----------------------------------------------------------------

pro Image_Process_Shift_rows, Event, dx, silent=silent, no_undo=no_undo, select=ii, $
					x=x, y=y, odd=odd, even=even, test_ysize=test_ysize

; Main work horse for row and column pixel shifting in images.
; It uses 'shift_image_rows', which allows fractional pixel shifts.
; Only the '/all' option is missing. Use "Image_Process_Shift_all".

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0
if n_elements(x) lt 1 then x=0
if n_elements(y) lt 1 then y=0
if n_elements(odd) lt 1 then odd=0
if n_elements(even) lt 1 then even=0
if n_elements(test_ysize) lt 1 then test_ysize=0
if y then x=0
if x then y=0
if y eq 0 then x=1
if odd then even=0
if even then odd=0
if odd eq 0 then even=1

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

if (*pstate).display_mode ne 0 then goto, done
p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(i) lt 1 then i = (*pstate).image
xanes_stack_test, p, xanes, n_el, el, el_xanes

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

lowy = 0
highy = sy-1
lowx = 0
highx = sx-1
sskip = 'Odd'
if even then sskip = 'Even'
sdir = 'rows'
if Y then sdir = 'columns'
if n_elements(ii) lt 1 then ii=0
el_chosen = (*(*p).el)[ii]

if (*pstate).analyze_mode eq 0 then begin          ; include mode
    mark_vertices, pstate, xv,yv, nxy
    if nxy eq 0 then begin
       pq = (*pstate).qc
       if ptr_valid(pq) then begin
         q_to_xy, *pq, sx, tx,ty
         lowx = min(tx)
         highx = max(tx)
         lowy = min(ty)
         highy = max(ty)
       endif
    endif else begin
       q = polyfillv( xv,yv, (*p).xsize,(*p).ysize)
       if q[0] eq -1 then goto, done

       if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
       (*pstate).q = ptr_new(q, /no_copy)
       pq = (*pstate).q
       (*pstate).analyze_type[1] = 0
       if ptr_valid(pq) then begin
         lowx = min(xv)
         highx = max(xv)
         lowy = min(yv)
         highy = max(yv)
       endif
    endelse
endif else begin                   			     ; exclude mode (multipass)
    pq = (*pstate).q
    if ptr_valid(pq) then begin
       q_to_xy, *pq, sx, tx,ty
	   lowx = min(tx)
       highx = max(tx)
       lowy = min(ty)
       highy = max(ty)
    endif
endelse

flip = test_ysize and (2*(sy/2) ne sy)			; toggle mode if odd number of Y lines in image
odd = (odd eq 1)
sytest = test_ysize ? '(w/ toggle for odd Y rows)' : ''

for i=0L,n_el-1 do begin
	img2 = shift_image_rows( (*pimg)[*,*,i], dx, x=x, y=y, odd=odd)
	if flip then odd = ~odd
	
    (*pimg)[lowx:highx,lowy:highy,i] = img2[lowx:highx,lowy:highy]

    if (*pstate).display_mode eq 0 then begin
       add_history, (*p).history, i, '* Shift '+sskip+' '+sdir+', by = '+ str_tidy(dx)+' '+sytest+' ['+el_chosen+']'
    endif
endfor
set_image_minmax, p, pimg, opt

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Shift, Event, n, silent=silent, no_undo=no_undo, select=ii, all=all

;	Shift rows in an image. Only 'Image_Process_Shift, /all' is used now.
;	All other uses use 'Image_Process_Shift_rows', which s more general.

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0
if n_elements(all) lt 1 then all=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

if (*pstate).display_mode ne 0 then goto, done
p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(i) lt 1 then i = (*pstate).image
xanes_stack_test, p, xanes, n_el, el, el_xanes

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

lowy = 0
highy = sy-1
lowx = 0
highx = sx-1
skip = 2
sskip = 'odd'
if all then begin
    skip = 1
    sskip = 'all'
endif
if n_elements(ii) lt 1 then ii=0
el_chosen = (*(*p).el)[ii]

if (*pstate).analyze_mode eq 0 then begin          ; include mode
    mark_vertices, pstate, x,y, nxy
    if nxy eq 0 then begin
       pq = (*pstate).qc
       if ptr_valid(pq) then begin
         q_to_xy, *pq, sx, tx,ty
         lowx = min(tx)
         highx = max(tx)
         lowy = min(ty)
         highy = max(ty)
       endif
    endif else begin
       q = polyfillv( x,y, (*p).xsize,(*p).ysize)
       if q[0] eq -1 then goto, done

       if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
       (*pstate).q = ptr_new(q, /no_copy)
       pq = (*pstate).q
       (*pstate).analyze_type[1] = 0
       if ptr_valid(pq) then begin
         lowx = min(x)
         highx = max(x)
         lowy = min(y)
         highy = max(y)
       endif
    endelse
endif else begin                    			    ; exclude mode (multipass)
    pq = (*pstate).q
    if ptr_valid(pq) then begin
       q_to_xy, *pq, sx, tx,ty
	   lowx = min(tx)
       highx = max(tx)
       lowy = min(ty)
       highy = max(ty)
    endif
endelse

for i=0L,n_el-1 do begin
    img = (*pimg)[*,*,i]

;	"odd" actually shifts 'even' numbered rows (based on 0 for first) to right (n=1).

    for j=lowy,highy,skip do begin
       img[lowx:highx,j] = shift(img[lowx:highx,j], n)
    endfor

;   clear_image_border, img, n                 ; clear border

    (*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt

    if (*pstate).display_mode eq 0 then begin
       add_history, (*p).history, i, 'Shift '+sskip+' rows, by = '+ str_tidy(n)+' ['+el_chosen+']'
    endif
endfor

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Sobel, Event, n, silent=silent, no_undo=no_undo, select=ii

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes
if xanes eq 0 then i=ii else i=0L

; point_image returns either image or error, depending on (*pstate).display_mode

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if (no_undo eq 0) and (xanes eq 0) then image_save_undo, p, i

process:
	img = sobel( (*pimg)[*,*,i])

;	r = ceil(n/2.)
;	clear_image_border, img, r               ; clear border

	(*pimg)[*,*,i] = img
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, i, 'Sobel edge enhancement'
	endif
	if xanes then begin
		i = i+1
		if i lt n_el then goto, process
	endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Rotate, Event, angle, flipx=flipx, flipy=flipy

COMPILE_OPT STRICTARR
if n_elements(flipx) lt 1 then flipx=0
if n_elements(flipy) lt 1 then flipy=0
widget_control, /hour

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
xanes_stack_test, p, xanes, n_el, el, el_xanes

if (flipx eq 0) and (flipy eq 0) and (n_elements(angle) lt 1) then goto, done

nex = n_elements( (*(*p).error)[*,0,0])
ney = n_elements( (*(*p).error)[0,*,0])
if flipx then begin
    simple = 1
    use_rot = 0
    dir = 5
    nx = (*p).xsize
    ny = (*p).ysize
    nxe = nex
    nye = ney
    history = '*Mirrored X axis'
endif else if flipy then begin
    simple = 1
    use_rot = 0
    dir = 7
    nx = (*p).xsize
    ny = (*p).ysize
    nxe = nex
    nye = ney
    history = '*Mirrored Y axis'
endif else if (abs(angle-90.0) lt 0.1) then begin
    simple = 1
    use_rot = 0
    dir = 1
    nx = (*p).ysize
    ny = (*p).xsize
    nxe = ney
    nye = nex
    if nx ne ny then simple = 0
    history = '*Rotated 90 degrees'
endif else if (abs(angle+90.0) lt 0.1) or (abs(angle-270.0) lt 0.1) then begin
    simple = 1
    use_rot = 0
    dir = 3
    nx = (*p).ysize
    ny = (*p).xsize
    nxe = ney
    nye = nex
    if nx ne ny then simple = 0
    history = '*Rotated -90 degrees'
endif else begin
    simple = 0
    use_rot = 1
    nx = (*p).xsize
    ny = (*p).ysize
    nxe = nex
    nye = ney
    history = '*Rotated by ' + string(angle) + ' degrees'
endelse

(*p).undo.ok = 0
img = fltarr(nx,ny,n_el)
opt = (*p).options

for i=0L,n_el-1 do begin
    if use_rot then begin
       t = rot( (*(*p).image)[*,*,i], angle)
       img[*,*,i] = t
    endif else begin
       img[*,*,i] = rotate( (*(*p).image)[*,*,i], dir)
    endelse
    add_history, (*p).history, i, history
endfor

if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
;    opt = (*p).escale
    err = fltarr(nxe,nye,n_el)

    for i=0L,n_el-1 do begin
       if use_rot then begin
         t = rot( (*(*p).error)[*,*,i], angle)
         err[*,*,i] = t
       endif else begin
         err[*,*,i] = rotate( (*(*p).error)[*,*,i], dir)
       endelse
    endfor
endif else (*p).has_errors=0

if simple then begin
    *(*p).image = img
    *(*p).error = err

    draw_images, pstate
    notify, 'image-display', from=event.top

endif else begin
    ptr_free, (*p).image
    (*p).image = ptr_new( img, /no_copy)
    if (*p).has_errors eq 1 then begin
       ptr_free, (*p).error
       (*p).error = ptr_new( err, /no_copy)
    endif

    (*p).xsize = nx
    (*p).ysize = ny
    set_image_view, pstate, event.top

    if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
    (*pstate).pstate = ptr_new( *pstate)
    if ((*pstate).clone eq 1) then begin
       (*pstate).local = 0
       (*p).orphan = 1
    endif else begin
       (*pstate).local = 1
       (*p).orphan = 0
    endelse
    notify, 'image-clone', (*pstate).pstate, from=event.top
    notify, 'images-changed', (*pstate).p, from=event.top
    *(*pstate).pselect = 1
endelse

set_image_minmax, p, (*p).image, (*p).options, /reset_bounds
if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif

done:
end

;-----------------------------------------------------------------

pro Image_Process_Scale, Event, axis, factor

COMPILE_OPT STRICTARR
; Scale axis (0:x, 1:y, 2:both) by a factor

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p
opt = (*p).options
xanes_stack_test, p, xanes, n_el, el, el_xanes

if axis eq 0 then begin
    nx = fix( (*p).xsize * factor)
    ny = (*p).ysize
    (*p).scaled_x = (*p).scaled_x * factor
	(*p).scan.x = (*p).scan.x * factor
    charge_factor = factor
endif else if axis eq 1 then begin
    nx = (*p).xsize
    ny = fix( (*p).ysize * factor)
    (*p).scaled_y = (*p).scaled_y * factor
	(*p).scan.y = (*p).scan.y * factor
    charge_factor = factor
endif else begin
    nx = fix( (*p).xsize * factor)
    ny = fix( (*p).ysize * factor)
    (*p).scaled_x = (*p).scaled_x * factor
    (*p).scaled_y = (*p).scaled_y * factor
	(*p).scan.x = (*p).scan.x * factor
	(*p).scan.y = (*p).scan.y * factor
    charge_factor = factor * factor
endelse
(*p).xsize = nx
(*p).ysize = ny
saxis = ['X','Y','XY']

(*p).undo.ok = 0
img = fltarr(nx,ny,n_el)
for i=0L,n_el-1 do begin
    img[*,*,i] = smart_congrid( (*(*p).image)[*,*,i], nx,ny, /interp) / charge_factor
    add_history, (*p).history, i, 'Scaled ' + saxis[axis] + ' by ' + str_tidy(factor)
endfor

if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
    opt = (*p).escale
    err = fltarr( (nx/2)>1 ,(ny/2)>1, n_el)
    for i=0L,n_el-1 do begin
       err[*,*,i] = smart_congrid( (*(*p).error)[*,*,i], (nx/2)>1, (ny/2)>1, /interp) / charge_factor
    endfor
endif else (*p).has_errors=0

ptr_free, (*p).image
(*p).image = ptr_new( img, /no_copy)
if (*p).has_errors eq 1 then begin
    ptr_free, (*p).error
    (*p).error = ptr_new( err, /no_copy)
endif

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif
set_image_view, pstate, event.top

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
if ((*pstate).clone eq 1) then begin
    (*pstate).local = 0
    (*p).orphan = 1
endif else begin
    (*pstate).local = 1
    (*p).orphan = 0
endelse
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top
*(*pstate).pselect = 1

done:
end

;-----------------------------------------------------------------
; Correct for charge fluctuations, using 'q' part of current image
; axis=0 for X, axis=1 for Y

pro Image_Process_Correct_Current, Event, axis

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
i = (*pstate).image
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
opt = (*p).options
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found
axes = ['X','Y']

drop = ['Anchor normalization at bottom/left (start of scan)','Anchor normalization at top/right (end of scan)','Anchor normalization at average']
help_drop = 'Select where to anchor the normalization, to either the start (left [X=0], bottom [Y=0]) or end (right [X=max], top [Y=max]) of the scan where a standard was scanned. '			
explanation = ['This routine will normalize out variation seen in the selected pixels, along the selected axis. Select where to anchor the normalization. ' + $
			'Normally, this will be to either the start (left/bottom) or end (right/top) of the scan where a standard was analyzed. ']
help_default = ['Select where to anchor the normalization. Normally, this will be to either the start (left/bottom) or end (right/top) of the scan where a standard was analyzed. ']
text = 'Norm Axis'
initial_text = axes[axis]
help_text = 'Select the axis along which the normalization will take place ("X","Y").
check = ['none', 'median 3', 'median 5', 'median 10', 'median 20', 'median 30', 'median 50']
help_check = 'Apply "'+ ['no filter', 'median 3 filter', 'median 5 filter', 'median 10 filter', 'median 20 filter', 'median 30 filter', 'median 50 filter']+'" to correction vector.'

r = options_popup( title='Correction of flux along axis', explanation=explanation, $
			text=text, initial_text=initial_text, help_text=help_text, $
			check=check, help_check=help_check, /exclusive, columns_check=4, $
			help_default=help_default, drop=drop, help_drop=help_drop, min_xsize=400, error=error)
if error then return
	
axis = where( strupcase(strtrim(r.text[0],2)) eq axes, nq)
if nq eq 0 then begin
	warning,'Image_Process_Correct_Current','Unknown axes for normalization'
	return
endif
mode = r.drop[0]
filter = (where(r.check eq 1))[0]
file = strip_file_ext((*p).file) + '-Correct' + axes[axis] + '.txt'

if (*pstate).analyze_mode eq 0 then begin          ; include mode
    mark_vertices, pstate, x,y, n
    if (n eq 0) or ((*pstate).analyze_type[0] eq 0) then begin
       pq = (*pstate).qc
       if ptr_valid(pq) eq 0 then goto, done
    endif else begin
       q = polyfillv( x,y, (*p).xsize,(*p).ysize)
       if q[0] eq -1 then goto, done

       if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
       (*pstate).q = ptr_new(q, /no_copy)
       pq = (*pstate).q
       (*pstate).analyze_type[1] = 0
    endelse
endif else begin                        ; exclude mode (multipass)
    if ptr_valid((*pstate).q) eq 0 then begin
       warning,'Image_Process_Correct_Region',['You need to select a region first.', $
         'Then press the analyze button, and repeat this operation.']
       goto, done
    endif
    pq = (*pstate).q
endelse

q_to_xy, *pq, (*p).xsize, x,y

if axis eq 0 then begin
    z = x
endif else if axis eq 1 then begin
    z = y
endif else return
hist_xy, x,y,z, (*p).image, (*p).xsize,(*p).ysize,n_el, hist,nhist
if (nhist[0] eq -1) or (hist[0] eq -1) then goto, done

hist = hist[*,(*pstate).image]
n = n_elements(nhist)

q1 = where(nhist ne 0, nq1)
if nq1 eq 0 then begin
    warning,'Image_Process_Correct_Current',['No valid sample points to correct image.', $
       'Select a broader region coverage, and press the analyze button.', $
       'Then try this correction again.']
    goto, done
endif
zmin = q1[0]
zmax = q1[nq1-1]
hist[q1] = hist[q1] / nhist[q1]
ml = mean(hist[zmin:((zmin+10)<(n-1))])
mh = mean(hist[((zmax-10)>0):zmax])
meanh = mean(hist[q1])

q2 = where(nhist eq 0, nq2)
if nq2 ne 0 then begin
    warning,'Image_Process_Correct_Current',['Some points along axis are not sampled by this region.', $
									'You might need to select a broader region coverage and try again.','', $
									'Interpolation will be used to bridge any sampling gaps.']
	mask = bytarr(n)
	mask[q1] = 1
	k = 0
	while k lt zmin do begin
		hist[k] = ml						; extend low end
		mask[k] = 1
		k++
	endwhile
	k = n-1
	while k gt zmax do begin
		hist[k] = mh						; extend high end
		mask[k] = 1
		k--
	endwhile
	q1 = where( mask eq 1, nq1)
endif

ramp = indgen(n)
x = ramp[q1]
y = hist[q1]
hist2 = interpol( y,x, ramp, /lsquadratic)						; interpolate across any gaps
case mode of
	0: begin
		hist3 = ml / hist2
		end
	1:begin
		hist3 = mh / hist2
		end
	2: begin
		hist3 = meanh / hist2
		end
endcase

; filters: 0=none, 1=median3, 2=median5, 3=median10, 4=median20, 5=median30, 6=median50

case filter of
	1: begin
		hist = median2(hist3,3)
		end
	2: begin
		hist = median2(hist3,5)
		end
	3: begin
		hist = median2(hist3,10)
		end
	4: begin
		hist = median2(hist3,20)
		end
	5: begin
		hist = median2(hist3,30)
		end
	6: begin
		hist = median2(hist3,50)
		end
	else: begin
		hist = hist3
		end
endcase
rstart = hist[q1[0]]
rend = hist[q1[nq1-1]]

(*p).undo.ok = 0
for i=0L,n_el-1 do begin
    if axis eq 0 then begin                 				; X axis
       for j=0L,n-1 do begin
         (*(*p).image)[j,*,i] = (*(*p).image)[j,*,i] * hist[j]
       endfor
    endif else if axis eq 1 then begin       				; Y axis
       for j=0L,n-1 do begin
         (*(*p).image)[*,j,i] = (*(*p).image)[*,j,i] * hist[j]
       endfor
    endif

    if (*pstate).display_mode eq 0 then begin
       add_history, (*p).history, i, 'Correct '+ axes[axis] + ' current ['+file+']'
    endif
endfor
if (*p).has_errors then begin
    if ptr_valid( (*p).escale ) eq 0 then fix_options, p
    opt = (*p).escale
    n = n/2
    ehist = congrid( hist, n)
    for i=0L,n_el-1 do begin
       if axis eq 0 then begin              				; X axis
         for j=0L,n-1 do begin
          (*(*p).error)[j,*,i] = (*(*p).error)[j,*,i] * ehist[j]
         endfor
       endif else if axis eq 1 then begin         			; Y axis
         for j=0L,n-1 do begin
          (*(*p).error)[*,j,i] = (*(*p).error)[*,j,i] * ehist[j]
         endfor
       endif
    endfor
endif

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif

draw_images, pstate
notify, 'image-display', from=event.top

case !version.os_family of
	'MacOS': begin
		retain = 2
		end
	'unix': begin
		retain = 2
		end
	else: begin
		retain = 1
		end
endcase

on_ioerror, bad_file
openw, lun, file, /get_lun
printf, lun, axis, n_elements(hist)
printf, lun, hist
warning,/info,'Image_Process_Correct_Current',['Correction vector saved to file: '+file, $
				'','Corrections varied over the range: '+str_tidy(rstart)+' to '+str_tidy(rend), $
				'','Use "Corrections from file" menu options to re-apply this again in future.']

window,0, xsize=700, ysize=400, retain=retain
!p.title = 'Normalization versus '+axes[axis]
!x.title = axes[axis]
!y.title = (mode eq 2) ? 'Normalization relative to mean' : 'Normalization relative to Anchor'
!p.color = spec_colour('black')
!p.background = spec_colour('white')
!p.charsize = 1.2
!p.charthick = 1.0
!p.thick = 1.0
erase
plot, hist, /nodata, xstyle=1
oplot, hist, color=spec_colour('red')

done:
	close_file, lun
	return
	
bad_file:
	warning,'Image_Process_Correct_Current','Error writing Correction output file: '+file
	goto, done
end

;-----------------------------------------------------------------
; Correct for charge fluctuations, using 'q' part of current image
; axis=0 for X, axis=1 for Y

pro Image_Process_Correct_Current_file, Event, axis, first=first

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour
if n_elements(axis) lt 1 then axis=0
if n_elements(first) lt 1 then first=1

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
xanes_stack_test, p, xanes, n_el, el, el_xanes

pimg = (*p).image							; pointer to the image arrays for all elements
opt = (*p).options							; display ranges, etc.
nx = (*p).xsize								; X size of image in pixels
ny = (*p).ysize								; Y size of image in pixels
axes = ['X','Y']

if first then begin
	path = extract_path((*p).file)
	f = file_requester(  /read, filter = '*.txt', $
				title='Select Correct'+axes[axis]+' TXT file', path=path, /fix_filter)
endif else f=*(*pstate).pfile
if f[0] eq '' then return
*(*pstate).pfile = f[0]
on_ioerror, bad_file
openr, lun, f[0], /get_lun
axis = 0
n = 0L
readf, lun, axis, n
if n lt 10 then goto, done
hist = fltarr(n)
readf, lun, hist

for i=0L,n_el-1 do begin
	case axis of
		0: begin							; X axis
			n = n < nx
			for j=0L,n-1 do begin
				(*pimg)[j,*,i] = (*pimg)[j,*,i] * hist[j]
			endfor
			end
		1: begin							; Y axis
			n = n < ny
			for j=0L,n-1 do begin
				(*pimg)[*,j,i] = (*pimg)[*,j,i] * hist[j]
			endfor
			end
		else: warning,'Image_Process_Correct_Current_file','unknown "axis".'
	endcase
	add_history, (*p).history, i, 'Correct '+ axes[axis] + ' from file: '+f
endfor
set_image_minmax, p, pimg, opt

draw_images, pstate
notify, 'image-display', from=event.top

done:
	close_file, lun
	return
	
bad_file:
	warning,'Image_Process_Correct_Current_file','error reading file.'
	goto, done
end

;-----------------------------------------------------------------

; Correct for error in Y encoder on fly-back
; Find Y offset at bottom of image and offset it away.

pro Image_Process_Correct_Ydrift, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p
opt = (*p).options
xanes_stack_test, p, xanes, n_el, el, el_xanes

(*p).undo.ok = 0
nx = (*p).xsize
ny = (*p).ysize
off = lonarr(n_el)
for i=0L,n_el-1 do begin
	hy = fltarr(ny)
	off[i] = 0
	for y=0,ny-1 do begin
		hy[y] = total( (*(*p).image)[*,y,i])
	endfor
	q = where( hy ne 0., nq)
	if nq gt 0 then off[i] = q[0]
endfor
if xanes eq 0 then begin
	off[*] = min(off)
	(*p).yoffset = (*p).yoffset + off[0]
endif

for i=0L,n_el-1 do begin
	if off[i] ne 0 then begin
		img = shift( (*(*p).image)[*,*,i], 0, -off[i])
		(*(*p).image)[*,*,i] = img
		add_history, (*p).history, i, 'Correct Y drift by ' + str_tidy(off[i])

		if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
			img = shift( (*(*p).error)[*,*,i], 0, -off[i]/2)
			(*(*p).error)[*,*,i] = img
		endif
	endif
endfor

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif
draw_images, pstate

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
if ((*pstate).clone eq 1) then begin
    (*pstate).local = 0
    (*p).orphan = 1
endif else begin
    (*pstate).local = 1
    (*p).orphan = 0
endelse
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top
*(*pstate).pselect = 1

done:
end

;-----------------------------------------------------------------
; Suppress all regions in region table in image by 'w',
; for all element planes

pro Image_Process_Kill_All_Region, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	widget_control, /hourglass
	silent = 1

	for i=0,n_elements( (*(*pstate).pregions))-1 do begin
		print,'Kill pixels in all planes for region #',i
   		clear_mark, pstate, /both
   		image_region_select, pstate, i, kvs=0

		if i eq n_elements( (*(*pstate).pregions))-1 then silent=0
   		Image_Process_Suppress_Region, Event, 0.0, /all, silent=silent
	endfor
	return
end

;-----------------------------------------------------------------
; Suppress a region of the current image by 'w',
; using the current region area.
; /all suppress in all element planes.
; Do soft edges of suppress, unless w=0

pro Image_Process_Suppress_Region, Event, w, silent=silent, no_undo=no_undo, select=ii, all=all

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hourglass

if (*pstate).display_mode ne 0 then goto, done
p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(ii) lt 1 then ii = (*pstate).image
if ptr_valid( (*p).image ) eq 0 then goto, done
xanes_stack_test, p, xanes, n_el, el, el_xanes
if n_elements(all) lt 1 then all = 0
if xanes or all then i=0L else i=ii

if ptr_valid( (*p).options ) eq 0 then fix_options, p
opt = (*p).options
corr_mode = 0
if ((*pstate).analyze_type[(*pstate).analyze_mode] lt 1) then corr_mode=1

; corr_mode = 0		Normal image regions
;
; corr_mode = 1		Corr spline regions
;					Use (*pstate).qc for q, do not calculate q.

if corr_mode eq 0 then begin
	if (*pstate).analyze_mode eq 0 then begin		; include mode
	    mark_vertices, pstate, x,y, n
	    if n eq 0 then begin
	       pq = (*pstate).qc
	       if ptr_valid(pq) eq 0 then goto, done
	    endif else begin
			if n_elements(x) gt 1 then begin
				q = polyfillv( x,y, (*p).xsize,(*p).ysize)
			endif else begin
		   		q = long64(round(y))*(*p).xsize + long64(round(x))
			endelse
			if q[0] eq -1 then goto, done

	    	if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
			(*pstate).q = ptr_new(q, /no_copy)
			pq = (*pstate).q
			(*pstate).analyze_type[1] = 0
	    endelse
	endif else begin								; exclude mode, just use q mask
	    if ptr_valid((*pstate).q) eq 0 then goto, bad_exclude
	    pq = (*pstate).q
	endelse
endif else begin

	if ptr_valid((*pstate).qc) then begin
		pq = (*pstate).qc							; corr_mode 1, use spline selection
	endif else goto, bad_region
endelse

; Undo will save BOTH image and error, for both image operations (which effect both
; image and error) and error operations (which effect only error).

if (no_undo eq 0) and (xanes eq 0) and (all eq 0) then image_save_undo, p, i
nxy = (*p).xsize * (*p).ysize	

process:
	if all and (w lt 1.0e-6) then begin				; kill /all needs to be streamlined for speed;
		(*(*p).image)[*pq + nxy*i] = 0.0			; ignore smooth edges of kill region in this case
		print,'	Cleared plane ',i

	endif else begin
		if w gt 1.0e-6 then begin
			scale = (*(*p).image)[*,*,i]
			scale[*,*] = 1.0
			scale[*pq] = w
			
			q_to_xy, *pq, (*p).xsize, x,y
			d1 = min([max(x)-min(x),max(y)-min(y)])
			d = ceil(((*p).xsize * 0.0001) < d1)
			if d ge 2 then scale = smooth2( scale, d, /edge_truncate)
			
			(*(*p).image)[*,*,i] = (*(*p).image)[*,*,i] * scale
		endif else begin
			(*(*p).image)[*pq + nxy*i] = 0.0
		endelse
	endelse
	
	add_history, (*p).history, i, 'Suppress a region, by = '+ str_tidy(w)
	if xanes or all then begin
		i = i+1
		if i lt n_el then goto, process
	endif

	if silent then goto, done						; do these only at end of all
	set_image_minmax, p, (*p).image, opt

	draw_images, pstate
	notify, 'image-display', from=event.top

done:
	widget_control, hourglass=0
    return
bad_exclude:
	warning,'Image_Process_Suppress_Region',['You need to select a region first.', $
		 'Using both "include" and "exclude" region select modes.', $
         'Then press the analyze button, and repeat this operation.']
    goto, done
bad_region:
	warning,'Image_Process_Suppress_Region',['No region selected, or Association highlight found.', $
		 'You need to select a region (or an Association highlight) first.', $
         'Then press the analyze button, and repeat this operation.']
    goto, done
end

;-----------------------------------------------------------------

pro Image_Process_Merge_Gamma, Event

COMPILE_OPT STRICTARR
common c_pige, use_PIGE_yields

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hour

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p
(*p).undo.ok = 0
xanes_stack_test, p, xanes, n_el, el, el_xanes

; If this flag is set, then apply empirical yield factors (only valid at CSIRO set-up) to get ppm.

if use_PIGE_yields then begin
    n_comp = 9
    comps = ['B','F','Na','Mg','Al','Si','Cl','P','Li']
endif else begin
    comps = ''
    for i=0L,n_el-1 do begin
       s = strsplit( (*(*p).el)[i], ' ', /extract)
       q = where( s[0] eq comps)
       if q[0] eq -1 then begin
         comps = [comps, s[0]]
       endif
    endfor
    if n_elements(comps) gt 1 then begin
       comps = comps[1:*]
       n_comp = n_elements(comps)
    endif else begin
       goto, done
    endelse
endelse

; donut2x-2, Benning, "Gamma.cuts" gamma element list:

;img = fltarr((*p).xsize,(*p).ysize,n_comp)
;img[*,*,0] = (*(*p).image)[*,*,1] + (*(*p).image)[*,*,2] + (*(*p).image)[*,*,10]     ; B
;img[*,*,1] = (*(*p).image)[*,*,11] + (*(*p).image)[*,*,12]                    ; F
;img[*,*,2] = (*(*p).image)[*,*,0] + (*(*p).image)[*,*,8]                    ; Na
;img[*,*,3] = (*(*p).image)[*,*,6] + (*(*p).image)[*,*,13] + $                 ; Mg
;      (*(*p).image)[*,*,14] + (*(*p).image)[*,*,15] + (*(*p).image)[*,*,16]
;img[*,*,4] = (*(*p).image)[*,*,3] + (*(*p).image)[*,*,4]                    ; Al
;img[*,*,5] = (*(*p).image)[*,*,5] + (*(*p).image)[*,*,9]                    ; Si

img = fltarr((*p).xsize,(*p).ysize,n_comp)
for i=0L,n_el-1 do begin
    s = strsplit( (*(*p).el)[i], ' ', /extract)
    q = where( s[0] eq comps)
    if q[0] ne -1 then begin
       img[*,*,q[0]] = img[*,*,q[0]] + (*(*p).image)[*,*,i]
    endif
endfor

; Yield factors ...

if use_PIGE_yields then begin
    img[*,*,0] = 0.44 * img[*,*,0]                                   ; B
    img[*,*,1] = 0.1 * img[*,*,1]                                	 ; F
    img[*,*,2] = 0.29 * img[*,*,2]                                   ; Na
    img[*,*,3] = 4.3 * img[*,*,3]                                 	 ; Mg
    img[*,*,4] = 1.53 * img[*,*,4]                                   ; Al
    img[*,*,5] = 39.7 * img[*,*,5]                                   ; Si
    img[*,*,6] = 1.0 * img[*,*,6]                                    ; Cl		need to update!
    img[*,*,7] = 1.0 * img[*,*,7]                                    ; P			"
    img[*,*,8] = 0.77 * img[*,*,8]                                   ; Li
	(*p).type = 0									; now in ppm.uC units
endif

; Variance (needs scale factor-squared) ...

if (*p).has_errors eq 1 then begin
    xesize = n_elements( (*(*p).error)[*,0,0] )
    yesize = n_elements( (*(*p).error)[0,*,0] )
    var = fltarr(xesize,yesize,n_comp)

    for i=0L,n_el-1 do begin
       s = strsplit( (*(*p).el)[i], ' ', /extract)
       q = where( s[0] eq comps)
       if q[0] ne -1 then begin
         var[*,*,q[0]] = var[*,*,q[0]] + (*(*p).error)[*,*,i]
       endif
    endfor

;   var[*,*,0] = (*(*p).error)[*,*,1] + (*(*p).error)[*,*,2] + (*(*p).error)[*,*,10]  ; B
;   var[*,*,1] = (*(*p).error)[*,*,11] + (*(*p).error)[*,*,12]                   ; F
;   var[*,*,2] = (*(*p).error)[*,*,0] + (*(*p).error)[*,*,8]                 ; Na
;   var[*,*,3] = (*(*p).error)[*,*,6] + (*(*p).error)[*,*,13] + $               ; Mg
;          (*(*p).error)[*,*,14] + (*(*p).error)[*,*,15] + (*(*p).error)[*,*,16]
;   var[*,*,4] = (*(*p).error)[*,*,3] + (*(*p).error)[*,*,4]                 ; Al
;   var[*,*,5] = (*(*p).error)[*,*,5] + (*(*p).error)[*,*,9]                 ; Si

;   Yield factors ...

    if use_PIGE_yields then begin
       var[*,*,0] = 0.44*0.44 * var[*,*,0]                            ; B
       var[*,*,1] = 0.1*0.1 * var[*,*,1]                              ; F
       var[*,*,2] = 0.29*0.29 * var[*,*,2]                            ; Na
       var[*,*,3] = 4.3*4.3 * var[*,*,3]                              ; Mg
       var[*,*,4] = 1.53*1.53 * var[*,*,4]                            ; Al
       var[*,*,5] = 39.7*39.7 * var[*,*,5]                            ; Si
       var[*,*,6] = 1.0*1.0 * var[*,*,6]                              ; Cl
       var[*,*,7] = 1.0*1.0 * var[*,*,7]                              ; P
       var[*,*,8] = 0.78*0.78 * var[*,*,8]                            ; Li
    endif
endif

history = ptrarr( n_comp)
if ptr_valid((*p).history) then begin
    for i=0L,n_elements(*(*p).history)-1 do begin
       if ptr_valid((*(*p).history)[i]) then begin
         ptr_free, (*(*p).history)[i]
       endif
    endfor
    ptr_free, (*p).history
endif
(*p).history = ptr_new( history, /no_copy)

opt = define( /options_image)
options = replicate( opt, n_comp) 
escale = options

if ptr_valid((*p).options) then ptr_free, (*p).options
(*p).options = ptr_new( options, /no_copy)

mdl = fltarr( n_comp)
if ptr_valid((*p).matrix.mdl) then ptr_free, (*p).matrix.mdl
(*p).matrix.mdl = ptr_new( mdl, /no_copy)

if (*p).has_errors then begin
    if ptr_valid((*p).escale) then ptr_free, (*p).escale
    (*p).escale = ptr_new( escale, /no_copy)

    if ptr_valid((*p).error) then ptr_free, (*p).error
    (*p).error = ptr_new( var, /no_copy)
endif

n_el = n_comp
ptr_free, (*p).el
(*p).el = ptr_new(comps,/no_copy)
(*p).n_el = n_el

ptr_free, (*p).image
(*p).image = ptr_new( img, /no_copy)

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif
set_image_view, pstate, event.top

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
if ((*pstate).clone eq 1) then begin
    (*pstate).local = 0
    (*p).orphan = 1
endif else begin
    (*pstate).local = 1
    (*p).orphan = 0
endelse
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top
*(*pstate).pselect = 1

done:
end

;-----------------------------------------------------------------

; Later do a spectrum ADD function ...

pro Image_Process_Add, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p
xanes_stack_test, p, xanes, n_el, el, el_xanes

for i=0L,n_el-1 do begin
    print, i, el[i]
endfor

id = 1   ; destination
ia = 1   ; source 1
ib = 2   ; source 2

(*(*p).image)[*,*,id] = (*(*p).image)[*,*,ia] + (*(*p).image)[*,*,ib]
el[id] = strtrim(el[ia],2) + "+" + strtrim(el[ib],2)
if xanes eq 0 then *(*p).el = el

set_image_view, pstate, event.top

if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
(*pstate).pstate = ptr_new( *pstate)
if ((*pstate).clone eq 1) then begin
    (*pstate).local = 0
    (*p).orphan = 1
endif else begin
    (*pstate).local = 1
    (*p).orphan = 0
endelse
notify, 'image-clone', (*pstate).pstate, from=event.top
notify, 'images-changed', (*pstate).p, from=event.top
*(*pstate).pselect = 1

done:
end

;-----------------------------------------------------------------

;Principal component images

pro Image_Process_Test, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p1
opt = (*p).options
widget_control, /hourglass
xanes_stack_test, p, xanes, n_el, el, el_xanes

pimg = (*p).image                ; pointer to the image arrays for all elements
nx = (*p).xsize                    ; X size of image in pixels
ny = (*p).ysize                    ; Y size of image in pixels

q = where( ( strmid(el,0,4) ne 'Back') and (el ne 'Sum'), nq)
if nq lt 1 then goto, done

image = reform((*pimg)[*,*,q], nx*ny, nq) > 0.0
data = transpose(image)

result = pcomp( data, coefficients=coeff, eigenvalues=eigen, $
         variances=var, /standardize, /double)

img = reform( transpose(result), nx,ny, nq)

for i=0L,nq-1 do begin
    if xanes eq 0 then (*(*p).el)[q[i]] = 'c'+str_tidy(i)
    (*opt)[q[i]].min = min(img[*,*,i])
    (*opt)[q[i]].max = max(img[*,*,i])
    (*pimg)[*,*,q[i]] = img[*,*,i]
    add_history, (*p).history, i, 'Eigenvalue ' + str_tidy(eigen[i])
endfor

;if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
;   opt = (*p).escale
;   err = fltarr(nx/2,ny/2,n_el)
;   for i=0L,n_el-1 do begin
;     (*opt)[i].min = min(err[*,*,i])
;     (*opt)[i].max = max(err[*,*,i])
;   endfor
;endif else (*p).has_errors=0

set_image_view, pstate, event.top

(*pstate).image = 0
(*pstate).display_mode = 0
widget_control, (*pstate).element_id, set_combobox_select=0
widget_control, (*pstate).element_id, set_value=el
widget_control, (*pstate).mode_id, set_combobox_select=0

draw_images, pstate
notify, 'image-display', from=event.top

done:
end

;-----------------------------------------------------------------

; test of clusters

pro Image_Process_Test_cluster, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if ptr_valid( (*p).image ) eq 0 then goto, done
if ptr_valid( (*p).options ) eq 0 then fix_options, p
if ((*p).has_errors eq 1) and (ptr_valid( (*p).escale ) eq 0) then fix_options, p1
opt = (*p).options
widget_control, /hourglass
xanes_stack_test, p, xanes, n_el, el, el_xanes

pimg = (*p).image                ; pointer to the image arrays for all elements
nx = (*p).xsize                    ; X size of image in pixels
ny = (*p).ysize                    ; Y size of image in pixels

q = where( ( strmid(el,0,4) ne 'Back') and (el ne 'Sum'), nq)
if nq lt 1 then goto, done

image = reform((*pimg)[*,*,q], nx*ny, nq) > 0.0
data = transpose(image)

weights = clust_wts( data, n_clusters=15)
result = cluster( data, weights )

img = reform( transpose(result), nx,ny, nq)

q = [1]
nq = 1
for i=0L,nq-1 do begin
    if xanes eq 0 then (*(*p).el)[q[i]] = 'c'+str_tidy(i)
    (*opt)[q[i]].min = min(img[*,*,i])
    (*opt)[q[i]].max = max(img[*,*,i])
    (*pimg)[*,*,q[i]] = img[*,*,i]
;   add_history, (*p).history, i, 'Cluster ' + str_tidy(eigen[i])
endfor

;if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
;   opt = (*p).escale
;   err = fltarr(nx/2,ny/2,n_el)
;   for i=0L,n_el-1 do begin
;     (*opt)[i].min = min(err[*,*,i])
;     (*opt)[i].max = max(err[*,*,i])
;   endfor
;endif else (*p).has_errors=0

set_image_view, pstate, event.top

(*pstate).image = 0
(*pstate).display_mode = 0
widget_control, (*pstate).element_id, set_combobox_select=0
widget_control, (*pstate).element_id, set_value=el
widget_control, (*pstate).mode_id, set_combobox_select=0

draw_images, pstate
notify, 'image-display', from=event.top

done:
end

;-----------------------------------------------------------------

pro image_process_wizard, event

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
       warning,'Image_Process_wizard',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check wizard source code,', $
          'and that wizard SAV file is on path.'], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, event.id, get_uvalue=file
if n_elements(file) lt 1 then goto, done

routine = strip_path( file)
routine = strip_file_ext( routine)

restore, file, /verbose
call_procedure, routine

done:
end

;-----------------------------------------------------------------

pro Image_Process_Zeroes, Event, arg, silent=silent, no_undo=no_undo, select=i

; arg not used (passed from process menu, etc.)

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
if n_elements(no_undo) lt 1 then no_undo=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
widget_control, /hourglass

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
if n_elements(i) lt 1 then i = (*pstate).image

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done
xanes_stack_test, p, xanes, n_el, el, el_xanes

img = (*pimg)[*,*,i]							; image data for this element

threshold = ((*opt)[i].bottom + 0.03 * ((*opt)[i].top - (*opt)[i].bottom)) * (*opt)[i].max/100.

q = where( img lt threshold)					; threshold at 3% of top slider
if q[0] eq -1 then begin
	Print, 'Image, No zeroes found to repair.'
	return
endif

if (*pstate).display_mode eq 0 then begin
	Print, 'Image, first pass ...'
endif else begin
	Print, 'Variance, first pass ...'
endelse
image_correct_zero, q, pimg, (*p).flux, neighbours=2, remain=qz
Print, '	Second pass ...'
if qz[0] ne -1 then image_correct_zero, qz, pimg, (*p).flux, neighbours=2, remain=qz
Print, '	Third pass ...'
if qz[0] ne -1 then image_correct_zero, qz, pimg, (*p).flux, neighbours=2, remain=qz

for j=0L,n_el-1 do begin
	set_image_minmax, p, pimg, opt
	
	if (*pstate).display_mode eq 0 then begin
	    add_history, (*p).history, j, '* Correct zero pixels ['+(*(*p).el)[i]+']'
	endif
endfor

if (*pstate).display_mode eq 0 then begin   	   ; correct variance as well
    (*pstate).display_mode = 1
    check_mode, pstate
    if (*pstate).display_mode eq 1 then begin
       Image_Process_Zeroes, event, /silent, /no_undo, select=i
    endif
    (*pstate).display_mode = 0
endif

if silent eq 0 then begin
    draw_images, pstate
    notify, 'image-display', from=event.top
endif

done:
end

;-----------------------------------------------------------------

pro image_Print, Event

end

;-----------------------------------------------------------------

pro image_reset_display_range, event

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
       warning,'image_reset_display_range',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugin source code.'], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good( p) eq 0 then goto, done

	set_image_minmax, p, (*p).image, (*p).options
	if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
		set_image_minmax, p, (*p).error, (*p).escale
	endif
	draw_images, pstate

done:
end

;-----------------------------------------------------------------

pro Image_Reload_Plugins, Event

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
       warning,'Image_Reload_Plugins',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	
	plugins = Image_Load_Plugins( error=error)
	if error eq 0 then begin
		if ptr_valid( (*pstate).plugins) then ptr_free, (*pstate).plugins
		(*pstate).plugins = plugins

		widget_control, (*pstate).plugin_menus, /destroy
		plugin_menus = Widget_Button( (*pstate).plugin_menus_root, UNAME='W_MENU_57', /menu, VALUE='User Plugins' )		
		(*pstate).plugin_menus = plugin_menus

		for i=0L,n_elements((*plugins).title)-1 do begin
			W_MENU = Widget_Button((*pstate).plugin_menus, UNAME='Plugin', VALUE=(*plugins).title[i], $
						uvalue=(*plugins).list[i] )
		endfor
	endif
	return
end

;-----------------------------------------------------------------

pro image_Results_Table, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

image_table, group_leader=event.top, path=*(*pstate).path, TLB=tlb, $
          pregions=(*pstate).pregions, dpath=*(*pstate).dpath, $
          realtime=(*pstate).realtime, xanes=(*pstate).xanes
register_notify, event.top, $
	[	'spectra', $					; spectra have changed (pass on)
		'path', $						; new path
		'dpath', $						; new raw data path
		'image-results', $				; new conc results (pass on)
		'image-regions', $				; regions pointers, if valid w/ spectra
		'image-match-centroids', $		; match centroids for all XANES frames
		'image-region-update', $		; update all request from Image_Table
		'image-region-update-one', $	; update one request from Image_Table
		'image-region-delete', $		; delete selected region (and spectrum)
		'image-region-clear', $			; clear current marker, just prior to select
		'image-region-select', $		; pass on notify of image_table region-select
		'image-spectrum-throttle', $		; pass on notify of image_table region throttle
		'image-kill-regions-all-planes' $ ; image_table clear region in all planes
	], from=tlb
end

;-----------------------------------------------------------------

pro image_results_Plot, Event

end

;-----------------------------------------------------------------

pro image_Select_Image, Event

;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate

;image_select, group_leader=event.top, TLB=tlb
;register_notify, event.top, 'image-display', from=tlb
end

;-----------------------------------------------------------------

pro Image_Spectrum_Display, Event, $
       start_identify=start_identify, start_PIXE=start_PIXE

COMPILE_OPT STRICTARR
if n_elements(start_identify) lt 1 then start_identify=0
if n_elements(start_PIXE) lt 1 then start_PIXE=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

screen = get_screen_size()

spectrum_display, group_leader=event.top, path=*(*pstate).path, TLB=tlb, test=(*pstate).test, $
              image=(*pstate).p, start_identify=start_identify, start_PIXE=start_PIXE, $
              dpath=*(*pstate).dpath

register_notify, event.top, $
          ['images', $              ; new images loaded
          'path', $               ; new path
          'dpath', $               ; new raw data path
          'mark-e', $				; mark line from Identify (pass to setup-filter)
          'image-display', $          ; returned after image pileup subtraction
          'image-region-select', $	; new Region row selected --> result-properties
          'spectra' $               ; pass on notify of new spectra loaded
          ], from=tlb
end

;-----------------------------------------------------------------

pro image_Save, Event, default=default, overwrite=overwrite

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(default) lt 1 then default=0
if n_elements(overwrite) lt 1 then overwrite=0
if ptr_valid((*pstate).p) then begin
    if ptr_valid((*(*pstate).p).image) eq 0 then goto, nothing
endif else goto, nothing

if (*pstate).xanes then begin
	ext1 = '*.xan'
	ext = ['-m.xan','-g.xan','-r.xan','-e.xan','-s.xan','-c.xan','-d.xan','-x.xan']
;	ext = '-' + (*(*pstate).p).el + ['-m.xan','-g.xan','-r.xan','-e.xan','-s.xan','-c.xan','-d.xan','-x.xan']
	type = strip_file_ext(ext[ (*(*pstate).p).detector])
	if overwrite then ext = '.xan'
	title = '3D Image Stack file to write'
	preview = 'image_stack_preview'
	image_stack_preview
endif else begin
	ext1 = '*.dai'																		; '*.dai'
	ext = ['-m.dai','-g.dai','-r.dai','-e.dai','-s.dai','-c.dai','-d.dai','-x.dai']		; '-m.dai'
	type = strip_file_ext(ext[ (*(*pstate).p).detector])								; '-m'
	if overwrite then ext = '.dai'														; '.dai'
	title = 'Image file to write'
	preview = 'image_geopixe_preview'
	image_geopixe_preview
endelse

file = find_file2( (*pstate).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
file = strip_file_m( strip_file_ext((*(*pstate).p).file[0]), ending=type)				; 'dir/fred'
file = strip_path(file) + ext[ (*(*pstate).p).detector < (n_elements(ext)-1)]			; 'fred-m.dai', 'fred.dai'

if default then begin
    F = *(*pstate).path + file
endif else begin
    F = file_requester( /write, filter=ext1, path=*(*pstate).path, /image, $
				title=title, file=file, dialog_parent=event.top, fix_filter=1, $
				preview_routine=preview)
endelse
if F ne '' then begin
    widget_control, /hourglass
    *(*pstate).path = extract_path(F[0])
    
    if (*pstate).xanes then begin
		write_geopixe_image, (*pstate).p, F
    endif else begin
    	write_geopixe_image, (*pstate).p, F, no_null=(*pstate).realtime
    endelse
	(*pstate).file = F[0]
endif

done:
    return

nothing:
    warning,'image_Save','There is no image data to save!'
    goto, done
end

;-----------------------------------------------------------------

pro Image_Save_Chimage, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

; This uses an old 'element_select' modal popup
; For new parameters, make a new popup that uses the new 'select_element'
; compount widget (see 'export_select' as a guide).

select = element_select( event.top, *(*(*pstate).p).el, old_select=*(*pstate).pselect, path=*(*pstate).path )
qselect = where(select eq 1)
if qselect[0] eq -1 then goto, done
*(*pstate).pselect = select

ext = '.chi'
title = 'Chimage Export file to Write'

file = find_file2( (*pstate).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
chi_file = strip_path( strip_file_ext( (*pstate).file)) + ext

F = file_requester( /write, filter = '*'+ext, path=path, $
         title=title, file=chi_file, group=event.top, fix_filter=1)
if F ne '' then begin
    F = strip_file_ext(F) + ext
    widget_control, /hourglass

    if ptr_valid((*pstate).p) then begin
       if ptr_valid((*(*pstate).p).image) then begin
         write_chimage, (*pstate).p, F, select=select
       endif else goto, nothing2
    endif else goto, nothing2
endif

done:
    return

nothing2:
    warning,'Image_Save_Chimage','There is no image data to save!'
    goto, done
end

;-----------------------------------------------------------------

pro Image_Save_Zarr, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

title = 'Select Zarr Export dir'

file = find_file2( (*pstate).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
file = fix_path( strip_file_ext(file[0]) + '.zarr')

F = file_requester( /write, /dir, filter = '*.zarr', path=file, $ 
         title=title, group=event.top, fix_filter=1)
if F ne '' then begin
    if ptr_valid( (*pstate).p) then begin
	    widget_control, /hourglass
		write_geopixe_zarr, (*pstate).p, F[0]
    endif else goto, nothing2
endif

done:
    return

nothing2:
    warning,'Image_Save_Zarr','There is no image data to save!'
    goto, done
end

;-----------------------------------------------------------------

pro image_Save_GIF, Event, png=png, tiff=tiff, only_shape=only_shape, only_highlight=only_highlight, $
					colour=colour, jpeg=jpeg, RGB=RGB

;	/PNG	save as PNG
;	/TIFF	save as TIFF (float)
;	/RGB	used to save images that are elements "Red", "Green", "Blue"

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(png) lt 1 then png=0
if n_elements(jpeg) lt 1 then jpeg=0
if n_elements(only_shape) lt 1 then only_shape=0
if n_elements(only_highlight) lt 1 then only_highlight=0
if n_elements(colour) lt 1 then colour=spec_colour('green')
if n_elements(RGB) lt 1 then RGB=0
if n_elements(TIFF) lt 1 then TIFF=0

if png then begin
    ext = '.png'
    title = 'PNG file to Write'
endif else if tiff then begin
    ext = ['.tiff','.tif']
    title = 'TIFF file to Write'
endif else if jpeg then begin
    ext = '.jpg'
    title = 'JPEG file to Write'
endif else begin
    ext = '.gif'
    title = 'GIF file to Write'
endelse
p = (*pstate).p
if ptr_good( p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes

s = strtrim( el[(*pstate).image], 2)

file = find_file2( (*pstate).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
gif_file = strip_path( strip_file_ext( (*pstate).file))

if RGB then begin
	if n_el ne 3 then begin
		print,"Doesn't seem to be an RGB image ..."
		return
	endif
	opt = *(*p).options
	bottom = (opt.bottom)*(opt.max)/100.

	sx = (*p).xsize
	sy = (*p).ysize
	r = uintarr(3,sx,sy)
	for j=0,2 do begin
		r[j,*,*] = uint(((*(*p).image)[*,*,j] - bottom[j]) > 0)
	endfor

	file = strip_file_ext(file) + '.png'
	write_png, file, r
	return
endif

if only_shape then begin
    gif_file = gif_file + '-shape'
endif else if only_highlight then begin
    gif_file = gif_file + '-highlight'
endif else begin
    gif_file = gif_file + '-' + s
endelse
gif_file = gif_file + ext[0]
image_preview

F = file_requester( /write, filter = '*'+ext, path=path, /image, preview_routine='image_preview', $
         title=title, file = gif_file, group=event.top, fix_filter=0)
if F ne '' then begin
    F = strip_file_ext(F) + ext[0]

	b = make_tvb( pstate, (*pstate).image, highlight=only_highlight, colour=colour)
    window, /free, xsize=n_elements(b[*,0]), ysize=n_elements(b[0,*]), /pixmap
    tpix = !d.window

    if only_shape then begin
       erase
    endif else begin
		tv, b
    endelse
    plot_mark, pstate, /wide

	if jpeg then begin
		b = tvrd(true=1)
		write_jpeg, F, b, true=1, quality=100
	endif else if tiff then begin
		data = (*(*p).image)[*,*,(*pstate).image]
	   	write_tiff, F, data, /float, orientation=0;	, description=s2;	, compression=1
	endif else begin
		b = color_quan( tvrd(true=3), 3, rcol,gcol,bcol, colors=128)
		if png then begin
			if only_shape or only_highlight then begin
				write_png, F, b, rcol,gcol,bcol, transparent=[0]
			endif else begin
				write_png, F, b, rcol,gcol,bcol
			endelse
		endif else begin
			write_gif, F, b, rcol,gcol,bcol
		endelse
	endelse
    if tpix ne 0 then wdelete, tpix
endif

done:
end

;-----------------------------------------------------------------

; This is the full HTML writer. There is also one used for external 
; programs (see "save_image_all_HTML") that generate HTML, such as
; the dir crawler app and 'geopixe_index', which does not show a SHAPE.

pro image_Save_all_HTML, Event, bw=bw, PNG=PNG, gif=gif, file=F, first=first

COMPILE_OPT STRICTARR
;common Colors, ro,go,bo, rr,gg,bb

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
       warning,'Image_save_all_HTML',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, bad_io
    endif
endif

if n_elements(bw) lt 1 then bw=0
if n_elements(png) lt 1 then png=0
if n_elements(gif) lt 1 then gif=0
if n_elements(first) lt 1 then first=1
if png eq 1 then gif=0
if gif eq 0 then png=1

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, done

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done
xanes_stack_test, p, xanes, n_el, el, el_xanes

tpix = 0
if (*pstate).display_mode eq 1 then begin
    stype = 'Variance'
endif else if (*p).type eq 1 then begin
    stype = 'Mass Fraction'
endif else if (*p).type eq 2 then begin
    stype = 'Counts'
endif else begin
    stype = 'Concentration'
endelse
if png then begin
    ext = '.png'
    gtitle = 'PNG'
endif else begin
    ext = '.gif'
    gtitle = 'GIF'
endelse

if n_elements(F) lt 1 then begin
    file = find_file2( (*pstate).file)
    path = extract_path( file[0])
    if lenchr(path) eq 0 then path = *(*pstate).path
    path = path + 'html' + path_sep()
	if file_test(path,/dir) eq 0 then begin
		safe_file_mkdir, path, error=error
		if error then begin
			warning,'image_Save_all_HTML',['Illegal directory name:',path]
		endif
	endif
    s = strip_path(strip_file_m(strip_file_ext( (*pstate).file)))
    if (*p).detector eq 1 then begin
       s = strip_file_g(s)
       s = s + '-g'
    endif
    if (*pstate).display_mode eq 1 then s = s + '-var'
    if bw then s = s + '-bw'
    file = s + '.html'

    F = file_requester( /write, filter = '*.html', fix_filter=1, path=path, $
       title='Write a '+stype+' HTML file (and '+gtitle+'s)', file = file, dialog_parent=event.top)
endif
if F ne '' then begin

;   This uses an old 'element_select' modal popup
;   For new parameters, make a new popup that uses the new 'select_element'
;   compount widget (see 'export_select' as a guide).

	if first then begin
		title = 'Select elements for HTML file'
		if bw then title = title + ' (in b/w)'
		select = element_select( event.top, el, old_select=*(*pstate).pselect3, $
					title=title, path=*(*pstate).path )
		qselect = where(select eq 1)
		if qselect[0] eq -1 then goto, done
		*(*pstate).pselect3 = select
	endif
	qselect = where(*(*pstate).pselect3 eq 1)
	if qselect[0] eq -1 then goto, done

	F = strip_file_ext(F) + '.html'
	path = extract_path(F)
	if file_test(path,/dir) eq 0 then begin
		safe_file_mkdir, path, error=error
		if error then begin
			warning,'image_Save_all_HTML',['Illegal directory name:',path]
		endif
	endif

	if bw then begin
;		rc = rr
;		gc = gg
;		bc = bb
		tvlct,rc,gc,bc,/get
		loadct, 0, bottom=16, ncolors=100
		tvlct, rr,gg,bb, /get
		rr[16:115] = reverse(rr[16:115])
		gg[16:115] = reverse(gg[16:115])
		bb[16:115] = reverse(bb[16:115])
		tvlct, rr[16:115],gg[16:115],bb[16:115], 16
	endif else begin
		tvlct, rr,gg,bb, /get
	endelse
	widget_control, /hourglass

	on_ioerror, bad_io
	openw,1, F
	printf,1,'<HTML>'
	printf,1,'<HEAD>'
	printf,1,'<TITLE>',F,'</TITLE>'
	printf,1,'</HEAD>'
	printf,1,'<BODY BGCOLOR="#FFFFFF">'
	printf,1,'<H3>Dynamic Analysis PIXE/SXRF Imaging</H3>'
	printf,1,'<H4>',(*p).source,'</H4>'
	printf,1,'<font size="-1">These X-ray elemental images were collected in "'+(*p).DevObj->title()+'" format.'
	printf,1,'The event data were analyzed using the'
	printf,1,'<A href="http://www.nmp.csiro.au/dynamic.html">CSIRO Dynamic Analysis method</A> in GeoPIXE,'
	printf,1,'which enables quantitative, true-elemental images to be un-mixed from the generally complex'
	printf,1,'PIXE/SXRF energy spectrum. The images shown below are at fixed size (200 pixels) in this summary. To view its original size,'
	printf,1,'click on the image.</font><p>'
	if (*pstate).display_mode eq 1 then begin
		printf,1,'<p><font size="-1">These images show VARIANCE.</font><p>'
	endif
	if (*p).type eq 1 then begin
		printf,1,'<p><font size="-1">These images show end-member Mass Fraction.</font><p>'
	endif

	first = 1
	for i=0L,n_elements(qselect)-1 do begin
		s = strtrim( el[qselect[i]], 2)
		st = s
		if (*pstate).display_mode eq 1 then s = s + '-var'
		if bw then s = s + '-bw'
		s = fix_file_name(s, /all)

		F = strip_path(F)
		gif_file = strip_file_ext(F) + '-small-' + s + ext       ; small postage stamps
		gif_file2 = strip_file_ext(F) + '-' + s + ext        ; normal size
		gif_file = path + fix_file_name( gif_file)
		gif_file2 = path + fix_file_name( gif_file2)

;		Build the image and its mark area shape in the temp pixmap.
;		make_tvb may scale the image. Compress will return the scaling factor.
;		Scale plot_mark by the same amount.

;		b = make_tvb( pstate, qselect[i], /nozoom, xtgt=200,ytgt=200, compress=compress, $
;				xmin=100,ymin=100, xmax=700,ymax=1000)

		b = make_tvb( pstate, qselect[i], /nozoom, xtgt=200,ytgt=200, compress=compress)
		if first then begin
			window, /free, xsize=n_elements(b[*,0]), ysize=n_elements(b[0,*]), /pixmap
			tpix = !d.window
			first = 0
		endif
		tv, b
		plot_mark, pstate, compress=compress, /wide
		col = 115
		colb = 16
		if bw then col = spec_colour('red')
;		if bw then colb = spec_colour('white')
		xyouts, 1,2, st, /device, color=colb, charsize=1.5,charthick=10.0
		xyouts, 1,2, st, /device, color=col, charsize=1.5,charthick=1.2
		b = color_quan( tvrd(true=3), 3, rcol,gcol,bcol, colors=128)

;		b = pseudo_match( tvrd(true=3), true=3, ncolors=116)

		if n_elements(b) gt 1 then begin
;			print,'HTML: save small ',gtitle,' file - ',gif_file

			if png then begin
				write_png, gif_file, b, rcol,gcol,bcol
			endif else begin
				write_gif, gif_file, b, rcol,gcol,bcol
			endelse

			printf,1,'<A HREF="',strip_path(gif_file2),'"><IMG SRC="',strip_path(gif_file),'" ALIGN="CENTER"></A> '

		endif else begin
			print,'image_save_all_HTML:  Bad small image number ', qselect[i]
		endelse

		b = make_tvb( pstate, qselect[i], /nozoom)

		if n_elements(b) gt 1 then begin
;			print,'HTML: save large ',gtitle,' file - ',gif_file2

			if png then begin
				write_png, gif_file2, b, rr, gg, bb
;				write_png, gif_file2, b
         	endif else begin
				write_gif, gif_file2, b, rr, gg, bb
			endelse

		endif else begin
			print,'image_save_all_HTML:  Bad large image number ', qselect[i]
		endelse
	endfor
	printf,1,'<P>'

	legend_file = strip_file_ext( F) + '-legend'+ext
	legend_file = path + fix_file_name( legend_file)

	b = bytarr(300,30)
	for i=0L,300-1 do b[i,*] = byte(i/3) + 16B

	if png then begin
		write_png, legend_file, b, rr, gg, bb
;		write_png, legend_file, b
	endif else begin
		write_gif, legend_file, b, rr, gg, bb
	endelse

	if (*pstate).display_mode eq 1 then begin
		printf,1,'<center><H4>Relative Statistical Variance Legend</H4></center>'
	endif else begin
		if (*p).type eq 1 then begin
			printf,1,'<center><H4>Relative Mass Fraction Legend</H4></center>'
		endif else begin
			printf,1,'<center><H4>Relative Concentration Legend</H4></center>'
		endelse
	endelse
	printf,1,'<center>Zero <img SRC="'+strip_path(legend_file)+'" align="CENTER" valign="CENTER"> Maximum</center><p>'

    printf,1,'<H4>Dynamic Matrix: ',(*p).matrix.file,'</H4>'
	printf,1,'<font size="-1">Sample: ', (*p).sample, ' '
	printf,1,'Grain: ', (*p).grain, '<BR>'
	printf,1,'Comment: ', (*p).comment, '<BR>'
	printf,1,'Charge: ', (*p).charge, '<BR>'
	printf,1,'Cal:  A= ', (*p).cal.poly[1],', B= ',(*p).cal.poly[0],', Units= ',(*p).cal.units, '<BR><P>'
	printf,1,'Scan:  X= ', (*p).scan.x,', Y= ',(*p).scan.y, ' mm<BR>'
	printf,1,'X Compress: ', (*p).xcompress,', Y Compress: ',(*p).ycompress, '<BR>'
	printf,1,'X Scaled: ', (*p).scaled_x,', Y Scaled: ',(*p).scaled_y, '<BR>'
	printf,1,'X size: ', (*p).xsize,', Y Size: ',(*p).ysize, '<BR><P>'
	if xanes eq 0 then begin
		printf,1,'Processed: ', (*p).processed,', Bad: ',(*p).bad_xy, '<BR>'
		printf,1,'Valid: ', (*p).valid,', Clipped: ',(*p).clipped, '<BR><P>'
	endif
	printf,1,'DA Matrix: ', (*p).matrix.file, '<BR>'
	printf,1,'DA Source: ', (*p).matrix.label, '<BR>'
	printf,1,'DA Charge: ', (*p).matrix.charge, '<BR><P>'
	if (*p).xstep_on eq 1 then begin
		printf,1,'X Step Scan mode. XStep count= ', (*p).xstep, '<BR><P>'
	endif
	if (*p).xstep_on eq 2 then begin
		printf,1,'Y Step Scan mode. YStep count= ', (*p).xstep, '<BR><P>'
	endif
	printf,1,'</font>'

	if (*p).type eq 1 then begin
		charge_per_pixel = 1.0
		units = ''
		percent = 0
	endif else begin
		charge_per_pixel = (*p).charge / (sx * sy)
		units = 'ppm'
		percent = 1
	endelse
	printf,1,'<H4>Display maximum values</H4>'

	printf,1,'<font size="-1"><table>'
;	printf,1,'<colgroup span=6 width="130,130,130,130,130,130"></colgroup>'
	printf,1,'<tr>'
	j = 0
	for i=0L,n_elements(qselect)-1 do begin
		top = ((*opt)[qselect[i]].max * (*opt)[qselect[i]].top / 100.) / charge_per_pixel
		if (*pstate).display_mode eq 1 then begin
			sc = build_result( sqrt(top), sqrt(top)*0.01, 0.0)
			su = units
			if locate('%',sc) ge 0 then su=''
			printf,1,'<td width=135>',el[qselect[i]],' (',sc,' ',+su+')^2 </td>'
		endif else begin
			sc = build_result( top, top*0.01, 0.0, percent=percent)
			su = units
			if locate('%',sc) ge 0 then su=''
			printf,1,'<td width=135>',el[qselect[i]],' ',sc,' '+su+' </td>'
		endelse
		j = j+1
		if j eq 6 then begin
			j = 0
			printf,1,'</tr><tr>'
		endif
	endfor
	printf,1,'</tr></table></font><p>'

	printf,1,'<font size="-1">The counting statistics in a single pixel are generally low. This means that there can'
	printf,1,'be large differences in deduced ppm.charge between neighbouring pixels.'
	printf,1,'This is normal. Estimation of concentration in portions of the image involves'
	printf,1,'integrating an average over the region. This averages out much of this'
	printf,1,'statistics variation to yield concentration estimation with low detection limits.<p>'
	printf,1,'However, these statistics can lead to a misleading colour scale for the image, as '
	printf,1,'the image appears to show concentration variation over a large range at the pixel level.'
	printf,1,'This means that some images show large maximum values despite a generally low concentration.'
	printf,1,'(To help alleviate this problem, use image smoothing filters, or use average concentrations over a selected region, as shown below.)</font><p>'

	printf,1,'<font size="-1">It should also be noted that overlap and background subtraction may lead to some pixels having negative values.'
	printf,1,'For example, areas with zero concentration will have as many negative as positive pixels. '
	printf,1,'When regions are integrated, all values are averaged and the correct concentration results. However,'
	printf,1,'some images may appear to show areas with positive signal due to statistical fluctuations'
	printf,1,'despite zero average concentration because negative pixels are shown in black.</font><p>'

	if (*pstate).display_mode eq 0 then begin
		if (*p).type eq 1 then begin
			printf,1,'<H4>Average Mass Fraction in Current Region</H4>'
		endif else begin
			printf,1,'<H4>Average Concentration in Current Region</H4>'
		endelse

 		analyze_image, pstate
		OK = 1
		if ptr_valid((*pstate).pregions) eq 0 then OK=0
		if ((*pstate).region_id lt 0) or ((*pstate).region_id ge n_elements(*(*pstate).pregions)) then OK=0
		if OK then begin
			pr = (*(*pstate).pregions)[(*pstate).region_id]
			if ptr_valid( pr) eq 0 then OK=0
		endif
		if OK then begin
			printf,1,'<font size="-1">This table lists the average elemental concentrations over a selected region of the image.'
			printf,1,'This is intended as a guide for solid samples; please disregard for fluid inclusions.'
			if (*p).type eq 0 then printf,1,'Values shown are ppm unless shown as weight %.'
			printf,1,'The region is shown in the small versions of each image shown below.<p>'
			printf,1,'<table>'
;			printf,1,'<colgroup span=6 width="130,130,130,130,130,130"></colgroup>'
			printf,1,'<tr>'

			j = 0
			for i=1L,(*pr).n_el-1 do begin
				q = where( el eq (*(*pr).el)[i] )
				if q[0] ne -1 then begin
					if select[q[0]] then begin
						sc = build_result( (*(*pr).conc)[i], (*(*pr).error)[i], (*(*pr).mdl)[i], percent=percent)
						su = units
						if locate('%',sc) ge 0 then su=''
						printf,1,'<td width=135>',el[i],' ',sc,' '+su+' </td>'
						j = j+1
						if j eq 6 then begin
							j = 0
							printf,1,'</tr><tr>'
						endif
					endif
				endif
			endfor
			printf,1,'</tr></table></font><p>'
		endif else begin
			printf,1,'<font size="-1">No specific region was specified.</font><p>'
		endelse
	endif

    printf,1,'</BODY>
    printf,1,'</HTML>

bad_io:
	if bw then begin
;		rr = rc
;		gg = gc
;		bb = bc
		tvlct, rc,gc,bc
	endif

	close,1
	if tpix ne 0 then wdelete, tpix
endif

done:
end

;-----------------------------------------------------------------

pro image_Save_all_TIFF, Event, file=F, first=first, mode=mode, var=var, txm=txm

; mode =	0	concentration
;			1	counts
;			2	ng/cm^2
; /var			also variance maps
; /txm			XANES maps for TXM program (for XANES image data)

COMPILE_OPT STRICTARR
;common Colors, ro,go,bo, rr,gg,bb

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
       warning,'image_Save_all_TIFF',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, bad_io
    endif
endif

if n_elements(first) lt 1 then first=1
if n_elements(mode) lt 1 then mode=0
if n_elements(var) lt 1 then var=0
if n_elements(txm) lt 1 then txm=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, done
xanes_stack_test, p, xanes, n_el, els, el_xanes

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

image_flux_charge, p, xanes=xanes, charge=charge, pixels=pixels, conv=conv, flux=flux

; 'image_flux_charge' uses the whole image array
; 'pixels' refers to main 'image' array, not 'error'

tpix = 0
ext = '.tiff'
gtitle = 'TIFF'
if (*p).type eq 1 then begin
    stype = 'Mass Fraction'
	charge_per_pixel = 1.0
	units = ''
	percent = 0
	if (mode ne 0) then begin
		warning,'image_Save_all_TIFF',['This output mode is not allowed with this type of image.','Use normal TIFF output.']
		goto, done
	endif
endif else if (*p).type eq 2 then begin
    stype = 'Counts'
	charge_per_pixel = charge / float(pixels)
	units = 'Counts'
	percent = 0
	if (mode ne 0) and (mode ne 1) then begin
		warning,'image_Save_all_TIFF',['This output mode is not allowed with this type of image.','Use normal TIFF output.']
		goto, done
	endif
endif else begin
    stype = 'Concentration'
	charge_per_pixel = charge / float(pixels)
	percent = 0
	case mode of
		0: begin									; ppm per pixel
			units = 'ppm'
			end
		1: begin									; counts total for full array selected
			units = 'Counts'
			end
		2: begin									; ng/cm**2
			units = 'ng/cm**2'
			end
	endcase
endelse

if n_elements(F) lt 1 then begin
    file = find_file2( (*pstate).file)
    path = extract_path( file[0])
    if lenchr(path) eq 0 then path = *(*pstate).path
    ipath = path
    path = path + 'tiff' + path_sep()
	if file_test(path,/dir) eq 0 then begin
		safe_file_mkdir, path, error=error
		if error then begin
			warning,'image_Save_all_TIFF',['Illegal directory name:',path]
		endif
	endif
    s = strip_path(strip_file_m(strip_file_ext( (*pstate).file)))
    file = s + '.html'

    F = file_requester( /write, filter = '*.html', fix_filter=1, path=path, $
       title='Write a details HTML file (and TIFF files)', file = file, dialog_parent=event.top)
endif
if F ne '' then begin

;   This uses an old 'element_select' modal popup
;   For new parameters, make a new popup that uses the new 'select_element'
;   compount widget (see 'export_select' as a guide).

    if first then begin
       title = 'Select elements for TIFF export'
       select = element_select( event.top, els, old_select=*(*pstate).pselect4, $
          title=title, path=*(*pstate).path )
       qselect = where(select eq 1)
       if qselect[0] eq -1 then goto, done
       *(*pstate).pselect4 = select
    endif
    qselect = where(*(*pstate).pselect4 eq 1)
    if qselect[0] eq -1 then goto, done

    F = strip_file_ext(F) + '.html'
    path = extract_path(F)
	if file_test(path,/dir) eq 0 then begin
		safe_file_mkdir, path, error=error
		if error then begin
			warning,'image_Save_all_TIFF',['Illegal directory name:',path]
		endif
	endif
	
	if (*p).type eq 0 then begin
		mfile = (*p).matrix.file
		matrix = read_da( mfile, error=error)
		if error then begin
			k = 0
			dpath = ipath
			dext = extract_extension(mfile)
			mfile = dpath + strip_file_ext( strip_path( mfile)) + '.'+dext
			file = mfile
			repeat begin
				free_DA, matrix
				matrix = read_da( file, error=error)
				file = file_pop_dir(file)
				k = k+1
			endrep until (error eq 0) or (k eq 5)
		endif
		if error then begin
			free_DA, matrix
			mfile = file_requester( /read, /must_exist, filter = '*.'+dext, group=event.top, /skip_if_exists, /translate, updir=3, $
						title='Select original '+dext+' file used to generate images', file=strip_path(mfile), path=dpath, fix_filter=0)
			matrix = read_da( mfile, error=error)
			if error then begin
				goto, bad_io
			endif
		endif
		if (mode eq 2) and (matrix.thick lt 1.0e-6) then begin
			warning,'image_Save_all_TIFF',['No sample thickness found in old DA matrix file.', $
							'Invalid condition for this output mode.','','Regenerate DA matrix and re-build Images.']
			goto, done
		endif
	endif
          
    on_ioerror, bad_io
    openw,1, F
    printf,1,'<HTML>'
    printf,1,'<HEAD>'
    printf,1,'<TITLE>',F,'</TITLE>'
    printf,1,'</HEAD>'
    printf,1,'<BODY BGCOLOR="#FFFFFF">'
    printf,1,'<H3>Dynamic Analysis PIXE/SXRF Imaging - TIFF maps</H3>'
    printf,1,'<H4>',(*p).source,'</H4>'
    printf,1,'<font size="-1">These X-ray elemental images were collected in "'+(*p).DevObj->title()+'" format.'
    printf,1,'The event data were analyzed using the'
    printf,1,'<A href="http://www.nmp.csiro.au/dynamic.html">CSIRO Dynamic Analysis method</A>,'
    printf,1,'which enables quantitative, true-elemental images to be un-mixed from the generally complex'
    printf,1,'PIXE/SXRF energy spectrum. The images are stored as TIFF files in this directory.</font><p>'

    first = 1
    for i=0L,n_elements(qselect)-1 do begin
		el = els[qselect[i]]
		s = strtrim( el, 2)
		st = s
		if fnumeric(s) then s=str_tidy(float(s),places=4)
		s = fix_file_name(s, /all)
		s2 = 'TIFF image for ' + st
		s2v = 'TIFF variance image for ' + st

		F = strip_path(F)
		if xanes and (txm eq 1) then begin
			var=0
			run = strip_path(strip_file_ext( (*p).source))
			sen = strtrim( string( 1000.*float2(el)), 2)
			ld = locate('.',sen)
			sen = strmid(sen,0,ld+3)
			whole = strmid(sen,0,ld)
			nw = strlen(whole)
			if nw lt 5 then sen=strjoin(replicate('0',5-nw)) + sen
			gif_file2 = path + run + '_' + el_xanes + '_' + sen + '_eV.tif'
		endif else begin
			gif_file2 = strip_file_ext(F) + '-' + s + ext			; normal size
			gif_file2 = path + fix_file_name( gif_file2)
			vgif_file2 = strip_file_ext(F) + '-' + s + '-var' + ext
			vgif_file2 = path + fix_file_name( vgif_file2)			; variance name
       endelse
       
        if (*p).type eq 0 then begin
	       	q = where(matrix.el eq el, nq)
	       	if nq ge 1 then begin
	       		case mode of
	       			0: begin									; ppm per pixel
						y = 1. / charge_per_pixel
	       				end
	       			1: begin									; counts total for full array selected
						y = matrix.yield[q[0]]
						if (*p).array and ptr_good( (*p).pactive) and matrix.array.On then begin
							totGamma = total( matrix.array.rGamma[ *(*p).pactive, q[0]] )
						endif else totGamma=1.
						y = y * totGamma
	       				end
	       			2: begin									; ng/cm**2 (incl: mg-->ng and ppm-->wt.fraction)
						y = matrix.thick / charge_per_pixel
	       				end
	       		endcase
			endif else y=1.
		endif else y=1.
	   	data = y * (*(*p).image)[*,*,qselect[i]]
	   	write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
		if var then begin
		   	vdata = 0.25 * y * (*(*p).error)[*,*,qselect[i]]
		   	write_tiff, vgif_file2, vdata, /float, orientation=0, description=s2v ;	, compression=1
		endif
    endfor
    printf,1,'<P>'

	if (*p).has_rates then begin
		gif_file2 = strip_file_ext(F) + '-count_rate' + ext	
		gif_file2 = path + fix_file_name( gif_file2)
		s2 = 'TIFF count rate map (c/s)'

		data = *(*p).count_rate_map
		write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
	endif
	if (*p).has_dwell then begin
		gif_file2 = strip_file_ext(F) + '-dwell' + ext	
		gif_file2 = path + fix_file_name( gif_file2)
		s2 = 'TIFF dwell map (ms)'

		data = *(*p).dwell_map
		write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
	endif
	if (*p).has_dead then begin
		gif_file2 = strip_file_ext(F) + '-dead_time' + ext	
		gif_file2 = path + fix_file_name( gif_file2)
		s2 = 'TIFF dead time fraction map'

		data = *(*p).dead_fraction
		write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
	endif
	if (*p).has_pileup then begin
		gif_file2 = strip_file_ext(F) + '-pileup' + ext	
		gif_file2 = path + fix_file_name( gif_file2)
		s2 = 'TIFF pileup fraction map'

		data = *(*p).pileup_map
		write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
	endif
	if (*p).has_flux then begin
		if ptr_good( (*p).flux) then begin
			gif_file2 = strip_file_ext(F) + '-flux' + ext	
			gif_file2 = path + fix_file_name( gif_file2)
			s2 = 'TIFF final flux map'
	
			data = *(*p).flux
			write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
		endif
		if ptr_good( (*p).raw_flux) then begin
			gif_file2 = strip_file_ext(F) + '-raw_flux' + ext	
			gif_file2 = path + fix_file_name( gif_file2)
			s2 = 'TIFF raw flux map'
	
			data = *(*p).raw_flux
			write_tiff, gif_file2, data, /float, orientation=0, description=s2;	, compression=1
		endif
	endif

    if (*p).type eq 0 then printf,1,'<H4>Dynamic Matrix: ',(*p).matrix.file,'</H4>'
    printf,1,'<font size="-1">Sample: ', (*p).sample, ' '
    printf,1,'Grain: ', (*p).grain, '<BR>'
    printf,1,'Comment: ', (*p).comment, '<BR>'
    printf,1,'Charge: ', (*p).charge, '<BR>'
    printf,1,'Cal:  A= ', (*p).cal.poly[1],', B= ',(*p).cal.poly[0],', Units= ',(*p).cal.units, '<BR><P>'
    printf,1,'Scan:  X= ', (*p).scan.x,', Y= ',(*p).scan.y, ' mm<BR>'
    printf,1,'X Compress: ', (*p).xcompress,', Y Compress: ',(*p).ycompress, '<BR>'
    printf,1,'X Scaled: ', (*p).scaled_x,', Y Scaled: ',(*p).scaled_y, '<BR>'
    printf,1,'X size: ', (*p).xsize,', Y Size: ',(*p).ysize, '<BR><P>'
    if xanes eq 0 then begin
	    printf,1,'Processed: ', (*p).processed,', Bad: ',(*p).bad_xy, '<BR>'
	    printf,1,'Valid: ', (*p).valid,', Clipped: ',(*p).clipped, '<BR><P>'
    endif
	if (*p).type eq 0 then begin
	    printf,1,'DA Matrix: ', (*p).matrix.file, '<BR>'
	    printf,1,'DA Source: ', (*p).matrix.label, '<BR>'
	    printf,1,'DA Charge: ', (*p).matrix.charge, '<BR><P>'
	endif
    if (*p).xstep_on eq 1 then begin
       printf,1,'X Step Scan mode. XStep count= ', (*p).xstep, '<BR><P>'
    endif
    if (*p).xstep_on eq 2 then begin
       printf,1,'Y Step Scan mode. YStep count= ', (*p).xstep, '<BR><P>'
    endif
    printf,1,'</font>'

    printf,1,'<H4>TIFF maximum values</H4>'
    printf,1,'<font size="-1"><table>'
;   printf,1,'<colgroup span=6 width="130,130,130,130,130,130"></colgroup>'
    printf,1,'<tr>'
    j = 0
    for i=0L,n_elements(qselect)-1 do begin
		el = els[qselect[i]]
        if (*p).type eq 0 then begin
	       	q = where(matrix.el eq el, nq)
	       	if nq ge 1 then begin
	       		case mode of
	       			0: begin									; ppm per pixel
						y = 1. / charge_per_pixel
	       				end
	       			1: begin									; counts total for full array selected
						y = matrix.yield[q[0]]
						if (*p).array and ptr_good( (*p).pactive) and matrix.array.On then begin
							totGamma = total( matrix.array.rGamma[ *(*p).pactive, q[0]] )
						endif else totGamma=1.
						y = y * totGamma
	       				end
	       			2: begin									; ng/cm**2 (incl: mg-->ng and ppm-->wt.fraction)
						y = matrix.thick / charge_per_pixel
	       				end
	       		endcase
			endif else y=1.
		endif else y=1.
	   	data = y * (*(*p).image)[*,*,qselect[i]]
		top = max(data)
		sc = build_result( top, top*0.01, 0.0)
		su = units
		printf,1,'<td width=135>',el,' ',sc,' '+su+' </td>'
		j = j+1
		if j eq 6 then begin
			j = 0
			printf,1,'</tr><tr>'
		endif
    endfor
    printf,1,'</tr></table></font><p>'
	free_DA, matrix

    printf,1,'</BODY>
    printf,1,'</HTML>

bad_io:
    close,1
endif

done:
end

;-----------------------------------------------------------------

pro image_Select, Event

COMPILE_OPT STRICTARR
;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate

image_select, group_leader=event.top, TLB=tlb
register_notify, event.top, 'image-display', from=tlb
end

;-----------------------------------------------------------------

pro Image_update_geopixe, event

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

dir = file_expand_path('.')
path = '/update/' + geopixe_version(/major)

	case !version.os_family of
		'Windows': begin
			code = expand_path('<IDL_DIR>\' + 'bin\' + '<IDL_BIN_DIRNAME>') + '\'
			idl = '"' + code + 'idlrt.exe"'
			sav = geopixe_root + 'geopixe_update.sav'
			args = 'path='+path+' '+'dir='+dir
			child = idl + ' -rt="' + sav + '" -args ' + args

			print,'Spawn process: ',child
			spawn, child, /noshell, /nowait
			end
		'unix': begin
			code = expand_path('<IDL_DIR>') + '/bin/'
			idl = code + 'idl'
			sav = geopixe_root + 'geopixe_update.sav'
			args = 'path='+path+' '+'dir='+dir
			child = idl + ' -rt=' + sav + ' -args ' + args

			print,'Spawn process: ',child
			spawn, child + ' &'
			end
		else: begin
			warning,'Image_update_geopixe',['Spawn not supported on this platform.','Could not spawn child processes.']
			end
	endcase
	return
end

;-----------------------------------------------------------------

pro image_xanes, Event

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
       warning,'Image_xanes',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

path = *(*pstate).path
dpath = *(*pstate).dpath
test = (*pstate).test
debug = (*pstate).debug

geom = widget_info( event.top, /geometry)

xoffset = geom.xoffset + 50
yoffset = geom.yoffset + 50

gimage, group_leader=event.top, TLB=tlb, path=path, dpath=dpath, $
          xoffset=xoffset, yoffset=yoffset, test=test, debug=debug, /xanes

register_notify, event.top, $
		['path', $					; new path
		'images', $					; new images loaded somewhere
		'image-clone', $			; new images from Image clone
		'dpath' $					; new raw data path
		], from=tlb
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

pro OnButton_Image_Full, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then return

set_image_view, pstate, event.top, /full
end
;
;-----------------------------------------------------------------

pro OnButton_Image_Zoom_In, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_image_view, pstate, event.top, zoom=+1

done:
end
;
;-----------------------------------------------------------------

pro OnButton_Image_Zoom_Out, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_image_view, pstate, event.top, zoom=-1

done:
end
;
;-----------------------------------------------------------------

pro OnButton_Image_Analyze, Event

image_analyze, event
end
;
;-----------------------------------------------------------------
; Note: operation here depends on whether this is a clone or not.
; Caused by kill notify to Draw widget.
; No NOT destroy Device Objects, as they may be in use elsewhere.

pro OnDestroy_image, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return

	if (*pstate).pix gt 0 then wdelete, (*pstate).pix
	if (*pstate).pix2 gt 0 then wdelete, (*pstate).pix2
	(*pstate).pix = -1
	(*pstate).pix2 = -1

	free_image_state, pstate
	return
end

;-----------------------------------------------------------------

pro OnKill_image, Event

cancel_notify, event.top

widget_control, event.top, /destroy
;heap_gc, /verbose
end

;-----------------------------------------------------------------
; Slider Callback Procedure.
;
;   {WIDGET_SLIDER, ID:0L, TOP:0L, HANDLER:0L, VALUE:0,  DRAG:0}
;
;   VALUE returns the new value of the slider.
;   DRAG returns integer 1
;     if the slider event was generated as part of a drag operation,
;     or zero if the event was generated when the user had finished
;     positioning the slider.
;-----------------------------------------------------------------

pro OnMove_Image_Bottom, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
i = (*pstate).image
xanes = tag_present('xanes',*p)

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)

if ptr_valid( pimg ) eq 0 then goto, done

if xanes then begin
	for ii=0,(*p).zsize-1 do begin
		(*opt)[ii].bottom = float(event.value)		; keep all together for XANES
	endfor
endif else begin
	(*opt)[i].bottom = float(event.value) 
endelse
draw_images, pstate

notify, 'image-display', from=event.top

done:
end

;-----------------------------------------------------------------

pro OnMove_Image_Top, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
i = (*pstate).image
xanes = tag_present('xanes',*p)

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)

if ptr_valid( pimg ) eq 0 then goto, done

if xanes then begin
	for ii=0,(*p).zsize-1 do begin
		(*opt)[ii].top = float(event.value)		; keep all together for XANES
	endfor
endif else begin
	(*opt)[i].top = float(event.value)          ; 1.4* for absorb plot
endelse
draw_images, pstate

notify, 'image-display', from=event.top

done:
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

pro OnNotify_image, Event

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
       warning,'OnNotify_image',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, finish
    endif
endif
common image_region_window_1, region_window

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.tag of

    'path': begin
       if ptr_valid( event.pointer) eq 0 then goto, finish
       *(*pstate).path = (*event.pointer)
       end

    'dpath': begin
       if ptr_valid( event.pointer) eq 0 then goto, finish
       *(*pstate).dpath = (*event.pointer)
       end

    'snapshot': begin
       name = 'image.snap'
       if (*pstate).clone then name='image_clone.snap'
       on_ioerror, finish
       openw, lun, name, /xdr, /get_lun
       geom = widget_info( event.top, /geometry)
       on_ioerror, snap_done
       writeu, lun, geom.xoffset>0, geom.yoffset>0, geom.scr_xsize>200, geom.scr_ysize>100
snap_done:
       close_file, lun
       on_ioerror, null
       end

    'image-kill-regions-all-planes': begin		 ; returned from 'image_table'

	   Image_Process_Kill_All_Region, Event
       end

    'image-process': begin						 ; returned from 'image_process' "get"
		print,'Image: image-process ...........................................................'

		if ptr_valid( event.pointer) eq 0 then goto, finish

		ops = *event.pointer
		n_ops = n_elements(ops)
		widget_control, /hourglass

		image_do_operations, event, ops

		pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
		if ptr_valid( opt) then begin
			widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
			widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
			widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
			widget_control, (*pstate).interp_id, set_button = (*opt)[(*pstate).image].interp
		endif

		draw_images, pstate
		end

    'batch-operations-open': begin

;		If the Image Operations window is not open then batch processing will stall.
;		Test here and open it, if needed. Remember that the call expects 'state' back.

		print,'Image: batch-operations-open ...........................................................'
		if (*pstate).operations eq 0 then begin
			image_Image_Operations, Event
			return
		endif
		end

    'batch-rgb-open': begin
		print,'Image: batch-RGB-open ...........................................................'
		if (*pstate).RGB_open eq 0 then begin
			image_Image_RGB, Event
			return
		endif
		end

    'batch-save': begin
		print,'Image: batch-save ...........................................................'
		if (*event.pointer).mirrorX then begin
			Image_Process_Rotate, Event, 0, /flipx
			(*event.pointer).save = 1
		endif
		if (*event.pointer).correctX then begin
			Image_Process_Correct_Current_file, Event, 0, first=(*event.pointer).first
					(*event.pointer).save = 1
		endif
		if (*event.pointer).save then begin
			Image_Save, Event, /default, overwrite=(*event.pointer).overwrite
		endif
		if (*event.pointer).metadata then begin
			print_image_metadata, (*pstate).p, stats=0
		endif
		
		html = (*event.pointer).html
		if strlen(html) gt 0 then begin
			image_save_all_HTML, event, /PNG, file=html, first=(*event.pointer).first
		endif
		bw = (*event.pointer).bw
		if strlen(bw) gt 0 then begin
			image_save_all_HTML, event, /PNG, file=bw, first=(*event.pointer).first, /bw
		endif
		export = (*event.pointer).export
		if strlen(export) gt 0 then begin
			export_images_csv, pstate, file=export, first=(*event.pointer).first, event=event
		endif
		tiff = (*event.pointer).tiff
		if strlen(tiff) gt 0 then begin
			mode = (*event.pointer).tiff_type
			image_save_all_TIFF, event, file=tiff, first=(*event.pointer).first, mode=mode
		endif
       
      	notify, 'done-save', from=event.top
		return
		end

    'image-operations-closed': begin
       (*pstate).operations = 0
       end

    'image-rgb-closed': begin
       (*pstate).RGB_open = 0
       end

    'corr-analyze-spline': begin          ; returned from 'corr' show pixels inside spline

		if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
		if ptr_valid( event.pointer) eq 0 then goto, finish
		p = (*pstate).p
		if ptr_valid( p) eq 0 then goto, finish
		elx = (*event.pointer).elx
		ely = (*event.pointer).ely
		qc = (*event.pointer).qc
		pmark = (*event.pointer).pmark

;		In the case of Line-XANES (e.g. 2D XE map), check for energies as 'elx' and 'ely',
;		which indicates return from an "Energy Association" variant corr window.

		xanes_stack_test, p, xanes, n_el, el, el_xanes
		if (xanes eq 0) and fnumeric(elx) and fnumeric(ely) then begin
			mask = bytarr( (*p).xsize, (*p).ysize)
			mask[*qc,*] = 1
			q = where(mask eq 1)
			*qc = q
	   	endif
	
       clear_mark, pstate, /both
       (*pstate).corr_mode = 1
       (*pstate).analyze_mode = 0
       (*pstate).analyze_type[0] = 0
       widget_control, (*pstate).analyze_mode_id, set_combobox_select = 0
       widget_control, (*pstate).analyze_type_id, set_combobox_select = 0
       if ptr_valid( (*pstate).pcorr_mark) then ptr_free, (*pstate).pcorr_mark
       (*pstate).pcorr_mark = ptr_new( *pmark)

       (*pstate).elx = elx
       (*pstate).ely = ely
       (*pstate).qc = ptr_new( *qc)
       draw_images, pstate

       analyze_image, pstate
       notify, 'image-results', from=event.top
       notify, 'image-corr-q', (*pstate).qc, from=event.top
       end

    'corr-analyze-clear': begin       ; returned from 'corr' to clear pixels inside spline

       if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
       draw_images, pstate
       end

    'image-corr-q': begin       ; returned from 'clone' to pass on corr qc

       if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
       if ptr_valid( event.pointer) then (*pstate).qc = ptr_new( *event.pointer)
       draw_images, pstate
       end

    'image-corr-clear': begin   ; returned from 'image' to clear pixels inside spline

       if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
       draw_images, pstate
       end

	'image-match-centroids': begin				; "match" in XANES stack regions

		if ptr_valid( event.pointer) eq 0 then goto, finish
		if ptr_valid( (*pstate).pregions) eq 0 then goto, finish
		if (*pstate).xanes ne 1 then goto, finish
		pimg = (*pstate).p
		if tag_present( 'xanes', *pimg) eq 0 then goto, finish
		n = n_elements( *(*pstate).pregions)
		if n lt 1 then goto, finish
		i = (*event.pointer).sel.top

		if (i ge 0) and (i lt n) then begin
			p = (*(*pstate).pregions)[i]
			n_steps = (*p).n_el
			if n_steps lt 2 then goto, finish
			cent0 = (*(*p).centroid)[0]
			found = 0
			for j=1,n_steps-1 do begin
				cent = (*(*p).centroid)[j]
				dx = cent.x - cent0.x
				dy = cent.y - cent0.y
				label = (*(*p).el)[j]
				fl = float2(label)
				q = where( abs(*(*pimg).pz_coords - fl[0]) lt 0.00005, nq)
				if nq gt 0 then begin
					k = q[0]
					found = 1
					print,k,'  ',label,'  correct shift: dx,dy=',dx,dy
					(*(*pimg).image)[ *,*,k] = shift_image( (*(*pimg).image)[ *,*,k], -dx,-dy) 
					add_history, (*pimg).history, k, 'Corrected image shift using Region, dx,dy='+str_tidy(-dx)+', '+str_tidy(-dy)
				endif
			endfor
			if found eq 0 then warning,'image_eventcb',['No labels match energy.','Match aborted.','Have you updated from element regions?']
			draw_images, pstate
		endif
		end
		
    'image-region-clear': begin                   ; returned from 'image_table' before region-select

       clear_mark, pstate, /both
        end

    'image-clear-all-marks': begin                ; returned from 'maia_launch' on NewRun

		Clear_All_Image_Marks, Event
        end

	'image-region-select': begin               ; returned from 'image_table'

		if ptr_valid(event.pointer) eq 0 then goto, finish
		i = (*event.pointer).sel.top

		image_region_select, pstate, i, /kvs

		p = (*pstate).p
		if ptr_good(p) then begin
			xanes_stack_test, p, xanes, n_els, el, el_xanes
			if (xanes eq 0) then begin
				if (((*p).energy_proxy_axis eq 1) or ((*p).energy_proxy_axis eq 2)) then begin
					export_images_csv, pstate, event=event, /plot
				endif
			endif
		endif
		end

    'image-region-update': begin               ; returned from 'image_table'

		if ptr_valid( (*pstate).pregions) eq 0 then goto, finish
		n = n_elements( *(*pstate).pregions)
		if n lt 1 then goto, finish

		for i=0L,n-1 do begin
			p = (*(*pstate).pregions)[i]
			(*pstate).region_id = i
			el_shown = (*p).el_shown
			note = (*p).note
		
			p2 = translate_region( p, pstate, error=err)			; translate from old region coords in 'p'
			if err then goto, finish								; to the local image coords, as in (*pstate).p

			if (*p).mode eq 1 then begin
				(*pstate).corr_mode = (*p).mode
				(*pstate).analyze_mode = 0
				(*pstate).analyze_type[0] = 0
				if ptr_valid( (*pstate).pcorr_mark) then ptr_free, (*pstate).pcorr_mark
				(*pstate).pcorr_mark = ptr_good((*p).pmark[0]) ? ptr_new( *(*p).pmark[0]) : ptr_new()
				if ptr_valid((*pstate).qc) then ptr_free, (*pstate).qc
				(*pstate).qc = ptr_new( *(*p2).q)
				(*pstate).elx = (*p).elx
				(*pstate).ely = (*p).ely
		
			endif else if (*p).mode eq 0 then begin
				(*pstate).corr_mode = (*p).mode
				(*pstate).analyze_mode = 0
				(*pstate).analyze_type[0] = max( [0,min( [(*pstate).max_set,(*p).analyze_type[0]]) ])
				if ptr_valid( (*(*pstate).pmark[0])[(*pstate).analyze_type[0]] ) then begin
					ptr_free, (*(*pstate).pmark[0])[(*pstate).analyze_type[0]]
				endif
				(*(*pstate).pmark[0])[(*pstate).analyze_type[0]] = ptr_new( *(*p2).pmark[0])
				if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
				(*pstate).q = ptr_new( *(*p2).q)
		
				if (*p).analyze_type[1] gt 0 then begin
					(*pstate).analyze_type[1] = max( [0,min( [(*pstate).max_set,(*p).analyze_type[1]]) ])
					if ptr_valid((*(*pstate).pmark[1])[(*pstate).analyze_type[1]]) then begin
						ptr_free, (*(*pstate).pmark[1])[(*pstate).analyze_type[1]]
					endif
					(*(*pstate).pmark[1])[(*pstate).analyze_type[1]] = ptr_new( *(*p2).pmark[1])
					(*pstate).analyze_mode = 1
				endif
			endif
		
			clear_mark, pstate, /both
			analyze_image, pstate, i
		
			p = (*(*pstate).pregions)[i]							; because a new region pointer
			(*p).el_shown = el_shown								; is created in 'analyze_image'
			(*p).note = note
		endfor

       widget_control, (*pstate).analyze_mode_id, set_combobox_select = (*pstate).analyze_mode
       widget_control, (*pstate).analyze_type_id, set_combobox_select = (*pstate).analyze_type[(*pstate).analyze_mode]

       notify, 'image-results', from=event.top
       end

    'image-region-update-one': begin             ; update one request returned from 'image_table'

       if ptr_valid( (*pstate).pregions) eq 0 then goto, finish
       n = n_elements( *(*pstate).pregions)
       if n lt 1 then goto, finish
       if ptr_valid( event.pointer) eq 0 then goto, finish
       i = (*event.pointer).sel.top
       if (i lt 0) or (i ge n) then goto, finish
       if ptr_valid( (*(*pstate).pregions)[i]) eq 0 then goto, finish

		p = (*(*pstate).pregions)[i]
		(*pstate).region_id = i
		el_shown = (*p).el_shown
		note = (*p).note
	
		p2 = translate_region( p, pstate, error=err)			; translate from old region coords in 'p'
		if err then goto, finish								; to the local image coords, as in (*pstate).p

		if (*p).mode eq 1 then begin
			(*pstate).corr_mode = (*p).mode
			(*pstate).analyze_mode = 0
			(*pstate).analyze_type[0] = 0
			if ptr_valid( (*pstate).pcorr_mark) then ptr_free, (*pstate).pcorr_mark
			if ptr_good((*p).pmark[0]) then (*pstate).pcorr_mark = ptr_new( *(*p).pmark[0])
			if ptr_valid((*pstate).qc) then ptr_free, (*pstate).qc
			(*pstate).qc = ptr_new( *(*p2).q)
			(*pstate).elx = (*p).elx
			(*pstate).ely = (*p).ely
	
		endif else if (*p).mode eq 0 then begin
			(*pstate).corr_mode = (*p).mode
			(*pstate).analyze_mode = 0
			(*pstate).analyze_type[0] = max( [0,min( [(*pstate).max_set,(*p).analyze_type[0]]) ])
			if ptr_valid( (*(*pstate).pmark[0])[(*pstate).analyze_type[0]] ) then begin
				ptr_free, (*(*pstate).pmark[0])[(*pstate).analyze_type[0]]
			endif
			(*(*pstate).pmark[0])[(*pstate).analyze_type[0]] = ptr_new( *(*p2).pmark[0])
			if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
			(*pstate).q = ptr_new( *(*p2).q)
	
			if (*p).analyze_type[1] gt 0 then begin
				(*pstate).analyze_type[1] = max( [0,min( [(*pstate).max_set,(*p).analyze_type[1]]) ])
				if ptr_valid((*(*pstate).pmark[1])[(*pstate).analyze_type[1]]) then begin
					ptr_free, (*(*pstate).pmark[1])[(*pstate).analyze_type[1]]
				endif
				(*(*pstate).pmark[1])[(*pstate).analyze_type[1]] = ptr_new( *(*p2).pmark[1])
				(*pstate).analyze_mode = 1
			endif
		endif
	
		clear_mark, pstate, /both
		analyze_image, pstate, i
	
		p = (*(*pstate).pregions)[i]
		(*p).el_shown = el_shown
		(*p).note = note

       notify, 'image-results', from=event.top
       end

    'image-analyze-type': begin                   ; returned from 'image' clone

       if ptr_valid(event.pointer) then begin
         clear_mark, pstate, /both
         (*pstate).analyze_type[(*pstate).analyze_mode] = max( [0,min( [(*pstate).max_set,*event.pointer]) ])
         wset, (*pstate).wid2
         plot_mark, pstate

         widget_control, (*pstate).analyze_type_id, set_combobox_select = (*pstate).analyze_type[(*pstate).analyze_mode]
       endif
       end

    'image-analyze-mode': begin                   ; returned from 'image' clone

       if ptr_valid(event.pointer) then begin
         clear_mark, pstate, /both
         (*pstate).analyze_mode = max( [0,min( [1,*event.pointer]) ])
         wset, (*pstate).wid2
         plot_mark, pstate

         widget_control, (*pstate).analyze_mode_id, set_combobox_select = (*pstate).analyze_mode
         widget_control, (*pstate).analyze_type_id, set_combobox_select = (*pstate).analyze_type[(*pstate).analyze_mode]
       endif
       end

    'image-analyze-clear': begin                ; returned from 'image' clone

		print, 'image-analyze-clear'
		clear_mark, pstate
		end

    'image-analyze-all-clear': begin               ; returned from 'image' clone

		print, 'image-analyze-all-clear'
		wset, (*pstate).wid2
		device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).pix]
		end

    'image-analyze-mark': begin                    ; returned from 'image' clone

		print, 'image-analyze-mark'
		wset, (*pstate).wid2
		plot_mark, pstate
		end

    'image-analyze-q': begin                    ; returned from 'image' clone

       if (*pstate).analyze_mode eq 1 then clear_mark, pstate, /both
       if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
       if ptr_valid( event.pointer) then begin
       pq = event.pointer
       ptr_free, (*pstate).q
       if ptr_valid((*pq).q) then (*pstate).q = ptr_new( *(*pq).q)
         if (*pstate).analyze_mode eq 1 then begin
          wset, (*pstate).wid2
          plot_mark, pstate
         endif
       endif
       end

	'image-display': begin                ; returned from 'image_select'

		p = (*pstate).p
		if ptr_good( p) eq 0 then break
		xanes_stack_test, p, xanes, n_el, el, el_xanes

		pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
		(*pstate).image = (*pstate).image < (n_el-1)
		if ptr_valid( opt) then begin
			widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
			widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
			widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
			widget_control, (*pstate).interp_id, set_button = (*opt)[(*pstate).image].interp
		endif
		if ptr_valid( pimg) and (*pstate).realtime then begin
			if str_equal( *(*pstate).old_el, el) eq 0 then begin
				widget_control, (*pstate).element_id, set_value = el, SET_COMBOBOX_SELECT =(*pstate).image
				*(*pstate).old_el = el
			endif
		endif

		draw_images, pstate
		end

	'image-elements': begin
	
		p = (*pstate).p
		if ptr_good( p) eq 0 then break
		xanes_stack_test, p, xanes, n_el, el, el_xanes

		widget_control, (*pstate).element_id, set_value=el
		end
		
    'image-clone': begin                       ; returned from 'image' clone

;     Here it is assumed that another cloned 'image' has read in new data to be
;     shared with this image program. It is assumed that the other has disposed
;     of the image array already.

       p2 = event.pointer                        ; pointer to snapshot of other 'state'
       if ptr_valid(p2) eq 0 then goto, finish
       p = (*p2).p
       if ptr_valid(p) eq 0 then goto, finish
       
       if (*pstate).xanes ne (*p2).xanes then goto, finish

       if ptr_valid((*pstate).p) then begin
         if (*(*pstate).p).orphan eq 1 then begin
          (*pstate).local = 1
          (*(*pstate).p).orphan = 0
         endif
         if ((*pstate).p ne p) and ((*pstate).local eq 1) then free_images, (*pstate).p
       endif

;     Don't set 'local' here, unless this is a master (ie. clone=0),
;     and the notify image has an 'orphan=1' request set.
;     If it is not, then someone else will take control of freeing the image.
;     If we take over the image (local=1), then we set 'orphan=0' so that
;     anyone else getting this notify will know it's owned here.

       (*pstate).p = p
       if ((*pstate).clone eq 0) and ((*p).orphan eq 1) then begin
         (*pstate).local = 1
         (*p).orphan = 0
       endif else begin
         (*pstate).local = 0
       endelse
       (*pstate).file = (*p2).file
       (*pstate).width = (*p2).width
       (*pstate).height = (*p2).height
       (*pstate).zoom = (*p2).zoom
       (*pstate).display_mode = (*p2).display_mode
       if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc

       set_image_view, pstate, event.top, /clone
       pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
       if ptr_valid( opt) then begin
         widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
         widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
         widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
         widget_control, (*pstate).interp_id, set_button = (*opt)[(*pstate).image].interp
       endif

       notify, 'images-changed', (*pstate).p, from=event.top
       *(*pstate).pselect = 1
       end

    'images': begin                   						          ; returned from other apps

;     Unlike the 'image-clone' notify, where the other image.pro will continue to own
;     the image, here this image.pro must take-over ownership of the image, in case
;     the other app exits, for example.
;
;     But, assuming we have clone cousins, only the 'master' image.pro should take
;     ownership.

		print,'Image: new images ...'
       p = event.pointer                  							   ; pointer to other 'image'
       if ptr_valid(p) eq 0 then goto, finish
	   xanes_stack_test, p, xanes, n_el, el, el_xanes
	   if (*pstate).xanes ne xanes then goto, finish
	   
       new_select = replicate( 1,n_el)
       if ptr_valid((*pstate).p) then begin               	      	 ; match up old element selections
			pold = (*pstate).p
			xanes_stack_test, pold, xanes_old, n_el_old, el_old, el_xanes_old

         if n_elements(*(*pstate).pselect) ge n_el then begin 		; with new element list
	          old_select = *(*pstate).pselect
	
	          old_els = strtrim( el_old,2)
	          new_els = strtrim( el,2)
	          q = where( old_els eq 'back')	
	          if q[0] ne -1 then old_els[q] = 'Back'
	          q = where( new_els eq 'back')
	          if q[0] ne -1 then new_els[q] = 'Back'
	
	          for i=0L,n_el-1 do begin
	              q = where( old_els eq new_els[i])
	              if q[0] ne -1 then begin
	                 new_select[i] = old_select[q[0]]
	              endif
	          endfor
         endif

         if (*(*pstate).p).orphan eq 1 then begin
          (*pstate).local = 1
          (*(*pstate).p).orphan = 0
         endif
         if (pold ne p) and ((*pstate).local eq 1) then free_images, pold
       endif
;
;     Don't set 'local' here, unless this is a master (ie. clone=0),
;     and the notify image has an 'orphan=1' request set.
;     If it is not, then someone else will take control of freeing the image.
;     If we take over the image (local=1), then we set 'orphan=0' so than
;     anyone esle getting this notify will know it's owned here.

       (*pstate).p = p
       if ((*pstate).clone eq 0) and ((*p).orphan eq 1) then begin
         (*pstate).local = 1
         (*p).orphan = 0
       endif else begin
         (*pstate).local = 0
       endelse
       (*pstate).file = (*p).file
       if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc

       set_image_view, pstate, event.top

       notify, 'images-changed', (*pstate).p, from=event.top
       *(*pstate).pselect = new_select
       end

	'wizard-action': begin
		if ptr_valid( event.pointer) then begin
			if (*event.pointer).window eq 'Image' then begin
				case (*event.pointer).command of
					'open-test': begin
;						print,'*** Wizard Image: test if window is open ...'
						pw = (*pstate).pwiz
						*pw = *event.pointer
						(*pw).top = event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'load-image': begin
						pw = event.pointer
						file = *(*pw).pdata
						print,'*** Wizard Image: load image= '+file
						Image_Load2, pstate, file, error=err
						notify, 'images-changed', (*pstate).p, from=event.top
						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'sum-region': begin
						pw = event.pointer
						file = *(*pw).pdata
						print,'*** Wizard Image: sum region'
						pd = (*pw).pdata
						
						wset, (*pstate).wid2
						clear_mark, pstate
						(*pstate).analyze_mode = (*pd).mode					; “+”/"-" mode
						(*pstate).analyze_type[(*pd).mode] = (*pd).shape	; “Box” mode
						pm = (*pstate).pmark[(*pstate).analyze_mode]
						p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
						n = n_elements( (*pd).x)
						xmin = (*(*pstate).p).bounds.xmin
						xmax = (*(*pstate).p).bounds.xmax
						ymin = (*(*pstate).p).bounds.ymin
						ymax = (*(*pstate).p).bounds.ymax
						(*p).x[0:n-1] = clip((*pd).x[0:n-1],xmin,xmax)		; X handles
						(*p).y[0:n-1] = clip((*pd).y[0:n-1],ymin,ymax)		; Y handles
						(*p).Theta = (*pd).theta							; rotation
						get_stats = (*pd).get_stats
						plot_mark, pstate
						
						if ptr_valid( (*pstate).px) then ptr_free, (*pstate).px
						if ptr_valid( (*pstate).py) then ptr_free, (*pstate).py
						(*pstate).px = ptr_new( (*p).x)
						(*pstate).py = ptr_new( (*p).y)
						(*pstate).theta = (*p).theta

						Image_Analyze, event, get_stats=get_stats, uniform_element=(*pd).uniform_element, error=err						
						if err eq 0 then begin
							nr = n_elements(*(*pstate).pregions)
							pr = (*(*pstate).pregions)[nr-1]
							if ptr_good(pr) eq 0 then begin
								err = 1
							endif else begin
								if ptr_valid( (*pd).presults) then begin
									*(*pd).presults = *pr
								endif else begin
									(*pd).presults = ptr_new( *pr)
								endelse
							endelse
							(*pd).local = 0
						endif
						(*pw).error = err
						notify, 'wizard-return', pw
						end

					'set-display': begin
						pw = event.pointer
						pd = (*pw).pdata
						print,'*** Wizard Image: set display parameters ...'
						nd = n_elements(*pd)

						pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
						p = (*pstate).p
						for i=0, nd-1 do begin
							j = (where((*pd)[i].el eq *(*p).el))[0]
							if j ne -1 then begin
								(*opt)[j].bottom = (*pd)[i].bottom
								(*opt)[j].top = (*pd)[i].top
								(*opt)[j].log = (*pd)[i].log
							endif
						endfor
						widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
						widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
						widget_control, (*pstate).Zscale_mode_id, set_combobox_select = clip((*opt)[(*pstate).image].log,0,2)
						draw_images, pstate
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'image-corrections': begin
						pw = event.pointer
						pd = (*pw).pdata
						print,'*** Wizard Image: do image operations ...'
						image_do_operations, event, (*pd).ops
						image_Save, event, /default, overwrite=(*pd).overwrite
		
						draw_images, pstate
						notify, 'images-changed', (*pstate).p, from=event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

				    'save-batch': begin
						pw = event.pointer
						pd = (*pw).pdata
						print,'*** Wizard Image: do image saves and exports ...'
						if (*pd).mirrorX then begin
							Image_Process_Rotate, Event, 0, /flipx
							(*pd).save = 1
						endif
						if (*pd).correctX then begin
							Image_Process_Correct_Current_file, Event, 0, first=(*pd).first
							(*pd).save = 1
						endif
						if (*pd).save then begin
							Image_Save, Event, /default, overwrite=(*pd).overwrite
						endif
						if (*pd).metadata then begin
							print_image_metadata, (*pstate).p, stats=0
						endif
						
						html = (*pd).html
						if strlen(html) gt 0 then begin
							image_save_all_HTML, event, /PNG, file=html, first=(*pd).first
						endif
						bw = (*pd).bw
						if strlen(bw) gt 0 then begin
							image_save_all_HTML, event, /PNG, file=bw, first=(*pd).first, /bw
						endif
						export = (*pd).export
						if strlen(export) gt 0 then begin
							export_images_csv, pstate, file=export, first=(*pd).first, event=event
						endif
						tiff = (*pd).tiff
						if strlen(tiff) gt 0 then begin
							mode = (*pd).tiff_type
							image_save_all_TIFF, event, file=tiff, first=(*pd).first, mode=mode
						endif

						draw_images, pstate
						if (*pd).save then begin
							notify, 'images-changed', (*pstate).p, from=event.top
						endif
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					else: begin
						warning,'image: Notify',['Unknown wizard command: '+(*event.pointer).command, $
								'Make sure GeoPIXE version is compatible with Wizard.']
					endelse
				endcase
			endif
		endif
		end

    else: begin
;       print,'OnNotify_image: unknown tag = ',event.tag
       end
endcase

finish:
	close_file, lun
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die
    return

die:
    widget_control, event.top, /destroy
    return
    end

;-----------------------------------------------------------------

pro OnRealize_Element, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).element_id = wWidget
p = (*pstate).p
if ptr_valid( p) then begin
	xanes_stack_test, p, xanes, n_el, els, el_xanes

    widget_control, wWidget, set_value=els
    *(*pstate).old_el = els
endif
widget_control, wWidget, set_combobox_select = (*pstate).image
end

;-----------------------------------------------------------------

pro OnRealize_Analyze_Type, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).analyze_type_id = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).analyze_type[(*pstate).analyze_mode]
end

;-----------------------------------------------------------------

pro OnRealize_Analyze_Mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).analyze_mode_id = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).analyze_mode
end

;-----------------------------------------------------------------

pro OnRealize_Image_Bottom_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).bottom_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Image_Top_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).top_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Image_Interp, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).interp_id = wWidget
widget_control, wWidget, set_button = (*pstate).interp
end

pro OnRealize_Image_Help1, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help1 = wWidget
(*pstate).help = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_Image_Help2, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help2 = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_Image, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).dw =   16
(*pstate).dh =   0

widget_control, wWidget, get_value=wid2
wset,wid2
(*pstate).wid2 = wid2
(*pstate).draw2 = wWidget

set_image_view, pstate, top, /realize
return
end

;-----------------------------------------------------------------

pro OnRealize_Image_Mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).mode_id = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).display_mode
end

;-----------------------------------------------------------------

pro OnRealize_Image_Zscale_Mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Zscale_mode_id = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).Zscale_mode
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

pro OnSelect_Image_Element, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).image = event.index

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
i = (*pstate).image
;print,'Element, i, source=', i, '  ',(*p).source

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)

if ptr_valid( pimg ) eq 0 then goto, done
if i ge n_elements( (*pimg)[0,0,*]) then return

widget_control, (*pstate).bottom_slider, set_value = fix((*opt)[i].bottom)
widget_control, (*pstate).top_slider, set_value = fix((*opt)[i].top)
widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[i].log >0)<2)
widget_control, (*pstate).interp_id, set_button = (*opt)[i].interp

draw_images, pstate

s = legend_string(pstate, (*p).type, pimg, opt, i, sx, sy)
widget_control, (*pstate).help, set_value=s

done:
    if ptr_valid( (*pstate).pelement) then ptr_free, (*pstate).pelement
    (*pstate).pelement = ptr_new( (*pstate).image)
    notify, 'image-show-element', (*pstate).pelement, from=event.top
end

;-----------------------------------------------------------------

pro OnSelect_Image_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).display_mode = event.index
check_mode, pstate

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)

if ptr_valid( pimg ) eq 0 then goto, done

i = (*pstate).image
widget_control, (*pstate).bottom_slider, set_value = fix((*opt)[i].bottom)
widget_control, (*pstate).top_slider, set_value = fix((*opt)[i].top)
widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[i].log >0)<2)
widget_control, (*pstate).interp_id, set_button = (*opt)[i].interp

draw_images, pstate

s = legend_string(pstate, (*p).type, pimg, opt, i, sx, sy)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_Image_Analyze_Type, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (event.index eq 0) and ((*pstate).analyze_mode eq 1) then begin
    clear_mark, pstate, /both
    (*pstate).analyze_mode = 0
    (*pstate).analyze_type[0] = event.index

    if ptr_valid( (*pstate).pmode) then ptr_free, (*pstate).pmode
    (*pstate).pmode = ptr_new( (*pstate).analyze_mode)
    notify, 'image-analyze-mode', (*pstate).pmode, from=event.top

    widget_control, (*pstate).analyze_mode_id, set_combobox_select = 0
endif else begin
    clear_mark, pstate
    (*pstate).analyze_type[(*pstate).analyze_mode] = event.index
endelse
wset, (*pstate).wid2
plot_mark, pstate

if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
(*pstate).ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
notify, 'image-analyze-type', (*pstate).ptype, from=event.top
end

;-----------------------------------------------------------------

pro OnSelect_Image_Analyze_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;if ((*pstate).analyze_type[(*pstate).analyze_mode] eq 0) and (event.index eq 1) then begin
;    widget_control, (*pstate).analyze_mode_id, set_combobox_select = 0
;endif

clear_mark, pstate, /both
(*pstate).analyze_mode = event.index
wset, (*pstate).wid2
plot_mark, pstate

if ptr_valid( (*pstate).pmode) then ptr_free, (*pstate).pmode
(*pstate).pmode = ptr_new( (*pstate).analyze_mode)
notify, 'image-analyze-mode', (*pstate).pmode, from=event.top

widget_control, (*pstate).analyze_type_id, set_combobox_select = (*pstate).analyze_type[(*pstate).analyze_mode]

if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
(*pstate).ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
notify, 'image-analyze-type', (*pstate).ptype, from=event.top
end

;-----------------------------------------------------------------

pro OnSelect_Image_Interp, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).interp = event.select

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
i = (*pstate).image

xanes_stack_test, p, xanes, n_el, el, el_xanes

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

if xanes then begin
	for i=0,n_el-1 do begin
		(*opt)[i].interp = (*pstate).interp
	endfor
endif else begin
;	(*opt)[i].interp = (*pstate).interp

	for i=0,n_el-1 do begin
		(*opt)[i].interp = (*pstate).interp
	endfor
endelse

draw_images, pstate
notify, 'image-display', from=event.top

done:
end

;-----------------------------------------------------------------

pro OnSelect_Image_Zscale_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Zscale_mode = event.index

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done
i = (*pstate).image

pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
if ptr_valid( pimg ) eq 0 then goto, done

(*opt)[i].log = (*pstate).Zscale_mode

draw_images, pstate
notify, 'image-display', from=event.top

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

pro OnSize_image, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case !version.os_family of
    'MacOS': begin
       draw_trim = 15
       scr_trim = 21
       scr_xtrim = 0
       scr_ytrim = 0
       end
    'unix': begin
       draw_trim = 0
       scr_trim = 0			; 15
       scr_xtrim = 32
       scr_ytrim = 3
       end
    else: begin
       draw_trim = 0
       scr_trim = 15		; 15
       scr_xtrim = 0
       scr_ytrim = 0
       end
endcase
;print, 'Resize ... '
help,event
widget_control, (*pstate).draw2	, get_draw_view=vv
;print,'	Read back draw view =', vv

if (event.x eq (*pstate).size_event_x) and (event.y eq (*pstate).size_event_y) then return
(*pstate).size_event_x = event.x
(*pstate).size_event_y = event.y

; The offsets 'scr_xsize_off', 'scr_ysize_off' get manipulated in 'map_help' as the window size
; changes and we may switch between help1 and help2 text widgets for help.
; Determine new scr size (w,h) for draw widget, save old in 'ow,oh'.

w1 = (event.x - (*pstate).scr_xsize_off)
h1 = (event.y - (*pstate).scr_ysize_off)
w = (w1 > (376 + scr_trim)) 
h = (h1 > (64 + scr_trim)) 
ow = (*pstate).w
oh = (*pstate).h
(*pstate).w = w
(*pstate).h = h

; Check to see if we need to automatically zoom image in/out ...

if (float(w1) / ow ge 2) and (float(h1) / oh ge 2) then begin
	zx = round( alog( float(w1) / float(ow)) / alog(2.))
	zy = round( alog( float(h1) / float(oh)) / alog(2.))
	set_image_view, pstate, event.top, zoom = max([zx,zy]) > 1
endif

; May need to switch between help1 and help2 if window size changes ...
; The offsets 'scr_xsize_off', 'scr_ysize_off' get manipulated in 'map_help'.

map_help, pstate

w = ((event.x - (*pstate).scr_xsize_off) > (356 + scr_trim)) < ((*pstate).width + scr_xtrim)
h = ((event.y - (*pstate).scr_ysize_off) > (64 + scr_trim)) < ((*pstate).height + scr_ytrim)

; Note that setting "draw_xsize=(*pstate).width, draw_ysize=(*pstate).height" is redundant,
; but necessary to keep the scrolling window working ...

widget_control, (*pstate).draw2	, set_draw_view=[0,0]
widget_control, (*pstate).draw2, scr_xsize=w, scr_ysize=h, $
         draw_xsize=(*pstate).width+draw_trim, draw_ysize=(*pstate).height+draw_trim

; Bug in Linux makes scroll bars position wrong. Need to write these back to fix ...
widget_control, (*pstate).draw2	, set_draw_view=vv > 0

(*pstate).width = (*pstate).width
(*pstate).height = (*pstate).height
(*pstate).w = w
(*pstate).h = h
(*pstate).xview = w - (*pstate).dw
(*pstate).yview = h - (*pstate).dh
end

;-----------------------------------------------------------------

; This is used to pop-up windows after start-up

pro OnTimer_image, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good(pstate,/struct) eq 0 then goto, done
if ptr_good((*pstate).pprefs,/struct) eq 0 then goto, done

	startup = (*(*pstate).pprefs).startup
	test = (*pstate).test
	debug = (*pstate).debug
	if (*(*pstate).pprefs).slave then goto, done
	
	if startup.spectrum then image_spectrum_display, event, $
	       start_identify=startup.identify, start_PIXE=startup.fit
	if startup.clone then image_clone, event
	if startup.sort then image_EVT, event
	if startup.regions then image_Results_Table, event

done:
	return
end


;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_image, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if event.enter eq 1 then begin
	widget_control, event.id, get_uvalue=message
	if n_elements(message) lt 1 then message=''
	if size(message,/tname) ne 'STRING' then message=''
    widget_control, (*pstate).help, set_value=message
endif else begin
    p = (*pstate).p
    if ptr_valid( p) eq 0 then goto, done
    i = (*pstate).image

    pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)

    if ptr_valid( pimg ) eq 0 then goto, done

    s = legend_string(pstate, (*p).type, pimg, opt, i, sx, sy)
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

pro OnViewport_image, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).xlow = event.x
(*pstate).ylow = event.y
;help,event
print,'New View:', (*pstate).xlow, (*pstate).ylow

pixel_to_xy, pstate, (*pstate).xlow, (*pstate).ylow, xorg,yorg
pixel_to_xy, pstate, (*pstate).xlow+(*pstate).xview, (*pstate).ylow+(*pstate).yview, xtop,ytop

s1 = '   View bottom-left  X =' + str_tidy(event.x) + '  Y =' + str_tidy(event.y)
s2 = 'Image View origin  X =' + str_tidy(min([xorg,xtop])) + '  Y =' + str_tidy(min([yorg,ytop]))
s3 = '   Image View top  X =' + str_tidy(max([xorg,xtop])) + '  Y =' + str_tidy(max([yorg,ytop]))
widget_control, (*pstate).help, set_value=[s1,s2,s3]

end

;-----------------------------------------------------------------

pro PostCreate_Image_Base, wWidget, Help1_Base, Help2_Base, $
          parent=parent, clone=clone, path=path, _EXTRA=_VWBExtra_, $
          plugins=plugins, wizards=wizards, test=test, debug=debug, prefs=prefs, $
          dpath=dpath, pimage=pimage, realtime=realtime, ppercent=ppercent, $
          version=version, xanes=xanes, kvs_obj1=kvs, kvs_prefix1=kvs_prefix, $
		  kvs_obj2=kvs2, kvs_prefix2=kvs_prefix2, shape_file=shape_file

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
       warning,'Postcreate_image_base',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

if n_elements(parent) lt 1 then parent=0L
if n_elements(clone) lt 1 then clone=0
if n_elements(test) lt 1 then test=0
if n_elements(debug) lt 1 then debug=0
if n_elements(version) lt 1 then version='?'
if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=path
if n_elements(pimage) lt 1 then pimage=ptr_new()
if n_elements(xanes) lt 1 then xanes=0

w = 360
h = 360
vx = 360
vy = 360
tlb = tlb_id( wWidget)

if n_elements(prefs) lt 1 then prefs = geopixe_defaults(source='PostCreate_Image_Base')
root = ptr_new(/allocate_heap)
path = build_output_path( dpath, path, root, /set)

max_set = 12                			    ; max marker types

state = {	p:			pimage, $			; pointer to image pointer array
			local:		0, $				; flags local responsibiltiy for image data
			xanes:		xanes, $			; 0 element images, 1 XANES stack

			kvs:		kvs, $				; optional KVS object (for Box in 'OnButton_Image')
			kvs_prefix:	kvs_prefix, $		; KVS key prefix
			kvs2:		kvs2, $				; optional KVS 2 object (for Box in 'OnButton_Image')
			kvs_prefix2: kvs_prefix2, $		; KVS 2 key prefix
			shape_file:	shape_file, $		; optional file to save current Box shape to

			file:		'', $				; current image filename
			path:		ptr_new(path), $	; current path
			dpath:		ptr_new(dpath), $	; current raw data path
			root:		root, $				; path root
			get_file:	ptr_new(/allocate_heap), $ ; current get-file for operations
			clone:		clone, $			; flags that this is a clone
			test:		test, $				; test mode
			debug:		debug, $			; debug mode
			version:	version, $			; GeoPIXE version string
			operations:	0, $				; image operations open
			RGB_open:	0, $				; image RGB window open
			realtime:	realtime, $			; realtime Maia mode
			ppercent:	ppercent, $			; ptr to % time (redraw)
			last_time:	systime(/seconds), $ ; last time draw
		
			b:			ptr_new(), $		; pointer to screen image byte array
			q:			ptr_new(), $		; pointer to area selection 'q' array
			qc:			ptr_new(), $		; pointer to corr area selection 'q' array
			corr_on:	1, $				; enables the corr highlight overlay
			elx:		'', $				; corr X axis element
			ely:		'', $				; corr Y axis element
			pcorr_mark:	ptr_new(), $		; corr spline coordinates

			pline:		ptr_new(), $		; pointer to line projection results struct
			llocal:		0, $				; local responsibility for line spectrum

			region_id:	0, $				; id of Image_Table shape (used for update)

			ptype:		ptr_new(), $		; pointer used for notify 'image-analyze-type'
			pmode:		ptr_new(), $		; pointer used for notify 'image-analyze-mode'
			pq:			ptr_new(/allocate_heap), $ ; pointer to q parameters for notify 'image-analyze-q'
			pstate:		ptr_new(), $		; pointer used for notify 'image'
			pelement:	ptr_new(), $		; pointer used for notify 'image-show-element'

			plugins:	plugins, $			; pointer to list of user plugins
			plugin_menus: 0L, $				; plugin menus base menu widget ID
			plugin_menus_root: 0L, $		; parent widget for plugin menus base ID
			wizards:	wizards, $			; pointer to list of user wizards
			wizard_menus: 0L, $				; wizard menus base menu widget ID
			wizard_menus_root: 0L, $		; parent widget for wizard menus base ID
			pwiz:		ptr_new(/alloc), $	; pointer for Wizard return

			pyield:		ptr_new(), $		; pointer to mineral yields for correction (are these used now?)
			plast:		ptr_new(), $		; pointer to last iteration yields per pixel

			pregions:	ptr_new(/allocate_heap), $	; pointer to regions parameters
			pevt:		ptr_new(/allocate_heap), $	; pointer to EVT parameters
			pcorrect:	ptr_new(/allocate_heap), $	; pointer to Correct_Yield parameters
			pfilter:	ptr_new(/allocate_heap), $	; pointer to Filter_Setup parameters
			pdetector:	ptr_new(/allocate_heap), $	; pointer to Detector_Setup parameters
			pselect:	ptr_new(/allocate_heap), $	; pointer to element_select list
			pselect2:	ptr_new(/allocate_heap), $	; pointer to element_select list for export
			pselect3:	ptr_new(/allocate_heap), $	; pointer to element_select list for html
			pselect4:	ptr_new(/allocate_heap), $	; pointer to element_select list for tiff
			pexport:	ptr_new(/allocate_heap), $	; pointer to plot_image_select options
			pprefs:		ptr_new(prefs), $	; pointer to current prefs
			pfile:		ptr_new(''), $		; pointer to a file name

			wid2:		0L, $				; draw 2 window id
			draw2:		0L, $				; draw 2 widget ID
			pix:		0L, $				; pixmap window id
			pix2:		0L, $				; pixmap 2 window id

			tlb:		tlb, $				; top level base
			element_id:	0L, $				; element droplist ID
			analyze_type_id: 0L, $			; analyze type droplist ID
			analyze_mode_id: 0L, $			; analyze mode droplist ID
			help1:		0L, $				; help 1 text widget ID
			help2:		0L, $				; help 2 text widget ID
			help:		0L, $				; current help text widget ID
			help1_base:	Help1_Base, $		; base to map for help 1
			help2_base:	Help2_Base, $		; base to map for help 2
			top_slider:	0L, $				; top slider ID
			bottom_slider:	0L, $			; bottom slider ID
			Zscale_mode_id:	0L, $			; Z display scale droplist
			interp_id:		0L, $			; interpolate images checkbox ID
			old_el:		ptr_new(/allocate_heap), $	; previous element list

			image:		0, $				; current image displayed
			corr_mode:	0, $				; 0: normal regions, 1: corr splines
			analyze_mode: 0, $				; analyze mode
			analyze_type: intarr(2), $		; analyze type

			zoom:		0, $				; zoom factor
			display_mode: 0, $				; display mode (0:conc, 1:errors)
			Zscale_mode:	0, $			; Z axis display mode
			interp:		1, $				; interpolate images
			mode_id:	0L, $				; ID of display mode droplist
			left_button: 0, $				; flags left mouse button
			right_button: 0, $				; right mouse

			max_set:	max_set, $			; number of normal marker sets
			pmark:		ptrarr(2), $		; pointers to include, exclude markers
			id:			-1, $				; id of current vertex
			oldx:		0, $				; old mouse X (zoomed to image coords)
			oldy:		0, $				; old mouse Y
			px:			ptr_new(), $		; pointer to temp X array
			py:			ptr_new(), $		; pointer to temp Y array
			theta:		0.0, $				; temp theta angle
			shear:		0.0, $				; temp shear angle
			curvature:	0.0, $				; temp curvature
			sizex:		0.0, $				; current size X of box, ellipse
			sizey:		0.0, $				; current size Y of box, ellipse
			size_units: 'pixels', $			; current size units

			width:		w, $				; width of pixmap     (pixels)
			height:		h, $				; height of pixmap     (pixels)
			w:			vx, $				; scr width of draw
			h:			vy, $				; scr height of draw

			size_event_x:	0, $			; previous resize X
			size_event_y:	0, $			; previous resize Y

			xview:		vx, $				; X view width      (pixels)
			yview:		vy, $				; Y view width      (pixels)
			xlow:		0, $				; X view low      (pixels)
			ylow:		0, $				; Y view low      (pixels)

			dw:			0, $				; width difference/border
			dh:			0, $				; height difference/border
			scr_xsize_off:  0, $			; resize offsets (see also map_help
			scr_ysize_off:  0, $
			tlb_width:	0, $				; tlb scr_xsize (initial)
			tlb_height:	0 }					; tlb scr_ysize (initial)

; The following assumes that we only have another 'image' as parent

OK = 0
if parent ne 0 then begin
    if widget_info( parent, /valid) then OK = 1
endif

if OK and clone then begin
    child = widget_info( parent, /child)
    widget_control, child, get_uvalue=other_pstate
    if ptr_valid( (*other_pstate).p) eq 0 then OK=0
endif
if OK and clone then begin
    if ptr_valid( (*other_pstate).p) then state.p = (*other_pstate).p
    if ptr_valid( (*other_pstate).pregions) then state.pregions = (*other_pstate).pregions
    state.local = 0
    state.file = (*other_pstate).file
    state.width = (*other_pstate).width
    state.height = (*other_pstate).height
    state.zoom = (*other_pstate).zoom
    state.display_mode = (*other_pstate).display_mode
    state.image = (*other_pstate).image
    state.analyze_type[0] = (*other_pstate).analyze_type[0]
    state.analyze_type[1] = (*other_pstate).analyze_type[1]
    state.analyze_mode = (*other_pstate).analyze_mode
    if ptr_valid( (*other_pstate).q) then state.q = ptr_new(*(*other_pstate).q)
    if ptr_valid( (*other_pstate).qc) then state.qc = ptr_new(*(*other_pstate).qc)

    if ptr_valid( (*other_pstate).pmark[0]) then state.pmark[0] = (*other_pstate).pmark[0]
    if ptr_valid( (*other_pstate).pmark[1]) then state.pmark[1] = (*other_pstate).pmark[1]
endif else begin

    state.pmark[0] = ptr_new( ptrarr( max_set+1), /no_copy)         ; include
    state.pmark[1] = ptr_new( ptrarr( max_set+1), /no_copy)         ; exclude

;	0 distance		2 ends
;	1 box			4 corners		centre handle   rotate handle
;	2 circle		2 diameters		centre handle
;	3 curve 8		9 spline pts	2 width handles (right mouse curvature)
;	4 traverse		4 handles		centre handle, 2 length, 2 width (right mouse shear)
;	5 ellipse		4 diameters		centre handle
;	6 spline 10		centre handle	10 control points
;	7 spline 32		centre handle	32 control points
;	8 projectX		4 handles		centre handle (no shear)
;	9 projectY		4 handles		centre handle (no shear)
;	10 spline 100	centre handle	100 control points
;	11 S pixel		1 point
;	12 M pixel		1 point			(not implemented yet)

    (*state.pmark[0])[0] = ptr_new( {present:0, x:fltarr(2), y:fltarr(2) }, /no_copy)									; distance

    (*state.pmark[0])[1] = ptr_new( {present:0, x:fltarr(6), y:fltarr(6), theta:0.0 }, /no_copy)						; box
    (*state.pmark[1])[1] = ptr_new( {present:0, x:fltarr(6), y:fltarr(6), theta:0.0 }, /no_copy)

    (*state.pmark[0])[2] = ptr_new( {present:0, x:fltarr(3), y:fltarr(3) }, /no_copy)									; circle
    (*state.pmark[1])[2] = ptr_new( {present:0, x:fltarr(3), y:fltarr(3) }, /no_copy)

    (*state.pmark[0])[3] = ptr_new( {present:0, x:fltarr(11), y:fltarr(11) }, /no_copy)									; curve traverse
    (*state.pmark[1])[3] = ptr_new( {present:0, x:fltarr(11), y:fltarr(11) }, /no_copy)

    (*state.pmark[0])[4] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }, /no_copy)	; linear traverse
    (*state.pmark[1])[4] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }, /no_copy)

    (*state.pmark[0])[5] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0 }, /no_copy)						; ellipse
    (*state.pmark[1])[5] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0 }, /no_copy)

    (*state.pmark[0])[6] = ptr_new( {present:0, x:fltarr(11), y:fltarr(11) }, /no_copy)									; spline 10
    (*state.pmark[1])[6] = ptr_new( {present:0, x:fltarr(11), y:fltarr(11) }, /no_copy)

    (*state.pmark[0])[7] = ptr_new( {present:0, x:fltarr(33), y:fltarr(33) }, /no_copy)									; spline 32
    (*state.pmark[1])[7] = ptr_new( {present:0, x:fltarr(33), y:fltarr(33) }, /no_copy)

    (*state.pmark[0])[8] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }, /no_copy) ; X projection
    (*state.pmark[1])[8] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }, /no_copy)

    (*state.pmark[0])[9] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }, /no_copy) ; Y projection
    (*state.pmark[1])[9] = ptr_new( {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }, /no_copy)

    (*state.pmark[0])[10] = ptr_new( {present:0, x:fltarr(101), y:fltarr(101) }, /no_copy)									; spline 32
    (*state.pmark[1])[10] = ptr_new( {present:0, x:fltarr(101), y:fltarr(101) }, /no_copy)

    (*state.pmark[0])[11] = ptr_new( {present:0, x:fltarr(1), y:fltarr(1) }, /no_copy)									; S pixel
    (*state.pmark[1])[11] = ptr_new( {present:0, x:fltarr(1), y:fltarr(1) }, /no_copy)

    (*state.pmark[0])[12] = ptr_new( {present:0, x:fltarr(1), y:fltarr(1) }, /no_copy)									; M pixel (later?)
    (*state.pmark[1])[12] = ptr_new( {present:0, x:fltarr(1), y:fltarr(1) }, /no_copy)

endelse

finish:
	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	return
end

;-----------------------------------------------------------------

pro PostCreate_image, wWidget, _EXTRA=_VWBExtra_

widget_control, wWidget, set_draw_view=[0,0]
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;

pro image_eventcb
end
