
; IDL Event Callback Procedures
; image_table_eventcb
;
;-----------------------------------------------------------------

pro clear_image_table_table, pstate

COMPILE_OPT STRICTARR
if image_table_invalid(pstate) then goto, clear

;free_image_table_all, pstate		; don't kill pointer data here!

clear:
	n = (*pstate).rows_max
	n_el = 50
	columns = replicate(' ',n_el)
	widths = replicate(10,n_el) * !d.x_ch_size
	(*pstate).columns = n_el
	(*pstate).file = ''

rows = string(indgen(n))
t = strarr(n_el,n)

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n
(*pstate).rows = 0

return
end

;-----------------------------------------------------------------

pro free_image_table_all, pstate

COMPILE_OPT STRICTARR
if image_table_invalid(pstate) then return
(*pstate).n = n_elements( *(*pstate).p)
if (*pstate).n lt 1 then return

	p = *(*pstate).p
	for i=0L,(*pstate).n-1 do begin
		if ptr_valid( p[i]) then begin
			free_table_entry, pstate, i
		endif
	endfor
	
	(*pstate).n = 0
	*(*pstate).p = !null		; clear contents of (*pstate).p, but don't change p itself
	
	return
end

;-----------------------------------------------------------------

pro free_table_entry, pstate, n

COMPILE_OPT STRICTARR
if image_table_invalid(pstate) then return

free_region_entry, (*pstate).p, n
return
end

;-----------------------------------------------------------------

function image_table_invalid, pstate

	if ptr_valid( (*pstate).p) eq 0 then goto, bad
	if n_elements( *(*pstate).p) lt 1 then goto, bad

	if ptr_valid( (*(*pstate).p)[0]) eq 0 then goto, bad
	return, 0

bad:
	(*pstate).n = 0
	*(*pstate).p = !null
	return, 1
end

;-----------------------------------------------------------------

pro load_image_table_table, pstate, plain=plain

COMPILE_OPT STRICTARR
common c_pige, use_PIGE_yields

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
		warning,'Load_image_table_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if image_table_invalid(pstate) then return
(*pstate).n = n_elements( *(*pstate).p)
if (*pstate).n lt 1 then return
if n_elements(plain) eq 0 then plain=0

n = (*pstate).n < (*pstate).rows_max
p = *(*pstate).p
obj = (*p[0]).DevObj

enable_cluster = 0
if obj_valid(obj) then begin
	if widget_info( (*pstate).cluster_id, /valid) then begin
		enable_cluster = obj->cluster()
	endif
endif
if enable_cluster eq 0 then (*pstate).cluster = 0
if widget_info( (*pstate).cluster_id, /valid) then widget_control, (*pstate).cluster_id, set_value=(*pstate).cluster, sensitive=enable_cluster

if widget_info( (*pstate).adc_combobox, /valid) then begin
	list = [(*pstate).adc_prelist, adc_list_device(obj)]
	widget_control, (*pstate).adc_combobox, set_value=list, $
						set_combobox_select=(*pstate).adc+(*pstate).adc_offset
endif

; Note order changes here need to be updated in 'image_table_update_cel' as well.

rows = string(indgen(n))
qe = where( (strlowcase( strmid((*(*p[0]).el)[*],0,4)) ne 'back') and (strlowcase( strmid((*(*p[0]).el)[*],0,3)) ne 'sum'), nqe)
if nqe eq 0 then return

columns = (*(*p[0]).el)[qe]
n_el = nqe
columns = ['Image','Note',columns,'Pixels']
col_index = [-1L, -1L, qe, -1L]
nc = n_elements(columns)
t = strarr(nc,n)
npx = nc-1
widths = replicate(10,nc) * !d.x_ch_size

for i=0L,n-1 do begin

	if i eq (n-1) then begin
		present = 0
		for k=0,(*p[i]).n_el-1 do begin
			present = present or ((where( (*(*p[i]).el)[k] eq (*(*p[0]).el)[qe]))[0] ne -1)
		endfor
		if not present then begin
			warning,'Load_image_table_table',['No elements in new region sum are present in the table.','', $
				'You always need to "Update: All" to apply loaded regions', $
				'to the current image data BEFORE adding new regions.']
		endif
	endif

	pc = (*p[i]).conc
	pe = (*p[i]).error
	pm = (*p[i]).mdl
	pq = (*p[i]).q
	pcent = (*p[i]).centroid
	mx = nqe
	conc = fltarr(mx)
	err = fltarr(mx)
	mdl = fltarr(mx)
	xcent = fltarr(mx)
	ycent = fltarr(mx)
	percent = 1
;	if (*p[i]).detector ne 0 then percent=0
	if strlowcase(extract_extension((*p[i]).matrix)) eq 'cuts' then percent=0
	if ((*p[i]).detector eq 1) and use_PIGE_yields then percent=1

	if ptr_valid( pc) then conc = (*pc)[qe]
	if ptr_valid( pe) then err = (*pe)[qe] > 0.0
	if ptr_valid( pm) then mdl = (*pm)[qe] > 0.0
	if ptr_valid( pcent) then begin
		xcent = (*pcent)[qe].x
		ycent = (*pcent)[qe].y
	endif
	merr = err > conc*0.01

	cut = 0
	if strlowcase(extract_extension((*p[i]).matrix)) eq 'cuts' then cut=1
	high = cut ? 1.0E+15 : 1.0E+10
	misc = strlowcase( special_elements() )

	rows[i] = str_tidy((*p[i]).index)
	t[0,i] = (*p[i]).el_shown
	t[1,i] = (*p[i]).note
	t[npx,i] = n_elements( *pq)			; pixels
	for j=0L,mx-1 do begin
		q = where( strlowcase(columns[(j+2)<(nc-1)]) eq misc, nq)
		if nq eq 0 then begin
			perc = percent
			hi = high
		endif else begin
			perc = 0
			hi = 1.0e+30
		endelse
		case (*pstate).display_mode of
			0: t[j+2,i] = build_result( conc[j], merr[j], mdl[j], high=hi, percent=perc, plain=plain)	; conc
			1: t[j+2,i] = build_result( err[j], err[j], 0.0, percent=perc, plain=plain)					; error
			2: t[j+2,i] = build_result( mdl[j], mdl[j], 0.0, percent=perc, plain=plain)					; mdl
			3: t[j+2,i] = build_result( err[j]/(conc[j]>1.0), 0.001, 0.0, high=0.9, plain=plain)		; rel error
			4: t[j+2,i] = build_result( conc[j], 0.01, 0.0, plain=plain)								; raw
			5: t[j+2,i] = build_result( xcent[j], 0.001, 0.0, plain=plain)								; X centroid
			6: t[j+2,i] = build_result( ycent[j], 0.001, 0.0, plain=plain)								; Y centroid
		endcase
	endfor
endfor
(*pstate).columns = nc
*(*pstate).pcol_index = col_index

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, align=2
(*pstate).rows = n

return
end

;-----------------------------------------------------------------

pro image_table_restore_index_order, pstate

COMPILE_OPT STRICTARR
p = *(*pstate).p
(*pstate).n = n_elements( p)
if ptr_good(p) eq 0 then return

	index = fltarr((*pstate).n)								; restore index order
	for k=0,(*pstate).n-1 do index[k] = (*p[k]).index
	q = sort(index)
	*(*pstate).p = p[q]
	load_image_table_table, pstate
	return
end

;-----------------------------------------------------------------

pro new_table_pselect, pstate, i, select=select

; Set 'pselect' for Notify to unsorted row (top) selected
; Also return 'select' as all (unsorted) indices select between top and bottom

COMPILE_OPT STRICTARR
p = *(*pstate).p
(*pstate).n = n_elements( p)
if n_elements(i) lt 1 then i=(*pstate).sel.top
if (i lt 0) or (i ge (*pstate).n) then return

index = lonarr((*pstate).n)
for j=0,(*pstate).n-1 do index[j] = (*p[j]).index
qs = sort(index)
qs = index[qs]
q = where( qs eq (*p[i]).index, nq)

sel = (*pstate).sel
sel.top = q[0]				; unsorted position in table
sel.bottom = q[0]
*(*pstate).pselect =  { sel:sel, pregion:(*(*pstate).p)[i] }

(*pstate).sel.top <= ((*pstate).n-1)
(*pstate).sel.bottom <= ((*pstate).n-1)

select = lonarr((*pstate).sel.bottom-(*pstate).sel.top+1)
k = 0
for j=(*pstate).sel.top,(*pstate).sel.bottom do begin
	select[k++] = (*p[j]).index
endfor
return
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

pro OnButton_Image_Table_Update, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).n = n_elements( *(*pstate).p)
if (*pstate).n lt 1 then goto, done

;new_table_pselect, pstate, 0
notify, 'image-region-clear', from=event.top
notify, 'image-region-update', from=event.top
(*pstate).file_valid = 0

done:
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Update_One, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).n = n_elements( *(*pstate).p)
if (*pstate).n lt 1 then goto, done

notify, 'image-region-update-one', (*pstate).pselect, from=event.top
(*pstate).file_valid = 0

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

pro OnButton_Image_Table_Load, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

file = strip_path(strip_file_ext( (*pstate).file))
if (file eq '') and widget_info( (*pstate).group, /valid) then begin
	child = widget_info( (*pstate).group, /child)
	widget_control, child, get_uvalue=pstate_image
	file = strip_path(strip_file_ext( (*pstate_image).file))
endif
file = strip_file_keys( file, remove=['-trav','-q1'], add=['-q1'])
file = file + '.region'

if ptr_good((*pstate).p) then begin
	p = (*(*pstate).p)[0]
	if ptr_valid(p) then begin
		if ((*p).analyze_type[0] eq 3) or ((*p).analyze_type[0] eq 4) or ((*p).analyze_type[0] eq 8) or ((*p).analyze_type[0] eq 9) then tag='trav' else tag='q1'
		file2 = strip_path(strip_file_ext( (*p).file))
		file2 = strip_file_keys( file2, remove=['-trav','-q1'], add=['-'+tag])
		if file2 eq '' then file2 = strip_file_ext( file)
		file2 = file2 + '.region'
	endif
endif else file2=file

F = file_requester( filter = '*.region',  file=file2, $
			title='Load Regions from file', path=*(*pstate).path, group=event.top, /fix_filter)
if F ne '' then begin
	free_image_table_all, pstate
	clear_image_table_table, pstate

	p = read_regions(F, path=*(*pstate).path)
	if ptr_valid(p[0]) then begin
		(*pstate).n = n_elements(p)
;		(*pstate).adc = (*p[0]).channel
		(*pstate).adc = -1
		*(*pstate).p = p
		(*pstate).file = F
		(*pstate).file_valid = 1
		*(*pstate).path = extract_path( F)
;		notify, 'path', (*pstate).path, from=event.top
		load_image_table_table, pstate

		OnCellSelect_Image_Table2, pstate, event.top, (*pstate).n-1

;		Load matching region spectra, if file found ...

		F = strip_file_ext(F) + '.spec'
;		widget_control, /hourglass
		pp = read_spec( F)
		good = 1
		if ptr_valid(pp[0]) then begin
			if ptr_valid( (*pstate).pspec) then begin
				if ptr_valid( (*(*pstate).pspec)[0] ) then begin
					if (*(*(*pstate).pspec)[0]).orphan eq 1 then begin
						(*pstate).local = 1
						(*(*(*pstate).pspec)[0]).orphan = 0
					endif
				endif
				if ((*pstate).local eq 1) then free_spectra, (*pstate).pspec
			endif

;			Check spectra for number, and reorder region labels ...

			if n_elements(p) ne n_elements(pp) then begin
				warning,'OnButton_Image_Table_Load',['Number of spectra does not match number of regions.', '', $
						'You may need to re-extract the region spectra.']
				good = 0
			endif else begin
				for i=0,n_elements(p)-1 do begin
					s = strsplit( (*pp[i]).label, ' ,', /extract, count=ns)
					if s[0] ne 'Region' then begin
						warning,'OnButton_Image_Table_Load',['Region spectrum file contains non-region spectra.', '', $
								'You may need to re-extract the region spectra.']
						good = 0
						break
					endif else begin
						(*pp[i]).label = 'Region ' + str_tidy(i)
						if ns ge 3 then (*pp[i]).label = (*pp[i]).label + ', ' + strjoin(s[2:*],' ')
					endelse
				endfor
			endelse
			(*pstate).pspec = ptr_new(pp, /no_copy)
			(*pstate).local = 0
			(*(*(*pstate).pspec)[0]).orphan = 1
			notify, 'spectra', (*pstate).pspec, from=event.top
		endif else good=0
		notify, 'image-results', from=event.top

		if good then begin
			print,'OnButton_Image_Table_Load: Matching regions found. Pass regions p[].'
		endif else begin
			print,'OnButton_Image_Table_Load: Matching regions NOT found: '+F
		endelse
		*(*pstate).pregions = good ? *(*pstate).p : 0L
		notify, 'image-regions', (*pstate).pregions, from=event.top
	endif
endif
return
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Match, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if image_table_invalid(pstate) then goto, done
(*pstate).n = n_elements( *(*pstate).p)
if (*pstate).n lt 1 then goto, done
i = (*pstate).sel.top
if (i lt 0) or (i ge (*pstate).n) then goto, done

notify, 'image-match-centroids', (*pstate).pselect, from=event.top

done:
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Modify, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	if image_table_invalid(pstate) then goto, done
	(*pstate).n = n_elements( *(*pstate).p)
	if (*pstate).n lt 1 then goto, done
	i = (*pstate).sel.top
	if (i lt 0) or (i ge (*pstate).n) then goto, done
	p = (*(*pstate).p)[i]
	
	mask = bytarr((*p).nx,(*p).ny)
	mask[ *(*p).q] = 1
	
	case (*pstate).modify_mode of
		0: begin							; erode 1
			s = round_kernel(3)
			mask = erode( mask, s)
			end
		1: begin							; erode 2
			s = round_kernel(5)
			mask = erode( mask, s)
			end
		2: begin							; erode 3
			s = round_kernel(7)
			mask = erode( mask, s)
			end
		3: begin							; erode 5
			s = round_kernel(11)
			mask = erode( mask, s)
			end
		4: begin							; erode 10
			s = round_kernel(21)
			mask = erode( mask, s)
			end
		5: begin							; erode 20
			s = round_kernel(41)
			mask = erode( mask, s)
			end
		6: begin							; erode 50
			s = round_kernel(101)
			mask = erode( mask, s)
			end
		7: begin							; erode 100
			s = round_kernel(201)
			mask = erode( mask, s)
			end
		8: begin							; dilate 1
			s = round_kernel(3)
			mask = dilate( mask, s)
			end
		9: begin							; dilate 2
			s = round_kernel(5)
			mask = dilate( mask, s)
			end
		10: begin							; dilate 3
			s = round_kernel(7)
			mask = dilate( mask, s)
			end
		11: begin							; dilate 5
			s = round_kernel(11)
			mask = dilate( mask, s)
			end
		12: begin							; dilate 10
			s = round_kernel(21)
			mask = dilate( mask, s)
			end
		13: begin							; dilate 20
			s = round_kernel(41)
			mask = dilate( mask, s)
			end
		14: begin							; dilate 50
			s = round_kernel(101)
			mask = dilate( mask, s)
			end
		15: begin							; dilate 100
			s = round_kernel(201)
			mask = dilate( mask, s)
			end
		else:
	endcase

	q = where(mask ne 0, nq)

	if (nq eq 0) and widget_info( (*pstate).minimal_centroid_element, /valid) then begin
		widget_control, (*pstate).minimal_centroid_element, get_value=s
		q3 = where( *(*p).el eq s, nq3)
		if nq3 eq 0 then begin
			el_code, *(*p).el, el1, z1, shell1, bad1, error1
			el_code, s, el2, z2, shell2, bad2, error2
			q3 = where( (z1 eq z2) and (shell1 eq shell2), nq3)
			if (nq3 eq 0) and (shell2 eq 1) then q3 = where( (z1 eq z2), nq3)
			if nq3 gt 1 then begin
				q4 = sort(shell1[q3])
				q3 = q3[q4]
			endif
			if z2 eq 0 then nq3=0
		endif
		if nq3 eq 0 then begin
			warning,'OnButton_Image_Table_Modify',['No selected pixels remain.', '', $
				'Tried using centroid from Hotspot element as single pixel selection.', $
				'But element "'+s+'" was not found.']
		endif else begin
			j = q3[0]
			x = round((*(*p).centroid)[j].x)
			y = round((*(*p).centroid)[j].y)
			mask[x,y] = 1
			q = where(mask ne 0, nq)
		endelse
	endif

	*(*pstate).pmodify = *(*p).q
	*(*p).q = q

	new_table_pselect, pstate, i
	notify, 'image-region-clear', from=event.top
	notify, 'image-region-select', (*pstate).pselect, from=event.top
	wait, 1.0
	notify, 'image-region-update-one', (*pstate).pselect, from=event.top

done:
	return
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Modify_Undo, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	if image_table_invalid(pstate) then goto, done
	(*pstate).n = n_elements( *(*pstate).p)
	if (*pstate).n lt 1 then goto, done
	i = (*pstate).sel.top
	if (i lt 0) or (i ge (*pstate).n) then goto, done
	p = (*(*pstate).p)[i]
	
	*(*p).q = *(*pstate).pmodify

	new_table_pselect, pstate, i
	notify, 'image-region-clear', from=event.top
	notify, 'image-region-select', (*pstate).pselect, from=event.top
	wait, 1.0
	notify, 'image-region-update-one', (*pstate).pselect, from=event.top

done:
	return
end
	
;-----------------------------------------------------------------

pro OnButton_Image_Table_Save, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if image_table_invalid(pstate) then goto, done
(*pstate).n = n_elements( *(*pstate).p)
if (*pstate).n lt 1 then goto, done
n = 0
p = (*(*pstate).p)[n]

file = (*pstate).file
if file[0] eq '' then file=strip_file_ext((*p).file) + '.region'

path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
if ((*p).analyze_type[0] eq 3) or ((*p).analyze_type[0] eq 4) or ((*p).analyze_type[0] eq 8) or ((*p).analyze_type[0] eq 9) then tag='trav' else tag='q1'
file = strip_file_keys( file, remove=['-trav','-q1'], add=['-'+tag])

F = file_requester( /write, filter = '*.region', path=*(*pstate).path, $
			title='Save Regions to file', file=file, group=event.top, /fix_filter)
if F ne '' then begin
	image_table_restore_index_order, pstate
	write_regions, (*pstate).p, F
	(*pstate).file = F
	(*pstate).file_valid = 1
	for i=0L,(*pstate).n-1 do (*(*(*pstate).p)[i]).region_file = F
endif

done:
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Export, Event, hotspots=hotspots, hs_el=hs_el

;	/hotspots	hot-spots export table, with centroids for element 'hs_el'

COMPILE_OPT STRICTARR
if n_elements(hotspots) eq 0 then hotspots=0
if hotspots then begin
	if n_elements(hs_el) eq 0 then hs_el=''
	if n_elements(hs_el) ge 1 then hs_el = hs_el[0]
	if hs_el eq '' then goto, bad_element
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	path = *(*pstate).path
	file = strip_file_ext((*pstate).file) + '.csv'
	if hotspots then begin
		title = 'Export values with '+hs_el+' hotspot centroids to a CSV file' 
		remove = ''
		if locate('-centroids',file) ne -1 then remove = ['-centroids','-'+hs_el]
		file = strip_file_keys( file, remove=remove, add=['-centroids','-'+hs_el])
	endif else begin
		title = 'Export the displayed values to an ASCII CSV file'
	endelse

	F = file_requester( /write, filter = '*.csv', file=file, path=path, group=event.top, $
							title=title, /fix_filter)
	if F ne '' then begin
		F = strip_file_ext(F) + '.csv'
		*(*pstate).path = extract_path(F)
		p = *(*pstate).p

		q = where( (strlowcase( strmid((*(*p[0]).el)[*],0,4)) ne 'back') and (strlowcase( strmid((*(*p[0]).el)[*],0,3)) ne 'sum'), nq)
		if nq lt 1 then return

		columns = [ (*(*p[0]).el)[q],'Pixels']			; skips over 'Image' and 'Note' columns
		nc = n_elements(columns)

		xanes = strlowcase(extract_extension( (*p[0]).file)) eq 'xan'		; from a 3D data-set --> XANES
		qe = indgen(nc)
		allnum = 1
		for j=0,nc-2 do allnum = allnum and gnumeric(columns[j]) 			; now all columns are numeric --> XANES
		xanes = xanes or allnum
		nbig = 50
		if xanes and (nc gt 2*nbig+1) then begin
			tcolumns = columns
			columns = [columns[0:nbig-1],columns[nbig]+'...'+columns[nc-nbig-1],columns[nc-nbig:nc-1]]
			qet = [qe[0:nbig-1], qe[nbig], qe[nc-nbig:nc-1]]
			qspan = qe[nbig:nc-nbig-1]
		endif else begin
;			if strupcase( strmid((*(*p[0]).el)[0],0,4)) eq 'BACK' then begin
;				strip_back = 1
;				columns = columns[1:*]
;				qe = qe[1:*]
;			endif
		endelse
		good_element = 1

		select = element_select( event.top, columns, old_select=*(*pstate).pexport, path=*(*pstate).path )
		qselect = where(select eq 1, nqselect)
		if qselect[0] eq -1 then goto, done
		*(*pstate).pexport = select
		if xanes and (nc gt 2*nbig+1) then begin
			qselect = qet[qselect]
			q = where(qselect eq nbig, nq)
			if nq gt 0 then qselect = [qselect[0:q[0]-1], qspan, qselect[q[0]+1:*]]
			nqselect = n_elements(qselect)
			columns = tcolumns
		endif
		
		child2 = widget_info( (*pstate).group, /child)
		widget_control, child2, get_uvalue=pstate_image
		pimg = (*pstate_image).p

		r = image_absolute( pimg, absolute=0, error=err)
		cx = r.absolute.size.x / r.uncompressed.size.x
		cy = r.absolute.size.y / r.uncompressed.size.y

		close_file, 1
		on_ioerror, bad_open
		openw,1, F
		on_ioerror, bad_write

		if xanes then begin
			names = columns[qselect]
			columns = ['Energy','#'+strtrim(string(indgen((*pstate).n)),2)]
			s = strjoin(columns,',')
			printf,1, s
			qtable = qselect + 2							; skip over 'Image' and 'Note'
		
			load_image_table_table, pstate, /plain
	
			widget_control, (*pstate).table, get_value=t
			t = string(t[qtable, 0:(*pstate).n-1])

			for j=0L,nqselect-1 do begin
				s = strjoin(reform(t[j,*]),',')
				printf,1, names[j] + ',' + s
			endfor
		endif else begin
			columns = columns[qselect]
			columns2 = ['#','Image','Note',columns]
			s = strjoin(columns2,',')
			qtable = qselect + 2							; skip over 'Image' and 'Note'
		
			if hotspots then begin
				printf,1, s + ',pX,pY,rX,rY'
				old_mode = (*pstate).display_mode

;				(*pstate).display_mode = 0
				load_image_table_table, pstate, /plain
				widget_control, (*pstate).table, get_value=t		; conc
				t2 = string(t[qtable, 0:(*pstate).n-1])
				t2 = str_tidy( float(t2) > 0.)						; suppress negatives

				(*pstate).display_mode = 5
				load_image_table_table, pstate, /plain
				widget_control, (*pstate).table, get_value=t		; X centroid
				t3 = string(t[qtable, 0:(*pstate).n-1])
				q3 = where( columns eq hs_el, nq3)
				if nq3 eq 0 then begin
					el_code, columns, el1, z1, shell1, bad1, error1
					el_code, hs_el, el2, z2, shell2, bad2, error2
					q3 = where( (z1 eq z2) and (shell1 eq shell2), nq3)
					if (nq3 eq 0) and (shell2 eq 1) then q3 = where( (z1 eq z2), nq3)
					if nq3 gt 1 then begin
						q4 = sort(shell1[q3])						; use lowest shell match
						q3 = q3[q4]
					endif
					if z2 eq 0 then nq3=0
				endif
				if nq3 gt 0 then begin
					t3 = t3[q3[0],*]
					t3 = str_tidy( float(t3) > 0.)					; suppress negatives
				endif else begin
					t3 = t3[0,*]
					t3[*] = ''
					good_element = 0
				endelse
				
				(*pstate).display_mode = 6
				load_image_table_table, pstate, /plain
				widget_control, (*pstate).table, get_value=t		; Y centroid
				t4 = string(t[qtable, 0:(*pstate).n-1])
				if nq3 gt 0 then begin
					t4 = t4[q3[0],*]
					t4 = str_tidy( float(t4) > 0.)					; suppress negatives
				endif else begin
					t4 = t4[0,*]
					t4[*] = ''
				endelse
				if good_element eq 0 then goto, bad_element

				(*pstate).display_mode = old_mode
			endif else begin
				printf,1, s

				load_image_table_table, pstate, /plain
				widget_control, (*pstate).table, get_value=t
				t2 = string(t[qtable, 0:(*pstate).n-1])
				st = size(t2)
				t2 = str_tidy( float2(t2) > 0.)						; suppress negatives
				t3 = '' & t4 = ''
			endelse

;			absx, absy are pixel coords in full scan
;			These * cx,cy are the scan coords (mm).

			for j=0L,(*pstate).n-1 do begin
				s = strjoin( t2[*,j],',')
				if hotspots then begin
					absx = (*p[j]).xoffset + float2(t3[0,j]) * (*p[j]).xcompress
					absy = (*p[j]).yoffset + float2(t4[0,j]) * (*p[j]).ycompress
					s = s + ',' + str_tidy(absx) + ',' + str_tidy(absy)
					s = s + ',' + str_tidy( cx*absx) + ',' + str_tidy( cy*absy)
				endif
				label = string(t[0,j])
				note = t[1,j]
				printf,1, string(j) + ',' + label + ',' + note + ',' + s
			endfor
		endelse

		load_image_table_table, pstate
	endif
done:
	on_ioerror, null
	close_file, 1
	return
	
bad_element:
	warning,'OnButton_Image_Table_Export',['Invalid "element" supplied for centroids.','Select the element to use to calculate centroids.']
	goto, done
bad_open:
	warning,'OnButton_Image_Table_Export',['Failed to open file: '+F[0],'Check that file is not open in Excell.']
	goto, done
bad_write:
	warning,'OnButton_Image_Table_Export',['Bad write to file: '+F[0],'Check that file is not open in Excell.']
	goto, done
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Import_Regions, Event

;	Import region parameters and pixel selections from CSV files, one file per region.

	COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	image_table_restore_index_order, pstate

	child2 = widget_info( (*pstate).group, /child)
	widget_control, child2, get_uvalue=pstate_image
	pimg = (*pstate_image).p

	path = *(*pstate).path
	file = strip_file_ext((*pstate).file) + '.csv'
	title = 'Import regions from separate ASCII CSV file(s)'

	F = file_requester( /read, filter = '*.csv', file='', path=path, group=event.top, $
							title=title, /fix_filter, /multiple_files)
	for i=0,n_elements(F)-1 do begin
		if F[i] ne '' then begin
			*(*pstate).path = extract_path(F[i])
			p = ptr_new( define(/table))

			on_ioerror, bad_open
			openr, unit, F[i], /get_lun
			on_ioerror, bad_read

			line = ''
			while EOF(unit) eq 0 do begin
				readf, unit, line
				if (lenchr(line) gt 0) and (extract(line,0,0) ne '#') then begin
					str = strsplit( line, ',', /extract)
					n_str = n_elements(str)
					s = strlowcase(str[0])
		
					case s of
						'files': begin
							if n_str ge 2 then (*p).region_file = str[1]
							if n_str ge 3 then (*p).file = str[2]
							end
						'offset': begin
							if n_str ge 2 then (*p).xoffset = long2(str[1])
							if n_str ge 3 then (*p).yoffset = long2(str[2])
							end
						'compress': begin
							if n_str ge 2 then (*p).xcompress = long2(str[1])
							if n_str ge 3 then (*p).ycompress = long2(str[2])
							end
						'image': begin
							if n_str ge 2 then (*p).nx = long2(str[1])
							if n_str ge 3 then (*p).ny = long2(str[2])
							end
						'scan': begin
							if n_str ge 2 then (*p).scanx = float2(str[1])
							if n_str ge 3 then (*p).scany = float2(str[2])
							end
						'original': begin
							if n_str ge 2 then (*p).original_xsize = long2(str[1])
							if n_str ge 3 then (*p).original_ysize = long2(str[2])
							end
						'scaled': begin
							if n_str ge 2 then (*p).scaled_x = long2(str[1])
							if n_str ge 3 then (*p).scaled_y = long2(str[2])
							end
						'sample': begin
							if n_str ge 2 then (*p).sample = str[1]
							end
						'grain': begin
							if n_str ge 2 then (*p).grain = str[1]
							end
						'comment': begin
							if n_str ge 2 then (*p).comment = str[1]
							end
						'dwell': begin
							(*p).dwell.on = 0
							if n_str ge 2 then (*p).dwell.val = float2(str[1])
							end
						'array': begin
							if n_str ge 2 then (*p).array = long2(str[1])
							end
						'active': begin
							if n_str ge 2 then (*p).pactive = ptr_new( long2(str[1:*]))
							end
						'mode': begin
							if n_str ge 2 then (*p).mode = 1		; fixed by convention
							end
						'display': begin
							if n_str ge 2 then (*p).el_shown = str[1]
							if n_str ge 3 then (*p).elx = str[2]
							if n_str ge 4 then (*p).ely = str[3]
							end
						'note': begin
							if n_str ge 2 then (*p).note = strjoin(str[1:*],'_')
							end
						'charge': begin
							if n_str ge 2 then (*p).charge = float2(str[1])
							if n_str ge 3 then (*p).IC_total = float2(str[2])
							end
						'n_elements': begin
							if n_str ge 2 then (*p).n_el = long2(str[1])
							end
						'element': begin
							if n_str ge 2 then (*p).el = ptr_new(str[1:*])
							n_el = n_str-1
							if n_el ne (*p).n_el then begin
								warning,'OnButton_Image_Table_Import_Regions','Inconsistent "n_el" encountered on read.'
							endif
							end
						'conc': begin
							if n_str ge 2 then (*p).conc = ptr_new( float2(str[1:*]))
							end
						'error': begin
							if n_str ge 2 then (*p).error = ptr_new( float2(str[1:*]))
							end
						'mdl': begin
							if n_str ge 2 then (*p).mdl = ptr_new( float2(str[1:*]))
							end
						'centroidx': begin
							if n_str ge 2 then centroidx = float2(str[1:*])
							end
						'centroidy': begin
							if n_str ge 2 then centroidy = float2(str[1:*])
							end
						'q': begin
							if n_str ge 2 then (*p).q = ptr_new( long2(str[1:*]))
							goto, get_data
							end
						else:
					endcase
				endif
			endwhile
			goto, bad_q
get_data:
			while EOF(unit) eq 0 do begin
				readf, unit, line
				if (lenchr(line) gt 0) and (extract(line,0,0) ne '#') then begin
					str = strsplit( line, ',', /extract)
					n_str = n_elements(str)
					if n_str ge 2 then *(*p).q = [ *(*p).q, long2(str[1:*]) ]
				endif
			endwhile
		endif

		if n_elements(centroidx) ge 1 then begin
			c = replicate( {x:0.0, y:0.0}, n_elements(centroidx))
			for j=0, n_elements(centroidx)-1 do begin
				c[j].x = centroidx[j]
				if n_elements(centroidy) ge j+1 then c[j].y = centroidy[j]
			endfor
			(*p).centroid = ptr_new(c)
		endif
		(*p).mode = 1		; fixed by convention
done:
		on_ioerror, null
		close_file, unit
	
		if (*p).note eq '' then (*p).note = 'Import: ' + F[i]
		if n_elements( *(*pstate).p) eq 0 then begin
			*(*pstate).p = p
		endif else begin
			*(*pstate).p = [ *(*pstate).p, p]
		endelse
		(*pstate).n = n_elements( *(*pstate).p)
		ii = (*pstate).n -1
	endfor

	for k=0,(*pstate).n-1 do (*(*(*pstate).p)[k]).index = k			; assign fresh index to each row including new ones
	load_image_table_table, pstate

	*(*pstate).pregions = *(*pstate).p								; for Notify to spectra
	notify, 'image-regions', (*pstate).pregions, from=event.top		; send to spectra

	OnCellSelect_Image_Table2, pstate, event.top, ii				; select last row
	notify, 'image-results', from=event.top							; notify of new table results

	warning,'OnButton_Image_Table_Import_Regions', $
			['', 'Remember to use "Update: All" to apply the imported', $
				'region pixel selections to the current images.','', $
				'This will add missing image metadata to the ', $
				'imported region data. ', '', $
				'This will be necessary for a subsequent', $
				'extraction of spectra to work correctly.'],/info
	return
	
bad_open:
	warning,'OnButton_Image_Table_Import_Regions',['Failed to open file: '+F[i],'Check that file is not open in Excell.']
	goto, done
bad_read:
	warning,'OnButton_Image_Table_Import_Regions',['Bad read from file: '+F[i]]
	goto, done
bad_q:
	warning,'OnButton_Image_Table_Import_Regions','No "Q" data found on read.'
	goto, done
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Export_Regions, Event

;	Export region parameters and pixel selections to CSV files, one per region.

	COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	child2 = widget_info( (*pstate).group, /child)
	widget_control, child2, get_uvalue=pstate_image
	pimg = (*pstate_image).p

	path = *(*pstate).path
	file = strip_file_ext((*pstate).file) + '.csv'
	title = 'Export each region to a separate ASCII CSV file'

	F0 = file_requester( /write, filter = '*.csv', file=file, path=path, group=event.top, $
							title=title, /fix_filter)
	if F0 ne '' then begin
		*(*pstate).path = extract_path(F0)
		p = *(*pstate).p

		select = element_select( event.top, str_tidy(indgen((*pstate).n)), path=*(*pstate).path, $
						title='Select regions to export')
		q = where(select eq 1, nq)
		if nq eq 0 then goto, done

		for j=0,nq-1 do begin
			i = q[j]
			F = strip_file_ext(F0) + '-region'+str_tidy(i) + '.csv'
			close_file, 1
			on_ioerror, bad_open
			openw, 1, F
	
			printf,1, '# GeoPIXE region export file, saved by "OnButton_Image_Table_Export_Regions".'
			printf,1, '#'
			printf,1, '# Saves pixel selection as "Q" vector of indices into the image array.'
			printf,1, '# This is for current image size given by "Image", which may be some "Offset" into'
			printf,1, '# full image ("Original"). Additionally, both the image and original may have been'
			printf,1, '# "Compressed" in X or Y during sorting from event data.'
			printf,1, '# These parameters become important to import correctly back into GeoPIXE, to translate'
			printf,1, '# "Q", if image parameters in GeoPIXE differ from those relating to the import "Q".'
			printf,1, '#'
			printf,1, '# A minimum set for the "Import" of a region are: Image size ("Image" line), X,Y offsets'
			printf,1, '# ("Offset" line) and the compression ("Compress" line). Typically, add a "Note" too.'
			printf,1, '# You can set "Display", if you would like to tag elements as relevant to a region.'
			printf,1, '# All other paramters are optional on Import. Typically, use "Update: All" after import,'
			printf,1, '# which applies the imported "Q" to the current images, which determines other data.'
			printf,1, '#'
			printf,1, '# Legend:'
			printf,1, '#    Files: region and DAI image file names'
			printf,1, '#    Compress: image was compressed by integer factor in X and/or Y during sort from events'
			printf,1, '#    Original: original full pixel size of image after compress'
			printf,1, '#    Offset: offset of image within full pixel range of "Original" image (windowed sort)'
			printf,1, '#    Image: current pixel size of image (after offset and compress)'
			printf,1, '#    Scan: size of image in mm'
			printf,1, '#    Scaled: a manual floating scale factor was applied to X/Y (old, not used now)'
			printf,1, '#    Sample: sample name'
			printf,1, '#    Grain: grain or analysis point/grain'
			printf,1, '#    Comment: comment added at data acquisition time'
			printf,1, '#    Note: note added to region'
			printf,1, '#    Dwell: dwell time per pixel (ms)'
			printf,1, '#    Array: 1 (detector array), 0 (single detector)'
			printf,1, '#    Active: detector channel #s used for image'
			printf,1, '#    Charge: charge (uC equivalent), flux value'
			printf,1, '#    n_elements: number of element planes including background, dwell, flux, etc.'
			printf,1, '#    Element: names of "Element" planes'
			printf,1, '#    Conc: average concentration (ppm) in selected pixels (includes number of pixels)'
			printf,1, '#    Error: uncertainty (1-sigma) of concentration (ppm) in selected pixels'
			printf,1, '#    MDL: minimum detection limit (99% confidence) for selected pixels'
			printf,1, '#    CentroidX: X centroid conc-weighted across selected pixels'
			printf,1, '#    CentroidY: Y centroid conc-weighted across selected pixels'
			printf,1, '#    Mode: 0 (shape region on element image), 1 (Association element-element correlation)'
			printf,1, '#        Note: All regions become Mode=1 on import as shapes are lost.'
			printf,1, '#    Display: element displayed to set shape or element-element pair for Association'
			printf,1, '#    Q: indices of selected pixels in compressed, offset image (continues on multiple lines to EOF)'
			printf,1, '#'
			format = '(100(A,:,","))'
			printf,1, format=format, 'Files', (*p[i]).region_file, (*p[i]).file
			printf,1, format=format, 'Offset', str_tidy((*p[i]).Xoffset), str_tidy((*p[i]).Yoffset)
			printf,1, format=format, 'Compress', str_tidy((*p[i]).Xcompress), str_tidy((*p[i]).Ycompress)
			printf,1, format=format, 'Image', str_tidy((*p[i]).nx), str_tidy((*p[i]).ny)
			printf,1, format=format, 'Scan', str_tidy((*p[i]).scanx), str_tidy((*p[i]).scany)
			printf,1, format=format, 'Original', str_tidy((*p[i]).original_xsize), str_tidy((*p[i]).original_ysize)
			printf,1, format=format, 'Scaled', str_tidy((*p[i]).scaled_x), str_tidy((*p[i]).scaled_y)
			printf,1, format=format, 'Sample', replace( ',','_', (*p[i]).sample)
			printf,1, format=format, 'Grain', replace( ',','_', (*p[i]).grain)
			printf,1, format=format, 'Comment', replace( ',','_', (*p[i]).comment)
			printf,1, format=format, 'Dwell', str_tidy((*p[i]).dwell.val)
			printf,1, format=format, 'Array', str_tidy((*p[i]).array)
			if (*p[i]).array then begin
				printf,1, 'Active,' + strjoin( str_tidy(*(*p[i]).pactive), ",")
			endif

			printf,1, format=format, 'Mode', str_tidy((*p[i]).mode)
			printf,1, format=format, 'Display', (*p[i]).el_shown, (*p[i]).elx, (*p[i]).ely 
			printf,1, format=format, 'Note', (*p[i]).note
			printf,1, format=format, 'Charge', str_tidy((*p[i]).charge), str_tidy((*p[i]).IC_total)

			n = (*p[i]).n_el
			printf,1, format=format, 'N_elements', str_tidy(n)
			printf,1, format=format, 'Element', (*(*p[i]).el)[0:n-1]
			printf,1, format=format, 'Conc', str_tidy( (*(*p[i]).conc)[0:n-1])
			printf,1, format=format, 'Error', str_tidy( (*(*p[i]).error)[0:n-1])
			printf,1, format=format, 'MDL', str_tidy( (*(*p[i]).mdl)[0:n-1])

			cx = fltarr(n)
			cy = fltarr(n)
			for k=0,n-1 do begin
				cx[k] = (*(*p[i]).centroid)[k].x
				cy[k] = (*(*p[i]).centroid)[k].y
			endfor
			printf,1, format=format, 'CentroidX', str_tidy( cx[0:n-1])
			printf,1, format=format, 'CentroidY', str_tidy( cy[0:n-1])

			printf,1, format=format, 'Q', str_tidy(*(*p[i]).q)
			
			on_ioerror, null
			close_file, 1
		endfor
	endif

done:
	on_ioerror, null
	close_file, 1
	return

bad_open:
	warning,'OnButton_Image_Table_Export_Regions',['Failed to open file: '+F[0],'Check that file is not open in Excell.']
	goto, done
end

;-----------------------------------------------------------------

;pro OnButton_Image_Table_Export_XFM, Event
;
;;	XFM export of region bounds
;
;COMPILE_OPT STRICTARR
;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate
;
;	default = geopixe_defaults( source='Image Table XFM Export')
;	if default.custom.enable eq 0 then return
;	if default.custom.lab ne 'XFM' then return
;
;	path = fix_path( default.custom.path)
;	file = strip_file_ext(default.custom.file) + '.csv'
;
;	p = *(*pstate).p
;
;		child2 = widget_info( (*pstate).group, /child)
;		widget_control, child2, get_uvalue=pstate_image
;		pimg = (*pstate_image).p
;
;		r = image_absolute( pimg, absolute=0, error=err)
;		cx = r.absolute.size.x / r.uncompressed.size.x
;		cy = r.absolute.size.y / r.uncompressed.size.y
;
;	close_file, 1
;	on_ioerror, bad_open
;	openw,1, path + file
;	on_ioerror, bad_write
;
;;	absx, absy are pixel coords in full scan
;;	These * cx,cy are the scan coords (mm).
;
;	for j=0L,(*pstate).n-1 do begin
;		pj = p[j]
;
;		pm = (*pj).pmark[0]
;		if ptr_valid(pm) eq 0 then goto, bad_region
;
;		r = image_absolute( pimg, crop={x:[min((*pm).x),max((*pm).x)], y:[min((*pm).y),max((*pm).y)]}, error=err)
;		if err eq 0 then begin
;			ox = r.absolute.org.x
;			oy = r.absolute.org.y
;			sx = r.absolute.size.x
;			sy = r.absolute.size.y
;
;			printf,1, sx,ox, sy,oy, format='(F7.3,",",F7.3,",,,",F7.3,",",F7.3)'
;		endif
;	endfor
;
;done:
;	on_ioerror, null
;	close_file, 1
;	return
;	
;bad_open:
;	warning,'OnButton_Image_Table_Export_XFM',['Failed to open file: '+F[0],'Check that file is not open in Excell.']
;	goto, done
;bad_write:
;	warning,'OnButton_Image_Table_Export_XFM',['Bad write to file: '+F[0],'Check that file is not open in Excell.']
;	goto, done
;bad_region:
;	warning,'OnButton_Image_Table_Export_XFM',['Bad region pointer.']
;	goto, done
;end
;
;-----------------------------------------------------------------

pro OnButton_Image_Table_Delete, Event, keep_zero=keep_zero, select=select

; See also the delete case in Image_table event routine.
;
; If select=select is present, then prompt for region #'s to delete and return here as 'select'
; (return original, unsorted, region # index)

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if image_table_invalid(pstate) then goto, done
if n_elements(keep_zero) eq 0 then keep_zero=0

index = lonarr(n_elements( *(*pstate).p))
for j=0,(*pstate).n-1 do index[j] = (*(*(*pstate).p)[j]).index

if arg_present(select) eq 0 then begin
	i1 = (*pstate).sel.top
	i2 = (*pstate).sel.bottom
	if (i1 eq 0) and (i2 eq (*pstate).n-1) then begin
		OnButton_Image_Table_Clear, Event
		goto, done
	endif
	
	(*pstate).n = n_elements( *(*pstate).p)
	i1 = clip( i1, 0, (*pstate).n-1)
	i2 = clip( i2, 0, (*pstate).n-1)
	nq = i2-i1+1
	q = i1 + indgen(nq)
	if keep_zero then begin
		qt = where( index[q] ne 0, nq)
		if nq eq 0 then return
		q = q[qt]
	endif
endif else begin
	sel = index
	start_region = 0
	final_region = (*pstate).n-1
	stop_region = final_region
	n_max = 500
	if (*pstate).n gt n_max then stop_region = n_max-1
	good = 0
loop:	
	seli = sel[start_region:stop_region]
	names = str_tidy(seli)
	s = element_select( event.top, names, title='Select regions to delete',/off)
	qi = where(s eq 1, nqi)
	if nqi gt 0 then begin
		good = 1
		if nqi ge 1 then begin
			if n_elements(q) eq 0 then begin
				i1 = qi[0] > 0
				select = seli[qi]
				q = qi + start_region
			endif else begin
				select = [select, seli[qi]]
				q = [q, qi + start_region]
			endelse
		endif
	endif
	if stop_region lt final_region then begin
		start_region = stop_region + 1
		stop_region = (start_region + n_max-1) < final_region
		goto, loop
	endif

	if keep_zero then begin
		qt = where( select gt 0, nqt)
		if nqt gt 0 then begin
			select = select[qt]
			q = q[qt]
			nq = nqt
		endif else select=-1
	endif else nq=n_elements(q)
endelse
if nq eq 0 then goto, done

for j=nq-1,0,-1 do begin			; index in (sorted) table
	i = q[j]						; region index (unsorted value)

	n_el = (*(*(*pstate).p)[0]).n_el
	if ptr_valid( (*(*pstate).p)[i]) then free_table_entry, pstate, i
	(*pstate).file_valid = 0
	
	if i eq 0 then begin
		if (*pstate).n eq 1 then begin
			*(*pstate).p = !null
			sel = 0
		endif else begin
			*(*pstate).p = (*(*pstate).p)[1:*]
			sel = 1
			widget_control, (*pstate).table, /delete_rows, use_table_select=[0,i,1,i]
		endelse
	endif else if i eq (*pstate).n-1 then begin
		*(*pstate).p = (*(*pstate).p)[0:(*pstate).n-2]
		sel = 0
		widget_control, (*pstate).table, /delete_rows, use_table_select=[0,i,1,i]
		set_table_select, (*pstate).table, i-1,i-1, rows=(*pstate).n-1, columns=n_el
	endif else begin
		*(*pstate).p = [(*(*pstate).p)[0:i-1],(*(*pstate).p)[i+1:*]]
		sel = 1
		widget_control, (*pstate).table, /delete_rows, use_table_select=[0,i,1,i]
	endelse
	(*pstate).sel.bottom = (*pstate).sel.bottom < ((*pstate).n-1) 
	(*pstate).sel.top = (*pstate).sel.top < (*pstate).sel.bottom
	(*pstate).n = (*pstate).n - 1
endfor
;if (*pstate).n ge 1 then widget_control, (*pstate).table, row_labels=string(indgen((*pstate).n))

new_table_pselect, pstate, (i1 < ((*pstate).n - 1))
notify, 'image-region-clear', from=event.top
if keep_zero eq 0 then begin
	if (*pstate).n ge 1 then notify, 'image-region-select', (*pstate).pselect, from=event.top
endif

done:
end

;-----------------------------------------------------------------

;	Does not work as this loop completes before notify acted upon ...

;pro OnButton_Image_Table_Kill_all, Event
;
;; Kill all pixels for all regions in all planes
;
;COMPILE_OPT STRICTARR
;
;	child = widget_info( event.top, /child)
;	widget_control, child, get_uvalue=pstate
;	if image_table_invalid(pstate) then goto, done
;
;	(*pstate).n = n_elements( *(*pstate).p)
;	
;	for i=0,((*pstate).n)-1 do begin
;		OnCellSelect_Image_Table2, pstate, event.top, i
;	
;		print,'Do region ',i
;;		notify, 'image-kill-regions-all-planes', from=event.top
;	endfor
;	
;done:
;	return
;end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Delete_all, Event, keep_first=keep, clean=clean

; Delete all, or keep the first /keep
; Clean = array of strings to 'strip_file_m' from file-name

COMPILE_OPT STRICTARR
if n_elements(keep) eq 0 then keep=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if image_table_invalid(pstate) then goto, done

(*pstate).n = n_elements( *(*pstate).p)
if (keep eq 0) or ((*pstate).n eq 1) then begin
	OnButton_Image_Table_Clear, Event, clean=clean
	return
endif

i = 0
for j=((*pstate).n)-1,1,-1 do begin
	widget_control, (*pstate).table, /delete_rows, use_table_select=[0,j,1,j]
	if ptr_valid( (*(*pstate).p)[j]) then free_table_entry, pstate, j
endfor
*(*pstate).p = (*(*pstate).p)[0]

n_el = (*(*(*pstate).p)[0]).n_el
(*pstate).sel.top = i
(*pstate).sel.bottom = i
set_table_select, (*pstate).table, i,i, rows=(*pstate).n, columns=n_el

(*pstate).n = 1
widget_control, (*pstate).table, row_labels=string(indgen((*pstate).n))

if n_elements(clean) ne 0 then begin
	for j=0,n_elements(clean)-1 do begin
		file = strip_file_m(strip_file_ext((*pstate).file),ending='-q1')
		if strlen(clean[j]) ne 0 then begin
			file = strip_file_m(file, ending=clean[j])
		endif
		file = file + '-q1.region'
	endfor
	(*pstate).file = file
endif

new_table_pselect, pstate, i						; this sets pselect
notify, 'image-region-clear', from=event.top
if (*pstate).n ge 1 then notify, 'image-region-select', (*pstate).pselect, from=event.top

*(*pstate).pregions = 0L							; regions do not match spectra anymore
notify, 'image-regions', (*pstate).pregions, from=event.top

done:
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Clear, Event, clean=clean

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(clean) ne 0 then begin
	for j=0,n_elements(clean)-1 do begin
		(*pstate).file = strip_file_m(strip_file_ext((*pstate).file),ending='-q1')
		if strlen(clean[j]) ne 0 then begin
			(*pstate).file = strip_file_m((*pstate).file, ending=clean[j])
			(*pstate).file = (*pstate).file + '-' + clean[j]
		endif
		(*pstate).file = (*pstate).file + '-q1.region'
	endfor
endif

free_image_table_all, pstate
clear_image_table_table, pstate
(*pstate).file_valid = 0
notify, 'image-results', from=event.top

*(*pstate).pregions = 0L							; regions do not match spectra anymore
notify, 'image-regions', (*pstate).pregions, from=event.top
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_EVT, Event

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
		warning,'OnButton_image_table_EVT',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
	
if (*pstate).file_valid eq 0 then begin
	OnButton_Image_Table_Save, Event
endif

Image_Table_EVT, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Hotspot_Update, Event, stub=stub

; Update region #0 to include pixel 'q' from other regions (e.g. after deleting some hot-spots)

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
		warning,'OnButton_Image_Table_Hotspot_Update',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
		
	if image_table_invalid(pstate) then goto, done
	(*pstate).n = n_elements( *(*pstate).p)
	n = (*pstate).n
	if n le 1 then goto, done

	index = lonarr(n_elements( *(*pstate).p))
	for j=0,n-1 do index[j] = (*(*(*pstate).p)[j]).index
	q0 = where( index eq 0, nq0)
	if nq0 eq 0 then goto, done					; no region #0

	p = (*(*pstate).p)[q0[0]]
	if (*p).mode ne 1 then begin
		warning,'OnButton_Image_Table_Hotspot_Update','No "highlighted" pixels found in region #0.'
		goto, done
	endif

	mask = intarr( (*p).nx, (*p).ny)

	for j=0,n-1 do begin
		if j eq q0[0] then continue
		pj = (*(*pstate).p)[j]
		mask[ *(*pj).q] = 1
	endfor
	qm = where( mask eq 1, nm)

	r = float(nm) / n_elements(*(*p).q)
	*(*p).q = where( mask eq 1, nm)
	print, 'OnButton_Image_Table_Hotspot_Update', 'pixels #0 = ',nm

	(*p).charge *= r
	(*p).ic_total *= r
	if (*(*p).el)[(*p).n_el-1] eq 'Flux' then (*(*p).conc)[(*p).n_el-1] *= r			; also fix flux

;	Also need to update conc table, some how?
;	In the meantime, spectrum overlays will be wrong for #0.
;	'Flux' in conc table is still wrong!

;	new_table_pselect, pstate, 0
;	notify, 'image-region-clear', from=event.top
;	notify, 'image-region-update', from=event.top
;	(*pstate).file_valid = 0
;	(*pstate).file = strip_file_keys( file, remove=remove, element=1, add=add)

done:
	return
end

;------------------------------------------------------------------------------------

pro Image_Table_EVT, pstate, gencom=gencom

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
		warning,'Image_Table_EVT',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif
fylut = ''

if n_elements(gencom) eq 0 then gencom=0
if image_table_invalid(pstate) then goto, done
p = *(*pstate).p
(*pstate).n = n_elements( p)
good = intarr((*pstate).n)
if (*pstate).n ge 1 then begin
	for i=0,(*pstate).n-1 do begin
		if ptr_good( (*(*pstate).p)[i]) then good[i] = 1
	endfor
	n = where(good eq 1, nn)
	if (nn eq 0) then begin
		warning,'Image_Table_EVT','No valid region pointers found.'
		return
	endif
	*(*pstate).p = p[n]										; reject bad pointers
	
	image_table_restore_index_order, pstate

	p = (*(*pstate).p)[0]
;	pn = (*(*pstate).p)[n]
	obj = (*p).DevObj
	cluster = (*pstate).cluster

	file = (*p).source
	dpath = *(*pstate).dpath
	paths = [dpath,*(*pstate).path]
	if lenchr(file) lt 1 then begin
		file = strip_path(strip_file_m( strip_file_ext( (*p).file))) + obj->extension()
	endif
	F = file_requester( /read, filter = '*'+ obj->extension(), /skip_if_exists, get_path=gpath, /translate, $	;/debug, $
			title='Select (first) event file to Read', file=file, group=(*pstate).tlb, fix_filter=0, updir=5, path=paths)
	print,'EVT: first blog file = '+F[0]
	if F[0] eq '' then goto, done
	dpath = gpath
	*(*pstate).dpath = dpath

	if obj->multi_files() then begin
		file = (*p).source2
		F3 = file_requester( /read, filter = '*'+ obj->extension(), /skip_if_exists, /skip_if_null, get_path=gpath,  $
			title='Select final event file to Read', file=file, group=(*pstate).tlb, fix_filter=0, path=dpath)
			print,'EVT: last blog file = '+F3[0]
;		if F3[0] eq '' then goto, done
	endif
	by_detector=((*pstate).adc eq -2)
	by_odd = ((*pstate).adc eq -4)
	by_even = ((*pstate).adc eq -3)
	by_detector = by_detector or by_odd or by_even

	if F ne '' then begin
		file = find_file2( (*p).file)
		path = extract_path( (*pstate).file)
		if lenchr(path) eq 0 then path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		file = strip_path(strip_file_ext( (*pstate).file))
		if by_detector then file=file + '-detector'
		file = file + '.spec'
		F2 = file_requester( /write, filter = '*.spec', path=path, $
			title='SPEC file to Write', file=file, group=(*pstate).tlb, /fix_filter)

		if file_write_test(F2, path=path) then begin
	    	if obj->linear() then begin
		       linearize = (*p).linearize
		       tpath = extract_path(linearize)
			   if file_test(linearize, /read) eq 0 then tpath=path
;		       linearize = strip_path(linearize)
		       Ft = file_requester( /read, filter = ['*.linear.var','*.linear'], file=linearize, group=(*pstate).tlb, path=tpath, $
						title='Select Linearization file (or Cancel)', /fix_filter, /skip_if_exists, /translate, updir=3, /skip_if_null)
		       if Ft[0] ne '' then linearize = Ft[0]
		       print,'image_table EVT:, linear =',linearize
			endif
			if obj->pileup() then begin
		       pileup = (*p).pileup
		       tpath = extract_path(pileup)
			   if file_test(pileup, /read) eq 0 then tpath=path
;		       pileup = strip_path(pileup)
		       Ft = file_requester( /read, filter = ['*.pileup.var','*.txt'], file=pileup, group=(*pstate).tlb, path=tpath, $
						title='Select Pileup limits file (or Cancel)', /fix_filter, /skip_if_exists, /translate, updir=3, /skip_if_null)
		       if Ft[0] ne '' then pileup = Ft[0]
		       print,'image_table EVT:, pileup =',pileup
			endif
			if obj->throttle() then begin
		       throttle = (*p).throttle
		       tpath = extract_path(throttle)
			   if file_test(throttle, /read) eq 0 then tpath=path
;		       throttle = strip_path(throttle)
		       Ft = file_requester( /read, filter = ['*.throttle.var','*.txt'], file=throttle, group=(*pstate).tlb, path=tpath, $
						title='Select Throttle file (or Cancel)', /fix_filter, /skip_if_exists, /translate, updir=3, /skip_if_null)
		       if Ft[0] ne '' then throttle = Ft[0]
		       print,'image_table EVT:, throttle =',throttle
			endif

			evt_files0 = select_evt_files( F, F3, obj->multi_files(), obj->multi_char(), obj->extension(), $
								embed_detector=obj->embed_detector())
					
;			Max regions to handle in each pass. Mask array [nqmask,nx,ny] gets large, so spread qmask
;			over muliple passes for large nqmask and large [nx,ny].

			if gencom eq 0 then begin
				gd = Geopixe_defaults( source='Image_table_EVT')
				max_n_mask = clip( long( 0.4*gd.default.memory / ( float((*p).nx) * float((*p).ny))), 10, 10000)
;			max_n_mask = 3
				print,'image_table EVT:, max_n_mask =',max_n_mask
			endif else begin
				max_n_mask = 9000000L
			endelse


			n_mask = n_elements( *(*pstate).p)
			repeat_loop = 0
			start_mask = 0
			final_mask = n_mask-1
			if (n_mask gt max_n_mask) and (by_detector eq 0) and ((*p).xstep_on eq 0) then begin
				repeat_loop = 1
				stop_mask = max_n_mask-1				; repeat extract for more regions (not if /by_detector) ...

				warning,'image_table EVT',['Very large images and long regions list.','','This extraction will be completed in', $
						str_tidy(1 + n_mask/max_n_mask) + ' passes.']
			endif else begin
				stop_mask = n_mask-1
			endelse
loop:
			if obj->ylut() then begin
				obj->change_options, 0					; stop changes to options (e.g. Maia flip.x)

				fylut = extract_path(F2) + strip_path( (*p).file)
				head = get_header_info( obj, F, group=(*pstate).tlb, output=fylut, /silent, error=err)
				if (err eq 0) then begin
					if (n_elements(*head.scan.pYlut) gt 1) then begin
						n1 = n_elements(evt_files0)
						pt = ptr_new( (*(*pstate).p)[start_mask:stop_mask])
						evt_files = obj->trim_evt_files( evt_files0, mask=pt, pYlut=head.scan.pYlut)
						print, 'image_table EVT: evt_files number before/after YLUT filter = ',n1, n_elements(evt_files)
					endif else if (n_elements(*head.scan.pYlut) eq 1) then begin
						cluster = 0
						evt_files = evt_files0
					endif else begin
						print, 'image_table EVT: null YLUT ?'
						if n_elements(evt_files0) gt 1 then begin
							warning, 'image_table EVT',['Y lookup table is missing ("'+fylut+'").','','Check that the YLUT table is located in the Output directory.', $
								'','Proceed without YLUT table.']
						endif
						cluster = 0
						evt_files = evt_files0
;						goto, bad_ylut
					endelse
				endif else begin
					print, 'image_table EVT: error reading YLUT'
					if n_elements(evt_files0) gt 1 then begin
						warning, 'image_table EVT',['Error reading Y lookup table ("'+fylut+'").','','Check that the YLUT table is located in the Output directory.', $
							'','Proceed without YLUT table.']
					endif
					cluster = 0
					evt_files = evt_files0
;					goto, bad_ylut
				endelse
				obj->change_options, 1
			endif else evt_files = evt_files0

			devpars = obj->get_options()
			if n_elements(evt_files) lt 5 then cluster=0
			
			if (*p).xstep_on then begin
				if gencom then begin
					warning,'image_table EVT','Command Files not implemented in Xstep mode.'
					goto, done
				endif
				widget_control, /hourglass
				spec_xstep_evt, evt_files, group=(*pstate).tlb, mask=(*pstate).p, device=obj->name(), devpars=devpars, $
								station=(*pstate).adc, output=F2, spectra=pp, /progress, by_odd=by_odd, by_even=by_even, $
								by_detector=by_detector, detector_select=((*pstate).sel.top>0), $
								pileup=pileup			; , linear=linearize, throttle=throttle
			endif else begin

				if cluster or gencom then begin
					args = ['spec_evt', + $
							'files=' + stringify(evt_files), $
;							'mask=' + stringify((*pstate).p, /embed_ptr), $		; can get too large; pass as 'mfile' instead
							'mfile=' + stringify( (*pstate).file), $
							'start_mask='+ stringify( start_mask), $
							'stop_mask='+ stringify( stop_mask), $
							'linear=' + stringify( linearize), $
							'pileup=' + stringify( pileup), $
							'throttle=' + stringify( throttle), $
							'output=' + stringify(F2), $
							'device=' + stringify(obj->name()), $
							'devpars=' + stringify(devpars), $
							'station=' + stringify((*pstate).adc), $
							'by_detector=' + stringify(by_detector), $
							'detector_select=' + stringify((*pstate).sel.top>0), $
							'by_odd=' + stringify(by_odd), $
							'by_even=' + stringify(by_even) ]
							
					if gencom then begin
						F = file_requester( /write, filter='*.gcf', path=path, group=group, $
							title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
						if F eq '' then return
						args = [args,'cluster='+str_tidy((*pstate).cluster)]
						geopixe_gen_commands, F, args
						return
					endif else begin
						args = strjoin( args, ', ')
					endelse

					cluster_client, title = 'GeoPIXE Cluster Region Spectrum Extract', $
										subtitle = strip_path(F2), $
										args=args, $
										n_files = n_elements(evt_files), $
										group_leader = (*pstate).tlb, $
										presult = presults, $
										error = error
												
					if (error eq 0) and ptr_good(presults) then begin
						cluster_merge_spectra, presults, spectra=pp, error=error
					endif

				endif else begin
					widget_control, /hourglass
					tic
					spec_evt, evt_files, group=(*pstate).tlb, output=F2, device=obj->name(), devpars=devpars, $		; mask=(*pstate).p
								mfile= (*pstate).file, pileup=pileup, linear=linearize, throttle=throttle, $
								start_mask=start_mask, stop_mask=stop_mask, $
								station=(*pstate).adc, spectra=pp, /progress, by_detector=by_detector, $
								detector_select=((*pstate).sel.top>0), by_odd=by_odd, by_even=by_even
				endelse
			endelse
			if ptr_good(pp) eq 0 then goto, done

			if ptr_good(pp0) then begin
				*pp0 = [*pp0, *pp]
			endif else begin
				pp0 = pp
			endelse
			if stop_mask lt final_mask then begin				; repeat extract for more regions ...
				start_mask = stop_mask+1
				stop_mask = (start_mask + max_n_mask-1) < final_mask
				goto, loop
			endif else begin
				pp = pp0
				if repeat_loop or cluster then begin
					if ptr_good(pp) eq 1 then begin
						if var_type( *(*pp)[0]) eq 8 then begin
							write_spec, *pp, (*(*pp)[0]).file	
						endif
					endif
				endif
			endelse

;			if ptr_good( pmaster) then begin
;				free_region_entry, pmaster, 0
;				ptr_free, pmaster
;				file_delete, master_file
;			endif

			notify, 'dpath', (*pstate).dpath, from=(*pstate).tlb

			if n_elements(pp) ne 0 then begin
				if ptr_valid(pp) then begin
					if ptr_valid( (*pp)[0]) then begin
						if ptr_valid( (*pstate).pspec) then begin
							if ptr_valid( (*(*pstate).pspec)[0]) then begin
								if (*(*(*pstate).pspec)[0]).orphan eq 1 then begin
									(*pstate).local = 1
									(*(*(*pstate).pspec)[0]).orphan = 0
								endif
								if ((*pstate).pspec ne pp) and ((*pstate).local eq 1) then free_spectra, (*pstate).pspec
							endif
						endif
						(*pstate).pspec = pp
						(*(*(*pstate).pspec)[0]).file = F2
						(*pstate).local = 0
						(*(*(*pstate).pspec)[0]).orphan = 1
						notify, 'spectra', pp, from=(*pstate).tlb
					endif
				endif
			endif
		endif else begin
			warning, 'image_table EVT','Output file ("'+F2+'") not writable.'
		endelse
	endif
endif

done:
	return

bad_ylut:
	warning, 'image_table EVT',['Y lookup table is missing ("'+fylut+'").','','Check that the YLUT table is located in the Output directory.']
	return
end

;-----------------------------------------------------------------
; Table Cell Image_Table Callback Procedure
;
;   {WIDGET_TABLE_CELL_SEL, ID:0L, TOP:0L, HANDLER:0L, TYPE:4,
;       SEL_LEFT:0L, SEL_TOP:0L, SEL_RIGHT:0L, SEL_BOTTOM:0L}
;
;   The range of cells image_tableed is given by the zero-based indices
;       into the table specified by the SEL_LEFT, SEL_TOP, SEL_RIGHT,
;       and SEL_BOTTOM fields. When cells are deimage_tableed (either by
;       changing the image_tableion or by clicking in the upper left
;       corner of the table) an event is generated in which the
;       SEL_LEFT, SEL_TOP, SEL_RIGHT, and SEL_BOTTOM fields contain
;       the value -1.
;-----------------------------------------------------------------

pro OnCellSelect_Image_Table, Event

COMPILE_OPT STRICTARR
if event.sel_left eq -1 then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then goto, done
p = *(*pstate).p
(*pstate).n = n_elements(p)

(*pstate).sel.top = event.sel_top
(*pstate).sel.bottom = event.sel_bottom
(*pstate).sel.left = event.sel_left
(*pstate).sel.right = event.sel_right
;help, (*pstate).sel

whole_column =  ((*pstate).sel.top eq 0) and ((*pstate).sel.bottom eq ((*pstate).n-1)) and $
				((*pstate).sel.left eq (*pstate).sel.right) and ((*pstate).n gt 1)
if whole_column then begin
	if ((*pstate).n eq 0) then goto, done
	j = (*pstate).sel.left
	if j eq 0 then begin
		image_table_restore_index_order, pstate
	endif else begin
		ic = (*(*pstate).pcol_index)[j]
		conc = fltarr((*pstate).n)
		for k=0,(*pstate).n-1 do begin
			if ptr_good((*p[k]).conc) then conc[k] = (*(*p[k]).conc)[ic]
		endfor
		q = reverse( sort(conc))
		*(*pstate).p = p[q]
		load_image_table_table, pstate
	endelse
	goto, done
endif

whole_line =  ((*pstate).sel.left eq 0) and ((*pstate).sel.right eq ((*pstate).columns-1))
if not whole_line then goto, done

if image_table_invalid(pstate) then goto, done
i = event.sel_top

OnCellSelect_Image_Table2, pstate, event.top, i

; Fudge to test for a XANES spectrum ...
	p = (*(*pstate).p)[i]
	if ptr_good( (*p).el) eq 0 then goto, done
	el = *(*p).el
	if (n_elements(el) lt 3) then goto, done
	if (fnumeric(el[1]) eq 0) or (fnumeric(el[2]) eq 0) then goto, done

; Plot the XANES spectrum
	x = float2( el)
	y = *(*p).conc
	err = *(*p).error
	y1 = y+err
	y2 = y-err
	q = where(x gt 0.1, nq)
	if nq lt 2 then goto, done

	if !version.os_family eq 'unix' then begin
		retain=2
	endif else begin
    	retain=1
	endelse

	catch, ErrorNo
	if ErrorNo ne 0 then begin
		catch, /cancel
		on_error, 1
		window, 4, retain=retain
	endif

	wset, 4
	!p.title = 'Spectrum for Region #'+str_tidy(i)
	!x.title = 'Energy (keV)'
	!y.title = ''
	!p.thick = 1.0
	!p.charsize = 1.2
	!p.charthick = 1.0
	!p.background = 0
	!p.color = spec_colour('white')
	yrange = [min(y[q]), max(y[q])]
	ylog = (*pstate).xanes ? 0 : 1
	if ylog then yrange[0] = 0.5
	plot,x[q],y[q],/nodata, yrange=yrange, ylog=ylog
	oplot,x[q],y[q],color=spec_colour('green')
	for i=0,nq-1 do begin
		plots, [x[q[i]],x[q[i]]], [y1[q[i]],y2[q[i]]], color=spec_colour('green')
	endfor
	oplot,x[q],y[q],color=spec_colour('green'),psym=6
done:
	return
end

;-----------------------------------------------------------------

pro OnCellSelect_Image_Table2, pstate, tlb, i

COMPILE_OPT STRICTARR
if image_table_invalid(pstate) then goto, done

	(*pstate).n = n_elements( *(*pstate).p)
	if (i lt 0) or (i ge (*pstate).n) then goto, done
	p = (*(*pstate).p)[i]
	p0 = (*(*pstate).p)[0]
	
	new_table_pselect, pstate, i
	notify, 'image-region-clear', from=tlb
	notify, 'image-region-select', (*pstate).pselect, from=tlb
	
	set_table_select, (*pstate).table, i,i, rows=(*pstate).n, columns=(*pstate).columns

done:
	return
end

;-----------------------------------------------------------------
; Table Insert String Callback Procedure.
;
;   {WIDGET_TABLE_STR, ID:0L, TOP:0L, HANDLER:0L, TYPE:1, OFFSET:0L,
;       STR:'', X:0L, Y:0L }
;
;   OFFSET is the (zero-based) insertion position that will result
;       after the character is inserted. STR is the string to be
;       inserted. X and Y give the zero-based address of the cell
;       within the table.
;-----------------------------------------------------------------

pro OnChangeValue_Image_Table, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (event.ch eq 13B) or (event.ch eq 10B) then begin
	if (*pstate).cr_found eq 1 then goto, finish
	(*pstate).cr_found = 1
endif else begin
	(*pstate).cr_found = 0
	goto, finish
endelse

image_table_update_cel, pstate, event.x,event.y, event.top

finish:
end

;-----------------------------------------------------------------

pro image_table_update_cel, pstate, x,y, top

; Remember the order assumed here is set in "load_image_table_table"
; any changes there must be reflected here.

if ptr_valid( (*(*pstate).p)[0]) eq 0 then return

widget_control, (*pstate).table, get_value=v, $
		use_table_select=[x,y,x,y]

print,'Image Table: update cel x,y=',x,y,'  with ',v

case x of
	1: begin
		(*(*(*pstate).p)[y]).note = v
		end
	else:
endcase

return
end

;-----------------------------------------------------------------

pro OnKill_Image_Table, Event

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
		warning,'OnKill_image_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, die
	endif
endif

cancel_notify, event.top

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, die
if ptr_valid(pstate) eq 0 then goto, die
if size(*pstate,/tname) ne 'STRUCT' then goto, die

if ptr_valid((*pstate).pselect) then ptr_free, (*pstate).pselect
if ptr_valid((*pstate).path) then ptr_free, (*pstate).path
if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz

if ptr_valid((*pstate).pspec) then begin
	if (*(*(*pstate).pspec)[0]).orphan eq 1 then begin
		(*pstate).local = 1
		(*(*(*pstate).pspec)[0]).orphan = 0
	endif
	if ((*pstate).local) then free_spectra, (*pstate).pspec
endif

die:
	widget_control, event.top, /destroy
	return
	end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:f }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or zero).
;-----------------------------------------------------------------

pro OnNotify_Image_Table, Event

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
		warning,'OnNotify_image_table',['IDL run-time error caught.', '', $
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
		if ptr_valid( event.pointer) then begin
			*(*pstate).path = (*event.pointer)
		endif
		end

	'dpath': begin
		if ptr_valid( event.pointer) then begin
			*(*pstate).dpath = (*event.pointer)
		endif
		end

	'snapshot': begin
		name = 'image_table.snap'
		on_ioerror, finish
		openw, lun, name, /xdr, /get_lun
		geom = widget_info( event.top, /geometry)
		on_ioerror, snap_done
		writeu, lun, geom.xoffset, geom.yoffset, geom.scr_xsize, geom.scr_ysize
snap_done:
		close_file, lun
		on_ioerror, null
		end

	'image-results': begin

		if ptr_good( (*pstate).p) eq 0 then goto, finish
		p = *(*pstate).p
		(*pstate).n = 0
		if image_table_invalid(pstate) then begin
			clear_image_table_table, pstate
			*(*pstate).pregions = 0L							; regions do not match spectra anymore
			notify, 'image-regions', (*pstate).pregions, from=event.top
		endif else begin
			(*pstate).n = n_elements( p)
			obj = (*p[0]).DevObj
			if widget_info((*pstate).adc_combobox, /valid) then begin
				list = [(*pstate).adc_prelist, adc_list_device( obj)]
				widget_control, (*pstate).adc_combobox, set_value=list, $
								set_combobox_select=(*pstate).adc+(*pstate).adc_offset
			endif
			load_image_table_table, pstate
			n_el = (*pstate).columns
			(*pstate).file_valid = 0
			(*pstate).sel.top = (*pstate).n-1
			(*pstate).sel.bottom = (*pstate).n-1
			set_table_select, (*pstate).table, (*pstate).n-1,(*pstate).n-1, rows=(*pstate).n, columns=(*pstate).columns
		endelse
		end

	'image-region-throttle': begin

		if ptr_good( (*pstate).p) eq 0 then goto, finish
		p = *(*pstate).p
		(*pstate).n = 0
		if image_table_invalid(pstate) then begin
			clear_image_table_table, pstate
		endif else begin
			(*pstate).n = n_elements( p)
			obj = (*p[0]).DevObj
			n_el = (*p[0]).n_el
			list = [(*pstate).adc_prelist, adc_list_device( obj)]
			widget_control, (*pstate).adc_combobox, set_value=list, $
							set_combobox_select=(*pstate).adc+(*pstate).adc_offset

			load_image_table_table, pstate
			(*pstate).file_valid = 0
			(*pstate).sel.top = (*pstate).n-1
			(*pstate).sel.bottom = (*pstate).n-1
			set_table_select, (*pstate).table, (*pstate).n-1,(*pstate).n-1, rows=(*pstate).n, columns=(*pstate).columns

			new_table_pselect, pstate, (*pstate).n-1
			notify, 'image-spectrum-throttle', (*pstate).pselect, from=event.top
		endelse
		end

;	'image-update-results': begin
;
;		warning,'image_table_event',['should not be getting a notify:', $
;						'image-update-results']
;		end

	'image-results-clear': begin

;		Table clear already done in 'image' in 'free_image_regions' in 'set_image_view'.
;		But here need to clear actual table.

		clear_image_table_table, pstate
		(*pstate).file_valid = 0
		end

	'wizard-action': begin
		if ptr_valid( event.pointer) then begin
			if (*event.pointer).window eq 'Image Regions' then begin
				case (*event.pointer).command of
					'open-test': begin
;						print,'*** Wizard Image Regions: test if window is open ...'
						pw = (*pstate).pwiz
						*pw = *event.pointer
						(*pw).top = event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'extract-individual': begin
						print,'*** Wizard Image Regions: extract individual ...'
						pw = event.pointer
						(*pstate).adc = -2
						widget_control, (*pstate).adc_combobox, set_combobox_select=(*pstate).adc+(*pstate).adc_offset

						OnButton_Image_Table_EVT, Event
						file = ''
						err = 1
						if ptr_good( (*pstate).pspec) then begin
							file = (*(*(*pstate).pspec)[0]).file
							err = 0
						endif
						(*pw).error = err
						*(*pw).pdata = file
						notify, 'wizard-return', pw
						end
					else: begin
						warning,'image_table: Notify',['Unknown wizard command: '+(*event.pointer).command, $
								'Make sure GeoPIXE version is compatible with Wizard.']
					endelse
				endcase
			endif
		endif
		end

	else: begin
		print,'OnNotify_Image_Table: unknown tag = ',event.tag
		end
endcase

finish:
	close_file, lun
	return
end

;-----------------------------------------------------------------

pro OnRealize_Image_Table_Mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).mode_id = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Image_Table_DMode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Dmode_combobox = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Image_Table_MMode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Mmode_combobox = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Image_Table_ADC, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate
if ptr_good( (*pstate).p) eq 0 then return
p = *(*pstate).p

(*pstate).adc_combobox = wWidget

if image_table_invalid(pstate) eq 0 then begin
	obj = (*p[0]).DevObj
	list = [(*pstate).adc_prelist, adc_list_device( obj)]
	widget_control, (*pstate).adc_combobox, set_value=list
endif
widget_control, (*pstate).adc_combobox, set_combobox_select=(*pstate).adc+(*pstate).adc_offset
end

;-----------------------------------------------------------------

pro OnRealize_Image_Table, wWidget

COMPILE_OPT STRICTARR
top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

w = widget_info( wWidget, /row_heights)
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

(*pstate).table = wWidget
(*pstate).row_height = w[0]
(*pstate).rows = 6
(*pstate).yoffset = tlb_geom.ysize - geom.scr_ysize + 2
(*pstate).xoffset = tlb_geom.xsize - geom.scr_xsize

(*pstate).n = 0
if image_table_invalid(pstate) eq 0 then begin
	(*pstate).n = n_elements( *(*pstate).p)

	load_image_table_table, pstate
	(*pstate).sel.top = (*pstate).n-1
	(*pstate).sel.bottom = (*pstate).n-1
	set_table_select, (*pstate).table, (*pstate).n-1,(*pstate).n-1, rows=(*pstate).n, columns=(*pstate).columns
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

pro OnSelect_Image_Table_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).display_mode = event.index

if ptr_valid((*pstate).p) then begin
	if ptr_valid((*(*pstate).p)[0]) then begin
		load_image_table_table, pstate

		n_el = (*(*(*pstate).p)[0]).n_el
;		(*pstate).sel.top = (*pstate).n-1
;		(*pstate).sel.bottom = (*pstate).n-1
		set_table_select, (*pstate).table, (*pstate).sel.top,(*pstate).sel.top, rows=(*pstate).n, columns=n_el
	endif
endif
end

;-----------------------------------------------------------------

pro OnSelect_Image_Table_Delete_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).delete_mode = event.index

widget_control, (*pstate).delete_map_base, map=(event.index ne 2)
end

;-----------------------------------------------------------------

pro OnSelect_Image_Table_Modify_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).modify_mode = event.index
end

;-----------------------------------------------------------------

pro OnSelect_Image_Table_ADC_Mode, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).adc = event.index - (*pstate).adc_offset
end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_Image_Table, event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

x = (event.x - (*pstate).xoffset) > 544
y = (event.y - (*pstate).yoffset +2) > 54

; Use integer arithmetic for y to ensure a whole number of rows
;n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
;y = (n + 2) * (*pstate).row_height

widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
widget_control, (*pstate).help, scr_xsize=x
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_Image_Table, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if event.enter eq 1 then begin
	widget_control, event.id, get_uvalue=message
	if n_elements(message) lt 1 then message=''
	if size(message,/tname) ne 'STRING' then message=''
    widget_control, (*pstate).help, set_value=message
endif
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;
pro image_table_eventcb
end
