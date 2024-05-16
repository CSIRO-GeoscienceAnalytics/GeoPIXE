
;	Fit Results table.

pro fit_results_event, event

COMPILE_OPT STRICTARR

common c_working_dir, geopixe_root

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
		warning,'fit_results_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
  widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_results = 0
if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).presults
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
	no_results = 1
endif else begin
	if ptr_valid( (*p)[0] ) eq 0 then no_results=1
	if no_results eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_results=1
endelse

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit results: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'new-results': begin
;				print,'Fit_Results_Event: new results notified.'
				update_fit_results_table, pstate, event
				goto, finish
				end
			'delete-results': begin
;				print,'Fit_Results_Event: delete results notified.'
				fit_results_delete, p, (*event.pointer)[0], (*event.pointer)[1]
				update_fit_results_table, pstate, event
				goto, finish
				end
			else:
		endcase
		end
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				widget_control, (*pstate).help, set_value=' '
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request fit_results ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'results-table': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
				*(*pstate).pselect = (*pstate).sel

				if (*pstate).sel.top ge 0 then begin
					notify, 'results-select', (*pstate).pselect, from=event.top
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end

	'fit_results_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = (event.x - (*pstate).xoffset) > 525
				n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
				y = (n + 2) * (*pstate).row_height
				widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
				end
			else:
		endcase
		end

	'load-button': begin
		path = *(*pstate).path
		file = ''
		if ptr_valid(p) then begin
			if n_elements(*p) ge 1 then begin
				if ptr_valid( (*p)[0]) then begin
					if size( *(*p)[0], /tname) eq 'STRUCT' then begin
						file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.pfr'
					endif
				endif
			endif
		endif
		F = file_requester( /read, filter = '*.pfr', $
			path=path, group=event.top, file=file, $
			title='Select the PIXE Fit Results PFR file', /fix_filter)
		if F ne '' then begin
;			F = strip_file_ext(F) + '.pfr'
			*(*pstate).path = extract_path(F)
			print,'Load fit results from ',F
			load_fit_results, pstate, F

			update_fit_results_table, pstate, event
			(*pstate).sel.top = -1
			(*pstate).sel.bottom = -1
			*(*pstate).pselect = (*pstate).sel
			notify, 'results-select', (*pstate).pselect, from=event.top
		endif
		end

	'save-button': begin
		if no_results then goto, finish
		path = *(*pstate).path
		file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.pfr'
		F = file_requester( /write, filter = '*.pfr', $
			path=path, file=file, group=event.top, $
			title='Save the fit results to a PFR file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.pfr'
			*(*pstate).path = extract_path(F)
			print,'save fit results to ',F
			save_fit_results, pstate, F
		endif
		end

	'table-mode': begin
		(*pstate).mode = event.index
		widget_control,(*pstate).conc_mode_base,map=(*pstate).table_ppm[(*pstate).mode]
		update_fit_results_table, pstate, event
		end

	'conc-mode': begin
		(*pstate).conc_mode = event.index
		update_fit_results_table, pstate, event
		end

	'export-button': begin
		if no_results then goto, finish
		path = *(*pstate).path
		file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.csv'
		F = file_requester( /write, filter = '*.csv', $
			path=path, file=file, group=event.top, $
			title='Export the displayed values to an ASCII file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.csv'
			*(*pstate).path = extract_path(F)
			export_results_table, pstate, F
		endif
		end

	'plot-button': begin
		end

	'properties-button': begin
		if no_results then goto, finish
		print,'properties ...'
			result_properties, group_leader=event.top, TLB=tlb, path=*(*pstate).path, $
					presults=(*pstate).presults, select=[(*pstate).sel.top,(*pstate).sel.bottom], $
					layer_pars=(*pstate).player

			register_notify, event.top, $
								['path', $					; new path
								'delete-results', $			; delete fit rows
								'new-results' $				; new results
								], from=tlb
		end

	'veto-button': begin
		sel = widget_info((*pstate).table, /table_select)
;		print,sel
		if (sel[0] ne 0) and (sel[3] ge sel[1]) and (sel[2] ge sel[0]) then begin
			for i=sel[1],sel[3] do begin
				(*(*p)[i]).veto[sel[0]-1:sel[2]-1] = clip(1 - (*(*p)[i]).veto[sel[0]-1:sel[2]-1], 0,1)
;				print, 'i= ',i,sel[0]-1,sel[2]-1, '  veto= ',(*(*p)[i]).veto[sel[0]-1:sel[2]-1]
			endfor
		endif
		update_fit_results_table, pstate, event
		end

	'delete-button': begin
		if no_results then goto, finish
		fit_results_delete, p, (*pstate).sel.top, (*pstate).sel.bottom, nshow=ns
		update_fit_results_table, pstate, event, ns
		(*pstate).sel.bottom = (*pstate).sel.top
		*(*pstate).pselect = (*pstate).sel
		notify, 'results-select', (*pstate).pselect, from=event.top
		end

	'clear-button': begin
		if no_results then goto, finish
		for i=0L,n_elements(*p)-1 do begin
			if ptr_valid( (*p)[i]) then ptr_free, (*p)[i]
		endfor
		*p = ptr_new()
		update_fit_results_table, pstate, event
		(*pstate).sel.top = -1
		(*pstate).sel.bottom = -1
		*(*pstate).pselect = (*pstate).sel
		notify, 'results-select', (*pstate).pselect, from=event.top
		end

	'close-button': begin
		print,'Close fit results ...'
		goto, kill
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	close, 1
	return

bad_state:
	warning,'fit_results_event',['STATE variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill
bad_ptr:
	warning,'fit_results_event',['Parameter structure variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-----------------------------------------------------------------

pro fit_results_delete, p, top, bottom, nshow=ns

	np = n_elements(*p)
	if (top ge 0) and (bottom lt np) then begin
		for i=top,bottom do begin
			if ptr_valid( (*p)[i]) then ptr_free, (*p)[i]
		endfor
		ns = -1
		if top eq 0 then begin
			if bottom eq np-1 then begin
				*p = ptr_new()
			endif else begin
				*p = (*p)[bottom+1:np-1]
				ns = 0
			endelse
		endif else begin
			t = (*p)[0:top-1]
			if bottom lt np-1 then begin
				t = [t,(*p)[bottom+1:np-1]]
				ns = top
			endif else begin
				ns = top-1
			endelse
			*p = t
		endelse
	endif

	return
end

;-----------------------------------------------------------------

pro export_results_table, pstate, F

	if (*pstate).rows eq 0 then return

	presults = (*pstate).presults
	if ptr_valid(presults) eq 0 then return
	if size(*presults,/tname) ne 'POINTER' then return
	if ptr_valid( (*presults)[0] ) eq 0 then return
	if size(*(*presults)[0],/tname) ne 'STRUCT' then return

	old_mode = (*pstate).mode
	(*pstate).mode = 0
	update_fit_results_table, pstate
	el_names = strcompress( *(*pstate).headings, /remove_all)

	titles = (*pstate).table_titles
	q = where( (*pstate).table_ppm eq 1)
	titles[q] = titles[q] + ' (' + strtrim((*pstate).table_units[(*pstate).conc_mode],2) + ')'
	titles = [titles,'Veto light shallow elements','Veto twice MDL']
	nt = n_elements((*pstate).table_titles)
	ideep = nt
	itwice = nt+1
	initial_mode_on = [0,1,2]
;	initial_mode_on = [0,1,2,ideep]

	select = export_select( (*pstate).table, el_names, titles, initial_mode_on=initial_mode_on, $
					old_select=*(*pstate).pexport, path=*(*pstate).path)
	if select.error then goto, done

	qmode = where( select.mode_enable[0:nt-1] eq 1)
	if qmode[0] eq -1 then goto, done
	qexport = where(select.el_enable eq 1)
	if qexport[0] eq -1 then goto, done
	*(*pstate).pexport = select
	veto_deep = select.mode_enable[ideep]
	veto_twice_mdl = select.mode_enable[itwice]

	on_ioerror, bad_open
	openw,1, F
	on_ioerror, bad_io

	n = (*pstate).rows

	for i=0L,n_elements(qmode)-1 do begin
		if strtrim( (*pstate).table_modes[qmode[i]],2) eq 'Inclusion' then begin
			OK = 0
			for j=0L,n-1 do begin
				p = (*(*pstate).presults)[j]
				if n_elements( (*p).yield.thick) ge 3 then OK=1
			endfor
			if OK eq 0 then goto, next
		endif
		(*pstate).mode = qmode[i]
		update_fit_results_table, pstate, /export, veto_deep=veto_deep, veto_mdl=veto_twice_mdl
		n = (*pstate).rows

		widget_control, (*pstate).table, get_value=t
		t = string(t[0:(*pstate).columns-1,0:n-1])

		q = indgen( n_elements(*(*pstate).headings))
		if (*pstate).table_els[qmode[i]] then q=qexport
		all_els = 1
		te = where( (*pstate).table_els[qmode] ne 1)
		if te[0] ne -1 then all_els=0

		if (n eq 1) and all_els then begin
			if i eq 0 then begin
				s2 = strjoin( (*(*pstate).headings)[q],',')
				printf,1, ' ,' + s2
			endif
			s3 = strjoin( strip_char(t[q,0],','),',')
			printf,1, (*pstate).table_modes[qmode[i]] + ',' + s3
		endif else begin
			printf,1, titles[qmode[i]]

			s2 = strjoin( (*(*pstate).headings)[q],',')
			printf,1, '#,' + s2

			for j=0L,n-1 do begin
				s3 = strjoin( strip_char(t[q,j],','),',')
				printf,1, string(j) + ',' + s3
			endfor
			printf,1,' '
		endelse
next:
	endfor
done:
	close, 1
	(*pstate).mode = old_mode
	update_fit_results_table, pstate
	n = (*pstate).rows
	return

bad_open:
	warning,'export_results_table','error opening file: '+F
	goto, done
bad_io:
	warning,'export_results_table','I/O error writing file: '+F
	goto, done
end

;-----------------------------------------------------------------

pro load_fit_results, pstate, F

; Read the results from 'F'

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
		warning,'load_fit_results',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_params() lt 2 then begin
		print,'load_fit_results: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return

	no_results = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	presults = (*pstate).presults							; pointer to array of pointers to results
	if ptr_valid(presults) eq 0 then goto, bad_ptr
	if size(*presults,/tname) ne 'POINTER' then begin
		no_results = 1
	endif else begin
		if ptr_valid( (*presults)[0] ) eq 0 then no_results=1
		if no_results eq 0 then if size(*(*presults)[0],/tname) ne 'STRUCT' then no_results=1
	endelse

	results = read_fit_results( F, error=error)				; returns an array of pointers to results
	if error then return
	
	if no_results then begin
		*presults = results
		no_results = 0
	endif else begin
		*presults = [ *presults, results]
	endelse

finish:
	return

bad_ptr:
	warning,'load_fit_results','Bad initial results pointer',/error
	return
end

;-----------------------------------------------------------------

pro save_fit_results, pstate, F

; Write the results to 'F'

	if n_params() lt 2 then begin
		print,'save_fit_results: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	no_results = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	presults = (*pstate).presults
	if ptr_valid(presults) eq 0 then goto, bad_ptr
	if size(*presults,/tname) ne 'POINTER' then begin
		no_results = 1
	endif else begin
		if ptr_valid( (*presults)[0] ) eq 0 then no_results=1
		if no_results eq 0 then if size(*(*presults)[0],/tname) ne 'STRUCT' then no_results=1
	endelse

	n = 0
	for i=0L,n_elements(*presults)-1 do begin
		p = (*presults)[i]

		n_els = (*p).n_els							; number of elements
		n_layers = n_elements((*p).yield.thick)		; number of layers

		if (n_els gt 0) and (n_layers gt 0) then begin
			n = n+1
		endif
	endfor
	if n eq 0 then no_results=1
	if no_results then goto, bad_data

	version = -16L									; version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR


	writeu,1, version
	writeu,1, n

	for i=0L,n-1 do begin
		p = (*presults)[i]

		n_els = (*p).n_els							; number of elements
		n_layers = n_elements((*p).yield.thick)		; number of layers
		org = (*p).org
		rorg = (*p).rorg
		if (org lt 1) or (org gt 100) then org = 10

		if (n_els gt 0) and (n_layers gt 0) then begin
			writeu,1, n_els, n_layers, org
			writeu,1, rorg
			writeu,1, (*p).type
			writeu,1, (*p).conc, (*p).error, (*p).mdl, (*p).area, (*p).aerror, (*p).amdl
			writeu,1, (*p).scale, (*p).mode
			writeu,1, (*p).el
			writeu,1, (*p).setup, (*p).fit, (*p).nlinear
			writeu,1, (*p).cuts, (*p).detector, (*p).filter
			writeu,1, (*p).spectrum, (*p).background
			writeu,1, (*p).back_split
			writeu,1, (*p).flux
			writeu,1, (*p).yield
			writeu,1, (*p).inclusion
			writeu,1, (*p).veto
			writeu,1, (*p).tweek
			writeu,1, (*p).counts_per_ppm_uc
			writeu,1, (*p).compton
			writeu,1, (*p).array.on
			if (*p).array.on then begin
				writeu,1, (*p).array.N_det, n_elements((*p).array.active)
				writeu,1, (*p).array.active
				writeu,1, (*p).array.rGamma
				writeu,1, (*p).array.nk
				writeu,1, (*p).array.cIntensity
			endif
			writeu,1, (*p).stim
			writeu,1, (*p).correct
			writeu,1, (*p).deadtime_correction

			writeu,1, (*p).beam.continuum
			use_beam = 0L
			err = 0
			if (*p).beam.continuum ge 1 then use_beam=1L
			if use_beam then begin
				writeu,1, (*p).beam.model
				case (*p).beam.model of
					1: begin
						write_source, (*p).beam, unit=1, error=err
						end
					2: begin
						write_pink, (*p).beam, unit=1, error=err
						end
					else:
				endcase
			endif
			if err then goto, bad_beam
		endif
	endfor

	close,1
	return

bad_io:
	warning,'save_fit_results','Error writing results file',/error
	return
bad_beam:
	warning,'save_fit_results','Error writing results beam struct',/error
	return
bad_ptr:
	warning,'save_fit_results','bad results pointer',/error
	return
bad_data:
	warning,'save_fit_results','bad results data structure',/error
	return
end

;-----------------------------------------------------------------

pro OnRealize_Results_Table, wWidget

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

update_fit_results_table, pstate

done:
end

;------------------------------------------------------------------------------------------

pro update_fit_results_table, pstate, event, nsi, export=export, veto_deep=veto_deep_in, $
							 veto_mdl=veto_mdl_in

COMPILE_OPT STRICTARR

common c_working_dir, geopixe_root

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
		warning,'update_fit_results_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

nc = 1
if n_elements(nsi) lt 1 then nsi=(*pstate).sel.top
ns = nsi
if n_elements(ns) lt 1 then ns=(*pstate).sel.top
if n_elements(export) lt 1 then export=0
if n_elements(veto_deep_in) lt 1 then veto_deep_in=0
if n_elements(veto_mdl_in) lt 1 then veto_mdl_in=0
if export eq 0 then begin
	veto_deep_in = 0
	veto_mdl_in = 0
endif
percent = 1
if export then percent=0				; don't include "%" symbols in export tables

presults = (*pstate).presults
if ptr_valid(presults) eq 0 then goto, bad
if size(*presults,/tname) ne 'POINTER' then goto, bad
if ptr_valid( (*presults)[0] ) eq 0 then goto, bad
if size(*(*presults)[0],/tname) ne 'STRUCT' then goto, bad

back_mode = ['SNIP','Boost','User','Gamma']
back_split_mode = ['Off','Split Back']
weight = ['atomic','wt %']
microns = ['mg/cm^2','microns']
fluid_types = strarr(15)
fluid_types[0:6] = [' Dilute aqueous ',' 5-10%   NaCl ',' 10-20% NaCl ',' 20-30% NaCl ',' 30-40% NaCl ',' 40-50% NaCl ',' >50%    NaCl ']
beam_shapes = ['XY scan','Ellipse','Gaussian',' ',' ',' ',' ',' ']
bubble_options = ['None','Large dilute','Specify size',' ',' ',' ',' ']
types = detector_types()
corrections = [' - none - ','Subtracted uniform beam scatter contributions','Subtracted host matrix contributions']

; build Z, shell element lookup tables

n = n_elements(*presults)
ns = ns < (n-1)
lookup = intarr(4,100)
shell_codes = strarr(4,100)												
shell_codes[1,*] = ''
shell_codes[2,*] = ' L'
shell_codes[3,*] = ' M'
shell_z = bytarr(4,100)
shell_z[1,*] = indgen(100)
shell_z[2,*] = shell_z[1,*]
shell_z[3,*] = shell_z[1,*]

for i=0L,n-1 do begin
	p = (*presults)[i]
	lookup[ (*p).el.shell, (*p).el.z] = 1
endfor
lookup[0:1,0] = 0														; veto sum with shell=0
if ((*pstate).mode ne 4) and ((*pstate).mode ne 16) then begin			; Compton, elastic have shell=1,2
	lookup[*,0] = 0
	lookup[0,*] = 0
endif
q = where( lookup eq 1)
n_els = n_elements(q)
lookup[q] = indgen(n_els)
el_names = element_name(shell_z[q]) + shell_codes[q]
q1 = where(shell_z[q] eq 0)
if q1[0] ne -1 then begin
	q2 = where(shell_codes[q[q1]] eq ' L')
	if q2[0] ne -1 then el_names[q1[q2]]='Compton'
	q2 = where(shell_codes[q[q1]] eq ' M')
	if q2[0] ne -1 then el_names[q1[q2]]='elastic'
endif

; Build phase lookup table

minerals = ''
nc_max = 10
for i=0L,n-1 do begin
	p = (*presults)[i]
	if tag_present('multiphase', *p) then begin
		if (*p).multiphase.mode ne 0 then begin
			for j=0,n_elements((*p).multiphase.minerals)-1 do begin
				q = where( (*p).multiphase.minerals[j] eq minerals, nq)
				if nq eq 0 then minerals = [minerals,(*p).multiphase.minerals[j]]
			endfor
		endif
	endif
	nc_max = nc_max > n_elements( (*p).nlinear.A)
endfor
if n_elements(minerals) gt 1 then begin
	minerals=minerals[1:*]
	n_minerals = n_elements(minerals)
endif else begin
	n_minerals = 0
	minerals = 'none'
endelse

conc_mode = (*pstate).conc_mode

; Assume for now that all analyses have same element lists

p = (*presults)[0]

case (*pstate).mode of
	0: begin														; conc
		nc = n_els+2
		columns = ['File','Label',el_names]
		widths = [14,12,replicate(10,n_els)] * !d.x_ch_size
		end
	1: begin														; error
		nc = n_els+2
		columns = ['File','Label',el_names]
		widths = [14,12,replicate(10,n_els)] * !d.x_ch_size
		end
	2: begin														; MDL
		nc = n_els+2
		columns = ['File','Label',el_names]
		widths = [14,12,replicate(10,n_els)] * !d.x_ch_size
		end
	3: begin														; rel error
		nc = n_els+2
		columns = ['File','Label',el_names]
		widths = [14,12,replicate(10,n_els)] * !d.x_ch_size
		end
	4: begin														; raw
		nc = n_els+2
		columns = ['File','Label',el_names]
		widths = [14,12,replicate(10,n_els)] * !d.x_ch_size
		end
	5: begin														; spectrum
		nc = 22
		columns = ['File','Type','Label','Charge','Cal A','error','Cal B','error','Units', $
				'FWHM (Mn)','Units','FWHM w0','error','FWHM w1','error','Tail/Step amp', $
				'Tail/Step length L','Tail length S','Compton Tail','Compton Length', $
				'Compton Shift','Compton Spread']
		widths = replicate(12,nc) * !d.x_ch_size
		widths[0] = 25 * !d.x_ch_size
		widths[1] = 10 * !d.x_ch_size
		widths[8] = 8 * !d.x_ch_size
		widths[10] = 8 * !d.x_ch_size
		end
	6: begin														; background
		nc = 6
		columns = ['Mode','B-Scale','Correction','Split Mode','E mid','B2-Scale']
		widths = replicate(10,nc) * !d.x_ch_size
		widths[2] = 35 * !d.x_ch_size
		widths[3] = 20 * !d.x_ch_size
		end
	7: begin														; fitting
		nc = 6
		columns = ['Phases','# its','Chi','RMS','PU ratio','DT Corr']
		widths = replicate(12,nc) * !d.x_ch_size
		widths[0] = 45 * !d.x_ch_size
		widths[1] = 8 * !d.x_ch_size
		end
	8: begin														; experimental
		nc = 9
		columns = ['Charge','Flux','Conversion','Filter','Detector Name','ADC','Multiplicity','Scale','Cuts']
		widths = replicate(10,nc) * !d.x_ch_size
		widths[2] = 12 * !d.x_ch_size
		widths[3] = 18 * !d.x_ch_size
		widths[4] = 22 * !d.x_ch_size
		widths[5] = 8 * !d.x_ch_size
		widths[6] = 10 * !d.x_ch_size
		widths[8] = 40 * !d.x_ch_size
		end
	9: begin														; yield setup
		nc = 16
		columns = ['Title','File','Z1','A1','E Beam','state','Theta','Phi','Alpha','Beta', $
				'Unknown','Formula','Weight','Thickness','Units','Density']
		widths = replicate(12,nc) * !d.x_ch_size
		widths[0] = 20 * !d.x_ch_size
		widths[1] = 20 * !d.x_ch_size
		widths[2] = 6 * !d.x_ch_size
		widths[3] = 6 * !d.x_ch_size
		widths[5] = 8 * !d.x_ch_size
		widths[10] = 9 * !d.x_ch_size
		widths[11] = 25 * !d.x_ch_size
		widths[12] = 7 * !d.x_ch_size
		widths[14] = 10 * !d.x_ch_size
		end
	10: begin														; Major lines
		nc = n_els
		columns = el_names
		widths = replicate(8,n_els) * !d.x_ch_size
		end
	11: begin														; geometry
		nc = n_els
		columns = el_names
		widths = replicate(8,n_els) * !d.x_ch_size
		end
	12: begin														; inclusions
		nc = 14
		columns = ['Salinity','X','Y','Thickness','Mid-plane','Depth','Beam X','Beam Y','Beam Shape', $
					'Bubble Option','Fluid Density','Fluid Type','Host','Host Density']
		widths = replicate(10,nc) * !d.x_ch_size
		widths[9] = 12 * !d.x_ch_size
		widths[11] = 12 * !d.x_ch_size
		end
	13: begin														; setup
		nc = 3
		columns = ['E low','E high','PCM File']
		widths = replicate(10,nc) * !d.x_ch_size
		widths[2] = 40 * !d.x_ch_size
		end
	14: begin														; veto
		nc = n_els+1
		columns = ['File',el_names]
		widths = [16,replicate(6,n_els)] * !d.x_ch_size
		end
	15: begin														; tweeks
		nc = n_elements( (*p).tweek.lines)
		q = where( (*p).tweek.lines ne -1)
		columns = (*p).tweek.id
		widths = replicate(8,nc) * !d.x_ch_size
		end
	16: begin														; area
		nc = n_els+2
		columns = ['File','Label',el_names]
		widths = [14,12,replicate(10,n_els)] * !d.x_ch_size
		end
	17: begin														; counts per ppm.uC
		nc = n_els+1
		columns = ['File',el_names]
		widths = [14,replicate(10,n_els)] * !d.x_ch_size
		end
	18: begin														; Non-linear parameters A
		nc = nc_max
		columns = 'A' + str_tidy(indgen(nc))
		widths = replicate(12,nc) * !d.x_ch_size
		end
	19: begin														; Non-linear parameter names
		nc = nc_max
		columns = 'A' + str_tidy(indgen(nc)) + ' name'
		widths = replicate(18,nc) * !d.x_ch_size
		end
	20: begin														; Non-linear parameter notes
		nc = nc_max
		columns = 'A' + str_tidy(indgen(nc)) + ' note'
		widths = replicate(24,nc) * !d.x_ch_size
		end
	21: begin														; STIM
		nc = 5
		columns = ['File','OK','E0','Emean','X']
		widths = [20,8,replicate(14,nc-2)] * !d.x_ch_size
		end
	22: begin														; Phase fractions
		nc = n_minerals+4
		columns = ['File','Label','Loops',minerals]
		widths = [14,12,7,replicate(7,n_minerals+1)] * !d.x_ch_size
		end

	else: goto, done
endcase
t = strarr(nc,n)

for i=0L,n-1 do begin
  p = (*presults)[i]
  iramp = indgen(nc)
  if ((*pstate).mode ne 4) and ((*pstate).mode ne 16) then begin
	  qe = where( ((*p).el.z gt 0) and ((*p).el.shell gt 0) and (iramp lt n_elements((*p).conc)))
  endif else begin
	  qe = where( ((*p).el.shell gt 0) and (iramp lt n_elements((*p).conc)))
  endelse
  n_ok = n_elements(qe)
  if ptr_valid(p) and (n_ok gt 0) then begin
    veto_deep = veto_deep_in AND (n_elements((*p).yield.formula) ge 2)
    veto_mdl = veto_mdl_in

	if veto_deep then begin
		layer1 = make_layer( (*p).yield.formula[0], (*p).yield.thick[0], microns=(*p).yield.microns[0], density=(*p).yield.density[0])
		e = e_line( (*p).el.z[qe], major_line( (*p).el.z[qe], (*p).el.shell[qe]))
		attenZ = transmit( layer1, e)
		lsi = make_layer( 'SiO2', 20.0, /microns, density=2.6)				; cf. Cl through 20 microns of quartz
		asi = transmit( lsi, e_line(17,major_line(17,1)) )
		qveto = where( attenZ lt asi)
	endif
;	file = strip_path((*p).spectrum.file)
	file = strip_file_m(strip_file_ext(strip_path((*p).spectrum.file)),ending=['-q1','-q2','-q3','-q4','-m'])

	case (*pstate).mode of
		0: begin														; conc
			t[0,i] = file
			t[1,i] = (*p).spectrum.label
			for j=0L,n_ok-1 do begin
				u = ((*p).yield.unknown-1) > 0
				case conc_mode of
					1: begin							; mmol/l
						z = (*p).el.z[qe[j]]
						rho = (*p).yield.density[u]
						po = 0
						s = (z gt 0) ? rho / mass(z) : 0.0
						end
					2: begin							; ng/cm2
						microns = (*p).yield.microns[u]
						x = (*p).yield.thick[u]
						if microns then begin
							rho = (*p).yield.density[u]
							s = rho*x / 10.
						endif else begin
							s = x
						endelse
						po = 0
						end
					else: begin
						po = percent
						s = 1.							; ppm (wt)
						end
				endcase
				t[2+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
						build_result( s*(*p).conc[qe[j]], s*(*p).error[qe[j]], s*(*p).mdl[qe[j]], $
											veto=(*p).veto[qe[j]], twice_mdl=veto_mdl, percent=po, export=export)
			endfor
			if veto_deep then begin
				if qveto[0] ne -1 then t[1+qe[qveto],i] = 'deep'
			endif
			end
		1: begin														; error
			t[0,i] = file
			t[1,i] = (*p).spectrum.label
			for j=0L,n_ok-1 do begin
				u = ((*p).yield.unknown-1) > 0
				case conc_mode of
					1: begin							; mmol/l
						z = (*p).el.z[qe[j]]
						rho = (*p).yield.density[u]
						po = 0
						s = (z gt 0) ? rho / mass(z) : 0.0
						end
					2: begin							; ng/cm2
						microns = (*p).yield.microns[u]
						x = (*p).yield.thick[u]
						if microns then begin
							rho = (*p).yield.density[u]
							s = rho*x / 10.
						endif else begin
							s = x
						endelse
						po = 0
						end
					else: begin
						po = percent
						s = 1.							; ppm (wt)
						end
				endcase
				t[2+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
						build_result( s*(*p).error[qe[j]], s*(*p).error[qe[j]], 0.0, percent=po, export=export)
			endfor
			if veto_deep then begin
				if qveto[0] ne -1 then t[1+qe[qveto],i] = 'deep'
			endif
			end
		2: begin														; MDL
			t[0,i] = file
			t[1,i] = (*p).spectrum.label
			for j=0L,n_ok-1 do begin
				u = ((*p).yield.unknown-1) > 0
				case conc_mode of
					1: begin							; mmol/l
						z = (*p).el.z[qe[j]]
						rho = (*p).yield.density[u]
						po = 0
						s = (z gt 0) ? rho / mass(z) : 0.0
						end
					2: begin							; ng/cm2
						microns = (*p).yield.microns[u]
						x = (*p).yield.thick[u]
						if microns then begin
							rho = (*p).yield.density[u]
							s = rho*x / 10.
						endif else begin
							s = x
						endelse
						po = 0
						end
					else: begin
						po = percent
						s = 1.							; ppm (wt)
						end
				endcase
				t[2+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
						build_result( s*(*p).mdl[qe[j]], s*(*p).mdl[qe[j]]/10., 0.0, percent=po, export=export)
			endfor
			if veto_deep then begin
				if qveto[0] ne -1 then t[1+qe[qveto],i] = 'deep'
			endif
			end
		3: begin														; rel error
			t[0,i] = file
			t[1,i] = (*p).spectrum.label
			for j=0L,n_ok-1 do begin
				t[2+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
						build_result( (*p).error[qe[j]]/abs((*p).conc[qe[j]]>1.0), 0.001, 0.0, high=0.9, export=export)
			endfor
			end
		4: begin														; raw
			t[0,i] = (*p).spectrum.file
			t[1,i] = (*p).spectrum.label
			for j=0L,n_ok-1 do begin
				u = ((*p).yield.unknown-1) > 0
				case conc_mode of
					1: begin							; mmol/l
						z = (*p).el.z[qe[j]]
						rho = (*p).yield.density[u]
						po = 0
						s = (z gt 0) ? rho / mass(z) : 0.0
						end
					2: begin							; ng/cm2
						microns = (*p).yield.microns[u]
						x = (*p).yield.thick[u]
						if microns then begin
							rho = (*p).yield.density[u]
							s = rho*x / 10.
						endif else begin
							s = x
						endelse
						po = 0
						end
					else: begin
						po = percent
						s = 1.							; ppm (wt)
						end
				endcase
				t[2+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
;***						build_result( s*(*p).conc[qe[j]], 1.0 > abs(0.01*s*(*p).conc[qe[j]]), 0.0, percent=po, export=export)
						build_result( s*(*p).conc[qe[j]], abs(0.01*s*(*p).conc[qe[j]]), 0.0, percent=po, export=export)
			endfor
			end
		5: begin														; spectrum
			FWHM_Mn = 1000.*sqrt(abs( (*p).spectrum.FWHM.w1 * 5.898 + (*p).spectrum.FWHM.w0 ))

			t[0,i] = (*p).spectrum.file
			t[1,i] = types[(*p).type]
			t[2,i] = (*p).spectrum.label
			t[3,i] = string((*p).spectrum.charge)
			t[4,i] = string((*p).spectrum.cal.a)
			t[5,i] = string((*p).spectrum.cal.da)
			t[6,i] = string((*p).spectrum.cal.b)
			t[7,i] = string((*p).spectrum.cal.db)
			t[8,i] = (*p).spectrum.cal.units
			t[9,i] = FWHM_mn
			t[10,i] = 'eV'
			t[11,i] = string((*p).spectrum.FWHM.w0)
			t[12,i] = string((*p).spectrum.FWHM.dw0)
			t[13,i] = string((*p).spectrum.FWHM.w1)
			t[14,i] = string((*p).spectrum.FWHM.dw1)
			t[15,i] = string((*p).spectrum.Tail.amp)
			t[16,i] = string((*p).spectrum.Tail.L)
			t[17,i] = string((*p).spectrum.Tail.S)
			t[18,i] = string((*p).Compton.Tail.Amp)
			t[19,i] = string((*p).Compton.Tail.Len)
			t[20,i] = string((*p).Compton.Shift)
			t[21,i] = string((*p).Compton.Spread)
			end
		6: begin														; background
			if (*p).type eq 1 then begin
				t[0,i] = back_mode[3]
				t[1,i] = ' '
			endif else begin
				t[0,i] = back_mode[ (*p).background.mode]
				t[1,i] = string((*p).background.scale)
			endelse
			t[2,i] = corrections[ (*p).correct]
			t[3,i] = back_split_mode[(*p).back_split.mode]
			if (*p).back_split.mode eq 1 then begin
				t[4,i] = string((*p).back_split.emid)
				t[5,i] = string((*p).back_split.scale)
			endif else begin
				t[4,i] = ' '
				t[5,i] = ' '
			endelse
			end
		7: begin														; fitting
			t[0,i] = (*p).fit.phases
			t[1,i] = string( (*p).fit.n_its)
			t[2,i] = string( (*p).fit.chi)
			t[3,i] = string( (*p).fit.rms)
			t[4,i] = string( (*p).nlinear.pileup)
			t[5,i] = string( (*p).deadtime_correction)
			end
		8: begin														; experimental
			if (*p).array.on then begin
				m = n_elements((*p).array.active)
				first = min( (*p).array.active)
				last = max( (*p).array.active)
				sadc = str_tidy(first)										; + 1+adc_offset_device((*p).DevObj)),2)
				if last ne first then sadc=sadc + '...' + str_tidy(last)	; + 1+adc_offset_device((*p).DevObj)),2)
			endif else begin
				a = (*p).array.active[0]									; + adc_offset_device((*p).DevObj)
				sadc = str_tidy(a)
				m = (*p).spectrum.multiplicity
			endelse
			t[0,i] = string( (*p).spectrum.charge)
			t[1,i] = string( (*p).flux)
			t[2,i] = string( (*p).spectrum.charge/(*p).flux)
			t[3,i] = (*p).filter.name
			t[4,i] = (*p).detector.name
			t[5,i] = sadc
			t[6,i] = string( m)
			t[7,i] = string( (*p).scale)
			t[8,i] = (*p).cuts.file
			end
		9: begin														; PIXE yields
			t[0,i] = (*p).yield.title
			t[1,i] = (*p).yield.file
			t[2,i] = string((*p).yield.z1)
			t[3,i] = string((*p).yield.a1)
			t[4,i] = string((*p).yield.e_beam)
			t[5,i] = string((*p).yield.state)
			t[6,i] = string((*p).yield.theta)
			t[7,i] = string((*p).yield.phi)
			t[8,i] = string((*p).yield.alpha)
			t[9,i] = string((*p).yield.beta)
			t[10,i] = string((*p).yield.unknown)
			t[11,i] = (*p).yield.formula[((*p).yield.unknown-1)>0]			; only unknown layer for now
			t[12,i] = weight[(*p).yield.weight[((*p).yield.unknown-1)>0]]
			t[13,i] = string((*p).yield.thick[((*p).yield.unknown-1)>0])
			t[14,i] = microns[(*p).yield.microns[((*p).yield.unknown-1)>0]]
			t[15,i] = string((*p).yield.density[((*p).yield.unknown-1)>0])
			end
		10: begin														; Major lines
			for j=0L,n_ok-1 do begin
				t[lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] = (*p).el.line[qe[j]]
			endfor
			end
		11: begin														; geometry norm factors
			for j=0L,n_ok-1 do begin
				t[lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
						build_result( (*p).inclusion.norm[qe[j]], 0.01, 0.0)
			endfor
			end
		12: begin														; inclusions
			if n_elements( (*p).yield.thick) ge 3 then begin
				t[0,i] = string(salinity(p))
				t[1,i] = string((*p).inclusion.x)
				t[2,i] = string((*p).inclusion.y)
				t[3,i] = string((*p).yield.thick[1])
				t[4,i] = string((*p).inclusion.m)
				t[5,i] = string((*p).yield.thick[0])
				t[6,i] = string((*p).inclusion.beam.x)
				t[7,i] = string((*p).inclusion.beam.y)
				t[8,i] = beam_shapes[(*p).inclusion.beam.shape]
				t[9,i] = bubble_options[(*p).inclusion.option]
				t[10,i] = string((*p).inclusion.density)
				t[11,i] = fluid_types[(*p).inclusion.type]
				t[12,i] = (*p).yield.formula[0]
				t[13,i] = string((*p).yield.density[0])
			endif
			end
		13: begin														; setup
			t[0,i] = string((*p).setup.elow)
			t[1,i] = string((*p).setup.ehigh)
			t[2,i] = (*p).setup.pcm
			end
		14: begin														; veto
			t[0,i] = file
			for j=0L,n_ok-1 do begin
				t[1+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] = string((*p).veto[qe[j]])
			endfor
			end
		15: begin														; tweeks
			q = where( (*p).tweek.lines ne -1)
			if q[0] ne -1 then t[q,i] = (*p).tweek.a[ (*p).tweek.lines[q]]
			end
		16: begin														; area
			t[0,i] = (*p).spectrum.file
			t[1,i] = (*p).spectrum.label
			for j=0L,n_ok-1 do begin
				t[2+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] =   $
						build_result( (*p).area[qe[j]]>0.0, 1.0 > abs(0.01*(*p).area[qe[j]]), 0.0, percent=0, export=export)
			endfor
			end
		17: begin														; counts per ppm.uC
			t[0,i] = (*p).spectrum.file
			for j=0L,n_ok-1 do begin
				t[1+lookup[(*p).el.shell[qe[j]],(*p).el.z[qe[j]]],i] = string( (*p).counts_per_ppm_uc[qe[j]])
			endfor
			end
		18: begin														; non-linear parameters A
			nck = nc_max < n_elements((*p).nlinear.A)
			for j=0L,nck-1 do begin
				t[j,i] = string((*p).nlinear.A[j])
			endfor
			end
		19: begin														; non-linear parameter name
			nck = nc_max < n_elements((*p).nlinear.A)
			for j=0L,nck-1 do begin
				t[j,i] = (*p).nlinear.name[j]
			endfor
			end
		20: begin														; non-linear parameter notes
			nck = nc_max < n_elements((*p).nlinear.A)
			for j=0L,nck-1 do begin
				t[j,i] = (*p).nlinear.note[j]
			endfor
			end
		21: begin														; STIM
			t[0,i] = (*p).spectrum.file
			t[1,i] = (*p).stim.OK
			t[2,i] = (*p).stim.E0
			t[3,i] = (*p).stim.Emean
			t[4,i] = (*p).stim.X
			end
		22: begin														; phase fractions
			t[0,i] = file
			t[1,i] = (*p).spectrum.label
			t[2,i] = 0
			if tag_present('multiphase', *p) then begin
				if (*p).multiphase.mode ne 0 then begin
					t[2,i] = (*p).multiphase.mode + 1
					for j=0,n_elements((*p).multiphase.minerals)-1 do begin
						q = where( (*p).multiphase.minerals[j] eq minerals, nq)
						if nq gt 0 then t[3+q[0],i] =   $
							build_result( (*p).multiphase.phase[j], 0.001, 0.001, percent=0, export=export)
					endfor
				endif
			endif
			end
		else: goto, done
	endcase
  endif
endfor

if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
(*pstate).headings = ptr_new(columns)
(*pstate).columns = nc
(*pstate).rows = n
rows = string( indgen(n))

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, align=2
;			use_table_select=[0,0,nc-1,n-1]
goto, done

bad:
	t = strarr(50,16)
	widget_control, (*pstate).table, set_value = t, $
			row_labels = '', column_labels='', $
			table_xsize=50, table_ysize=16, align=2
	ns = 0
done:
	if (ns ge 0) and (export eq 0) then begin
		widget_control, (*pstate).table, set_table_select=[0,ns,nc-1,ns]
		(*pstate).sel.top = ns
		(*pstate).sel.bottom = ns
		*(*pstate).pselect = (*pstate).sel
		if n_elements(event) gt 0 then notify, 'results-select', (*pstate).pselect, from=event.top
	endif
	return
	end

;------------------------------------------------------------------------------------------

pro fit_results, group_leader=group, TLB=tlb, presults=presults, path=path, $
				layer_pars=player, _extra=extra, xoffset=xoffset, yoffset=yoffset

COMPILE_OPT STRICTARR

common c_working_dir, geopixe_root

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
		warning,'Fit_Results',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''

case !version.os_family of
	'MacOS': begin
		fnt = 'COURIER*BOLD*10'
		yw = 220
		mode_xsize = 90
		end
	'unix': begin
		fnt = '6x10'
		yw = 252
		mode_xsize = 130
		end
	else: begin
		fnt = 'COURIER*10'
		yw = 219
		mode_xsize = 90
		end
endcase

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
endif
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = ((xoff + w - 250) < (screen[0]-34 - 562)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff - yw) < (screen[1]-28 - 200)) > 0
endif

if n_elements(player) lt 1 then player = ptr_new(/allocate_heap)

presults = bad_pars_struct( presults, make_pars=no_results)

; 	top-level base

tracking = 0					; later have context-sensitive help window

tlb = widget_base( /column, title='Fit Results', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='fit_results_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, yoffset=yoffset, $
					/base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

; Table

t = strarr(70,256)

table = Widget_Table( tbase, UNAME='results-table', /all_events, value=t, Notify_Realize='OnRealize_Results_Table',  $
			X_SCROLL_SIZE=7, Y_SCROLL_SIZE=6, /RESIZEABLE_COLUMNS, alignment=2)		; , font=fnt )

; Buttons

table_modes = [' Conc ',' Error ',' MDL ',' Rel error ',' Raw ',' Spectrum ',' Background ', $
			' Fitting ',' Experimental',' Yield setup',' Major lines ',' Geometry ',' Inclusion ',' Setup',' Veto', $
			' Adjust',' Peak Area',' Counts/ppm.uC ', 'A parameters','A names','A notes','STIM','Phases']
table_titles = ['Concentration','Uncertainty','Detection Limits (99% confid.)','Relative Error', $
			'Raw Concentration','Spectrum Details','Background Parameters', $
			'Fitting Statistics','Experimental Set-up','Yield Calculation','Major Lines', $
			'Geometry Corrections','Fluid Inclusion parameters','Fit Setup','Display Veto', $
			'Line Adjustments', 'Peak Area','Counts per ppm.uC','Non-linear Fitting Parameters (A)', $
			'A[] names','A[] notes','STIM','Phase fractions']
table_ppm = [1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
table_els = [1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0]
table_units = [' ppm (wt) ',' mmol / l',' ng / cm2']

bbase = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
table_mode = widget_combobox( bbase, value=table_modes, $
					uname='table-mode', tracking=tracking, xsize=mode_xsize, $
					uvalue='Select content to display in the table above.')
conc_mode_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=0, map=1)
conc_mode = widget_combobox( conc_mode_base, value=table_units, $
					uname='conc-mode', tracking=tracking, $			; xsize=mode_xsize, $
					uvalue='Select the units for concentration in the table above, and in export data.')
lab = widget_label( bbase, value=' ')
button = widget_button( bbase, value='Load', uname='load-button', tracking=tracking, $
					uvalue='Load the table with previous results from a PIXE Fit Results .PFR file.')
button = widget_button( bbase, value='Save', uname='save-button', tracking=tracking, $
					uvalue='Save all the current results displayed in the table to a PIXE Fit Results .PFR file.')
lab = widget_label( bbase, value='  ')
button = widget_button( bbase, value='Properties', uname='properties-button', tracking=tracking, $
					uvalue='Open the Properties window to display or edit various experimental and analytical settings.')
lab = widget_label( bbase, value='  ')
button = widget_button( bbase, value='Veto', uname='veto-button', tracking=tracking, $
					uvalue='Veto the currently selected range of specific element results. Select a range by clicking and dragging a range of elements with the mouse.')
button = widget_button( bbase, value='Export', uname='export-button', tracking=tracking, $
					uvalue='Export selected catagories of results to an ASCII .CSV table. ' + $
					'A pop-up window enables selection of elements and catagories of parameters.')
;button = widget_button( bbase, value='Export All', uname='exportall-button', tracking=tracking, $
;					uvalue='Export ALL categories of results and parameters to a single ASCII .CSV file.')
;lab = widget_label( bbase, value='  ')
;button = widget_button( bbase, value='Plot', uname='plot-button', tracking=tracking, $
;					uvalue='Open the Plot window to display the results in various ways.')
lab = widget_label( bbase, value='  ')
button = widget_button( bbase, value='Delete', uname='delete-button', tracking=tracking, $
					uvalue='Delete the currently selected range of results. Select a range by clicking and dragging a range with the mouse.')
button = widget_button( bbase, value='Clear', uname='clear-button', tracking=tracking, $
					uvalue='Clear the table and delete ALL results in memory.')

;.................................................................................

;help = widget_text( tbase, scr_xsize=380, ysize=3, /wrap, uname='help', tracking=tracking, $
;				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
;				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		presults:		presults, $				; pointer to array of pointers to results
		player:			player, $				; pointer to struct for layer_setup

		pselect:		ptr_new(/allocate_heap), $		; pointer to select for notify to results_properties
		pexport:		ptr_new(/allocate_heap), $		; pointer to export select

		mode:			0, $					; table shows 0:conc, 1:error, 2:mdl, etc.
		conc_mode:		0, $					; concentration mode
		table_mode:		table_mode, $			; ID of table mode droplist
		conc_mode_id:	conc_mode, $			; ID of concentration mode droplist
		conc_mode_base:	conc_mode_base, $		; ID of conc mode base to map
		table_modes:	table_modes, $			; list of mode short strings
		table_titles:	table_titles, $			; list of mode title strings
		table_ppm:		table_ppm, $			; flags values in ppm
		table_els:		table_els, $			; flags columns by element
		table_units:	table_units, $			; table conc units
		tracking:		tracking, $				; is tracking enabled

		table:			table, $				; table ID
		rows:			256, $					; number of rows
		columns:		50, $					; number of colums
		headings:		ptr_new(), $			; pointer to headings array
		row_height:		0, $					; table row height
		xoffset:		0, $					; offset in xsize for resize
		yoffset:		0, $					; offset in ysize for resize
		sel: {left:-1, top:-1, right:-1, bottom:-1 }, $	; use "(*pstate).sel.top" as current region

		cr_found:		0 }						; to fight MAC IDL bug

;		cr_found:		0, $					; to fight MAC IDL bug
;		help:			help $					; ID of help text
;	}

*state.pselect = state.sel

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
			          'image-region-select', $	; new Region row selected --> result-properties
						'new-results' ], $		; new results
						from=group

xmanager, 'fit_results', tlb, /no_block

return
end
