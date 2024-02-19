pro maia_rates_event, event

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
		warning,'maia_rates_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

if (*(*pstate).playout).N eq 384 then begin
	status = {on:0, mask:bytarr(384), text:''}
	member_max = [12,4,20,20,20,1,1]
endif else begin
	status = {on:0, mask:bytarr(96), text:''}
	member_max = [3,1,12,8,10,1,1]
endelse

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		widget_control, event.id, get_uvalue=s0
		if size(s0,/tname) eq 'STRING' then begin
			s = s0
		endif else if size(s0,/tname) eq 'STRUCT' then begin
			s = s0.help
		endif else s=''
		if event.enter eq 1 then begin
			widget_control, (*pstate).help, set_value=s
		endif else begin
			widget_control, (*pstate).help, set_value='Select detector count rate histogram or map on the array. Use cursor on map to identify detectors; click to select spectra; right click to toggle setup enable.'
		endelse
		goto, finish
		end
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit results: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'maia-process': begin					; update process stats histogram
				maia_rates_plot_process, pstate
 				end
			'maia-rates': begin						; update count rate plots and map
				maia_rates_colour, pstate
				maia_rates_update_plot, pstate
				widget_control, (*pstate).detector, set_value={legend:maia_rates_legend( pstate)}
 				end
 			'update-elk': begin						; update ELK map
 				if ptr_good(event.pointer) then begin
					(*pstate).pelk = event.pointer
	 				maia_rates_colour_elk, pstate, (*pstate).pelk
 				endif
				end
			'select-highlight': begin
 				if ptr_good(event.pointer) then begin
					maia_rates_highlight, pstate, *event.pointer
				endif
 				end
			else:
		endcase
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request Maia rates ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'maia-rates-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				case !version.os_family of
					'MacOS': begin
						xoff = 0
						yoff = 0
						end
					'unix': begin
						xoff = 0
						yoff = 0
						end
					else: begin
						xoff = 0
						yoff = 0
						end
				endcase
				widget_control, event.id, scr_xsize=(*pstate).scr_xsize+xoff, scr_ysize=(*pstate).scr_ysize+yoff
				end
			else:
		endcase
		end

	'maia-rates-tab-panel': begin
		maia_rates_colour, pstate
		end

	'select-mode': begin
		(*pstate).mode = event.index
		widget_control, (*pstate).member_base, map=((*pstate).mode lt 5)
		(*pstate).member = (*pstate).member < (member_max[event.index]-1)
		widget_control, (*pstate).member_number, set_combobox_select=(*pstate).member
		maia_rates_update_plot, pstate
		end
		
	'select-member': begin
		(*pstate).member = event.index < (member_max[(*pstate).mode]-1)
		widget_control, (*pstate).member_number, set_combobox_select=(*pstate).member
		maia_rates_update_plot, pstate
		end

	'clear-button': begin
		(*(*pstate).pmrates).detectors[*] = 0
		end
		
	'draw': begin
		end
		
	'detector': begin
		pd = (*pstate).playout
		q = where( event.detector eq (*pd).data.index, nq)
		if nq eq 0 then goto, finish
;		help, event
		
		(*pstate).cursor = event.detector
		s = maia_rates_legend( pstate)
		widget_control, (*pstate).detector, set_value={legend:s}
		s = maia_rates_legend( pstate, /microamp)
		widget_control, (*pstate).detector_elk, set_value={legend:s}
		
		if event.type eq 0 then begin									; clicked on pad
			if event.alt then begin
				*(*pstate).pselect =  { detector:event.detector, $
						index:event.index, row:(*pd).data[q[0]].row, column:(*pd).data[q[0]].column }
				notify, 'detector-toggle', (*pstate).pselect, from=event.top
			endif else begin
				*(*pstate).pselect =  { detector:event.detector, $
						index:event.index, row:(*pd).data[q[0]].row, column:(*pd).data[q[0]].column }
				notify, 'detector-select', (*pstate).pselect, from=event.top
			endelse
		endif
		end
		
	else:
endcase

finish:
	widget_control, hourglass=0
	return

done:
	goto, kill

bad_state:
	warning,'maia_rates_event',['STATE variable has become ill-defined.','Abort Maia setup.'],/error
	goto, kill

kill:

		
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;--------------------------------------------------------------------------

pro maia_rates_colour, pstate

;	Set the mimic display pad colours to bytscl() the values 'vals',
;	which is a vector of 384 values.
;	'd' order is detector number order; index 'n' is pad order from top-left.

COMPILE_OPT STRICTARR
pd = (*pstate).playout
n = (*pd).data.index	
d = (*(*pstate).prates).detectors[n]
i = indgen(n_elements(d))
pm = (*pstate).pmaia

;c = (*pstate).colours[ value_locate( (*pstate).vector, d)]
c = 116B + value_locate( (*pstate).vector, d)

; This should not be needed if Activity records respect ECH
;q = where( *(*pstate).pdisable eq 1, nq)
;if nq ne 0 then c[q]=0

r = (*pm).control.status.event_rate
widget_control, (*pstate).detector, set_value={mode:1, select:i, colour:c}
s1 = rates_string((*pm).control.status.link_rate)
widget_control, (*pstate).link_rate_text, set_value=s1
s = rates_string(r)
widget_control, (*pstate).event_rate_text, set_value=s
s = rates_string((*pm).control.status.discard_rate)
widget_control, (*pstate).discard_rate_text, set_value=s
s = rates_string((*pm).control.status.charge_rate)
widget_control, (*pstate).charge_rate_text, set_value=s
s = rates_string((*pm).control.status.flux1_rate)
widget_control, (*pstate).flux1_rate_text, set_value=s
;s2 = rates_string((*pm).ROI.rate * r)
;widget_control, (*pstate).ROI_rate_text, set_value=s2
;print,'Events='+s1+', ROI='+s2
return
end

;--------------------------------------------------------------------------

pro maia_rates_highlight, pstate, i

;	Set the mimic display highlight for the selected pad.
;	'd' order is detector number order; index 'n' is pad order from top-left.

COMPILE_OPT STRICTARR
pd = (*pstate).playout
n = (*pd).ref[i]	
print,'maia_rates_highlight: detector =',i,', pad =',n
select = indgen(n_elements((*pd).data.index))
highlight = select
highlight[*]=0
highlight[n] = 1

widget_control, (*pstate).detector, set_value={select:select, highlight:highlight}
return
end

;--------------------------------------------------------------------------

pro maia_rates_colour_elk, pstate, pelk

;	Set the mimic display pad colours to bytscl() the ELK values 'vals',
;	which is a vector of 384 values.
;	N.B. Vector_elk is an integer lookup table (mV at present).
;	'pelk' order is detector number order; index 'n' is pad order from top-left.

COMPILE_OPT STRICTARR
if ptr_good(pstate, /struct) eq 0 then return
if ptr_good(pelk) eq 0 then return

pd = (*pstate).playout
n = (*pd).data.index	
d = (*pelk)[n]							; pA after correction
i = indgen(n_elements(d))
pm = (*pstate).pmaia

c = 116B + value_locate( (*pstate).vector_elk, d * 1000.)

widget_control, (*pstate).detector_elk, set_value={mode:1, select:i, colour:c}
s = rates_string((*pm).control.leakage, /microamp)
widget_control, (*pstate).bias_leakage_text, set_value=s
s = rates_string( total(*pelk) * 1.0e-6, /microamp)
widget_control, (*pstate).elk_leakage_text, set_value=s
return
end

;--------------------------------------------------------------------------

function maia_rates_legend, pstate, microamp=microamp

if n_elements(microamp) lt 1 then microamp=0

if (*pstate).cursor lt 0 then return, ['','']

if microamp then begin
	r = 'unknown'
	if ptr_good((*pstate).pelk) then begin
		d = (*(*pstate).pelk)[(*pstate).cursor]
		r = rates_string(d * 1.0e-6, /microamp)
	endif
endif else begin
	d = (*(*pstate).prates).detectors[(*pstate).cursor]
	r = rates_string(d)
endelse

s = ['Pad '+str_tidy((*pstate).cursor), r]
return, s
end

;--------------------------------------------------------------------------

pro maia_rates_update_plot, pstate

;	plot rates
;	initial 'q' is index into detector pad table (not detector number)

pd = (*pstate).playout

case (*pstate).mode of
	0: begin
		(*pstate).member = (*pstate).member < max((*pd).data.hermes)
		q = where( (*pd).data.hermes eq (*pstate).member, nq)
		if nq eq 0 then return
		end
	1: begin
		(*pstate).member = (*pstate).member < max((*pd).data.quadrant)
		q = where( (*pd).data.quadrant eq (*pstate).member, nq)
		if nq eq 0 then return
		end
	2: begin
		(*pstate).member = (*pstate).member < max((*pd).data.row)
		q = where( (*pd).data.row eq (*pstate).member, nq)
		if nq eq 0 then return
		end
	3: begin
		(*pstate).member = (*pstate).member < max((*pd).data.column)
		q = where( (*pd).data.column eq (*pstate).member, nq)
		if nq eq 0 then return
		end
	4: begin
		(*pstate).member = (*pstate).member < max((*pd).data.radial)
		q = where( (*pd).data.radial eq (*pstate).member, nq)
		if nq eq 0 then return
		end
	5: begin
		q = indgen(n_elements((*pd).data))
		end
	6: begin
		q = indgen(16)
		end
endcase
widget_control, (*pstate).member_number, set_combobox_select=(*pstate).member

if (*pstate).mode eq 6 then begin
	d = (*(*pstate).prates).groups								; rates per group
	dm = d
	q2 = q
	n = q
endif else begin
	n = (*pd).data[q].index										; detector numbers
	d = (*(*pstate).prates).detectors[n]						; rates per detector
	dm = (*(*pstate).pmrates).detectors[n]						; max rates per detector
	q2 = sort(n)
endelse
wset, (*pstate).pix
xs = !d.x_size
ys = !d.y_size
histogram_plot, d[q2], x=n[q2], mdata=dm[q2], title=''

wset, (*pstate).wid
device,copy=[0,0, xs,ys, 0,0, (*pstate).pix]
return
end

;--------------------------------------------------------------------------

pro maia_rates_plot_process, pstate

; Plot %, rates stats.

COMPILE_OPT STRICTARR
if widget_info((*pstate).master, /valid) eq 0 then return 
lchild = widget_info( (*pstate).master, /child)
widget_control, lchild, get_uvalue=pstate_launch

d = [ launch_total_activity( (*pstate_launch).activity.display.image), $
	launch_total_activity( (*pstate_launch).activity.display.multi), $
	launch_total_activity( (*pstate_launch).activity.display.ET2d), $
	launch_total_activity( (*pstate_launch).activity.display.ET_spectra) + $
	launch_total_activity( (*pstate_launch).activity.display.ET_spectraT), $
	launch_total_activity( (*pstate_launch).activity.display.spectra), $
	(*pstate_launch).activity.process.DA, $
	(*pstate_launch).activity.process.ET_spectra, $
	(*pstate_launch).activity.process.spectra, $
	(*pstate_launch).activity.process.maia, $
	(*pstate_launch).activity.process.activity, $
	(*pstate_launch).activity.buffers.DA, $
	(*pstate_launch).activity.buffers.ET_spectra, $
	(*pstate_launch).activity.buffers.spectra ]
n_activity = 10
labels = ['Image Display','Multi Display','ET 2D Display','ET spectra Display','Group spectra Display','DA Update', $
		'ET spectra Update','Group spectra Update','Maia Update','Activity Update','DA buffers lost','ET buffers lost', $
		'Group buffers lost']
title = '% Busy / Buffers Lost'

d[0:n_activity-1] /= !cpu.HW_nCPU				; multicores in parallel

wset,(*pstate).pix2
xs = !d.x_size
ys = !d.y_size
histogram_plot, d, labels=labels, title=title, /horizontal, credits=0.32
xyouts, 0.015,0.04, '% Busy Total: ' + str_tidy(total(d[0:n_activity-1]), length=4), /norm

wset, (*pstate).wid2
device,copy=[0,0, xs,ys, 0,0, (*pstate).pix2]
return
end
 
;--------------------------------------------------------------------------

pro onrealize_maia_rates_detector, wWidget

COMPILE_OPT STRICTARR
top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

print, 'Maia_rates: before realize channel enable colours:', systime(/seconds) - time_in

widget_control, (*pstate).detector, set_value={select:indgen(384), value:(*(*pstate).pdisable), alt:1}

print, 'Maia_rates: after realize channel enable colours:', systime(/seconds) - time_in
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_rates_draw, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid

wset, wid
window, /free, xsize=!d.x_size, ysize=!d.y_size, /pixmap
(*pstate).pix = !d.window
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_rates_process_draw, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid2 = wid

wset, wid
window, /free, xsize=!d.x_size, ysize=!d.y_size, /pixmap
(*pstate).pix2 = !d.window
return
end

;------------------------------------------------------------------------------------------

pro maia_rates, group_leader=group, data=data, mdata=mdata, layout=layout, maia=maia_pars, $
			disable=maia_disable, path=path, tlb=tlb, tracking=tracking

; Display count-rate data from the Maia-384 detector array
;
; Layout	is a pointer (/allocate_heap) to a struct of layout parameters.
; Maia		is a pointer (/allocate_heap) to a struct of Maia detector ASIC and control parameters. 
; Disable	is a pointer (/allocate_heap) to an array of Maia channel disable (ECH) parameters.
; Data		is a pointer (/allocate_heap) to count-rate data in Maia-Launch
; MData		is a pointer (/allocate_heap) to maximum count-rate data in Maia-Launch
; /tracking enables the Help window and contexnt-sensitive help

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
		warning,'maia_rates',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_timing_1, time_in
time_in = systime(/seconds)

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(tracking) lt 1 then tracking=1

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
lviolet = spec_colour('l.violet')
pink = spec_colour('pink')
black = spec_colour('black')
white = spec_colour('white')
colours2 = [green, yellow, red]										; for warning "buttons"

;	  select: 0    1     2    3    4     5     6     7
rates =  	['0','10','100','1K','10K','20K','50K','100K']
colours =  [black,blue,lblue,green,yellow,orange,red,lviolet]

; Rate thresholds for each colour map band
threshold = [0,10,100,1000,10000,20000,50000,100000]
load_rate_colours, colours=colours, threshold=threshold, vector=vector

; ELK thresholds (pA) for vector_elk lookup table (integer)
;threshold_elk = [0,1,2,5,10,20,50,100]*1000.
;leaks = rates_string( threshold_elk * 1.0e-9, /microamp, digits=3)

threshold_elk = [0,10,30,100,300,1000,10000,100000]
leaks = rates_string( threshold_elk * 1.0e-9, /microamp, digits=3)
load_rate_colours, colours=colours, threshold=threshold_elk, vector=vector_elk

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
	xoffset = (xoff + w) < (screen[0]- (458) > 0)
	if xoffset lt (xoff + w) then begin
		t = xoff - (458)
		if t ge 0 then xoffset=t
	endif
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff) < (screen[1]-28 - 200)) > 0
endif

; Detector rates parameters 

pr = bad_pars_struct( data, make_pars=make_pr)
if make_pr then begin
	dr = 11000. * randomu( seed, 384)
	dr[0] = 123456.
	dr[1] = 33000.
	dr[2] = 13000.
	*pr = {detectors:dr, groups:fltarr(16)}
endif else t=randomu(seed,1)
pmr = bad_pars_struct( mdata, make_pars=make_pmr)
if make_pmr then begin
	dr = 11000. * randomu( seed, 384)
	dr[0] = 123456.
	dr[1] = 33000.
	dr[2] = 13000.
	*pmr = {detectors:dr}
endif
;(*pmr).detectors[*] = 0.

; Detector layout struct parameters from file "Maia_384.csv"

pd = bad_pars_struct( layout, make_pars=make_pd)
if make_pd then begin
	d = read_detector_layout('Maia_384.csv', maia=maia, error=error)
	if error then begin
		warning,'maia_rates','Failed to read "Maia_384.csv" to initialize layout.'
		return
	endif
	if maia eq 0 then begin
		warning,'maia_rates','"Maia_384.csv" file does not contain extended columns.'
		return
	endif
	*pd = d
endif

; Maia control parameters struct

pm = bad_pars_struct( maia_pars, make_pars=make_pm)
if make_pm then begin
	*pm = define(/maia_struct)
	error = read_maia_parameters( 'Maia.parameters.csv', data=pm, path=path, group=group)
	if error then begin
		warning,'maia_rates','Failed to read "Maia.parameters.csv" to initialize Maia parameters.'
	endif
	groups = read_maia_groups( 'Maia.groups.csv', error=err)
	if err eq 0 then begin
		(*pm).groups.file = 'Maia.groups.csv'
		(*pm).groups.group = groups
		(*pm).groups.on = 1
	endif
endif
n_detectors = (*pm).n_detectors

; Maia disable parameters struct

pe = bad_pars_struct( maia_disable, make_pars=make_pe)
if make_pe then begin
	disable = read_maia_enable( 'Maia.enable.csv', index=(*pd).data.index, error=error)
	if error then begin
		warning,'maia_rates','Failed to read "Maia_384.csv" to initialize layout.'
		disable = intarr(384)
	endif
	*pe = disable
endif

case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		csize = 1.3
		mimic_xsize = 295
		mimic_ysize = ((n_detectors eq 384) ? 300 : 260)
		tab_xsize = 440
		tab_ysize = 330
		help_xsize = tab_xsize
		draw_xsize = tab_xsize-15
		draw_ysize = tab_ysize-60
		legend_ysize = tab_ysize-40
		leg_space = 3
		retain = 2
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		csize = 1.3
		mimic_xsize = 285
		mimic_ysize = ((n_detectors eq 384) ? 290 : 250)
		tab_xsize = 440
		tab_ysize = 330
		help_xsize = tab_xsize
		draw_xsize = tab_xsize-15
		draw_ysize = tab_ysize-60
		legend_ysize = tab_ysize-80
		leg_space = 3
		retain = 2
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		csize = 1.3
		mimic_xsize = 295
		mimic_ysize = ((n_detectors eq 384) ? 300 : 260)
		tab_xsize = 440
		tab_ysize = 333
		help_xsize = tab_xsize
		draw_xsize = tab_xsize-15
		draw_ysize = tab_ysize-60
		legend_ysize = tab_ysize-40
		leg_space = 5
		retain = 1
		end
endcase
;@2	widget_control, default_font=def_font        ; set font for all windows

; 	top-level base

tlb = widget_base( /column, title='Maia Rates', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='maia-rates-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

tab_panel = widget_tab( tbase, location=0, /align_center, uname='maia-rates-tab-panel')

; Rates -----------------------------------------

rates_base = widget_base( tab_panel, title='  Count Rates  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=tab_xsize, scr_ysize=tab_ysize)
label = widget_label( rates_base, value='Detector Count Rates by Class')

r1base = widget_base( rates_base, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
label = widget_label( r1base, value='Detector Class:')
detector_mode = widget_combobox(r1base, uname='select-mode', scr_xsize=100, $
			value=['Chip','Quadrant','Row','Column','Radial','Detector','Groups'], tracking=tracking, uvalue='Select the grouping Class of detectors. Rates will be displayed for these Classes')
member_base = widget_base( r1base, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center, map=1)
label = widget_label( member_base, value='  Member:')
member_number = widget_combobox(member_base, uname='select-member', scr_xsize=80, $
			value=str_tidy(indgen(20)), tracking=tracking, uvalue='Select the member of the Class to display rates for.')
button = widget_button( r1base, value='Clear', uname='clear-button', tracking=tracking, uvalue='Clear display of maximum rates per channel (red histogram).')

rates_draw = widget_draw( rates_base, uname='draw', xsize=draw_xsize, ysize=draw_ysize, retain=retain, notify_realize='OnRealize_maia_rates_draw')

; Mimic rates -----------------------------------------

mimic_base = widget_base( tab_panel, title=' Detector Rate Map ', /column, xpad=5, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=tab_xsize, scr_ysize=tab_ysize)

m1base = widget_base( mimic_base, /row, xpad=0, ypad=5, space=10, /base_align_top, /align_center)

m2base = widget_base( m1base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_top)
label = widget_label( m2base, value='Rate Legend')

legend_id = lonarr(8)
legend_text = lonarr(8)
for i=7,0,-1 do begin
	lbase = widget_base( m2base, /row, xpad=0, ypad=0, space=leg_space, /base_align_center, /align_center)
	legend_id[i] = state_button( lbase, xsize=20, ysize=18, value='', $
			  uname='legend', charsize=0, select=i, uvalue=i, $
			  n_states=n_elements(colours), alt=0, n_alt_states=0, colours=colours, /freeze )
	legend_text = widget_label( lbase, value='  '+rates[i], scr_xsize=50, tracking=tracking, uvalue='Count rate bands for each legend colour.')
endfor

label = widget_label( m2base, value=' ')
label = widget_label( m2base, value='Total Count rates')
m2base1 = widget_base( m2base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_top)
m2base2 = widget_base( m2base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m2base2, value='Link:')
link_rate_text = widget_label( m2base2, value='         ', scr_xsize=60)
m2base3 = widget_base( m2base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m2base3, value='Events:')
event_rate_text = widget_label( m2base3, value='         ', scr_xsize=60)
m2base4 = widget_base( m2base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m2base4, value='Lost:')
discard_rate_text = widget_label( m2base4, value='         ', scr_xsize=60)
m2base5 = widget_base( m2base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m2base5, value='Flux0:')
charge_rate_text = widget_label( m2base5, value='         ', scr_xsize=60)
m2base6 = widget_base( m2base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m2base6, value='Flux1:')
flux1_rate_text = widget_label( m2base6, value='         ', scr_xsize=60)
;m2base7 = widget_base( m2base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
;label = widget_label( m2base7, value='ROI:')
;ROI_rate_text = widget_label( m2base7, value='         ', scr_xsize=60)
ROI_rate_text = 0L

m4base = widget_base( m1base, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

detector = detector_mimic( m4base, data=pd, uname='detector', tracking=2, legend=2, position=1, lstyle=1, $
					xsize_min=mimic_xsize, xsize_max=mimic_xsize, ysize_max=mimic_ysize, csize=0, colours=colours, /fill)

; Processes -----------------------------------------

process_base = widget_base( tab_panel, title=' Process Load ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=tab_xsize, scr_ysize=tab_ysize)
label = widget_label( process_base, value='CPU Load and Blog Buffers Lost')

process_draw = widget_draw( process_base, uname='process-draw', xsize=draw_xsize, ysize=draw_ysize+40, retain=retain, notify_realize='OnRealize_maia_rates_process_draw')

; Mimic ELK -----------------------------------------

mimic_elk_base = widget_base( tab_panel, title=' Detector Leakage ', /column, xpad=5, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=tab_xsize, scr_ysize=tab_ysize)

m11base = widget_base( mimic_elk_base, /row, xpad=0, ypad=5, space=10, /base_align_top, /align_center)

m12base = widget_base( m11base, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_top)
label = widget_label( m12base, value='Leakage Legend')

legend_elk_id = lonarr(8)
legend_elk_text = lonarr(8)
for i=7,0,-1 do begin
	l11base = widget_base( m12base, /row, xpad=0, ypad=0, space=leg_space, /base_align_center, /align_center)
	legend_elk_id[i] = state_button( l11base, xsize=20, ysize=18, value='', $
			  uname='legend-elk', charsize=0, select=i, uvalue=i, $
			  n_states=n_elements(colours), alt=0, n_alt_states=0, colours=colours, /freeze )
	legend_elk_text = widget_label( l11base, value='  '+leaks[i], scr_xsize=50, tracking=tracking, uvalue='Leakage bands for each legend colour.')
endfor

label = widget_label( m12base, value=' ')
label = widget_label( m12base, value='Total Leakage')
m12base1 = widget_base( m12base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_top)
m12base2 = widget_base( m12base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m12base2, value='Bias:')
bias_leakage_text = widget_label( m12base2, value='         ', scr_xsize=60)
m12base3 = widget_base( m12base1, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_right)
label = widget_label( m12base3, value='ELK:')
elk_leakage_text = widget_label( m12base3, value='         ', scr_xsize=60)

m13base = widget_base( m11base, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

detector_elk = detector_mimic( m13base, data=pd, uname='detector', tracking=2, legend=2, position=1, lstyle=1, $
					xsize_min=mimic_xsize, xsize_max=mimic_xsize, ysize_max=mimic_ysize, csize=0, colours=colours, /fill)

;------------------------------------------------

if tracking then begin
	hbase = widget_base( tlb, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
	help = widget_text( hbase, scr_xsize=help_xsize-99, ysize=3, /wrap, uname='help', /tracking, $
					uvalue='Context sensitive help. Move cursor over widgets to see help on them.', frame=0)
	picpath = geopixe_root + 'maia' + slash()
	pic = picture_button( hbase, picpath + 'Maia-Logo2.png', tracking=0, pushbutton_events=0)	;, xsize=144, ysize=48
endif else help = 0L

state = { $
		path:				ptr_new(path), $			; pointer to current path
		prates:				pr, $						; pointer to detector rates array
		pmrates:			pmr, $						; pointer to detector max rates array
		playout:			pd, $						; pointer to layout struct array
		pmaia:				pm, $						; pointer to maia parameters struct array
		pdisable:			pe, $						; pointer to maia disable detector channels
		plaunch:			ptr_new(/allocate_heap), $	; pointer data for use with Notify to Launch
		pselect:			ptr_new(/allocate_heap), $	; pointer data for use with Notify to Spectra display
		pelk:				ptr_new(), $				; pointer to pelk in maia_setup
		tracking:			tracking, $					; tracking mode
		seed:				seed, $						; randumu seed
		cursor:				-1, $						; current detector pad
		
		mode:				0, $						; detector mode
		member:				0, $						; detector class member
		wid:				0L, $						; draw WID for rates
		pix:				0L, $						; pixmap WID
		wid2:				0L, $						; process draw widget ID
		pix2:				0L, $						; pixmap2 WID for process stats
		scr_xsize:			0, $						; TLB X size
		scr_ysize:			0, $						; TLB Y size
		colours:			colours, $					; legend colours
		vector:				vector, $					; rate legend thresholds
		vector_elk:			vector_elk, $				; elk legend thresholds
		
		tlb:				tlb, $						; TLB ID
		master:				group, $					; Launch TLB ID
		tab_panel:			tab_panel, $				; tab ID
		detector_mode:		detector_mode, $			; detector Class selection mode ID
		member_number:		member_number, $			; detector class member number ID
		member_base:		member_base, $				; member base for map
		rates_draw:			rates_draw, $				; rates draw widget ID
		legend_id:			legend_id, $				; legend button IDs
		legend_text:		legend_text, $				; legend rate text IDs
		event_rate_text:	event_rate_text, $			; event rate text ID
		link_rate_text:		link_rate_text, $			; link rate text ID
		charge_rate_text:	charge_rate_text, $			; charge (Flux0) rate text ID
		flux1_rate_text:	flux1_rate_text, $			; Flux1 rate text ID
		ROI_rate_text:		ROI_rate_text, $			; ROI rate text ID
		discard_rate_text:	discard_rate_text, $		; discard rate text ID
		detector:			detector, $					; detector mimic ID
		legend_elk_id:		legend_elk_id, $			; elk legend button IDs
		legend_elk_text:	legend_elk_text, $			; elk legend leakage text IDs
		bias_leakage_text:	bias_leakage_text, $		; bias leakage text ID
		elk_leakage_text:	elk_leakage_text, $			; total elk leakage text ID
		detector_elk:		detector_elk, $				; ELK detector mimic ID
		
		help:				help $						; help text ID
		}

child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

geo = widget_info( tlb, /geometry)
(*pstate).scr_xsize = geo.scr_xsize
(*pstate).scr_ysize = geo.scr_ysize

maia_rates_colour_elk, pstate, (*pstate).pelk

register_notify, tlb, ['path', $				; new path
						'select-highlight', $	; highlight spectrum #
						'maia-process', $		; update from Maia Launch for new process stats
						'update-elk', $			; update Rate ELK map from Maia-Setup
						'maia-rates'], $		; update from Maia Launch for new count rates
						from=group

xmanager, 'maia_rates', tlb, /no_block

print, 'Maia_rates: end of widget realize', systime(/seconds) - time_in
end
		