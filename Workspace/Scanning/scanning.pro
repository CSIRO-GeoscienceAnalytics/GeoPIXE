;
;	Modelling scanning, delays and overheads ...
;
pro scanning_event, event

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
		warning,'scanning_event',['IDL run-time error caught.', '', $
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

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin

				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s
		if event.enter eq 1 then begin
			if size(s,/tname) eq 'STRING' then begin
				widget_control, (*pstate).help, set_value=s
			endif else if size(s,/tname) eq 'STRUCT' then begin
				t = tag_names( s)
				q = where( t eq 'HELP')
				if q[0] ne -1 then begin
					if size(s.Help,/tname) eq 'STRING' then begin
						widget_control, (*pstate).help, set_value=s.Help
					endif
				endif
			endif
			goto, done
		endif else begin
			widget_control, (*pstate).help, set_value=(*pstate).warning
		endelse
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request scanning ...'
		goto, kill
		end
	else:
endcase

;	Read text widgets and update ...

	widget_control, (*pstate).x_text, get_value=s
	(*pstate).scan.x = float2(s)
	widget_control, (*pstate).y_text, get_value=s
	(*pstate).scan.y = float2(s)
	widget_control, (*pstate).dwell_text, get_value=s
	(*pstate).scan.dwell = float2(s)
	widget_control, (*pstate).pixel_text, get_value=s
	(*pstate).scan.pixel = float2(s)
	widget_control, (*pstate).accel_text, get_value=s
	(*pstate).stage.accel= float2(s)
	widget_control, (*pstate).max_vel_text, get_value=s
	(*pstate).stage.max_v = float2(s)
	widget_control, (*pstate).line_oh_text, get_value=s
	(*pstate).stage.line_oh = float2(s)
	widget_control, (*pstate).stack_text, get_value=s
	(*pstate).stack.count = long2(s)

uname = widget_info( event.id, /uname)
case uname of

	'start_button': begin
		end
		
	else:
endcase

finish:
	vd = (*pstate).scan.pixel / (*pstate).scan.dwell
	vm = min( [ (*pstate).stage.max_v, vd])
	s1 = 0.5 * vm*vm / (*pstate).stage.accel
	if s1 lt (*pstate).scan.x/2 then begin
		s2 = (*pstate).scan.x - 2.*s1
		t2 = s2 / vm
		t1 = vm / (*pstate).stage.accel
		t = 2*t1 + t2
		nx = round(1000. * (*pstate).scan.x / (*pstate).scan.pixel)
		ny = round(1000. * (*pstate).scan.y / (*pstate).scan.pixel)
		
		top_speed = vm
		(*pstate).warning = 'Stage always accelerating; fails to come up to speed.'
		if vm lt vd then begin
			(*pstate).warning = 'Dwell time is too short. Desired slew speed ('+str_tidy(vd)+') exceeds stage maximum. Increase "Dwell" or reduce "Pixel" / "Dwell".'
		endif else begin
			(*pstate).warning = 'Stage reaches desired slew speed.'
		endelse
		
	endif else begin
		s1 = (*pstate).scan.x / 2
		v = sqrt( (*pstate).stage.accel * (*pstate).scan.x)
		t1 = v / (*pstate).stage.accel
		t = 2*t1
		nx = round(1000. * (*pstate).scan.x / (*pstate).scan.pixel)
		ny = round(1000. * (*pstate).scan.y / (*pstate).scan.pixel)
		
		top_speed = v
		(*pstate).warning = 'Stage always accelerating; fails to come up to speed.'
	endelse
	
	scan_x = nx
	scan_y = ny
	accel_dist = s1
	accel_time = t1 
	accel_perc = 100. * 2.*t1 / t
	tt_oh = ny*(t + (*pstate).stage.line_oh)
	tt_noh = ny*(t)
	perc_oh = 100. * (*pstate).stage.line_oh / (t + (*pstate).stage.line_oh)
	mp = long(nx)*long(ny)/(1000L*1000L)
	simg = str_tidy(nx) + ' x ' + str_tidy(ny) + '   (' + str_tidy(mp) + 'M)'
	
	tt_oh = tt_oh * (*pstate).stack.count
	tt_noh = tt_noh * (*pstate).stack.count
	
	if tt_oh gt 3600. then begin
		st = '  (h)'
		ts = 1./3600.
	endif else if tt_oh gt 59.9 then begin
		st = '  (m)'
		ts = 1./60.
	endif else begin
		st = '  (s)'
		ts = 1.
	endelse
	
	widget_control, (*pstate).image_text, set_value=simg
	widget_control, (*pstate).speed_text, set_value=str_tidy(top_speed) + '  (mm/s)'
	widget_control, (*pstate).accel_dist_text, set_value=str_tidy(accel_dist) + '  (mm)'
	widget_control, (*pstate).accel_time_text, set_value=str_tidy(accel_time) + '  (s)'
	widget_control, (*pstate).accel_perc_text, set_value=str_tidy(accel_perc) + '  (%)'
	widget_control, (*pstate).time_oh_text, set_value=str_tidy(tt_oh*ts) + st
	widget_control, (*pstate).time_noh_text, set_value=str_tidy(tt_noh*ts) + st
	widget_control, (*pstate).time_perc_text, set_value=str_tidy(perc_oh) + '  (%)'
	widget_control, (*pstate).help, set_value=(*pstate).warning
done:
	return

bad_state:
	warning,'scanning',['STATE variable has become ill-defined.','Abort Sort EVT.'],/error
	goto, kill

kill:
	cancel_notify, event.top
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

;	if ptr_valid((*pstate).path) then ptr_free, (*pstate).path

die:
	widget_control, event.top, /destroy
	return
end

;------------------------------------------------------------------------------------------

pro scanning

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
		warning,'scanning',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
startupp

case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		left_xsize = 200
		right_xsize = 200
		text_xsize = 70
		text_xsize2 = 150
		help_xsize = 400
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		left_xsize = 200
		right_xsize = 200
		text_xsize = 70
		text_xsize2 = 150
		help_xsize = 400
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
	;	widget_control, default_font='Arial*14'			; set font for all windows
		left_xsize = 160
		right_xsize = 240
		text_xsize = 70
		text_xsize2 = 150
		help_xsize = 410
		end
endcase
		
xscan = 30.
yscan = 20.
dwell = 0.2
pixel = 2.
accel = 100.
max_v = 10.
line_oh = 0.5
stack_count = 1
register_notify
		
; 	top-level base

tlb = widget_base( /column, title='Scanning Set-up', /TLB_KILL_REQUEST_EVENTS, $
					uname='scanning_TLB', /base_align_center, xpad=0, ypad=0, space=0 )
tbase = widget_base( tlb, /column, xpad=2, ypad=2, space=2, /base_align_center)
r1base = widget_base( tbase, /row, xpad=2, ypad=2, space=6, /base_align_top)

scan_base = widget_base( r1base, /column, xpad=1, ypad=1, space=1, /frame, /base_align_right, scr_xsize=left_xsize)
rc_base = widget_base( r1base, /column, xpad=0, ypad=0, space=1, /base_align_right)

label = widget_label( scan_base, value='Scan', /align_center)

sbase1 = widget_base( scan_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase1, value='X (mm):')
x_text = widget_text( sbase1, value=strtrim(string(xscan),2), uname='x-text', /tracking, /editable, $
					uvalue='Select the X (mm) size of the scan area. Hit <return> to calculate.', scr_xsize=text_xsize)

sbase2 = widget_base( scan_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase2, value='Y (mm):')
y_text = widget_text( sbase2, value=strtrim(string(yscan),2), uname='y-text', /tracking, /editable, $
					uvalue='Select the Y (mm) size of the scan area. Hit <return> to calculate.', scr_xsize=text_xsize)

sbase3 = widget_base( scan_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase3, value='Dwell (ms):')
dwell_text = widget_text( sbase3, value=strtrim(string(dwell),2), uname='dwell-text', /tracking, /editable, $
					uvalue='Select the nominal dwell time (ms) per pixel. Hit <return> to calculate.', scr_xsize=text_xsize)

sbase4 = widget_base( scan_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase4, value='Pixel (um):')
pixel_text = widget_text( sbase4, value=strtrim(string(pixel),2), uname='pixel-text', /tracking, /editable, $
					uvalue='Select the nominal pixel size (um). Hit <return> to calculate.', scr_xsize=text_xsize)

stage_base = widget_base( rc_base, /column, xpad=1, ypad=1, space=1, /frame, /base_align_right, scr_xsize=right_xsize)
label = widget_label( stage_base, value='Stage', /align_center)

tbase1 = widget_base( stage_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( tbase1, value='Acceleration (mm/s2):')
accel_text = widget_text( tbase1, value=strtrim(string(accel),2), uname='accel-text', /tracking, /editable, $
					uvalue='Select the acceleration (mm/s2) limit for the stage. Hit <return> to calculate.', scr_xsize=text_xsize)

tbase2 = widget_base( stage_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( tbase2, value='Maximum Velocity (mm/s):')
max_vel_text = widget_text( tbase2, value=strtrim(string(max_v),2), uname='max-vel-text', /tracking, /editable, $
					uvalue='Select the maximum velocity (mm/s) limit for the stage. Hit <return> to calculate.', scr_xsize=text_xsize)

tbase3 = widget_base( stage_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( tbase3, value='Line overhead/delay (s):')
line_oh_text = widget_text( tbase3, value=strtrim(string(line_oh),2), uname='line-oh-text', /tracking, /editable, $
					uvalue='Select the overhead from the beamline delay at end of line (s). Hit <return> to calculate.', scr_xsize=text_xsize)

stack_base = widget_base( rc_base, /column, xpad=1, ypad=1, space=1, /frame, /base_align_right, scr_xsize=right_xsize)
tbase5 = widget_base( stack_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( tbase5, value='XANES/ Tomo stack count:')
stack_text = widget_text( tbase5, value=strtrim(string(stack_count),2), uname='stack-count-text', /tracking, /editable, $
					uvalue='Select the number of frames in a XANES stack or tomography series. Hit <return> to calculate.', scr_xsize=text_xsize)

results_base = widget_base( tbase, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_right, scr_xsize=help_xsize)
label = widget_label( results_base, value='Results', /align_center)

rbase0 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase0, value='Image size:')
image_text = widget_text( rbase0, value='', uname='image-text', /tracking, editable=0, $
					uvalue='Shows the desired image area in pixels.', scr_xsize=text_xsize2)

rbase1 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase1, value='Top Stage Speed:')
speed_text = widget_text( rbase1, value='', uname='speed-text', /tracking, editable=0, $
					uvalue='Shows the top stage speed (mm/s) within the scan.', scr_xsize=text_xsize2)

rbase2 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase2, value='Acceleration distance:')
accel_dist_text = widget_text( rbase2, value='', uname='accel-dist-text', /tracking, editable=0, $
					uvalue='Shows the distance required to accelerate up to top speed (mm).', scr_xsize=text_xsize2)

rbase3 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase3, value='Acceleration time:')
accel_time_text = widget_text( rbase3, value='', uname='accel-time-text', /tracking, editable=0, $
					uvalue='Shows the time required to accelerate up to top speed (s).', scr_xsize=text_xsize2)

rbase4 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase4, value='Acceleration fraction:')
accel_perc_text = widget_text( rbase4, value='', uname='accel-perc-text', /tracking, editable=0, $
					uvalue='Shows the % of time spent in acceleration mode.', scr_xsize=text_xsize2)

rbase6 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase6, value='Total scan Time, for zero line OH:')
time_noh_text = widget_text( rbase6, value='', uname='time-noh-text', /tracking, editable=0, $
					uvalue='Shows the total scan time required, excluding the beamline end of line delays/ overheads.', scr_xsize=text_xsize2)

rbase5 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase5, value='Total scan Time, w/ line overheads:')
time_oh_text = widget_text( rbase5, value='', uname='time-oh-text', /tracking, editable=0, $
					uvalue='Shows the total scan time required, including the beamline end of line delays/ overheads.', scr_xsize=text_xsize2)

rbase7 = widget_base( results_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( rbase7, value='Fraction of scan time lost to line overheads:')
time_perc_text = widget_text( rbase7, value='', uname='time-perc-text', /tracking, editable=0, $
					uvalue='Shows the fraction of total scan time lost to beamline end of line delays/ overheads.', scr_xsize=text_xsize2)

hbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
help = widget_text( hbase, scr_xsize=help_xsize, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Help window. Pass mouse cursor over widgets to get information on them.', frame=0)

state = {	scan: {	x:		xscan, $					; scan X size (mm)
					y:		yscan, $					; scan Y size (mm)
					dwell:	dwell, $					; scan dwell time (ms)
					pixel:	pixel}, $					; scan pixel size (um)
			stage: { accel:	accel, $					; stage accel (mm/s2)
					max_v:	max_v, $					; stage maximum velocity (mm/s)
					line_OH: line_OH}, $				; Epics line delay/overhead (s)
			stack: { count:	stack_count}, $				; XANES/Tomo stack count
			warning:		'', $						; Help Warning message text
			
			x_text:			x_text, $					; X text ID
			y_text:			y_text, $					; Y text ID
			dwell_text:		dwell_text, $				; Dwell text ID
			pixel_text:		pixel_text, $				; Pixel text ID
			accel_text:		accel_text, $				; accel text ID
			max_vel_text:	max_vel_text, $				; max Vel text ID
			line_oh_text:	line_oh_text, $				; Line OH text ID
			image_text:		image_text, $				; image area text ID
			speed_text:		speed_text, $				; speed text ID
			accel_dist_text: accel_dist_text, $			; accel distance text ID
			accel_time_text: accel_time_text, $			; accel time text ID
			accel_perc_text: accel_perc_text, $			; accel % text ID
			time_oh_text:	time_oh_text, $				; time (w/ OH) text ID
			time_noh_text:	time_noh_text, $			; time (no OH) text ID
			time_perc_text:	time_perc_text, $			; % time lost to OH ID
			stack_text:		stack_text, $				; stack count text ID
			help:			help }						; help text ID
			
child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

xmanager, 'scanning', tlb, /no_block
end

		