;
; Image_History    display history information for selected element image
;
pro Image_history_event, Event

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
		warning,'Image_history_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

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

	wWidget =  Event.top
	widget_control, hourglass=0
  
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate	
	if ptr_good(pstate,/struct) eq 0 then goto, bad_state
  
	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_Image_history, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_Image_history, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'Image_history_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_Image_history, Event
				end
			'WIDGET_BASE': begin
				OnSize_Image_history, Event
				end
			else:
		endcase
		end
	'metadata-button': begin
		print_image_metadata, (*pstate).p, stats=0
		end
	'GCF-button': begin
		p = (*pstate).p
		if ptr_good(p,/struct) eq 0 then goto, bad_ptr
		F = file_requester( /write, filter='*.gcf', path=extract_path((*p).file), group=event.top, $
			title='Write a "GeoPIXE Command File"', file='image-history.gcf', /fix_filter, /latest)
		if F eq '' then return
		args = ['print_image_metadata','files='+str_tidy((*p).file),'stats=0','output='+strip_file_ext((*p).file)+'-metadata.txt']
		geopixe_gen_commands, F, args
		end
	'Image_history_List': begin
		p = (*pstate).p
		if ptr_good(p,/struct) eq 0 then goto, bad_ptr
		if (event.clicks eq 2) and (event.index lt n_elements(*(*pstate).pindex)) then begin
			index = (*(*pstate).pindex)[event.index]
			xs = 500  &  ys = 400
			case index of
				45: begin
					text = [(*p).sample, (*p).grain, (*p).comment]
					label = ['Sample','Grain','Comment']
					string_edit, event.top, title='Edit Image Property Strings',label=label, text=text, error=err
					if err eq 0 then begin
						(*p).sample = text[0]
						(*p).grain = text[1]
						(*p).comment = text[2]
					endif
					end
				70: begin
					text = str_tidy([(*p).scan.x, (*p).scan.y])
					label = ['Scan X size (mm)','Scan Y size (mm)']
					string_edit, event.top, title='Edit Image Properties',label=label, text=text, /numeric, error=err
					if err eq 0 then begin
						(*p).scan.x = float2(text[0])
						(*p).scan.y = float2(text[1])
					endif
					end
				90: begin
					text = str_tidy([(*p).scan.origin.x, (*p).scan.origin.y])
					label = ['Scan X origin (mm)','Scan Y origin (mm)']
					string_edit, event.top, title='Edit Image Properties',label=label, text=text, /numeric, error=err
					if err eq 0 then begin
						(*p).scan.origin.x = float2(text[0])
						(*p).scan.origin.y = float2(text[1])
					endif
					end
				111: begin
					if (*p).has_flux then Begin
						q = qsample( (*p).raw_flux, nq, /positive, veto_low=0.5, veto_high=0.5)
						if nq gt 0 then begin
							window, 0, xsize=xs, ysize=ys, retain=retain
							h = histogram(  (*(*p).raw_flux)[q], nbins=100, omin=omin, omax=omax, locations=x)
							!p.title = 'Raw flux'
							!x.title = 'Flux per pixel'
							!y.title = 'Frequency'
							plot, x, h, /nodata
							oplot, x, h, color=spec_colour('green'), psym=10
						endif
					endif
					end
				112: begin
					if (*p).has_dwell then Begin
						q1 = qsample( (*p).dwell_map, nq1, /positive, veto_low=0.5, veto_high=0.5)
						if nq1 gt 0 then begin
							window, 0, xsize=xs, ysize=ys, retain=retain
							h = histogram(  (*(*p).dwell_map)[q1], nbins=100, omin=omin, omax=omax, locations=x)
							!p.title = 'Dwell'
							!x.title = 'Dwell per pixel (ms)'
							!y.title = 'Frequency'
							plot, x, h, /nodata
							oplot, x, h, color=spec_colour('green'), psym=10
						endif
					endif
					end
				113: begin
					if (*p).has_dead then Begin
						q1 = qsample( (*p).dead_fraction, nq1, /positive, veto_low=0.5, veto_high=0.5)
						if nq1 gt 0 then begin
							window, 0, xsize=xs, ysize=ys, retain=retain
							h = histogram(  (*(*p).dead_fraction)[q1], nbins=100, omin=omin, omax=omax, locations=x)
							!p.title = 'Dead Time Fraction'
							!x.title = 'Dead Time Fraction'
							!y.title = 'Frequency'
							plot, x, h, /nodata
							oplot, x, h, color=spec_colour('green'), psym=10
						endif
					endif
					end
				114: begin
					if (*p).has_pileup then Begin
						q1 = qsample( (*p).pileup_map, nq1, /positive, veto_low=0.5, veto_high=0.5)
						if nq1 gt 0 then begin
							window, 0, xsize=xs, ysize=ys, retain=retain
							h = histogram(  (*(*p).pileup_map)[q1], nbins=100, omin=omin, omax=omax, locations=x)
							!p.title = 'Pile-up Fraction'
							!x.title = 'Pile-up Fraction'
							!y.title = 'Frequency'
							plot, x, h, /nodata
							oplot, x, h, color=spec_colour('green'), psym=10
						endif
					endif
					end
				115: begin
					if (*p).has_rates then Begin
						q1 = qsample( (*p).count_rate_map, nq1, /positive, veto_low=0.5, veto_high=0.5)
						if nq1 gt 0 then begin
							window, 0, xsize=xs, ysize=ys, retain=retain
							h = histogram(  (*(*p).count_rate_map)[q1], nbins=100, omin=omin, omax=omax, locations=x)
							!p.title = 'Count-Rates'
							!x.title = 'Count rate (c/s)'
							!y.title = 'Frequency'
							plot, x, h, /nodata
							oplot, x, h, color=spec_colour('green'), psym=10
						endif
					endif
					end
				170: begin
					image_flux_charge, p, xanes=xanes, charge=charge, conv=conv, flux=flux, RT_mode=RT_mode, error=error
					if error eq 0 then begin
						charge_select, event.top, title='Flux to Charge conversion', conv=conv, charge=charge, flux=flux, error=error
						if error eq 0 then begin
							if RT_mode eq 0 then (*p).charge = charge
							(*p).IC.conversion = conv
							(*p).temp.valid = 0
						endif
					endif
					end
				else: begin
					if index gt 1000 then begin
						warning,'image_history_event',['Selected image property cannot be changed.', '', $
									'To change image processing parameters,', $
									'load original image file and redo processing steps.']
					endif else begin
						warning,'image_history_event',['Selected image property cannot be changed.', '', $
									'May need to regenerate images to change', $
									'some image properites.']
					endelse
					end
			endcase
			Update_Image_history, pstate
		endif
		end
	else:
  endcase
  return
  
bad_state:
	warning,'image_history_event',['STATE variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill
bad_ptr:
	warning,'image_history_event',['Parameter structure variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill

kill:
	widget_control, hourglass=0
	OnKill_Image_history, Event
end

;-------------------------------------------------------------------------------------------------------

pro Image_history, GROUP_LEADER=wGroup, TLB=Image_history_TLB, xoffset=xoffset, $
		yoffset=yoffset, pimages=pimages, show=show, path=path, stats=stats

 image_history_eventcb     ; Load event callback routines

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
		warning,'Image_history',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(wGroup) lt 1 then wGroup=0L
;if n_elements(pimages) lt 1 then pimages=ptr_new()
if ptr_good(pimages,/struct) eq 0 then return
if n_elements(show) lt 1 then show=0
if n_elements(stats) lt 1 then stats=0

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
		xoffset = ((xoff + w) < (screen[0]-34 - 192)) > 0
	endif
	if n_elements(yoffset) lt 1 then begin
		screen = get_screen_size()
		yoffset = ((yoff + h-289) < (screen[1]-28 - 236)) > 0
	endif

	Image_history_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Image_history_TLB'  $
			,/TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset  $
			,/TLB_SIZE_EVENTS ,TITLE='Image History' ,SPACE=3 ,XPAD=3  $
			,YPAD=3 ,COLUMN=1 ,/BASE_ALIGN_CENTER, _EXTRA=_VWBExtra_)

	Image_history_List = Widget_List(Image_history_TLB, UNAME='Image_history_List',  $
			value = List, NOTIFY_REALIZE='OnRealize_Image_history', $
			scr_xsize=400 ,scr_ysize=550)

	base1 = Widget_Base(Image_history_TLB, UNAME='Button_Base', /row, space=10, /align_center)

	metadata_button = Widget_Button( base1, value='Output metadata', uname='metadata-button') 

	GCF_button = Widget_Button( base1, value='C*', uname='GCF-button') 

	Widget_Control, /REALIZE, Image_history_TLB

	child = widget_info( Image_history_TLB, /child)
	widget_control, child, get_uvalue=pstate

	(*pstate).plist = ptr_new(list)
	(*pstate).pindex = ptr_new([0])
	(*pstate).stats = stats
	(*pstate).show = show
	if ptr_valid(pimages) then (*pstate).p = pimages
	Update_Image_history, pstate

	register_notify, Image_history_TLB, $
  				 ['images-changed', $					; new images loaded
  				 'image-display', $						; display changes, filters
  				'image-show-element'], $				; new element image selected
  				from=wGroup

	XManager, 'Image_history', Image_history_TLB ,/no_block
	return
end
