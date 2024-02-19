;
; Image_Table
;
; Results table and region I/O and EVT --> spectrum sorting set-up.
;
;
pro Image_Table_event, Event

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
		warning,'Image_table_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_regions = 0
if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

  wWidget =  Event.top
  widget_control, hourglass=0

	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_Image_Table, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_Image_Table, Event
			return
			end
		else:
	endcase

p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
	no_regions = 1
endif else begin
	if ptr_valid( (*p)[0] ) eq 0 then no_regions=1
	if no_regions eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_regions=1
endelse
if no_regions eq 0 then obj = (*(*p)[0]).DevObj

  uname = widget_info( event.id, /uname)

  case uname of
	'Image_Table_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_Image_Table, Event
				end
			'WIDGET_BASE': begin
				OnSize_Image_Table, Event
				end
			else:
		endcase
		end

	'Image_Table': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_TABLE_CH': begin
			        OnChangeValue_Image_Table, Event
				end
			'WIDGET_TABLE_CELL_SEL': begin
				OnCellSelect_Image_Table, Event
				end
			else:
		endcase
		end

	'Mode_combobox': begin 
		OnSelect_Image_Table_Mode, Event
		end

	'ADC_combobox': begin
		OnSelect_Image_Table_ADC_Mode, Event
		end

	'Load_Button': begin
		OnButton_Image_Table_Load, Event
		end

	'Save_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Save, Event
		end

	'Update_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Update, Event
		end

	'Update_One_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Update_One, Event
		end

	'Export_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Export, Event
		end

	'Export_Regions_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Export_Regions, Event
		end

	'Import_Regions_Button': begin
		OnButton_Image_Table_Import_Regions, Event
		end

;	'Add_Button': begin
;		if no_regions then goto, finish
;		OnButton_Image_Table_Add, Event
;		end

	'Modify_combobox': begin
		OnSelect_Image_Table_modify_Mode, Event
		end

	'Modify_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Modify, Event
		end

	'Undo_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Modify_Undo, Event
		end

	'Delete_combobox': begin
		OnSelect_Image_Table_delete_Mode, Event
		end

	'Delete_Button': begin
		if no_regions then goto, finish
		case (*pstate).delete_mode of
			0: begin				; delete selected row(s)
				if (*pstate).spectra_too then begin
					new_table_pselect, pstate, select=sel
					*(*pstate).pdelete = {select:sel, keep:(*pstate).keep_zero}
					notify, 'image-region-delete', (*pstate).pdelete, from=event.top
				endif
				OnButton_Image_Table_Delete, Event, keep_zero=(*pstate).keep_zero
				if (*pstate).keep_zero then OnButton_Image_Table_Hotspot_Update, Event
				load_image_table_table, pstate
				end
			1: begin				; delete popup seletion
				OnButton_Image_Table_Delete, Event, select=sel, keep_zero=(*pstate).keep_zero
				if n_elements(sel) eq 0 then goto, finish
				if (*pstate).spectra_too then begin
					*(*pstate).pdelete = {select:sel, keep:(*pstate).keep_zero}
					notify, 'image-region-delete', (*pstate).pdelete, from=event.top
				endif
				new_table_pselect, pstate		
				if (*pstate).keep_zero then OnButton_Image_Table_Hotspot_Update, Event
				load_image_table_table, pstate
				end
			2: begin				; delete all except row #0
				if  widget_info( (*pstate).minimal_centroid_element, /valid) then begin
					widget_control, (*pstate).minimal_centroid_element, get_value=s
					stub1 = s + '-hotspots'
					stub2 = s + '-neighbourhood'
					OnButton_Image_Table_Delete_all, Event, /keep_first, clean=[stub2,stub1]
				endif else begin
					OnButton_Image_Table_Delete_all, Event, /keep_first
				endelse
				end
		endcase
		end

	'Spectra_too': begin
		(*pstate).Spectra_too = event.select
		end

	'Keep_zero': begin
		(*pstate).Keep_zero = event.select
		end

	'Clear_Button': begin
;		if no_regions then goto, finish
		OnButton_Image_Table_Clear, Event
		end

	'EVT_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_EVT, Event
		end

	'command_file_button':begin
		Image_Table_EVT, pstate, /gencom
		end

	'cluster': begin
		(*pstate).cluster = event.select
		if no_regions eq 0 then begin
			enable_cluster = obj->cluster()
		endif else enable_cluster=1
		if enable_cluster eq 0 then (*pstate).cluster = 0
		if widget_info( (*pstate).cluster_id, /valid) then widget_control, (*pstate).cluster_id, set_value=(*pstate).cluster, sensitive=enable_cluster
		end

	'Match_Button': begin
		if no_regions then goto, finish
		OnButton_Image_Table_Match, Event
		end

	else:
  endcase

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'image_table_event',['STATE variable has become ill-defined.','Abort Image Table.'],/error
	goto, kill
bad_ptr:
	warning,'image_table_event',['Parameter structure variable has become ill-defined.','Abort Image Table.'],/error
	goto, kill

kill:
	OnKill_Image_Table, event
	return
end

;---------------------------------------------------------------------

pro Image_Table, GROUP_LEADER=wGroup, TLB=Image_Table_TLB, path=path, pregions=pregions, $
					_EXTRA=_VWBExtra_, dpath=dpath, xoffset=xoffset, yoffset=yoffset, $
					realtime=realtime, xanes=xanes

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_geopixe_adcs, geopixe_max_adcs

image_table_eventcb     ; Load event callback routines

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
		warning,'Image_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  if n_elements(wGroup) lt 1 then wGroup = 0L
  if n_elements(path) lt 1 then path=''
  if n_elements(dpath) lt 1 then dpath=path
  if n_elements(realtime) lt 1 then realtime = 0
  if n_elements(xanes) lt 1 then xanes = 0

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
	default = geopixe_defaults( source='Image Table')

  case !version.os_family of
	'MacOS': begin
		fnt = 'COURIER*BOLD*10'
		help_xsize = 630
		com_xsize = 40
		end
	'unix': begin
		fnt = '6x10'
		yoff = yoff-20
		xoff = xoff+5
		help_xsize = 630
		com_xsize = 40
		end
	else: begin
		fnt = 'COURIER*10'
		help_xsize = 630
		com_xsize = 40
 		end
  endcase

if ptr_good( pregions) then begin
	p = (*pregions)[0]
	obj = (*p).DevObj
	clean_obj = 0
endif else begin
	obj = obj_new('MAIA_DEVICE')
	clean_obj = 1
endelse
list = ['individual','array', adc_list_device( obj)]
title = xanes ? 'XANES Stack Regions' : 'Image Regions'
  if n_elements(xoffset) lt 1 then begin
	xoffset = ((xoff+w) < (screen[0]-34 - 562)) > 0
  endif
  if n_elements(yoffset) lt 1 then yoffset = yoff

pregions = bad_pars_struct( pregions, make_pars=no_regions)

Image_Table_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Image_Table_TLB'  $
      ,/TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset $
      ,/TLB_SIZE_EVENTS ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER  $
      ,TITLE=title ,SPACE=2 ,XPAD=2 ,YPAD=2 ,COLUMN=1)

  Base1 = Widget_Base(Image_Table_TLB,  $
      UNAME='Image_Table_Button_Base1' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,/column)

  n_rows = 10000
  t = strarr(50,n_rows)

  Image_Table_Table = Widget_Table(Base1, UNAME='Image_Table'  $
      ,NOTIFY_REALIZE='OnRealize_Image_Table', /editable, /all_events $
      ,X_SCROLL_SIZE=8 ,Y_SCROLL_SIZE=6 ,value=t, scr_xsize=help_xsize	$	;	, font=fnt $
      ,/RESIZEABLE_COLUMNS, alignment=2, /tracking, uvalue='Table of concentrations in each region. ' + $
	  'Click on row to display the region shape (or highlighted pixels) on the image, and associated spectrum. Click on element column heading to sort and on "Image" heading to restore initial order.' )


  Image_Table_Button_Base = Widget_Base(Base1,  $
      UNAME='Image_Table_Button_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1)



tab_panel = widget_tab( Base1, location=0, /align_center, scr_xsize=help_xsize, uname='tab-panel')


; ---------------- Load/Save/Display  -------------------------------------------------------------------

general_base = widget_base( tab_panel, title=' Load/ Save/ Display ', /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=help_xsize)

g0base = widget_base( general_base, /row, /align_center, /base_align_center, ypad=0, xpad=0, space=5)

  Mode_combobox = widget_combobox(g0base, uname='Mode_combobox', /tracking, $
	value=[' Conc',' Error',' MDL',' Rel.Error',' Raw',' Centroid X',' Centroid Y  '], notify_realize='OnRealize_Image_Table_Mode', uvalue='Select display of "Conc", "Error", "MDL", "Raw", "Centroid X", "Centroid Y", etc.' )

  Spacer_1 = Widget_Base(g0base, UNAME='Spacer_2', SPACE=0 ,XPAD=0 ,YPAD=0, xsize=3)

  Load_Button = Widget_Button(g0base, /tracking, UNAME='Load_Button'  $
      ,/ALIGN_CENTER ,VALUE='Load', uvalue='Load regions from a REGION file.')

  Save_Button = Widget_Button(g0base, /tracking,  $
      UNAME='Save_Button' ,/ALIGN_CENTER ,VALUE='Save', uvalue='Save regions to a REGION file. Must save before using "Extract spectra".')

  Spacer_2a = Widget_Base(g0base, UNAME='Spacer_2a', SPACE=0 ,XPAD=0 ,YPAD=0, xsize=3)

  Update_Base = Widget_Base( g0base, UNAME='Image_Table_UButton_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, SPACE=1 ,XPAD=0 ,YPAD=0 , /ROW, uvalue='')

  label = widget_label( Update_Base, value='Update:')

  Update_One_Button = Widget_Button( Update_Base, /tracking,  $
      UNAME='Update_One_Button' ,/ALIGN_CENTER ,VALUE='One', uvalue='Apply selected region to image data, to update region table conc values, etc.')

  Update_Button = Widget_Button( Update_Base, /tracking,  $
      UNAME='Update_Button' ,/ALIGN_CENTER ,VALUE='All', uvalue='Apply all regions in turn to image data, to update region table conc values, etc. for all.')

  Spacer_2 = Widget_Base(g0base, UNAME='Spacer_2', SPACE=0 ,XPAD=0 ,YPAD=0, xsize=3)

  Export_Button = Widget_Button(g0base, /tracking,  $
      UNAME='Export_Button' ,/ALIGN_CENTER ,VALUE='Export Table', uvalue='Export table values for selected elements to a CSV file. You will be prompted for element columns to include.')

  Export2_Button = Widget_Button(g0base, /tracking,  $
      UNAME='Export_Regions_Button' ,/ALIGN_CENTER ,VALUE='Export Regions', uvalue='Export region parameters and pixel selections to CSV files, one file per region. ' + $
	  				'See notes in an Export regions CSV file for details.')

  Import_Button = Widget_Button(g0base, /tracking,  $
      UNAME='Import_Regions_Button' ,/ALIGN_CENTER ,VALUE='Import Regions', uvalue='Import region parameters and pixel selections from CSV files, one file per region. ' + $
	  				'See notes in an Export regions CSV file for details.')

;	if default.custom.enable then begin
;		if default.custom.lab eq 'XFM' then begin
;			XFM_Button = Widget_Button(g0base, /tracking,  $
;				UNAME='XFM_Button' ,/ALIGN_CENTER ,VALUE='XFM', uvalue='Export region coordinates to a CSV file to be imported into the XFM Scan Script spreadsheet.' )
;	  	endif
;	endif


;  Add_Button = Widget_Button(g0base, /tracking, UNAME='Add_Button' ,/ALIGN_CENTER ,VALUE='Add', uvalue='?')


; ---------------- Modify  -------------------------------------------------------------------

modify_base = widget_base( tab_panel, title=' Modify ', /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=help_xsize)

m0base = widget_base( modify_base, /row, /base_align_center, ypad=0, xpad=0, space=10)

  m1base = Widget_Base( m0base, UNAME='Image_Table_MButton_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, SPACE=1 ,XPAD=0 ,YPAD=0 , /ROW, uvalue='')

  Modify_Button = Widget_Button(m1base, /tracking,  $
      UNAME='Modify_Button' ,/ALIGN_CENTER ,VALUE='Modify:', uvalue='Modify the image plane of highlighted (Association) pixels of a selected region row, using mode selector droplist to the right. Click row label to select one entire row.')

  Mmode_combobox = widget_combobox(m1base, uname='Modify_combobox', /tracking, $
		value=[' Erode 1',' Erode 2',' Erode 3',' Erode5',' Erode 10',' Erode 20',' Erode 50',' Erode 100',' Dilate 1',' Dilate 2',' Dilate 3',' Dilate 5',' Dilate 10',' Dilate 20',' Dilate 50',' Dilate 100  '], scr_xsize=120, $
		notify_realize='OnRealize_Image_Table_MMode', uvalue='Select modify mode. Do modify of highlighted pixels using "Modify" button to the left. Select a row first.' )

  Undo_Button = Widget_Button(m0base, /tracking,  $
      UNAME='Undo_Button' ,/ALIGN_CENTER ,VALUE='Undo', uvalue='Undo last Modify operation.')


; ---------------- Delete  -------------------------------------------------------------------

delete_base = widget_base( tab_panel, title=' Delete ', /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=help_xsize)

d0base = widget_base( delete_base, /row, /base_align_center, ypad=0, xpad=0, space=10)

  label = Widget_label(d0base, value='     ')

  d1base = Widget_Base( d0base, UNAME='Image_Table_DButton_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, SPACE=1 ,XPAD=0 ,YPAD=0 , /ROW, uvalue='')

  Delete_Button = Widget_Button(d1base, /tracking,  $
      UNAME='Delete_Button' ,/ALIGN_CENTER ,VALUE='Delete:', uvalue='Delete selected region row(s), using mode selector droplist to the right. Also delete matching spectra, if "spectra too" is enabled. Click and drag to select mutiple cells and rows. Click row label to select one entire row.')

  Dmode_combobox = widget_combobox(d1base, uname='Delete_combobox', /tracking, $
	value=[' Selected row(s)',' Popup selection',' All except row #0  '], notify_realize='OnRealize_Image_Table_DMode', uvalue='Select delete mode. Do delete using "Delete" button to the left. Also delete matching spectra, if "spectra too" is enabled. For delete selection, drag select row(s) first.' )

  delete_map_base = Widget_Base( d0base, UNAME='Image_Table_DButton_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, SPACE=1 ,XPAD=0 ,YPAD=0 , /ROW, uvalue='')

  spectra_too_id = cw_bgroup2( delete_map_base, ['Spectra too'], /row, set_value=[1], /return_index, /tracking, $
  						uname='Spectra_too',/ nonexclusive, xpad=0, ypad=0, sensitive=enable_cluster, uvalue='Enable deleting matching spectra as well, by matching region number. Uncheck this if your spectra are unrelated to regions.' )
  spectra_too_id = cw_bgroup2( delete_map_base, ['Keep #0'], /row, set_value=[0], /return_index, /tracking, $
  						uname='Keep_zero',/ nonexclusive, xpad=0, ypad=0, sensitive=enable_cluster, uvalue='Protect region #0 from delete, and also update its contents to be sum of all other regions. Use this to maintain region #0 as ALL hotspots (similarly for spectra); NOT for "neighbourhoods".' )

  Clear_Button = Widget_Button(d0base, /tracking,  $
      UNAME='Clear_Button' ,/ALIGN_CENTER ,VALUE='Clear', uvalue='Clear entire region table')


; ---------------- Extract  -------------------------------------------------------------------

extract_base = widget_base( tab_panel, title=' Extract ', /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=help_xsize)

e0base = widget_base( extract_base, /row, /base_align_center, ypad=0, xpad=0, space=10)

if (xanes eq 0) and (realtime eq 0) then begin

  detectors_Base = Widget_Base( e0base, UNAME='Image_Table_EButton_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, SPACE=1 ,XPAD=0 ,YPAD=0 , /ROW, uvalue='')

  label = Widget_label(detectors_Base, value='Detectors:')

  adc_combobox = widget_combobox(detectors_Base, uname='ADC_combobox', /tracking, $
		value=' '+list+'  ', notify_realize='OnRealize_Image_Table_ADC', xsize=140, uvalue='Select detector for "Extract spectra", ' + $
		'"array" to merge all detectors in a detector array, "individual" for separate detector spectra JUST for selected region.')

  if ptr_good(pregions) then begin
		enable_cluster = Obj->cluster()
  endif else enable_cluster=1
	
  evt_Button = Widget_Button(e0base, /tracking,  $
      UNAME='EVT_Button' ,/ALIGN_CENTER ,VALUE='Extract spectra', uvalue='Extract spectra for all regions for selected detectors. ' + $
	  'Use the droplist to the left to select a single detector or "array" for all. Use "individual" to extract a separate spectrum for each detector (only for selected region).')

  cluster_id = cw_bgroup2( e0base, ['Cluster'], /row, set_value=[0], /return_index, /tracking, $
  						uname='cluster',/ nonexclusive, xpad=0, ypad=0, sensitive=enable_cluster, uvalue='Enable parallel processing for Extract for some devices, such as Maia.' )

  button = widget_button( e0base, value='C*', uname='command_file_button', /tracking, xsize=com_xsize, $
					uvalue='Generate a "GeoPIXE Command File" to extract region spectra for execution by GeoPIXE in a processing pipeline. ')

endif else begin
	cluster_id = 0L
	adc_combobox = 0L
endelse

if xanes then begin
	match_Button = Widget_Button(e0base, /tracking,  $
			UNAME='Match_Button' ,/ALIGN_CENTER ,VALUE='Match Centroids', uvalue='Match all XANES frames based on centroid XY.')
endif


help = widget_text( base1, scr_xsize=help_xsize, ysize=2, /wrap, uname='HELP', /tracking, uvalue='Help window. Displays info about widgets; move pointer over a widget to learn more. ' + $
			'Click row index to select region and spectrum. Click on column heading to sort table.',frame=0)


state = {	p:			pregions, $			; pointer array
			n:			0, $				; number of analyses loaded
			path:		ptr_new(path), $	; current path
			dpath:		ptr_new(dpath), $	; current raw data path
			file:		'', $				; current file name
			file_valid:	0, $				; file-name (in (*(*p)[0]).region_file) up to date

			pselect:	ptr_new(), $ 		; pointer to use to send 'image-region-select'
			pdelete:	ptr_new(/alloc), $	; pointer to use to send 'image-region-delete'
			pregions:	ptr_new(/alloc), $	; pointer to use to send 'image-regions'
			pspec:		ptr_new(), $ 		; pointer to use with notify 'spectra'
			local:		0, $				; flags local responsibility for spectra
			pexport:	ptr_new(/allocate_heap), $	; heap space for export element list
			pwiz:		ptr_new(/alloc), $	; pointer for Wizard return

			display_mode:	0, $			; table shows 0:conc, 1:error, 2:mdl
			mode_id:	0L, $				; mode droplist ID
			adc_combobox:	adc_combobox, $		; ADC select droplist ID
			cluster_id:	cluster_id, $		; cluster select ID
			table:		0L, $				; table ID
			group:		wGroup, $			; image parent TLB
			tlb:		Image_Table_TLB, $	; TLB ID
			help:		help, $				; help widget
			Dmode_combobox: Dmode_combobox, $ ; Delete mode droplist ID
			Mmode_combobox: Mmode_combobox, $ ; Modify mode droplist ID
			minimal_centroid_element: 0L, $	; will be set to hotspot_element ID on hotspot tab
			delete_map_base: delete_map_base, $ ; map base for checkboxes ID
			
			adc_offset:	4, $				; offset from droplist start to first ADC #0
			adc_prelist: ['odd rows','even rows','individual','array'], $	; in ADC droplist before ADC #0
			adc:		-1, $				; ADC droplist initial ADC (set to "array")
			cluster:	0, $				; cluster mode
			realtime:	realtime, $			; real-time mode
			Spectra_too: 1, $				; delete matching spectra too
			Keep_zero: 	0, $				; keep region #0 (and maintain #0 as sum of others, spectra too)
			delete_mode: 0, $				; delete droplist mode
			modify_mode: 0, $				; modify droplist mode
			pmodify:	ptr_new(/alloc), $	; modify undo buffer
			xanes:		xanes, $			; XANES mode parent and data
			rows:		256, $				; number of rows
			rows_max:	n_rows, $		; max number of rows
			columns:	50, $				; number of colums
			pcol_index:	ptr_new(/alloc), $	; index from column back to conc[] index
			row_height:	0, $				; table row height
			xoffset:	0, $				; offset in xsize for resize
			yoffset:	0, $				; offset in ysize for resize
			sel: {left:-1, top:-1, right:-1, bottom:-1 }, $	; use "(*pstate).sel.top" as current region
			cr_found:	0 }					; to fight MAC IDL bug

  state.pselect = ptr_new( { sel:state.sel, pregion:ptr_new() } )

  child = widget_info( Image_Table_TLB, /child)
  pstate = ptr_new(state, /no_copy)
  widget_control, child, set_uvalue=pstate
  
;	Add optional plugin GUI tabs here ...

	add_plugins = 0
	plugin_path = geopixe_root+'plugins'+slash()
	plugins = ptr_new()
	plugin_list = find_file2(plugin_path+'image_table_*_gui_plugin.sav')
	if plugin_list[0] eq '' then begin
		plugin_title = ['-- none --']
		print,'Image_Table: no GUI plugins found.'
	endif else begin
		nf = n_elements(plugin_list)
		print,'Image_Table:  process ',nf,' GUI plugin files ...'
		plugin_title = strarr(nf)

		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			warning,'GeoPIXE',['Errors detected in Image Table GUI plugins.', $
					'Check source plugin SAV files.','','Make sure version is less than or', $
					'equal to current IDL session,', $
					'and at least v6.0 for VM mode.']
			goto, cont_table
		endif

		for i=0L,nf-1 do begin
			print,'Image Table GUI: restore plugin: ', plugin_list[i]
			restore, plugin_list[i], /verbose
			plugin_list[i] = strip_path( plugin_list[i])
			plugin_list[i] = strip_file_ext( plugin_list[i])
			tab_new = call_function( plugin_list[i], tab_panel, pstate)
			print,'Image_Table: Added GUI for plugin: ', plugin_list[i]
		endfor
	endelse
cont_table:
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'Image_Table',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif else begin
		Catch, /cancel
	endelse

Widget_Control, /REALIZE, Image_Table_TLB

  register_notify, Image_Table_TLB, [ $
  					'image-results', $					; conc regions/results from image
  					'image-region-throttle', $			; throttle region results from image
					'path', $							; new path
					'dpath', $							; new raw data path
  					'image-results-clear', $			; clear all command from image
  					'image-update-results' $			; update data for current region
  					], from=wGroup
  register_notify, Image_Table_TLB, 'snapshot'
  register_notify, Image_Table_TLB, ['wizard-action']	; global notify from a wizard

  XManager, 'Image_Table', Image_Table_TLB  ,/no_block

	if clean_obj and obj_valid(obj) then obj_destroy, obj
	return
end
