function read_regions, F, path=path, error=err

;	Read Image Regions, as defined in Image_Table
;	
;	obsolete: For now, set discard=1 to not set device object parameters and widgets.
;	these are only done from image load.

ErrorNo = 0
err = 1
COMPILE_OPT STRICTARR
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
		warning,'Read_regions',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif
common c_null_image_1, max_image_cal
if n_elements(max_image_cal) lt 1 then t=define()

if n_params() lt 1 then return, 0
if lenchr(F) lt 1 then return, 0

	discard = 1						; do not set object parameters or widgets from file
	
	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	valid = [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20, $
		-21,-22,-23,-24,-25,-26,-27,-28,-29,-30]
	version = 0L
	readu,1, version
	q = where( version eq valid)
	if q[0] eq -1 then goto, bad_version

	new_line = 0
	if version le -11 then new_line=1

	zero = 0			
	if version le -24 then zero=0L			; change original size and nx,ny to long
	
	matrix = ' '
	if version le -2 then readu,1, matrix
	n = 0
	readu,1, n
	p = ptrarr(n)

	for i=0L,n-1 do begin
		mode = 0
		if version le -12 then begin
			readu,1, mode					; mode (0: image regions, 1: corr splines)
		endif

		type0 = 0
		readu,1, type0						; type 0

		case mode of
			0: begin						; image regions (mode 0)
				case type0 of
					1: begin
						mark0 = {present:0, x:fltarr(6), y:fltarr(6), theta:0.0 }		; box
						end
					2: begin
						mark0 = {present:0, x:fltarr(3), y:fltarr(3) }					; circle
						end
					3: begin
						mark0 = {present:0, x:fltarr(11), y:fltarr(11) }				; curve 8
						end
					4: begin
						if new_line then begin
							mark0 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }		; line
						endif else begin
							mark0 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0 }
						endelse
						end
					5: begin
						mark0 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0 }		; ellipse
						end
					6: begin
						mark0 = {present:0, x:fltarr(11), y:fltarr(11) }				; spline 10
						end
					7: begin
						mark0 = {present:0, x:fltarr(33), y:fltarr(33) }				; spline 32
						end
					8: begin
					    mark0 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 } ; X projection
						end
					9: begin
						mark0 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 } ; Y projection
						end						
					10: begin
						mark0 = {present:0, x:fltarr(101), y:fltarr(101) }				; spline 100
						end
					11: begin
						mark0 = {present:0, x:fltarr(1), y:fltarr(1) }					; S pixel
						end
					else:
				endcase
				readu,1, mark0
				end
			1: begin						; corr splines (mode 1)
				case type0 of
					0: begin
						mark0 = {present:0, x:fltarr(11), y:fltarr(11), cx:fltarr(11), cy:fltarr(11)}	; corr spline
						readu,1, mark0
						end
					1: begin
						mark0 = ''								; no shape for import region
						type0 = 0
						end
					else:
				endcase
				end
		endcase

		if (type0 eq 4) and (new_line eq 0) and (mode eq 0) then begin
			mark0 = {present:mark0.present, x:mark0.x, y:mark0.y, theta:mark0.theta, shear:0.0, curvature:0.0 }
		endif

		type1 = 0
		readu,1, type1						; type 1 (should be zero for corr spline, mode=1)

		mark1 = 0
		if type1 gt 0 then begin
			case type1 of
				1: begin
					mark1 = {present:0, x:fltarr(6), y:fltarr(6), theta:0.0 }	; box
					end
				2: begin
					mark1 = {present:0, x:fltarr(3), y:fltarr(3) }				; circle
					end
				3: begin
					mark1 = {present:0, x:fltarr(11), y:fltarr(11) }			; curve 8
					end
				4: begin
					if new_line then begin
						mark1 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 }		; line
					endif else begin
						mark1 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0 }
					endelse
					end
				5: begin
					mark1 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0 }	; ellipse
					end
				6: begin
					mark1 = {present:0, x:fltarr(11), y:fltarr(11) }			; spline 10
					end
				7: begin
					mark1 = {present:0, x:fltarr(33), y:fltarr(33) }			; spline 32
					end
				8: begin
				    mark1 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 } ; X projection
					end
				9: begin
					mark1 = {present:0, x:fltarr(5), y:fltarr(5), theta:0.0, shear:0.0, curvature:0.0 } ; Y projection
					end						
				10: begin
					mark1 = {present:0, x:fltarr(101), y:fltarr(101) }			; spline 100
					end
				11: begin
					mark1 = {present:0, x:fltarr(1), y:fltarr(1) }				; S pixel
					end
			else:
			endcase
			readu,1, mark1
			if (type1 eq 4) and (new_line eq 0) then begin
				mark1 = {present:mark1.present, x:mark1.x, y:mark1.y, theta:mark1.theta, shear:0.0, curvature:0.0 }
			endif
		endif

		el_shown = ''
		if version le -8 then readu,1, el_shown

		elx = ''
		ely = ''
		if (mode eq 1) and (version le -12) then begin
			readu,1, elx, ely
		endif
		note = ''
		if (version le -30) then begin
			readu,1, note
		endif

		n_el = 0
		readu,1, n_el
		el = strarr(n_el)
		readu,1, el
		conc = fltarr(n_el)
		error = fltarr(n_el)
		mdl = fltarr(n_el)
		ayield = fltarr(n_el)
		centroid = replicate( {X:0.0, Y:0.0}, n_el)
		readu,1, conc
		if version le -6 then begin
			readu,1, error
			readu,1, mdl
		endif
		if version le -22 then begin
			readu,1, centroid
		endif		
;												(*p).sd, (*p).relsd not saved/read
;												(used locally onlyby Standards Wizard)			
		aok = 1L
		if version le -28 then begin
			readu,1, aok
		endif
		if version le -27 then begin
			if aok then begin
				readu,1, ayield
			endif
		endif

		n_comp = 0L
		if version le -26 then begin
			readu,1, n_comp
			if n_comp gt 0 then begin
				phase = fltarr(n_comp)
				readu,1, phase
			endif
		endif

		nc = 0L & nel = 0L
		if version le -29 then begin
			readu,1, nc, nel
			if (nc gt 0) and (nel gt 0) then  begin
				overlay = fltarr(nc, nel)
				readu,1, overlay
			endif
		endif

		nx = zero
		ny = zero
		readu,1, nx, ny
		original_xsize = nx
		original_ysize = ny
		show_back = 0
		device_name = 'MPSYS_DEVICE'
		xoffset = 0L
		yoffset = 0L
		if version le -24 then begin
			readu,1, xoffset, yoffset
		endif
		
		xcompress = 0
		ycompress = 0
		readu,1, xcompress, ycompress
		ystep = 0
		if version le -13 then readu,1, ystep
		xstep_on = 0
		readu,1, xstep_on
		xstep = 0
		readu,1, xstep
		cal_a = 1.0
		cal_b = 0.0
		readu,1, cal_a, cal_b
		charge = 0.0
		readu,1, charge
		flux = 0.0
		if version le -17 then begin
			readu,1, flux
		endif

		IC = {	mode:			0, $		; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
				conversion:		1.0, $		; IC count to charge conversion
				pv: {	name:	'', $		; Epics PV name string
						val:	0.0, $		; PV value multipler
						unit:	0.0}}		; PV units, range value
		IC_mode = 0
		dwell = { on:	0, $				; dwell used (convert IC rate to count in a pixel)
				val:	0.0}				; dwell value (ms)
		np = 0

		if version le -18 then begin
			readu,1, IC_mode
			if IC_mode ne 0 then begin
				readu,1, IC
				print,'version -18: Region IC parameters= PV: ',IC.pv.name,' = ',IC.pv.val*IC.pv.unit
			endif
			readu,1, np
			if np gt 0 then begin
				sic = strarr(np)
				readu, 1, sic
			endif	
			readu,1, dwell
			print,'version -18: Region dwell = ',dwell.val
		endif
		
		obj = obj_new('MPSYS_DEVICE')
		if version le -21 then begin
			readu,1, device_name
			print,'version -20: device_name=',device_name

			if device_name eq 'none' then begin
				obj = obj_new()
			endif else begin
				obj = obj_new(device_name)
				if obj_valid(obj) eq 0 then goto, bad_obj
			endelse
		endif

;-------------- object specific parameter read -----------------
;
; Now all device specific options are stored and managed by the Object,
; and not explicitly known by GeoPIXE, except the pointer (*p).devpar.
; Also skip this for a null device object (e.g. due to import regions).

		if obj_valid(obj) then begin
			if version le -21 then begin
				if obj->name() eq 'MAIA_DEVICE' then begin
					use_version = 0
					if version le -23 then use_version=1
					dev_skip = 0
				endif else begin
					use_version = 1					; 'use_version' only varied for Maia device
					dev_skip = 0
					if version gt -25 then dev_skip = 1
				endelse
				if (dev_skip eq 0) then begin
					obj->set_versioning, use_version
					obj->read_options, 1, error=err
					obj->set_versioning, 1
					if err then goto, bad_obj_io
				endif
	
			endif else if version le -19 then begin
				obj = obj_new('MAIA_DEVICE')
				devpars = obj->get_options()
				maia = { encoder_y_correct:	0, $	; 1 = Y encoder mode
						x_margin:	5, $			; default X margin pixels in Y_mode=1
						clear: {x: 0, $				; clear X border
								y: 0}}				; clear Y border
				readu,1, maia
				
				if tag_present('ENCODER_Y_CORRECT',devpars) then devpars.encoder_y_correct = maia.encoder_y_correct
				if tag_present('X_MARGIN',devpars) then devpars.x_margin = maia.x_margin
				if tag_present('CLEAR',devpars) then devpars.clear.x = maia.clear.x
				if tag_present('CLEAR',devpars) then devpars.clear.y = maia.clear.y
				obj->set_options, devpars
			endif
		endif
		
;----------------------------------------------------------------

		file = ''
		readu,1, file
		source = ''
		source2 = ''
		ecompress = 1L
		throttle = ''
		pileup = ''
		linearize = ''
		if version le -10 then begin
			readu,1, source
			readu,1, source2
			readu,1, ecompress
		endif
		if version le -15 then begin
			readu,1, throttle
		endif
		if version le -16 then begin
			readu,1, pileup
			readu,1, linearize
		endif

		scaled_x = 1.0
		scaled_y = 1.0
		if version le -7 then begin
			readu,1, original_xsize, original_ysize
			readu,1, scaled_x, scaled_y
			readu,1, show_back
		endif

		if n_elements(path) gt 0 then file = path + strip_path( file)

		readu,1, q_encode							; 0 means 'q' NOT encoded
		if q_encode ne 0 then print,'Unknown "q" format'
		
		nq = 0L
		readu,1, nq
		q = lonarr(nq)
		readu,1, q

		step_events = 0
		events = 0L

		if version le -3 then begin
			readu,1, events, step_events
		endif

		step_toggle = 0L
		toggle_bit = 0L
		toggle_station = 0L
		if version le -4 then begin
			readu,1, step_toggle, toggle_bit, toggle_station
		endif

		scanx = 0.0
		scany = 0.0
		detector = 0
		channel = 0
		sample = '?'
		grain = '?'
		comment = ''
		if version le -5 then begin
			readu,1, scanx, scany
			readu,1, detector, channel
			readu,1, sample
			readu,1, grain
			readu,1, comment
		endif

		if version le -9 then begin
			if version gt -21 then begin
				device = 0
				readu,1, device
				print,'version -9: device=',device
				old = device_index_from_old_index( device, name=name, error=err)
				if err then print, '    Error converting old device index.' 
				device_name = name

				obj = obj_new(device_name)
				if obj_valid(obj) eq 0 then goto, bad_obj
				if n_elements(devpars) gt 0 then obj->set_options, devpars
			endif
		endif
		print,'all versions: device_name=',device_name

;	Now that device is known, for old version, write options to object NO !!
		
;		if update_maia and (discard eq 0) then begin
;			if obj->name() eq 'MAIA_DEVICE' then obj->set_options, maia
;		endif
		
		array = 0
		active = channel
		n_active = 0L
		cal = 0
		if version le -14 then begin
			readu,1, array
			if array eq 1 then begin
				readu,1, n_active
				active = lonarr(n_active)
				readu,1, active

				poly = fltarr(max_image_cal+1)
				cal0 = {order:1, units:'keV', poly:poly}
				cal = replicate(cal0, n_active)
				readu,1, cal
			endif
		endif

results = define(/table)
	results.mode = 				mode						; mode (0:image regions, 1:corr splines)
	results.file = 				file						; file name for image
	results.region_file =		F							; file name for the region file
	results.source =			source 						; start EVT file
	results.source2 =			source2 					; end EVT file
	results.throttle =			throttle 					; throttle file name
	results.pileup =			pileup 						; pileup limits file name
	results.linearize =			linearize 					; linearize gain file-name

	results.DevObj =			obj			 				; list-mode device object

	results.analyze_type =		[type0,type1]	 			; type for modes 0,1 (INCLUDE, EXCLUDE)
	results.el_shown =			el_shown				 	; element currently displayed
	results.elx =				elx						 	; corr X axis element name (corr mode 1)
	results.ely =				ely						 	; corr Y axis element name (corr mode 1)
	results.note =				note					 	; Note
	results.n_el =				n_el 						; # elements
	results.el =				ptr_new( el, /no_copy) 		; element names
	results.conc =				ptr_new( conc, /no_copy) 	; conc values
	results.error =				ptr_new( error, /no_copy) 	; error values
	results.mdl =				ptr_new( mdl, /no_copy) 	; MDL values

	results.centroid =			ptr_new( centroid, /no_copy) ; centroid (x,y) in pixels
	
	if n_comp gt 0 then begin
		results.phase =			ptr_new( phase, /no_copy) 	; phase proprtions
	endif
	if (nc gt 0) and (nel gt 0) then  begin
		results.poverlay =		ptr_new( overlay, /no_copy) ; overlay spectra
	endif
	if aok then begin
		results.ayield =		ptr_new( ayield, /no_copy) 	; 1/Y and Y averaged yields
	endif
	
	results.q =					ptr_new( q, /no_copy)		; q array
	results.nx =				nx 							; x size of image,q
	results.ny =				ny 							; y size of image,q
	results.xoffset =			xoffset						; X offset of this image segment in total image
	results.yoffset =			yoffset						; Y offset of this image "stripe"
	results.scanx =				scanx 						; X scan size (mm)
	results.scany =				scany 						; Y scan size (mm)
	results.xcompress =			xcompress 					; xcompress used in sort
	results.ycompress =			ycompress 					; ycompress used
	results.original_xsize = 	original_xsize 				; xsize after compress, before scale
	results.original_ysize = 	original_ysize 				; ysize
	results.scaled_x =			scaled_x 					; x scaled in image
	results.scaled_y =			scaled_y 					; y scaled
	results.show_back =			show_back
	results.sample =			sample 						; sample name
	results.grain =				grain 						; grain name
	results.comment =			comment 					; a comment
	results.channel =			channel 					; ADC channel
	results.detector =			detector 					; detector type (0=PIXE, 1=PIGE)
	results.ystep =				ystep 						; ystep mode on
	results.xstep_on =			xstep_on 					; was xstep/ystep used
	results.xstep =				xstep 						; xstep count
	results.step_events = 		step_events 				; step advance by events
	results.step_toggle = 		step_toggle 				; step advance by toggle bit
	results.toggle_bit = 		toggle_bit 					; toggle bit
	results.toggle_station = 	toggle_station 				; toggle station
	results.events =			events 						; stopped at 'events'
	results.cal_a =				cal_a 						; cal_a
	results.cal_b =				cal_b 						; cal_b
	results.ecompress=			ecompress 					; e spectrum compression
	results.charge =			charge 						; fractional charge
	results.IC_total =			flux	 					; fractional flux (IC count)
	results.IC.mode =			IC_mode 					; charge mode, PV used?
	if IC_mode ne 0 then begin
		results.IC =			IC		 					; IC PV details and preamp setting
	endif
	results.dwell =				dwell	 					; dwell details (from image)
	if np gt 0 then begin
		results.plist =			ptr_new(sic)				; PV string list (from image)
	endif
	results.array =				array 						; Is the image from a detector array
	results.pactive =			ptr_new( active, /no_copy)	; Pointer to Active channels in sort
	results.pcal =				ptr_new( cal, /no_copy)		; Pointer to active Cals
	results.pmark =				ptrarr(2)					; INC, EXCLUDE marker areas
	results.matrix =			matrix				 		; local file name of DA matrix

	results.index = i										; index in table

		p[i] = ptr_new( results, /no_copy)

		if (type1 gt 0) then begin
			(*p[i]).pmark[0] = ptr_new( mark0, /no_copy)
			(*p[i]).pmark[1] = ptr_new( mark1, /no_copy)
		endif else begin
			(*p[i]).pmark[0] = (var_type(mark0) eq 8) ? ptr_new( mark0, /no_copy) : ptr_new()
			(*p[i]).pmark[1] = ptr_new()
		endelse
	endfor
	
	close_file, 1
	err = 0
	return, p

bad_io: 
	print,'Read_Regions: bad region I/O'
	close_file, 1
	return, 0

bad_obj:
	print,'Read_Regions: device object not valid'
	close_file, 1
	return, 0

bad_obj_io:
	print,'Read_Regions: bad region object parameter I/O'
	close_file, 1
	return, 0

bad_version:
	print,'read_regions: bad version number'
	close_file, 1
	return, 0
end
