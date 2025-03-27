
;	Image_Routines, for Image.pro
;
;--------------------------------------------------------------------------------

pro Analyze_Image, pstate, i_update, throttle=throttle, error=aerror, $
				uniform_element=uniform_element, get_stats=wizard_stats_needed

;	What is this Throttle option?
;	Wizard call: /get_stats and pass 'i_uniform' element index (will check this elememnt for uniformity)

	COMPILE_OPT STRICTARR
	common c_hist_fudge, hist

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
			warning,'Analyze_image',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	aerror = 1
	p = (*pstate).p
	if ptr_valid( p) eq 0 then return
	if n_elements(throttle) lt 1 then throttle=0
	if n_elements(wizard_stats_needed) lt 1 then wizard_stats_needed=0
	if n_elements(uniform_element) lt 1 then uniform_element='nothing'

	if (*p).has_yield then begin
		if ptr_good((*p).yield) eq 0 then begin
			warning,'analyze_image',['MPDA image, but yield ptr bad.', 'Clearing has_yield, has_phase flags.']
			(*p).has_yield = 0
			(*p).has_phase = 0
		endif
	endif
	if (*p).has_phase then begin
		if ptr_good((*p).phase) eq 0 then begin
			warning,'analyze_image',['MPDA image, but phase ptr bad.', 'Clearing has_yield, has_phase flags.']
			(*p).has_phase = 0
			(*p).has_yield = 0
		endif
	endif

	; (*pstate).corr_mode = 0		Normal image regions
	;
	; (*pstate).corr_mode = 1		Corr spline regions
	;								Use (*pstate).qc for q, do not calculate q.

	xanes_stack_test, p, xanes, n_el, el, el_xanes

	if ptr_good( (*p).image ) eq 0 then return
	use_yield = 0
	if xanes eq 0 then begin
		if ((*p).mode eq 0) or ((*p).mode eq 3) then begin						; DA or MPDA mode
;		if ((*p).mode eq 0)  then begin											; DA mode
			if ptr_good( (*p).pda) eq 0 then begin
				ext = extract_extension((*p).matrix.file)
				file = file_requester( /read, /must_exist, filter='*.'+ext, group=(*pstate).tlb, $
					title='Select original '+ext+' file to append fit', file=(*p).matrix.file, fix_filter=0, $
					path=*(*pstate).path, /translate, updir=3, /skip_if_exists)
				da = read_DA( file, error=err)
				if err eq 0 then begin
					(*p).matrix.file = file
					(*p).pda = ptr_new( da, /no_copy)
				endif
			endif

			if ptr_good( (*p).pda) then begin
				matrix = *(*p).pda
				if size( matrix, /tname) eq 'STRUCT' then begin
					if ptr_good( matrix.pmore) then begin
						nda_extra = n_elements( *matrix.pmore)
						pdm = ptrarr(nda_extra)
						e = fltarr(nda_extra)
						for i=0,nda_extra-1 do begin
							pdm[i] = (*matrix.pmore)[i]
							e[i] = (*pdm[i]).e_beam
						endfor
						q = sort( abs( e - (*p).energy))
						if abs(e[q[0]]-(*p).energy) gt 0.002 then begin
							warning,'analyze_image',['Chosen stack DA matrix for XANES using poor energy match.', $
								'E requested ='+str_tidy(eb),'E match ='+str_tidy(e[q[0]])]
						endif
						matrix = *pdm[q[0]]
					endif
					use_yield = (*p).has_yield
				endif
			endif
		endif
	endif
	if ((*pstate).corr_mode eq 0) and ((*pstate).analyze_type[(*pstate).analyze_mode] lt 1) then return
	corr_mode = (*pstate).corr_mode

	if corr_mode eq 0 then begin
		if (*pstate).analyze_mode eq 0 then begin				; include mode
			mark_vertices, pstate, x1,y1, n
			if n eq 0 then return

			if n eq 1 then begin
				x = round(x1-0.5 > 0)
				y = round(y1-0.5 > 0)
				q = x + long(y)*(*p).xsize
			endif else begin
				x = round(x1)
				y = round(y1)
				q = polyfillv( x,y, (*p).xsize,(*p).ysize)
				if q[0] eq -1 then goto, no_points
			endelse

			;		q_to_xy,q,(*p).xsize,tx,ty
			;		print, tx,ty

			if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
			(*pstate).q = ptr_new(q)
			(*pstate).analyze_type[1] = 0						; corr_mode 0, use region q
		endif else begin										; exclude mode (multipass)
			mark_vertices, pstate, x1,y1, n
			if n eq 0 then return

			if n eq 1 then begin
				x = round(x1-0.5 > 0)
				y = round(y1-0.5 > 0)
				q2 = x + long(y)*(*p).xsize
			endif else begin
				x = round(x1)
				y = round(y1)
				q2 = polyfillv( x,y, (*p).xsize,(*p).ysize)
				if q2[0] eq -1 then goto, no_veto_points
			endelse

			if ptr_valid((*pstate).q) eq 0 then begin
				if ptr_valid((*pstate).qc) then begin
					*((*pstate).qc) = veto( q2, *((*pstate).qc))	; exclude some corr selection
					q = *(*pstate).qc
					corr_mode = 1
					draw_images, pstate
				endif else begin
					goto, missing_include
				endelse
			endif else begin
				*((*pstate).q) = veto( q2, *((*pstate).q))			; candidate for FORTRAN
				clear_mark, pstate, /both
				wset, (*pstate).wid2
				plot_mark, pstate
				q = *(*pstate).q									; corr_mode 0, use region q
			endelse
		endelse
	endif else begin												; corr_mode = 1

		if ptr_valid((*pstate).qc) then begin
			q = *(*pstate).qc										; corr_mode 1, use spline selection
		endif else return
	endelse

	; Update the (*p).temp struct with 'charge_map', 'total_pixels', 'total_flux'

	image_flux_charge, p, xanes=xanes, charge=charge, pixels=pixels, error=err
	if err then begin
		print,'analyze_image: "image_flux_charge" error; return.'
		return
	endif	
	print,'Charge=',charge,' Flux=',total(*(*p).temp.flux_map),' N pixels=',pixels

	; Mod here for Wakasa UNiDAQ data device images, with offset (not filling frame) images.
	; Now uses the 'bounds' to set the size of the image area (to fill out with charge), if
	; valid is set. Note that bounds are usually filled in, but not valid=1.

	pixeln = (*p).xsize * (*p).ysize
	if ((*p).bounds.valid eq 1) then begin
		q_accept = bounds_mask( p, /accept)
		if q_accept[0] eq -1 then goto, no_points
		;	pixels_for_charge = n_elements(q_accept)

		q2 = bounds_mask( p, /reject)
		q = veto( q2, q)											; veto selection within border
		if corr_mode eq 0 then begin
			if q[0] ne -1 then begin
				*((*pstate).q) = q
			endif else begin
				(*pstate).q = ptr_new()
			endelse
		endif
		if q[0] eq -1 then goto, no_points
	endif else begin
		;	pixels_for_charge = pixeln
	endelse

;	If 'uniform_element' is selected (called by Wizard_standards), then insist on this element being uniform.
;	This (using 'uniformity_mask(/reject)') constructs a mask of pixels to veto from the region list,
;	so that what remains is uniform.

	if xanes eq 0 then begin
		q1 = where( *(*p).el eq uniform_element, nq1)
		if nq1 gt 0 then begin
			nq = n_elements(q)
			q2 = uniformity_mask( p, q, q1[0], /reject)
			q = veto( q2, q)											; veto non-uniform pixels
			print,'analyze_image: Uniformity: Use '+str_tidy(n_elements(q))+' of '+str_tidy(nq)+' pixels.
		endif
	endif

	q2 = where( *(*p).temp.charge_map le 0.0, nq2)
	if nq2 gt 0 then q = veto( q2, q)								; veto zero charge pixels
	if q[0] eq -1 then goto, no_points

	if charge lt 1.0e-10 then begin
		warning,'Analyze_Image',['Charge is zero!','You need to set an integrated charge,', $
			'before running Sort EVT.','Will set charge to 1.0 now, and continue ...']
		charge = 1.0
	endif
	;flux = (*p).has_flux ? total( *(*p).flux) : (*p).temp.total_flux

	if ((*p).type eq 0) or ((*p).type eq 2) then begin				; ppm (0), counts (2)
		;	charge_per_pixel = charge / float(pixels_for_charge)
		;	flux_per_pixel = flux / float(pixels_for_charge)
		scale_charge = 1.0
		units = ' ppm'
	endif else begin												; e.g. mineral fraction (1), STIM energy (3)
		;	charge_per_pixel = 1.0
		;	flux_per_pixel = 1.0
		scale_charge = 1.0
		units = ' '
	endelse
	;print,'charge per pixel = ',charge_per_pixel

	if (corr_mode eq 0) and (((*pstate).analyze_type[0] eq 3) or ((*pstate).analyze_type[0] eq 4) or $
		((*pstate).analyze_type[0] eq 8) or ((*pstate).analyze_type[0] eq 9)) then begin		; line conc mode

		compress_by_2 = 0
		pixels = float(n_elements( q))
		if pixels lt 1.0 then goto, bad_shape
		;	fcharge = pixels * charge_per_pixel
		;	fflux = pixels * flux_per_pixel

		fcharge = total( (*(*p).temp.charge_map)[q])
		fflux = total( (*(*p).temp.flux_map)[q])

		;	Find 'x,y' coords of all points in selected region shape.
		;	The determine the 'z' coordinate along the line/curve for each 'x,y'.

		q_to_xy, q, (*p).xsize, x,y

		if ((*pstate).analyze_type[0] eq 3) then begin							; spline curve 8
			z = curve_projection( pstate, x,y, nc=n)
			nq = n_elements(x)
		endif else if ((*pstate).analyze_type[0] eq 4) then begin					; traverse
			pm = (*(*pstate).pmark[0])[4]
			rotatev, x,y, (*pm).x[0],(*pm).y[0], -(*pm).theta, xr0,yr0
			shearv, xr0,yr0, (*pm).x[0],(*pm).y[0], -(*pm).shear, xr,yr
			z = long(xr - min(xr))
		endif else if ((*pstate).analyze_type[0] eq 8) then begin					; project X
			pm = (*(*pstate).pmark[0])[8]
			z = long(x - min(x))
			if ptr_good((*p).px_coords) then begin
				touch = intarr(max(x)+1)
				touch[x] = 1
				qt = where( touch eq 1, nq)
				if nq gt 0 then begin
					x_coords = (*(*p).px_coords)[qt]
					x_coord_units = (*p).x_coord_units
				endif
			endif
		endif else if ((*pstate).analyze_type[0] eq 9) then begin					; project Y
			pm = (*(*pstate).pmark[0])[9]
			z = long(y - min(y))
			if ptr_good((*p).py_coords) then begin
				touch = intarr(max(y)+1)
				touch[y] = 1
				qt = where( touch eq 1, nq)
				if nq gt 0 then begin
					x_coords = (*(*p).py_coords)[qt]
					x_coord_units = (*p).y_coord_units
				endif
			endif
		endif

		;	Accumulate histogram along 'z' to integrate contents of all 'x,y' pixels that map to 'z'
		;	also integrate the 'charge_map' the same way.

		hist_xy, x,y,z, (*p).image, (*p).xsize,(*p).ysize,n_el, hist,nhist
		if (nhist[0] eq -1) or (hist[0] eq -1) then return
		hist_xy, x,y,z, (*p).temp.charge_map, (*p).xsize,(*p).ysize,1, hcharge,nchist
		if compress_by_2 then begin
			hist = compress2(hist,2)
			nhist = compress(nhist,2)
			hcharge = compress(hcharge,2) * scale_charge
		endif
		;	hcharge = nhist * charge_per_pixel * scale_charge

		if ((*p).type eq 1) or ((*p).type eq 3) then hcharge = float(nhist)

		n_el_none_blank = 1
		for i=0L,n_el-1 do begin
			if lenchr(el[i]) eq 0 then break
			n_el_none_blank = i+1
		endfor
		n_el = n_el_none_blank

		has_mdl = 0
		n = n_elements(hist[*,0])
		hmdl = fltarr(n,n_el)
		if ptr_good( (*p).matrix.mdl) then begin
			has_mdl = 1
			nmdl = n_elements(*(*p).matrix.mdl) - 1
			for i=0L,n_el-1 do begin
				xmdl = xanes ? (*(*p).matrix.mdl)[(*p).ixanes] : (*(*p).matrix.mdl)[i<nmdl]
				hmdl[*,i] = sqrt((*p).matrix.charge / hcharge) * xmdl
				hmdl[*,i] = smooth2( hmdl[*,i], 4)
			endfor
		endif
		for i=0L,n_el-1 do begin
			hist[*,i] = hist[*,i] / hcharge
		endfor

		has_errors = 0
		if (*p).has_errors eq 1 then begin
			x2 = x/2									; now do variance
			y2 = y/2
			xesize = n_elements( (*(*p).error)[*,0,0] )
			yesize = n_elements( (*(*p).error)[0,*,0] )
			b = bytarr(xesize,yesize)
			b[x2,y2] = 1
			q2 = where(b eq 1)
			q_to_xy, q2, xesize, x,y

			if ((*pstate).analyze_type[0] eq 3) then begin						; spline curve 8
				;			pm = (*(*pstate).pmark[0])[3]
				;			spline_p2, (*pm).x[0:8], (*pm).y[0:8], xc,yc, interval=1.4
				;			xc = xc/2
				;			yc = yc/2
				;			sx = shift(xc,-1)
				;			sy = shift(yc,-1)
				;			n2 = n_elements(xc)
				;			nq = n_elements(x)
				;			z = lonarr(nq)

				z = curve_projection( pstate, x,y, nc=n2)
				nq = n_elements(x)

				;			for i=0L,nq-1 do begin
				;				alpha = abs( angle_lines( xc[0:n2-2],yc[0:n2-2], sx[0:n2-2],sy[0:n2-2], x[i],y[i], /degrees))
				;				qa = sort(alpha)
				;				j = binary_search( alpha[qa], 90.)
				;				z[i] = qa[j]
				;			endfor
			endif else if ((*pstate).analyze_type[0] eq 4) then begin				; traverse
				pm = (*(*pstate).pmark[0])[4]
				z = long(x - min(x))
			endif else if ((*pstate).analyze_type[0] eq 8) then begin				; project X
				pm = (*(*pstate).pmark[0])[8]
				z = long(y - min(y))
			endif else if ((*pstate).analyze_type[0] eq 9) then begin				; project Y
				pm = (*(*pstate).pmark[0])[9]
				z = long(y - min(y))
			endif

			hist_xy, x,y,z, (*p).error, xesize,yesize,n_el, ehist,nehist
			if (nehist[0] ne -1) and (ehist[0] ne -1) then begin
				has_errors = 1
				;			ehcharge = nehist * 4.0*charge_per_pixel * scale_charge
				if compress_by_2 eq 0 then begin
					ehist = smart_congrid( ehist, n_elements(hist[*,0]), n_elements(ehist[0,*]))
					nehist = smart_congrid( nehist, n_elements(nhist))
				endif
				if (*pstate).display_mode eq 1 then begin							; 'var' displayed
					for i=0L,n_elements(ehist[0,*])-1 do begin										; so just plot histogram
						ehist[*,i] = ehist[*,i] / nehist							; of average var contents
					endfor															; which is 'variance' or 'yield', ...
				endif else begin
					for i=0L,n_elements(ehist[0,*])-1 do begin
						ehist[*,i] = sqrt( ehist[*,i]) / hcharge[*]
					endfor
				endelse
				n = min( [n, n_elements(ehist[*,0]) ])
			endif
		endif

		conc = reform(hist[0,*])
		centroid = replicate( {X:0.0, Y:0.0}, n_el)
		error = replicate(0.0, n_elements(hist[0,*]))
		xmdl = fltarr(n_el)
		if has_mdl then begin
			if xanes then begin
				xmdl = replicate((*(*p).matrix.mdl)[(*p).ixanes],n_el)
			endif else begin
				nm = n_elements(*(*p).matrix.mdl)
				xmdl[0:(nm<n_el)-1] = (*(*p).matrix.mdl)[0:(nm<n_el)-1]
			endelse
		endif
		mdl = xmdl * sqrt((*p).matrix.charge / hcharge[0])

		if strmid(el[0],0,4) eq 'Back' then hist[*,0] = 100000.0 * hist[*,0]
		if has_errors then begin
			error = reform(ehist[0,*])
			mdl = mdl < 3.*error
			if strmid(el[0],0,4) eq 'Back' then ehist[*,0] = 100000.0 * ehist[*,0]
		endif

		if (*pstate).display_mode eq 1 then begin									; var displayed
			has_errors = 0															; so veto error bars, etc.
			has_mdl = 0
			hist = ehist
			n_el = n_elements(ehist[0,*])
		endif

		null_spectrum = define(/spectrum)
		ps = ptrarr(n_el)
		for i=0L,n_el-1 do begin
			spec = null_spectrum
			sObj = clone_device_object( (*p).DevObj)
			spec.DevObj = sObj
			spec.source = (*p).source
			spec.source2 = (*p).source2
			spec.throttle = (*p).throttle
			spec.pileup = (*p).pileup
			spec.linearize = (*p).linearize
			spec.label = el[i]
			spec.cal.order = 1
			sc = 1.0
			if compress_by_2 then sc = 2.0
			if ((*p).scan.x gt 0.1) and ((*p).scan.y gt 0.1) then begin
				spec.cal.poly[0] = 0.0
				spec.cal.poly[1] = sc* 1000.0 * ((*p).scan.x/(*p).xsize)
				spec.cal.units = 'microns'
			endif else begin
				spec.cal.poly[0] = 0.0
				spec.cal.poly[1] = sc
				spec.cal.units = 'pixel'
			endelse
			spec.ecompress = (*p).ecompress
			spec.log = 0
			spec.charge = fcharge
			spec.IC_total = fflux
			spec.type = 1							; traverse
			spec.station = (*p).channel+1			; redundant?
			spec.channel = (*p).channel
			spec.sample = (*p).sample
			spec.grain = (*p).grain
			;		spec.x = (*p).
			;		spec.y = (*p).
			;		spec.z = (*p).
			;		spec.theta = (*p).
			;		spec.phi = (*p).
			spec.scan.x = (*p).scan.x
			spec.scan.y = (*p).scan.y
			if n_elements(x_coords) gt 0 then begin
				spec.px_coords = ptr_new( x_coords)
				spec.x_coord_units = x_coord_units
			endif

			spec.size = n
			spec.data = ptr_new( hist[0:n-1,i] )

			spec.has_errors = has_errors
			if has_errors then spec.error = ptr_new( ehist[0:n-1,i] )

			spec.has_mdl = has_mdl
			if has_mdl then spec.mdl = ptr_new( hmdl[0:n-1,i] )

			ps[i] = ptr_new( spec, /no_copy)
		endfor
		pp = ptr_new( ps, /no_copy)
		if ptr_valid( (*pstate).pline) then begin
			if (*(*(*pstate).pline)[0]).orphan eq 1 then begin
				(*pstate).llocal = 1
				(*(*(*pstate).pline)[0]).orphan = 0
			endif
			if ((*pstate).llocal eq 1) then free_spectra, (*pstate).pline
		endif
		(*pstate).pline = pp
		(*pstate).llocal = 0
		(*(*(*pstate).pline)[0]).orphan = 1

;		phase = fltarr(1)						; do not set these here, as they are
;		ayield = fltarr(n_el)					; tested a ptr_good() in 'build_da_fit'
		sd = fltarr(n_el)
		relsd = fltarr(n_el)
		use_yield = 0
		n_comp = 1

	endif else begin							; area conc mode

		conc = fltarr(n_el)
		error = fltarr(n_el)
		mdl = fltarr(n_el)
		sd = fltarr(n_el)
		relsd = fltarr(n_el)
		if xanes eq 0 then begin
			n_comp = ptr_good( (*p).phase) ? n_elements( (*(*p).phase)[0,0,*]) : 1L
		endif else n_comp = 1L
		phase = fltarr(n_comp)
;@3-16	ayield = fltarr(n_el)
		centroid = replicate( {X:0.0, Y:0.0}, n_el)
		if use_yield then overlay = fltarr(matrix.size, n_el)												;@3-16
		if (*p).has_phase then if ptr_good((*p).phase) then *(*p).phase = check_phase( *(*p).phase)			;@3-16

		;	Should this deadtime now be internal to a device?

		sObj = (*p).DevObj
		dname = sObj->name()							; device name
		deadtime_cal = sObj->get_deadtime_cal()			; cal in ns
		deadtime_scale= 1.0e-6							; convert to ms to match dwell

		;	Normally, flux and charge maps are deadtime/pileup corrected already. If the 'TT' image is found
		;	along with 'elements' and 'Dwell' (as in Maia Control or DAQ control), then we need to apply 
		;	a deadtime and/or pileup correction here. This is the case for the on-line Maia imaging.

		has_deadtime = 0
		has_pu = 0
		fdt = 0.0
		fpu = 0.0
		nactive = ptr_good( (*p).pactive) ? float( n_elements( *(*p).pactive)) : 1
		qtt = where( strlowcase(el) eq 'tt', nqtt)
		if nqtt ne 0 then begin
			qdwell = where( strlowcase(el) eq 'dwell', nqdwell)
			if nqdwell ne 0 then begin
				has_deadtime = 1
				tdwell = total( (*(*p).image)[q + qdwell[0]*pixeln], /NaN)
				fdt = deadtime_scale * (total( (*(*p).image)[q + qtt[0]*pixeln], /NaN) * deadtime_cal.a + deadtime_cal.b) / (nactive * tdwell)
				if fdt gt 0.8 then begin
					warning,'Analyze_Image',['Excessive "dead-time" or dead-time not calibrated.', $
						'Will ignore deadtime fraction ('+str_tidy(fdt)+').', $
						'','Make sure deadtime is calibrated in '+dname+'.']
					fdt = 0.0
				endif
			endif
		endif
		qpu = where( strlowcase(el) eq 'nnpu', nqpu)
		if nqpu ne 0 then begin
			qnn = where( strlowcase(el) eq 'nn', nqnn)
			if nqnn ne 0 then begin
				has_pu = 1
				tnn = total( (*(*p).image)[q + qnn[0]*pixeln], /NaN)
				fpu = total( (*(*p).image)[q + qpu[0]*pixeln], /NaN) / tnn
			endif
		endif

		n_el_none_blank = 1
		for i=0L,n_el-1 do begin
			if lenchr(el[i]) eq 0 then break
			n_el_none_blank = i+1
			if n_elements(q) lt 1 then goto, bad_shape
			if (*p).type eq 3 then begin									; energy (ignore zero pixels)
				qz = where( (*(*p).image)[q + i*pixeln] gt 0.0, nqz)
				if nqz lt 1 then goto, emore
				qe = q[qz]
			endif else begin
				qe = q
			endelse
			nqe = (qe[0] eq -1) ? 0 : n_elements(qe)
			if nqe eq 0 then goto, no_points								; @4-23

			q_to_xy, qe, (*p).xsize, x,y

			tix = total( ((*(*p).image)[qe + i*pixeln] > 0) )
			if tix gt 1.0e-20 then begin
				xav = total( float(x) * ((*(*p).image)[qe + i*pixeln] > 0) ) / tix
				yav = total( float(y) * ((*(*p).image)[qe + i*pixeln] > 0) ) / tix
			endif else begin
				tix = total( ((*(*p).image)[qe + i*pixeln] > 1.) )
				xav = total( float(x) * ((*(*p).image)[qe + i*pixeln] > 1.) ) / tix
				yav = total( float(y) * ((*(*p).image)[qe + i*pixeln] > 1.) ) / tix
			endelse
			centroid[i] = {x:xav, y:yav}

			if (*p).has_errors then begin
				x2 = x/2
				y2 = y/2
				xesize = n_elements( (*(*p).error)[*,0,0] )
				yesize = n_elements( (*(*p).error)[0,*,0] )
				b2 = bytarr( xesize, yesize)
				b2[x2,y2] = 1
				q2 = where(b2 eq 1)							; don't be tempted to use this in 'yield' below
				pixeln2 = xesize * yesize
			endif

			pixels = float(n_elements( qe))
			if pixels lt 1.0 then begin
				if (*p).type eq 3 then goto, emore
				goto, bad_shape
			endif
			;		fcharge = pixels * charge_per_pixel
			;		fflux = pixels * flux_per_pixel
			fcharge = total( (*(*p).temp.charge_map)[qe])
			fflux = total( (*(*p).temp.flux_map)[qe])

			fcharge = fcharge * (1. - fdt) * (1. - fpu)			; apply DT and PU correction, if needed
			fflux = fflux * (1. - fdt) * (1. - fpu)
			if i eq 0 then print,'fcharge=',fcharge,'   fflux=',fflux

			; Average yields, to be used later for spectra overlays, etc.
			; Within a pixel, we need ~1/Y averaging by phase, which accounts for self-absorption.
			; This is done now in 'da_evt', via 'da_accumulator9,10', which forms weighted yield images.
			; Between pixels, we assume independence largely, and we want arithmentic averages weighted by conc.

			if xanes eq 0 then begin
				if use_yield then begin
					
;@3-16				nbig = n_elements((*(*p).yield)[0,0,*])
;@3-16				if i lt nbig then begin
;						sum = total( (*(*p).image)[x,y,i])
;						yield = (*(*p).yield)[x2,y2,i]
;						ayield[i] = total( yield * (*(*p).image)[x,y,i]) / sum			; average Y across all pixel in region for element 'i'
;
;						Take care with indices here. Can't use above [x2,y2,i] form in IDL 'cos it's stupid.
;						But need to map each full image pixel onto the compressed ones, without missing any 
;						(i.e. need to include duplicates).
;						Use /Nan because some of 'yield' will be NaN from 'fny/(n_comp*invy)' in 'da_evt' (not any more).
;
;@3-16					sum = total( (*(*p).image)[qe + pixeln*i], /NaN)				; this average code may be noisy
;@3-16					yield = (*(*p).yield)[x2 + y2*xesize + pixeln2*i]
;@3-16					ayield[i] = total( yield * (*(*p).image)[qe + pixeln*i], /NaN) / sum
;					endif

;					k = contains( matrix.el, (*(*p).el)[i])			;@3-16

					q3 = where( (*(*p).el)[i] eq matrix.el, nq3)
					if (nq3 ge 2) and (i ge 1) then begin
						q4 = where( (*(*p).el)[i] eq (*(*p).el)[0:i-1], nq4)			; prior duplicates (e.g. "Compton")?
						if nq4 eq 0 then begin
							k = q3[0]													; if 'i' is first, use first
						endif else begin
							k = q3[nq3-1]												; if 'i' in not first, use last
						endelse					
					endif else k=q3[0]
;print,'element i=',i, n_elements(qe)
					for j=0,n_comp-1 do begin						;@3-16
								
;						This assumes that phase map is normalized previously (checked above) ...
;
;						(*(*p).image)[qe + i*pixeln]				; conc*Q per pixel vector qe, element i
;						(*(*p).yield)[x2 + y2*xesize + pixeln2*i]	; yield per pixel (larger pixels), element i
;						(*(*p).phase)[qe + j*pixeln]				; phase per pixel vector qe, phase j
;						matrix.pure[*,k,j]							; spec per element k, phase j

;						Need to correct Back DA matrix pure, which is still scaled by charge for each phases' DA matrix fit.
;						For all elements, including Back, charge is effectively included in image map. Hence, we do NOT
;						scale Back up by region 'fcharge'.
;
;						But this is now done in 'read_da' ...
;						sb = (strlowcase( strmid( (*(*p).el)[i],0,4) ) eq 'back') ? (mean(matrix.charge)/matrix.charge[j]) : 1.0	;@3-16
						sb =  1.0																									;@3-16
;print,'	comp j=',j
						overlay[*,i] = overlay[*,i] + sum_region_overlay( (*(*p).image)[qe + i*pixeln], $		; image				;@3-16
															(*(*p).yield)[x2 + y2*xesize + pixeln2*i], $		; yield
															(*(*p).phase)[qe + j*pixeln], $						; phase
															sb * matrix.pure[*,k,j]) 							; pure spectrum
					endfor	
				endif
			endif
;			if el[i] eq 'La' then begin
;				print, 'debug'
;			endif
			
;			What is this 'throttle' code? Is it remnant debug code? Is it still used with Throttle?
;			It comes from the "Image->Analyze->Throttle" menu (not the spectrum one).

			if throttle then begin
				conc[i] = max( median((*(*p).image)[qe + i*pixeln],5)) * pixels
			endif else begin
				conc[i] = total( (*(*p).image)[qe + i*pixeln], /NaN)
			endelse
			if xanes eq 0 then begin
				if ptr_good((*p).phase) then begin
					if i lt n_comp then phase[i] = total( (*(*p).phase)[qe + i*pixeln], /NaN) / pixels
				endif
			endif
			misc1 = strlowcase( special_elements(/counts))					; av counts for these
			qel1 = where( strlowcase(el[i]) eq misc1, nqel1)
			if ((nqel1 ne 0) or ((*p).type eq 3) ) then begin				; av counts for these
				conc[i] = conc[i] / pixels
				error[i] = sqrt( conc[i]) / pixels
				mdl[i] = 0.0
			endif else if ((*p).type eq 1) then begin						; av component fraction for minerals
				conc[i] = conc[i] / pixels
				if (*p).has_errors then begin
					error[i] = sqrt( total( (*(*p).error)[q2 + i*pixeln2], /NaN)) / pixels
				endif
			endif else begin												; treat like ppm
				conc[i] = conc[i] / (fcharge * scale_charge)
				if (*p).has_errors then begin
					error[i] = sqrt( total( (*(*p).error)[q2 + i*pixeln2], /NaN)) / (fcharge * scale_charge)
				endif
				mdl[i] = 2. * error[i]
				if ptr_good((*p).matrix.mdl) then begin
					if (n_elements(*(*p).matrix.mdl) gt i) then begin
						mdl[i] = sqrt((*p).matrix.charge / (fcharge * scale_charge)) * (*(*p).matrix.mdl)[i]
					endif
				endif
				mdl[i] = mdl[i] < 3.*error[i]
				
				if wizard_stats_needed then begin
					mask = intarr( (*p).xsize, (*p).ysize)
					mask[qe] = 1
					image_statistics, (*(*p).image)[*,*,i], mask=mask, mean=cav, stddev=csd
					av = cav * pixels /  (fcharge * scale_charge)
					sd[i] = csd * pixels /  (fcharge * scale_charge)
					relsd[i] = (sd[i]/sqrt(pixels)) / error[i]
				endif
			endelse

	emore:
			print,'  El=',el[i],'  	conc=',conc[i],' + ',error[i], '	(',mdl[i],' ) ', $
				units,',	min=',min((*(*p).image)[q + i*pixeln]), '	max=',max((*(*p).image)[q + i*pixeln]), $
				',	phase=',phase[i<(n_comp-1)]				;@3-16	, ',	yield=',ayield[i]
		endfor
		n_el = n_el_none_blank

		if has_deadtime then begin
			conc = [conc[0:n_el-1], fdt]
			error = [error[0:n_el-1],0.]
			mdl = [mdl[0:n_el-1],0.]
			sd = [sd[0:n_el-1],0.]
			relsd = [relsd[0:n_el-1],0.]
;			ayield = [ayield[0:n_el-1],0.]
			centroid = [centroid[0:n_el-1],{X:0.0,Y:0.0}]
			el = [el[0:n_el-1],'DT']
			n_el = n_el + 1
		endif
		if has_pu then begin
			conc = [conc[0:n_el-1], fpu]
			error = [error[0:n_el-1],0.]
			mdl = [mdl[0:n_el-1],0.]
			sd = [sd[0:n_el-1],0.]
			relsd = [relsd[0:n_el-1],0.]
;			ayield = [ayield[0:n_el-1],0.]
			centroid = [centroid[0:n_el-1],{X:0.0,Y:0.0}]
			el = [el[0:n_el-1],'PU']
			n_el = n_el + 1
		endif
		if (*p).has_flux then begin
			conc = [conc[0:n_el-1], fflux]
			error = [error[0:n_el-1],0.]
			mdl = [mdl[0:n_el-1],0.]
			sd = [sd[0:n_el-1],0.]
			relsd = [relsd[0:n_el-1],0.]
;			ayield = [ayield[0:n_el-1],0.]
			centroid = [centroid[0:n_el-1],{X:0.0,Y:0.0}]
			el = [el[0:n_el-1],'Flux']
			n_el = n_el + 1
		endif
	endelse

	if corr_mode eq 1 then begin
		pmark0 = ptr_good((*pstate).pcorr_mark) ? ptr_new( *(*pstate).pcorr_mark) : ptr_new()	; corr spline coordinates
		pmark1 = ptr_new()
	endif else begin
		if ptr_valid( (*(*pstate).pmark[1])[(*pstate).analyze_type[1]]) then begin
			pmark0 = ptr_new( *(*(*pstate).pmark[0])[(*pstate).analyze_type[0]])	; INCLUDE marker areas
			pmark1 = ptr_new( *(*(*pstate).pmark[1])[(*pstate).analyze_type[1]])	; EXCLUDE marker areas
		endif else begin
			pmark0 = ptr_new( *(*(*pstate).pmark[0])[(*pstate).analyze_type[0]])	; INCLUDE marker areas
			pmark1 = ptr_new()														; EXCLUDE marker areas
		endelse
	endelse

;	'corr_mode'=1 is also used for import regions with the absence of shape pmark data.
;	In this case 'elx' and 'ely' are both missing. Only if elx,ely are present is it a real corr.

	if corr_mode eq 1 then begin
		if (*pstate).elx eq '' and (*pstate).ely eq '' then begin					; @4-23
			el_shown = el[(*pstate).image]
		endif else begin
			el_shown = strcompress((*pstate).elx + '-' + (*pstate).ely, /remove_all)
		endelse
		analyze_type = [0,0]
	endif else begin
		el_shown = el[(*pstate).image]
		analyze_type = (*pstate).analyze_type
	endelse

	cal_ab, (*p).cal, ca,cb,cu, error=err			; convert cal to 'keV'
	if err then begin
		ca = 1.0
		cb = 0.0
		cu = ''
	endif

	active = (*p).channel
	cals = 0
	if (*p).array eq 1 then begin
		if ptr_valid( (*p).pactive) then active = *(*p).pactive
		if ptr_valid( (*p).pcal) then cals = *(*p).pcal
	endif
	sObj = clone_device_object( (*p).DevObj)

	results = define(/table)
	results.mode = 				corr_mode					; mode (0:image regions, 1:corr splines)
	results.file = 				(*pstate).file				; file name for image
	results.source =			(*p).source 				; start EVT file
	results.source2 =			(*p).source2 				; end EVT file
	results.throttle =			(*p).throttle 				; throttle file name
	results.pileup =			(*p).pileup 				; pileup limits file name
	results.linearize =			(*p).linearize 				; linearize gain file-name

	results.DevObj =			sObj		 				; list-mode device object

	if strlen( (*p).matrix.file) gt 1 then begin
		results.matrix =		(*p).matrix.file 			; local file name of DA matrix
	endif else begin
		results.matrix =		(*p).matrix.label 			; original file name of DA matrix
	endelse
	results.analyze_type =		analyze_type		 		; type for modes 0,1 (INCLUDE, EXCLUDE)
	results.el_shown =			el_shown				 	; element currently displayed
	results.elx =				(*pstate).elx			 	; corr X axis element name (corr mode 1)
	results.ely =				(*pstate).ely			 	; corr Y axis element name (corr mode 1)
	results.n_el =				n_el	 					; # elements/ XANES energy steps
	results.el =				ptr_new( el[0:n_el-1])	 				; element names, XANES energies
	results.conc =				ptr_new( conc[0:n_el-1], /no_copy) 		; conc values
	results.error =				ptr_new( error[0:n_el-1], /no_copy) 	; error values
	results.mdl =				ptr_new( mdl[0:n_el-1], /no_copy) 		; MDL values

	results.sd =				ptr_new( sd[0:n_el-1], /no_copy) 		; std.dev values (not saved, used in Wizard)
	results.relsd =				ptr_new( relsd[0:n_el-1], /no_copy) 	; SD/Poisson values
	results.centroid = 			ptr_new( centroid[0:n_el-1], /no_copy)	; centroids (pixels) of region, weighted for each elements
	
	if xanes eq 0 then begin												;@3-16
		if use_yield then begin												;@3-16
			if (n_comp gt 1) then begin										;@3-16
				results.phase =				ptr_new( phase, /no_copy) 		; average phase proportion values
;				results.ayield =			ptr_new( ayield, /no_copy) 		; average yield values			;@3-16
				results.poverlay =			ptr_new( overlay, /no_copy) 	; overlay spectra				;@3-16
			endif
		endif
	endif
	
	results.q =					ptr_new( q, /no_copy)		; q region index array
	results.nx =				(*p).xsize 					; x size of image,q
	results.ny =				(*p).ysize 					; y size of image,q
	results.xoffset =			(*p).xoffset				; X offset of this image segment in total image
	results.yoffset =			(*p).yoffset				; Y offset of this image "stripe"
	results.scanx =				(*p).scan.x 				; X scan size (mm)
	results.scany =				(*p).scan.y 				; Y scan size (mm)
	results.xcompress =			(*p).xcompress 				; xcompress used in sort
	results.ycompress =			(*p).ycompress 				; ycompress used
	results.original_xsize = 	(*p).original_xsize 		; xsize after compress, before scale
	results.original_ysize = 	(*p).original_ysize 		; ysize
	results.scaled_x =			(*p).scaled_x 				; x scaled in image
	results.scaled_y =			(*p).scaled_y 				; y scaled
	results.show_back =			(*p).show_back
	results.sample =			(*p).sample 				; sample name
	results.grain =				(*p).grain 					; grain name
	results.comment =			(*p).comment 				; a comment
	results.channel =			(*p).channel 				; ADC channel
	results.detector =			(*p).detector 				; detector type (0=PIXE, 1=PIGE)
	results.ystep =				(*p).ystep	 				; ystep mode on
	results.xstep_on =			(*p).xstep_on 				; was xstep/ystep used
	results.xstep =				(*p).xstep 					; xstep count
	results.step_events = 		(*p).step_events 			; step advance by events
	results.step_toggle = 		(*p).step_toggle 			; step advance by toggle bit
	results.toggle_bit = 		(*p).toggle_bit 			; toggle bit
	results.toggle_station = 	(*p).toggle_station 		; toggle station
	results.events =			(*p).events 				; stopped at 'events'
	results.cal_a =				ca 							; cal_a
	results.cal_b =				cb	 						; cal_b
	results.ecompress=			(*p).ecompress 				; e spectrum compression
	results.charge =			fcharge 					; fractional charge
	results.IC_total =			fflux	 					; fractional flux (IC count)
	results.IC =				(*p).IC	 					; IC PV details from image
	results.dwell =				(*p).dwell	 				; dwell details from image
	if ptr_good((*p).plist) then begin
		results.plist =			ptr_new( *(*p).plist)		; PV list from image
	endif
	results.array =				(*p).array 					; Is the image from a detector array
	results.pactive =			ptr_new(active)				; Pointer to Active channels in sort
	results.pcal =				ptr_new(cals)				; Pointer to active Cals
	results.pmark =				[pmark0,pmark1]				; INC, EXCLUDE marker areas

	presults = ptr_new(results, /no_copy)
	if n_elements(i_update) lt 1 then begin
		no_results = 0
		if n_elements( *(*pstate).pregions) eq 0 then no_results=1
		if no_results eq 0 then if ptr_valid( (*(*pstate).pregions)[0]) eq 0 then no_results=1
		if no_results then begin
			(*presults).index = 0
			*(*pstate).pregions = presults
		endif else begin
			(*presults).index = n_elements(*(*pstate).pregions)
			*(*pstate).pregions = [ *(*pstate).pregions, presults]
		endelse
	endif else begin
		if n_elements( *(*pstate).pregions)-1 ge i_update then begin
			if ptr_valid( (*(*pstate).pregions)[i_update]) then begin
				lindex = (*(*(*pstate).pregions)[i_update]).index 
				free_region_entry, (*pstate).pregions, i_update
				(*presults).index = lindex
				(*(*pstate).pregions)[i_update] = presults
			endif
		endif
	endelse
	aerror = 0
	return

missing_include:
	warning,'Analyze_Image','Need to do INCLUDE analyze a region first!.'
	return
no_points:
	warning,'Analyze_Image',['No valid points included (zero q).','For region #'+str_tidy(i_update),'', $
		'Or, no pixels selected have valid charge/flux.', $
		'Check charge/flux settings for this image.','You need to set an integrated charge,', $
		'before running Sort EVT.']
	return
no_veto_points:
	warning,'Analyze_Image','No points in exclude area (zero q2).'
	return
bad_shape:
	warning,'Analyze_Image','Bad shape, no points included.'
	return
bad_charge:
	warning,'Analyze_Image',['Charge is zero!','You need to set an integrated charge,', $
		'before running Sort EVT.']
	return
end

;-----------------------------------------------------------------
; Build q indices for image pointer 'p' for XY points
;	'accept' within the bounds, or
;	'reject' outside it

function bounds_mask, p, reject=reject, accept=accept

	if ptr_valid(p) eq 0 then return, -1L
	if n_elements(reject) eq 0 then reject=0
	if n_elements(accept) eq 0 then accept=0
	if accept eq 1 then reject=0
	if reject eq 0 then accept=1

	mask = bytarr( (*p).xsize, (*p).ysize)
	mask[*] = 1
	xmin = clip((*p).bounds.xmin - (*p).xoffset,0,(*p).xsize-1)
	xmax = clip((*p).bounds.xmax - (*p).xoffset,0,(*p).xsize-1)
	ymin = clip((*p).bounds.ymin - (*p).yoffset,0,(*p).ysize-1)
	ymax = clip((*p).bounds.ymax - (*p).yoffset,0,(*p).ysize-1)

	if xmin gt 0 then mask[0:xmin-1, *] = 0
	mask[xmax+1: (*p).xsize-1, *] = 0

	if ymin gt 0 then mask[*, 0:ymin-1] = 0
	mask[*, ymax+1: (*p).ysize-1] = 0

	if accept then begin
		q = where(mask eq 1)			; use only pixels within 'bounds'
	endif else if reject then begin
		q = where(mask eq 0)
	endif

	return, q
end

;-----------------------------------------------------------------
; Build list of box vertices (in image coords)

pro box_vertices, pstate, x,y, n

;	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 1 then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	;  0-3 are corners, 4 is centre handle, 5 is rotate handle

	x = [(*p).x[0:3], (*p).x[0] ]
	y = [(*p).y[0:3], (*p).y[0] ]
	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;-----------------------------------------------------------------

pro call_image_plugin

end
;-----------------------------------------------------------------

pro check_mode, pstate

	if ptr_valid( (*pstate).p) then begin
		if ptr_valid( (*(*pstate).p).error) eq 0 then (*(*pstate).p).has_errors = 0
		was = (*pstate).display_mode
		if (*(*pstate).p).has_errors eq 0 then (*pstate).display_mode = 0
		if (*pstate).display_mode ne was then begin
			widget_control, (*pstate).mode_id, set_combobox_select = (*pstate).display_mode
		endif
	endif

	return
end

;-----------------------------------------------------------------
; Build list of circle vertices (in image coords)

pro circle_vertices, pstate, x,y, n

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 2 then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	;  0-1 are diameters, 2 is centre handle

	circle_diam, (*p).x[0],(*p).y[0], (*p).x[1],(*p).y[1], x,y
	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;-----------------------------------------------------------------

pro clear_image_border, img, ri, sides=sides

	if n_elements(sides) eq 0 then sides=0

	r = abs([ri])
	sx = n_elements(img[*,0])
	sy = n_elements(img[0,*])

	img[0:r,0:sy-1] = 0.						; clear side borders
	img[sx-r:sx-1,0:sy-1] = 0.
	if sides eq 0 then begin
		img[0:sx-1,0:r] = 0.					; clear top and bottom borders
		img[0:sx-1,sy-r:sy-1] = 0.
	endif

	return
end

;-----------------------------------------------------------------

pro clear_all_markers, pstate

	clear_mark, pstate, /both
	for j=0L,1 do begin
		(*pstate).analyze_mode = j
		for i=1L,(*pstate).max_set do begin
			(*pstate).analyze_type[j] = i
			clear_mark, pstate, /init, /zero
		endfor
	endfor

	(*pstate).analyze_mode = 0
	(*pstate).analyze_type[0] = 0
	(*pstate).analyze_type[1] = 0
	clear_mark, pstate, /init, /zero

	if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc

	widget_control, (*pstate).analyze_type_id, set_combobox_select=0
	widget_control, (*pstate).analyze_mode_id, set_combobox_select=0
	return
end

;-----------------------------------------------------------------
; clear marker areas by copying a rectangle. Normally copy from
; (*pstate).pix to (*pstate).wid2, unless 'from' and 'to' used.
;
;   /init		initial clear (no markers are drawn yet) to zero 'present'
;   /zero		zero the marker vertex arrays, etc.
;   /both		clear markers for both modes.

pro clear_mark, pstate, init=init, zero=zero, both=both, from=from, to=to

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
			warning,'Clear_marker',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(init) lt 1 then init=0
	if n_elements(both) lt 1 then both=0
	if n_elements(zero) lt 1 then zero=0

	;if ((*pstate).analyze_type[(*pstate).analyze_mode] lt 1) $
	;		and ((*pstate).analyze_mode eq 1) then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	case (*pstate).analyze_type[(*pstate).analyze_mode] of
		0: clear_distance, pstate, init=init, zero=zero, from=from, to=to
		1: clear_box, pstate, init=init, zero=zero, from=from, to=to
		2: clear_circle, pstate, init=init, zero=zero, from=from, to=to
		3: clear_curve, pstate, init=init, zero=zero, from=from, to=to
		4: clear_line, pstate, init=init, zero=zero, from=from, to=to
		5: clear_ellipse, pstate, init=init, zero=zero, from=from, to=to
		6: clear_spline, pstate, init=init, zero=zero, from=from, to=to
		7: clear_spline, pstate, init=init, zero=zero, from=from, to=to
		8: clear_line, pstate, init=init, zero=zero, from=from, to=to
		9: clear_line, pstate, init=init, zero=zero, from=from, to=to
		10: clear_spline, pstate, init=init, zero=zero, from=from, to=to
		11: clear_spoint, pstate, init=init, zero=zero, from=from, to=to
		;	12: clear_mpoint, pstate, init=init, zero=zero, from=from, to=to
		else:
	endcase

	if both then begin
		(*pstate).analyze_mode = 1 - (*pstate).analyze_mode
		case (*pstate).analyze_type[(*pstate).analyze_mode] of
			0: clear_distance, pstate, init=init, zero=zero, from=from, to=to
			1: clear_box, pstate, init=init, zero=zero, from=from, to=to
			2: clear_circle, pstate, init=init, zero=zero, from=from, to=to
			3: clear_curve, pstate, init=init, zero=zero, from=from, to=to
			4: clear_line, pstate, init=init, zero=zero, from=from, to=to
			5: clear_ellipse, pstate, init=init, zero=zero, from=from, to=to
			6: clear_spline, pstate, init=init, zero=zero, from=from, to=to
			7: clear_spline, pstate, init=init, zero=zero, from=from, to=to
			8: clear_line, pstate, init=init, zero=zero, from=from, to=to
			9: clear_line, pstate, init=init, zero=zero, from=from, to=to
			10: clear_spline, pstate, init=init, zero=zero, from=from, to=to
			11: clear_spoint, pstate, init=init, zero=zero, from=from, to=to
			;		12: clear_mpoint, pstate, init=init, zero=zero, from=from, to=to
			else:
		endcase
		(*pstate).analyze_mode = 1 - (*pstate).analyze_mode
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a distance line.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_distance, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
	if (from eq 0) or (to eq 0) then return

	if (*pstate).analyze_mode eq 1 then return
	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 0 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		xy_to_pixel, pstate, (*p).x,(*p).y, x,y
		wset, to
		minx = clip( (min(x) - 4), 0, (*pstate).width)
		miny = clip( (min(y) - 4), 0, (*pstate).height)
		maxx = clip( (max(x) + 4), 0, (*pstate).width)
		maxy = clip( (max(y) + 4), 0, (*pstate).height)
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a box shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_box, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
;	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 1 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		xy_to_pixel, pstate, (*p).x,(*p).y, x,y
		wset, to
		minx = clip( (min(x) - 4), 0, (*pstate).width)
		miny = clip( (min(y) - 4), 0, (*pstate).height)
		maxx = clip( (max(x) + 4), 0, (*pstate).width)
		maxy = clip( (max(y) + 4), 0, (*pstate).height)
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
			(*p).theta = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a circle shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_circle, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 2 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		circle_vertices, pstate, x,y,n
		xy_to_pixel, pstate, x,y, px,py
		wset, to
		minx = clip( (min(px) - 4), 0, (*pstate).width)
		miny = clip( (min(py) - 4), 0, (*pstate).height)
		maxx = clip( (max(px) + 4), 0, (*pstate).width)
		maxy = clip( (max(py) + 4), 0, (*pstate).height)
		;	print,'clear_circle: min,max=',minx,maxx,miny,maxy
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a spline shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_spline, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
	if ((*pstate).analyze_type[(*pstate).analyze_mode] ne 6) $
		and ((*pstate).analyze_type[(*pstate).analyze_mode] ne 7)  and ((*pstate).analyze_type[(*pstate).analyze_mode] ne 10) then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		spline_vertices, pstate, x,y,n
		xy_to_pixel, pstate, x,y, px,py
		wset, to
		minx = clip( (min(px) - 4), 0, (*pstate).width)
		miny = clip( (min(py) - 4), 0, (*pstate).height)
		maxx = clip( (max(px) + 4), 0, (*pstate).width)
		maxy = clip( (max(py) + 4), 0, (*pstate).height)
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy point from 'from' to 'to' to clear a Spoint shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_spoint, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
;	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 11 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		xy_to_pixel, pstate, (*p).x,(*p).y, x,y
		w = 4 + 2^(*pstate).zoom
		wset, to
		minx = clip( (min(x) - w), 0, (*pstate).width)
		miny = clip( (min(y) - w), 0, (*pstate).height)
		maxx = clip( (max(x) + w), 0, (*pstate).width)
		maxy = clip( (max(y) + w), 0, (*pstate).height)
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a line shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_curve, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 3 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		curve_vertices, pstate, x,y,n
		if n gt 0 then begin
			xy_to_pixel, pstate, x,y, px,py
			wset, to
			minx = clip( (min(px) - 4), 0, (*pstate).width)
			miny = clip( (min(py) - 4), 0, (*pstate).height)
			maxx = clip( (max(px) + 4), 0, (*pstate).width)
			maxy = clip( (max(py) + 4), 0, (*pstate).height)
			device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
		endif
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a line shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_line, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
	if (*pstate).analyze_type[(*pstate).analyze_mode] eq 4 then goto, cont
	if (*pstate).analyze_type[(*pstate).analyze_mode] eq 8 then goto, cont
	if (*pstate).analyze_type[(*pstate).analyze_mode] eq 9 then goto, cont
	return
	cont:
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		line_vertices, pstate, x,y,n
		xy_to_pixel, pstate, x,y, px,py
		wset, to
		minx = clip( (min(px) - 4), 0, (*pstate).width)
		miny = clip( (min(py) - 4), 0, (*pstate).height)
		maxx = clip( (max(px) + 4), 0, (*pstate).width)
		maxy = clip( (max(py) + 4), 0, (*pstate).height)
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
			(*p).theta = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a projectX shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_ellipse, pstate, init=init, zero=zero, from=from, to=to

	if n_elements(init) lt 1 then init=0
	if n_elements(zero) lt 1 then zero=0
	if n_elements(from) lt 1 then begin
		from = (*pstate).pix
	endif
	if n_elements(to) lt 1 then begin
		to = (*pstate).wid2
	endif
	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 5 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	if init eq 0 then begin
		ellipse_vertices, pstate, x,y,n
		xy_to_pixel, pstate, x,y, px,py
		wset, to
		minx = clip( (min(px) - 4), 0, (*pstate).width)
		miny = clip( (min(py) - 4), 0, (*pstate).height)
		maxx = clip( (max(px) + 4), 0, (*pstate).width)
		maxy = clip( (max(py) + 4), 0, (*pstate).height)
		device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
	endif

	if ptr_good(p) then begin
		if zero then begin
			(*p).x[*] = 0.0
			(*p).y[*] = 0.0
			(*p).theta = 0.0
		endif
		(*p).present = 0
	endif
	return
end

;-----------------------------------------------------------------
; Build list of curve traverse vertices (in image coords)

pro curve_vertices, pstate, x,y, n, clx=xc,cly=yc

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 3 then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then begin
		n = 0
		return
	endif

	;  0-8 are spline pts, 9,10 are width/curvature handles

	spline_p2, (*p).x[0:8], (*p).y[0:8], txc,tyc

	theta = atan( (*p).y[10]-(*p).y[9], (*p).x[10]-(*p).x[9])
	rotatev, (*p).x,(*p).y, (*p).x[4],(*p).y[4], -theta, xr,yr

	w = 0.5* (xr[10] - xr[9])
	delta = yr[4] - 0.5*(yr[9]+yr[10])
	if (w gt 1.) and (abs(delta) gt 0.5) then begin
		if (delta gt 0.5) then begin
			R = 0.5*(w+delta)*(w-delta)/delta > (yr[4]-yr[0]+2)*1.05
			print,'delta +ve, R=',R,'  limit=', (yr[4]-yr[0]+2)*1.05
		endif else if (delta lt -0.5) then begin
			R = 0.5*(w+delta)*(w-delta)/delta < (yr[4]-yr[8]-2)*1.05
			print,'delta -ve, R=',R,'  limit=', (yr[4]-yr[8]-2)*1.05
		endif
		xo = xr[4]
		yo = yr[4] - R
		phi = asin( clip(w/R,-1.,1.) )
		print,'phi=',phi
		rotatev, xr[0:8],yr[0:8], xo,yo, -phi, xrt,yrt
		rotatev, xr[0:8],yr[0:8], xo,yo, phi, xrb,yrb
		rotatev, xrt,yrt, (*p).x[4],(*p).y[4], theta, xt,yt
		rotatev, xrb,yrb, (*p).x[4],(*p).y[4], theta, xb,yb
	endif else begin
		rotatev, xr[0:8]+w,yr[0:8], (*p).x[4],(*p).y[4], theta, xt,yt
		rotatev, xr[0:8]-w,yr[0:8], (*p).x[4],(*p).y[4], theta, xb,yb
	endelse
	print,'xt,yt minmax=',min(xt),max(xt),min(yt),max(yt)
	print,'xb,yb minmax=',min(xb),max(xb),min(yb),max(yb)

	spline_p2, xt,yt, xts,yts
	spline_p2, xb,yb, xbs,ybs

	tt0 = [yt[0]-yt[1],xt[1]-xt[0]]/sqrt((yt[0]-yt[1])^2 + (xt[1]-xt[0])^2)
	tb0 = [yb[0]-yb[1],xb[1]-xb[0]]/sqrt((yb[0]-yb[1])^2 + (xb[1]-xb[0])^2)
	spline_p2, [xt[0],(*p).x[0],xb[0]],[yt[0],(*p).y[0],yb[0]], xc1,yc1, tan0=tt0,tan1=tb0
	tt8 = [yt[7]-yt[8],xt[8]-xt[7]]/sqrt((yt[7]-yt[8])^2 + (xt[8]-xt[7])^2)
	tb8 = [yb[7]-yb[8],xb[8]-xb[7]]/sqrt((yb[7]-yb[8])^2 + (xb[8]-xb[7])^2)
	spline_p2, [xt[8],(*p).x[8],xb[8]],[yt[8],(*p).y[8],yb[8]], xc2,yc2, tan0=tt8,tan1=tb8

	x = [xbs,reverse(xc2),reverse(xts),xc1]						; outline curved box
	y = [ybs,reverse(yc2),reverse(yts),yc1]
	n = n_elements(x)

	nb8 = n_elements(txc)/8
	k = nb8
	xc = txc[0:nb8-1]
	yc = tyc[0:nb8-1]

	for i=1,7 do begin
		tt0 = [yt[i-1]-yt[i],xt[i]-xt[i-1]]/sqrt((yt[i-1]-yt[i])^2 + (xt[i]-xt[i-1])^2)
		tb0 = [yb[i-1]-yb[i],xb[i]-xb[i-1]]/sqrt((yb[i-1]-yb[i])^2 + (xb[i]-xb[i-1])^2)
		spline_p2, [xt[i],(*p).x[i],xb[i]],[yt[i],(*p).y[i],yb[i]], xcm,ycm, tan0=tt0,tan1=tb0
		nm = n_elements(xcm)
		nm2 = nm/2
		if 2*nm2 eq nm then begin
			xc = [xc, (*p).x[i], reverse(xcm[0:nm2-1]), xcm, reverse(xcm[nm2:*]), (*p).x[i], txc[k:k+nb8-1]]
			yc = [yc, (*p).y[i], reverse(ycm[0:nm2-1]), ycm, reverse(ycm[nm2:*]),  (*p).y[i], tyc[k:k+nb8-1]]
		endif else begin
			xc = [xc, reverse(xcm[0:nm2]), xcm, reverse(xcm[nm2:*]), txc[k:k+nb8-1]]
			yc = [yc, reverse(ycm[0:nm2]), ycm, reverse(ycm[nm2:*]), tyc[k:k+nb8-1]]
		endelse
		k = round(n_elements(txc) * float(i+1)/8)
	endfor

	return
end

;-----------------------------------------------------------------
; Build mapping of x,y coordinates onto z projection (in image coords)

function curve_projection, pstate, x,y, dz=dz, nc=n

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 3 then return, 0.
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return, 0.

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) or (max([max(x),max(y)]) eq 0) then return, 0.

	;  0-8 are spline pts, 9,10 are width/curvature handles

	theta = atan( (*p).y[10]-(*p).y[9], (*p).x[10]-(*p).x[9])
	rotatev, (*p).x,(*p).y, (*p).x[4],(*p).y[4], -theta, xr,yr

	w = 0.5* (xr[10] - xr[9])
	delta = yr[4] - 0.5*(yr[9]+yr[10])
	if (w gt 1.) and (abs(delta) gt 0.5) then begin
		if (delta gt 0.5) then begin
			R = 0.5*(w+delta)*(w-delta)/delta > (yr[4]-yr[0]+2)*1.05
			;		print,'delta +ve, R=',R,'  limit=', (yr[4]-yr[0]+2)*1.05
		endif else if (delta lt -0.5) then begin
			R = 0.5*(w+delta)*(w-delta)/delta < (yr[4]-yr[8]-2)*1.05
			;		print,'delta -ve, R=',R,'  limit=', (yr[4]-yr[8]-2)*1.05
		endif
		xo = xr[4]
		yo = yr[4] - R
		phi = asin( clip(w/R,-1.,1.) )
		rotatev, xr[0:8],yr[0:8], xo,yo, -phi, xrt,yrt
		rotatev, xr[0:8],yr[0:8], xo,yo, phi, xrb,yrb
		rotatev, xrt,yrt, (*p).x[4],(*p).y[4], theta, xt,yt
		rotatev, xrb,yrb, (*p).x[4],(*p).y[4], theta, xb,yb
	endif else begin
		rotatev, xr[0:8]-w,yr, (*p).x[4],(*p).y[4], theta, xt,yt
		rotatev, xr[0:8]+w,yr, (*p).x[4],(*p).y[4], theta, xb,yb
	endelse

	spline_p2, xt,yt, xts,yts, interval=0.5
	spline_p2, xb,yb, xbs,ybs, interval=0.5
	spline_p2, (*p).x[0:8], (*p).y[0:8], xc,yc, interval=0.5

	nt = n_elements(xts)
	nb = n_elements(xbs)
	n = n_elements(xc)
	mx = max(x)
	my = max(y)
	map = lonarr(mx+1,my+1)

	dx = (max(xc)-min(xc)) / float(n-1)
	dy = (max(yc)-min(yc)) / float(n-1)
	dz = sqrt( dx*dx + dy*dy)

	for i=0,n-1 do begin
		it = round((nt-1)*(i/float(n-1)) < (nt-1))
		ib = round((nb-1)*(i/float(n-1)) < (nb-1))
		if i eq 0 then begin
			tt0 = [yts[0]-yts[1],xts[1]-xts[0]]/sqrt((yts[0]-yts[1])^2 + (xts[1]-xts[0])^2)
			tb0 = [ybs[0]-ybs[1],xbs[1]-xbs[0]]/sqrt((ybs[0]-ybs[1])^2 + (xbs[1]-xbs[0])^2)
		endif else begin
			it1 = (it-1) > 0
			ib1 = (ib-1) > 0
			tt0 = [yts[it1]-yts[it],xts[it]-xts[it1]]/sqrt((yts[it1]-yts[it])^2 + (xts[it]-xts[it1])^2)
			tb0 = [ybs[ib1]-ybs[ib],xbs[ib]-xbs[ib1]]/sqrt((ybs[ib1]-ybs[ib])^2 + (xbs[ib]-xbs[ib1])^2)
		endelse
		spline_p2, [xts[it],xc[i],xbs[ib]],[yts[it],yc[i],ybs[ib]], xcm,ycm, interval=0.5, tan0=tt0,tan1=tb0
		map[clip(xcm,0,mx),clip(ycm,0,my)] = i
	endfor

	z = map[x,y]
	dz = 2.*dz
	return, z/2					; back from interval=0.5 to ~unity
end

;-----------------------------------------------------------------
; Build list of distance vertices (in image coords)

pro distance_vertices, pstate, x,y, n

	n = 0
	if (*pstate).analyze_mode ne 0 then return
	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 0 then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-1 are ends

	x = (*p).x[0:1]
	y = (*p).y[0:1]
	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;-----------------------------------------------------------------

function dot_fill, nx,ny, col

	pattern = bytarr(nx,ny)
	for i=0L,ny-1 do begin
		k = i - 2*(i/2)
		for j=0L,nx-2,2 do begin
			pattern[j+k,i] = col		; dotted fill pattern
		endfor
	endfor

	return, pattern
end

;-----------------------------------------------------------------
; Draw the current image ((*pstate).image) on the draw widget

pro draw_images, pstate

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
			warning,'Draw_images',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	tin  = systime(/seconds)
	wset, (*pstate).wid2

	p = (*pstate).p
	if ptr_valid( p) eq 0 then goto, fin
	if ptr_valid( (*p).image ) eq 0 then goto, fin
	xanes = (*pstate).xanes
	check_mode, pstate

	i = (*pstate).image
	b = make_tvb( pstate, i)
	if n_elements(b) le 1 then goto, fin

	erase
	tv, b, 0,0								; plot image

	if ptr_valid ((*pstate).b) then ptr_free, (*pstate).b
	(*pstate).b = ptr_new( b, /no_copy)

	wset, (*pstate).pix
	device,copy=[0,0,(*pstate).width>1,(*pstate).height>1, 0,0,(*pstate).wid2]

	clear_mark, pstate, /init
	wset, (*pstate).wid2
	plot_mark, pstate

fin:
	t = systime(/seconds)
	*(*pstate).ppercent = 100.*(t-tin)/(t-(*pstate).last_time)
	(*pstate).last_time = t
	return
end

;-----------------------------------------------------------------
; Build list of ellipse vertices (in image coords)

pro ellipse_vertices, pstate, x,y, n

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 5 then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	;  0-3 are diameters, 4 is centre handle

	ellipse_diam, (*p).x[0],(*p).y[0], (*p).x[1],(*p).y[1], $
		(*p).x[2],(*p).y[2], (*p).x[3],(*p).y[3], x,y
	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;-----------------------------------------------------------------

pro fix_options, p

	if ptr_valid( (*p).error) eq 0 then (*p).has_errors = 0
	xanes = tag_present( 'XANES', *p)

	if ptr_valid( (*p).options) and ( (ptr_valid( (*p).escale) and (*p).has_errors) or ((*p).has_errors eq 0)) then return
	n_el = xanes ? (*p).zsize : (*p).n_el

	options = replicate( define(/options_image), n_el)

	history = ptrarr( n_el)

	if (*p).has_errors eq 1 then begin
		escale = options
		(*p).escale = ptr_new( escale, /no_copy)
	endif

	(*p).options = ptr_new( options, /no_copy)
	(*p).history = ptr_new( history, /no_copy)

	set_image_minmax, p, (*p).image, (*p).options
	if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
		set_image_minmax, p, (*p).error, (*p).escale
	endif
	return
end

;-----------------------------------------------------------------

pro free_image_regions, pregions

	if ptr_valid(pregions) then begin
		n = n_elements( *pregions)
		if n gt 0 then begin
			for i=0L,n-1 do begin
				free_region_entry, pregions, i
			endfor
		endif
	endif

	*pregions = ptr_new()
	return
end

;------------------------------------------------------------------------------

pro free_region_entry, pr, n

	COMPILE_OPT STRICTARR
	if ptr_valid(pr) eq 0 then return
	if n_elements( *pr) lt 1 then return
	if ptr_valid( (*pr)[0]) eq 0 then return
	if (n lt 0) or (n ge n_elements((*pr))) then return

	p = (*pr)[n]
	free_region, p
	return
end

;------------------------------------------------------------------------------

pro free_region, p

	COMPILE_OPT STRICTARR
	if ptr_valid(p) eq 0 then return
	if n_elements( *p) lt 1 then return
;	print,'##### free_region: p ...
;	help, p
;	help,/traceback

	if ptr_valid( (*p).el) then ptr_free, (*p).el
	if ptr_valid( (*p).conc) then ptr_free, (*p).conc
	if ptr_valid( (*p).error) then ptr_free, (*p).error
	if ptr_valid( (*p).mdl) then ptr_free, (*p).mdl
	if ptr_valid( (*p).centroid) then ptr_free, (*p).centroid
	if ptr_valid( (*p).q) then ptr_free, (*p).q
	if ptr_valid( (*p).plist) then ptr_free, (*p).plist
	if ptr_valid( (*p).pactive) then ptr_free, (*p).pactive
	if ptr_valid( (*p).pcal) then ptr_free, (*p).pcal
	if ptr_valid( (*p).pmark[0]) then ptr_free, (*p).pmark[0]
	if ptr_valid( (*p).pmark[1]) then ptr_free, (*p).pmark[1]
	if ptr_valid( (*p).sd) then ptr_free, (*p).sd
	if ptr_valid( (*p).relsd) then ptr_free, (*p).relsd
	if ptr_valid( (*p).phase) then ptr_free, (*p).phase
	if ptr_valid( (*p).ayield) then ptr_free, (*p).ayield
	if ptr_valid( (*p).poverlay) then ptr_free, (*p).poverlay
	if obj_valid( (*p).DevObj) then obj_destroy, (*p).DevObj
	ptr_free, p
	return
end

;-----------------------------------------------------------------

pro free_image_state, pstate

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
			warning,'Free_image_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return

	if (*pstate).pix gt 0 then wdelete, (*pstate).pix
	if (*pstate).pix2 gt 0 then wdelete, (*pstate).pix2
	(*pstate).pix = -1
	(*pstate).pix2 = -1

	if ptr_valid( (*pstate).b) then ptr_free, (*pstate).b
	if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
	if ptr_valid( (*pstate).pmode) then ptr_free, (*pstate).pmode
	if ptr_valid( (*pstate).pstate) then ptr_free, (*pstate).pstate
	if ptr_valid( (*pstate).pelement) then ptr_free, (*pstate).pelement
	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).dpath) then ptr_free, (*pstate).dpath
	if ptr_valid( (*pstate).get_file) then ptr_free, (*pstate).get_file
	if ptr_valid( (*pstate).px) then ptr_free, (*pstate).px
	if ptr_valid( (*pstate).py) then ptr_free, (*pstate).py
	if ptr_valid( (*pstate).pyield) then ptr_free, (*pstate).pyield
	if ptr_valid( (*pstate).plast) then ptr_free, (*pstate).plast
	if ptr_valid( (*pstate).pfilter) then ptr_free, (*pstate).pfilter
	if ptr_valid( (*pstate).pdetector) then ptr_free, (*pstate).pdetector
	if ptr_valid( (*pstate).pselect) then ptr_free, (*pstate).pselect
	if ptr_valid( (*pstate).pselect2) then ptr_free, (*pstate).pselect2
	if ptr_valid( (*pstate).pexport) then ptr_free, (*pstate).pexport
	if ptr_valid( (*pstate).pprefs) then ptr_free, (*pstate).pprefs
	if ptr_valid( (*pstate).pfile) then ptr_free, (*pstate).pfile

	if ptr_good((*pstate).pline) then begin
		p = (*pstate).pline
		if (*(*p)[0]).orphan eq 1 then begin
			(*pstate).llocal = 1
			(*(*p)[0]).orphan = 0
		endif
		if ((*pstate).llocal) then free_spectra, p
	endif
	if ptr_good( (*pstate).pcorrect) then begin
		p = (*pstate).pcorrect
		if size(*p,/tname) eq 'STRUCT' then begin
			if ptr_valid( (*p).pyield) then ptr_free, (*p).pyield
			if ptr_valid( (*p).plast) then ptr_free, (*p).plast
		endif
		ptr_free, p
	endif
	if ptr_good( (*pstate).pevt) then begin
		p = (*pstate).pevt
		if size(*p,/tname) eq 'STRUCT' then begin
			free_device_objects, (*p).pDevObjList
			if ptr_valid( (*p).root) then ptr_free, (*p).root
			if ptr_valid( (*p).pic_list) then ptr_free, (*p).pic_list
			if ptr_valid( (*p).layout) then ptr_free, (*p).layout
		endif
		ptr_free, p
	endif

	if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	if ptr_valid( (*pstate).pcorr_mark) then ptr_free, (*pstate).pcorr_mark
	if ptr_valid( (*pstate).pq) then ptr_free, (*pstate).pq
	if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz

	if (*pstate).clone eq 0 then begin
		if ptr_good( (*pstate).p) and ((*pstate).local eq 1) then free_images, (*pstate).p

		free_image_regions, (*pstate).pregions
		(*pstate).region_id = 0

		if ptr_valid( (*pstate).pmark[0]) then begin
			p = (*pstate).pmark[0]
			for i=0L,(*pstate).max_set do begin
				if ptr_valid( (*(p))[i] ) then ptr_free, (*(p))[i]
			endfor
			ptr_free, p
		endif
		if ptr_valid( (*pstate).pmark[1]) then begin
			p = (*pstate).pmark[1]
			for i=0L,(*pstate).max_set do begin
				if ptr_valid( (*(p))[i] ) then ptr_free, (*(p))[i]
			endfor
			ptr_free, p
		endif
	endif
	return
end

;-----------------------------------------------------------------

function image_absolute, pimg, crop=crop, absolute=do_absolute, error=error

;	Determine the absolute coordinates of origin and size for the displayed area, 
;	or an optional sub-region, specified via 'crop' in pixel units.
;	Make use of device FlipX (or FlipY) to indicate a flipped axis.
;
;	crop = {x:[min,max], y:[min,max]}	(not sensitive to order of min,max)
;										will accept single point for x,y
;				crop is in compressed pixel coordinates.
;
;	absolute=0	for relative to image origin (ignore FlipXY)
;
;	Returns r = {absolute:absolute, pixel:pixel, uncompressed:uncompressed, range:range}
;
;	Absolute:	absolute stage coordinates of origin,size of image (or crop if provided) (mm)
;	Pixel:		pixel coordinates of origin,size of image (or crop if provided), compresssed pixels
;	Uncompressed: 	pixel coordinates of origin,size of image (or crop if provided) in uncompressed units
;	Range:		Range for XY that can be used in PLOT for xrange, yrange (Flip will flip axis plot)
;
;	If FlipX, then absolute origin and pixel origin will be on the right corner.
;	If FlipY, then absolute origin and pixel origin will be on the top corner.
;	However, uncompressed origin will ALWAYS be the bottom-left corner for windowed sort usage.

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
			warning,'Free_image_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0
		endif
	endif
	if n_elements(do_absolute) eq 0 then do_absolute = 1

;	crop = {x:[min,max], y:[min,max]} in compressed pixel coordinates.
;
;	orgx, orgy		are pixel origin in compressed units, which may be on right/top for Flipped axes.
;	orgx0, orgy0	are always bottom-left pixel origin in compressed units.
;	sx0, sy0		pixel size in uncompressed units
;	sx1, sy1		pixel size in compressed pixels

	error = 1
	use_crop = 0
	if n_elements(crop) gt 0 then begin
		if size(crop,/tname) eq 'STRUCT' then begin
			sx = (max(crop.x) - min(crop.x) +1)					; compressed
			sy = (max(crop.y) - min(crop.y) +1)
			if (*pimg).DevObj->flipX() then begin
				orgx = max(crop.x) > 0							; right corner if FlipX
			endif else begin
				orgx = min(crop.x) > 0
			endelse
			if (*pimg).DevObj->flipY() then begin
				orgy = max(crop.y) > 0							; top corner if FlipY
			endif else begin
				orgy = min(crop.y) > 0
			endelse
			orgx0 = min(crop.x)									; always bottom-left corner
			orgy0 = min(crop.y)
			if (sx gt 0) and (sy gt 0) then use_crop = 1
			sx0 = sx * (*pimg).xcompress						; not compressed
			sy0 = sy * (*pimg).ycompress
			sx1 = sx											; compressed
			sy1 = sy
		endif
	endif else begin
		sx = (*pimg).x_sub_range								; not compressed
		sy = (*pimg).y_sub_range
		sx0 = sx												; not compressed
		sy0 = sy
		sx1 = sx / (*pimg).xcompress							; compressed
		sy1 = sy / (*pimg).ycompress
		if (*pimg).DevObj->flipX() then begin
			orgx = (*pimg).x_sub_range / (*pimg).xcompress		; right corner if FlipX
		endif else begin
			orgx = 0L
		endelse
		if (*pimg).DevObj->flipY() then begin
			orgy = (*pimg).y_sub_range / (*pimg).ycompress		; top corner if FlipY
		endif else begin
			orgy = 0L
		endelse
		orgx0 = 0L
		orgy0 = 0L
	endelse
;	print,'sx,sx0,sx1,(*pimg).xcompress: ',sx,sx0,sx1,(*pimg).xcompress

;	Note that 'original_size' is AFTER compression on processing.

	dx = (*pimg).scan.x / ((*pimg).original_xsize * (*pimg).xcompress)
	dy = (*pimg).scan.y / ((*pimg).original_ysize * (*pimg).ycompress)
	if do_absolute then begin
		if (*pimg).DevObj->flipX() then begin
			ox = (*pimg).scan.origin.x + dx*((*pimg).original_xsize*(*pimg).xcompress - (*pimg).xoffset-(*pimg).x_sub_range)
			if use_crop then ox = ox + dx*((*pimg).x_sub_range - orgx*(*pimg).xcompress)
		endif else begin
			ox = (*pimg).scan.origin.x + dx*((*pimg).xoffset)
			if use_crop then ox = ox + dx*orgx*(*pimg).xcompress
		endelse
		uox = (*pimg).xoffset + orgx0*(*pimg).xcompress
		if (*pimg).DevObj->flipY() then begin
			oy = (*pimg).scan.origin.y + dy*((*pimg).original_ysize*(*pimg).ycompress - (*pimg).yoffset-(*pimg).y_sub_range)
			if use_crop then oy = oy + dy*((*pimg).y_sub_range - orgy*(*pimg).ycompress)
		endif else begin
			oy = (*pimg).scan.origin.y + dy*((*pimg).yoffset)
			if use_crop then oy = oy + dy*orgy*(*pimg).ycompress
		endelse
		uoy = (*pimg).yoffset + orgy0*(*pimg).ycompress

		xrange = (*pimg).DevObj->flipX() ? [ox+dx*sx0,ox] : [ox,ox+dx*sx0]
		yrange = (*pimg).DevObj->flipY() ? [oy+dy*sy0,oy] : [oy,oy+dy*sy0]
	endif else begin
		ox = dx*((*pimg).xoffset)
		if use_crop then ox = ox + dx*(orgx0*(*pimg).xcompress)
		oy = dy*((*pimg).yoffset)
		if use_crop then oy = oy + dy*(orgy0*(*pimg).ycompress)
		uox = (*pimg).xoffset + orgx0*(*pimg).xcompress
		uoy = (*pimg).yoffset + orgy0*(*pimg).ycompress

		xrange = [ox-0.5*dx,ox+dx*(sx0+1)]
		yrange = [oy-0.5*dy,oy+dy*(sy0+1)]
	endelse

;	Absolute:	absolute stage coordinates of origin,size of image (or crop if provided) (mm)
;	Pixel:		pixel coordinates of origin,size of image (or crop if provided), compresssed pixels
;	Uncompressed: 	pixel coordinates of origin,size of image (or crop if provided) in uncompressed units
;	Range:		Range for XY that can be used in PLOT for xrange, yrange (Flip will flip axis plot)
;
;	If FlipX, then absolute origin and pixel origin will be on the right corner.
;	If FlipY, then absolute origin and pixel origin will be on the top corner.
;	However, uncompressed origin will ALWAYS be the bottom-left corner for windowed sort usage.

	absolute = {org:{x:ox, y:oy}, size:{x:dx*sx0, y:dy*sy0}}
	pixel = {org:{x:orgx, y:orgy}, size:{x:sx1, y:sy1}}
	uncompressed = {org:{x:uox, y:uoy}, size:{x:sx0, y:sy0}}
	range = {x:xrange, y:yrange}

	r = {absolute:absolute, pixel:pixel, uncompressed:uncompressed, range:range}
	error = 0
	return, r
end

;-----------------------------------------------------------------

pro image_region_select, pstate, i, kvs=kvs

;	Select region 'i' and display shape on image

	COMPILE_OPT STRICTARR
	common image_region_window_1, region_window

	if ptr_valid( (*pstate).pregions) eq 0 then goto, clear_select
	n = n_elements( *(*pstate).pregions)
	if n lt 1 then goto, clear_select
	if n_elements(i) eq 0 then i=0
	if n_elements(kvs) eq 0 then kvs=0

	if (i ge 0) and (i lt n) then begin
		p = (*(*pstate).pregions)[i]
		if ptr_good(p) eq 0 then goto, clear_select
		(*pstate).region_id = i
		pimg = (*pstate).p

		(*pstate).elx = (*p).elx							; added May'09
		(*pstate).ely = (*p).ely							; for "-" mode applied to highlight
     
	 	if ptr_valid(p) eq 0 then goto, clear_select
		p2 = translate_region( p, pstate, error=err)		; translate from old region coords in 'p'
		if err then goto, done								; to the local image coords, as in (*pstate).p

		if (*p).mode eq 1 then begin
			(*pstate).analyze_mode = 0
			(*pstate).analyze_type[0] = 0
			(*pstate).corr_mode = 1
			if ptr_valid( (*pstate).pcorr_mark) then ptr_free, (*pstate).pcorr_mark
			if ptr_good( (*p2).pmark[0]) then (*pstate).pcorr_mark = ptr_new( *(*p2).pmark[0])
			if ptr_valid((*pstate).qc) then ptr_free, (*pstate).qc
			(*pstate).qc = ptr_new( *(*p2).q)
			draw_images, pstate
			image_region_extent, p2, (*pstate).qc, origin,extent, error=err

			if err eq 0 then begin
				xy_to_pixel, pstate, origin.x + extent.x/2, origin.y + extent.y/2, px,py
				xlow2 = (px - (*pstate).w/2) > 0
				ylow2 = (py - (*pstate).h/2) > 0
				print,'Region centre:',px,py
			
				widget_control, (*pstate).draw2, set_draw_view=[xlow2,ylow2]
				(*pstate).xlow = xlow2
				(*pstate).ylow = ylow2
				print, 'View: xlow, ylow = ',xlow2,ylow2
			endif
		endif else begin
			if (*pstate).corr_mode then begin
				if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
				(*pstate).corr_mode = 0
				draw_images, pstate
			endif

			(*pstate).analyze_mode = 0
			(*pstate).analyze_type[0] = max( [0,min( [(*pstate).max_set,(*p).analyze_type[0]]) ])
			if ptr_valid( (*(*pstate).pmark[0])[(*pstate).analyze_type[0]] ) then begin
				ptr_free, (*(*pstate).pmark[0])[(*pstate).analyze_type[0]]
			endif
			(*(*pstate).pmark[0])[(*pstate).analyze_type[0]] = ptr_new( *(*p2).pmark[0])
			if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
			(*pstate).q = ptr_new( *(*p2).q)
			image_region_extent, p2, (*pstate).q, origin,extent, error=err

			if err eq 0 then begin
				xy_to_pixel, pstate, origin.x + extent.x/2, origin.y + extent.y/2, px,py
				xlow2 = (px - (*pstate).w/2) > 0
				ylow2 = (py - (*pstate).h/2) > 0
				print,'Region centre:',px,py
			
				widget_control, (*pstate).draw2, set_draw_view=[xlow2,ylow2]
				(*pstate).xlow = xlow2
				(*pstate).ylow = ylow2
				print, 'View: xlow, ylow = ',xlow2,ylow2
			endif
			if (*p).analyze_type[1] gt 0 then begin
				(*pstate).analyze_type[1] = max( [0,min( [(*pstate).max_set,(*p).analyze_type[1]]) ])
				if ptr_valid((*(*pstate).pmark[1])[(*pstate).analyze_type[1]]) then begin
					ptr_free, (*(*pstate).pmark[1])[(*pstate).analyze_type[1]]
				endif
				(*(*pstate).pmark[1])[(*pstate).analyze_type[1]] = ptr_new( *(*p2).pmark[1])
				(*pstate).analyze_mode = 1
			endif

			if kvs and ((*pstate).analyze_type[0] eq 1) then begin				; for a "Box" write to KVS
				pm = (*(*pstate).pmark[0])[(*pstate).analyze_type[0]]
				iox = min( (*pm).x[0:3])
				ioy = min( (*pm).y[0:3])
				imx = max( (*pm).x[0:3])
				imy = max( (*pm).y[0:3])
				xy_to_microns, pstate, iox,ioy, ox,oy,ounits
				xy_to_microns, pstate, imx,imy, mx,my,munits
				sx = mx - ox
				sy = my - oy
				ox = ox + (*pimg).scan.origin.x
				oy = oy + (*pimg).scan.origin.y
				set_kvs_box, pstate, ox,oy, sx,sy

				w = (imx-iox) * (*pimg).xcompress
				h = (imy-ioy) * (*pimg).ycompress
				uox = (*pimg).xoffset + min((*pm).x) * (*pimg).xcompress
				uoy = (*pimg).yoffset + min((*pm).y) * (*pimg).ycompress
				region_window = {offset:{x:round(uox), y:round(uoy)}, range:{x:round(w), y:round(h)}} 
				print, 'Region window: ',region_window
			endif

			wset, (*pstate).wid2
			plot_mark, pstate
		endelse
		free_region, p2

		widget_control, (*pstate).analyze_mode_id, set_combobox_select = (*pstate).analyze_mode
		widget_control, (*pstate).analyze_type_id, set_combobox_select = (*pstate).analyze_type[(*pstate).analyze_mode]
	endif
Done:
	return
clear_select:
	clear_mark, pstate
	goto, done
end

;-----------------------------------------------------------------

pro image_region_extent, p, q, origin,extent, error=error

;	Determine extent of region on image and return bounding origin, extent.

	COMPILE_OPT STRICTARR

	error = 1
	if ptr_good(p) eq 0 then return
	if ptr_good(q) eq 0 then return

	q_to_xy, *q, (*p).nx, x,y
	origin = {x:min(x), y:min(y)}
	extent = {x:(max(x)-min(x))+1, y:(max(y)-min(y))+1}
	error = 0
	return
end

;-----------------------------------------------------------------

pro image_save_undo, p, i

	; 'p' is a pointer to an image struct

	if ptr_valid(p) eq 0 then return

	if ptr_valid( (*p).undo.image) then ptr_free, (*p).undo.image
	if (*p).undo.has_errors then begin
		if ptr_valid( (*p).undo.error) then ptr_free, (*p).undo.error
	endif
	if ptr_valid( (*p).undo.history) then ptr_free, (*p).undo.history

	(*p).undo.image = ptr_new( (*((*p).image))[*,*,i])
	(*p).undo.id = i
	(*p).undo.ok = 1
	(*p).undo.min = (*(*p).options)[i].min
	(*p).undo.max = (*(*p).options)[i].max
	if ptr_valid( (*p).history) then begin
		if ptr_valid( (*(*p).history)[i]) then begin
			(*p).undo.history = ptr_new( *(*(*p).history)[i] )
		endif
	endif
	if (*p).has_errors then begin
		(*p).undo.error = ptr_new( (*((*p).error))[*,*,i])
		(*p).undo.has_errors = 1
		(*p).undo.emin = (*(*p).escale)[i].min
		(*p).undo.emax = (*(*p).escale)[i].max
	endif else begin
		(*p).undo.error = ptr_new()
		(*p).undo.has_errors = 0
	endelse
	return
end

;-----------------------------------------------------------------

pro image_restore_undo, event, arg1, silent=silent, select=i

	; '(*pstate).p' is a pointer to an image struct.
	; 'select' keyword only used for compatibility with normal operations routines.

	COMPILE_OPT STRICTARR
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	if n_elements(silent) lt 1 then silent=0

	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, done
	if (*p).undo.ok ne 1 then begin
		warning,'image_restore_undo','Last operation cannot be undone.'
		goto, done
	endif
	if ptr_valid( (*p).undo.image) eq 0 then goto, done
	if ptr_valid( (*p).image) eq 0 then goto, done
	if ptr_valid( (*p).undo.error) eq 0 then (*p).undo.has_errors=0

	i = (*p).undo.id
	if i ge n_elements((*((*p).image))[0,0,*]) then goto, done
	if n_elements(*((*p).undo.image)) ne n_elements((*((*p).image))[*,*,i]) then goto, done

	(*((*p).image))[*,*,i] = *((*p).undo.image)
	(*(*p).options)[i].min = (*p).undo.min
	(*(*p).options)[i].max = (*p).undo.max
	if ptr_valid( (*(*p).history)[i]) then ptr_free, (*(*p).history)[i]
	if ptr_valid( (*p).undo.history) then (*(*p).history)[i] = ptr_new( *(*p).undo.history)

	if (*p).undo.has_errors and (*p).has_errors then begin
		if n_elements(*((*p).undo.error)) ne n_elements((*((*p).error))[*,*,i]) then goto, done
		(*((*p).error))[*,*,i] = *((*p).undo.error)
		(*(*p).escale)[i].min = (*p).undo.emin
		(*(*p).escale)[i].max = (*p).undo.emax
	endif

	done:
	if silent eq 0 then begin
		draw_images, pstate
		notify, 'image-display', from=event.top
	endif
end

;-----------------------------------------------------------------

pro image_axes_units, p, xunits=xunits, yunits=yunits

	COMPILE_OPT STRICTARR

	xunits = 'mm'
	yunits = 'mm'
	if ptr_good(p,/struct) eq 0 then return

	if (*p).x_coord_units eq 'µm' then xunits = 'mm'
	if (*p).y_coord_units eq 'eV' then yunits = 'keV'
	return
end

;-----------------------------------------------------------------
; Now obsolete - use device objects ...

;pro image_update_prefs, prefs
;
;COMPILE_OPT STRICTARR
;common c_errors_1, catch_errors_on
;common c_prefs_scan, prefs_XY_scan, prefs_X_step, prefs_Y_step, prefs_Resolution
;common c_sandia_4, sync, TimerEvent, RTCmask, Dummymask, maxADCs, mpa_x_adc, mpa_y_adc
;
;if n_elements(prefs_XY_scan) lt 1 then prefs_XY_scan = {X:100.0, Y:100.0}
;if n_elements(prefs_X_step) lt 1 then prefs_X_step = {Y:2000.0}
;if n_elements(prefs_Y_step) lt 1 then prefs_Y_step = {X:640.0}
;if n_elements(prefs_Resolution) lt 1 then prefs_Resolution = {X:0.635, Y:0.08333333}
;if n_elements(mpa_x_adc) lt 1 then mpa_x_adc = 0S
;if n_elements(mpa_y_adc) lt 1 then mpa_y_adc = 2S
;
;ErrorNo = 0
;if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
;if catch_errors_on then begin
;    Catch, ErrorNo
;    if (ErrorNo ne 0) then begin
;       Catch, /cancel
;       on_error, 1
;       help, calls = s
;       n = n_elements(s)
;       c = 'Call stack: '
;       if n gt 2 then c = [c, s[1:n-2]]
;       warning,'Image_update_prefs',['IDL run-time error caught.', '', $
;          'Error:  '+strtrim(!error_state.name,2), $
;          !error_state.msg,'',c,'','Check for corrupt Preferences file.'], /error
;       MESSAGE, /RESET
;       return
;    endif
;endif
;
;    if size(prefs,/tname) eq 'STRUCT' then begin
;       prefs_XY_scan = prefs.XY_scan
;       prefs_X_step = prefs.X_step
;       prefs_Y_step = prefs.Y_step
;       prefs_Resolution = prefs.Resolution
;       mpa_x_adc = prefs.mpa3.x
;       mpa_y_adc = prefs.mpa3.y
;    endif
;    return
;end

;-----------------------------------------------------------------

; Estimate "max" value for image that uses 0.1% (threshold) below top of
; image histogram as default, and top/3 as min (so that image
; display top slider can still "see" the true top if need be.
;
;	scope	can change default factor 3
;
; If the remaining image here contains nothing, return nothing_remains=1

function image_weighted_max, img, threshold=threshold, nothing_remains=nothing_remains, scope=scope

	if n_elements(threshold) eq 0 then threshold = 0.001
	if n_elements(scope) eq 0 then scope = 3.
	nbins = 1000
	totimg = total(img > 0.0)
	top = max(img)
	nothing_remains = 0
	if totimg eq 0.0 then begin
		nothing_remains = 1
		return, top
	endif

	h = histogram(float(img), nbins=nbins, min=0.0, omax=big, /NaN)
	if big le 0.0 then return, top
	tot = total(h)
	if tot lt 1 then return, top
	inc = big/float(nbins)

	sum = 0L
	trip = long(tot * (threshold < 0.5))
	top = big
	for i=nbins-1, 1, -1 do begin
		sum = sum + h[i]
		if (sum gt trip) or (top le big/scope) then break
		top = top - inc
	endfor
	return, top
end

;-----------------------------------------------------------------------------------

function legend_string, pstate, type, pimg, opt, i, sx, sy

	;	Legend string to put in Image window Help box.
	;	'pimg' may point to 'image' or 'error' array, in which case 'opt'
	;	and 'sx, 'sy' reflect options and size of this array.
	;
	;	If charge=0, this means that this is RT mode in Maia-control. In this
	;	case, determine current number of active pixels from Flux map.
	;	If charge present, then offline mode. Still need to determine active
	;	number of pixels.

	if ptr_good(pstate,/struct) eq 0 then return,''
	if ptr_good(pimg) eq 0 then return,''
	if ptr_good(opt,/struct) eq 0 then return,''
	if i ge n_elements(*opt) then return,''
	;print,'legend_string: i=',i

	if (*pstate).display_mode eq 1 then begin
		top = sqrt((*opt)[i].max * (*opt)[i].top / 100.)
		minx = sqrt((*opt)[i].min)
		maxx = sqrt((*opt)[i].max)
	endif else begin
		top = ((*opt)[i].max * (*opt)[i].top / 100.)
		minx = (*opt)[i].min
		maxx = (*opt)[i].max
	endelse

	p = (*pstate).p
	if ptr_good(p) eq 0 then return,''
	xanes_stack_test, p, xanes, n_el, els, el_xanes
	nx = (*p).xsize
	ny = (*p).ysize
	var_mode = (sx ne nx)

	image_flux_charge, p, xanes=xanes, charge=charge, pixels=pixels

	; 'image_flux_charge' uses the whole image array
	; 'pixels' refers to main 'image' array, not 'error'

	if var_mode then pixels = pixels / 4
	charge_per_pixel = charge / float(pixels)

	mean_counter = 0
	arb_units = 0
	time_units = 0
	counts_units = 0
	el = els[i]
	if strmid(el,0,2) eq 'n(' then mean_counter=1
	arb = strlowcase( special_elements(/arb))
	time = strlowcase( special_elements(/time))
	counts = strlowcase( special_elements(/counts))
	q = where( strlowcase(el) eq arb, nq)
	if nq ne 0 then arb_units=1
	q = where( strlowcase(el) eq time, nq)
	if nq ne 0 then time_units=1
	q = where( strlowcase(el) eq counts, nq)
	if nq ne 0 then counts_units=1

	if type eq 1 then begin
		units = ''
		style = 'fraction'
	endif else if (type eq 2) or mean_counter or counts_units then begin
		units = ''
		style = 'counts'
	endif else if type eq 3 then begin
		units = ''
		style = 'energy'
	endif else if arb_units then begin
		units = ''
		style = 'arbitrary'
	endif else if time_units then begin
		units = 'ms'
		style = 'time'
	endif else begin
		units = 'ppm'
		style = 'conc'
		top = top / charge_per_pixel
		if top gt 999.9 then begin
			top = top/10000.
			units = 'wt%'
		endif
	endelse
	if (*pstate).display_mode eq 1 then style='variance'

	if (*pstate).display_mode eq 1 then begin
		s = 'Display top sqrt(' + style + ') = ' + str_tidy(top)
		s = s + ' ' + units + ' (zoom='+str_tidy((*pstate).zoom)+')'
		;		s = s + ')^2'
	endif else begin
		s = 'Display top (' + style + ') = ' + str_tidy(top)
		s = s + ' ' + units + ' (zoom='+str_tidy((*pstate).zoom)+')'
	endelse

	s1 = (*pstate).file
	n1 = strlen(s1)
	if n1 gt 42 then s1 = strmid(s1,n1-42,42)
	if s1 eq '' then s1 = 'Charge = '+str_tidy(charge)+', pixels = '+str_tidy(pixels)
	if tag_present('XANES',*p) then begin
		;print,'legend_string: i, label=',i,'  ',(*(*p).pz_label)[i]
		if ptr_good(pz_label) then begin
			if i lt n_elements(*(*p).pz_label) then begin
				s2 = 'Image = ' + strip_path( (*(*p).pz_label)[i])
			endif else s2=''
		endif else s2=''
		s = [s1,s,s2]
	endif else begin
		if (*p).energy gt 0.0 then begin
			s2 = 'E = ' + str_tidy((*p).energy)
			s2 = s2 + '     (' + str_tidy(minx) + ',' + str_tidy(maxx) + ')'
		endif else s2=(*p).comment
		s = [s1,s,s2]
	endelse
	return, s
end

;-----------------------------------------------------------------
; Build list of line vertices (in image coords)

pro line_vertices, pstate, x,y, n

	if (*pstate).analyze_type[(*pstate).analyze_mode] eq 4 then goto, cont
	if (*pstate).analyze_type[(*pstate).analyze_mode] eq 8 then goto, cont
	if (*pstate).analyze_type[(*pstate).analyze_mode] eq 9 then goto, cont
	cont:
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	;  0-1	are centre line
	;  4	centre handle
	;  2-3  width handles

	dx = (*p).x[3] - (*p).x[4]
	dy = (*p).y[3] - (*p).y[4]

	x = [(*p).x[0]+dx, (*p).x[1]+dx, (*p).x[1]-dx, (*p).x[0]-dx ]
	y = [(*p).y[0]+dy, (*p).y[1]+dy, (*p).y[1]-dy, (*p).y[0]-dy ]
	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;-----------------------------------------------------------------
; Make the byte array to TV to the draw area
; Use either parameters via *pstate, or use the args struct.
;
; Use /nozoom for non-screen applications.
;
; Clip using xmin,xmax, ymin,ymax.
; Rescale onto different output size using /nozoom, xtgt, ytgt

function make_tvb, pstate, i, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, $
	xtgt=xtgt, ytgt=ytgt, compress=compress, highlight=highlight_only, $
	args=args, colour=colouri, centroids=centroids

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
			warning,'Make_TVB',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0B
		endif
	endif
	if n_elements(nozoom) lt 1 then nozoom=0
	if n_elements(xmax) lt 1 then xmax=0
	if n_elements(ymax) lt 1 then ymax=0
	if n_elements(xmin) lt 1 then xmin=0
	if n_elements(ymin) lt 1 then ymin=0
	if n_elements(xtgt) lt 1 then xtgt=0
	if n_elements(ytgt) lt 1 then ytgt=0
	if n_elements(highlight_only) lt 1 then highlight_only=0
	if n_elements(centroids) lt 1 then centroids=0
	if n_elements(colouri) lt 1 then begin
		colour = spec_colour('green')
		dcolour = spec_colour('d.green')
	endif else if n_elements(colouri) eq 2 then begin
		colour = colouri[0]
		dcolour = colouri[1]
	endif else begin
		colour = colouri
		dcolour = colouri
	endelse
	no_interelement = 1
	b = 0B
	if ptr_valid( pstate) then begin
		p = (*pstate).p
		if (*pstate).display_mode eq 0 then begin
			imgx = interelement_transform( p, i, error=no_interelement)
		endif else no_interelement=1

		xanes = (*pstate).xanes
		pimg = point_image( pstate, opt=opt, nx=sx0, ny=sy0)
		display_mode = (*pstate).display_mode
		qc = (*pstate).qc
		corr_on = (*pstate).corr_on
		width = (*pstate).width
		height = (*pstate).height
		izoom = (*pstate).zoom
	endif else begin
		if n_elements(args) eq 0 then return, b
		p = args.p
		xanes = args.xanes
		pimg = args.pimg
		opt = args.opt
		sx0 = args.sx
		sy0 = args.sy
		display_mode = args.display_mode
		qc = args.qc
		corr_on = args.corr_on
		width = args.width
		height = args.height
		izoom = args.zoom
		centroids = 0					; no pstate access
	endelse
	sx = sx0
	sy = sy0

	if ptr_valid( p) eq 0 then return, b
	if ptr_valid( pimg ) eq 0 then return, b
	if i ge n_elements( (*pimg)[0,0,*]) then return, b

	if (xmax ne 0) or (ymax ne 0) then begin
		if xmax eq 0 then xmax = sx0-1
		if ymax eq 0 then ymax = sy0-1
		sx = (xmax - xmin +1) < sx0
		sy = (ymax - ymin +1) < sy0
		xl = xmin > 0
		yl = ymin > 0
		xh = xmax < (sx0-1)
		yh = ymax < (sy0-1)
	endif else begin
		xl = 0
		xh = sx - 1
		yl = 0
		yh = sy - 1
	endelse
	
	if no_interelement then begin
		build_image_scale, (*opt)[i], low, high, image=(*pimg)[xl:xh,yl:yh,i], output=img, root=display_mode
	endif else begin
		build_image_scale, (*opt)[i], low, high, image=imgx[xl:xh,yl:yh], output=img, root=display_mode
	endelse

	b = bytscl( img, top=99, min=low, max=high) + 16B

	if var_type(centroids) eq 8 then begin				; must be a 'struct'
		was_el = centroids.element
		pr = (*pstate).pregions
		nreg = n_elements(*pr)
		q = where( centroids.element eq *(*p).el, nq)
		if nq eq 0 then begin
			el_code, centroids.element, el1, z1, shell1, bad1, error1
			el_code, *(*p).el, el2, z2, shell2, bad2, error2
			q = where( z1 eq z2, nq)
			if z1 eq 0 then centroids=0
		endif
		if (nq ge 1) then begin
			i = q[0]
		endif else centroids=0
		if (var_type(centroids) ne 8) then begin
			warning,'Make_TVB',['Centroid element "'+was_el+'" not found.','', $
					'Ignoring centroids ...']
		endif
	endif

	do_corr_highlight = 0
	bc = bytarr( sx0, sy0)

;	For each region, mark out a circle centred on the centroid.
;	If circles overlap, remove the intersections, and only show the outer envelope.
;	Comments below for "clear" (pixels enclosed by circles) and "line" (the circle shape/outline).

	if (var_type(centroids) eq 8) then begin			; must be a 'struct' 
		if (nreg ge 1) then begin
			r = clip( 15 * (float(sy)/700), 8, 25)
			do_corr_highlight = 1
			for j=1,nreg-1 do begin
				x = (*(*(*pr)[j]).centroid)[i].x
				y = (*(*(*pr)[j]).centroid)[i].y
				absx = (*(*pr)[j]).xoffset + x * (*(*pr)[j]).xcompress
				absy = (*(*pr)[j]).yoffset + y * (*(*pr)[j]).ycompress
				px = (absx - (*p).xoffset) / (*p).xcompress
				py = (absy - (*p).yoffset) / (*p).ycompress
				print,j,'  centroid: ',x,y, '  abs: ', absx,absy, '  pixel: ', px,py

				circle_diam, px-r,py-r, px+r,py+r, xc,yc, n=100
				qf = polyfillv( xc,yc, sx0, sy0)
				bc[qf] = bc[qf] + 1						; clear fill
				bc[xc,yc] = bc[xc,yc] + 100				; circle surround

				xmin = (min(xc)-2) > 0					; use a sub-region for speed ...
				xmax = (max(xc)+2) < (sx0-1)
				ymin = (min(yc)-2) > 0
				ymax = (max(yc)+2) < (sy0-1)
				bt = bc[xmin:xmax,ymin:ymax]
				q = where( bt eq 102, nq)				; line on prev clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 3, nq)					; clear on prev clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 121, nq)				; clear on prev line
				if nq gt 0 then bt[q]=2
				q = where( bt eq 103, nq)				; line+clear on prev clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 220, nq)				; line on prev line
				if nq gt 0 then bt[q]=120
				q = where( bt eq 221, nq)				; line+clear on prev line
				if nq gt 0 then bt[q]=120
		
				q = where( bt eq 1, nq)					; final clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 101, nq)				; this line on this clear
				if nq gt 0 then bt[q]=120
				q = where( bt eq 100, nq)				; final line
				if nq gt 0 then bt[q]=120
				bc[xmin:xmax,ymin:ymax] = bt			; copy sub-region back

			endfor
			q = where( (bc eq 0) and (shift(bc,1,0) eq 2), nq)	; missing circle pixels (horiz)
			if nq gt 0 then bc[q] = 120
			q = where( (bc eq 2) and (shift(bc,1,0) eq 0), nq)
			if nq gt 0 then bc[q] = 120
			q = where( (bc eq 0) and (shift(bc,0,1) eq 2), nq)	; missing circle pixels (vert)
			if nq gt 0 then bc[q] = 120
			q = where( (bc eq 2) and (shift(bc,0,1) eq 0), nq)
			if nq gt 0 then bc[q] = 120

			q = where( bc eq 2, nq)						; zero clear
			if nq gt 0 then bc[q]=0
			q = where( bc eq 120, nq)					; final line
			if nq gt 0 then bc[q]=100
		endif
	endif

	if ptr_valid( qc) and corr_on then begin		
		if (*qc)[0] ne -1 then begin
			bc[*qc] = 100
			if highlight_only then b[*]=0				; only highlight, not image
			do_corr_highlight = 1
		endif
	endif
	bc = bc[xl:xh,yl:yh]

	compress = 1.
	if nozoom then begin			; non-screen applications

		if (ytgt ne 0) and (xtgt ne 0) then begin
			compress = min( [ float(xtgt) / float(sx), float(ytgt) / float(sy) ])
		endif

;		Hot spots tend to disappear for compress<1 or on big plots
;		Use 'dilate' to expand them so that they remain visible.

		if compress lt 1. then begin
			nmin = ceil(ytgt/500.)
			n = round((1./compress) + 0.4) > nmin
			s = round_kernel(n)
			if (xh-xl+1 ge n) and (yh-yl+1 ge n) then bc = dilate(bc,s,/gray)
		endif else if sy gt 400 then begin
			nmin = 2
			n = ceil(sy/400.) > nmin
			s = round_kernel(n)
			if (xh-xl+1 ge n) and (yh-yl+1 ge n) then bc = dilate(bc,s,/gray)
		endif

		if compress ne 1. then begin
			b = smart_congrid( b, sx*compress, sy*compress, interp=(*opt)[i].interp)
			if do_corr_highlight then begin
				bc = smart_congrid( bc, sx*compress, sy*compress, interp=(*opt)[i].interp)
			endif
		endif
	endif else begin				; into screen buffer

		compress = float(width) / float(sx)

;		Hot spots tend to disappear for compress<1 or on big plots
;		Use 'dilate' to expand them so that they remain visible.

		if compress lt 1. then begin
			n = round((1./compress) + 0.4)
			s = round_kernel(n < (sx<sy))
			bc = dilate(bc,s,/gray)
		endif

		if compress ne 1. then begin
;		if (izoom ne 0) or (display_mode ne 0) then begin
			b = smart_congrid( b, width, height, interp=(*opt)[i].interp)
			if do_corr_highlight then begin
				bc = smart_congrid( bc, width, height, interp=(*opt)[i].interp)
			endif
		endif
	endelse

	if do_corr_highlight then begin
		q = where( bc ge 50)		
		if q[0] ne -1 then b[q] = colour
		q = where( (bc ge 20) and (bc lt 50))
		if q[0] ne -1 then b[q] = dcolour
	endif
	return, b
end

;-----------------------------------------------------------------

pro map_help, pstate

; Switch between help1 and help2 text widgets for help as the window is resized.
; help1 is under window controls, help2 is to the right.
; 
; 'scr_xsize_off' and 'scr_ysize_off' provide offsets from window size x,y to
; window scr_xsize, scr_ysize size (w,h) in 'OnSize_image'.

	if (*pstate).w gt 600 then begin
		if (*pstate).help eq (*pstate).help2 then goto, more

		(*pstate).help = (*pstate).help2
		widget_control, (*pstate).help1_base, map=0
		widget_control, (*pstate).help1, scr_ysize=1
		widget_control, (*pstate).help2_base, map=1
	endif else begin
		if (*pstate).help eq (*pstate).help1 then goto, more

		(*pstate).help = (*pstate).help1
		widget_control, (*pstate).help2_base, map=0
		widget_control, (*pstate).help2, scr_xsize=1
		widget_control, (*pstate).help1, ysize=3
		widget_control, (*pstate).help1_base, map=1
	endelse

more:
	if (*pstate).w gt 600 then begin
		case !version.os_family of
			'MacOS': begin
				(*pstate).scr_xsize_off =	1
				(*pstate).scr_ysize_off =	68
			end
			'unix': begin
				(*pstate).scr_xsize_off =	1
				(*pstate).scr_ysize_off =	110
			end
			else: begin
				(*pstate).scr_xsize_off =	10		; 8
				(*pstate).scr_ysize_off =	74		; 68
			end
		endcase
	endif else begin
		case !version.os_family of
			'MacOS': begin
				(*pstate).scr_xsize_off =	1
				(*pstate).scr_ysize_off =	100
			end
			'unix': begin
				(*pstate).scr_xsize_off =	8
				(*pstate).scr_ysize_off =	152
			end
			else: begin
				(*pstate).scr_xsize_off =	8
				(*pstate).scr_ysize_off =	123		; 102
			end
		endcase
	endelse

	case !version.os_family of
		'unix': begin
			xoff = 367
			end
		else: begin
			xoff = 363		; 363
			end
	endcase

	if (*pstate).help eq (*pstate).help2 then begin
		widget_control, (*pstate).help2, scr_xsize=((*pstate).w - xoff)
	endif
end

;-----------------------------------------------------------------
; Build list of shape vertices to be used by analyze_image

pro mark_vertices, pstate, x,y, n

	n = 0
	if ((*pstate).analyze_type[(*pstate).analyze_mode] lt 1) $
		and ((*pstate).analyze_mode eq 1) then return

	case (*pstate).analyze_type[(*pstate).analyze_mode] of
		0: distance_vertices, pstate, x,y, n
		1: box_vertices, pstate, x,y, n
		2: circle_vertices, pstate, x,y, n
		3: curve_vertices, pstate, x,y, n
		4: line_vertices, pstate, x,y, n
		5: ellipse_vertices, pstate, x,y, n
		6: spline_vertices, pstate, x,y, n
		7: spline_vertices, pstate, x,y, n
		8: line_vertices, pstate, x,y, n
		9: line_vertices, pstate, x,y, n
		10: spline_vertices, pstate, x,y, n
		11: spoint_vertices, pstate, x,y, n
		;	12: mpoint_vertices, pstate, x,y, n
		else:
	endcase

	return
end

;--------------------------------------------------------------------
;
;; Is point 'xb,yb' close to point 'xp,yp'
;
;function near_xy, xp,yp, xb,yb
;
;r = 0
;if sqrt( float(xb-xp)*float(xb-xp) + float(yb-yp)*float(yb-yp) ) lt 5 then r = 1
;
;return, r
;end
;
;-----------------------------------------------------------------
;
; Convert image microns to 'x,y' (if a scan size is set), else image x,y
; Take care as these routines do not known about origin.

pro microns_to_xy, pstate, mx,my, x,y, z=z 

	if n_elements(z) eq 0 then z=0
	p = z ? (*pstate).pz : (*pstate).p
	if ptr_valid( p) eq 0 then return

	j = (*pstate).image
	x = mx
	y = my

	if ((*p).scan.x gt 0.0001) and ((*p).scan.y gt 0.0001) then begin
		x = round( mx * float((*p).original_xsize * (*p).scaled_x) / float((*p).scan.x)) 
		y = round( my * float((*p).original_ysize * (*p).scaled_x) / float((*p).scan.y))
	endif

	x = clip( x, 0, (*p).xsize-1)
	y = clip( y, 0, (*p).ysize-1)
	return
end

;--------------------------------------------------------------------

; Which is closest index of 'x,y' vectors to point ,'xp,yp'
; This version accepts vectors 'x', 'y' and returns the closest index, else -1
; The 'zoom' is the factor of two image zoom index

function near_xy, xp,yp, x,y, reverse=reverse, zoom=zoom

	if n_elements(reverse) lt 1 then reverse=0
	if n_elements(zoom) lt 1 then zoom=0
	;print,'near_xy: xp,yp,zoom=',xp,yp,zoom
	;print,'near_xy: x=',x
	;print,'near_xy: y=',y

	r = sqrt( float(x-xp)*float(x-xp) + float(y-yp)*float(y-yp) )
	i = indgen(n_elements(r))
	margin = (5. / 2.^zoom)
	;print,'near_xy: r=',r
	;print,'near_xy: margin=',margin

	q = where( r lt margin)				; close proximity (was 5)
	if q[0] eq -1 then return, -1
	r = r[q]							; list of all in close proximity
	i = i[q]							; indices of these

	q = sort(r)							; sort in ascending distance order
	r = r[q]
	i = i[q]
	;print, 'Closest distance = ',r[0],'  margin=',margin

	if reverse then begin
		result = max(i)					; larger indices first
	endif else begin
		result = i[0]					; smaller distance first
	endelse

	return, result
end

;-----------------------------------------------------------------
;
; Convert pixel x,y position to image 'x,y'
;
pro pixel_to_xy, pstate, px,py, x,y, fractional=fractional

	if n_elements(fractional) eq 0 then fractional=0

	j = (*pstate).image
	x = 0
	y = 0
	if ptr_valid( (*pstate).p) eq 0 then return
	p = (*pstate).p

	if fractional then begin
		x = clip( zoom(pstate,px,/down, /fractional), 0,(*p).xsize-0.1)
		y = clip( zoom(pstate,py,/down, /fractional), 0,(*p).ysize-0.1)
	endif else begin
		x = clip( zoom(pstate,px,/down), 0,(*p).xsize-1)
		y = clip( zoom(pstate,py,/down), 0,(*p).ysize-1)
	endelse
	return
end
;
;-----------------------------------------------------------------
; plot marker area shape on current wid.
;
; If /include and in exclude mode, then just plot include shape.
; Otherwise, if include (mode=0) plot just include,
; else if exclude (mode=1) plot both.
; If compress set and mode=1, then don't use shade.

pro plot_mark, pstate, include=include, compress=compress, wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale
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
			warning,'Plot_mark',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(include) eq 0 then include=0
	if n_elements(wide) eq 0 then wide=0

	if include or ( (*pstate).analyze_mode eq 1) then begin
		shade = (*pstate).analyze_mode
		if n_elements(compress) ne 0 then shade = 0
		if( (*pstate).analyze_mode eq 1) then begin
			(*pstate).analyze_mode  = 0
			;		if (*pstate).analyze_type[(*pstate).analyze_mode] lt 1 then return
			case (*pstate).analyze_type[(*pstate).analyze_mode] of
				1: plot_box, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				2: plot_circle, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				3: plot_curve, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				4: plot_line, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				5: plot_ellipse, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				6: plot_spline, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				7: plot_spline, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				8: plot_projectX, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				9: plot_projectY, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				10: plot_spline, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				11: plot_spoint, pstate, shade=shade, compress=compress, wide=wide, $
					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				;			12: plot_mpoint, pstate, shade=shade, compress=compress, wide=wide, $
				;					xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
				else:
			endcase
			(*pstate).analyze_mode  = 1
			if include eq 0 then goto, more
		endif
		return
	endif

	more:
	if ((*pstate).analyze_type[(*pstate).analyze_mode] lt 1) $
		and ((*pstate).analyze_mode eq 1) then return

	case (*pstate).analyze_type[(*pstate).analyze_mode] of
		0: plot_distance, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		1: plot_box, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		2: plot_circle, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		3: plot_curve, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		4: plot_line, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		5: plot_ellipse, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		6: plot_spline, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		7: plot_spline, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		8: plot_projectX, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		9: plot_projectY, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		10: plot_spline, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		11: plot_spoint, pstate, compress=compress, wide=wide, $
			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		;	12: plot_mpoint, pstate, compress=compress, wide=wide, $
		;			xoff=xoff,yoff=yoff, xscale=xscale,yscale=yscale
		else:
	endcase

	return
end

;-----------------------------------------------------------------
; Plot handles at vector positions x,y

pro plot_handles, pstate, x,y, color, thick=thick

	if n_elements(x) lt 1 then return
	if n_elements(thick) lt 1 then thick=1.0
	boxx = [-1,1,1,-1,-1]
	boxy = [-1,-1,1,1,-1]
	scale = (thick eq 1.) ? 2. : 4.*thick
	for i=0L,n_elements(x)-1 do begin
;		px = clip( x[i] + boxx * scale, 0, (*pstate).width-1)
;		py = clip( y[i] + boxy * scale, 0, (*pstate).height-1)
		px = x[i] + boxx * scale
		py = y[i] + boxy * scale
		plots, px,py, /device, color=color, thick=thick
	endfor

	return
end

;----------------------------------------------------------

; Plot current distance marker

pro plot_distance, pstate, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if (*pstate).analyze_mode eq 1 then return
	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 0 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return
 
	;  0-1 are ends

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; plot the distance line

	color = spec_colour('green')
	xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
	if wide then begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=4.0	*xscale		; b/w figures
	endif else begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color
	endelse


	; plot handles

	plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, spec_colour('green'), thick=xscale

	(*p).present = 1
	return
end

;----------------------------------------------------------

; Plot current box marker

pro plot_box, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		col = spec_colour('l.grey')
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, col
		return
	endif

;	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 1 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-3 are corners, 4 is centre handle, 5 is rotate handle

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; box_vertices only returns the corners

	box_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot the box

	color = spec_colour('green')
	xy_to_pixel, pstate, x,y, px,py, compress=compress
	if wide then begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'), thick=(2.0 * (1 > xscale))			; b/w figures
	endif else begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color
	endelse


	; add centre and rotate handles and plot handles

	xy_to_pixel, pstate, [(*p).x[4],(*p).x[5]],[(*p).y[4],(*p).y[5]], px4,py4, compress=compress
	if (wide eq 0) then plot_handles, pstate, xoff+xscale*[px,px4],yoff+yscale*[py,py4], color

	; plot the rotate handle line

	xy_to_pixel, pstate, [(*p).x[1],(*p).x[5]],[(*p).y[1],(*p).y[5]], px,py, compress=compress
	plots, xoff+xscale*px,yoff+yscale*py, /device, color=color, thick=xscale

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current circle marker

pro plot_circle, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 2 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-1 are diameters, 2 is centre handle

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; circle_vertices returns the 24 circle points

	circle_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot the circle

	color = spec_colour('green')
	xy_to_pixel, pstate, x,y, px,py, compress=compress
	if wide then begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0	*xscale		; b/w figures
	endif else begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color
	endelse


	; plot handles

	xy_to_pixel, pstate, (*p).x[0:2],(*p).y[0:2], px,py, compress=compress
	if (wide eq 0) then plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current curve traverse marker

pro plot_curve, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 3 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-8 are spline curve, 9,10 are width handles

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; curve vertices

	curve_vertices, pstate, x,y,n, clx=xc, cly=yc
	if n lt 1 then return

	; plot the box
	; add plot handles

	color = spec_colour('green')
	if wide then begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0*xscale		; b/w figures

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		;	plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color, thick=2.0
	endif else begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color
	endelse

	; plot the centre-line

	xy_to_pixel, pstate, xc,yc, px,py, compress=compress
	plots, xoff+xscale*px,yoff+yscale*py, /device, color=color, thick=xscale	

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current ellipse marker

pro plot_ellipse, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 5 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-3 are diameters, 4 is centre handle

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; ellipse_vertices returns the 24 circle points

	ellipse_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot the ellipse

	color = spec_colour('green')
	xy_to_pixel, pstate, x,y, px,py, compress=compress
	if wide then begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0	*xscale		; b/w figures
	endif else begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color
	endelse


	; add plot handles

	xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
	if (wide eq 0) then plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color

	; plot the centre-line

	xy_to_pixel, pstate, [(*p).x[0],(*p).x[1]],[(*p).y[0],(*p).y[1]], px,py, compress=compress
	plots, xoff+xscale*px,yoff+yscale*py, /device, color=color, thick=xscale

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current spline marker

pro plot_spline, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if ((*pstate).analyze_type[(*pstate).analyze_mode] ne 6) and $
		((*pstate).analyze_type[(*pstate).analyze_mode] ne 7)  and ((*pstate).analyze_type[(*pstate).analyze_mode] ne 10) then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0 is centre handle, 1-* are control points

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; circle_vertices returns the spline interpolation points

	spline_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot handles

	color = spec_colour('green')
	xy_to_pixel, pstate, (*p).x[0:*],(*p).y[0:*], px,py, compress=compress
	if (wide eq 0) then plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color

	; plot the spline

	xy_to_pixel, pstate, x,y, px,py, compress=compress

	if wide then begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0	*xscale		; b/w figures
	endif else begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color
	endelse

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current line marker

pro plot_line, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 4 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-3 are corners, 4 is centre handle

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; box_vertices only returns the corners

	line_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot the box
	; add plot handles

	color = spec_colour('green')
	if wide then begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0	*xscale		; b/w figures
		;	plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('white'),thick=3.0, linestyle=1

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		;	plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color, thick=2.0
	endif else begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color
	endelse

	; plot the centre-line

	xy_to_pixel, pstate, [(*p).x[0],(*p).x[1]],[(*p).y[0],(*p).y[1]], px,py, compress=compress
	plots, xoff+xscale*px,yoff+yscale*py, /device, color=color, thick=xscale

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current project X marker

pro plot_projectX, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 8 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-3 are corners, 4 is centre handle
	;  Like Traverse (plot_line), but must constrain moves to pure X

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; box_vertices only returns the corners

	line_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot the box
	; add plot handles

	color = spec_colour('green')
	if wide then begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0	*xscale		; b/w figures
		;	plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('white'),thick=3.0, linestyle=1

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		;	plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color, thick=2.0
	endif else begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color
	endelse

	; plot the centre-line

	xy_to_pixel, pstate, [(*p).x[0],(*p).x[1]],[(*p).y[0],(*p).y[1]], px,py, compress=compress
	plots, xoff+xscale*px,yoff+yscale*py, /device, color=color, thick=xscale

	(*p).present = 1
	return
end

;-----------------------------------------------------------------
; Plot current project Y marker

pro plot_projectY, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 9 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0-3 are corners, 4 is centre handle
	;  Like Traverse (plot_line), but must constrain moves to pure Y

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; box_vertices only returns the corners

	line_vertices, pstate, x,y,n
	if n lt 1 then return

	; plot the box
	; add plot handles

	color = spec_colour('green')
	if wide then begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=3.0	*xscale		; b/w figures
		;	plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('white'),thick=3.0, linestyle=1

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		;	plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color, thick=2.0
	endif else begin
		xy_to_pixel, pstate, [x,x[0]],[y,y[0]], px,py, compress=compress
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color

		xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
		plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, color
	endelse

	; plot the centre-line

	xy_to_pixel, pstate, [(*p).x[0],(*p).x[1]],[(*p).y[0],(*p).y[1]], px,py, compress=compress
	plots, xoff+xscale*px,yoff+yscale*py, /device, color=color, thick=xscale

	(*p).present = 1
	return
end

;----------------------------------------------------------

; Plot S pixel point

pro plot_spoint, pstate, shade=shade, compress=compress,wide=wide, xoff=xoff,yoff=yoff, $
	xscale=xscale,yscale=yscale

	if n_elements(shade) lt 1 then shade=0
	if n_elements(wide) eq 0 then wide=0
	if n_elements(xoff) eq 0 then xoff=0
	if n_elements(yoff) eq 0 then yoff=0
	if n_elements(xscale) eq 0 then xscale=1.0
	if n_elements(yscale) eq 0 then yscale=1.0
	if shade then begin
		tv_q_mask, (*pstate).q, (*(*pstate).p).xsize, (*(*pstate).p).ysize, (*pstate).zoom, spec_colour('l.grey')
		return
	endif

	if (*pstate).analyze_mode eq 1 then return
;	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 11 then return
	pm = (*pstate).pmark[ (*pstate).analyze_mode ]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]
	if ptr_good(p) eq 0 then return

	;  0 is point

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then return

	; plot the point ...

	color = spec_colour('green')
	xy_to_pixel, pstate, (*p).x,(*p).y, px,py, compress=compress
	if wide then begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=spec_colour('green'),thick=4.0	*xscale		; b/w figures
	endif else begin
		plots, xoff+xscale*px,yoff+yscale*py, /device, color=color
	endelse

	; plot handles

	plot_handles, pstate, xoff+xscale*px,yoff+yscale*py, spec_colour('green'), thick=xscale

	(*p).present = 1
	return
end

;-----------------------------------------------------------------

function point_image, pstate, opt=opt, nx=nx, ny=ny

	p = (*pstate).p
	if ptr_valid( p) eq 0 then return, 0L
	check_mode, pstate
	if ptr_valid( (*p).options ) eq 0 then fix_options, p

	if (*pstate).display_mode eq 1 then begin
		if ptr_valid( (*p).escale ) eq 0 then fix_options, p
		pimg = (*p).error
		opt = (*p).escale
		nx = n_elements( (*pimg)[*,0,0] )
		ny = n_elements( (*pimg)[0,*,0] )
	endif else begin
		pimg = (*p).image
		opt = (*p).options
		nx = (*p).xsize
		ny = (*p).ysize
	endelse

	return, pimg
end

;-----------------------------------------------------------------

; This is not the full HTML writer (see "image_Save_all_HTML").
; This one is used for external programs that generate HTML, such as
; the dir crawler app and 'geopixe_index'. This one does not show a SHAPE.
; 'F' should include the full final path (e.g. with /html).

pro save_image_all_HTML, p, bw=bw, PNG=PNG, gif=gif, file=F, $
	qselect=qselect, pimg=pimg, opt=opt, nx=sx, ny=sy, display_mode=display_mode

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
			warning,'Save_image_all_HTML',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			close_file, 1
			return
		endif
	endif

	if n_elements(bw) lt 1 then bw=0
	if n_elements(png) lt 1 then png=0
	if n_elements(gif) lt 1 then gif=0
	if png eq 1 then gif=0
	if gif eq 0 then png=1
	if png then begin
		ext = '.png'
	endif else begin
		ext = '.gif'
	endelse
	tpix = 0L
	
	if ptr_valid(p) eq 0 then goto, done
	if n_elements(display_mode) lt 1 then display_mode=0
	if n_elements(pimg) lt 1 then pimg = (*p).image
	if n_elements(opt) lt 1 then opt = (*p).options
	if n_elements(sx) lt 1 then sx = (*p).xsize
	if n_elements(sy) lt 1 then sy = (*p).ysize

	xanes_stack_test, p, xanes, n_el, el, el_xanes
	if n_elements(qselect) eq 0 then qselect=indgen(n_el)

	; The supplied 'F' should contain the correct path (e.g. with /html)

	if F ne '' then begin

		F = strip_file_ext(F) + '.html'
		path = extract_path(F)
		if file_test(path,/dir) eq 0 then begin
			safe_file_mkdir, path, error=error
			if error then begin
				warning,'Save_image_all_HTML',['Failed to create directory,','or, illegal directory name:',path]
			endif
		endif

		if bw then begin
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
		printf,1,'<A href="http://www.nmp.csiro.au/dynamic.html">CSIRO Dynamic Analysis method</A>,'
		printf,1,'which enables quantitative, true-elemental images to be un-mixed from the generally complex'
		printf,1,'SXRF/PIXE energy spectrum. The images shown below are at fixed size (200 pixels) in this summary. To view its original size,'
		printf,1,'click on the image.</font><p>'
		if display_mode eq 1 then begin
			printf,1,'<p><font size="-1">These images show VARIANCE.</font><p>'
		endif
		if (*p).type eq 1 then begin
			printf,1,'<p><font size="-1">These images show end-member Mass Fraction.</font><p>'
		endif

		first = 1
		for i=0L,n_elements(qselect)-1 do begin
			s = strtrim( el[qselect[i]], 2)
			st = s
			if display_mode eq 1 then s = s + '-var'
			if bw then s = s + '-bw'
			;		print,'i=',qselect[i],' el=',s,' F=',F
			s = fix_file_name(s, /all)

			F2 = strip_path(F)
			gif_file = strip_file_ext(F2) + '-small-' + s + ext		; small postage stamps
			gif_file2 = strip_file_ext(F2) + '-' + s + ext			; normal size
			gif_file = path + fix_file_name( gif_file)
			gif_file2 = path + fix_file_name( gif_file2)

			; Build the image.
			; make_tvb may scale the image. Compress will return the scaling factor.

			args = {p:p, xanes:xanes, pimg:pimg, display_mode:display_mode, $
				qc:0L, corr_on:0, opt:opt, sx:sx, sy:sy, width:sx, height:sy, zoom:0 }

;			b = make_tvb( 0L, qselect[i], /nozoom, xtgt=200,ytgt=200, compress=compress, $
;				xmin=100,ymin=100, xmax=700,ymax=1000, args=args)

			b = make_tvb( 0L, qselect[i], /nozoom, xtgt=200,ytgt=200, compress=compress, args=args)
			if first then begin
				window, /free, xsize=n_elements(b[*,0]), ysize=n_elements(b[0,*]), /pixmap
				tpix = !d.window
				first = 0
			endif
			tv, b
			col = 115
			colb = 16
			if bw then col = spec_colour('red')
			xyouts, 1,2, st, /device, color=colb, charsize=1.5,charthick=10.0
			xyouts, 1,2, st, /device, color=col, charsize=1.5,charthick=1.2
			b = color_quan( tvrd(true=3), 3, rcol,gcol,bcol, colors=128)

			if n_elements(b) gt 1 then begin
				;			print,'HTML: save small ',gtitle,' file - ',gif_file

				if (n_elements(b[*,0]) gt 1) and (n_elements(b[0,*]) gt 1) then begin
					if png then begin
						write_png, gif_file, b, rcol,gcol,bcol
					endif else begin
						write_gif, gif_file, b, rcol,gcol,bcol
					endelse
				endif

				printf,1,'<A HREF="',strip_path(gif_file2),'"><IMG SRC="',strip_path(gif_file),'" ALIGN="CENTER"></A> '

			endif else begin
				print,'image_save_all_HTML:  Bad small image number ', qselect[i]
			endelse

			b = make_tvb( 0L, qselect[i], /nozoom, args=args)

			if n_elements(b) gt 1 then begin
				;			print,'HTML: save large ',gtitle,' file - ',gif_file2

				if (n_elements(b[*,0]) gt 1) and (n_elements(b[0,*]) gt 1) then begin
					if png then begin
						write_png, gif_file2, b, rr, gg, bb
					endif else begin
						write_gif, gif_file2, b, rr, gg, bb
					endelse
				endif

			endif else begin
				print,'image_save_all_HTML:  Bad large image number ', qselect[i]
			endelse
		endfor
		printf,1,'<P>'

		legend_file = strip_file_ext( F2) + '-legend'+ext
		legend_file = path + fix_file_name( legend_file)

		b = bytarr(300,30)
		for i=0L,300-1 do b[i,*] = byte(i/3) + 16B

		if png then begin
			write_png, legend_file, b, rr, gg, bb
		endif else begin
			write_gif, legend_file, b, rr, gg, bb
		endelse

		if display_mode eq 1 then begin
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
		printf,1,'<tr>'
		j = 0
		for i=0L,n_elements(qselect)-1 do begin
			top = ((*opt)[qselect[i]].max * (*opt)[qselect[i]].top / 100.) / charge_per_pixel
			if display_mode eq 1 then begin
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

		printf,1,'</BODY>
		printf,1,'</HTML>

		bad_io:
		if bw then begin
			tvlct, rc,gc,bc
		endif

		close_file,1
		if tpix ne 0 then wdelete, tpix
	endif

	done:
end

;-----------------------------------------------------------------

pro set_map_help, wWidget

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	map_help, pstate
end

;-----------------------------------------------------------------

pro set_image_minmax, p, pimg, opt, border=border, xonly=xonly, reset_bounds=reset_bounds

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
			warning,'set_image_minmax',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if ptr_good(p,/struct) eq 0 then return
	if n_elements(opt) eq 0 then opt = (*p).options
	if n_elements(xonly) eq 0 then xonly = 0
	if n_elements(reset_bounds) eq 0 then reset_bounds = 0
	if n_elements(pimg) eq 0 then pimg = (*p).image
	if n_elements(border) lt 1 then border = ceil(0.02*(*p).xsize) > 2
	if ptr_good(opt,/struct) eq 0 then return
	if ptr_good(pimg) eq 0 then return

	xanes_stack_test, p, xanes, n_el, el_names, el_xanes

	nx = n_elements( (*pimg)[*,0,0])
	ny = n_elements( (*pimg)[0,*,0])
	nz = n_el < n_elements( (*pimg)[0,0,*])
	first = 1

;	Need to worry about various cases, such as whether the image was sorted complete, or just
;	a few rows, in which bounds will be far less that total size. Note that 'bounds.valid' flags
;	the special device types that define a 'bounds' subset of total range for charge. Hence,
;	need to test min/max values themselves rather than 'valid'. Assume equal min/max case 
;	means that bounds have not been set properly.

; if /reset_bounds, this means a rotation or something that invalidates the bounds for X,Y or swaps them, etc.

	if reset_bounds then begin
		temp = reform((*pimg)[*,*,0])
		for i=0,nz-1 do begin
			temp = temp + (*pimg)[*,*,i]
		endfor
		q = where( temp ne 0., nq)
		if nq gt 0 then begin
			q_to_xy, q, nx, x,y
			(*p).bounds.xmin = min(x)
			(*p).bounds.xmax = max(x)
			(*p).bounds.ymin = min(y)
			(*p).bounds.ymax = max(y)
		endif
	endif

retry:
	if ((*p).bounds.xmax gt (*p).bounds.xmin) and ((*p).bounds.ymax gt (*p).bounds.ymin) then begin
		xmax = clip((*p).bounds.xmax - (*p).xoffset/(*p).xcompress, 0, nx-1)
		xmin = clip((*p).bounds.xmin - (*p).xoffset/(*p).xcompress, 0, (xmax-1)>0)
		ymax = clip((*p).bounds.ymax - (*p).yoffset/(*p).ycompress, 0, ny-1)
		ymin = clip((*p).bounds.ymin - (*p).yoffset/(*p).ycompress, 0, (ymax-1)>0)

		borderx = border < (xmax-xmin)/10				; border not too big
		bordery = border < (ymax-ymin)/10
		if xonly and first then bordery = 1

		xmax = clip((*p).bounds.xmax - (*p).xoffset/(*p).xcompress - borderx, 0, nx-1)
		xmin = clip((*p).bounds.xmin - (*p).xoffset/(*p).xcompress + borderx, 0, (xmax-1)>0)
		ymax = clip((*p).bounds.ymax - (*p).yoffset/(*p).ycompress - bordery, 0, ny-1)
		ymin = clip((*p).bounds.ymin - (*p).yoffset/(*p).ycompress + bordery, 0, (ymax-1)>0)
	endif else begin
		xmax = nx-1
		xmin = 0
		ymax = ny-1
		ymin = 0

		borderx = border < (xmax-xmin)/10				; border not too big
		bordery = border < (ymax-ymin)/10
		if xonly and first then bordery = 1
	
		xmax = nx-1-borderx
		xmin = borderx
		ymax = ny-1-bordery
		ymin = bordery
	endelse

;	print, 'set_image_minmax: xmin, xmax, ymin, ymax = ',xmin, xmax, ymin, ymax

	zmin_min = 1.0e+8
	zmax_max = 0.0
	for i=0L,nz-1 do begin
;		if (*p).bounds.valid and (xmin lt xmax) and (ymin lt ymax) then begin
		if (xmin le xmax) and (ymin le ymax) then begin
			zmin = min((*pimg)[xmin:xmax,ymin:ymax,i])
			zmax = image_weighted_max((*pimg)[xmin:xmax,ymin:ymax,i], nothing_remains=nothing_remains)
		endif else begin
			zmin = min((*pimg)[*,*,i])
			zmax = image_weighted_max((*pimg)[*,*,i], nothing_remains=nothing_remains)
		endelse
		if nothing_remains and first then begin
			border = 0
			first = 0
			goto, retry
		endif
		(*opt)[i].min = zmin
		(*opt)[i].max = zmax
		zmin_min = zmin_min < zmin
		zmax_max = zmax_max > zmax
	endfor

	if xanes then begin
		for i=0L,nz-1 do begin
			(*opt)[i].min = zmin_min
			(*opt)[i].max = zmax_max
		endfor
	endif
	return
end

;-----------------------------------------------------------------
; Set the view size and zoom for new images.
; If this is a clone, then assume that shapes already cleared.
; For zoom= and /full, don't change the tlb size, or the element.
;
;	/clone		for a clone
;	/full		for zoom to full image
;	zoom=+1,-1	for zoom in,out
;	/no_change	does not seem to be used anymore

pro set_image_view, pstate, top, clone=clone, full=full, zoom=izoom, no_change=no_change, $
	realize=realize

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
			warning,'Set_image_view',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			if widget_info( (*pstate).draw2, /valid) eq 1 then return
			free_image_state, pstate
			if n_elements(top) ge 1 then begin
				if widget_info(top,/valid) then widget_control, top, /destroy
			endif
			return
		endif
	endif

	if n_elements(clone) lt 1 then clone=0
	if n_elements(full) lt 1 then full=0
	if n_elements(izoom) lt 1 then izoom=0
	if n_elements(no_change) lt 1 then no_change=0
	if n_elements(realize) lt 1 then realize=0

	p = (*pstate).p
	if ptr_good(p) eq 0 then return
	xanes_stack_test, p, xanes, n_el, els, el_xanes
	if els[0] eq els[n_el-1] then begin
		print,'set_image_view: "els" all the same, append index ...'
		els = els + '  ' + str_tidy(indgen(n_el))
	endif

	pimg = point_image( pstate, opt=opt, nx=nx, ny=ny)
	i = (*pstate).image < (n_elements( (*pimg)[0,0,*]) - 1)

	if (realize eq 0) then begin
		widget_control, (*pstate).mode_id, set_combobox_select = (*pstate).display_mode
		check_mode, pstate
	endif
	(*pstate).interp = (*opt)[i].interp
	if widget_info( (*pstate).interp_id,/valid) then begin
		widget_control, (*pstate).interp_id, set_button = (*pstate).interp
	endif

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

;	Limit zoom for large images

	zoom0 = 0
	if (full eq 0) and (izoom eq 0) and (no_change eq 0) and ptr_valid(p) then begin
		if n_elements( *p) gt 0 then begin
			if ((*p).xsize gt 1600) or ((*p).ysize gt 1600) then begin
				r = max([ (*p).xsize/1600., (*p).ysize/1600.])
				zoom0 = -( 1 + fix(alog(r)/alog(2)))
			endif
		endif
	endif

;	Centre of view, then convert this to position on image

	print,''
	widget_control, (*pstate).draw2, get_draw_view=vv
	(*pstate).xlow = vv[0]
	(*pstate).ylow = vv[1]
	print, 'Set image view: xlow, ylow = ',(*pstate).xlow,(*pstate).ylow
	print, '	View = ', (*pstate).xview,(*pstate).yview

	xcentre = (*pstate).xlow + (*pstate).xview/2
	ycentre = (*pstate).ylow + (*pstate).yview/2
	pixel_to_xy, pstate, xcentre,ycentre, xcentre0,ycentre0
	print,'	View centre:',xcentre,ycentre,' pixmap centre:',xcentre0,ycentre0

;	Enforce some max zooms for large images

	zoom_max = 4
	if long((*p).xsize)*long((*p).ysize) gt 1000*1000L then zoom_max = 2
	if long((*p).xsize)*long((*p).ysize) gt 5000*5000L then zoom_max = 0

;	'width', 'height' are the image sizes with zoom applied
;	'w', 'h' are the viewport size to display (with scroll bars)

	new = {zoom:(*pstate).zoom, width:(*pstate).width, height:(*pstate).height, w:(*pstate).w, h:(*pstate).h }
	if clone eq 0 then begin
		if no_change eq 0 then begin
			new_zoom = ((*pstate).zoom + izoom) < zoom_max
			new.zoom = ( (izoom eq 0) or (full eq 1) ) ? zoom0 : (new_zoom)
		endif
		(*pstate).zoom = new.zoom
		wx = 300
		hy = 300
		if ptr_valid(p) then begin
			if n_elements( *p) gt 0 then begin
				wx = (*p).xsize
				hy = (*p).ysize
			endif
		endif
		new.width = zoom( pstate, wx) > 1
		new.height = zoom( pstate, hy) > 1
		new.zoom = (*pstate).zoom
	endif
	if (izoom eq 0) and (no_change eq 0) then begin
		new.w = (new.width + scr_trim) < 600
		new.h = (new.height + scr_trim) < 600
	endif else begin
		new.w = (( new.w) > (356 + scr_trim)) < (new.width + scr_xtrim)
		new.h = (( new.h) > (64 + scr_trim)) < (new.height + scr_ytrim)
	endelse
	print, '	new width,height, w,h= ', new.width, new.height, new.w, new.h 

;	Set draw widget viewport position, draw_size (pixmap size) to pixel size of image 'width','height'.
;	Set draw widget "screen" size (viewport size) to 'w', 'h'.

	widget_control, (*pstate).draw2, set_draw_view=[0,0]
	widget_control, (*pstate).draw2, draw_xsize=new.width+draw_trim, $
		draw_ysize=new.height+draw_trim, scr_xsize=new.w, scr_ysize=new.h

;	If this fails, we have run out of memory and failed to make large draw widget.
;	IDL can't seem to make a good error catch for this kind of thing, so we check by other means ...

	if widget_info( (*pstate).draw2, /valid) eq 0 then begin
		warning,'set_image_view',['Failed to allocate memory for larger image.','Draw ID has become undefined.', $
			'','Can not recover Draw widget.', 'Will need to close window.','', $
			'Retry Image window open,','and avoid excessive Zoom in "+".']
		free_image_state, pstate
		if n_elements(top) ge 1 then widget_control, top, /destroy
		return
	endif

;	Convert centre on image to centre at this zoom and offset to corner
;	to keep object in centre in the new view.

	xy_to_pixel, pstate, xcentre0,ycentre0, px,py
	xlow2 = (px - new.w/2) > 0
	ylow2 = (py - new.h/2) > 0
	print,'	After zoom - View centre:',px,py

	widget_control, (*pstate).draw2, set_draw_view=[xlow2,ylow2]
	(*pstate).xlow = xlow2
	(*pstate).ylow = ylow2
	print, '	new xlow, ylow = ',xlow2,ylow2

;	Allocate pixmaps for the foreground 'pix' and background/overlay 'pix2' pixmaps.

	allocate_pixmap, new.width, new.height, new_wid=wid, old_wid=pix, error=error
	if error then goto, bad_pix
	(*pstate).pix = wid

	allocate_pixmap, new.width, new.height, new_wid=wid, old_wid=pix2, error=error
	if error then goto, bad_pix
	(*pstate).pix2 = wid

	if (full eq 0) and (izoom eq 0) and (no_change eq 0) and (realize eq 0) then begin
		(*pstate).image = 0
		widget_control, (*pstate).element_id, set_value=els, set_combobox_select = 0
	endif

	if (full eq 0) and (izoom eq 0) and (realize eq 0) and (xanes eq 0) then begin
		if ptr_valid( pyield) then ptr_free, pyield
		if ptr_valid( plast) then ptr_free, plast
	endif
	if (clone eq 0) and (full eq 0) and (izoom eq 0) and (realize eq 0) then begin
		clear_all_markers, pstate
		if ptr_valid( pmode) then ptr_free, pmode
		pmode = ptr_new( (*pstate).analyze_mode)
		if ptr_valid( ptype) then ptr_free, ptype
		ptype = ptr_new( (*pstate).analyze_type[(*pstate).analyze_mode])
		free_image_regions, (*pstate).pregions
		(*pstate).region_id = 0

		notify, 'image-analyze-mode', pmode, from=top
		notify, 'image-analyze-type', ptype, from=top
		notify, 'image-analyze-all-clear', from=top
		notify, 'image-results-clear', from=top			; cleared in 'free_image_regions', but need to clear region table
	endif

	(*pstate).zoom = new.zoom
	(*pstate).width = new.width
	(*pstate).height = new.height
	(*pstate).w = new.w
	(*pstate).h = new.h
	(*pstate).xview = new.w - (*pstate).dw
	(*pstate).yview = new.h - (*pstate).dh

;	May need to reorganize the help widgets

	if (realize eq 0) then map_help, pstate

;	Forget why we need to do this again, nudge/wake up IDL

	if ptr_valid( p) then begin
		widget_control, (*pstate).draw2, set_draw_view=[0,0]
		draw_images, pstate
		widget_control, (*pstate).draw2, set_draw_view=[xlow2,ylow2]
	endif
	return

bad_pix:
	widget_control, (*pstate).draw2, draw_xsize=(*pstate).width+draw_trim, $
		draw_ysize=(*pstate).height+draw_trim, scr_xsize=(*pstate).w, scr_ysize=(*pstate).h
	return
end

;-----------------------------------------------------------------

; Box shape will be saved to a KVS key and/or a shape file, depending on which are enabled.
;
;	Set the Box shape in the KVS
;		origin	ox,oy (mm)
;		size	sx,sy (mm)

pro set_kvs_box, pstate, ox,oy, sx,sy

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
			warning,'set_kvs_box',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	pimg = (*pstate).p
	if ptr_valid(pimg) eq 0 then return

	w =  {	new: 0, $							; newscan
			origin: {	x: 0.0, $				; origin X
						y: 0.0, $				; 		Y
						z: 0.0}, $				; 		Z
			size: {		x: 0.0, $				; scan X size
						y: 0.0}, $				; 		Y
			zsurface: [0.,0.,0.,0.]}			; bilinear coefficients	(k,y,x,xy terms) not used yet?
	w.new = 1
	w.origin.x = ox
	w.origin.y = oy
	w.size.x = sx
	w.size.y = sy

;	Get the facility and endstation number "1" from the image metadata ...

	facility = ((*pimg).facility ne '') ? (*pimg).facility : (*pstate).kvs_prefix
	endstation = ((*pimg).endstation ne '') ? (*pimg).endstation : '1'
	if strmid( facility, strlen(facility)-1,1) ne '.' then facility = facility+'.'
	kname = facility + 'GP' + '.' + endstation
	ref = kname + '.newscan'
	kvs = (facility eq (*pstate).kvs_prefix2) ? (*pstate).kvs2 : (*pstate).kvs
	set_kvs, kvs, ref, w, error=error
	if error then begin
;			warning,'OnButton_image','Error setting Box "Newscan" in KVS.'
	endif

	print,'Box (mm): ',w.origin.x,w.origin.y, w.size.x,w.size.y
	print,'  Facility: ',facility, ' (',(*pimg).facility,'), endstation: ',endstation
	print,'  Z surface: ',w.zsurface

	if (*pstate).shape_file ne '' then begin
		safe_file_mkdir, extract_path( (*pstate).shape_file), /verbose, error=error
		if error then goto, fin
		on_ioerror, fin
		openw, unit, (*pstate).shape_file, /get_lun
		printf, unit, ox, oy
		printf, unit, sx, sy
		close_file, unit
	endif
fin:
	on_ioerror, null
	close_file, unit
	return
end

;-----------------------------------------------------------------
; Build list of single point vertices (in image coords)

pro spoint_vertices, pstate, x,y, n

	if (*pstate).analyze_type[(*pstate).analyze_mode] ne 11 then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	;  0 is point

	x = [(*p).x[0] ]
	y = [(*p).y[0] ]
	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;-----------------------------------------------------------------
; Build list of spline vertices (in image coords).

pro spline_vertices, pstate, x,y, n

	if ((*pstate).analyze_type[(*pstate).analyze_mode] ne 6) $
		and ((*pstate).analyze_type[(*pstate).analyze_mode] ne 7)  and ((*pstate).analyze_type[(*pstate).analyze_mode] ne 10) then return
	pm = (*pstate).pmark[(*pstate).analyze_mode]
	p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]

	;  1-10 (or 1-32, 1-100) are control points, 0 is centre handle

	spline_shape, (*p).x[1:*],(*p).y[1:*], x,y

	n = n_elements(x)

	maxx = max((*p).x)
	maxy = max((*p).y)
	if (max([maxx,maxy]) eq 0) then n=0

	return
end

;----------------------------------------------------------
; Build a transparent mask in colour 'col' including
; all pixels with indices given by 'q'.
; Use a grey colour, such as spec_colour('l.grey').

pro tv_q_mask, pq, nx,ny,zoom, col

	if ptr_valid(pq) eq 0 then return
	if n_elements(*pq) lt 1 then return

	mask = bytarr(nx,ny)
	mask[*pq] = 255
	nx2 = nx
	ny2 = ny

	; Use congrid to scale so that NO interpolation occurs

	if zoom gt 0 then begin
		nx2 = nx*2^zoom
		ny2 = ny*2^zoom
		mask = rebin( mask, nx2, ny2, /sample)
		;	mask = smart_congrid( mask, nx2, ny2)
	endif else if zoom lt 0 then begin
		nx2 = nx/2^(-zoom)
		ny2 = ny/2^(-zoom)
		mask = smart_congrid( mask, nx2, ny2)
	endif
	pattern = mask AND dot_fill(nx2,ny2,col)

	Write_OR = 7
	device, get_graphics_function = oldg, set_graphics_function = Write_OR
	tv, pattern

	device, set_graphics_function = oldg
	return
end

;-----------------------------------------------------------------
; Build q indices for image pointer 'p' for selected element 'i'
;	'accept' variation around mean within +/-20%, or
;	'reject' outside that

function uniformity_mask, p, qin, i, reject=reject, accept=accept

	if ptr_valid(p) eq 0 then return, -1L
	if n_elements(reject) eq 0 then reject=0
	if n_elements(accept) eq 0 then accept=0
	if accept eq 1 then reject=0
	if reject eq 0 then accept=1

	mask = bytarr( (*p).xsize, (*p).ysize)
	nxy = long((*p).xsize) * long((*p).ysize)
	mask[*] = 0

	ave = mode( (*(*p).image)[qin + nxy*i])
	r = (*(*p).image)[*,*,i] / ave
	q = where( (r le 1.2) and (r ge 0.8), nq)
	if nq eq 0 then return,-1
	mask[q] = 1

	if accept then begin
		q = where(mask eq 1)			; use only pixels within 'bounds'
	endif else if reject then begin
		q = where(mask eq 0)
	endif

	return, q
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to pixel position 'px,py'
; If compress, ignore zoom, scale x,y by compress.

pro xy_to_pixel, pstate, x,y, px,py, compress=compress, z=z

	if n_elements(z) eq 0 then z=0
	px = 0
	py = 0
	p = z ? (*pstate).pz : (*pstate).p
	if ptr_valid( p) eq 0 then return

	j = (*pstate).image
	px = 0
	py = 0

	if n_elements(compress) eq 0 then begin
		px = zoom( pstate, x)
		py = zoom( pstate, y)
	endif else begin
		px = fix(compress * x)
		py = fix(compress * y)
	endelse

	return
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to microns (if a scan size is set), else image x,y
; Use 'charge=0' to flag realtime images, in which case use 'original_xsize',
; 'original_ysize' as the actual size.
; Take care as these routines do not known about origin.

pro xy_to_microns, pstate, x,y, mx,my,munits, z=z

	if n_elements(z) eq 0 then z=0
	p = z ? (*pstate).pz : (*pstate).p
	if ptr_valid( p) eq 0 then return

	j = (*pstate).image
	mx = x
	my = y
	munits = 'pixels'

	if ((*p).scan.x gt 0.0001) and ((*p).scan.y gt 0.0001) then begin
		munits = 'mm'															; use (*pstate).size_units ???
		;	if (*p).charge eq 0.0 then begin
		mx = float(x) * float((*p).scan.x) / float((*p).original_xsize * (*p).scaled_x)
		my = float(y) * float((*p).scan.y) / float((*p).original_ysize * (*p).scaled_x)
		;	endif else begin
		;		mx = float(x) * float((*p).scan.x) / float((*p).xsize)
		;		my = float(y) * float((*p).scan.y) / float((*p).ysize)
		;	endelse
	endif

	return
end

;-----------------------------------------------------------------

function zoom, pstate, ni, down=down, fractional=fractional

	if n_elements(down) eq 0 then down=0
	if n_elements(fractional) eq 0 then fractional=0
	if fractional then begin
		n = float(ni)
		zmin = 0
	endif else begin
		n = ni
		zmin = 1
	endelse

	if down eq 0 then begin
		if (*pstate).zoom gt 0 then begin
			x = n * 2^((*pstate).zoom)
		endif else if (*pstate).zoom lt 0 then begin
			x = n / 2^(-(*pstate).zoom)
		endif else begin
			x = n
		endelse
	endif else begin
		if (*pstate).zoom gt 0 then begin
			x = (n / 2^((*pstate).zoom)) > zmin
		endif else if (*pstate).zoom lt 0 then begin
			x = (n * 2^(-(*pstate).zoom)) > zmin
		endif else begin
			x = n
		endelse
	endelse

	return, x
end

;------------------------------------------------------------------------------

; Stub routine for autoloading ...

pro image_routines
end
