;
;	Spectrum Routines for Spectrum_Display
;
;-----------------------------------------------------------------

function all_active, pstate

COMPILE_OPT STRICTARR
all = 1
for i=0L,n_elements(*(*pstate).p)-1 do begin
	if (*(*pstate).pshow)[i] ne 1 then all = 0
endfor

return, all
end

;-----------------------------------------------------------------

function active_channels, pstate

COMPILE_OPT STRICTARR

	p = *(*pstate).p
	np = n_elements(p)
	mask = bytarr( 1000)
	if np ge 1 then begin
		for i=0,np-1 do begin
			if (*(*pstate).pshow)[i] then begin
				if (*p[i]).array and ptr_good((*p[i]).pactive) then begin
					mask[ *(*p[i]).pactive] = 1
				endif else begin
					mask[ (*p[i]).station + adc_offset_device( (*p[i]).DevObj) ] = 1
				endelse
			endif
		endfor
	endif
	q2 = where( mask eq 1, nq2)

return, q2
end

;-----------------------------------------------------------------
;
; Convert channel 'ch' to pixel position 'px'
;
function ch_to_pixel, pstate, ch

COMPILE_OPT STRICTARR
j = current_plot( pstate)
if ptr_valid( (*pstate).p) eq 0 then return, 0
p = (*((*pstate).p))[j]
if ptr_valid( p) eq 0 then return, 0

e = (*p).cal.poly[1] * float(ch) + (*p).cal.poly[0]

ey_to_pixel, pstate, e,0.0, px,py, /noy

return, px
end
;
;-----------------------------------------------------------------
; Which is the current plot, for default parameters
; Normally this is the first active one.
; With highlight on, is it the highlight one? Perhaps not --> jump around with next.

function current_plot, pstate

COMPILE_OPT STRICTARR
common c_spectrum_current, ispec_current

j = 0
if (*pstate).highlight_on then begin
	nmax = n_elements( *(*pstate).p)
	(*pstate).highlight = (*pstate).highlight < (nmax-1)
	j = (*pstate).highlight
	goto, done
endif else begin
	if ptr_valid( (*pstate).pshow) then begin
		n = n_elements(*(*pstate).pshow)
		for i=0L,n-1 do begin
			if (*(*pstate).pshow)[i] eq 1 then begin
				j = i
				goto, done
			endif
		endfor
	endif
endelse

done:
	ispec_current = j
	return, j
end
;
;-----------------------------------------------------------------

; Check markers with these rules:
;	x0,x1,x4,x5 must be always present, and x5 > x4 > x1 > x0
;	if x2 not there (or outside of x1:x4, then set x2=x1
;	if x3 not there (or outside of x1:x4, then set x3=x4
; Fix markers to make these rules apply.

function check_X0X5_markers, x

COMPILE_OPT STRICTARR
if n_elements(x) lt 6 then goto, bad
if x[0] ge x[5] then goto, bad
if (x[1] le x[0]) or (x[1] ge x[4]) then goto, bad
if (x[4] le x[1]) or (x[4] ge x[5]) then goto, bad

if (x[2] le x[0]) or (x[2] ge x[5]) then x[2]=x[1]
if (x[3] le x[0]) or (x[3] ge x[5]) then x[3]=x[4]
return, 0

bad:
return, 1
end

;-----------------------------------------------------------------

; Initial parameters values for the CURVEFIT function defined by 'fname'
;	pd	points to spectrum data
;	ps	points to spectrum struct
;	mark	is X0-X5 marker array (assumed to be checked)

pro curvefit_initial, fname, pd, ps, mark, a

COMPILE_OPT STRICTARR
case fname of
	'error_function': begin
		error_function_initial, fname, pd, ps, mark, a
		end
	'gauss_function': begin
		gauss_function_initial, fname, pd, ps, mark, a
		end
	else: begin
		warning,'curvefit_initial','Unknown fitting function name ['+fname+'].'
		end
endcase

return
end

;-----------------------------------------------------------------
;  Draw one spectrum 'n'
;
; If count=0 then draw axes first
; If /first, then draw with colour=1.
; If 'fit' set then draw fit/overlay 'fit' of 'n'

pro draw_spectrum, ps, n, pstate, count=count, first=first, fit=fit, scale=scale

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
		warning,'Draw_spectrum',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

!p.charsize = 1.0
!p.charthick = 1.0
!p.linestyle = 0
!p.multi = 0
!p.psym = 0
!p.thick = 1.0
!p.title = ''
!x.charsize = 1.0
!x.style = 1
!x.thick = 1.0
!x.ticks = 0
!x.title = ''
!y.charsize = 1.0
!y.style = 1
!y.thick = 1.0
!y.ticks = 0
!y.title = ''
!p.background = spec_colour('black')
!p.color = spec_colour('white')
;!p.background = spec_colour('white')			; temp black on white
;!p.color = spec_colour('black')

if n_elements(fit) lt 1 then fit=-1
if ptr_valid( ps) eq 0 then return
if n_elements(count) lt 1 then count=0

wset, (*pstate).pix
if ((*ps).size lt 2) or (ptr_valid((*ps).data) eq 0) then begin
	erase
	return
endif

init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, select=ps, fit=fit, negative=(*pstate).show_negative
j1 = current_plot( pstate)
p1 = (*(*pstate).p)[j1]

(*ps).size = min([ (*ps).size, n_elements( *(*ps).data) ])
cal = (*ps).cal

if (count eq 0) and (fit lt 0) then begin					; plot first spectrum
	low = long(((*pstate).elow - (*ps).cal.poly[0]) / (*ps).cal.poly[1])
	low = max([0,low])
	high = long(((*pstate).ehigh - (*ps).cal.poly[0])/ (*ps).cal.poly[1])
	high = min([high > (low+2),(*ps).size-1])
	if low ge high then return
	(*ps).elow = (*pstate).elow
	(*ps).ehigh = (*pstate).ehigh

	(*pstate).cal_a = (*ps).cal.poly[1]
	(*pstate).cal_b = (*ps).cal.poly[0]
	(*pstate).cal_units = (*ps).cal.units

	x = indgen(high+1)
	e = (*ps).cal.poly[0] + (*ps).cal.poly[1] * x

	rescale_spectra, pstate, mode=(*pstate).scale_mode

	y1 = (*pstate).log ? (0.5 > (*pstate).ylow) : ((*pstate).show_negative ? (*pstate).ylow : ((*pstate).ylow > 0.0))
	y2 = (*pstate).yhigh						; > (*pstate).ylow + 0.01
	if y2 lt y1 then y2=y1+1.0
	
	(*ps).ylow = (*pstate).ylow					;*** y scale
	(*ps).yhigh = (*pstate).yhigh				;    other spectra (*ps).yhigh, ylow set in 'rescale_spectra'

	de = one_sig_figure( ((*pstate).ehigh - (*pstate).elow) / 25., /positive, /nice)

	plot, e[low:high], (*(*ps).data)[low:high], /nodata, $
			xrange = [(*pstate).elow,(*pstate).ehigh], xstyle=1, $
			yrange = [y1,y2], ystyle=1, ylog=(*pstate).log, $
			color=!p.color, $
			xmargin=[10,0], position=(*pstate).position, $
			xtickinterval=de, $
			xticklen=0.02, $
			yticklen=0.003

	if (*pstate).show_negative then plots, [e[low],e[high]],[0.0,0.0], color=!p.color, linestyle=2
	
	if ((*pstate).highlight_on eq 0) then begin
		xyouts,0.1,(*pstate).position[3]-0.06,(*ps).label,color=spec_colour('green'),/norm
		xyouts,0.5,(*pstate).position[3]-0.06,(*ps).label,color=spec_colour('green'),/norm,align=0.5
		xyouts,0.95,(*pstate).position[3]-0.06,(*ps).label,color=spec_colour('green'),/norm,align=1.0
	endif
	
	wset, (*pstate).wid1
	erase
	w = (*pstate).xoffset * (*pstate).off
	device,copy=[0,0,w,(*pstate).height, 0,(*pstate).vyoff,(*pstate).pix]
	wset, (*pstate).pix
	device,copy=[0,0,10,(*pstate).height, (*pstate).xoffset,0,(*pstate).wid1]	
endif else begin
	cal_ab, cal, ca,cb,cu, units=(*pstate).cal_units

	low = long(((*pstate).elow - cb) / ca)
	low = max([0,low])
	high = long(((*pstate).ehigh - cb)/ ca)
	high = min([high > (low+2),(*ps).size-1])
	if low ge high then return
	x = indgen(high+1)
	e = cb + ca * x
endelse

cal1 = (*p1).cal
cal_ab, cal1, ca1,cb1,cu1, units=(*pstate).cal_units
e1 = cb1 + ca1 * x

wset, (*pstate).pix
if ((*pstate).highlight_on eq 1) and ((*pstate).highlight eq n) and (fit lt 0) then begin
	colour = 'green'
endif else if ((*pstate).highlight_on eq 1) and ((*pstate).highlight ne n) and (fit lt 0) then begin
	colour = 'd.grey'
endif else if n_elements(first) ge 1 then begin
	colour = 'green'
endif else begin
	colour = count
endelse

if not keyword_set(scale) then begin
	case (*pstate).scale_mode of
		0: begin
			scale = 1.0
			end
		1: begin
			scale = 1.0
			end
		2: begin
			scale = float((*p1).yhigh) / float((*ps).yhigh)
			end
		3: begin
			scale = float((*p1).yhigh) / float((*ps).yhigh)
			end
	endcase
endif
;print, 'Mode, Scale = ',(*pstate).scale_mode,scale
;print, 'cal1.poly[1], cal.poly[1] = ', cal1.poly[1], cal.poly[1]

if (*ps).has_errors or ((*pstate).show_errors and (fit lt 0)) then begin
	for i=low,high do begin
		x = [e[i],e[i]]
		y1 = (*pstate).log ? 10.0^!y.crange[0] : !y.crange[0]
		y2 = (*pstate).log ? 10.0^!y.crange[1] : !y.crange[1]
		if y2 lt y1 then y2=y1+1.0
		dy = ptr_valid((*ps).error) ? (*(*ps).error)[i] : sqrt((*(*ps).data)[i])
		y = [(*(*ps).data)[i] - dy, (*(*ps).data)[i] + dy]
		y = clip(scale* y, y1,y2)
		plots, x, y, color=spec_colour(colour)
 	endfor
	oplot, e[low:high], scale* (*(*ps).data)[low:high], psym=6, color=spec_colour(colour)
	if (*ps).has_mdl then begin
		oplot, e[low:high], scale* (*(*ps).mdl)[low:high], psym=0, linestyle=2, color=spec_colour(colour)
	endif
endif else begin
	if fit ge 0 then begin
		oplot, e[low:high], scale* (*(*ps).data)[low:high], psym=0, color=spec_colour(colour)
		if (*pstate).show_diff and (fit eq 0) then begin
			ybase = (*p1).log ? 0.5 : 0.0
			t = map_spec( (*ps).data, cal, cal1, error=err)
			if (*p1).n_fit ge 2 then begin								; look for other "Back" components to add to 't' to subtract
				for j=1,(*p1).n_fit-1 do begin
					if locate( 'Back', (*(*p1).fit[j]).label) ge 0 then begin
						t = t + map_spec( (*(*p1).fit[j]).data, cal, cal1, error=err)
					endif
				endfor
			endif
			scale_to_1 = cal1.poly[1] / cal.poly[1]						; @9-20
			if err eq 0 then oplot, e1[low:high], (scale/scale_to_1)*( (*(*p1).data)[low:high] - t[low:high])>ybase, psym=0, color=spec_colour(colour)
		endif
	endif else begin
		if (*pstate).chart then begin
			psym_hist = 0
			q = where( *(*ps).data ne 0.0, nq)
		endif else begin
			psym_hist = 10
			nq = high-low+1
			q = low + indgen(nq)
		endelse
		if nq gt 0 then oplot, e[q], scale* (*(*ps).data)[q], psym=psym_hist, color=spec_colour(colour)
	endelse
endelse

if ((*pstate).highlight_on) and (n eq (*pstate).highlight) then begin
	xyouts,0.1,(*pstate).position[3]-0.06,(*ps).label,color=spec_colour('green'),/norm
	xyouts,0.5,(*pstate).position[3]-0.06,(*ps).label,color=spec_colour('green'),/norm,align=0.5
	xyouts,0.95,(*pstate).position[3]-0.06,(*ps).label,color=spec_colour('green'),/norm,align=1.0
endif

; if (fit lt 0) and ((*p[n]).n_fit gt 0) then begin
if ((*ps).n_fit gt 0) and ((*pstate).highlight_on eq 0) then begin
	fcount = count+1
	for j=0L,(*ps).n_fit-1 do begin
		if ptr_valid( (*ps).fit[j]) then begin
			scale_to_1 = cal1.poly[1] / (*(*ps).fit[j]).cal.poly[1]						; @9-20
;			print,'scale_to_1 =',scale_to_1
			if (*ps).showfit and ( (*(*ps).fit[j]).size gt 0) then begin
				draw_spectrum, (*ps).fit[j], n, pstate, count=fcount, fit=j, scale=scale * scale_to_1
				fcount = fcount+1
			endif
		endif
	endfor
endif

return
end
;
;-----------------------------------------------------------------------
;
pro draw_spectra, pstate

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
		warning,'Draw_spectra',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

tin  = systime(/seconds)
if ptr_valid( (*pstate).p) eq 0 then goto, fin
p = *((*pstate).p)
if ptr_valid( p[0]) eq 0 then goto, fin
np = n_elements(p)

i = current_plot( pstate)
(*pstate).a = ((*pstate).ehigh - (*pstate).elow) / $
				(float((*pstate).width*(1.+(*pstate).position[0])+1) * $
				((*pstate).position[2]-(*pstate).position[0]))
(*pstate).b = (*pstate).elow - (*pstate).a * (*pstate).position[0] * float((*pstate).width*(1.+(*pstate).position[0])+1) + $
				float((*pstate).xoffset * (*pstate).a)

draw_spectrum, p[i], i, pstate, count=0
count = 0
for j=0L,np-1 do begin
	if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then count = count+1
endfor
if count gt 1 then begin
	for j=np-1,0,-1 do begin
		if (*(*pstate).pshow)[j] and ((*p[j]).size gt 0) then begin
			count = count-1
			if (j eq i) or (count eq 0) then begin
				draw_spectrum, p[j], j, pstate, count=1, /first
			endif else begin
				draw_spectrum, p[j], j, pstate, count=count
			endelse
		endif
	endfor
endif
(*pstate).highlight = (*pstate).highlight < (n_elements(p)-1)
if (*pstate).highlight_on then draw_spectrum, p[(*pstate).highlight], (*pstate).highlight, pstate, count=1

if (*pstate).log then begin
	yl = (0.5 > (*pstate).ylow)
	(*pstate).ya = (alog10((*pstate).yhigh) - alog10(yl)) / $
			(float((*pstate).height)*(!y.window[1]-!y.window[0]))
	(*pstate).yb = alog10(yl) - (*pstate).ya * !y.window[0] * float((*pstate).height)
endif else begin
	yl = ((*pstate).show_negative ? (*pstate).ylow : ((*pstate).ylow > 0.0))
	(*pstate).ya = ((*pstate).yhigh - (*pstate).ylow) / $
			(float((*pstate).height)*(!y.window[1]-!y.window[0]))
	(*pstate).yb = (*pstate).ylow - (*pstate).ya * !y.window[0] * float((*pstate).height)
endelse
 
wset, (*pstate).wid2
device,copy=[(*pstate).xoffset,0,(*pstate).width-(*pstate).xoffset,(*pstate).height, 0,0,(*pstate).pix]
(*pstate).pix2_valid = 0

plot_markers, pstate
fin:
	t = systime(/seconds)
	*(*pstate).ppercent = 100.*(t-tin)/(t-(*pstate).last_time)
	(*pstate).last_time = t
	return
end

;-----------------------------------------------------------------
;
; Convert 'e','counts' to pixel position 'px,py'
;
pro ey_to_pixel, pstate, e,counts, px,py, noy=noy

COMPILE_OPT STRICTARR
if n_elements(noy) lt 1 then noy=0
py = 0
if noy eq 0 then begin
	c = counts
	if (*pstate).log then c = alog10(counts > 0.5)
	py = (c - (*pstate).yb) / (*pstate).ya
endif

px = round((e - (*pstate).b) / (*pstate).a)

return
end
;
;-----------------------------------------------------------------

function first_active, pstate

COMPILE_OPT STRICTARR
for i=0L,n_elements(*(*pstate).p)-1 do begin
	if ptr_valid( (*(*pstate).p)[i]) then begin
		if (*(*pstate).pshow)[i] eq 1 then return, i
	endif
endfor

return, 0
end

;-----------------------------------------------------------------

pro free_spectrum_state, pstate

if n_elements(pstate) lt 1 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

*(*pstate).ppercent = 0.0
if (*pstate).pix ge 0 then wdelete, (*pstate).pix
(*pstate).pix = -1
if (*pstate).pix2 ge 0 then wdelete, (*pstate).pix2
(*pstate).pix2 = -1

if ptr_valid((*pstate).p) then begin
    if (*(*(*pstate).p)[0]).orphan eq 1 then begin
       (*pstate).local = 1
       (*(*(*pstate).p)[0]).orphan = 0
    endif
    if ((*pstate).local) then free_spectra, (*pstate).p
endif

if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
if ptr_valid( (*pstate).dpath) then ptr_free, (*pstate).dpath
if ptr_valid( (*pstate).root) then ptr_free, (*pstate).root
if ptr_valid( (*pstate).matrix) then free_DA, (*pstate).matrix
if ptr_valid( (*pstate).pshow) then ptr_free, (*pstate).pshow
if ptr_valid( (*pstate).pe) then ptr_free, (*pstate).pe
if ptr_valid( (*pstate).px) then ptr_free, (*pstate).px
if ptr_valid( (*pstate).pv) then ptr_free, (*pstate).pv
if ptr_valid( (*pstate).pf) then ptr_free, (*pstate).pf
if ptr_valid( (*pstate).pevt) then ptr_free, (*pstate).pevt
if ptr_valid( (*pstate).pfilter) then ptr_free, (*pstate).pfilter
if ptr_valid( (*pstate).pdetector) then ptr_free, (*pstate).pdetector
if ptr_valid( (*pstate).pexport) then ptr_free, (*pstate).pexport
if ptr_valid( (*pstate).pselect) then ptr_free, (*pstate).pselect
if ptr_valid( (*pstate).pileup) then ptr_free, (*pstate).pileup
if ptr_valid( (*pstate).pflux) then ptr_free, (*pstate).pflux
if ptr_valid( (*pstate).pdepth) then ptr_free, (*pstate).pdepth
if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz

if ptr_valid( (*pstate).pfit) then begin
    if n_elements( *(*pstate).pfit) gt 0 then begin
       if ptr_valid( (*(*pstate).pfit).cuts) then ptr_free, (*(*pstate).pfit).cuts
       if ptr_valid( (*(*pstate).pfit).yields) then ptr_free, (*(*pstate).pfit).yields
       if ptr_valid( (*(*pstate).pfit).peaks) then ptr_free, (*(*pstate).pfit).peaks
       if ptr_valid( (*(*pstate).pfit).pz_fit) then ptr_free, (*(*pstate).pfit).pz_fit
       if ptr_valid( (*(*pstate).pfit).pz_mdl) then ptr_free, (*(*pstate).pfit).pz_mdl
       if ptr_valid( (*(*pstate).pfit).filter) then ptr_free, (*(*pstate).pfit).filter
       if ptr_valid( (*(*pstate).pfit).filter_list) then ptr_free, (*(*pstate).pfit).filter_list
       if ptr_valid( (*(*pstate).pfit).detector) then ptr_free, (*(*pstate).pfit).detector
       if ptr_valid( (*(*pstate).pfit).detector_list) then ptr_free, (*(*pstate).pfit).detector_list
       if ptr_valid( (*(*pstate).pfit).save_detector) then ptr_free, (*(*pstate).pfit).save_detector
       if ptr_valid( (*(*pstate).pfit).playout) then ptr_free, (*(*pstate).pfit).playout
       if ptr_valid( (*(*pstate).pfit).pcorrect) then ptr_free, (*(*pstate).pfit).pcorrect
       if ptr_valid( (*(*pstate).pfit).compton) then ptr_free, (*(*pstate).pfit).compton
       if ptr_valid( (*(*pstate).pfit).xanes_energies) then ptr_free, (*(*pstate).pfit).xanes_energies
       ptr_free, (*pstate).pfit
    endif
endif
if ptr_valid( (*pstate).pfitg) then begin
    if n_elements( *(*pstate).pfitg) gt 0 then begin
       if ptr_valid( (*(*pstate).pfitg).cuts) then ptr_free, (*(*pstate).pfitg).cuts
       if ptr_valid( (*(*pstate).pfitg).yields) then ptr_free, (*(*pstate).pfitg).yields
       if ptr_valid( (*(*pstate).pfitg).peaks) then ptr_free, (*(*pstate).pfitg).peaks
       if ptr_valid( (*(*pstate).pfitg).pz_fit) then ptr_free, (*(*pstate).pfitg).pz_fit
       if ptr_valid( (*(*pstate).pfitg).pz_mdl) then ptr_free, (*(*pstate).pfitg).pz_mdl
       if ptr_valid( (*(*pstate).pfitg).filter) then ptr_free, (*(*pstate).pfitg).filter
       if ptr_valid( (*(*pstate).pfitg).filter_list) then ptr_free, (*(*pstate).pfitg).filter_list
       if ptr_valid( (*(*pstate).pfitg).detector) then ptr_free, (*(*pstate).pfitg).detector
       if ptr_valid( (*(*pstate).pfitg).detector_list) then ptr_free, (*(*pstate).pfitg).detector_list
       ptr_free, (*pstate).pfitg
    endif
endif
if ptr_valid( (*pstate).player) then begin
    if n_elements( *(*pstate).player) gt 0 then begin
       if ptr_valid( (*(*pstate).player).peaks) then ptr_free, (*(*pstate).player).peaks
       if ptr_valid( (*(*pstate).player).peaks2) then ptr_free, (*(*pstate).player).peaks2
       ptr_free, (*pstate).player
    endif
endif
if ptr_valid( (*pstate).playerg) then begin
    if n_elements( *(*pstate).playerg) gt 0 then begin
       if ptr_valid( (*(*pstate).playerg).peaks) then ptr_free, (*(*pstate).playerg).peaks
       if ptr_valid( (*(*pstate).playerg).peaks2) then ptr_free, (*(*pstate).playerg).peaks2
       ptr_free, (*pstate).playerg
    endif
endif
if ptr_valid( (*pstate).pcut) then begin
    for i=0L,n_elements( *(*pstate).pcut)-1 do begin
       if ptr_valid( (*(*pstate).pcut)[i]) then ptr_free, (*(*pstate).pcut)[i]
    endfor
endif
if ptr_valid( (*pstate).presults) then begin
    for i=0L,n_elements( *(*pstate).presults)-1 do begin
       if ptr_valid( (*(*pstate).presults)[i]) then ptr_free, (*(*pstate).presults)[i]
    endfor
endif
return
end

;-----------------------------------------------------------------

pro init_spectra, pp, w,v, new=new, select=pselect, fit=fit, negative=negative

;	'fit' seems redundant now, with passing of pselect pointer (which can be a fit)

COMPILE_OPT STRICTARR
if n_elements(new) lt 1 then new=0
if n_elements(fit) lt 1 then fit=-1
if n_elements(negative) lt 1 then negative=0

if ptr_valid(pp) eq 0 then return
p = *pp
n = n_elements(p)
if n_elements(pselect) gt 0 then begin
	i1 = 0
	i2 = 0
endif else begin
	i1 = 0
	i2 = n-1
endelse

for i=i1,i2 do begin
	if n_elements(pselect) gt 0 then begin
		ps = pselect
	endif else begin
		ps = p[i]
	endelse
	if ptr_valid( ps) then begin
		if ((*ps).cal.order lt 1) or (abs((*ps).cal.poly[1]) lt 1.0e-20) then begin
			(*ps).cal.poly[0] = 0.0
			(*ps).cal.poly[1] = 1.0
			(*ps).cal.order = 1
			(*ps).cal.units = 'channel'
		endif
		(*ps).emin = (*ps).cal.poly[0]
		(*ps).emax = ((*ps).cal.poly[0] + (*ps).cal.poly[1] * ((*ps).size-1)) $
					* (float(w) / float(v))

		if ptr_valid((*ps).data) eq 0 then (*ps).data = ptr_new(0L)

		if new then begin
			(*ps).elow = (*ps).emin > 0.01
			(*ps).ehigh = (*ps).emax
			(*ps).ylow = min(*(*ps).data)
			(*ps).ylow = ((*ps).ylow lt 0.) ? 1.3*(*ps).ylow : 0.7*(*ps).ylow
			if negative eq 0 then (*ps).ylow = (*ps).ylow > 0.0
			(*ps).yhigh = (max(*(*ps).data) * 1.1) > (0.01 > ((*ps).ylow + 0.01))
	;		(*ps).log = 1

	;		if fit lt 0 then begin
	;			for j=0L,(*ps).n_fit-1 do begin
	;				init_spectra, pp, w,v, new=new, select=(*ps).fit[j], fit=j, negative=negative
	;			endfor
	;		endif
		endif
		if negative then begin
			(*ps).ylow = min(*(*ps).data)
			(*ps).ylow = ((*ps).ylow lt 0.) ? 1.3*(*ps).ylow : 0.7*(*ps).ylow
		endif
	endif
endfor
return
end
;
;-----------------------------------------------------------------

function last_active, pstate

COMPILE_OPT STRICTARR
for i=n_elements(*(*pstate).p)-1,0,-1 do begin
	if ptr_valid( (*(*pstate).p)[i]) then begin
		if (*(*pstate).pshow)[i] eq 1 then return, i
	endif
endfor

return, 0
end

;--------------------------------------------------------------------

function near, a,b

r = 0
if abs(a-b) lt 6 then r = 1
return, r
end

;-----------------------------------------------------------------
;
; Convert pixel x,y position to energy 'e', channel 'ch' and 'counts'
;
pro pixel_to_exy, pstate, px,py, e, ch, counts, select=j

COMPILE_OPT STRICTARR
e = 0.0
ch = 0L
counts = 0L
if ptr_valid( (*pstate).p) eq 0 then return

if n_elements(j) lt 1 then j = current_plot( pstate)
p = (*((*pstate).p))[j]
if ptr_valid( p) eq 0 then return

pixel_to_ey, pstate, px,py, e, counts

ch = (e - (*p).cal.poly[0]) / (*p).cal.poly[1]
return
end
;
;-----------------------------------------------------------------
;
; Convert pixel x,y position to energy 'e', and 'counts'
;
pro pixel_to_ey, pstate, px,py, e,counts

COMPILE_OPT STRICTARR
e = (*pstate).a * float(px) + (*pstate).b
counts = (*pstate).ya * py + (*pstate).yb
if (*pstate).log then counts = 10.0^counts
;print,'pixel=',py,' (*pstate).a,b=',(*pstate).a,(*pstate).b,' counts=',counts

return
end
;
;-----------------------------------------------------------------

pro plot_markers, pstate, veto=veto, moving=moving

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
		warning,'Plot_markers',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if (*pstate).mark_set eq (*pstate).element_set then begin
	plot_element, pstate
	return
endif
if (*pstate).mark_set ge (*pstate).max_set then return

if n_elements(veto) lt 1 then veto=-1
if n_elements(moving) lt 1 then moving=-1

if moving ge 0 then begin
	x = [(*pstate).xmark[moving,(*pstate).mark_set],(*pstate).xmark[moving,(*pstate).mark_set]]
	plots,x,[0,(*pstate).height],color=spec_colour('red'),/device
endif else begin
	for i=0L,(*pstate).nmark[(*pstate).mark_set]-1 do begin
		if i ne veto then begin
			(*pstate).xmark[i,(*pstate).mark_set] = ch_to_pixel( pstate, (*pstate).cmark[i,(*pstate).mark_set])
			x = [(*pstate).xmark[i,(*pstate).mark_set],(*pstate).xmark[i,(*pstate).mark_set]]
			plots,x,[0,(*pstate).height],color=spec_colour('red'),/device
		endif
	endfor
endelse

return
end

;-----------------------------------------------------------------

pro plot_element, pstate

COMPILE_OPT STRICTARR
if (*pstate).n_lines lt 1 then return
if ptr_good(pstate) eq 0 then return
if ptr_good( (*pstate).p) eq 0 then return
j = current_plot( pstate)
p = (*(*pstate).p)[j]

wset, (*pstate).wid2
bot = !y.window[0] * (*pstate).height	; warning: !y.window may not stay valid

for shell=0,2 do begin
	top = 1.0E+6
	q = where( (*pstate).shell[0:(*pstate).n_lines-1] eq shell)
	if q[0] ne -1 then begin
		for i=0L, n_elements(q)-1 do begin
			if (*pstate).rel[q[i]] gt 0.05 then begin
				k = ((*pstate).e[q[i]] - (*p).cal.poly[0]) / (*p).cal.poly[1]
				if (k ge 0) and (k lt (*p).size) then begin
					r = (*(*p).data)[k] / (*pstate).rel[q[i]]
					if (r lt top) and ((*(*p).data)[k] ge 1) then top = r
				endif
			endif
		endfor
		top = top > ((*pstate).ya*bot + (*pstate).yb)

		for i=0L, n_elements(q)-1 do begin								; lines
			ey_to_pixel, pstate, (*pstate).e[q[i]],top*(*pstate).rel[q[i]], px,py

			py = py > bot + 20
			plots,[px,px]+1,[bot,py],color=spec_colour('orange'),/device,thick=2
		endfor

		q2 = where( (*pstate).shell[0:(*pstate).n_lines-1] eq -shell)			; edges
		if shell eq 0 then q2 = where( (*pstate).shell[0:(*pstate).n_lines-1] eq -10)
		if q2[0] ne -1 then begin
			for i=0L, n_elements(q2)-1 do begin
				ey_to_pixel, pstate, (*pstate).e[q2[i]],top*(*pstate).rel[q2[i]], px,py

				py = py > bot + 20
				plots,[px,px]+1,[bot,py],color=spec_colour('l.blue'),/device, $
					thick=2,linestyle=1
			endfor
		endif
	endif
endfor

return
end

;--------------------------------------------------------------------

; Rescale spectra

pro rescale_spectra, pstate, mode=mode

if n_elements(mode) lt 1 then mode=0
if mode eq 0 then return
r_high = 1.08				; max head room
r_low_neg = 1.2				; negative max head room
r_low = 0.7					; bottom head room

if ptr_valid( (*pstate).p) then begin
	cj = current_plot( pstate)
	pc = (*(*pstate).p)[cj]
	cut = round( (*pstate).cmark[0:1,4])					; Cut 0,1
	ecut = cut * (*pc).cal.poly[1] + (*pc).cal.poly[0]
	ca = (*pc).cal.poly[1]

	if (mode eq 3) and (cut[1] le cut[0]) then begin
		warning,'rescale_spectra',['Need to set a cut range first.','Then use "Cut" vertical scale mode.']
		widget_control, (*pstate).rescale, set_combobox_select=0
		(*pstate).scale_mode = 0
		return
	endif

	pixel_to_exy, pstate, (*pstate).vlow,0, e,low,c
	pixel_to_exy, pstate, (*pstate).vhigh,0, e,high,c
	np = n_elements( *(*pc).data)
	low = clip( low, 0, np-1)
	high = clip( high, low+1, np-1)

	dl = (*pc).has_errors ? ((*(*pc).data)[low:high]-(*(*pc).error)[low:high]) : (*(*pc).data)[low:high]
	dh = (*pc).has_errors ? ((*(*pc).data)[low:high]+(*(*pc).error)[low:high]) : (*(*pc).data)[low:high]
	ylow = min(dl)
	ylow = (ylow lt 0.) ? r_low_neg*ylow : r_low*ylow
	if (*pstate).show_negative eq 0 then ylow = ylow > 0.0
	yhigh = max(dh) * r_high
	sylow = ylow
	syhigh = yhigh

	case mode of
		1: begin													; auto
			ylow = min(dl)
			ylow = (ylow lt 0.) ? r_low_neg*ylow : r_low*ylow
			if (*pstate).show_negative eq 0 then ylow = ylow > 0.0
			yhigh = max(dh) * r_high
			end
		2: begin													; norm
			ylow = min(dl)
			ylow = (ylow lt 0.) ? r_low_neg*ylow : r_low*ylow
			if (*pstate).show_negative eq 0 then ylow = ylow > 0.0
			yhigh = max(dh) * r_high
			end
		3: begin													; cut
			ylow = 0.
			ch = clip((ecut - (*pc).cal.poly[0]) / (*pc).cal.poly[1],0,np-1)
			cr = (*pc).cal.poly[1] / ca
			yhigh = total((*(*pc).data)[ch[0]:ch[1]]) * cr
			end
	endcase
	rylow = abs(ylow) gt 1.0e-6 ? abs(sylow / ylow) : 1.0
	ryhigh = abs(yhigh) gt 1.0e-6 ? abs(syhigh / yhigh) : 1.0

	(*pc).ylow = 1.0E+10
	(*pc).yhigh = -1.0E+10
	for j=0L,n_elements(*(*pstate).p)-1 do begin
		p = (*(*pstate).p)[j]
		if ((*(*pstate).pshow)[j] eq 1) or (j eq cj) then begin
			pixel_to_exy, pstate, (*pstate).vlow,0, e,low,c, select=j
			pixel_to_exy, pstate, (*pstate).vhigh,0, e,high,c, select=j
			np = n_elements( *(*p).data)
 			low = clip( low, 0, np-1)
			high = clip( high, low+1, np-1)

			dl = (*p).has_errors ? ((*(*p).data)[low:high]-(*(*p).error)[low:high]) : (*(*p).data)[low:high]
			dh = (*p).has_errors ? ((*(*p).data)[low:high]+(*(*p).error)[low:high]) : (*(*p).data)[low:high]
			case mode of
				1: begin										; auto
					ylow = min(dl)
					ylow = (ylow lt 0.) ? r_low_neg*ylow : r_low*ylow
					if (*pstate).show_negative eq 0 then ylow = ylow > 0.0
					yhigh = max(dh) * r_high
					(*pc).ylow = min( [(*pc).ylow, ylow * rylow])
					(*pc).yhigh = max( [(*pc).yhigh, yhigh * ryhigh])
					end
				2: begin										; norm
					ylow = min(dl)
					ylow = (ylow lt 0.) ? r_low_neg*ylow : r_low*ylow
					if (*pstate).show_negative eq 0 then ylow = ylow > 0.0
					yhigh = max(dh) * r_high
					(*p).ylow = ylow * rylow
					(*p).yhigh = yhigh * ryhigh
					end
				3: begin										; cut
					ylow = 0.
					ch = clip((ecut - (*p).cal.poly[0]) / (*p).cal.poly[1],0,np-1)
					cr = (*p).cal.poly[1] / ca
					yhigh = total((*(*p).data)[ch[0]:ch[1]]) * cr
					(*p).ylow = ylow * rylow
					(*p).yhigh = yhigh * ryhigh
					end
			endcase
		endif
	endfor

	if mode eq 1 then begin
		for j=0L,n_elements(*(*pstate).p)-1 do begin
			p = (*(*pstate).p)[j]
			if ((*(*pstate).pshow)[j] eq 1) then begin
				(*p).ylow = (*pc).ylow
				(*p).yhigh = (*pc).yhigh
			endif
		endfor
	endif
	if mode ge 1 then begin
		(*pstate).ylow = (*pc).ylow
		(*pstate).yhigh = (*pc).yhigh
	endif
endif

return
end

;--------------------------------------------------------------------

; Select just spectrum 'n'.
; If /keep_less, then don't alter selection of spectra < 'n'

pro show_one_spectrum, pstate, n, keep_less=keep

COMPILE_OPT STRICTARR
if (n lt 0) or ( n ge n_elements(*(*pstate).p)) then return
if n_elements(keep) lt 1 then keep=0

active = first_active(pstate)

elow = (*(*(*pstate).p)[active]).elow
ehigh = (*(*(*pstate).p)[active]).ehigh
cal_a = (*(*(*pstate).p)[active]).cal.poly[1]
cal_b = (*(*(*pstate).p)[active]).cal.poly[0]

active = n

if (abs(cal_a - (*(*(*pstate).p)[active]).cal.poly[1]) lt 0.001) and $
   (abs(cal_b - (*(*(*pstate).p)[active]).cal.poly[0]) lt 0.1) then begin
		(*(*(*pstate).p)[active]).elow = elow
		(*(*(*pstate).p)[active]).ehigh = ehigh
endif

for i=n_elements(*(*pstate).p)-1,0,-1 do begin
	(*(*pstate).pshow)[i] = ((i eq active) ? 1 : 0)
	if (i eq active) and keep then return
endfor

return
end

;-----------------------------------------------------------------

pro Spectrum_background, pstate, pvalues

COMPILE_OPT STRICTARR
if ptr_valid( pvalues) eq 0 then goto, done
s = size(*pvalues, /structure)
if s.type_name ne 'STRUCT' then goto, done
if ptr_valid( (*pstate).p) eq 0 then goto, done
p = *((*pstate).p)
n = current_plot( pstate)
ps = p[n]
if ptr_valid( ps) eq 0 then goto, done
pd = (*ps).data
if ptr_valid( pd) eq 0 then goto, done

e = findgen((*ps).size)
e = (*ps).cal.poly[1] * e + (*ps).cal.poly[0]

a1 = (*pvalues).value1
a2 = (*pvalues).value2
a3 = (*pvalues).value3
a4 = (*pvalues).value4
a5 = (*pvalues).value5

u = e^(-3)
y = exp(-a1*u)*( a2*exp(-a3*e) ) + a4*exp(-a5*e)

null_spectrum = define(/spectrum)
spec = null_spectrum
spec.source = (*ps).source
spec.label = 'Adjust background'
spec.cal.poly[0] = (*ps).cal.poly[0]
spec.cal.poly[1] = (*ps).cal.poly[1]
spec.cal.units = (*ps).cal.units
spec.cal.order = 1
spec.comment = 'Adjusted background'

spec.size = n_elements(y)
spec.data = ptr_new(y, /no_copy)

if (*ps).n_fit gt 0 then begin
	for i=0L,(*ps).n_fit-1 do begin
		if ptr_valid((*ps).fit[i]) then free_spectrum, (*ps).fit[i]
	endfor
	(*ps).n_fit = 0
endif
(*ps).fit[0] = ptr_new(spec, /no_copy)
(*ps).n_fit = 1
draw_spectra, pstate

done:
end

;----------------------------------------------------------------------------

; Spectrum stub routine ...

pro spectrum_routines
end
