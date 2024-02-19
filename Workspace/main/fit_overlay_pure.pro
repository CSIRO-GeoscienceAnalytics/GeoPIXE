pro fit_overlay_pure, pstate

; Overlay the pure element spectra on the spectrum (after a fit and DAM generate).

COMPILE_OPT STRICTARR
common c_null_spec_1, max_history, max_cal,  max_fit
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'fit_overlay_pure',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

pmatrix = (*pstate).pda
if ptr_valid(pmatrix) eq 0 then goto, bad_matrix
pspec = (*pstate).pspec
if ptr_valid(pspec) eq 0 then goto, bad_ptr
if ptr_valid((*pstate).presults) eq 0 then goto, bad_ptr
presults = (*(*pstate).presults)[n_elements(*(*pstate).presults)-1]

shell_menu = ['','','L','M']
el = (*presults).el.name
el_code, strcompress(el,/remove_all), t,z,shell
q = where( z gt 0)
if q[0] ne -1 then el[q] = element_name( z[q] ) + shell_menu[ shell[q]]
conc = (*presults).conc
charge = (*presults).spectrum.charge
multiplicity = (*presults).spectrum.multiplicity
cal_ratio = (*pmatrix).cal.a / (*pspec).cal.poly[1]

; cal_offset  what happended to this> (see build_da_fit).
; offset = fix(((*pmatrix).cal.b - (*pspec).cal.poly[0]) / (*pspec).cal.poly[1])

cal = (*pspec).cal
cal_ab, cal, (*pmatrix).cal.a,(*pmatrix).cal.b,'keV', /set
null_spectrum = define(/spectrum)

for i=0L,(*pspec).n_fit-1 do begin
	if ptr_valid( (*pspec).fit[i] ) then free_spectrum, (*pspec).fit[i]
endfor
(*pspec).n_fit = 0

if (*pmatrix).array.on then begin
	if (*pspec).array then begin
		active = *(*pspec).pactive
	endif else begin
		active = (*pspec).station + adc_offset_device((*pspec).DevObj)
	endelse
	rG = fltarr((*pmatrix).n_el)
	q = where(active lt (*pmatrix).array.n_det, nd)
	if q[0] ne -1 then begin
		active = active[q]
		for j=0L,nd-1 do begin
			rG = rG + (*pmatrix).array.rGamma[active[q[j]],*]
		endfor
	endif else begin
		warning,'fit_overlay_pure','Array detector data, but no "pactive".'
		rG = 1.
	endelse
endif else begin
	rG = replicate(float(multiplicity > 1), (*pmatrix).n_el)
endelse

; Normal: select all pure overlays ...
select_overlay = intarr((*pmatrix).n_pure)
select_overlay[0] = -1

; For selective pure overlays uncomment this ...
;select_overlay = intarr((*pmatrix).n_pure)
;q = where( (*pmatrix).el eq 'Fe')
;if q[0] ne -1 then select_overlay[q[0]] = 1
;q = where( (*pmatrix).el eq 'AuL')
;if q[0] ne -1 then select_overlay[q[0]] = 1

for j=0L,(*pmatrix).n_pure-1 do begin
	if (strlowcase( strmid((*pmatrix).el[j],0,4)) ne 'back') then begin
		k = contains( el, (*pmatrix).el[j])
;		el_code, (*pmatrix).el[j], mt,mz,mshell
;		k = where((mz[0] eq z) and (mshell[0] eq shell))
;		if (mz[0] ge 1) and (k[0] ge 0) then begin
		if (k[0] ge 0) then begin
			s = (*pmatrix).pure[*,j] / cal_ratio

			scale = (*pmatrix).yield[j] * conc[k] * charge * rG[j]

			if finite(scale) and ((select_overlay[0] eq -1) or (select_overlay[j] eq 1))then begin
				spec = null_spectrum
				spec.source = (*pspec).source
				spec.label = 'DA fit to ' + (*pmatrix).el[j]
				spec.cal.poly[0] = cal.poly[0]
				spec.cal.poly[1] = cal.poly[1]
				spec.cal.units = cal.units
				spec.cal.order = 1
				spec.comment = 'DA pure component'
				spec.charge = charge
				spec.multiplicity = multiplicity

				spec.size = n_elements(s)
				spec.data = ptr_new(s*scale[0])

				(*pspec).fit[(*pspec).n_fit] = ptr_new(spec, /no_copy)
				(*pspec).n_fit = (*pspec).n_fit+1
				if (*pspec).n_fit eq max_fit then return
				print,'overlay pure: el=',(*pmatrix).el[j],' added; scale=',scale[0]
			endif else begin
				print,'overlay pure: el=',(*pmatrix).el[j],' not added; scale infinite'
			endelse
		endif
	endif
endfor

return

bad_state:
	warning,'fit_overlay_pure','bad state'
	return
bad_ptr:
	warning,'fit_overlay_pure','bad ptr'
	return
bad_matrix:
	warning,'fit_overlay_pure','need to "Generate DA Matrix" first'
	return
end