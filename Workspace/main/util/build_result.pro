function build_result, xi, ei, mdli, high=high, low=low, twice_mdl=twice_mdl, $
					percent=percenti, export=exporti, veto=veto, plain=plain

; Make result strings for x +/- e and mdl.
;
; /percent	treat values as ppm, and change these to wt% when large enough.
; /veto		force it below MDL as "<mdl"
; /plain	don't use fancy "%" and "<" forms (e.g. for export as numbers only).
; /export	don't use blanks for tiny results on export.

if n_elements(low) lt 1 then low = -1.0E+10
if n_elements(high) lt 1 then high = 1.0e+10
if n_elements(percenti) lt 1 then percenti = 0
if n_elements(exporti) lt 1 then exporti = 0
if n_elements(veto) lt 1 then veto = 0
if n_elements(twice_mdl) lt 1 then twice_mdl = 0
if n_elements(ei) lt 1 then ei = 0.001*xi
if n_elements(mdli) lt 1 then mdli = 0.0
if n_elements(plain) eq 0 then plain=0

x = xi
e = ei
mdl = (twice_mdl eq 1) ? 2.0*mdli : mdli
percent = percenti
export = exporti
if plain then begin
	percent = 0
	mdl = 0.0
	export = 1
endif

if (x lt low) or (x gt high) then return, ''
if (x gt 1.0E+7) then begin
	fs ='(G10.3)'
	s = string(x, format=fs)
	return, s
endif 	;else if (abs(x) lt 0.00000005) then begin
;	return, '0.0'
;endif

use_percent = 0
if x lt mdl then begin
	if percent and (mdl gt 999.9) then begin
		use_percent = 1
		x = x/10000.
		e = e/10000.
		mdl = mdl/10000.
	endif
endif else begin
	if percent and (x gt 999.9) then begin
		use_percent = 1
		x = x/10000.
		e = e/10000.
		mdl = mdl/10000.
	endif
endelse

dx = [1000000., 100000., 10000., 1000., 100., 10., 1.]
de = [1.0, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001]

q = where(de gt e)
nne = n_elements(q)
if q[0] eq -1 then nne=0

q = where(abs(x) gt dx)
nx = n_elements(q)
if q[0] eq -1 then nx=0

q = where(mdl gt dx)
nm = n_elements(q)
if q[0] eq -1 then nm=0
;print,'nx,nm=',nx,nm,'  min([nx,nm])=', min([nx,nm])

nx = max([nx,nm])
m = nx + nne + 3
if (nx gt 1) and (export eq 0) then nne = nne < ((3-nx) > 0)
if use_percent then begin
	fs ='(F'+string(m)+'.'+string(nne)+',"%")'
endif else begin
	fs ='(F'+string(m)+'.'+string(nne)+')'
endelse
fs = strcompress(fs, /remove_all)
;print,'fs=',fs

if export then begin
	if (x gt mdl) and (veto eq 0) then begin
		s = string(x, format=fs)
	endif else begin
		if mdl gt 0.0 then begin
			s = string(max([mdl,e]), format=fs)
			s = '< ' + s
		endif else begin
			s = string(x, format=fs)
		endelse
	endelse
endif else begin
	if (x gt mdl) and (veto eq 0) then begin				; if (x gt 0.5*mdl) then begin
		if (x ge 10.^(-nne)) then begin
			s = string(x, format=fs)
		endif else begin
			s = '0.0'
		endelse
	endif else if (x lt 0.01*mdl) or (x lt 1.0e-6) then begin
		s = ' '
	endif else begin
		if mdl gt 0.0 then begin
			s = string(max([mdl,e]), format=fs)
			s = '< ' + s
		endif else begin
			s = string(x, format=fs)
		endelse
	endelse
endelse

s = strtrim(strcompress(s, /remove_all),2)
return, s
end