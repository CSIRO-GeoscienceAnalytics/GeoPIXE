function xrf_calc_int, energy=e, jump=jump, verbose=verbose

;	Use Elam XRF database for relative intensities,
;	and Ebel absorption parameterisations.
;	(These are read in by 'init_xrf_lines')
;
;	W.T. Elam et al., Radiation Physics and Chemistry 63 (2002) 121-128.
;	H. Ebel, et al., X-Ray Spectrom. 32 (2003) 442-451
;
;	energy		beam energy (scalar only)
;	/jump		use jump ratios, else use Ebel sub-shell cross-sections
;	/verbose	print messages

COMPILE_OPT STRICTARR

common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK
common c_xray, energy, relint, xray_OK
common c_ebel, ebel, xrf_ebel_OK
common c_elam, elam

if n_elements(jump) lt 1 then jump=0
if n_elements(verbose) lt 1 then verbose=0
if n_elements(e) lt 1 then e=30.0
if verbose then print,'xrf_calc_int:  calculate line intensities for E =',e,' keV'

f = elam
cK = fltarr(100)
cL = fltarr(6,100)
cM = fltarr(6,100)
cascade_K = 1
cascade_L = 1

;-------------------------------------------------------------------------------------

for z=6,92 do begin

; K lines (these are not used in Init_XRF_lines.pro)

	cK[z] = photo_subshell(z,1,e)						; cross section for K shell

	q = [1,2,3,4,5,6,7,8,9,10,11,12]
	f[z].intense[q] = relative_intensity(z,q)			; use PIXE database

	if f[z].intense[9] gt 1.0e-6 then begin
		f[z].id[9] = 'Ka_'
		f[z].e[9] = (f[z].e[1]*f[z].intense[1] + f[z].e[2]*f[z].intense[2] + f[z].e[3]*f[z].intense[3])/ f[z].intense[9]
	endif
	if f[z].intense[10] gt 1.0e-6 then begin
		f[z].id[10] = 'Kb_'
		f[z].e[10] = (f[z].e[4]*f[z].intense[4] + f[z].e[5]*f[z].intense[5] + f[z].e[6]*f[z].intense[6] + f[z].e[7]*f[z].intense[7] + f[z].e[8]*f[z].intense[8])/ f[z].intense[10]
	endif
endfor

;-------------------------------------------------------------------------------------

for z=20,94 do begin

; L lines

;if z eq 42 then begin
;	print,'test 42'
;endif

if jump then begin										; subshell cross-sections based
	abs = photo_cross_section(z,e)						; on total abs and jump-ratios
	abs = abs[0]
	r3 = ebel[z].jump[4]
	r2 = ebel[z].jump[3]
	r1 = ebel[z].jump[2]
	if e gt ebel[z].edge[2] then begin
		c3 = abs * (r3 - 1.0) / (r1*r2*r3)
		c2 = abs * (r2 - 1.0) / (r1*r2)
		c1 = abs * (r1 - 1.0) / (r1)
	endif else if e gt ebel[z].edge[3] then begin
		c3 = abs * (r3 - 1.0) / (r2*r3)
		c2 = abs * (r2 - 1.0) / (r2)
		c1 = 0.0
	endif else if e gt ebel[z].edge[4] then begin
		c3 = abs * (r3 - 1.0) / (r3)
		c2 = 0.0
		c1 = 0.0
	endif else begin
		c3 = 0.0
		c2 = 0.0
		c1 = 0.0
	endelse
endif else begin										; Ebel subshell cross-sections
	c1 = photo_subshell(z,2,e)
	c2 = photo_subshell(z,3,e)
	c3 = photo_subshell(z,4,e)
endelse
c30 = c3												; uncorrected c3

w1 = f[z].fluor[2]										; fluor yields
w2 = f[z].fluor[3]
w3 = f[z].fluor[4]

f12 = f[z].ck.l.f12										; Coster-Kronig terms
F13 = f[z].ck.l.F13
f23 = f[z].ck.l.f23

if cascade_K then begin
	dc1 = cK[z] * f[z].branch[3]		; ka3  K-L1		; cascade from K shell
	dc2 = cK[z] * f[z].branch[2]		; ka2  K-L2 
	dc3 = cK[z] * f[z].branch[1]		; ka1  K-L3
	if (dc1 gt 1.0e-30) or (dc2 gt 1.0e-30) or (dc3 gt 1.0e-30) and verbose then begin
		print,z,' ',element_name(z),' K-->L rel change=', dc1/c1, dc2/c2, dc3/c3, dc1/(c1+c2+c3), dc2/(c1+c2+c3), dc3/(c1+c2+c3)
	endif
	c1 = c1 + dc1
	c2 = c2 + dc2 
	c3 = c3 + dc3
endif

;c1L = (c1 - c1*f12 - c1*F13)							; may make sense, but not by convention ????
;c2L = (c2 + c1*f12 - c2*f23)
;c3L = (c3 + c2*f23 + c1*F13)

c1L = c1												; Coster-Kronig vacancy movement
c2L = (c2 + c1*f12)
c3L = (c3 + c2*f23 + c1*F13)
cL[*,z] = [0.0, c1L, c2L, c3L, 0.0, 0.0]				; subshell cross-sections after CK

c1x = c1L * w1
c2x = c2L * w2
c3x = c3L * w3
cx = [0.0, c1x, c2x, c3x, 0.0, 0.0]						; X-ray production cross-sections

; Normalize to c30 so that these relative intensities can be combined with the
; L3 cross-sections only from "calc_cross".
; See s/w log 14, p. 108
; Note that relative intensities do not add to 1 for L3 lines due to CK from L1,L2 and cascade from K.

q = [13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,51,52]

if (c30 * w3) gt 1.0e-35 then begin
	ck_ratio = cx[f[z].subshell[q]] / (c30 * w3)
endif else ck_ratio=1.
f[z].intense[q] = f[z].branch[q] * ck_ratio

;if z eq 42 then begin
;	print, 'Energy = ',e,' Z=42 ...'
;	print, f[z].branch[q]
;	print, ck_ratio
;	print, f[z].intense[q]
;endif

if (f[z].intense[25] gt 1.0e-6*f[z].intense[23]) and (energy[z,25] lt 0.001) then begin
	energy[z,25] = f[z].e[25]/1000.
	relint[z,25] = 0.001
	relint[z,24] = relint[z,24]-0.001
	if verbose then print,'z=',z,'  ',element_name(z),'  no PIXE Lg3 line present, share intensity with Lg2'
endif
if (f[z].intense[26] eq 0.0) and (f[z].intense[24] gt 1.0e-6*f[z].intense[15]) then begin
	g2 = relative_intensity(z,24)
	g4 = relative_intensity(z,26)
	if (g2 gt 1.0e-6) and (g4 gt 1.0e-6) then begin
		f[z].id[26] = 'Lg4'
		if verbose then print,'z=',z,'  ',element_name(z),'  Lg4 from PIXE relative to Lg2'
		f[z].intense[26]=f[z].intense[24]*g4/g2
	endif
endif
if (f[z].intense[27] eq 0.0) and (f[z].intense[23] gt 1.0e-6*f[z].intense[15]) then begin
	g1 = relative_intensity(z,23)
	g5 = relative_intensity(z,27)
	if (g1 gt 1.0e-6) and (g5 gt 1.0e-6) then begin
		f[z].id[27] = 'Lg5'
		if verbose then print,'z=',z,'  ',element_name(z),'  Lg5 from PIXE relative to Lg1'
		f[z].intense[27]=f[z].intense[23]*g5/g1
	endif
endif
if (f[z].intense[21] eq 0.0) and (f[z].intense[18] gt 1.0e-6*f[z].intense[15]) then begin
	b2 = relative_intensity(z,18)
	b5 = relative_intensity(z,21)
	if (b2 gt 1.0e-6) and (b5 gt 1.0e-6) then begin
		f[z].id[21] = 'Lb5'
		if verbose then print,'z=',z,'  ',element_name(z),'  Lb5 from PIXE relative to Lb2'
		f[z].intense[21]=f[z].intense[18]*b5/b2
	endif
endif
if (f[z].intense[28] eq 0.0) and (f[z].intense[23] gt 1.0e-6*f[z].intense[15]) then begin
	g1 = relative_intensity(z,23)
	g6 = relative_intensity(z,28)
	if (g1 gt 1.0e-6) and (g6 gt 1.0e-6) then begin
		f[z].id[28] = 'Lg6'
		if verbose then print,'z=',z,'  ',element_name(z),'  Lg6 from PIXE relative to Lg1'
		f[z].intense[28]=f[z].intense[23]*g6/g1
	endif
endif
f[z].intense[9] = f[z].intense[1] + f[z].intense[2] + f[z].intense[3]
f[z].intense[10] = f[z].intense[4] + f[z].intense[5] + f[z].intense[6] + f[z].intense[7] + f[z].intense[8]
f[z].intense[29] = f[z].intense[15] + f[z].intense[16]
f[z].intense[30] = f[z].intense[17] + f[z].intense[18] + f[z].intense[19] + f[z].intense[20] + f[z].intense[21] + f[z].intense[22]
f[z].intense[31] = f[z].intense[23] + f[z].intense[24] + f[z].intense[25] + f[z].intense[26] + f[z].intense[27] + f[z].intense[28]
if f[z].intense[29] gt 1.0e-6 then begin
	f[z].id[29] = 'La_'
	f[z].e[29] = (f[z].e[15]*f[z].intense[15] + f[z].e[16]*f[z].intense[16])/ f[z].intense[29]
endif
if f[z].intense[30] gt 1.0e-6 then begin
	f[z].id[30] = 'Lb_'
	f[z].e[30] = (f[z].e[17]*f[z].intense[17] + f[z].e[18]*f[z].intense[18] + f[z].e[19]*f[z].intense[19] + f[z].e[20]*f[z].intense[20] + f[z].e[21]*f[z].intense[21] + f[z].e[22]*f[z].intense[22])/ f[z].intense[30]
endif
if f[z].intense[31] gt 1.0e-6 then begin
	f[z].id[31] = 'Lg_'
	f[z].e[31] = (f[z].e[23]*f[z].intense[23] + f[z].e[24]*f[z].intense[24] + f[z].e[25]*f[z].intense[25] + f[z].e[26]*f[z].intense[26] + f[z].e[27]*f[z].intense[27] + f[z].e[28]*f[z].intense[28])/ f[z].intense[31]
endif

; Normalize shell intensites ...

endfor

;-------------------------------------------------------------------------------------

for z=73,92 do begin

; M lines

;	q = [32,33,34,35,36,37,48,49,50]
;	f[z].intense[q] = relative_intensity(z,q)		; use PIXE database


if jump then begin									; subshell cross-sections based
	abs = photo_cross_section(z,e)					; on total abs and jump-ratios
	abs = abs[0]
	r5 = ebel[z].jump[9]
	r4 = ebel[z].jump[8]
	r3 = ebel[z].jump[7]
	r2 = ebel[z].jump[6]
	r1 = ebel[z].jump[5]
	if e gt ebel[z].edge[5] then begin
		c5 = abs * (r5 - 1.0) / (r1*r2*r3*r4*r5)
		c4 = abs * (r4 - 1.0) / (r1*r2*r3*r4)
		c3 = abs * (r3 - 1.0) / (r1*r2*r3)
		c2 = abs * (r2 - 1.0) / (r1*r2)
		c1 = abs * (r1 - 1.0) / (r1)
	endif else if e gt ebel[z].edge[6] then begin
		c5 = abs * (r5 - 1.0) / (r2*r3*r4*r5)
		c4 = abs * (r4 - 1.0) / (r2*r3*r4)
		c3 = abs * (r3 - 1.0) / (r2*r3)
		c2 = abs * (r2 - 1.0) / (r2)
		c1 = 0.0
	endif else if e gt ebel[z].edge[7] then begin
		c5 = abs * (r5 - 1.0) / (r3*r4*r5)
		c4 = abs * (r4 - 1.0) / (r3*r4)
		c3 = abs * (r3 - 1.0) / (r3)
		c2 = 0.0
		c1 = 0.0
	endif else if e gt ebel[z].edge[8] then begin
		c5 = abs * (r5 - 1.0) / (r4*r5)
		c4 = abs * (r4 - 1.0) / (r4)
		c3 = 0.0
		c2 = 0.0
		c1 = 0.0
	endif else if e gt ebel[z].edge[9] then begin
		c5 = abs * (r5 - 1.0) / (r5)
		c4 = 0.0
		c3 = 0.0
		c2 = 0.0
		c1 = 0.0
	endif else begin
		c5 = 0.0
		c4 = 0.0
		c3 = 0.0
		c2 = 0.0
		c1 = 0.0
	endelse
endif else begin								; Ebel subshell cross-sections
	c1 = photo_subshell(z,5,e)
	c2 = photo_subshell(z,6,e)
	c3 = photo_subshell(z,7,e)
	c4 = photo_subshell(z,8,e)
	c5 = photo_subshell(z,9,e)
endelse
c50 = c5										; uncorrected c5

w1 = f[z].fluor[5]
w2 = f[z].fluor[6]
w3 = f[z].fluor[7]
w4 = f[z].fluor[8]
w5 = f[z].fluor[9]

f12 = f[z].ck.m.f12								; Coster Kronig terms
F13 = f[z].ck.m.F13
F14 = f[z].ck.m.F14
F15 = f[z].ck.m.F15
f23 = f[z].ck.m.f23
F24 = f[z].ck.m.F24
F25 = f[z].ck.m.F25
f34 = f[z].ck.m.f34
F35 = f[z].ck.m.F35
f45 = f[z].ck.m.f45

;	Cascade approach appears to assume that ratio of cross-sections in K/L, L/M are approx.
;	independent of energy. In reality, over a larger range (e.g. 1-50 keV), these vary by ~2x.

if cascade_K then begin
	dc1 = 0.0
	dc2 = cK[z] * f[z].branch[6]				; kb3   K-M2		; cascade from K shell
	dc3 = cK[z] * f[z].branch[4]				; kb1   K-M3
	dc4 = cK[z] * f[z].branch[8]/2				; kb5   K-M4,5
	dc5 = cK[z] * f[z].branch[8]/2				; kb5   K-M4,5
	if (dc1 gt 1.0e-30) or (dc2 gt 1.0e-30) or (dc3 gt 1.0e-30) or (dc4 gt 1.0e-30) or (dc5 gt 1.0e-30) and verbose then begin
		print,z,' ',element_name(z),' K-->M rel change=', dc1/c1, dc2/c2, dc3/c3, dc4/c4, dc5/c5, dc1/(c1+c2+c3+c4+c5), dc2/(c1+c2+c3+c4+c5), dc3/(c1+c2+c3+c4+c5), dc4/(c1+c2+c3+c4+c5), dc5/(c1+c2+c3+c4+c5)
	endif
endif else begin
	dc1 = 0.0
	dc2 = 0.0
	dc3 = 0.0
	dc4 = 0.0
	dc5 = 0.0
endelse
if cascade_L then begin
	ddc1 = cL[2,z] * f[z].branch[14]			; Leta   L2-M1		; cascade from L shell
	ddc1 = ddc1 + cL[3,z] * f[z].branch[13]		; Ll     L3-M1
	ddc2 = cL[1,z] * f[z].branch[20]			; Lb4    L1-M2
	ddc2 = ddc2 + cL[3,z] * f[z].branch[51]		; Lt     L3-M2
	ddc3 = cL[1,z] * f[z].branch[19]			; Lb3    L1-M3
	ddc4 = cL[2,z] * f[z].branch[17]			; Lb1    L2-M4
	ddc4 = ddc4 + cL[3,z] * f[z].branch[16]		; La2    L3-M4
	ddc4 = ddc4 + cL[1,z] * f[z].branch[52]/2	; Lb9,10 L1-M4,5
	ddc5 = cL[1,z] * f[z].branch[52]/2			; Lb9,10 L1-M4,5
	ddc5 = ddc5 + cL[3,z] * f[z].branch[15]		; La1    L3-M5

	dc1 = dc1 + ddc1
	dc2 = dc2 + ddc2
	dc3 = dc3 + ddc3
	dc4 = dc4 + ddc4
	dc5 = dc5 + ddc5
	if (ddc1 gt 1.0e-30) or (ddc2 gt 1.0e-30) or (ddc3 gt 1.0e-30) or (ddc4 gt 1.0e-30) or (ddc5 gt 1.0e-30) and verbose then begin
		print,z,' ',element_name(z),' L-->M rel change=', ddc1/c1, ddc2/c2, ddc3/c3, ddc4/c4, ddc5/c5, ddc1/(c1+c2+c3+c4+c5), ddc2/(c1+c2+c3+c4+c5), ddc3/(c1+c2+c3+c4+c5), ddc4/(c1+c2+c3+c4+c5), ddc5/(c1+c2+c3+c4+c5)
	endif
endif
c1 = c1 + dc1
c2 = c2 + dc2 
c3 = c3 + dc3
c4 = c4 + dc4
c5 = c5 + dc5 

c1M = c1
c2M = c1*f12 + c2
c3M = c1*F13 + c2*f23 + c3
c4M = c1*F14 + c2*F24 + c3*f34 + c4
c5M = c1*F15 + c2*F25 + c3*F35 + c4*f45 + c5
cM[*,z] = [0.0, c1M, c2M, c3M, c4M, c5M]		; subshell cross-sections after CK

c1x = c1M * w1
c2x = c2M * w2
c3x = c3M * w3
c4x = c4M * w4
c5x = c5M * w5

cx = [0.0, c1x, c2x, c3x, c4x, c5x]

; Normalize to c50 so that these relative intensities can be combined with the
; M5 cross-sections only from calc_cross.
; See s/w log 14, p. 108
; Note that relative intensities do not add to 1 due to CK and cascade from K,L.

q = [32,33,34,35,36,37,48,49,50]

if (c50 * w5) gt 1.0e-35 then begin
	ck_ratio = cx[f[z].subshell[q]] / (c50 * w5) 
endif else ck_ratio = 1.
f[z].intense[q] = f[z].branch[q] * ck_ratio

endfor

;-------------------------------------------------------------------------------------

f[*].e[*] = f[*].e[*] / 1000.
return, f

bad:
	warning, /error, 'xrf_calc_int','bad input structs.'
	return, 0
end
