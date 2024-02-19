function ion_xsect_m_pajek, z, E, line_group, zeta=zeta

; Return M shell cross-section for protons energy 'E' MeV on
; target element 'z'.
;
; Return for line group 'line_group':
;	0	Ma,b	subshell=	5, 4		(default)
;	1	Mg					3
;	2	M3-O4,5				3, (2)

COMPILE_OPT STRICTARR
if n_params(0) lt 3 then line_group=0
line_group = clip( line_group, 0, 2)

a = [	[5.359,	3.143,	-1.274,	1.101,	-1.052,	0.329], $
		[1.811,	4.64,	-1.564,	0.0,	0.0,	0.0], $
		[0.624,	4.942,	-0.364,	-0.401,	-1.572,	0.843]]

nz = n_elements(z)
nen = n_elements(E)
if (nz lt 1) or (nen lt 1) then return, 0.0
if (nz gt 1) then begin
	print,'Ion_xsect_m_pajek: Z can not be a vector.'
endif

edge_index = [9,7,7]
shield = [21.15, 11.25, 11.25]

B = edge( z, edge_index[line_group])
Ze = float(z) - shield[line_group]

vv = sqrt( E * 548.597 / (B * 1007.27663))

theta = 661.8 * B / (Ze*Ze)

zeta = 2.0 * vv / sqrt(theta)
lzm = alog( zeta)
lzk = replicate(1.0,nen)
lsmx = fltarr(nen)

for i=0L,5 do begin
	lsmx = lsmx + a[i,line_group] * lzk
	lzk = lzk * lzm
endfor
smx = exp( lsmx)

return, smx
end
