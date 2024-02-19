pro calc_charge_sharing, rays,nx,ny, pads,gaps, T,E,Mo, mask=mask, level=level, $
		good_signal=good_signal, gap_signal=gap_signal, through=through

; Calculate the signal and gap signal for these rays and detector
; structures.
; 	good		good detector signal
; 	gap			gap signal
; 	through		transmission intensity for each ray on output
; 	
; Input data:
;	rays		rays struct array
;	nx,ny		dimensions of rays
;	pads		struct array of pad boxes
;	gaps		struct array of gap boxes
;	mask		struct array of mask boxes (limited to 6 layers)
; 	level		select a specific 'level' (0,1,2,3) or -1 for ALL levels
;	T			thickness of wafer
;	E			energy of beam
;	Mo			material of mask
; 	through		intensity of each ray on input

if n_elements(through) lt 1 then begin
	through = fltarr(nx,ny)
	through[*] = 1.0
endif
if n_elements(level) lt 1 then level=-1

nxy = long(nx)*long(ny)
n_hit = 4
pad_length = fltarr(nxy,n_hit)
pad_order = lonarr(nxy,n_hit)
pad_i = bytarr(nxy,n_hit)
pad_j = bytarr(nxy,n_hit)
pad_offset = bytarr(nxy)
gap_length = fltarr(nxy,n_hit)
gap_order = lonarr(nxy,n_hit)
gap_offset = bytarr(nxy)
gap_i = bytarr(nxy,n_hit)
gap_j = bytarr(nxy,n_hit)

n_hit2 = 6							; increase for mask layers > 6
mask_length = fltarr(nxy,n_hit2)
mask_order = lonarr(nxy,n_hit2)
mask_offset = bytarr(nxy)
mask_i = bytarr(nxy,n_hit2)
mask_j = bytarr(nxy,n_hit2)

if n_elements(mask) gt 1 then begin
	print,'Ray intersections with mask ...'
	for i=0L,n_elements(mask)-1 do begin
		q1 = where( (rays.x gt mask[i].x[0]-2.*T) and (rays.x lt mask[i].x[1]+2.*T) and $
		 			(rays.y gt mask[i].y[0]-2.*T) and (rays.y lt mask[i].y[1]+2.*T), nq1)
		if nq1 ge 1 then begin
			l = intersection( mask[i], rays[q1], hit=hit, entry=enter)	
			qh = where( hit eq 1, nqh)	
			if nqh ge 1 then begin
				mask_length[q1[qh],mask_offset[q1[qh]]] = l[qh]
				mask_order[q1[qh],mask_offset[q1[qh]]] = mask[i].order
				mask_i[q1[qh],mask_offset[q1[qh]]] = mask[i].IDx
				mask_j[q1[qh],mask_offset[q1[qh]]] = mask[i].IDy
				mask_offset[q1[qh]] = (mask_offset[q1[qh]] + 1) < (n_hit2-1)
			endif
		endif
	endfor
endif

print,'Ray intersections with pads ...'
nq = n_elements(pads)
q = indgen(nq)
if level ge 0 then q=where(pads.level eq level, nq)
if nq lt 1 then goto, bad_level
for i=0L,nq-1 do begin
	q1 = where( (rays.x gt pads[q[i]].x[0]-2.*T) and (rays.x lt pads[q[i]].x[1]+2.*T) and $
	 			(rays.y gt pads[q[i]].y[0]-2.*T) and (rays.y lt pads[q[i]].y[1]+2.*T), nq1)
	if nq1 ge 1 then begin
		l = intersection( pads[q[i]], rays[q1], hit=hit, entry=enter)	
		qh = where( hit eq 1, nqh)	
		if nqh ge 1 then begin
			pad_length[q1[qh],pad_offset[q1[qh]]] = l[qh]
			pad_order[q1[qh],pad_offset[q1[qh]]] = pads[q[i]].order
			pad_i[q1[qh],pad_offset[q1[qh]]] = pads[q[i]].IDx
			pad_j[q1[qh],pad_offset[q1[qh]]] = pads[q[i]].IDy
			pad_offset[q1[qh]] = (pad_offset[q1[qh]] + 1) < (n_hit-1)
		endif
	endif
endfor

print,'Ray intersections with gaps ...'
nq = n_elements(gaps)
q = indgen(nq)
if level ge 0 then q=where(gaps.level eq level, nq)
if nq lt 1 then goto, bad_level
for i=0L,nq-1 do begin
	q1 = where( (rays.x gt gaps[q[i]].x[0]-2.*T) and (rays.x lt gaps[q[i]].x[1]+2.*T) and $
	 			(rays.y gt gaps[q[i]].y[0]-2.*T) and (rays.y lt gaps[q[i]].y[1]+2.*T), nq1)
	if nq1 ge 1 then begin
		l = intersection( gaps[q[i]], rays[q1], hit=hit, entry=enter)
		qh = where( hit eq 1, nqh)	
		if nqh ge 1 then begin
			gap_length[q1[qh],gap_offset[q1[qh]]] = l[qh]
			gap_order[q1[qh],gap_offset[q1[qh]]] = gaps[q[i]].order
			gap_i[q1[qh],gap_offset[q1[qh]]] = gaps[q[i]].IDx
			gap_j[q1[qh],gap_offset[q1[qh]]] = gaps[q[i]].IDy
			gap_offset[q1[qh]] = (gap_offset[q1[qh]] + 1) < (n_hit-1)
		endif
	endif
endfor

; Build up arrays of the length of each part of the ray traversing:
; 	mask
; 	previous pad attenuation
; 	gap between pads
; 	pad signal absorption length
; 	gap after pads
; 	
;	pad1	first pad hit by a ray, only gap1 comes before this
;	gap1	comes before pad1
;	gap3	comes after pad1, when no further pad follows
;	
;	pad2	second pad hit by a ray
;	gap2	total gap before pad2 (may include gap1)

; want to collect all mask, for each ray, which are always on top
; fot 'through', collect total path through pad and gap for each ray ...

mask_trans = mask_length[*,0]
pad_trans = pad_length[*,0]
gap_trans = gap_length[*,0]
for i=1L,n_hit2-1 do mask_trans = mask_trans + mask_length[*,i]
for i=1L,n_hit-1 do pad_trans = pad_trans + pad_length[*,i]
for i=1L,n_hit-1 do gap_trans = gap_trans + gap_length[*,i]

print,'Build transit lengths ...'
order1 = pad_order[*,0]						; first pad hit by each ray, else zero
pad1_trans = fltarr(nxy)
pad1_i = intarr(nxy)
pad1_j = intarr(nxy)
q = where( order1 ge 1, nq)
if nq gt 0 then begin
	pad1_trans[q] = pad1_trans[q] + pad_length[q,0]
	pad1_i[q] = pad_i[q,0]
	pad1_j[q] = pad_j[q,0]
endif

order1g = gap_order[*,0]					; first gap hit by each ray, else zero
gap1_trans = fltarr(nxy)
q = where( order1g ge 1, nq)
if nq gt 0 then begin
	gap1_trans[q] = gap1_trans[q] + gap_length[q,0]
endif

; Pad 1 atten
; want to collect all gaps with lower order, for each ray
; only gaps with order less than pad can effect pad

pad1_atten = fltarr(nxy)
for i=0L,n_hit-1 do begin
	q = where( gap_order[*,i] lt order1 and order1 ge 1, nq)
	if nq gt 0 then pad1_atten[q] = pad1_atten[q] + gap_length[q,i]
endfor

; Gap 1 atten
; want to collect all pads with lower or equal order, for each ray
; only pads with order same or less than gap can effect gap

gap1_atten = fltarr(nxy)
for i=0L,n_hit-1 do begin
	q = where( pad_order[*,i] le order1g and order1g ge 1, nq)
	if nq gt 0 then gap1_atten[q] = gap1_atten[q] + pad_length[q,i]
endfor

; find next order pad, for each ray

order2 = lonarr(nxy)
for i=1L,n_hit-1 do begin
	q = where( pad_order[*,i] gt order1 and order1 ge 1, nq)
	if nq gt 0 then order2[q] = order2[q] > pad_order[q,i]
endfor

; find next order gap, for each ray

order2g = lonarr(nxy)
for i=1L,n_hit-1 do begin
	q = where( gap_order[*,i] gt order1g and order1g ge 1, nq)
	if nq gt 0 then order2g[q] = order2g[q] > gap_order[q,i]
endfor

; Pad 2 signal, atten
; want to collect all gaps and pads with lower order, for each ray
; only gaps and pads with order less than pad can effect pad

pad2_trans = fltarr(nxy)
for i=1L,n_hit-1 do begin
	q = where( pad_order[*,i] ge order2 and order2 ge 1, nq)
	if nq gt 0 then pad2_trans[q] = pad2_trans[q] + pad_length[q,i]
endfor

pad2_atten = fltarr(nxy)
for i=0L,n_hit-1 do begin
	q = where( pad_order[*,i] lt order2 and order2 ge 1, nq)
	if nq gt 0 then pad2_atten[q] = pad2_atten[q] + pad_length[q,i]
endfor

for i=0L,n_hit-1 do begin
	q = where( gap_order[*,i] lt order2 and order2 ge 1, nq)
	if nq gt 0 then pad2_atten[q] = pad2_atten[q] + gap_length[q,i]
endfor

; Gap 2 signal, atten
; want to collect all pads with lower or equal order, for each ray
; only pads with order same or less than gap can effect gap

gap2_trans = fltarr(nxy)
for i=1L,n_hit-1 do begin
	q = where( gap_order[*,i] ge order2g and order2g ge 1, nq)
	if nq gt 0 then gap2_trans[q] = gap2_trans[q] + gap_length[q,i]
endfor

gap2_atten = fltarr(nxy)
for i=0L,n_hit-1 do begin
	q = where( pad_order[*,i] le order2g and order2g ge 1, nq)
	if nq gt 0 then gap2_atten[q] = gap2_atten[q] + pad_length[q,i]
endfor

for i=0L,n_hit-1 do begin
	q = where( gap_order[*,i] lt order2g and order2g ge 1, nq)
	if nq gt 0 then gap2_atten[q] = gap2_atten[q] + gap_length[q,i]
endfor

; 	Then can apply these tables to different energy X-rays
; 	to compute efficiency, peak-to-background and effective solid-angle, etc.

print,'Build efficiencies ...'
si_density = density(atomic_number('Si'))
si_material = make_layer('Si',1.0,name='silicon')
; for thick use: si_material.thick = 100. * t(mm) * density

mask_density = density(atomic_number(Mo))
mask_material = make_layer(Mo,1.0,name='mask')
; for thick use: mask_material.thick =100. *  t(mm) * density

f_si = 100.*si_density
f_mo = 100.*mask_density
a_si = f_si * atten(si_material, E)
a_mo = f_mo * atten(mask_material, E)

; pad1: trans thru: mask + gap1; absorption in pad1
; pad2: trans thru: mask + pad1 + gap2; absorption in pad2

signal1 = fltarr(nxy)
signal2 = fltarr(nxy)
gap1 = fltarr(nxy)
gap2 = fltarr(nxy)
through = reform(through, nxy, /overwrite)

signal1 = through * exp(-a_mo*mask_trans -a_si*pad1_atten) * (1.-exp(-a_si*pad1_trans))
gap1 = through * exp(-a_mo*mask_trans -a_si*gap1_atten) * (1.-exp(-a_si*gap1_trans))

signal2 = through * exp(-a_mo*mask_trans -a_si*pad2_atten) * (1.-exp(-a_si*pad2_trans))
gap2 = through * exp(-a_mo*mask_trans -a_si*gap2_atten) * (1.-exp(-a_si*gap2_trans))

through = through * exp(-a_mo*mask_trans -a_si*gap_trans -a_si*pad_trans)

print,'Reform ...'
signal1 = reform(signal1, nx,ny, /overwrite)
signal2 = reform(signal2, nx,ny, /overwrite)
gap1 = reform(gap1, nx,ny, /overwrite)
gap2 = reform(gap2, nx,ny, /overwrite)
through = reform(through, nx,ny, /overwrite)

;good_signal = signal1
good_signal = signal1 + signal2
gap_signal = gap1 + gap2
return

bad_level:
	warning,'calc_charge_sharing','illegal level search'
	return
end
