function decode_hubbell, file

; Read the XCOM .txt file outputs, and build a structure of
;
; {		el: name							element name
;		Z: z								atomic number
;
;		e: energy, units:'MeV',				all vectors, except 'units'
;		n: #energies,						number of energy steps
;
;		Rayleigh: b/atom, Compton:b/at,		elastic, inelastic scattering
;		Photo: b/at,						photoelectric effect
;		Pair: b/at,							total pair production (nuclear + electric fields)
;
;		Atten_All: cm2/g, 					total mass attenuation
;		Atten: cm2/g,						mass attenuation without elastic scattering
;
;		ns: #edges							# of egdes found
;		edge_label,							all edge labels found
;		edge_ratio }						edge jump ratios (ratio of photo cross-section)

	data = 0

	on_ioerror, bad_file
	openr, lun, file, /get_lun

	el = ''
	readf,lun, el
	el = strtrim(el,2)

	s = ''
	readf,lun, s
	readf,lun, s
	readf,lun, s

	set_separators, ': ,	'
	chop_string, s, sub, n_sub
	if abs(float(sub[1])-1.0) gt 0.001 then goto, bad_comp
	z = fix(sub[0])
	if (z lt 1) or (z gt 92) then goto, bad_z

	for i=0L,9 do readf,lun,s
	set_separators, ' ,	'								; separators and white space for chop_string

	on_ioerror, bad_io
	n = 0
	ns = 0
	edge_label = ''
	edge_ratio = 0.0
	edge_e = 0.0
	edge_shell = 0
	edge_subshell = 0
	shells = ['','K','L','M','N','O','P']

	repeat begin
		readf,lun, s
		b = byte(s)
		if b[0] eq 12 then for i=0L,14 do readf,lun,s	; new page, skip header lines
		if lenchr(s) lt 2 then readf,lun, s				; skip blank lines

		off = 0
		s1 = strmid(s,0,6)
		if lenchr(s1) gt 0 then off=2					; found an edge label

		chop_string, s, sub, n_sub
		if n_sub lt 8 then goto, bad_line

		vals = float(sub[off:*])

		if off ne 0 then begin							; save edge label and ratio
			q = where( strmid(sub[1],0,1) eq shells)
			if ns eq 0 then begin
				if n gt 0 then begin
					edge_e = 1000.*vals[0]
					edge_label = sub[1]					; first edge label
					edge_shell = fix(q[0] > 0)
					edge_subshell = fix(strmid(sub[1],1,1)) > 1
					edge_ratio = vals[3] / photo[n-1]	; and jump ratio
					ns = 1
				endif
			endif else begin
				edge_e = [edge_e, 1000.*vals[0]]
				edge_label = [edge_label, sub[1]]
				edge_shell = [edge_shell, fix(q[0] > 0)]
				edge_subshell = [edge_subshell, fix(strmid(sub[1],1,1)) > 1]
				edge_ratio = [edge_ratio, vals[3] / photo[n-1]]
				ns = ns+1
			endelse
		endif

		if off ne 0 then vals[0] = vals[0]+2.0e-6		; delta Energy past an edge
		if n eq 0 then begin
			e = 1000.*vals[0]
			rayleigh = vals[1]
			compton = vals[2]
			photo = vals[3]
			pair = vals[4] + vals[5]
			atten_all = vals[6]
			atten2 = vals[7]
			n = 1
		endif else begin
			e = [e, 1000.*vals[0]]
			rayleigh = [rayleigh,vals[1]]
			compton = [compton,vals[2]]
			photo = [photo,vals[3]]
			pair = [pair,vals[4] + vals[5]]
			atten_all = [atten_all,vals[6]]
			atten2 = [atten2,vals[7]]
			n = n+1
		endelse
	endrep until EOF(lun)
	if n eq 0 then goto, bad_io

	data = { el:el, Z:z, e:e, n:n, units:'keV', Rayleigh:rayleigh, $
		Compton:compton, Photo:photo, Pair:pair, $
		Atten_All:atten_all, Atten:atten2, ns:ns, edge_e:edge_e, $
		edge_shell:edge_shell, edge_subshell:edge_subshell, edge_label:edge_label, edge_ratio:edge_ratio }
	goto, finish

bad_file:
	warning,'decode_hubbell','bad file open for '+file
	goto, finish
bad_comp:
	warning,'decode_hubbell','bad composition. Should be pure.'
	goto, finish
bad_z:
	warning,'decode_hubbell','bad atomic number'
	goto, finish
bad_line:
	warning,'decode_hubbell','bad read of data line'
	goto, finish
bad_io:
	warning,'decode_hubbell','error reading file '+file
	goto, finish

finish:
	close_file, lun
	return, data
end






