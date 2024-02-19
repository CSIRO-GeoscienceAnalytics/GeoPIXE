function get_poly_config, file, header=header, error=err

; Read a .csv (comma separated) file containing Polycapillary details:
;
;	Model name
;	Gain, Energy (for gain), relative to Pinhole (mm), pinhole distance (mm)
;	Beam diameter at exit (mm), focus spot (mm), focal distance (mm)
;	Followed by E, Trans, one pair per line.
;
;	/header		only read the model name/title
;
;	Ignore comment lines starting with #
;	A blank file '' means the "XOS default", as in define(/source) already.

	COMPILE_OPT STRICTARR
	if n_elements(header) lt 1 then header=0

	source = define(/source)
	poly = source.poly
	err = 1
	if file eq '' then begin						; "XOS default"
		e = 0.33 * findgen(300)						; must be 300 elements
		trans = xos_transmission(e) / xos_transmission(17.4)
		poly.etrans = e								; table energies (keV)
		poly.trans = trans							; transmissions
		goto, done
	endif

	on_ioerror, bad_file
	openr, unit, file, /get_lun
	
	energy = fltarr(4096)
	energy[*] = -1.
	trans = fltarr(4096)

	str = ''
	repeat begin
		readf, unit, str
	endrep until extract(str,0,0) ne '#'

	poly.model = strtrim(str,2)
	if header then goto, done

	repeat begin
		readf, unit, str
	endrep until extract(str,0,0) ne '#'

	s = strsplit( str, ' 	,', /extract)
	ns = n_elements(s)
	if ns lt 4 then goto, bad_format
	poly.gain = float2(s[0])
	poly.energy = float2(s[1])
	poly.pinhole = float2(s[2])
	poly.distance = float2(s[3])

	repeat begin
		readf, unit, str
	endrep until extract(str,0,0) ne '#'

	s = strsplit( str, ' 	,', /extract)
	ns = n_elements(s)
	if ns lt 3 then goto, bad_format
	poly.diameter = float2(s[0])
	poly.spot = float2(s[1])
	poly.focus = float2(s[2])
	
	i = 0
	while( EOF(unit) ne 1) do begin
		readf, unit, str
		if extract(str,0,0) ne '#' then begin
			s = strsplit( str, ' 	,', /extract)
			ns = n_elements(s)
			if ns lt 2 then goto, bad_format
			energy[i] = float(s[0])
			trans[i] = float(s[1])
			i = i+1
		endif
	endwhile
	close_file, unit
	
	if i gt 1 then begin
		q = where( energy[0:i-1] ge 0., nq)
		if nq gt 0 then begin
			energy = energy[q]
			trans = trans[q]
		endif
	endif else nq=300

	n = min([nq,300])
	poly.etrans[*] = 0.
	poly.trans[*] = 0.
	poly.etrans[0:n-1] = energy[0:n-1]

;	Normalize to 1.0 at poly calibration 'energy'

	scale = interpol( trans[0:n-1], energy[0:n-1], poly.energy)
	trans = trans / scale
	poly.trans[0:n-1] = trans[0:n-1]

done:
	err = 0
	return, poly
	
bad_file:
	warning,'get_xanes_energies','Error reading file: '+file
	close_file, unit
	return, poly
bad_format:
	warning,'get_xanes_energies','Bad format in file: '+file
	close_file, unit
	return, poly
end
