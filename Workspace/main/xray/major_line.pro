	FUNCTION major_line, zi, shelli

;	returns the index for the major line of the requested
;	shell for use with E_LINE etc.

@line_split_definitions.def

	z = zi
	shell = shelli

	nz = n_elements(z)
	nshell = n_elements(shell)
	nt = nz

	if (nz lt 1) or (nshell lt 1) then return, 0.0
	if nz eq 1 then begin
		nt = nshell
		z = replicate(Z,nt)
	endif else if nshell eq 1 then begin
		nt = nz
		shell = replicate(shell,nt)
	endif else if nz ne nshell then begin
		print,'major_line: Z,Shell can only both be vectors if they have same length.'
		major_line = intarr(nt)
		goto, done
	endif
	major_line = intarr(nt)

	q = where( shell eq 1)
	if q[0] ne -1 then begin
		q2 = where( z[q] le Ka_s)
		if q2[0] ne -1 then major_line[q[q2]] = Ka_lo

		q2 = where( z[q] gt Ka_s)
		if q2[0] ne -1 then major_line[q[q2]] = Ka_hi
	endif

	q = where( shell eq 2)
	if q[0] ne -1 then begin
		q2 = where( z[q] le La_s)
		if q2[0] ne -1 then major_line[q[q2]] = La_lo

		q2 = where( z[q] gt La_s)
		if q2[0] ne -1 then major_line[q[q2]] = La_hi
	endif

	q = where( shell eq 3)
	if q[0] ne -1 then major_line[q] = Ma_Lo

done:
	major_line = reform(major_line)
	if n_elements(major_line) eq 1 then major_line = major_line[0]
	return, major_line
	END

