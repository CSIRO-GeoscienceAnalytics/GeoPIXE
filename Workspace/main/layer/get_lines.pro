function get_lines, e_min, e_max, branch_ratio=branch_ratio, $
			line_indx=line_indx, shell=shell, n_lines=n_lines, $
			e_bind=e_bind, fluoro=fluoro, jump=jump, z2=z2, $
			select=select_z, gamma=gamma, $
			beam=beam, e_beam=e_beam, photo=photo 

;	Get all X-ray lines and parameters, for all shells, for all line energies
;	in the range 'e_min' to 'e_max'. Treat K, L, M as separate elements.
;
;	If 'select=z' is used, then only calculate for these Z.
;	Use 'select=0' to use the defaults, or omit the 'select' keyword.
;
;	Lines are in order of descending relative intensity for each
;	element/shell combination. Hence, the major line is line index 0.
;
;	e_beam	mono or ion beam energy, if used, else pass continuum 'beam' (/photo only)

;	/photo	photon mode
;	beam	continuum beam struct, if used, only for /photo, else pass e_beam for mono
;	
;	/Gamma	return PIGE lines.
;			Branch_ratio and fluoro equal 1 for now.
;			All shell=0 for now. jump=0.

	COMPILE_OPT STRICTARR
	if n_elements(gamma) lt 1 then gamma=0
	if n_elements(photo) lt 1 then photo=0
	if n_elements(beam) lt 1 then begin					; beam spectrum flags
		continuum = 0
		if n_elements(e_beam) lt 1 then e_beam = 3.0
	endif else begin
		continuum = beam[0].continuum					; for a continuum source, e_beam is maximum energy
		e_beam = beam[0].energy							; only support continuum source for /photons
	endelse
	if photo eq 0 then continuum = 0

	klm = [0,1,4,9,16]
	nmax = 300
	linmax = 20
	z2 = intarr(nmax)
	n_lines = intarr(nmax)
	e_lines = fltarr(linmax,nmax)
	line_indx = intarr(linmax,nmax)
	shell = intarr(nmax)
	e_bind = fltarr(nmax)
	fluoro = fltarr(nmax)
	jump = fltarr(nmax)

	if continuum then begin
		es = beam[0].spectrum.E
		nes = beam[0].spectrum.N
		nhs = n_elements(beam)							; # half-slice steps
;		branch_ratio = fltarr(linmax,nmax,nhs)
		min_rel = 0.000001
	endif else begin
		branch_ratio = fltarr(linmax,nmax)
		min_rel = 0.000001
	endelse
	if photo then init_xrf_lines, e_beam

	gamma_elements = [9,11]

	if n_elements(select_z) gt 0 then begin
		qz = intarr(95)
		q = where( (select_z ge 2) and (select_z le 94))
		if q[0] eq -1 then begin
			warning,'get_lines',['No elements were selected.','Revise your element list.']
			z = (gamma eq 1) ? 9 : 26
		endif else begin
			qz[ select_z[q]] = 1
			z = where( qz eq 1)
		endelse
	endif else begin
		if gamma then begin
			z = gamma_elements
		endif else begin
			z = indgen(93)+2
		endelse
	endelse

	q = where( (z ge 2) and ( z le 94))
	if q[0] eq -1 then begin
		if gamma then begin
			z = gamma_elements
		endif else begin
			z = indgen(93)+2
		endelse
	endif

	n = 0
	nz = n_elements(z)
	if nz lt 1 then goto, done

;	As gamma-ray line mneumonics are invented here, they must be added
;	to the definitions in LINE_ID and LINE_INDEX.

	for i=0L,nz-1 do begin
		if gamma then begin
			case z[i] of
				9: begin
					if (e_min lt 197.0) and (e_max gt 197.0) then begin
						e_lines[0,n] = 197.0
						branch_ratio[0,n] = 1.0
						line_indx[0,n] = line_index('F197')
						n_lines[n] = 1

						z2[n] = z[i]
						shell[n] = 1
						e_bind[n] = 0.0
						fluoro[n] = 1.0
						jump[n] = 0.0
						n = n+1
						if n ge nmax then goto, done
					endif
					end
				11: begin
					if (e_min lt 440.0) and (e_max gt 440.0) then begin
						e_lines[0,n] = 439.9
						branch_ratio[0,n] = 1.0
						line_indx[0,n] = line_index('Na440')
						n_lines[n] = 1

						z2[n] = z[i]
						shell[n] = 1
						e_bind[n] = 0.0
						fluoro[n] = 1.0
						jump[n] = 0.0
						n = n+1
						if n ge nmax then goto, done
					endif
					end
				else:
			endcase
		endif else begin
			for ishell=1,3 do begin

				if include_shell( z[i],ishell, e_min,e_max, list=list, $
							min_rel=min_rel, e=e, rel=rel, photo=photo) then begin

					m = n_elements(e) < linmax
					n_lines[n] = m
					z2[n] = z[i]
					shell[n] = ishell
					e_lines[0:m-1,n] = e[0:m-1]
					line_indx[0:m-1,n] = list[0:m-1]
					e_bind[n] = edge( z[i], klm[ishell])
					fluoro[n] = fluor_yield( z[i], klm[ishell], photo=photo)
					jump[n] = jump_ratio( z[i], ishell, photo=photo)
					if continuum eq 0 then branch_ratio[0:m-1,n] = rel[0:m-1]
					n = n+1
					if n ge nmax then goto, done
				endif
			endfor
		endelse
	endfor

done:
	if n eq 1 then begin
		e_lines = e_lines[*,0]
		if continuum eq 0 then branch_ratio = branch_ratio[*,0]
		line_indx = line_indx[*,0]
		n_lines = n_lines[0]
		z2 = z2[0]
		shell = shell[0]
		e_bind = e_bind[0]
		fluoro = fluoro[0]
		jump = jump[0]

	endif else if n gt 1 then begin
		e_lines = e_lines[*,0:n-1]
		if continuum eq 0 then branch_ratio = branch_ratio[*,0:n-1]
		line_indx = line_indx[*,0:n-1]
		n_lines = n_lines[0:n-1]
		z2 = z2[0:n-1]
		shell = shell[0:n-1]
		e_bind = e_bind[0:n-1]
		fluoro = fluoro[0:n-1]
		jump = jump[0:n-1]

	endif else begin
		e_lines = 0.0
		if continuum eq 0 then branch_ratio = 0.0
		line_indx = 0
		n_lines = 0
		z2 = 0
		shell = 0
		e_bind = 0.0
		fluoro = 0.0
		jump = 0.0

	endelse
	return, e_lines
	end

