function get_linearize, file, max=nmax, do_linear=do_linear, $
						inverse=inverse, multi=multi, n_det=n_det, $
						details=details, new=new

; Read in linearization polynomial from file 'file' and
; the linearization lookup table, and return it.
;
; /inverse  invert linearize table relative to straight line.
; /new      file must use newer linearize tables (ver=3,4).
;
; Returns:
;	F	integral lookup table for new 'e', based on old 'e'
;		F[n], unless multi=1, then returns F[n,det]
;
;	multi=1 returned to indicate individual linearization for each detector		
;
; Use cases:

; .linear files
;	(a) Version=1  Old pre Maia linearization files
;		Return simple lookup table F[n] for one detector (multi=0)
;
;	(b) Version=2  Individual detector linearization tables
;		Return table for each detector F[n,det] (multi=1)
;			Used in 'da_evt' to correct each detector 'e'
;
;	(c) Version=3  Fractional tables (multi=0)
;		Return table for fractional channel steps F[frac,n]
;
; .linear.var files
;	(d) Version=3  Fractional tables in .var file (multi=0)
;		Return table for fractional channel steps F[frac,n]
;
;	(e) Version=4  Coarse interp tables in .var file for each chip
;		Return table for each detector F[n,det] (multi=1)
;			Used in 'da_evt' to correct each detector 'e'

do_linear = 0
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
		warning,'get_linearize',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_file
	endif
endif
if n_elements(nmax) lt 1 then nmax = 8191
if n_elements(inverse) lt 1 then inverse=0
if n_elements(n_det) lt 1 then n_det=384
if n_elements(new) lt 1 then new=0

if n_elements(file) lt 1 then begin
	file = ''
	goto, bad_file
endif
if strlen(file) lt 1 then goto, bad_file
multi = 0

	on_ioerror, bad_file
	openr, lun, file, /get_lun
	
if extract_extension(file) eq 'linear' then goto, old_linear

	error = 1
	data = {	version:	3, $					; version
				n_table:	4096L, $				; size of spectrum table
				n_fraction:	32, $					; fractional steps
				n_det:		0, $					; detector fitted
				n_pars:		0, $					; number of fitting pars
				pars:		ptr_new(0.0)}			; pointer to pars
	line = ''
	
	on_ioerror, bad_read
	while NOT EOF(lun) do begin
		readf, lun, line
		i = locate('#', line)
		if i eq -1 then goto, read_table
		if i eq 0 then begin
			s0 = strsplit( line, '# : 	',/extract)
			s = strlowcase(s0)
			ns = n_elements(s)
			if ns ge 2 then begin
				case s[0] of
					'version': begin
						data.version = long2(s0[1])
						end
					'n_table': begin
						data.n_table = long2(s0[1])
						end
					'n_fraction': begin
						data.n_fraction = long2(s0[1])
						end
					'n_det': begin
						data.n_det = long2(s0[1])
						end
					'n_pars': begin
						data.n_pars = long2(s0[1])
						end
					'fit_pars': begin
						data.pars = ptr_new( float2(s0[1:*]))
						end
					else:
				endcase
			endif
		endif
	endwhile

read_table:
	n = data.n_table
	nfrac = data.n_fraction
	na = data.n_pars
	coeff = *data.pars
	ndet = data.n_det
	version = data.version
	f = fltarr(nfrac,n)
	chip_max = 0
	
	while NOT EOF(lun) do begin
		i = locate('#', line)
		if i eq -1 then begin
			s = strsplit( line, '[] 	', /extract)
			ns = n_elements(s)
			if ns ge 4 then begin
				if (s[0] eq 'linearise.energy') and (s[2] eq '.efrac') then begin
					if ns ge nfrac+3 then begin
						j = long2(s[1])
						f[0:nfrac-1,j] = float2(s[3:3+31])
					endif
				endif else if (s[0] eq 'linearise.energy') and (s[1] eq '.efrac') then begin
					nval = ns - 2
					nf = nval/n
					f[0:nf-1,0:n-1] = float2(s[2:2+nf*n-1])
				endif else if (s[0] eq 'linearise2.chip') and (s[3] eq '.etrim') then begin
					nval = ns - 4
					nm = n < nval
					chip = fix2(s[1])
					chip_max = chip_max > chip
					f[chip,0:nm-1] = float2(s[4:4+nm-1])
				endif
			endif
		endif

		readf, lun, line
	endwhile
	close_file, lun

	if data.version eq 4 then begin
		multi = 1
		scale = 4096L / n
		x = scale * findgen( long(n))
		f2 = fltarr(4096,n_det)

		for i=0,n_det-1 do begin
			ichip = i / 32
			y = reform(f[ichip,*]) + x
	
			t = findgen( 4096L)
			t2 = interpol( x,y, t)
			f2[*,i] = t2
		endfor
		f = f2
		n = 4096L
		ndet = n_det
		Fo = f
	endif else begin
		Fo = reform(f[0,*])
	endelse
	goto, cont
	
;............................................................................

old_linear:
	n = 0
	version = 0
	readu, lun, n, version
	if n lt 1 then goto, bad_file
	big = nmax < (n-1)
	nfrac = 1L
	na = 2L
	ndet = 0
	
	case version of
		1: begin									; simple interp table
			coeff = fltarr(2)
			f = fltarr(n)
			readu, lun, coeff	
			readu, lun, f
			Fo = F
			end
		2: begin									; multiple tables for n detectors
			multi = 1
			f = fltarr(n,n_det)
			while not EOF(lun) do begin
				ndet = 0
				na = 0L
				readu, lun, ndet
				readu, lun, na
				if n_elements(coeff) lt 1 then coeff = fltarr(na,n_det)
				coeff2 = fltarr(na)
				f2 = fltarr(n)
				readu, lun, coeff2
				readu, lun, f2
				f[*,ndet] = f2
				coeff[*,ndet] = coeff2
			endwhile
			Fo = F
			end
		3: begin									; single table with fracional steps
			ndet = 0
			na = 0L
			nfrac = 0L
			readu, lun, ndet
			readu, lun, na
			coeff = fltarr(na)
			readu, lun, coeff
			readu, lun, nfrac
			f = fltarr(n*nfrac)
			readu, lun, f
			f = reform(f, nfrac, n)			; 5 June 2012
			Fo = reform(f[0,*])
			end
		else: goto, bad_file
	endcase
	close_file, lun
	
;............................................................................

cont:	
	if new and (version lt 3) then begin
		warning,'get_linearize','Cannot use old .Linear file with Maia 384.'
		goto, bad_file
	endif
	
	if inverse then begin
		case version of
			4: begin
				fl = findgen(n)
				for i=0L,n_det-1 do f[*,i] = fl - (f[*,i]-fl)
				Fo = f
				end
			3: begin
				fl = findgen(n*nfrac) / nfrac
				f = fl - (f-fl)
				f = reform(f,nfrac,n)
				Fo = reform(f[0,*])
				end
			2: begin
				fl = findgen(n)
				for i=0L,n_det-1 do f[*,i] = fl - (f[*,i]-fl)
				Fo = f
				end
			else:
		endcase
	endif
	
	do_linear = 1
	details = {n_int:n, n_frac:nfrac, a:coeff, na:na, n_det:ndet, f:f, multi:multi}
	return, Fo > 0.0

bad_file:
	close_file, lun
	return, -1
bad_read:
	close_file, lun
	return, -1
end
