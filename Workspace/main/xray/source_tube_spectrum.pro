function source_tube_spectrum, E0, power, bin=bin, Energy=E, detector=det, phi=phi, eps=eps, $
				filter=filter, proportion=proportion, char=char, mono=mono, $
				formula=formula, weight=weight, lines=lines, transmission=transmission, $
				pressure=pressure, temp=temp, error=error

; Model an X-ray tube source spectrum, built on 'tube_spectrum2.pro'
;
; Electron beam incident and X-rays exiting from same side usually, unless a "transmission" thickness
; is specified, then a thin transmission target is assumed.
;
; input:
;	E0		electron energy/tube voltage (keV)
;	power	power (W)
;	phi		incident angle (to surface)
;	eps		takeoff angle (to surface)
;	bin		energy spectrum channel width (keV)
;	formula	use a chemical 'formula' for the anode
;	/weight	formula uses weight% on elements (outside any brackets if present)
;	transmission	thickness of thin transmission target (mg/cm2)
;			
;	filter	source filter specification struct (or pointer) array
;			(must adhere to std form (thick in mg/cm2))
;
;	If 'presssure' and 'temp' present, this indicates a gas filter.
;		In this case assume thick is mg/cm2 NPT (1013.25 HPa, 20 C) and scale this for new PT.
;		These are scalars, for ambient conditions.
;
;	mono = [energy, bandwidth, eff]
;			energy		centre energy (keV)
;			bandwith	% bandwith around centre (default = 3%)
;			eff			transmission/reflection efficiency (default = 0.5)
;
;	det		optional detector struct (or pointer) (else return spectrum per sr)
;
; return:
;	spec	spectrum per each dE bin (ph / (bin) keV / sr / s)
;	Energy	E values for spectrum
;	proportion	proportion in each channel due to continuum
;	char	characteristic line spectrum
;	lines	details of all characteristic lines
;	error	error=1, OK=0

	COMPILE_OPT STRICTARR
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
			warning,'source_tube_spectrum',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0.
		endif
	endif
	error = 1

	if n_elements(E0) lt 1 then E0=70.
	if n_elements(power) lt 1 then power=100.
	if n_elements(bin) lt 1 then bin=E0/1000.
	if n_elements(phi) lt 1 then phi=90.
	if n_elements(eps) lt 1 then eps=5.
	if n_elements(mono) lt 1 then begin
		use_mono = 0
	endif else begin
		use_mono = 1
		monE = mono[0]
		if monE gt 0.5 then begin
			if n_elements(mono) lt 2 then begin
				bw = 0.03
			endif else begin
				bw = mono[1] / 100.
			endelse
			if n_elements(mono) lt 3 then begin
				meff = 0.5
			endif else begin
				meff = mono[2]
			endelse
		endif else use_mono = 0
	endelse
	
	startupp, /database
	
; Bremsstrahlung spectrum (/s) plus lines (only for a single element for now) 

	time = 1.					; acquisition time (s)
	ImA = power/E0				; beam current (mA)
	
	dN = tube_spectrum2( E0, phi, eps, bin=bin, Energy=E, formula=formula, weight=weight, $
							detector=det, proportion=proportion, char=charspec, lines=lines, $
							transmission=transmission, error=error)
	if error then return, 0.0
	spec = ImA * time * dN
	char = ImA * time * charspec
	
; Modify this spectrum for a source filter ...
; Optionally, also select within monochromator bandwith ...

	t = transmit( filter, E, pressure=pressure, temp=temp)
	
	if use_mono then begin
		dE = monE * bw / 2.
		q = where( (E lt monE-dE) or (E gt monE+dE), nq)
		if nq gt 0 then t[q] = 0. 
		t = t * meff
	endif
	
	spec = spec * t
	char = char * t	

;	Correct the relative intensities of the lines, and veto weak elements ...
	
	if n_elements(lines) gt 0 then begin
		bad = 0
		q = where( lines.line ne 0, nq)
		if nq gt 0 then begin
			tinterp = interpol(t, e, lines.e[q])
			lines.arel[q] = lines.arel[q] * tinterp
			
			amax = max(lines.arel)
			q = where( lines.arel[0,*] gt 1.0e-3*amax, nz)				; veto
			if nz gt 0 then begin
				nl = n_elements( lines.line[*,0])
				lines = { n_lines:	lines.n_lines[q], $					; number of lines per element/shell
						Z:			lines.Z[q], $						; Z
						shell:		lines.shell[q], $					; shells of elements
						line:		lines.line[0:nl-1,q], $				; line index
						e:			lines.e[0:nl-1,q], $				; line energy
						rel:		lines.rel[0:nl-1,q], $				; line relative intensity
						arel:		lines.arel[0:nl-1,q]}				; absolute line intensity
				for i=0,nz-1 do begin
					lines.rel[*,i] = lines.arel[*,i] / lines.arel[0,i]
				endfor
			endif else bad=1
		endif else bad=1
		if bad then lines.line = 0
	endif
	
	q = where( (E ge 2.0) and (E lt 100.), nq)
	spec = spec[q]
	char = char[q]
	E = E[q]
	proportion = proportion[q]
	
	return, spec
end
