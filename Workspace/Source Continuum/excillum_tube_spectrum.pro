function excillum_tube_spectrum, E0, power, Z, bin=bin, Energy=E, detector=det, phi=phi, eps=eps, $
				galinstan=galinstan, filter=filter, proportion=proportion, char=char, mono=mono, $
				formula=formula, weight=weight, error=error

; Excillum tube spectrum, built on 'source_tube_spectrum.pro'
;
; input:
;	Z		target Z (use Z only if no 'galinstan' index or 'formula' used)
;	galinstan=n	selects one of the alloys:
;			0	off								use formula or Z
;			1	Ga = 95%, In = 5% 						(Alloy G1)
;			2	Ga = 68.5%, In = 21.5%, Sn = 10% 		(galinstan, Alloy I1)
;			3	Ga = 47%, In = 37%, Sn = 16% 			(Alloy I2)
;			4	Bi = 49%, Pb = 18%, Sn = 12%, In = 21%	(Ostalloy 136)
;			5	Bi = 32.5%, In = 51%, Sn = 16.5%		(Fields metal)
;	formula	use a chemical 'formula' for the anode (takes priority over galinstan)
;	/weight	formula uses weight% on elements (outside any brackets if present)
;	
; Arguments passed to 'source_tube_spectrum':
;	E0		electron energy/tube voltage (keV)
;	power	power (W)
;	phi		incident angle (to surface)
;	eps		takeoff angle (to surface)
;	bin		energy spectrum channel width (keV)
;			
;	filter	source filter specification struct (or pointer)
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
			warning,'excillum_tube_spectrum',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0.
		endif
	endif
	error = 1

	if n_elements(galinstan) lt 1 then galinstan = 0
	if (galinstan eq 0) and (n_elements(Z) lt 1) then galinstan = 2
	if n_elements(formula) gt 0 then galinstan = 0
	if n_elements(galinstan) lt 1 then galinstan = 0
	
	startupp, /database
	
	case galinstan of
		1: begin
			formula = '(Ga)95(In)5'
			weight = 1
			end
		2: begin
			formula = '(Ga)68.5(In)21.5(Sn)10'
			weight = 1
			end
		3: begin
			formula = '(Ga)47(In)37(Sn)16'
			weight = 1
			end
		4: begin
			formula = '(Bi)49(Pb)18(Sn)12(In)21'
			weight = 1
			end
		5: begin
			formula = '(Bi)32.5(In)51(Sn)16.5'
			weight = 1
			end
		else: begin
			if n_elements(formula) eq 0 then begin
				if n_elements(Z) eq 0 then begin
					warning,'excillum_tube_spectrum','No "Z" or "formula" passed.'
					return, 0.0
				endif else begin
					formula = element_name(Z)
					weight = 0
				endelse
			endif
			end
	endcase
	
	spec = source_tube_spectrum( E0, power, bin=bin, Energy=E, detector=det, phi=phi, eps=eps, $
				filter=filter, proportion=proportion, char=char, mono=mono, $
				formula=formula, weight=weight, error=error)

	return, spec
end
