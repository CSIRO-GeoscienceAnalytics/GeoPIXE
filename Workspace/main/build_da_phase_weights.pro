function build_DA_phase_weights, da, phase, pcorr, q=q, compress=compress, error=error

; Build the [E,phase,X,Y] weighting matrix for use with phase DA event processing.
; It will be compressed to mamage size, and return a index array 'q' for access 
; into the new table. Hence, to use the table:
; 		table[ q[Eold], j, X/compress, Y/compress]
; 
; Input:
;	da		DA matrix struct (for mpdam version with n_comp components)
;	phase	phase maps (for selected sub-region or compression in DA_evt)
;	pcorr	pointer to Correct setup struct
;	
; Output:
;	q		index into the output table for E, use q[E]
;	compress additional compression to use on X,Y
;
; Return:
;	table	weighting table table[e,c,x,y]


COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'build_DA_phase_weights',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	error = 1
	if n_elements(pcorr) eq 0 then begin
		warning,'build_DA_phase_weights','pcorr missing.'
		return, 0
	endif
	if n_elements(phase) eq 0 then begin
		warning,'build_DA_phase_weights','phase missing.'
		return, 0
	endif
	n_comp = n_elements( da.matrix[0,0,*])
	if n_comp ne (*pcorr).n_comp+1 then begin
		warning,'build_DA_phase_weights','n_comp not matching.'
		return, 0
	endif
	
	conc = (*pcorr).R * 1.e+4
	n_energy = n_elements(da.pure[*,0,0])
	spec = fltarr(n_energy,n_comp)
	y = fltarr(n_comp,n_comp)
	nx = n_elements(phase[*,0,0])
	ny = n_elements(phase[0,*,0])
	
	help, spec, output=s
	gprint,level=2, 'mpda: Help=',s
	
; Form weighted 'pure' spectra (for each comp) as weights for this energy channel.

	for j=0,n_comp-1 do begin								; i element in phase
		if j lt n_comp-1 then begin							; j phase
			for i=0,n_comp-2 do begin
				q = where( (*pcorr).comp[i] eq da.el, nq)
;				gprint,level=1, 'mpda: spec i,j=',i,j,' q=',q[0]
				if nq gt 0 then begin
					y[i,j] = (da.yield[q[0],j] > 1.0e-10) * (conc[i,j] > 1.0e+4)		; mix in at least 1% of each
					spec[*,j] = spec[*,j] + y[i,j] * da.pure[*,q[0],j]		;	* 90.	; scale by rG total ??
				endif
			endfor
		endif
		y[n_comp-1,j] = 1.0									; background
		spec[*,j] = spec[*,j] + y[n_comp-1,j] * da.pure[*,0,j] / da.charge[j]
	endfor
	gprint,level=2, 'mpda: done spec, smart_congrid it ...'

;	##### Disabled the 'spec' in the Table weights, to make them just 'phase' weights
;	See	##### lines below.

	spec[*,*] = 1.0				; #####			; test a flat spectrum weighting approach
	gprint,level=2, 'mpda: a flat spectrum weight has been used AS A TEST ...'
	
	if !version.memory_bits eq 32 then begin
		nx_big = 1000L						; 700L	; #####
		nxy_big = 1000*1000L * 4/n_comp		; 500*500L * 4/n_comp	; #####
		n_energy2 = 2						; 200	; #####
	endif else begin
		nx_big = 2000L						; 1000L	; #####
		nxy_big = 2000*2000L * 4/n_comp		; 700*700L * 4/n_comp	; #####
		n_energy2 = 2						; 300	; #####
	endelse

	q = long(round(float(n_energy2-1) * findgen(n_energy) / float(n_energy-1)))
	spec = smart_congrid( spec, n_energy2, n_comp)
	gprint,level=2, 'mpda: done smart_congrid of "spec".'

	compress = 1
	nx1 = nx
	ny1 = ny
	while nx1 gt nx_big do begin
		compress = compress+1
		nx1 = nx / compress
		ny1 = ny / compress
	endwhile
	while nx1*ny1 gt nxy_big do begin
		compress = compress+1
		nx1 = nx / compress
		ny1 = ny / compress
	endwhile
	help, spec, nx1,ny1,n_comp,n_energy2, output=s
	gprint,level=2, 'mpda: Help=',s

; Compress 'phase' map to limit final size of 4D 'table.

	phase1 = smart_congrid( phase, nx1, ny1, n_comp)
	table = fltarr( n_energy2, n_comp, nx1, ny1)
	
	help, table, phase1, output=s
	gprint,level=2, 'mpda: Help=',s

	gprint,level=2, 'Build MPDA table ...'
	
; Combine product of 'spec' weights times 'phase' components to form lookup 'table' for MPDA
; These are the weights for each DA component matrix, for each event e,x,y

	error = da_build_mpda_table( table, spec, phase1, nx1, ny1, n_comp, n_energy2)

	gprint,level=2, 'MPDA table done.'
	return, table
end
		