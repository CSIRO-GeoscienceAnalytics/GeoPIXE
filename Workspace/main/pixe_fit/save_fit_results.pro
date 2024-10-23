pro save_fit_results, pstate, F

	; Write the results to 'F'

	if n_params() lt 2 then begin
		print,'save_fit_results: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	no_results = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	presults = (*pstate).presults
	if ptr_valid(presults) eq 0 then goto, bad_ptr
	if size(*presults,/tname) ne 'POINTER' then begin
		no_results = 1
	endif else begin
		if ptr_valid( (*presults)[0] ) eq 0 then no_results=1
		if no_results eq 0 then if size(*(*presults)[0],/tname) ne 'STRUCT' then no_results=1
	endelse

	n = 0
	for i=0L,n_elements(*presults)-1 do begin
		p = (*presults)[i]

		n_els = (*p).n_els							; number of elements
		n_layers = n_elements((*p).yield.thick)		; number of layers

		if (n_els gt 0) and (n_layers gt 0) then begin
			n = n+1
		endif
	endfor
	if n eq 0 then no_results=1
	if no_results then goto, bad_data

	version = -16L									; version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR


	writeu,1, version
	writeu,1, n

	for i=0L,n-1 do begin
		p = (*presults)[i]

		n_els = (*p).n_els							; number of elements
		n_layers = n_elements((*p).yield.thick)		; number of layers
		org = (*p).org
		rorg = (*p).rorg
		if (org lt 1) or (org gt 100) then org = 10

		if (n_els gt 0) and (n_layers gt 0) then begin
			writeu,1, n_els, n_layers, org
			writeu,1, rorg
			writeu,1, (*p).type
			writeu,1, (*p).conc, (*p).error, (*p).mdl, (*p).area, (*p).aerror, (*p).amdl
			writeu,1, (*p).scale, (*p).mode
			writeu,1, (*p).el
			writeu,1, (*p).setup, (*p).fit, (*p).nlinear
			writeu,1, (*p).cuts, (*p).detector, (*p).filter
			writeu,1, (*p).spectrum, (*p).background
			writeu,1, (*p).back_split
			writeu,1, (*p).flux
			writeu,1, (*p).yield
			writeu,1, (*p).inclusion
			writeu,1, (*p).veto
			writeu,1, (*p).tweek
			writeu,1, (*p).counts_per_ppm_uc
			writeu,1, (*p).compton
			writeu,1, (*p).array.on
			if (*p).array.on then begin
				writeu,1, (*p).array.N_det, n_elements((*p).array.active)
				writeu,1, (*p).array.active
				writeu,1, (*p).array.rGamma
				writeu,1, (*p).array.nk
				writeu,1, (*p).array.cIntensity
			endif
			writeu,1, (*p).stim
			writeu,1, (*p).correct
			writeu,1, (*p).deadtime_correction

			writeu,1, (*p).beam.continuum
			use_beam = 0L
			err = 0
			if (*p).beam.continuum ge 1 then use_beam=1L
			if use_beam then begin
				writeu,1, (*p).beam.model
				case (*p).beam.model of
					1: begin
						write_source, (*p).beam, unit=1, error=err
					end
					2: begin
						write_pink, (*p).beam, unit=1, error=err
					end
					else:
				endcase
			endif
			if err then goto, bad_beam
		endif
	endfor

	close,1
	return

	bad_io:
	warning,'save_fit_results','Error writing results file',/error
	return
	bad_beam:
	warning,'save_fit_results','Error writing results beam struct',/error
	return
	bad_ptr:
	warning,'save_fit_results','bad results pointer',/error
	return
	bad_data:
	warning,'save_fit_results','bad results data structure',/error
	return
end
