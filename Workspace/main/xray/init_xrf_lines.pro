	pro init_xrf_lines, e, force=force, jump=jump, verbose=verbose
;
;	Read in Elam XRF database for relative intensities,
;	and Ebel absorption parameterisations.
;
;	W.T. Elam et al., Radiation Physics and Chemistry 63 (2002) 121-128.
;	H. Ebel, et al., X-Ray Spectrom. 32 (2003) 442-451
;
;	Missing lines added using PIXE database.
;	New lines added to ElamDB12.txt
;
;	X-ray relative intensities calculated in 'xrf_calc_int'
;
;	/force		force the data reads
;	/jump		use jump ratios, else use Ebel sub-shell cross-sections
;	/verbose	print messages

;	Do not use /force in GeoPIXE code destined for GeoPIXE.sav

COMPILE_OPT STRICTARR

	common c_xray, energy, relint, xray_OK
	common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK
	common c_ebel, ebel, xrf_ebel_OK
	common c_elam, elam

	if n_elements(old_xrf_e) lt 1 then old_xrf_e=0.0
	if n_elements(e) lt 1 then e=old_xrf_e
	if e lt 0.001 then e=30.0
	if n_elements(force) lt 1 then force=0
	if n_elements(jump) lt 1 then jump=0
	if n_elements(verbose) lt 1 then verbose=0

	if n_elements(xrf_ebel_OK) lt 1 then xrf_ebel_OK=0
	if n_elements(xrf_OK) lt 1 then xrf_OK=0
	if xrf_OK and xrf_ebel_OK and (force eq 0) and $
			(e eq old_xrf_e) then return

	if( n_elements(xray_OK) eq 0) then xray_OK = 0
	if( xray_OK ne 1) then init_xray_lines
	if( xray_OK ne 1) then goto, bad

	if force or (xrf_ok eq 0) then begin
		d = decode_elam()
		if size(d,/tname) ne 'STRUCT' then goto, bad
		elam = d
		xrf_ok = 1
	endif

	if force or (xrf_ebel_ok eq 0) then begin
		f = build_ebel_data()
		if size(f,/tname) ne 'STRUCT' then goto, bad
		ebel = f
		xrf_ebel_OK = 1
	endif

	g = xrf_calc_int(energy=e, jump=jump, verbose=verbose)
	if size(g,/tname) ne 'STRUCT' then goto, bad

	xenergy = fltarr(53,95)			; z from 0 to 95
	xrelint = fltarr(53,95)			; 52 line mneumonics
	old_xrf_e = e

;	Copy PIXE DB values (use for K shell) ...

	for z=1,94 do begin
		xenergy[*,z] = energy[z,*]
		xrelint[*,z] = relint[z,*]
	endfor

;	Use Elam XRF DB values for L and M shell ...

	xenergy[13:31,1:94] = g[1:94].e[13:31]					; L
	xrelint[13:31,1:94] = g[1:94].intense[13:31]
	xenergy[51:52,1:94] = g[1:94].e[51:52]
	xrelint[51:52,1:94] = g[1:94].intense[51:52]

	xenergy[32:37,1:94] = g[1:94].e[32:37]					; M
	xrelint[32:37,1:94] = g[1:94].intense[32:37]
	xenergy[48:50,1:94] = g[1:94].e[48:50]
	xrelint[48:50,1:94] = g[1:94].intense[48:50]

	return

bad:
	warning, /error, 'init_xrf_lines','failed to open data file.'
	return
	end
