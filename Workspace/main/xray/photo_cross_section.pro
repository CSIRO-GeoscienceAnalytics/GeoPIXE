	function photo_cross_section, Zi, Ei, elastic=elastic, compton=compton, pair=pair, $
									shell=shell, subshell=subshell

; Returns the photon cross-sections (photoelectric by default) for the element of atomic number Z
; at energy E keV, based on the tables of Hubbell and Seltzer.
;
; If 'shell' is omitted or zero, it returns total photoelectric cross-section.
;
; If 'shell' and 'subshell' are included it returns just the cross-section for this sub-shell.
; 'shell' = 1,2,3,4 for K,L,M,N.
;
; If 'subshell' is zero, and 'shell' is not, then return total cross-section for that shell,
; 											 summing all sub-shells.
;
; Z and E can be vectors. If they both are vectors, then they must be equal length.
; Works much better for Z scalar and E vector.

common c_hubbell_2, hubbell_raw_ok, hubbell_data

	if n_elements(elastic) lt 1 then elastic=0
	if n_elements(compton) lt 1 then compton=0
	if n_elements(pair) lt 1 then pair=0
	if n_elements(shell) lt 1 then shell=0
	if n_elements(subshell) lt 1 then subshell=0
	if( n_elements(hubbell_raw_OK) eq 0) then hubbell_raw_OK = 0
	if( hubbell_raw_OK ne 1) then init_hubbell_raw
	if( hubbell_raw_OK ne 1) then return, 0.0

	shells = ['','K','L','M','N','O']
	label = ''
	if shell gt 0 then label = shells[shell] + str_tidy(subshell > 1)

	z = zi
	e = ei

	nen = n_elements(E)
	nz = n_elements(Z)
	nn = nz

	if (nen lt 1) or (nz lt 1) then return, 0.0
	if nen eq 1 then begin
		nn = nz
		result = fltarr(nn)

		q = where((z ge 1) and (z le 92))
		if q[0] eq -1 then goto, done

		for i=0L,n_elements(q)-1 do begin
			n = hubbell_data[z[q[i]]-1].n
			if z[q[i]] ne hubbell_data[z[q[i]]-1].z then print,'photo_cross_section: Z out of sync.'

			extra = 0
			if elastic then begin
				result[i] = exp( interpol( alog( hubbell_data[z[q[i]]-1].rayleigh[0:n-1]), $
									alog( hubbell_data[z[q[i]]-1].e[0:n-1]), alog(e)))
			endif else if compton then begin
				result[i] = exp( interpol( alog( hubbell_data[z[q[i]]-1].compton[0:n-1]), $
									alog( hubbell_data[z[q[i]]-1].e[0:n-1]), alog(e)))
			endif else if pair then begin
				result[i] = exp( interpol( alog( hubbell_data[z[q[i]]-1].pair[0:n-1]), $
									alog( hubbell_data[z[q[i]]-1].e[0:n-1]), alog(e)))
			endif else begin
				result[i] = exp( interpol( alog( hubbell_data[z[q[i]]-1].photo[0:n-1]), $
									alog( hubbell_data[z[q[i]]-1].e[0:n-1]), alog(e)))

				if (shell ne 0) then begin
					ratk = 1.
					ratj = 1.
					if subshell eq 0 then begin
						q1 = where( hubbell_data[z[q[i]]-1].edge_shell eq shell, nq1)
						if q1[0] eq -1 then begin
							result[i] = 0.0
							continue
						endif

						edm = max( hubbell_data[z[q[i]]-1].edge_e[q1])					; max of edges found
						for k=0L,nq1-1 do begin
							if e gt hubbell_data[z[q[i]]-1].edge_e[q1[k]] then begin
								ratk = ratk * hubbell_data[z[q[i]]-1].edge_ratio[q1[k]]
							endif
						endfor
						q1 = where( hubbell_data[z[q[i]]-1].edge_shell ne shell, nq1)
					endif else begin
						q1 = where( label eq hubbell_data[z[q[i]]-1].edge_label, nq1)
						q1 = q1[0]
						nq1 = 1
						if q1 eq -1 then begin
							result[i] = 0.0
							continue
						endif

						edm = hubbell_data[z[q[i]]-1].edge_e[q1]							; edge found
						if e gt edm then begin
							ratk = ratk * hubbell_data[z[q[i]]-1].edge_ratio[q1]
						endif

						mask = intarr( hubbell_data[z[q[i]]-1].ns)
						mask[q1] = 1
						q1 = where( mask eq 0, nq1)
					endelse

					result[i] = result[i] * (1. - 1./ratk)

					if nq1 eq 0 then goto, done
					for k=0L,nq1-1 do begin
						if (e gt hubbell_data[z[q[i]]-1].edge_e[q1[k]]) and $
										(hubbell_data[z[q[i]]-1].edge_e[q1[k]] gt edm) then begin
							ratj = ratj / hubbell_data[z[q[i]]-1].edge_ratio[q1[k]]
						endif
					endfor

					result[i] = result[i] * ratj
				endif
			endelse
		endfor
	endif else if nz eq 1 then begin
		nn = nen
		result = fltarr(nn)

		if (z lt 1) or (z gt 92) then goto, done
		n = hubbell_data[z-1].n
		if z ne hubbell_data[z-1].z then print,'photo_cross_section: Z out of sync.'

		if elastic then begin
			result = exp( interpol( alog( hubbell_data[z-1].rayleigh[0:n-1]), $
							alog( hubbell_data[z-1].e[0:n-1]), alog(e)))
		endif else if compton then begin
			result = exp( interpol( alog( hubbell_data[z-1].compton[0:n-1]), $
							alog( hubbell_data[z-1].e[0:n-1]), alog(e)))
		endif else if pair then begin
			result = exp( interpol( alog( hubbell_data[z-1].pair[0:n-1]), $
							alog( hubbell_data[z-1].e[0:n-1]), alog(e)))
		endif else begin
			result = exp( interpol( alog( hubbell_data[z-1].photo[0:n-1]), $
							alog( hubbell_data[z-1].e[0:n-1]), alog(e)))

			if shell ne 0 then begin
				ratk = replicate(1., nn)
				ratj = replicate(1., nn)
				if subshell eq 0 then begin
					q1 = where( hubbell_data[z-1].edge_shell eq shell, nq1)
					if q1[0] eq -1 then begin
						result[*] = 0.0
						goto, done
					endif

					edm = max( hubbell_data[z-1].edge_e[q1])					; max of edges found
					for k=0L,nq1-1 do begin
						qk = where( e gt hubbell_data[z-1].edge_e[q1[k]] )
						if qk[0] ne -1 then ratk[qk] = ratk[qk] * hubbell_data[z-1].edge_ratio[q1[k]]
					endfor

					q1 = where( hubbell_data[z-1].edge_shell ne shell, nq1)
				endif else begin
					q1 = where( label eq hubbell_data[z-1].edge_label, nq1)
					q1 = q1[0]
					nq1 = 1
					if q1 eq -1 then begin
						result[*] = 0.0
						goto, done
					endif

					edm = hubbell_data[z-1].edge_e[q1]							; edge found
					qk = where( e gt edm )
					if qk[0] ne -1 then ratk[qk] = ratk[qk] * hubbell_data[z-1].edge_ratio[q1]

					mask = intarr( hubbell_data[z-1].ns)
					mask[q1] = 1
					q1 = where( mask eq 0, nq1)
				endelse

				result = result * (1. - 1./ratk)

				if nq1 eq 0 then goto, done
				for k=0L,nq1-1 do begin
					qk = where( (e gt hubbell_data[z-1].edge_e[q1[k]]) and $
								(hubbell_data[z-1].edge_e[q1[k]] gt edm), nqk )
					if qk[0] ne -1 then ratj[qk] = ratj[qk] / hubbell_data[z-1].edge_ratio[q1[k]]
				endfor

				result = result * ratj
			endif
		endelse
	endif else if nz ne nn then begin
		print,'photo_cross_section: Z,E can only both be vectors if they have same length.'
		result = fltarr(nn)
		goto, done
	endif

done:
	if n_elements(result) eq 1 then result=reform(result)
	return, result * 1.0e-24
end
