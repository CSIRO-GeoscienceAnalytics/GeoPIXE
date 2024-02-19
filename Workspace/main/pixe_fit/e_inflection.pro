function e_inflection, filters

;	Return the background inflection approximation at energy E
;	for the current filters.

	t_inflection = 0.15		; transmission at inflection

	for e=6.,16.,2. do begin
		e1 = e
		if transmit(filters,e) gt t_inflection then begin
			return, e
		endif
	endfor

	return, 16.
	end

