function select_x, e_low, e_high, cal_a, cal_b, n_channels, pcuts

	x_low = (fix( (e_low - cal_b) / cal_a + 0.5) > 0) < (n_channels-1)
	x_high = (fix( (e_high - cal_b) / cal_a + 0.5) > 0) < (n_channels-1)
	if x_low gt x_high then return, -1

	ok = intarr(n_channels)
	ok[x_low:x_high] = 1

	if ptr_valid( pcuts) then begin
		for i=0L,n_elements(*pcuts)-1 do begin
			if (*pcuts)[i].e[2] lt (*pcuts)[i].e[3] then begin
				xl = (fix( ((*pcuts)[i].e[2] - cal_b) / cal_a + 0.5) > 0) < (n_channels-1)
				xh = (fix( ((*pcuts)[i].e[3] - cal_b) / cal_a + 0.5) > 0) < (n_channels-1)
				ok[xl:xh] = 0
			endif
		endfor
	endif

	x = where(ok eq 1)
	return, x
	end
