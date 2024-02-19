pro merge_slices, slices, last, lid

; Merge all slices after 'last' into their full layer thicknesses
;
; These will be used to calculate the full cmux back through a
; transmission target to detector.

	n_slices = n_elements(slices)
	last = last < (n_slices-1)

	slices_out = slices[0:last]
	lid_out = lid[0:last]
	n_slices_out = last+1
	if n_slices_out ge n_slices then goto, finish

	current = -1
	j = last
	for i=last+1,n_slices-1 do begin
		if lid[i] ne current then begin
			slices_out = [slices_out, slices[i]]
			lid_out = [lid_out,lid[i]]
			current = lid[i]
			j = j+1
		endif else begin
			slices_out[j].thick = slices_out[j].thick + slices[i].thick
		endelse
	endfor

finish:
	slices = slices_out
	lid = lid_out
	return
end
