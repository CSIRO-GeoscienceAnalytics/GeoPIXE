function find_charge_val_unit_index, sensitivity, ounit, iunit=iunit, write_back=write_back, time=time, ionbeam=ionbeam

; Find the 'val' and 'unit' droplist indices for the input 'sensivitity'
; 'sensitivity' is assumed to be in 'nA/V' (or 'nA/Hz') units.
;
; If /time use the time units only, else skip time units.
;
; Optionally, a units scaling from nA/V can be input as 'ounit'.
; If /write_back, write back sensitivity and ounit in standard ranges.
;		make sure they are local writable variables passed by reference.

COMPILE_OPT STRICTARR
	iunit = 0
	if n_elements(sensitivity) eq 0 then return, 0
	if n_elements(write_back) eq 0 then write_back=0
	if n_elements(ionbeam) eq 0 then ionbeam=0
	if n_elements(time) eq 0 then time=0
	if (arg_present(sensitivity) eq 0) or (arg_present(ounit) eq 0) then begin
		if write_back then begin
			warning,'find_charge_val_unit_index',['Option to save altered parameters using /write-back.', $
				'But either "sensivity" or "ounit" is not passed by reference and writable.']
		endif
	endif
	if n_elements(ounit) eq 0 then ounit=1.
	
	val = charge_gain_units( sensitivity, units=vunit)
	vunit = ounit*vunit

	if write_back then begin
		ounit = vunit				; write these back
		sensitivity = val
	endif
	
	charge_gain_unit_lists, vals, units, vunits, alt_units=alt_units, times=times, ionbeam=ionbeam
	q = where( times eq time)

	q1 = sort( abs(vunit - vunits[q])/(vunits[q] > 1.0e-12) )
	iunit = q[q1[0]]
	
	q2 = sort( abs(val - vals)/(vals))
	ival = q2[0]
	
	return, ival
end

 