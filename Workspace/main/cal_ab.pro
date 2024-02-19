    pro cal_ab, cal, ca, cb, cu, error=error, set=set, units=units

;   Convert the calibration 'cal' to keV, as 'ca', 'cb', 'cu',
;   or to 'units' if supplied.
;
;   If /set, then set 'cal' from 'ca', 'cb', 'cu',
;   converting to the current units of 'cal'.
;
;   *** NOTE: In /set mode, remember that IDL only passes simple variable names
;   by reference. So this /set update will not work passing (*pspec).cal, for example.
;   You may need to use a temporary variable for this.

    if n_elements(set) lt 1 then set=0
    if n_elements(units) lt 1 then units='keV'
    error = 1
    units_list = ['EV','KEV','MEV','GEV','ENERGY']
    units_ev = [1.,1000.,1000000.,1000000000.,1000.]
	if cal.order ne 1 then return
	
    if set then begin          			  ; assume ca,cb in keV
       eu = strupcase(cal.units)
       error = 0
       if eu eq 'EV' then begin
         cal.poly[1] = ca * 1000.
         cal.poly[0] = cb * 1000.
       endif else if (eu eq 'KEV') or (eu eq 'ENERGY') then begin
         cal.poly[1] = ca
         cal.poly[0] = cb
       endif else if eu eq 'MEV' then begin
         cal.poly[1] = ca / 1000.
         cal.poly[0] = cb / 1000.
       endif else return
    endif else begin
       ca = cal.poly[1]
       cb = cal.poly[0]
       cu = cal.units
       if (abs(ca - 1.0) lt 0.001) or (ca lt 1.0e-6) then return
       was = strupcase(cu)
       now = strupcase(units)
       q1 = where( was eq units_list)
       q2 = where( now eq units_list)
       if (q1[0] eq -1) or (q2[0] eq -1) then return
       scale = units_ev[q1[0]] / units_ev[q2[0]]
       ca = ca * scale
       cb = cb * scale
       cu = units
       error = 0
    endelse

    return
    end

