function geopixe_scale, relative=relative

; Return (relative) scaling for OS if system fonts change
;
; Use geopixe_scale() to scale old explicit Linux/Win sizes.
; Use geopixe_scale(relative=0) to scale ones where the sizes included *sxy from common.

common c_working_dir, geopixe_root
common c_geopixe_scaling, sxy
if n_elements(geopixe_root) lt 1 then startupp
if n_elements(relative) eq 0 then relative=1

	if relative eq 0 then return, sxy

	case os_type() of
		'Linux': begin
			sxyr = sxy /1.136
			end
		'Mac': begin
			sxyr = sxy /1.136
			end
		'Win': begin					
			sxyr = sxy /1.0
			end
	endcase

	return, sxyr
end
