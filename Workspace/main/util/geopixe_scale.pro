function geopixe_scale

; Return scaling for OS if system fonts change

common c_working_dir, geopixe_root
common c_geopixe_scaling, sxy
if n_elements(geopixe_root) lt 1 then startupp

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
