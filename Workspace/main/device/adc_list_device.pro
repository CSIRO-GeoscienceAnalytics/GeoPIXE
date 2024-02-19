function adc_list_device, obj, max_adcs=max_adcs

; Return a list of SADC number string, to be used in droplists, etc.
; 'obj' is a new Device Object.

common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=16
if n_elements(max_adcs) lt 1 then max_adcs=geopixe_max_adcs

define_devices
if obj_valid( obj) eq 0 then begin
	list = '#' + str_tidy( (indgen(max_adcs)) )
	return, list
endif

offset = obj->start_adc() - 1
list = '#' + str_tidy( (indgen(max_adcs)+1+offset) )

return, list
end
