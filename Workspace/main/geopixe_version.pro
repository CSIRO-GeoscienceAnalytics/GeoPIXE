function geopixe_version, major=major

; Note a dummy version of "geopixe_version" gets defined
; to build idl_query. See "make_idl_query.pro".

COMPILE_OPT STRICTARR

; Format of version MUST be:
;	simple version n.m (numeric)
;	which also gets used for update dir name
;	followed by a SINGLE alphabet letter if needed.

geo_ver='8.8v'

if n_elements(major) eq 0 then major=0
if major then begin
	n = strlen(geo_ver)
	if inumeric(strmid(geo_ver,n-1,1)) eq 0 then begin
		geo_ver = strmid(geo_ver,0,n-1)
	endif
endif

return, geo_ver
end
