function geolib_cdecl

COMPILE_OPT STRICTARR
common c_library_1, postfix, cdecl
common c_library_2, prefix
if n_elements(cdecl) lt 1 then init_library

return, cdecl
end
