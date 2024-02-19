function geolib_name, name

COMPILE_OPT STRICTARR
common c_library_1, postfix, cdecl
common c_library_2, prefix
if n_elements(prefix) lt 1 then init_library

return, prefix + name + postfix
end
