function dir_up, pathi

; move up one level in a directory path 'path'

n_path = n_elements(pathi)
if n_path gt 1 then begin
	f = ''
	for i=0,n_path-1 do begin
		f = [f,dir_up(pathi[i])]
	endfor
	return, f[1:*]
endif

path = extract_path(pathi)
n = strlen(path)

; Locate final path separator ...

i = locate_last( path_sep(), path)
if i eq (n-1) then path = strmid(path,0,i)

; Locate previous separator ...

i = locate_last( path_sep(), path)
if i gt 0 then path = strmid(path,0,i+1)

return, path
end