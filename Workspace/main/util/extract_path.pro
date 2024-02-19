function extract_path, file

; Extract the path (before last "/","]","\",":")
; Include last "\,/,:,]"

if n_elements(file) lt 1 then return, ''
n = n_elements(file)
if n gt 1 then begin
	s = strarr(n)
	for i=0,n-1 do begin
		s[i] = extract_path(file[i])
	endfor
	return, s
endif

found = 0
if strlen(file) lt 1 then return, ''
n = 0

k = locate(".",file)
if k eq 0 then begin
	found = 1
	n=[n,k]
endif

k = locate("..",file)
if k eq 0 then begin
	found = 1
	n=[n,k]
endif

k = locate_last("/",file)
if k ge 0 then begin
	found = 1
	n=[n,k]
endif

k = locate_last("]",file)
if k ge 0 then begin
	found = 1
	n=[n,k]
endif

k = locate_last("\",file)
if k ge 0 then begin
	found = 1
	n=[n,k]
endif

k = locate_last(":",file)
if k ge 0 then begin
	found = 1
	n=[n,k]
endif

n = max(n)
if (found eq 0) then return, ''

path = strmid( file, 0, n+1)
path = fix_path(path)

return, path
end
