function strip_path, filei, keep=keep

; Strip out the path (before last "/","]","\",":")
; if /keep and the file is a path only, keep the last part of the
; path name.

if n_elements(keep) eq 0 then keep=0
if n_elements(filei) lt 1 then return, ''

if keep then return, file_basename(filei)

file2 = strarr(n_elements(filei))

for j=0L,n_elements(filei)-1 do begin
	file = strtrim(filei[j],2)
	if keep then begin
		nl = strlen(file)
		if nl ge 2 then begin
			if strmid(file,nl-1,1) eq path_sep() then file=strmid(file,0,nl-1)
		endif
	endif
	found = 0
	nl = strlen(file)
;	if nl lt 1 then return, ''
	n = -1

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
	if (found eq 0) or (n eq -1) then begin
		file2[j] = file
	endif else begin
		file2[j] = strmid( file, n+1, nl-n-1)
	ENDelse
endfor

if n_elements(file2) eq 1 then file2=file2[0]
return, file2
end
