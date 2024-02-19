function strip_file_ext, file, double=double, triple=triple
;
; remove extension (after ".")
; /double	remove double extension with two "."
; /triple	remove triple extension with three "."

COMPILE_OPT STRICTARR

if n_elements(double) lt 1 then double=0
if n_elements(triple) lt 1 then triple=0
nf = n_elements(file)
if nf lt 1 then return, ''
f = strarr(nf)

;if nf gt 1 then begin
;	for i=0,nf-1 do begin
;		f[i] = strip_file_ext( file[i], double=double, triple=triple)
;	endfor
;	return, f
;endif

i = locate_last( '.', file)					; last '.' before extension
if double or triple then begin
	i2 = locate_last('.', strmid2(file,0,i))
	q = where( i2 ge 0, nq)
	if nq ge 1 then i[q]=i2[q]				; 2nd last '.' for /double
endif
if triple then begin
	i2 = locate_last('.', strmid2(file,0,i))
	q = where( i2 ge 0, nq)
	if nq ge 1 then i[q]=i2[q]				; 3rd last '.' for /triple
endif
j = locate_last( path_sep(), file)			; last separator

q = where( i lt j)							; ignore when no valid extension
if q[0] ne -1 then f[q] = file[q]			; just pass 'file' to 'f' in this case

q = where(i ge j) 							; valid extensions
if q[0] ne -1 then begin
	q2 = where(i lt 0)						; no "." found at all

	if q2[0] ne -1 then begin
		n = strlen(file[q[q2]])				; set 'i' to length in this case
		i[q[q2]] = n
	endif

	f[q] = extract( file[q], 0, i[q]-1)
endif

if n_elements(f) eq 1 then f=f[0]			; fix irritating length=1 array bug
return, f
end
