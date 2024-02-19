pro spectrum_preview, file, preview=preview

; Provide preview spectrum and data for 'file_requester'

COMPILE_OPT STRICTARR
define_devices

preview = 0L
if n_elements(file) lt 1 then return
;if strlowcase(extract_extension(file)) ne 'spec' then return
print,'Spectrum preview: ',file

p = read_spec( file, /header)
if ptr_valid(p) eq 0 then return

;------------------------------------------------------------
; Details string array:

	list = spectrum_details( p)

;------------------------------------------------------------
; Preview spectrum:

	if ptr_valid((*p).data) eq 0 then goto, just_details
	
	d = *(*p).data
	siz = (*p).size
	if siz lt 5 then goto, just_details
	d[0:2] = 0
	d[siz-3:siz-1] = 0
	q = where(d ne 0,nq)
	if nq lt 2 then goto, just_details
	n = max(q)
	ymax = max( d)
	x = indgen(n)
	e = (*p).cal.poly[1]*x + (*p).cal.poly[0]
	y = d[0:n-1]

	preview = {details:list, spectrum:{x:e,y:d[0:n-1]}}
	return
	
just_details:
	preview = {details:list}
	return

end
