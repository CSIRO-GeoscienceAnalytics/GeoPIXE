pro make_back, a2, a3, a4, a5, file

if n_params(0) lt 4 then goto, usage

COMPILE_OPT STRICTARR
common c_make_back_last, last

if n_elements(file) lt 1 then begin
	if n_elements(last) ge 1 then begin
		path = extract_path(last)
	endif else begin
		path = 'g:\nmp'
	endelse

	file = file_requester(/write, path=path, $
		title='Select spec file to write', filter='*.spec', $
		/fix_filter)
endif else begin
	path = extract_path( file)
	if path eq file then begin
		file = file_requester(/write, path=path, file=file, $
			title='Select spec file to write', filter='*.spec', $
			/fix_filter)
	endif
endelse

if strlen(file) lt 1 then return
last = file

p = read_spec(file)

n = n_elements(p)
if n gt 1 then begin
	for i=0L,n-1 do begin
		free_spectrum, p[i]
	endfor
	p = p[0]
endif

a = (*p[0]).cal.poly[1]
b = (*p[0]).cal.poly[0]

a1 = 338.

size = (*p[0]).size
x = fltarr(size)

e = a*x+b
u = e^(-3)

y = exp(-a1*u) * a2*exp(-a3*e) + a4*exp(-a5*e)

null_spectrum = define(/spectrum)

spec = null_spectrum
spec.source = 'make_back pro'
spec.label = 'A: '+string(a1)+','+string(a2)+','+string(a3)+','+string(a4)+','+string(a5)
spec.cal.order = 1
spec.cal.poly[0] = b
spec.cal.poly[1] = a
spec.cal.units = 'keV'
spec.charge = 0.0
spec.size = size
spec.data = ptr_new( y, /no_copy)

p = [p, ptr_new(spec, /no_copy)]

write_spec, p, file

pp = ptr_new( p, /no_copy)
free_spectra, pp
return

usage:
	print,'Usage: make_back, a2,a3,a4,a5, file'
	print,'		where "p" is pointer to spectrum struct(s)'
	print,'		and "a2,a3" are amp, decay (1/keV) for SEB'
	print,'		and "a4,a5" are amp, decay (1/keV) for Compton'
	print,'		filename "file" is optional (will prompt)'
	return
end