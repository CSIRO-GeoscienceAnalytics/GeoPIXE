function read_da_as_spec, file
;
;	Read a DA file 'file' like a spectra file
;
;	Return 'p', a pointer (or pointer array) pointing to
;	spectrum structs, containing the spectrum details
;	and data.
;

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Read_DA_as_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_da
	endif
endif
  
p = 0
matrix = read_da(file, error=error)
if error eq 1 then goto, bad_da

n = matrix.n_el
np = matrix.n_pure
p = ptrarr(n+np)

null_spectrum = define(/spectrum)

for i=0L,n-1 do begin
	spec = null_spectrum
	spec.file = matrix.file
	spec.source = matrix.label
	d = matrix.matrix[*,i]
	scale = 0.1
	repeat scale*= 10. until max( scale*d) gt 10000.
	spec.label = matrix.el[i] + ' * '+str_tidy(scale,places=0)
	spec.cal.order = 1
	spec.cal.poly[0] = matrix.cal.b
	spec.cal.poly[1] = matrix.cal.a
	spec.cal.units = 'keV'
	spec.charge = matrix.charge
	spec.size = matrix.size
	spec.ecompress = matrix.ecompress
	spec.data = ptr_new( scale*d, /no_copy)

	p[i] = ptr_new(spec, /no_copy)
endfor

if np gt 0 then begin
	for i=0L,n-1 do begin
		spec = null_spectrum
		spec.source = matrix.file
		d = matrix.pure[*,i]
		scale = 0.1
		repeat scale*= 10. until max( scale*d) gt 10000.
		spec.label = 'Pure ' + matrix.el[i] + ' * '+str_tidy(scale,places=0)
		spec.cal.order = 1
		spec.cal.poly[0] = matrix.cal.b
		spec.cal.poly[1] = matrix.cal.a
		spec.cal.units = 'keV'
		spec.ecompress = matrix.ecompress
		spec.charge = matrix.charge
		spec.size = matrix.size
		spec.data = ptr_new( scale*d, /no_copy)

		p[i+n] = ptr_new(spec, /no_copy)
	endfor
endif

finish:
	free_DA, matrix
	return, p

bad_da:
	print,'read_da_as_spec: bad read of DA file'
	goto, error

usage:
	print,'read_da_as_spec: Usage: p = read_da_as_spec(da-file)'
	print,'		where "p" is pointer to spectrum struct(s)'
	print,'		and "file" is the name of the input DA file'
	goto, error

error:
	p = 0
	goto, finish
end