function read_filters, F, error=error

;	Read filter definitions from 'F'
;	return a pointer to the array of filter structs.

;	Filters are stored with thick in g/cm^2 units always.
;	The /microns flag just says that the filter WAS specified in microns.
;	But the thick is still stored in g/cm^2. For microns, convert thick
;	back to microns using density (t[microns] = 10 * t[g/cm^2] / density)

error = 1
if n_params() lt 1 then return, 0
if lenchr(F) lt 1 then return, 0
error = 0

	valid = [-1,-2,-3]

	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	version = 0L
	readu,1, version
	q = where( version eq valid)
	if q[0] eq -1 then return, 0

	max_n = 100
	n = 0L
	readu,1, n
	if (n le 0) or (n gt max_n) then goto, bad_io

	if version le -3 then begin
		absorber = define(/filter)
		filters = replicate( absorber, n)
		readu,1, filters
	endif else if version le -2 then begin
		absorber = define(/old_filter2)
		rfilters = replicate( absorber, n)
		readu,1, rfilters
	endif else begin
		absorber = define(/old_filter1)
		rfilters = replicate( absorber, n)
		readu,1, rfilters
	endelse
	close,1

	if version ge -2 then begin
		filter = define(/filter)
		filters = replicate( filter, n)

		if version le -2 then begin
			filters.microns = rfilters.microns
			filters.density = rfilters.density
			filters.formula = rfilters.formula
			filters.weight = rfilters.weight
		endif

		filters.n = rfilters.n
		filters.z = rfilters.z
		filters.f = rfilters.f
		filters.thick = rfilters.thick
		filters.pinhole = rfilters.pinhole
		filters.pinratio = rfilters.pinratio
		filters.name = rfilters.name

		for i=0L,n-1 do begin
			s = ''
			for j=0L,filters[i].n-1 do begin
				s = s + element_name(filters[i].z[j])
				if abs(filters[i].f[j]-1.0) gt 0.005 then begin
					s = s + string(filters[i].f[j])
				endif
			endfor
			filters[i].formula = strcompress(s,/remove_all)
		endfor
	endif

	p = ptr_new( filters, /no_copy)

return, p

bad_io:
print,'Read_Filters: bad Filters I/O'
error = 1
return, 0

end

