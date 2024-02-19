function line_energy, z, line
;
;	More general version of 'line_e'
;	Treats Ka_ same as Ka, use Ka1 if no Ka_ (and vica versa)
;	same for L lines.
;
common c_line_list_1, element_start, line_list, n_line_list
common c_atomic_1, periodic_table, n_periodic
common c_detector_1, escape_energy1, escape_energy2, detector_type

if n_elements(n_periodic) lt 1 then t = atomic_number('He')
if n_elements(n_line_list) lt 1 then n_line_list = 0
if n_elements(detector_type) lt 1 then detector, 'Si'
if n_line_list lt 1 then build_line_list, 0.0001

if (z[0] le 0) or (z[0] ge n_periodic) then return, 0.0

set_separators, [',',' ','+']
chop_string, line, sub, ns
et = 0.0
i = 0
j = 1
while (i lt ns) do begin
	esc = 0.0
	if j lt ns then begin
		if (sub[j] eq 'esc') or (sub[j] eq 'esc1') then begin
			esc = escape_energy1
			j = j+1
		endif else if sub[j] eq 'esc2' then begin
			esc = escape_energy2
			j = j+1
		endif else if (sub[j] eq 'Si-esc')   then begin
			detector, 'Si'
			esc = escape_energy1
			j = j+1
		endif else if (sub[j] eq 'Ge-esc') or (sub[j] eq 'Ge-esc1')  then begin
			detector, 'Ge'
			esc = escape_energy1
			j = j+1
		endif else if (sub[j] eq 'Ge-esc2')  then begin
			detector, 'Ge'
			esc = escape_energy2
			j = j+1
		endif
	endif
	e = line_e( z[0], sub[i])
	if e lt 0.001 then begin
		if sub[i] eq 'Ka' then begin
			e = line_e( z[0], 'Ka_')
			if e lt 0.001 then begin
				e = line_e( z[0], "Ka1")
			endif
		endif else if sub[i] eq 'Ka_' then begin
			e = line_e( z[0], 'Ka1')
		endif else if sub[i] eq 'Ka1' then begin
			e = line_e( z[0], 'Ka_')
		endif
	endif
	if e lt 0.001 then begin
		if sub[i] eq 'La' then begin
			e = line_e( z[0], 'La_')
			if e lt 0.001 then begin
				e = line_e( z[0], "La1")
			endif
		endif else if sub[i] eq 'La_' then begin
			e = line_e( z[0], 'La1')
		endif else if sub[i] eq 'La1' then begin
			e = line_e( z[0], 'La_')
		endif
	endif
	if e gt 0.001 then et = et + (e-esc > 0.0)
	i = j
	j = i+1
endwhile

return, et
end
