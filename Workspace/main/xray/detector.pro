pro detector, type

common c_detector_1, escape_energy1, escape_energy2, detector_type

if n_elements(n_periodic) lt 1 then t = atomic_number('He')

if (type eq 'Si') or (type eq 'Si(Li)') then begin
	detector_type = 'Si'
	escape_energy1 = line_e( atomic_number('Si'), 'Ka_')
	escape_energy2 = line_e( atomic_number('Si'), 'Kb_')

endif else if (type eq 'Ge') then begin
	detector_type = 'Ge'
	escape_energy1 = line_e( atomic_number('Ge'), 'Ka1')
	escape_energy2 = line_e( atomic_number('Ge'), 'Kb1')

endif else begin
	print,'detector: illegal detector type = ',type
endelse

return
end

