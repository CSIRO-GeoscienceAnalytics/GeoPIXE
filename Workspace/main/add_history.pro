pro add_history, hist, i, s, clear=clear

; Add a history record for element 'i'

if ptr_valid( hist) eq 0 then return
if n_elements(clear) lt 1 then clear=0

if clear then begin
	if ptr_valid( (*hist)[i]) then ptr_free, (*hist)[i]
	if n_elements(s) lt 1 then return
endif

if ptr_valid( (*hist)[i] ) eq 0 then begin
	(*hist)[i] = ptr_new(s)
endif else begin
	s1 = *(*hist)[i]
	ptr_free, (*hist)[i]
	(*hist)[i] = ptr_new( [ s1, s] )
endelse

return
end

