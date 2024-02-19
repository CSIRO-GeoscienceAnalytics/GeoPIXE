pro add_spectrum_history, p, hist, clear=clear

; Add a history record for spec pointer 'p'

common c_null_spec_1, max_history, max_cal,  max_fit

if ptr_valid( p) eq 0 then return
if n_elements(clear) lt 1 then clear=0

if clear then begin
	(*p).n_history = 0
	if n_elements(hist) lt 1 then return
endif

if (*p).n_history lt max_history then begin
	(*p).history[(*p).n_history] = hist
	(*p).n_history = (*p).n_history+1
endif

return
end

