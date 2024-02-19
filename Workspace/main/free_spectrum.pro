pro free_spectrum, p

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
		warning,'Free_Spectrum',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(p) lt 1 then return
if ptr_good(p, /struct) eq 0 then return
if n_elements(*p) lt 1 then return

if ptr_valid( (*p).matrix.mdl) then begin
	ptr_free, (*p).matrix.mdl
endif
if ptr_valid( (*p).plist) then begin
	ptr_free, (*p).plist
endif
if ptr_valid( (*p).px_coords) then begin
	ptr_free, (*p).px_coords
endif
if ptr_valid( (*p).pactive) then begin
	ptr_free, (*p).pactive
endif
if ptr_valid( (*p).data) then begin
	ptr_free, (*p).data
endif
if ptr_valid( (*p).error) then begin
	ptr_free, (*p).error
endif
if ptr_valid( (*p).mdl) then begin
	ptr_free, (*p).mdl
endif

if ptr_valid( (*p).pileup_loss_det) then begin
	ptr_free, (*p).pileup_loss_det
endif
if ptr_valid( (*p).deadtime_det) then begin
	ptr_free, (*p).deadtime_det
endif

if (*p).n_fit ge 1 then begin
	for i=0L,n_elements((*p).n_fit)-1 do begin
		if ptr_valid((*p).fit[i]) then begin
			free_spectrum, (*p).fit[i]
			ptr_free, (*p).fit[i]
		endif
	endfor
endif

if obj_valid( (*p).DevObj) then free_device_objects, (*p).DevObj
ptr_free, p

return
end
