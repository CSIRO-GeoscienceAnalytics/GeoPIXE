pro free_images, p

; Free images struct pointed by 'p'

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
		warning,'Free_images_Bug',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if ptr_valid(p) eq 0 then return

if ptr_valid( (*p).matrix.mdl) then ptr_free, (*p).matrix.mdl
if ptr_valid( (*p).el) then ptr_free, (*p).el

if ptr_valid( (*p).options) then ptr_free, (*p).options
if ptr_valid( (*p).escale) then ptr_free, (*p).escale

if ptr_valid( (*p).image) then ptr_free, (*p).image
if ptr_valid( (*p).error) then ptr_free, (*p).error
if ptr_valid( (*p).flux) then ptr_free, (*p).flux
if ptr_valid( (*p).preview) then ptr_free, (*p).preview

if ptr_valid( (*p).px_coords) then ptr_free, (*p).px_coords
if ptr_valid( (*p).py_coords) then ptr_free, (*p).py_coords
if tag_present('PZ_COORDS',*p) then if ptr_valid( (*p).pz_coords) then ptr_free, (*p).pz_coords
if tag_present('PZ_LABEL',*p) then if ptr_valid( (*p).pz_label) then ptr_free, (*p).pz_label

if ptr_valid( (*p).temp.flux_map) then ptr_free, (*p).temp.flux_map
if ptr_valid( (*p).temp.charge_map) then ptr_free, (*p).temp.charge_map

if ptr_valid( (*p).undo.image) then ptr_free, (*p).undo.image
if ptr_valid( (*p).undo.error) then ptr_free, (*p).undo.error
if ptr_valid( (*p).undo.history) then ptr_free, (*p).undo.history
(*p).undo.OK = 0

if ptr_valid( (*p).pactive) then ptr_free, (*p).pactive
if ptr_valid( (*p).pcal) then ptr_free, (*p).pcal
if ptr_valid( (*p).plist) then ptr_free, (*p).plist

if tag_present('HIST',*p) then if ptr_valid( (*p).hist) then ptr_free, (*p).hist
if tag_present('RAW_FLUX',*p) then if ptr_valid( (*p).raw_flux) then ptr_free, (*p).raw_flux
if tag_present('DEAD_FRACTION',*p) then if ptr_valid( (*p).dead_fraction) then ptr_free, (*p).dead_fraction
if tag_present('DWELL_MAP',*p) then if ptr_valid( (*p).dwell_map) then ptr_free, (*p).dwell_map
if tag_present('PILEUP_MAP',*p) then if ptr_valid( (*p).pileup_map) then ptr_free, (*p).pileup_map
if tag_present('COUNT_RATE_MAP',*p) then if ptr_valid( (*p).count_rate_map) then ptr_free, (*p).count_rate_map

if tag_present('PHASE',*p) then if ptr_valid( (*p).phase) then ptr_free, (*p).phase
if tag_present('PDA',*p) then if ptr_valid( (*p).PDA) then free_DA, (*p).pda

if ptr_valid( (*p).history) then begin
	for i=0L, n_elements( *(*p).history )-1 do begin
		if ptr_valid( (*(*p).history)[i] ) then begin
			ptr_free, (*(*p).history)[i]
		endif
	endfor
	ptr_free, (*p).history
endif

if obj_valid( (*p).DevObj) then free_device_objects, (*p).DevObj
ptr_free, p
return
end
