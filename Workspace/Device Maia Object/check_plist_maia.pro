pro check_plist_maia, plist

; Need to keep the FC0  and FC1 as the first two to keep the 'index'
; correct.

COMPILE_OPT STRICTARR
ptr_mode = 0
if n_elements( plist) eq 0 then plist=''
if size( plist, /tname) eq 'POINTER' then begin
	if ptr_valid(plist) eq 0 then return
	if ptr_good(plist) eq 0 then *plist='none'
	list = *plist
	ptr_mode = 1
endif else list = plist

	list = ['Maia:scaler.FC0', 'Maia:scaler.FC1', 'Maia:dwell.time', list]
	
	q = where( strmid(list,0,4) eq 'none', nq)
	if nq eq 0 then begin
		list = [list, 'none']
	endif
	q = where( strmid(list,0,11) eq 'Maia:scaler', nq)
	if nq gt 2 then begin
		list[q[2:*]] = ''
	endif
	q = where( list eq 'Maia:dwell.time', nq)
	if nq gt 1 then begin
		list[q[1:*]] = ''
	endif
	q = where( (list ne ''), nq)
	if nq ne 0 then begin
		list = list[q]
	endif
	if ptr_mode then *plist=list else plist=list
	return
end
