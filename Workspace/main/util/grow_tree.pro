pro grow_tree, root, path, level, expanded=expanded, uname=uname

; Populate a directory widget_tree from 'path'

COMPILE_OPT STRICTARR
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
		warning,'grow_tree',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(expanded) lt 1 then expanded=0
if n_elements(uname) lt 1 then uname='tree'
if n_elements(level) lt 1 then level=0
if n_elements(path) lt 1 then return

; If #dirs in 'path' is large, then group dirs into virtual dir ranges.
; Do this here in find_file2 ("path/first ... last/*").
; An existing "..." in a path indicates a 'virtual dir' grouping.
; If path has "..." then dirs needs to expand to all dirs in the range.

dirs = find_file2( path + '*', /test_directory, /mark_directory, /virtual)
if dirs[0] eq '' then return

; Does this leaf have children (opened previously)?
; If so, take note of names so we don't open more with same names.

id = widget_info( root, /all_children)
if widget_info(id[0],/valid_id) eq 0 then begin
	val = '%% no children %%'
endif else begin
	nv = n_elements(id)
	val = strarr(nv)
	s = ''
	for i=0L,nv-1 do begin
		widget_control, id[i], get_uvalue=s
		val[i] = s
	endfor
endelse

; Create new leaf nodes for new children, and open the root
; node if new leaves are added.

for i=0L,n_elements(dirs)-1 do begin
	q = where(dirs[i] eq val, nq)
	if nq eq 0 then begin
		leaf = widget_tree( root, value=file_basename(dirs[i]), uvalue=dirs[i], $
						/folder, expanded=0, uname=uname)
		widget_control, root, set_tree_expanded=expanded
	endif
endfor

return
end
