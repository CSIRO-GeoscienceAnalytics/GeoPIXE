function find_leaf, root, uvalue

; Search a widget tree 'root' for 'uvalue'

if widget_info( root, /valid_id) eq 0 then return, 0L
uv = uvalue
nuv = strlen(uv)
if nuv lt 1 then return, 0L

s = strmid( uv, nuv-1,1)
ps = path_sep()
if s ne ps then uv = uv+ps
match = file_lowcase(uv)

widget_control, root, get_uvalue=uv2
uv2 = file_lowcase(uv2)
if uv2 eq match then begin
	widget_control, root, /set_tree_expanded
	return, root
endif

w = widget_info(root, /all_children)
if w[0] eq 0 then begin
	if locate(uv2,match) eq 0 then begin		; uvalue starts with root
		grow_tree, root, uv2, uname='tree'		; build tree below root
		w = widget_info(root, /all_children)
		widget_control, root, /set_tree_expanded
	endif
endif
if w[0] eq 0 then return, 0L

for i=0L,n_elements(w)-1 do begin
	widget_control, w[i], get_uvalue=uv2
	uv2 = file_lowcase(uv2)
	if uv2 eq match then begin
		widget_control, w[i], /set_tree_expanded
		return, w[i]
	endif
	
;	This compare needs to become a bit smarter to be able to
;	look for a member of a 'virtual' dir range.

	if locate(uv2,match) eq 0 then begin		; uvalue starts with root
		widget_control, w[i], /set_tree_expanded
		r = find_leaf( w[i], match)
		if r ne 0 then return, r
	endif
endfor

return, 0L
end
