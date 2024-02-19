function find_id, id, uname=uname

; Search down a widget hierarchy for 'uname', startind at 'id'.

if n_elements(uname) eq 0 then return, id

parent = id
top = 0L
repeat begin
	if widget_info( parent, /valid) eq 0 then return, 0L
	top = parent
	name = widget_info( top, /uname)
	if name eq uname then return, top
	parent = widget_info( parent, /parent)
endrep until parent eq 0

return, 0L
end
