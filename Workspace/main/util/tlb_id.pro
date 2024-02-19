function tlb_id, id

parent = id
top = 0L
repeat begin
	if widget_info( parent, /valid) eq 0 then return, top
	top = parent
	parent = widget_info( parent, /parent)
endrep until parent eq 0

return, top
end
