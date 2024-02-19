pro set_table_select, table, ii, ii2, rows=n, columns=nc

;	Set the 'set_table_select' to 'i' (of 'n' total rows)
;	But show row 0 if possible.

COMPILE_OPT STRICTARR

	if widget_info( table, /valid) eq 0 then return
	if n eq 0 then return
	i = clip(ii, 0, n-1)
	i2 = clip(ii2, 0, n-1)

	view = widget_info( table, /table_view)
	geom = widget_info( table, /geometry)
	row_heights = widget_info( table, /row_heights)
	select = widget_info( table, /table_select)

	nvis = (geom.scr_ysize / row_heights[0]) - 2

	if (i lt view[1]) then begin
		view[1] = view[1] - (view[1]-i)
	endif
	if (i2 gt view[1]+nvis-1) then begin
		view[1] = view[1] + (i2 - fix(view[1]+nvis-1))
	endif

	widget_control, table, set_table_select=[-1,i,nc-1,i2]
	widget_control, table, set_table_view=view
	return
end
