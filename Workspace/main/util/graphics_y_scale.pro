function graphics_y_scale

	base = widget_base(/row)
	button = widget_button( base, value='Test')
	g = widget_info( button, /geometry)
	ysize = g.scr_ysize
	widget_control, base, /destroy

	ys = ysize / 22.
	return, ys
end
