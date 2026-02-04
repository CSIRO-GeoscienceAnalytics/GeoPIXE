;-----------------------------------------------------------------

; BUTTON_EVENTS Callback Procedure.
;
;   {WIDGET_DRAW, ID:0L, TOP:0L, HANDLER:0L, TYPE: 0, X:0, Y:0,
;       PRESS:0B, RELEASE:0B, CLICKS:0}
;
;   TYPE returns a value that describes the type of draw widget
;       interaction that generated an event: 0 - Button Press, 1 -
;       Button Release, 2 - Motion, 3 - Viewport Moved, 4 -
;       Visibility Changed (Expose)
;   PRESS returns a bitmask of which button was pressed:
;		1 left, 2 centre, 4 right.
;--------------------------------------------------------------------

pro OnButton_image, Event

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
		warning,'OnButton_image',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif
common image_region_window_1, region_window

;print,'event: type=',event.type, '  press=',event.press
if event.type gt 2 then begin
	print,'OnButton_image: Found illegal button "type" =',event.type
	return
endif
if event.press gt 4 then begin
	print,'OnButton_image: Found illegal button "press" =',event.press
	return
endif
possible = ['DOWN','UP','MOTION','SCROLL']
button = ['none','LEFT','MIDDLE','VIEW','RIGHT']

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

pm = (*pstate).pmark[ (*pstate).analyze_mode ]						; shape struct
if ptr_valid(pm) eq 0 then goto, finish
p = (*pm)[(*pstate).analyze_type[(*pstate).analyze_mode]]			; control points
if ptr_valid(p) eq 0 then goto, finish
pimg = (*pstate).p
if ptr_valid(pimg) eq 0 then goto, finish

mag_xy = 10
magnify = 10

case possible[ event.type] of
	'DOWN': begin
		(*pstate).left_button = 0
		(*pstate).right_button = 0
		(*pstate).middle_button = 0
		case button[event.press] of
			'LEFT': begin
;				print,'OnButton_image: left mouse button press ...'
				(*pstate).left_button = 1
				end
			'RIGHT': begin
;				print,'OnButton_image: right mouse button press ...'
				(*pstate).right_button = 1
				end
			'MIDDLE': begin
;				print,'OnButton_image: right mouse button press ...'
				(*pstate).middle_button = 1
				end
			else:
		endcase

		(*pstate).id = -1
		pixel_to_xy, pstate, event.x,event.y, x,y					; coords on map
		pixel_to_xy, pstate, event.x,event.y, xf,yf, /fractional
		x = xf
		y = yf
		(*pstate).oldx = x
		(*pstate).oldy = y
		(*pstate).movex = x
		(*pstate).movey = y

		if (*pstate).right_button then begin
			if near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom) eq -1 then begin
				if ptr_valid((*pstate).qc) then begin
					(*pstate).corr_on = 0
					draw_images, pstate
					goto, finish
				endif
			endif
		endif
		if (*pstate).middle_button then begin
			b = make_tvb( pstate, (*pstate).image, /nozoom)
			b2 = congrid( b[x-10:x+9,y-10:y+9], 100,100)
			wset, (*pstate).pix2
			tv, b2, 0,0
			plots,[0,99,99,0,0],[0,0,99,99,0],/device,color=spec_colour('white')
			xyouts,3,3,'x5',/device,color=spec_colour('white')
			xy_to_pixel, pstate, x,y, px,py
			wset, (*pstate).wid2
			device, copy=[0,0,100,100, px+10,py+10, (*pstate).pix2]
			goto, motion_on
		endif

;		print,'Down: analyze_type=',(*pstate).analyze_type[(*pstate).analyze_mode],' present=',(*p).present
;		print,'zoomed mouse x,y=',x,y

		case (*pstate).analyze_type[(*pstate).analyze_mode] of
			0: begin																; distance
				; 0,1 ends

				if (*pstate).right_button then goto, finish
				wset, (*pstate).wid2
				clear_mark, pstate
				(*pstate).id = 1
				(*p).x = [x,x+1]
				(*p).y = [y,y+1]
				plot_mark, pstate
				end
			1: begin																; box
				; 0,1,2,3 corners, 4 centre, 5 rotate handle

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 2
					(*p).x = [x,x+1,x+1,x,  x,x]
					(*p).y = [y,y,y-1,y-1,  y,y]
					(*p).theta = 0.0
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			2: begin																; circle
				; 0,1 main diameter, 2 centre

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 1
					(*p).x = [x,x+1,  x]
					(*p).y = [y,y-1,  y]
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			3: begin																; curve traverse
				; 0-8 traverse curve, 9,10 width/curvature, (4 centre, with right MSB)

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).right_button then begin
						q = where( (*pstate).id eq [4,9,10])
						if q[0] eq -1 then (*pstate).id=-1
					endif
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 8
					(*p).x = [x,x+1,x+2,x+3,x+4,x+5,x+6,x+7,x+8,x+4,x+4]-8
					(*p).y = [y,y,y,y,y,y,y,y,y,y+10,y-10]
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			4: begin																; line traverse
				; 0,1 traverse line, 2,3 width, 4 centre

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).right_button then begin
						q = where( (*pstate).id eq [2,3])
						if q[0] eq -1 then (*pstate).id=-1
					endif
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 1
					(*p).x = [x,x+2,x+1,x+1, x+1]
					(*p).y = [y,y,y+10,y-10,  y]
					(*p).theta = 0.0
					(*p).shear = 0.0
					(*p).curvature = 0.0
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			5: begin																; ellipse
				; 0,1 main diameter, 2,3 minor diameter, 4 centre

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 1
					(*p).x = [x,x+2,x+1,x+1, x+1]
					(*p).y = [y,y,y+20,y-20,  y]
					(*p).theta = 0.0
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			6: begin																; spline 10
				; 0 centre, 1-10 control points, 100 drag
				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 100
					circle_diam, x-3.,y-3., x,y, xs,ys, n=10
					(*p).x[1:*] = xs[0:9]
					(*p).y[1:*] = ys[0:9]
					(*p).x[0] = mean(xs)
					(*p).y[0] = mean(ys)
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			7: begin																; spline 32
				; 0 centre, 1-32 control points, 100 drag

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 100
					circle_diam, x-3.,y-3., x,y, xs,ys, n=32
					(*p).x[1:*] = xs[0:31]
					(*p).y[1:*] = ys[0:31]
					(*p).x[0] = mean(xs)
					(*p).y[0] = mean(ys)
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			8: begin																; project X
				; 0,1 projectX line, 2,3 width, 4 centre
				; like Traverse, but must constrain to pure X line

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).right_button then begin
						q = where( (*pstate).id eq [2,3])
						if q[0] eq -1 then (*pstate).id=-1
					endif
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 1
					(*p).x = [x,x+2,x+1,x+1, x+1]
					(*p).y = [y,y,y+10,y-10,  y]
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			9: begin																; project Y
				; 0,1 projectX line, 2,3 width, 4 centre
				; like Traverse, but must constrain to pure X line

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).right_button then begin
						q = where( (*pstate).id eq [2,3])
						if q[0] eq -1 then (*pstate).id=-1
					endif
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 1
					(*p).x = [x,x,x+10,x-10,  x]
					(*p).y = [y,y+2,y+1,y+1, y+1]
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			10: begin																; spline 100
				; 0 centre, 1-100 control points, 200 drag

				if (*p).present then begin
					(*pstate).id = near_xy( xf,yf, (*p).x,(*p).y, zoom=(*pstate).zoom)
					if (*pstate).id eq -1 then goto, finish
				endif else if (*pstate).right_button then begin
					goto, finish
				endif else begin
					(*pstate).id = 200
					circle_diam, x-3.,y-3., x,y, xs,ys, n=100
					(*p).x[1:*] = xs[0:99]
					(*p).y[1:*] = ys[0:99]
					(*p).x[0] = mean(xs)
					(*p).y[0] = mean(ys)
					wset, (*pstate).wid2
					plot_mark, pstate
				endelse
				end
			11: begin																; S pixel
				; 0 point

				if (*pstate).right_button then goto, finish
				wset, (*pstate).wid2
				clear_mark, pstate
				(*pstate).id = 0
				(*p).x = [x]
				(*p).y = [y]
				plot_mark, pstate
				end
			else:  goto, finish
		endcase
;		print,'(*pstate).id=',(*pstate).id

		if ptr_valid( (*pstate).px) then ptr_free, (*pstate).px
		if ptr_valid( (*pstate).py) then ptr_free, (*pstate).py
		(*pstate).px = ptr_new( (*p).x)
		(*pstate).py = ptr_new( (*p).y)
		if ((*pstate).analyze_type[(*pstate).analyze_mode] eq 1) or $
					((*pstate).analyze_type[(*pstate).analyze_mode] eq 4) or $
					((*pstate).analyze_type[(*pstate).analyze_mode] eq 5) then begin
			(*pstate).theta = (*p).theta
			if ((*pstate).analyze_type[(*pstate).analyze_mode] eq 4) then begin
				(*pstate).shear = (*p).shear
				(*pstate).curvature = (*p).curvature
			endif
		endif
		if (*pstate).analyze_mode eq 1 then begin
			(*pstate).analyze_mode = 0
			clear_mark, pstate, from=(*pstate).pix, to=(*pstate).pix2		; save shaded mode=0 area
			(*pstate).analyze_mode = 1
			wset, (*pstate).pix
			plot_mark, pstate, /include
		endif
motion_on:
;		print,'set motion events ON'
		widget_control, event.id, draw_motion_events=1						; enable motion events
		notify, 'image-analyze-clear', from=event.top
		end

	'MOTION': begin
		xc = 0
		yc = 0
		pixel_to_xy, pstate, event.x,event.y, x,y
		pixel_to_xy, pstate, event.x,event.y, xf,yf, /fractional
		x = xf
		y = yf
		
		if (*pstate).middle_button then begin
;			print,'Middle motion ...'
			xc = xf
			yc = yf
			xy_to_pixel, pstate, (*pstate).movex,(*pstate).movey, px,py
			wset, (*pstate).wid2
			device, copy=[px+8,py+8,106,106, px+8,py+8, (*pstate).pix]

			b = make_tvb( pstate, (*pstate).image, /nozoom)
			b2 = congrid( b[x-10:x+9,y-10:y+9], 100,100)
			wset, (*pstate).pix2
			tv, b2, 0,0
			plots,[0,99,99,0,0],[0,0,99,99,0],/device,color=spec_colour('white')
			xyouts,3,3,'x5',/device,color=spec_colour('white')
			xy_to_pixel, pstate, x,y, px,py
			wset, (*pstate).wid2
			device, copy=[0,0,100,100, px+10,py+10, (*pstate).pix2]
		endif
		(*pstate).movex = x
		(*pstate).movey = y
		if (*pstate).middle_button then goto, legend2

;		print,'Motion: id=',(*pstate).id
		if (*pstate).id lt 0 then begin
;			widget_control, event.id, draw_motion_events=0
			goto, finish
		endif

		wset, (*pstate).wid2
		case (*pstate).analyze_type[(*pstate).analyze_mode] of
			0: begin																; distance
				if (x ne (*p).x[(*pstate).id]) or (y ne (*p).y[(*pstate).id]) then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin
							(*p).x = [x,(*p).x[1]]
							(*p).y = [y,(*p).y[1]]
							end
						1: begin
							(*p).x = [(*p).x[0],x]
							(*p).y = [(*p).y[0],y]
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			1: begin																; box
				if (x ne (*p).x[(*pstate).id]) or (y ne (*p).y[(*pstate).id]) then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin
							rotatev, [(*p).x,x],[(*p).y,y], (*p).x[0],(*p).y[0], -(*p).theta, xr,yr
							xy_to_microns, pstate, abs(xr[1]-xr[0]), abs(yr[2]-yr[1]), sx,sy,sunits
							(*pstate).sizex = sx
							(*pstate).sizey = sy
							(*pstate).size_units = sunits
							vx = [ xr[6], xr[1], xr[2], xr[6]]
							vy = [ yr[6], yr[6], yr[2], yr[3]]
							vcx = mean(vx)
							vcy = mean(vy)
							xc = vcx
							yc = vcy
							x2 = [ vx,  vcx, (2*xr[1]+vcx)/3]
							y2 = [ vy,  vcy, (2*y+vcy)/3]
							rotatev, x2,y2, (*p).x[0],(*p).y[0], (*p).theta, xr,yr
							(*p).x = xr
							(*p).y = yr
							end
						1: begin
							rotatev, [(*p).x,x],[(*p).y,y], (*p).x[1],(*p).y[1], -(*p).theta, xr,yr
							xy_to_microns, pstate, abs(xr[1]-xr[0]), abs(yr[2]-yr[1]), sx,sy,sunits
							(*pstate).sizex = sx
							(*pstate).sizey = sy
							(*pstate).size_units = sunits
							vx = [ xr[0], xr[6], xr[6], xr[3]]
							vy = [ yr[6], yr[6], yr[2], yr[3]]
							vcx = mean(vx)
							vcy = mean(vy)
							xc = vcx
							yc = vcy
							x2 = [ vx,  vcx, (2*x+vcx)/3]
							y2 = [ vy,  vcy, (2*y+vcy)/3]
							rotatev, x2,y2, (*p).x[1],(*p).y[1], (*p).theta, xr,yr
							(*p).x = xr
							(*p).y = yr
							end
						2: begin
							rotatev, [(*p).x,x],[(*p).y,y], (*p).x[2],(*p).y[2], -(*p).theta, xr,yr
							xy_to_microns, pstate, abs(xr[1]-xr[0]), abs(yr[2]-yr[1]), sx,sy,sunits
							(*pstate).sizex = sx
							(*pstate).sizey = sy
							(*pstate).size_units = sunits
							vx = [ xr[0], xr[6], xr[6], xr[3]]
							vy = [ yr[0], yr[1], yr[6], yr[6]]
							vcx = mean(vx)
							vcy = mean(vy)
							xc = vcx
							yc = vcy
							x2 = [ vx,  vcx, (2*x+vcx)/3]
							y2 = [ vy,  vcy, (2*yr[1]+vcy)/3]
							rotatev, x2,y2, (*p).x[2],(*p).y[2], (*p).theta, xr,yr
							(*p).x = xr
							(*p).y = yr
							end
						3: begin
							rotatev, [(*p).x,x],[(*p).y,y], (*p).x[3],(*p).y[3], -(*p).theta, xr,yr
							xy_to_microns, pstate, abs(xr[1]-xr[0]), abs(yr[2]-yr[1]), sx,sy,sunits
							(*pstate).sizex = sx
							(*pstate).sizey = sy
							(*pstate).size_units = sunits
							vx = [ xr[6], xr[1], xr[2], xr[6]]
							vy = [ yr[0], yr[1], yr[6], yr[6]]
							vcx = mean(vx)
							vcy = mean(vy)
							xc = vcx
							yc = vcy
							x2 = [ vx,  vcx, (2*xr[1]+vcx)/3]
							y2 = [ vy,  vcy, (2*yr[1]+vcy)/3]
							rotatev, x2,y2, (*p).x[3],(*p).y[3], (*p).theta, xr,yr
							(*p).x = xr
							(*p).y = yr
							end
						4: begin									; centre
							dx = x - (*p).x[4]
							dy = y - (*p).y[4]
							(*p).x = [ (*p).x[0]+dx, (*p).x[1]+dx, (*p).x[2]+dx, (*p).x[3]+dx,  x, (*p).x[5]+dx]
							(*p).y = [ (*p).y[0]+dy, (*p).y[1]+dy, (*p).y[2]+dy, (*p).y[3]+dy,  y, (*p).y[5]+dy]
;							print,'Box: x=',(*p).x
;							print,'Box: y=',(*p).y
							xc = x
							yc = y
							end
						5: begin									; rotate
							theta = angle_lines( (*(*pstate).px)[4],(*(*pstate).py)[4], (*(*pstate).px)[5],(*(*pstate).py)[5], x,y)
							rotatev, (*(*pstate).px),(*(*pstate).py), (*(*pstate).px)[4],(*(*pstate).py)[4], theta, xr,yr
							(*p).x = xr
							(*p).y = yr
							(*p).theta = (*pstate).theta + theta
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			2: begin																; circle
				if (x ne (*p).x[(*pstate).id]) or (y ne (*p).y[(*pstate).id]) then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin
							vx = [ x, (*p).x[1] ]
							vy = [ y, (*p).y[1] ]
							xy_to_microns, pstate, abs(vx[1]-vx[0]), abs(vy[1]-vy[0]), sx,sy,sunits
							(*pstate).sizex = sqrt( sx*sx + sy*sy)
							(*pstate).size_units = sunits
							vcx = mean(vx)
							vcy = mean(vy)
							(*p).x = [ vx,  vcx]
							(*p).y = [ vy,  vcy]
							xc = vcx
							yc = vcy
							end
						1: begin
							vx = [ (*p).x[0], x ]
							vy = [ (*p).y[0], y ]
							xy_to_microns, pstate, abs(vx[1]-vx[0]), abs(vy[1]-vy[0]), sx,sy,sunits
							(*pstate).sizex = sqrt( sx*sx + sy*sy)
							(*pstate).size_units = sunits
							vcx = mean(vx)
							vcy = mean(vy)
							(*p).x = [ vx,  vcx]
							(*p).y = [ vy,  vcy]
							xc = vcx
							yc = vcy
							end
						2: begin
							dx = x - (*p).x[2]
							dy = y - (*p).y[2]
							(*p).x = [ (*p).x[0]+dx, (*p).x[1]+dx,  x]
							(*p).y = [ (*p).y[0]+dy, (*p).y[1]+dy,  y]
							xc = x
							yc = y
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			3: begin																; curve traverse
				if (x eq (*p).x[(*pstate).id]) and (y eq (*p).y[(*pstate).id]) then goto, legend
				if (*pstate).left_button then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin										; start
							r1 = sqrt( ((*(*pstate).px)[8]-(*pstate).oldx)^2 + ((*(*pstate).py)[8]-(*pstate).oldy)^2 )
							r2 = sqrt( ((*(*pstate).px)[8]-x)^2 + ((*(*pstate).py)[8]-y)^2 )
							theta1 = atan( (*pstate).oldy-(*(*pstate).py)[8], (*pstate).oldx-(*(*pstate).px)[8])
							theta2 = atan( y-(*(*pstate).py)[8], x-(*(*pstate).px)[8])

							x1 = *(*pstate).px - (*(*pstate).px)[8]
							y1 = *(*pstate).py - (*(*pstate).py)[8]
							rotatev, x1,y1, 0.0,0.0, -theta1, xr,yr
							xr = xr * (r2/r1)
							rotatev, xr,yr, 0.0,0.0, theta2, xt,yt
							(*p).x = xt + (*p).x[8]
							(*p).y = yt + (*p).y[8]
							end
						8: begin										; end
							r1 = sqrt( ((*(*pstate).px)[0]-(*pstate).oldx)^2 + ((*(*pstate).py)[0]-(*pstate).oldy)^2 )
							r2 = sqrt( ((*(*pstate).px)[0]-x)^2 + ((*(*pstate).py)[0]-y)^2 )
							theta1 = atan( (*pstate).oldy-(*(*pstate).py)[0], (*pstate).oldx-(*(*pstate).px)[0])
							theta2 = atan( y-(*(*pstate).py)[0], x-(*(*pstate).px)[0])

							x1 = *(*pstate).px - (*(*pstate).px)[0]
							y1 = *(*pstate).py - (*(*pstate).py)[0]
							rotatev, x1,y1, 0.0,0.0, -theta1, xr,yr
							xr = xr * (r2/r1)
							rotatev, xr,yr, 0.0,0.0, theta2, xt,yt
							(*p).x = xt + (*p).x[0]
							(*p).y = yt + (*p).y[0]
							end
						9: begin										; width 1
							theta1 = atan( (*(*pstate).py)[9]-(*(*pstate).py)[10], (*(*pstate).px)[9]-(*(*pstate).px)[10])
							tx = [*(*pstate).px, x]
							ty = [*(*pstate).py, y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -theta1, xr,yr
							w = xr[11] - xr[4]
							xr[9] = xr[11]
							xr[10] = xr[4] - w
							rotatev, xr,yr, (*(*pstate).px)[4],(*(*pstate).py)[4], theta1, tx,ty
							(*p).x = tx[0:10]
							(*p).y = ty[0:10]
							end
						10: begin										; width 2
							theta1 = atan( (*(*pstate).py)[10]-(*(*pstate).py)[9], (*(*pstate).px)[10]-(*(*pstate).px)[9])
							tx = [*(*pstate).px, x]
							ty = [*(*pstate).py, y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -theta1, xr,yr
							w = xr[11] - xr[4]
							xr[10] = xr[11]
							xr[9] = xr[4] - w
							rotatev, xr,yr, (*(*pstate).px)[4],(*(*pstate).py)[4], theta1, tx,ty
							(*p).x = tx[0:10]
							(*p).y = ty[0:10]
							end
						4: begin   										; mid (move width handles too)
								dx = x - (*p).x[4]
								dy = y - (*p).y[4]
								(*p).x[4] = x
								(*p).y[4] = y
								(*p).x[9] = (*p).x[9] + dx
								(*p).y[9] = (*p).y[9] + dy
								(*p).x[10] = (*p).x[10] + dx
								(*p).y[10] = (*p).y[10] + dy
							end
						else: begin										; other spline curve control points
							(*p).x[(*pstate).id] = x
							(*p).y[(*pstate).id] = y
							end
					endcase
					plot_mark, pstate
				endif else if (*pstate).right_button then begin
					clear_mark, pstate
					case (*pstate).id of
						4: begin										; centre (move all control points)
							dx = x - (*p).x[4]
							dy = y - (*p).y[4]
							(*p).x = (*p).x + dx
							(*p).y = (*p).y + dy
							end
						9: begin										; curvature offset 1
							theta1 = atan( (*(*pstate).py)[9]-(*(*pstate).py)[10], (*(*pstate).px)[9]-(*(*pstate).px)[10])
							tx = [*(*pstate).px, x]
							ty = [*(*pstate).py, y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -theta1, xr,yr
							delta = yr[11] - yr[4]
							yr[9] = yr[11]
							yr[10] = yr[11]
							rotatev, xr,yr, (*(*pstate).px)[4],(*(*pstate).py)[4], theta1, tx,ty
							(*p).x = tx[0:10]
							(*p).y = ty[0:10]
							end
						10: begin										; curvature offset 2
							theta1 = atan( (*(*pstate).py)[10]-(*(*pstate).py)[9], (*(*pstate).px)[10]-(*(*pstate).px)[9])
							tx = [*(*pstate).px, x]
							ty = [*(*pstate).py, y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -theta1, xr,yr
							delta = yr[11] - yr[4]
							yr[9] = yr[11]
							yr[10] = yr[11]
							rotatev, xr,yr, (*(*pstate).px)[4],(*(*pstate).py)[4], theta1, tx,ty
							(*p).x = tx[0:10]
							(*p).y = ty[0:10]
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			4: begin																; line traverse
				if (x eq (*p).x[(*pstate).id]) and (y eq (*p).y[(*pstate).id]) then goto, legend
				if (*pstate).left_button then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin										; start
							tx = [(*(*pstate).px)[2],(*(*pstate).px)[3]]
							ty = [(*(*pstate).py)[2],(*(*pstate).py)[3]]
							rotatev, tx,ty, (*(*pstate).px)[1],(*(*pstate).py)[1], -(*pstate).theta, xr,yr
							dx = 0.5*(xr[1] - xr[0])
							dy = 0.5*(yr[1] - yr[0])

							(*p).x[0] = x
							(*p).y[0] = y
							(*p).x[4] = 0.5*(x + (*p).x[1] )
							(*p).y[4] = 0.5*(y + (*p).y[1] )
							xc = (*p).x[4]
							yc = (*p).y[4]

							theta = angle_lines( (*p).x[0],(*p).y[0], (*p).x[1],(*p).y[0], (*p).x[1],(*p).y[1])
							(*p).theta = theta

							rotatev, dx,dy, 0.0,0.0, theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							end
						1: begin										; end
							tx = [(*(*pstate).px)[2],(*(*pstate).px)[3]]
							ty = [(*(*pstate).py)[2],(*(*pstate).py)[3]]
							rotatev, tx,ty, (*(*pstate).px)[0],(*(*pstate).py)[0], -(*pstate).theta, xr,yr
							dx = 0.5*(xr[1] - xr[0])
							dy = 0.5*(yr[1] - yr[0])

							(*p).x[1] = x
							(*p).y[1] = y
							(*p).x[4] = 0.5*(x + (*p).x[0] )
							(*p).y[4] = 0.5*(y + (*p).y[0] )
							xc = (*p).x[4]
							yc = (*p).y[4]

							theta = angle_lines( (*p).x[0],(*p).y[0], (*p).x[1],(*p).y[0], (*p).x[1],(*p).y[1])
							(*p).theta = theta

							rotatev, dx,dy, 0.0,0.0, theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							end
						2: begin										; width 1
							tx = [x,(*(*pstate).px)[4]]
							ty = [y,(*(*pstate).py)[4]]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr0,yr0
							shearv, xr0,yr0, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).shear, xr,yr
							dx = 0.0
							dy = yr[1] - yr[0]

							shearv, dx,dy, 0.0,0.0, (*pstate).shear, dx0,dy0
							rotatev, dx0,dy0, 0.0,0.0, (*pstate).theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						3: begin										; width 2
							tx = [(*(*pstate).px)[4],x]
							ty = [(*(*pstate).py)[4],y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr0,yr0
							shearv, xr0,yr0, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).shear, xr,yr
							dx = 0.0
							dy = yr[1] - yr[0]

							shearv, dx,dy, 0.0,0.0, (*pstate).shear, dx0,dy0
							rotatev, dx0,dy0, 0.0,0.0, (*pstate).theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						4: begin										; centre
							dx = x - (*p).x[4]
							dy = y - (*p).y[4]
							(*p).x = [ (*p).x[0]+dx, (*p).x[1]+dx, (*p).x[2]+dx, (*p).x[3]+dx,  x]
							(*p).y = [ (*p).y[0]+dy, (*p).y[1]+dy, (*p).y[2]+dy, (*p).y[3]+dy,  y]
							xc = x
							yc = y
							end
						else:
					endcase
					plot_mark, pstate
				endif else if (*pstate).right_button then begin
					clear_mark, pstate
					case (*pstate).id of
						2: begin										; width 1
							tx = [(*(*pstate).px)[2],(*(*pstate).px)[3]]
							ty = [(*(*pstate).py)[2],(*(*pstate).py)[3]]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr,yr
							dy = 0.5*(yr[1] - yr[0])

							tx = [x,(*(*pstate).px)[4]]
							ty = [y,(*(*pstate).py)[4]]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr,yr
							dx = xr[1] - xr[0]
							(*p).shear = atan(dx,dy)

							rotatev, dx,dy, 0.0,0.0, (*pstate).theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						3: begin										; width 2
							tx = [(*(*pstate).px)[2],(*(*pstate).px)[3]]
							ty = [(*(*pstate).py)[2],(*(*pstate).py)[3]]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr,yr
							dy = 0.5*(yr[1] - yr[0])

							tx = [(*(*pstate).px)[4],x]
							ty = [(*(*pstate).py)[4],y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr,yr
							dx = xr[1] - xr[0]
							(*p).shear = atan(dx,dy)

							rotatev, dx,dy, 0.0,0.0, (*pstate).theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			5: begin																; ellipse
				if (x ne (*p).x[(*pstate).id]) or (y ne (*p).y[(*pstate).id]) then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin										; main diameter
							tx = [(*(*pstate).px)[2],(*(*pstate).px)[3]]
							ty = [(*(*pstate).py)[2],(*(*pstate).py)[3]]
							rotatev, tx,ty, (*(*pstate).px)[1],(*(*pstate).py)[1], -(*pstate).theta, xr,yr
							dx = 0.0
							dy = 0.5*(yr[1] - yr[0])

							(*p).x[0] = x
							(*p).y[0] = y
							(*p).x[4] = 0.5*(x + (*p).x[1] )
							(*p).y[4] = 0.5*(y + (*p).y[1] )
							xc = (*p).x[4]
							yc = (*p).y[4]

							theta = angle_lines( (*p).x[0],(*p).y[0], (*p).x[1],(*p).y[0], (*p).x[1],(*p).y[1])
							(*p).theta = theta

							rotatev, dx,dy, 0.0,0.0, theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							end
						1: begin										; main diameter
							tx = [(*(*pstate).px)[2],(*(*pstate).px)[3]]
							ty = [(*(*pstate).py)[2],(*(*pstate).py)[3]]
							rotatev, tx,ty, (*(*pstate).px)[0],(*(*pstate).py)[0], -(*pstate).theta, xr,yr
							dx = 0.0
							dy = 0.5*(yr[1] - yr[0])

							(*p).x[1] = x
							(*p).y[1] = y
							(*p).x[4] = 0.5*(x + (*p).x[0] )
							(*p).y[4] = 0.5*(y + (*p).y[0] )
							xc = (*p).x[4]
							yc = (*p).y[4]

							theta = angle_lines( (*p).x[0],(*p).y[0], (*p).x[1],(*p).y[0], (*p).x[1],(*p).y[1])
							(*p).theta = theta

							rotatev, dx,dy, 0.0,0.0, theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							end
						2: begin										; short diameter
							tx = [(*(*pstate).px)[4],x]
							ty = [(*(*pstate).py)[4],y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr,yr
							dx = 0.0
							dy = yr[1] - yr[0]

							rotatev, dx,dy, 0.0,0.0, (*pstate).theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						3: begin										; short diameter
							tx = [(*(*pstate).px)[4],x]
							ty = [(*(*pstate).py)[4],y]
							rotatev, tx,ty, (*(*pstate).px)[4],(*(*pstate).py)[4], -(*pstate).theta, xr,yr
							dx = 0.0
							dy = yr[1] - yr[0]

							rotatev, dx,dy, 0.0,0.0, (*pstate).theta, dxr,dyr
							(*p).x[2] = (*p).x[4] - dxr
							(*p).y[2] = (*p).y[4] - dyr
							(*p).x[3] = (*p).x[4] + dxr
							(*p).y[3] = (*p).y[4] + dyr
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						4: begin										; centre
							dx = x - (*p).x[4]
							dy = y - (*p).y[4]
							(*p).x = [ (*p).x[0]+dx, (*p).x[1]+dx, (*p).x[2]+dx, (*p).x[3]+dx,  x]
							(*p).y = [ (*p).y[0]+dy, (*p).y[1]+dy, (*p).y[2]+dy, (*p).y[3]+dy,  y]
							xc = x
							yc = y
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			6: begin																; spline 10
				clear_mark, pstate
				if (*pstate).id eq 100 then begin							; drag expand
					circle_diam, (*p).x[1],(*p).y[1], x,y, xs,ys, n=10
					(*p).x[1:*] = xs[0:9]
					(*p).y[1:*] = ys[0:9]
					xc = mean((*p).x[1:*])
					yc = mean((*p).y[1:*])
					(*p).x[0] = xc
					(*p).y[0] = yc
				endif else if (*pstate).id eq 0 then begin					; centre
					dx = x - (*p).x[0]
					dy = y - (*p).y[0]
					(*p).x = [ x, (*p).x[1:*]+dx]
					(*p).y = [ y, (*p).y[1:*]+dy]
					xc = x
					yc = y
				endif else begin										; control points
					(*p).x[(*pstate).id] = x
					(*p).y[(*pstate).id] = y
					xc = mean((*p).x[1:*])
					yc = mean((*p).y[1:*])
					(*p).x[0] = xc
					(*p).y[0] = yc
				endelse
				plot_mark, pstate
				end
			7: begin																; spline 32
				clear_mark, pstate
				if (*pstate).id eq 100 then begin							; drag expand
					circle_diam, (*p).x[1],(*p).y[1], x,y, xs,ys, n=32
					(*p).x[1:*] = xs[0:31]
					(*p).y[1:*] = ys[0:31]
					xc = mean((*p).x[1:*])
					yc = mean((*p).y[1:*])
					(*p).x[0] = xc
					(*p).y[0] = yc
				endif else if (*pstate).id eq 0 then begin					; centre
					dx = x - (*p).x[0]
					dy = y - (*p).y[0]
					(*p).x = [ x, (*p).x[1:*]+dx]
					(*p).y = [ y, (*p).y[1:*]+dy]
					xc = x
					yc = y
				endif else begin										; control points
					(*p).x[(*pstate).id] = x
					(*p).y[(*pstate).id] = y
					xc = mean((*p).x[1:*])
					yc = mean((*p).y[1:*])
					(*p).x[0] = xc
					(*p).y[0] = yc
				endelse
				plot_mark, pstate
				end
			8: begin																; project X
				if (x eq (*p).x[(*pstate).id]) and (y eq (*p).y[(*pstate).id]) then goto, legend
				if (*pstate).left_button then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin										; start
							dy = (*p).y[3] - (*p).y[4]
							(*p).x[0] = x
							(*p).y[0] = y
							(*p).y[1] = y
							(*p).y[4] = y
							(*p).x[4] = (x+(*p).x[1])/2
							(*p).x[2] = (x+(*p).x[1])/2
							(*p).y[2] = (*p).y[4] - dy
							(*p).x[3] = (x+(*p).x[1])/2
							(*p).y[3] = (*p).y[4] + dy
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						1: begin										; end
							dy = (*p).y[3] - (*p).y[4]
							(*p).x[1] = x
							(*p).y[1] = y
							(*p).y[0] = y
							(*p).y[4] = y
							(*p).x[4] = (x+(*p).x[0])/2
							(*p).x[2] = (x+(*p).x[0])/2
							(*p).y[2] = (*p).y[4] - dy
							(*p).x[3] = (x+(*p).x[0])/2
							(*p).y[3] = (*p).y[4] + dy
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						2: begin										; width 1
							dy = (*p).y[4] - (*p).y[2]
							(*p).y[2] = y
							(*p).y[3] = (*p).y[4] + dy
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						3: begin										; width 2
							dy = (*p).y[3] - (*p).y[4]
							(*p).y[3] = y
							(*p).y[2] = (*p).y[4] - dy
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						4: begin										; centre
							dx = x - (*p).x[4]
							dy = y - (*p).y[4]
							(*p).x = [ (*p).x[0]+dx, (*p).x[1]+dx, (*p).x[2]+dx, (*p).x[3]+dx,  x]
							(*p).y = [ (*p).y[0]+dy, (*p).y[1]+dy, (*p).y[2]+dy, (*p).y[3]+dy,  y]
							xc = x
							yc = y
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			9: begin																; project Y
				if (x eq (*p).x[(*pstate).id]) and (y eq (*p).y[(*pstate).id]) then goto, legend
				if (*pstate).left_button then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin										; start
							dx = (*p).x[3] - (*p).x[4]
							(*p).y[0] = y
							(*p).x[0] = x
							(*p).x[1] = x
							(*p).x[4] = x
							(*p).y[4] = (y+(*p).y[1])/2
							(*p).y[2] = (y+(*p).y[1])/2
							(*p).x[2] = (*p).x[4] - dx
							(*p).y[3] = (y+(*p).y[1])/2
							(*p).x[3] = (*p).x[4] + dx
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						1: begin										; end
							dx = (*p).x[3] - (*p).x[4]
							(*p).y[1] = y
							(*p).x[1] = x
							(*p).x[0] = x
							(*p).x[4] = x
							(*p).y[4] = (y+(*p).y[0])/2
							(*p).y[2] = (y+(*p).y[0])/2
							(*p).x[2] = (*p).x[4] - dx
							(*p).y[3] = (y+(*p).y[0])/2
							(*p).x[3] = (*p).x[4] + dx
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						2: begin										; width 1
							dx = (*p).x[4] - (*p).x[2]
							(*p).x[2] = x
							(*p).x[3] = (*p).x[4] + dx
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						3: begin										; width 2
							dx = (*p).x[3] - (*p).x[4]
							(*p).x[3] = x
							(*p).x[2] = (*p).x[4] - dx
							xc = (*p).x[4]
							yc = (*p).y[4]
							end
						4: begin										; centre
							dx = x - (*p).x[4]
							dy = y - (*p).y[4]
							(*p).x = [ (*p).x[0]+dx, (*p).x[1]+dx, (*p).x[2]+dx, (*p).x[3]+dx,  x]
							(*p).y = [ (*p).y[0]+dy, (*p).y[1]+dy, (*p).y[2]+dy, (*p).y[3]+dy,  y]
							xc = x
							yc = y
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			10: begin																; spline 100
				clear_mark, pstate
				if (*pstate).id eq 200 then begin						; drag expand
					circle_diam, (*p).x[1],(*p).y[1], x,y, xs,ys, n=100
					(*p).x[1:*] = xs[0:99]
					(*p).y[1:*] = ys[0:99]
					xc = mean((*p).x[1:*])
					yc = mean((*p).y[1:*])
					(*p).x[0] = xc
					(*p).y[0] = yc
				endif else if (*pstate).id eq 0 then begin				; centre
					dx = x - (*p).x[0]
					dy = y - (*p).y[0]
					(*p).x = [ x, (*p).x[1:*]+dx]
					(*p).y = [ y, (*p).y[1:*]+dy]
					xc = x
					yc = y
				endif else begin										; control points
					(*p).x[(*pstate).id] = x
					(*p).y[(*pstate).id] = y
					xc = mean((*p).x[1:*])
					yc = mean((*p).y[1:*])
					(*p).x[0] = xc
					(*p).y[0] = yc
				endelse
				plot_mark, pstate
				end
			11: begin																; S pixel
				if (x ne (*p).x[(*pstate).id]) or (y ne (*p).y[(*pstate).id]) then begin
					clear_mark, pstate
					case (*pstate).id of
						0: begin
							(*p).x = [x]
							(*p).y = [y]
							end
						else:
					endcase
					plot_mark, pstate
				endif
				end
			else:
		endcase
legend:
		case (*pstate).analyze_type[(*pstate).analyze_mode] of
			0: begin													; distance
				(*pstate).sizex = (*p).x[1] - (*p).x[0]
				(*pstate).sizey = (*p).y[1] - (*p).y[0]
				xy_to_microns, pstate, (*pstate).sizex, (*pstate).sizey, sx,sy,sunits
				d = sqrt( sx*sx + sy*sy)
				angle = atan( sy, sx)
				(*pstate).size_units = sunits
				s = 'Distance =' + string(d) + ' ' + (*pstate).size_units+', angle = '+str_tidy(angle * !radeg, places=2)
				widget_control, (*pstate).help, set_value=s
				goto, finish
				end
			11: begin													; S pixel
				xy_to_microns, pstate, (*p).x[0], (*p).y[0], sx,sy,sunits
				s = 'pixel = ' + str_tidy((*p).x[0])+','+str_tidy((*p).y[0]) + ' (' + str_tidy(sx)+','+str_tidy(sy)+ ' '+(*pstate).size_units+')'
				widget_control, (*pstate).help, set_value=s
				goto, finish
				end
			else:
		endcase
legend2:
		if (xc ne 0) and (yc ne 0) then begin
			s1 = 'Centre  X =' + str_tidy(xc) + '  Y =' + str_tidy(yc)
			s2 = 'Absolute  X =' + str_tidy(xc+(*pimg).xoffset) + '  Y =' + str_tidy(yc+(*pimg).yoffset)
			s3 = '(uncompress: X =' + str_tidy((xc+(*pimg).xoffset)*(*pimg).xcompress) + '  Y =' + str_tidy((yc+(*pimg).yoffset)*(*pimg).ycompress) + ')'
			widget_control, (*pstate).help, set_value=[s1,s2,s3]
		endif
		end

	'UP': begin
		pixel_to_xy, pstate, event.x,event.y, x,y
		pixel_to_xy, pstate, event.x,event.y, xf,yf, /fractional

		if (*pstate).middle_button then begin
			xy_to_pixel, pstate, (*pstate).movex,(*pstate).movey, px,py
			wset, (*pstate).wid2
			device, copy=[px+8,py+8,106,106, px+8,py+8, (*pstate).pix]
			goto, motion_off
		endif
		if ((*pstate).corr_on eq 0) and ptr_valid((*pstate).qc) then begin
			(*pstate).corr_on = 1
			draw_images, pstate
		endif
		(*pstate).corr_on = 1

		case (*pstate).analyze_type[(*pstate).analyze_mode] of
			0: begin
				(*pstate).sizex = (*p).x[1] - (*p).x[0]
				(*pstate).sizey = (*p).y[1] - (*p).y[0]
				xy_to_microns, pstate, (*pstate).sizex, (*pstate).sizey, sx,sy,sunits
				d = sqrt( sx*sx + sy*sy)
				angle = atan( sy, sx)
				(*pstate).size_units = sunits
				s = 'Distance =' + string(d) + ' ' + (*pstate).size_units+', angle = '+str_tidy(angle * !radeg, places=2)
				widget_control, (*pstate).help, set_value=s
				end
			1: begin
				rotatev, [(*p).x],[(*p).y], (*p).x[0],(*p).y[0], -(*p).theta, xr,yr
				xy_to_microns, pstate, abs(xr[1]-xr[0]), abs(yr[2]-yr[1]), sx1,sy1,sunits		; need for units
				(*pstate).sizex = sx1
				(*pstate).sizey = sy1

				r = image_absolute( pimg, crop={x:[min((*p).x),max((*p).x)], y:[min((*p).y),max((*p).y)]}, error=err)
				if err eq 0 then begin
					tag = '*'
					ox = r.absolute.org.x
					oy = r.absolute.org.y
					sx = r.absolute.size.x
					sy = r.absolute.size.y
					pox = r.pixel.org.x
					poy = r.pixel.org.y
					px = r.pixel.size.x
					py = r.pixel.size.y
					uox = r.uncompressed.org.x
					uoy = r.uncompressed.org.y
					usx = r.uncompressed.size.x
					usy = r.uncompressed.size.y
					if sunits eq 'microns' then begin
						ox = ox*1000. & oy = oy*1000.
					endif

					s1 = 'Box size X=' + str_tidy(sx) + ' Y=' + str_tidy(sy) + ' ' + $
							(*pstate).size_units + ' (' + str_tidy(round(px)) + ', ' + str_tidy(round(py)) + ' pixels)'
					s1 = s1 + ', theta =' + str_tidy((*p).theta*180./!pi)
					s2 = 'Origin X'+tag+'=' + str_tidy(ox ) + ' Y'+tag+'=' + str_tidy(oy ) + ' '+sunits
					s3 = ' (' + str_tidy(round(pox)) + ', ' + str_tidy(round(poy)) + ' pixels)'
					s4 = 'Uncompressed: Size=' + str_tidy(round(usx)) + 'x' + str_tidy(round(usy)) +  $
							', Origin=' + str_tidy(round(uox)) + 'x' + str_tidy(round(uoy)) + ' pixels'
				endif

				widget_control, (*pstate).help, set_value=[s1,s2+s3,s4]
				set_kvs_box, pstate, ox,oy, (*pstate).sizex,(*pstate).sizey

;widget_control, event.id, draw_motion_events=0

;				Set 'region_window', which is used in Sort EVT to set a windowed sort.

				region_window = {offset:{x:round(uox), y:round(uoy)}, range:{x:round(usx), y:round(usy)}} 
				print, 'Region window: ',region_window
				end
			2: begin
				xm = (*p).x[2]
				ym = (*p).y[2]
				xy_to_microns, pstate, xm, ym, sx,sy,sunits			; need for units

				r = image_absolute( pimg, crop={x:[xm], y:[ym]}, error=err)
				if err eq 0 then begin
					tag = '*'
					ox = r.absolute.org.x
					oy = r.absolute.org.y
					pox = r.pixel.org.x
					poy = r.pixel.org.y
					if sunits eq 'microns' then begin
						ox = ox*1000. & oy = oy*1000.
					endif

					s2 = 'Origin X'+tag+'=' + str_tidy(ox) + ' Y'+tag+'=' + str_tidy(oy) + ' '+sunits + ' (centre)'
					s3 = ' (' + str_tidy(round(pox)) + ', ' + str_tidy(round(poy)) + ' pixels)'
	
					s = 'Circle diameter =' + string((*pstate).sizex) + ' ' + (*pstate).size_units
					widget_control, (*pstate).help, set_value=[s,s2,s3]
	
					w =  {		x:	0.0, $				; X stage/ captured cursor position
								y:	0.0, $				; Y
								z:	0.0}				; Z
					w.x = ox
					w.y = oy
;					w.z = (*pimg).scan.origin.z			; once Z origin is added to DAI header
;					help,w

;					Get the facility and endstation number "1" from the image metadata ...

					facility = ((*pimg).facility ne '') ? (*pimg).facility : (*pstate).kvs_prefix
					endstation = ((*pimg).endstation ne '') ? (*pimg).endstation : '1'
					if strmid( facility, strlen(facility)-1,1) ne '.' then facility = facility+'.'
					kname = facility + 'GP' + '.' + endstation
					ref = kname + '.position'
					kvs = (facility eq (*pstate).kvs_prefix2) ? (*pstate).kvs2 : (*pstate).kvs
					set_kvs, kvs, ref, w, error=error
					if error then begin
;					warning,'OnButton_image','Error setting Cursor "Position" in KVS.'
					endif
				endif
				end
			4: begin
				xy_to_microns, pstate, abs((*p).x[0]-(*p).x[1]), abs((*p).y[0]-(*p).y[1]), sx,sy,sunits
				(*pstate).sizex = sqrt( sx*sx + sy*sy)
				xy_to_microns, pstate, abs((*p).x[2]-(*p).x[3]), abs((*p).y[2]-(*p).y[3]), sx,sy,sunits
				(*pstate).sizey = sqrt( sx*sx + sy*sy)
				(*pstate).size_units = sunits
				s = ['Line length =' + string((*pstate).sizex) + ' ' + (*pstate).size_units, $
					'Line width =' + string((*pstate).sizey) + ' ' + (*pstate).size_units]
				widget_control, (*pstate).help, set_value=s
				end
			5: begin
				(*pstate).sizex = sqrt( ((*p).x[1]-(*p).x[0])*((*p).x[1]-(*p).x[0]) + ((*p).y[1]-(*p).y[0])*((*p).y[1]-(*p).y[0]) )
				(*pstate).sizey = sqrt( ((*p).x[2]-(*p).x[3])*((*p).x[2]-(*p).x[3]) + ((*p).y[2]-(*p).y[3])*((*p).y[2]-(*p).y[3]) )
				xy_to_microns, pstate, (*pstate).sizex, (*pstate).sizey, sx,sy,sunits
				(*pstate).sizex = sx
				(*pstate).sizey = sy
				(*pstate).size_units = sunits
				s = 'Size  X =' + string((*pstate).sizex) + '  Y =' + string((*pstate).sizey) + ' ' + (*pstate).size_units + ',  theta =' + string((*p).theta*180./!pi)
				widget_control, (*pstate).help, set_value=s
				end
			8: begin
				xy_to_microns, pstate, abs((*p).x[0]-(*p).x[1]), abs((*p).y[0]-(*p).y[1]), sx,sy,sunits
				(*pstate).sizex = sqrt( sx*sx + sy*sy)
				xy_to_microns, pstate, abs((*p).x[2]-(*p).x[3]), abs((*p).y[2]-(*p).y[3]), sx,sy,sunits
				(*pstate).sizey = sqrt( sx*sx + sy*sy)
				(*pstate).size_units = sunits
				s = ['Line length =' + string((*pstate).sizex) + ' ' + (*pstate).size_units, $
					'Line width =' + string((*pstate).sizey) + ' ' + (*pstate).size_units]
				widget_control, (*pstate).help, set_value=s
				end
			9: begin
				xy_to_microns, pstate, abs((*p).x[0]-(*p).x[1]), abs((*p).y[0]-(*p).y[1]), sx,sy,sunits
				(*pstate).sizex = sqrt( sx*sx + sy*sy)
				xy_to_microns, pstate, abs((*p).x[2]-(*p).x[3]), abs((*p).y[2]-(*p).y[3]), sx,sy,sunits
				(*pstate).sizey = sqrt( sx*sx + sy*sy)
				(*pstate).size_units = sunits
				s = ['Line length =' + string((*pstate).sizex) + ' ' + (*pstate).size_units, $
					'Line width =' + string((*pstate).sizey) + ' ' + (*pstate).size_units]
				widget_control, (*pstate).help, set_value=s
				end
			11: begin
				xm = (*p).x[0]
				ym = (*p).y[0]
				xy_to_microns, pstate, xm, ym, xp,yp,sunits			; need for units and xp,yp mm relative position

				r = image_absolute( pimg, crop={x:[xm], y:[ym]}, error=err)
				if err eq 0 then begin
					tag = '*'
					ox = r.absolute.org.x
					oy = r.absolute.org.y
					pox = r.pixel.org.x
					poy = r.pixel.org.y
					if sunits eq 'microns' then begin
						ox = ox*1000. & oy = oy*1000.
					endif

					s = 'pixel = ' + str_tidy(pox)+','+str_tidy(poy) + ' (' + str_tidy(xp)+','+str_tidy(yp)+ ' '+sunits+')'
					s2 = 'Absolute X'+tag+'=' + str_tidy(ox) + ' Y'+tag+'=' + str_tidy(oy) + ' '+sunits
					s3 = ' (' + str_tidy(round(pox)) + ', ' + str_tidy(round(poy)) + ' pixels)'
					widget_control, (*pstate).help, set_value=[s,s2,s3]
	
					w =  {		x:	0.0, $				; X stage/ captured cursor position
								y:	0.0, $				; Y
								z:	0.0}				; Z
					w.x = ox
					w.y = oy
;					w.z = (*pimg).scan.origin.z			; once Z origin is added to DAI header
;					help,w

;					Get the facility and endstation number "1" from the image metadata ...

					facility = ((*pimg).facility ne '') ? (*pimg).facility : (*pstate).kvs_prefix
					endstation = ((*pimg).endstation ne '') ? (*pimg).endstation : '1'
					if strmid( facility, strlen(facility)-1,1) ne '.' then facility = facility+'.'
					kname = facility + 'GP' + '.' + endstation
					ref = kname + '.position'
					kvs = (facility eq (*pstate).kvs_prefix2) ? (*pstate).kvs2 : (*pstate).kvs
					set_kvs, kvs, ref, w, error=error
						if error then begin
;						warning,'OnButton_image','Error setting Cursor "Position" in KVS.'
					endif
				endif
				end
			else:
		endcase
		if (*pstate).analyze_mode eq 1 then begin
			(*pstate).analyze_mode = 0
			clear_mark, pstate, from=(*pstate).pix2, to=(*pstate).pix		; restore pristine area
			(*pstate).analyze_mode = 1
		endif else begin
;			print,'UP: kill q!
			if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
			(*pstate).q = ptr_new()
			if ptr_valid((*pstate).pq) then ptr_free, (*pstate).pq
			if ptr_valid((*pstate).p) then begin
				(*pstate).pq = ptr_new( {q:(*pstate).q, nx:(*(*pstate).p).xsize, ny:(*(*pstate).p).ysize} )
				notify, 'image-analyze-q', (*pstate).pq, from=event.top
			endif
		endelse

motion_off:
		widget_control, event.id, draw_motion_events=0
		notify, 'image-analyze-mark', from=event.top
		end
endcase

finish:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die
	return

die:
	widget_control, event.top, /destroy
	return
end

