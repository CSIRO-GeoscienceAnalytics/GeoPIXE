function detector_mimic_event, event

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
	   warning,'detector_mimic',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   goto, finish
	endif
endif

if widget_info( event.id, /valid) eq 0 then return, 0L
child = widget_info( event.handler, /child)
widget_control, child, get_uvalue=pstate

if size(pstate,/tname) ne 'POINTER' then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

pd = (*pstate).data
if size(pd,/tname) ne 'POINTER' then goto, bad_ptr
if size(*pd,/tname) ne 'STRUCT' then goto, bad_ptr
return_event = 0L

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 2 then begin
			widget_control, event.id, get_uvalue=i
			k = (*pd).data[i].index
;			print, 'Tracking: Detector=',k
			return_event = {DETECTOR_MIMIC, ID:event.handler, TOP:event.top, HANDLER:0L, $
								detector:k, index:i, select:0, alt:0, type:1}
		endif else if (*pstate).tracking eq 1 then begin
			return_event = event
		endif ;else then begin
;			event.id = event.handler
;			return_event = event
;		endelse
		goto, finish
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'ARRAY': begin
		widget_control, event.id, get_uvalue=i
		k = (*pd).data[i].index
;		print, 'Detector=',k, '  select=',event.select, '  alt=',event.alt
		return_event = {DETECTOR_MIMIC, ID:event.handler, TOP:event.top, HANDLER:0L, $
						detector:k, index:i, select:event.select, alt:event.alt, type:0}
		end
	else:
endcase

finish:
	return, return_event

bad_state:
	warning,'detector_mimic_event',['STATE variable has become ill-defined.','Abort Fit Setup.'],/error
	return_event = 0L
	goto, finish
bad_ptr:
	warning,'detector_mimic_event',['Parameter structure variable has become ill-defined.','Abort Fit Setup.'],/error
	return_event = 0L
	goto, finish
end

;------------------------------------------------------------------------------------------

function detector_mimic_scale, x,y, width, height, xsize_min, xsize_max, ysize_max, csize

	s = {x:{m:0.0, c:0.0, n:0, min:0.0, max:0.0, size:0.0}, y:{m:0.0, c:0.0, n:0, min:0.0, max:0.0, size:0.0}, $
			csize:0.0 }

 	s.x.min = min(x - width/2)
	s.x.max = max(x + width/2)
	s.y.min = min(y - height/2)
	s.y.max = max(y + height/2)
	dx = max(width)
	dy = max(height)
	s.x.size = 32
	s.y.size = 32
	s.csize = csize
	nx = (s.x.max-s.x.min)/dx
	ny = (s.y.max-s.y.min)/dy
	if nx*s.x.size lt xsize_min then begin
		xs0 = s.x.size
		s.x.size = round(xsize_min/float(nx)) < 50
		s.y.size = round(s.y.size*(s.x.size/float(xs0)))
	endif
	if nx*s.x.size gt xsize_max then begin
		xs0 = s.x.size
		s.x.size = round(xsize_max/float(nx)) < 50
		s.y.size = round(s.y.size*(s.x.size/float(xs0)))
	endif
	if ny*s.y.size gt ysize_max then begin
		ys0 = s.y.size
		s.y.size = round(ysize_max/float(ny)) < 50
		s.x.size = round(s.x.size*(s.y.size/float(ys0)))
	endif
	xsize_min = s.x.size *nx
	s.csize = s.csize * s.y.size/float(32)
	if s.csize gt 0.1 then s.csize = s.csize > 0.6

	s.x.m = s.x.size/dx
	s.y.m = -s.y.size/dy
	s.x.c = -s.x.m * s.x.min
	s.y.c = -s.y.m * s.y.max
	return, s
end

;-----------------------------------------------------------------------------

pro detector_mimic_set, id, vset

; Set attributes:
;	select		the colour index for button
;	value		the text string
;	alt			enable alt (right click) colour index
;	mode		0 normal, 1 use mode_colour, not select index
;	colour		set this colour only in mode=1 state
;	highlight	draw highlight central spot on selected
;	legend		optional string legend areas
;	label		?

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate
select_found = 0
value_found = 0
alt_found = 0
label_found = 0
colour_found = 0
legend_found = 0
highlight_found = 0
mode = 0
colour = 0
highlight = 0

if size(vset,/tname) eq 'STRUCT' then begin
    tags = tag_names(vset)
    if n_elements(tags) gt 0 then begin
       for i=0L, n_elements(tags)-1 do begin
         case tags[i] of
          'MODE': begin
          	  mode = vset.mode[0]
          	  end
          'COLOUR': begin
          	  colour = vset.colour
          	  colour_found = 1
          	  end
          'SELECT': begin
              select = vset.select
              select_found = 1
              end
          'HIGHLIGHT': begin
              highlight = vset.highlight
              highlight_found = 1
              end
          'VALUE': begin
              value = vset.value
              value_found = 1
              end
          'ALT': begin
              alt = vset.alt
              alt_found = 1
              end
          'LABEL': begin
              label = vset.label
              label_found = 1
              end
          'LEGEND': begin
              legend = vset.legend
              legend_found = 1
              end
          else:
         endcase
       endfor

		if legend_found then begin
			for i=0L,min([n_elements(legend),3])-1 do begin
				widget_control, (*pstate).legend[i], set_value=legend[i]
			endfor
		endif
		if select_found then begin
			for i=0L,n_elements(select)-1 do begin
				j = select[i]
				if mode eq 1 then begin
					if colour_found then begin
						if n_elements(select) eq n_elements(colour) then begin
							widget_control, (*(*pstate).pid)[j], set_value={mode:1, colour:colour[i]}
						endif
					endif
				endif else begin
					if value_found then begin
						v = (n_elements(select) eq n_elements(value)) ? value[i] : value[0]
						widget_control, (*(*pstate).pid)[j], set_value={mode:0, select:v}
					endif else if highlight_found then begin
						v = (n_elements(select) eq n_elements(highlight)) ? highlight[i] : highlight[0]
						widget_control, (*(*pstate).pid)[j], set_value={mode:0, highlight:v}
					endif else begin
						widget_control, (*(*pstate).pid)[j], set_value={mode:0, select:1}
					endelse
					if alt_found then begin
						v = (n_elements(select) eq n_elements(alt)) ? alt[i] : alt[0]
						widget_control, (*(*pstate).pid)[j], set_value={mode:0, alt:v}
					endif
				endelse
				if label_found then begin
					if n_elements(select) eq n_elements(label) then begin
						widget_control, (*(*pstate).pid)[j], set_value={label:label[i]}
					endif
				endif
			endfor
		endif else begin
			if mode eq 1 then begin
				if colour_found then begin
					for i=0L,n_elements(colour)-1 do begin
						widget_control, (*(*pstate).pid)[i], set_value={mode:1, colour:colour[i]}
					endfor
				endif
			endif else begin
				if value_found then begin
					for i=0L,n_elements(value)-1 do begin
						widget_control, (*(*pstate).pid)[i], set_value={mode:0, select:value[i]}
					endfor
				endif
				if alt_found then begin
					for i=0L,n_elements(alt)-1 do begin
						widget_control, (*(*pstate).pid)[i], set_value={mode:0, alt:alt[i]}
					endfor
				endif
			endelse
		endelse
    endif
endif
return
end

;-----------------------------------------------------------------------------

function detector_mimic_get, id

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

n = n_elements(*(*pstate).pid)
value = intarr(n)
v = 0
for i=0L,n-1 do begin
	widget_control, (*(*pstate).pid)[i], get_value=v
	value[i] = v
endfor

return, value
end

;------------------------------------------------------------------------------------------

function detector_mimic, parent, data=pd, uname=uname, uvalue=uvalue, tracking=tracking, $
			xsize_min=xsize_min, xsize_max=xsize_max, csize=csize, colours=colours, $
			legend=legend, position=legend_position, fill=fill, ysize_max=ysize_max, $
			lstyle=lstyle

; tracking	1	normal tracking return for caller to use with uvalue and help
;			2	returns a struct including index to detector pad
; legend	number of legend strings (up to 4, depending on overall mimic size)
; position	0 (centre), 1 (bottom)
; /fill		fill out pad size so all = maximum
; lstyle	0 (text boxes, vertical), 1 (labels, horizontal list)

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
	   Catch, /cancel
	   on_error, 1
	   help, calls = s
	   n = n_elements(s)
	   c = 'Call stack: '
	   if n gt 2 then c = [c, s[1:n-2]]
	   warning,'detector_mimic',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   return, 0L
	endif
endif

if n_elements(parent) lt 1 then return,0L
if n_elements(uname) lt 1 then uname=''
if n_elements(uvalue) lt 1 then uvalue=''
if n_elements(tracking) lt 1 then tracking=0
if n_elements(xsize_min) lt 1 then xsize_min=300
if n_elements(xsize_max) lt 1 then xsize_max=600
if n_elements(ysize_max) lt 1 then ysize_max=600
if n_elements(csize) lt 1 then csize=1.
if n_elements(fill) lt 1 then fill=0
if n_elements(legend) lt 1 then legend=0
if n_elements(legend_position) lt 1 then legend_position=0
if n_elements(lstyle) lt 1 then lstyle=0

pd = bad_pars_struct( pd, make_pars=make_pd)
if make_pd then begin
	warning,'detector_mimic','No "Data" supplied for layout.'
	return, 0L
endif

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
black = spec_colour('black')
white = spec_colour('white')

;                     				  select: 0     1   2      3   4      5   6     7       8   9
;if n_elements(colours) lt 1 then colours = [grey,green,yellow,red,lblue,orange,white,dgreen,blue,violet]

;                     				  select: 0     1     2     3
if n_elements(colours) lt 1 then colours = [grey,yellow,violet,red]

; Rotate layout according to symmetry and reorient count
; Remember: any changes here likely to affect "detector_geometry" too.

u = (*pd).data.x
if (*pd).mirrorX then u=-u
v = (*pd).data.y
if (*pd).mirrorY then v=-v
z = replicate(0.,(*pd).N)
w = replicate(1.,(*pd).N)
h = [[u],[v],[z],[w]]									; 4D homogenous coordinates

if (*pd).symmetry eq 0 then (*pd).symmetry=4
ddel = 360./(*pd).symmetry
delta = ddel * ((*pd).reorient mod (*pd).symmetry)

t3d, /reset, matrix=matrix
t3d, matrix, matrix=matrix, rotate=[0.,0.,delta]

h2 = h # matrix											; apply the transformations
r2 = transpose(h2[*,0:2])								; cartesian coords (ignore 'w' since no scaling)
x = reform(r2[0,*])
y = reform(r2[1,*])

s = detector_mimic_scale( x,y, (*pd).data.width, (*pd).data.height, xsize_min, xsize_max, ysize_max, csize)

tlb = widget_base( parent, $
	pro_set_value='detector_mimic_set', func_get_value='detector_mimic_get', $
	event_func='detector_mimic_event' )

; make layout in window

w = (*pd).data.width
h = (*pd).data.height
if fill then begin
	w[*] = max(w)
	h[*] = max(h)
endif

hmax = abs(s.y.m* max((*pd).data.height))
ysize = s.y.m*(s.y.min-s.y.max) + hmax
if legend gt 0 then begin
	if legend_position eq 1 then begin
		ysize = ysize + ((lstyle eq 0) ? legend*hmax : hmax)
		ysize = ysize + 2*hmax
	endif
endif
base2 = widget_base( tlb, scr_xsize=s.x.m*(s.x.max-s.x.min)+10, scr_ysize=ysize )

ybottom = 0
ytop = 10000
xright = 0
xleft = 10000
id = lonarr((*pd).N)
for i=0L,(*pd).N-1 do begin
	k = (*pd).data[i].index
	if (*pd).reorient mod 2 then begin
		ys = s.x.m* w[i]
		xs = -s.y.m* h[i]
	endif else begin
		xs = s.x.m* w[i]
		ys = -s.y.m* h[i]
	endelse
	xoff = s.x.m* (x[i] - w[i]/2) + s.x.c + 5
	yoff = s.y.m* (y[i] ) + s.y.c
	ybottom = ybottom > yoff
	ytop = ytop < yoff
	xleft = xleft < xoff
	xright = xright > xoff
;	if i eq 32 then begin
;		print,'debug ...'
;	endif

	id[i] = state_button( base2, xsize=xs, ysize=ys, value=str_tidy(k), $
			  uname='ARRAY', charsize=s.csize, select=(*pd).data[i].bad, $
			  n_states=n_elements(colours)/2, right=0, alt=0, n_alt_states=n_elements(colours)/2, colours=colours, /freeze, $
			  uvalue=i, tracking=(tracking ne 0), xoffset=xoff, yoffset=yoff, shape=1-(*pd).shape )
endfor

w = max(s.x.m* ((*pd).data.width))
h = max(-s.y.m* ((*pd).data.height))
xright = xright + w
xcentre = 0.5*(xleft + xright)
ybottom = ybottom + h
ycentre = 0.5*(ytop + ybottom)

if legend gt 0 then begin
	case legend_position of
		0: begin
			xoff = xcentre - 2*w
			yoff = ycentre - h*(legend/2)
			xw = 4*w
			dx = 0
			dy = h
			end
		1: begin
			case lstyle of
				0: begin
					xoff = xcentre - 3.5*w
					yoff = ybottom + 0.3*h
					xw = 7*w
					dx = 0
					dy = h
					end
				1: begin
;					xoff = xleft + w
;					yoff = ybottom + 0.3*h
;					dx = (xright-xleft-2*w)/float(legend)
;					dy = 0
;					xw = dx - 0.5*w

					xoff = xleft
					yoff = ybottom + 0.3*h
					dx = (xright-xleft)/float(legend)
					dy = 0
					xw = dx
					end
			endcase
			end
	endcase
;	print,'Mimic array: xcentre=',xcentre,' ybottom=',ybottom,' w,h=',w,h
;	print,'Mimic legend: xleft=',xleft,' ybottom=',ybottom,' xoff, yoff=',xoff,yoff,' dx,dy=',dx,dy,' xw=',xw
	legend_text = lonarr(legend < 4)

	for i=0L,legend-1 do begin
		legend_text[i] = widget_label( base2, xoffset=xoff, yoffset=yoff, value='                 ', scr_xsize=xw) ;, scr_ysize=h
		xoff = xoff + dx
		yoff = yoff + dy
	endfor
endif else legend_text=0L

state = {	data:		pd, $				; pointer to layout data struct
			legend:		legend_text, $		; legend text widget IDs
			tracking:	tracking, $			; tracking mode (1 normal, 2 return IDs)
			pid:		ptr_new() $			; widget ID's of all detector pad buttons
	   }
pstate = ptr_new(state, /no_copy)

(*pstate).pid = ptr_new(id, /no_copy)
widget_control, base2, set_uvalue=pstate
widget_control, tlb, set_uname=uname, set_uvalue=uvalue
return, tlb
end
