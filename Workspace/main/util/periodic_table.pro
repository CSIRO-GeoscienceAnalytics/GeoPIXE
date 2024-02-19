;	Periodic table compound widget
;
;	tlb = periodic_table( parent)
;
; Keywords:
;	n_states	number of ON states
;	z_on		atomic numbers to be set ON
;	z_state		state for on elements
;
;	n_alt_states	number of ALT states
;	z_alt		atomic number set to ALT state
;	z_astate	state for ALT elements
;
;	legend		vector of legend strings for ON and ALT states
;
;	/right		enable use of right mouse button to switch to ALT
;			(this is automatic if n_alt_states is specified)
;	/start_Li	only start table at Li
;	/start_Na	only start table at Na
;	/no_REE		do not do REE elements

;	/tiny		for tiny buttons with no labels
;	/exclusive	simple exclusive button action. Only one can be regardless
;			of the number of states
;
;	colours		a vector to set the colour indices for ON and ALT states
;	_extra		these passed onto TLB

function periodic_table_event, event

if event.id eq event.handler then begin
;	help,event,/structure
	return, event
endif
child = widget_info( event.handler, /child)
widget_control, child, get_uvalue=pstate

if widget_info( event.id, /uname) eq 'PERIODIC' then begin
	widget_control, event.id, get_uvalue=z
	el = element_name(z)
	if tag_names( event, /structure) eq 'WIDGET_TRACKING' then begin
		if event.enter eq 1 then begin
			widget_control, (*pstate).track[(*pstate).tiny], set_value=el
		endif else begin
			widget_control, (*pstate).track[(*pstate).tiny], set_value=' '
		endelse
	endif else begin
		if (*pstate).exclusive then begin
			if (event.id eq (*pstate).last_id) then begin
				if widget_info( event.id, /valid_id) then begin
					widget_control, event.id, set_value={SELECT: 1, ALT:0}
				endif
			endif else begin
				if widget_info( (*pstate).last_id, /valid_id) then begin
					widget_control, (*pstate).last_id, set_value={SELECT: 0}
				endif
			endelse
		endif
		(*pstate).last_id = event.id

		ev = {PERIODIC, ID:event.handler, TOP:event.top, HANDLER:0L, ZID:event.id, $
						Z:z, ELEMENT:el, Shell:event.select, $
						Alt:event.alt, Select:(event.select ne 0) }
		return, ev
	endelse
endif

return, 0
end

;-------------------------------------------------------------------------

pro periodic_table_set_value, id, x

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

tags = tag_names(x)
if n_elements(x) gt 0 then begin
	for i=0L,n_elements(tags)-1 do begin
		case tags[i] of
			'TINY': begin

;				Value = 0 normal mode,  1 = tiny font mode

				if (*pstate).allow then begin
					(*pstate).tiny = clip( x.tiny, 0,1)
					tiny = (*pstate).tiny

					widget_control, (*pstate).base2[tiny], map=1
;					geom = widget_info( (*pstate).base2[tiny], /geometry)
					widget_control, (*pstate).base2[tiny], get_uvalue=siz
					widget_control, (*pstate).base2[tiny], scr_xsize=siz[0], scr_ysize=siz[1]

					widget_control, (*pstate).base2[1-tiny], map=0
					widget_control, (*pstate).base2[1-tiny], scr_xsize=100, scr_ysize=100
				endif
				end

			'SHOW': begin

;				Value = 0 hide,  1 = show

				tiny = (*pstate).tiny
				if x.show eq 1 then begin
					widget_control, (*pstate).base2[tiny], map=1
;					geom = widget_info( (*pstate).base2[tiny], /geometry)
					widget_control, (*pstate).base2[tiny], get_uvalue=siz
					widget_control, (*pstate).base2[tiny], scr_xsize=siz[0], scr_ysize=siz[1]
				endif else begin
					widget_control, (*pstate).base2[tiny], map=0
					widget_control, (*pstate).base2[tiny], scr_xsize=100, scr_ysize=100
				endelse
				end

			'Z': begin

;				set value of Z button to 'state'
;				Z, STATE can be both vectors.
;				Optional: ALT flag.

				q = where((x.z gt 0) and (x.z le 103))
				nq = n_elements(q)
				if (*pstate).exclusive then nq = 1
				if q[0] ne -1 then begin
					t = where(tag_names(x) eq 'ALT')
					if t[0] ne -1 then begin
						alt = x.alt[q]
					endif else begin
						alt = intarr(nq)
					endelse

					for i=0L,nq-1 do begin
						if (*pstate).exclusive then begin
							if widget_info( (*pstate).last_id, /valid_id) then begin
								widget_control, (*pstate).last_id, set_value={SELECT: 0}
							endif
						endif
						if widget_info( (*pstate).id[x.z[q[i]],(*pstate).tiny], /valid_id) then begin
							widget_control, (*pstate).id[x.z[q[i]],(*pstate).tiny], set_value={SELECT: x.state[q[i]], ALT:alt[i]}
						endif
					endfor
					(*pstate).last_id = (*pstate).id[x.z[0],(*pstate).tiny]
				endif
				end
			else:
		endcase
	endfor
endif

finish:
	return
end

;-----------------------------------------------------------

pro OnRealize_periodic_table_base20, wWidget

geo = widget_info( wWidget, /geometry)

widget_control, wWidget, set_uvalue=[geo.xsize,geo.ysize]
end

;-----------------------------------------------------------

pro OnRealize_periodic_table_base21, wWidget

geo = widget_info( wWidget, /geometry)

widget_control, wWidget, set_uvalue=[geo.xsize,geo.ysize]
end

;-------------------------------------------------------------------------

;	Periodic table compound widget
;
;	periodic_table( parent)
;
; Keywords:
;	n_states	number of ON states
;	z_on		atomic numbers to be set ON
;	z_state		state for on elements
;
;	n_alt_states	number of ALT states
;	z_alt		atomic number set to ALT state
;	z_astate	state for ALT elements
;
;	legend		vector of legend strings for ON and ALT states
;
;	/right		enable use of right mouse button to switch to ALT
;			(this is automatic if n_alt_states is specified)
;	/start_Li	only start table at Li
;	/start_Na	only start table at Na
;	/no_REE		do not do REE elements

;	/tiny		for tiny buttons with no labels
;	/exclusive	simple exclusive button action. Only one can be regardless
;			of the number of states
;
;	colours		a vector to set the colour indices for ON and ALT states
;	_extra		these passed onto TLB


function periodic_table, parent, tiny=tiny, no_tiny=no_tiny, exclusive=exclusive, $
			n_states=n_states, n_alt_states=n_alt_states, right=right, $
			z_on=zon, z_state=zstate, z_alt=zalt, z_astate=zaltstate, $
			legend=legend, start_Na=start_Na, start_Li=start_Li, $
			no_ree=no_ree, colours=colours, _extra=extra

if n_elements(tiny) lt 1 then tiny=0
if n_elements(no_tiny) lt 1 then no_tiny=0
if n_elements(exclusive) lt 1 then exclusive=0
if n_elements(n_states) lt 1 then n_states=2
if n_elements(right) lt 1 then right=0
if n_elements(n_alt_states) gt 0 then right=1
if n_elements(n_alt_states) lt 1 then n_alt_states=n_states
if no_tiny then tiny = 0

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
;									select: 0		1	 2	   3	 4	   5	 6	  7	    8	  9
if n_elements(colours) lt 1 then colours = [grey,green,yellow,red,lblue,orange,white,dgreen,blue,violet]

zinitial = intarr(104)
n = n_elements(zon)
if exclusive then n=1
if n_elements(zon) ge 1 then begin
	if n_elements(zstate) eq n_elements(zon) then begin
		zinitial[zon[0:n-1]] = zstate[0:n-1]
	endif else begin
		zinitial[zon[0:n-1]] = 1
	endelse
endif

q = where(zinitial ne 0)
if (q[0] eq -1) or (exclusive eq 0) then begin
	n = n_elements(zalt)
	if exclusive then n=1
	if n_elements(zalt) ge 1 then begin
		if n_elements(zaltstate) eq n_elements(zalt) then begin
			zinitial[zalt[0:n-1]] = n_states + zaltstate[0:n-1]
		endif else begin
			zinitial[zalt[0:n-1]] = n_states + 1
		endelse
	endif
endif
alt = intarr(104)
q = where(zinitial gt n_states)
if (q[0] ne -1) then begin
	alt[q] = 1
endif

if n_elements(start_Na) lt 1 then start_Na=0
if n_elements(start_Li) lt 1 then start_Li=0
if n_elements(no_ree) lt 1 then no_ree=0
do_legend = 0
n_legend = 0
if n_elements(legend) ge 1 then begin
	do_legend = 1
	n_legend = n_states
	if right then n_legend = n_states + n_alt_states
	if n_elements(legend) lt n_legend then legend = [legend,replicate('',n_legend-n_elements(legend))]
endif

case !version.os_family of
	'MacOS': begin
		fnt1 = 'HELVETICA*9'
		fnt2 = 'HELVETICA*BOLD*11'
		spc = 0
		spc_blank = 22
		xs1 = 22		; normal
		xse = 6
		ys1 = 18
		x21 = 10
		xs2 = 9			; tiny
		ys2 = 9
		x22 = 1
		end
	'unix': begin
		fnt1 = '6x12'
		fnt2 = '7x14'
		spc = 1
		spc_blank = 18
		xs1 = 22		; normal
		xse = 0
		ys1 = 18
		x21 = 10
		xs2 = 9			; tiny
		ys2 = 9
		x22 = 1
		end
	else: begin
		fnt1 = 'ARIAL*12'
		fnt2 = 'ARIAL*14'
		spc = 1
		spc_blank = 18
		xs1 = 22		; normal
		xse = 0
		ys1 = 18
		x21 = 10
		xs2 = 9			; tiny
		ys2 = 9
		x22 = 1
		end
endcase

fnt1 = fnt2

on = [[ 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ], $
      [ 1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1 ], $
      [ 1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1 ], $
      [ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ], $
      [ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ], $
      [ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ], $
      [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ], $
      [ 0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0 ], $
      [ 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0 ]]

j_start = 0
j_end = 8
z_start = 0
n_rows = 9
j_legend = 1
if start_Na then begin
	j_start = 2
	z_start = 10
	n_rows = 7
	j_legend = 2
endif
if start_Li then begin
	j_start = 1
	z_start = 2
	n_rows = 8
	j_legend = 1
endif
if no_ree then begin
	n_rows = 5
	j_end = 5
endif

tlb = widget_base( parent, /column, xpad=0, ypad=0, $
			space=0, _extra=extra, event_func='periodic_table_event', $
			pro_set_value='periodic_table_set_value')

interbase = widget_base( tlb)
base2 = lonarr(2)
base2[0] = widget_base( interbase, row=n_rows, xpad=0, ypad=0, space=spc, $
		notify_realize='OnRealize_periodic_table_base20')
base2[1] = widget_base( interbase, row=n_rows, xpad=0, ypad=0, space=spc, $
		notify_realize='OnRealize_periodic_table_base21')

xs = [xs1,xs2]
ys = [ys1,ys2]
x2 = [x21,x22]
id = lonarr(110,2)
blank = lonarr(64)
track = lonarr(2)

allow = 1-no_tiny					; do we allow changes to tiny buttons and fonts.
for itiny=0,allow do begin
z = z_start
k = 0
m = 0
  for j=j_start,j_end do begin		; do table rows
	for i=0L,17 do begin			; do columns within rows
		if on[i,j] then begin		; only cells that are ON
			z = z+1
			if z eq 72 then z=88	; jump from Lu (71) to Ra (88)
			if z eq 58 then z=72	; jump from La (57) to Hf (72)
			if z eq 87 then z=58	; jump from Rn (86) to Ce (58)
			el = element_name(z)
			if itiny eq 0 then begin
				id[z,itiny] = state_button( base2[itiny], xsize=xs[itiny], ysize=ys[itiny], $
					value=el[0], uvalue=z, uname='PERIODIC', font=fnt1, $
					select=zinitial[z], alt=alt[z], n_states=n_states, right=right, $
					n_alt_states=n_alt_states, colours=colours )
			endif else begin
				id[z,itiny] = state_button( base2[itiny], xsize=xs[itiny], ysize=ys[itiny], $
					value=' ', uvalue=z, uname='PERIODIC', /tracking, $
					select=zinitial[z], alt=alt[z], n_states=n_states, right=right, $
					n_alt_states=n_alt_states, colours=colours )
			endelse
		endif else begin
			if itiny eq 0 then begin
				y = ys[itiny]
				if j eq 6 then y=1
				OK = (j eq j_legend) and (i ge 2) and (i-2 lt n_legend)
				if OK then OK = OK and (lenchr(legend[i-2]) gt 0)
				if OK and do_legend then begin
					blank[k] = state_button( base2[itiny], xsize=xs[itiny], ysize=ys[itiny], $
						value=legend[i-2], uvalue=0, uname='LEGEND', font=fnt1, $
						select=i-2, n_states=n_states, /silent, /right, $
						n_alt_states=n_alt_states, colours=colours )
				endif else begin
					blank[k] = widget_base( base2[itiny], xsize=xs[itiny]+xse, ysize=y )
				endelse
				k = k+1
			endif else begin
				if (i eq 17) and (j eq 6) then begin
					track[itiny] = widget_label( base2[itiny], scr_xsize=18, $
						scr_ysize=14, font=fnt2, value=' ')
				endif else if (i eq 16) and (j eq 6) then begin
					blank[k] = widget_base( base2[itiny], scr_xsize=x2[itiny], scr_ysize=14)
					k = k+1
				endif else begin
					y = ys[itiny]
					if j eq 6 then y=14
					blank[k] = widget_base( base2[itiny], xsize=xs[itiny], ysize=y )
					k = k+1
				endelse
			endelse
		endelse
	endfor
  endfor
endfor

widget_control, base2[tiny], map=1
widget_control, base2[1-tiny], map=0

state = { 	id:id, $					; widget ID's of all element buttons
			last_id: 0L, $				; ID of last/active button
			exclusive: exclusive, $		; are elements mutually exclusive
			track:track, $				; id's of buttons
			on: on, $
			blank: blank, $				; id's of blank objects
			small_font:fnt1, $
			big_font:fnt2, $			; fonts
			base2: base2, $				; IDs of map bases
			tiny: tiny, $				; flags small size buttons
			allow: allow, $				; allow size change
			xs:xs, $
			ys:ys, $
			x2:x2 $						; sizes
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
return, tlb
end
