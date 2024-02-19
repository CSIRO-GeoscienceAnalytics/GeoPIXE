pro hopg_filter_event, event

COMPILE_OPT STRICTARR
compile_opt idl2

common c_xray, energy, relint, xray_OK
common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK

;eventName = Tag_Names(event, /Structure_Name)
;If eventName ne 'widget_button' then return
;Only exit? widget if press cancel or accept!

widget_control, event.top, get_uvalue=p

uname = widget_info( event.id, /uname)
case uname of

'hopg_filter_TLB': begin
	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
		widget_control, event.top, /destroy
		return
	endif
	end

'cancel': begin
	(*p).cancel = 1
	widget_control, event.top, /destroy
	goto, done
	end

'Eopt': begin
	widget_control, (*p).Eopt_field, get_value=x
	(*p).hopg.Eopt = x
	end

'amplitude': begin
	widget_control, (*p).a0_field, get_value=x
	(*p).hopg.a0 = x
	end

'width_below': begin
	widget_control, (*p).a2a_field, get_value=x
	(*p).hopg.a2a = x
	end

'width_above': begin
	widget_control, (*p).a2b_field, get_value=x
	(*p).hopg.a2b = x
	end

'base_line': begin
	widget_control, (*p).a3_field, get_value=x
	(*p).hopg.a3 = x
	end

'base_ramp': begin
	widget_control, (*p).a4_field, get_value=x
	(*p).hopg.a4 = x
	end

'accept': begin
	widget_control, (*p).Eopt_field, get_value=x
	(*p).hopg.Eopt = x
	widget_control, (*p).a0_field, get_value=x
	(*p).hopg.a0 = x
	widget_control, (*p).a2a_field, get_value=x
	(*p).hopg.a2a = x
	widget_control, (*p).a2b_field, get_value=x
	(*p).hopg.a2b = x
	widget_control, (*p).a3_field, get_value=x
	(*p).hopg.a3 = x
	widget_control, (*p).a4_field, get_value=x
	(*p).hopg.a4 = x

	widget_control, event.top, /destroy
	return
	end
endcase

	print, (*p).hopg
	wset, (*p).win_id
	plot_hopg_filter, (*p).hopg

done:
end

;-----------------------------------------------------------------

pro plot_hopg_filter, hopg

	energy = findgen(10000)*0.002
	q = where( (energy gt (hopg.Eopt-hopg.a2a*5.)) and (energy lt (hopg.Eopt+hopg.a2b*5.)))
	energy = energy[q]
	gauss = hopg_function( hopg, energy)
	plot, energy, gauss, psym=3   ;linestyle=1				;psym=2 also nice

return
end

;-----------------------------------------------------------------

pro OnRealize_hopg_filter, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
widget_control, top, get_uvalue=p

widget_control, wWidget, get_value=wid
(*p).win_id = wid
plot_hopg_filter, (*p).hopg
end

;---------------------------------------------------------------------------------------------------

function hopg_filter, group, hopg=hopg

;use hopg as a vehicle to return hopg Gaussian parameters?
;pointer???

COMPILE_OPT STRICTARR
compile_opt idl2

if n_elements(group) lt 1 then begin
	group = 0L
	no_block = 1
	modal = 0
	floating = 0
endif else begin
	no_block = 0
	modal = 1
	floating = 1
endelse

	make_hopg = 0
	if n_elements(hopg) ge 1 then begin
		if hopg.Eopt lt 0.001 then make_hopg=1
	endif else make_hopg = 1
	if make_hopg then begin
		hopg = {  $
			Eopt: 	9.7467, $
			a0: 	3.986, $
			a2a: 	0.8, $
			a2b: 	1.4, $
			a3: 	0.046, $
			a4: 	0.0 }
	endif

		hopg_tlb = widget_base(title='HOPG details', /TLB_KILL_REQUEST_EVENTS, /column, uname='hopg_filter_TLB', $
						group_leader=group, modal=modal, floating=floating)
		hopg_draw = widget_draw(hopg_tlb, xsize=400, ysize=200, notify_realize='OnRealize_hopg_filter')
		hopg_row1 = widget_base(hopg_tlb, /row, /align_center, ypad=0, xpad=0, space=5)
		hopg_row2 = widget_base(hopg_tlb, /row, /align_center, ypad=0, xpad=0, space=5)
		Eopt_field = cw_field( hopg_row1, title = ' Eopt:', value=hopg.Eopt, xsize=7, /return_events, $
					/floating, uname='Eopt')
		a0_field = cw_field( hopg_row1, title = 'amplitude', value=hopg.a0, xsize=7, /return_events, $
					/floating, uname='amplitude')
		a2a_field = cw_field( hopg_row1, title = 'width below:', value=hopg.a2a, xsize=7, /return_events, $
					/floating, uname='width_below')
		a2b_field = cw_field( hopg_row2, title = 'width above:', value=hopg.a2b, xsize=7, /return_events, $
					/floating, uname='width_above')
		a3_field = cw_field( hopg_row2, title = 'base line:', value=hopg.a3, xsize=7, /return_events, $
					/floating, uname='base_line')
		a4_field = cw_field( hopg_row2, title = 'base ramp:', value=hopg.a4, xsize=7, /return_events, $
					/floating, uname='base_ramp')

		butbase = widget_base(hopg_tlb, row=1)
		cancel=widget_button(butbase, Value='Cancel', uname='cancel')
		accept=widget_button(butbase, Value='Accept', uname='accept')

		pstate = ptr_new(	{	hopg:hopg, $
					cancel:		0, $
					eopt_field:	eopt_field, $
					a0_field:	a0_field, $
					a2a_field:	a2a_field, $
					a2b_field:	a2b_field, $
					a3_field:	a3_field, $
					a4_field:	a4_field, $
					win_id:		0L } )

		widget_control, hopg_tlb, set_uvalue=pstate
		widget_control, hopg_tlb, /realize

		xmanager, 'hopg_filter', hopg_tlb, event_handler='hopg_filter_event', $
					no_block=no_block

		bragg = { hopg:(*pstate).hopg, cancel:(*pstate).cancel}
		if no_block eq 0 then ptr_free, pstate

	return, bragg
end