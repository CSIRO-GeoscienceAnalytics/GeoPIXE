;
;	lund_charge setup and execution.

pro lund_charge_event, event

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
		warning,'lund_charge_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
  widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					print,'lund_charge Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			else:
		endcase
		end
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				widget_control, (*pstate).help, set_value=' '
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request lund_charge ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'load-file-button': begin
		file = find_file2( (*p).file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = dialog_pickfile (/read, filter = '*.hist', $
			/noconfirm, /must_exist, path=path, group=event.top, $
			title='Select the charge spectrum HIST file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.hist'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).charge_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).charge_file, set_value=F, set_text_select=[n-1,0]
			(*p).file = F
		endif
		end

	'charge-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.hist'
		(*p).file = F
		*(*pstate).path = extract_path(F)
		n = lenchr(F)
		k = lenchr(strip_path(F))
		widget_control, (*pstate).charge_file, set_value=F, set_text_select=[n-k,k]
		widget_control, (*pstate).charge_file, set_value=F, set_text_select=[n-1,0]
		end

	'charge-mode': begin
		(*p).charge_mode = event.index
		end

	'compress-mode': begin
		(*p).compress_mode = event.index
		end

	'ok-button': begin
		lund_charge_update_pars, pstate

		counts = load_lund_charge( (*p).file)
		q = (*pstate).qvalue[(*p).charge_mode] * float(counts)
		(*p).charge = q
		(*p).ecompress = (*pstate).cvalue[(*p).compress_mode]
;		warning,'lund charge','charge= '+string(q)

		goto, kill
		end

	'cancel-button': begin
		print,'Cancel lund_charge setup ...'
		goto, kill
		end

	else:
endcase

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'lund_charge_event',['STATE variable has become ill-defined.','Abort lund_charge Setup.'],/error
	goto, kill
bad_ptr:
	warning,'lund_charge_event',['Parameter structure variable has become ill-defined.','Abort lund_charge Setup.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro lund_charge_update_pars, pstate

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
		warning,'lund_charge_update_pars',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'STRUCT' then return

	widget_control, (*pstate).charge_file, get_value=F
	(*p).file = F[0]

	return
end

;------------------------------------------------------------------------------------------------------

function load_lund_charge, file

;	Load the file to integrate counts for charge

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
		warning,'load_lund_charge',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

	if n_elements(file) lt 1 then goto, bad

	on_ioerror, bad_file
	close, 2
	openr, 2, file
	on_ioerror, bad_io

	lund_header,header
	READU, 2, header
	swap_bytes, header, /big_endian_data	; swap bytes on a little endian machine

	spectrum = lonarr(header.DataXLength)
	readu, 2, spectrum
	swap_bytes, spectrum, /big_endian_data	; swap bytes on a little endian machine

	counts = total(spectrum[5:*])
;	warning,'load_lund_charge','Total charge counts = '+string(counts)

	close, 2
	return, counts

bad:
	warning,'load_lund_charge','bad input parameters'
	goto, done
bad_io:
	warning,'load_lund_charge','error reading HIST file'
	goto, done
bad_file:
	warning,'load_lund_charge',['error opening HIST file:', file]
	goto, done
done:
	close,2
	return, 0.0
	end

;------------------------------------------------------------------------------------------

pro OnRealize_charge_file, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	F = (*(*pstate).p).file
	n = lenchr(F)
	k = lenchr(strip_path(F))
	widget_control, (*pstate).charge_file, set_value=F, set_text_select=[n-k,k]
	widget_control, (*pstate).charge_file, set_value=F, set_text_select=[n-1,0]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_charge_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).charge_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_compress_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).compress_mode
endif
end

;------------------------------------------------------------------------------------------

function lund_charge, group_leader=group, pars=p, path=path, _extra=extra

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
		warning,'lund_charge',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.0
	endif
endif

if n_elements(group) lt 1 then return, {charge:0.0, ecompress:1 }
if n_elements(path) lt 1 then path=''

;modal = 0
;floating = 0
;if group ne 0 then return, {charge:0.0, ecompress:1 }
modal = 1
floating = 1
if group eq 0 then return, {charge:0.0, ecompress:1 }

screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = (screen[0]-34)/2 - 100
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = (screen[1]-28)/2 - 60
endif

p = bad_pars_struct( p, make_pars=make_p)

if make_p then begin
	pars = {	$
			file:				path+'ADC6.hist', $		; charge file
			charge_mode:		0,  $					; charge factor mode
			compress_mode:		0,  $					; compression factor mode
			ecompress:			1L, $					; e compression factor
			charge:				0.0  $					; total charge
		}
	*p = pars
endif

(*p).charge = 0.0

; 	top-level base

tlb = widget_base( /column, title='Lund Charge Spectrum Select', /TLB_KILL_REQUEST_EVENTS, modal=modal, floating=floating, xpad=10, ypad=10, space=5, $
					group_leader=group, _extra=extra, uname='lund_charge_TLB', xoffset=xoffset, yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; set-up file droplist and buttons

cbase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( cbase, value='Charge Spectrum:')
charge_file = widget_text( cbase, value=(*p).file, uname='charge-file', /tracking, /editable, $
					notify_realize='OnRealize_charge_file', $
					uvalue='Enter the name of a Lund HIST spectrum file to integrate for total charge counts.',scr_xsize=200)
load_file_button = widget_button( cbase, value='Load', uname='load-file-button', /tracking, $
					uvalue='Load a Lund HIST spectrum file to integrate for total charge counts.', scr_xsize=38)

qbase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)

lab = widget_label( qbase, value='Compression:', /align_center)
cstring = ['8192   (/1)','4096  (/2)','2048   (/4)','1024   (/8)','512   (/16)','256   (/32)','128   (/64)']
cvalue = long([1,2,4,8,16,32,64])
compress_mode = widget_combobox( qbase, value=cstring, uname='compress-mode', /tracking, $
					notify_realize='OnRealize_compress_mode', $
					uvalue='Select the ADC range (compression factor) used for this HIST file.', xsize=84)

lab = widget_label( qbase, value='Charge per count:', /align_center)
qstring = ['0.1 pC','1 pC','10 pC','100 pC','1 nC','10 nC','100 nC','1 uC']
qvalue = [1.0e-7,1.0e-6,1.0e-5,1.0e-4,1.0e-3,1.0e-2,1.0e-1,1.0]
charge_mode = widget_combobox( qbase, value=qstring, uname='charge-mode', /tracking, $
					notify_realize='OnRealize_charge_mode', $
					uvalue='Select the charge per ADC count.', xsize=84)

; Buttons

bbase = widget_base( tbase, /row, /base_align_center, ypad=0, space=15)
button = widget_button( bbase, value='  OK  ', uname='ok-button', /tracking, $
					uvalue='Integrate the selected Lund HIST file for total counts. ' + $
					'Scale this by the "charge per count" to determine total charge.')
lab = widget_label( bbase, value='                         ')
button = widget_button( bbase, value=' Cancel ', uname='cancel-button', /tracking, $
					uvalue='Exit Lund charge set-up, and return zero charge.')

;.................................................................................

help = widget_text( tbase, scr_xsize=335, ysize=2, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:			ptr_new(path), $		; pointer to current path
		p:				p, $					; pointer to parameters
		charge_file:	charge_file, $			; ID of file text widget
		charge_mode:	charge_mode, $			; ID of charge-mode droplist
		qvalue:			qvalue, $				; charge scale values
		compress_mode:	compress_mode, $		; ID of compresse-mode droplist
		cvalue:			cvalue, $				; compression values

;		lund_charge_base:		lund_charge_base, $			; ID array of map bases for lund_charges
		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path'], $				; new path
					from=group

xmanager, 'lund_charge', tlb, no_block=0

return, {charge:(*p).charge, ecompress:(*p).ecompress }
end
