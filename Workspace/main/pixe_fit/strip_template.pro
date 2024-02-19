
function strip_template, pspec, elow, ehigh, passes=passesi

;	Do a background strip, template
;
;	pspec		pointer to spectrum structure (see DEFINE.pro)
;	elow		low energy marker
;	ehigh		high energy
;	passes		number of passes through ??

	if ptr_valid(pspec) eq 0 then goto, bad_ptr
	if n_elements(passesi) lt 1 then passesi = 8

	elow = elow > 0.603							; 0.6 keV minimum (also in pixe_fit)

	spec = *(*pspec).data
	n = n_elements(spec)
	cal_a = (*pspec).cal.poly[1]
	cal_b = (*pspec).cal.poly[0]

;.....................................................................................

;	... do your stuff here on the spectrum data array 'spec' ...

;	... end up in a floating vector called 'back'

;.....................................................................................
;	Put it a fit overlay ...
;	This assumes that 'back' and 'spec' have the same energy calibration.

	fit = define(/spectrum)
	fit.source = (*pspec).source
	fit.label = 'Template'
	fit.cal.poly[0] = cal_b
	fit.cal.poly[1] = cal_a
	fit.cal.units = (*pspec).cal.units
	fit.cal.order = 1
	fit.comment = 'Template background algorithm'

	fit.size = n_elements(back)
	fit.data = ptr_new( back, /no_copy)

	return, ptr_new( fit, /no_copy)

bad_ptr:
	warning, 'strip_template', 'bad spectrum pointer'
	goto, bad_exit

bad_exit:
	return, ptr_new()
	end

;-------------------------------------------------------------------------------------
