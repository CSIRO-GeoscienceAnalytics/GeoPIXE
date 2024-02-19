function make_cuts, el, low, high, blow=blow, bhigh=bhigh, units=units, file=file

;	Make a cuts array, and return pointer to it.

	if n_elements(blow) lt 1 then blow = -1.0
	if n_elements(bhigh) lt 1 then bhigh = -1.0
	if n_elements(units) lt 1 then units = ''
	if n_elements(file) lt 1 then file = ''

	n = n_elements(low)
	cuts = replicate( define(/cuts), n)

	cuts.el = el
	cuts.low = low
	cuts.high = high
	cuts.blow = blow
	cuts.bhigh = bhigh
	cuts.units = units
	cuts.file = file

	p = ptr_new(cuts, /no_copy)

	return, p
	end
