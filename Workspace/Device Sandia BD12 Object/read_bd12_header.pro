function read_bd12_header, unit, error=error

	cal0 = { use:	0L, $			; Cal in use
			poly:	fltarr(2)}		; Polynomial
	cal = replicate( cal0, 12)

	head = { version:		0L, $
			n_ADCs:			12, $				; number of ADCs
			ADCenabled:		4095, $				; Is each of 12 ADCs on
			stepsX:			64, $				; number of steps in X
			stepsY:			64, $				; number of steps in X
			incX:			32, $				; inc of each step
			incY:			32, $				; inc of each step
			cal:			cal }				; 16 Cals

	error = 0
	on_ioerror, bad_io

	version = 0L
	readu, unit, version
	head.version = version

	valid = [-1L]
	q = where( version eq valid)
	if q[0] eq -1 then begin
		point_lun, unit, 0
		stat = fstat(unit)
		return, head
	endif

	stepsX = 0
	stepsY = 0
	readu, unit, stepsX, stepsY
	head.stepsX = stepsX
	head.stepsY = stepsY

	incX = 0
	incY = 0
	readu, unit, incX, incY
	head.incX = incX
	head.incY = incY

	n_ADCs = 0
	readu, unit, n_ADCs
	head.n_ADCs = n_ADCs
	if n_ADCs ne 12 then begin
		warning, 'readBD12_header','number of detectors not 12.'
	endif

	ADCEnabled = 0						; ADC is in use
	readu, unit, ADCEnabled
	head.ADCEnabled = ADCEnabled

	for i=0L,11 do begin
		readu, unit, cal0
		head.cal[i] = cal0
	endfor

	stat = fstat(unit)
	return, head

bad_io:
	error = 1
	return, 0
	end