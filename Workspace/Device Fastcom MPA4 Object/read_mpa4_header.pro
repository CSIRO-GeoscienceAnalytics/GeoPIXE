function read_mpa4_header, unit, error=error

	error = 0
	on_ioerror, bad_io

	ADCEnabled = intarr(16)			; ADC is in use
	range = intarr(16)				; range of ADC
	ADCid = strarr(16)				; ID code for MPAWIN channel
	cal0 = { use:	0, $			; Cal in use
			unit:	'', $			; Units
			poly:	fltarr(4)}		; Polynomial
	cal = replicate( cal0, 16)

	adcmode = 0
	rtcuse = 0
	EndHeader = 0
	format = ''
	s = ''

	while EndHeader eq 0 do begin
		readf, unit, s
		if strmid(s,0,1) eq "[" then begin
			adcmode = 0
			if strmid(s,0,4) eq "[ADC" then begin
				str = strsplit( s, '[ADC ]', /extract)
				i = long(str[0])
				if (i ge 1) and (i le 16) then begin
					adcmode = 1
					print,'ADC',i,' found'
				endif
			endif else if (s eq "[LISTDATA]") or (s eq "[DATA]") then begin
				EndHeader = 1
			endif
		endif

		if adcmode eq 1 then begin
			sub = strsplit( s, '=', /extract, count=n_sub)
			if n_sub ge 2 then begin
				case sub[0] of
					'active' : begin
						j = fix(sub[1])
						if ( j and 'ff'xus ) ne 0 then begin
							ADCEnabled[i-1]=1
							print,'ADC',i,' active= ',j
						endif
						end
					'range': begin
						range[i-1] = fix(sub[1])
						print,'            range= ',range[i-1]
						end
					'caluse': begin
						cal[i-1].use = fix(sub[1])
						end
					'calunit': begin
						cal[i-1].unit = sub[1]
						end
					'caloff': begin
						cal[i-1].poly[0] = float(sub[1])
						end
					'calfact': begin
						cal[i-1].poly[1] = float(sub[1])
						end
					'calfact2': begin
						cal[i-1].poly[2] = float(sub[1])
						end
					'calfact3': begin
						cal[i-1].poly[3] = float(sub[1])
						end
					'rtcuse': begin
						RTCuse = fix(sub[1])
						end
					'cmline1': begin
						ADCid[i-1] = strtrim(sub[1],2)
						end
					else:
				endcase
			endif
		endif else begin
			sub = strsplit( s, '=', /extract, count=n_sub)
			if n_sub ge 2 then begin
				case sub[0] of
					'mpafmt': begin
						format = sub[1]
						end
					else:
				endcase
			endif
		endelse
	endwhile

	maxADCs = long(total(ADCEnabled))		; max( where(ADCEnabled eq 1) + 1)
	ADCpntr = replicate(255US, 16)
	j=0

	;Set up the pointer to the active ADCs

	for i=0L,15 do begin
		if ADCEnabled[i] ne 0 then begin
			ADCpntr[i] = j
			j = j+1
		endif
	endfor
	stat = fstat(unit)

	head = { ADCenabled:	ADCenabled, $		; Is each of 16 ADCs on
			ADCid:			ADCid, $			; ID code for ADC channel
			ADCpntr:		ADCpntr, $			; Lookup table from active ADC # to ADC index
			n_ADCs:			maxADcs, $			; number of ADCs active
			RTCuse:			RTCuse, $			; Realtime clock in use
			file:			stat.name, $		; File name
			bytes:			stat.cur_ptr, $		; number of bytes in header
			size:			stat.size, $		; Size of file
			format:			format, $			; format of LIST data
			range:			range, $			; Range of each of 16 ADCs
			cal:			cal }				; 16 Cals

	return, head

bad_io:
	error = 1
	return, 0
	end