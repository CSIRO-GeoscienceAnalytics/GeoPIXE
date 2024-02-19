function spectrum_index, p, mode=mode, number=number

; Examine the spectra *p[] label for "region", then return 'number' after "region".
; else, return channel or station 'number'.
; 
; 'mode' returns spectrum mode (0="/E", 1="/X", 2="/Y", 3="/T")
;
; Returned index is 4*'number'+'mode'.

	COMPILE_OPT STRICTARR

	if ptr_good(p, /struct, /all) eq 0 then return, 0
	np = n_elements(p)
	indx = intarr(np)
	mode = intarr(np)
	number = intarr(np)
	lut = bytarr(256)
	lut[byte('E')] = 0
	lut[byte('X')] = 1
	lut[byte('Y')] = 2
	lut[byte('T')] = 3
	bad = 0
	
;	Spectra label types:
;		import:		
;				long-file-name\999.9 n/E				; E spectrum for detector number 'n'
;				long-file-name\999.9 n/X				; X spectrum
;		Maia control spectra:
;				Maia detector #n /E						; detector number 'n' E spectrum
;				Maia detector #n /T						; detector number 'n' T spectrum
;
;;		region (single, array):
;				Region n, element						; region number 'n', element name
;		region (individual detector):
;				Detector n								; detector number 'n'

	for i=0L,np-1 do begin
		j = locate_last('/',(*p[i]).label)
		if j ge 1 then begin									; found a "/" --> normal import or Maia spectra
			k = locate('#',(*p[i]).label)
			if k ge 1 then begin								; found a "#" --> a Maia channel spectrum
				str = strsplit( (*p[i]).label, '	 ', /extract)
				ns = n_elements(str)
				if ns ge 2 then begin
					if strmid(str[ns-1],0,1) eq '/' then begin
						mode[i] = lut[ byte(strupcase(strmid(str[ns-1],1,1)))]
					endif else bad=1
					if strmid(str[ns-2],0,1) eq '#' then begin
						number[i] = long2(strmid(str[ns-2],1))
						indx[i] = 4*number[i] + mode[i]
					endif else bad=1
				endif else bad=1
			endif else begin									; normal import spectra
				str = strarr(2)
				if j ge 1 then begin
					str[0]=strmid((*p[i]).label,0,j)
				endif
				nl = strlen((*p[i]).label)
				if j lt nl-1 then begin
					str[1]=strmid((*p[i]).label,j+1,nl-j-1)
				endif
				str2 = strsplit( str[0], ' 	', /extract)
				ns2 = n_elements(str2)
				if ns2 ge 2 then begin
					mode[i] = lut[ byte(strupcase(strmid(str[1],0,1)))]
					number[i] = long2(str2[ns2-1])
					indx[i] = 4*number[i] + mode[i]
				endif else bad=1
			endelse
		endif else begin										; no "/" --> region or detector label
			str2 = strsplit( (*p[i]).label, ', 	', /extract)
			ns2 = n_elements(str2)
			if ns2 ge 2 then begin
				tag = strlowcase(str2[0])
				if tag eq 'region' then begin					; region (EVT button singles, "array")
					number[i] = long2( str2[1])
					indx[i] = number[i]
				endif else if tag eq 'detector' then begin		; detector (EVT button "individual")
					number[i] = long2( str2[1])
					indx[i] = number[i]
				endif else bad=1
			endif else bad=1
		endelse
	endfor
	
	if bad then print,'spectrum_index: bad format "label" for index ['+(*p[0]).label+']'
	return, indx
end
