function falconx_version, json, tick=tick

;	Determine 'silist' file version from enclosed JSON.
;	Also look for digital sampling rate (MHz) and return the clock tick value (ms).
;
;	Versions will return as:
;		For AS Version 0.0 case (SI formats, no merged detectors):
;			810			0.8.1
;			900			0.9.0
;		For a new AS Version 1.0 case
;			100,810		0.8.1 base format plus merged detector options
;			100,900		0.9.0 base format plus merged detector options

COMPILE_OPT STRICTARR

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'falconx_version',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, 999999L
    endif
endif

	if n_elements(tick) eq 0 then tick = 4.0e-6

	v = 0			; 999999L
	if n_elements(json) eq 0 then return, v
	
	if (typename(json) eq 'STRING') then begin
		tson = json_parse(json)
	endif else return, v

	if (typename(tson) eq 'ORDEREDHASH') or (typename(tson) eq 'HASH') then begin
		keys = tson.keys()
		q = where( keys eq "List Mode Version", nq)
		if nq ge 1 then begin
			t = tson["List Mode Version"]	
		endif else begin
			t = tson["instrument.firmwareVersion"]	
		endelse

		if typename(t) eq 'STRING' then begin
			s = strsplit( t, '._', /extract, count=ns)
			v = long2(s[ns-1])
			if ns gt 1 then begin
				scale = 100L
				for i=ns-2,0,-1 do begin
					v = v + scale*long2(s[i])
					scale = scale*100
				endfor
			endif
		endif else v=long2(t)

		q = where( keys eq "List Mode AS Version", nq)
		if nq ge 1 then begin
			t2 = tson["List Mode AS Version"]	
			v2 = 0
			if typename(t2) eq 'STRING' then begin
				s = strsplit( t2, '._', /extract, count=ns)
				v2 = long2(s[ns-1])
				if ns gt 1 then begin
					scale = 100L
					for i=ns-2,0,-1 do begin
						v2 = v2 + scale*long2(s[i])
						scale = scale*100L
					endfor
				endif
			endif else v2=long2(t2)
			if v2 gt 0 then v = v2*1000L + v
		endif else t2=''

		t3 = tson["afe.sampleRate"]		

		v3 = long2(t3)
		tick = 1.0e-3 / v3
		print,'Found sample rate ',v3,', for "tick" = ',tick
	endif
	print,'falconx_version: ',v,'  ('+t2+' '+t+')'
	return, v
end
