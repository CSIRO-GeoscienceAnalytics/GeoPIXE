function read_csv_xrf_table, file, error=error

; Read CSV table of XRF results ...
; ... actually already done as "get_delta_ascii.pro"

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if  n_elements(catch_errors_on) eq 0 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'read_csv_xrf_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

	error = 1
	info = 0
	data = 0
	line = ''
	on_ioerror, bad_file
	openr, unit, file, /get_lun
	
	on_ioerror, bad_io
	first = 1
	while EOF(unit) eq 0 do begin
		readf, unit, line
		if (strlen(line) eq 0) or (extract(line,0,0) eq '#') then continue

		str = strsplit( line, ',', /extract, /preserve_null)
		n = n_elements(str)
		if first then begin
			ncol = n-1
			first = 0
			info = { $
				TestID:			strarr(ncol), $		; ID
				ExposureNum:	intarr(ncol), $		;
				Offset:			fltarr(ncol), $		; Cal B
				Slope:			fltarr(ncol), $		; Cal A
				Livetime:		fltarr(ncol), $		; live time (s)
				Realtime:		fltarr(ncol), $		; real time (s)
				NumData:		intarr(ncol), $		; # channels
				FirstRate:		fltarr(ncol), $		; first count rate
				LastRate:		fltarr(ncol), $		; last count rate
				VacPressure:	intarr(ncol), $		; tube pressure (mbar)
				AmbientPressure: intarr(ncol), $	; ambient pressure (mbar)
				ProbeTemp:		intarr(ncol), $		; Temp (C)
				TubeCurrentSet:	fltarr(ncol), $		; Tube current (uA?)
				TubeCurrentMon:	fltarr(ncol), $		; 
				TubeVoltageSet:	fltarr(ncol), $		; Tube voltage (kV)
				TubeVoltageMon:	fltarr(ncol), $		; 
				LTMult:			intarr(ncol), $		;
				ColMult:		intarr(ncol), $		;
				FilterPosition:	intarr(ncol), $		; filter #
				TimeStamp:		strarr(ncol)}		; time stamp date/time string		
		endif
		
		if str[0] eq '' then goto, do_data
		case str[0] of
			'TestID': begin
				info.TestID = str[1:*]
				end
			'TimeStamp': begin
				info.TimeStamp = str[1:*]
				end

			'ExposureNum': begin
				info.ExposureNum = long2(str[1:*])
				end
			'NumData': begin
				ndata = long2(str[1:*])
				info.ExposureNum = ndata
				end
			'VacPressure': begin
				info.VacPressure = long2(str[1:*])
				end
			'AmbientPressure': begin
				info.AmbientPressure = long2(str[1:*])
				end
			'ProbeTemp': begin
				info.ProbeTemp = long2(str[1:*])
				end
			'LTMult': begin
				info.LTMult = float2(str[1:*])
				end
			'ColMult': begin
				info.ColMult = long2(str[1:*])
				end
			'FilterPosition': begin
				info.FilterPosition = long2(str[1:*])
				end

			'Offset': begin
				info.Offset = float2(str[1:*])
				end
			'Slope': begin
				info.Slope = float2(str[1:*])
				end
			'Livetime': begin
				info.LiveTime = float2(str[1:*])
				end
			'FirstRate': begin
				info.FirstRate = float2(str[1:*])
				end
			'LastRate': begin
				info.LastRate = float2(str[1:*])
				end
			'Realtime': begin
				info.RealTime = float2(str[1:*])
				end
			'TubeVoltageSet': begin
				info.TubeVoltageSet = float2(str[1:*])
				end
			'TubeVoltageMon': begin
				info.TubeVoltageMon = float2(str[1:*])
				end
			'TubeCurrentSet': begin
				info.TubeCurrentSet = float2(str[1:*])
				end
			'TubeCurrentMon': begin
				info.TubeCurrentMon = float2(str[1:*])
				end
			else:
		endcase
	endwhile

do_data:
	data = lonarr(ndata[0],ncol)
	on_ioerror, bad_data
	for i=0,ndata[0]-1 do begin
		data[i,0:ncol-1] = long2(str[1:*])
		if i lt ndata[0]-1 then begin
			readf, unit, line
			str = strsplit( line, ',', /extract, /preserve_null)
		endif
	endfor
	error = 0
	
finish:
	close_file, unit
	return, {info:info, data:data}
	
bad_file:
	warning,'read_csv_xrf_table','error opening file: '+file
	goto, finish
bad_io:
	warning,'read_csv_xrf_table','error during I/O'
	goto, finish
bad_data:
	warning,'read_csv_xrf_table','error read data block'
	goto, finish
end
	