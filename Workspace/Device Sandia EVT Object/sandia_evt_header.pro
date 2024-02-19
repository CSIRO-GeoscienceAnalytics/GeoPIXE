function sandia_evt_header, file, error=error

;	Read Sandia EVT 'trailer' data
;	Must read all data first - use 'read_buffer'

common c_sandia_1, evt_data, evt_trailer
common c_sandia_2, evt_xrange, evt_yrange

	head = {title:'', xrange:0L, yrange:0L}
	error = 1

	if n_elements(file) lt 1 then return, head
	on_ioerror, bad_file
	close,1
	openr, 1, file, bufsiz=1500*1024L
	on_ioerror, bad_read
	unit = 1
	
		stat = fstat(unit)
		recnum = (stat.size - 630)/8 - 1
		if (stat.open eq 0) or (recnum le 0) then begin
			warning,'sandia_evt_header',['Error reading file, or','zero data records in EVT file.']
			error = 1
			return, 1
		endif

		evt_data = replicate( {x:0, y:0, e:0, t:0}, recnum)

		evt_trailer = {	ones: intarr(4), $			; -1's
						skip1: 0B, $				; skip 1 byte
						Title: bytarr(104), $		; header (in byte format)
						Ion: bytarr(25), $			; E, Ion
						Fluence: bytarr(25), $		; Fluence
						Gains: bytarr(25), $		; Gains
						Shape: bytarr(25), $		; Shaping
						Dwell: bytarr(25), $		; Dwell time
						Scans: bytarr(25), $		; # of scans
						Clock: bytarr(25), $		; clock time
						skip2: 0B, $				; skip 1 byte
						IC: bytarr(25), $			; IC
						Bias: bytarr(25), $			; Bias
						Logic: bytarr(25), $		; Logic
						Xroi: bytarr(25), $			; X ROI
						Yroi: bytarr(25), $			; Y ROI
						Zroi: bytarr(25), $			; Z ROI
						Troi: bytarr(25), $			; t ROI
						Events: bytarr(25) }		; Events

;						Spare1: bytarr(25), $		;
;						Spare2: bytarr(25), $		;
;						Spare3: bytarr(25), $		;
;						Spare4: bytarr(25), $		;
;						Spare5: bytarr(25), $		;
;						Spare6: bytarr(24), $		;

       readu, unit, evt_data
       swap_bytes, evt_data, big_endian_data=0

       readu, unit, evt_trailer
       swap_bytes, evt_trailer, big_endian_data=0

       evt_xrange = 0
       set_separators, [':']
       chop_string, string(evt_trailer.Xroi), sub,ns
       if ns ge 2 then begin
         set_separators, ['-',' ']
         chop_string, sub[1], sub,ns
         if ns ge 2 then evt_xrange = clip( long(sub[1]),0,4095)
         if evt_xrange gt 0 then evt_xrange = evt_xrange+1
       endif
       evt_yrange = 0
       set_separators, [':']
       chop_string, string(evt_trailer.Yroi), sub,ns
       if ns ge 2 then begin
         set_separators, ['-',' ']
         chop_string, sub[1], sub,ns
         if ns ge 2 then evt_yrange = clip( long(sub[1]),0,4095)
         if evt_yrange gt 0 then evt_yrange = evt_yrange+1
       endif
       n = n_elements(evt_data.e)

       title = string(evt_trailer.title)
       scan_time = evt_data[n-1].t - evt_data[0].t + 1
       title = strtrim(title,2) + ', scan time = ' + strtrim(string(scan_time),2)

	head.xrange = evt_xrange
	head.yrange = evt_yrange
	head.title = title
	error = 0

done:
	close, 1
	return, head

bad_file:
	warning,'sandia_evt_header','Error opening EVT file: '+file
	goto, done
bad_read:
	warning,'sandia_evt_header','Error reading EVT file: '+file
	goto, done
end
