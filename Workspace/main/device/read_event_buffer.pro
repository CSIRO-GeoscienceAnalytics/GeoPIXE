pro read_event_buffer, unit, obj, n_actual, bytes=bytes, $
			use_ascii=use_ascii, ascii_event_array=ascii_event_array

COMPILE_OPT STRICTARR
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

if n_elements(bytes) lt 1 then bytes=4
if n_elements(use_ascii) lt 1 then use_ascii=0

	stat = fstat(unit)
	cur = stat.cur_ptr

	if use_ascii then begin
		on_ioerror, trim
		readf, unit, ascii_event_array
		event_array[*] = hex_jam( ascii_event_array)
		n_actual = n_buffer
		goto, finish
trim:
		q = where( ascii_event_array ne '', nq)
		if nq ge 1 then begin
			event_array[0:nq-1] = hex_jam( ascii_event_array[q])
		endif
		n_actual = nq

	endif else begin
		on_ioerror, more
		readu, unit, event_array
more:
		stat = fstat(unit)										; transfer_count is only 1?, so calculate
		n_actual = ((stat.cur_ptr - cur)/bytes) < n_buffer		; transfer count from change in CUR_PTR
		if n_actual le 0 then return
	endelse

finish:
	if bytes ge 2 then begin
		swap_bytes, event_array, big_endian_data=obj->big_endian()
	endif
	return
end
