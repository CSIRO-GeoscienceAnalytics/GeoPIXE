pro blog_read_next, ps, p, tags, tag_select, timer_on=timer_on

; Read next record from socket *ps and add it to the array of
; pointers '*p'. Don't add to p if it has >100 elements.

	if ptr_valid(ps) eq 0 then return
	if (*ps).open eq 0 then return
	
	case !version.os_family of
    	'MacOS': begin
			timeout_msg = "Resource temporarily unavailable"
			end
		'unix': begin
			timeout_msg = "Resource temporarily unavailable"
			end
		else: begin
			timeout_msg = "Connection timed out"
			end
	endcase
	
	nt = n_elements(tags)
	q = where( tag_select eq 1, nq)
	if nq eq 0 then return
	
	n_header = 32
	n_sub_head = 36
	index_mask = '000000E0'x
	index_offset = -5
	error_mask = 'FFFFFFF000000000'x		; error bits in "dwell/readout"
	error_offset = -40
	duration_mask = '0000000FFFFFFFFF'x		; 32 bits plus 8 bits of next 32-bit word

	desired = uint(q)
	sendnext = 7US
	request = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:uint(2*nq), desired:desired }

	swap_bytes, request, /big_endian_data

	on_ioerror, bad_write
	writeu, (*ps).unit, request

; This definition is also in "read_maia" used in blog-browse ...

	header = {aa:0B, tag:0US, bb:0B, len:0US, prevlen:0US, $
			seq:0UL, tseq:0UL, tv_sec:0UL, tv_usec:0UL, $
			client:0UL, unused:0UL}
	sub_header = { X:0UL, Y:0UL, Z:0UL, trigger:0UL, duration:0ULL, flux1:0L, flux2:0L, count:0L }
	
	on_ioerror, bad_read
	readu, (*ps).unit, header
	
	swap_bytes, header, /big_endian_data
	
	if (header.aa ne 'AA'xub) or (header.bb ne 'BB'xub) then goto, bad_read0
	 
	zero_sub_header = { X:0UL, Y:0UL, Z:0UL, trigger:0UL, duration:0ULL, error:0UL, count:0UL, flux1:0L, flux2:0L }
	state = {x:0, y:0, enable:replicate(1B,384), time:0 }
	n_bytes = 64*1024L
	record = {tag:header.tag, length:header.len, prev_length:header.prevlen, seq:header.seq, $
			tseq:header.tseq, tv_sec:header.tv_sec, tv_us:header.tv_usec, client:header.client, $
			b:ptr_new(), state:state, file:'', index:0UL, ptr:0UL, sub_header:zero_sub_header, $
			accum_index:0}

	nbytes = header.len
	if nbytes lt 1 then goto, done
	payload = bytarr(nbytes)
	if (header.tag lt 35) or (header.tag gt 41) then goto, reread3
	 
reread2:
	on_ioerror, bad_read2
	readu, (*ps).unit, sub_header
	
	swap_bytes, sub_header, /big_endian_data
	
	record.sub_header.X = sub_header.X
	record.sub_header.Y = sub_header.Y
	record.sub_header.Z = sub_header.Z
	record.sub_header.trigger = sub_header.trigger
	record.sub_header.duration = sub_header.duration and duration_mask
	record.sub_header.error = ulong( ishft( ulong64(sub_header.duration) and error_mask, error_offset))
	record.sub_header.flux1 = sub_header.flux1
	record.sub_header.flux2 = sub_header.flux2
	record.sub_header.count = sub_header.count
	
	nbytes = (4*sub_header.count) < (64 * 1024UL - 1)
	payload = bytarr(nbytes)

reread3:
	on_ioerror, bad_read3
	readu, (*ps).unit, payload
	
	record.b = ptr_new( payload)
	record.length = nbytes
	record.accum_index = ishft( sub_header.trigger and index_mask, index_offset)

done:
	if ptr_valid( p ) then begin
		if n_elements(*p) eq 0 then begin
			*p = ptr_new(record, /no_copy)
		endif else if n_elements(*p) lt 300 then begin
			if size(*p,/tname) ne 'POINTER' then begin
				*p = ptr_new(record, /no_copy)
			endif else begin
				*p = [*p, ptr_new(record, /no_copy)]
			endelse
		endif else timer_on=0
	endif
	return
	
bad_write:
	print,'blog_read_next',' error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then return
	warning,'blog_read_next',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
	print,'blog_read_next',' error reading socket, re-open socket'
	goto, retry
bad_read0:
	if !error_state.sys_msg eq timeout_msg then goto, reread3
	print,'blog_read_next',' error reading socket 0 (bad "AA BB"), re-open socket'
	goto, retry
bad_read2:
	if !error_state.sys_msg eq timeout_msg then goto, reread2
	print,'blog_read_next',' error reading socket 2, re-open socket'
	goto, retry
bad_read3:
	if !error_state.sys_msg eq timeout_msg then goto, reread3
	print,'blog_read_next',' error reading socket 3, re-open socket'
	goto, retry
	
retry:
	print,'retry socket open ...'
	socket_retry, ps, error=error
	if error then begin
		close_file,(*ps).unit					; close socket
	endif
	return
end
	