function read_midas, unit, n_buffer=n_bufferi, type=next, state=state, n_actual=n_actual, $
					progressbar=do_progress, select=select, skip=skip, accept=accept

; unit		open unit number to read from
; n_actual	actual byte read
; n_buffer	buffer byte length (default=2Mb, unless 'select' then=64kb)
; type		0=event, 1=bank
; state		place current 'state' (XY,time,det enable) in record
; /progressbar show a progress bar
; select	jump to a selected byte pointer in file (must be on record boundary/start)
; skip		skip this number of bytes (approx.) first
; accept	optional list of tags to accept, else all
; /old		old Blog format

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
       warning,'read_midas',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pr
       return, 0L
    endif
endif

if n_elements(n_bufferi) lt 1 then n_bufferi = 200000LL
if n_elements(state) lt 1 then state = {x:0, y:0, enable:replicate(1B,16), time:0 }
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(select) lt 1 then select=-1L
if n_elements(next) lt 1 then next=0
if n_elements(skip) lt 1 then skip=0LL
if n_elements(accept) lt 1 then accept=indgen(1000)
use = intarr(1000)
use[accept] = 1

if select ge 0 then do_progress=0
if do_progress then progress, tlb=progress_tlb, title='Read midas'

;	ev and n_buffer are longs this time ...
;	But n_bufferi as input is bytes.

if select eq -1 then begin
	n_buffer = n_bufferi
	ev = bytarr(n_buffer)
;	n_bytes = 1024UL		; 64*1024UL
	n_bytes = 64*1024UL
endif else begin
	n_buffer = 100*1024L
	point_lun, unit, select
	ev = bytarr(n_buffer)
	n_bytes = 64*1024L		; 64*1024UL
endelse
pr = ptrarr(100000L)
npr = 100000L
ipr = 0L
cancel = 0
skipb = 0

    stat = fstat(unit)
    cur = stat.cur_ptr
    on_ioerror, cont
    readu, unit, ev
cont:
    on_ioerror, NULL

;	The record has been packed for Unix (big endian) byte ordering. To make it
;	read OK for other platforms (e.g. PC), use swap_bytes, recursively.
    
    stat = fstat(unit)									; transfer_count is only 1?, so calculate
    n_actual = ((stat.cur_ptr - cur)) < n_buffer		; transfer count from change in CUR_PTR

    if n_actual ne n_buffer then begin
       print,'n_actual =',n_actual,'  n_buffer =',n_buffer
    endif

;	This list in read_midas, get_midas_details too ... also effects list of tags in main 'blog_browse' routine
;														Hex
;	0	ODB1				ODB begin					8000
;	1	ODB2				ODB end						8001
;	2	NMP					MRD NMP pixel event			1
;	3	ADCx				ADCx bank					101
;	4	DACx				DACx bank					102
;		
;	NOTE: This list in 'fx_browse' too ...
;	See also list of tags in 'read_midas', 'get_midas_details', and 'update_midas_records', 'update_midas_details' too ...

@midas_listmode.def

use[n_data_tags:*] = 0
record = {tag:0L, length:0US, type: 0, prev_length:0UL, b:ptr_new(), state:state, file:'', index:0UL, ptr:0UL }

i = 0L
n_loop = 0L
iprog = 0
nprog = 10L
ltime = systime(/seconds)
if next eq 1 then goto, next_bank

new_block:
	if do_progress and (iprog ge nprog) then begin
		time = systime(/seconds)
		progress, /update, progress_tlb, {unit:0, value:0, current:i, size:n_actual}, cancel=cancel, skip=skipb
		iprog = 0
		t = systime(/seconds)
		ft = t - ltime
		dt = t - time
		if dt gt 0.2*ft then begin
			nprog = nprog*2
			print,'read_midas: Extend progress period to ',nprog
		endif
		ltime = time
	endif
	iprog = iprog+1
	if cancel or skipb then goto, done

;........................................................................
;	The record has been packed for Unix (big endian) byte ordering. To make it
;	read OK for other platforms (e.g. PC), use swap_bytes, recursively.
;	Don't swap state, as this is local data.

;	if i ge 38240 then begin
;		print,'debug ...'
;	endif

	iptr = i
	if (i + 2*size_midas_header_id) ge n_buffer then goto, done
	header = uint( ev, i, size_midas_header_id )		; midas header ID/trigger
	swap_bytes, header, big_endian_data=0
	i = i + 2*size_midas_header_id
	
	tag = header[0] 
	trigger = ulong(header[1]) 
	
	if (i + 4*size_midas_header_body) ge n_buffer then goto, done
	header = ulong( ev, i, size_midas_header_body)		; midas header body
	swap_bytes, header, big_endian_data=0
	i = i + 4*size_midas_header_body
	
	serial = header[0]
	time = header[1]
	length = header[2]
	i_next_event = i + length
	next = 0						; event
	
	q = where( tag eq data_type, nq)
	if nq eq 0 then begin
		print,'read_midas: tag not recognized = ',tag
		goto, more
	endif

	j = q[0]
	use_tag = use[j]
	if use_tag eq 0 then goto, more

	record.tag = j
	record.type = 0					; flags an "event"
	; record.prev_length = uint(ev,i+6,1)
	
	if data_has_bank[j] eq 0 then begin
		nh = min([length,ulong(n_bytes)])
		if nh gt 0 then begin
			b = ev[ i: (i + nh-1) < (n_buffer-1)]
			nb = min([n_elements(b),n_bytes])
			record.b = ptr_new(b[0:nb-1])
			record.length = nb
		endif else begin
			record.length = 0
			record.b = ptr_new(0)
		endelse
		next = 0					; event
		goto, append
	endif
	   
	if (i + 4*size_global_bank_header) ge n_buffer then goto, done
	header = ulong( ev, i, size_global_bank_header )	; global bank header
	swap_bytes, header, big_endian_data=0
	i = i + 4*size_global_bank_header 

	bank_all_size = header[0] 
	bank_flags = header[1]			; indicates 'Bank' or 'Bank32' bank type?
									; assume all are 'Bank' type for now ...
	i_next_bank = i
;	if bank32_type then goto, next_bank32
	
;	print,ipr,i,tag,length,bank_all_size
	
	record.b = ptr_new([trigger, serial, time, bank_flags])
	record.length = length			; 4*4

	next = 1						; bank
	goto, append
	
next_bank:	
	iptr = i
	if (i + 4*size_bank_header_name) ge n_buffer then goto, done
	header = ulong( ev, i, size_bank_header_name )		; bank header name only
	swap_bytes, header, big_endian_data=0
	i = i + 4*size_bank_header_name

	bank_name = string(byte( header, 0, 4))

	if (i + 2*size_bank_header_body) ge n_buffer then goto, done
	header = uint( ev, i, size_bank_header_body )		; bank header body only
	swap_bytes, header, big_endian_data=0
	i = i + 2*size_bank_header_body

	bank_type = header[0]
	bank_size = header[1]
	i_next_bank = i + long( 8 * ceil( bank_size / 8.))

	q = where( bank_name eq data_tags, nq)
	if nq eq 0 then goto, more
	j = q[0]
	use_tag = use[j]
	if use_tag eq 0 then goto, more
	record.tag = j
	record.type = 1					; flags a "Bank"
	length = bank_size				; length of payload

;	if i ge 511920 then begin
;		print,'debug ...'
;	endif
	
	if use_tag and ((skip eq 0) or ((skip gt 0) and (i gt skip))) then begin
		nh = min([length,ulong(n_bytes)])
		if nh gt 0 then begin
			b = ev[ i: (i + nh-1) < (n_buffer-1)]
			nb = min([n_elements(b),n_bytes])
			record.b = ptr_new(b[0:nb-1])
			record.length = nb
		endif else begin
			record.length = 0
			record.b = ptr_new(0)
		endelse
	endif
	
append:
	record.ptr = iptr
	record.state = state

	p = ptr_new(record)
	if ipr+1 ge npr then begin
		pr = [pr, ptrarr(100000L)]
		npr = npr + 100000L
	endif
	pr[ipr] = p
	ipr = ipr+1		   

more:
    if i lt 0 then warning,'read_midas','negative byte counter detected.'
	if (i lt n_actual-size_midas_header*4) and (select eq -1) then begin
	    n_loop = n_loop+1
	    case next of
	    	0: begin
				i = i_next_event
	    		goto, new_block
				end
	    	1: begin
				i = i_next_bank
				if i gt i_next_event-8 then begin
					i = i_next_event
					goto, new_block
				endif
	    		goto, next_bank
	    		end
	    endcase
	endif

done:
	if do_progress then begin
		wait,0.3
		progress, /complete, progress_tlb, 'Read midas completed.'
		progress, /ending, progress_tlb
	endif
	
	if npr gt ipr then ptr_free, pr[ipr:npr-1]
	return, (ipr gt 0) ? pr[0:ipr-1] : 0L
end

