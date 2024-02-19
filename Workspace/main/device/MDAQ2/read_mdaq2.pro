function read_mdaq2, unit, n_buffer=n_bufferi, state=state, n_actual=n_actual, $
					progressbar=do_progress, select=select, skip=skip, accept=accept

; unit		open unit number to read from
; n_actual	actual byte read
; n_buffer	buffer byte length (default=2Mb, unless 'select' then=64kb)
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
       warning,'read_mdaq2',['IDL run-time error caught.', '', $
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
if n_elements(skip) lt 1 then skip=0LL
if n_elements(accept) lt 1 then accept=indgen(1000)
use = intarr(1000)
use[accept] = 1

if select ge 0 then do_progress=0
if do_progress then progress, tlb=progress_tlb, title='Read mdaq2'

;	ev and n_buffer are longs this time ...
;	But n_bufferi as input is bytes.

if select eq -1 then begin
	n_buffer = n_bufferi /4
	ev = lonarr(n_buffer)
	n_longs = 1024UL		; 64*1024UL
endif else begin
	n_buffer = 64*1024UL / 4UL
	skip = 0L
	point_lun, unit, select
	ev = lonarr(n_buffer)
	n_longs = 1024UL		; 64*1024UL
endelse
pr = ptrarr(100000L)
npr = 100000L
ipr = 0L
cancel = 0
skipb = 0
skip = 4*(skip/4)										; must be long boundary byte count

    stat = fstat(unit)
    cur = stat.cur_ptr
	point_lun, unit, cur + skip							; not done this way in Maia, uses skip records below ...
	cur = cur + skip									; must be this way in case njson is not long boundary

    on_ioerror, cont
    readu, unit, ev
cont:
    on_ioerror, NULL

;	The record has been packed for Unix (big endian) byte ordering. To make it
;	read OK for other platforms (e.g. PC), use swap_bytes, recursively.

    swap_bytes, ev, big_endian_data=0
    
    stat = fstat(unit)									; transfer_count is only 1?, so calculate
    n_actual = ((stat.cur_ptr - cur)/4) < n_buffer		; transfer count from change in CUR_PTR

    if n_actual ne n_buffer then begin
       print,'n_actual =',n_actual,'  n_buffer =',n_buffer
    endif

;	This list in read_mdaq2, get_mdaq2_details too ... also effects list of tags in main 'mdaq2_browse' routine
;														Hex		Length
;	0		Pulse1			Pulse data type				0		1
;	2		Busy			Busy data type (per det)	2		1
;	3		Busy MSB		Busy MSB (per det)			3		1
;	12		Position		Spatial position			C		6
;	11		Spatial 		Spatial statistics			B		6
;	15		Error			Analogue/Overflow status	F		1
;		
;	NOTE: This list in 'mdaq2_browse' too ...
;	See also list of tags in 'read_mdaq2', 'get_mdaq2_details', and 'update_mdaq2_records', 'update_mdaq2_details' too ...

@mdaq2_listmode.def

use[n_data_tags:*] = 0
record = {tag:0US, length:0US, prev_length:0US, b:ptr_new(), state:state, file:'', index:0UL, ptr:0UL }

i = 0L
n_loop = 0L
iprog = 0
nprog = 10L
ltime = systime(/seconds)
n_header = 0

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
			print,'read_mdaq2: Extend progress period to ',nprog
		endif
		ltime = time
	endif
	iprog = iprog+1
	if cancel or skipb then goto, done

	tag = uint( ishft(ev[i] and data_type_mask, data_type_offset))
	q = where( tag eq data_type, nq)
	if nq eq 0 then begin
		print,'read_mdaq2: unknown data-type at word ',i,' = ',tag
		record_length = 1
		n_head = 0
		goto, more
	endif
	j = q[0]
	record.length = uint(data_length [j] - n_header)			; length of payload
	record.tag = j

	; record.prev_length = uint(ev,i+6,1)

;	The record has been packed for Unix (big endian) byte ordering. To make it
;	read OK for other platforms (e.g. PC), use swap_bytes, recursively.
;	Don't swap state, as this is local data.

	   use_tag = use[j]
	   
	   length = long(record.length)
	   if i+length gt n_actual then begin
	   		use_tag = 0
	   endif
	   
	   n_head = n_header
	   offset = n_head
	   record_length = record.length		; save this, as we modify it below sometimes (truncated block)

;	NOTE: the payload has not been reordered here. It will need to be
;		reordered (swap_bytes) elsewhere in blog_browse.

;	   if use_tag and ((skipbutton eq 0) or ((skip gt 0) and (i gt skip))) then begin
	   if use_tag then begin
		   nh = min([ulong(length),ulong(n_longs)])
		   if nh gt 0 then begin
			   b = ev[ i+offset: (i+offset + nh-1) < (n_buffer-1)]

;			   Take care to only group words associated with this tag (if we started in the middle of this tag)

			   k = 0L
			   while (tag eq uint( ishft(ev[i+offset+k] and data_type_mask, data_type_offset))) and (k lt nh) and (i+offset+k lt (n_buffer-1)) do k=k+1

		       record_length = min([record_length,k])
		       nb = min([n_elements(b),k])
			   record.b = ptr_new(b[0:nb-1])
			   record.length = nb
		   endif else begin
		   		record.length = 0
		   		record.b = ptr_new(0)
		   	endelse
		   record.ptr = 4*i + cur
	       record.state = state
	
			p = ptr_new(record)
			if ipr+1 ge npr then begin
				pr = [pr, ptrarr(100000L)]
				npr = npr + 100000L
			endif
			pr[ipr] = p
			ipr = ipr+1		   
	   endif
;	   	if ipr gt 3663 then begin
;	   		print, 'debug ...'
;		endif

more:
    i = i + n_head + ulong(record_length)
    if i lt 0 then warning,'read_mdaq2','negative byte counter detected.'
	if (i lt n_actual-n_head-1) and (select eq -1) then begin
	    n_loop = n_loop+1
	    goto, new_block
	endif

done:
	if do_progress then begin
		wait,0.3
		progress, /complete, progress_tlb, 'Read mdaq2 completed.'
		progress, /ending, progress_tlb
	endif
	
	if npr gt ipr then ptr_free, pr[ipr:npr-1]
	return, (ipr gt 0) ? pr[0:ipr-1] : 0L
end

