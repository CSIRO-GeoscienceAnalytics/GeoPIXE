function read_unidaq, unit, n_buffer=n_bufferi, state=state, n_actual=n_actual, $
					progressbar=do_progress, select=select

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
       warning,'read_unidaq',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, 0L
    endif
endif

if n_elements(old) lt 1 then old=0
if n_elements(n_bufferi) lt 1 then n_bufferi = 500000LL
if n_elements(state) lt 1 then state = {x:0, y:0, enable:replicate(1B,4), time:0 }
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(select) lt 1 then select=-1L

pout = 0L
cancel = 0
skip = 0
if select ge 0 then do_progress=0
if do_progress then progress_tlb = progress( title='Read UniDAQ')

if select eq -1 then begin
	n_buffer = n_bufferi
	ev = lonarr(n_buffer)
	n_words = 8035UL
endif else begin
	n_buffer = 10000UL
	point_lun, unit, select
	ev = lonarr(n_buffer)
	n_words = 8035UL
endelse

    stat = fstat(unit)
    cur = stat.cur_ptr
    on_ioerror, cont
    readu, unit, ev
cont:
    on_ioerror, NULL
    stat = fstat(unit)									; transfer_count is only 1?, so calculate
    n_actual = ((stat.cur_ptr - cur)/4) < n_buffer		; transfer count from change in CUR_PTR

    if n_actual ne n_buffer then begin
       print,'n_actual =',n_actual,'  n_buffer =',n_buffer
    endif

;	Since all data is 32-bit words, can do reordering for all here.
;	Data was collected on a PC

   swap_bytes, ev, big_endian_data=0

n_head = 6
tseq = 0L

; Type
;	1     begin               begin
;	2     pause               pause
;	3     resume              resume
;	4     end                 end

tags = ['begin','pause','resume','end']

xlast = -1000
ylast = -1000

record = {length:0UL, type:0UL, run:0UL, event:0UL, mode:0UL, reserved:0UL, $
			b:lonarr(n_words), state:state, file:'', index:0UL, ptr:0UL}

i = 0L
n_loop = 0L
first = 1

new_block:
	if do_progress then progress_update, progress_tlb, {unit:0, value:0, current:i, size:n_actual}, cancel=cancel, skip=skip
	if cancel or skip then goto, done

       record.length = ulong(ev,4*i,1)			; BYTE offset!
       record.type = ulong(ev,4*(i+1),1)
       record.run = ulong(ev,4*(i+2),1)
       record.event = ulong(ev,4*(i+3),1)
       record.mode = ulong(ev,4*(i+4),1)
       record.reserved = ulong(ev,4*(i+5),1)
	   record.file = stat.name

	   nh = min([ulong(record.length - n_head),ulong(n_words)])
	   record.b[*] = 0
	   if nh gt 0 then begin
		   b = ev[i+n_head:i+n_head + nh-1 < (n_buffer-1)]
	       nb = min([n_elements(b),n_words])
		   record.b[0:nb-1] = b[0:nb-1]
	   endif
	   record.ptr = 4*i							; byte pointer
       record.state = state

	   p = ptr_new(record)
	   if first then begin
		   pout = p
		   first = 0
	   endif else begin
		   pout = [pout, p]
	   endelse

    i = i + ulong(record.length)
    if i lt 0 then warning,'read_unidaq','negative byte counter detected.'
	if (i lt n_actual-n_head - n_words) and (select eq -1) then begin
	    n_loop = n_loop+1
	    goto, new_block
	endif

done:
	if do_progress then begin
		wait,0.3
		progress_complete, progress_tlb, 'Read UniDAQ completed.'
		progress_end, progress_tlb
	endif
	return, pout
end

