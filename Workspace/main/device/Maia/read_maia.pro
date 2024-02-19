function read_maia, unit, old=old, n_buffer=n_bufferi, state=state, n_actual=n_actual, $
					progressbar=do_progress, select=select, skip=skip, accept=accept

; unit		open unit number to read from
; n_actual	actual byte read
; n_buffer	buffer byte length (default=2Mb, unless 'select' then=40kb)
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
       warning,'read_maia',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pr
       return, 0L
    endif
endif

if n_elements(old) lt 1 then old=0
if n_elements(n_bufferi) lt 1 then n_bufferi = 2000000LL
if n_elements(state) lt 1 then state = {x:0, y:0, enable:replicate(1B,384), time:0 }
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(select) lt 1 then select=-1L
if n_elements(skip) lt 1 then skip=0LL
if n_elements(accept) lt 1 then accept=indgen(1000)
use = intarr(1000)
use[accept] = 1

pr = ptrarr(100000L)
npr = 100000L
ipr = 0L
cancel = 0
skipb = 0
if select ge 0 then do_progress=0
if do_progress then progress, tlb=progress_tlb, title='Read Maia'

if select eq -1 then begin
	n_buffer = n_bufferi
	ev = bytarr(n_buffer)
	n_bytes = 64*1024UL			; 2048
endif else begin
	n_buffer = 64*1024UL
	point_lun, unit, select
	ev = bytarr(n_buffer)
	n_bytes = 64*1024UL
endelse

    stat = fstat(unit)
    cur = stat.cur_ptr
    on_ioerror, cont
    readu, unit, ev
cont:
    on_ioerror, NULL
    stat = fstat(unit)									; transfer_count is only 1?, so calculate
    n_actual = ((stat.cur_ptr - cur)) < n_buffer		; transfer count from change in CUR_PTR

    if n_actual ne n_buffer then begin
       print,'n_actual =',n_actual,'  n_buffer =',n_buffer
    endif

;	This list in read_maia, get_maia_details too ... also effects list of tags in main 'blog_browse' routine
;
;	1     id                  identity'
;	2     newrun              new run'
;	3     newseg              new segment file'
;	4     tod                 time of day'
;	5     summary             activity summary'
;	6     comment             comment strings'
;	7     sendnext            request next block'
;	8     et                  ET Maia block'
;	9     xy                  XY position Maia block'
;	10    PA                  PA pixel advance Maia block'
;	11    da_put              DA pile-up init table Maia block'
;	12    da_cal              DA cal init coefficients Maia block'
;	13    da_cal2             DA cal init table Maia block'
;	14    da_mat              DA matrix Maia block'
;	15    maia_da_pixel_1     DA pixel record - pixel ppm-uC contributions'
;	16	  maia_da_init_file_1 DA initialization file name
;	17	  maia_da_element_1	  DA initialization element string
;	18    maia_da_params_1    DA parameters'
;	19    maia_da_matrix_raw_1  DA raw matrix'
;	20    maia_da_cal_1       DA calibration'
;	21    maia_da_throttle_1  Throttle table factors'
;	22    maia_da_enable_1    Dali enable status bits'
;	23    sendprev            client stuff'
;	24    sendprevornext      "  '
;	25    et2                 ET 96 element ET block'
;	26    monitor             Epics stuff'
;	27	  pm_eterr_1		  Wollongong boards ...'
;	28	  id2				  revised ID'
;	29	  endrun			  end of run'
;	30	  maia_rexec_1		  MIRO rexec parameterz
;	31	  et3				  ET 384 Maia block'
;	32	  summary_2			  summary, version 2'
;	33	  setgroup			  set the data storage group dir tree for blog'
;	34	  event_1			  (was ET4) ET 384 Maia block'
;	35*	  da_accum			  DA accumulator
;	36*	  ROI_accum			  ROI accumulator
;	37*	  DT_accum			  Deadtime accumulator
;	38*	  DTpp_accum		  DT per pixel accumulator
;	39*	  activity_accum	  DA accumulator
;	40*	  E_spectra			  spectrum accumulator
;	41*	  ET2D_accum		  ET 2D accumulator
;	42	  maia_scaninfo_1	  new scan information, replaces rexec_1
;	43	  T_spectra			  ToT spectrum accumulator
;	44	  maia_da_info_1	  DA/DT info block
;	45	  var_list_1		  Common library var list
;	46	  var_value_1		  library var values
;	47	  maia_scaninfo_2	  Maia ScanInfo 2
;	48	  pm_event_ts_1		  DAQ ET w/o TS
;	49	  pm_event_nots_1	  DAQ ET no TS
;	50	  pm_activity_1		  DAQ Activity
;	51	  setproject		  set next run project string
;	52	  client			  client connect/disconnect log
;	53	  summary_3			  summary, version 3
;	54	  name				  name used (set by client) to identify a client
;	55    metadata			  metadata records (key/value pairs, separated by newlines \n)
;	56    summary 4			  summary, version 4
;	57    report			  detailed report summary
;		
;	* Indicates Maia records with sub_header data (these flagged in test below)
;	
;	NOTE: This list in 'blog_browse' too ...
;	See also list of tags in 'read_maia', 'get_maia_details', and 'update_maia_records', 'update_maia_details' too ...
	tags = ['ignore','id','newrun','newseg','tod','summary','comment','send_next','et','xy', $
		'pa','DA_put', 'DA_cal','DA_cal2','DA_mat','DA_pixel1','DA_init_file1','DA_element1','DA_params1', 'DA_raw1', $
		'DA_cal1','DA_throttle1','DA_enable1','send_prev','prev/next','et2','monitor', 'pm_eterr_1', $
		'id2','endrun','rexec1','et3','summary2','setgroup','event_1','DA_accum','ROI_accum','DT_accum','DTpp_accum', $
		'activity','E_spectra','ET2D','scaninfo_1','T_spectra','DA_info','Var_List_1','Var_Value_1','Maia scan Info 2', $
		'DAQ ET w/ TimeStamp','DAQ ET no TimeStamp','DAQ Activity','set-project','client','summary3','client-name', $
		'Metadata','summary4','report']
	n_tags = n_elements(tags)
	use[n_tags:*] = 0

tseq = 0L
tv_sec = 0L
tv_us = 0L
client = 0L
xlast = -1000
ylast = -1000

n_header = 32
n_sub_head = 36
n_pa_offset = 4
if old then begin
    n_header = 16
    n_pa_offset = 0
endif
error_mask = 'FFFFFFF000000000'x		; error bits in "dwell/readout"
error_offset = -40
duration_mask = '0000000FFFFFFFFF'x		; 32 bits plus 8 bits of next 32-bit word

; This definition is also in "blog_read_next" used for on-line blog-browse ...

zero_sub_header = { X:0L, Y:0L, Z:0L, trigger:0UL, duration:0ULL, error:0UL, count:0UL, flux1:0UL, flux2:0UL }

record = {tag:0US, length:0US, prev_length:0US, seq:0UL, tseq:0UL, tv_sec:0UL, tv_us:0UL, client:0UL, $
			b:ptr_new(), state:state, file:'', index:0UL, ptr:0UL, sub_header:zero_sub_header }

sub_header = { X:0L, Y:0L, Z:0L, trigger:0UL, duration:0ULL, flux1:0UL, flux2:0UL, count:0UL }

if select eq -1 then begin
	q = where((ev[0:(10000 < n_buffer)-1] eq 'AA'xub) and (shift(ev[0:(10000 < n_buffer)-1],-3) eq 'BB'xub), n_records)
	i = q[0]
	if n_records eq 0 then begin
;		sprint, s, ev[0:5*16-1<(n_buffer-1)], format= '("Header  ",(10(16Z4)))'
		f = ''
		if n_elements(stat) gt 0 then f=stat.name
;		warning,'read_maia',['Tags "AA", "BB" not found in file: '+f ]
		goto, done
	endif
	if i ne 0 then begin
;		print,'Skip to AA,BB tags at start of file by =',i
		goto, done																;@9-21
	endif
endif else begin
	i = 0L
endelse
n_loop = 0L
iprog = 0
nprog = 10L
ltime = systime(/seconds)

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
			print,'read_maia: Extend progress period to ',nprog
		endif
		ltime = time
	endif
	iprog = iprog+1
	if cancel or skipb then goto, done

;........................................................................

    ab_ok = ((ev[i] eq 'AA'xub) and (ev[i+3] eq 'BB'xub))

;	if ipr ge 23655 then begin
;		print, 'debug'
;	endif
    if ab_ok then begin
       record.tag = uint(ev,i+1,1)
       record.length = uint(ev,i+4,1) 
       record.prev_length = uint(ev,i+6,1)
       record.seq = ulong(ev,i+8,1)

       if old eq 0 then begin
           record.tseq = ulong(ev,i+12,1)
           record.tv_sec = ulong(ev,i+16,1)
           record.tv_us = ulong(ev,i+20,1)
           record.client = ulong(ev,i+24,1)
;           record.unused = ulong(ev,i+28,1)
       endif else begin
           record.tseq = 0L
           record.tv_sec = 0L
           record.tv_us = 0L
           record.client = 0L
;           record.unused = 0L
       endelse

;	The record has been packed for Unix (big endian) byte ordering. To make it
;	read OK for other platforms (e.g. PC), use swap_bytes, recursively.
;	Don't swap state, as this is local data.

	   swap_bytes, record, /big_endian_data
;      record.tag = record.tag < (n_tags-1)
	   use_tag = use[record.tag]
	   
	   length = long(record.length)
;	   if i+length+n_header gt n_actual then begin
	   if i+length gt n_actual then begin
	   		use_tag = 0
	   endif
;   		if ipr eq 21 then begin
;   			print,'debug ...'
;   		endif
	   
	   n_head = n_header
	   offset = n_head
	   record.sub_header = zero_sub_header
	   if use_tag and (record.tag ge 35) and (record.tag le 41) then begin	   
	   		sub_header.X = ulong(ev,i+n_header,1)
	   		sub_header.Y = ulong(ev,i+n_header+4,1)
	   		sub_header.Z = ulong(ev,i+n_header+8,1)
	   		sub_header.trigger = ulong(ev,i+n_header+12,1)
	   		sub_header.duration = ulong64(ev,i+n_header+16,1)
	   		sub_header.flux1 = ulong(ev,i+n_header+24,1)
	   		sub_header.flux2 = ulong(ev,i+n_header+28,1)
	   		sub_header.count = ulong(ev,i+n_header+32,1)

			swap_bytes, sub_header, /big_endian_data

	   		record.sub_header.X = sub_header.X
	   		record.sub_header.Y = sub_header.Y
	   		record.sub_header.Z = sub_header.Z
	   		record.sub_header.flux1 = sub_header.flux1
	   		record.sub_header.flux2 = sub_header.flux2
	   		record.sub_header.count = sub_header.count
	   		record.sub_header.duration = sub_header.duration and duration_mask
	   		record.sub_header.error = ulong( ishft( sub_header.duration and error_mask, error_offset))
	   		offset = offset + n_sub_head
	   		length = (4* record.sub_header.count) < (64 * 1024UL - 1)
	   endif
	   record_length = record.length		; save this, as we modify it below sometimes (truncated block)

;	NOTE: the payload has not been reordered here. It will need to be
;		reordered (swap_bytes) elsewhere in blog_browse.

	   if use_tag and ((skip eq 0) or ((skip gt 0) and (i gt skip))) then begin
		   nh = min([ulong(length),ulong(n_bytes)])
		   if (nh gt 0) and (i+offset lt n_buffer) then begin
			   b = ev[ i+offset: (i+offset + nh-1) < (n_buffer-1)]
		       nb = min([n_elements(b),n_bytes])
			   record.b = ptr_new([b[0:nb-1],0B])
			   record.length = nb
		   endif else begin
		   		record.length = 0
		   		record.b = ptr_new(0)
		   	endelse
		   record.ptr = i
	       record.state = state
	
			p = ptr_new(record)
			if ipr+1 ge npr then begin
				pr = [pr, ptrarr(100000L)]
				npr = npr + 100000L
			endif
			pr[ipr] = p
			ipr = ipr+1		   
	   endif
	   
    endif else begin
       print, 'tags AA, BB not found ...'
       print, ev[i-5*16+1>0:i], format= '("Before ",(10(16Z4)))'
       print, ev[i:i+5*16-1<(n_buffer-1)], format= '("After  ",(10(16Z4)))'
       goto, done
    endelse

    i = i + n_head + ulong(record_length)
    if i lt 0 then warning,'read_maia','negative byte counter detected.'
	if (i le n_actual-n_head) and (select eq -1) then begin
	    n_loop = n_loop+1
	    goto, new_block
	endif

done:
	if do_progress then begin
		wait,0.3
		progress, /complete, progress_tlb, 'Read Maia completed.'
		progress, /ending, progress_tlb
	endif
	
	if npr gt ipr then ptr_free, pr[ipr:npr-1]
	return, (ipr gt 0) ? pr[0:ipr-1] : 0L
end

