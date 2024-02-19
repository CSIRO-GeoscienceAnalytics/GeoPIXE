pro geopixe_parallel, args=sargs

; The executive routine for background processing in GeoPIXE.
; Uses the "execute" command, so must use a full IDL RT license, as VM
; mode does not support "execute". The foreground set-up of shared memory
; for this is done in "cluster_client" using the routines in "parallel_client".
;
; Geopixe_parallel:
;	Sets (*ppar)[5]=1 to indicate up and running.
;	Waits for (*ppar)[2] to  be set to indicate command data loaded.
;	Clears (*ppar)[2] once data has been read.
;	Sets (*ppar)[4]=1 to indicate results have been uploaded.
;	Clears (*pb1)[0] on exit, if null command string.
;	Clears (*ppar)[5] on exit.
;
; parallel_client routines (used in foreground):
; Config:
;	Tests pointers to shared memory in common. If OK, just use them.
;	Loop through all nodes to make sure all are setup.
; Init:
;	Spawn background processes, link to logical units to enable closure.
;	Use /noshell for Linux, in which case command and args must be a string vector.
; Start:
;	NOTE: No modification to any shared memory before here!
;	If we get here, “busy” must not be set, so initialize memory.
;	Clear (*ppar)[2] to indicate wait for command string.
;	Upload command data.
;	Sets (*ppar)[2]=1 to indicate command data is ready.
;	Return good status, even if command string is null.
; Monitor:
;	Checks for (*ppar)[4]=1 (all processes) to indicate all done.
; Results:
;	Reads back all result data.
; Release:
;	Sets (*ppar)[3]=1 kill flags to stop background process.
; Cancel:
;	Destroy Bridge object connected to backgropund processes to kill them.
;	Frees shared memory using 'unmap'.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) eq 0 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'geopixe_parallel',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, cleanup
    endif
endif

; geopixe_parallel loads routines from GeoPIXE.sav, as these may be referenced
; from the device object. 

	found = 0
	file = 'GeoPIXE.sav'
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'
		if file_test(file) eq 0 then begin
			a = dialog_message('geopixe_parallel: Failed to restore GeoPIXE.sav.',/error)
		endif else found=1
	endif else found = 1
	if found then restore, file

; Loads all device object SAV files, which includes Maia.

	startupp, /devices
	
	parallel_worker
	home = geopixe_environment( temp=temp, geopixe=geodir)
	lib = geopixe_library( version=lversion)
	gversion = geopixe_version()

	if n_elements(sargs) eq 0 then begin
		args = {workerIndex:0, totalWorkers:1, prefix:'prefix'}
	endif else begin
		args = unstringify( sargs)
	endelse
	workerIndex = args.workerIndex
	totalWorkers = args.totalWorkers
	prefix = args.prefix

	gprint, active=1									; Enable gprint diagnostics with level at least this
														; set to active=2 normally, =1 for most diagnostics.
	debug = 1											; Print debug lines to log file.
	timeout_processes = 100.							; Timeout for args load at start.

	if lmgr(/vm) then goto, bad_idl_vm					; VM mode no good with "execute"

	ofilename = home + prefix + '_log.txt'
	olun = 50 + workerIndex								; theory get_lun in Bridge was issue

	on_ioerror, bad_debugfile
;	openw, olun, ofilename, /get_lun
	openw, olun, ofilename
	tic
	
	gprint,level=2, output=olun,'Process ',workerIndex,', Shared memory prefix=',prefix
	gprint,level=2, output=olun,'GeoPIXE dir= ',geodir
	gprint,level=2, output=olun,'Working dir= ',home
	gprint,level=2, output=olun,'Temp dir=',temp
	gprint,level=2, output=olun,'GeoPIXE version= ',gversion,', Library version=',lversion
	gprint,level=2, output=olun,'GeoPIXE Library= ',lib
	gprint,level=2, output=olun,' '

; Shared memory areas referenced here:
;
;	par array:	0	n_buffers
;				1	busy (used for command hand shaking)
;				2	reset blog client (used here to hold until args loaded)
;				3	kill blog client (from elsewhere to blog client here)
;				4	done (used here to flag processing complete)
;				5	blog client is up and running
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	  incoming (from cluster_client ...): 
;	                                       reset kill
;	  outgoing (to cluster_client ...):   
;	                                                 done running          buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers, /byted, output=olun )
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size

	worker_init, olun, psh									; init worker routines

	ppar = (*psh).ppar
	pf = (*psh).pfloat[0]									; progress fraction return
	pb0 = (*psh).pdat[0]									; args
	pb1 = (*psh).pdat[1]									; result return
	(*ppar)[5] = 1											; up and running

;	stat = fstat(olun)
;	help, stat, output=s
;	warning,'geopixe_parallel: '+str_tidy(workerIndex),['Test warning before waiting for args to be loaded.', '', $
;			'Using "olun"='+str_tidy(olun), s]

	t = 0.0
	while (*ppar)[2] eq 0 do begin
		gprint,level=1, output=olun,str_tidy(workerIndex)+': Wait for args to be loaded.'
		wait, 0.5
		t = t+0.5
		if t gt timeout_processes then begin
			warning,'geopixe_parallel: '+str_tidy(workerIndex),'Child waited too long for args to be loaded.'
			(*psh).error = 1
			goto, bad_timeout
		endif
	endwhile

;	pf	0	% complete			pl	0	errors				pb0	*	command string
;		1							1
;		2							2						pb1	*	result string

	workerParam = string( *pb0)
	(*ppar)[2] = 0											; clear handshaking wait

	if lenchr( workerParam) eq 0 then goto, bad_command
	(*pf)[0] = 0.01

;........................................................................................

;	At this point, the worker is running in the GeoPIXE runtime directory
;	with the GeoPIXE.sav file restored, and we have all the details we
;	need to do the work ... total number of workers that will be run (in
;	'totalWorkers'), which of those workers we are (in 'workerIndex') and
;	the string describing the work to be done (in 'workerParam')

	geopixe_execute, workerParam, olun=olun, workerIndex=workerIndex, totalWorkers=totalWorkers, $
					Result=strResult, error=error
	if error then goto, cleanup

	b = byte( strResult)
	n = n_elements(b)
	
	(*pb1)[0:n-1] = b											; return results string
	(*pb1)[n] = 0B

cleanup:
	if ptr_good( ppar) then begin
		(*ppar)[4] = 1											; flag finished
		(*ppar)[5] = 0											; up and running
	endif
	toc, lun=olun
	flush, olun
	wait, 1.0
	close_file, olun
	return

bad_idl_vm:
	warning,'geopixe_parallel',['IDL VM mode detected.','"Cluster" mode will not work in VM mode.', $
					'A run-time license is needed for "Cluster" mode.']
	goto, cleanup
bad_debugfile:
	warning,'geopixe_parallel','bad open of debug file.'
	goto, cleanup
bad_args:
	warning,'geopixe_parallel','bad number of arguments.'
	goto, cleanup
bad_shrmem:
	warning,'geopixe_parallel','error allocating shared memory in background process.'
	goto, cleanup
bad_command:
	warning,'geopixe_parallel','null command args string.'
	(*pb1)[0] = 0B
	goto, cleanup
bad_timeout:
	warning,'geopixe_parallel','timeout waiting for args, exit.'
	goto, cleanup
end





