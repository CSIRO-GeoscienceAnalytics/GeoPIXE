;
;  EVT (event) raw data file read template Image plugin routine
;  A template for reading EVT event files and doing something with them ...
;  
;  It assumes you have an image loaded in GeoiPIXE, and you want to them
;  read the raw event files(s) and do something new with the raw event stream.
;  
;  -----------------------------
;  
;  All Image template routines MUST be named with "_image__plugin.pro"
;  at the end of the file name. For a new "Fred" plugin, copy and rename this file
;  to "Fred_image_plugin.pro" and edit the first line to:
;  "pro fred_image_plugin, p, i, title=title, history=history"
;
;  Plugins should be compiled in IDLDE and saved as a SAV file.
;  Only compile routines for ONE plugin and save using the command:
;
;  "SAVE, /routines, filename='fred_image_plugin.sav'"
;
;  for a "fred_image_plugin" plugin.
;
;  To ensure that only one routine exists in a file, exit IDLDE and start it again to compile
;  and save a plugin.
;
;  NOTE: It is important to ensure that ONLY routines for ONE plugin is in each SAV file.
;  Otherwise, unexpected results may result when the SAV files are restored at run-time.
;
;
;  The plugin SAV files will then be loaded automatically when GeoPIXE.sav runs,
;  if the plugin SAV files are located in the same directory as GeoPIXE.sav.
;
;  Plugin arguments:
;	p		pointer to the GeoPIXE image structure for the present loaded images
;	i		the number of the presently displayed element.
;
;  keywords:
;	history		return a history string along with the image result.
;	title		just return the title string (to go in the menu).
;
;  On return to GeoPIXE, it is assumed that only the contents of the selected element image have
;  been changed, and the sizes of images, their total number and element names all remain unchanged.
;  Avoid tinkering with image structure parameters, as strange things may happen.
;
;----------------------------------------------------------------------------------------------

pro evt_read_template_image_plugin, p, ii, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'EVT read template Plugin'			; return the menu title for this plugin
	return
endif

;...............................................................................................
;	Get some details from the incoming image struct ...
;	(for other parameters available, see "define.pro")

	sx = (*p).xsize								; X size of image in pixels
	sy = (*p).ysize								; Y size of image in pixels
	obj = (*p).DevObj							; device object
	if obj_valid(obj) eq 0 then begin
		device = 'MAIA_DEVICE'					; default to Maia
		obj = obj_new(device)
	endif else begin
		device = obj->name()
	endelse
	if (*p).array then begin					; if an array detector
		active = *(*p).pactive
		cal_a = (*(*p).pcal).poly[1]			; array of cal_a, cal_b
		cal_b = (*(*p).pcal).poly[0]
	endif else begin							; else use single cal from image/ DA matrix
		cal_a = (*p).cal.poly[1]
		cal_b = (*p).cal.poly[0]
	endelse
	
;...............................................................................................
;	Get raw event file(s) details from the incoming image struct, or open the file-requester
;	if these are not found ...

	file = file_requester( /read, title='Select First Event file', file=(*p).source, path=extract_path((*p).source), /skip_if_exists )
	if obj->multi_files() then begin
		file2 = file_requester( /read, title='Select Last Event file', file=(*p).source2, path=file_dirname(file), /skip_if_exists )
	endif else file2 = ''
	if file eq '' then return

;...............................................................................................
;	If the device supports pileup and throttle mechanisms, then makes sure we can read these too ...
;	(to ignore one of these, just select "cancel" when the file-requester opens)

	if obj->pileup() then begin
		pileup = file_requester( /read, title='Select Pileup specs file', file=(*p).pileup, path=extract_path((*p).pileup), /skip_if_exists )
	endif
	if obj->throttle() then begin
		throttle = file_requester( /read, title='Select Throttle vector file', file=(*p).throttle, path=extract_path((*p).throttle), /skip_if_exists )
	endif

;...............................................................................................
;	For a mutlifile event file set, use these first and last file names to build the full
;	list of files to read ...

	evt_files = select_evt_files( file, file2, obj->multi_files(), obj->multi_char(), obj->extension())

	print,'EVT start time = ',systime()
	print,'EVT files: # = ',n_elements(evt_files)
	print,evt_files

;...............................................................................................
;	Select an output file name and path ...

	output = file_requester( /write, title='Select Output file', filter='*.csv', path=file_dirname(file[0]), $
					file=strip_file_ext(file_basename(file[0]))+'.csv' )
	if output eq '' then output=strip_file_ext(file_basename(file[0]))+'.csv'
 
;...............................................................................................
;	Call the custom event file reading routine (source below) ...
;	Make sure when you midify this plugin and the process routine that you give them BOTH
;	new unique names ...

 	process_EVT_template, evt_files, xrange=sx, yrange=sy, cal_a=cal_a, cal_b=cal_b, $
 			channel=active, output=output, device=device, pileup=pileup, throttle=throttle, /progress
    
	history = 'EVT read template to: '+ output
	return
end

;--------------------------------------------------------------------------------------------------
;	Template event processing/accumulation routine. See further notes below in code
;	about using the event stream returned

pro process_EVT_template, filei, xrange=xrange, yrange=yrange, $
          cal_a=cal_a, cal_b=cal_b, xcompress=xcompress, ycompress=ycompress, $
          output=outputi, channel=channeli, suppress=suppress, throttle=throttle, pileup=pileup, $
          progress=do_progress, group=group, device=devicei
          
;   file		list-mode file(s) to process.
;   device		list-mode device object name
;   throttle 	gives name of throttle factors file.
;   pileup 		give name of pileup limits file.
;   xrange		the full X ranges in the original data
;   yrange		full Y range
;   xcompress	compress these by an integral factor.
;   ycompress
;
;   output		Output file, Write elemental images out as a .dai DA image file.
;
;   group		group_leader for progress
;   /do_progress pop-up a progress bar
;   /suppress	suppress pop-ups when in Batch mode for subsequent image sorts
;
;   Channel 	gives the station/ADC numbers, which start at '0'.
;   			This can be a vector of channels to sort, or -1 meaning all.
;   cal_a,cal_b	 the energy calibrations.
;
; Multiple detectors:
;   This is supported by passing a vector of channels to all detectors in 'channel',
;   and passing 'cal_a' and 'cal_b' as vectors over ALL channels, not just those in 'channel'.

COMPILE_OPT STRICTARR
common c_evt_last, last
common c_maia_6, maia_y_min
common c_geopixe_adcs, geopixe_max_adcs
common c_null_image_1, max_image_cal
common c_seed, seed
common c_debug_warnings, enable_warning_popup
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384
if n_elements(seed) lt 1 then seed=1L
if n_elements(throttle) lt 1 then throttle = ''
if n_elements(pileup) lt 1 then pileup = ''

if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(suppress) lt 1 then suppress=0

print, '================================================================================================'
print, 'process_EVT_template: start time = ',systime()

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
		s2 = ['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c]
		warning,'process_EVT_template', s2, /error
		MESSAGE, /RESET
		return
	endif
endif

define_devices
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei

if n_params() lt 1 then begin
    print, 'process_EVT_template: missing arguments'
    return
endif
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then goto, bad_file
if n_elements(channeli) lt 1 then channeli = -1L
if n_elements(xrange) lt 1 then xrange = 1024
if n_elements(yrange) lt 1 then yrange = 1024
if n_elements(xcompress) lt 1 then xcompress = 1
if n_elements(ycompress) lt 1 then ycompress = 1
if n_elements(outputi) lt 1 then outputi=file[0]

evt_file = file
output = strip_file_ext(strip_non_print(outputi,/no_tab)) + '.csv'
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

print, 'Output="',output,'"'

; ***	Custom output file opening and header here ...
;		as a simple example, we'll open a file to take a detector histogram ...

on_ioerror, bad_output
openw, luno, output, /get_lun
	

channel = channeli			
if n_elements(cal_a) lt 1 then cal_a = 1.0
if n_elements(cal_b) lt 1 then cal_b = 0.0
xcompress = (xcompress > 1)
ycompress = (ycompress > 1)

processed = 0LL
valid = 0LL
bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
cancel = 0
etitle = ''
min_x = 100000L
min_y = 100000L
max_x = 0L
max_y = 0L

; 'xrange, yrange' are the full scan area, not compressed.
; 'xrange2, yrange2' are these compressed. These will become the 'original_size' values.

xrange2 = long( xrange / xcompress)
yrange2 = long( yrange / ycompress)
xrange3 = xrange2
yrange3 = yrange2

do_pileup = 0
do_throttle = 0
do_attributes = 0
if obj->pileup() then begin
	pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
	if do_pileup then print,'Pileup file =',pileup
endif
if obj->throttle() then begin
	throttle_factor = get_throttle(throttle, do_throttle=do_throttle)
	if do_throttle then print,'Throttle file =',throttle
endif
attributes = obj->get_attribute_list()
if attributes[0] ne '' then begin
	do_attributes = 1
	n_attributes = n_elements(attributes)
endif else n_attributes=0

flux = fltarr(xrange3,yrange3, n_attributes+1)
dead_fraction = fltarr(xrange3,yrange3)
pileup_loss_map = fltarr(xrange3,yrange3)
nnpu = lonarr(xrange3,yrange3)
nn = lonarr(xrange3,yrange3)

;	'n_det' is the number of detectors selected, in array mode.
 
n_det = n_elements(channel)
array = 0
if n_det gt 1 then array=1
if channel[0] eq -1 then begin
    n_det = n_elements(cal_a)
    channel = indgen(n_det)							; all channels
    if n_det gt 1 then array=1
endif
nmax = max([channel,n_det-1])
print,'max_det=',nmax+1
print,'channel=',channel

n = 0L
j = 0L
nj = n_elements(evt_file)
ndj = (nj/20) > 1
njc = 0L
first = 1
last_time = systime(/seconds)

loop_file:
	njc += 1
	if njc ge ndj then begin
		print, 'File loop, evt_file[j] =', evt_file[j]
		njc = 0L
	endif
	on_ioerror, bad_file
	close,1
	openr, 1, evt_file[j], bufsiz=1500*1024L
	on_ioerror, next									; was next

	device_specific, obj,1, xrange,yrange, n_guide,progress_file, first=first, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			suppress=suppress, error=err

	if err then goto, finish
	if first then begin
		nprogress = ((100000L / n_guide) > 1L) < 500L
	endif

	if j eq 0 then begin
		case progress_file of
			0: begin
				p = { unit:1, value:[0LL,0LL,0LL,0LL,0LL,0LL]}
				end
			1: begin
				p = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
				end
			2: begin
				p = { unit:0, current:0L, size:xrange*yrange, file:evt_file[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
				end
			else:
		endcase
		if do_progress then begin
			t = 'Clipped'
			progress, tlb=progress_tlb, title='Convert Evt File to ??', $           ; put up a progress bar
					pars=['Events','Valid','Blocks','Bad XY','Size',t]

			iprogress = 0L
		endif
	endif

	i = 0L
	while ~ EOF(1) do begin
		sbad_xy = 0L
		veto = 0US

;		Call read_buffer: 
;			'n' returns number of events returned (arrays e,tot,x1,y1,veto,ste).
;			'veto' =1 flags bad/rejected/pseudo events, within the 'n'.
;			'good' returns number of good (veto=0) events.
;
;		Take care with vectors that some devices do not set, such as veto, ste, tot
;		The test in read_buffer will detect if they are not set-up. But after that we need to
;		clear them for each buffer. The easiest way to ensure that is to set them to "0L" here
;		(for veto) or in 'read_buffer' (for ste, t, multiple).

		read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress, $
			station_e=ste, time=tot, veto=veto, multiple=multiple, $
			title=etitle, file=evt_file[j], error=err, $
			flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
			total_processed=processed, processed=count1, valid=good, total_bad_xy=sbad_xy
    
		if err then goto, next
		if (n eq 0) or (good eq 0) then goto, cont
		bad_xy = bad_xy + sbad_xy
		
		if n_elements(veto) ne n then veto = uintarr(n)
		if do_pileup then begin
			pu = uint( (tot lt pileup_limit[0,e]) or (tot gt pileup_limit[1,e]))
			qp = where( pu eq 1, nqp)
			pileup_losses = pileup_losses + nqp
		endif else pu = uintarr(n)
		if do_throttle then begin
			multiple = temporary(multiple) * throttle_factor[e]
		endif

;     Reject those events with col outside the matrix, outside the pixel range
;     'count2' is the total remaining good events.

		q1 = where( (veto eq 0) and (	((x1 lt 0) or (x1 ge xrange3)) or  $
										((y1 lt 0) or (y1 ge yrange3))), nq1)
		clipped = clipped + nq1
		if nq1 gt 0 then veto[q1] = 1
		q = where( veto eq 0, count2)

;		          		Events	Valid	Blocks	Bad XY	Size	Clipped/pileup
		case progress_file of
			0: begin
				p.value = [processed,good,i,bad_xy,n,clipped]
				r = 0.5
				end
			1: begin
				p.value = [processed,good,i,bad_xy,n,clipped]
				p.current = j
				p.file = evt_file[j]
				r = float(j)/nj
				end
			2: begin
				p.value = [processed,good,i,bad_xy,n,clipped]
				p.current = i
				r = float(i)/(xrange*yrange)
				end
			else:
		endcase
		if do_progress then begin
			iprogress = iprogress + 1
			if iprogress ge nprogress then begin
				time = systime(/seconds)
				progress, /update, progress_tlb, p, cancel=cancel, skip=skip
				t = systime(/seconds)
				if skip then goto, finish
				if cancel then begin
					close, 1
					return
				endif
				ft = t - last_time
				dt = t - time
				if dt gt 0.2*ft then begin
					nprogress = nprogress*2
					gprint, level=2, output=cluster_debug,'process_EVT_template: Extend progress period to ',nprogress
				endif
				iprogress = 0L
				last_time = time
			endif
		endif

		if count2 gt 0 then begin
			valid = valid + count2
	
			min_x = min( [min_x, min(x1[q])])
			min_y = min( [min_y, min(y1[q])])
			max_x = max( [max_x, max(x1[q])])
			max_y = max( [max_y, max(y1[q])])

;			Here we need to make use of the following vectors for writing to 'output'
;			or to write a custom 'accumulation' routine to accumulate these events in some
;			way: 
;				e		energy
;				tot		time (Maia device)
;				x1		X coordinate for event
;				y1		Y
;				ste		detector number for event
;				pu		1=pileup reject, 0=keep
;				veto	1=bad event to ignore
;
;				n		length of vector
;				count2	valid events within vectors
;				
;			In this accumulation loop (or called out to a Fortran or C library), make
;			use of the following:
;				ste		detector number/index for each event. You might use this to accumulate
;						by detector number, or to select a sub-set of detectors.
;				pu		test pu for event to reject pileups, if rejected increment nnpu @ X,Y
;				veto	test this and skip if veto=1 (skips non event data, for example)
;			
;			Optionally, accumulate these too:
;				nn		total events at XY map
;				nnpu	total events rejected as pileup map


; ***		your 'accumulation' routine here ... (return 'err=1' if an error occurs)
;			as a simple example, we'll form a histogram of filtered detector activity ...

			if array then begin
				q = where((veto eq 0) and (pu eq 0), nq)
				if (nq gt 0) then begin
					if (n_elements(hist) lt 1) then begin
						hist = histogram( ste[q], max=nmax, min=0)
					endif else begin
						hist = histogram( ste[q], max=nmax, min=0, input=hist)
					endelse
				endif
			endif

			
			if err ne 0 then begin
				print,'process_EVT_template: error (',err,') return from template accumulate'
				goto, finish
			endif
		endif

cont:
		i = i+1
	endwhile

next:
	j = j+1
	first = 0
	gprint, level=1, output=cluster_debug,'process_EVT_template: next file ...'
	if j lt nj then goto, loop_file

finish:
	print,'process_EVT_template: finished.'
	print, ' processed = ', processed
	print, ' valid events = ',valid
	print, ' bad event triplets = ',bad_xy
	print, ' clipped to image,matrix bounds, or not station = ', clipped
	print, ' X range = ', min_x, max_x
	print, ' Y range = ', min_y, max_y
	if n_elements(flux) gt 1 then print,' found FLUX array'
	if n_elements(dead_fraction) gt 1 then print,' found DEAD_FRACTION array'
 
 	if n_elements(p) gt 0 then begin
		if do_progress then begin
			p.value = [processed,valid,i,bad_xy,n,clipped]
			case progress_file of
				1: begin
					p.current = j
					end
				2: begin
					p.current = i
					end
				else:
			endcase
			progress, /update, progress_tlb, p
		endif
	endif

	close, 1
	on_ioerror, null
	t = 'EVT reading complete.'
	if do_progress then begin
		progress, /complete, progress_tlb, t
	endif	
	goto, cleanup

cleanup:
	print, 'process_EVT_template: finish time = ',systime()
	print, 'Data saved in  = ',output
    if do_progress then progress, /ending, progress_tlb
    
; ***	Custom file write and close here ...
	
	if n_elements(hist) gt 0 then begin
		for i=0,nmax-1 do begin
			printf, luno, format='(I4,",",I10)', i, hist[i]
		endfor
	endif
	close_file, luno
	
    return

bad_output:
    warning, output=cluster_debug, 'process_EVT_template', 'Output file could not be opened.'
    goto, cleanup
bad_file:
    warning, output=cluster_debug, 'process_EVT_template', 'EVT file not found.'
    goto, cleanup
bad_obj:
	warning, output=cluster_debug,'process_EVT_template', 'Bad device object for: '+device
    goto, cleanup
end
