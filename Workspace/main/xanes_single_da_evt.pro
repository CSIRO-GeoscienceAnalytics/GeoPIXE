pro xanes_single_da_evt, filei, matrixi, charge=charge, $
          cal_a=cal_a, cal_b=cal_b, suppress=suppress, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          progress=do_progress, device=devicei, group=group, cuts=use_cuts, $
          throttle=throttle, pileup=pileup, linearize=linearize, energies=energies

;	Quick hack to scan a single list of blog segments, and put the results
;	into separate XANES energy bins, based on its energy position in an energy list.
;	List must be in ascending order, and in the same units as the desired energy PV,
;	which defaults to containing "ENERGY_".
;	
;   file		list-mode file(s) to process.
;   			If 'file' is missing, then prompt for files directly.
;   matrix		Analyze using this Dynamic Matrix.
;   /cuts		'matrix' is actually a cuts struct. Cuts will be done via
;   			setting DA style matrix rows.
;   detector	type of data (0=PIXE, 1=PIGE, ...).
;   device		list-mode device/format type
;   charge		charge/flux for whole image (some devices will pop-up a
;   			requester to set PVs and charge conversion)
;
;   cal_a,cal_b	 the energy calibration (mandatory).
;   			These must be in 'keV' to match the DA matrix file.
;   			If these are vectors, then they will be indexed with the 'ste' channel returned.
;
;   output		Output file, Write elemental images out as a .csv XANES file.
;
;   group		group_leader for progress
;   /do_progress pop-up a progress bar
;
;	energies	file giving list of energies (in same units as ENERGY_ PV in monitor records)
;   throttle 	gives name of throttle factors file.
;   pileup 		give name of pileup Time-over-threshold limits file.
;	linearize 	give the name of a linearization function file.
;   events		If set, stop at this number of events.
;	
;   Channel 	gives the station/ADC numbers, which start at '0'.
;   			This can be a vector of channels to sort, or -1 meaning all.
;
; Multiple detectors:
;   This is supported by passing a vector of channels to all detectors in 'channel',
;   and passing 'cal_a' and 'cal_b' as vectors over ALL channels, not just those in 'channel'.

COMPILE_OPT STRICTARR
common c_evt_last, last
common c_om_3b, c_per_pulse, lmf_charge
common c_om_4, lmf_size
common c_om_5, lmf_live
common aps_4, aps_count_to_charge
common c_geopixe_adcs, geopixe_max_adcs
common c_null_image_1, max_image_cal
common c_seed, seed
common c_debug_dummy, dummy_write
if n_elements(dummy_write) lt 1 then dummy_write=0
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8
if n_elements(seed) lt 1 then seed=1L
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=0.

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'xanes_single_da_evt',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

images = 0L
define_devices
if n_elements(devicei) lt 1 then devicei='MAIA_DEVICE'
device = devicei
if n_elements(use_cuts) lt 1 then use_cuts = 0

if n_elements(filei) lt 2 then begin
	evt_file = file_requester(filter = '*', title='Select multiple Maia XANES files', /multiple)
endif else evt_file=filei

if (n_elements(matrixi) lt 1) and use_cuts then goto, bad_matrix
if n_elements(matrixi) lt 1 then begin
	f = file_requester(filter = '*.damx', title='Select the DA matrix file', fix_filter=1)
	if f eq '' then return
	matrix = read_da(f, error=error)
	if error then begin
		warning,'xanes_single_da_evt','error reading DA matrix file'
		return
	endif
endif else matrix=matrixi

if use_cuts then begin
	cuts = *matrixi				; matrix par is actually cuts in /cuts mode
	nc = n_elements(cuts)
	nm = max(cuts.x[3]+1)
	matrix = { $
		file:		cuts[0].file, $				; local file name
		cal_orig: 	{a:cuts[0].cal_a, b:cuts[0].cal_b}, $	; original spectrum cal
		cal:		{a:cuts[0].cal_a, b:cuts[0].cal_b}, $	; cal of DA matrix rows
		station: 	0, $						; detector station number
		charge:		1.0, $						; charge of DA matrix. for MDLs
		n_el:		nc, $						; number of element rows
		el:			cuts.el, $					; element names
		mu_zero: 	fltarr(nc), $				; major-line mass absorption coeffs
		density0: 	0.0, $						; density of top layer
		ecompress: 	1, $						; spectrum compression factor
		mdl:		fltarr(nc), $				; mdls at this charge
		size:		nm, $						; size of a row
		matrix:		fltarr(nm,nc), $			; matrix
		yield:		replicate(1.,nc), $			; yield factors
		array:		{on:0, n_det:0, rGamma:0.0 }, $		; array, yield-ratios, ...
		n_pure:		0, $						; # of pure element unit spectra
		pure:		0 }							; pure spectra
	
	for i=0L,nc-1 do begin
		matrix.matrix[ cuts[i].x[2]:cuts[i].x[3], i] = 1.0
	endfor
endif

if n_elements(channeli) lt 1 then channeli = 0L
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(cal_a) lt 1 then cal_a = matrix.cal_orig.a
if n_elements(cal_b) lt 1 then cal_b = matrix.cal_orig.b
if n_elements(events) lt 1 then events=0L
if n_elements(sample) lt 1 then sample = '?'
if n_elements(grain) lt 1 then grain = '?'
if n_elements(comment) lt 1 then comment = '?'
if n_elements(throttle) lt 1 then throttle = ''
if n_elements(pileup) lt 1 then pileup = ''
if n_elements(energies) lt 1 then energies = ''
if n_elements(detector) lt 1 then detector=0L
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(suppress) lt 1 then suppress=0

output = strip_file_ext(outputi, /double) + '.xanes.csv'
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

channel = channeli									; this is new
s = size(matrix,/structure)
if s.type_name ne 'STRUCT' then goto, bad_matrix

charge = float(charge)
processed = 0LL
valid = 0LL
bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
cancel = 0
etitle = ''
min_x = 100000
max_x = 0

do_linear = 0
do_pileup = 0
do_throttle = 0
if obj->linear() then begin
	flinear = get_linearize(linearize, do_linear=do_linear, multi=multilinear, max=8191)
endif
if obj->pileup() then begin
	pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
endif
if obj->throttle() then begin
	throttle_factor = get_throttle(throttle, do_throttle=do_throttle)
endif
xenergy = get_xanes_energies(energies, do_xanes=do_xanes)

if do_xanes eq 0 then goto, bad_xanes
;n_xanes = n_elements(xenergy)
n_xanes = 200000L

print,' Sort using DA matrix: n_el = ', matrix.n_el

image = fltarr(n_xanes,1,matrix.n_el)
image_error = fltarr(n_xanes,1,matrix.n_el)
flux = fltarr(n_xanes)
time_array = dblarr(n_xanes)
beam_array = fltarr(n_xanes)
dead_fraction = fltarr(n_xanes,1)
nt_index = lonarr(n_xanes)
et_time = dblarr(n_xanes)

da_matrix = matrix.matrix

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

j = 0L
first = 1
n_record = 0L
nt_last = 0L
random_subset = 0
nj = n_elements(evt_file)

loop_file:
		print,'		file = ',evt_file[j]
	    on_ioerror, bad_file
	    close,1
	    openr, 1, evt_file[j], bufsiz=1500*1024L
	    on_ioerror, nextj
	
	    device_specific, obj,1, n_xanes,1, n_guide,progress_file,progress_size=progress_size, first=first, $
	                   flux=flux, dead_fraction=dead_fraction, group=group, $
	                   suppress=suppress, /xanes, error=err
	    if err then goto, finish
	    nprogress = ((100000L / n_guide) > 1L) < 500L
	    first = 0
	    progress_file = 1
	
	    if (j eq 0) then begin
	       if do_progress then begin
	         t = 'Clipped'
	         if do_pileup then t = 'Pileup'
	         progress, tlb=progress_tlb, title='Sort XANES EVT file', $           ; put up a progress bar
			 		pars=['Events','Valid','Blocks','Bad XY','Size',t]
	
	         iprogress = 0L
	         case progress_file of
	          0: begin
	              p = { unit:1, value:[0LL,0LL,0LL,0LL,0LL,0LL]}
	              end
	          1: begin
	              p = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
	              end
	          2: begin
	              p = { unit:0, current:0L, size:nj, file:evt_file[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
	              end
	          3: begin
	              p = { unit:0, current:0L, size:progress_size, file:evt_file[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
	              end
	          else:
	         endcase
	       endif
	    endif
	
	    i = 0L
	    while ~ EOF(1) do begin
	       sbad_xy = 0L
	
	       read_buffer, obj, 1, x1,y1,e, channel,n, 1,1,  $
	          ecompress=matrix.ecompress, title=etitle, multiple=multiple, $
	          total_bad_xy=sbad_xy, total_processed=processed, $
	          processed=count1, valid=good, station_e=ste, time=tot, $
	          flux=flux, dead_fraction=dead_fraction, file=evt_file[j], $
	          /xanes, xenergy=xenergy, n_record=n_record, nt_last=nt_last, $
	          tstamp=time_array, beam_array=beam_array, nt_index=nt_index, $
	          et_time=et_time, error=err
	       if err then goto, nextj
		   bad_xy = bad_xy + sbad_xy
	       if good eq 0 then goto, contj
	
	       if do_pileup or do_throttle then begin
	         if multiple[0] eq -1 then multiple = replicate(1L, n)
	       endif
	       if do_pileup and (n_elements(tot) eq n) then begin
	         q = where( (tot ge pileup_limit[0,e]) and (tot le pileup_limit[1,e]), good)
	         if good eq 0 then goto, contj
	         pileup_losses = pileup_losses + (n-good)
	         e = e[q]
	         ste = ste[q]
	         multiple = multiple[q]
	       endif
	       if do_throttle then begin
	         multiple = multiple * throttle_factor[e]
	       endif
	
			if do_linear then begin
				if multilinear then begin
				 	e = flinear[e,ste]		      ; linearize (individual tables)
				endif else begin				  ; dither not needed here (as in spec_evt)
				 	e = flinear[e]   			  ; linearize (single table)
				endelse
			endif
				 
	;     Translate event energy to DA matrix column.
	
	       if n_elements(cal_a) eq 1 then begin
	         energy = cal_a * float(e) + cal_b
	       endif else begin
	         energy = cal_a[ste] * float(e) + cal_b[ste]
	       endelse
	
	       col = uint( (energy - matrix.cal.b)/matrix.cal.a + 0.5)
	
	       if array then begin
	         if n_elements(hist) lt 1 then begin
	          hist = histogram( ste, max=nmax, min=0)
	         endif else begin
	          hist = histogram( ste, max=nmax, min=0, input=hist)
	         endelse
	       endif
	
	;     Find those events with correct station,
	;     col within the matrix, within the pixel range
	;     'count2' is the total satisfying these crtiteria.
	
	       q = where( ((col ge 0) and (col lt matrix.size)) and $
              ((x1 ge 1) and (x1 le n_xanes-1)) and (y1 eq 0), count2)
	       clipped = clipped + ( good - count2)
	
	       if do_progress then begin
	         iprogress = iprogress + 1
	         if iprogress ge nprogress then begin
	;		          			Events	Valid	Blocks	Bad XY	Size	Clipped/pileup
	          case progress_file of
	              0: begin
	                 p.value = [processed,good,i,bad_xy,n,clipped]
	                 end
	              1: begin
	                 p.value = [processed,good,i,bad_xy,n,clipped]
	                 p.current = j
	                 p.file = evt_file[j]
	                 end
	              2: begin
	                 p.value = [processed,good,i,bad_xy,n,clipped]
	                 p.current = i
	                 end
	              3: begin
	                 p.value = [processed,good,i,bad_xy,n,clipped]
	                 p.current = i
	                 end
	              else:
	          endcase
	          progress, /update, progress_tlb, p, cancel=cancel, skip=skip
	          if skip then goto, finish
	          if cancel then begin
	              close, 1
	              return
	          endif
	          iprogress = 0L
	         endif
	       endif
	
	       if count2 gt 0 then begin
	         x2 = x1[q]							; this is now LONG, hence modified xanes_accumulate
	         y2 = replicate(0US,count2)
	         col2 = col[q]
	         if n_elements(multiple) gt 1 then begin
	          multiple2 = multiple[q]
	         endif else begin
	          multiple2 = multiple[0]
	         endelse
	                       				         ; warning: count is only ~50000
	         if random_subset then begin         ; so this limits how small fraction can be
	          seed = i+1            			 ; to force a known seed and random sequence
	          accept_fraction = 1.0
	          nr = long(float(count2) * accept_fraction) > 1
	          qr = long(float(count2) * randomu(seed,nr) < (count2-1))
	          x2 = x2[qr]
	          y2 = y2[qr]
	          col2 = col2[qr]
	          multiple2 = (n_elements(multiple2) gt 1) ? multiple2[qr] : multiple2[0]
	          count2 = n_elements(qr)
	         endif
	         valid = valid + count2
	
	;      Pass the event arrays (x2,y2,col2) and the 'image' array
	;      and the DA 'matrix' to the DLL routine 'da_accumulate2'.
	;      This will accumulate the DA values in 'col2' into
	;      'image' at coords 'x2,y2', for each element.
	
	         err = xanes_accumulate3( x2,y2,col2,count2, image,n_xanes,1, $
	                   image_error,n_xanes,1, matrix.n_el, da_matrix, matrix.size, $
	                    multiple=multiple2)
	         if err ne 0 then begin
	          print,'xanes_single_da_evt: error (',err,') return from da_accumulate3'
	          goto, finish
	         endif
	       endif
	       if events gt 0 then if processed gt events then begin
	         print,'xanes_single_da_evt: requested event count exceeded; stop.'
	         goto, finish
	       endif
	
	contj:
	       i = i+1
	       endwhile
	
	nextj:
	    j = j+1
	    if j lt nj then goto, loop_file

finish:
    if do_throttle then print,'    Used THROTTLE file ',throttle
    if do_pileup then print,'  Used PILEUP file ',pileup
    if do_linear then print,'  Used LINEARIZE file ',linearize
    if do_xanes then print,'  Used XANES energies file ',energies
    print, ' processed = ', processed
    print, ' valid events = ',valid
    print, ' bad event triplets = ',bad_xy
    print, ' clipped to image,matrix bounds, or not station ',channel+1,' = ', clipped
    print, ' pileup losses = ',pileup_losses
    if bad_xy gt 10 then warning,'xanes_single_da_evt','Bad XY found; possible loss of record position in file.'
    if n_elements(flux) gt 1 then print,' found FLUX array'
    if n_elements(dead_fraction) gt 1 then print,' found DEAD_FRACTION array'
    if n_elements(hist) gt 1 then print,' Hist=',hist
    if n_elements(p) gt 0 then begin
       if do_progress then begin
         p.value = [processed,valid,i,bad_xy,n,(do_pileup ? pileup_losses: clipped)]
         case progress_file of
          1: begin
              p.current = j
              end
          2: begin
              p.current = j
              end
          else:
         endcase
         progress, /update, progress_tlb, p
       endif
    endif

    close, 1
    on_ioerror, null
    if do_progress then progress, /complete, progress_tlb, 'EVT sorting complete. Save XANES.'

;--------------------------------------------------------------------------
;
;   Write the image results file ...

nxy = long(n_xanes) * long(1)
ninf = 0L
for i=0L,matrix.n_el-1 do begin
	q = where(finite(image[*,*,i]) eq 0, nq)
	if nq gt 0 then begin
		image[q + nxy*i] = 0.0
		ninf = ninf + nq
	endif
endfor
if ninf gt 0 then print,'Killed ',nq,' non finite pixels.'

; For detector arrays, need to scale down result, depending on the number of
; detectors actually used to sort the EVT. Without an array, just count n_det.
; With an array, use the rGamma yield-ratio arrays.

if matrix.array.on then begin
	rG = fltarr(matrix.n_el)
	for i=0L,n_det-1 do begin
		if channel[i] lt matrix.array.n_det then begin
			rG = rG + matrix.array.rGamma[channel[i],*]
		endif
	endfor
	xd = 1./rG
endif else begin
	xd = replicate(1./float(n_det), matrix.n_el)
endelse
xd2 = xd*xd
for i=0L,matrix.n_el-1 do begin
	image[*,*,i] = image[*,*,i] * xd[i]
	image_error[*,*,i] = image_error[*,*,i] * xd2[i]
endfor

; Correct for flux/current variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, so the correction here must
; preserve the average.

;if n_elements(flux) gt 1 then begin
;	tflx = total(flux)
;	if tflx gt 0.001 then begin
;		use_flux = 1
;		if (device eq 17) or (device eq 14) or (device eq 15) or (device eq 16) or (device eq 10) then begin
;			c = charge
;			if aps_count_to_charge gt 1.e-20 then c=aps_count_to_charge*tflx
;			head = {charge:c, fluence:tflx, conversion:aps_count_to_charge, use_flux:1, error:0}
;			if suppress eq 0 then head = APS_charge_select( group, head, /flux_question)
;			if head.error eq 0 then begin
;				charge = head.charge
;				aps_count_to_charge = head.conversion
;			endif
;			use_flux = head.use_flux
;		endif
;		if use_flux then begin
;			avf = tflx / (float(n_xanes)*float(1))
;			q = where(flux gt 0.1*avf)
;			scale = flux
;			scale[*] = 1.0
;			tf = total(flux[q])
;			scale[q] = ((flux[q] * n_elements(q) / tf) > 0.1) < 10.
;			
;			for i=0L,matrix.n_el-1 do begin
;				image[*,*,i] = image[*,*,i] / scale
;			endfor
;		endif
;	endif
;endif
if random_subset then charge = charge*accept_fraction

print,'xanes_single_da_evt: write XANES file - ',output

n_xanes = n_xanes < n_record			; trim buffer back to max record

openw, lun2, output, /get_lun
printf, lun2, format="(I7,',',I3)", n_xanes , matrix.n_el
printf, lun2, 'i',',','Mon.index',',','ET.Time',',','Energy',',','Flux',',','M.Time',',',strjoin( matrix.el,',')

t0 = min([et_time[0], time_array[0]])
for i=0L,n_xanes-1 do begin
	j = nt_index[i]
	s = strjoin( str_tidy(reform(image[i,0,*])), ',')
;	printf, lun2, format="(I4,',',F9.4,',',A)", i, xenergy[i], s
	printf, lun2, format="(I7,',',I7,',',G16.8,',',G16.8,',',G16.8,',',G16.8,',',A)", i, j, et_time[i]-t0, beam_array[i], flux[i], time_array[i]-t0, s
endfor
close_file, lun2

cleanup:
    progress, /ending, progress_tlb
    if obj_valid(obj) then obj_destroy, obj
    return

bad_xanes:
    warning, 'xanes_single_da_evt', 'Bad energies.'
    goto, cleanup
bad_matrix:
    warning, 'xanes_single_da_evt', 'Bad DA matrix.'
    goto, cleanup
bad_file:
    warning, 'xanes_single_da_evt', 'EVT file not found.'
    goto, cleanup
bad_obj:
	warning, 'xanes_single_da_evt', 'Bad device object for: '+device
    goto, cleanup
end
