pro xanes_da_evt, dirfilei, matrix=matrix_file, charge=charge, xcompress=xcompress, ycompress=ycompress, $
          cal_a=cal_a, cal_b=cal_b, suppress=suppress, xrange=xrange, yrange=yrange, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          scanx=scanx, scany=scany, progress=do_progress, device=devicei, group=group, $
          throttle=throttle, pileup=pileup, linearize=linearize, $
          sample=sample, grain=grain, comment=comment, devpars=devpars, ic=flux_ic, $
          pv_list=pv_list, flatten=flatten

;	Quick hack to scan a series of directories for blog segments, and put the results
;	into separate XANES energy bins, one for each directory. Get energy from header info
;	for first file in each run dir.
;	
;   dirfile		list-mode file dir(s) to process.
;   			groups of files in directories grouped by beam energy.
;   			If 'dirfile' is missing, then prompt for dirs directly.
;   matrix_file	load Dynamic Analysis Matrix from file 'matrix_file'
;   detector	type of data (0=PIXE, 1=PIGE, ...).
;   device		list-mode device/format type
;   devpars		device dependent options parameter struct
;   charge		charge/flux for whole image (some devices will pop-up a
;   			requester to set PVs and charge conversion)
;   xrange		the X ranges in the original data
;   yrange		Y range
;   xcompress	compress these by an integral factor.
;   ycompress
;
;   cal_a,cal_b	 the energy calibration (mandatory).
;   			These must be in 'keV' to match the DA matrix file.
;   			If these are vectors, then they will be indexed with the 'ste' channel returned.
;   scanx,scany  are optional scan sizes (microns).
;
;   output		Output file, Write elemental images out as a .csv XANES file.
;
;   group		group_leader for progress
;   /do_progress pop-up a progress bar
;
;   sample,grain,comment are optional strings.
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
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384
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
       warning,'xanes_da_evt',['IDL run-time error caught.', '', $
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

if n_elements(dirfilei) lt 2 then begin
	dirfile = file_requester(filter = '*', title='Select multiple Maia XANES directories', /multiple, /dir)
	if xrange lt 2 then return
endif else dirfile=extract_path( dirfilei)
n_xanes = n_elements(dirfile)

if n_elements(channeli) lt 1 then channeli = 0L
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(xrange) lt 1 then xrange = 1024
if n_elements(yrange) lt 1 then yrange = 1024
if n_elements(cal_a) lt 1 then cal_a = matrix.cal_orig.a
if n_elements(cal_b) lt 1 then cal_b = matrix.cal_orig.b
if n_elements(xcompress) lt 1 then xcompress = 1
if n_elements(ycompress) lt 1 then ycompress = 1
if n_elements(events) lt 1 then events=0L
if n_elements(scanx) lt 1 then scanx = 0.0
if n_elements(scany) lt 1 then scany = 0.0
if n_elements(sample) lt 1 then sample = '?'
if n_elements(grain) lt 1 then grain = '?'
if n_elements(comment) lt 1 then comment = '?'
if n_elements(throttle) lt 1 then throttle = ''
if n_elements(pileup) lt 1 then pileup = ''
if n_elements(detector) lt 1 then detector=0L
if n_elements(outputi) lt 1 then outputi=file[0]
if n_elements(do_progress) lt 1 then do_progress=0
if n_elements(suppress) lt 1 then suppress=0
if n_elements(flux_ic) lt 1 then flux_ic = {mode:0, pv:'', val:0.0, unit:0.0, conversion:1., use_dwell:0, dwell:1.0}
if n_elements(pv_list) lt 1 then pv_list=['']
if n_elements(flatten) lt 1 then flatten=0

output = strip_file_ext(outputi, /double) + '.xanes.csv'
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

channel = channeli
if (n_elements(matrix_file) lt 1) then goto, bad_matrix
print, 'Use Matrix File="',matrix_file,'"'
matrix_all = read_da( matrix_file, error=err)			; read DA for all energies
matrix = matrix_all
if err then goto, bad_matrix

charge = float(charge)
xcompress = (xcompress > 1)
ycompress = (ycompress > 1)
xrange2 = fix( xrange / xcompress)
yrange2 = fix( yrange / ycompress)
;xrange2e = (xrange2+1)/2
;yrange2e = (yrange2+1)/2
xrange2e = xrange2
yrange2e = yrange2

processed = 0LL
valid = 0LL
bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
cancel = 0
etitle = ''
min_x = 100000L
max_x = 0L

if n_elements(devpars) gt 0 then obj->set_options, devpars
do_linear = 0
do_pileup = 0
do_throttle = 0
do_attributes = 0
if obj->linear() then begin
	flinear = get_linearize(linearize, do_linear=do_linear, multi=multilinear, max=8191)
endif
if obj->pileup() then begin
	pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
endif
if obj->throttle() then begin
	throttle_factor = get_throttle(throttle, do_throttle=do_throttle)
endif

; Note: matrix.n_el may change later when read_da is called with a valid beam_energy.
; (happens if beam_energy is less than 10 keV as "Compton, elastic" combined to "scatter" in DA)
; This means that the size of 'image' and 'image_error' here may be larger than saved later.
; This is only OK (e.g. to pass to Fortran) because n_el is LAST index in arrays.

print,' Sort using DA matrix: n_el = ', matrix.n_el

image = fltarr(n_xanes,1,matrix.n_el)
image_error = fltarr(n_xanes,1,matrix.n_el)
flux = fltarr(n_xanes)
dead_fraction = fltarr(n_xanes)
pileup_loss_map = fltarr(n_xanes)
nnpu = lonarr(n_xanes,1)
nn = lonarr(n_xanes,1)
count_rate_map = fltarr(n_xanes)
xanes_energy = fltarr(n_xanes)
dwell_time = fltarr(n_xanes)

da_matrix = matrix.matrix

n_det = n_elements(channel)
array = 0
if n_det gt 1 then array=1
if channel[0] eq -1 then begin
    n_det = n_elements(cal_a)
    channel = indgen(n_det)								; all channels
    if n_det gt 1 then array=1
endif
nmax = max([channel,n_det-1])
print,'max_det=',nmax+1
print,'channel=',channel

flux2 = 0.0												; flux for each energy
dead_fraction2 = fltarr(nmax+1,1)						; dead_fraction per detector at each energy
k = 0L
random_subset = 0			; 1							; edit for manual random subset
accept_fraction = 1.		; 0.005
if random_subset then warning,'xanes_DA_evt','Random subset of "'+str_tidy(accept_fraction)+'" selected.'

loop_xanes:
	f = dirfile[k]
	print,'Xanes_DA_evt: dir = ',f
	n = strlen(f)
	t = strmid( f, n-1,1)
	ps = path_sep()
	if t ne ps then f = f+ps
	file = find_file2(f + '*', /extension_numeric)
	flux2 = 0.0
	dead_fraction2[*] = 0.0
	elapsed2 = 0.0
	first = 1

	j = 0L
	nj = n_elements(file)
	if nj lt 1 then begin
		print,'xanes_DA_evt: no files for dir=',dirfile[k]
		goto, nextk
	endif
	evt_file = file
	
	loop_file:
		print,'		file = ',evt_file[j]
	    on_ioerror, bad_file
		close,1
		openr, 1, evt_file[j], bufsiz=1500*1024L
		on_ioerror, nextj
	
		device_specific, obj,1, xrange,yrange, n_guide,progress_file,progress_size=progress_size, first=first, $
						suppress=suppress, flux=flux2, dead_fraction=dead_fraction2, $
						ic=flux_ic, beam_energy=beam_energy, error=err
		if err then goto, finish
		if first then begin
			if beam_energy lt 0.5 then begin
				warning,'xanes_da_evt','Missing beam energy for XANES step, file='+evt_file[j]
			endif

			nl = n_elements(*matrix_all.pmore)
			e_da = fltarr(nl)
			for l=0,nl-1 do begin
				e_da[l] = (*(*matrix_all.pmore)[l]).e_beam
			endfor
			q = sort( abs( e_da - beam_energy))
			if abs(e_da[q[0]]-beam_energy) gt 0.001 then begin
				warning,'xanes_da_evt',['Chosen stack DA matrix for XANES using poor energy match.', $
						'E requested ='+str_tidy(beam_energy),'E match ='+str_tidy(e_da[q[0]])]
			endif
			matrix = *(*matrix_all.pmore)[q[0]]
			print,' Sort using DA matrix: n_el = ', matrix.n_el
			print,'Use Matrix File="',matrix_file,'", energy=',beam_energy
			da_matrix = matrix.matrix
			xanes_energy[k] = beam_energy
		endif
	    
		first = 0
		progress_file = 1
	
		if (k eq 0) and (j eq 0) then begin
			nprogress = ((100000L / n_guide) > 1L) < 500L
			if do_progress then begin
				t = 'Clipped'
				if do_pileup then t = 'Pileup'
				progress, tlb=progress_tlb, title='Sort XY EVT file', $           ; put up a progress bar
						pars=['Events','Valid','Blocks','Bad XY','Size',t]
	
				iprogress = 0L
				case progress_file of
					0: begin
						p = { unit:1, value:[0LL,0LL,0LL,0LL,0LL,0LL]}
						end
					1: begin
						p = { unit:0, current:0L, size:n_xanes, file:dirfile[k], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
						end
					2: begin
						p = { unit:0, current:0L, size:xrange*yrange, file:dirfile[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
						end
					3: begin
						p = { unit:0, current:0L, size:progress_size, file:dirfile[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
						end
					else:
				endcase
			endif
		endif
	
		i = 0L
		while ~ EOF(1) do begin
			sbad_xy = 0L
	
			read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,ycompress,  $
				ecompress=matrix.ecompress, title=etitle, multiple=multiple, $
				total_bad_xy=sbad_xy, total_processed=processed, beam_energy=beam_energy, $
				processed=count1, valid=good, station_e=ste, time=tot, $
				flux=flux2, dead_fraction=dead_fraction2, file=evt_file[j], error=err
			if err then goto, nextj
			bad_xy = bad_xy + sbad_xy
			if good eq 0 then goto, contj
	
			if do_pileup or do_throttle then begin
				if multiple[0] eq -1 then multiple = replicate(1L, (n>1))
			endif
			if do_pileup and (n gt 0) and (n_elements(tot) eq n) then begin
				pu = uint( (tot lt pileup_limit[0,e]) or (tot gt pileup_limit[1,e]))
				q = where( pu eq 1, nq)
				pileup_losses = pileup_losses + nq
			endif
			if do_throttle then begin
				multiple = multiple * throttle_factor[e]
			endif
	
			if do_linear then begin
				if multilinear then begin
					e = flinear[e,ste]		     ; linearize (individual tables)
				endif else begin				 ; dither not needed here (as in spec_evt)
					e = flinear[e]   			 ; linearize (single table)
				endelse
			endif
				 
	;		Translate event energy to DA matrix column.
	
			if n_elements(cal_a) eq 1 then begin
				energy = cal_a * float(e) + cal_b
			endif else begin
				energy = cal_a[ste] * float(e) + cal_b[ste]
			endelse
			if beam_energy gt 0. then xanes_energy[k]=beam_energy
	
			col = uint( (energy - matrix.cal.b)/matrix.cal.a + 0.5)
	
			if array then begin
				if n_elements(hist) lt 1 then begin
					hist = histogram( ste, max=nmax, min=0)
				endif else begin
					hist = histogram( ste, max=nmax, min=0, input=hist)
				endelse
			endif
	
			x1 = replicate(uint(k),n)			; 'k' is step in XANES energy series
			y1 = replicate(0US,n)
	
	;		Find those events with correct station,
	;		col within the matrix, within the pixel range
	;		'count2' is the total satisfying these crtiteria.
	
			q = where( ((col ge 0) and (col lt matrix.size)), count2)
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
							p.current = k
							p.file = dirfile[k]
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
				x2 = x1[q]	
				y2 = y1[q]
				col2 = col[q]
				pu2 = pu[q]
				if n_elements(multiple) gt 1 then begin
					multiple2 = multiple[q]
				endif else begin
					multiple2 = multiple[0]
				endelse
														; warning: count is only ~50000
				if random_subset then begin				; so this limits how small fraction can be
					seed = i+1							; to force a known seed and random sequence
					nr = long(float(count2) * accept_fraction) > 1
					qr = long(float(count2) * randomu(seed,nr) < (count2-1))
					x2 = x2[qr]
					y2 = y2[qr]
					col2 = col2[qr]
					pu2 = pu2[qr]
					multiple2 = (n_elements(multiple2) gt 1) ? multiple2[qr] : multiple2[0]
					count2 = n_elements(qr)
				endif
				valid = valid + count2
	
	;			Pass the event arrays (x2,y2,col2) and the 'image' array
	;			and the DA 'matrix' to the DLL routine 'da_accumulate2'.
	;			This will accumulate the DA values in 'col2' into
	;			'image' at coords 'x2,y2', for each element.
	
				err = da_accumulate4( x2,y2,col2,pu2, count2, image,n_xanes,1, nnpu,nn, $
							image_error,n_xanes,1, matrix.n_el, da_matrix, matrix.size, $
							multiple=multiple2)
				if err ne 0 then begin
					print,'xanes_da_evt: error (',err,') return from da_accumulate3'
					goto, finish
				endif
			endif
			if events gt 0 then if processed gt events then begin
				print,'xanes_da_evt: requested event count exceeded; stop.'
				goto, finish
			endif
	
	contj:
			i = i+1
		endwhile
	
	nextj:
		t = obj->get_total_time()
		elapsed2 = elapsed2 + t
		j = j+1
		if j lt nj then goto, loop_file

nextk:
	flux[k] = flux2
	dead_fraction2 = dead_fraction2 / elapsed2
	q = where(dead_fraction gt 1., nq)
	if nq gt 0 then print,'xanes_da_evt: k=',k,' DT>1 for ',nq,' detectors.'
	dead_fraction2 = clip(dead_fraction2, 0., 1.)
	dead_fraction[k] = total(dead_fraction2) / float(nmax+1)
	dwell_time[k] = elapsed2
	
	k = k+1
	if k lt n_xanes then goto, loop_xanes

finish:
    if do_throttle then print,'    Used THROTTLE file ',throttle
    if do_pileup then print,'  Used PILEUP file ',pileup
    if do_linear then print,'  Used LINEARIZE file ',linearize
    print, ' processed = ', processed
    print, ' valid events = ',valid
    print, ' bad event triplets = ',bad_xy
    print, ' clipped to image,matrix bounds, or not station ',channel+1,' = ', clipped
    print, ' pileup losses = ',pileup_losses
    print, ' pileup fraction = ', float(pileup_losses) / processed
    if n_elements(flux) gt 1 then print,' found FLUX array'
    if n_elements(dead_fraction) gt 1 then print,' found DEAD_FRACTION array'
    if n_elements(hist) gt 1 then print,' Hist=',hist
    if n_elements(p) gt 0 then begin
       if do_progress then begin
         p.value = [processed,valid,i,bad_xy,n,(do_pileup ? pileup_losses: clipped)]
         case progress_file of
          1: begin
              p.current = k
              end
          2: begin
              p.current = k
              end
          else:
         endcase
         progress, /update, progress_tlb, p
       endif
    endif

    close, 1
    on_ioerror, null
    t = 'EVT sorting complete. Save XANES.'
    if do_progress then begin
       progress, /complete, progress_tlb, t
    endif

;--------------------------------------------------------------------------
;
;   Write the results file ...

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

; Test dead_fraction for magnitude.

if n_elements(dead_fraction) gt 1 then begin
	if n_elements(flux) eq n_elements(dead_fraction) then begin
		tdead = total(dead_fraction)/n_elements(dead_fraction)
    	if tdead gt 0.001 then begin
			q = where(dead_fraction gt 0.5, nq)
			if nq gt 0 then begin
				print,'xanes_da_evt','Extreme dead-time fraction for '+str_tidy(nq)+' pixels. Ignore these.'
				dead_fraction[q] = 0.
			endif
		endif
	endif
endif

; Combine PU losses map with dead-time map.

DT = dead_fraction
if do_pileup and (n_elements(dead_fraction) gt 1) then begin
	if (n_elements(flux) eq n_elements(pileup_loss_map)) then begin
		q = where( nn gt 0, nq)
		if nq gt 0 then begin
			pileup_loss_map[q] = float(nnpu[q]) / float(nn[q])
			print,'xanes_da_evt: pile-up losses, ave: = ', mean(pileup_loss_map[q])
		endif	

		if (n_elements(dead_fraction) gt 1) then begin
			print,'xanes_da_evt: dead-time losses, ave: = ', mean(dead_fraction)
			if (n_elements(flux) eq n_elements(dead_fraction)) then begin
				dead_fraction = 1. - (1. - dead_fraction)*(1. - pileup_loss_map)
			endif
		endif
	endif
endif
 
; Correct flux for dead-time variation. This is only needed if the charge used in the sort
; is not already 'live charge' corrected, and the flux is not corrected for DT losses
; already. Only set dead_fraction in device object if flux needs DT correction.
; Note: We don't correct extra planes of 'flux' as these do not have this dead-time.
; But we do normalize these to raw_flux.

if n_elements(dead_fraction) gt 1 then begin
	if n_elements(flux) eq n_elements(dead_fraction) then begin
		tdead = total(dead_fraction)/n_elements(dead_fraction)
    	if tdead gt 0.001 then begin
			print,'xanes_da_evt: correct flux array for DT+PU losses, ave: = ', mean(dead_fraction)

	    	flux[*] = flux * (1. - dead_fraction)
		endif
	endif
endif

; Correct for flux/current variation. Note that the charge used in the sort
; is assumed to be 'live charge' already, so the correction here must
; preserve the average.

if n_elements(flux) gt 1 then begin
	aps_count_to_charge = flux_ic.conversion
	print,'xanes_da_evt: Correct across spectra for flux, if /flatten set.'

	if flatten then begin
		pimg = ptr_new({image:ptr_new(image,/no_copy)})
		image_flux_flatten, pimg, flux, raw=raw_flux
		image = *(*pimg).image
	endif
	charge = total(flux[*]) * flux_IC.conversion
	
	if random_subset then begin
		charge = charge * accept_fraction
		flux = flux * accept_fraction
		raw_flux = raw_flux * accept_fraction
	endif
endif

print,'xanes_da_evt: write XANES file - ',output

openw, 1, output
printf, 1, format="('First-dir:,',A)", dirfile[0]
printf, 1, format="('Last-dir:,',A)", dirfile[n_xanes-1]
printf, 1, format="('Pileup:,',A)", pileup
printf, 1, format="('Throttle:,',A)", throttle
printf, 1, format="('Flux: mode:,',I,',PV:,',A,',Sensitivity:,',G,',Unit:,',G,',Conv:,',G)", $
				flux_ic.mode, flux_ic.pv, flux_ic.val, flux_ic.unit, flux_ic.conversion 
printf, 1, format="('Matrix:,',A)", matrix_file

printf, 1, format="('N_xanes:,',I,',n_el:,',I)", n_xanes , matrix.n_el
printf, 1, format="('iStep, dir, DT, Dwell(s), Energy(keV), Charge, ',A)", strjoin( matrix.el,',')
for i=0L,n_xanes-1 do begin
	s = strjoin( str_tidy(reform(image[i,0,*])), ',')
;	s2 = strjoin( str_tidy(reform(sqrt(image_error[i,0,*]))), ',')

	q = flux[i] * flux_IC.conversion
	printf, 1, format="(I4,',',A,',',G,',',G,',',G,',',G,',',A)", $
				i, dirfile[i], dead_fraction[i], dwell_time[i]/1000., xanes_energy[i], q, s
endfor

cleanup:
    progress, /ending, progress_tlb
	close_file, 1
	free_DA, matrix_all
	if obj_valid(obj) then obj_destroy, obj
    return

bad_matrix:
    warning, 'xanes_da_evt', 'Bad DA matrix.'
    goto, cleanup
bad_file:
    warning, 'xanes_da_evt', 'EVT file not found.'
    goto, cleanup
bad_obj:
	warning, 'xanes_da_evt', 'Bad device object for: '+device
    goto, cleanup
end
