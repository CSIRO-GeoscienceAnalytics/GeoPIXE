pro da_trav_evt, filei, matrix, xrange=xrange, charge=charge, $
          cal_a=cal_a, cal_b=cal_b, xcompress=xcompress, microns=microns, $
          output=outputi, events=events, channel=channeli, detector=detector, $
          scanx=scanx, sample=sample, grain=grain, comment=comment, $
          progress=do_progress, spectra=spectra, device=devicei, group=group, $
          throttle=throttle, pileup=pileup

;   Read an .evt file.
;   Analyze using the Dynamic Matrix 'matrix'.
;   Sort events from station 'channel', for 'detector' type of data (0=PIXE, 1=PIGE, ...).
;
;   If 'events' set, stop at this number of events.
;
;   'xrange' is the X range in the original data.
;   Use 'xcompress' to compress these by an integral factor.
;
;   'cal_a', 'cal_b', are the energy calibration (mandatory).
;   These must be in 'keV' to match the DA matrix file.
;   If these are vectors, then they will be indexed with the 'ste' channel returned.
;
;   Write elemental images out as a .dai DA image file.
;   Use filename 'output', if specified.
;
;   'scanx' is optional scan size (microns).
;   'sample', 'grain', 'comment' are optional strings.
;   'throttle' gives name of throttle factors file.
;   'pileup' give name of pileup Time-over-threshold limits file.
;
;   'Channel' gives the station/ADC numbers, which start at '0'.
;   This can be a vector of channels to sort, or -1 meaning all.
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
;common c_null_image_1, max_image_cal
common c_seed, seed
;if n_elements(max_image_cal) lt 1 then max_image_cal = 8
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
       warning,'da_trav_evt',['IDL run-time error caught.', '', $
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
obj = obj_new(device)
if obj_valid(obj) eq 0 then goto, bad_obj

if n_params() lt 2 then begin
    print,'da_trav_evt: missing arguments'
    return
endif
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then begin
    if n_elements(last) ge 1 then begin
       path = extract_path(last)
    endif else begin
       path = 'g:\nmp'
    endelse

    file = file_requester(/read, path=path, $
       title='Select EVT file to sort', filter='*'+ obj->extension(), $
       /fix_filter)
endif else begin
    path = extract_path( file[0])
    if path eq file[0] then begin
       file = file_requester(/read, path=path, file=file[0], $
         title='Select EVT file to sort', filter='*'+ obj->extension(), $
         /fix_filter)
    endif
endelse
if strlen(file[0]) lt 1 then goto, bad_file
last = file[0]

if n_elements(channeli) lt 1 then channeli = 0L
if n_elements(xrange) lt 1 then xrange = 256
if n_elements(charge) lt 1 then charge = 0.0
if n_elements(cal_a) lt 1 then cal_a = matrix.cal_orig.a
if n_elements(cal_b) lt 1 then cal_b = matrix.cal_orig.b
if n_elements(xcompress) lt 1 then xcompress = 1
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
if (n_elements(microns) lt 1) and (scanx gt 1.0e-6) then microns = float(scanx)/float(xrange)

output = strip_file_ext(outputi) + '.trav'

channel = channeli									; this is new
s = size(matrix,/structure)
if s.type_name ne 'STRUCT' then goto, bad_matrix

charge = float(charge)

xcompress = (xcompress > 1)
xrange2 = fix( xrange / xcompress)

processed = 0LL
valid = 0LL
bad_xy = 0LL
clipped = 0LL
pileup_losses = 0LL
cancel = 0
etitle = ''

do_pileup = 0
do_throttle = 0
if obj->pileup() then begin
	pileup_limit = get_pileup(pileup, do_pileup=do_pileup)
endif
if obj->throttle() then begin
	throttle_factor = get_throttle(throttle, do_throttle=do_throttle)
endif

print,' Sort using DA matrix: n_el = ', matrix.n_el

image = fltarr(xrange2,1,matrix.n_el)
image_error = fltarr(xrange2,1,matrix.n_el)

da_matrix = matrix.matrix

n_det = n_elements(channel)
array = 0
if n_elements(channel) gt 1 then array=1
if channel[0] eq -1 then begin
    n_det = n_elements(cal_a)
    channel = indgen(n_det)							; all channels
    if n_det gt 1 then array=1
endif
nmax = max([channel,n_det-1])
print,'max_det=',nmax+1
print,'channel=',channel

j = 0L
nj = n_elements(file)
;evt_file = strip_file_ext( file) + obj->extension()
evt_file = file
first = 1
random_subset = 0

loop_file:
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, next

    device_specific, obj,1, xrange,1, n_guide,progress_file,progress_size=progress_size, first=first, $
                   flux=flux, dead_fraction=dead_fraction, group=group, error=err
    if err then goto, finish
    nprogress = ((100000L / n_guide) > 1L) < 500L
    first = 0

    if j eq 0 then begin
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
              p = { unit:0, current:0L, size:nj, file:evt_file[j], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
              end
          2: begin
              p = { unit:0, current:0L, size:xrange*yrange, file:evt_file[0], value:[0LL,0LL,0LL,0LL,0LL,0LL]}
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

       read_buffer, obj, 1, x1,y1,e, channel,n, xcompress,1,  $
          ecompress=matrix.ecompress, title=etitle, multiple=multiple, $
          total_bad_xy=sbad_xy, total_processed=processed, $
          processed=count1, valid=good, station_e=ste, time=tot, $
          flux=flux, dead_fraction=dead_fraction, file=evt_file[j], error=err
       if err then goto, next
       if good eq 0 then goto, cont
        bad_xy = bad_xy + sbad_xy

       if do_pileup or do_throttle then begin
         if multiple[0] eq -1 then multiple = replicate(1L, n)
       endif
       if do_pileup and (n_elements(tot) eq n) then begin
         q = where( (tot ge pileup_limit[0,e]) and (tot le pileup_limit[1,e]), good)
         if good eq 0 then goto, cont
         pileup_losses = pileup_losses + (n-good)
         e = e[q]
         x1 = x1[q]
         y1 = y1[q]
         ste = ste[q]
         multiple = multiple[q]
       endif
       if do_throttle then begin
         multiple = multiple * throttle_factor[e]
       endif

;     Translate event energy to DA matrix column.

       if n_elements(cal_a) eq 1 then begin
         energy = cal_a * float(e) + cal_b
       endif else begin
         energy = cal_a[ste] * float(e) + cal_b[ste]
       endelse

       col = uint( (energy - matrix.cal.b)/matrix.cal.a + 0.5)      ; - 1.0)    ; VAX - PC channel error

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
              ((x1 ge 0) and (x1 lt xrange2)) and $
              ((y1 ge 0) and (y1 lt 1)), count2)
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
         x2 = x1[q]
         y2 = y1[q]
         col2 = col[q]
         if n_elements(multiple) gt 1 then begin
          multiple2 = multiple[q]
         endif else begin
          multiple2 = multiple[0]
         endelse
                       				       ; warning: count is only ~50000
         if random_subset then begin       ; so this limits how small fraction can be
          seed = i+1            			 ; to force a known seed and random sequence
          accept_fraction = 1.0
          nr = long(float(count2) * accept_fraction) > 1
          qr = long(float(count2) * randomu(seed,nr) < (count2-1))
          x2 = x2[qr]
          y2 = y2[qr]
          col2 = col2[qr]
          multiple2 = multiple2[qr]
          count2 = n_elements(qr)
         endif
         valid = valid + count2

;      Pass the event arrays (x2,y2,col2) and the 'image' array
;      and the DA 'matrix' to the DLL routine 'da_accumulate2'.
;      This will accumulate the DA values in 'col2' into
;      'image' at coords 'x2,y2', for each element.

;      For detector arrays, divide accumulated DA increments here (in da_accumulate2)
;      by the number of detector elements. This does assume that all detectors sample
;      the same sensivity and count rate.

         err = da_accumulate3( x2,y2,col2,count2, image,xrange2,1, $
                   image_error,xrange2,1, matrix.n_el, da_matrix, matrix.size, $
                    multiple=multiple2)
         if err ne 0 then begin
          print,'da_trav_evt: error (',err,') return from da_accumulate3'
          goto, finish
         endif
       endif
       if events gt 0 then if processed gt events then begin
         print,'da_trav_evt: requested event count exceeded; stop.'
         goto, finish
       endif

cont:
       i = i+1
       endwhile

next:
    j = j+1
    if j lt nj then goto, loop_file

finish:
    if do_throttle then print,'    Used THROTTLE file ',throttle
    if do_pileup then print,'  Used PILEUP file ',pileup
    print, ' processed = ', processed
    print, ' valid events = ',valid
    print, ' bad event triplets = ',bad_xy
    print, ' clipped to image,matrix bounds, or not station ',channel+1,' = ', clipped
    print, ' pileup losses = ',pileup_losses
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
              p.current = i
              end
          else:
         endcase
         progress, /update, progress_tlb, p
       endif
    endif

    close, 1
    on_ioerror, null
    t = 'EVT sorting complete. Save Images: '+strip_file_ext(output)+'.dai'
    if do_progress then begin
       progress, /complete, progress_tlb, t
    endif

;--------------------------------------------------------------------------
;
;   Write the image results file ...

null_image = define(/image)
img = null_image
q = where(finite(image) eq 0, nq)
if q[0] ne -1 then begin
	image[q] = 0.0
	print,'Killed ',nq,' non finite pixels.'
endif

print,'da_trav_evt: write image file - ',strip_file_ext(output)+'.dai'

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

xstep_on = 0L
xstep_count = 0L
step_events = 0L
step_toggle = 0L
toggle_bit = 0L
step_station = 0L
type = 1L  			              ; 0=counts data, 1=traverse concs

null_spectrum = define(/spectrum)
siz = xrange2

ca = 1.0
units = 'steps'
if microns gt 0.0 then begin
    units = 'microns'
    ca = microns * xcompress
endif

p = ptrarr(matrix.n_el)
for i=0L,matrix.n_el-1 do begin
    spec = null_spectrum
    spec.file = output
    spec.source = evt_file[0]
	spec.source2 = evt_file[nj-1]
	spec.throttle = throttle
    spec.pileup = pileup
	spec.DevObj = clone_device_object(obj)
    spec.label = strtrim(matrix.el[i],2)
	spec.type = type

    spec.history[0] = 'Extracted from EVT using DA ' + strtrim(matrix.label,2)
    spec.n_history = 1
    spec.x = 0.0
    spec.y = 0.0

    spec.charge = charge

    ; need to collect 'dead_fraction here and calculate a 'live'
    ; but do we correct 'charge' or scale the spectrum?
    ; is the charge already DT corrected?

    live = 1.0
    spec.scan.y = 0.0
	if (obj->name() eq 'OM_DAQ_DEVICE') then begin
	    charge = lmf_charge
	    dead = 0.0
	    if n_elements(group) ge 1 then charge = OM_charge_select(group,charge, live=lmf_live, dead=dead)
	    live = 1.0-dead
	    spec.charge = charge
	    scanx = float(lmf_size[0])
	    spec.scan.x = lmf_size[0]*0.001
	endif else begin
	    spec.scan.x = scanx*0.001
	endelse

	if n_elements(flux) gt 1 then begin
	    tflx = total(flux)
	    if tflx gt 0.001 then begin
;	       if (device eq 17) or (device eq 14) or (device eq 15) or (device eq 10) then begin
;	         c = spec.charge
;	         if aps_count_to_charge gt 1.e-20 then c=aps_count_to_charge*tflx
;	         head = {charge:c, fluence:tflx, error:0}
;	         head = APS_charge_select( group, head)
;	         if head.error eq 0 then spec.charge = head.charge
;	       endif
	       avf = tflx / (float(xrange2))
	       q = where(flux gt 0.1*avf)
	       scale = flux
	       scale[*] = 1.0
	       tf = total(flux[q])
	       scale[q] = ((flux[q] * n_elements(q) / tf) > 0.1) < 10.

	       for i=0L,matrix.n_el-1 do begin
	         image[*,0,i] = image[*,0,i] / scale
	       endfor
	    endif
	endif
	if random_subset then spec.charge = spec.charge*accept_fraction

	if spec.charge lt 1.0e-10 then begin
	    print,'da_trav_evt: Error - charge zero; can not set concentration scale.'
	    charge_per_channel = 1.0
	endif else begin
	    charge_per_channel = spec.charge/siz
	endelse

    spec.cal.order = 1
    spec.cal.units = units							; distance scale
    spec.cal.poly[0] = 0.0
    spec.cal.poly[1] = ca

	ic = 0
	if channel[0] ge 0 then ic=channel[0]
    spec.ecal.order = 1
    spec.ecal.units = units							; energy cal
	spec.ecal.poly[0:1] = [cal_b[ic], cal_a[ic]]

    spec.station = channel[0]+1
    spec.channel = channel[0]
	spec.detector = detector
    spec.multiplicity = n_elements(channel) > 1
	if array eq 0 then begin
	    spec.array = 0
	    n_active = 1
	    spec.pactive = ptr_new( channel[0])
	endif else begin
	    spec.array = 1
	    n_active = n_elements(channel)
	    spec.pactive = ptr_new( channel)
	endelse

	spec.ecompress = matrix.ecompress
	spec.sample = sample
	spec.grain = grain
	if lenchr(comment) gt 0 then begin
	    spec.comment = comment
	endif else if n_elements(etitle) gt 0 then begin
	    spec.comment = etitle
	endif

    spec.processed = processed
    spec.valid = valid
    spec.bad_xy = bad_xy
    spec.clipped = clipped

    spec.matrix.label = matrix.label
    spec.matrix.file = matrix.file
    spec.matrix.charge = matrix.charge
    spec.matrix.mdl = ptr_new(matrix.mdl)

    spec.xstep_on = xstep_on
    spec.xstep = xstep_count
    spec.step_events = step_events
    spec.step_toggle = step_toggle
    spec.toggle_bit = toggle_bit
    spec.toggle_station = step_station
    spec.microns = microns
    spec.events = events                ; terminate events, NOT total events

    spec.xcompress = xcompress
    spec.ycompress = 1
    spec.nx = xrange2
    spec.ny = 0

    spec.size = siz
    scale = (i eq 0) ? 100000.0 : 1.0
    spec.data = ptr_new( scale * reform(image[0:siz-1,0,i]) / charge_per_channel )

    spec.has_errors = 1
    spec.error = ptr_new( scale * sqrt(reform(image_error[0:siz-1,0,i])) / charge_per_channel )

    spec.log = 0
    spec.showfit = 1
    spec.show = 0
    if i eq 0 then spec.show=1

    p[i] = ptr_new( spec, /no_copy)
endfor

write_spec, p, output, /trav

pp = ptr_new( p, /no_copy)

if arg_present(spectra) eq 0 then begin
    free_spectra, pp
endif else begin
    spectra = pp
endelse

cleanup:
	if obj_valid(obj) then obj_destroy, obj
    progress, /ending, progress_tlb
    return

bad_matrix:
    warning, 'da_trav_evt', 'Bad DA matrix.'
    goto, cleanup
bad_file:
    warning, 'da_trav_evt', 'EVT file not found.'
    goto, cleanup
bad_obj:
	warning, 'da_trav_evt', 'Bad device object for: '+device
    goto, cleanup
end
