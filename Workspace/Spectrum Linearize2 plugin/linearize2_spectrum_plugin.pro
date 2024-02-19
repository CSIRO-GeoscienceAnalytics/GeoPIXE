;
;  Linearize Spectrum plugin routine
;  -----------------------------
;
;  Linearize a spectrum based on centroids peaks detected in View range evenly spaced pulser peaks.
;  The result is to develop a correction mapping to linearize the spectrum and output this table
;  with fitting parameters, etc.

;  Fit linearization function to each detector channel. The View markers are assumed to be set
;  for all detectors in "energy" space, i.e. all channels have been trimmed to match first detector.
;
;  Plugin arguments:
;	p		pointer to the GeoPIXE spectrum structure array for the present loaded spectra
;	i		the number of the presently displayed spectrum.
;	marks	array of all marker channel values (all marker sets, see below)
;
;  keywords:
;	history		return a history string along with the spectrum result.
;	title		just return the title string (to go in the menu).
;
;  On return to GeoPIXE, it is assumed that only the contents of the selected spectrum have
;  been changed, and the sizes of spectra all remain unchanged.
;  Avoid tinkering with spectrum structure parameters, as strange things may happen.
;
;----------------------------------------------------------------------------------------------

pro linearize2_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR
common c_non_linearity3, centre_spec

if arg_present(title) then begin
	title = '* Build Linearize2 by chip Plugin'					; return the menu title for this plugin
	return												; "*" indicates multi-spectrum processing
endif

fraction = 0.01											; fraction threshold for peak
resolve_routine, 'non_linearity', /is_function
resolve_routine, 'non_linearity3', /is_function
resolve_routine, 'lmfit', /is_function

i = ii
;cview = round(marks[0:1,3])									; View: V0-V1 markers (channel #)
;eview = cview * (*p[i]).cal.poly[1] + (*p[i]).cal.poly[0]	; in "energy" (gain trimmed) units
;if eview[1] le eview[0] then goto, bad_view
iview = [0,4095]
 
;...............................................................................................

drop = [['Simple polynomial non-linearity model','Two component non-linearity model','','',''], $
		['Fifth order polynomial (recommended)','Seventh order polynomial','Ninth order polynomial', $
		'11th order polynomial','13th order polynomial']]
help_drop = ['Select: (i) simple polynomial, (ii) Two component non-linearity model using an exponential plus polynomial' , $
		'Select order of the polynomial component of the fit (Fifth order is recommended).']
lfile = strip_file_ext((*p[i]).file)
if lenchr(lfile) lt 1 then lfile='Maia'
file = 'Output file'
initial_file = lfile + '.linear.var'
help_file = 'Select the name for the output Linear file, which will have the extension .linear.var extension.'
text = ['Peak Threshold']
initial_text = str_tidy(string([0.25]))
help_text = ['Fraction threshold for each peak, relative to top of the peak. Take centroid of all channels in each peak down to this threshold.']
Help_default = 'Linearize to the centroids of all peaks above Threshold. Automatically rejects partial peaks at ends.'
r = options_popup( title='Linearization Options', text=text, initial_text=initial_text, help_text=help_text, $
			help_default=help_default, file=file, filter='*.linear.var', initial_file=initial_file, help_file=help_file, $
			drop=drop, help_drop=help_drop, min_xsize=300, error=error)
if error then return

n_table = 4096
n_fraction = 32L
base = float2(r.text[0])								; minimum for peak detection
lfile = r.file[0] 
no_fit1 = 1
if r.drop[0] eq 1 then no_fit1=0
poly_order = r.drop[1]

print,'Linear file saved as: ',lfile					; .linear.var file
on_ioerror, bad_linear
openw, lun, lfile, /get_lun

printf, lun, '# linearize energy'
printf, lun, '# written by "linearize2_spectrum_plugin", ' + systime()
printf, lun, '# file: ' + (*p[i]).file
printf, lun, '# source: ' + (*p[i]).source
printf, lun, '# comment: ' + (*p[i]).comment
printf, lun, '# '
printf, lun, '# version: 4'								; version 4
printf, lun, '# n_table: ' + string(n_table/16)			; table size, number of channels
;printf, lun, '# n_fraction: ' + string(n_fraction)		; fractional steps
printf, lun, '# '

n_detectors = n_elements(p)
sum = fltarr(n_detectors)
for i=0,n_detectors-1 do begin
	sum[i] = total( *(*p[i]).data)
endfor
qc = where( sum gt 0.3*max(sum), nqc)

for jj=0,nqc-1 do begin	; ============================ loop over detector i =======================
	i = qc[jj]

	ndet = fix((*p[i]).station + adc_offset_device((*p[i]).DevObj))
	ns = (*p[i]).size
	data = float( *(*p[i]).data)						; spectrum data
	ca = (*p[i]).cal.poly[1]							; gain trimmed calibration energy per channel
	cb = (*p[i]).cal.poly[0]							; gain trimmed calibration offset
;	view = (((eview-cb)/ca) > 0) < (ns-1)				; view in channels for this detector
	view = iview < (ns-1)

	mmx = max(data[ns/3:2*(ns/3)])						; maxima in middle
	jtop = ns-1
	for j=ns-1, 2*(ns/3), -1 do begin
		if data[j] gt mmx*1.5 then jtop = j-1			; detect top overflows
	endfor
	view = view < jtop									; avoid top overflows
	print,'Detector = ',i,'  view = ',view

	channel = findgen(ns)
	sn = data * channel
	
	y = lonarr(ns)
	y[view[0]:view[1]] = median(data[view[0]:view[1]] > 0,10) > 0
	ys = smooth(y,200)
	thresh = min(y[view[0]:view[1]]) + base*(max(ys[view[0]:view[1]])-min(y[view[0]:view[1]]))
	y[view[0]:view[1]] = y[view[0]:view[1]] ge thresh
	
	kernel = intarr(10)
	kernel[*] = 1
	y = dilate(y,kernel)
	y = erode(y,kernel)
	y[view[0]] = 0
	y[view[1]] = 0
	x2 = where( (y eq 1) and (shift(y,1) eq 0), nc)
	x3 = where( (y eq 0) and (shift(y,1) eq 1), nc2)
	nc = nc < nc2
	if nc lt 5 then begin
		warning,'Linearize2_spectrum_plugin',['Too few peaks (1) to work with in View range', $
						'or few dominant peaks.','','This may indicate inclusion of large', $
						'overflow peaks at high end of spectrum,', $
						'or "odd channel" peaks at low end.','','Detector channel ='+str_tidy(ndet)+', Chip = '+str_tidy(ndet/32)]
		continue
	endif
	
	centroid = fltarr(nc)
	err = fltarr(nc)
	sump = fltarr(nc)

; Find centroids of all observed peaks

	for j=0,nc-1 do begin
		d = data[x2[j]:x3[j]]
		c = channel[x2[j]:x3[j]]
		m = max(d)
		q = where( d gt m*fraction, nq)
		tsn = total( c[q]*d[q])
		ts2 = total( c[q]*c[q]*d[q])
		ts = total( d[q])
		sump[j] = ts
		centroid[j] = tsn/ts
		err[j] = sqrt( (ts2/ts - centroid[j]*centroid[j] ) / nq)
	endfor

; Trim peaks at ends if they are strange amplitude

	midp = total(sump[(nc/3):2*(nc/3)]) / float(nc/3)
	jlow = 0
	jhigh = nc-1
	for j=0,nc/3 do begin
		if (sump[j] ge 0.8*midp) and (sump[j] le 1.2*midp) then break
		jlow = j+1
	endfor
	for j=nc-1,2*(nc/3),-1 do begin
		if (sump[j] ge 0.8*midp) and (sump[j] le 1.2*midp) then break
		jhigh = j-1
	endfor
	q = indgen(nc)
	q = q[jlow:jhigh]
	nq = n_elements(q)
	
	print,'Trim odd peaks at ends: all = ',nc,', good = ',nq
	sump = sump[q]
	centroid = centroid[q]
	err = err[q]
	nc = nq
	
	if nc lt 5 then begin
		warning,'Linearize2_spectrum_plugin',['Too few peaks (2) to work with in View range', $
						'or few dominant peaks.','','This may indicate inclusion of large', $
						'overflow peaks at high end of spectrum,', $
						'or "odd channel" peaks at low end.','','Detector channel ='+str_tidy(ndet)+', Chip = '+str_tidy(ndet/32)]
		continue
	endif

; Anchor points

	tlow = 2000.
	thigh = 4000.

; Build linear centroid values

	q = sort( abs(centroid - thigh))
	na2 = q[0]
	q = sort( abs(centroid - tlow))
	na1 = q[0]
	centre_spec = na1
	
	xna2 = float(na2) * (thigh-centroid[0])/(centroid[na2]-centroid[0])
	xna1 = float(na1) * (tlow-centroid[0])/(centroid[na1]-centroid[0])
	
	a = (thigh - tlow) / (xna2 - xna1)
	b = tlow - a*xna1
	x = findgen(nc)
	clinear = a*x + b
	c = findgen(n_table*n_fraction)/float(n_fraction)
	
	!p.title = 'Non-linear centroid correction'
	!x.title = 'Channel'
	!y.title = 'Centroid Error (channels)'
	!p.charsize = 1.0
	window, jj
	plot,clinear,centroid-clinear, xrange=[0,4100], yrange=[-40,40], ystyle=1, /nodata
	oplot,clinear,centroid-clinear,psym=1,color=spec_colour('green')
	xyouts, !x.window[0]+0.05, !y.window[0]+0.05, 'Correction function for: '+(*p[i]).label, /normal, charsize=1.0

; Function #1 -------------------------

	yfit1 = fltarr(nc)
	if no_fit1 then goto, fit2
	A1 = [20000.,0.002]
	mask = [1,1]
	t = non_linearity(clinear,A1)
	
	yfit1 = lmfit( clinear, centroid, A1, chisq=chi, convergence=converged, $
					fita=mask, function_name='non_linearity', sigma=sigma, $
					measure_errors=err, itmax=100, iter=iter, /double)
	if converged ne 1 then begin
		warning,'Linearize2_spectrum_plugin',['Bad fit1, iter = '+string(iter),'continue with polynomial fit']
		f1 = 0.0
		A1[*] = 0.0
		yfit1[*] = 0.0
		no_fit1 = 1
		goto, fit2
	endif
	
	for j=0,nc-1 do begin
		print,j,' centroid =',centroid[j],' err =',err[j],' clinear =',clinear[j],' yfit =',yfit1[j], ' clinear-yfit =',yfit1[j]-clinear[j]
	endfor
	print,' coeff =',reform(A1)
	print,' sigma =',sigma
	
	t = non_linearity(c,A1)
	f1 = t[*,0]
	
	oplot,c,f1-c, color=spec_colour('violet')

; Function #2 ---fit residual----------------

fit2:
	case poly_order of
		0: begin			; 5th
			A2 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
			mask = [1,1,1,1,1,1]
			end
		1: begin			; 7th
			A2 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
			mask = [1,1,1,1,1,1,1,1]
			end
		2: begin			; 9th
			A2 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
			mask = [1,1,1,1,1,1,1,1,1,1]
			end
		3: begin			; 11th
			A2 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
			mask = [1,1,1,1,1,1,1,1,1,1,1,1]
			end
		4: begin			; 13th
			A2 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
			mask = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]
			end
		else: return
	endcase
	A2 = 1.d+0 * A2

	t = non_linearity3(centroid,A2)

	yfit2 = lmfit( clinear, centroid-yfit1, A2, chisq=chi, convergence=converged, $
				fita=mask, function_name='non_linearity3', sigma=sigma, $
				measure_errors=err, itmax=100, iter=iter, /double)
	if converged ne 1 then begin
		warning,'Linearize2_spectrum_plugin','Bad fit2, iter = '+string(iter)
		goto, done
	endif
	
	for j=0,nc-1 do begin
		print,j,' centroid =',centroid[j],' err =',err[j],' clinear =',clinear[j],' yfit =',yfit2[j], ' clinear-yfit1-yfit2 =',clinear[j]-yfit1[j]-yfit2[j]
	endfor
	print,' coeff =',reform(A2)
	print,' sigma =',sigma
	
	t = non_linearity3(c,A2)
	f2 = t[*,0]
	
	if no_fit1 then goto, cont2
	!p.title = 'Non-linear centroid correction2'
	!x.title = 'Channel'
	!y.title = 'Centroid Error2 (channels)'
	!p.charsize = 1.0
	window, jj+nq
	plot,c,f2, xrange=[0,4100], yrange=[-40,40], ystyle=1
	oplot,clinear,centroid-yfit1,psym=1,color=spec_colour('green')
	xyouts, !x.window[0]+0.05, !y.window[0]+0.05, 'Correction function (2nd term) for: '+(*p[i]).label, /normal, charsize=1.0

cont2:
	if no_fit1 then begin
		A = [A2]
		f = f2
	endif else begin
		A = [A1,A2]
		f = f1 + f2
	endelse

	fy = findgen(n_table*n_fraction)/float(n_fraction)

	f3 = fy - (f-fy)					; invert lookup table
;	f3 = f								; no inversion

	wset, jj
	oplot,c,f-c, color=spec_colour('red')

;---------------------------------------

	printf, lun, '# n_det: ' + string(ndet)							; ID of detector channel
	printf, lun, '# n_pars: ' + string(n_elements(A))				; number of pars
	printf, lun, '# fit_pars: ' + strjoin(string(float(A)),' ')		; fitting pars (both sets)
	printf, lun, '#'
	
	ft = reform(f3, n_fraction, n_table)
	ct = reform(c, n_fraction, n_table)

	chip = ndet/32

	printf, lun, 'linearise2.chip['+str_tidy(chip)+'].seg[].etrim ' + strjoin(string(reform(clip(ft[0,0:*:16]-ct[0,0:*:16],-32,31.96874))),' ')
	printf, lun, '#'

endfor	; =========================== loop over detector i =================================================

;printf, lun, 'linearise.enable 0'
;printf, lun, 'linearise2.enable 1'
printf, lun, '# end'

;print,' Found linearization, applying to spectrum ...'
print,' Found linearization, not applied to spectrum now.'

; write back the modified spectrum data
; assumes that spectrum length has not changed

; *(*p[i]).data = map_spec( (*p[i]).data, table=f)

;...............................................................................................

done:
	print,' All done, return spectra.'
	history = 'Build Linearize2 plugin   [linearize2_Spectrum_Plugin]'		; a history record for this plugin

finish:
	close_file, lun
	return

bad_view:
	warning,'Linearize2_spectrum_plugin','Need to select a range with View first.'
	goto, finish
bad_linear:
	warning,'Linearize2_spectrum_plugin','Bad open/write of linearize file '+lfile
	goto, finish
bad_peaks:
	warning,'Linearize2_spectrum_plugin',['Too few peaks to work with in View range', $
					'or few dominant peaks.','','This may indicate inclusion of large', $
					'overflow peaks at high end of spectrum,', $
					'or "odd channel" peaks at low end.']
	goto, finish
end

