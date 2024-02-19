;
;  Linearize Spectrum using Cut Energies plugin routine
;  -----------------------------
;
;  Linearize a spectrum based on centroids of peaks detected in CUTs. The CUT name supplies the
;  energy of the peak.
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

pro linearize_cuts_fit_offset_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR
common c_non_linearity3, centre_spec

if arg_present(title) then begin
	title = 'Build Linearize using peak offsets in Fit Plugin'	; return the menu title for this plugin
	return														; "*" indicates multi-spectrum processing
endif

fraction = 0.5													; 50% fraction threshold for peak
resolve_routine, 'non_linearity', /is_function					; centroid data down to 50% peak max
resolve_routine, 'non_linearity3', /is_function
resolve_routine, 'lmfit', /is_function

cview = round(marks[0:1,3])										; View: V0-V1 markers (channel #)
eview = cview * (*p[i]).cal.poly[1] + (*p[i]).cal.poly[0]		; in "energy" (gain trimmed) units
if eview[1] le eview[0] then goto, bad_view

cuts_file = dialog_pickfile (/read, filter = '*.cuts', $
          /noconfirm, /must_exist, path=extract_path((*p[i]).file), $
          title='Select peak definition CUTs file', /fix_filter)

pcuts = read_cuts( cuts_file, error=error)
if error then goto, bad_cuts

nc = n_elements( *pcuts)
x0 = (*pcuts).x[0]
x1 = (*pcuts).x[1]
x2 = (*pcuts).x[2]
x3 = (*pcuts).x[3]
x4 = (*pcuts).x[4]
x5 = (*pcuts).x[5]
dleft = (*pcuts).dleft
dright = (*pcuts).dright
ctype = (*pcuts).type
		
if ptr_good((*p[i]).fit) eq 0 then goto, bad_fit
if ptr_good((*(*p[i]).fit[1]).data) eq 0 then goto, bad_fit
	
;...............................................................................................

n_table = 4096
n_fraction = 32L
lfile = strip_file_ext((*p[0]).file) + '.linear'		; .linear file
on_ioerror, bad_linear
openw, lun, lfile, /get_lun
writeu, lun, n_table, 3									; size, version number 3

ns = (*p[i]).size
data = float( *(*p[i]).data)							; spectrum data
fit = float( *(*(*p[i]).fit[1]).data)					; fit data
data[ns-10:ns-1] = 0.0									; zero top overflows
ca = (*p[i]).cal.poly[1]								; gain trimmed calibration energy per channel
cb = (*p[i]).cal.poly[0]								; gain trimmed calibration offset
view = (((eview-cb)/ca) > 0) < (ns-1)					; view in channels for this detector

channel = findgen(ns)
sn = data * channel
centroid = fltarr(nc)
energy = fltarr(nc)
err = fltarr(nc)

; Find centroids of all observed peaks

for j=0,nc-1 do begin
	if ctype[j] eq 1 then begin
		c01 = (x0[j]+x1[j])/2.							; cut marker band centres
		c23 = (x2[j]+x3[j])/2.
		c45 = (x4[j]+x5[j])/2.
		d01 = x1[j]-x0[j]								; cut marker band spans
		d23 = x3[j]-x2[j]
		d45 = x5[j]-x4[j]
		z01 = total( data[x0[j]:x1[j]] ) / d01			; left, right heights
		z45 = total( data[x4[j]:x5[j]] ) / d45
		bka = (z45 - z01) / (c45 - c01)					; back 'line' parameters
		bkb = z01 - bka*c01
		dback = bkb + bka*(x2[j]+indgen(x3[j]-x2[j]+1))
	endif else begin
		dback = min(data[x2[j]:x3[j]])
	endelse
	d = (data[x2[j]:x3[j]] - dback) > 0
	c = channel[x2[j]:x3[j]]
	m = max(d)
	q = where( d gt m*fraction, nq)
	tsn = total( c[q]*d[q])
	ts2 = total( c[q]*c[q]*d[q])
	ts = total( d[q])
	centroid[j] = tsn/ts
	err[j] = sqrt( (ts2/ts - centroid[j]*centroid[j] ) / nq)

	d = (fit[x2[j]:x3[j]] - dback) > 0
	c = channel[x2[j]:x3[j]]
	m = max(d)
	q = where( d gt m*fraction, nq)
	tsn = total( c[q]*d[q])
	ts2 = total( c[q]*c[q]*d[q])
	ts = total( d[q])
	energy[j] = tsn/ts
endfor

na1 = nc/2
na2 = nc-1
centre_spec = 0.5*(energy[0] + energy[na2])

; Build linear centroid values
; Now use relative energies from table

ecentroid = ca*centroid + cb
energy = ca*energy + cb
eerr = ca*err

;a = (centroid[na2] - centroid[na1]) / (energy[na2] - energy[na1])
;b = centroid[na1] - a*energy[na1]
;x = energy
;clinear = a*x + b
c = findgen(n_table*n_fraction)/float(n_fraction)
ce = ca*c + cb

!p.title = 'Non-linear centroid correction'
!x.title = 'Energy'
!y.title = 'Centroid Error (channels)'
!p.charsize = 1.0
window,0
plot,[0.0],[0.0], xrange=eview, yrange=[-20,20], xstyle=1, ystyle=1, /nodata
oplot,energy,(ecentroid-energy)/ca, psym=1,color=spec_colour('green')
xyouts, !x.window[0]+0.05, !y.window[0]+0.1, 'Correction function for: ', /normal, charsize=1.0
xyouts, !x.window[0]+0.05, !y.window[0]+0.05, (*p[i]).label, /normal, charsize=1.0

; Function #1 -------------------------

A1 = [0.0, 0.0]
yfit1 = energy
f1 = ce
goto, fit2

A1 = [20000.,0.002]
mask = [1,1]
t = non_linearity(ce,A1)

yfit1 = lmfit( energy, ecentroid, A1, chisq=chi, convergence=converged, $
				fita=mask, function_name='non_linearity', sigma=sigma, $
				measure_errors=eerr, itmax=100, iter=iter, /double)
if converged ne 1 then begin
	warning,'linearize_fixed_cuts_spectrum_plugin',['Bad fit1, iter = '+string(iter),'continue with polynomial fit']
	f1 = ce
	A1[*] = 0.0
	yfit1 = energy
	goto, fit2
endif

for j=0,nc-1 do begin
	print,j,' centroid =',ecentroid[j],' err =',eerr[j],' energy =',energy[j],' yfit1 =',yfit1[j], ' yfit1-energy =',yfit1[j]-energy[j]
endfor
print,' coeff =',reform(A1)
print,' sigma =',sigma

t = non_linearity(ce,A1)
f1 = t[*,0]

oplot,ce,(f1-ce)/ca, color=spec_colour('violet')

; Function #2 ---fit residual----------------

fit2:
	norder = (nc-1) < 6
	A2 = replicate(0.0d+0, norder)
	mask = replicate(1, norder)

	t = non_linearity3(ecentroid,A2)

	yfit2 = lmfit( energy, ecentroid-yfit1, A2, chisq=chi, convergence=converged, $
				fita=mask, function_name='non_linearity3', sigma=sigma, $
				measure_errors=eerr, itmax=1000, iter=iter, /double)
if converged ne 1 then begin
	warning,'linearize_fixed_cuts_spectrum_plugin','Bad fit2, iter = '+string(iter)
	goto, done
endif

for j=0,nc-1 do begin
	print,j,' centroid =',ecentroid[j],' err =',eerr[j],' energy =',energy[j],' yfit2 =',yfit2[j]
endfor
print,' coeff =',reform(A2)
print,' sigma =',sigma

t = non_linearity3(ce,A2)
f2 = t[*,0]
oplot,ce,f2/ca,color=spec_colour('green')

q = where(ce lt eview[0],nq)
if nq gt 0 then f2[q] = f2[q[nq-1]]
q = where(ce gt eview[1],nq)
if nq gt 0 then f2[q] = f2[q[0]]

;!p.title = 'Non-linear centroid correction2'
;!x.title = 'Channel'
;!y.title = 'Centroid Error2 (channels)'
;!p.charsize = 1.0
;window,1
;plot,ce,f2, xrange=[0,4000], yrange=[-20,40], ystyle=1
;oplot,energy,ecentroid-yfit1,psym=1,color=spec_colour('green')
;xyouts, !x.window[0]+0.05, !y.window[0]+0.05, 'Correction function (2nd term) for: '+(*p[i]).label, /normal, charsize=1.0

A = [A1,A2]
f = f1 + f2

f3 = ce - (f-ce)					; invert lookup table
f3 = (f3-cb)/ca

f = (f-cb)/ca						; transform 'f' to channels for map-spec remapping later

;wset, 0
;oplot,ce,f-ce, color=spec_colour('red')

;---------------------------------------

ndet = fix((*p[i]).station + adc_offset_device((*p[i]).DevObj))
writeu, lun, ndet								; ID of detector channel
writeu, lun, n_elements(A)						; number of pars
writeu, lun, float(A)							; fitting pars (both sets)
writeu, lun, n_fraction							; fractional steps
writeu, lun, float(f3)							; table (w/ fractionsl steps)

;print,' Found linearization, applying to spectrum ...'
print,' Found linearization, not applied to spectrum now. Saved to file: '+lfile

; write back the modified spectrum data
; assumes that spectrum length has not changed

; *(*p[i]).data = map_spec( (*p[i]).data, table=f[0:*:n_fraction])

;...............................................................................................

done:
;	print,' All done, return spectra.'
	history = 'Linear file saved   [linearize_cuts_fit_offset_spectrum_plugin]'			; a history record for this plugin

finish:
	close_file, lun
	return

bad_view:
	warning,'linearize_cuts_fit_offset_spectrum_plugin','Need to select a range with View first.'
	goto, finish
bad_cuts:
	warning,'linearize_cuts_fit_offset_spectrum_plugin','Bad cuts read'
	goto, finish
bad_cut_number:
	warning,'linearize_cuts_fit_offset_spectrum_plugin','Wrong number of Cuts'
	goto, finish
bad_cut_energies:
	warning,'linearize_cuts_fit_offset_spectrum_plugin',['CUT labels must contain line energy.','Express CUT labels as either:', $
				'(i) a "label" followed by "energy" after a <space>, or', '(ii) just an "energy".']
	goto, finish
bad_fit:
	warning,'linearize_cuts_fit_offset_spectrum_plugin','No fit to spectrum found'
	goto, finish
bad_linear:
	warning,'linearize_cuts_fit_offset_spectrum_plugin','Bad open/write of linearize file '+lfile
	goto, finish
bad_peaks:
	warning,'linearize_cuts_fit_offset_spectrum_plugin',['Too few peaks to work with in View range', $
					'or few dominant peaks.','','This may indicate inclusion of large', $
					'overflow peaks at high end of spectrum,', $
					'or "odd channel" peaks at low end.']
	goto, finish
end

