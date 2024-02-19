;
;  Linearize Spectrum cuts plugin routine
;  -----------------------------
;
;  All Spectrum template routines MUST be named with "_spectrum__plugin.pro"
;  at the end of the file name. For a new "Fred" plugin, copy and rename this file
;  to "Fred_spectrum_plugin.pro" and edit the first line to:
;  "pro fred_spectrum_plugin, p, i, title=title, history=history"
;
;  Plugins should be compiled in IDLDE and saved as a SAV file.
;  Only compile routines for ONE plugin and save using the command:
;  "SAVE, /routines, filename='fred_spectrum_plugin.sav'" for a "fred_spectrum_plugin" plugin.
;  To ensure this, exit IDLDE and start it again to compile and save another plugin.
;
;  NOTE: It is important to ensure that ONLY routines for ONE plugin is in each SAV file.
;  Otherwise, unexpected results may result when the SAV files are restored at run-time.
;  To ensure this, use the IDLDE Run->Reset menu to clear out all compiled routines,
;  then compile your plugin, and save it.
;
;  The plugin SAV files will then be loaded automatically when GeoPIXE.sav runs,
;  if the plugin SAV files are located in the same directory as GeoPIXE.sav.
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

pro linearize_cuts_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Build Linearize with Cuts spaced linearly Plugin'				; return the menu title for this plugin
	return
endif

;...............................................................................................

; Linearize a spectrum based on centroids in Cut regions set on
; evenly spaced pulser peaks. The result is to develop a correction
; mapping to linearize the spectrum and output this table with
; fitting parameters, etc.

pspec = p[i]

cuts_file = dialog_pickfile (/read, filter = '*.cuts', $
          /noconfirm, /must_exist, path=extract_path((*pspec).file), $
          title='Select peak definition CUTs file', /fix_filter)

fraction = 0.01				; fraction threshold for peak
resolve_routine, 'non_linearity', /is_function
resolve_routine, 'lmfit', /is_function

pcuts = read_cuts( cuts_file, error=error)
if error then goto, bad_cuts

ns = (*pspec).size
data = float( *(*pspec).data)
channel = findgen(ns)
sn = data * channel
nc = n_elements( *pcuts)
centroid = fltarr(nc)
err = fltarr(nc)

na1 = nc/2
na2 = nc-1

; Find centroids of all Cuts regions

for i=0,nc-1 do begin
	x2 = (*pcuts)[i].x[2]
	x3 = (*pcuts)[i].x[3]
	d = data[x2:x3]
	c = channel[x2:x3]
	m = max(d)
	q = where( d gt m*fraction, nq)
	tsn = total( c[q]*d[q])
	ts2 = total( c[q]*c[q]*d[q])
	ts = total( d[q])
	centroid[i] = tsn/ts
	err[i] = sqrt( (ts2/ts - centroid[i]*centroid[i] ) / nq)
endfor

; Build linear centroid values

nd = na2 - na1
a = (centroid[na2] - centroid[na1]) / float(nd)
b = centroid[na1] - a*float(na1)
x = findgen(nc)
clinear = a*x + b

A = [20000.,0.002]
mask = [1,1]
t = non_linearity(centroid,A)
for i=0,nc-1 do begin
	x2 = (*pcuts)[i].x[2]
	x3 = (*pcuts)[i].x[3]
	print, i, x2,x3, centroid[i], err[i], clinear[i], t[i,0]
endfor

yfit = lmfit( centroid, clinear, A, chisq=chi, convergence=converged, $
				fita=mask, function_name='non_linearity', sigma=sigma, $
				measure_errors=err, itmax=100, iter=iter, /double)
if converged ne 1 then goto, bad_fit

for i=0,nc-1 do begin
	print,i,' centroid =',centroid[i],' clinear =',clinear[i],' yfit =',yfit[i], ' clinear-yfit =',clinear[i]-yfit[i]
endfor
print,' coeff =',reform(A)
print,' sigma =',sigma

c = findgen(4096)
t = non_linearity(c,A)
f = t[*,0]

lfile = strip_file_ext((*pspec).file) + '.linear'
on_ioerror, bad_linear
openw, 1, lfile
writeu, 1, 4096, 1					; size, function number 1
writeu, 1, float(A)
writeu, 1, float(f)
close, 1

!p.title = 'Non-linear centroid correction'
!x.title = 'Channel'
!y.title = 'Centroid Error (channels)'
window,0
plot,c,f-c, yrange=[-30,10], ystyle=1
oplot,centroid,clinear-centroid,psym=1,color=spec_colour('green')

fl = findgen(4096)
f = fl - (float(f)-fl)				; inverse lookup table

print,' Found linearization, applying to spectrum ...'

; write back the modified spectrum data
; assumes that spectrum length has not changed

*(*pspec).data = map_spec( (*pspec).data, table=f)

history = 'Build Linearize plugin   [linearize_Spectrum_Plugin]'		; a history record for this plugin

;...............................................................................................

return

bad_linear:
	warning,'Linearize_spectrum_cuts_plugin','Bad open/write of linearize file '+lfile
	return
bad_cuts:
	warning,'Linearize_spectrum_cuts_plugin','Bad cuts read'
	return
bad_fit:
	warning,'Linearize_spectrum_cuts_plugin','Bad fit, status = '+string(status)
	return
end

