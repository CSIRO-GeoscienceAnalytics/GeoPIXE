;
;  Peak Cal by centroids Spectrum plugin routine
;	Use 2 cuts to define 2 peaks and give the energies for these to Cal spectra.
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

pro peak_cal_cut_centroid_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Cal by X marker range centroids (2) Plugin'		; return the menu title for this plugin
	return									; * indicates that it accepts all displayed spectra
endif
np = n_elements(p)
show = intarr(np)
for i=0,np-1 do show[i] = (*p[i]).show
q = where( show eq 1, nq)
if nq eq 0 then return

; Later versions of GeoPIXE will set 'ispec_current' to the main 'current_plot()'
; which changes between colour and highlight spectrum modes.

common c_spectrum_current, ispec_current
common c_cal_elements, tE1, tE2
if n_elements(ispec_current) ne 0 then begin
	icurrent = clip( ispec_current, 0, np-1)
endif else begin
	icurrent = 0
endelse
if n_elements(tE1) eq 0 then tE1 = '6.4'
if n_elements(tE2) eq 0 then tE2 = '17.44'

id = intarr(nq)
sdev = fltarr(nq)

marks = marks[0:5,1]                 	    ; View: V0-V1 [3] markers; Cuts: C0-C1 [4]; X: X0-X5 [1]
marks = marks[sort(marks)]
qm = where( marks gt 0, nqm)
if nqm lt 4 then begin
	warning,'peak_cal_cut_centroid_spectrum_plugin',['Zero X markers.','Select two peaks using 4 X markers and try again.']
	return
endif
if nqm gt 4 then begin
	qm = qm[nqm-4:nqm-1]
endif

mark1 = marks[qm[0:1]]						; 2 CUTs for 2 peaks
mark2 = marks[qm[2:3]]

text = ['First X cut energy (keV)','Second X cut energy (keV)']
initial_text = str_tidy(string([tE1,tE2]))
help_text = ['Energy (keV) for first X range cut (using smallest 2 non-zero X markers).','Energy (keV) for second X range cut (using next 2 non-zero X markers).']
Help_default = 'Select 2 channel ranges defined by X markers (e.g. using 0,1, 2,3 or 2,3, 4,5 X markers) and then provide the energies for the centroids of the spectrum within each X range. ' + $
		'Remember to set first X markers starting from the right, and then proceed left.'
r = options_popup( title='Cal by Centroids between X markers Options', text=text, initial_text=initial_text, help_text=help_text, $
			help_default=help_default, min_xsize=300, error=error)
if error then return

tE1 = r.text[0]
tE2 = r.text[1]
e1 = float2( r.text[0])						; energy CUT 1
e2 = float2( r.text[1])						; energy CUT 2

cunits = (*p[icurrent]).cal.units          	; energy calibration units string
if cunits eq 'keV' then begin
	ca = (*p[icurrent]).cal.poly[1]         ; energy calibration energy per channel
	cb = (*p[icurrent]).cal.poly[0]         ; energy calibration offset
	elow1 = ca*mark1[0]+cb
	ehigh1 = ca*mark1[1]+cb
	elow2 = ca*mark2[0]+cb
	ehigh2 = ca*mark2[1]+cb
endif else begin
	elow1 = mark1[0]
	ehigh1 = mark1[1]
	elow2 = mark2[0]
	ehigh2 = mark2[1]
endelse

for j=0,nq-1 do begin
	i = q[j]								; spectrum number with show=1
	pspec = (*p[i]).data                    ; pointer to the spectrum data array
	siz = (*p[i]).size                      ; Size of spectrum in channels
	charge = (*p[i]).charge                 ; integrated charge for spectrum (uC)

	if cunits eq 'keV' then begin
		ca = (*p[i]).cal.poly[1]			; energy calibration energy per channel
		cb = (*p[i]).cal.poly[0]			; energy calibration offset

		xl1 = (elow1 - cb) / ca
		xh1 = (ehigh1 - cb) / ca	
		x1 = xl1 + indgen(xh1-xl1+1)		; channel

		xl2 = (elow2 - cb) / ca
		xh2 = (ehigh2 - cb) / ca	
		x2 = xl2 + indgen(xh2-xl2+1)		; channel
	endif else begin
		xl1 = elow1
		xh1 = ehigh1	
		x1 = xl1 + indgen(xh1-xl1+1)		; channel

		xl2 = elow2
		xh2 = ehigh2	
		x2 = xl2 + indgen(xh2-xl2+1)		; channel
	endelse
	y1 = (*pspec)[x1]						; counts CUT 1
	y2 = (*pspec)[x2]						; counts CUT 2
	
	if (total(y1) gt 0) and (total(y2) gt 0) then begin
		cent1 = total( y1*x1) / total(y1)			; centroid 1
		cent2 = total( y2*x2) / total(y2)			; centroid 2
	
		a = (e2 - e1) / (cent2 - cent1)
		b = e1 - a*cent1

		(*p[i]).cal.poly[0] = b
		(*p[i]).cal.poly[1] = a
		(*p[i]).cal.units = 'keV'

		id = (*p[i]).station + adc_offset_device( (*p[i]).DevObj)
		print,j,', channel =',id,'  Cal A, B = ', a, b
	endif else begin
		id = (*p[i]).station + adc_offset_device( (*p[i]).DevObj)
		print,j,', channel =',id,'  Zero counts, Cal A, B not determined.'
	endelse

endfor

fin:
	return
end

