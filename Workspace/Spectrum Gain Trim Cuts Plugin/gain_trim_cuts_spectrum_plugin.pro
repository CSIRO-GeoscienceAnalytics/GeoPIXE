;
;  Gain trim Cuts Spectrum plugin routine
;  
;  Use any or all of X0-X1, X2-X3, X4-X5 marker ranges to define peaks in
;  spectra to use for gain trimming, i.e. matching peak centroids in these ranges
;  with the first spectrum.
;  
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

pro Gain_Trim_cuts_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Build Gain Trim Xcuts Plugin'		; return the menu title for this plugin
	return											; * indicates that it accepts all displayed spectra
endif

;...............................................................................................

; Build gain trim cal coefficients for all spectra based on centroids of two 
; evenly spaced pulser peaks. The result is to develop a correction
; mapping to match the "cal" of all spectra, and output a table of coefficients.

fraction = 0.3										; fraction threshold for each peak
threshold = 0.002									; threshold for initial peak search
low_gain = 0.5
high_gain = 2.0

close, 1
close, 2
fil = strip_file_ext((*p[0]).file)
if strlen(fil) lt 1 then fil = 'temp'
lfile = fil + '-trim-energy.txt'
on_ioerror, bad_linear
openw, 1, lfile
printf, 1, '#trim-energy'

lfile = fil + '-trim-time.txt'
on_ioerror, bad_linear
openw, 2, lfile
printf, 2, '#trim-time'

view = round([min (marks[0:5,1]), max(marks[0:5,1])]) 		; View: X markers
use = bytarr(view[1]+1)
use[marks[0,1]:marks[1,1]] = 1
use[marks[2,1]:marks[3,1]] = 1
use[marks[4,1]:marks[5,1]] = 1
use[0] = 0

np = n_elements(p)
poor = intarr(np)
first_e = 1
first_t = 1
for ip=0,np-1 do begin
    if (*p[ip]).show and ((*p[ip]).size gt 0) then begin
		pspec = p[ip]
		nl = lenchr((*pspec).label)
		label = extract( (*pspec).label, nl-2, nl-1)
		do_energy = (label eq '/E')
		do_time = (label eq '/T')
		if do_energy or do_time then begin
			if strlowcase((*p[ip]).cal.units) ne 'channel' then goto, clear_cal
			lun = (do_energy ? 1 : 2)
			adc = (*pspec).station + adc_offset_device((*pspec).DevObj)
			ns = (*pspec).size
			data = float( *(*pspec).data)
			channel = findgen(ns)
			sn = data * channel
			
			view = ( view	< (ns-1)) > 0						; View: V0-V1     markers
			if view[1] le view[0] then goto, bad_view
		
			y = lonarr(ns)
			y[view[0]:view[1]] = median(data[view[0]:view[1]] > 0,3) > 0
			thresh = (min(y[view[0]:view[1]]) + threshold*(max(y[view[0]:view[1]])-min(y[view[0]:view[1]]))) > 3.
			y[view[0]:view[1]] = y[view[0]:view[1]] ge thresh

			q = where( use eq 0, nq)							; veto not in X0-X1, X2-X3, or X4-X5 ranges
			if nq ne 0 then y[q]=0
			
;			kernel = intarr(20)
;			kernel[*] = 1
;			y = dilate(y,kernel)								; no good for close T peaks
			y[view[0]] = 0
			y[view[1]] = 0
			x2 = where( (y eq 1) and (shift(y,1) eq 0), nc)
			x3 = where( (y eq 0) and (shift(y,1) eq 1), nc2)
			nc = nc < nc2
			if nc lt 2 then begin
				print,'Too few peaks for ADC =',adc				;goto, bad_peaks
				goto, cont
			endif
			
			centroid = fltarr(nc)
			err = fltarr(nc)
			top = fltarr(nc)
			
			; Find centroids of all peaks
			
			for i=0,nc-1 do begin
				d = data[x2[i]:x3[i]]
				c = channel[x2[i]:x3[i]]
				m = max(d)
				q = where( d gt m*fraction, nq)
				if nq gt 0 then begin
					tsn = total( c[q]*d[q])
					ts2 = total( c[q]*c[q]*d[q])
					ts = total( d[q])
					centroid[i] = tsn/ts
					top[i] = data[centroid[i]]
					err[i] = sqrt( (ts2/ts - centroid[i]*centroid[i] ) / nq)
				endif
			endfor
			mxc = max(centroid)
			mnc = min(centroid)
			if nc gt 2 then begin
				q = sort(centroid)								; sort peaks in centroid order
				centroid = centroid[q]
				err = err[q]
				top = top[q]
				q = where( abs(shift(centroid,-1) - centroid) lt 0.2*(mxc-mnc), nq)
				if nq gt 0 then begin
					q2 = where( top[q] ge top[q+1], nq2)		; remove the smaller of close
					if nq2 gt 0 then top[q[q2]+1]=0.0			; peaks
					q2 = where( top[q] lt top[q+1], nq2)
					if nq2 gt 0 then top[q[q2]]=0.0
				endif
				
				q = reverse(sort(top))							; accept only 2 most intense peaks
				centroid = centroid[q[0:1]]
				err = err[q[0:1]]
				top = top[q[0:1]]
				nc = 2
				q = sort(centroid)								; sort 2 peaks back in centroid order
				centroid = centroid[q]
				err = err[q]
				top = top[q]
			endif
			
			if first_e and do_energy then begin
				clowe = centroid[0]
				chighe = centroid[1]
			endif
			if first_t and do_time then begin
				clowt = centroid[0]
				chight = centroid[1]
			endif
			if (first_e and do_energy) or (first_t and do_time) then begin
				if do_energy then first_e = 0
				if do_time then first_t = 0
				(*pspec).cal.poly[0] = 0.0
				(*pspec).cal.poly[1] = 1.0
				(*pspec).cal.order = 1
				(*pspec).cal.units = 'channel'
;				printf, lun, strtrim(string(adc),2) + ' ' + strtrim(string((*pspec).cal.poly[0]),2) + ' ' + strtrim(string((*pspec).cal.poly[1]),2)
			endif else begin
				if do_energy or do_time then begin
					if do_energy then begin
						clow = clowe
						chigh = chighe
					endif
					if do_time then begin
						clow = clowt
						chigh = chight
					endif
					a = (chigh - clow)/(centroid[1] - centroid[0])
					b = clow - a*centroid[0]			
					(*pspec).cal.poly[0] = b
					(*pspec).cal.poly[1] = a
					(*pspec).cal.order = 1
					(*pspec).cal.units = 'channel'
					poor[ip] = (a gt high_gain) or (a lt low_gain)
;					if poor[ip] eq 0 then printf, lun, strtrim(string(adc),2) + ' ' + strtrim(string((*pspec).cal.poly[0]),2) + ' ' + strtrim(string((*pspec).cal.poly[1]),2)
				endif
			endelse
		endif
	endif
	cont:
endfor

;	Now determine average of cal a,b and scale/offset all cal.poly so that the average is 0.0,1.0 ...

av_a = 0.0
av_b = 0.0
np = n_elements(p)
ns = 0L
for ip=0,np-1 do begin
    if (*p[ip]).show and ((*p[ip]).size gt 0) then begin
		pspec = p[ip]
		av_b = av_b + (*pspec).cal.poly[0]
		av_a = av_a + (*pspec).cal.poly[1]
		ns = ns+1
	endif
endfor
av_a = av_a/float(ns)
av_b = av_b/float(ns)

for ip=0,np-1 do begin
    if (*p[ip]).show and ((*p[ip]).size gt 0) then begin
		pspec = p[ip]
		adc = (*pspec).station + adc_offset_device((*pspec).DevObj)
		(*pspec).cal.poly[1] = (*pspec).cal.poly[1] / av_a
		(*pspec).cal.poly[0] = ((*pspec).cal.poly[0] - av_b) / av_a
		if poor[ip] eq 0 then printf, lun, strtrim(string(adc),2) + ' ' + strtrim(string((*pspec).cal.poly[0]),2) + ' ' + strtrim(string((*pspec).cal.poly[1]),2)
	endif
endfor

history = 'Build Gain Trim plugin   [Gain_Trim_cuts_spectrum_plugin]'		; a history record for this plugin

q = where( poor eq 1, nq)
if nq gt 0 then begin
	s = 'Poor gains estimated for the following detectors:'
	for i=0,nq-1 do begin
		s = [s,'ADC = ' + strtrim(string( (*p[q[i]]).station + adc_offset_device((*p[q[i]]).DevObj)),2) + ' gain = ' + strtrim(string( (*p[q[i]]).cal.poly[1] ),2)]
	endfor
	warning,'Gain_Trim_cuts_spectrum_plugin',s
endif

fin:
	close, 1
	close, 2
	return

bad_view:
	warning,'Gain_Trim_cuts_spectrum_plugin','Need to select a range with View first.'
	goto, fin
bad_linear:
	warning,'Gain_Trim_cuts_spectrum_plugin','Bad open/write of gain trimming file '+lfile
	goto, fin
clear_cal:
	warning,'Gain_Trim_cuts_spectrum_plugin','Clear Cals first using "Display/Clear ALL Energy Cals" menu'
	goto, fin
bad_peaks:
	warning,'Gain_Trim_cuts_spectrum_plugin',['Too few peaks to work with in View range', $
					'or few dominant peaks for detector =' + strtrim(string(adc),2)]
	goto, fin
end

