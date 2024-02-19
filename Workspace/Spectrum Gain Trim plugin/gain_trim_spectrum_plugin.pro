;-----------------------------------------------------------------
;
;  Gain trim Spectrum plugin routine
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

pro Gain_Trim_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Build Gain Trim Plugin'				; return the menu title for this plugin
	return											; * indicates that it accepts all displayed spectra
endif

;...............................................................................................

; Build gain trim cal coefficients for all spectra based on centroids of two 
; evenly spaced pulser peaks. The result is to develop a correction
; mapping to match the "cal" of all spectra, and output a table of coefficients.

;drop = ['New Gain-trim var file output','Old Gain-trim file output']
;help_drop = '' 
file = ['Output file','Cuts file']
indx = spectrum_index( p, mode=modei)
mode = ['energy','X','Y','time']
initial_file = [strip_file_ext((*p[0]).file) + '.gaintrim.'+mode[modei[0]]+'.var','']
help_file = ['Select filename for E or T gaintrim. Any E gaintrim will get a name with .gaintrim.energy.var extension, ' + $
			'and any T gaintrim will get a similar name with .gaintrim.time.var extension.','Optional CUTs file to veto selected channel range(s).']
text = ['Peak fraction','Threshold','Pre-smooth']
if mode[modei[0]] eq 'time' then begin
	initial_text = str_tidy(string([0.1,0.1,3]))
endif else begin
	initial_text = str_tidy(string([0.3,0.1,3]))
endelse
help_text = ['Threshold cut-off for initial peak search, relative to largest peak. Use all peaks above this relative intensity.', $
			'Fraction threshold for each peak, relative to top of the peak. Take centroid of all channels in each peak down to this threshold.', $
			'Apply a median filter to each spectrum with this width as a pre-smooth.']
Help_default = 'Gain-trim to the 2 largest peaks in the VIEW range (excluding optional CUT ranges). Remember to set the VIEW and Clear Cals first.'
r = options_popup( title='Gain-trim Options', text=text, initial_text=initial_text, help_text=help_text, $
			help_default=help_default, file=file, filter=['*.gaintrim.*.var','*.cuts'], initial_file=initial_file, help_file=help_file, $
			min_xsize=300, error=error)
if error then return

threshold = float2(r.text[0])		; threshold for initial peak search, relative to largest peak
fraction = float2(r.text[1])		; fraction threshold for each peak, relative to top of peak
smoothw = float2(r.text[2])
low_gain = 0.5
high_gain = 2.0
lfile = r.file[0]
cfile = r.file[1]

use = bytarr(4096)
use[*] = 1
use[0] = 0
if cfile ne '' then begin
	pc = read_cuts( cfile, error=err)
	if err eq 0 then begin
		for i=0,n_elements(*pc)-1 do begin
			use[ (*pc)[i].x[2]: (*pc)[i].x[3] ] = 0
		endfor
	endif
endif
 
np = n_elements(p)
poor = intarr(np)
poor[*] = 1
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
			
			view = ( round(marks[0:1,3])	< (ns-1)) > 0		; View: V0-V1     markers
			if view[1] le view[0] then goto, bad_view
		
;			if adc eq 64 then begin
;				print,'debug ...'
;			endif
			y = lonarr(ns)
			y[view[0]:view[1]] = median(data[view[0]:view[1]] > 0,smoothw) > 0
			thresh = (min(y[view[0]:view[1]]) + threshold*(max(y[view[0]:view[1]])-min(y[view[0]:view[1]]))) > 3.
			y[view[0]:view[1]] = y[view[0]:view[1]] ge thresh
			
			q = where( use[0:min([ns,4096])-1] eq 0, nq)		; veto CUTs ranges
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
				print,'Too few peaks for ADC =',adc				; goto, bad_peaks
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
				poor[ip] = 0
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
					poor[ip] = (a gt high_gain) or (a lt low_gain)
					if poor[ip] eq 0 then begin
						(*pspec).cal.poly[0] = b
						(*pspec).cal.poly[1] = a
						(*pspec).cal.order = 1
						(*pspec).cal.units = 'channel'
					endif
				endif
			endelse
		endif
	endif
	cont:
endfor

;	Now determine average of cal a,b and scale/offset all cal.poly so that the average is 0.0,1.0 ...

ave_a = 0.0
ave_b = 0.0
avt_a = 0.0
avt_b = 0.0
np = n_elements(p)
got_e = 0
got_t = 0
nse = 0L
nst = 0L
for ip=0,np-1 do begin
    if (*p[ip]).show and ((*p[ip]).size gt 0) and poor[ip] eq 0 then begin
		pspec = p[ip]
		nl = lenchr((*pspec).label)
		label = extract( (*pspec).label, nl-2, nl-1)
		do_energy = (label eq '/E')
		do_time = (label eq '/T')
		if do_energy then begin
			pspec = p[ip]
			ave_b = ave_b + (*pspec).cal.poly[0]
			ave_a = ave_a + (*pspec).cal.poly[1]
			nse = nse+1
			got_e = 1
		endif else if do_time then begin
			pspec = p[ip]
			avt_b = avt_b + (*pspec).cal.poly[0]
			avt_a = avt_a + (*pspec).cal.poly[1]
			nst = nst+1
			got_t = 1
		endif
	endif
endfor
ave_a = ave_a/float(nse)
ave_b = ave_b/float(nse)
avt_a = avt_a/float(nst)
avt_b = avt_b/float(nst)

close_file, 1
close_file, 2
on_ioerror, bad_open
if got_e then begin
	l1file = strip_file_ext(lfile, /triple) + '.gaintrim.energy.var'
	openw, 1, l1file
	printf, 1, '# gaintrim energy'
	printf, 1, '# written by "gain_trim_spectrum_plugin", ' + systime()
	printf, 1, '# file: ' + (*p[0]).file
	printf, 1, '# source: ' + (*p[0]).source
	printf, 1, '# comment: ' + (*p[0]).comment
	printf, 1, '#'
	printf, 1, 'gaintrim.det[].ecoeff[0] 0'
	printf, 1, 'gaintrim.det[].ecoeff[1] 1'
endif
if got_t then begin
	l2file = strip_file_ext(lfile, /triple) + '.gaintrim.time.var'
	openw, 2, l2file
	printf, 2, '# gaintrim time'
	printf, 2, '# written by "gain_trim_spectrum_plugin", ' + systime()
	printf, 2, '# file: ' + (*p[0]).file
	printf, 2, '# source: ' + (*p[0]).source
	printf, 2, '# comment: ' + (*p[0]).comment
	printf, 2, '#'
	printf, 2, 'gaintrim.det[].tcoeff[0] 0'
	printf, 2, 'gaintrim.det[].tcoeff[1] 1'
endif

for ip=0,np-1 do begin
    if (*p[ip]).show and ((*p[ip]).size gt 0) and poor[ip] eq 0 then begin
		pspec = p[ip]
		nl = lenchr((*pspec).label)
		label = extract( (*pspec).label, nl-2, nl-1)
		do_energy = (label eq '/E')
		do_time = (label eq '/T')
		lun = (do_energy ? 1 : 2)
		adc = (*pspec).station + adc_offset_device((*pspec).DevObj)
		if do_energy and nse gt 0 then begin
			(*pspec).cal.poly[1] = (*pspec).cal.poly[1] / ave_a
			(*pspec).cal.poly[0] = ((*pspec).cal.poly[0] - ave_b) / ave_a
			printf, lun, 'gaintrim.det['+strtrim(string(adc),2)+'].ecoeff[] ' + strtrim(string((*pspec).cal.poly[0]),2) + ' ' + strtrim(string((*pspec).cal.poly[1]),2)
		endif
		if do_time and nst gt 0 then begin
			(*pspec).cal.poly[1] = (*pspec).cal.poly[1] / avt_a
			(*pspec).cal.poly[0] = ((*pspec).cal.poly[0] - avt_b) / avt_a
			printf, lun, 'gaintrim.det['+strtrim(string(adc),2)+'].tcoeff[] ' + strtrim(string((*pspec).cal.poly[0]),2) + ' ' + strtrim(string((*pspec).cal.poly[1]),2)
		endif
	endif
endfor

history = 'Build Gain Trim plugin   [Gain_Trim_Spectrum_Plugin]'		; a history record for this plugin

q = where( poor eq 1, nq)
if nq gt 0 then begin
	s = 'Poor gains estimated for the following detectors:'
	k = 0
	for i=0,nq-1 do begin
		if k lt 8 then begin
			ns = n_elements(s)
			s[ns-1] = s[ns-1] + ' ADC = ' + strtrim(string( (*p[q[i]]).station + adc_offset_device((*p[q[i]]).devObj)),2) + ' gain = ' + strtrim(string( (*p[q[i]]).cal.poly[1] ),2) + ','
		endif else begin
			k = 0
			s = [s,' ADC = ' + strtrim(string( (*p[q[i]]).station + adc_offset_device((*p[q[i]]).DevObj)),2) + ' gain = ' + strtrim(string( (*p[q[i]]).cal.poly[1] ),2) + ',']
		endelse
		k = k+1
	endfor
	warning,'gain_trim_spectrum_plugin',s
endif
if got_e then printf, 1, '# end'
if got_t then printf, 2, '# end'

fin:
	close_file, 1
	close_file, 2
	return

bad_view:
	warning,'Gain_Trim_spectrum_plugin','Need to select a range with View first.'
	goto, fin
bad_open:
	warning,'Gain_Trim_spectrum_plugin','Bad open/write of gain trimming file '+lfile
	goto, fin
clear_cal:
	warning,'Gain_Trim_spectrum_plugin','Clear Cals first using "Display/Clear ALL Energy Cals" menu'
	goto, fin
bad_peaks:
	warning,'Gain_Trim_spectrum_plugin',['Too few peaks to work with in View range', $
					'or few dominant peaks for detector(s) =' + strjoin(strtrim( string(adc),2),', ')]
	goto, fin
end

