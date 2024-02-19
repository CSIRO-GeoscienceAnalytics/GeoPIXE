;-----------------------------------------------------------------
;
;  Gain trim from Ecal Spectrum plugin routine
;
; Use individual E cal data to build a gain-trim, and write back as new cal.
;
; Assumes that the gain-trim has not been enabled for this data.
; To adjust a gain-trim based on an Ecal spectrum, use the new plugin
; "gain_trim_adjust_spectrum_plugin".
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

pro Gain_Trim_Ecal_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Build Gain Trim from Ecal Plugin'	; return the menu title for this plugin
	return											; * indicates that it accepts all displayed spectra
endif

;...............................................................................................

; Use individual E cal data to build a gain-trim, and write back as new cal.

np = n_elements(p)
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
		
			if (first_e and do_energy) or (first_t and do_time) then begin
				if do_energy then begin
					first_e = 0
					ca_e = (*pspec).cal.poly[1]
					cb_e = (*pspec).cal.poly[0]
				endif 
				if do_time then begin
					first_t = 0
					ca_t = (*pspec).cal.poly[1]
					cb_t = (*pspec).cal.poly[0]
				endif 
				(*pspec).cal.poly[0] = 0.0
				(*pspec).cal.poly[1] = 1.0
				(*pspec).cal.order = 1
				(*pspec).cal.units = 'channel'
			endif else begin
				if do_energy or do_time then begin
					if do_energy then begin
						ca = ca_e
						cb = cb_e
					endif
					if do_time then begin
						ca = ca_t
						cb = cb_t
					endif
					ao = (*pspec).cal.poly[1]
					bo = (*pspec).cal.poly[0]
					a = ao / ca
					b = (bo - cb) / ca			
					(*pspec).cal.poly[0] = b
					(*pspec).cal.poly[1] = a
					(*pspec).cal.order = 1
					(*pspec).cal.units = 'channel'
				endif
			endelse
		endif
	endif
endfor

;	Now determine average of cal a,b and scale/offset all cal.poly so that the average is 0.0,1.0 ...

av_a = 0.0
av_b = 0.0
ns = 0L
for ip=0,np-1 do begin
	pspec = p[ip]
	if (*pspec).show then begin
		av_b = av_b + (*pspec).cal.poly[0]
		av_a = av_a + (*pspec).cal.poly[1]
		ns = ns+1
	endif
endfor
av_a = av_a/float(ns)
av_b = av_b/float(ns)

close_file, 1
on_ioerror, bad_open
lfile = strip_file_ext((*p[0]).file) + '.gaintrim.energy.var'
openw, 1, lfile
printf, 1, '# gaintrim energy'
printf, 1, '# written by "gain_trim_ecal_spectrum_plugin", ' + systime()
printf, 1, '# file: ' + (*p[0]).file
printf, 1, '# source: ' + (*p[0]).source
printf, 1, '# comment: ' + (*p[0]).comment
printf, 1, '#'
printf, 1, 'gaintrim.det[].ecoeff[0] 0'
printf, 1, 'gaintrim.det[].ecoeff[1] 1'

for ip=0,np-1 do begin
	pspec = p[ip]
	adc = (*pspec).station + adc_offset_device((*pspec).DevObj)
	if (*pspec).show then begin
		(*pspec).cal.poly[1] = (*pspec).cal.poly[1] / av_a
		(*pspec).cal.poly[0] = ((*pspec).cal.poly[0] - av_b) / av_a
		if ns gt 0 then begin
			printf, 1, 'gaintrim.det['+strtrim(string(adc),2)+'].ecoeff[] ' + strtrim(string((*pspec).cal.poly[0]),2) + ' ' + strtrim(string((*pspec).cal.poly[1]),2)
		endif
	endif
endfor
printf, 1, '# end'

fin:
	close_file, 1
	history = 'Build Gain Trim Ecal plugin   [gain_trim_Ecal_spectrum_plugin]'		; a history record for this plugin
	return

bad_read:
	warning,'gain_trim_Ecal_spectrum_plugin','Error reading existing gain trimming file '+lfile
	return
bad_open:
	warning,'gain_trim_Ecal_spectrum_plugin','Bad open/write of gain trimming file '+lfile
	goto, fin
end

