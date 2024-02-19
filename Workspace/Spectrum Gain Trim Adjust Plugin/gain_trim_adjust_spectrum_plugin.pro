;
;  Gain trim Adjust Spectrum plugin routine
;  
;  Apply an adjustment to a set of gain-trimming coefficients. This plugin
;  actually ignores the current spectrum and reads and writes files.
;  
;  i.e. Two step process:
;  
;  1. Use X-ray peaks and the "gain_trim_cuts_spectrum_plugin" to build a spectrum
;     calibration set to Maia data with an original gain-trim active.
;  2. Perhaps do some manual tweeks to correct skew peaks.
;  3. Apply this new set of cal coeficients as an adjustment to the original set to
;     build the combined set, to be loaded back into maia.
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

pro Gain_Trim_Adjust_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Build Gain Trim Adjust Plugin'		; return the menu title for this plugin
	return											; * indicates that it accepts all displayed spectra
endif

;...............................................................................................

 ; Select first data file (or multiply select several to append) ...

path = ''
if ptr_good(p[0], /struct) then begin
	path  = extract_path( (*p[0]).file)
endif
F1 = file_requester(filter = '*.spec', title='Select original Gain-Trim spectrum', path=path, $	
					fix_filter=0)		;, preview_routine='spectrum_preview')
if F1[0] eq '' then return

path  = extract_path( F1[0])
F2 = file_requester(filter = '*.spec', title='Select calibrated spectra to apply adjustment to Gain-Trim', path=path, $	
					fix_filter=0)		;, preview_routine='spectrum_preview')
if F2[0] eq '' then return

F3 = file_requester(filter = '*.spec', title='New Gain-Trim output spectrum', path=path, $	
					fix_filter=0)		;, preview_routine='spectrum_preview')
if F3[0] eq '' then return


p1 = read_spec( F1[0], error=error)
if error ne 0 then goto, bad_file1

p2 = read_spec( F2[0], error=error)
if error ne 0 then goto, bad_file2

s = intarr(n_elements(p2))
for k=0,n_elements(p2)-1 do s[k]=(*p2[k]).station

for i=0,n_elements(p1)-1 do begin
	if ((*p1[i]).multiplicity gt 1) or (*p1[i]).array then goto, bad_mult
	
	j1 = (*p1[i]).station
	adc = j1 + adc_offset_device((*p1[i]).DevObj)
	q2 = where( s eq j1, nq2)
	if nq2 ne 0 then begin
		(*p1[i]).show = 1
		a1 = (*p1[i]).cal.poly[1]
		b1 = (*p1[i]).cal.poly[0]
		a = (*p2[q2[0]]).cal.poly[1]
		b = (*p2[q2[0]]).cal.poly[0]
		a2 = a * a1
		b2 = a * b1 + b
		(*p1[i]).cal.poly[1] = a2
		(*p1[i]).cal.poly[0] = b2
		print,'det #',adc,' old trim A,B=',a1,b1,'  new trim A,B=',a2,b2
	endif else begin
		(*p1[i]).show = 0
	endelse
endfor

;	Now determine average of cal a,b and scale/offset all cal.poly so that the average is 0.0,1.0 ...

av_a = 0.0
av_b = 0.0
np = n_elements(p1)
ns = 0L
for ip=0,np-1 do begin
	pspec = p1[ip]
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
lfile = strip_file_ext(F3[0]) + '.gaintrim.energy.var'
openw, 1, lfile
printf, 1, '# gaintrim energy'
printf, 1, '# written by "gain_trim_adjust_spectrum_plugin", ' + systime()
printf, 1, '# file: ' + (*p1[0]).file
printf, 1, '# source: ' + (*p1[0]).source
printf, 1, '# comment: ' + (*p1[0]).comment
printf, 1, '#'
printf, 1, 'gaintrim.det[].ecoeff[0] 0'
printf, 1, 'gaintrim.det[].ecoeff[1] 1'

for ip=0,np-1 do begin
	pspec = p1[ip]
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
	write_spec, p1, F3[0]
	return

bad_file1:
	warning,'Gain_Trim_Adjust_spectrum_plugin','Bad original gain-trim file read.'
	goto, fin
bad_file2:
	warning,'Gain_Trim_Adjust_spectrum_plugin','Bad adjustment gain-trim file read.'
	goto, fin
bad_open:
	warning,'Gain_Trim_Adjust_spectrum_plugin','Bad open/write of gain trimming file '+lfile
	goto, fin
bad_mult:
	warning,'Gain_Trim_Adjust_spectrum_plugin','Spectra have multiple detector numbers each.'
	goto, fin
end

