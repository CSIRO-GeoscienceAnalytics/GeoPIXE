;
;  Peak Stats Spectrum plugin routine
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

pro Peak_Stats_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Peak Stats Plugin'			; return the menu title for this plugin
	return									; * indicates that it accepts all displayed spectra
endif
np = n_elements(p)
show = intarr(np)
for i=0,np-1 do show[i] = (*p[i]).show
q = where( show eq 1, nq)
if nq eq 0 then return

id = intarr(nq)
sdev = fltarr(nq)

mark = marks[0:1,3]                 	    ; View: V0-V1     markers
if (mark[0] eq 0) and (mark[1] eq 0) then begin
	warning,'',['Zero VIEW markers.','Select VIEW and try again.']
	return
endif
cunits = (*p[q[0]]).cal.units          	    ; energy calibration units string
if cunits eq 'keV' then begin
	ca = (*p[q[0]]).cal.poly[1]             ; energy calibration energy per channel
	cb = (*p[q[0]]).cal.poly[0]             ; energy calibration offset
	elow = ca*mark[0]+cb
	ehigh = ca*mark[1]+cb
endif else begin
	elow = mark[0]
	ehigh = mark[1]
endelse

for j=0,nq-1 do begin
	i = q[j]								; spectrum number with show=1
	pspec = (*p[i]).data                    ; pointer to the spectrum data array
	siz = (*p[i]).size                      ; Size of spectrum in channels
	ca = (*p[i]).cal.poly[1]                ; energy calibration energy per channel
	cb = (*p[i]).cal.poly[0]                ; energy calibration offset
	charge = (*p[i]).charge                 ; integrated charge for spectrum (uC)

	if cunits eq 'keV' then begin
		xl = (elow - cb) / ca
		xh = (ehigh - cb) / ca	
		x = xl + indgen(xh-xl+1)			; channel
		e = ca * x + cb               		; energy
	endif else begin
		xl = elow
		xh = ehigh	
		x = xl + indgen(xh-xl+1)			; channel
		e = x
	endelse
	y = (*pspec)[x]							; counts
	
	xav = total( y*x) / total(y)
	var = total( y*(x-xav)^2) / total(y)
	sdev[j] = sqrt(var)

	id[j] = (*p[i]).station + adc_offset_device( (*p[i]).DevObj)
	print,j,', channel =',id[j],' sdev = ',sdev[j],', dE.FWHM = ', 2.3*sdev[j]*ca,' '+cunits
endfor

fin:
;	plot_maia_parameter, id, 2.306*sdev*ca, title='View FWHM (keV)'
	plot_maia_parameter, id, sdev, title='View standard deviation (channels)'	;, /white
	return
end

