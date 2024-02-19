;
;  Pileup Detector Spectrum plugin routine
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

pro Pileup_Detector_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Pileup by Detector Plugin'			; return the menu title for this plugin
	return											; * indicates that it accepts all displayed spectra
endif

	np = n_elements(p)
	if np lt 1 then return
	if ptr_good( p[0], /struct) eq 0 then return
	if (*p[0]).has_dead eq 0 then goto, bad_hist
	
	h = 100. * *(*p[0]).pileup_loss_det
	
	window, 0
	!p.title = 'Pileup Losses per Detector'
	!x.title = 'Detector Channel'
	!y.title = 'Pileup Loss Fraction (%)'
	n = n_elements(h)
	x = indgen(n)
	y = h[x]
	plot, x,y, /nodata
	oplot, x,y, psym=10, color=spec_colour('green')

;	history = 'Pileup Detector Histogram plugin'		; a history record for this plugin

	return

bad_hist:
	warning,'Pileup_Detector_spectrum_plugin','No Pileup detector histogram found.'
	return
end

