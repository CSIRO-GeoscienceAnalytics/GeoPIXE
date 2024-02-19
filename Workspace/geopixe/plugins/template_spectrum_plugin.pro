;
;  Template Spectrum plugin routine
;  -----------------------------
;
;  All Spectrum template routines MUST be named with "_spectrum__plugin.pro"
;  at the end of the file name. For a new "Fred" plugin, copy and rename this file
;  to "Fred_spectrum_plugin.pro" and edit the first line to:
;  "pro fred_spectrum_plugin, p, i, title=title, history=history"
;
;  Plugins should be compiled in IDLDE and saved as a SAV file. Do not 'RESOLVE_ALL'.
;  Only compile routines for ONE plugin and save using the command:
;
;  "SAVE, /routines, filename='fred_spectrum_plugin.sav'" for a "fred_spectrum_plugin" plugin.
;
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

pro template_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Template Plugin'				; return the menu title for this plugin
	return
endif

;...............................................................................................
;
; Now comes your code here. Use the spectrum data "*(*p[i]).data" as source,
; and form a new spectrum called "new_spec". The example below will just do a simple smooth:
; Make use of these parameters from the spectrum structure.

; First select any markers that you need (if any)
; mark = marks[0,0]							; Peak identification marker
 mark = marks[0:5,1]						; X:    X0-X5     markers
; mark = marks[0:1,2]						; Cal:  C0-C1     markers
; mark = marks[0:1,3]						; View: V0-V1     markers
; mark = marks[0:1,4]						; Cut:  Cut0-Cut1 markers

; First test to see if the markers are valid. For X markers, use check_X0X5_markers:
 err = check_X0X5_markers(mark)				; this tests for valid ascending X0-X5 markers
; err = (mark[1] le mark[0])				; sufficient to test a pair of markers
;if err then begin
;	warning,'Template_Spectrum_Plugin','Markers bad '+strcompress(string(indgen(n_elements(mark))),/remove_all)+' ='+string(mark)
;	return
;endif

; In IDLDE, stop execution here and use "Help, *p[i],/struct to examine the spectrum data structure

pspec = (*p[i]).data						; pointer to the spectrum data array
siz = (*p[i]).size							; Size of spectrum in channels
ca = (*p[i]).cal.poly[1]					; energy calibration energy per channel
cb = (*p[i]).cal.poly[0]					; energy calibration offset
cunits = (*p[i]).cal.units					; energy calibration units string
charge = (*p[i]).charge						; integrated charge for spectrum (uC)
x = mark									; vector of X0-X5 markers
;print, x*ca + cb

old_spec = *pspec							; spectrum data

new_spec = smooth( old_spec, 2)				; smooth spectrum over 2 channels
											; (a simple example)

*pspec = new_spec							; write back the modified spectrum data
											; assumes that spectrum length has not changed

history = 'Template plugin smooth 2   [Template_Spectrum_Plugin]'		; a history record for this plugin

;...............................................................................................

return
end

