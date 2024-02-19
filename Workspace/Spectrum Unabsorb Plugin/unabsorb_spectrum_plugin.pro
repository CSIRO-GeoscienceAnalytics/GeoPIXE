;
;  Unabsorb Spectrum plugin routine
;  -----------------------------
;
;  All Spectrum template routines MUST be named with "_spectrum__plugin.pro"
;  at the end of the file name. For a new "Fred" plugin, copy and rename this file
;  to "Fred_spectrum_plugin.pro" and edit the first line to:
;  "pro fred_spectrum_plugin, p, i, title=title, history=history"
;
;  Plugins should be compiled in IDLDE and saved as a SAV file.
;  Only compile routines for ONE plugin and save using the command:
;  "SAVE, /routines, filename='unabsorb_spectrum_plugin.sav'" for a "unabsorb_spectrum_plugin" plugin.
;  To ensure this, exit IDLDE and start it again to compile and save another plugin.
;
;  NOTE: It is important to ensure that ONLY routines for ONE plugin is in each SAV file.
;  Otherwise, unexpected results may result when the SAV files are restored at run-time.
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

pro unabsorb_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Un-Absorb Plugin'				; return the menu title for this plugin
	return
endif

;...............................................................................................
;
; Now comes your code here. Use the spectrum data "*(*p[i]).data" as source,
; and form a new spectrum called "new_spec". The example below will just do a simple smooth:
; Make use of these parameters from the spectrum structure.

 mark = marks[0:1,3]						; View: V0-V1     markers

err = (mark[1] le mark[0])					; sufficient to test a pair of markers
if err then begin
	warning,'unabsorb_Spectrum_Plugin','Markers bad '+strcompress(string(indgen(n_elements(mark))),/remove_all)+' ='+string(mark)
	return
endif

pspec = (*p[i]).data						; pointer to the spectrum data array
siz = (*p[i]).size							; Size of spectrum in channels
ca = (*p[i]).cal.poly[1]					; energy calibration energy per channel
cb = (*p[i]).cal.poly[0]					; energy calibration offset
cunits = (*p[i]).cal.units					; energy calibration units string
charge = (*p[i]).charge						; integrated charge for spectrum (uC)
v = mark									; vector of View markers

F1 = dialog_pickfile(title='Remove this filter [in View range] ...',/must_exist, filter='*.filter')
if lenchr(F1[0]) lt 1 then return

filter1 = read_filters(F1[0],error=err)
if err then begin
	warning,'unabsorb_Spectrum_Plugin','Filter error'
	return
endif
F2 = dialog_pickfile(title='Apply this filter [in View range] ...',/must_exist, filter='*.filter')
if lenchr(F2[0]) lt 1 then return

filter2 = read_filters(F2[0],error=err)
if err then begin
	warning,'unabsorb_Spectrum_Plugin','Filter error'
	return
endif

x = indgen(siz)
q = where( (x ge v[0]) and (x le v[1]))
e = ca*x[q] + cb
new_spec = *pspec

t1 = transmit(filter1, e)
q1 = where( finite(t1) and (t1 gt 1.0e-15))
if q1[0] eq -1 then begin
	warning,'unabsorb_Spectrum_Plugin','All infinite?'
	return
endif
new_spec[q[q1]] = (*pspec)[q[q1]] / t1[q1]

t2 = transmit(filter2, e)
q2 = where( finite(t2) and (t2 gt 1.0e-15))
if q2[0] eq -1 then begin
	warning,'unabsorb_Spectrum_Plugin','All infinite?'
	return
endif
new_spec[q[q2]] = new_spec[q[q2]] * t2[q2]

*pspec = new_spec							; write back the modified spectrum data
											; assumes that spectrum length has not changed

history = 'unabsorb filter   [unabsorb_Spectrum_Plugin]'		; a history record for this plugin

;...............................................................................................

return
end

