;
;  Clear CUTs channels in spectra plugin routine
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

pro clear_cuts_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Clear CUTs channels in spectra Plugin'		; return the menu title for this plugin
	return									; * indicates that it accepts all displayed spectra
endif

np = n_elements(p)
show = intarr(np)
for i=0,np-1 do show[i] = (*p[i]).show
q = where( show eq 1, nq)
if nq eq 0 then return

cuts_file = file_requester( /read, filter = '*.cuts', $
          /noconfirm, /must_exist, path=extract_path((*p[q[0]]).file), $
          title='Select channels to Clear CUTs file', /fix_filter)

pcuts = read_cuts( cuts_file, error=error)
if error then goto, bad_cuts_file

nc = n_elements( *pcuts)
if nc lt 1 then goto, bad_cuts
e2 = (*pcuts).e[2]
e3 = (*pcuts).e[3]

for j=0,nq-1 do begin
	i = q[j]								; spectrum number with show=1
	ca = (*p[i]).cal.poly[1]
	cb = (*p[i]).cal.poly[0]
	if (*p[i]).cal.units ne 'channels' then begin
		x2 = (e2 - cb) / ca
		x3 = (e3 - cb) / ca
	endif else begin
		x2 = (*pcuts).x[2]
		x3 = (*pcuts).x[3]
	endelse
	pspec = (*p[i]).data                    ; pointer to the spectrum data array
	nm = n_elements(*pspec)

	for k=0,nc-1 do begin
		i3 = x3[k] < (nm-1)
		i2 = x2[k] < i3
		(*pspec)[ i2:i3] = 0.				; clear channels
	endfor
endfor

finish:
	return

bad_cuts_file:
	warning,'clear_cuts_spectrum_plugin','Bad CUTs file read'
	goto, finish
bad_cuts:
	warning,'clear_cuts_spectrum_plugin',['Zero CUT markers.','Select regions of the spectrum to clear using CUT markers, and try again.']
	goto, finish
end

