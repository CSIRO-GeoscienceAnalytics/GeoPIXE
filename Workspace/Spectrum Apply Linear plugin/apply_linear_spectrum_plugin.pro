;
;  Linearize Spectrum plugin routine
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

pro apply_linear_spectrum_plugin, p, i, marks, title=title, history=history, first=first

COMPILE_OPT STRICTARR

common c_apply_linearize_1, lfile
if n_elements(lfile) lt 1 then lfile=''

if arg_present(title) then begin
	title = 'Apply Linearize Plugin'				; return the menu title for this plugin
	return
endif

;...............................................................................................

; Linearize a spectrum based on centroids in Cut regions set on
; evenly spaced pulser peaks and stored in the .linear file.
;
; Note: This is only needed if the Maia data-stream was NOT linearized during
; data collection, which is the normal mode now. This routine is only needed
; for old data (e.g. pre 2010), or to apply a linearization tweak.

pspec = p[i]

ns = (*pspec).size
data = float( *(*pspec).data)
path = extract_path((*pspec).file)
if lenchr(lfile) gt 0 then path = extract_path(lfile)
ndet = -1

if first then begin
	lfile = file_requester( /read, filter = ['*.linear.var','*.linear'], file=lfile, $
		/noconfirm, /must_exist, group=group, path=path, $
		title='Select Linearization file (or Cancel)', /fix_filter)
endif
first = 0
if lenchr(lfile) lt 1 then return

f = get_linearize(lfile, max=4096, do_linear=do_linear, multi=multilinear, /inverse)
;f = get_linearize(lfile, max=4096, do_linear=do_linear, multi=multilinear)
if do_linear eq 0 then goto, bad_linear
print,' Found linearization, applying to spectrum ...'

; write back the modified spectrum data
; assumes that spectrum length has not changed

if multilinear then begin
	ndet = (*pspec).station + adc_offset_device((*pspec).DevObj)
	if ndet lt n_elements(f[0,*]) then begin
		*(*pspec).data = map_spec( (*pspec).data, table=f[*,ndet])
	endif else begin
		goto, bad_table
	endelse
endif else begin
	*(*pspec).data = map_spec( (*pspec).data, table=f)
endelse

history = 'Apply Linearize plugin   [apply_linear_spectrum_plugin]'		; a history record for this plugin

;...............................................................................................

return

bad_linear:
	warning,'apply_linear_spectrum_plugin','Bad open/write of linearize file '+lfile
	return
bad_cuts:
	warning,'apply_linear_spectrum_plugin','Bad cuts read'
	return
bad_fit:
	warning,'apply_linear_spectrum_plugin','Bad fit, status = '+string(status)
	return
bad_table:
	warning,'apply_linearize_plugin','Linearize table not found for detector # ' + strtrim(string(ndet),2)
	return
end

