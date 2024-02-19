;
;  Template Spectrum plugin routine
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
;
;  The plugin SAV files will then be loaded automatically when GeoPIXE.sav runs,
;  if the plugin SAV files are located in the same directory as GeoPIXE.sav.
;
;  Plugin arguments:
;   p     pointer to the GeoPIXE spectrum structure array for the present loaded spectra
;   i     the number of the presently displayed spectrum.
;   marks array of all marker channel values (all marker sets, see below)
;
;  keywords:
;   history     return a history string along with the spectrum result.
;   title     just return the title string (to go in the menu).
;
;  On return to GeoPIXE, it is assumed that only the contents of the selected spectrum have
;  been changed, and the sizes of spectra all remain unchanged.
;  Avoid tinkering with spectrum structure parameters, as strange things may happen.
;
;----------------------------------------------------------------------------------------------

pro add_by_region_spectrum_plugin, p, ii, marks, title=title, history=history

;	Add a file of spectra for a range if individual detectors to the spectra pointer array 'p',
;	which are also for a range of individual detectors.
;	Combine data, errors, as well as charge, flux, processed, ...
;	and charge weight DT_corr values.

COMPILE_OPT STRICTARR

if arg_present(title) then begin
    title = '* Add by Region Plugin'          ; return the menu title for this plugin
    return
endif

path = extract_path( (*p[0]).file)
F = file_requester(filter = '*.spec', title='Load Region SPEC file to add', path=path, $	
			fix_filter=0, preview_routine='spectrum_preview')
if F[0] eq '' then return

p2 = read_spec( F[0], error=err)
pp2 = ptr_new(p2, /no_copy)
if err then begin
	free_spectra, pp2
	return
endif

;	Add spectra arrays based on pointers 'pp1', 'pp2' to  spectra pointer arrays.
;	Combine data, errors, as well as charge, flux, processed, ...
;	and charge weight DT_corr values.

pp1 = ptr_new(p)
spectrum_load_spectrum_increment, pp1, pp2, /free
									
history = 'Add by Region, File='+F[0]			; a history record for this plugin

;...............................................................................................

return
end

