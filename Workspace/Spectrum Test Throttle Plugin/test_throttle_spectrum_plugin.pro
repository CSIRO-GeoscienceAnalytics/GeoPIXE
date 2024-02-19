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

pro test_throttle_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
    title = 'Test Throttle Plugin'		; return the menu title for this plugin
    return
endif

if n_elements(p) le i then return
if size(p[i],/tname) ne 'POINTER' then return
if size(*p[i],/tname) ne 'STRUCT' then return
siz = (*p[i]).size						; Size of spectrum in channels

; Call modal pop-up to gather parameters.
; Hide away the parent widget ID in spectrum struct.

beam = 17.0								; X-ray beam energy --> more throttling near scatter peaks

pars = throttle_select( (*p[i]).group, total(*(*p[i]).data))

if n_elements(pars) lt 1 then return
if size(pars,/tname) ne 'STRUCT' then return
if pars.error then return

time = pars.time						; acquisition time (sec)
detectors = pars.detectors				; number of detectors in array
max_rate = pars.max						; maximum rate for Blog
rate = pars.rate						; actual count-rate
factor = pars.factor					; factor by which to reduce spectrum total
if factor lt 1. then begin
	warning,'throttle','no throttling needed'
	return
endif

throttle = build_throttle( p[i], factor)

if max(throttle) eq 255 then warning,'throttle','maximum 8 bit throttle used'
if max(throttle) eq 1 then warning,'throttle','no throttling needed'

save_file = strip_file_ext(strip_path((*p[i]).file))+'-throttle.txt'
path = extract_path((*p[i]).file)

F = dialog_pickfile (/write, filter = '*.txt', /noconfirm, path=path, $
			title='Save Throttle Spectrum', file = save_file, group=(*p[i]).group, fix_filter=1)
if F ne '' then begin
	close, 1
	openw, 1, F
	printf, 1, '#throttle ',factor
	for j=0,siz-1 do begin
		if throttle[j] ne 1 then begin
			printf, 1, strtrim(string(j),2), throttle[j], format='(A4,1x,I4)'
		endif
	endfor
	close, 1
endif

if (*p[i]).n_fit gt 0 then begin
	for j=0,(*p[i]).n_fit-1 do begin
		free_spectrum, (*p[i]).fit[j]
	endfor
endif

sfit = define(/spectrum)
sfit.source = (*p[i]).source
sfit.cal = (*p[i]).cal
sfit.label = 'Throttle factor spectrum'
sfit.comment = 'Throttle spectrum overlay'
sfit.size = siz
sfit.data = ptr_new( float(throttle), /no_copy)

(*p[i]).fit[0] = ptr_new(sfit, /no_copy)	; overlay the throttle array

sfit = define(/spectrum)
sfit.source = (*p[i]).source
sfit.cal = (*p[i]).cal
sfit.label = 'Throttled down spectrum'
sfit.comment = 'Throttled spectrum'
sfit.size = siz
sfit.data = ptr_new( float( *(*p[i]).data/throttle ), /no_copy)

(*p[i]).fit[1] = ptr_new(sfit, /no_copy)	; overlay the throttled spectrum
(*p[i]).n_fit = 2

print,'Total = ',total(*(*p[i]).data),'  throttled = ',total(*(*p[i]).data/throttle)
return
end

