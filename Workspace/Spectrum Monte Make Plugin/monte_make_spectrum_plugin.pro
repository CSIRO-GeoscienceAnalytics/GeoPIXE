;
;  Monte_make Spectrum plugin routine
;  -----------------------------
;
;  All Spectrum Monte_make routines MUST be named with "_spectrum__plugin.pro"
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

pro Monte_make_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
    title = 'Monte Make Plugin'          ; return the menu title for this plugin
    return
endif
widget_control, /hourglass

pspec = (*p[i]).data                  ; pointer to spectrum data array
siz = (*p[i]).size                    ; size of spectrum in channels
ca = (*p[i]).cal.poly[1]              ; energy calibration energy per channel
cb = (*p[i]).cal.poly[0]              ; energy calibration offset
cunits = (*p[i]).cal.units            ; energy calibration units string
charge = (*p[i]).charge               ; integrated charge for spectrum (uC)
mark = marks[0:1,3]                   ; View: V0-V1     markers
if (mark[1] le mark[0]) or (mark[1] lt 5) then begin
    mark[0] = 1
    mark[1] = 4095
endif

w1 = 0.003
w0 = (0.18)*(0.18) - w1*5.895
x = mark[0] + indgen(mark[1]-mark[0]+1)
e = ca * x + cb                  ; energy
w = sqrt(w0 + w1*e) / ca          ; FWHM (channels)

AF = w1 * ca
BF = w0 + w1 * cb
t = median(*pspec, 0.3*mean(w) > 3)
new = *pspec
err = low_stats_filter( t,new, siz, mark[0],mark[1], AF,BF)
*pspec = new

cumm = fltarr(siz)
cumm[0] = (*pspec)[0]
for k=1,siz-1 do begin
    cumm[k] = cumm[k-1] + (*pspec)[k]
endfor
cumm = float(cumm) * float(siz-1)/cumm[siz-1]

for j=1,32 do begin
    r = siz*randomu( seed, 500000L)
    m = binary_search2( cumm, r)
    if j eq 1 then begin
       h = histogram( m, binsize=1, min=0,max=4095)
    endif else begin
       h = histogram( m, binsize=1, min=0,max=4095, input=h)
    endelse
    print,'Done',j
endfor

top = min([n_elements(h),siz])
(*pspec)[0:top-1] = float(h[0:top-1])

history = 'Monte_make plugin [Monte_make_Spectrum_Plugin]'     ; a history record for this plugin

return
end

