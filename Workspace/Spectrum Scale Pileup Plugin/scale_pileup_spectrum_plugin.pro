;
;  Scale_pileup Spectrum plugin routine
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

pro scale_pileup_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
    title = 'Plot scaled pileup calc Plugin'          ; return the menu title for this plugin
    return
endif

pspec = (*p[i]).data                    ; pointer to the spectrum data array
siz = (*p[i]).size                     ; Size of spectrum in channels
ca = (*p[i]).cal.poly[1]              ; energy calibration energy per channel
cb = (*p[i]).cal.poly[0]              ; energy calibration offset
cunits = (*p[i]).cal.units            ; energy calibration units string
charge = (*p[i]).charge               ; integrated charge for spectrum (uC)
mark = marks[0:1,3]                   ; View: V0-V1     markers
elow = ca*mark[0]+cb
ehigh = ca*mark[1]+cb

w1 = 0.003
w0 = (0.16)*(0.16) - w1*5.895
x = mark[0] + indgen(mark[1]-mark[0]+1)
e = ca * x + cb               			; energy
w = sqrt(w0 + w1*e)		          		; FWHM (energy)
wm = sqrt( w0 + w1*(4*elow+ehigh)/5)/ca	; middle FWHM

AF = w1 * ca							; FWHM**2 (energy) coefficients
BF = w0 + w1 * cb
AF = AF / (ca*ca)						; FWHM**2 (channels)
BF = BF / (ca*ca)

if (*p[i]).n_fit eq 0 then return

ppu = (*p[i]).fit[(*p[i]).n_fit-1]

;copy_pointer_data, ppu, pt, /init
;*(*pt).data = 10. * *(*pt).data 
;(*p[i]).fit[(*p[i]).n_fit] = pt
;(*p[i]).n_fit ++
;
;copy_pointer_data, ppu, pt, /init
;*(*pt).data = 100. * *(*pt).data 
;(*p[i]).fit[(*p[i]).n_fit] = pt
;(*p[i]).n_fit ++

copy_pointer_data, ppu, pt, /init
*(*pt).data = 1000. * *(*pt).data 
(*p[i]).fit[(*p[i]).n_fit] = pt
(*p[i]).n_fit ++

;copy_pointer_data, ppu, pt, /init
;*(*pt).data = 10000. * *(*pt).data 
;(*p[i]).fit[(*p[i]).n_fit] = pt
;(*p[i]).n_fit ++

;q = where( *(*p[i]).data lt 2000, nq)
;if nq gt 0 then (*(*p[i]).data)[q] = 0
(*(*p[i]).data)[*] = 0

history = 'Scale pileup, plot overlays'		; a history record for this plugin

;...............................................................................................

return
end

