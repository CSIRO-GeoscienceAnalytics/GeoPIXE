;
;  Fit Gamma Spectrum plugin routine
;  -----------------------------
;
;  Plugins should be compiled in IDLDE and saved as a SAV file.
;  Only compile routines for ONE plugin and save using the command:
;  "SAVE, /routines, filename='fit_gamma_spectrum_plugin.sav'" for a "fit_gamma_spectrum_plugin" plugin.
;  To ensure this, exit IDLDE and start it again to compile and save another plugin.
;
;  NOTE: It is important to ensure that ONLY routines for ONE plugin is in each SAV file.
;  Otherwise, unexpected results may result when the SAV files are restored at run-time.
;
;  The plugin SAV files will then be loaded automatically when GeoPIXE.sav runs,
;  if the plugin SAV files are located in the same directory as GeoPIXE.sav.
;
;  Plugin arguments:
;	p		pointer to the GeoPIXE spectrum structure array for the present loaded spectrum
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

pro fit_gamma, x, a, f, pder

;	Fit gamma testr function, where	a[0] = base		a[3] = height
;									a[1] = centre	a[2] = width of source Gaussian
;
;	For use with CURVEFIT function.

if n_params() lt 3 then return

n = n_elements(x)

z = (x - a[1]) / a[2]
g = exp( -z*z)
f = a[0] + a[3] * g

if n_params() ge 4 then begin

	pder = replicate( a[0], n,4)
	pder[*,0] = 1.0
	pder[*,1] = 2.0*a[3]*g*z/a[2]
	pder[*,2] = 2.0*a[3]*g*z*z/a[2]
	pder[*,3] = g
endif

return
end

;----------------------------------------------------------------------------------------------

pro fit_gamma_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Fit Gamma Plugin'				; return the menu title for this plugin
	return
endif

;...............................................................................................
;
; Now comes your code here. Use the spectrum data "*(*p[i]).data" as source,
; and form a new spectrum called "fit". This we want to write back as a fit overlay.
; Make use of these parameters from the spectrum structure.

mark = marks[0:5,1]							; X:    X0-X5     markers
err = check_X0X5_markers(mark)				; this tests for valid ascending X0-X5 markers
if err then begin
	warning,'Fit_Gamma_Spectrum_Plugin','Markers bad '+strcompress(string(indgen(n_elements(mark))),/remove_all)+' ='+string(mark)
	return
endif

ps = p[i]
pd = (*ps).data								; pointer to the spectrum data array
if ptr_valid( pd) eq 0 then return
siz = (*ps).size							; Size of spectrum in channels
ca = (*ps).cal.poly[1]						; energy calibration energy per channel
cb = (*ps).cal.poly[0]						; energy calibration offset
cunits = (*ps).cal.units					; energy calibration units string
charge = (*ps).charge						; integrated charge for spectrum (uC)

spec = *pd									; spectrum data

a = dblarr(4)
a[0] = mean( spec[mark[0]:mark[1]] )		; mean background
a[1] = 0.5*( mark[2] + mark[3])				; centroid estimate
a[2] = 0.2*( mark[3] - mark[2])				; "width" parameters
a[3] = 1000.0								; height

y = spec[mark[0]:mark[5]]
x = findgen(mark[5]-mark[0]+1) + mark[0]
w = 1./(y > 1.0)

r = curvefit( x,y,w, a,sigma, chisq=chisq, function_name='fit_gamma', iter=iter, tol=1.0e-3)

print,'a=',a
print,'s=',sigma
print,'chisq=',chisq,'  iterations=',iter,' reduced chisq=',chisq/(n_elements(x)-n_elements(a))

fit = define(/spectrum)
fit.source = (*ps).source
fit.label = 'Fit_Gamma CurveFit to ' + (*ps).label
fit.cal.poly[0] = (*ps).cal.poly[0] + (*ps).cal.poly[1]*mark[0]
fit.cal.poly[1] = (*ps).cal.poly[1]
fit.cal.units = (*ps).cal.units
fit.cal.order = 1
fit.comment = 'CurveFit ...'

fit.size = n_elements(r)
fit.data = ptr_new(r, /no_copy)

if ptr_valid( (*ps).fit[0] ) then free_spectrum, (*ps).fit[0]
(*ps).fit[0] = ptr_new(fit, /no_copy)
if (*ps).n_fit lt 1 then (*ps).n_fit = 1

history = fit.label

;...............................................................................................

return
end

