;
;
;  Apply an IDL approximation to Fortran low-stats filter (SNIP algorithm)
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

pro low_stats_IDL_filter_spectrum_plugin, p, i, marks, title=title, history=history

COMPILE_OPT STRICTARR
if arg_present(title) then begin
    title = 'low_stats_IDL_filter Plugin'          ; return the menu title for this plugin
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
t2 = float(*pspec)
;t1 = median(t2, wm/4 > 2)
t1 = t2

;...............................................................................................

spec1 = t1
n_spec = siz
lowi = mark[0]
highi = mark[1]
err = 1

;	Implement Fortran code exactly here in IDL for testing (this will be slow) ...

low = round(lowi)
high = round(highi)
if low lt 0 then low=0
if low gt n_spec-2 then low=n_spec-2
if high gt n_spec-1 then  high=n_spec-1
debug = 0

if low gt high-2 then begin
	err = 1
	goto, done
endif
spec2 = spec1

	BIG_A = 75.              ; desired sum slope			Original parameters set (SNIP, 1988)
	BIG_B = 10.              ; desired sum min
	TOO_SMALL = 10.          ; minimum sum
	RATIO = 1.3              ; maximum slope
	SCALE = 1.5              ; FWHM scaling

;	BIG_A = 300.             ; desired sum slope
;	BIG_B = 10.              ; desired sum min
;	TOO_SMALL = 10.          ; minimum sum
;	RATIO = 1.3              ; maximum slope
;	SCALE = 1.               ; FWHM scaling

for k=low,high do begin

;	if i eq 1300 then begin
;		print, 'debug ...'
;		debug = 1
;	endif
	
	FWHM = sqrt( AF*float(k) + BF)

	FILTER = spec1[k]
	BIG = BIG_A * SQRT(ABS(FILTER))          ; desired sum
	IF BIG LT BIG_B then BIG=BIG_B           ; minimum sum

	NW = round(SCALE*FWHM)
	IF NW LT 2 then NW=2

	LEFT = 0.
	RIGHT = 0.
	W = 1.
	for j=1,NW do begin                      ; full side sums
		LEFT = LEFT + spec1[ CLIP(k-j,0,n_spec-1)]
		RIGHT = RIGHT + spec1[ CLIP(k+j,0,n_spec-1)]
		W = W+2.
	endfor

	if debug then print,'k=',k,' nw=',nw,' filt=',filter,' left,right=',left,right,' y=',y,' big=',big
	
	Y = FILTER + LEFT+RIGHT                  ; reduce span for good
	for j=NW,1,-1 do begin                   ; statistics on peaks

		sr = ABS(RIGHT)+1.
		sl = ABS(LEFT)+1.
		Z1 = (sr-sqrt(sr))/(sl+sqrt(sl))
		Z2 = (sl-sqrt(sl))/(sr+sqrt(sr))
		if debug then print,'	j=',j,' nw=',nw,' left,right=',left,right,' y=',y,' z1,z2=',z1,z2
		
		IF (Y LE BIG) AND (Z1 LE RATIO) AND (Z2 LE RATIO) then GOTO, cont_79

		LEFT = LEFT - spec1[ CLIP(k-j,0,n_spec-1)]
		RIGHT = RIGHT - spec1[ CLIP(k+j,0,n_spec-1)]
		IF FILTER+LEFT+RIGHT LE TOO_SMALL then GOTO, cont_79

		Y = FILTER + LEFT+RIGHT
		W = W-2.
	endfor
	
cont_79:
    spec2[k] = Y/W
endfor

	t2 = spec2
	err = 0
done:

;...............................................................................................

t2 = median(t2, wm/4 > 2)				; clip any remaining single spikes

*pspec = t2								; write back the modified spectrum data
										; assumes that spectrum length has not changed

history = 'low_stats_IDL filter'		; a history record for this plugin

return
end

