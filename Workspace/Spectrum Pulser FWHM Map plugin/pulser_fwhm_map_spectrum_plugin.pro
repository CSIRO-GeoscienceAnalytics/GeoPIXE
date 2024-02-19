;-----------------------------------------------------------------
;
;  Pulser FWHM map Spectrum plugin routine
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

pro Pulser_FWHM_map_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Plot Pulser FWHM map Plugin'			; return the menu title for this plugin
	return											; * indicates that it accepts all displayed spectra
endif

	case !version.os_family of
		'MacOS': begin
			retain = 2
			end
		'unix': begin
			retain = 2
			end
		else: begin
			retain = 1
			end
	endcase

;...............................................................................................

	drop = ['Use peak within VIEW markers','Use peak above background using X markers']
	help_drop = 'Select markers to use to sample pulser peak: (i) 2 View markers, or (ii) 6 X markers to define peak and left/right backgrounds.'
	text = ['Calibration A (keV/channel','Minimum FWHM (eV)','Maximum FWHM (eV)']
	initial_text = str_tidy(string([0.0086,180.,600.]))
	help_text = ['Assumed energy calibration A parameter (keV / channel).','Minimum FWHM (eV) to display in colour map.','Maximum FWHM (eV) to display in colour map.']
	Help_default = 'Map the FWHM of the pulser peaks in the VIEW range (or using X markers to subtract background). Remember to set the VIEW (or X) markers around the pulser peak.'
	r = options_popup( title='Pulser FWHM Mapping Options', text=text, initial_text=initial_text, help_text=help_text, $
				help_default=help_default, drop=drop, help_drop=help_drop, min_xsize=500, error=error)
	if error then return
	
	use_x = r.drop[0]
	ca = float2(r.text[0])
	ymin = float2(r.text[1])
	ymax = float2(r.text[2])
	fwhm = fltarr(384)
	fwhm[*] = 100000.
	first = 1
	xview = marks[0:1,3]
	eview = xview * (*p[0]).cal.poly[1] + (*p[0]).cal.poly[0:1]
	 
	np = n_elements(p)
	for ip=0,np-1 do begin
	    if (*p[ip]).show and ((*p[ip]).size gt 0) then begin
			pspec = p[ip]
			str = strsplit( (*pspec).label, ' ', /extract)
			if first then begin
				lab = str[0]
				ls = strlen(lab)
				if ls gt 20 then lab = '...' + strmid(lab,ls-20,20)
				first = 0
			endif
			nl = lenchr((*pspec).label)
			label = extract( (*pspec).label, nl-2, nl-1)
			do_energy = (label eq '/E')
			if do_energy then begin
				adc = (*pspec).station + adc_offset_device((*pspec).DevObj)
				if adc eq 133 then begin
					print,'debug ...'
				endif
				ns = (*pspec).size
				data = float( *(*pspec).data)
				channel = findgen(ns)
				
				if use_x then begin										; X: X0-X5     markers
					xmark = ( round(marks[0:5,1])	< (ns-1)) > 0
					x = channel[xmark[2]:xmark[3]]
	
					left = total(data[xmark[0]:xmark[1]])
					dleft = xmark[1]-xmark[0]+1
					left = left / dleft
					xleft = 0.5*(xmark[1]+xmark[0])
					right = total(data[xmark[4]:xmark[5]])
					dright = xmark[5]-xmark[4]+1
					right = right / dright
					xright = 0.5*(xmark[5]+xmark[4])
					ba = (right-left)/(xright-xleft)
					bb = left - ba*xleft
	
					back = bb + ba*channel
					data = (data - back) 		;	> 0.0	; no clip negatives?
				endif else begin										; View: V0-V1     markers
	;				view = ( round(marks[0:1,3])	< (ns-1)) > 0
					view = ( round((eview-(*pspec).cal.poly[0])/(*pspec).cal.poly[1])	< (ns-1)) > 0
					x = channel[view[0]:view[1]]
				endelse

;				Need to filter 'x' to the immediate confines of the pulser peak.
;				Perhaps use first occurence of a negative weight 'data[x]' or max/20 to limit 'x'

				nx = n_elements(x)
				top = max(data[x])
				if top le 0. then continue
				q1 = where( data[x] gt top/2.)
				centre = centroid( x[q1], weight=data[x[q1]], error=err)
				test = (data[x] gt 0.) and (data[x] gt top/20.)

;				Find first zero above and below centre ...

				qup = where( (test eq 0) and (x gt centre), nup)
				qdown = where( (test eq 0) and (x lt centre), ndown)
				if nup eq 0 then qup=nx-1
				if ndown eq 0 then begin
					qdown = 0
					ndown = 1
				endif
				itop = qup[0]
				ibot = qdown[ndown-1]

				ave = centroid( x[ibot:itop], weight=data[x[ibot:itop]], variance=var, error=err)
				if ((adc ge 0) and (adc lt 384)) then fwhm[adc] = ca * 2.32* sqrt(var) * 1000.
			endif
		endif
	endfor

	window,0, xsize=500, ysize=500, retain=retain
	title = 'Pulser Noise FWHM Map (eV) ('+lab+')'
	id = indgen(384)
	plot_maia_parameter, id, fwhm, title=title, min=ymin,max=ymax, /white, /screen, layout=layout

	window,1, xsize=500, ysize=500, retain=retain
	h = histogram( fwhm, min=ymin, max=ymax, omin=omin,omax=omax, binsize=10, locations=x)
	x = x+5.
	
	!p.color = spec_colour('black')
	!p.background = spec_colour('white')
	!p.title = 'Pulser FWHM Histogram ('+lab+')'
	!x.title = 'Resolution (eV)'
	!y.title = 'Frequency'
	erase
	plot, x,h, xrange=[omin-10,omax+10], /nodata
	oplot, x,h, color=spec_colour('green'), thick=2.

	av = total( x*h) / total(h)
	q3 = reverse(sort(h))
	mode = x[q3[0]]	
	xyouts,0.90,0.87, 'Average FWHM (eV) = '+str_tidy(av,places=1), /norm,align=1, charsize=1.1
	xyouts,0.90,0.81, 'Mode FWHM (eV) = '+str_tidy(mode), /norm,align=1, charsize=1.1

	return
end

