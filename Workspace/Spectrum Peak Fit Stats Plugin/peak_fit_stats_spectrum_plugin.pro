;
;  Peak Fit Stats Spectrum plugin routine
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

pro Peak_Fit_Stats_spectrum_plugin, p, ii, marks, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Fit file Peak FWHM Plugin'	; return the menu title for this plugin
	return									; * indicates that it accepts all displayed spectra
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

	file = ['Fit Results file','Layout file']
	initial_file = ['','Maia_384C.csv']
	help_file = ['Select filename for Fit Results PFR file.','Maia detector pad layout file (e.g. "Maia_384C.csv").']
	text = ['FWHM Cut-off (eV)','FWHM plot min (eV)','FWHM plot max (eV)', 'FWHM median test energy (eV)']
	initial_text = ['2000.','200.','600.','270.']
	help_text = ['Cut-off value for FWHM (eV). Use this to exclude channels with FWHM above this value.','Minimum FWHM (eV) for colour map.', $
			'Maximum FWHM (eV) for colour map.','Median energy to test FWHM distributioin against.']
	Help_default = 'Select Fit Results file to plot a peak fit FWHM map.'
	r = options_popup( title='Fit Results FWHM', text=text, initial_text=initial_text, help_text=help_text, $
				help_default=help_default, file=file, initial_file=initial_file, filter=['*.pfr','*.csv'], help_file=help_file, min_xsize=300, error=error)
	if error then return
	rfile = r.file[0]
	layout = r.file[1]
	if rfile eq '' then return
	threshold = float2(r.text[0])			; threshold FWHM cut-off
	ymin = float2(r.text[1])				; value for colour map min
	ymax = float2(r.text[2])				; value for colour map max
	etest = float2(r.text[3])				; median test energy
	
	pstate = ptr_new( {presults:ptr_new(/allocate_heap)} )
	presults = read_fit_results( rfile, error=error)
	if error then return

;	This looks at the 'label' on the spectrum. It could also use 'spectrum_index' number return.

	np = n_elements( presults)
	if np lt 1 then return
	id = intarr(np)
	fwhm = fltarr(np)
	first = 1
	for i=0,np-1 do begin
		spectrum = (*(presults)[i]).spectrum
		FWHM_Mn = 1000.*sqrt(abs( spectrum.FWHM.w1 * 5.898 + spectrum.FWHM.w0 ))
		fwhm[i] = FWHM_Mn
		id[i] = -1
		str = strsplit( spectrum.label, ' ', /extract)
		if first then begin
			lab = str[0]
			ls = strlen(lab)
			if ls gt 20 then lab = '...' + strmid(lab,ls-20,20)
			first = 0
		endif
		for j=0,n_elements(str)-1 do begin
			if strmid( str[j], 0,1) eq '#' then begin
				id[i] = long2( strmid(str[j],1))
				continue
			endif
			nj = strlen(str[j])
			if strmid( str[j], nj-2,2) eq '/E' then begin
				s = strmid(str[j],0,nj-2)
				if inumeric( s) then id[i] = long2( s)
				continue
			endif		
		endfor
		if strmid( str[0], 0,8) eq 'Detector' then begin
			id[i] = long2( str[1])
		endif
	endfor
	q = where( (id ge 0) and (fwhm lt threshold), nq)
	if nq eq 0 then return
	
	id = id[q]
	fwhm = fwhm[q]
	
	window,1, xsize=500, ysize=500, retain=retain
	h = histogram( fwhm, min=ymin, max=ymax, omin=omin,omax=omax, binsize=10, locations=x)
	x = x+5.
	
	!p.color = spec_colour('black')
	!p.background = spec_colour('white')
	!p.title = 'Mn Ka Resolution Histogram ('+lab+')'
	!x.title = 'Resolution (eV)'
	!p.thick = 1.0
	!p.charthick = 1.0
	erase
	plot, x,h, xrange=[omin-10,omax+10], /nodata
	oplot, x,h, color=spec_colour('green'), thick=2.

	av = total( x*h) / total(h)
	q3 = reverse(sort(h))
	mode = x[q3[0]]	
	xyouts,0.90,0.87, 'Average FWHM (eV) = '+str_tidy(av,places=1), /norm,align=1, charsize=1.1
	xyouts,0.90,0.81, 'Mode FWHM (eV) = '+str_tidy(mode), /norm,align=1, charsize=1.1

	dlayout = read_detector_layout( layout, maia=ismaia, error=err)
	if err then begin
		warning,'Peak_Fit_Stats_spectrum_plugin','Bad detector layout.'
		return
	endif

	q2 = where( x lt etest, nq2)
	if nq2 gt 0 then begin
		frac = 100. * total( h[q2])
		frac2 = frac / dlayout.n
		frac = frac / total(h)

		xyouts,0.90,0.75, 'Fraction (live) below '+str_tidy(etest,places=0)+' eV = '+str_tidy(frac,places=1)+' %', /norm,align=1, charsize=1.1
		xyouts,0.90,0.69, 'Fraction (all) below '+str_tidy(etest,places=0)+' eV = '+str_tidy(frac2,places=1)+' %', /norm,align=1, charsize=1.1
	endif else frac = 0.
	
fin:
	title = 'Fitted Mn Peak FWHM (eV) ('+lab+')'
	plot_maia_parameter, id, fwhm, title=title, min=ymin,max=ymax, /white, /screen, layout=layout
	return
end

