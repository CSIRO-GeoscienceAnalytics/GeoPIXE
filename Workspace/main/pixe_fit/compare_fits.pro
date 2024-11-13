pro compare_fits, files, output, error=err, bad=bad

; Compare two fits PFR files for consistency. Use tol=0.001 for model value tests
; and 0.03 for fitted areas, conc, etc.
;
; 'files' are input PFR file (2) names to compare. Prompts for these if missing.
; 'output' is a txt file report to create.
; 'err=1'	error reading yield files, or some crash.
; 'bad=1'	an input parameter difference found.

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	common c_errors_1, catch_errors_on
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'compare_fits',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			err = 1
			return
		endif
	endif

; Use tol=0.001 for model value tests
; and 0.03 for fitted areas, conc, etc.

	tol_model = 0.001
	tol_fits = 0.03
	
	err = 1
	if n_elements(files) eq 0 then begin
		fold = file_requester( /read, filter='*.pfr', /fix_filter, title='Reference PFR fit file.')
		if fold eq '' then return
	endif else fold=files[0]
	if n_elements(files) lt 2 then begin
		path = extract_path(fold)
		fnew = file_requester( /read, path=path, filter='*.pfr', /fix_filter, title='New PFR fit file to check.')
		if fnew eq '' then return
	endif else fnew=files[1]
	
	new = read_fit_results( fnew, error=err)
	if err then begin
		warning, 'compare_fits','Failed to read NEW file: '+fnew
		return
	endif
	old = read_fit_results( fold, error=err)
	if err then begin
		warning, 'compare_fits','Failed to read REFERENCE file: '+fold
		return
	endif
	if n_elements(output) eq 0 then begin
		output = strip_file_ext( fnew) + '.txt'
	endif
	
	err = 0
	on_ioerror, bad_open
	openw, lun, output, /get_lun
	on_ioerror, bad_write
	
	nnew = n_elements(new)
	nold = n_elements(old)
	if nnew ne nold then begin
		printf, lun,'	Number of rows changed (Ref=',nnew,'    new=',nold,').'
		bad = 1
	endif
	nrows = nnew < nold
	
  for k=0,nrows-1 do begin 		;==========================================
	
	printf, lun,'======================================================================================='
	printf, lun,'Setup parameters (results row ',k,') ...'
	bad = 0
	if (*new[k]).org ne (*old[k]).org then begin
		printf, lun,'	ORG changed (Ref=',(*old[k]).org,'    new=',(*new[k]).org,').'
		bad = 1
	endif
	if (*new[k]).rorg ne (*old[k]).rorg then begin
		printf, lun,'	RORG changed (Ref=',(*old[k]).rorg,'    new=',(*new[k]).rorg,').'
		bad = 1
	endif
	if (*new[k]).scale ne (*old[k]).scale then begin
		printf, lun,'	scale changed (Ref=',(*old[k]).scale,'    new=',(*new[k]).scale,').'
		bad = 1
	endif
	if (*new[k]).mode ne (*old[k]).mode then begin
		printf, lun,'	mode changed (Ref=',(*old[k]).mode,'    new=',(*new[k]).mode,').'
		bad = 1
	endif
	if (*new[k]).type ne (*old[k]).type then begin
		printf, lun,'	type changed (Ref=',(*old[k]).type,'    new=',(*new[k]).type,').'
		bad = 1
	endif
	if (*new[k]).tweek.el ne -1 then begin
		printf, lun,'	New "tweek" active. Warning *****.'
		bad = 1
	endif
	if (*old[k]).tweek.el ne -1 then begin
		printf, lun,'	Ref "tweek" active. Warning *****.'
		bad = 1
	endif
	
	if (*new[k]).n_els ne (*old[k]).n_els then begin
		printf, lun,'	n_els changed (Ref=',(*old[k]).n_els,'    new=',(*new[k]).n_els,').'
		bad = 1
	endif
	used = intarr(100,4)
	used[ (*old[k]).el.z, (*old[k]).el.shell] = 1
	q = where( used[ (*new[k]).el.z, (*new[k]).el.shell] eq 0, nq)
	if nq ge 1 then begin
		for j=0,nq-1 do begin
			q_to_xy, q, 100, z, shell
			printf, lun,'	New: element added (Z=',z[j],'    shell=',shell[j],'    name=',element_name(z[j]),').'
		endfor
		bad = 1
	endif
	used[*] = 0
	used[ (*new[k]).el.z, (*new[k]).el.shell] = 1
	q = where( used[ (*old[k]).el.z, (*old[k]).el.shell] eq 0, nq)
	if nq ge 1 then begin
		for j=0,nq-1 do begin
			q_to_xy, q, 100, z, shell
			printf, lun,'	Ref: element not used (Z=',z[j],'    shell=',shell[j],'    name=',element_name(z[j]),').'
		endfor
		bad = 1
	endif
	
	if (*new[k]).setup.elow ne (*old[k]).setup.elow then begin
		printf, lun,'	setup.elow changed (Ref=',(*old[k]).setup.elow,'    new=',(*new[k]).setup.elow,').'
		bad = 1
	endif
	if (*new[k]).setup.ehigh ne (*old[k]).setup.ehigh then begin
		printf, lun,'	setup.ehigh changed (Ref=',(*old[k]).setup.ehigh,'    new=',(*new[k]).setup.ehigh,').'
		bad = 1
	endif
	if strip_path((*new[k]).setup.pcm) ne strip_path((*old[k]).setup.pcm) then begin
		printf, lun,'	setup.pcm changed (Ref=',(*old[k]).setup.pcm,'    new=',(*new[k]).setup.pcm,').'
		bad = 1
	endif
	
	if (*new[k]).nlinear.free.cal ne (*old[k]).nlinear.free.cal then begin
		printf, lun,'	nlinear.free.cal changed (Ref=',(*old[k]).nlinear.free.cal,'    new=',(*new[k]).nlinear.free.cal,').'
		bad = 1
	endif
	if (*new[k]).nlinear.free.fwhm ne (*old[k]).nlinear.free.fwhm then begin
		printf, lun,'	nlinear.free.fwhm changed (Ref=',(*old[k]).nlinear.free.fwhm,'    new=',(*new[k]).nlinear.free.fwhm,').'
		bad = 1
	endif
	if (*new[k]).nlinear.free.fano ne (*old[k]).nlinear.free.fano then begin
		printf, lun,'	nlinear.free.fano changed (Ref=',(*old[k]).nlinear.free.fano,'    new=',(*new[k]).nlinear.free.fano,').'
		bad = 1
	endif
	if (*new[k]).nlinear.free.tail ne (*old[k]).nlinear.free.tail then begin
		printf, lun,'	nlinear.free.tail changed (Ref=',(*old[k]).nlinear.free.tail,'    new=',(*new[k]).nlinear.free.tail,').'
		bad = 1
	endif
	if (*new[k]).nlinear.no_tail ne (*old[k]).nlinear.no_tail then begin
		printf, lun,'	nlinear.no_tail changed (Ref=',(*old[k]).nlinear.no_tail,'    new=',(*new[k]).nlinear.no_tail,').'
		bad = 1
	endif
	
	if (*new[k]).yield.z1 eq 0 then begin
		if (*new[k]).compton.tail.amp ne (*old[k]).compton.tail.amp then begin
			printf, lun,'	compton.taila.amp changed (Ref=',(*old[k]).compton.tail.amp,'    new=',(*new[k]).compton.tail.amp,').'
			bad = 1
		endif
		if (*new[k]).compton.tail.len ne (*old[k]).compton.tail.len then begin
			printf, lun,'	compton.tail.len changed (Ref=',(*old[k]).compton.tail.len,'    new=',(*new[k]).compton.tail.len,').'
			bad = 1
		endif
		if (*new[k]).compton.spread ne (*old[k]).compton.spread then begin
			printf, lun,'	compton.spread changed (Ref=',(*old[k]).compton.spread,'    new=',(*new[k]).compton.spread,').'
			bad = 1
		endif
		if (*new[k]).compton.shift ne (*old[k]).compton.shift then begin
			printf, lun,'	compton.shift changed (Ref=',(*old[k]).compton.shift,'    new=',(*new[k]).compton.shift,').'
			bad = 1
		endif
	endif

	if strip_path((*new[k]).cuts.file) ne strip_path((*old[k]).cuts.file) then begin
		printf, lun,'	cuts.file changed (Ref=',(*old[k]).cuts.file,'    new=',(*new[k]).cuts.file,').'
		bad = 1
	endif
	if strip_path((*new[k]).detector.file) ne strip_path((*old[k]).detector.file) then begin
		printf, lun,'	detector.file changed (Ref=',(*old[k]).detector.file,'    new=',(*new[k]).detector.file,').'
		bad = 1
	endif
	if strip_path((*new[k]).filter.file) ne strip_path((*old[k]).filter.file) then begin
		printf, lun,'	filter.file changed (Ref=',(*old[k]).filter.file,'    new=',(*new[k]).filter.file,').'
		bad = 1
	endif
	if strip_path((*new[k]).spectrum.file) ne strip_path((*old[k]).spectrum.file) then begin
		printf, lun,'	spectrum.file changed (Ref=',(*old[k]).spectrum.file,'    new=',(*new[k]).spectrum.file,').'
		bad = 1
	endif
	if strip_path((*new[k]).spectrum.label) ne strip_path((*old[k]).spectrum.label) then begin
		printf, lun,'	spectrum.label changed (Ref=',(*old[k]).spectrum.label,'    new=',(*new[k]).spectrum.label,').'
		bad = 1
	endif

	if (*new[k]).spectrum.charge ne (*old[k]).spectrum.charge then begin
		printf, lun,'	spectrum.charge changed (Ref=',(*old[k]).spectrum.charge,'    new=',(*new[k]).spectrum.charge,').'
		bad = 1
	endif
	if (*new[k]).flux ne (*old[k]).flux then begin
		printf, lun,'	flux changed (Ref=',(*old[k]).flux,'    new=',(*new[k]).flux,').'
		bad = 1
	endif

	if (*new[k]).array.on ne (*old[k]).array.on then begin
		printf, lun,'	array.on changed (Ref=',(*old[k]).array.on,'    new=',(*new[k]).array.on,').'
		bad = 1
	endif
	if ((*new[k]).array.on eq 1) and ((*old[k]).array.on eq 1) then begin
		if (*new[k]).array.n_det ne (*old[k]).array.n_det then begin
			printf, lun,'	array.n_det changed (Ref=',(*old[k]).array.n_det,'    new=',(*new[k]).array.n_det,').'
			bad = 1
		endif
		used = intarr(1000)
		used[ (*old[k]).array.active] = 1
		q = where( used[ (*new[k]).array.active] eq 0, nq)
		if nq ge 1 then begin
			for j=0,nq-1 do begin
				printf, lun,'	New: detector added (det=',q[j],').'
			endfor
			bad = 1
		endif
		used[*] = 0
		used[ (*new[k]).array.active] = 1
		q = where( used[ (*old[k]).array.active] eq 0, nq)
		if nq ge 1 then begin
			for j=0,nq-1 do begin
				printf, lun,'	Ref: detector not used (det=',q[j],').'
			endfor
			bad = 1
		endif
		
;		Need to print changes in rgamma, cintensity

		x = fltarr( (*new[k]).array.n_det, (*new[k]).n_els)
		y = fltarr( (*new[k]).array.n_det, (*new[k]).n_els)
		z = (*new[k]).el.z
		shell = (*new[k]).el.shell
		name = (*new[k]).el.name
		for i=0,(*new[k]).n_els-1 do begin
			q = where( ((*old[k]).el.z eq (*new[k]).el.z[i]) and ((*old[k]).el.shell eq (*new[k]).el.shell[i]) $
							and ((*new[k]).el.name ne 'Compton') and ((*new[k]).el.name ne 'elastic') and ((*new[k]).el.name ne 'sum'), nq)
			if nq ge 1 then begin
				x[*,i] = (*old[k]).array.rgamma[*,q[0]]
				y[*,i] = (*new[k]).array.rgamma[*,i]
			endif
		endfor
		sig = sig_change( x, y, tol=tol_model, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			q_to_xy, q, (*new[k]).array.n_det, id,iz
			printf, lun,'rGamma not consistent ...'
			printf, lun,'  Index    Z    Name  Shell  Det       Ref        New'
			for j=0,nq-1 do begin
				printf, lun, q[j], z[iz[j]], name[iz[j]], shell[iz[j]], id[j], x[id[j],iz[j]], y[id[j],iz[j]], $
					format='(I6,I6,3x,A4,I6,I7,2x,G11.4,G11.4)'
			endfor
		endif else begin
			printf, lun,'rGamma all consistent.'
		endelse
	endif
	
	if (*new[k]).background.mode ne (*old[k]).background.mode then begin
		printf, lun,'	background.mode changed (Ref=',(*old[k]).background.mode,'    new=',(*new[k]).background.mode,').'
		bad = 1
	endif
	if (*new[k]).background.scale ne (*old[k]).background.scale then begin
		printf, lun,'	background.scale changed (Ref=',(*old[k]).background.scale,'    new=',(*new[k]).background.scale,').'
		bad = 1
	endif
	if (*new[k]).back_split.mode ne (*old[k]).back_split.mode then begin
		printf, lun,'	back_split.mode changed (Ref=',(*old[k]).back_split.mode,'    new=',(*new[k]).back_split.mode,').'
		bad = 1
	endif
	if (*new[k]).back_split.emid ne (*old[k]).back_split.emid then begin
		printf, lun,'	back_split.emid changed (Ref=',(*old[k]).back_split.emid,'    new=',(*new[k]).back_split.emid,').'
		bad = 1
	endif
	if (*new[k]).back_split.scale ne (*old[k]).back_split.scale then begin
		printf, lun,'	back_split.scale changed (Ref=',(*old[k]).back_split.scale,'    new=',(*new[k]).back_split.scale,').'
		bad = 1
	endif

	compare_yields, [ ptr_new((*old[k]).yield), ptr_new((*new[k]).yield)], /basic, el=[ptr_new((*old[k]).el.name), ptr_new((*new[k]).el.name)], $
						error=err, bad=bad1, lun=lun, tol=tol_model
	bad = bad or bad1

	if (*new[k]).yield.unknown ne (*old[k]).yield.unknown then begin
		printf, lun,'	yield.unknown changed (Ref=',(*old[k]).yield.unknown,'    new=',(*new[k]).yield.unknown,').'
		bad = 1
	endif

	if (*new[k]).beam.continuum ne 0 then begin
		case (*new[k]).beam.model of
			1: begin
				compare_source, [(*old[k]).beam, (*new[k]).beam], error=err, bad=bad1, lun=lun, tol=tol_model
				bad = bad or bad1
				end
			2: begin
				compare_pink, [(*old[k]).beam, (*new[k]).beam], error=err, bad=bad1, lun=lun, tol=tol_model
				bad = bad or bad1
				end
			else: begin
				printf, lun,'	Unknown beam continuum model (new=',(*new[k]).beam.model,').'
				end
		endcase
	endif

	if (*new[k]).inclusion.type ne (*old[k]).inclusion.type then begin
		printf, lun,'	inclusion.type changed (Ref=',(*old[k]).inclusion.type,'    new=',(*new[k]).inclusion.type,').'
		bad = 1
	endif

;	Need to check inclusion parameters and norm's across elements (list may have changed?)

	x = fltarr( (*new[k]).n_els)
	y = fltarr( (*new[k]).n_els)
	for i=0,(*new[k]).n_els-1 do begin
		q = where( ((*old[k]).el.z eq (*new[k]).el.z[i]) and ((*old[k]).el.shell eq (*new[k]).el.shell[i]) $
					and ((*new[k]).el.name ne 'Compton') and ((*new[k]).el.name ne 'elastic') and ((*new[k]).el.name ne 'sum'), nq)
		if nq ge 1 then begin
			x[i] = (*old[k]).counts_per_ppm_uc[q[0]]
			y[i] = (*new[k]).counts_per_ppm_uc[i]
		endif else if ((*new[k]).el.name[i] ne 'Compton') and ((*new[k]).el.name[i] ne 'elastic') and ((*new[k]).el.name[i] ne 'sum') then begin
;			warning,'compare_yields','Element Z='+str_tidy((*new[k]).el.z[i])+', shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new[k]).el.z[i])+' ('+(*new[k]).el.name[i]+'), shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
		endif
	endfor
	sig = sig_change( x, y, tol=tol_model, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'counts_per_ppm_uc not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new[k]).el.z[q[j]], (*new[k]).el.name[q[j]], (*new[k]).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'counts_per_ppm_uc all consistent.'
	endelse

	if bad then begin
;		return
	endif else begin
		printf, lun,'Fit Parameters consistent.'
	endelse

	printf, lun,'-------------------------------------------------------------------------------------'

	if sig_change( (*old[k]).fit.rms, (*new[k]).fit.rms, tol=tol_fits, error=err1) then begin
		printf, lun,'fit.rms not consistent: ref=',(*old[k]).fit.rms, ' new=',(*new[k]).fit.rms
	endif
	if sig_change( (*old[k]).spectrum.cal.a, (*new[k]).spectrum.cal.a, tol=tol_fits, error=err1) then begin
		printf, lun,'spectrum.cal.a not consistent: ref=',(*old[k]).spectrum.cal.a, ' new=',(*new[k]).spectrum.cal.a
	endif
	if sig_change( (*old[k]).spectrum.cal.b, (*new[k]).spectrum.cal.b, tol=tol_fits, error=err1) then begin
		printf, lun,'spectrum.cal.b not consistent: ref=',(*old[k]).spectrum.cal.b, ' new=',(*new[k]).spectrum.cal.b
	endif
	if sig_change( (*old[k]).spectrum.fwhm.w0, (*new[k]).spectrum.fwhm.w0, tol=tol_fits, error=err1) then begin
		printf, lun,'spectrum.fwhm.w0 not consistent: ref=',(*old[k]).spectrum.fwhm.w0, ' new=',(*new[k]).spectrum.fwhm.w0
	endif
	if sig_change( (*old[k]).spectrum.fwhm.w1, (*new[k]).spectrum.fwhm.w1, tol=tol_fits, error=err1) then begin
		printf, lun,'spectrum.fwhm.w1 not consistent: ref=',(*old[k]).spectrum.fwhm.w1, ' new=',(*new[k]).spectrum.fwhm.w1
	endif
	if sig_change( (*old[k]).nlinear.pileup, (*new[k]).nlinear.pileup, tol=tol_fits, error=err1) then begin
		printf, lun,'spectrum.nlinear.pileup not consistent: ref=',(*old[k]).nlinear.pileup, ' new=',(*new[k]).nlinear.pileup
	endif

	x = fltarr( (*new[k]).n_els)
	y = fltarr( (*new[k]).n_els)
	for i=0,(*new[k]).n_els-1 do begin
		q = where( ((*old[k]).el.z eq (*new[k]).el.z[i]) and ((*old[k]).el.shell eq (*new[k]).el.shell[i])$
					and ((*new[k]).el.name ne 'Compton') and ((*new[k]).el.name ne 'elastic') and ((*new[k]).el.name ne 'sum'), nq)
		if nq ge 1 then begin
			x[i] = (*old[k]).conc[q[0]]
			y[i] = (*new[k]).conc[i]
		endif else if ((*new[k]).el.name[i] ne 'Compton') and ((*new[k]).el.name[i] ne 'elastic') and ((*new[k]).el.name[i] ne 'sum') then begin
;			warning,'compare_yields','Element Z='+str_tidy((*new[k]).el.z[i])+', shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new[k]).el.z[i])+' ('+(*new[k]).el.name[i]+'), shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
		endif
	endfor
	sig = sig_change( x, y, tol=tol_fits, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'Conc not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new[k]).el.z[q[j]], (*new[k]).el.name[q[j]], (*new[k]).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'Conc all consistent.'
	endelse

	x = fltarr( (*new[k]).n_els)
	y = fltarr( (*new[k]).n_els)
	for i=0,(*new[k]).n_els-1 do begin
		q = where( ((*old[k]).el.z eq (*new[k]).el.z[i]) and ((*old[k]).el.shell eq (*new[k]).el.shell[i])$
						and ((*new[k]).el.name ne 'Compton') and ((*new[k]).el.name ne 'elastic') and ((*new[k]).el.name ne 'sum'), nq)
		if nq ge 1 then begin
			x[i] = (*old[k]).error[q[0]]
			y[i] = (*new[k]).error[i]
		endif else if ((*new[k]).el.name[i] ne 'Compton') and ((*new[k]).el.name[i] ne 'elastic') and ((*new[k]).el.name[i] ne 'sum') then begin
			;			warning,'compare_yields','Element Z='+str_tidy((*new[k]).el.z[i])+', shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new[k]).el.z[i])+' ('+(*new[k]).el.name[i]+'), shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
		endif
	endfor
	sig = sig_change( x, y, tol=tol_fits, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'Error not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new[k]).el.z[q[j]], (*new[k]).el.name[q[j]], (*new[k]).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'Error all consistent.'
	endelse

	x = fltarr( (*new[k]).n_els)
	y = fltarr( (*new[k]).n_els)
	for i=0,(*new[k]).n_els-1 do begin
		q = where( ((*old[k]).el.z eq (*new[k]).el.z[i]) and ((*old[k]).el.shell eq (*new[k]).el.shell[i])$
						and ((*new[k]).el.name ne 'Compton') and ((*new[k]).el.name ne 'elastic') and ((*new[k]).el.name ne 'sum'), nq)
		if nq ge 1 then begin
			x[i] = (*old[k]).mdl[q[0]]
			y[i] = (*new[k]).mdl[i]
		endif else if ((*new[k]).el.name[i] ne 'Compton') and ((*new[k]).el.name[i] ne 'elastic') and ((*new[k]).el.name[i] ne 'sum') then begin
			;			warning,'compare_yields','Element Z='+str_tidy((*new[k]).el.z[i])+', shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new[k]).el.z[i])+' ('+(*new[k]).el.name[i]+'), shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
		endif
	endfor
	sig = sig_change( x, y, tol=tol_fits, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'MDL not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new[k]).el.z[q[j]], (*new[k]).el.name[q[j]], (*new[k]).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'MDL all consistent.'
	endelse

	x = fltarr( (*new[k]).n_els)
	y = fltarr( (*new[k]).n_els)
	for i=0,(*new[k]).n_els-1 do begin
		q = where( ((*old[k]).el.z eq (*new[k]).el.z[i]) and ((*old[k]).el.shell eq (*new[k]).el.shell[i])$
						and ((*new[k]).el.name ne 'Compton') and ((*new[k]).el.name ne 'elastic') and ((*new[k]).el.name ne 'sum'), nq)
		if nq ge 1 then begin
			x[i] = (*old[k]).area[q[0]]
			y[i] = (*new[k]).area[i]
		endif else if ((*new[k]).el.name[i] ne 'Compton') and ((*new[k]).el.name[i] ne 'elastic') and ((*new[k]).el.name[i] ne 'sum') then begin
			;			warning,'compare_yields','Element Z='+str_tidy((*new[k]).el.z[i])+', shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new[k]).el.z[i])+' ('+(*new[k]).el.name[i]+'), shell='+str_tidy((*new[k]).el.shell[i])+' not found in reference.'
		endif
	endfor
	sig = sig_change( x, y, tol=tol_fits, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'Area not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new[k]).el.z[q[j]], (*new[k]).el.name[q[j]], (*new[k]).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'Area all consistent.'
	endelse

  endfor					;==================================================
	
	err = 0
	printf, lun,'All done.'
	printf, lun,'-------------------------------------------------------------------------------------'
	close_file, lun
	return
	
bad_open:
	printf, lun,'compare_fits: Error opening output file: '+output
	err = 1
	return
bad_write:
	printf, lun,'compare_fits: Error writing to output file: '+output
	close_file, lun
	err = 1
	return
end

	