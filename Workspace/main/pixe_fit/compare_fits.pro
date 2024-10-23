pro compare_fits, files, output, error=err, bad=bad

; Compare two fits PFR files for consistency.
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
	
	printf, lun,'-------------------------------------------------------------------------------------'
	printf, lun,'Setup parameters ...'
	bad = 0
	if (*new).org ne (*old).org then begin
		printf, lun,'	ORG changed (Ref=',(*old).org,'    new=',(*new).org,').'
		bad = 1
	endif
	if (*new).rorg ne (*old).rorg then begin
		printf, lun,'	RORG changed (Ref=',(*old).rorg,'    new=',(*new).rorg,').'
		bad = 1
	endif
	if (*new).scale ne (*old).scale then begin
		printf, lun,'	scale changed (Ref=',(*old).scale,'    new=',(*new).scale,').'
		bad = 1
	endif
	if (*new).mode ne (*old).mode then begin
		printf, lun,'	mode changed (Ref=',(*old).mode,'    new=',(*new).mode,').'
		bad = 1
	endif
	if (*new).type ne (*old).type then begin
		printf, lun,'	type changed (Ref=',(*old).type,'    new=',(*new).type,').'
		bad = 1
	endif
	if (*new).tweek.el ne -1 then begin
		printf, lun,'	New "tweek" active. Warning.'
		bad = 1
	endif
	if (*old).tweek.el ne -1 then begin
		printf, lun,'	Ref "tweek" active. Warning.'
		bad = 1
	endif
	
	if (*new).n_els ne (*old).n_els then begin
		printf, lun,'	n_els changed (Ref=',(*old).n_els,'    new=',(*new).n_els,').'
		bad = 1
	endif
	used = intarr(100,4)
	used[ (*old).el.z, (*old).el.shell] = 1
	q = where( used[ (*new).el.z, (*new).el.shell] eq 0, nq)
	if nq ge 1 then begin
		for j=0,nq-1 do begin
			q_to_xy, q, 100, z, shell
			printf, lun,'	New: element added (Z=',z[j],'    shell=',shell[j],'    name=',element_name(z[j]),').'
		endfor
		bad = 1
	endif
	used[*] = 0
	used[ (*new).el.z, (*new).el.shell] = 1
	q = where( used[ (*old).el.z, (*old).el.shell] eq 0, nq)
	if nq ge 1 then begin
		for j=0,nq-1 do begin
			q_to_xy, q, 100, z, shell
			printf, lun,'	Ref: element not used (Z=',z[j],'    shell=',shell[j],'    name=',element_name(z[j]),').'
		endfor
		bad = 1
	endif
	
	if (*new).setup.elow ne (*old).setup.elow then begin
		printf, lun,'	setup.elow changed (Ref=',(*old).setup.elow,'    new=',(*new).setup.elow,').'
		bad = 1
	endif
	if (*new).setup.ehigh ne (*old).setup.ehigh then begin
		printf, lun,'	setup.ehigh changed (Ref=',(*old).setup.ehigh,'    new=',(*new).setup.ehigh,').'
		bad = 1
	endif
	if strip_path((*new).setup.pcm) ne strip_path((*old).setup.pcm) then begin
		printf, lun,'	setup.pcm changed (Ref=',(*old).setup.pcm,'    new=',(*new).setup.pcm,').'
		bad = 1
	endif
	
	if (*new).nlinear.free.cal ne (*old).nlinear.free.cal then begin
		printf, lun,'	nlinear.free.cal changed (Ref=',(*old).nlinear.free.cal,'    new=',(*new).nlinear.free.cal,').'
		bad = 1
	endif
	if (*new).nlinear.free.fwhm ne (*old).nlinear.free.fwhm then begin
		printf, lun,'	nlinear.free.fwhm changed (Ref=',(*old).nlinear.free.fwhm,'    new=',(*new).nlinear.free.fwhm,').'
		bad = 1
	endif
	if (*new).nlinear.free.fano ne (*old).nlinear.free.fano then begin
		printf, lun,'	nlinear.free.fano changed (Ref=',(*old).nlinear.free.fano,'    new=',(*new).nlinear.free.fano,').'
		bad = 1
	endif
	if (*new).nlinear.free.tail ne (*old).nlinear.free.tail then begin
		printf, lun,'	nlinear.free.tail changed (Ref=',(*old).nlinear.free.tail,'    new=',(*new).nlinear.free.tail,').'
		bad = 1
	endif
	if (*new).nlinear.no_tail ne (*old).nlinear.no_tail then begin
		printf, lun,'	nlinear.no_tail changed (Ref=',(*old).nlinear.no_tail,'    new=',(*new).nlinear.no_tail,').'
		bad = 1
	endif
	
	if (*new).compton.tail.amp ne (*old).compton.tail.amp then begin
		printf, lun,'	compton.taila.amp changed (Ref=',(*old).compton.tail.amp,'    new=',(*new).compton.tail.amp,').'
		bad = 1
	endif
	if (*new).compton.tail.len ne (*old).compton.tail.len then begin
		printf, lun,'	compton.tail.len changed (Ref=',(*old).compton.tail.len,'    new=',(*new).compton.tail.len,').'
		bad = 1
	endif
	if (*new).compton.spread ne (*old).compton.spread then begin
		printf, lun,'	compton.spread changed (Ref=',(*old).compton.spread,'    new=',(*new).compton.spread,').'
		bad = 1
	endif
	if (*new).compton.shift ne (*old).compton.shift then begin
		printf, lun,'	compton.shift changed (Ref=',(*old).compton.shift,'    new=',(*new).compton.shift,').'
		bad = 1
	endif

	if strip_path((*new).cuts.file) ne strip_path((*old).cuts.file) then begin
		printf, lun,'	cuts.file changed (Ref=',(*old).cuts.file,'    new=',(*new).cuts.file,').'
		bad = 1
	endif
	if strip_path((*new).detector.file) ne strip_path((*old).detector.file) then begin
		printf, lun,'	detector.file changed (Ref=',(*old).detector.file,'    new=',(*new).detector.file,').'
		bad = 1
	endif
	if strip_path((*new).filter.file) ne strip_path((*old).filter.file) then begin
		printf, lun,'	filter.file changed (Ref=',(*old).filter.file,'    new=',(*new).filter.file,').'
		bad = 1
	endif
	if strip_path((*new).spectrum.file) ne strip_path((*old).spectrum.file) then begin
		printf, lun,'	spectrum.file changed (Ref=',(*old).spectrum.file,'    new=',(*new).spectrum.file,').'
		bad = 1
	endif
	if strip_path((*new).spectrum.label) ne strip_path((*old).spectrum.label) then begin
		printf, lun,'	spectrum.label changed (Ref=',(*old).spectrum.label,'    new=',(*new).spectrum.label,').'
		bad = 1
	endif

	if (*new).spectrum.charge ne (*old).spectrum.charge then begin
		printf, lun,'	spectrum.charge changed (Ref=',(*old).spectrum.charge,'    new=',(*new).spectrum.charge,').'
		bad = 1
	endif
	if (*new).flux ne (*old).flux then begin
		printf, lun,'	flux changed (Ref=',(*old).flux,'    new=',(*new).flux,').'
		bad = 1
	endif

	if (*new).array.on ne (*old).array.on then begin
		printf, lun,'	array.on changed (Ref=',(*old).array.on,'    new=',(*new).array.on,').'
		bad = 1
	endif
	if ((*new).array.on eq 1) and ((*old).array.on eq 1) then begin
		if (*new).array.n_det ne (*old).array.n_det then begin
			printf, lun,'	array.n_det changed (Ref=',(*old).array.n_det,'    new=',(*new).array.n_det,').'
			bad = 1
		endif
		used = intarr(1000)
		used[ (*old).array.active] = 1
		q = where( used[ (*new).array.active] eq 0, nq)
		if nq ge 1 then begin
			for j=0,nq-1 do begin
				printf, lun,'	New: detector added (det=',q[j],').'
			endfor
			bad = 1
		endif
		used[*] = 0
		used[ (*new).array.active] = 1
		q = where( used[ (*old).array.active] eq 0, nq)
		if nq ge 1 then begin
			for j=0,nq-1 do begin
				printf, lun,'	Ref: detector not used (det=',q[j],').'
			endfor
			bad = 1
		endif
		
;		Need to print changes in rgamma, cintensity

		x = fltarr( (*new).array.n_det, (*new).n_els)
		y = fltarr( (*new).array.n_det, (*new).n_els)
		z = (*new).el.z
		shell = (*new).el.shell
		name = (*new).el.name
		for i=0,(*new).n_els-1 do begin
			q = where( ((*old).el.z eq (*new).el.z[i]) and ((*old).el.shell eq (*new).el.shell[i]), nq)
			if nq ge 1 then begin
				x[*,i] = (*old).array.rgamma[*,q[0]]
				y[*,i] = (*new).array.rgamma[*,i]
			endif
		endfor
		sig = sig_change( x, y, tol=0.001, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			q_to_xy, q, (*new).array.n_det, id,iz
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
	
	if (*new).background.mode ne (*old).background.mode then begin
		printf, lun,'	background.mode changed (Ref=',(*old).background.mode,'    new=',(*new).background.mode,').'
		bad = 1
	endif
	if (*new).background.scale ne (*old).background.scale then begin
		printf, lun,'	background.scale changed (Ref=',(*old).background.scale,'    new=',(*new).background.scale,').'
		bad = 1
	endif
	if (*new).back_split.mode ne (*old).back_split.mode then begin
		printf, lun,'	back_split.mode changed (Ref=',(*old).back_split.mode,'    new=',(*new).back_split.mode,').'
		bad = 1
	endif
	if (*new).back_split.emid ne (*old).back_split.emid then begin
		printf, lun,'	back_split.emid changed (Ref=',(*old).back_split.emid,'    new=',(*new).back_split.emid,').'
		bad = 1
	endif
	if (*new).back_split.scale ne (*old).back_split.scale then begin
		printf, lun,'	back_split.scale changed (Ref=',(*old).back_split.scale,'    new=',(*new).back_split.scale,').'
		bad = 1
	endif

	compare_yields, [ ptr_new((*old).yield), ptr_new((*new).yield)], /basic, el=[ptr_new((*old).el.name), ptr_new((*new).el.name)], error=err, bad=bad1, lun=lun
	bad = bad or bad1

	if (*new).yield.unknown ne (*old).yield.unknown then begin
		printf, lun,'	yield.unknown changed (Ref=',(*old).yield.unknown,'    new=',(*new).yield.unknown,').'
		bad = 1
	endif

	case (*new).beam.model of
		1: begin
			compare_source, [(*old).beam, (*new).beam], error=err, bad=bad1, lun=lun
			bad = bad or bad1
		end
		2: begin
			compare_pink, [(*old).beam, (*new).beam], error=err, bad=bad1, lun=lun
			bad = bad or bad1
		end
		else: begin
			printf, lun,'	Unknown beam continuum model (new=',(*new).beam.model,').'
		end
	endcase

	if (*new).inclusion.type ne (*old).inclusion.type then begin
		printf, lun,'	inclusion.type changed (Ref=',(*old).inclusion.type,'    new=',(*new).inclusion.type,').'
		bad = 1
	endif

;	Need to check inclusion parameters and norm's across elements (list may have changed?)

	x = fltarr( (*new).n_els)
	y = fltarr( (*new).n_els)
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).el.z eq (*new).el.z[i]) and ((*old).el.shell eq (*new).el.shell[i]), nq)
		if nq ge 1 then begin
			x[i] = (*old).counts_per_ppm_uc[q[0]]
			y[i] = (*new).counts_per_ppm_uc[i]
		endif else begin
;			warning,'compare_yields','Element Z='+str_tidy((*new).el.z[i])+', shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new).el.z[i])+' ('+(*new).el.name[i]+'), shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
		endelse
	endfor
	sig = sig_change( x, y, tol=0.001, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'counts_per_ppm_uc not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new).el.z[q[j]], (*new).el.name[q[j]], (*new).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'counts_per_ppm_uc all consistent.'
	endelse

	printf, lun,'-------------------------------------------------------------------------------------'

	x = fltarr( (*new).n_els)
	y = fltarr( (*new).n_els)
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).el.z eq (*new).el.z[i]) and ((*old).el.shell eq (*new).el.shell[i]), nq)
		if nq ge 1 then begin
			x[i] = (*old).conc[q[0]]
			y[i] = (*new).conc[i]
		endif else begin
;			warning,'compare_yields','Element Z='+str_tidy((*new).el.z[i])+', shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new).el.z[i])+' ('+(*new).el.name[i]+'), shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
		endelse
	endfor
	sig = sig_change( x, y, tol=0.001, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'Conc not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new).el.z[q[j]], (*new).el.name[q[j]], (*new).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'Conc all consistent.'
	endelse

	x = fltarr( (*new).n_els)
	y = fltarr( (*new).n_els)
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).el.z eq (*new).el.z[i]) and ((*old).el.shell eq (*new).el.shell[i]), nq)
		if nq ge 1 then begin
			x[i] = (*old).error[q[0]]
			y[i] = (*new).error[i]
		endif else begin
			;			warning,'compare_yields','Element Z='+str_tidy((*new).el.z[i])+', shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new).el.z[i])+' ('+(*new).el.name[i]+'), shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
		endelse
	endfor
	sig = sig_change( x, y, tol=0.001, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'Error not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new).el.z[q[j]], (*new).el.name[q[j]], (*new).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'Error all consistent.'
	endelse

	x = fltarr( (*new).n_els)
	y = fltarr( (*new).n_els)
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).el.z eq (*new).el.z[i]) and ((*old).el.shell eq (*new).el.shell[i]), nq)
		if nq ge 1 then begin
			x[i] = (*old).mdl[q[0]]
			y[i] = (*new).mdl[i]
		endif else begin
			;			warning,'compare_yields','Element Z='+str_tidy((*new).el.z[i])+', shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new).el.z[i])+' ('+(*new).el.name[i]+'), shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
		endelse
	endfor
	sig = sig_change( x, y, tol=0.001, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'MDL not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new).el.z[q[j]], (*new).el.name[q[j]], (*new).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'MDL all consistent.'
	endelse

	x = fltarr( (*new).n_els)
	y = fltarr( (*new).n_els)
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).el.z eq (*new).el.z[i]) and ((*old).el.shell eq (*new).el.shell[i]), nq)
		if nq ge 1 then begin
			x[i] = (*old).area[q[0]]
			y[i] = (*new).area[i]
		endif else begin
			;			warning,'compare_yields','Element Z='+str_tidy((*new).el.z[i])+', shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
			printf, lun,'	Element Z='+str_tidy((*new).el.z[i])+' ('+(*new).el.name[i]+'), shell='+str_tidy((*new).el.shell[i])+' not found in reference.'
		endelse
	endfor
	sig = sig_change( x, y, tol=0.001, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'Area not consistent ...'
		printf, lun,'  Index    Z    Name   Shell       Ref        New'
		for j=0,nq-1 do begin
			printf, lun, q[j], (*new).el.z[q[j]], (*new).el.name[q[j]], (*new).el.shell[q[j]], x[q[j]], y[q[j]], $
				format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
		endfor
	endif else begin
		printf, lun,'Area all consistent.'
	endelse

	if bad then begin
;		return
	endif else begin
		printf, lun,'Parameters consistent.'
	endelse
		
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

	