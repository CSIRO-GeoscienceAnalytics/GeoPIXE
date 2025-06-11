pro compare_yields, files, output, error=err, bad=bad, lun=lun, basic=basic, el=pel, tol=tol

; Compare two yields files for consistency.
; Check input parameters first, such as beam, detector, layers.
; Compare 'yield' and 'intensity' results, but only for matching cases (Z, shell) with
; finite, non-zero yield results.
;
; NOTE: This command is suitable to execute directly using the callable GeoPIXE approach,
;		passing the two file names to be compared.
;
; 'files' are input yield file names (2) to compare. 
; 			Prompts for these if missing.
; 			Can also pass yield structure pointers
; 'output' is a txt file report to create.
; 'lun=lun'	if a logical unit is input, then ignore 'output' and write/append to that lun.
; /basic	for basic yield struct, as in fit results PFR file (also pass vector of pointers to El names in 'el').
;			else full yield struct as in yield file.
; el		vector of pointers to El names for /basic mode
; 'err=1'	error reading yield files, or some crash.
; 'bad=1'	an input parameter difference found.
; 'tol'		fractional tolerance to use for yields

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
			warning,'compare_yields',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			err = 1
			return
		endif
	endif
	err = 1
	local = 1
	bad = 0

; Use tol=0.001 as default for model value tests

	if n_elements(tol) eq 0 then tol = 0.001
	if n_elements(lun) ne 0 then local=0
	if n_elements(basic) eq 0 then basic=0
	if basic then begin
		if n_elements(pel) lt 2 then goto, bad_basic
		old_el = *pel[0]
		new_el = *pel[1]
	endif

	if n_elements(files) eq 0 then begin
		fold = file_requester( /read, filter='*.yield', /fix_filter, title='Reference YIELD file.', $
					preview_routine='file_yield_preview')
		if fold eq '' then return
	endif else fold=files[0]
	if n_elements(files) lt 2 then begin
		path = extract_path(fold)
		fnew = file_requester( /read, path=path, filter='*.yield', /fix_filter, title='New YIELD file to check.', $
					preview_routine='file_yield_preview')
		if fnew eq '' then return
	endif else fnew=files[1]
	
	if typevar(fnew) eq 'POINTER' then begin
		new = fnew
	endif else begin
		new = read_yield( fnew, error=err)
		if err then begin
			warning, 'compare_yields','Failed to read NEW file: '+fnew
			return
		endif
	endelse
	if typevar(fold) eq 'POINTER' then begin
		old = fold
	endif else begin
		old = read_yield( fold, error=err)
		if err then begin
			warning, 'compare_yields','Failed to read REFERENCE file: '+fold
			return
		endif
	endelse
	err = 0

	if local then begin
		if n_elements(output) eq 0 then begin
			output = strip_file_ext( fnew) + '.txt'
		endif	
		on_ioerror, bad_open
		openw, lun, output, /get_lun
		on_ioerror, bad_write
	endif
	
	if local then begin
		printf, lun,'-------------------------------------------------------------------------------------'
		printf, lun,'Yield Setup parameters ...'
	endif
	bad = 0
	
	if (*new).Z1 ne (*old).Z1 then begin
		printf, lun,'	Beam Z1 changed (Ref=',(*old).Z1,'    new=',(*new).Z1,').'
		bad = 1
	endif
	if (*new).A1 ne (*old).A1 then begin
		printf, lun,'	Beam A1 changed (Ref=',(*old).A1,'    new=',(*new).A1,').'
		bad = 1
	endif
	if (*new).state ne (*old).state then begin
		printf, lun,'	Beam state changed (Ref=',(*old).state,'    new=',(*new).state,').'
		bad = 1
	endif
	if (*new).e_beam ne (*old).e_beam then begin
		printf, lun,'	Beam E Beam changed (Ref=',(*old).e_beam,'    new=',(*new).e_beam,').'
		bad = 1
	endif
	if (*new).theta ne (*old).theta then begin
		printf, lun,'	Detector theta changed (Ref=',(*old).theta,'    new=',(*new).theta,').'
		bad = 1
	endif
	if (*new).phi ne (*old).phi then begin
		printf, lun,'	Detector phi changed (Ref=',(*old).phi,'    new=',(*new).phi,').'
		bad = 1
	endif
	if (*new).alpha ne (*old).alpha then begin
		printf, lun,'	Target alpha changed (Ref=',(*old).alpha,'    new=',(*new).alpha,').'
		bad = 1
	endif
	if (*new).beta ne (*old).beta then begin
		printf, lun,'	Target beta changed (Ref=',(*old).beta,'    new=',(*new).beta,').'
		bad = 1
	endif

	if basic then begin
		if (*new).unknown ne (*old).unknown then begin
			printf, lun,'	Unknown changed (Ref=',(*old).unknown,'    new=',(*new).unknown,').'
			bad = 1
		endif
		
		q = where( (*old).thick ne 0.0, nq)
		qn = where( (*new).thick ne 0.0, nqn)
		if nqn ne nq then begin
			printf, lun,'	Layers changed (Ref=',nq,'    new=',nqn,').'
			bad = 1
		endif
		
		for l=0, (nq<nqn)-1 do begin
			if (*new).formula[l] ne (*old).formula[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' formula changed (Ref=',(*old).formula[l],'    new=',(*new).formula[l],').'
				bad = 1
			endif
			if (*new).thick[l] ne (*old).thick[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' thickness changed (Ref=',(*old).thick[l],'    new=',(*new).thick[l],').'
				bad = 1
			endif
			if (*new).density[l] ne (*old).density[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' density changed (Ref=',(*old).density[l],'    new=',(*new).density[l],').'
				bad = 1
			endif
			if (*new).weight[l] ne (*old).weight[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' weight changed (Ref=',(*old).weight[l],'    new=',(*new).weight[l],').'
				bad = 1
			endif
			if (*new).microns[l] ne (*old).microns[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' microns changed (Ref=',(*old).microns[l],'    new=',(*new).microns[l],').'
				bad = 1
			endif
		endfor
		
	endif else begin
		if (*new).beam.continuum ne (*old).beam.continuum then begin
			printf, lun,'	Beam continuum changed (Ref=',(*old).beam.continuum,'    new=',(*new).beam.continuum,').'
			bad = 1
		endif
		if (*new).beam.continuum ne 0 then begin
			if strip_path((*new).beam.file) ne strip_path((*old).beam.file) then begin
				printf, lun,'	Beam continuum file changed (Ref=',(*old).beam.file,'    new=',(*new).beam.file,').'
				bad = 1
			endif
			if strip_path((*new).beam.model) ne strip_path((*old).beam.model) then begin
				printf, lun,'	Beam continuum model changed (Ref=',(*old).beam.model,'    new=',(*new).beam.model,').'
				printf, lun,'		Therefore, cannot compare continuum beam parameters and spectra details.'
				bad = 1
			endif else begin
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
			endelse
		endif
		if (*new).n_layers ne (*old).n_layers then begin
			printf, lun,'	Layer number changed (Ref=',(*old).n_layers,'    new=',(*new).n_layers,').'
			bad = 1
		endif
		for l=0,(*new).n_layers-1 do begin
			if (*new).n_layers le (*old).n_layers then begin
				if (*new).formula[l] ne (*old).formula[l] then begin
					printf, lun,'	Layer '+str_tidy(l)+' formula changed (Ref=',(*old).formula[l],'    new=',(*new).formula[l],').'
					bad = 1
				endif
				if (*new).thick[l] ne (*old).thick[l] then begin
					printf, lun,'	Layer '+str_tidy(l)+' thickness changed (Ref=',(*old).thick[l],'    new=',(*new).thick[l],').'
					bad = 1
				endif
				if (*new).density[l] ne (*old).density[l] then begin
					printf, lun,'	Layer '+str_tidy(l)+' density changed (Ref=',(*old).density[l],'    new=',(*new).density[l],').'
					bad = 1
				endif
			endif
		endfor
	endelse
	
	if bad then begin
;		return
	endif else begin
		printf, lun,'Yield Parameters consistent.'
	endelse
	
	if basic  then begin
		for l=0,n_elements((*new).yield[0,*])-1 do begin

;			Check yields, for layer=l

			printf, lun,'-------------------------------------------------------------------------------------'
			printf, lun,'Layer ',l,' ...'
	
			x = fltarr( n_elements((*new).yield[*,0]))
			y = fltarr( n_elements((*new).yield[*,0]))
			for i=0,n_elements(new_el)-1 do begin
				q = where( old_el eq new_el[i], nq)
				if nq ge 1 then begin
					x[i] = (*old).yield[q[0],l]
					y[i] = (*new).yield[i,l]
				endif else begin
;					warning,'compare_yields: Element ='+el[i]+' not found in reference.'
					printf, lun,'	Element ='+new_el[i]+' not found in reference.'
				endelse
			endfor
			sig = sig_change( x, y, tol=tol, error=err1)
			q = where( sig ne 0, nq)
			if nq gt 0 then begin
				printf, lun,'Yields not consistent ...'
				printf, lun,'  Index    Name     Ref        New'
				for j=0,nq-1 do begin
					printf, lun, q[j], new_el[q[j]], x[q[j]], y[q[j]], $
						format='(I6,3x,A4,2x,G11.4,G11.4)'
				endfor
			endif else begin
				printf, lun,'Yields all consistent.'
			endelse
		endfor
		
	endif else begin
		for l=0,(*new).n_layers-1 do begin

;		Check yields, for layer=l

			printf, lun,'-------------------------------------------------------------------------------------'
			printf, lun,'Layer ',l,' ...'

			x = fltarr( (*new).n_els)
			y = fltarr( (*new).n_els)
			z = (*new).z
			shell = (*new).shell
			for i=0,(*new).n_els-1 do begin
				q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
				if nq ge 1 then begin
					x[i] = (*old).yield[q[0],l]
					y[i] = (*new).yield[i,l]
				endif else begin
;					warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+' not found in reference.'
					printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+' not found in reference.'
				endelse
			endfor
			sig = sig_change( x, y, tol=tol, error=err1)
			q = where( sig ne 0, nq)
			if nq gt 0 then begin
				printf, lun,'Yields not consistent ...'
				printf, lun,'  Index    Z    Name   Shell       Ref        New'
				for j=0,nq-1 do begin
					printf, lun, q[j], z[q[j]], element_name(z[q[j]]), shell[q[j]], x[q[j]], y[q[j]], $
						format='(I6,I6,3x,A4,I7,2x,G11.4,G11.4)'
				endfor
			endif else begin
				printf, lun,'Yields all consistent.'
			endelse
		
;			Check intensities, for layer=l and non zero yields

			n_lines_max = n_elements( (*new).intensity[*,0])
			x = fltarr( n_lines_max, (*new).n_els)
			y = fltarr( n_lines_max, (*new).n_els)
			z = (*new).z
			shell = (*new).shell
			lines = (*new).lines
			e = (*new).e
			for i=0,(*new).n_els-1 do begin
				q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
				if nq ge 1 then begin
					for k=0,(*new).n_lines[i]-1 do begin
						q1 = where( (*old).lines[*,q[0]] eq (*new).lines[k,i], nq1)
						if nq1 gt 0 then begin
							if (*old).yield[q[0],l] gt 1.0e-19 then begin
								x[k,i] = (*old).intensity[q1[0],q[0]]
								y[k,i] = (*new).intensity[k,i]
							endif
						endif else begin
	;						warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+', E='+str_tidy((*new).e[k,i])+' not found in reference.'
							printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+' ('+line_id((*new).lines[k,i])+'), E='+str_tidy((*new).e[k,i])+' not found in reference.'
						endelse				
					endfor
				endif
			endfor
			sig = sig_change( x, y, tol=tol, error=err1)
			q = where( sig ne 0, nq)
			if nq gt 0 then begin
				q_to_xy, q, n_lines_max, il,iz
				printf, lun,'Intensities not consistent ...'
				printf, lun,'  Index    Z    Name  Shell  Line   Name      E        Ref        New'
				for j=0,nq-1 do begin
					printf, lun, q[j], z[iz[j]], element_name(z[iz[j]]), shell[iz[j]], lines[il[j],iz[j]], line_id(lines[il[j],iz[j]]), e[il[j],iz[j]], x[il[j],iz[j]], y[il[j],iz[j]], $
						format='(I6,I6,3x,A4,I6,I7,A8,2x,F8.3,G11.4,G11.4)'
				endfor
			endif else begin
				printf, lun,'Intensities all consistent.'
			endelse
		endfor
	
		printf, lun,'-------------------------------------------------------------------------------------'
	
;		Check mu_zero for non zero yields

		n_lines_max = n_elements( (*new).intensity[0,*,0])
		x = fltarr( n_lines_max, (*new).n_els)
		y = fltarr( n_lines_max, (*new).n_els)
		z = (*new).z
		shell = (*new).shell
		lines = (*new).lines
		e = (*new).e
		for i=0,(*new).n_els-1 do begin
			q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
			if nq ge 1 then begin
				for k=0,(*new).n_lines[i]-1 do begin
					q1 = where( (*old).lines[*,q[0]] eq (*new).lines[k,i], nq1)
					if nq1 gt 0 then begin
						if (*old).yield[q[0],0] gt 1.0e-19 then begin
							x[k,i] = (*old).mu_zero[q1[0],q[0]]
							y[k,i] = (*new).mu_zero[k,i]
						endif
					endif else begin
						;						warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+', E='+str_tidy((*new).e[k,i])+' not found in reference.'
						printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+' ('+line_id((*new).lines[k,i])+'), E='+str_tidy((*new).e[k,i])+' not found in reference.'
					endelse
				endfor
			endif
		endfor
		sig = sig_change( x, y, tol=tol, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			q_to_xy, q, n_lines_max, il,iz
			printf, lun,'mu_zero not consistent ...'
			printf, lun,'  Index    Z    Name  Shell  Line  Name      E        Ref        New'
			for j=0,nq-1 do begin
				printf, lun, q[j], z[iz[j]] ,element_name(z[iz[j]]), shell[iz[j]], lines[il[j],iz[j]], line_id(lines[il[j],iz[j]]), e[il[j],iz[j]], x[il[j],iz[j]], y[il[j],iz[j]], $
					format='(I6,I6,3x,A4,I6,I7,A8,2x,F8.3,G11.4,G11.4)'
			endfor
		endif else begin
			printf, lun,'mu_zero all consistent.'
		endelse

;		Check ratio_yield for non zero yields

		n_dets = n_elements( (*new).ratio_yield[*,0])
		old_array = 0
		if n_elements( (*old).ratio_yield[*,0]) gt 1 then old_array=1
		if old_array and (n_dets lt 2) then begin
			printf, lun,'"Ref" is an array detector, but "New" is not.'		
		endif
		if n_dets gt 1 then begin
			if old_array then begin
				x = fltarr( n_dets, (*new).n_els)
				y = fltarr( n_dets, (*new).n_els)
				z = (*new).z
				shell = (*new).shell
				for i=0,(*new).n_els-1 do begin
					q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
					if nq ge 1 then begin
						if (*old).yield[q[0],0] gt 1.0e-19 then begin
							x[*,i] = (*old).ratio_yield[*,q[0]]
							y[*,i] = (*new).ratio_yield[*,i]
						endif
					endif
				endfor
				sig = sig_change( x, y, tol=tol, error=err1)
				q = where( sig ne 0, nq)
				if nq gt 0 then begin
					q_to_xy, q, n_dets, id,iz
					printf, lun,'ratio_yield not consistent ...'
					printf, lun,'  Index    Z    Name   Shell  Detector   Ref        New'
					for j=0,nq-1 do begin
						printf, lun, q[j], z[iz[j]], element_name(z[iz[j]]), shell[iz[j]], id[j], x[id[j],iz[j]], y[id[j],iz[j]], $
							format='(I6,I6,3x,A4,I7,I7,G11.4,G11.4)'
					endfor
				endif else begin
					printf, lun,'ratio_yield all consistent.'
				endelse
			endif else begin
				printf, lun,'"New" is an array detector, but "Ref" is not.'
			endelse
		endif

;		Check ratio_intensity for non zero yields

		n_dets = n_elements( (*new).ratio_intensity[*,0,0])
		if n_dets gt 1 and old_array then begin
			n_lines_max = n_elements( (*new).ratio_intensity[0,*,0])
			x = fltarr( n_dets, n_lines_max, (*new).n_els)
			y = fltarr( n_dets, n_lines_max, (*new).n_els)
			z = (*new).z
			shell = (*new).shell
			lines = (*new).lines
			e = (*new).e
			for i=0,(*new).n_els-1 do begin
				q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
				if nq ge 1 then begin
					x1 = fltarr( n_dets, n_lines_max)
					y1 = fltarr( n_dets, n_lines_max)
					for k=0,(*new).n_lines[i]-1 do begin
						q1 = where( (*old).lines[*,q[0]] eq (*new).lines[k,i], nq1)
						if nq1 gt 0 then begin
							if (*old).yield[q[0],0] gt 1.0e-19 then begin
								x[*,k,i] = (*old).ratio_intensity[*,q1[0],q[0]]
								y[*,k,i] = (*new).ratio_intensity[*,k,i]
							endif
						endif else begin
							;						warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+', E='+str_tidy((*new).e[k,i])+' not found in reference.'
							printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+' ('+line_id((*new).lines[k,i])+'), E='+str_tidy((*new).e[k,i])+' not found in reference.'
						endelse
					endfor
				endif
			endfor
			sig = sig_change( x, y, tol=tol, error=err1)
			q = where( sig ne 0, nq)
			if nq gt 0 then begin
				q_to_xyz, q, n_dets,n_lines_max, id,il,iz
				printf, lun,'ratio_intensity not consistent ...'
				printf, lun,'  Index    Z    Name  Shell   Det   Line   Name       E        Ref        New'
				for j=0,nq-1 do begin
					printf, lun, q[j], z[iz[j]], element_name(z[iz[j]]), shell[iz[j]], id[j], lines[il[j],iz[j]], line_id(lines[il[j],iz[j]]), e[il[j],iz[j]], x[id[j],il[j],iz[j]], y[id[j],il[j],iz[j]], $
						format='(I6,I6,3x,A4,I6,I7,I7,A8,2x,F8.3,G11.4,G11.4)'
				endfor
			endif else begin
				printf, lun,'ratio_intensity all consistent.'
			endelse
		endif
	endelse
	
done:
	err = 0
	if local then begin
		printf, lun,'All done.'
		printf, lun,'-------------------------------------------------------------------------------------'
		close_file, lun
	endif
	return
	
bad_open:
	printf, lun,'compare_yields: Error opening output file: '+output
	err = 1
	return
bad_basic:
	printf, lun,'compare_yields: /Basic mode, but no vector to EL names provided.'
	err = 1
	return
bad_write:
	printf, lun,'compare_yields: Error writing to output file: '+output
	if local then close_file, lun
	err = 1
	return
end

	