function sig_change, old, new, error=err

; Flag a significant change in 'new' compared to 'old'
; Assumes quantities should be positive.
; Assumes "significance" is more than 1 ppm.

	err = 1
	if n_elements(old) eq 0 then return, 1
	
	sig = fix( old)
	sig[*] = 1
	if n_elements(new) ne n_elements(old) then return, sig
	
	err = 0
	f = (new-old)/(old > 1.0e-35)
	sig[*] = abs(f) gt 1.0e-6
	return, sig
end

;------------------------------------------------------------------------------------

pro compare_yields, files, output, error=err

; Compare two yields files for consistency.
; Check input parameters first, such as beam, detector, layers.
; Compare 'yield' and 'intensity' results, but only for matching cases (Z, shell) with
; finite, non-zero yield results.
;
; 'files' are input yield file names to compare. Prompts for these if missing.
; 'output' is a txt file report to create.
; 'err=1'	error reading yield files, or some crash.

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
			warning,'Geo_yield2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			err = 1
			return
		endif
	endif

	err = 1
	if n_elements(files) eq 0 then begin
		fold = file_requester( /read, filter='*.yield', /fix_filter, title='Reference YIELD file.')
		if fold eq '' then return
	endif else fold=files[0]
	if n_elements(files) lt 2 then begin
		path = extract_path(fold)
		fnew = file_requester( /read, path=path, filter='*.yield', /fix_filter, title='New YIELD file to check.')
		if fnew eq '' then return
	endif else fnew=files[1]
	
	new = read_yield( fnew, error=err)
	if err then begin
		warning, 'compare_yields','Failed to read NEW file: '+fnew
		return
	endif
	old = read_yield( fold, error=err)
	if err then begin
		warning, 'compare_yields','Failed to read REFERENCE file: '+fold
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
	if (*new).beam.continuum ne (*old).beam.continuum then begin
		printf, lun,'	Beam continuum changed (Ref=',(*old).beam.continuum,'    new=',(*new).beam.continuum,').'
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
	if (*new).beam.continuum ne 0 then begin
		if (*new).beam.model ne (*old).beam.model then begin
			printf, lun,'	Beam continuum model changed (Ref=',(*old).beam.model,'    new=',(*new).beam.model,').'
			bad = 1
		endif
		if (*new).beam.energy ne (*old).beam.energy then begin
			printf, lun,'	Beam continuum energy changed (Ref=',(*old).beam.energy,'    new=',(*new).beam.energy,').'
			bad = 1
		endif
		if strip_path((*new).beam.file) ne strip_path((*old).beam.file) then begin
			printf, lun,'	Beam continuum file changed (Ref=',(*old).beam.file,'    new=',(*new).beam.file,').'
			bad = 1
		endif
		if (*new).beam.modata.volts ne (*old).beam.modata.volts then begin
			printf, lun,'	Beam continuum volts changed (Ref=',(*old).beam.modata.volts,'    new=',(*new).beam.modata.volts,').'
			bad = 1
		endif
		if (*new).beam.modata.power ne (*old).beam.modata.power then begin
			printf, lun,'	Beam continuum power changed (Ref=',(*old).beam.modata.power,'    new=',(*new).beam.modata.power,').'
			bad = 1
		endif
		if (*new).beam.modata.phi ne (*old).beam.modata.phi then begin
			printf, lun,'	Beam continuum phi changed (Ref=',(*old).beam.modata.phi,'    new=',(*new).beam.modata.phi,').'
			bad = 1
		endif
		if (*new).beam.modata.eps ne (*old).beam.modata.eps then begin
			printf, lun,'	Beam continuum eps changed (Ref=',(*old).beam.modata.eps,'    new=',(*new).beam.modata.eps,').'
			bad = 1
		endif
		if (*new).beam.modata.anode.formula ne (*old).beam.modata.anode.formula then begin
			printf, lun,'	Beam continuum anode formula changed (Ref=',(*old).beam.modata.anode.formula,'    new=',(*new).beam.modata.anode.formula,').'
			bad = 1
		endif
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
	
	if bad then begin
;		return
	endif else begin
		printf, lun,'Parameters consistent.'
	endelse
	
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
;				warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+' not found in reference.'
				printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+' not found in reference.'
			endelse
		endfor
		sig = sig_change( x, y, error=err1)
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

;		Check intensities, for layer=l and non zero yields

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
		sig = sig_change( x, y, error=err1)
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
	
;	Check mu_zero for non zero yields

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
	sig = sig_change( x, y, error=err1)
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

;	Check ratio_yield for non zero yields

	n_dets = n_elements( (*new).ratio_yield[*,0])
	if n_dets gt 1 then begin
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
		sig = sig_change( x, y, error=err1)
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
	endif

;	Check ratio_intensity for non zero yields

	n_dets = n_elements( (*new).ratio_intensity[*,0,0])
	if n_dets gt 1 then begin
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
		sig = sig_change( x, y, error=err1)
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
	
	err = 0
	printf, lun,'All done.'
	printf, lun,'-------------------------------------------------------------------------------------'
	close_file, lun
	return
	
bad_open:
	printf, lun,'compare_yields: Error opening output file: '+output
	err = 1
	return
bad_write:
	printf, lun,'compare_yields: Error writing to output file: '+output
	close_file, lun
	err = 1
	return
end

	