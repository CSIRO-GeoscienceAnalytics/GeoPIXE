function sig_change, old, new, error=err

; Flag a significant change in 'new' compared to 'old'
; Assumes quantities should be positive.
; Assumes "significance" is more than 1 part in 1,000,000.

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
		printf, lun,'	Beam Z1 changed.
		bad = 1
	endif
	if (*new).A1 ne (*old).A1 then begin
		printf, lun,'	Beam A1 changed.
		bad = 1
	endif
	if (*new).state ne (*old).state then begin
		printf, lun,'	Beam state changed.
		bad = 1
	endif
	if (*new).e_beam ne (*old).e_beam then begin
		printf, lun,'	Beam E Beam changed.
		bad = 1
	endif
	if (*new).beam.continuum ne (*old).beam.continuum then begin
		printf, lun,'	Beam continuum changed.
		bad = 1
	endif
	if (*new).theta ne (*old).theta then begin
		printf, lun,'	Detector theta changed.
		bad = 1
	endif
	if (*new).phi ne (*old).phi then begin
		printf, lun,'	Detector phi changed.
		bad = 1
	endif
	if (*new).alpha ne (*old).alpha then begin
		printf, lun,'	Target alpha changed.
		bad = 1
	endif
	if (*new).beam.continuum ne 0 then begin
		if (*new).beam.model ne (*old).beam.model then begin
			printf, lun,'	Beam continuum model changed.
			bad = 1
		endif
		if (*new).beam.energy ne (*old).beam.energy then begin
			printf, lun,'	Beam continuum energy changed.
			bad = 1
		endif
		if strip_path((*new).beam.file) ne strip_path((*old).beam.file) then begin
			printf, lun,'	Beam continuum file changed.
			bad = 1
		endif
		if (*new).beam.modata.volts ne (*old).beam.modata.volts then begin
			printf, lun,'	Beam continuum volts changed.
			bad = 1
		endif
		if (*new).beam.modata.power ne (*old).beam.modata.power then begin
			printf, lun,'	Beam continuum power changed.
			bad = 1
		endif
		if (*new).beam.modata.phi ne (*old).beam.modata.phi then begin
			printf, lun,'	Beam continuum phi changed.
			bad = 1
		endif
		if (*new).beam.modata.eps ne (*old).beam.modata.eps then begin
			printf, lun,'	Beam continuum eps changed.
			bad = 1
		endif
		if (*new).beam.modata.anode.formula ne (*old).beam.modata.anode.formula then begin
			printf, lun,'	Beam continuum anode formula changed.
			bad = 1
		endif
	endif
	if (*new).n_layers ne (*old).n_layers then begin
		printf, lun,'	Layer number changed.
		bad = 1
	endif
	for l=0,(*new).n_layers-1 do begin
		if (*new).n_layers le (*old).n_layers then begin
			if (*new).formula[l] ne (*old).formula[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' formula changed.
				bad = 1
			endif
			if (*new).thick[l] ne (*old).thick[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' thickness changed.
				bad = 1
			endif
			if (*new).density[l] ne (*old).density[l] then begin
				printf, lun,'	Layer '+str_tidy(l)+' density changed.
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
		x = 0.0
		y = 0.0
		z = 0
		shell = 0
		for i=0,(*new).n_els-1 do begin
			q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
			if nq ge 1 then begin
				x = [x, (*old).yield[q[0],l]]
				y = [y, (*new).yield[i,l]]
				z = [z, (*new).z[i]]
				shell = [shell, (*new).shell[i]]			
			endif else begin
;				warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+' not found in reference.'
				printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+' not found in reference.'
			endelse
		endfor
		x = x[1:*]
		y = y[1:*]
		z = z[1:*]
		shell = shell[1:*]
		sig = sig_change( x, y, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'Yields not consistent ...'
			printf, lun,'        Index,     Z,   Name,   Shell,       Ref,         New'
			for j=0,nq-1 do begin
				printf, lun,q[j], z[q[j]], '     ',element_name(z[q[j]]), shell[q[j]], '    ',x[q[j]], y[q[j]]
			endfor
		endif else begin
			printf, lun,'Yields all consistent.'
		endelse

;		Check intensities, for layer=l and non zero yields

		x = 0.0
		y = 0.0
		z = 0
		shell = 0
		lines = 0
		e = 0.0
		qo = 0
		for i=0,(*new).n_els-1 do begin
			q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
			if nq ge 1 then begin
				for k=0,(*new).n_lines[i]-1 do begin
					q1 = where( (*old).lines[*,q[0]] eq (*new).lines[k,i], nq1)
					if nq1 gt 0 then begin
						if (*old).yield[q[0],l] gt 1.0e-19 then begin
							qo = [qo, q1[0]]
							x = [x, (*old).intensity[q1[0],q[0]]]
							y = [y, (*new).intensity[k,i]]
							z = [z, (*new).z[i]]
							shell = [shell, (*new).shell[i]]
							lines = [lines, (*new).lines[k,i]]
							e = [e, (*new).e[k,i]]
						endif
					endif else begin
;						warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+', E='+str_tidy((*new).e[k,i])+' not found in reference.'
						printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+' ('+line_id((*new).lines[k,i])+'), E='+str_tidy((*new).e[k,i])+' not found in reference.'
					endelse				
				endfor
			endif
		endfor
		qo = qo[1:*]
		x = x[1:*]
		y = y[1:*]
		z = z[1:*]
		shell = shell[1:*]
		lines = lines[1:*]
		e = e[1:*]
		sig = sig_change( x, y, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'Intensities not consistent ...'
			printf, lun,'  Index    Z    Name  Shell  Lindex  Line  Name      E        Ref        New'
			for j=0,nq-1 do begin
				printf, lun, q[j], z[q[j]], element_name(z[q[j]]), shell[q[j]], qo[q[j]], lines[q[j]], line_id(lines[q[j]]), e[q[j]], x[q[j]], y[q[j]], $
					format='(I6,I6,3x,A4,I6,I7,I7,A7,3x,F8.3,G11.4,G11.4)'
			endfor
		endif else begin
			printf, lun,'Intensities all consistent.'
		endelse
	endfor
	
;	Check mu_zero for non zero yields

	x = 0
	y = 0
	z = 0
	shell = 0
	lines = 0
	e = 0.0
	qo = 0
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
		if nq ge 1 then begin
			for k=0,(*new).n_lines[i]-1 do begin
				q1 = where( (*old).lines[*,q[0]] eq (*new).lines[k,i], nq1)
				if nq1 gt 0 then begin
					if (*old).yield[q[0],0] gt 1.0e-19 then begin
						qo = [qo, q1[0]]
						x = [x, (*old).mu_zero[q1[0],q[0]]]
						y = [y, (*new).mu_zero[k,i]]
						z = [z, (*new).z[i]]
						shell = [shell, (*new).shell[i]]
						lines = [lines, (*new).lines[k,i]]
						e = [e, (*new).e[k,i]]
					endif
				endif else begin
					;						warning,'compare_yields','Element Z='+str_tidy((*new).z[i])+', shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+', E='+str_tidy((*new).e[k,i])+' not found in reference.'
					printf, lun,'	Element Z='+str_tidy((*new).z[i])+' ('+element_name((*new).z[i])+'), shell='+str_tidy((*new).shell[i])+', line='+str_tidy((*new).lines[k,i])+' ('+line_id((*new).lines[k,i])+'), E='+str_tidy((*new).e[k,i])+' not found in reference.'
				endelse
			endfor
		endif
	endfor
	qo = qo[1:*]
	x = x[1:*]
	y = y[1:*]
	z = z[1:*]
	shell = shell[1:*]
	lines = lines[1:*]
	e = e[1:*]
	sig = sig_change( x, y, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		printf, lun,'mu_zero not consistent ...'
		printf, lun,'        Index,     Z,   Name,   Shell,   Lindex,   Line,    Name,       E,         Ref,         New'
		for j=0,nq-1 do begin
			printf, lun,q[j], z[q[j]], '     ',element_name(z[q[j]]), shell[q[j]], qo[q[j]], lines[q[j]], '      ',line_id(lines[q[j]]), e[q[j]], x[q[j]], y[q[j]]
		endfor
	endif else begin
		printf, lun,'mu_zero all consistent.'
	endelse

;	Check ratio_yield for non zero yields

	n_dets = n_elements( (*new).ratio_yield[*,0])
	x = intarr( n_dets, 1)
	y = intarr( n_dets, 1)
	z = 0
	shell = 0
	n = 0
	for i=0,(*new).n_els-1 do begin
		q = where( ((*old).z eq (*new).z[i]) and ((*old).shell eq (*new).shell[i]), nq)
		if nq ge 1 then begin
			if (*old).yield[q[0],0] gt 1.0e-19 then begin
				x = [x, (*old).ratio_yield[*,q[0]]]
				y = [y, (*new).ratio_yield[*,i]]
				z = [z, (*new).z[i]]
				shell = [shell, (*new).shell[i]]
				n++
			endif
		endif
	endfor
	x = x[n_dets:*]
	y = y[n_dets:*]
	x = reform( x, n_dets, n)
	y = reform( y, n_dets, n)
	z = z[1:*]
	shell = shell[1:*]
	sig = sig_change( x, y, error=err1)
	q = where( sig ne 0, nq)
	if nq gt 0 then begin
		q_to_xy, q, n_dets, id,iz
		printf, lun,'ratio_yield not consistent ...'
		printf, lun,'        Index,     Z,   Name,   Shell,    Detector,    Ref,         New'
		for j=0,nq-1 do begin
			printf, lun, iz[j], z[iz[j]], '     ',element_name(z[iz[j]]), shell[iz[j]], id[j], x[id[j],iz[j]], y[id[j],iz[j]]
		endfor
	endif else begin
		printf, lun,'ratio_yield all consistent.'
	endelse

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

	