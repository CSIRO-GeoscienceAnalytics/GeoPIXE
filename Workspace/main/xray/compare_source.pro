pro compare_source, files, output, error=err, bad=bad, lun=lun

	; Compare two source files for consistency.
	; Check input parameters first, such as source, continuum spectrum, filters.
	;
	; 'files' are input source file names (2) to compare. 
	; 			Prompts for these if missing.
	; 			Can also pass source structures
	; 'output' is a txt file report to create.
	; 'lun=lun'	if a logical unit is input, then ignore 'output' and write/append to that file.
	; 
	; 'err=1'	error reading source files, or some crash.
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
			warning,'compare_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			err = 1
			return
		endif
	endif
	local = 1
	if n_elements(lun) ne 0 then local=0
	
	err = 1
	if n_elements(files) eq 0 then begin
		if local eq 0 then goto, bad_input
		fold = file_requester( /read, filter='*.source', /fix_filter, title='Reference SOURCE file.')
		if fold eq '' then return
	endif else fold=files[0]
	if n_elements(files) lt 2 then begin
		if local eq 0 then goto, bad_input
		path = extract_path(fold)
		fnew = file_requester( /read, path=path, filter='*.source', /fix_filter, title='New SOURCE file to check.')
		if fnew eq '' then return
	endif else fnew=files[1]

	if typevar(fnew) eq 'STRUCT' then begin
		new = fnew
	endif else begin
		new = read_source( fnew, error=err)
		if err then begin
			warning, 'compare_source','Failed to read NEW file: '+fnew
			return
		endif
	endelse
	if typevar(fold) eq 'STRUCT' then begin
		old = fold
	endif else begin
		old = read_source( fold, error=err)
		if err then begin
			warning, 'compare_source','Failed to read REFERENCE file: '+fold
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
		printf, lun,'Setup source parameters ...'
	endif
	bad = 0

	if new.continuum eq 0 then begin
		goto, done
	endif else begin
		if old.model ne 1 then begin
			printf, lun,'	Beam continuum model was wrong (should be "1", Ref=',old.model,').'
			bad = 1
		endif
		if new.model ne 1 then begin
			printf, lun,'	Beam continuum model now wrong (should be "1", new=',new.model,').'
			bad = 1
		endif
		if new.model ne old.model then begin
			printf, lun,'	Beam continuum model changed (Ref=',old.model,'    new=',new.model,').'
			bad = 1
		endif
		if new.energy ne old.energy then begin
			printf, lun,'	Beam continuum energy changed (Ref=',old.energy,'    new=',new.energy,').'
			bad = 1
		endif
		if new.acceptance ne old.acceptance then begin
			printf, lun,'	Beam continuum acceptance changed (Ref=',old.acceptance,'    new=',new.acceptance,').'
			bad = 1
		endif
		if new.beam.mode ne old.beam.mode then begin
			printf, lun,'	Beam eBeam mode changed (Ref=',old.beam.mode,'    new=',new.beam.mode,').'
			bad = 1
		endif
		if (old.beam.mode ne 0) and (new.beam.thick ne old.beam.thick) then begin
			printf, lun,'	Beam eBeam transmission thick changed (Ref=',old.beam.thick,'    new=',new.beam.thick,').'
			bad = 1
		endif
		if new.modata.volts ne old.modata.volts then begin
			printf, lun,'	Beam continuum volts changed (Ref=',old.modata.volts,'    new=',new.modata.volts,').'
			bad = 1
		endif
		if new.modata.power ne old.modata.power then begin
			printf, lun,'	Beam continuum power changed (Ref=',old.modata.power,'    new=',new.modata.power,').'
			bad = 1
		endif
		if new.modata.phi ne old.modata.phi then begin
			printf, lun,'	Beam continuum phi changed (Ref=',old.modata.phi,'    new=',new.modata.phi,').'
			bad = 1
		endif
		if new.modata.eps ne old.modata.eps then begin
			printf, lun,'	Beam continuum eps changed (Ref=',old.modata.eps,'    new=',new.modata.eps,').'
			bad = 1
		endif
		if new.modata.spot ne old.modata.spot then begin
			printf, lun,'	Beam continuum spot changed (Ref=',old.modata.spot,'    new=',new.modata.spot,').'
			bad = 1
		endif
		if new.modata.bin ne old.modata.bin then begin
			printf, lun,'	Beam continuum bin changed (Ref=',old.modata.bin,'    new=',new.modata.bin,').'
			bad = 1
		endif
		if new.modata.anode.formula ne old.modata.anode.formula then begin
			printf, lun,'	Beam continuum anode formula changed (Ref=',old.modata.anode.formula,'    new=',new.modata.anode.formula,').'
			bad = 1
		endif
		if new.modata.anode.weight ne old.modata.anode.weight then begin
			printf, lun,'	Beam continuum anode weight changed (Ref=',old.modata.anode.weight,'    new=',new.modata.anode.weight,').'
			bad = 1
		endif
		if new.mono.mode eq 1 then begin
			if new.modata.mono[0] ne old.modata.mono[0] then begin
				printf, lun,'	Beam continuum mono energy changed (Ref=',old.modata.mono[0],'    new=',new.modata.mono[0],').'
				bad = 1
			endif
			if new.modata.mono[1] ne old.modata.mono[1] then begin
				printf, lun,'	Beam continuum mono bandwidth changed (Ref=',old.modata.mono[1],'    new=',new.modata.mono[1],').'
				bad = 1
			endif
			if new.modata.mono[2] ne old.modata.mono[2] then begin
				printf, lun,'	Beam continuum mono efficiency changed (Ref=',old.modata.mono[2],'    new=',new.modata.mono[2],').'
				bad = 1
			endif
		endif
		if new.poly.mode eq 1 then begin
			if new.poly.gain ne old.poly.gain then begin
				printf, lun,'	Beam continuum poly.gain changed (Ref=',old.poly.gain,'    new=',new.poly.gain,').'
				bad = 1
			endif
			if new.poly.energy ne old.poly.energy then begin
				printf, lun,'	Beam continuum poly.energy changed (Ref=',old.poly.energy,'    new=',new.poly.energy,').'
				bad = 1
			endif
			if new.poly.model ne old.poly.model then begin
				printf, lun,'	Beam continuum poly.model changed (Ref=',old.poly.model,'    new=',new.poly.model,').'
				bad = 1
			endif
			if new.poly.diameter ne old.poly.diameter then begin
				printf, lun,'	Beam continuum poly.diameter changed (Ref=',old.poly.diameter,'    new=',new.poly.diameter,').'
				bad = 1
			endif
			if new.poly.focus ne old.poly.focus then begin
				printf, lun,'	Beam continuum poly.focus changed (Ref=',old.poly.focus,'    new=',new.poly.focus,').'
				bad = 1
			endif
			if new.poly.spot ne old.poly.spot then begin
				printf, lun,'	Beam continuum poly.spot changed (Ref=',old.poly.spot,'    new=',new.poly.spot,').'
				bad = 1
			endif
			if new.poly.pinhole ne old.poly.pinhole then begin
				printf, lun,'	Beam continuum poly.pinhole changed (Ref=',old.poly.pinhole,'    new=',new.poly.pinhole,').'
				bad = 1
			endif
			if new.poly.distance ne old.poly.distance then begin
				printf, lun,'	Beam continuum poly.distance changed (Ref=',old.poly.distance,'    new=',new.poly.distance,').'
				bad = 1
			endif
		endif
		
		if new.n_filters ne old.n_filters then begin
			printf, lun,'	Filter number changed (Ref=',old.n_filters,'    new=',new.n_filters,').'
			bad = 1
		endif
		for l=0,new.n_filters-1 do begin
			if new.n_filters le old.n_filters then begin
				if new.filters[l].formula ne old.filters[l].formula then begin
					printf, lun,'	Filter '+str_tidy(l)+' formula changed (Ref=',old.filters[l].formula,'    new=',new.filters[l].formula,').'
					bad = 1
				endif
				if new.filters[l].thick ne old.filters[l].thick then begin
					printf, lun,'	Filter '+str_tidy(l)+' thickness changed (Ref=',old.filters[l].thick,'    new=',new.filters[l].thick,').'
					bad = 1
				endif
				if new.filters[l].density ne old.filters[l].density then begin
					printf, lun,'	Filter '+str_tidy(l)+' density changed (Ref=',old.filters[l].density,'    new=',new.filters[l].density,').'
					bad = 1
				endif
				if new.filters[l].weight ne old.filters[l].weight then begin
					printf, lun,'	Filter '+str_tidy(l)+' weight changed (Ref=',old.filters[l].weight,'    new=',new.filters[l].weight,').'
					bad = 1
				endif
				if new.filters[l].microns ne old.filters[l].microns then begin
					printf, lun,'	Filter '+str_tidy(l)+' microns changed (Ref=',old.filters[l].microns,'    new=',new.filters[l].microns,').'
					bad = 1
				endif
				if new.filters[l].pinhole ne old.filters[l].pinhole then begin
					printf, lun,'	Filter '+str_tidy(l)+' pinhole changed (Ref=',old.filters[l].pinhole,'    new=',new.filters[l].pinhole,').'
					bad = 1
				endif
			endif
		endfor
		
		if new.spectrum.n ne old.spectrum.n then begin
			printf, lun,'	Derived spectrum size (N) changed (Ref=',old.spectrum.n,'    new=',new.spectrum.n,').'
			bad = 1
		endif
		if new.spectrum.cal.a ne old.spectrum.cal.a then begin
			printf, lun,'	Derived spectrum cal A changed (Ref=',old.spectrum.cal.a,'    new=',new.spectrum.cal.a,').'
			bad = 1
		endif
		if new.spectrum.cal.b ne old.spectrum.cal.b then begin
			printf, lun,'	Derived spectrum cal B changed (Ref=',old.spectrum.cal.b,'    new=',new.spectrum.cal.b,').'
			bad = 1
		endif

		if bad then begin
;		return
		endif else begin
			printf, lun,'Source Parameters consistent.'
		endelse

		sig = sig_change( old.spectrum.e, new.spectrum.e, tol=0.001, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'	Derived Beam spectrum E not consistent ...'
			printf, lun,'          Index       Ref       New'
			for j=0,nq-1 do begin
				printf, lun, q[j], old.spectrum.e[q[j]], new.spectrum.e[q[j]], $
					format='(6x,I7,3x,G10.3,G10.3)'
			endfor
		endif
		sig = sig_change( old.spectrum.data, new.spectrum.data, tol=0.001, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'	Derived Beam spectrum Data not consistent ...'
			printf, lun,'          Index     E        Ref       New'
			for j=0,nq-1 do begin
				printf, lun, q[j], old.spectrum.e[q[j]], old.spectrum.data[q[j]], new.spectrum.data[q[j]], $
					format='(6x,I7,3x,F7.3,3x,G10.3,G10.3)'
			endfor
		endif
		sig = sig_change( old.spectrum.char, new.spectrum.char, tol=0.001, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'        Derived Beam spectrum Characteristic line spectrum not consistent ...'
			printf, lun,'          Index     E        Ref       New'
			for j=0,nq-1 do begin
				printf, lun, q[j], old.spectrum.e[q[j]], old.spectrum.char[q[j]], new.spectrum.char[q[j]], $
					format='(6x,I7,3x,F7.3,3x,G10.3,G10.3)'
			endfor
		endif
		
		n_lines = n_elements( new.lines.line[*,0])
		n_z = n_elements( new.lines.z)
		x = fltarr( n_lines, n_z)
		y = fltarr( n_lines, n_z)
		z = new.lines.z
		shell = new.lines.shell
		lines = new.lines.line
		e = new.lines.e
		for i=0,n_z-1 do begin
			q = where( (old.lines.z eq z[i]) and (old.lines.shell eq shell[i]), nq)
			if nq ge 1 then begin
				for k=0,new.lines.n_lines[i]-1 do begin
					q1 = where( old.lines.line[*,q[0]] eq lines[k,i], nq1)
					if nq1 gt 0 then begin
						x[k,i] = old.lines.e[q1[0],q[0]]
						y[k,i] = e[k,i]
					endif else begin
;						warning,'compare_source','Element Z='+str_tidy(new.lines.z[i])+', shell='+str_tidy(new.lines.shell[i])+', line='+str_tidy(new.lines.line[k,i])+', E='+str_tidy(new.lines.e[k,i])+' not found in reference.'
						printf, lun,'	Element Z='+str_tidy(new.lines.z[i])+' ('+element_name(new.lines.z[i])+'), shell='+str_tidy(new.lines.shell[i])+', line='+str_tidy(new.lines.line[k,i])+' ('+line_id(new.lines.line[k,i])+'), E='+str_tidy(new.lines.e[k,i])+' not found in reference.'
					endelse
				endfor
			endif
		endfor
		sig = sig_change( x, y, tol=0.001, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			q_to_xy, q, n_lines, il,iz
			printf, lun,'        Derived Characteristic line energies not consistent ...'
			printf, lun,'         Index    Z    Name  Shell  Line   Name      E        Ref        New'
			for j=0,nq-1 do begin
				printf, lun, q[j], z[iz[j]], element_name(z[iz[j]]), shell[iz[j]], lines[il[j],iz[j]], line_id(lines[il[j],iz[j]]), e[il[j],iz[j]], x[il[j],iz[j]], y[il[j],iz[j]], $
					format='(7x,I6,I6,3x,A4,I6,I7,A8,2x,F8.3,G11.4,G11.4)'
			endfor
		endif
					
		n_lines = n_elements( new.lines.line[*,0])
		n_z = n_elements( new.lines.z)
		x = fltarr( n_lines, n_z)
		y = fltarr( n_lines, n_z)
		z = new.lines.z
		shell = new.lines.shell
		lines = new.lines.line
		e = new.lines.e
		for i=0,n_z-1 do begin
			q = where( (old.lines.z eq z[i]) and (old.lines.shell eq shell[i]), nq)
			if nq ge 1 then begin
				for k=0,new.lines.n_lines[i]-1 do begin
					q1 = where( old.lines.line[*,q[0]] eq lines[k,i], nq1)
					if nq1 gt 0 then begin
						x[k,i] = old.lines.rel[q1[0],q[0]]
						y[k,i] = new.lines.rel[k,i]
					endif else begin
						;						warning,'compare_source','Element Z='+str_tidy(new.lines.z[i])+', shell='+str_tidy(new.lines.shell[i])+', line='+str_tidy(new.lines.line[k,i])+', E='+str_tidy(new.lines.e[k,i])+' not found in reference.'
						printf, lun,'	Element Z='+str_tidy(new.lines.z[i])+' ('+element_name(new.lines.z[i])+'), shell='+str_tidy(new.lines.shell[i])+', line='+str_tidy(new.lines.line[k,i])+' ('+line_id(new.lines.line[k,i])+'), E='+str_tidy(new.lines.e[k,i])+' not found in reference.'
					endelse
				endfor
			endif
		endfor
		sig = sig_change( x, y, tol=0.001, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			q_to_xy, q, n_lines, il,iz
			printf, lun,'        Derived Characteristic line Intensities not consistent ...'
			printf, lun,'         Index    Z    Name  Shell  Line   Name      E        Ref        New'
			for j=0,nq-1 do begin
				printf, lun, q[j], z[iz[j]], element_name(z[iz[j]]), shell[iz[j]], lines[il[j],iz[j]], line_id(lines[il[j],iz[j]]), e[il[j],iz[j]], x[il[j],iz[j]], y[il[j],iz[j]], $
					format='(7x,I6,I6,3x,A4,I6,I7,A8,2x,F8.3,G11.4,G11.4)'
			endfor
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

bad_input:
	printf, lun,'compare_source: Inconsistent input parameters.'
	err = 1
	return
bad_open:
	printf, lun,'compare_source: Error opening output file: '+output
	err = 1
	return
bad_write:
	printf, lun,'compare_source: Error writing to output file: '+output
	if local then close_file, lun
	err = 1
	return
end
