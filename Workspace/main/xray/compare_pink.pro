pro compare_pink, files, output, error=err, bad=bad, lun=lun, tol=tol

; Compare two pink beam files for consistency.
; Check input parameters first, such as source, continuum spectrum, filters.
;
; NOTE: This command is suitable to execute directly using the callable GeoPIXE approach,
;		passing the two file names to be compared.
;
; 'files' are input yield file names (2) to compare. Prompts for these if missing.
; 'output' is a txt file report to create.
; 'lun=lun'	if a logical unit is input, then ignore 'output' and write/append to that file.
; 
; 'err=1'	error reading yield files, or some crash.
; 'bad=1'	an input parameter difference found.
; 'tol'		fractional tolerance to use for yiel

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
			warning,'compare_pink',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			err = 1
			return
		endif
	endif
	local = 1

; Use tol = 0.001 as default for model value tests

	if n_elements(tol) eq 0 then tol = 0.001
	if n_elements(lun) ne 0 then local=0
	
	err = 1
	if n_elements(files) eq 0 then begin
		if local eq 0 then goto, bad_input
		fold = file_requester( /read, filter='*.pink', /fix_filter, title='Reference PINK beam file.')
		if fold eq '' then return
	endif else fold=files[0]
	if n_elements(files) lt 2 then begin
		if local eq 0 then goto, bad_input
		path = extract_path(fold)
		fnew = file_requester( /read, path=path, filter='*.pink', /fix_filter, title='New PINK beam file to check.')
		if fnew eq '' then return
	endif else fnew=files[1]

	if typevar(fnew) eq 'STRUCT' then begin
		new = fnew
	endif else begin
		new = read_pink( fnew, error=err)
		if err then begin
			warning, 'compare_pink','Failed to read NEW file: '+fnew
			return
		endif
	endelse
	if typevar(fold) eq 'STRUCT' then begin
		old = fold
	endif else begin
		old = read_pink( fold, error=err)
		if err then begin
			warning, 'compare_pink','Failed to read REFERENCE file: '+fold
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
		printf, lun,'Setup PINK beam parameters ...'
	endif
	bad = 0

	if new.continuum eq 0 then begin
		goto, done
	endif else begin
		if old.model ne 2 then begin
			printf, lun,'	Pink beam continuum model was wrong (should be "2", Ref=',old.model,').'
			bad = 1
		endif
		if new.model ne 2 then begin
			printf, lun,'	Pink beam continuum model now wrong (should be "2", new=',new.model,').'
			bad = 1
		endif
		if new.model ne old.model then begin
			printf, lun,'	Pink beam continuum model changed (Ref=',old.model,'    new=',new.model,').'
			bad = 1
		endif
		if new.energy ne old.energy then begin
			printf, lun,'	Pink beam continuum energy changed (Ref=',old.energy,'    new=',new.energy,').'
			bad = 1
		endif
		if strip_path(new.fe_spectrum_file) ne strip_path(old.fe_spectrum_file) then begin
			printf, lun,'	Pink beam FE spectrum file changed (Ref=',strip_path(old.fe_spectrum_file),'    new=',strip_path(new.fe_spectrum_file),').'
			bad = 1
		endif
		if new.acceptance ne old.acceptance then begin
			printf, lun,'	Pink beam continuum acceptance changed (Ref=',old.acceptance,'    new=',new.acceptance,').'
			bad = 1
		endif
		if new.modata.spot ne old.modata.spot then begin
			printf, lun,'	Pink beam continuum spot changed (Ref=',old.modata.spot,'    new=',new.modata.spot,').'
			bad = 1
		endif
		if new.modata.bin ne old.modata.bin then begin
			printf, lun,'	Pink beam continuum bin changed (Ref=',old.modata.bin,'    new=',new.modata.bin,').'
			bad = 1
		endif
		if new.mono.mode eq 1 then begin
			if new.modata.mono[0] ne old.modata.mono[0] then begin
				printf, lun,'	Pink beam continuum mono energy changed (Ref=',old.modata.mono[0],'    new=',new.modata.mono[0],').'
				bad = 1
			endif
			if new.modata.mono[1] ne old.modata.mono[1] then begin
				printf, lun,'	Pink beam continuum mono bandwidth changed (Ref=',old.modata.mono[1],'    new=',new.modata.mono[1],').'
				bad = 1
			endif
			if new.modata.mono[2] ne old.modata.mono[2] then begin
				printf, lun,'	Pink beam continuum mono efficiency changed (Ref=',old.modata.mono[2],'    new=',new.modata.mono[2],').'
				bad = 1
			endif
		endif
;		if new.poly.mode eq 1 then begin
;			if new.poly.gain ne old.poly.gain then begin
;				printf, lun,'	Pink beam continuum poly.gain changed (Ref=',old.poly.gain,'    new=',new.poly.gain,').'
;				bad = 1
;			endif
;			if new.poly.energy ne old.poly.energy then begin
;				printf, lun,'	Pink beam continuum poly.energy changed (Ref=',old.poly.energy,'    new=',new.poly.energy,').'
;				bad = 1
;			endif
;			if new.poly.model ne old.poly.model then begin
;				printf, lun,'	Pink beam continuum poly.model changed (Ref=',old.poly.model,'    new=',new.poly.model,').'
;				bad = 1
;			endif
;			if new.poly.diameter ne old.poly.diameter then begin
;				printf, lun,'	Pink beam continuum poly.diameter changed (Ref=',old.poly.diameter,'    new=',new.poly.diameter,').'
;				bad = 1
;			endif
;			if new.poly.focus ne old.poly.focus then begin
;				printf, lun,'	Pink beam continuum poly.focus changed (Ref=',old.poly.focus,'    new=',new.poly.focus,').'
;				bad = 1
;			endif
;			if new.poly.spot ne old.poly.spot then begin
;				printf, lun,'	Pink beam continuum poly.spot changed (Ref=',old.poly.spot,'    new=',new.poly.spot,').'
;				bad = 1
;			endif
;			if new.poly.pinhole ne old.poly.pinhole then begin
;				printf, lun,'	Pink beam continuum poly.pinhole changed (Ref=',old.poly.pinhole,'    new=',new.poly.pinhole,').'
;				bad = 1
;			endif
;			if new.poly.distance ne old.poly.distance then begin
;				printf, lun,'	Pink beam continuum poly.distance changed (Ref=',old.poly.distance,'    new=',new.poly.distance,').'
;				bad = 1
;			endif
;		endif
		
		if new.n_mirrors ne old.n_mirrors then begin
			printf, lun,'	Filter number changed (Ref=',old.n_mirrors,'    new=',new.n_mirrors,').'
			bad = 1
		endif
		for l=0,new.n_mirrors-1 do begin
			if new.n_mirrors le old.n_mirrors then begin
				if new.mirrors[l].title ne old.mirrors[l].title then begin
					printf, lun,'	Filter '+str_tidy(l)+' title changed (Ref=',old.mirrors[l].title,'    new=',new.mirrors[l].title,').'
					bad = 1
				endif
				if strip_path(new.mirrors[l].file) ne strip_path(old.mirrors[l].file) then begin
					printf, lun,'	Filter '+str_tidy(l)+' file changed (Ref=',strip_path(old.mirrors[l].file),'    new=',strip_path(new.mirrors[l].file),').'
					bad = 1
				endif
			endif
		endfor

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
			printf, lun,'PINK beam Parameters consistent.'
		endelse

		sig = sig_change( old.spectrum.e, new.spectrum.e, tol=tol, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'	Derived Pink beam spectrum E not consistent ...'
			printf, lun,'          Index       Ref       New'
			for j=0,nq-1 do begin
				printf, lun, q[j], old.spectrum.e[q[j]], new.spectrum.e[q[j]], $
					format='(6x,I7,3x,G10.3,G10.3)'
			endfor
		endif
		sig = sig_change( old.spectrum.data, new.spectrum.data, tol=tol, error=err1)
		q = where( sig ne 0, nq)
		if nq gt 0 then begin
			printf, lun,'	Derived Pink beam spectrum Data not consistent ...'
			printf, lun,'          Index     E        Ref       New'
			for j=0,nq-1 do begin
				printf, lun, q[j], old.spectrum.e[q[j]], old.spectrum.data[q[j]], new.spectrum.data[q[j]], $
					format='(6x,I7,3x,F7.3,3x,G10.3,G10.3)'
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
	printf, lun,'compare_pink: Inconsistent input parameters.'
	err = 1
	return
bad_open:
	printf, lun,'compare_pink: Error opening output file: '+output
	err = 1
	return
bad_write:
	printf, lun,'compare_pink: Error writing to output file: '+output
	if local then close_file, lun
	err = 1
	return
end
