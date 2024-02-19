pro depth_ratio_yields, pdat=p, group_leader=group, path=path

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
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
       warning,'depth_ratio_yields',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, finish
    endif
endif
;startupp, /database, /colours, error=0
if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=geopixe_root

	if ptr_good(p) then begin
		Ffilter = (*p).file[0]
		Fyields = (*p).file[1]
		Factive1 = (*p).file[2]
		Factive2 = (*p).file[3]
		output = (*p).file[4]
	endif else begin
		Ffilter = 'C:\software\IDL\GeoPIXE-source2\Workspace\GeoPIXE\Air-5mm.filter'
		Fyields = 'C:\NMP\AS\June-2013\Godel_6618\analysis\PGM-in-olivine_18_5KeV-depth-steps.yield'
		Factive1 = 'C:\NMP\AS\June-2013\Godel_6618\analysis\outer2b.select.csv'	; outer
		Factive2 = 'C:\NMP\AS\June-2013\Godel_6618\analysis\inner2.select.csv'	; inner
		output = 'C:\NMP\AS\June-2013\Godel_6618\analysis\PGM-in-olivine_18_5KeV-depth-steps.csv'
	endelse	
	
	file = ['Filters','Yields','"Outer"','"Inner"','Output']
	initial_file = [Ffilter,Fyields,Factive1,Factive2,output]
	help_file = ['Filter(s) file.','Yields file for depth series, with a layer #1 depth range as a +1D grid series.', $
			'File (".select.csv" or ".spec") to select "outer" detector elements.', 'File (".select.csv" or ".spec") to select "inner" detector elements.', $
			'Output file name (.csv table), for a table of the ratio "outer"/"inner" yields as a function of depth (thickness of layer #1).' ]
	
	r = options_popup( group, title='Depth Profile Calculation',file=file, initial_file=initial_file, help_file=help_file, $
			min_xsize=450, path=path, debug=0, error=error)
	if r.error then goto, finish

	Ffilter = r.file[0]
	Fyields = r.file[1]
	Factive1 = r.file[2]
	Factive2 = r.file[3]
	output = r.file[4]
	
	pfilter = read_filters( Ffilter, error=error)
	if error then begin
		warning, 'depth_ratio_yields','Error reading Filter file '+Ffilter, /error
		goto, finish
	endif
	
	pyields = read_yield(Fyields, error=error, /many)
	if error then begin
		warning, 'depth_ratio_yields','Error reading Yield file: '+Fyields, /error
		goto, finish
	endif else begin
		if (*pyields)[0].array then begin
			fdetector = strip_path((*pyields)[0].detector_file)
			detector_update, present=fdetector, new=i, file=f
			pdetector = read_detector( f, error=error)
			if error then begin
				warning, 'depth_ratio_yields','Error reading Detectors file: '+fdetector, /error
				goto, finish
			endif else begin
				if (*pdetector).array and (strlen((*pdetector).layout) gt 0) then begin
					d = read_detector_layout( (*pdetector).layout, error=error)
					if error then begin
						warning, 'depth_ratio_yields','Error reading Detectors layout file: '+(*pdetector).layout, /error
						goto, finish
					endif else begin
						playout = ptr_new( d, /no_copy)
					endelse
				endif else begin
					warning, 'depth_ratio_yields',['Error reading Detectors layout file.','Not an array or no layout found.'], /error
					goto, finish
				endelse				
			endelse
		endif else begin
			warning, 'depth_ratio_yields',['Error in yields file.','Not a detector array yield file.'], /error
			goto, finish
		endelse				
	endelse

	ext = extract_extension(Factive1[0])
	case strupcase(ext) of
	'SPEC': begin
		pp = read_spec(Factive1)
		if ptr_valid(pp[0]) eq 0 then begin
			warning, 'depth_ratio_yields','error reading "active1" spectrum.', /error
			goto, finish
		endif
		npp = n_elements(pp)
		if (*pp[0]).array then begin
			if ptr_good( (*pp[0]).pactive) then begin
				n = *(*pp[0]).pactive
			endif else begin
				for j=0L,npp-1 do begin
					free_spectrum, pp[j]
				endfor
				goto, finish
			endelse
		endif else begin
			n = intarr(npp)
			for j=0L,npp-1 do begin
				if ptr_good(pp[j]) then n[j] = (*pp[j]).station + adc_offset_device( (*pp[j]).DevObj)
			endfor
		endelse

		for j=0L,npp-1 do begin
			free_spectrum, pp[j]
		endfor
		end
	'CSV': begin
		n = get_select( Factive1[0], error=err)
		if err or (n[0] eq -1) then begin
			warning, 'depth_ratio_yields','error reading "active2" .select.csv file.', /error
			goto, finish
		endif
		end
	endcase
	active1 = n

	ext = extract_extension(Factive2[0])
	case strupcase(ext) of
	'SPEC': begin
		pp = read_spec(Factive2)
		if ptr_valid(pp[0]) eq 0 then begin
			warning, 'depth_ratio_yields','error reading "active2" spectrum.', /error
			goto, finish
		endif
		npp = n_elements(pp)
		if (*pp[0]).array then begin
			if ptr_good( (*pp[0]).pactive) then begin
				n = *(*pp[0]).pactive
			endif else begin
				for j=0L,npp-1 do begin
					free_spectrum, pp[j]
				endfor
				goto, finish
			endelse
		endif else begin
			n = intarr(npp)
			for j=0L,npp-1 do begin
				if ptr_good(pp[j]) then n[j] = (*pp[j]).station + adc_offset_device((*pp[j]).DevObj)
			endfor
		endelse

		for j=0L,npp-1 do begin
			free_spectrum, pp[j]
		endfor
		end
	'CSV': begin
		n = get_select( Factive2[0], error=err)
		if err or (n[0] eq -1) then begin
			warning, 'depth_ratio_yields','error reading "active2" .select.csv file.', /error
			goto, finish
		endif
		end
	endcase
	active2 = n
	
	progress, tlb=progress_tlb, title='GeoPIXE: PIXE/SXRF Array depth profile Calculation'

	openw, 1, output
	printf,1, 'Table of ratio versus depth for each element'
	printf,1, 'Z, ' + strjoin( (*pyields)[0].z, ', ')
	printf,1, 'element, ' + strjoin( element_name((*pyields)[0].z), ', ')
	printf,1, 'shell, ' + strjoin( (*pyields)[0].shell, ', ')
	for i=0,n_elements(*pyields)-1 do begin
		progress, /update, progress_tlb, {unit:0, value:0, current:i, size:n_elements(*pyields)}, cancel=cancel
		if cancel then break

		ratio = ratio_detector_yields( pdetector, playout, pfilter, ptr_new((*pyields)[i]), $
						active1, active2)
		printf,1, string( (*pyields)[i].thick[0]) + ', ' + strjoin( ratio, ', ')
	endfor
	
	progress, /complete, progress_tlb, 'Yield calculation completed.'
	progress, /ending, progress_tlb
	if ptr_valid(p) then *p = r

finish:
	close_file, 1
	return
end
