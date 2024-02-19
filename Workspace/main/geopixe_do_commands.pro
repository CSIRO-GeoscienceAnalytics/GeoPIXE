pro geopixe_do_replace, par, str, new, non_zero=non_zero

;	Replace any detected 'par=' term in 'str' with the 'new' value, stringified.
;
;	/non_zero	only make change if 'new' is non-zero (numbers only).

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
			warning,'geopixe_do_replace',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(non_zero) eq 0 then non_zero=0
	if n_elements(par) eq 0 then return
	if n_elements(str) eq 0 then return
	if n_elements(new) eq 0 then return
	if strlen(par) eq 0 then return

	ns = strlen(par)
	q = where( strmid(str,0,ns+1) eq par+'=', nq)
	if nq eq 0 then return

	if non_zero then begin
		if float2(new) gt 1.0e-10 then begin
			str[q[0]] = par+'=' + stringify(new)
		endif
	endif else begin
		str[q[0]] = par+'=' + stringify(new)
	endelse
	return
end

;-------------------------------------------------------------------------------------

function geopixe_do_found, par, str, q=q, found=found

;	Search for 'par' key (i.e. "par=") in 'str' vector. 
;	If found, return where indices in 'q' vector and the terms found in 'found' vector.
;	
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
			warning,'geopixe_do_found',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0
		endif
	endif

	q = -1
	found = ''
	if n_elements(par) eq 0 then return, 0
	if n_elements(str) eq 0 then return, 0 
	if strlen(par) eq 0 then return, 0

	ns = strlen(par)
	q = where( strmid( str,0,ns+1) eq par+'=', nq)

	if nq ge 1 then found = strmid( str[q], ns+1)

	return, (nq ge 1)
end

;-------------------------------------------------------------------------------------

pro geopixe_do_commands, argc=argc, argv=argv, error=error, cluster=cluster_override

; Read GCF command file and execute the enclosed command and arguments.
;			Needs extra work for regions spectra, as they may need multipe passes.
;			The GCF file is created in "geopixe_gen_commands".
; 
; argv[0]	command file
; argv[1]	optional file list to replace "files=" line in GCF file.
;			May contain wild-cards ("*", "?").
;			Preceed with "@" to supply a file-name containing the file-list,
;			with one input file-name per line.
; argv[2]	optional 'output' file-name to replace "output=" line in GCF file.
;			usually, both argv[1] and argv[2] will be supplied together.
;
; File list should be in stringify format ( '["file1","file2", ...]' or "file1" ). 
; Will there be a character limit? ; If so, then use the alternate approach 
; (e.g. argv[1] = "@file", where "@" signifies reading the file list from this "file")
; or use a wild-card.
;
; cluster=0,1 to override setting in command file.
 
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
			warning,'geopixe_do_commands',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

;	For testing, simply code in GCF file, input, output files in the argv and argc ...
;	Comment out all for normal compilation.

;	argv=[ "C:\Software\Demo\Maia\Test-Standards\analysis\level2\112658\112658-sort-images.gcf", $
;		"C:\Software\Demo\Maia\Test-Standards\raw\level2\112663\112663.*", $
;;		"@C:\Software\Demo\Maia\Test-Standards\analysis\level2\112663\112663-files.txt", $
;;		"C:\Software\Demo\Maia\Test-Standards\raw\level2\112677\112677.*", $
;		"C:\Software\Demo\Maia\Test-Standards\analysis\level2\112663\112663.dai"]
;;		"C:\Software\Demo\Maia\Test-Standards\analysis\level2\112677\112677.dai"]
;	argc = 3

;	argv=[ "C:\Software\Demo\Maia\Test-Standards\analysis\level2\112658\112658-region-spectra.gcf", $
;		"C:\Software\Demo\Maia\Test-Standards\raw\level2\112658\112658.*", $
;		"C:\Software\Demo\Maia\Test-Standards\analysis\level2\112658\test-q1.spec"]
;	argc = 3

;	argv = [ "C:\Software\Demo\MM\analysis\924\924-export-zarr.gcf", $
;			"C:\Software\Demo\MM\analysis\924\924-x.dai", $
;			"C:\Software\Demo\MM\analysis\924\924-x.zarr" ]
;	argc = 3

;	argv=[ "C:\Software\Demo\Maia\Rock3\mpdam\analysis\111163\111163-mpdam5-correct4.gcf", $
;		"C:\Software\Demo\Maia\Rock3\blog\111163\111163.*", $
;		"C:\Software\Demo\Maia\Rock3\mpdam\analysis\111163\test-workflow-MPDA.dai"]
;	argc = 3

;	argv = [ "C:\Software\Demo\MM\analysis\925\image-history.gcf" , $
;			"C:\Software\Demo\MM\analysis\925\925-Bi-x.dai" , $
;			"C:\Software\Demo\MM\analysis\925\925-Bi-x-metadata2.txt"]
;	argc = 3

	error = 1
	print, 'ARGV:'
	for i=0,argc-1 do begin
		print,i,'  ',argv[i]
	endfor
	q = where( argv ne '', argc)
	if argc ge 1 then argv = argv[q]

	if n_elements(argc) lt 1 then goto, bad_args
	if n_elements(argv) lt argc then goto, bad_args

	output = ''
	ixsize = 0
	iysize = 0

;	Regions use a special processing which allows multiple passes.
;	All other processing will be simpler (see below).
;
;	Currently, 'spec_evt' is equated with regions processing. If we
;	also use 'spec_evt' to "import spectra" (no regions), then additional
;	tests (lack of "mfile=") are used here to direct the import to the 
;	simpler code along with images, etc. 

	regions = ['spec_evt']
	images = ['da_evt','cut_evt','da_stack_evt','da_tomo_evt']
	export = ['export_zarr','print_image_metadata']

  	prefs = geopixe_defaults( error=err, source='geopixe_do_commands')
	cluster_prefs = prefs.cluster

	str = strarr(1000)
	n_files = 0
	s = ''
	cluster = 0
	mask_file = ''
	sub_region = 0

	print,'GeoPIXE Command File: '+argv[0]
	on_ioerror, bad_file
 	openr, unit, argv[0], /get_lun

	on_ioerror, bad_read
	readf, unit, str

bad_read:
	close_file, unit
	q = where( (str ne '') and (strmid(str,0,1) ne '#'), ns)
	if ns eq 0 then goto, bad_data
	str = str[q]
	command = str[0]

	do_images = ((where( command eq images))[0] ne -1)
	do_regions = ((where( command eq regions))[0] ne -1)
	do_export = ((where( command eq export))[0] ne -1)
	do_header = do_images or do_regions

;	Create a device object based on input parameters (if found) ...

	device = ''
	devpars = ''
	if geopixe_do_found( 'device', str, q=q, found=found) then begin
		device = unstringify( found)
	endif
	if geopixe_do_found( 'devpars', str, q=q, found=found) then begin
		devpars = unstringify( found)
	endif
	if device ne '' then begin
		obj = new_device_object( device, options=devpars, error=err)
		if err then goto, bad_obj
	endif

;	Override input file list using argv[1] ...

	found_files = geopixe_do_found( 'files', str, q=q, found=found)
	files_index = q[0]
	if argc ge 2 then begin
		print,'Use input files: '+argv[1]
		if strmid( argv[1],0,1) eq '@' then begin
			str2 = strarr(1000)
			on_ioerror, bad_file2
		 	openr, unit2, strmid( argv[1], 1), /get_lun
			on_ioerror, bad_read2
			readf, unit2, str2
bad_read2:
			close_file, unit2
			q2 = where( (str2 ne ''), ns2)
			if ns2 eq 0 then goto, bad_filelist
			s = str2[q2]

		endif else begin
			s = unstringify( argv[1])
			if n_elements(s) eq 1 then begin
				s = file_search(s)
				if s[0] eq '' then goto, bad_input
			endif
		endelse
		n_files = n_elements(s)
		file1 = s[0]
		evt_files0 = s

		if found_files then begin
			str[files_index] = 'files=' + stringify(s)
		endif else begin
			str = [str, 'files=' + stringify(s)]
			files_index = n_elements(str)-1
		endelse

;		Read input data file header to set certain key parameters ...
;		Need to adapt to the scan size and metadata of the new data/scan.
 
		if do_header then begin
			mp = get_header_info( obj, file1, /silent, dir_mode=0, error=error)
			if error eq 0 then begin
				ok1 = geopixe_do_found( 'x_sub_range', str, q=q1, found=found1)
				ok2 = geopixe_do_found( 'xrange', str, q=q2, found=found2)
				if ok1 and ok2 then begin
					if unstringify(found1) ne unstringify(found2) then sub_region = 1
				endif
				ok1 = geopixe_do_found( 'y_sub_range', str, q=q1, found=found1)
				ok2 = geopixe_do_found( 'yrange', str, q=q2, found=found2)
				if ok1 and ok2 then begin
					if unstringify(found1) ne unstringify(found2) then sub_region = 1
				endif
				geopixe_do_replace, 'xrange', str, mp.scan.x_pixels, /non_zero
				geopixe_do_replace, 'yrange', str, mp.scan.y_pixels, /non_zero
				geopixe_do_replace, 'zrange', str, mp.scan.z_pixels, /non_zero
				if sub_region eq 0 then begin
					geopixe_do_replace, 'x_sub_range', str, mp.scan.x_pixels, /non_zero
					geopixe_do_replace, 'y_sub_range', str, mp.scan.y_pixels, /non_zero
					geopixe_do_replace, 'xoffset', str, 0
					geopixe_do_replace, 'yoffset', str, 0
				endif
				geopixe_do_replace, 'scanx', str, mp.scan.x_mm * 1000., /non_zero
				geopixe_do_replace, 'scany', str, mp.scan.y_mm * 1000., /non_zero
				geopixe_do_replace, 'scanz', str, mp.scan.z_mm * 1000., /non_zero
				geopixe_do_replace, 'xorigin', str, mp.scan.x
				geopixe_do_replace, 'yorigin', str, mp.scan.y
				geopixe_do_replace, 'zorigin', str, mp.scan.z
		
				geopixe_do_replace, 'sample', str, mp.sample
				geopixe_do_replace, 'grain', str, mp.grain
				geopixe_do_replace, 'comment', str, mp.title
				geopixe_do_replace, 'facility', str, mp.metadata.facility
				geopixe_do_replace, 'endstation', str, mp.metadata.endstation
		
				ixsize = mp.scan.x_pixels
				iysize = mp.scan.y_pixels

;				What about pileup, throttle, linear?
;				No keep to the template. This acts like Sort EVT window (evt.pro), where we
;				typically load new raw data for a series of scans and keep using the current
;				settings for Throttle, Pileup, Linearize, E Cal, etc.

				geopixe_do_replace, 'charge', str, mp.charge, /non_zero
		
				if geopixe_do_found( 'ic', str, q=q, found=found) then begin
					ic = unstringify( found)
		
					if mp.sensitivity gt 1.0e-30 then begin
						val = charge_gain_units( mp.sensitivity, units=unit)
						l = locate('time', strlowcase( ic.pv))
						ival = find_charge_val_unit_index( val, unit, iunit=iunit, /write_back, time=(l ge 0))
						ic.val = val
						ic.unit = unit
						if mp.IC_name ne '' then begin
							ic.pv = mp.IC_name
							ic.mode = 1
						endif
					endif
					if mp.scan.dwell gt 1.0e-6 then begin
						ic.dwell = mp.scan.dwell
					endif
					geopixe_do_replace, 'ic', str, ic
				endif
			endif
		endif

	endif else begin
		if found_files then begin
			s = unstringify( found)
			n_files = n_elements(s)
			file1 = s[0]
			evt_files0 = s
		endif
	endelse

;	Override output filename using argv[2] ...

	found_output = geopixe_do_found( 'output', str, q=q, found=found)
	if argc ge 3 then begin
		print,'Use output file: '+argv[2]
		if found_output then begin
			str[q[0]] = 'output=' + stringify(argv[2])
		endif else begin
			str = [str, 'output=' + stringify(argv[2])]
		endelse
		output = argv[2]
	endif else begin
		if found_output then output = unstringify( found[0])
	endelse
	if lenchr(output) gt 0 then begin
		if file_write_test( output) eq 0 then goto, bad_output				; this will blank the file?
	endif

;	Detect and remove any "Cluster" keyword ...

	if geopixe_do_found( 'cluster', str, q=q, found=found) then begin
		cluster = unstringify( found[0])
		str[q[0]] = ''
		q = where( str ne '', ns)
		if ns eq 0 then goto, bad_data
		str = str[q]
	endif
	if n_elements(cluster_override) ge 1 then cluster = cluster_override
	if n_elements( evt_files0) le 3 then cluster=0

;	Decide what type of processing is needed. "Regions" mode is different, as it  
;	may need multiple passes.

	if do_regions then begin						; regions
		if geopixe_do_found( 'mfile', str, found=found2) then begin
			mask_file = unstringify( found2)
		endif else goto, bad_mfile

		p = read_regions( mask_file)
		if ptr_valid( p[0]) eq 0 then goto, bad_region

;		It is possible to translate the 'q' index array for region masks to treat a
;		new scan (see code below). But this would require passing stringify(p regions), 
;		which can get extremely large for a large array of regions. Hence, we'll just
;		detect incompatibility and return an error.

		if ((*p[0]).nx ne ixsize) or ((*p[0]).ny ne iysize) then goto, bad_scan

;;		If using a region mask from a different scan, may need to correct for effect of
;;		scan size changes on the 'q' mask index array?
;
;		for i=0,n_elements(p)-1 do begin
;			p2 = do_translate_region( p[i], ixoffset=(*p[i]).xoffset, iyoffset=(*p[i]).yoffset, ixcompress=(*p[i]).xcompress, $
;									iycompress=(*p[i]).ycompress, ixsize=ixsize, error=error)
;			(*p2).nx = ixsize
;			(*p2).ny = iysize
;			(*p2).original_xsize = ixsize
;			(*p2).original_ysize = iysize
;			free_region, p[i]
;			p[i] = p2
;		endfor

		gd = geopixe_defaults()
		max_n_mask = clip( long( 0.4*gd.default.memory / ( float((*p[0]).nx) * float((*p[0]).ny))), 10, 10000)
;		max_n_mask = 3
		print,'geopixe_do_commands:, max_n_mask =',max_n_mask

		n_mask = n_elements( p)
		if n_mask eq 0 then goto, bad_region
		start_mask = 0
		final_mask = n_mask-1
		if (n_mask gt max_n_mask) then begin
			stop_mask = max_n_mask-1				; repeat extract for more regions ...

			warning,'geopixe_do_commands',['Very large images and long regions list.','','This extraction will be completed in', $
					str_tidy(1 + n_mask/max_n_mask) + ' passes.']
		endif else begin
			stop_mask = n_mask-1
		endelse
			
loop:
		if obj->ylut() then begin
			obj->change_options, 0					; stop changes to options (e.g. Maia flip.x)

;			Trim file list for pixels 'touched' by these masks (for this pass) ...

			fylut = extract_path(output) + strip_path( file1)
			head = get_header_info( obj, file1, output=fylut, /silent, error=err)
			if (err eq 0) then begin
				if (n_elements(*head.scan.pYlut) ge 1) then begin
					n1 = n_elements(evt_files0)
					pt = ptr_new( p[start_mask:stop_mask])
					evt_files = obj->trim_evt_files( evt_files0, mask=pt, pYlut=head.scan.pYlut)
					print, 'geopixe_do_commands: evt_files number before/after YLUT filter = ',n1, n_elements(evt_files)
				endif else begin
					print, 'geopixe_do_commands: null YLUT ?'
					goto, bad_ylut
					evt_files = evt_files0
				endelse
			endif else begin
				print, 'geopixe_do_commands: error reading YLUT'
				goto, bad_ylut
			endelse
			obj->change_options, 1
		endif else evt_files = evt_files0

		str[files_index] = 'files=' + stringify(evt_files)
		args = strjoin( str, ', ')

		if cluster then begin
			cluster_client, title = 'GeoPIXE Command File regions', $
;				group_leader = group, $
				subtitle = strip_path(output), $
				args = args, $
				n_files = n_files, $
				presult = presults, $
				prefs = cluster_prefs, $
				error = error
			if error then goto, bad_cluster
		
			if ptr_good(presults) then begin
				cluster_merge_spectra, presults, spectra=pp, error=error
				if error then goto, bad_merge
			endif else goto, bad_results
		
		endif else begin
			geopixe_execute, args, error=error, /progress, spectra=pp
			if error then goto, bad_execute
		endelse
		if error or ptr_good(pp) eq 0 then return

;		Accumulate spectra array results from passes ...

		if ptr_good(pp0) then begin
			*pp0 = [*pp0, *pp]
		endif else begin
			pp0 = pp
		endelse
		if stop_mask lt final_mask then begin				; repeat extract for more regions ...
			start_mask = stop_mask+1
			stop_mask = (start_mask + max_n_mask-1) < final_mask
			goto, loop
		endif else begin
			pp = pp0
			if ptr_good(pp) eq 1 then begin
				if var_type( *(*pp)[0]) eq 8 then begin
					write_spec, *pp, (*(*pp)[0]).file	
				endif
			endif
		endelse
		return
	endif

;	Processing mode for images ...

	if do_images then begin								; images

;		Check out matrix for MPDAM. If all good, proceed. If a MPDAM, but phases are not good, then need
;		to (i) perform normal DA, (ii) Correct images, (iii) Project minerals, (iv) save a new DAM file, then 
;		(v) proceed with MPDA.

		title = 'GeoPIXE GCF Imaging'

		if geopixe_do_found( 'matrix', str, q=q, found=mfile) then begin
			mpdam = unstringify( mfile)

			mpda = check_mpdam( mpdam, obj, raw=file1, stringed=mpdam_string, phase_good=phase_good, original=original, error=err)
			if err then goto, bad_matfile

			if mpda and (phase_good eq 0) then begin
				path = extract_path( output)

				geopixe_do_DA, str, obj, raw=evt_files0, stringed=mpdam_string, path=path, original=original, cluster=cluster, error=error
				if error then goto, bad_side_trip

				title = title +' (MPDA)'
			endif
		endif

		args = strjoin( str, ', ')
		if cluster then begin
			cluster_client, title = title, $
;			group_leader = group, $
				subtitle = strip_path(output), $
				args = args, $
				n_files = n_files, $
				presult = presults, $
				prefs = cluster_prefs, $
				error = error
			if error then goto, bad_cluster
		
			if ptr_good(presults) then begin
				cluster_merge_images, presults, error=error
				if error then goto, bad_merge
			endif else goto, bad_results
		
		endif else begin
			geopixe_execute, args, error=error, /progress
			if error then goto, bad_execute
		endelse

		return
	endif

;	Processing mode for export ... This catagory can also include any simple work-flow operations
;	that can be simply executed, as is, after changing "files=" and "output=" arguments.

	if do_export then begin							; export
		args = strjoin( str, ', ')
	
		geopixe_execute, args, error=error
		return
	endif

	return

bad_args:
	warning, 'geopixe_do_commands', 'Bad argc/argv argument list.'
	return
bad_file:
	warning, 'geopixe_do_commands', 'Error opening GCF file.'
	close_file, unit
	return
bad_matfile:
	warning, 'geopixe_do_commands', 'Error accessing DA file(s): '+mfile
	close_file, unit
	return
bad_ofile:
	warning, 'geopixe_do_commands', 'Missing "output" for MPDA tests.
	close_file, unit
	return
bad_input:
	warning, 'geopixe_do_commands', ['Input data files not found:', s]
	return
bad_file2:
	warning, 'geopixe_do_commands', 'Error opening file-list file.'
	close_file, unit2
	return
bad_filelist:
	warning, 'geopixe_do_commands', 'Bad file-list file.'
	return
bad_output:
	warning, 'geopixe_do_commands', 'Output file "'+output+'" not writable.'
	return
bad_data:
	warning, 'geopixe_do_commands', 'Bad read from GCF file.'
	close_file, unit
	return
bad_scan:
	warning,'',['Scan size for data file:', '"'+file1+'"', $
				'is not compatible with the chosen regions source data:', $
				'"'+(*p[0]).file+'".']
	return
bad_cluster:
	warning, 'geopixe_do_commands', 'Bad cluster run. Error returned.'
	return
bad_merge:
	warning, 'geopixe_do_commands', 'Bad merge of stripe data.'
	return
bad_results:
	warning, 'geopixe_do_commands', 'Bad results pointer.'
	return
bad_side_trip:
;	warning, 'geopixe_do_commands', 'Bad merge of stripe data.'
	return
bad_execute:
	warning, 'geopixe_do_commands', 'Bad execute. Error returned.'
	return
bad_region:
	warning, 'geopixe_do_commands', 'Bad regions file.'
	return
bad_mfile:
	warning, 'geopixe_do_commands', 'No "mfile=" region mask key file found.'
	return
bad_obj:
	warning, 'geopixe_do_commands', 'Bad or unknown device specified.'
	return
bad_ylut:
	warning, 'geopixe_do_commands', 'Bad Y lookup table.'
	return
bad_header:
	warning, 'geopixe_do_commands', 'Bad input file header read.'
	return
bad_load:
	warning, 'geopixe_do_commands', 'Bad load of DA images after Normal DA.'
	return
end
