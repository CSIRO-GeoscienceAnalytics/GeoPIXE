pro filter_update, list=filter_list, title=filter_title, $
					present=present, new=new, file=file, count=count

; Check for new filter files, and update local lists, and the
; current droplist settings.
;
; list		the new filter file list found (no paths)
; title		the new filter title strings
; count		total number found
;
; present	the name of the current filter file list member
; new		the new 'index' to the present filter within the new list returned
; file		return full filename with path too

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

	read_filts = 0
	count = 0
	if arg_present(filter_title) then read_filts=1

	filter_list = find_file2(geopixe_root+'*.filter')
	
	prefs = geopixe_defaults( source='filter_update')
	path = fix_path( prefs.path.config)
	path = fix_path( path + 'filters')
	filter_list2 = file_search( path, '*.filter')
	if filter_list2[0] ne '' then filter_list = [filter_list, filter_list2]
	
	filter_title = filter_list
	filter_title[*] = ''
	nf = n_elements( filter_list)
	count = nf
	ok = replicate( 1, nf)
	
	if filter_list[0] ne '' and read_filts then begin
		print,'filter_update:  process ',nf,' filter files ...'
		for i=0L,nf-1 do begin
			pf = read_filters( filter_list[i], error=error)
			if error eq 0 then begin
				filter_title[i] = (*pf)[0].name
				ok[i] = 1
			endif else begin
				print,'filter_update:  failed: '+ filter_list[i]
				ok[i] = 0
			endelse
			if ptr_valid(pf) then ptr_free, pf
		endfor
	endif
	filter_files = filter_list
	filter_list = strip_path( filter_list)

	q = where( (ok eq 1) and (filter_list ne ''))
	if q[0] eq -1 then begin
		filter_title = ['-- none --']
		filter_list = ''
		filter_files = ''
		count = 0
	endif else begin
		q2 = sort_unique( filter_list[q], ns, /last)
		filter_title = filter_title[q[q2]]
		filter_list = filter_list[q[q2]]
		filter_files = filter_files[q[q2]]
		count = ns
	endelse

	new = -1
	if size( present, /tname) eq 'STRING' then begin
		q = where( filter_list eq present)			; assumes filename without path
		if q[0] ne -1 then begin
			new = q[0]
			file = filter_files[q[0]]
		endif
	endif
	return
end
