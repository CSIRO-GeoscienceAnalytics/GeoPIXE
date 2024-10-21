pro source_update, list=source_list, title=source_title, $
					present=present, new=new, file=file, count=count

; Check for new source files, and update local lists, and the
; current droplist settings. This has become reduntant for now, as we explicitly
; put different continuum source types in droplists (e.g. lab, pink in 'layer_setup').
;
; list		the new source file list found (no paths)
; title		the new source title strings
; count		total number found
;
; present	the name of the current source file list member
; new		the new 'index' to the present source within the new list returned
; file		return full filename with path too

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

	read_srcs = 0
	count = 0
	if arg_present(source_title) then read_srcs=1

	source_list = find_file2(geopixe_root+'*.source')
	
	prefs = geopixe_defaults( source='source_update')
	path = fix_path( prefs.path.config)
	path = fix_path( path + 'sources')
	source_list2 = file_search( path, '*.source')
	if source_list[0] ne '' then begin
		if source_list2[0] ne '' then source_list = [source_list, source_list2]
	endif else source_list = source_list2
	
	source_title = source_list
	source_title[*] = ''
	nf = n_elements( source_list)
	count = nf
	ok = replicate( 1, nf)
	
	if source_list[0] ne '' and read_srcs then begin
		print,'source_update:  process ',nf,' source files ...'
		for i=0L,nf-1 do begin
			src = read_source( source_list[i], error=error)
			if error eq 0 then begin
				source_title[i] = src.title
				ok[i] = 1
			endif else begin
				print,'source_update:  failed: '+ source_list[i]
				ok[i] = 0
			endelse
		endfor
	endif
	source_files = source_list
	source_list = strip_path( source_list)

	q = where( (ok eq 1) and (source_list ne ''))
	if q[0] eq -1 then begin
		source_title = ['-- none --']
		source_list = ''
		source_files = ''
		count = 0
	endif else begin
		q2 = sort_unique( source_list[q], ns, /last)
		source_title = source_title[q[q2]]
		source_list = source_list[q[q2]]
		source_files = source_files[q[q2]]
		count = ns
	endelse

	new = -1
	if size( present, /tname) eq 'STRING' then begin
		q = where( source_list eq present)			; assumes filename without path
		if q[0] ne -1 then begin
			new = q[0]
			file = source_files[q[0]]
		endif
	endif
	return
end
