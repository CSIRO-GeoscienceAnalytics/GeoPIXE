pro poly_update, list=poly_files, title=poly_title, count=count, error=error

; Check for new poly config CSV files, and update local lists.
;
; list		the new back plugin file list found
; title		the new back plugin title strings
; count		total number found

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
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
	       warning,'poly_update',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
		   poly_list = ''
		   poly_title = 'No "title" return'
	       return
	    endif
	endif
	error = 1

	read_poly = 0
	count = 0
	if arg_present(poly_title) then read_poly=1

	root_path = geopixe_root+slash()
	poly_list = find_file2( root_path+'polycapillary-*.csv')
	
	prefs = geopixe_defaults( source='poly_update')
	path = fix_path( prefs.path.config)
;	path = fix_path( path + 'source')
	poly_list2 = file_search( path, 'polycapillary-*.csv')
	if poly_list2[0] ne '' then poly_list = [poly_list, poly_list2]

	poly_title = poly_list
	poly_title[*] = ''
	nf = n_elements( poly_list)
	count = nf
	ok = replicate( 1, nf)
	
	if poly_list[0] ne '' and read_poly then begin
		print,'poly_update:  process ',nf,' poly CSV files ...'
		for i=0L,nf-1 do begin
			print,'Poly: read CSV header: ', poly_list[i]
			poly = get_poly_config( poly_list[i], /header, error=err)
			if err then return
			poly_title[i] = poly.model
			ok[i] = 1
			print,'poly_update: register poly: ', poly_title[i]
		endfor
	endif
	poly_files = poly_list
	poly_list = strip_file_ext( strip_path( poly_list))

	q = where( (ok eq 1) and (poly_list ne ''))
	if q[0] eq -1 then begin
		poly_title = ['-- none --']
		poly_list = ''
		poly_files = ''
		count = 0
	endif else begin
		q2 = sort_unique( poly_list[q], ns, /last)
		poly_title = poly_title[q[q2]]
		poly_list = poly_list[q[q2]]
		poly_files = poly_files[q[q2]]
		count = ns
	endelse

	error = 0
	return
end
