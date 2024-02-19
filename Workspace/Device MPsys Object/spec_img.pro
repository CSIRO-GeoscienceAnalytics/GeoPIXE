pro spec_img, filei, once=once, p=p, all=all, mpsys_unix=mpsys_unix
;
;	Read an .img file and write it out
;	as a i.spec file
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
		warning,'Spec_IMG',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(mpsys_unix) lt 1 then mpsys_unix=0
	if n_elements(once) lt 1 then once=0
	if n_elements(all) lt 1 then all=0
	if n_elements(filei) lt 1 then filei=''
	file = filei

common c_img_last, last

loop:
	if n_elements(file) lt 1 then file=''
	if all then file=''
	if (strlen(file) lt 1) then begin
		if n_elements(last) ge 1 then begin
			path = extract_path(last)
		endif else begin
			path = 'g:\nmp'
		endelse
		if all then begin
			title = 'Select directory to translate all IMG files'
		endif else begin
			title = 'Select IMG file to translate'
		endelse

		file = dialog_pickfile(/read, path=path, $
			title='Select IMG file to translate', filter='*.img', $
			/fix_filter, /noconfirm, directory=all)
	endif else begin
		path = extract_path( file)
		if path eq file then begin
			file = dialog_pickfile(/read, path=path, file=file, $
				title='Select IMG file to translate', filter='*.img', $
				/fix_filter, /noconfirm)
		endif
	endelse
	if strlen(file) lt 1 then return
	if all then begin
		file = find_file2(file+path_sep()+'*.img')
	endif
	last = file[0]

	for i=0L,n_elements(file)-1 do begin
		print,'Translate file ',file[i]
		p = get_img( file[i], mpsys_unix=mpsys_unix)

		if ptr_valid(p[0]) then begin
			s = strip_file_ext( file[i]) + 'i.spec'
			write_spec, p, s
		endif
	endfor

	if once eq 0 then begin
		file = ''
		goto, loop
	endif
end
