pro export_zarr, files=file, output=dir

;	Write a ZARR organization dir tree for Cloud applications
;
;	Write the images given by filename 'file' to dir 'dir'
;
;	'file'	a DAI image filename.

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
		warning,'export_zarr',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(file) eq 0 then return
	
	file = file_requester( /read, filter='*.dai', file=file[0], $
		/translate, updir=4, title='Select the Image file', $
		fix_filter=1, /skip_if_exists)
	if file eq '' then return

	if n_elements(dir) eq 0 then dir = strip_file_ext( file) + '.zarr'
	
	p = read_geopixe_image( file, error=err)
	if err or (ptr_good(p) eq 0) then begin
		warning,'export_zarr','Error reading Image DAI file.'
		return
	endif
	
	write_geopixe_zarr, p, dir
	return
end
