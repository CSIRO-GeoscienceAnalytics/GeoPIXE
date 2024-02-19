function get_header_info, obj, file, output=output, group=group, silent=silent, $
					dir_mode=dir_mode, error=error

; Read header info. Assumes file is NOT open now.
;
; file		a raw data file to look for associated header, metadata
; output	if present, this is a file on the output path, if some metadata is
;			located in that path (e.g. Maia Y LUT). 

	COMPILE_OPT STRICTARR
    if n_elements(silent) lt 1 then silent=0
	if n_elements(dir_mode) lt 1 then dir_mode = 0
    error = 1
    if obj_valid(obj) eq 0 then return,0
    if n_elements(file) lt 1 then return,0
    if strlen(file) lt 1 then return,0

;	This hack for XANES dirs assumes that dir/*, with numerical extensions, collects all raw data files.
;	This works for Maia, but not all data formats.

	if dir_mode then begin
		f = file
		n = strlen(f)
		t = strmid( f, n-1,1)
		ps = path_sep()
		if t ne ps then f = f+ps
		f = find_file2(f + '*', /extension_numeric)
		evt_file = f[0]
	endif else evt_file=file
	
	head = obj->get_header_info( evt_file, output=output, silent=silent, error=error)		;, group=group
	return, head
end
