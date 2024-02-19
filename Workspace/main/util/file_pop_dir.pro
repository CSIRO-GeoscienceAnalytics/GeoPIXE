function file_pop_dir, file

; Pop directory up one level, keep same file name

	path = extract_path(file)
	n = strlen(path)
	i = locate_last( path_sep(), path)
	if i eq -1 then return, file
	if (strmid(path, n-1,1) eq path_sep()) then path=strmid(path,0,n-1) 

	i = locate_last( path_sep(), path)
	if (i gt 0) then path=strmid(path,0,i+1)
	
	n = strlen(path)
	if (strmid(path, n-1,1) ne path_sep()) then path=path+path_sep() 
	f = path + strip_path(file)
	return, f
end
