pro manage_path, add=add, remove=remove, reset=reset, save=isave, restore=irestore

; As an aid to build scripts, to permit building projects via scripts
; using IDL in an open source environment, 'manage_path' allows controlling
; the contents of the search path !PATH. 
;
;	NOTE: It is assumed that the current working directory is set in preferences
;	to the head of the "GeoPIXE" directory tree.
;
; /save		saves the current !path for restoration later.
; /restore	restore a saved !path
; /reset	sets the !path to just the GeoPIXE tree (expanded).
; add		adds a path(s) to !path, if they're not there already. Arg is strarr.
;			This will include any subdirectories.
; remove	removes a path(s) from !path, if they're present. Arg is strarr.
;			This will include all subdirectories. Also recognizes the final subdir
;			name and removes just that entry.
;
; For example:
;	manage_path, /reset							set path to GeoPIXE tree only
;	manage_path, /reset, add="Device Maia"		set path to GeoPIXE tree plus Maia device project
;	manage_path, /reset, remove="interface"		set path to GeoPIXE tree less the "interface" subdirectory

	COMPILE_OPT STRICTARR

	common c_working_dir, geopixe_root
	common c_path_manage, managed_path
	if n_elements(managed_path) eq 0 then managed_path = !path
	if n_elements(geopixe_root) eq 0 then geopixe_root = file_expand_path('.')
	workspace_root = file_dirname(geopixe_root)

	if n_elements(reset) eq 0 then reset=0
	if n_elements(isave) eq 0 then isave=0
	if n_elements(irestore) eq 0 then irestore=0

	if isave then managed_path = !path
	if irestore then !path = managed_path

;	It is assumed that the current working directory is set in preferences
;	to the head of the "GeoPIXE" directory tree.

	if reset then !path = expand_path('+'+geopixe_root)		; "+" expand subdirectories

	sep = path_sep(/search_path)
	sp = strsplit( !path, sep, /extract)

;	Remove as both full path name and as final subdir (basename).

	nr = n_elements(remove)									; remove these ...
	if nr gt 0 then begin
		for i=0,nr-1 do begin
			st = expand_path( remove[i])					; expand all paths (e.g. "+")
			sr = strsplit( st, sep, /extract)
			nsr = n_elements(sr)
			
			for j=0,nsr-1 do begin
				q = where( sp eq sr[j], nq)
				if nq gt 0 then sp[q] = ''					; veto these whole paths
				q = where( sp eq file_basename(sr[j]), nq)
				if nq gt 0 then sp[q] = ''					; veto these as base subdirs
			endfor
		endfor
		q = where( sp ne '', nq)
		sp = (nq ne 0) ? sp[q] : ''
		!path = strjoin( sp, sep)
	endif

;	Add all in vector 'add' separated by valid path separator.
;	Check for prepended "+" indicating expand path.
;	If item of 'add' is simple project name, prepend the workspace path.

	na = n_elements(add)									; add these ...
	if na eq 0 then goto, done

	plus = strpos(add,'+')
	q0 = where( plus eq 0, nq0)								; note "+" occurence
	if nq0 gt 0 then begin
		add[q] = strmid( add[q], 1)							; strip leading "+"
	endif
	q = where( strpos(add,path_sep()) ge 0, nq)				; check for simple project name, no path sep
	if nq gt 0 then add[q] = workspace_root + path_sep() + add[q]
	if nq0 gt 0 then add[q0] = '+' + add[q0]				; replace "+" if found

	if na gt 0 then begin
		t = expand_path( strjoin( add, sep))				; expand all paths (e.g. "+")
		!path = !path + sep + t
	endif

done:
	for i=0,n_elements(sp)-1 do print,i,'  ',sp[i]
	return
end
