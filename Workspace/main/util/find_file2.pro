function find_file2, file, extension_numeric=extension_numeric, virtual=virtual, $
						name_numeric=name_numeric, test_directory=test_directory, $
						part_numeric=part_numeric, multi_char=multi_char, n_big=n_big, _extra=extras

; Find files, like 'file_search' for a single file. If /virtual, then limit number of 
; (dir) returns to a manageable number, by grouping them with "...". Group into purely
; numeric (sorted numerically) and non-numeric (sorted alphabetically) virtual returns.
; Number controlled by 'n_big' below.
; 
; Does not do a directory tree traverse. Use "file_search" for that.
;
; If "..." in 'file' then expand this here, else do normal file_search. If parts
; of virtual are numeric, search for all in 'numeric' range, else search for all
; in 'alphabetical' range.
; 
; /extension_numeric	extension must be numeric, reject non-numeric extensions
; /name_numeric			file name must be numeric, reject non-numeric names
; /part_numeric			last part of file name must be numeric
; 						else accept all and sort in numeric-alpha order.
; multi_char			the character (e.g. from Device Obj) used to separate numeric part
; /virtual				if /test_dir then group returns into ranges "..."
; n_big					number after which we start to group virtual.
; /test_directory		only include directories
; /mark_directory		make sure there is a path separator at the end of each return
; 						(this and other "file_search" options passed onto 'file_search')
;
; Default sort is numeric (for all numeric parts) and alphabetical for non-numeric
; strings.
;
; Chris Ryan (CSIRO), revised 2021


COMPILE_OPT STRICTARR
ErrorNo = 0
sDebug = ''
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
		warning,'find_file2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',sDebug,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif

	if n_elements(file) lt 1 then return, ''
	if lenchr(file) lt 1 then return, ''
	if n_elements(extension_numeric) lt 1 then extension_numeric=0
	if n_elements(name_numeric) lt 1 then name_numeric=0
	if n_elements(part_numeric) lt 1 then part_numeric=0
	if n_elements(multi_char) lt 1 then multi_char='_'
	if n_elements(test_directory) lt 1 then test_directory=0
	if n_elements(virtual) lt 1 then virtual=0
	if n_elements(n_big) lt 1 then n_big = 50
	sep = string([byte(path_sep()),byte(' '),byte('.'),byte('*')])

; Look for a "..." that indicates a virtual directory range. Expand that here.
; If expanded it too numerous, new virtual ranges will be created later below.

	nle = locate('...', file)
	if nle ge 1 then begin
		sDebug = 'Within "..." ...'
		nf = lenchr(file)
		f = strmid(file,0,nle)
		path = extract_path(f)
		first = strip_path(f)
		last = strsplit(strmid(file,nle,nf-nle), sep, /extract)
		last = last[0]
		t = file_search( path+'*', test_directory=test_directory, mark_directory=test_directory, /expand_tilde, _extra=extras)
		t1 = strip_path(t, /keep)
		nt = n_elements(t1)
		if nt eq 0 then return,''
		if inumeric(first, /strict) and inumeric(last, /strict) then begin
			q = where( inumeric(t1, /strict), nq)
			if nq eq 0 then return,''
			q1 = where( (long64(t1[q]) ge long64(first)) and (long64(t1[q]) le long64(last)), nq1)
			if nq1 eq 0 then return,''
			q2 = sort_file_numeric(t1[q[q1]], /name, /strict)	; sort numeric, non-numeric
			t = t[q[q1[q2]]]									; parts in alphabetical order
		endif else begin
			q = sort(strupcase(t1))								; sort in alphabetical order
			n1 = where(t1[q] eq first)
			n2 = where(t1[q] eq last)
			m1 = min([n1,n2]) & m2 = max([n1,n2])
			t = t[q[m1:m2]]
		endelse
	endif else begin
		t = file_search( strtrim(file,2), test_directory=test_directory, mark_directory=test_directory, /expand_tilde, _extra=extras)
		q = sort_file_numeric( strip_path(t,/keep), /name, /ext, /strict)
		t = t[q]
	endelse
		
	if name_numeric then begin
		sDebug = 'In "numeric name" ...'
		name = strip_file_ext(strip_path(t,/keep))
		q = where( inumeric(name, /strict), nq )
		if nq gt 0 then begin
			q2 = sort_file_numeric(name[q], /name, /strict)
			t = t[q[q2]]
		endif else t=''
	endif else if part_numeric then begin
		sDebug = 'In "numeric name part" ...'
		name = strip_file_ext(strip_path(t,/keep))
		i = locate_last(multi_char,name)
		q1 = where( (i ge 0) and (i lt strlen(name)), nq1)
		if nq1 gt 0 then begin
			name2 = strarr(nq1)
			for k=0,nq1-1 do name2[k] = strmid(name[q1[k]],i[q1[k]]+1)
		endif else name2=''
			
		q = where( inumeric(name2, /strict), nq )
		if nq gt 0 then begin
			q2 = sort_file_numeric(name2[q], /name, /strict)
			t = t[q1[q[q2]]]
		endif else t=''
	endif
	
; If number too numerous, then group files into 'virtual' directories.
; Indicated by "..." in names.

	if virtual and test_directory then begin
		sDebug = 'If "Virtual" and "Test_Dir" ...'
		nt = n_elements(t)
		non_numeric = 0
		no_numeric = 0
		if nt ge n_big then begin
			f = strip_path(t, /keep)
			isn = inumeric(f, /strict)
			q1 = where( isn eq 0, nq1)
			if nq1 gt 0 then begin
				non_numeric = 1
				t1 = t[q1]
				q2 = where( isn eq 1, nq2)
				if nq2 gt 0 then begin
					t = t[q2]
					f = f[q2]
					nt = n_elements(t)
				endif else begin
					no_numeric = 1
					goto, more
				endelse
			endif
			
			q = sort_file_numeric(f, /name, /strict)
			m = ceil(float(nt)/n_big)
			t2 = strarr(nt/m +10)
			j = 0
			for i=0L,nt-1,m do begin
				sDebug = 'In "Virtual dir" loop (nt='+str_tidy(nt)+', m='+str_tidy(m)+', big='+str_tidy(n_big)+')...'
				if i gt nt-1 then warning,'find_file2','strange index out of range.'
				s = t[q[i]]
				if strmid(s,strlen(s)-1,1) eq path_sep() then begin
					s = strmid(s,0,strlen(s)-1)
				endif
				if i eq ((i+m-1)<(nt-1)) then begin
					t2[j] = extract_path(s) + f[q[i]] + path_sep()
				endif else begin
					t2[j] = extract_path(s) + f[q[i]] + ' ... ' + f[q[(i+m-1)<(nt-1)]] + path_sep()
				endelse
				j = j+1
			endfor
			t2 = t2[0:j-1]
more:
			if non_numeric then begin
				t = t1
				if no_numeric eq 0 then t = [t,t2]
			endif else begin
				t = ''
				if no_numeric eq 0 then t = t2
			endelse
		endif
	endif

	if extension_numeric then begin
		sDebug = 'In "numeric extension" ...'
		ext = extract_extension(t)
		q = where( inumeric(ext, /strict), nq )
		if nq gt 0 then begin
			q2 = sort_file_numeric(t[q], /ext, /strict)
			t = t[q[q2]]
		endif else t=''
	endif
		
	if n_elements(t) eq 1 then t=t[0]
	return, t
end
