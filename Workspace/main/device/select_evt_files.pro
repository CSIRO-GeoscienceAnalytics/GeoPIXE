function select_evt_files, starti, finishi, multi, delim, ext, test=test, dir=dir, offset=offset, $
				embed_detector=embed_detector

; Build a list of file names that spans 'start' to 'finish',
; keep going if 'finish' is blank.
; 
; input
;	start		first data filename (or dir)
;	finish		last data filename (or dir), if mode 'multi'
;	/multi		indicates multiple files used for data (finish present)
;	delim		deliminator, if non-blank, after which the numeric part exists
;	ext			fixed file extension, if used
;	/embed_detector  detector number is embedded implictly in file-name
;	/dir		files are directory names
;	/test		test mode, returns only a few files
;
; output:
;	offset		returned vector of offset ( {XYZ} triplets), non-zero if second data group
;				used for finish, one triplet per returned filename.
;	files		function return of file list
;
; increment numbers in file name after 'delim', and add extension 'ext'
; If there is an '_' in the number part, then need to treat this
; as X and Y number parts, separated by '_'. Will need NX to do this.
;
; If /dir and /multi then assume start, finish are directory names and just
; find numerical dirs spanning this range.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) eq 0 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'select_evt_files',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif

if n_elements(test) lt 1 then test=0
if n_elements(dir) lt 1 then dir=0
if n_elements(embed_detector) lt 1 then embed_detector=0

if dir then begin
	start = starti
	finish = finishi
	ns = strlen(start)
	if strmid(start,ns-1,1) eq path_sep() then start=strmid(start,0,ns-1)
	nf = strlen(finish)
	if strmid(finish,nf-1,1) eq path_sep() then finish=strmid(finish,0,nf-1)
	ss = strip_path(start)
	sf = strip_path(finish)
	
	if inumeric(ss) and inumeric(sf) then begin
		is = long(ss)
		ifs = long(sf)
		path = extract_path(start)
		f = find_file2( path + '*', /test_dir)
		if n_elements(f) lt 1 then return, ''
		fn = strip_path(f,/keep)
		q1 = where( inumeric(fn), nq1)
		if nq1 lt 1 then return, ''
		i = long(fn[q1])
		q2 = sort(i)
		q3 = where( (i[q2] ge is) and (i[q2] le ifs), nq3)
		if nq3 lt 1 then return, ''
		return, f[q1[q2[q3]]]
	endif
endif
 
drag_select = (n_elements(starti) gt 1)
start = starti[0]
if multi eq 0 then return, start
if drag_select and dir and multi then return, starti

finish = finishi[0]
fill = 0
if delim eq '.' then fill=1

nd = lenchr(delim)
if delim ne '.' then begin						; strip file ext, if this is fixed
	start = strip_file_ext(start)
	finish = (lenchr(finish) gt 0) ? strip_file_ext(finish) : ''
endif
i = locate_last( delim, start)
if i lt 0 then return, start

root = extract( start, 0, i+nd-1)

if embed_detector then begin
	root = extract( start, 0, i+nd-2)
	i = locate_last( delim, root)
	if i lt 0 then return, start
	root = extract( start, 0, i+nd-1)
endif

if drag_select then begin
	f = starti
endif else begin
	s = root + '*' + ext						; find all possible files
	f = find_file2(s)
endelse
offset = replicate(0L, n_elements(f))
use_offset = 0
flag = replicate(0,n_elements(f))
null_offset = {X:0L, Y:0L, Z:0L}
pixel_offset = null_offset

if (lenchr(finish) gt 0) and (drag_select eq 0) then begin
	i2 = locate_last( delim, finish)
	root2 = extract( finish, 0, i2+nd-1)
	if embed_detector then begin
		root2 = extract( finish, 0, i2+nd-2)
		i2 = locate_last( delim, root2)
		if i2 ge 0 then root2 = extract( finish, 0, i2+nd-1)
	endif
	if root2 ne root then begin					; in case finish is in the next group/run of files
		s2 = root2 + '*' + ext
		f2 = find_file2(s2)
		offset2 = replicate(1000000L, n_elements(f2))
		f = [f,f2]
		offset = [offset,offset2]
		flag = replicate(0,n_elements(f))
		use_offset = 1

		text = ['Offset X (pixels)','Offset Y (pixels)','Offset Z (pixels)']
		initial_text = ['0.0','0.0','0.0']
		help_text = ['Enter the X pixel offset for the second run data.','Enter the Y pixel offset for the second run data.', $
					'Enter the Z pixel offset for the second run data.']
		explanation = 'Optional offset of second group of data (run #) relative to first. Must disable "cluster" mode. If using Maia Mapper "Append" second run, then skip (cancel) this. ' + $
					'Better still, use the plugin "Combine and Align Image Sets" to combine multiple scans on the same area.'
		r = options_popup( title='Offset pixels for second run', text=text, initial_text=initial_text, help_text=help_text, $
					explanation=explanation, error=error)			; min_xsize=300
		if error eq 0 then begin
			pixel_offset = {X:long2(r.text[0]), Y:long2(r.text[1]), Z:long2(r.text[2])}
		endif
	endif
endif

n = n_elements(f)
if (n lt 1) or (strlen(f[0]) lt 1) then return, starti

js = locate_last( delim, start)					; extract the numerical part of start name
if js lt 0 then return, start
ls = strlen(start)
if js+nd ge ls then return, start

start1 = extract( start, js+nd, ls-1)			; seq# for start
ns = long( strip_non_numeric(start1))

if lenchr(finish) gt 0 then begin				; extract the numerical part of finish name
	jf = locate_last( delim, finish)
	if jf lt 0 then return, start
	lf = strlen(finish)
	if jf+nd ge lf then return, start

	finish1 = extract( finish, jf+nd, lf-1	)	; seq# for finish
	nf = long( strip_non_numeric(finish1))
	if n_elements(f2) gt 0 then nf = nf+1000000L
endif else begin
	nf = 10 * 1000000L
endelse
if test then nf = (ns+2) < nf					; test mode, limit files

j = locate_last( delim, f)
l = strlen(f)
s = extract( f, j+nd, l-1)						; number parts of all files

if lenchr(ext) lt 1 then begin
	ok = replicate(1,n_elements(s))
	k = locate('.',s)
	q = where( k ge 0)
	if q[0] ne -1 then ok[q]=0					; to reject '0001.hdf' type files ?????
	q1 = where(ok eq 1)
endif else begin
	ok = intarr(n_elements(s))
	k = locate_last(ext,s)						; get up to extension
	q1 = where(k ge 0)
	if q1[0] ne -1 then begin
		s[q1] = extract( s[q1], 0, k[q1]-1)
	endif
endelse
if q1[0] eq -1 then return, start
if ext ne '' then s[q1] = strip_non_numeric( s[q1])

q2 = where(inumeric( s[q1]) eq 1)
seq = long( s[q1[q2]])	+ offset[q1[q2]]		; assumes that numerical part is number, no '_'
flag[q1[q2]] = offset[q1[q2]] ne 0
q3 = where( (seq ge ns) and (seq le nf))
if q3[0] eq -1 then return, start
seq = seq[q3]
q4 = sort(seq)

file =    f[q1[q2[q3[q4]]]]
flag = flag[q1[q2[q3[q4]]]]

offset = replicate( null_offset, n_elements(flag))
if use_offset then begin
	q5 = where( flag ne 0, nq5)
	if nq5 gt 0 then offset[q5] = pixel_offset
endif
return, file
end
