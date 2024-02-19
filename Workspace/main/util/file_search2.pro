function file_search2, dir, spec, progress=show_progress, ptlb=p, debug=debug, $
				all=all, cancel=cancel, title=title, exclude=exclude, group_leader=group

; A version of file_search that uses a progress bar and can be aborted if taking too long.
; It will return when it finds its first match(es), unless /all is set.
; NOTE: It will return ALL matches to a wildcard 'spec'.
; Exclude searching in any paths in 'exclude'. Updated on return with any new paths searched.
; Use 'exclude' to avoid searching trees multiple times.

COMPILE_OPT STRICTARR
if n_elements(show_progress) eq 0 then show_progress=0
if n_elements(debug) eq 0 then debug=0
if n_elements(title) eq 0 then title='Search for files ...'
if n_elements(exclude) eq 0 then exclude=''
common c_progress, pcancel

	cancel = 0
	f = ''
	path = fix_path( dir[0])
	q = where( path eq exclude, nq)
	if nq ge 1 then return, ''									; skip out if in exclude list

	if n_elements(spec) eq 0 then begin
		f = file_search( path)
		return, f
	endif
	if n_elements(all) eq 0 then begin
		all = 0
		i = locate('*',spec)
		if i ge 0 then all=1
	endif
;	if debug and show_progress then print,'File_search2: All=',all
	if debug then print,' '
	if debug then print,'Entry: '+path+'  '+spec

;	Now find all sub-dirs to search ...

	if debug then print,'Find subdirs in: '+path
	fd = file_search( path+'*', /test_directory, /mark_directory, /expand_tilde, count=count)
	if debug then print,'	subdir count: '+str_tidy(count)

;	Only pop-up progress bar the first time in, if dirs are found ...

	if show_progress then begin
		pcancel = ptr_new(0)
		progress, tlb=progress_tlb, title=title, group_leader=group, pcancel=pcancel
		p = { progress_tlb:progress_tlb, current:0, count:count}
		if *pcancel then begin
			f = '#cancel'
			goto, done
		endif
	endif

	if var_type(p) eq 8 then begin
		progress, /update, p.progress_tlb, {unit:0, value:0, current:p.current, size:p.count, file:path}, cancel=cancel, skip=skip
		if cancel or skip or *pcancel then begin
			print,'file_search: cancel 1 from progress: '+path
			f = '#cancel'
			goto, done
		endif
	endif
	if count eq 0 then goto, top

;	For each sub-dir, dive deeper ...

	for i=0,count-1 do begin
		if debug then print,'	Try subdir('+str_tidy(i)+'): '+fd[i]
		g = file_search2( fd[i], spec, debug=debug, ptlb=p, all=all, exclude=exclude, cancel=cancel)
		if cancel then begin
			print,'file_search: cancel 2 from file_search: '+fd[i]
			f = '#cancel'
			goto, done
		endif

		if var_type(p) eq 8 then begin
			if show_progress then begin
				p.current = i+1					; advance loop only at top dir level
			endif
			progress, /update, p.progress_tlb, {unit:0, value:0, current:p.current, size:p.count, file:fd[i]}, cancel=cancel, skip=skip
			if cancel or skip or *pcancel then begin
				print,'file_search: cancel 3 from progress: '+fd[i]
				f = '#cancel'
				goto, done
			endif
		endif
		if all then begin
			f = [f,g]
		endif else begin
			if (g[0] ne '') then begin
				f = g
				goto, done
			endif
		endelse
		exclude = [exclude,fd[i]]
	endfor
		
;	Look for match in this directory ...

top:
	exclude = [exclude,path]
	if debug then print,'Find files in path: '+path
	g = file_search( path+spec, count=count)
	if count ge 1 then begin
		if debug then print,'	Found '+str_tidy(count)+' file match(es): '+g[0]
		if all then begin
			f = [f,g]
		endif else begin
			if (g[0] ne '') then begin
				f = g
				goto, done
			endif
		endelse
	endif
	if show_progress eq 0 then goto, finish

done:
	if f[0] eq '#cancel' then begin
		f[0] = ''
		cancel = 1
	endif
	if show_progress then begin
		if widget_info( p.progress_tlb, /valid) then begin
			progress, /complete, p.progress_tlb, 'Cleanup ...'
			wait, 0.5
			progress, /ending, p.progress_tlb
			ptr_free, pcancel
		endif
	endif

finish:
	if (n_elements(f) ge 1) then begin
		q = where( f ne '',nq)
		if nq gt 0 then begin
			f = f[q]
		endif else f=''
	endif else begin
		f = ''
		nq = 0
	endelse
	if all eq 0 then f=f[0]
	if debug then print,'	Return ('+str_tidy(nq)+'): '+f[0]
	return, f
end
