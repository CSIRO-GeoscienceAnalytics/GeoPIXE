pro detector_update, list=detector_list, title=detector_title, $
					present=present, new=new, array=array, file=file, count=count

; Check for new detector files, and update local lists, and the
; current droplist settings.
;
; list		the new detector file list found (no paths)
; title		the new deteector title strings
; count		total number found
; /array	only load the list for array detectors
;
; present	the name of the current detector file list member
; new		the new 'index' to the present detector within the new list returned
; file		return full filename with path too

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(array) lt 1 then array=0

	read_dets = 0
	count = 0
	if arg_present(detector_title) or (array eq 1) then read_dets=1
	
	path1 = geopixe_root
	detector_list = find_file2( path1+'*.detector')
	
	prefs = geopixe_defaults( source='detector_update')
	path2 = fix_path( prefs.path.config)
	path2 = fix_path( path2 + 'detectors')
	detector_list2 = file_search( path2, '*.detector')
	if detector_list2[0] ne '' then detector_list = [detector_list, detector_list2]
	
	detector_title = detector_list
	detector_title[*] = ''
	nf = n_elements( detector_list)
	count = nf
	ok = replicate( 1, nf)
	
	if detector_list[0] ne '' and read_dets then begin
		print,'detector_update:  process ',nf,' detector files ...'
		for i=0L,nf-1 do begin
			pf = read_detector( detector_list[i], error=error)
			if error eq 0 then begin
				detector_title[i] = (*pf)[0].crystal.name
				ok[i] = (array eq 1) ? (*pf)[0].array : 1
			endif else begin
				print,'detector_update:  failed: '+ detector_list[i]
				ok[i] = 0
			endelse
			if ptr_valid(pf) then ptr_free, pf
		endfor
	endif
	detector_files = detector_list
	detector_list = strip_path( detector_list)

	q = where( (ok eq 1) and (detector_list ne ''))
	if q[0] eq -1 then begin
		detector_title = (array eq 1) ? ['-- no arrays --'] : ['-- none --']
		detector_list = ''
		detector_files = ''
		count = 0
	endif else begin
		q2 = sort_unique( detector_list[q], ns, /last)		; prority to 'config/detectors' over 'geopixe' detectors
		detector_title = detector_title[q[q2]]
		detector_list = detector_list[q[q2]]
		detector_files = detector_files[q[q2]]
		count = ns
	endelse

	new = -1
	file = ''
	if size( present, /tname) eq 'STRING' then begin
		q = where( detector_list eq strip_path(present))	
		if q[0] ne -1 then begin
			new = q[0]
			file = detector_files[q[0]]
		endif else begin
			F = file_requester(/read, filter='*.detector', path=path2, file=present, $		; group=group
				title='Select detector file', /fix_filter, /skip_if_exists, updir=3)
			if F eq '' then return
			
			pf = read_detector( F, error=error)
			if error eq 0 then begin
				detector_title = [detector_title, (*pf)[0].crystal.name]
				detector_list = [detector_list, strip_path(F)]
				detector_files = [detector_files, F]
				count = ns+1
				if ptr_valid(pf) then ptr_free, pf
				new = ns-1
				file = F
			endif
		endelse
	endif
	return
end
