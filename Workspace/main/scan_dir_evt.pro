function scan_dir_evt, F, obj, image=image, dai_dir=dai_dir, ppath=ppath, proot=proot, $
				mode=mode, rmin=rmin, rmax=rmax, error=error

; Scan the dir tree F for list-mode files and construct an array
; of structs to describe them.
; Returns a pointer array to structs.
; 
;	obj		device obj
;	ppath	pointer to path (defaults to F)
;	proot	pointer to path root
;	error	returns error (1) or sucess(0)
;	/image	also scan for derived image DAI files.
;	dai_dir  path to DAI files (defaults to same as raw files 'F')
;	mode	normal image mode (0) or cuts mode (1)
;	rmin	minimum event file run number
;	rmax	maximum event file run number

error = 1
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''
if lenchr(F) lt 1 then return, 0L
if n_elements(obj) lt 1 then return, 0L
if n_elements(image) lt 1 then image=0
if n_elements(dai_dir) lt 1 then dai_dir=F
if n_elements(mode) lt 1 then mode=0
if n_elements(ppath) lt 1 then ppath=ptr_new(F)
if n_elements(proot) lt 1 then begin
	proot = ptr_new({strip:0, path:''})
	*ppath = build_output_path( *ppath, *ppath, proot, /set)
endif

	ext = obj->extension()
	if obj->multi_files() then begin								; for multi-file sets, need to
		s = (adc_offset_device(obj) eq -1) ? '0' : '1'				; search for only first file
		if obj->embed_detector() then begin
			evt = '*_00' + obj->multi_char() + s + ext
		endif else begin
			evt = '*' + obj->multi_char() + s + ext
		endelse
	endif else begin
		evt = '*' + ext
	endelse
	dai = '*' + '.dai'												; original dai files
	s = file_search( F, evt)										; all evt files in tree

	; Reject '', temp files like "1234.dai.0" and any files in a .zarr directory.

	q = where( (s ne '') and (locate('.dai.',s) eq -1) and (locate('.zarr',s) eq -1), ns)		
	
	if ns eq 0 then begin
		warning, 'scan_dir_evt', 'No list-mode files found.'
		return, 0L
	endif

	if (n_elements(rmin) ge 1) or (n_elements(rmax) ge 1) then begin
		if n_elements(rmin) eq 0 then rmin=0
		if n_elements(rmax) eq 0 then rmax=1						; filter run number range
		name = strip_file_ext( strip_path( s), /double)
		if obj->multi_files() then begin
			j = locate_last( obj->multi_char(), name)				; run # is before the multifile char
			q = where( j ge 0, nq)
			if nq ge 1 then name[q] = strmid2( name[q], 0,j[q])
		endif
		if obj->embed_detector() then begin
			j = locate_last( "_", name)								; det # is before the multifile char
			q = where( j ge 0, nq)
			if nq ge 1 then name[q] = strmid2( name[q], 0,j[q])
		endif
		q = where( inumeric(name) eq 1, nq)							; filter by run number name
		if nq ge 1 then begin
			q2 = where( (long2(name[q]) ge rmin) and (long2(name[q]) le rmax), nq2)
			if nq2 ge 1 then begin
				q = q[q2]
			endif
			nq = nq2
			if nq ge 1 then begin
				s = s[q]
			endif
		endif
		ns = n_elements(s)
	endif

	if lenchr(s[0]) eq 0 then begin
		warning, 'scan_dir_evt', 'No list-mode files found.'
		return, 0L
	endif
	p = ptrarr(ns)
	fs = strip_path(strip_file_ext(s))
	if obj->multi_files() and (obj->multi_char() ne '.') then begin
		fs0 = strip_file_m( fs, ending=obj->multi_char() + ((adc_offset_device(obj) eq -1) ? '0' : '1'))
	endif else fs0 = fs
	s0 = strip_file_ext(s)
	
	s2 = file_search( dai_dir, dai)									; all original dai files
	ok = file_test( s2, /read) and (file_test( s2, /zero_length) ne 1)
	q = where( ok eq 1, nd)
	if nd gt 0 then begin
		s2 = s2[q]
	endif else s2 = ''
	
	q = where_tokens( ['.dai.','.spec.'], /exclude, s2,nd)				; remove parallel temp files
	if nd gt 0 then begin
		s2 = s2[q]
	endif else begin
		s2 = ''
	endelse
	fs2 = strip_path(strip_file_ext(s2))
	nfs2 = n_elements(fs2)
	good = intarr(nfs2)
	for i=0,nfs2-1 do begin
		n = lenchr(fs2[i])
		if n ge 3 then begin
			fs3 = strmid(fs2[i],n-3,3)
			good[i] = (fs3 ne '-rt') and (fs3 ne '-RT')
		endif else good[i]=1
	endfor
	q = where( good eq 1, nd)
	if nd gt 0 then begin
		fs2 = fs2[q]
		s2 = s2[q]
	endif else begin
		fs2 = ''
		s2 = ''
	endelse
 
	for i=0L,ns-1 do begin
		t = { file:s[i], dam:'', output:'', suppress:0, enable:1, xrange:0,yrange:0, xsize:0.0,ysize:0.0, charge:0.0, $
			pv:'', conv:0.0, gain:1.0, pileup:'',throttle:'', sample:'',grain:'',comment:'', energy:0.0, $
			xorigin:0.0, yorigin:0.0, zorigin:0.0, facility:'', endstation:'' }

		if image and (nd gt 0) then begin	
			name = fs0[i]
			if obj->embed_detector() then begin
				j = locate_last( "_", name)										; det # is before the multifile char
				q = where( j ge 0, nq)
				if nq ge 1 then name[q] = strmid2( name[q], 0,j[q])
			endif
			if (mode eq 1) then name = strip_file_m( name, ending='-cuts') + '-cuts'
			if (mode eq 3) then name = strip_file_m( name, ending='-MPDA') + '-MPDA'
			q = where( fs2 eq name, nq)											; with same name root as list file
			if nq eq 0 then q = where( fs2 eq fs0[i], nq)						; If none, relax naming to include normal DAI files
			if nq gt 0 then begin
				output = extract_path(s2[q[0]]) + name + '.dai'
				*ppath = build_output_path( s[i], output, proot) 
				t.output = output
				
				pimg = read_geopixe_image( s2[q[0]], /header, error=error)
				if (error eq 0) then begin
					t.xrange = (*pimg).xsize * ((*pimg).xcompress > 1)
					t.yrange = (*pimg).ysize * ((*pimg).ycompress > 1)
					t.xsize = (*pimg).scan.x * 1000.
					t.ysize = (*pimg).scan.y * 1000.
					t.xorigin = (*pimg).scan.origin.x
					t.yorigin = (*pimg).scan.origin.y
					t.charge = (*pimg).charge
					t.pv = (*pimg).IC.pv.name
					t.conv = (*pimg).IC.conversion
					t.gain = (*pimg).IC.pv.val * (*pimg).IC.pv.unit
					t.energy = (*pimg).energy
					t.dam = (*pimg).matrix.file
					t.pileup = (*pimg).pileup
					t.throttle = (*pimg).throttle
					t.sample = (*pimg).sample
					t.grain = (*pimg).grain
					t.comment = (*pimg).comment
					t.endstation = (*pimg).endstation
					t.facility = (*pimg).facility
					t.suppress = 1
				endif
				free_images, pimg
			endif
		endif
		p[i] = ptr_new( t, /no_copy)
	endfor
	 
	silent = 0
	for i=0L,ns-1 do begin
		if (*p[i]).suppress eq 0 then begin
		
;			name = fs0[i]			; must be the full path
			name = s0[i]
			if lenchr(output) lt 1 then output=name
			*ppath = build_output_path( (*p[i]).file, output, proot)

			if obj->multi_files() and (obj->multi_char() ne '.') then begin
				name = strip_file_m( name, ending=obj->multi_char() + ((adc_offset_device(obj) eq -1) ? '0' : '1'))
			endif
			if obj->embed_detector() then begin
				j = locate_last( "_", name)										; det # is before the multifile char
				q = where( j ge 0, nq)
				if nq ge 1 then name[q] = strmid2( name[q], 0,j[q])
			endif
			if (mode eq 1) then name = strip_file_m( name, ending='-cuts') + '-cuts'
			if (mode eq 3) then name = strip_file_m( name, ending='-MPDA') + '-MPDA'
			(*p[i]).output = *ppath + strip_path(name) + '.dai'
	
			mp = get_header_info( obj, (*p[i]).file, group=group, silent=silent, error=error)
			if (error eq 0) then begin
				if mp.scan.on then begin
					(*p[i]).xrange = mp.scan.x_pixels
					(*p[i]).yrange = mp.scan.y_pixels
					(*p[i]).xsize = mp.scan.x_mm * 1000.
					(*p[i]).ysize = mp.scan.y_mm * 1000.
					(*p[i]).xorigin = mp.scan.x
					(*p[i]).yorigin = mp.scan.y
					(*p[i]).zorigin = mp.scan.z
				endif
				(*p[i]).charge = mp.charge
				(*p[i]).comment = mp.title
				(*p[i]).energy = mp.energy

				(*p[i]).sample = mp.sample
				(*p[i]).gain = mp.sensitivity
				(*p[i]).pv = mp.IC_name
				(*p[i]).pileup = mp.pileup.file
				if (mp.pileup.on eq 0) then (*p[i]).pileup = ''		
				(*p[i]).throttle = mp.throttle.file
				if (mp.throttle.on eq 0) then (*p[i]).throttle = ''		
		
				(*p[i]).facility = mp.metadata.facility
				(*p[i]).endstation = mp.metadata.endstation
				if (mp.metadata.facility eq 'MM.Mel.') and ((*p[i]).pv eq '') then begin
					(*p[i]).gain = 1.0										; to test using old Udimet data
					(*p[i]).pv = 'Maia:dwell.time'
				endif
				if (*p[i]).gain lt 1.0e-6 then begin
					(*p[i]).gain = 1.0										; is this a good idea?
					print, '** wizard_batch_scan_dir: gain was zero, so set it to 1.0 to continue.'
				endif
				silent = 1
			endif
		endif
		(*p[i]).suppress = (i ne 0)
	endfor

	error = 0
	return, p
end