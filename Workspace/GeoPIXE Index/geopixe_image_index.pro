pro geopixe_index

	geopixe_image_index
	return
end

;--------------------------------------------------------------------------------

pro geopixe_image_index

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'geopixe_image_index',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
	   close_file,2
       return
    endif
endif

;................................................................................................
; This code fragmwent will appear at the start of most stand-alone programs
; that need to load "GeoPIXE.sav" as a library. It needs to work when the current
; working directory is: (i) the "geopixe" runtime dir, i.e. for SAV files stored in the
; runtime dir along side GeoPIXE.sav, (ii) a subdir of "geopixe" runtime, such as the
; "maia", "daq", "wizards", e.g. for compiled Maia or Wizard SAV files, and (iii) a 
; project dir during debugging of this program in IDLDE. 
;
; The latter assumes that the runtime dir is named "geopixe" (see notes in "startupp.pro").

	found = 0
	file = 'GeoPIXE.sav'						; current dir is the runtime dir
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'					; current dir in a subdir of runtime dir
		if file_test(file) eq 0 then begin
			file = '../geopixe/GeoPIXE.sav'		; current dir is another project dir
			if file_test(file) eq 0 then begin
				r = dialog_message(['GeoPIXE library not found.','Failed to restore "GeoPIXE.sav".'],/error)
			endif else found=1
		endif else found=1
	endif else found = 1
	if found then restore, file

;.................................................................................................

	startupp, /colours, /devices
	
    dir = file_requester( /read, /dir, title='Select dir to scan for DAI files')
	if dir eq '' then begin
		warning,'geopixe_image_index','No dir specified.'
		return
	endif
	ndir = strlen(dir)
	index = dir + 'index.html'
	
	F = file_search(dir,'*.dai')
	if F[0] eq '' and n_elements(F) le 1 then begin
		warning,'geopixe_image_index',['No DAI files found in:',dir]
		return
	endif

	progress, tlb=progress_tlb, title='Search for DAI files ...'
	pp = { unit:0, current:0L, size:n_elements(F), file:F[0], value:intarr(6)}

	on_ioerror, bad_open
	openw,2, index
	on_ioerror, bad_io
	printf,2,'<HTML>'
	printf,2,'<HEAD>'
	printf,2,'<TITLE>',dir,'</TITLE>'
	printf,2,'</HEAD>'
	printf,2,'<BODY BGCOLOR="#FFFFFF">'
	printf,2,'<H3>Index to GeoPIXE image files in ' + dir + '</H3>'
	printf,2,'<H4>Click on a row to open its Image web page.</H4>'

	on_ioerror, finish
	printf,2,'<font size="-1"><table>'
	printf,2,'<tr>'
	printf,2,'<td width=180><strong> Image file </strong></td>'
	printf,2,'<td width=60><strong> X size </strong></td>'
	printf,2,'<td width=60><strong> Y size </strong></td>'
	printf,2,'<td width=80><strong> X compress </strong></td>'
	printf,2,'<td width=80><strong> Y compress </strong></td>'
	printf,2,'<td width=60><strong> Read comp </strong></td>'
	printf,2,'<td width=80><strong> Original X </strong></td>'
	printf,2,'<td width=80><strong> Original Y </strong></td>'
	printf,2,'<td width=60><strong> # dets </strong></td>'
	printf,2,'<td width=60><strong> Energy </strong></td>'
	printf,2,'<td width=120><strong> Sample </strong></td>'
	printf,2,'<td width=300><strong> Comments </strong></td>'
	printf,2,'</tr>'

	for i=0,n_elements(F)-1 do begin
	    path = extract_path( F[i])
	    path = path + 'html' + path_sep()
		file = path + strip_path( strip_file_ext(F[i])) + '.html'

;		Crude was to skip XANES DAI files for now ...
		if locate('xanes',path,/nocase) ge 0 then continue
		
		print,'**** Read:'+F[i]
    	p = read_geopixe_image( F[i], /silent, memory_compress_applied=read_comp, error=err)
    	
		pp.current = i+1
		pp.file = F[i]
		progress, /update, progress_tlb, pp, cancel=cancel, skip=skip
		if skip or cancel then begin
			free_images, p
			goto, finish
		endif

    	if err or (ptr_valid(p) eq 0) then begin
			printf,2,'<tr>'
			printf,2,'<td width=180>' + strip_path(F[i]) + ' </td>'
			printf,2,'<td width=60> *** </td>'
			printf,2,'<td width=60> Error </td>'
			printf,2,'<td width=80> reading file </td>'
			printf,2,'</tr>'
			free_images, p
			goto, finish
    	endif
    	
;		Crude was to skip XANES DAI files for now ...
;		Skip low energies, except 10 keV.

;    	if ((*p).energy lt 12.0) and (((*p).energy lt 9.99) or ((*p).energy gt 10.01)) then continue
    	
    	if (file_test( file) eq 0) then begin
			save_image_all_HTML, p, file=file
		endif
		if (*p).array then begin
			if ptr_valid((*p).pactive) then begin
				ndet = strtrim(string(n_elements(*(*p).pactive)),2)
			endif else ndet=''
		endif else ndet=''
		if read_comp ne 0 then begin
			scomp = strtrim(string(read_comp),2)
		endif else scomp=''

		rfile = strmid( file, ndir)						; relative file path/name
		printf,2,'<tr>'
		printf,2,'<td width=180>' + ' <A HREF="' + rfile + '">' + strip_path(F[i]) + ' </A></td>'
		printf,2,'<td width=60> ' + strtrim(string((*p).xsize),2) + ' </td>'
		printf,2,'<td width=60> ' + strtrim(string((*p).ysize),2) + ' </td>'
		printf,2,'<td width=80> ' + strtrim(string((*p).xcompress),2) + ' </td>'
		printf,2,'<td width=80> ' + strtrim(string((*p).ycompress),2) + ' </td>'
		printf,2,'<td width=60> ' + scomp + ' </td>'
		printf,2,'<td width=80> ' + strtrim(string((*p).original_xsize),2) + ' </td>'
		printf,2,'<td width=80> ' + strtrim(string((*p).original_ysize),2) + ' </td>'
		printf,2,'<td width=60> ' + ndet + ' </td>'
		printf,2,'<td width=60> ' + strtrim(string((*p).energy),2) + ' </td>'
		printf,2,'<td width=120> ' + (*p).sample + ' </td>'
		printf,2,'<td width=300> ' + (*p).comment + ' </td>'
		printf,2,'</tr>'
		free_images, p	
	endfor
	
finish:
	printf,2,'</table></font>'
	printf,2,'</br'
	printf,2,'<font size="-1">"Read comp" non-zero indicates an extra forced compress on read (memory limits using 32 bit O/S).</font>'
bad_io:
	printf,2,'</BODY>
	printf,2,'</HTML>
	progress, /complete, progress_tlb, 'Scan complete. Save index: '+index
	wait, 1
	goto, done
bad_open:
	warning,'geopixe_image_index',['Failed to open file:',index[0]]
done:
	progress, /ending, progress_tlb
	close_file,2
	return
end
    