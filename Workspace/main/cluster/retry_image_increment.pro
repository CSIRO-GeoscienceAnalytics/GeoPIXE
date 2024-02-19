pro retry_image_increment, path=path, group=group

;	Use this if a cluster sort of images bombs out due to a lack of memory. 
;	Maia device is assumed for now.
;	This routine will continue from the partial 'stripe' image files (.dai.n) and
;	finish the process of reconstructing the full image. Make sure you free some 
;	memeory first.
	
COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if n_elements(path) eq 0 then path=''
if n_elements(group) eq 0 then group=0L
	
; This test and load of GeoPIXE is now redundant, as the retry is typically launched from a
; menu in the main GeoPIXE image window (file menu), rather than being a stand-alone SAV.

	Catch, ErrorNo							; GeoPIXE.sav only loaded if needed
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		
		found = 0
		file = 'GeoPIXE.sav'
		if file_test(file) eq 0 then begin
			file = '../GeoPIXE.sav'
			if file_test(file) eq 0 then begin
				a = dialog_message('retry_image_increment: Failed to restore GeoPIXE.sav.',/error)
			endif else found=1
		endif else found = 1
		if found then begin
			restore, file
			print,'"GeoPIXE.sav" restored.'
		endif else begin
			a = dialog_message(['GeoPIXE is not loaded in memory.','No "GeoPIXE.sav" file found locally.','', $
					'Check that your working directory is the "geopixe" dir.'], /error)
			return
		endelse
	endif
	test_geopixe_loaded						; tests whether GeoPIXE.sav routines loaded
	Catch, /cancel							; this is a new feature of GeoPIXE v7.0e onwards
    
	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'retry_image_increment',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif

	startupp, /devices

;	'results' is an array containing the result strings from each slave.
;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
;	Read each image and combine into one. Also combine charge, flux, etc.

	results = file_requester( filter=['*.dai.*','*.xan.*'], /fix_filter, path=path, /multiple, group=group, /numeric, $
					title='Multiple select dai.*/xan.* to assemble')
	if results[0] eq '' then return
	
	presults = ptr_new( results)
	
	pp = read_geopixe_image( (*presults)[0], error=error)
	first_evt = (*pp).source
	last_evt = ''
	if error eq 0 then begin
		obj = (*pp).DevObj
		evt_start_image_increment2, pp, /init, path=path
		for i=1L,n_elements(*presults)-1 do begin
			p2 = read_geopixe_image( (*presults)[i], error=err)
			if err eq 0 then begin
				last_evt = (*p2).source2
				evt_start_image_increment2, pp, p2, path=path
			endif else begin
				warning,'retry_image_increment','Failed to open DAI/XAN file: ' + (*presults)[i]
			endelse
			free_images, p2
		endfor
			
		evt_start_image_increment2, pp, path=path, /flatten				; final global flatten
		set_image_minmax, pp, (*pp).image, (*pp).options
		if (*pp).has_errors then begin
			set_image_minmax, pp, (*pp).error, (*pp).escale
		endif
			
		if inumeric(extract_extension( (*pp).file)) then begin
			(*pp).file = strip_file_ext( (*pp).file)
		endif

		(*pp).source = first_evt
		(*pp).source2 = last_evt

		write_geopixe_image, pp, (*pp).file
		free_images, pp
	endif else begin
		warning,'retry_image_increment','Failed to open first DAI file: ' + (*presults)[0]
	endelse

	return
end
