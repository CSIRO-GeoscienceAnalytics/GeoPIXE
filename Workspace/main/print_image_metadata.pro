pro print_image_metadata, p, files=dai, output=file, stats=stats

;	Print a file of image metadata for image struct 'p'
;	or read in file 'dai' if 'p' not supplied.
;	Output to 'file', or (*p).file if this not provided.
;
;	files		source image file
;	output		output metadata file
;	
;	/stats		add image pixel statistics

	COMPILE_OPT STRICTARR

	if n_elements(stats) eq 0 then stats=0

	if n_elements(p) eq 0 then begin

;		No 'p' supplied, so read DAI file. Take care as (*p).file there may be old
;		and not correct (e.g. if DAI file has been moved). Best to use 'file',
;		or base output on 'dai' file read in.

		if n_elements(dai) eq 0 then return
		if dai eq '' then return
		p = read_geopixe_image( dai, error=err)
		if err then goto, bad2
		if n_elements(file) eq 0 then file = strip_file_ext(dai)+'-metadata.json'
	endif else begin

;		'p' passed from image window, so (*p).file is up to date.

		if ptr_good(p) eq 0 then goto, bad3
		if n_elements(file) eq 0 then file = strip_file_ext((*p).file)+'-metadata.json'
	endelse

	list = image_details( p, stats=stats, /json)

	openw, lun, file, /get_lun
	for i=0,n_elements(list)-1 do begin
		printf, lun, list[i]	
	endfor

done:
	close_file, lun
	return

bad: warning, 'print_image_metadata','Bad write to file: '+file
	goto, done
bad2: warning, 'print_image_metadata','Bad image file: '+dai
	goto, done
bad3: warning, 'print_image_metadata','Bad image pointer.'
	goto, done
end
