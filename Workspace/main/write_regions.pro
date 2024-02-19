pro write_regions, pp, F

; Write the Image Table region details to 'F'
; 'pp' is a pointer to the pointer array pointing to region structs.

ErrorNo = 0
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
		warning,'Write_regions',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif
common c_null_image_1, max_image_cal
if n_elements(max_image_cal) lt 1 then t=define()

if n_params() lt 2 then begin
	print,'write_regions: missing args.'
	return
endif
if lenchr(F) lt 1 then return
if ptr_valid(pp) eq 0 then return
if ptr_valid((*pp)[0]) eq 0 then return
F = strip_file_ext(F) + '.region'

	version = -30L									; .region version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR

	writeu,1, version
	writeu,1, (*(*pp)[0]).matrix
	n = n_elements(*pp)
	writeu,1, n
	for i=0L,n-1 do begin
		p = (*pp)[i]
		if ptr_valid(p) then begin
			writeu,1, (*p).mode
			writeu,1, ptr_good((*p).pmark[0]) ? (*p).analyze_type[0] : 1	; "1" for import region without pcorr shape
			if  ptr_good((*p).pmark[0]) then writeu,1, *((*p).pmark[0])

			writeu,1, (*p).analyze_type[1]
			if (*p).analyze_type[1] gt 0 then begin
				writeu,1, *((*p).pmark[1])
			endif
			writeu,1, (*p).el_shown
			if (*p).mode eq 1 then begin
				writeu,1, (*p).elx, (*p).ely
			endif
			writeu,1, (*p).note
			writeu,1, (*p).n_el
			writeu,1, *(*p).el
			writeu,1, *(*p).conc
			writeu,1, *(*p).error
			writeu,1, *(*p).mdl
			writeu,1, *(*p).centroid
;												(*p).sd, (*p).relsd not saved
;												(used locally onlyby Standards Wizard)			
			aok = ptr_good( (*p).ayield)
			writeu,1, aok
			if aok then begin
				writeu,1, *(*p).ayield
			endif
			
			n_comp =  ptr_good( (*p).phase) ? n_elements( *(*p).phase) : 0L
			if n_comp eq 1 then n_comp = 0L
			writeu,1, n_comp
			if n_comp gt 0 then writeu,1, *(*p).phase

			nc =  ptr_good( (*p).poverlay) ? n_elements( (*(*p).poverlay)[*,0]) : 0L
			nel =  ptr_good( (*p).poverlay) ? n_elements( (*(*p).poverlay)[0,*]) : 0L
			writeu,1, nc, nel
			if (nc gt 0) and (nel gt 0) then writeu,1, *(*p).poverlay

			writeu,1, (*p).nx, (*p).ny
			writeu,1, (*p).xoffset, (*p).yoffset
			writeu,1, (*p).xcompress, (*p).ycompress
			writeu,1, (*p).ystep
			writeu,1, (*p).xstep_on
			writeu,1, (*p).xstep
			writeu,1, (*p).cal_a, (*p).cal_b
			writeu,1, (*p).charge
			writeu,1, (*p).IC_total
			
			writeu, 1, (*p).IC.mode
			if (*p).IC.mode ne 0 then begin
				writeu, 1, (*p).IC
			endif
			np = 0
			if ptr_good((*p).plist) then np = n_elements(*(*p).plist)
			writeu, 1, np
			if np gt 0 then begin
				writeu, 1, *(*p).plist
			endif
			writeu, 1, (*p).dwell

			device_name = obj_valid((*p).DevObj) ? (*p).DevObj->name() : 'none'
 			writeu, 1, device_name
			
;-------------- object specific parameter write -----------------
;
; Now all device specific options are stored and managed by the Object,
; and not explicitly known by GeoPIXE. This includes the Maia options.

			if obj_valid((*p).DevObj) then begin
				(*p).DevObj->write_options, 1, error=err2
				if err2 then goto, bad_obj_io
			endif

;----------------------------------------------------------------

			writeu,1, (*p).file
			writeu,1, (*p).source
			writeu,1, (*p).source2
			writeu,1, (*p).ecompress
			writeu,1, (*p).throttle
			writeu,1, (*p).pileup
			writeu,1, (*p).linearize

			writeu,1, (*p).original_xsize, (*p).original_ysize
			writeu,1, (*p).scaled_x, (*p).scaled_y
			writeu,1, (*p).show_back

			writeu,1, 0L									; means 'q' NOT compressed
			writeu,1, n_elements( *((*p).q) )
			writeu,1, *((*p).q)

			writeu,1, (*p).events, (*p).step_events
			writeu,1, (*p).step_toggle, (*p).toggle_bit, (*p).toggle_station

			writeu,1, (*p).scanx, (*p).scany
			writeu,1, (*p).detector, (*p).channel
			writeu,1, (*p).sample
			writeu,1, (*p).grain
			writeu,1, (*p).comment

			writeu,1, (*p).array
			if (*p).array eq 1 then begin
				writeu,1, n_elements(*(*p).pactive)
				writeu,1, *(*p).pactive
				
				if ptr_good((*p).pcal) eq 0 then begin
					poly = fltarr(max_image_cal+1)
					cal0 = {order:1, units:'keV', poly:poly}
					cal = replicate(cal0, n_elements(*(*p).pactive))
				endif else begin
					cal = *(*p).pcal
				endelse
				writeu,1, cal
			endif
		endif
	endfor

	close,1
return

bad_io:
	print,'Write_regions: bad I/O'
	close, 1
	return
bad_obj_io:
	print,'Write_regions: bad object options I/O'
	close, 1
	return
end
