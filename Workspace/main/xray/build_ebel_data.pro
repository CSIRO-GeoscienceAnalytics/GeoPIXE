function build_ebel_data

;	H. Ebel, et al., X-Ray Spectrom. 32 (2003) 442-451

	common c_working_dir2, geopixe_database_path

ntot=94

for z=1,ntot do begin
	name = strcompress( strupcase(element_name(z)+'.mu'), /remove_all)
	name = geopixe_database_path+'Ebel/'+name

;	if z eq 92 then begin
;		print,'hello'
;	endif
	d = decode_ebel(name)
	if size(d,/tname) ne 'STRUCT' then begin
		print,'build_ebel_data','bad data for '+name
	endif else begin
		if n_elements(data) lt 1 then data=replicate(decode_ebel(''),100)
		data[z] = d
	endelse
endfor

return, data
end
