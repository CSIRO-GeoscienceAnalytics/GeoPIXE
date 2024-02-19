pro analyze_spring8_37U, filei, error=error

; 'file' is a "*_mapping.txt" file
; Will also look for a "*mapping_mca_ch1.txt", "*mapping_mca_ch2.txt", ... file(s)

COMPILE_OPT STRICTARR
ErrorNo = 0
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
       warning,'analyze_spring8_37U',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return
   endif
endif
error = 1

	if n_elements(filei) eq 0 then begin
		filei = 'C:\Software\Data\Spring8\Brugger\OM_SP8_TS5_909_mapping_Fine1-2_mca_ch1.txt'
	endif
	file2 = filei

	file = strip_file_ext( file2)
	k = locate( '_ch', file)
	if k ge 0 then begin
		i_ADC = fix2( strmid( file, k+3))
	endif else begin
		i_ADC = 1
	endelse

	file = strip_file_m( file, ending = ['_ch0','_ch1','_ch2','_ch3','_ch4'])
	file = strip_file_m( file, ending = '_mca')
	file1 = file + '.txt'
	s = ''

	on_ioerror, bad_file2
	openr, unit2, file2, /get_lun
	on_ioerror, bad_read2
	readf, unit2, s
	str = strsplit( s, string(9B), /extract)
	energy = float2( str[1:*])

;	Seems that energy row has one extra item, than the channels rows ...

	readf, unit2, s
	str = strsplit( s, string(9B), /extract)
	n_energy = n_elements(str)-1
	close_file, unit2
	
	on_ioerror, bad_file1
	openr, unit1, file1, /get_lun
	on_ioerror, bad_read1
	readf, unit1, s
	readf, unit1, s
	str = strsplit( s, string([9B]), /extract)
	n_title = n_elements(str)
	qx = where( str eq 'x/um', nqx)
	if nqx eq 0 then bad_x
	qy = where( str eq 'z/um', nqy)
	if nqy eq 0 then bad_y
	q0 = where( str eq 'ch1(I0)', nq0)
	if nq0 eq 0 then bad_I0
	qicr = where( str eq 'ICR_Ch1', nqicr)
	if nqicr eq 0 then bad_ICR

	data = fltarr(n_title,10000000L)
	on_ioerror, cont1
	readf, unit1, data
cont1:
	close_file, unit1
	q = where(data[0,*] ne 0., nq)
	if nq eq 0 then goto, bad_data1
	data = data[*,q]
	x = reform( data[qx[0],*])
	y = reform( data[qy[0],*])
	I0a = reform( data[q0[0],*])
	ICRa = reform( data[qicr[0],*])

	dy = y - shift(y,-1)							; Y changing in "next" pixel
	q = where( abs(dy) gt 0.1)
	x_coords = x[q[0]+1:q[1]]						; skips first scan line, which misses pixel 0
	y_coords = y[q]

	xsize = n_elements(x_coords)
	ysize = n_elements(y_coords)

	I0 = fltarr(xsize,ysize)
	ICR = fltarr(xsize,ysize)

	I0[0:n_elements(I0a)-1] = I0a
	ICR[0:n_elements(ICRa)-1] = ICRa

	name = strip_file_m( strip_file_ext( strip_path(file1)),ending='_mapping')
	str = strsplit( name, '_', /extract)
	id = str[ n_elements(str)-1]

	de = energy-shift(energy,-1)
	cal_a = mean( abs(de[1:n_elements(de)-2]))
	cal_b = energy[0]
	file3 = strip_file_ext( file1) + '.mdaq'

	on_ioerror, bad_mdaq
	openw, unit3, file3, /get_lun
	on_ioerror, bad_mdaq2
	printf, unit3, 'set run.id ' + id
	printf, unit3, 'set run.name ' + name

;	Will make the assumption that the scans move in +ve delta X,Y directions

	printf, unit3, 'set scan.mode STEP'
	printf, unit3, 'set scan.algorithm RASTER'
	printf, unit3, 'set scan.xpixels ' + str_tidy(xsize)
	printf, unit3, 'set scan.ypixels ' + str_tidy(ysize)
	printf, unit3, 'set scan.width ' + str_tidy( max(x_coords)-min(x_coords))
	printf, unit3, 'set scan.height ' + str_tidy( max(y_coords)-min(y_coords))
	printf, unit3, 'set scan.xorigin ' + str_tidy( min(x_coords))
	printf, unit3, 'set scan.yorigin ' + str_tidy( min(y_coords))

;	Or, should we set "st0" ?

	stn = 'st' + str_tidy( i_ADC)
	printf, unit3, 'set '+stn+'.detector_name SXRF'
	printf, unit3, 'set '+stn+'.energy_cal_a ' + str_tidy(cal_a)
	printf, unit3, 'set '+stn+'.energy_cal_b ' + str_tidy(cal_b)
	printf, unit3, 'set '+stn+'.channels ' + str_tidy(n_energy)
	close_file, unit3

	version = -1L
	file4 = strip_file_ext( file1) + '.flux0'
	on_ioerror, bad_flux
	openw, unit4, file4, /get_lun, /XDR
	on_ioerror, bad_flux2
	writeu, unit4, version
	writeu, unit4, long(xsize), long(ysize)
	writeu, unit4, I0
	close_file, unit4

	version = -1L
	file5 = strip_file_ext( file1) + '.ICR'
	on_ioerror, bad_ICR
	openw, unit5, file5, /get_lun, /XDR
	on_ioerror, bad_ICR2
	writeu, unit5, version
	writeu, unit5, long(xsize), long(ysize)
	writeu, unit5, ICR
	close_file, unit5
	return
	
bad_file1:
	print,'Spring8 analyze: error opening file: '+file1
	close_file, unit1
	return
bad_read1:
	print,'Spring8 analyze: bad read from file: '+file1
	close_file, unit1
	return
bad_data1:
	print,'Spring8 analyze: no frame metadata found in file: '+file1
	return
bad_file2:
	print,'Spring8 analyze: error opening file: '+file2
	close_file, unit2
	return
bad_read2:
	print,'Spring8 analyze: bad read from file: '+file2
	close_file, unit2
	return
bad_mdaq:
	print,'Spring8 analyze: bad open of MDAQ file: '+file3
	close_file, unit3
	return
bad_mdaq2:
	print,'Spring8 analyze: bad write to MDAQ file: '+file3
	close_file, unit3
	return
bad_flux:
	print,'Spring8 analyze: bad open of Flux0 file: '+file4
	close_file, unit4
	return
bad_flux2:
	print,'Spring8 analyze: bad write to Flux0 file: '+file4
	close_file, unit4
	return
bad_ICR:
	print,'Spring8 analyze: bad open of ICR file: '+file5
	close_file, unit5
	return
bad_ICR2:
	print,'Spring8 analyze: bad write to ICR file: '+file5
	close_file, unit5
	return
end
