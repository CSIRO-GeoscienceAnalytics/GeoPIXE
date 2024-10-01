pro export_da2, dain, F, big_endian=big_endian, ascii=ascii, group=group, $
					pileup=pileup, throttle=throttle, linear=linear1, max_size=max_size

;   Export the Dynamic Analysis matrix to a binary file 'F.dmx2'
;   for input into a data acquisition system.
;
;   By default the binary output will be BYTE-SWAPPED as appropriate for sending
;   to a 'little endian' processor, such as in a PC.
;   Set "big_endian=1" to disable this for a VME target processor.
;
;   If /ascii, then output as an ASCII dmt2 file.
;   'max_size' sets maimum length of DA matrix rows on export.
;
;   'dain' is a pointer to the matrix struct, or the struct itself.
;
;	'linear' suppressed for now, as we don't have real-time tools to support the
;	v2 Maia Linearization scheme, which uses per chip Linearization functions.

if n_elements(big_endian) lt 1 then big_endian=0
if n_elements(ascii) lt 1 then ascii=0
if n_elements(group) lt 1 then return
if n_elements(pileup) lt 1 then pileup=''
if n_elements(throttle) lt 1 then throttle=''
if n_elements(linear1) lt 1 then linear1=''
if n_elements(max_size) lt 1 then max_size=2048

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
       warning,'export_da2',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, bad_io
    endif
endif

	if n_params() lt 1 then goto, bad_args
	linear = linear1
	if linear ne '' then begin
		warning,'export_DA2',['Export does not support a linearization correction in real-time.', $
							'No tools for now to support the v2 Maia Linearization scheme,', $
							'which uses per chip Linearization functions.']
		linear = ''
	endif

	da = dain
    if size(da,/tname) eq 'POINTER' then begin
		if ptr_valid(da) eq 0 then goto, bad_da
		da = *da
	endif
    if size(da,/tname) ne 'STRUCT' then goto, bad_da

	mode = 1
	if ascii then mode=0
;	select = export_da_select( group, big_endian=big_endian, mode=mode, pileup=pileup, throttle=throttle, linear=linear)
	select = export_da_select( group, big_endian=big_endian, mode=mode, pileup=pileup, throttle=throttle)
	if select.error then goto, finish
	big_endian = select.options[0]
	pileup = select.pileup
	throttle = select.throttle
;	linear = select.linear
	linear = ''

	ext = ['dmt','dmx','dmm']
	if lenchr(F) lt 1 then begin
		file = strip_file_ext(da.file[0]) + '.' + ext[mode]
		path = extract_path(file)
		F = file_requester( /write, filter = '*.'+ext[mode], file=file, $
			path=path, group=group, $
			title='Export DA matrix for real-time', /fix_filter)
		if F[0] eq '' then return
	endif

    version = -2L							; revised Sep '24
	on_ioerror, bad_io
	close, 1
	openw, 1, F
	widget_control, /hourglass

	b = byte(da.el)
	ns = n_elements(b[*,0]) < 4
	if n_elements(b[0,*]) ne da.n_el then goto, bad_nel
	b[ns-1,*] = 0B

	q = where(finite(da.matrix) eq 0)
	if q[0] ne -1 then da.matrix[q] = 0.0

;	Later this ASCII format file should include details that will enable the on-line
;	display process to determine conc and MDL values. This will require the addition
;	of MDL and Charge data to this file. Extras, such as pure element overlays, will
;	require the addition of the Yield (only for Correct?) and Pure element data.

   	case mode of
    	0: begin							; ascii dmt
			n_el = da.n_el < 32				; limit to 32 elements and
			cal = da.cal					; map onto 'max_size' channels (see below)
			matrix = da.matrix
			mine = max_size
			maxe = 0
			for i=0L,da.n_el-1 do begin
				q = where(matrix[*,i] ne 0.0)
				if q[0] ne -1 then begin
					mine = mine < q[0]
					maxe = maxe > max(q)
				endif
			endfor
			if mine gt 4000 then warning,'export_da2','Large parts of DA matrix are zero.'
			if maxe le mine then warning,'export_da2','Zero or null DA matrix.'
	
	    	matrix = matrix[mine:maxe,0:n_el-1]
			siz = maxe - mine + 1
			if siz gt max_size then begin
				el = mine * cal.a + cal.b
				eh = maxe * cal.a + cal.b
				cal.a = (eh-el)/(max_size-1)
				cal.b = el
				matrix = congrid(matrix,max_size,n_el)
				siz = max_size
			endif else begin
				cal.b = mine * cal.a + cal.b
			endelse
	
	    	printf, 1, '#file'
			printf, 1, da.file
			printf, 1, '#element'
			for i=0L,n_el-1 do begin
				printf, 1, str_tidy(i), ' ', da.el[i]
			endfor
			printf, 1, '#Dcal'
			printf, 1, str_tidy(cal.b), ' ', str_tidy(cal.a), ' 0.0'
			printf, 1, '#DA'
			for i=0L,n_el-1 do begin
				for j=0L,siz-1 do begin
					printf, 1, str_tidy(i), ' ', str_tidy(j), ' ', str_tidy(matrix[j,i])
				endfor
			endfor
	
			if pileup ne ''  then begin
				printf, 1, '#pileup'
				on_ioerror, bad_pileup
				openr, 2, pileup
				s = ''
				while( EOF(2) eq 0) do begin
					readf, 2, s
					if extract(s,0,0) ne '#' then printf, 1, s
				endwhile
				on_ioerror, bad_io
bad_pileup:
				close, 2
			endif
	
	    	if throttle ne '' then begin
				printf, 1, '#throttle'
				on_ioerror, bad_throttle
				openr, 2, throttle
				s = ''
				while( EOF(2) eq 0) do begin
					readf, 2, s
					if extract(s,0,0) ne '#' then printf, 1, s
				endwhile
				on_ioerror, bad_io
bad_throttle:
				close, 2
			endif

;			if linear ne '' then begin
;				F = get_linearize( linear, max=maxe, do_linear=do_linear)
;				if do_linear then begin
;					printf, 1, '#linear'
;					for i=0L,n_elements(F)-1 do begin
;						printf, 1, str_tidy(i), '  ', str_tidy(F[i])
;					endfor
;				endif else begin
;					warning,'export_da2','Error reading Linearization file: '+linear,/error
;				endelse
;			endif
			end

		1: begin							; dmx binary

;			Binary export supports basic real-time accumulation from events using a DA matrix.
;			The 'pileup' and 'throttle' schemes (as used in Maia) are not included for now.
;			
;			Byte swap if appropriate to go to the Big-Endian VME processor
;			i.e. set /big_endian_data to go to a VME processor,
;		    	 and big_endian_data=0 to go to a PC (Windows or Linux).

			out = {	version:  long(version), $					; version #
					header:	{	n_el:	long(da.n_el), $		; # elements (rows)
								size:	long(da.size), $		; # channels (columns)
								ca:		da.cal.a, $				; energy cal A (gain keV/ch) of matrix
								cb:		da.cal.b, $				; energy cal B (offset keV) of matrix
								ns:		long(ns) }, $			; # of characters (includes NULL) in each element
							
;	Normally, the individual detector cals are not supplied here, as they are set in the data acquisition
;	system. The DA method only needs to know about the E cal of the DA matrix itself. It is up to the
;	realtime data acquisition system to map detector signals onto energy and then use the DA.cal supplied
;	to map this onto matrix column.
;	
;					compress:	long(da.ecompress), $			; optional spectrum compression (Lund?)
;					n_det:		long(n_det), $					; # of detectors and cals following
;					cal: { adc:	long(adc), $					; ADC number for each cal
;							a:	ca, $							; array of cal A from these detectors
;							b:	cb }, $							; array of cal B (n_det floats)

					matrix:	da.matrix, $						; DA matrix - float(size,n_el)
					el:		b }									; element names byte(ns,n_el) (NULL terminated strings)

			swap_bytes, out, big_endian_data=big_endian
	
			writeu, 1, out.version
			writeu, 1, out.header
;			writeu, 1, out.n_det								; should be supplied by real-time routine
;			writeu, 1, out.cal									; should be supplied by real-time routine
			writeu, 1, out.el
			writeu, 1, out.matrix
			end

		2: begin							; dmm binary (iThemba)

;			Set byte swap if appropriate to go to the Big-Endian VME processor
;			iThemba will mostly use big_endian_data=0 to go to a PC (Windows or Linux).
;
;			This does not include full detector array code, just multiplicity (n_det).
;			Later will need 'rGamma' and 'active' arrays for full array mode.

			cal = da.cal					; map onto 'max_size' channels (see below)
			matrix = da.matrix
			mine = max_size
			maxe = 0
			for i=0L,da.n_el-1 do begin
				q = where(matrix[*,i] ne 0.0)
				if q[0] ne -1 then begin
					mine = mine < q[0]
					maxe = maxe > max(q)
				endif
			endfor
			if mine gt 2000 then warning,'export_DA2','Large parts of DA matrix are zero.'
			if maxe le mine then warning,'export_DA2','Zero or null DA matrix.'
			
			matrix = matrix[mine:maxe,0:da.n_el-1]
			siz = maxe - mine + 1
			if siz gt max_size then begin
				el = mine * cal.a + cal.b
				eh = maxe * cal.a + cal.b
				cal.a = (eh-el)/(max_size-1)
				cal.b = el
				matrix = congrid(matrix,max_size,da.n_el)
				siz = max_size
			endif else begin
				cal.b = mine * cal.a + cal.b
			endelse

			out = {	version:  long(version), $					; version #
					header:	{	n_el:	long(da.n_el), $		; # elements (rows)
								size:	long(siz), $			; # channels (columns)
								ca:		cal.a, $				; energy cal A (gain keV/ch) of matrix
								cb:		cal.b, $				; energy cal B (offset keV) of matrix
								ns:		long(ns) }, $			; # of characters (includes NULL) in each element

;	Normally, the individual detector cals are not supplied here, as they are set in the data acquisition
;	system. The DA method only needs to know about the E cal of the DA matrix itself. It is up to the
;	realtime data acquisition system to map detector signals onto energy and then use the DA.cal supplied
;	to map this onto matrix column.
;
;					n_det:		long(n_det), $					; # of detectors and cals following
;					cal: { adc:	long(adc), $					; ADC number for each cal
;							a:	ca, $							; array of cal A from these detectors
;							b:	cb }, $							; array of cal B (n_det floats)

					matrix:	da.matrix, $						; DA matrix - float(size,n_el)
					el:		b }									; element names byte(ns,n_el) (NULL terminated strings)

			swap_bytes, out, big_endian_data=big_endian

			writeu, 1, out.version
			writeu, 1, out.header
;			writeu, 1, out.n_det								; should be supplied by real-time routine
;			writeu, 1, out.cal									; should be supplied by real-time routine
			writeu, 1, out.el
			writeu, 1, out.matrix
			end
	endcase
	
finish:
	close,1
	return

bad_args:
	warning,'export_da2','Missing or null arguments in call.',/error
	return
bad_da:
	warning,'export_da2','Bad DA matrix',/error
	return
bad_nel:
	warning,'export_da2','# elements does not match array dimension.',/error
	goto, finish
bad_io:
	warning,'export_da2','export_da: bad I/O',/error
	goto, finish
end
