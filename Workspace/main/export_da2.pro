pro export_da2, dain, F, big_endian=big_endian, ascii=ascii, group=group

;   Export the Dynamic Analysis matrix to a binary file 'F.dmx2'
;   for input into a data acquisition system.
;
;   If /ascii, then output as an ASCII dmt2 file.
;
;   By default the binary output will be BYTE-SWAPPED as appropriate for sending
;   to a 'little endian' processor, such as in a PC.
;   Set "big_endian=1" to disable this for a VME target processor.
;
;   'dain' is a pointer to the matrix struct, or the struct itself.

if n_elements(big_endian) lt 1 then big_endian=0
if n_elements(ascii) lt 1 then ascii=0
if n_elements(group) lt 1 then return

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

    if n_params() lt 3 then goto, bad_args
    if lenchr(F) lt 1 then goto, bad_args

    da = dain
    if size(da,/tname) eq 'POINTER' then begin
       if ptr_valid(da) eq 0 then goto, bad_da
       da = *da
    endif
    if size(da,/tname) ne 'STRUCT' then goto, bad_da

	mode = 0
	if ascii eq 0 then mode=1
	select = export_da_select( group, file=F, big_endian=big_endian, mode=mode)
	if select.error then goto, finish
	ascii = 1 - select.mode
	big_endian = select.options[0]
	pileup = select.pileup
	throttle = select.throttle
	linear = select.linear

    version = -1L
    on_ioerror, bad_io
    close, 1
    if ascii then begin
       F = strip_file_ext(F) + '.dmt2'
    endif else begin
       F = strip_file_ext(F) + '.dmx2'
    endelse
    openw, 1, F
    widget_control, /hourglass

    b = byte(da.el)
    ns = n_elements(b[*,0]) < 4
    if n_elements(b[0,*]) ne da.n_el then goto, bad_nel
    b[ns-1,*] = 0B

    q = where(finite(da.matrix) eq 0)
    if q[0] ne -1 then da.matrix[q] = 0.0

; Later this ASCII format file should include details that will enable the on-line
; display process to determine conc and MDL values. This will require the addition
; of MDL and Charge data to this file. Extras, such as pure element overlays, will
; require the addition of the Yield (only for Correct?) and Pure element data.

    if ascii then begin
        n_el = da.n_el < 32           ; limit to 32 elements and
        cal = da.cal                 ; map onto 2048 channels (see below)
       matrix = da.matrix
       mine = 2048
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
       if siz gt 2048 then begin
         el = mine * cal.a + cal.b
         eh = maxe * cal.a + cal.b
         cal.a = (eh-el)/2047
         cal.b = el
         matrix = congrid(matrix,2048,n_el)
         siz = 2048
       endif else begin
         cal.b = mine * cal.a + cal.b
       endelse

       printf, 1, '#file'
       printf, 1, da.file
       printf, 1, '#element'
       for i=0L,n_el-1 do begin
         printf, 1, str_tidy(i), ' ', da.el[i]
       endfor
       if n_elements(pileup) gt 0 then begin
       if lenchr(pileup) gt 0 then begin
	    printf, 1, '#pileup'
	    on_ioerror, bad_pileup
		openr, 2, pileup
		s = ''
		while( EOF(2) eq 0) do begin
			readf, 2, s
			if extract(s,0,0) ne '#' then printf, 1, s
		endwhile
	    on_ioerror, bad_io

;         if ptr_valid(pileup) then begin
;          n = n_elements((*pileup)[*,0])    ;< siz
;          for i=0L,n-1 do begin
;              qi = where((*pileup)[i,*] eq 1)
;              if qi[0] ne -1 then begin
;                 tmin = min(qi)
;                 tmax = max(qi)
;                 printf, 1, str_tidy(i), ' ',str_tidy(tmin), ' ',str_tidy(tmax)
;              endif
;          endfor
;         endif
bad_pileup:
		close, 2
      endif
      endif
       printf, 1, '#Dcal'
       printf, 1, str_tidy(cal.b), ' ', str_tidy(cal.a), ' 0.0'
       printf, 1, '#DA'
       for i=0L,n_el-1 do begin
         for j=0L,siz-1 do begin
          printf, 1, str_tidy(i), ' ', str_tidy(j), ' ', str_tidy(matrix[j,i])
         endfor
       endfor
       printf, 1, '#calibration'
       for i=0L,n_det-1 do begin
         printf, 1, str_tidy(adc[i]), ' ', str_tidy(cb[i]), ' ', sstr_tidy(ca[i]), ' 0.0'
       endfor

       if n_elements(throttle) gt 0 then begin
       if lenchr(throttle) gt 0 then begin
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
       endif

		if n_elements(linear) gt 0 then begin
			if lenchr(linear) gt 0 then begin
				F = get_linearize( linear, max=maxe, do_linear=do_linear)
				if do_linear then begin
					printf, 1, '#linear'
					for i=0L,n_elements(F)-1 do begin
						printf, 1, str_tidy(i), '  ', str_tidy(F[i])
					endfor
				endif else begin
					warning,'export_da2','Error reading Linearization file.',/error
				endelse
			endif
		endif

    endif else begin

;   Byte swap if appropriate to go to the Big-Endian VME processor
;   i.e. set /big_endian_data to go to a VME processor,
;     and big_endian_data=0 to go to a PC (Windows or Linux).

      out = {    header:    {version:  long(version), $     ; version #
              n_el:   long(da.n_el), $       ; # elements (rows)
              size:   long(da.size), $       ; # channels (columns)
              compress:  long(da.ecompress), $    ; optional spectrum compression
              adc:     long(da.station), $   ; detector used to make DA
              ca:         da.cal.a, $          ; energy cal A (gain keV/ch) of matrix
              cb:         da.cal.b, $          ; energy cal B (offset keV) of matrix
              ns:         long(ns), $          ; # of characters (includes NULL) in each element
              n_det:     long(n_det) }, $     ; # of detectors and cals following
         cal: { adc:   long(adc), $         ; ADC number for each cal
                a:     ca, $              ; array of cal A from these detectors
                b:     cb }, $            ; array of cal B (n_det floats)
         matrix:  da.matrix, $               ; DA matrix - float(size,n_el)
         el:   b }                     ; element names byte(ns,n_el) (NULL terminated strings)

       swap_bytes, out, big_endian_data=big_endian

       writeu, 1, out.header
       writeu, 1, out.cal
       writeu, 1, out.matrix
       writeu, 1, out.el
    endelse

finish:
    close,1
return

bad_args:
    warning,'export_da2','Missing or null arguments in call.',/error
    return
bad_da:
    warning,'export_da2','Bad DA matrix',/error
    return
bad_p:
    warning,'export_da2','Bad spectrum pointer; no spectra?',/error
    return
bad_nel:
    warning,'export_da2','# elements does not match array dimension.',/error
    goto, finish
no_adcs:
    warning,'export_da2','No valid ADC energy calibrations.',/error
    goto, finish
bad_io:
    warning,'export_da2','export_da: bad I/O',/error
    goto, finish
end
