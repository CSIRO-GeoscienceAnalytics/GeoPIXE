function build_mdaq2_ylut, file, output=output, slow_axis=slow_axis, error=error

; This assumes correct absolute Y values using step and dir.
; This is NOT necessarily correct using encoders and the empirical Y value
; algorithm in Fortran. Instead, we'll need to build this table
; during read_buffer reading of files during a full (no cluster) sort.
;
; Build a new Y table ...

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
       warning,'build_mdaq2_ylut',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pp
       error = 1
       return, 0L
    endif
endif
if n_elements(slow_axis) eq 0 then slow_axis=1

print,'build_mdaq2_ylut: Scan blog files for Y lookup table data ...'
error = 1
f = strip_file_ext( file[0])
l = locate_last('_',f)
stub = strmid(f,0,l)
f = stub + '_*.dat'
fs = find_file2(f)
nf = n_elements(fs)
if nf lt 1 then return, 0L

fs1 = strip_file_ext(fs)
l = locate_last('_',fs1)
ext = fs1
for i=0,nf-1 do ext[i] = strmid(fs1[i],l[i]+1)
q = where( inumeric( ext) eq 1, nq)
if nq eq 0 then return, 0L

n = max( long(ext[q]))+1
ylut = lonarr(n)
foundY = bytarr(n)
n_buffer = 200000L
first = 1

on_ioerror, bad
for i=0,nq-1 do begin
	j = long(ext[q[i]])
	if i mod 10 eq 0 then print,'	file = ',fs[q[i]],'  index = ',j

	if first then begin
		openr, lun, fs[q[i]], /get_lun
		first = 0
	endif else begin
		openr, lun, fs[q[i]]
	endelse
	p = read_mdaq2_segments( fs[q[i]], n_buffer=n_buffer, unit=lun, /veto_progress)
	if ptr_valid(p[0]) then begin
		pp = ptr_new( p)
		d = get_mdaq2_details( pp, error=err)
		if err eq 0 then begin
			case slow_axis of
				0: y = d.id.x
				1: y = d.id.y
				2: y = d.id.z
			endcase
			ylut[j] = (j eq 0) ? 0 : (y > 0)
			foundY[j] = d.id.noY eq 0
		endif
	endif
	if i lt (nq-1) then begin
		close, lun
	endif else begin
		close_file, lun
	endelse
	free_fx_record, pp
endfor

q1 = where( foundY eq 1, nq1)
if nq1 eq 0 then begin
	print,'build_mdaq2_ylut: No Y found in all files.'
	error = 1
	return, 0L
endif

; Fix ylut problems, especially for historical blog data.
;	1. large Y in blog #1, due to fly-back:
;		mostly effects blog #1, set it to zero.
;	2. missing ET data in a file:
;		need to Y for next blog file.

if (foundY[0] eq 0) then ylut[1] = 0			; if none in file #0, then #1 starts at 0

ylut[0] = 0										; blog #0 always should start from 0

; non monotonic Y means fly-back?
q1 = where( (ylut gt shift(ylut,-1)) and (shift(ylut,-1) ne 0), nq1)
if nq1 gt 0 then begin		
	ylut[q1[0:(nq1 < 2)-1]] = 0					; reset just for first 2 blog files.
endif

if nq gt 1 then begin
	q1 = where(foundY eq 0, nq1)
	if nq1 ge 1 then begin
		q2 = where(q1 ge 2, nq2)
		if nq2 gt 0 then begin
			shift_ylut = shift(ylut,-1)
			ylut[q1[q2]] = shift_ylut[q1[q2]]	; missing ET, use next file
		endif
		if max(q1) eq (n-1) then begin
			ylut[n-1] = 16*1024L - 1			; make missing last file Y large
		endif
	endif
endif

write_mdaq2_ylut, ylut, file[0], output=output, /strip, error=error

error = 0
return, ylut

bad:
	close_file, lun
	return, 0L
end
	
