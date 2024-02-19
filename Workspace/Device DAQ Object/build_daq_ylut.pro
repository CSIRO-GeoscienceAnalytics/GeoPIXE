function build_daq_ylut, file, output=output, error=error

; This assumes correct absolute Y values using step and dir.
; This is NOT necessarily correct using encoders and the empirical Y value
; algorithm in Fortran. Instead, we'll need to build this table
; during read_buffer reading of files during a full (no cluster) sort.
;
; Build a new Y table ...

COMPILE_OPT STRICTARR
print,'build_daq_ylut: Scan blog files for Y lookup table data ...'
error = 1
f = strip_file_ext( file[0]) + '.*'
fs = find_file2(f)
nf = n_elements(fs)
if nf lt 1 then return, 0L

q = where( inumeric( extract_extension(fs)) eq 1, nq)
if nq eq 0 then return, 0L

n = max( long(extract_extension(fs[q])))+1
ylut = lonarr(n)
foundY = bytarr(n)
n_buffer = 100000L
first = 1

on_ioerror, bad
for i=0,nq-1 do begin
	j = long(extract_extension(fs[q[i]]))
	if i mod 10 eq 0 then print,'	file = ',fs[q[i]],'  index = ',j

	if first then begin
		openr, lun, fs[q[i]], /get_lun
		first = 0
	endif else begin
		openr, lun, fs[q[i]]
	endelse
	p = read_maia_segments( fs[q[i]], n_buffer=n_buffer, unit=lun, /veto_progress)
	if ptr_valid(p[0]) then begin
		pp = ptr_new( p)
		d = get_maia_details( pp, error=err)
		if err eq 0 then begin
			ylut[j] = (j eq 0) ? 0 : d.id.Y
			foundY[j] = d.id.noY eq 0
		endif
	endif
	if i lt (nq-1) then begin
		close, lun
	endif else begin
		close_file, lun
	endelse
	ptr_free, p
	ptr_free, pp
endfor

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
			ylut[q1[q2]] = shift_ylut[q1[q2]]			; missing ET, use next file
		endif
		if max(q1) eq (n-1) then begin
			ylut[n-1] = 16*1024L - 1				; make missing last file Y large
		endif
	endif
endif

write_daq_ylut, ylut, file[0], output=output, /strip, error=error

error = 0
return, ylut

bad:
	close_file, lun
	return, 0L
end
	
