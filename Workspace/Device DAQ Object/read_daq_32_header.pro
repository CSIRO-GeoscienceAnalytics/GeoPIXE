function read_daq_32_header, unit, error=err, no_scan=no_scan

; /no_scan to suppress scanning for X range if a scan record is not found.

err = 1
if n_elements(no_scan) lt 1 then no_scan=0
n_buffer = (no_scan eq 0) ? 2000000L : 500000L

stat = fstat( unit)
file = stat.name

F = find_file2(strip_file_ext(strtrim(file,2))+'.*', /extension_numeric)
path = extract_path(F[0])
g = strip_path(F)
q = sort_file_numeric(g,/ext)
files = F[q]
nm = 3
nf = n_elements(files)

for i=0L,(nf<nm)-1 do begin
	if strlowcase(file) eq strlowcase(files[i]) then begin
		pr = read_maia_segments( files[i], n_buffer=n_buffer, unit=unit, /veto_progress)
	endif else begin
		pr = read_maia_segments( files[i], n_buffer=n_buffer, /veto_progress)
	endelse
	if ptr_valid(pr[0]) then begin
		if n_elements(p) gt 0 then begin
			p = [p,pr]
		endif else begin
			p = pr
		endelse
	endif
endfor

pp = ptr_new( p)
d = get_daq_details( pp, error=err)

if (err eq 0) then begin
	if ((d.scan.xrange eq 0) and (no_scan eq 0)) or (d.energy eq 0.) then begin
		gprint,level=2,'read_daq_32_header: Scan record or energy not found. Scan for energy, X range ...'
		n_buffer = 2000000L
		j = (((nm+1) < (nf-1)) -1) > 0
		if strlowcase(file) eq strlowcase(files[j]) then begin
			pr = read_maia_segments( files[j], n_buffer=n_buffer, unit=unit, /veto_progress)
		endif else begin
			pr = read_maia_segments( files[j], n_buffer=n_buffer, /veto_progress)
		endelse
		if ptr_valid(pr[0]) then begin
			if n_elements(p) gt 0 then begin
				p = [p,pr]
			endif else begin
				p = pr
			endelse
		endif
		pp = ptr_new( p)
		d = get_daq_details( pp, error=err)
		if (d.scan.xrange eq 0) then begin
			gprint,level=2,'read_daq_32_header: No scan X-range found on re-try!'
		endif
		if (d.energy eq 0.) then begin
			gprint,level=2,'read_daq_32_header: No energy found on re-try!'
		endif
	endif
endif

if ptr_good(pp) then begin
	ptr_free, *pp
	ptr_free, pp
endif
if err then return, 0L

err = 0
return, d
end

							