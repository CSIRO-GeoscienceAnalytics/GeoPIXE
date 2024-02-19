function get_xsort_threshold, thresh
;
;	Get sorted X-ray list, with rel > 'thresh'
;
common c_xsort_1, n_xsort, xsort_lines
common c_working_dir2, geopixe_database_path

if n_elements(thresh) lt 1 then thresh=0.0
if n_elements(n_xsort) lt 1 then n_xsort=0
if n_xsort lt 1 then begin
	xsort_lines = get_xsort(geopixe_database_path+'dat/sorted_xray_lines.txt')
	n_xsort = n_elements( xsort_lines.e)
endif

q = where( xsort_lines.rel gt thresh)
if q[0] eq -1 then return, 0

return, {e:xsort_lines.e[q], el:xsort_lines.el[q], $
			line:xsort_lines.line[q], rel:xsort_lines.rel[q], $
			IUPAC:iupac(xsort_lines.line[q])}
end
