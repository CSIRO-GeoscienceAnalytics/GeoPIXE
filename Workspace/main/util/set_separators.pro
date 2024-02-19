pro set_separators, sep
;
common c_separators, separators

ns = n_elements(sep)
if ns gt 1 then begin
	separators = sep[0]
	for i=1L,ns-1 do separators = separators + sep[i]
endif else begin
	separators = sep
endelse

return
end
