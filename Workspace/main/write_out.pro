pro write_out, unit, is_label, label, n_indent, command, $
			is_comment, comment

out = ''
if is_label eq 1 then begin
	out = 'L' + label + ':'
	printf, unit, out
endif

out = ''
if n_indent gt 0 then begin
	for k=0L,n_indent-1 do begin
		out = out + '	'
	endfor
endif
out = out + command
if is_comment eq 1 then out = out + '			; ' + comment
printf, unit, out
	
return
end