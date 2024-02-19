pro append_comment, str, is_comment, comment

if is_comment eq 0 then begin
	is_comment = 1
	comment = str
endif else begin
	comment = comment + str
endelse

return
end