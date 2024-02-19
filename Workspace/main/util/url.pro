function url, file_in

; Convert a file spec to URL form
; ":" to "|", "\" to "/"
; preceed by "file://localhost/"

file = file_in
sl = byte(slash())
sl = sl[0]
colon = byte(':')
colon = colon[0]

case !version.os_family of
	'Windows': begin
		b = byte(file)
		q = where(b eq colon)
		if q[0] ne -1 then b[q]=byte('|')
		q = where(b eq sl)
		if q[0] ne -1 then b[q]=byte('/')
		file = string(b)
		end

	'MacOS': begin
		i = locate(':',file)
		if i ge 0 then begin
			if i gt 0 then begin
				file = extract(file,0,i-1) + '|/' + extract(file,i+i,lenchr(file)-i-1)
			endif else begin
				file = '|/' + extract(file,i+i,lenchr(file)-i-1)
			endelse
			b = byte(file)
			q = where(b eq colon)
			if q[0] ne -1 then b[q]=byte('/')
			file = string(b)
		endif
		end

	else: begin
		end
endcase

return, 'file://localhost/' + file
end
