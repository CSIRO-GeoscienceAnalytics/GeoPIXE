function file_lowcase, s

; Force lower case only for systems without case-sensitive filenames

case !version.os_family of
	'MacOS': begin
		return, s
		end
	'unix': begin
		return, s
		end
	else:
endcase

return, strlowcase(s)
end
