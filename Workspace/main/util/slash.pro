function slash

; Could now use the new function "path_sep()" to do this ...

case !version.os_family of
	'Windows':	sl = '\'
	'MacOS':	sl = ':'
	'UNIX':		sl = '/'
	'unix':		sl = '/'
	'VMS':		sl = '.'
	else:		sl = '/'
endcase

return, sl
end
