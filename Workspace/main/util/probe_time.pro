pro probe_time, unit

if n_elements(unit) lt 1 then return

f = fstat(unit)
if f.open eq 1 then begin

	ft = long64( max([f.atime, f.ctime, f.mtime]))
	st = long64( systime(/seconds, /utc))
	dt = st - ft

	s = [	'Access time: '+string(f.atime), $
			'Create time: '+string(f.ctime), $
			'Modify time: '+string(f.mtime), $
			'File max time: '+string(ft), $
			'System time: '+string(st), $
			'Delta time: '+string(dt)]
	warning,'probe_time',s
endif
return
end
