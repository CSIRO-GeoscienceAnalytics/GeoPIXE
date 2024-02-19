function dfile, unit, key, dt=dt

; Test the unit for file modification time.
; Compare this to current system time.
; If 'in future' then flag an error.
;
; Use:
;	if dfile(1, 5481) ne -3175 then begin
;		warning,'routine-name',['Platform ERROR 71','Please consult CSIRO.'], /error
;		exit, /no_confirm
;	endif
;
; Put this line in routines:
;		read_geopixe_image, read_spec, spec_evt, da_evt

if key ne 5481 then return, 0
if n_elements(unit) lt 1 then return, 0

f = fstat(unit)
if f.open eq 1 then begin

	ft = long64( max([f.atime, f.mtime]))		; leave out f.ctime
	st = long64( systime(/seconds, /utc))
	dt = st - ft

; Any file time in the future ...

	if ft gt st+3000000LL then return, 0
endif

return, -3175
end
