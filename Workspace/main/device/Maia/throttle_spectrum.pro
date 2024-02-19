pro throttle_spectrum, p, group=group, view=view

; This is now not used.
; All this is now in throttle3_select.

if size(p,/tname) ne 'POINTER' then return
if size(*p,/tname) ne 'STRUCT' then return
if n_elements(group) lt 1 then group=(*p).group
siz = (*p).size							; Size of spectrum in channels
if n_elements(view) lt 2 then view=[1,siz-2]
if view[1] le view[0] then view=[1,siz-2]
view = view < (siz-1)

; Call modal pop-up to gather parameters.
; Hide away the parent widget ID in spectrum struct.

beam = 17.0								; X-ray beam energy --> more throttling near scatter peaks

pars = throttle2_select( group, total((*(*p).data)[view[0]:view[1]]))

if n_elements(pars) lt 1 then return
if size(pars,/tname) ne 'STRUCT' then return
if pars.error then return

time = pars.time						; acquisition time (sec)
detectors = pars.detectors				; number of detectors in array
max_rate = pars.max						; maximum rate for Blog
rate = pars.rate						; actual count-rate
factor = pars.factor					; factor by which to reduce spectrum total
if factor lt 1. then begin
	warning,'throttle_spectrum','no throttling needed'
	return
endif

throttle = build_throttle( p, factor, view=view)

if max(throttle) eq 255 then warning,'throttle_spectrum','maximum 8 bit throttle used'
if max(throttle) eq 1 then warning,'throttle_spectrum','no throttling needed'

save_file = strip_file_ext(strip_path((*p).file))+'-throttle.txt'
path = extract_path((*p).file)

F = file_requester( /write, filter = '*.txt', path=path, $
			title='Save Throttle Spectrum', file = save_file, group=group, fix_filter=1)
if F ne '' then begin
	close, 1
	openw, 1, F
	printf, 1, '#throttle ',factor
	for j=0L,siz-1 do begin
		if throttle[j] ne 1 then begin
			printf, 1, str_tidy(j), throttle[j], format='(A4,1x,I4)'
		endif
	endfor
	close, 1
endif

if (*p).n_fit gt 0 then begin
	for j=0L,(*p).n_fit-1 do begin
		free_spectrum, (*p).fit[j]
	endfor
endif

sfit = define(/spectrum)
sfit.source = (*p).source
sfit.cal = (*p).cal
sfit.label = 'Throttle factor spectrum'
sfit.comment = 'Throttle spectrum overlay'
sfit.size = siz
sfit.data = ptr_new( float(throttle), /no_copy)

(*p).fit[0] = ptr_new(sfit, /no_copy)	; overlay the throttle array

sfit = define(/spectrum)
sfit.source = (*p).source
sfit.cal = (*p).cal
sfit.label = 'Throttled down spectrum'
sfit.comment = 'Throttled spectrum'
sfit.size = siz
sfit.data = ptr_new( float( *(*p).data/throttle ), /no_copy)

(*p).fit[1] = ptr_new(sfit, /no_copy)	; overlay the throttled spectrum
(*p).n_fit = 2

print,'Total = ',total(*(*p).data),'  throttled = ',total(*(*p).data/throttle)

return
end
