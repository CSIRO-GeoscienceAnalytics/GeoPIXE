function hist_accumulate3, e,x,y,ste,pu,n, spec,specx,specy,ns,nc,found, nnpu,nn, $
							multiple=multiple, tot=tot, spect=spect, nt=nt, $
							et2D=et2d, n_energy=n_energy, n_time=n_time, $
							ecompress=ecompress, tcompress=tcompress
;
;  This assumes spec,specx,specy are:	lonarr(ns,nc)
;				found					lonarr(nc)
;				x,y,e,ste,pu:			uintarr(n)
;				multiple				lonarr(n) or -1L
;				nnpu, nn				lonarr(nc)
;				found					lonarr(nc)
;	optional:
;				tot						lonarr(nc)
;				spect					lonarr(nt,nc)
;				et2d					lonarr(n_energy,n_time)
;				ecompress				compress for E
;				tcompress				compress for T
;				nt						size of T spectra, defaults to ns
;
;	If tot, spect are present (and a suitable array), then
;	accumulate a ToT spectrum as well.
;	If et2D is present (and a suitable array), then
;	accumulate a et2D 'image' as well.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Hist_accumulate3',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L
if n_elements(tot) lt 1 then tot=-1L
if n_elements(spect) lt 1 then spect=-1L
if n_elements(nt) lt 1 then nt=ns
do_tot = 0L
if (n_elements(tot) eq n_elements(e)) and (n_elements(spect) eq long(nt)*nc) then do_tot=1L
if n_elements(n_energy) lt 1 then n_energy=0L
if n_elements(n_time) lt 1 then n_time=0L
if n_elements(et2d) lt 1 then et2d=-1L
if n_elements(ecompress) lt 1 then ecompress=1L
if n_elements(tcompress) lt 1 then tcompress=1L
do_et2d = 0L
if (n_elements(tot) eq n_elements(e)) and (n_elements(et2d) eq long(n_energy)*n_time) then do_et2d=1L
err = 0L

;if do_et2d eq 0 then err=1L

if size(spec,/tname) ne 'LONG' then goto, bad
if size(specx,/tname) ne 'LONG' then goto, bad
if size(specy,/tname) ne 'LONG' then goto, bad
if size(found,/tname) ne 'LONG' then goto, bad
if size(nnpu,/tname) ne 'LONG' then goto, bad
if size(nn,/tname) ne 'LONG' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(pu,/tname) ne 'UINT' then goto, bad
if size(multiple,/tname) ne 'LONG' then goto, bad
if do_tot then begin
	if size(tot,/tname) ne 'UINT' then goto, bad
	if size(spect,/tname) ne 'LONG' then goto, bad
endif
if do_et2d then begin
	if size(tot,/tname) ne 'UINT' then goto, bad
	if size(et2d,/tname) ne 'LONG' then goto, bad
endif

value = bytarr(25)
value[*] = 0					; pass all by reference

err = call_external( geopixe_library(), geolib_name( 'hist_accumulate3'), cdecl=geolib_cdecl(), $
				e,x,y,ste,pu,long(n), spec,specx,specy,long(ns),long(nc),found, nnpu,nn, multiple, $
				do_tot, tot, spect, long(nt), do_et2d, et2d,long(n_energy),long(n_time), long(ecompress),long(tcompress), value=value )

return, err

bad:
	print,' hist_accumulate3: Error - bad type for argument.'
	err = 1L
	return, err
end
