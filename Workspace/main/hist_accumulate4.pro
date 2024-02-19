function hist_accumulate4, e,tot,x,y,ste,pu,veto,n, spec,specx,specy,ns,nc,found, nnpu,nn, $
							multiple=multiple, spect=spect, nt=nt, $
							et2D=et2d, n_energy=n_energy, n_time=n_time, $
							ecompress=ecompress, tcompress=tcompress
;
;  This assumes spec,specx,specy are:	lonarr(ns,nc)
;				found					lonarr(nc)
;				x,y,e,ste,pu,veto:		uintarr(n)
;				tot						uintarr(n)
;				multiple				lonarr(n) or -1L
;				nnpu, nn				lonarr(nc)
;				found					lonarr(nc)
;	optional:
;				spect					lonarr(nt,nc), nt defaults to ns
;				et2d					lonarr(n_energy,n_time)
;				ecompress				compress for E
;				tcompress				compress for T
;
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
		warning,'hist_accumulate4',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L
if n_elements(spect) lt 1 then spect=-1L
if n_elements(n_energy) lt 1 then n_energy=0L
if n_elements(n_time) lt 1 then n_time=0L
if n_elements(nt) lt 1 then nt=ns
if n_elements(do_tot) lt 1 then do_tot=0
if n_elements(et2d) lt 1 then et2d=-1L
if n_elements(ecompress) lt 1 then ecompress=1L
if n_elements(tcompress) lt 1 then tcompress=1L
do_tot = 0L
if (n_elements(spect) eq long(nt)*nc) then do_tot=1L
do_et2d = 0L
if (n_elements(et2d) eq long(n_energy)*n_time) then do_et2d=1L
err = 0L

if size(spec,/tname) ne 'LONG' then begin sb='spec' & goto, bad & endif
if size(specx,/tname) ne 'LONG' then begin sb='specx' & goto, bad & endif
if size(specy,/tname) ne 'LONG' then begin sb='specy' & goto, bad & endif
if size(found,/tname) ne 'LONG' then begin sb='found' & goto, bad & endif
if size(nnpu,/tname) ne 'LONG' then begin sb='nnpu' & goto, bad & endif
if size(nn,/tname) ne 'LONG' then begin sb='nn' & goto, bad & endif
if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(pu,/tname) ne 'UINT' then begin sb='pu' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(multiple,/tname) ne 'LONG' then begin sb='multiple' & goto, bad & endif
if size(tot,/tname) ne 'UINT' then begin sb='tot' & goto, bad & endif
if do_tot then begin
	if size(spect,/tname) ne 'LONG' then begin sb='spect' & goto, bad & endif
endif
if do_et2d then begin
	if size(tot,/tname) ne 'UINT' then begin sb='tot' & goto, bad & endif
	if size(et2d,/tname) ne 'LONG' then begin sb='et2d' & goto, bad & endif
endif

value = bytarr(26)
value[*] = 0					; pass all by reference

err = call_external( geopixe_library(), geolib_name( 'hist_accumulate4'), cdecl=geolib_cdecl(), $
				e,tot,x,y,ste,pu,veto,long(n), spec,specx,specy,long(ns),long(nc),found, nnpu,nn, multiple, $
				do_tot, spect,long(nt), do_et2d, et2d,long(n_energy),long(n_time), long(ecompress),long(tcompress), value=value )

return, err

bad:
	print,' hist_accumulate4: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
