function read_cuts, F, error=error

;	Read CUT definitions from 'F'

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
		warning,'Read_cuts',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

error = 1
if n_params() lt 1 then return, 0
if lenchr(F) lt 1 then return, 0
good_versions = [-1,-2]

	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	version = 0L
	readu,1, version
	q = where( version eq good_versions)
	if q[0] eq -1 then return, 0

	max_n = 1000
	n = 0L
	readu,1, n
	if (n le 0) or (n gt max_n) then goto, bad_io

	if version eq -1 then begin				; old format

		ocut = define(/old_cut)
		ocuts = replicate( ocut, n)

		readu,1, ocuts
		close,1

		cut = define(/cut)
		cuts = replicate( cut, n)

		cuts.el = ocuts.el
		cuts.units = ocuts.units

		cuts.type = 2
		for i=0L,n-1 do begin
			cuts[i].e = [0,0,ocuts[i].low,ocuts[i].high,ocuts[i].blow,ocuts[i].bhigh]
			cuts[i].x = round( 1000.*cuts[i].e)
			cuts[i].dleft = 0.0
			cuts[i].dright = -float(cuts[i].e[3]-cuts[i].e[2])/float(cuts[i].e[5]-cuts[i].e[4])
		endfor
		cuts.cal_a = 0.001
		cuts.cal_b = 0.0

	endif else begin

		cut = define(/cut)
		cuts = replicate( cut, n)

		readu,1, cuts
		close,1

		for i=0L,n-1 do begin
			if cuts[i].type eq 2 then begin
				if cuts[i].cal_a lt 1.0e-6 then begin
					cuts[i].cal_a = 0.001
					cuts[i].cal_b = 0.0
					cuts[i].x = round( 1000.*cuts[i].e)
					cuts[i].dleft = 0.0
					cuts[i].dright = -float(cuts[i].e[3]-cuts[i].e[2])/float(cuts[i].e[5]-cuts[i].e[4])
				endif
			endif
		endfor
	endelse

	cuts[*].file = F[0]
	p = ptr_new( cuts, /no_copy)
	error = 0

return, p

bad_io:
	print,'Read_Cuts: bad CUTs I/O'
	error = 1
	return, 0

end

