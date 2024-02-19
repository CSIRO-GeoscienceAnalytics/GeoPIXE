function decode_ebel, file

;	H. Ebel, et al., X-Ray Spectrom. 32 (2003) 442-451

COMPILE_OPT STRICTARR

ebel_edge_names = ['?','K','L1','L2','L3','M1','M2','M3','M4','M5','N1','N2','N3', $
					'N4','N5','N6','N7','O1','O2','O3','O4','O5','P1','P2','P3']

	if n_elements(file) lt 1 then return, 0

	ebel = { coherent:fltarr(6), incoherent:fltarr(6), $
		tau:fltarr(6,25), edge:fltarr(25), jump:fltarr(25), $
		mass:0.0, density:0.0, abs:fltarr(6,4) }

	if lenchr(file) lt 1 then return, ebel

	on_ioerror, bad_file
	openr, lun, file, /get_lun
	on_ioerror, bad_io

	s = ''
	set_separators, '	 '

	readf,lun, s								; element title
	readf,lun, s								; Z, A, rho

	chop_string, s, sub, n_sub
	if n_sub ge 3 then begin
		ebel.mass = float(sub[1])
		ebel.density = float(sub[2])
	endif

	readf,lun, s								; skip
	readf,lun, s								; coherent/incoh title
	readf,lun, s								; coherent lsf

	chop_string, s, sub, n_sub
	if n_sub ge 6 then begin
		ebel.coherent = float(sub[0:5])
	endif

	readf,lun, s								; incoherent lsf

	chop_string, s, sub, n_sub
	if n_sub ge 6 then begin
		ebel.incoherent = float(sub[0:5])
	endif

	readf,lun, s								; skip
	readf,lun, s								; edge jumps title

	repeat begin
next:
		readf,lun, s							; edge name
		if lenchr(s) lt 1 then goto, next

		chop_string, s, sub, n_sub
		if sub[0] eq 'global' then goto, global

		q = where( sub[0] eq ebel_edge_names, count)
		if count ne 0 then begin
			readf,lun, s						; edge E, jump
			chop_string, s, sub, n_sub
			if n_sub ge 2 then begin
				ebel.edge[q[0]] = float(sub[0])
				ebel.jump[q[0]] = float(sub[1])
			endif
			readf,lun, s						; tau lsf coeffiients
			chop_string, s, sub, n_sub
			if n_sub ge 6 then begin
				ebel.tau[*,q[0]] = float(sub[0:5])
			endif
		endif
	endrep until EOF(lun)

global:
	readf,lun, s								; skip
	readf,lun, s								; abssorption lsf title

	readf,lun, s								; abs K lsf coeffiients
	chop_string, s, sub, n_sub
	if n_sub ge 6 then begin
		ebel.abs[*,0] = float(sub[0:5])
	endif
	readf,lun, s								; abs L lsf coeffiients
	chop_string, s, sub, n_sub
	if n_sub ge 6 then begin
		ebel.abs[*,1] = float(sub[0:5])
	endif
	readf,lun, s								; abs M lsf coeffiients
	chop_string, s, sub, n_sub
	if n_sub ge 6 then begin
		ebel.abs[*,2] = float(sub[0:5])
	endif
	readf,lun, s								; abs N lsf coeffiients
	chop_string, s, sub, n_sub
	if n_sub ge 6 then begin
		ebel.abs[*,3] = float(sub[0:5])
	endif

	goto, finish

bad_file:
	warning,'decode_ebel','bad file open for '+file
	ebel = 0
	goto, finish
bad_io:
	warning,'decode_ebel','bad file io for '+file
	ebel = 0
	goto, finish

finish:
	close_file, lun
	return, ebel
end
