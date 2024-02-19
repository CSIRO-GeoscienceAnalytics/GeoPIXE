pro secondary_fluorescence, qe, iel, qz, dyield, yield, syield, $
			z2, n_lines, e_lines, fluoro, branch_ratio, jump, $
			slices, js, k, cmux, exp_cmux, relmux, lid

;	Calculate secondary fluorescence for a list of source lines
;	on a list of destination elements. Do all lines for dest element.
;
;	'iel' is the index for the source element.
;	'qe' are the indices for the source X-ray energies (a subset of lines).
;	'qz' are the indices for the destination elements to fluoresce.
;	N.B. These lists go together as a pair.
;
;	z2, shell, e_lines, n_lines, shell, fluoro, branch_ratio, jump, are
;	to use for destination lines/elements.
;
; Note:
;	'k' is the current half-slice index.
;	'js' is the slice index.
;	'cmux', 'exp_cmux' index by half-shell.
;	'relmux' is indexed by layer.

n_e = n_elements(qe)
n_z = n_elements(qz)
na = n_elements(cmux[0,0,*])
n_layers = max(lid)+1
kk = indgen(na)
l = lid[kk/2]
order = sort(qe)
ilast = -1

for iq=0,n_e-1 do begin
	i = qe[order[iq]]											; source Xray
	j = qz[order[iq]]											; destination element
	n = n_lines[j]

	if i ne ilast then begin
		alfa = abs( double(cmux[i,iel,*] - cmux[i,iel,k]) )
		dmux = 0.5 * [abs( alfa[1:na-1] - alfa[0:na-2]), abs( alfa[na-1] - alfa[na-2])]
		alfa_plus_beta = alfa + dmux
		alfa = alfa - dmux

;		Evaluate contribution using elliptical integral ...

		delta = reform( exp(-alfa) - exp(-alfa_plus_beta) -  $
					alfa*Elliptical_1(abs(alfa),/noneg) + $
					alfa_plus_beta*Elliptical_1(abs(alfa_plus_beta),/noneg) )
		ilast = i
	endif

	src = 0.5 * dyield[i] * fluoro[j] * delta * relmux[i,iel,j,l] * jump[j]

	for m=0L,n-1 do begin
		sec = src * branch_ratio[m,j] * exp_cmux[m,j,*]

		for li=0,n_layers-1 do begin
			q = where( l eq li)
			if q[0] ne -1 then begin
				dfl = total( sec[q])
				syield[m,j,li] = syield[m,j,li] + dfl
				yield[m,j,li] = yield[m,j,li] + dfl
			endif
		endfor
	endfor
endfor

q = where( finite(syield) eq 0, nq)
if nq gt 0 then syield[q] = 0.0
return
end




