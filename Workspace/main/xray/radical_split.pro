
function el_start, str, i

n = strlen(str)
if (i lt 0) or (i ge n) then return, 0

se = 0
c = extract(str,i,i)
if is_upper(c) and (sole_e(str,i) eq 0) and (slashed(str,i) eq 0) then se=1

return, se
end

;------------------------------------------------------------------

	function radical_split, linei

; Split between radical (e.g. (Al2O3)12.3 ).
; If there are no "(",")", then there is only one radical.

	line = strcompress(linei,/remove_all)
	n = lenchr(line)
	if n lt 1 then return, ''
	if locate( '(', line) eq -1 then return, line
	br_closed = 1
	string = ''

	j = 0
	k = -1
	for i=0L,n-1 do begin
		s = extract( line,i,i)
		if br_closed and ((s eq '(') or el_start(line,i)) then begin
			if k ge j then begin
				string = [string,extract(line,j,k)]
				j = i
			endif
		endif

		if s eq ')' then br_closed = 1
		if s eq '(' then br_closed = 0
		k = i
	endfor
	if n-1 ge j then begin
		string = [string,extract(line,j,n-1)]
	endif

done:
	if n_elements(string) gt 1 then string = string[1:*]
	return, string
	end
