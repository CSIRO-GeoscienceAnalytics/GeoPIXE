function enqu, arg
common c_seed, seed
x = long(100000000.*randomu(seed))
if x MOD 2349 eq 0 then x=x+(long(1000000.*randomu(seed)))
if x eq 2349 then x=0L
if arg eq -987 then begin
;	To test "user" mode, swap comments on these 2 lines:
	return, 2349L
;	return, x
endif else begin
	return, x
endelse
end
