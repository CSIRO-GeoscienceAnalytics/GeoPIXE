function rates_string, z, bytes=bytes, microamp=microamp, digits=digits

if n_elements(bytes) lt 1 then bytes=0
if n_elements(microamp) lt 1 then microamp=0
if n_elements(digits) lt 1 then digits=5

n = n_elements(z)
if n lt 1 then return, '0'
x = float(z)
s = strarr(n)

postfix = ''
if microamp then begin
	suffix = ['u','n','p','f'] 
	postfix = 'A'
endif else begin
	suffix = ['','k','M','G']
	if bytes then postfix = 'B/s'
endelse

for j=0L,n-1 do begin
	i = 0
	if microamp then begin
		while (x[j] lt 1.) and (i lt 3) do begin
			x[j] = x[j]*1000.
			i = i+1
		endwhile
	endif else begin
		while (x[j] ge 1000.) and (i lt 3) do begin
			x[j] = x[j]/1000.
			i = i+1
		endwhile
	endelse
;	s[j] = str_tidy(x[j])
	s[j] = strtrim(x[j],2)
	s[j] = strmid(s[j],0,digits) + ' ' + suffix[i]
	s[j] = s[j] + postfix
endfor

if n eq 1 then s=s[0]
return, s
end
