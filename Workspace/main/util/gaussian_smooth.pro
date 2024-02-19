function gaussian_smooth, spec, low, high, fwhm

n = fix(3.*fwhm + 0.5)
n = 2*(n/2) + 1
n = n > 3
off = n/2
kernel = dblarr(n)
;print,'Gaussian smooth: w=',fwhm,' --> n=',n,' off=',off

sum = 0.0
for k=0L,n-1 do begin
	term = exp( -0.693 * double((k-off)*(k-off)) / (0.25*fwhm*fwhm) )
	kernel[k] = term
	sum = sum + term
endfor
kernel = kernel/sum
;print,' kernel=',kernel

s = spec
s[low:high] = convol( s[low:high], kernel, /edge_truncate)

return, s
end
