function hayden, spec, low, high, fwhm, loops

; Perform Hayden smooth (H.C. Hayden, Comp. in Phys., Nov (1987) 74)

ns = n_elements(spec)
s = double(spec)
t = s
t[low:high] = 0.0
t0 = dblarr(ns)

for i=0L,loops-1 do begin

	t0[low:high] = s[low:high] - t[low:high]

;	t3 = gaussian_smooth( t0, low, high, fwhm)
	t3 = smooth( t0, fwhm)

	t[low:high] = t[low:high] + t3[low:high]
endfor

return, float(t)
end
