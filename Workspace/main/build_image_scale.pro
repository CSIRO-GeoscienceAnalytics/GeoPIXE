pro build_image_scale, opt, low, high, image=img, output=out, label=label, $
					root=root, scale=scale, wt=wt, check_wt=check_wt

; Look at display parameters and set the 'low' and 'high' display range
; for the current display mapping (linear, log, sqrt, ...)
;
; Scale by the 'scale' factor, if present.
; If /check_wt then scale down by 10^4, and set the flag 'wt' true.
; /root indicates that images are 'variance' and need a sqrt() too.

if n_elements(scale) lt 1 then scale=1.0
if n_elements(check_wt) lt 1 then check_wt=0
if n_elements(root) lt 1 then root=0
wt = 0
label = ''

; Scale is applied to convert ppm.charge to ppm. Hence it is applied after sqrt()

big = opt.max
if root then big = sqrt(big)
big = big * scale
;print, 'opt.log scale type=',opt.log

case opt.log of
	0: begin											; linear
		if opt.top le opt.bottom then opt.top = opt.bottom + 0.3
		low = opt.bottom * big / 100.
		high = opt.top * big / 100.
		if check_wt and (high gt 999.9) then begin
			high = high/10000.
			low = low/10000.
			wt = 1
		endif
		if arg_present(out) and (n_elements(img) gt 0) then out = (root ? sqrt(img) : img)
		end
	1: begin											; log
		label = 'log '
		if opt.top le opt.bottom then opt.top = opt.bottom + 0.3
		m = 5./100.							; for zero slider at max/10^5
		c = alog10(big) - 5.
		low = m * opt.bottom + c
		high = m * opt.top + c
		if check_wt and (10.^high gt 999.9) then begin
			high = alog10( (10.^high)/10000.)
			low = alog10( (10.^low)/10000.)
			wt = 1
		endif
		if arg_present(out) and (n_elements(img) gt 0) then out = (root ? alog10(sqrt(img)) : alog10(img))
		end
	2: begin											; sqrt
		label = 'sqrt '
		if opt.top le opt.bottom then opt.top = opt.bottom + 0.3
		m = sqrt(big) / 100.
		c = 0.
		low = m * opt.bottom + c
		high = m * opt.top + c
		if check_wt and (high*high gt 999.9) then begin
			high = sqrt(high*high/10000.)
			low = sqrt(low*low/10000.)
			wt = 1
		endif
		if arg_present(out) and (n_elements(img) gt 0) then out = (root ? sqrt(sqrt(img)) : sqrt(img))
		end
endcase

return
end
