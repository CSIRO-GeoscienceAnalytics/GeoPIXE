function build_throttle, p, factor, view=view

; Build the throttle vector

siz = (*p).size							; Size of spectrum in channels
ca = (*p).cal.poly[1]					; energy calibration energy per channel
cb = (*p).cal.poly[0]					; energy calibration offset
throttle = replicate(1, siz)			; no throttle by default
if n_elements(view) lt 2 then view=[1,siz-2]
if view[1] le view[0] then view=[1,siz-2]
view = view < (siz-1)

w1 = 0.003
w0 = (0.18)*(0.18) - w1*5.895
w = 0.22 / ca							; FWHM (channels)

AF = w1 * ca
BF = w0 + w1 * cb
t1 = median(*(*p).data, mean(w) > 3)
new = *(*p).data
err = low_stats_filter( t1,new, siz, 1, siz-2, AF,BF)
tot = total(new[view[0]:view[1]])

dtot = tot / factor						; desired total in test spectrum
if factor lt 1. then return, throttle

top = max(new)
if top lt 1. then begin
	warning,'build_throttle','null spectrum?'
	return, throttle
endif

repeat begin
	top = 0.8 * top
	new2 = new < (top > new/255)		; apply clip, limit throttle to 255
	tot2 = total(new2[view[0]:view[1]])
endrep until tot2 le dtot

ratio = round( new / (new2 > 1.))		; construct the throttle array
throttle = (ratio > 1) < 255			; clip to 8 bits max

return, throttle
end
