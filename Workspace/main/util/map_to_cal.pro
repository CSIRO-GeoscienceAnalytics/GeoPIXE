pro map_to_cal, p, cal2, error=err

; Map *p spectrum onto cal2
; This is a cheap and nasty hack using congrid and scale.
; Better to use the new function 'map_spec', which follows
; the old Fortran version, and uses vectors well.

err = 1
d1 = *(*p).data
siz = n_elements(d1)
cal1 = (*p).cal

low = -1
high = siz-1

repeat begin
	low = low+1
	if low ge high then return

	e1 = cal1.poly[0] + cal1.poly[1]*low
	e2 = cal1.poly[0] + cal1.poly[1]*high

	c1 = round( (e1 - cal2.poly[0]) / cal2.poly[1])
	c2 = round( (e2 - cal2.poly[0]) / cal2.poly[1])
endrep until( (c1 ge 0) or (low ge high))

sum1 = total(d1[low:high])

d2 = fltarr(c2+1)
d2[c1:c2] = congrid(d1[low:high], c2-c1+1)

sum2 = total(d2)
d2 = d2 * sum1/sum2

ptr_free, (*p).data
(*p).size = n_elements(d2)
(*p).data = ptr_new( d2, /no_copy)
(*p).cal.poly[0] = cal2.poly[0]
(*p).cal.poly[1] = cal2.poly[1]
err = 0
return
end

