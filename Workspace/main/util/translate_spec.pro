function translate_spec,  new

; Translate new into old spec format
; new is a pointer to a struct

file = (*new).source
label = (*new).label
size = (*new).size
order = (*new).cal.order
units = (*new).cal.units
a = (*new).cal.poly[1]
b = (*new).cal.poly[0]
q = (*new).charge
y = *((*new).data)
x = a * findgen(size) + b

spec = {file:file, label:label, story1:' ', story2:' ', $
		story3:' ', size:size, order:order, units:units, a:a, b:b, $
		q:q, x:x, y:y }

return, spec
end
