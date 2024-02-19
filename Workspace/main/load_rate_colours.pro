pro load_rate_colours, colours=colours, threshold=threshold, vector=vector

; Load the colour table from 116 to 186 (71 entries) with
; colours for maia_rates rate map.

common Colors, r,g,b, rr,gg,bb
if n_elements(threshold) lt 1 then threshold=[0,10,100,1000,10000,20000,50000,100000]

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
lviolet = spec_colour('l.violet')
pink = spec_colour('pink')
black = spec_colour('black')
white = spec_colour('white')
;								   select: 0     1     2     3    4      5     6    7
if n_elements(colours) lt 1 then colours=[black,blue,lblue,green,yellow,orange,red,lviolet]

rs = bytarr(71)
gs = bytarr(71)
bs = bytarr(71)
v = fltarr(71)

for i=0L,6 do begin
	k = 10*i
	rs[k:k+9] = congrid( float([r[colours[i]], r[colours[i+1]]]), 10, /interp, /minus_one)
	gs[k:k+9] = congrid( float([g[colours[i]], g[colours[i+1]]]), 10, /interp, /minus_one)
	bs[k:k+9] = congrid( float([b[colours[i]], b[colours[i+1]]]), 10, /interp, /minus_one)
	v[k:k+9] = congrid( float([threshold[i],threshold[i+1]]), 10, /interp, /minus_one)
endfor
rs[70] = r[colours[7]]
gs[70] = g[colours[7]]
bs[70] = b[colours[7]]
v[70] = threshold[7]

r[116:186] = rs
g[116:186] = gs
b[116:186] = bs
rr = r
gg = g
bb = b
tvlct, rs,gs,bs, 116

vector = long(v)
return
end

