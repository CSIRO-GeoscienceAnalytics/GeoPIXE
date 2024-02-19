pro load_spec_colours, red_blind=red_blind

; Changed l.grey from 190 to 229 (to match XP background)	10-4-07 CGR
; and back again (buttons differ from background)			11-4-07
;  Add red_blind option for "Red" colour blind palette		19-6-14 CGR

common Colors, r,g,b, rr,gg,bb
common c_spec_colours, name
common c_spec_colours2, red_colour_blind
if n_elements(red_colour_blind) lt 1 then red_colour_blind=0
if n_elements(red_blind) ge 1 then red_colour_blind=red_blind

name1 = ['black','green', 'red',  'blue',  'cadmium','l.blue', $
	   'l.grey','yellow','pink','orange', 'violet', $
	 'l.violet','white','d.blue','d.grey','d.green']
r1 =   [    0,      1,	  253,  	0,      239,        0, $
		  190,    255,    255,    255,      200,  $
		  206,    255 ,     0,    100,        0]
g1 =   [    0,    255,      0,      0,      187,      250, $
		  190,    255,      0,    135,        70, $
		  179,    255 ,     100,    100,      135]
b1 =   [    0,      1,      0,    255,        6,      250, $
		  190,     0,    235,      0,      255, $
		  228,    255,    255,    100,        0]
n_name = n_elements(name1)

if red_colour_blind then begin
	r1[2] = 0
	g1[2] = 100
	b1[2] = 255
endif

order = [0,1,10,2,4,5, $
		6,7,8,3,9, $
		11,12,13,14,15]

name = name1[order]
if n_elements(r) lt 1 then loadct,5,bottom=16,ncolors=100
r2 = r1[order]
g2 = g1[order]
b2 = b1[order]
r[0:n_name-1] = r2
g[0:n_name-1] = g2
b[0:n_name-1] = b2
rr = r
gg = g
bb = b

tvlct, r2,g2,b2, 0

!p.color = 12							; was 15  16/9/04 ?
!p.background = 0
return
end

