function spec_colour, s, RGB=RGB

common c_spec_colours, name
common Colors, r,g,b, rr,gg,bb

if n_elements(name) lt 16 then load_spec_colours
if n_elements(s) lt 1 then return, 2
if n_elements(RGB) lt 1 then RGB=0

white = 12
black = 0

if size(s,/tname) eq 'STRING' then begin
	for i=0L,15 do begin
		if strlowcase(s) eq name[i] then goto, found
	endfor
	i = 1
	goto, found
endif

i = (fix(s) MOD 13) + 1

found:
	return, (RGB ? [r[i],g[i],b[i]] : i)
end

