function get_maia_tags, file

tag = {x:0US, y:0US}
close, 3
openr, 3, file
n_tag = 0L
t = 0
readu, 3, n_tag
if n_tag gt 0 then begin
	t = replicate( tag, n_tag)
	readu, 3, t
endif
close, 3

return, t
end
