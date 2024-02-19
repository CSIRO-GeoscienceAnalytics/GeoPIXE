function tag_present, tag, x

; Is tag present in structure x?

if n_elements(x) lt 1 then return, 0
if n_elements(tag) lt 1 then return, 0
if size(x, /tname) ne 'STRUCT' then return, 0

tags = tag_names(x)
if n_elements(tags) lt 1 then return, 0

q = where( strupcase(tag) eq tags, nq)
if nq eq 0 then return, 0

return, 1
end

