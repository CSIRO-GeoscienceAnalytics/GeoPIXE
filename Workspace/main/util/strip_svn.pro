function strip_svn, s

; Strip off SVN packing and extract first SVN version number

if n_elements(s) lt 1 then return,0

t = strsplit(s, '" 	,;.', /extract)

return, long(t[0])
end
