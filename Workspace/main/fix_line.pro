pro fix_line, line, is_label, label, is_comment, comment, error
;
;	Look for a label, comment
;
;	is_label=1		if label present
;	label			label string
;	is_comment	=1,2	if WHOLE line is a comment = 2
;	comment		any trailing comment string
;	error=1		if a continuation line
;	line			just returns the command part, less label, comment
;
is_label = 0
is_comment = 0
error = 0
label = ''
comment = ''
tab = '	'
blank = ' '
line = strlowcase(line)

; check for comment, continuation ...

n = 0
for i=0L,strlen(line)-1 do begin
	if i ge 6 then goto, nocont
	s = strmid( line, i, 1)
	if (i eq 0) then begin
		if ((s eq 'c') or (s eq '!')) then begin
			is_comment = 2
			comment = strmid( line, 1, strlen(line)-1)
			return
		endif
	endif
	if s eq tab then goto, nocont
	if (i eq 5) and (s ne blank) then goto, error
	n = i+1
endfor

nocont:
if n gt 0 then begin
	label = strtrim( strmid( line, 0, n), 2)
	if lenchr(label) gt 0 then is_label = 1
endif
k = strpos( line, '!', 1)
if k gt 0 then begin
	comment = strmid( line, k+1, strlen(line)-k-1)
	is_comment = 1
endif
if k lt 0 then k = strlen(line)
line = strtrim( strmid( line, n, k-n), 2)

; check for logical .eq., exponen **, go to, string stuff ...

fort = [	'.eq.',	'.ne.',	'.lt.',	'.le.',	'.gt.',	'.ge.', $
		'**',		'//',		'go to ',	'.and.',	'.or.',	'.xor.', $
		'.not.']

idl = [	' eq ',	' ne ',	' lt ',	' le ',	' gt ',	' ge ', $
		' ^',		' +',		'goto,L',	' and ',	' or ',	' xor ', $
		' not ']

for i=0L,n_elements(fort)-1 do begin
	j = -1
	repeat begin
		j = strpos( line, fort[i], j+1)
		if j ge 0 then begin
			strput, line, idl[i], j
		endif
	endrep until (j eq -1)
endfor

return

error:
error = 1
return
end
