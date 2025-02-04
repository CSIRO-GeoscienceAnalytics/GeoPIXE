function python_valid, message=message

; Is the python revision compatible with the IDL revision, to use the IDL-Python bridge?
;
; There is some subtlety with sub-revisions (e.g. 8.5.2) not covered here.
; https://www.nv5geospatialsoftware.com/Support/Maintenance-Detail/idl-python-bridge-which-python-versions-are-compatible-with-each-release-of-idl
; 
; Also, historically, we assume that IDL 8.5 and 8.5.1 are only used with python 2.7.
;
;							Python version
; IDL version    2.7  3.4  3.5  3.6  3.7  3.8  3.9  3.10  3.11  3.12
;	8.5           y    y
;	8.6           y    y    y
;	8.7           y    y    y    y
;	8.8                          y    y    y
;	8.9                                    y    y    y
;	9.0                                    y    y    y     y
;	9.1                                         y    y     y     y

	message = 'python {3.6-3.8, 3.8-3.10, 3.8-3.11, 3.9-3.12} with IDL {8.8, 8.9, 9.0, 9.1}'

	valid = 0
	pver = python_version( revision=prev)
	iver = idl_version( revision=irev)
	print, 'Python rev=',prev,' IDL rev=',irev
	
	s = strsplit( prev, '.', /extract, count=ns)
	if ns lt 2 then return, 0
	pmain = fix( s[0])
	pfrac = fix( s[1])
	
	case irev of
		'8.5': begin
			if (prev eq '2.7') then goto, good									; IDL 8.5.1 works with python 2.7
			end
		'8.8': begin
			if (pmain eq 3) and (pfrac ge 6) and (pfrac le 8) then goto, good	; IDL 8.8 works with python 3.6-3.8
			end
		'8.9': begin
			if (pmain eq 3) and (pfrac ge 8) and (pfrac le 10) then goto, good	; IDL 8.9 works with python 3.8-3.10
			end
		'9.0': begin
			if (pmain eq 3) and (pfrac ge 8) and (pfrac le 11) then goto, good	; IDL 9.0 works with python 3.8-3.11
			end
		'9.1': begin
			if (pmain eq 3) and (pfrac ge 9) and (pfrac le 12) then goto, good	; IDL 9.1 works with python 3.9-3.12
			end
		else:
	endcase
	valid = 0
	return, valid
	
good:
	valid = 1
	return, valid
end
