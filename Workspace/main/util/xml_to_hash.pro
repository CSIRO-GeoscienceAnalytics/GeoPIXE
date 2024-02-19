;
;	Read an XML file and convert its contents to a hash
;	
;	XML_to_Hash, str, file=file, ToStruct=DoStruct, error=error
;
;		str			XML as a string
;		file		XML file path
;		/ToStruct	convert Hash to a Struct
;		error		flags an error
;
;-------------------------------------------------------------------------------

;	This does not work properly as a method, as the reentrance fails.
;	Also fails to stop at a break point here on reenetering.

function hash::ToStruct_
	compile_opt idl2, logical_predicate	;, static

	h = self
	s = 0
	if typename(h) ne 'ORDEREDHASH' then return,s
	key = h.keys()
	if n_elements(key) eq 0 then return,s
	for i=0,n_elements(key)-1 do begin
		if typename(h[key[i]]) eq 'ORDEREDHASH' then begin
			d = h[key[i]].ToStruct()
		endif else begin
			d = h[key[i]]
		endelse
		if i eq 0 then begin
			s = create_struct( key[i], d)
		endif else begin
			s = create_struct( key[i], d, s)
		endelse
	endfor	
	return, s
end

;-------------------------------------------------------------------------------

pro hash::FromXML_, top
	compile_opt idl2

	if isa(top, 'IDLffXmlDomElement') eq 0 then return
	nodes = top.GetChildNodes()
	for i=0, nodes.GetLength()-1 do begin
		n = nodes.Item(i)
		if isa(n, 'IDLffXmlDomElement') then begin
			key = n.GetTagName()
			attr = n.GetAttributes()
			if attr.GetLength() ge 1 then begin
				for j=0,attr.GetLength()-1 do begin
					it = attr.Item(j)
					if strlowcase(it.GetName()) eq 'name' then begin
						sval = replace(' ','_',it.GetValue())
						sval = replace('.','_',sval)
						key = key + '_' + sval
					endif
				endfor
			endif
			obj = OrderedHash()
			obj.FromXML_, n
			if obj.HasKey('ValueOnly') then begin
				self[key] = obj['ValueOnly']
			endif else begin
				self[key] = obj
			endelse
		endif else if isa(n, 'IDLffXmlDomText') then begin
			str = strtrim(strip_non_print( n.GetData(), /no_tab),2)
			if str ne '' then begin
				key = 'ValueOnly'
				self[key] = n.GetData()
			endif
		endif
	endfor
end

;----------------------------------------------------------------------------------

function XML_to_Hash, str, file=file, ToStruct=DoStruct, error=error

COMPILE_OPT STRICTARR
error = 1
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'XML_to_Hash',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	if n_elements(DoStruct) eq 0 then DoStruct=0
	by_file = 0
	if n_elements(str) eq 0 then begin
		if n_elements(file) gt 0 then by_file = 1
	endif
	if (by_file eq 0) and (n_elements(str) eq 0) then return,0
	
	oXml = IDLffXmlDomDocument()
	if by_file then begin
		oXml.Load, filename=file, /EXCLUDE_IGNORABLE_WHITESPACE
	endif else begin
		oXml.Load, string=str, /EXCLUDE_IGNORABLE_WHITESPACE
	endelse
	top = oXml.GetDocumentElement()
	
	objref = OrderedHash()
	objref.FromXML_, top
	
	if DoStruct then begin
;		s = objref.ToStruct_()
		s = hash_to_struct( objref)
	endif else s=objref
	error = 0
	return, s
end
