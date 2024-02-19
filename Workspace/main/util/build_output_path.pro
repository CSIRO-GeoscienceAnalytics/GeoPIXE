function build_output_path, in, out, root, set=set

;	Build the 'out' path, based on a root (if set) and the path 'in'
;	
;	If /set, then set the root, based on the differences between
;	'in' and 'out'. 
;	
;	(*root).path is the base output path, and
;	(*root).strip is the number of characters to strip from in
;	before adding the root to make an output path.
;
;	'root' is a pointer to a structure. If new, it must be created
;	with /Allocate_heap set.

	if n_elements(set) lt 1 then set=0
	if n_elements(root) lt 1 then goto, bad
	if ptr_valid(root) eq 0 then goto, bad
	if n_elements(*root) lt 1 then *root = {strip:0, path:''}
	ic = (!version.os_family eq 'Windows') ? 1 : 0
	
	pin = extract_path(in)
	pout = extract_path(out)

	if set then begin
		if pout eq '' then pout=pin

		posin = strsplit(pin, '/\:[]', count=nin, length=lin)
		posout = strsplit(pout, '/\:[]', count=nout, length=lout)
		if nin eq 0 then return, pout
		if nout eq 0 then return, ''
		jin = nin-1
		jout = nout-1
		posin = [posin, posin[nin-1]+lin[nin-1]+1]
		posout = [posout, posout[nout-1]+lout[nout-1]+1]
		
		diff = 0
		repeat begin
;			if bin[jin] ne bout[jout] then diff=1

			sin = strmid(pin, posin[jin], lin[jin])
			sout = strmid(pout, posout[jout], lout[jout])			
			if strcmp(sin,sout,fold_case=ic) eq 0 then diff=1
			
			jin = jin-1
			jout = jout-1
		endrep until (diff eq 1) or (jin lt 0) or (jout lt 0)

		if jout ge 0 then begin
;			(*root).path = string( bout[0:(jout+2) < (n_elements(bout)-1)] )
;			(*root).strip = jin+3

			(*root).path = strmid( pout, 0, (posout[jout+2] < strlen(pout)) )
			(*root).strip = posin[jin+2]
		endif else if jin ge 0 then begin
;			(*root).strip = jin+3

			(*root).strip = posin[jin+2]
			(*root).path = ''
		endif else begin
			*root = {strip:0, path:''}
		endelse

		return, pout
	endif

	if lenchr(pout) lt 1 then begin
		*root = {strip:0, path:''}
		path = pin

	endif else if ((*root).strip le 0) and ((*root).path eq '') then begin
		path = pin

	endif else begin
		path = ''
		if (*root).path ne '' then begin
			path = (*root).path
		endif
		n = lenchr(pin)
		if ((*root).strip ge 1) and (n gt (*root).strip) then begin
			path = path + extract(in,(*root).strip,n-1)
		endif
	endelse

	return, path

bad:
	return, extract_path( in)
end
