	function iXRF_spectrum, p, select=ii
	
	; Spectrum for all TAG='101'x (ADCx) records in p array
	
	if n_elements(ii) lt 1 then ii=-1
	
@iXRF_listmode.def

	if ptr_good(p) eq 0 then return,0.0
	if ptr_good( (*p)[0], /struct ) eq 0 then return,0.0
	
	n = n_elements(*p)
	m = lonarr(n)
	for i=0L,n-1 do m[i] = ( data_type[(*(*p)[i]).tag] eq '102'x)			; ADCx
	q = where( m eq 1, nq)
	if nq eq 0 then return, 0L

	k16 = 16 * 1024L
	t = lonarr(k16)	
	first = 1
	if ii ge 0 then begin
		i1 = ii
		i2 = ii
		if data_type[(*(*p)[ii]).tag] ne '102'x then return, 0
		q = indgen(n)
	endif else begin
		i1 = 0
		i2 = nq-1
	endelse
	for i=i1,i2 do begin
		pd = (*p)[q[i]]
		b = (*(*pd).b)
		nb = n_elements(b)
		nd = ((*pd).length/4) < (nb/4)
		if nd ge 1 then begin
			data = uint(b,0,nd*2)
			swap_bytes, data, big_endian_data=0
			data = reform( data, 2,nd)
			energy = data[0,*]
			ste = data[1,*]										; ignore ADC # for now ...
			if first then begin
				h = histogram( energy, binsize=1, min=0, nbins=k16)
			endif else begin
				h = histogram( energy, binsize=1, min=0, nbins=k16, input=h)
			endelse
			first = 0
		endif
	endfor
	q1 = where( h gt 0, nq1)
	mx = max(q1) > 256

	return, h[0:mx]
end
