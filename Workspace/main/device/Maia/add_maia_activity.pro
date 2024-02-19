	function add_maia_activity, p
	
	; Add activity data for all TAG=39 records in p array
	
	if ptr_good(p) eq 0 then return,0.0
	if ptr_good( (*p)[0], /struct ) eq 0 then return,0.0
	
	n = n_elements(*p)
	m = lonarr(n)
	for i=0L,n-1 do m[i] = (*(*p)[i]).tag eq 39
	q = where( m eq 1, nq)
	if nq eq 0 then return, 0L

	pd = (*p)[q[0]]
	nd = (*pd).sub_header.count - 16
	t = lonarr(nd)
	
	for i=0L,nq-1 do begin
		pd = (*p)[q[i]]
		b = (*(*pd).b)[0:4*nd-1]
		rates = ulong(b,0,nd)
		swap_bytes, rates, /big_endian_data
		t = t+rates
	endfor
	
	return, float(t) / float(nq)
end
