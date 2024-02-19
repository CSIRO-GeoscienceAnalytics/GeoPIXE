function shorten_label, str, length=length

if n_elements(length) lt 1 then length=25

	label = str
	while strlen(label) gt length do begin
		n = strlen(label)

		k1 = locate('\',label)
		k2 = locate('/',label)
		k3 = locate(':',label)
		k4 = locate('.',label)
		k5 = locate(' ',label)
		k6 = locate(',',label)
		k7 = locate('[',label)
		k8 = locate(']',label)
		k9 = locate('(',label)
		k10 = locate(')',label)

		k = n-2
		if k1 ge 0 then if k1 lt k then k=k1
		if k2 ge 0 then if k2 lt k then k=k2
		if k3 ge 0 then if k3 lt k then k=k3
		if k4 ge 0 then if k4 lt k then k=k4
		if k5 ge 0 then if k5 lt k then k=k5
		if k6 ge 0 then if k6 lt k then k=k6
		if k7 ge 0 then if k7 lt k then k=k7
		if k8 ge 0 then if k8 lt k then k=k8
		if k9 ge 0 then if k9 lt k then k=k9
		if k10 ge 0 then if k10 lt k then k=k10

		if k eq n-2 then k=(n-length)>0

		label = extract( label, clip(k+1,0,n-1), n-1)
	endwhile

	return, label
end
