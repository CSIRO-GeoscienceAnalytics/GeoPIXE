    pro get_ascii_spec, p, name, energy=energy, channel=channel

;   Read in a spectrum

    on_ioerror,err
    openr,unit,name,/get_lun
    if n_elements(energy) lt 1 then energy=0
    if n_elements(channel) lt 1 then channel=0

    str = strarr(16384)					; strarr(8192)
    close, unit
    on_ioerror, process
    openr,unit,name
    readf,unit,str

process:
    on_ioerror, null
    q = where( str ne '')
    siz = max(q)+1
;   ex = fix(alog( siz>1)/alog(2) + 1.00001)
;   siz = (2L^ex) < 8192

    title = str[0]
    head = strsplit( str[0], ', 	', /extract)
    nh = n_elements(head)
    first = 1
    do_throttle = 0
    j = 0
    for i=1L,siz-1 do begin
		te = extract(str[i],0,0)
		if (te ne '#') and (te ne ';') and (te ne '!') and (te ne '/') then begin
			s = strsplit( str[i], ', 	', /extract)
			if first then begin
				n = n_elements(s)
				buff = fltarr(n,siz)
			endif
			if n_elements(s) ge n then begin
				buff[*,j] = float(s[0:n-1])
				j = j+1
			endif
			first = 0
		endif else begin
			if first then title = str[i]
		endelse
    endfor
    on_ioerror, null

    spec2 = define(/spectrum)
    spec2.file = name
    spec2.source = name
    i1 = 0
    if channel then i1=1
	ts = strsplit(title, '()[]	 #;!/', count=count, /extract)
	if strlowcase(ts[0]) eq 'throttle' then do_throttle = 1

    title = ['keV','keV']
    if energy then begin
       spec2.cal.order = 1
       if count ge 2 then begin
         if strlowcase(ts[0]) eq 'energy' then title[i1] = ts[1]
       endif
       case strlowcase(title[i1]) of
         'ev': begin
              scale = 0.001
              new = 'keV'
              end
         'mev': begin
              scale = 1000.0
              new = 'keV'
              end
         'kev': begin
              scale = 1.0
              new = 'keV'
              end
         else: begin
              scale = 1.0
              new = title[i1]
              end
       endcase
       spec2.cal.units = new
       spec2.cal.poly[0] = scale * buff[i1,1]
       spec2.cal.poly[1] = scale * (buff[i1,2] - buff[i1,1])
       i1 = i1+1
    endif

    p = ptrarr(n-i1)
    for i=i1,n-1 do begin
		obj = obj_new('GENERIC_DEVICE')
		if obj_valid(obj) eq 0 then goto, err
		spec2.DevObj = clone_device_object(obj)

		spec2.label = (nh ge n) ? head[i] : name

		if channel then begin
			bigc = max(buff[0,*])+100
			d = replicate(float(do_throttle),bigc+1)
			d[buff[0,*]>0] = reform(buff[i,*])
		endif else begin
			d = reform(buff[i,*])
		endelse
		spec2.size = n_elements(d)
		spec2.data = ptr_new( d, /no_copy )

		p[i-i1] = ptr_new( spec2)
    endfor

more:
    close_file,unit
    return

err:
    warning,'get_ascii_spec','error opening file: '+name
    p = 0L
	goto, more
    end

