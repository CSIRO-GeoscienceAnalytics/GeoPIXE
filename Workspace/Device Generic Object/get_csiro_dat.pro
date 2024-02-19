	pro get_csiro_dat, p, file, fit=fit, group=group
;
;	Read in an old CSIRO GeoPIXE DAT binary spectrum
;
	p = 0L
	define_devices

	if n_params(0) lt 2 then return
	if n_elements(group) lt 1 then group=0L
	if n_elements(fit) lt 1 then fit=0
	if file eq '' then return

	header = {	label:		bytarr(32), $		; first part of label
				story:		bytarr(32), $		; first story record
				story2:		bytarr(32), $		; second story record
				story3:		bytarr(32), $		; third story record
				label2:		bytarr(16), $		; second part of label
				comment:	bytarr(72), $		; comment
				sample:		bytarr(8), $		; sample name
				grain:		bytarr(8), $		; grain ID
				x:			0.0, $				; X coord
				y:			0.0, $				; Y coord
				reserve1:	0L, $				;
				extra_record: 0L, $				; flags extra header record for labels
				n_togo:		0L, $				; spectra to go in file
				size:		0L, $				; size of this spectrum
				header_type: 0L, $				; bit flags (like a version)
				vm:			fltarr(2), $		; view markers
				xm:			fltarr(10), $		; X markers
				cm:			fltarr(2,9), $		; cuts
				c_a:		0.0, $				; Cal A
				c_b:		0.0, $				; Cal B
				c_units:	bytarr(4), $		; Cal units
				c_c:		fltarr(7), $		; Cal higher poly terms
				reserve2:	lonarr(2), $		;
				c_order:	0L, $				; Cal order
				charge:		0.0, $				; charge (uC)
				reserve3:	0L, $				;
				absorber:	0L, $				; absorber
				thick:		0.0, $				; thick
				detector_pos:	0L, $			; detector position
				energy_beam:	0.0, $			; beam energy
				tag_word:	0L, $				; tag word
				extras:		lonarr(13) }		; extra parameters

	on_ioerror, bad_io
	openr, unit, file, bufsiz=100*1024L, /get_lun
	togo = 1
	labels = lonarr(128)

	while togo gt 0 do begin

		on_ioerror, bad_header
		readu, unit, header
		swap_bytes, header, big_endian_data=0, /vax_data

		siz = 128 * ((header.size + 127) / 128)
		if siz lt 128 then goto, bad_size
		data = fltarr(siz)

;		WARNING: lots of labels will overflow into a second (or third) label
;		record. This routine ignores this possibility and only reads one.

		if header.extra_record ne 0 then begin
			readu, unit, labels
			swap_bytes, labels, big_endian_data=0, /vax_data
		endif

		device_name = 'MPSYS_DEVICE'
		obj = obj_new(device_name)

		on_ioerror, bad_spectrum
		readu, unit, data
 		swap_bytes, data, big_endian_data=0, /vax_data

		spec2 = define(/spectrum)
		spec2.file = file
		spec2.source = ''
		spec2.DevObj = obj
		spec2.label = strtrim(string(header.label[0:16]),2) + $
						strtrim(string(header.label2),2) + '   ' + $
						strtrim(string(header.label[17:*]),2)
		spec2.ecompress = 1
		spec2.n_history = 3
		spec2.history[0] = string(header.story)
		spec2.history[1] = string(header.story2)
		spec2.history[2] = string(header.story3)
		spec2.comment = string(header.comment)
		spec2.sample = string(header.sample)
		spec2.grain = string(header.grain)
		spec2.x = header.x
		spec2.y = header.y
		spec2.charge = header.charge
		print, 'label=',string(header.label),'   ',string(header.label2)
		print, 'story=',string(header.story),'   ',string(header.story2),'   ',string(header.story3)
		print, 'sample=',string(header.sample),'   ',string(header.grain)
		print, 'comment=',string(header.comment)

		spec2.cal.order = header.c_order < 7
		spec2.cal.units = string(header.c_units)
		spec2.cal.poly[0] = header.c_b
		spec2.cal.poly[1] = header.c_a
		spec2.cal.poly[2:7] = header.c_c[0:5]

		spec2.size = siz
		spec2.data = ptr_new(data)

		if (n_elements(p) ge 1) and ptr_valid(p[0]) then begin
			if fit then begin
				(*p[0]).fit[(*p[0]).n_fit] = ptr_new( spec2, /no_copy)
				(*p[0]).n_fit = (*p[0]).n_fit + 1
			endif else begin
				p = [p, ptr_new(spec2,/no_copy)]
			endelse
		endif else begin
			p = ptr_new(spec2,/no_copy)
		endelse

		togo = header.n_togo
	endwhile

done:
	close,unit
	free_lun,unit
	return

bad_size:
	warning,'get_csiro_spec','Null spectrum size?'
	goto, done
bad_header:
	warning,'get_csiro_spec','Error reader header'
	goto, done
bad_spectrum:
	warning,'get_csiro_spec','Error reading spectrum'
	goto, done
bad_io:
	warning,'get_csiro_spec','error opening file: '+file
	goto, done
	end
