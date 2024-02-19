	function read_maia_parameters, file, data=p, index=index, path=path, group=group
	
;	Read Maia parameters to a CSV file.
;	to struct pointed to by 'p'.
;	detector indices in 'index'

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(p) eq 0 then goto, bad_ptr
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr
	if n_elements(file) lt 1 then file='Maia.parameters.csv'
	if lenchr(file) lt 1 then goto, bad_file

	F = strip_file_ext(file) + '.csv'
	on_ioerror, bad_file
	openr,unit, F, /get_lun
	on_ioerror,err
	(*p).file = file
	obj = (*p).DevObj

	s = ''
	n_detectors = 0
	readf, unit, s
	readf, unit, n_detectors
	if n_detectors ne (*p).n_detectors then goto, err_mismatch
	(*p).trim.file2 = ''
	
	while( not( EOF(unit)) ) do begin
		readf, unit, s
		str = strsplit( s, ', ', /extract)
		ns = n_elements(str)
		if ns ge 2 then begin
			case strlowcase(str[0]) of
				'linearization:': begin
					(*p).linear.on = fix(str[1])
					if ns ge 3 then begin
						t = str[2]
						if locate( slash(), t) eq -1 then t = fix_path(path) + t
						(*p).linear.file = t
						F = file_requester( /read, filter='*.linear.var', file=(*p).linear.file, $
							path=path, group=group, /translate, updir=3, $
							title='Select new path for linearization file', fix_filter=0, /skip_if_exists)
						if F[0] ne '' then (*p).linear.file = F[0] 
					endif else (*p).linear.on=0
					end
				'gain-trimming:': begin
					(*p).trim.on = fix(str[1])
					if ns ge 3 then begin
						t = str[2]
						if locate( slash(), t) eq -1 then t = fix_path(path) + t
						(*p).trim.file = t
						F = file_requester( /read, filter=['*.gaintrim.energy.var','*.spec'], file=(*p).trim.file, $
							path=path, group=group, /translate, updir=3, $
							title='Select new path for energy gaintrim file', fix_filter=0, /skip_if_exists)
						if F[0] ne '' then (*p).trim.file = F[0] 
					endif else (*p).trim.on=0
					end
				'gain-trimming2:': begin
					t = str[1]
					if locate( slash(), t) eq -1 then t = fix_path(path) + t
					(*p).trim.file2 = t
					F = file_requester( /read, filter=['*.gaintrim.time.var','*.spec'], file=(*p).trim.file2, $
						path=path, group=group, /translate, updir=3, $
						title='Select new path for time gaintrim file', fix_filter=0, /skip_if_exists)
					if F[0] ne '' then (*p).trim.file2 = F[0] 
					end
				'pileup:': begin
					(*p).pileup.on = fix(str[1])
					if ns ge 3 then begin
						t = str[2]
						if locate( slash(), t) eq -1 then t = fix_path(path) + t
						(*p).pileup.file = t
						F = file_requester( /read, filter=['*.pileup.var'], file=(*p).pileup.file, $
							path=path, group=group, /translate, updir=3, $
							title='Select new path for pileup field file', fix_filter=0, /skip_if_exists)
						if F[0] ne '' then (*p).pileup.file = F[0] 
					endif else (*p).pileup.on=0
					end
				'throttle:': begin
					(*p).throttle.on = fix(str[1])
					if ns ge 3 then begin
						t = str[2]
						if locate( slash(), t) eq -1 then t = fix_path(path) + t
						(*p).throttle.file = t
						F = file_requester( /read, filter=['*.throttle.var'], file=(*p).throttle.file, $
							path=path, group=group, /translate, updir=3, $
							title='Select new path for throttle vector file', fix_filter=0, /skip_if_exists)
						if F[0] ne '' then (*p).throttle.file = F[0] 
					endif else (*p).throttle.on=0
					end
				'energy-calibration:': begin
					t = str[1]
					if locate( slash(), t) eq -1 then t = fix_path(path) + t
					(*p).cal.file = t
					F = file_requester( /read, filter=['*.spec'], file=(*p).cal.file, $
						path=path, group=group, /translate, updir=3, $
						title='Select new path for energy cal file', fix_filter=0, /skip_if_exists)
					if F[0] ne '' then (*p).cal.file = F[0] 
					(*p).cal.mode = 0
					end
				'energy-calibration2:': begin
					(*p).cal.mode = fix(str[1])
					if ns ge 3 then begin
						t = str[2]
						if locate( slash(), t) eq -1 then t = fix_path(path) + t
						(*p).cal.file = t
						F = file_requester( /read, filter=['*.spec'], file=(*p).cal.file, $
							path=path, group=group, /translate, updir=3, $
							title='Select new path for energy cal file', fix_filter=0, /skip_if_exists)
						if F[0] ne '' then (*p).cal.file = F[0] 
					endif else (*p).cal.file = ''
					end
				'deadtime-calibration:': begin
					(*p).deadtime.cal.b = float2(str[1])
					if ns ge 3 then (*p).deadtime.cal.a = float2(str[2])
					end
				'asic': goto, asic
				else:
			endcase
		endif
	endwhile
	goto, err
	
asic:
	readf, unit, s
	str = strsplit( s, ', 	', /extract)
	n_str = n_elements(str)
	if n_str eq 16 then begin
		data = replicate( {ID:0, time:0, gain:0, eblk:0, elk:0, tdm:0, tds:0, tos:0, trk:0, trke:0, trim:0.0, thresh:0.0, clock:0.0, thpd:0.0, tcm:0, filt:0}, (*p).n_detectors)
	endif else if n_str eq 13 then begin
		data = replicate( {ID:0, time:0, gain:0, eblk:0, elk:0, tdm:0, tds:0, tos:0, trk:0, trke:0, trim:0.0, thresh:0.0, clock:0.0}, (*p).n_detectors)
	endif else goto, bad_format
	
	readf, unit, data
	
	(*p).channel.hermes.time = data.time
	(*p).channel.hermes.gain = data.gain
	(*p).channel.hermes.eblk = data.eblk
	(*p).channel.hermes.elk = data.elk
	(*p).channel.scepter.tdm = data.tdm
	(*p).channel.scepter.tds = data.tds
	(*p).channel.scepter.tos = data.tos
	(*p).channel.scepter.trk = data.trk
	(*p).channel.scepter.trke = data.trke
	(*p).channel.scepter.trim = data.trim
	(*p).channel.scepter.thresh = data.thresh
	(*p).channel.scepter.clock = data.clock
	if n_str ge 16 then begin
		(*p).channel.scepter.thpd = data.thpd
		(*p).channel.scepter.tcm = data.tcm
		(*p).channel.scepter.filt = data.filt
;		print,'Scepter tcm = ', (*p).channel.scepter.tcm
;		print,'Scepter filt = ', (*p).channel.scepter.filt
	endif
	index = data.ID

	ps = read_spec((*p).cal.file)
	if ptr_valid(ps[0]) then begin
		case (*p).cal.mode of
			0: begin
				for i=0L,n_elements(ps)-1 do begin
					n = strlen((*ps[i]).label)
					lab = strmid( (*ps[i]).label, n-2,2)
					if strupcase(lab) eq '/E' then begin
						j = fix((*ps[i]).station + adc_offset_device(obj))
						if (j ge 0) and (j lt n_detectors-1) then begin
							q = where( index eq j, nq)
							if nq ge 1 then begin
								(*p).channel[q[0]].cal.b = (*ps[i]).cal.poly[0]
								(*p).channel[q[0]].cal.a = (*ps[i]).cal.poly[1]
							endif
						endif
					endif
				endfor
				end
			1: begin
				b = (*ps[0]).cal.poly[0]
				a = (*ps[0]).cal.poly[1]
				if (abs(b) lt 0.01) and (abs(a-1.) lt 0.01) then warning,'read_maia_parameters','First spectrum in file is NOT calibrated.'
				(*p).channel.cal.b = b
				(*p).channel.cal.a = a
				end
		endcase
	endif
	error = 0
fin:
	close_file, unit
	return, error

err_mismatch:
	warning, 'read_maia_parameters', 'Mismatch with expected number of detectors.'
	goto, fin
err:
	warning, 'read_maia_parameters', 'Error reading file.'
	goto, fin
bad_ptr:
	warning, 'read_maia_parameters', 'Bad detector data pointer.'
	goto, fin
bad_file:
	warning, 'read_maia_parameters', 'Error opening file.'
	goto, fin
bad_format:
	warning, 'read_maia_parameters', 'Bad file format or number of columns.'
	goto, fin
	end

