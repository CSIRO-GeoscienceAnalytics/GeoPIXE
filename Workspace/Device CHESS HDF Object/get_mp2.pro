	function get_mp2, file, error=error
;
;	Read in a CHESS run text file for run details.
;	Assumed to be named like containing dir.
;	If not found, then return error=1.
;
	COMPILE_OPT STRICTARR
	common c_geopixe_adcs, geopixe_max_adcs
	if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

	error = 1
	mp = -1
	if n_elements(file) lt 1 then return,0
	if lenchr(file) lt 1 then return,0

	F = file_dirname( file)
	i = locate_last('_scan', F)
	if i gt 0 then F = strmid(F,0,i)

	on_ioerror,more
	openr,unit,F,/get_lun

	mp = 0

;	These details need to match those in get_header_info

	id = ''
	date = ''
	time = ''
	name = ''
	comment = ''
	user = ''
	charge = 0.0
	base_charge = 0.1
	clock = 80.0
	deadtime_mode = ''
	monitor = ''
	ncol = 0
	title = ''
	flux = 0.
	
;	scan: origin:x,y, size(mm):x_mm,y_mm, pixels:x_pixels,y_pixels

    scan = {on:0, x_pixels:0, y_pixels:0, z_pixels:0, x:0.0, y:0.0, z:0.0, x_mm:0.0, y_mm:0.0, z_mm:0.0, $
          	dwell:0.0 }
	detector_name = strarr(geopixe_max_adcs)
	detector = replicate(-1,geopixe_max_adcs)
	cal = replicate( {on:0, a:1.0, b:0.0, units:'channel'}, geopixe_max_adcs)
	
	line = ''

	on_ioerror,err
	while EOF(unit) eq 0 do begin
		readf, unit, line
		if lenchr(line) eq 0 then continue

		if (extract(line,0,0) eq '#') then begin

			s = strsplit( line, ' 	=', /extract, count=ns)
			sall = strjoin( s[1:*], ' ')
			
			case s[0] of
				'#F': begin
					name = sall
					end
				'#E': begin
					id = s[1]
					end
				'#D': begin
					date = sall
					end
				'#C': begin
					comment = sall
					user = s[3]
					end
				'#T': begin
					scan.dwell = 1000. * float2(s[1])
					end
				'#S': begin
					scan.on = 1
					scan.x_pixels = long2(s[6])+1
					scan.y_pixels = long2(s[10])+1
					scan.x = float2(s[4])
					scan.y = float2(s[8])
					scan.x_mm = float2(s[5]) - float2(s[4])
					scan.y_mm = float2(s[9]) - float2(s[8])
					scan.dwell = 1000. * float2(s[11])
					end
				'#U': begin
					monitor = s[2]
					end
				'#L': begin
					title = s[1:*]
					end
				'#N': begin
					ncol = fix2(s[1])
					end
				else:
			endcase
		endif else goto, read_data
	endwhile

	on_ioerror,null

read_data:
	comment = name + ' (' + str_tidy(id) + '), ' + date + ' ,' + comment
	if ncol eq 0 then goto, err

	q = where( title eq monitor, nq)
	if nq eq 0 then begin
		nic = -1
	endif else begin
		nic = q[0]
	endelse

	data = fltarr(ncol,10000000)
	xs = fltarr(10000000)
	ys = fltarr(10000000)

	on_ioerror,cont
	readf, unit, data

cont:
	xs = data[0,*]
	ys = data[1,*]
	q = where( (xs ne 0.) and (ys ne 0.), nq)
	data = data[*,q]
	xs = reform(data[0,*])
	ys = reform(data[1,*])
	if nic ge 0 then begin 
		nx = scan.x_pixels
		ny = scan.y_pixels
		if (nx gt 0) and (ny gt 0) then begin
			xs = xs[0:nx-1]
			ys = ys[0:nq-1:nx]
			flux = fltarr(nx,ny)
			flux[0:nq-1] = data[nic,*]
		endif
	endif

	error = 0

	mp = {	comment:	comment, $			; comment string
			id:			id, $				; ID number
			date:		date, $				; date
			user:		user, $				; user name
			name:		name, $				; run name, point name
			scan:		scan, $				; scan parameters
			xs:			xs, $				; X values
			ys:			ys, $				; Y values
			flux:		flux, $				; flux values

;			cal:		cal, $				; station E cal parameters
			error:		error $				; error return
	}

more:
	close_file, unit
	return, mp

err:
	warning, 'get_mp2', ['Serious error in MP file.','Ignoring MP file settings.']
	goto, more
	end

