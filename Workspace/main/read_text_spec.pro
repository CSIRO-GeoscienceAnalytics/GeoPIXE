function read_text_spec, file, header=header, find=find
;
;	Read the spectra file 'file'
;
;	Return 'p', a pointer (or pointer array) pointing to
;	spectrum structs, containing the spectrum details
;	and data.
;
;	If /header then only read header stuff for  spectrum 'find',
;	where 'find' is the station number (1,2,3,...), 0 means any.

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'read_text_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

if n_elements(file) lt 1 then goto, bad_file
if n_elements(header) lt 1 then header=0
if n_elements(find) lt 1 then find=0

on_ioerror, bad_io
close, 1
openr, 1, file

valid = [-1,-2,-3,-4,-5,-7,-8,-9,-10]
bad = [-6]
version = 0
readf,1, version
q = where( version eq valid)
if q[0] eq -1 then goto, bad_version

if version le -2 then begin
	processed = 0L
	valid = 0L
	bad_xy = 0L
	clipped = 0L
	xstep_on = 0
	xstep = 0L
	step_events = 0
	step_toggle = 0
	toggle_bit = 0
	toggle_station = 0
	events = 0L
	type = 0
	detector = 0
	xcompress = 1
	channel = 0				; redundant (use station)
	device_name = 'MPSYS_DEVICE'
	source2 = ''

	readf,1, processed, valid, bad_xy, clipped
	readf,1, xstep_on, xstep, step_events
	readf,1, step_toggle, toggle_bit, toggle_station
	readf,1, events
	readf,1, channel, type, detector, xcompress

	if version le -3 then begin
		ycompress = 1
		nx = 0
		ny = 0
		readf,1, ycompress, nx, ny
	endif
	if version le -4 then begin
		microns = 0.0
		ecal_a = 0.0
		ecal_b = 0.0
		readf,1, microns, ecal_a, ecal_b
	endif

	mfile = ''
	mcharge = 0.0
	nmdl = 0
	readf,1, mfile
	readf,1, mcharge
	readf,1, nmdl
	if nmdl gt 0 then begin
		mdl = fltarr(nmdl)
		readf,1, mdl
	endif
	
;	Note that this text spec format is not used later when then new
;	device objects appeared.

	if version le -8 then begin
		readf, 1, device
		print,'version old: device=',device
		old = device_index_from_old_index( device, name=name, error=err)
		if err then print, '    Error converting old device index.' 
		device_name = name
	endif
	if version le -9 then begin
		readf, 1, source2
	endif
endif
obj = obj_new( device_name)

n = 0
readf,1, n
if n lt 1 then goto, error

p = 0
for j=0L,n-1 do begin
	pj = get_text_spec( 1, header=header, find=find, version=version)
	if ptr_valid(pj) eq 0 then goto, bad_get
	(*pj).file = file

	if version le -2 then begin
		(*pj).processed = processed
		(*pj).valid = valid
		(*pj).bad_xy = bad_xy
		(*pj).clipped = clipped
		(*pj).xstep_on = xstep_on
		(*pj).xstep = xstep
		(*pj).step_events = step_events
		(*pj).step_toggle = step_toggle
		(*pj).toggle_bit = toggle_bit
		(*pj).toggle_station = toggle_station
		(*pj).events = events
		(*pj).type = type
		(*pj).channel = channel
		(*pj).detector = detector
		(*pj).xcompress = xcompress

		(*pj).matrix.file = mfile
		(*pj).matrix.charge = mcharge
		(*pj).matrix.mdl = ptr_new(mdl, /no_copy)
	endif
	if version le -3 then begin
		(*pj).ycompress = ycompress
		(*pj).nx = nx
		(*pj).ny = ny
	endif
	if version le -4 then begin
		(*pj).microns = microns
		(*pj).ecal.poly[1] = ecal_a
		(*pj).ecal.poly[0] = ecal_b
	endif
;	if version le -8 then begin
		(*pj).DevObj = clone_device_object(obj)
;	endif
	if version le -9 then begin
		(*pj).source2 = source2
	endif

	if header then begin
		if ((*pj).station eq 0) or ((*pj).station eq find) or (find eq 0) then begin
			p = pj
			goto, finish
		endif
		free_spectrum, pj
	endif else begin
		if j eq 0 then begin
			p = pj
		endif else begin
			p = [p, pj]
		endelse
	endelse
endfor
if header then begin
	warning,'read_text_spec','failed to find selected station.'
endif

finish:
	close,1
	return, p

bad_get:
	print,'read_text_spec: get_spec error'
	goto, finish
bad_io:
	print,'read_text_spec: I/O error'
	goto, error
bad_file:
	print,'read_text_spec: no file name supplied'
	goto, usage
bad_version:
	print,'read_text_spec: bad version number'
	goto, error

usage:
	print,'read_text_spec: Usage: p = read_text_spec(file)'
	print,'		where "p" is pointer to spectrum struct(s)'
	print,'		and "file" is the name of the input file'
	goto, error

error:
	p = 0
	goto, finish
end
