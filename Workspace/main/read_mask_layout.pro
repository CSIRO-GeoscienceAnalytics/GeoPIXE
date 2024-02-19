	function read_mask_layout, file, error=error
;
;	Read in a detector mask CSV file.
;	If not found, then return error=1.

	COMPILE_OPT STRICTARR
	error = 1
	mp = 0
	if n_elements(file) lt 1 then return,0
	if lenchr(file) lt 1 then goto, bad_file

	F = strip_file_ext(file) + '.csv'
	on_ioerror,bad_file
	openr,unit,F,/get_lun

	data = 0
	N = 1000
	line = ''
	readf, unit, line
	readf, unit, line
	set_separators, ','
	chop_string, line, str, n_str

	case n_str of
		10: begin
			data = replicate({IDx:0, IDy:0, xlow:0.0, xhigh:0.0, ylow:0.0, yhigh:0.0, $
					X:0.0, Y:0.0, width:0.0, height:0.0}, N)
			end
		6: begin
			data = replicate({IDx:0, IDy:0, xlow:0.0, xhigh:0.0, ylow:0.0, yhigh:0.0}, N)
			end
		else: goto, bad_line
	endcase

	on_ioerror, cont
	readf, unit, data
cont:
	q = where( data.xlow ne 0.0, nq)
	if nq eq 0 then goto, bad_n
	data = data[q]
	error = 0
	
finish:
	close_file, unit
	return, data

bad_line:
	warning, 'read_mask_layout', 'Wrong number of co,umns.'
	goto, finish
bad_n:
	warning, 'read_mask_layout', 'Bad number of detectors.'
	goto, finish
bad_file:
	warning, 'read_mask_layout', 'File "'+file+'" not found.'
	goto, finish
	end

