pro geopixe_gen_commands, F, args

; Write a GeoPIXE Command File (.gcf) instead of execution.
; The file can be used in a work flow pipeline to call GeoPIXE with this file
; as the first command-line argument (see "geopixe_do_commands"). 
;
; args[0] assumed to be 'command' name
; args[1] is input file-name string vector (will be passed as a "files=" argument)
; args[2:*] is argument list for 'command'.
;
; In "geopixe_do_commands", a second argument would optionally replace the input file-name 
; string vector ("files=") line. A third argument would replace the 'output' file-name 
; string ("output=") line (see "geopixe_do_commands").

	if F eq '' then return
	if n_elements(args) lt 3 then return

	F = strip_file_ext(F) + '.gcf'
	on_ioerror, bad_file
	openw, unit, F, /get_lun

	on_ioerror, bad_write
	printf, unit,'#'
	printf, unit,'# ' + args[0] + ' command file' 
	printf, unit,'#'
	printf, unit,'# Pass to GeoPIXE on the CLI command-line to execute processing as part of a'
	printf, unit,'# processing pipe-line. First line must remain "'+args[0]+'"'
	printf, unit,'#'
	printf, unit,'# Optionally, pass additional command-line arguments to replace the input files'
	printf, unit,'# in the "files=" line, and the output filename in the "output=" line.'
	printf, unit,'# Input files can be replaced by a file-list or a wild-card specification,'
	printf, unit,'# for some commands.'
	printf, unit,'#'

	printf, unit, args[0]
	s = args[1]
	if locate('files=',s) ne 0 then s='files='+s
	printf, unit, s
	printf, unit,'#'
	for i=2,n_elements(args)-1 do begin
		printf, unit, args[i]
	endfor
	close_file, unit
	return

bad_file:
	warning, 'geopixe_gen_commands', 'Error opening file.'
	close_file, unit
	return
bad_write:
	warning, 'geopixe_gen_commands', 'Bad write to GCF file.'
	close_file, unit
	return
end
