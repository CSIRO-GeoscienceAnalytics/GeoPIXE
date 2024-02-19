function daq_defaults, error=error, source=source, conf=file, reset=reset, any=any

; Read DAQ defaults from 'DAQ.conf' file.
; If none found, set some defaults (at the bottom) for UM beamline data.
; Pass 'file' to specify the conf file to use (e.g. as in blog clients).
; Optional "maia" fields are for a slave Maia detector.

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_daq_default, daq_conf
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
	if n_elements(source) lt 1 then source='?'
	if n_elements(reset) lt 1 then reset=0
	if n_elements(any) lt 1 then any=0
	if reset then daq_conf=''

	now = fix_path(file_expand_path('.'))
	error = 1
	daq = {		version:	0, $				; default software version
				detectors:	36, $				; default number of detectors
				DA:			32, $				; default number of DA images
				image: { x:	1000, $				; default DA image X size
						 y:	1000}, $			; default DA image Y size
				daq: {	ip:	'', $				; daq socket IP
						name: '', $				; unique name of daq system
						port: 0L, $				; daq socket port
						device:	'DAQ_DEVICE', $
						label32: '', $			; label NIM channel 32
						label33: '', $			; label NIM channel 33
						label34: '', $			; label NIM channel 34
						label35: '', $			; label NIM channel 35
						debug: 0, $				; enable debug in DAQ control?
						enable: 0}, $			; daq socket enable
				maia: {	ip:	'', $				; Maia socket IP
						name: '', $				; unique name of Maia system
						port: 0L, $				; Maia socket port
						device:	'MAIA_DEVICE', $
						enable: 0, $			; Maia socket enable
						debug: 0, $				; enable debug in Maia control?
						project_select: 1, $	; select whether "project" is changeable from Maia-Control
						timeout: 30.}, $ 		; timeout for launching background processes
				blog: {	ip:	'', $				; Blog socket IP
						port: 0L, $				; Blog socket port
						debug: 0, $				; enable debug in background processes
						enable: 0}, $			; Blog socket enable

				conf:	'' }					; config file name
				
	if n_elements(file) ne 0 then daq_conf=file
	if n_elements(daq_conf) eq 0 then goto, read_conf
	if daq_conf[0] eq '' then goto, read_conf
	goto, cont
	
read_conf:
	home_dir = geopixe_environment()
	conf_files = file_search( home_dir + '*.DAQ.conf')
	if conf_files[0] eq '' then begin
		if any then begin
			on_ioerror, do_warn
			print,'Copy "template.DAQ.conf" to home dir ...'
			mdef = 'daq' + path_sep() + 'template.DAQ.conf'
			file_copy, mdef, home_dir, /require_dir
			on_ioerror, null
			conf_files = file_search( home_dir + '*.DAQ.conf')
			if conf_files[0] ne '' then goto, check
		endif
do_warn:
		warning,'daq_defaults',['No DAQ conf files found in user home dir:', $
					home_dir,'Abort.','','You can make a config file (extension ".DAQ.conf") ', $
						'based on the template file "template.DAQ.conf" in the "geopixe/daq" directory. ', $
						'Create addition .DAQ.conf files to manage connection to multiple HYMODs.']
		error = 1
		return, 0
	endif

check:
	if any or (n_elements(conf_files) eq 1) then begin
		daq_conf = conf_files[0]
		goto, cont
	endif
	
	sel = options_popup( 0L, drop=strip_path(conf_files), path=home_dir, title='Select DAQ conf file', error=err, $
				min_xsize=350, $
				help_drop='Select a DAQ config file from this list from your Home .geopixe directory. You can make a config file ' + $
						'based on the template file "template.DAQ.conf" in the "geopixe/daq" directory.', $
				help_default='Select a DAQ config file from your Home .geopixe directory. You can make a config file (extension ".DAQ.conf") ' + $
						'based on the template file "template.DAQ.conf" in the "geopixe/daq" directory. ' + $
						'Create addition .DAQ.conf files to manage connection to multiple HYMODs.'		)
	if err then begin
		warning,'daq_defaults',['Error selecting DAQ conf files in user home dir:', $
					home_dir,'Abort.']
		error = 1
		return, 0
	endif
	daq_conf = conf_files[sel.drop[0]]

cont:
	on_ioerror, bad_open
	openr, unit, daq_conf, /get_lun
	line = ''
	
	on_ioerror, bad_read
	while NOT EOF(unit) do begin
		readf, unit, line
		i = locate('#', line)
		if i eq -1 then i=strlen(line)
		if i gt 0 then begin
			s0 = strsplit(strmid(line,0,i),' 	,',/extract)
			s = strlowcase(s0)
			ns = n_elements(s)
			if ns ge 2 then begin
				case s[0] of
					'daq': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									daq.daq.ip = s0[2]
									end
								'name': begin
									daq.daq.name = s0[2]
									end
								'port': begin
									daq.daq.port = long(s[2])
									end
								'device': begin
									daq.daq.device = s0[2]
									end
								'debug': begin
									daq.daq.debug = fix2(s[2])
									end
								'enable': begin
									daq.daq.enable = fix(s[2])
									end
								'label32': begin
									daq.daq.label32 = s0[2]
									end
								'label33': begin
									daq.daq.label33 = s0[2]
									end
								'label34': begin
									daq.daq.label34 = s0[2]
									end
								'label35': begin
									daq.daq.label35 = s0[2]
									end
								else:
							endcase
						endif
						end
					'blog': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									daq.blog.ip = s0[2]
									end
								'port': begin
									daq.blog.port = long(s[2])
									end
								'enable': begin
									daq.blog.enable = fix(s[2])
									end
								'debug': begin
									daq.blog.debug = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'maia': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									daq.maia.ip = s0[2]
									end
								'name': begin
									daq.maia.name = s0[2]
									end
								'port': begin
									daq.maia.port = long2(s[2])
									end
								'device': begin
									daq.maia.device = s0[2]
									end
								'enable': begin
									daq.maia.enable = fix2(s[2])
									end
								'debug': begin
									daq.maia.debug = fix2(s[2])
									end
								'timeout': begin
									daq.maia.timeout = float2(s[2])
									end
								'project_select': begin
									daq.maia.project_select = long2(s[2])
									end
								else:
							endcase
						endif
						end
					'default': begin
						if ns ge 3 then begin
							case s[1] of
								'version': begin
									daq.version = fix(s[2])
									end
								'detectors': begin
									daq.detectors = fix(s[2])
									end
								'da': begin
									daq.DA = fix(s[2])
									end
								'x': begin
									daq.image.x = fix(s[2])
									end
								'y': begin
									daq.image.y = fix(s[2])
									end
								else:
							endcase
						endif
						end
					else:
				endcase
			endif
		endif
	endwhile
	
	close_file, unit
	daq.conf = daq_conf
	error = 0
	return, daq
	
bad_find:	
	warning,'daq_defaults',['Failed to find daq config file: "' + daq_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'daq_defaults',['Failed to open daq config file: "' + daq_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
							
bad_cont:
	close_file, unit

	return, daq
	
bad_read:	
	warning,'daq_defaults','bad read from daq config file: "' + daq_conf + '".'
	close_file, unit
	return, daq
end
