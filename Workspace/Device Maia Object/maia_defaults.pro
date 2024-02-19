function maia_defaults, error=error, source=source, conf=file, reset=reset, any=any

; Read Maia defaults from selected conf file in .geopixe home directory.
; If none found, set some defaults (at the bottom) for XFM beamline data.
; Pass 'file' to specify the conf file to use (e.g. as in blog clients).
;
;	/reset	force showing file selector.
;	/any	return with first found.

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_maia_default, maia_conf
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
	if n_elements(source) lt 1 then source='?'
	if n_elements(reset) lt 1 then reset=0
	if n_elements(any) lt 1 then any=0
	if reset then maia_conf=''
	home_dir = geopixe_environment()
	retry = 1
	
	now = file_expand_path('.')
	error = 1
	maia = {	version:	1, $				; default software version
				detectors:	384, $				; default number of detectors
				facility:	'', $				; default facility name
				endstation:	'', $				; default endstation name
				spectra:	12, $				; default number of spectra groups
				DA:			32, $				; default number of DA images
				image: { x:	1000, $				; default DA image X size
						 y:	1000}, $			; default DA image Y size
				maia: {	ip:	'', $				; Maia socket IP
						name: '', $				; unique name of Maia system
						port: 0L, $				; Maia socket port
						device:	'MAIA_DEVICE', $
						enable: 0, $			; Maia socket enable
						debug: 0, $				; enable debug in Maia control?
						project_select: 0, $	; select whether "project" is changeable from Maia-Control
						timeout: 30.}, $ 		; timeout for launching background processes
				epics: { pic: ptr_new(/allocate_heap), $	; pointer to Epics IC PV string kernels
						npic: 0, $				; number of PV kernels to look for
						energy: ''}, $			; energy PV string kernel
				blog: {	ip:	'', $				; Blog socket IP
						port: 0L, $				; Blog socket port
						enable: 0, $			; Blog socket enable
						debug: 0}, $			; enable debug in background processes
				logfile: '', $					; optionally override log file path and name 
				conf:	'' }					; config file name
						
	if n_elements(file) ne 0 then maia_conf=file
	if n_elements(maia_conf) eq 0 then goto, read_conf
	if maia_conf[0] eq '' then goto, read_conf
	goto, cont
	
read_conf:
	conf_files = file_search( home_dir + '*.Maia.conf')
	if conf_files[0] eq '' then begin
		if any then begin
			on_ioerror, do_warn
			print,'Copy "template.Maia.conf" to home dir ...'
			mdef = 'maia' + path_sep() + 'template.Maia.conf'
			file_copy, mdef, home_dir, /require_dir
			on_ioerror, null
			conf_files = file_search( home_dir + '*.Maia.conf')
			if conf_files[0] ne '' then goto, check
		endif
do_warn:
		warning,'maia_defaults',['No Maia conf files found in user home dir:', $
					home_dir,'Abort.','','You can make a config file (extension ".Maia.conf") ', $
						'based on the template file "template.Maia.conf" in the "geopixe/maia" directory. ', $
						'Create additional .Maia.conf files to manage connection to multiple HYMODs.']
		error = 1
		return, 0
	endif

check:
	if any or (n_elements(conf_files) eq 1) then begin
		maia_conf = conf_files[0]
		goto, cont
	endif
	
	sel = options_popup( 0L, drop=strip_path(conf_files), path=home_dir, title='Select Maia conf file', error=err, $
				min_xsize=350, $
				help_drop='Select a Maia config file from this list from your Home .geopixe directory. You can make a config file ' + $
						'based on the template file "template.Maia.conf" in the "geopixe/maia" directory.', $
				help_default='Select a Maia config file from your Home .geopixe directory. You can make a config file (extension ".Maia.conf") ' + $
						'based on the template file "template.Maia.conf" in the "geopixe/maia" directory. ' + $
						'Create additional .Maia.conf files to manage connection to multiple Maia/HYMODs.'		)
	if err then begin
		warning,'maia_defaults',['Error selecting Maia conf files in user home dir:', $
					home_dir,'Abort.']
		error = 1
		return, 0
	endif
	maia_conf = conf_files[sel.drop[0]]

cont:
	if file_test(maia_conf) eq 0 then begin
		if file_test(home_dir + maia_conf) then begin
			maia_conf = home_dir + maia_conf
		endif else if retry then begin
			retry = 0
			goto, read_conf
		endif
	endif
	on_ioerror, bad_open
	openr, unit, maia_conf, /get_lun
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
					'epics': begin
						if ns ge 3 then begin
							case s[1] of
								'ic_pv': begin
									maia.epics.npic = maia.epics.npic +1
									if maia.epics.npic eq 1 then begin
										*maia.epics.pic = s0[2]
									endif else begin
										*maia.epics.pic = [*maia.epics.pic, s0[2]]
									endelse
									end
								'energy_pv': begin
									maia.epics.energy = s0[2]
									end
								else:
							endcase
						endif
						end
					'maia': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									maia.maia.ip = s0[2]
									end
								'name': begin
									maia.maia.name = s0[2]
									end
								'port': begin
									maia.maia.port = long2(s[2])
									end
								'device': begin
									maia.maia.device = s0[2]
									end
								'enable': begin
									maia.maia.enable = fix2(s[2])
									end
								'debug': begin
									maia.maia.debug = fix2(s[2])
									end
								'timeout': begin
									maia.maia.timeout = float2(s[2])
									end
								'project_select': begin
									maia.maia.project_select = long2(s[2])
									end
								else:
							endcase
						endif
						end
					'blog': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									maia.blog.ip = s0[2]
									end
								'port': begin
									maia.blog.port = long2(s[2])
									end
								'enable': begin
									maia.blog.enable = fix2(s[2])
									end
								'debug': begin
									maia.blog.debug = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'logfile': begin
						if ns ge 2 then begin
							maia.logfile = strjoin(s0[1:*],' ')
						endif
						end
					'default': begin
						if ns ge 3 then begin
							case s[1] of
								'version': begin
									maia.version = fix(s[2])
									end
								'detectors': begin
									maia.detectors = fix(s[2])
									end
								'spectra': begin
									maia.spectra = fix(s[2])
									end
								'da': begin
									maia.DA = fix(s[2])
									end
								'x': begin
									maia.image.x = fix(s[2])
									end
								'y': begin
									maia.image.y = fix(s[2])
									end
								'facility': begin
									maia.facility = s0[2]
									end
								'endstation': begin
									maia.endstation = s0[2]
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
	maia.conf = maia_conf
	error = 0
	return, maia
	
bad_find:	
	warning,'maia_defaults',['Failed to find Maia config file: "' + maia_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'maia_defaults',['Failed to open Maia config file: "' + maia_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
							
bad_cont:
	close_file, unit

	*maia.epics.pic = [':HVSUM_MON',':scaler',':CURRENT_MONITOR']
	maia.epics.npic = 3
	maia.epics.energy = ':ENERGY_MON'
	return, maia
	
bad_read:	
	warning,'maia_defaults','bad read from Maia config file: "' + maia_conf + '".'
	close_file, unit
	return, maia
end
