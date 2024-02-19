function mdaq2_defaults, error=error, source=source, conf=file, reset=reset, any=any

; Read mdaq2 defaults from selected conf file in .geopixe home directory.
; This is a draft, based on Maia, not completed, and not called as yet (see Midas device source).
; Edit to provide config parameters for Midas device.
;
; If none found, set some defaults (at the bottom) for XFM beamline data.
; Pass 'file' to specify the conf file to use (e.g. as in blog clients).

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_mdaq2_default, mdaq2_conf
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
	if n_elements(source) lt 1 then source='?'
	if n_elements(reset) lt 1 then reset=0
	if n_elements(any) lt 1 then any=0
	if reset then mdaq2_conf=''
	
	now = fix_path(file_expand_path('.'))
	error = 1
	mdaq2 = {	version:	0, $		; default software version
				detectors:	384, $		; default number of detectors
				spectra:	12, $		; default number of spectra groups
				DA:			32, $		; default number of DA images
				image: { x:	1000, $		; default DA image X size
						 y:	1000}, $	; default DA image Y size
				mdaq2: {	ip:	'', $		; mdaq2 socket IP
						name: '', $		; unique name of Maia system
						port: 0L, $		; Maia socket port
						enable: 0}, $	; Maia socket enable
				epics: { pic: ptr_new(/allocate_heap), $	; pointer to Epics IC PV string kernels
						npic: 0, $		; number of PV kernels to look for
						energy: ''}, $	; energy PV string kernel
				blog: {	ip:	'', $		; Blog socket IP
						port: 0L, $		; Blog socket port
						enable: 0}, $	; Blog socket enable
				logfile: 'Maia_384A_log.csv', $		; default log file name in s/w dir
				conf:	'' }			; config file name
						
	if n_elements(file) ne 0 then mdaq2_conf=file
	if n_elements(mdaq2_conf) eq 0 then goto, read_conf
	if mdaq2_conf[0] eq '' then goto, read_conf
	goto, cont
	
read_conf:
	home_dir = geopixe_environment()
	conf_files = file_search( home_dir + '*.mdaq2.conf')
	if conf_files[0] eq '' then begin
		if any then begin
			on_ioerror, do_warn
			print,'Copy "template.mdaq2.conf" to home dir ...'
			mdef = 'mdaq2' + path_sep() + 'template.mdaq2.conf'
			file_copy, mdef, home_dir, /require_dir
			on_ioerror, null
			conf_files = file_search( home_dir + '*.mdaq2.conf')
			if conf_files[0] ne '' then goto, check
		endif
do_warn:
		warning,'mdaq2_defaults',['No mdaq2 conf files found in user home dir:', $
					home_dir,'Abort.','','You can make a config file (extension ".mdaq2.conf") ', $
						'based on the template file "template.mdaq2.conf" in the "geopixe/mdaq2" directory. ', $
						'Create additional .mdaq2.conf files to manage connection to multiple HYMODs.']
		error = 1
		return, 0
	endif

check:
	if any or (n_elements(conf_files) eq 1) then begin
		mdaq2_conf = conf_files[0]
		goto, cont
	endif
	
	sel = options_popup( 0L, drop=strip_path(conf_files), path=home_dir, title='Select mdaq2 conf file', error=err, $
				min_xsize=350, $
				help_drop='Select a mdaq2 config file from this list from your Home .geopixe directory. You can make a config file ' + $
						'based on the template file "template.mdaq2.conf" in the "geopixe/mdaq2" directory.', $
				help_default='Select a mdaq2 config file from your Home .geopixe directory. You can make a config file (extension ".mdaq2.conf") ' + $
						'based on the template file "template.mdaq2.conf" in the "geopixe/mdaq2" directory. ' + $
						'Create additional .mdaq2.conf files to manage connection to multiple mdaq2/HYMODs.'		)
	if err then begin
		warning,'mdaq2_defaults',['Error selecting mdaq2 conf files in user home dir:', $
					home_dir,'Abort.']
		error = 1
		return, 0
	endif
	mdaq2_conf = conf_files[sel.drop[0]]

cont:
	on_ioerror, bad_open
	openr, unit, mdaq2_conf, /get_lun
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
									mdaq2.epics.npic = mdaq2.epics.npic +1
									if mdaq2.epics.npic eq 1 then begin
										*mdaq2.epics.pic = s0[2]
									endif else begin
										*mdaq2.epics.pic = [*mdaq2.epics.pic, s0[2]]
									endelse
									end
								'energy_pv': begin
									mdaq2.epics.energy = s0[2]
									end
								else:
							endcase
						endif
						end
					'mdaq2': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									mdaq2.mdaq2.ip = s0[2]
									end
								'name': begin
									mdaq2.mdaq2.name = s0[2]
									end
								'port': begin
									mdaq2.mdaq2.port = long2(s[2])
									end
								'enable': begin
									mdaq2.mdaq2.enable = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'blog': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									mdaq2.blog.ip = s0[2]
									end
								'port': begin
									mdaq2.blog.port = long2(s[2])
									end
								'enable': begin
									mdaq2.blog.enable = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'logfile': begin
						if ns ge 2 then begin
							mdaq2.logfile = strjoin(s0[1:*],' ')
						endif
						end
					'default': begin
						if ns ge 3 then begin
							case s[1] of
								'version': begin
									mdaq2.version = fix(s[2])
									end
								'detectors': begin
									mdaq2.detectors = fix(s[2])
									end
								'spectra': begin
									mdaq2.spectra = fix(s[2])
									end
								'da': begin
									mdaq2.DA = fix(s[2])
									end
								'x': begin
									mdaq2.image.x = fix(s[2])
									end
								'y': begin
									mdaq2.image.y = fix(s[2])
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
	mdaq2.conf = mdaq2_conf
	error = 0
	return, mdaq2
	
bad_find:	
	warning,'mdaq2_defaults',['Failed to find mdaq2 config file: "' + mdaq2_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'mdaq2_defaults',['Failed to open mdaq2 config file: "' + mdaq2_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
							
bad_cont:
	close_file, unit

	*mdaq2.epics.pic = [':HVSUM_MON',':scaler',':CURRENT_MONITOR']
	mdaq2.epics.npic = 3
	mdaq2.epics.energy = ':ENERGY_MON'
	return, mdaq2
	
bad_read:	
	warning,'mdaq2_defaults','bad read from mdaq2 config file: "' + mdaq2_conf + '".'
	close_file, unit
	return, mdaq2
end
