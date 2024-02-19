function midas_defaults, error=error, source=source, conf=file, reset=reset, any=any

; Read Midas defaults from selected conf file in user .geopixe home directory.
; This is a draft, based on Maia, not completed, and not called as yet (see Midas device source).
; Edit to provide config parameters for Midas device.
;
; If none found, set some defaults (at the bottom) for XFM beamline data.
; Pass 'file' to specify the conf file to use (e.g. as in blog clients).

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_midas_default, midas_conf
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
	if n_elements(source) lt 1 then source='?'
	if n_elements(reset) lt 1 then reset=0
	if n_elements(any) lt 1 then any=0
	if reset then midas_conf=''
	
	now = file_expand_path('.')
	error = 1
	midas = {	version:	0, $		; default software version
				detectors:	384, $		; default number of detectors
				spectra:	12, $		; default number of spectra groups
				DA:			32, $		; default number of DA images
				image: { x:	1000, $		; default DA image X size
						 y:	1000}, $	; default DA image Y size
				midas: {	ip:	'', $	; midas socket IP
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
						
	if n_elements(file) ne 0 then midas_conf=file
	if n_elements(midas_conf) eq 0 then goto, read_conf
	if midas_conf[0] eq '' then goto, read_conf
	goto, cont
	
read_conf:
	home_dir = geopixe_environment()
	conf_files = file_search( home_dir + '*.midas.conf')
	if conf_files[0] eq '' then begin
		if any then begin
			on_ioerror, do_warn
			print,'Copy "template.midas.conf" to home dir ...'
			mdef = 'midas' + path_sep() + 'template.midas.conf'
			file_copy, mdef, home_dir, /require_dir
			on_ioerror, null
			conf_files = file_search( home_dir + '*.midas.conf')
			if conf_files[0] ne '' then goto, check
		endif
do_warn:
		warning,'midas_defaults',['No midas conf files found in user home dir:', $
					home_dir,'Abort.','','You can make a config file (extension ".midas.conf") ', $
						'based on the template file "template.midas.conf" in the "geopixe/midas" directory. ', $
						'Create additional .midas.conf files to manage connection to multiple HYMODs.']
		error = 1
		return, 0
	endif

check:
	if any or (n_elements(conf_files) eq 1) then begin
		midas_conf = conf_files[0]
		goto, cont
	endif
	
	sel = options_popup( 0L, drop=strip_path(conf_files), path=home_dir, title='Select midas conf file', error=err, $
				min_xsize=350, $
				help_drop='Select a midas config file from this list from your Home .geopixe directory. You can make a config file ' + $
						'based on the template file "template.midas.conf" in the "geopixe/midas" directory.', $
				help_default='Select a midas config file from your Home .geopixe directory. You can make a config file (extension ".midas.conf") ' + $
						'based on the template file "template.midas.conf" in the "geopixe/midas" directory. ' + $
						'Create additional .midas.conf files to manage connection to multiple midas/HYMODs.'		)
	if err then begin
		warning,'midas_defaults',['Error selecting midas conf files in user home dir:', $
					home_dir,'Abort.']
		error = 1
		return, 0
	endif
	midas_conf = conf_files[sel.drop[0]]

cont:
	on_ioerror, bad_open
	openr, unit, midas_conf, /get_lun
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
									midas.epics.npic = midas.epics.npic +1
									if midas.epics.npic eq 1 then begin
										*midas.epics.pic = s0[2]
									endif else begin
										*midas.epics.pic = [*midas.epics.pic, s0[2]]
									endelse
									end
								'energy_pv': begin
									midas.epics.energy = s0[2]
									end
								else:
							endcase
						endif
						end
					'midas': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									midas.midas.ip = s0[2]
									end
								'name': begin
									midas.midas.name = s0[2]
									end
								'port': begin
									midas.midas.port = long2(s[2])
									end
								'enable': begin
									midas.midas.enable = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'blog': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									midas.blog.ip = s0[2]
									end
								'port': begin
									midas.blog.port = long2(s[2])
									end
								'enable': begin
									midas.blog.enable = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'logfile': begin
						if ns ge 2 then begin
							midas.logfile = strjoin(s0[1:*],' ')
						endif
						end
					'default': begin
						if ns ge 3 then begin
							case s[1] of
								'version': begin
									midas.version = fix(s[2])
									end
								'detectors': begin
									midas.detectors = fix(s[2])
									end
								'spectra': begin
									midas.spectra = fix(s[2])
									end
								'da': begin
									midas.DA = fix(s[2])
									end
								'x': begin
									midas.image.x = fix(s[2])
									end
								'y': begin
									midas.image.y = fix(s[2])
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
	midas.conf = midas_conf
	error = 0
	return, midas
	
bad_find:	
	warning,'midas_defaults',['Failed to find midas config file: "' + midas_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'midas_defaults',['Failed to open midas config file: "' + midas_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
							
bad_cont:
	close_file, unit

	*midas.epics.pic = [':HVSUM_MON',':scaler',':CURRENT_MONITOR']
	midas.epics.npic = 3
	midas.epics.energy = ':ENERGY_MON'
	return, midas
	
bad_read:	
	warning,'midas_defaults','bad read from midas config file: "' + midas_conf + '".'
	close_file, unit
	return, midas
end
