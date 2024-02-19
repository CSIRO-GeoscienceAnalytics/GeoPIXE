function iXRF_defaults, error=error, source=source, conf=file, reset=reset, any=any

; Read iXRF defaults from selected conf file in .geopixe home directory.
; This is a draft, based on Maia, not completed, and not called as yet (see iXRF device source).
; Edit to provide config parameters for Midas device.
;
; If none found, set some defaults (at the bottom) for XFM beamline data.
; Pass 'file' to specify the conf file to use (e.g. as in blog clients).

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_iXRF_default, iXRF_conf
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
	if n_elements(source) lt 1 then source='?'
	if n_elements(reset) lt 1 then reset=0
	if n_elements(any) lt 1 then any=0
	if reset then iXRF_conf=''
	
	now = fix_path(file_expand_path('.'))
	error = 1
	iXRF = {	version:	0, $		; default software version
				detectors:	384, $		; default number of detectors
				spectra:	12, $		; default number of spectra groups
				DA:			32, $		; default number of DA images
				image: { x:	1000, $		; default DA image X size
						 y:	1000}, $	; default DA image Y size
				iXRF: {	ip:	'', $		; iXRF socket IP
						name: '', $		; unique name of Maia system
						port: 0L, $		; Maia socket port
						enable: 0}, $	; Maia socket enable
				epics: { pic: ptr_new(/allocate_heap), $	; pointer to Epics IC PV string kernels
						npic: 0, $		; number of PV kernels to look for
						energy: ''}, $	; energy PV string kernel
				blog: {	ip:	'', $		; Blog socket IP
						port: 0L, $		; Blog socket port
						enable: 0}, $	; Blog socket enable
				logfile: 'iXRF_log.csv', $		; default log file name in s/w dir
				conf:	'' }			; config file name
						
	if n_elements(file) ne 0 then iXRF_conf=file
	if n_elements(iXRF_conf) eq 0 then goto, read_conf
	if iXRF_conf[0] eq '' then goto, read_conf
	goto, cont
	
read_conf:
	home_dir = geopixe_environment()
	conf_files = file_search( home_dir + '*.iXRF.conf')
	if conf_files[0] eq '' then begin
		if any then begin
			on_ioerror, do_warn
			print,'Copy "template.iXRF.conf" to home dir ...'
			mdef = 'iXRF' + path_sep() + 'template.iXRF.conf'
			file_copy, mdef, home_dir, /require_dir
			on_ioerror, null
			conf_files = file_search( home_dir + '*.iXRF.conf')
			if conf_files[0] ne '' then goto, check
		endif
do_warn:
		warning,'iXRF_defaults',['No iXRF conf files found in user home dir:', $
					home_dir,'Abort.','','You can make a config file (extension ".iXRF.conf") ', $
						'based on the template file "template.iXRF.conf" in the "geopixe/iXRF" directory. ', $
						'Create additional .iXRF.conf files to manage connection to multiple HYMODs.']
		error = 1
		return, 0
	endif

check:
	if any or (n_elements(conf_files) eq 1) then begin
		iXRF_conf = conf_files[0]
		goto, cont
	endif
	
	sel = options_popup( 0L, drop=strip_path(conf_files), path=home_dir, title='Select iXRF conf file', error=err, $
				min_xsize=350, $
				help_drop='Select a iXRF config file from this list from your Home .geopixe directory. You can make a config file ' + $
						'based on the template file "template.iXRF.conf" in the "geopixe/iXRF" directory.', $
				help_default='Select a iXRF config file from your Home .geopixe directory. You can make a config file (extension ".iXRF.conf") ' + $
						'based on the template file "template.iXRF.conf" in the "geopixe/iXRF" directory. ' + $
						'Create additional .iXRF.conf files to manage connection to multiple iXRF/HYMODs.'		)
	if err then begin
		warning,'iXRF_defaults',['Error selecting iXRF conf files in user home dir:', $
					home_dir,'Abort.']
		error = 1
		return, 0
	endif
	iXRF_conf = conf_files[sel.drop[0]]

cont:
	on_ioerror, bad_open
	openr, unit, iXRF_conf, /get_lun
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
									iXRF.epics.npic = iXRF.epics.npic +1
									if iXRF.epics.npic eq 1 then begin
										*iXRF.epics.pic = s0[2]
									endif else begin
										*iXRF.epics.pic = [*iXRF.epics.pic, s0[2]]
									endelse
									end
								'energy_pv': begin
									iXRF.epics.energy = s0[2]
									end
								else:
							endcase
						endif
						end
					'iXRF': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									iXRF.iXRF.ip = s0[2]
									end
								'name': begin
									iXRF.iXRF.name = s0[2]
									end
								'port': begin
									iXRF.iXRF.port = long2(s[2])
									end
								'enable': begin
									iXRF.iXRF.enable = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'blog': begin
						if ns ge 3 then begin
							case s[1] of
								'ip': begin
									iXRF.blog.ip = s0[2]
									end
								'port': begin
									iXRF.blog.port = long2(s[2])
									end
								'enable': begin
									iXRF.blog.enable = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'logfile': begin
						if ns ge 2 then begin
							iXRF.logfile = strjoin(s0[1:*],' ')
						endif
						end
					'default': begin
						if ns ge 3 then begin
							case s[1] of
								'version': begin
									iXRF.version = fix(s[2])
									end
								'detectors': begin
									iXRF.detectors = fix(s[2])
									end
								'spectra': begin
									iXRF.spectra = fix(s[2])
									end
								'da': begin
									iXRF.DA = fix(s[2])
									end
								'x': begin
									iXRF.image.x = fix(s[2])
									end
								'y': begin
									iXRF.image.y = fix(s[2])
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
	iXRF.conf = iXRF_conf
	error = 0
	return, iXRF
	
bad_find:	
	warning,'iXRF_defaults',['Failed to find iXRF config file: "' + iXRF_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'iXRF_defaults',['Failed to open iXRF config file: "' + iXRF_conf + '",', $
							'from dir "' + now + '", called by "' + source + '".', $
							'Will use default settings.']
							
bad_cont:
	close_file, unit

	*iXRF.epics.pic = [':HVSUM_MON',':scaler',':CURRENT_MONITOR']
	iXRF.epics.npic = 3
	iXRF.epics.energy = ':ENERGY_MON'
	return, iXRF
	
bad_read:	
	warning,'iXRF_defaults','bad read from iXRF config file: "' + iXRF_conf + '".'
	close_file, unit
	return, iXRF
end
