function geopixe_defaults, error=error, source=source

; Read geopixe defaults from 'geopixe.conf' file.
; If none found, set some defaults (at the bottom) for XFM beamline data.
; Use 'source' to label where this function called for traceback.
;
; rsyslog elsewhere (scan_list, maia_launch) assume server same as KVS node.
; If not, then add "logging server" to 'geopixe.conf' and use 'prefs.logging.server'.
;
; Box shape will be saved to a KVS key and/or a shape file, depending on which are enabled.
; (see "set_kvs_box" in "image_routines")
;
; See "startupp" for more info on geopixe_root.

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_geopixe_defaults, geopixe_defaults_done
if n_elements(geopixe_root) lt 1 then startupp
if n_elements(geopixe_defaults_done) lt 1 then geopixe_defaults_done=0
if n_elements(source) lt 1 then source='?'

	error = 1
	geopixe = {	path: { data:		'', $		; default raw data path on local system
						analysis:	'', $		; default analysis path on local system
						config:		'', $		; default path for local GeoPIXE config/analysis files
						projects:	''}, $		; default path for local GeoPIXE projects
				default: { 	device:	'MAIA_DEVICE', $	; default device object name
							memory:	20.e+9 }, $	; default memory limit
				cluster: { 	type:	'cores', $	; cluster mode ('CWS' or 'cores')
						nodes:		4}, $		; cluster number of nodes
				CWS: {	ServiceURL: '', $		; CWS service URL (CWS deprecated)
						Name:		'', $		; CWS cluster name
						User:		'', $		; CWS username
						Password:	'', $		; CWS Password
						OSType:		'unix', $	; CWS cluster OS type
						NodeStr:	'', $		; CWS Node initialization string
						pLocalPath:	ptr_new(''), $		; CWS local path(s) root translation mapping
						pNodePath:	ptr_new('') }, $	; CWS matching remote cluster node path(s)
				kvs: {	enable:		0, $		; enable storage of Box, S.Pixel shapes in KVS on move
						prefix:		'', $		; prefix for KVS keys
						endpoint:	'', $		; KVS endpoint
						prefix2:	'', $		; prefix for KVS 2 keys 
						endpoint2:	''}, $		; KVS 2 endpoint
				shape: { enable:	0, $		; enable saving Box to a shape file
						path:		'', $		; shape path
						file:		''}, $		; shape file
				custom: { enable:	0, $		; enable custom extensions
						lab:		'', $		; custom lab name (e.g. "XFM")
						path:		'', $		; custom path
						file:		''}, $		; custom file
				logging: { server:	''}, $		; rsyslog error logging via MM libs and python
				startup: { spectrum: 1, $		; startup with spectrum window open (=1)
						select:		0, $		; startup wirh spectrum select window
						identify:	0, $		; startup with the line identification window
						clone:		0, $		; startup with a clone of image window
						regions:	0, $		; startup with image region window open (=1)
						fit:		0, $		; startup with Xray spectrum fit window open (=1)
						sort:		1}, $		; startup with sort window open (=1)
				origin: {	x:		0.0, $		; manual X origin (mm)
							y:		0.0, $		; manual Y origin (mm)
							auto:	1}, $		; normally, do not use this fixed manual origin
				test:				0, $		; test mode
				debug:				0, $		; debug mode
				slave:				0 $			; slave mode (child window in realtime)
				}
	
	home_dir = geopixe_environment()
	geopixe_conf = home_dir + 'geopixe.conf'
	root_dir = home_dir
	if file_test(geopixe_conf, /read) eq 0 then begin
		root_dir = geopixe_root
		geopixe_conf = root_dir + 'geopixe.conf'
		if file_test(geopixe_conf, /read) eq 0 then goto, bad_find
		
		on_ioerror, bad_copy
		print,'Copy "geopixe.conf" to home dir ...'
		file_copy, geopixe_conf, home_dir, /require_dir
		goto, cont
		
bad_copy:
		warning,'geopixe_defaults',['Failed to copy geopixe config file: "' + geopixe_conf + '",', $
						'to dir "' + home_dir +'"','','Check permissions and copy it manually.']
	endif

cont:	
	on_ioerror, bad_open
	openr, unit, geopixe_conf, /get_lun
	line = ''
	local = ''
	remote = ''
	
	on_ioerror, bad_read
	while NOT EOF(unit) do begin
		readf, unit, line
;		print,'geopixe_defaults: '+line
		i = locate('#', line)
		if i eq -1 then i=strlen(line)
		if i gt 0 then begin
			s0 = strsplit(strmid(line,0,i),' 	',/extract)
			s = strlowcase(s0)
			ns = n_elements(s)
			if ns ge 2 then begin
				if ns ge 3 then srest = strjoin(s0[2:*], ' ')
;				print,'geopixe_defaults: loop = '+s[0]
				case s[0] of
					'path': begin
						if ns ge 3 then begin
							case s[1] of
								'data': begin
									geopixe.path.data = srest
									end
								'analysis': begin
									geopixe.path.analysis = srest
									end
								'config': begin
									geopixe.path.config = srest
									end
								'projects': begin
									geopixe.path.projects = srest
									end
								else:
							endcase
						endif
						end
					'default': begin
						if ns ge 3 then begin
							case s[1] of
								'device': begin
									geopixe.default.device = strupcase(s[2])
									end
								'memory': begin
									geopixe.default.memory = float2(s[2])
									end
								else:
							endcase
						endif
						end
					'cluster': begin
						if ns ge 3 then begin
							case s[1] of
								'type': begin
									geopixe.cluster.type = s[2]
									end
								'nodes': begin
									geopixe.cluster.nodes = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'cws': begin
						if ns ge 3 then begin
							case s[1] of
								'serviceurl': begin
									geopixe.cws.ServiceURL = srest
									end
								'name': begin
									geopixe.cws.Name = srest
									end
								'user': begin
									geopixe.cws.User = s0[2]
									end
								'password': begin
									geopixe.cws.Password = s0[2]
									end
								'ostype': begin
									geopixe.cws.OSType = srest
									end
								'nodestr': begin
									geopixe.cws.NodeStr = srest
									end
								'localpath': begin
									*geopixe.cws.pLocalPath = [ *geopixe.cws.pLocalPath, srest]
									end
								'nodepath': begin
									*geopixe.cws.pNodePath = [ *geopixe.cws.pNodePath, srest]
									end
								else:
							endcase
						endif
						end
					'kvs': begin
						if ns ge 3 then begin
							case s[1] of
								'enable': begin
									geopixe.kvs.enable = fix2(s0[2])
									end
								'prefix': begin
									if geopixe.kvs.prefix ne '' then begin
										geopixe.kvs.prefix2 = s0[2]
									endif else begin
										geopixe.kvs.prefix = s0[2]
									endelse
									end
								'endpoint': begin
									if geopixe.kvs.endpoint ne '' then begin
										geopixe.kvs.endpoint2 = s0[2]
									endif else begin
										geopixe.kvs.endpoint = s0[2]
										if geopixe.logging.server eq '' then begin
											st = strsplit(geopixe.kvs.endpoint,':/',/extract)
											if n_elements(st) ge 2 then geopixe.logging.server = st[1]
										endif
									endelse
									end
								else:
							endcase
						endif
						end
					'shape': begin
						if ns ge 3 then begin
							case s[1] of
								'enable': begin
									geopixe.shape.enable = fix2(s0[2])
									end
								'path': begin
									geopixe.shape.path = srest
									end
								'file': begin
									geopixe.shape.file = s0[2]
									end
								else:
							endcase
						endif
						end
					'custom': begin
						if ns ge 3 then begin
							case s[1] of
								'enable': begin
									geopixe.custom.enable = fix2(s0[2])
									end
								'lab': begin
									geopixe.custom.lab = s0[2]
									end
								'path': begin
									geopixe.custom.path = srest
									end
								'file': begin
									geopixe.custom.file = s0[2]
									end
								else:
							endcase
						endif
						end
					'logging': begin
						if ns ge 3 then begin
							case s[1] of
								'server': begin
									geopixe.logging.server = s0[2]
									end
								else:
							endcase
						endif
						end
					'origin': begin
						if ns ge 3 then begin
							case s[1] of
								'x': begin
									geopixe.origin.x = fix2(s[2])
									end
								'y': begin
									geopixe.origin.y = fix2(s[2])
									end
								'auto': begin
									geopixe.origin.auto = fix2(s[2])
									end
								else:
							endcase
						endif
						end
					'startup': begin
						if ns ge 3 then begin
							case s[1] of
								'spectrum': begin
									geopixe.startup.spectrum = fix2(s[2])
									end
								'regions': begin
									geopixe.startup.regions = fix2(s[2])
									end
								'fit': begin
									geopixe.startup.fit = fix2(s[2])
									end
								'sort': begin
									geopixe.startup.sort = fix2(s[2])
									end
								'image_clone': begin
									geopixe.startup.clone = fix2(s[2])
									end
								'identify': begin
									geopixe.startup.identify = fix2(s[2])
									end
								'select': begin
									geopixe.startup.select = fix2(s[2])
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
;	print, 'geopixe_defaults: CWS tidy ...'
	if n_elements( *geopixe.cws.pLocalPath) gt 1 then *geopixe.cws.pLocalPath = (*geopixe.cws.pLocalPath)[1:*]
	if n_elements( *geopixe.cws.pNodePath) gt 1 then *geopixe.cws.pNodePath = (*geopixe.cws.pNodePath)[1:*]
	
;	print, 'geopixe_defaults: Close ...'
	close_file, unit
	error = 0
	
; 	Test for valid paths ...
	if geopixe_defaults_done then goto, fin
	
	test = file_test( geopixe.path.data, /dir)
	if (test eq 0) or (geopixe.path.data eq '') then begin
		warning,'geopixe_defaults',['Failed to find the default geopixe "data" path.', $
						'Or, "path data" not setup in "geopixe.conf" file.','', $
						'Setup paths in your ".geopixe/geopixe.conf" file.']
	endif
	test = file_test( geopixe.path.config, /dir)
	if (test eq 0) or (geopixe.path.config eq '') then begin
		warning,'geopixe_defaults',['Failed to find the default geopixe "config" path.', $
			'Or, "path config" not setup in "geopixe.conf" file.','', $
			'Setup a "path config" in your ".geopixe/geopixe.conf" file, ', $
			'which is also used to store "detector" and "filter" ', $
			'definition dirs.']
	endif
fin:
	geopixe_defaults_done = 1
	return, geopixe
	
bad_find:	
	warning,'geopixe_defaults',['Failed to find geopixe config file: "' + geopixe_conf + '",', $
							'from dir "' + geopixe_environment() + ' or ' + root_dir + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'geopixe_defaults',['Failed to open geopixe config file: "' + geopixe_conf + '",', $
							'from dir "' + root_dir + '", called by "' + source + '".', $
							'Will use default settings.']
	goto, bad_cont
							
bad_cont:
	close_file, unit
	return, geopixe
	
bad_read:	
	warning,'geopixe_defaults','bad read from geopixe config file: "' + geopixe_conf + '".'
	close_file, unit
	return, geopixe
end
