pro build_project, lun, from, to, dir, resolve_all=resolve_all

; Part of "builder" (see below)
; Generates the script commands to set !path, compile project routines, and save routines to SAV file.
;
; lun		logical unit for printf
; from		project source name (names using wild card)
; to		destination SAV file name suffix (or full name if 'dir' is "")
;			if 'to' is "", then use 'from' explicitly (after replacing blanks or "-" with "_").
; dir		destination subdir of "geopixe" runtime dir for SAV file
;			'dir' = "" means top level of 'geopixe' runtime dir.
  
	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_working_dir3, workspace_root
	if n_elements(resolve_all) eq 0 then resolve_all=0

	cd, current=now
	fp = file_search( workspace_root + from)
	nfp = n_elements(fp)
	if nfp gt 0 then begin
		for i=0,nfp-1 do begin
			fpd = file_basename(fp[i])
			fpd = replace('-','_',fpd)
			s = strsplit(fpd,' ',/extract)
			ns = n_elements(s)
			if to eq '' then begin
				fs = strlowcase( strjoin(s,'_')) + '.sav'
			endif else begin
				case ns of
					1: begin
						fs = to + '.sav'
						end
					2: begin
						fs = strlowcase( s[1]) + '_' + to + '.sav'
						end
					else: begin
						fs = strlowcase( strjoin(s[1:ns-2],'_')) + '_' + to + '.sav'
						end
				endcase
			endelse
			f = file_search( fp[i], '*.pro')
			nf = n_elements(f)
			if nf gt 0 then begin
				printf, lun, 'print,"------------------------------------------------------------------"'
				printf, lun, 'print,"Project: '+fp[i]+'"'
				printf, lun, '.full_reset_session'
				printf, lun, '!path = !path + ";' + expand_path('+'+now) + ';'+fp[i]+'"'
;				printf, lun, 's = strsplit(!path,";",/extract)'
;				printf, lun, 'for i=0,n_elements(s)-1 do print,s[i]'

				for j=0,nf-1 do begin
					printf, lun, '.compile "' + f[j] + '"'
				endfor
				if resolve_all then begin
					printf, lun, 'resolve_all, /continue'
				endif
				root = geopixe_root
				if dir ne '' then root = root + dir + path_sep()
				printf, lun, 'save, /routines, file="' + root + fs + '"'
			endif
		endfor
	endif
	return
end

;-------------------------------------------------------------------------------------------------------------

pro build_database, lun, from

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_working_dir3, workspace_root

	cd, current=now
	fp = file_search( workspace_root + from)
	nfp = n_elements(fp)
	if nfp gt 0 then begin
		printf, lun, 'print,"------------------------------------------------------------------"'
		printf, lun, 'print,"Project: Database build"'
		printf, lun, '.full_reset_session'
		printf, lun, '!path = !path + ";' + expand_path('+'+now) + ';'+fp[0]+'"'
;		printf, lun, 's = strsplit(!path,";",/extract)'
;		printf, lun, 'for i=0,n_elements(s)-1 do print,s[i]'

		printf, lun, 'make_database, ' + '"' + fp[0] + '", "' + geopixe_root + '"'
	endif
	return
end

;-------------------------------------------------------------------------------------------------------------

pro builder, options=options

; Build GeoPIXE SAV files selectively or in bulk.
;
; Builds a script "build.spro" with commands to perform the build of GeoPIXE components.
; The compiled routines are saved to a file ("build.spro") in the "geopixe" runtime directory,
; if builder is run by double clicking on 'builder.sav', or to "main" if run by name from IDLDE.
;
; To complete the build, the script needs to be run at the IDL command line using "@build.spro". First,
; use the "cd" command at the IDL prompt to set the working directory to where 'build.spro' is located.
; We use the extension ".spro" here as this cannot be compiled inadvertently as a normal procedure.
;
; Use options (string array) to select from the following:
;	all			build all (main, plugins, devices, wizards, maia, daq, ...)
;	plugin		build image, spectra and back plugins (to geopixe/plugins)
;	back		build back plugins (to geopixe/plugins)
;	spectrum	build spectra plugins (to geopixe/plugins)
;	image		build image plugins (to geopixe/plugins)
;	device		build device objects (to geopixe/interface)
;	wizard		build wizard plugins (to geopixe/wizard)
;	maia		build Maia Control components (to geopixe/maia)
;	daq			build DAQ Control components (to geopixe/daq)
;	main		build main GeoPIXE program and library (to geopixe)
;	browse		build all data browser projects (to geopixe)
;	misc		build miscellaneous tools, such as geopixe_index, idl_query, ... (to geopixe)
;	database	build GeoPIXE database from files (stored as 'common' in SAV file "geopixe2.sav") (to geopixe)
;
; If no 'options' is provided, you will be prompted for them.

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_working_dir3, workspace_root
	modes = ['all','misc','main','database','plugin','back','spectrum','image','device','wizard','maia','daq','browse','gui']
	enable = dictionary( modes, intarr(n_elements(modes)))
		
;................................................................................................
; This code fragment will appear at the start of most stand-alone programs
; that need to load "GeoPIXE.sav" as a library for support routines. 

	found = 0
	file = 'GeoPIXE.sav'						; current dir is the runtime dir
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'					; current dir in a subdir of runtime dir
		if file_test(file) eq 0 then begin
			file = '../geopixe/GeoPIXE.sav'		; current dir is another project dir
			if file_test(file) eq 0 then begin
				r = dialog_message(['GeoPIXE library not found.','Failed to restore "GeoPIXE.sav".'],/error)
			endif else found=1
		endif else found=1
	endif else found = 1
	if found then begin
		restore, file
		geopixe_root = extract_path(file_expand_path(file))
		workspace_root = file_dirname(geopixe_root, /mark_dir)
	endif else begin
		r = dialog_message(['builder','Failed to locate geopixe_root, the "geopixe" runtime dir.', '', $
				'Check that the runtime dir is called "geopixe",', 'and "GeoPIXE.sav" is located there.'],/error)
		return
	endelse

;................................................................................................

;	If 'options' strarr not provided, pop up a window to select classes of project ...

	if n_elements(options) eq 0 then begin
		check_help = 'Select all projects of type "'+modes+'".'
		check_help[0] = 'Select all projects in all classes.'
		check_help[1] = 'Select all miscellaneous projects.'
		check_help[2] = 'Select the main GeoPIXE program project, also used as a library.'
		check_help[3] = 'Select the GeoPIXE Database build project.'
		r = options_popup(title='Select classes of projects to compile', check=modes, help_check=check_help, error=err)
		if err then return

		foreach m, modes, index do begin
			enable[m] = r.check[index]
		endforeach
	endif else begin
		no = n_elements(options)
		if no ne 0 then begin
			foreach opt, options do begin
				q = where( strlowcase(opt) eq modes, nq)
				if nq ne 0 then enable[modes[q[0]]] = 1
			endforeach
		endif
	endelse

	cd, current=now
	print,'Build from workspace root: '+workspace_root
	print,'Create "build.spro" script: '+now+path_sep()+'build.spro'
	print,'To build using this script use command: "@build.spro" from IDL command line.'

	on_ioerror, bad_open
	openw, lun, 'build.spro', /get_lun
	on_ioerror, bad_write

	if enable['all'] or enable['plugin'] or enable['back'] then begin
		build_project, lun, 'Back * Plugin', 'back_plugin', 'plugins'
	endif
	if enable['all'] or enable['plugin'] or enable['spectrum'] then begin
		build_project, lun, 'Spectrum * Plugin', 'spectrum_plugin', 'plugins'
	endif
	if enable['all'] or enable['plugin'] or enable['image'] then begin
		build_project, lun, 'Image * Plugin', 'image_plugin', 'plugins'
	endif
	if enable['all'] or enable['plugin'] or enable['wizard'] then begin
		build_project, lun, 'Wizard *', '', 'wizard'
	endif
	if enable['all'] or enable['plugin'] or enable['gui'] then begin
		build_project, lun, 'GUI * Plugin', 'gui_plugin', 'plugins'
	endif

;	Some of these are standalone programs, that do not restore "GeoPIXE.sav",
;	so we use /resolve_all **only** for these.

	if enable['all'] or enable['main'] then begin
		build_project, lun, 'main', 'GeoPIXE', '', /resolve_all
	endif

	if enable['all'] or enable['misc'] then begin
		build_project, lun, 'GeoPIXE Index', '', ''
		build_project, lun, 'GeoPIXE parallel', '', ''
		build_project, lun, 'IDL Query', '', '', /resolve_all
		build_project, lun, 'GeoPIXE update', '', '', /resolve_all
		build_project, lun, 'Builder', '', '', /resolve_all
		build_project, lun, 'Scanning', '', '', /resolve_all
		build_project, lun, 'Sim PIXE_XRF', '', ''
	endif

	if enable['all'] or enable['database'] then begin
		build_database, lun, 'Database build'
	endif

	if enable['all'] or enable['maia'] then begin
		build_project, lun, 'Blog *', '', 'maia'
		build_project, lun, 'Maia *', '', 'maia'
	endif

	if enable['all'] or enable['daq'] then begin
		build_project, lun, 'DAQ *', '', 'daq'
	endif

	if enable['all'] or enable['browse'] then begin
		build_project, lun, 'Browse *', 'browser', ''
	endif

	if enable['all'] or enable['device'] then begin
		build_project, lun, 'Device *', 'device__define', 'interface'
	endif
	printf, lun, 'print,"All done."'
	close_file, lun

	warning,'builder',['Build script output written to file:,','"'+now+path_sep()+'build.spro".', $
			'','Use "@build.spro" at the IDL command line to perform the build, ' + $
			'after a "cd" to the directory: '+now+path_sep() ],/info	
	return

bad_open:
	warning,'builder','error opening "build.spro" file.'
	goto, done
bad_write:
	warning,'builder','error writing to "build.spro" file.'
	goto, done

done:
	printf, lun, 'print,"Error return."'
	close_file, lun
	return
end
