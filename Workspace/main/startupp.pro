pro startupp, error=error, colours=colours, database=database, maia=maia, devices=devices, daq=daq

; Set-up various things/ load savs files for GeoPIXE environment
;
;	/colours	load the standard II colour tables and 'spec_colour' plot line colours
;	/error		enable catch on error pop-ups, else stop for debugging
;	/devices	load all interface/*_device__define.sav device objects
;	/database	load GeoPIXE database
;	/maia		load Maia_Control.sav for Maia detector background processes.
;	/daq		load DAQ_Control.sav for DAQ36 detector background processes.
;
; Notes on GeoPIXE organization and assumptions
;----------------------------------------------
; 'geopixe_root' will return the working doirectory containing the main GeoPIXE source (image.pro) during
; debug, and the directory of the SAV, for any programs run from SAV. For GeoPIXE (GeoPIXE.sav) this will
; be the directory containing GeoPIXE.sav, which by convention we assume is "geopixe", the runtime dir.
; For any background processes (e.g. Maia, DAQ), this will be the "maia" or "daq" subdirs of the "geopixe"
; runtime dir.
;
; Source organization:
; 	workspace					parent dir ("workspace" in source dir for debug)
;		geopixe					runtime dir
;			interface			device object SAVs
;			plugins				plugin SAVs
;			wizard				Wizard SAVs
;			maia				Maia detector background process SAVs
;			daq					DAQ interface background process SAVs
;		main					main GeoPIXE source dir
;
; Release (compiled) runtime organization:
; 	geopixe						main GeoPIXE release runtime dir
;		interface
;		plugins					plugin SAVs
;		wizard					Wizard SAVs
;		maia					Maia detector background process SAVs
;		daq						DAQ interface background process SAVs

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_working_dir2, geopixe_database_path
common c_geopixe_adcs, geopixe_max_adcs
common c_errors_1, catch_errors_on
common c_debug_warnings, enable_warning_popup
if n_elements(error) eq 1 then catch_errors_on = error
if n_elements(catch_errors_on) eq 0 then catch_errors_on = 1
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
common c_debug_linux_bug, first						;@8-18
if n_elements(first) lt 1 then first=3				;@8-18

if n_elements(colours) lt 1 then colours=0
if n_elements(database) lt 1 then database=0
if n_elements(maia) lt 1 then maia=0
if n_elements(devices) lt 1 then devices=0
if n_elements(daq) lt 1 then daq=0

if !version.os_family eq 'Windows' then device, decomposed=0
if !version.os_family eq 'unix' then device, true_color=24, decomposed=0
if !version.os_family eq 'MacOS' then device, decomposed=0

	geopixe_max_adcs = 384			; 32
	x = define()					; define commons for cal poly and fits lengths
	
; 'geopixe_root' is defined as the base directory for runtime files ("geopixe"), such
; as database, libraries, data files and the subdirs containing the routine SAV files 
; (interface, plugins, wizards, maia, daq).

;	geopixe_root = fix_path(file_expand_path('.'))			; old definition

; This requires the runtime dir to be named "geopixe". This will be found if current dir
; is "geopixe" or a dir at same level (e.g. source "main"). It will also find "geopixe" from
; a subdir (e.g. "wizard" or "maia" within runtime "geopixe").

	geopixe_root = ''
	if file_test('../geopixe', /dir) then begin
		geopixe_root = fix_path(file_expand_path('../geopixe'))
	endif else if file_test('../../geopixe', /dir) then begin
		geopixe_root = fix_path(file_expand_path('../../geopixe'))
	endif else begin

;		No "geopixe" runtime dir found (did it get renamed?), so find "GeoPIXE.sav"

		file = 'GeoPIXE.sav'
		if file_test(file) then geopixe_root = extract_path(file_expand_path(file))
		file = '../GeoPIXE.sav'
		if file_test(file) then geopixe_root = extract_path(file_expand_path(file))
		file = '../geopixe/GeoPIXE.sav'
		if file_test(file) then geopixe_root = extract_path(file_expand_path(file))
		if geopixe_root eq '' then begin
			warning,'startupp','Failed to locate geopixe_root, the "geopixe" runtime dir.'
		endif else begin
			warning,'startupp',['"GeoPIXE.sav" located to set "geopixe_root". ', $
					'However, no "geopixe" runtime dir was found.','', $
					'If the "geopixe" runtime dir has been renamed, it may give problems with', $
					'some background process execution.']
		endelse
	endelse

; Find the subdir for database source data files (not the output database SAV file
; "geopixe2.sav", which is in the runtime "geopixe" dir).

	if file_test('../main/database', /dir) then begin
		t = file_expand_path('../main/database')
		geopixe_database_path = fix_path(t)
	endif
	
	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'startupp',['IDL run-time error caught.', '', $
		   			'Error1:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
	       MESSAGE, /RESET

;	Due to an odd bug with very first pop-up, we try again ... 		;@8-18
;	This seems to only have happened on BNL chinook Linux blade.

			if first gt 0 then begin								;@8-18
loop:
				first = (first-1) >0
				Catch, ErrorNo
				if (ErrorNo ne 0) then begin
					Catch, /cancel
					on_error, 1
					help, calls = s
					n = n_elements(s)
					c = 'Call stack: '
					if n gt 2 then c = [c, s[1:n-2]]
					warning,'startupp',['IDL run-time error caught.', '', $
				   			'Error2:  '+strtrim(!error_state.name,2), $
							!Error_state.msg,'',c], /error
					MESSAGE, /RESET
					if first gt 0 then goto, loop
					return
				endif
			endif else return
	    endif
	endif

	if maia then begin
		found = 0
		file = geopixe_root+'maia_control.sav'
		if file_test(file) eq 0 then begin
			warning,'startupp','Failed to restore "maia_control.sav". 1'
		endif else found = 1
		if found then restore, file

		found = 0
		file = geopixe_root+'interface' + path_sep() + 'maia_device__define.sav'
		if file_test(file) eq 0 then begin
			warning,'startupp','Failed to restore "interface/maia_device__define.sav".'
		endif else found = 1
		if found then restore, file
	endif
	if daq then begin
		found = 0
		file = geopixe_root+'daq_control.sav'
		if file_test(file) eq 0 then begin
			warning,'startupp','Failed to restore "daq_control.sav".'
		endif else found = 1
		if found then restore, file

		found = 0
		file = geopixe_root+'interface' + path_sep() + 'daq_device__define.sav'
		if file_test(file) eq 0 then begin
			warning,'startupp','Failed to restore "interface/daq_device__define.sav".'
		endif else found = 1
		if found then restore, file
	endif
	
	if colours then begin
		loadct,5, bottom=16, ncolors=100		; 100 colours for images
		load_spec_colours						; bottom 16 colours for spectra and plots
	endif
	if database then begin
		restore_database						; load geopixe2.sav databases
	endif
	if devices then begin
		define_devices
	endif
	return
end
