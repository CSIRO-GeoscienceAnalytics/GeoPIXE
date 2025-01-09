pro idl_query

idl_query_geopixe
return
end

pro idl_query_cdecl

idl_query_geopixe
return
end

pro idl_query_underscore

idl_query_geopixe
return
end

pro idl_query_cdecl_underscore

idl_query_geopixe
return
end

pro idl_query_cdecl_2underscore

idl_query_geopixe
return
end

;--------------------------------------------------------------------------

pro idl_query_geopixe

COMPILE_OPT STRICTARR
common c_debug_warnings, enable_warning_popup
common c_working_dir, geopixe_root
enable_warning_popup = 1

ErrorNo = 0
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
Catch, ErrorNo
if (ErrorNo ne 0) then begin
	Catch, /cancel
	on_error, 1
	warning,'IDL_Query',['','IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!Error_state.msg], /error
	MESSAGE, /RESET
	return
endif

	file = 'GeoPIXE.sav'
	if file_test(file) then geopixe_root = file_dirname(file_expand_path(file),/mark_dir)
	file = '../GeoPIXE.sav'
	if file_test(file) then geopixe_root = file_dirname(file_expand_path(file),/mark_dir)
	file = '../geopixe/GeoPIXE.sav'
	if file_test(file) then geopixe_root = file_dirname(file_expand_path(file),/mark_dir)
	if geopixe_root eq '' then begin
		r = dialog_message(['IDL Query','Failed to locate geopixe_root, the "geopixe" runtime dir.'])
	endif else begin
		if file_basename(geopixe_root) ne 'geopixe' then begin
			r = dialog_message(['IDL Query','"GeoPIXE.sav" located to set "geopixe_root". ', '', $
					'However, no "geopixe" runtime dir was found.','', $
					'If the "geopixe" runtime dir has been renamed, it may give problems with', $
					'some background process execution.'])
		endif
	endelse
	print, 'geopixe_root: ',geopixe_root

;	lmgr functrion has been gutted. 'lmgr2' attempts to replace some functions.

	if float(!version.release) ge 8.6 then begin
		type = '?'
		r = lmgr2(lmhostid=host, install_num=install, site_notice=site)		; 8.6 obsolete host, install, site will crash out here!

		if lmgr2(/runtime) then type = 'Run-time'
		if lmgr2(/vm) then type = 'Virtual Machine'
	endif else begin
		type = 'IDL DE'
		r = lmgr(lmhostid=host, install_num=install, site_notice=site)

		if lmgr(/runtime) then type = 'Run-time'
		if lmgr(/vm) then type = 'Virtual Machine'
		if lmgr(/demo) then type = 'Timed Demo mode'	
		if lmgr(/trial) then type = 'Trial mode'		
		if lmgr(/student) then type = 'Student mode'	
	endelse
	if n_elements(install) lt 1 then install='not defined'

	gver = '?'
	gok = 0
	lib = '?'
	libver = -3
	if file_test(geopixe_root+'GeoPIXE.sav') then begin
		gsav = 'found GeoPIXE sav'
		gok = 1
	endif else gsav = 'no GeoPIXE sav'
	
	Catch, ErrorNo5
	if (ErrorNo5 ne 0) then begin
		Catch, /cancel
		on_error, 1
;		warning,'IDL_Query5',['','IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!Error_state.msg], /error
		goto, cont0
	endif	
	lib = geopixe_library(version=libver)

cont0:
	Catch, ErrorNo4
	if (ErrorNo4 ne 0) then begin
		Catch, /cancel
		on_error, 1
;		warning,'IDL_Query4',['','IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!Error_state.msg], /error
		tlib = 'failed'
		goto, cont1
	endif
	err = call_external( lib, geolib_name( 'init_maia_32'), cdecl=geolib_cdecl() )
	tlib = 'success'
	
cont1:
	Catch, ErrorNo2
	if (ErrorNo2 ne 0) then begin
		Catch, /cancel
		on_error, 1
;		warning,'IDL_Query2',['','IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!Error_state.msg], /error
		goto, cont4
	endif	
	if gok then begin
		restore, geopixe_root+'GeoPIXE.sav'
		gsav = 'restored "GeoPIXE.sav" OK'
		gver = geopixe_version()
	endif
	
cont4:
	Catch, ErrorNo5
	if (ErrorNo5 ne 0) then begin
		Catch, /cancel
		on_error, 1
;		warning,'IDL_Query4',['','IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!Error_state.msg], /error
		pver = 'not found'
		goto, cont
	endif
	pver = python_version()
	
cont:
	on_ioerror, NULL
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
;		warning,'IDL_Query6',['','IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!Error_state.msg], /error
		goto, cont3
	endif

cont3:
	help, !version, output=splat, /struct
	tuser = get_login_info()
	
	s = [ $
					'Host ID = '+host, $
					'Installation = '+install, $
					'User ID = '+tuser.user_name, $
					'Host node = '+tuser.machine_name, '', $
					'IDL Version = '+!version.release, $
					'Environment = '+type, $
					'Big endian = '+ (big_endian()?'yes':'no'), '', $
					'Platform = '+splat, '', $							; !version.os_family
					'GeoPIXE root = '+geopixe_root, $
					'GeoPIXE SAV = '+gsav, $
					'GeoPIXE version = '+gver, $
					'Library version = '+string(libver), $
					'Library file = '+lib, $
					'Test library access = '+tlib, $
					'Python version = '+pver ]

	warning,'IDL Query',s, /info
	return
end
