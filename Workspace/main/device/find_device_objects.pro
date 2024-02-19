function find_device_objects, error=error, files=files, generic_file=gfile

; Search for all XXX_device__define.pro (or XXX_device__define.sav) files
; Do not return "BASE_DEVICE" or "GENERIC_DEVICE".
;
; This needs to find devices even if called from blog clients in "maia" sub-dir.

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'find_device_objects',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','While finding device Objects.'], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif
if n_elements(geopixe_root) lt 1 then startupp
error = 1

; This old path has problems because IDL refuses to permit certain changes to !path. Hence,
; we can't remove "interface" from the path. This may mean that the interface SAV files get 
; incorporated into the GeoPIXE SAV file, which is NOT what we what (these get loaded at runtime).
;
; path = fix_path(geopixe_root) + 'interface'
;
; Moving "interface" outside of the "main" source dir, into a different project dir ("geopixe") used
; only for runtime files, such as SAVs, permits "interface" to be excluded from the path. The "geopixe"
; Eclipse runtime project should NOT enable the option to update !path.
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
;
; Hence, this search assumes "interface" is a subdir under "geopixe", with "geopixe" at the same dir level
; as the "main" GeoPIXE source directory.

path = fix_path(file_dirname(geopixe_root)) + 'geopixe' + path_sep() + 'interface'

sav_list = find_file2(path + path_sep() + '*_device__define.sav')

; If not found, then pop up a dir level (e.g. when executed from Maia, DAQ SAV files in "maia" and "daq" subdirs).

if sav_list[0] eq '' then begin
	path2 = fix_path(file_dirname(file_dirname(geopixe_root))) + 'geopixe' + path_sep() + 'interface'
	print,'try new path = ',path2
	sav_list = find_file2(path2 + path_sep() + '*_device__define.sav')
	if sav_list[0] eq '' then warning,'find_device_objects',['Device object SAVs not found.','root='+geopixe_root, $
						'path='+path,'path2='+path2]
endif

if sav_list[0] ne '' then begin
	f2 = strip_path( strip_file_ext( sav_list))
	n = n_elements(f2)
	for i=0L,n-1 do begin
		n2 = strlen(f2[i])
		if n2 gt 8 then begin
			f2[i] = strmid( f2[i], 0, n2-8)
		endif else f2[i]=''
		if strupcase(f2[i]) eq 'BASE_DEVICE' then f2[i]=''
		if strupcase(f2[i]) eq 'GENERIC_DEVICE' then begin
			f2[i] = ''
			gfile = sav_list[i]
		endif
	endfor
endif else begin
	f2 = ''
	sav_list = ''
endelse

f = f2
files = sav_list
q = where( f ne '', nq)
if nq eq 0 then goto, bad_list
q2 = sort_unique(f[q],nq2)
if nq2 eq 0 then goto, bad_list
files = files[q[q2]]

error = 0
return, strupcase( f[q[q2]])

bad_list:
	warning,'find_device_objects2',['No device objects found,','with file-names: "geopixe/interface/XXX_device__define.sav".']
	goto, err
	
err:
	error = 1
	return, 0L
end