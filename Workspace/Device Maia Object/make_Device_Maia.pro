pro make_Device_Maia

	COMPILE_OPT STRICTARR

	project_name = "Device Maia"
	sav_name = "maia_device__define"

; Compile a Device Object 
;
; Just compile the device directory.
; Do NOT 'resolve_all', as missing routines are assumed to be supplied
; once "GeoPIXE.sav" has been restored.
;
; Save to the "interface" directory.
;
; Assumes current working directory is "GeoPIXE".

	path = '../'+project_name+'/*.pro'
	print,'Compile all routines: '+file_expand_path(path)

	file = file_search(path)
	if file[0] eq '' then begin
		a = dialog_message(['Project: '+project_name, 'No files found to compile.', $
						'Check make PRO.'], /error)
		return
	endif

	routine = strip_file_ext(strip_path(file))
	local = replace( ' ', '_', project_name)
	q = where( routine ne 'make_'+local, nq)
	if nq eq 0 then begin
		a = dialog_message(['Project: '+project_name, 'No files found to compile.', $
						'Check make PRO.'], /error)
		return
	endif
	routine = routine[q]

;	This fails for 'maia_device__define' because it finds the SAV
;	file of the same name elsewhere in the "interface" subdir of GeoPIXE.
;	Need to control !path directly, or move "interface" aside.

	for i=0,n_elements(routine)-1 do begin
		resolve_routine, routine[i], /compile_full_file, /either
	endfor
	
	save, /routines, file='../geopixe/interface/'+sav_name+'.sav'
	return
end
