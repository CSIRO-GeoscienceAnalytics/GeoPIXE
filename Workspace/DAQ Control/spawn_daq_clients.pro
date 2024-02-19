pro spawn_daq_clients, units=luns, prefix=prefix, conf=conf

; Launch blog clients ...

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'spawn_daq_clients',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(prefix) eq 0 then prefix=''
if n_elements(conf) eq 0 then conf=''

print,'Spawn background processes ...'
luns = lonarr(5)
path = geopixe_root + 'daq' + slash()

case !version.os_family of
	'Windows': begin
		code = expand_path('<IDL_DIR>\' + 'bin\' + '<IDL_BIN_DIRNAME>') + '\'
		idl = '"' + code + 'idlrt.exe"'
		args = prefix + ' "' + conf + '"'

		sav = path + 'daq_client_activity.sav'
		child = idl + ' -rt="' + sav + '" -args ' + args
		spawn, child, unit=lun, /noshell, /hide
		luns[0] = lun

		sav = path + 'daq_client_et_spectra.sav'
		child = idl + ' -rt="' + sav + '" -args ' + args
		spawn, child, unit=lun, /noshell, /hide
		luns[1] = lun

;		sav = path + 'daq_client_da2.sav'
;;		sav = path + 'blog_file_da2.sav'
;		child = idl + ' -rt="' + sav + '" -args ' + args
;		spawn, child, unit=lun, /noshell, /hide
;		luns[2] = lun
		end
	'unix': begin
		code = expand_path('<IDL_DIR>') + '/bin/'
		idl = code + 'idl'
		args = [prefix, conf]

		sav = path + 'daq_client_activity.sav'
		com = [idl, '-rt=' + sav, '-args', args]
		spawn, [com,'&'], unit=lun, pid=pid, /noshell
		luns[0] = lun

		sav = path + 'daq_client_et_spectra.sav'
		com = [idl, '-rt=' + sav, '-args', args]
		spawn, [com,'&'], unit=lun, pid=pid, /noshell
		luns[1] = lun

;		sav = path + 'daq_client_da2.sav'
;;		sav = path + 'blog_file_da2.sav'
;		com = [idl, '-rt=' + sav, '-args', args]
;		spawn, [com,'&'], unit=lun, pid=pid, /noshell
;		luns[2] = lun
		end
	else: begin
		warning,'spawn_daq_clients',['Spawn not supported on this platform.','Could not spawn blog clients.']
		end
endcase

return
end
