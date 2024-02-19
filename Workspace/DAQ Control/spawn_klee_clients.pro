pro spawn_klee_clients, n_detectors, units=luns, prefix=prefix, conf=conf

; Launch DAQ clients ...

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
       warning,'spawn_klee_clients',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(prefix) eq 0 then prefix=''
if n_elements(conf) eq 0 then conf=''

print,'Spawn background Klee processes ...'
print,'		prefix = ',prefix
print,'		conf = ',conf
luns = lonarr(2)
path = geopixe_root + 'daq' + slash()

case !version.os_family of
	'Windows': begin
		code = expand_path('<IDL_DIR>\' + 'bin\' + '<IDL_BIN_DIRNAME>') + '\'
		idl = '"' + code + 'idlrt.exe"'
		args = prefix + ' ' + strtrim(string(n_detectors),2) + ' "' + conf + '"'

		sav = path + 'daq_client_parameters.sav'
		child = idl + ' -rt="' + sav + '" -args ' + args
		spawn, child, unit=lun, /noshell, /hide
		luns[0] = lun

;		sav = path + 'daq_client_parameters_slow.sav'
;		child = idl + ' -rt="' + sav + '" -args ' + args
;		spawn, child, unit=lun, /noshell, /hide
;		luns[1] = lun
		end
	'unix': begin
		code = expand_path('<IDL_DIR>') + '/bin/'
		idl = code + 'idl'
;		args = prefix + ' ' + strtrim(string(n_detectors),2) + ' "' + conf + '"'
		args = [prefix, strtrim(string(n_detectors),2), conf]

		sav = path + 'daq_client_parameters.sav'
		com = [idl, '-rt=' + sav, '-args', args]
		spawn, [com,'&'], unit=lun, pid=pid, /noshell
		luns[0] = lun

;		sav = path + 'daq_client_parameters_slow.sav'
;		com = [idl, '-rt=' + sav, '-args', args]
;		spawn, [com,'&'], unit=lun, pid=pid, /noshell
;		luns[1] = lun
		end
	else: begin
		warning,'spawn_klee_clients',['Spawn not supported on this platform.','Could not spawn DAQ clients.']
		end
endcase

return
end
