pro idl_query_test

COMPILE_OPT STRICTARR

	r = lmgr(lmhostid=host, install_num=install, site_notice=site)
	if n_elements(install) lt 1 then install='not defined'

	type = 'IDL DE'
	if lmgr(/runtime) then type = 'Run-time'
	if lmgr(/vm) then type = 'Virtual Machine'
	if lmgr(/demo) then type = 'Timed Demo mode'
	if lmgr(/trial) then type = 'Trial mode'

	a = dialog_message([ $
					'Host ID = '+host, $
					'Installation = '+install, $
					'IDL Version = '+!version.release, $
					'Environment = '+type], /information)
	return
end
