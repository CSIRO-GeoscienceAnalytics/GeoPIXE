function lmgr2, demo=demo, embedded=embedded, runtime=runtime, student=student, $
		trial=trial, vm=vm, lmhostid=lmhostid, site_notice=site_notice, install_num=install_num, $
		free_install=free_install

; Replacement for LMGR for IDL 8.6 onwards.
; Cannot take over from LMGR() as LMGR is built-in.
	
COMPILE_OPT STRICTARR

	if n_elements(vm) lt 1 then vm = 0
	if n_elements(runtime) lt 1 then runtime = 0
	if n_elements(free_install) lt 1 then free_install = 0
	try_spawn = 0
	if arg_present(install_num) or arg_present(lmhostid) or arg_present(site_notice) then try_spawn=1
	r = 0
	install_num = '?'
	site_notice = 'Unknown'
	lmhostid = ''

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		warning,'lmgr2',['','IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!Error_state.msg], /error
		MESSAGE, /RESET
		return, r
	endif

	if vm then r=lmgr(/vm)
	if runtime then r=lmgr(/runtime)

	if try_spawn then begin
		case !version.os_family of
			'Windows': begin
				spawn,'ipconfig /all', result, err, /noshell, /hide
				if err[0] ne '' then goto, bad_addr
				sep = ' 	.:'
				strip = '-'
				key = 'Physical'
				look = 'last'
				end
			'unix': begin
				com = '/sbin/ifconfig'
				args = '-a'		
				com = [com, args]
				spawn, [com], result,err, /noshell
				if err[0] ne '' then begin
					com = '/usr/sbin/ifconfig'
					com = [com, args]
					spawn, [com], result,err, /noshell
					if err[0] ne '' then goto, bad_addr
				endif
				sep = ' 	'
				strip = ':'
				key = ['HWaddr','ether']
				look = 'next'
				end
			'MacOS': begin
				com = '/sbin/ifconfig'
				args = '-a'		
				com = [com, args]
				spawn, [com], result,err, /noshell
				if err[0] ne '' then goto, bad_addr
				sep = ' 	'
				strip = ':'
				key = ['HWaddr','ether']
				look = 'next'
				end
				
			else: begin
				warning,'lmgr2','un-supported operating system ('+ !version.os_family +')'
				return, r
				end
		endcase
	
		n = n_elements(result)
		first = 1
		for i=0,n-1 do begin
			s = strsplit( result[i], sep, /extract, count=ns)
			if (ns eq 0) or (s[0] eq '') then continue
			j = 0
			repeat begin
				q = where( s eq key[j], nq)			; look for keys on line
				j++
			endrep until (nq gt 0) or (j ge n_elements(key)) 
			if nq ge 1 then begin
;				print,'Found: '+result[i]
				case look of						; where to find the MAC address on line
					'next': begin
						k = q[0]+1
						end
					'last': begin
						k = ns-1
						end
				endcase
				if first then begin
					lmhostid = s[k]
					first = 0
				endif else begin
					lmhostid = lmhostid + ' ' + s[k]
				endelse
			endif
		endfor
	
		if lmhostid ne '' then lmhostid = '"' + strlowcase(lmhostid) + '"'
		lmhostid = strip_char( lmhostid, strip)
;		print, lmhostid
	endif
	goto, next

bad_addr:
;	warning,'lmgr2',['Error using O/S "spawn".','Failed to run "/sbin/ifconfig" to find Physical Addresses.','',err]
	
next:
	if try_spawn and (strmid(!version.release,0,1) eq '8') then begin
		case !version.os of
			'Win32': begin
				code = expand_path('<IDL_DIR>\' + 'license_utils\' + '<IDL_BIN_DIRNAME>') + '\'
				activate = '"' + code + 'activate.exe"'
				args = ' "-l"'
		
				child = activate + args
				spawn, child, result,err,exit_status=flag,count=count, /noshell, /hide
				if err[0] ne '' then goto, bad
				end
			'linux': begin
				code = expand_path('<IDL_DIR>' + '/license_utils/' + '<IDL_BIN_DIRNAME>') + '/'
				activate = code + 'activate'
				args = '-l'
		
				com = [activate, args]
				spawn, [com], result,err,exit_status=flag,count=count, pid=pid, /noshell
				if err[0] ne '' then goto, bad
				end
			else: begin
				warning,'lmgr2','un-supported operating system ('+ !version.os +')'
				return, r
				end
		endcase
		if (err eq 0) and (count gt 0) then begin
			for i=0,count-1 do begin
				s = strsplit( strtrim(result[i],2), ':" 	,', /extract)
				if n_eLements(s) lt 2 then continue
				if s[0] eq 'vendor' then begin
					s2 = strsplit( s[1], ':', /extract)
					if n_eLements(s2) lt 1 then continue
					install_num = [install_num,s2[0]]
				endif
				if s[0] eq 'notice' then begin
					site_notice = [site_notice,strjoin(s[1:*],' ')]
				endif
			endfor
			if n_elements(install_num) gt 1 then install_num=install_num[1]
			if n_elements(site_notice) gt 1 then site_notice=site_notice[1]
		endif
	endif
;	help,lmhostid,site_notice,install_num

	goto, done

bad:
;	warning,'lmgr2',['Error using O/S "spawn".','Failed to run "activate -l" to find Installation Number.', $
;			'Licensing must fallback to using Physical Address only.','',err]

;	This was considered for extending licensing to node name. But it was decided this was
;	too "open". Instead, we'll use timed "demo" mode in license file.
;	Hence, this is not used now.

done:
	if free_install and (install_num eq '?') then begin			; if not found under 8.6+ use node name
		tuser = get_login_info()
		install_num = tuser.machine_name
	endif
	return, r
end
