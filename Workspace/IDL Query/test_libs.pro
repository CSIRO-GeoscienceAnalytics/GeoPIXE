pro test_libs

	Catch, ErrorNo99
	if (ErrorNo99 ne 0) then begin
		Catch, /cancel
		on_error, 1
		libver = -3
		lib = '?'
		goto, done
	endif	
	lib = geopixe_library(version=libver)

cont0:
	Catch, ErrorNo0
	if (ErrorNo0 ne 0) then begin
		Catch, /cancel
		on_error, 1
		tlib = 'failed 0'
		goto, cont1
	endif
	print,' Test:  no-pre, double-post underscore, cdecl=1'
	err = call_external( lib, 'init_maia_32__', /cdecl )
	tlib = 'success 0 -  no-pre, double-post underscore, cdecl=1'
	goto, done

cont1:
	Catch, ErrorNo1
	if (ErrorNo1 ne 0) then begin
		Catch, /cancel
		on_error, 1
		tlib = 'failed 1'
		goto, cont2
	endif
	print,' Test:  no-pre, single-post underscore, cdecl=1'
	err = call_external( lib, 'init_maia_32_', /cdecl )
	tlib = 'success 1 -  no-pre, single-post underscore, cdecl=1'
	goto, done

cont2:
	Catch, ErrorNo2
	if (ErrorNo2 ne 0) then begin
		Catch, /cancel
		on_error, 1
		tlib = 'failed 2'
		goto, cont3
	endif
	print,' Test:  no-pre, double-post underscore, cdecl=0'
	err = call_external( lib, 'init_maia_32__', cdecl=0 )
	tlib = 'success 2 -  no-pre, double-post underscore, cdecl=0'
	goto, done

cont3:
	Catch, ErrorNo3
	if (ErrorNo3 ne 0) then begin
		Catch, /cancel
		on_error, 1
		tlib = 'failed 3'
		goto, cont4
	endif
	print,' Test:  no-pre, single-post underscore, cdecl=0'
	err = call_external( lib, 'init_maia_32_', cdecl=0 )
	tlib = 'success 3 -  no-pre, single-post underscore, cdecl=0'
	goto, done

cont4:
	Catch, ErrorNo4
	if (ErrorNo4 ne 0) then begin
		Catch, /cancel
		on_error, 1
		tlib = 'failed 4'
		goto, cont5
	endif
	print,' Test:  single-pre, single-post underscore, cdecl=1'
	err = call_external( lib, '_init_maia_32_', /cdecl )
	tlib = 'success 4 -  single-pre, single-post underscore, cdecl=1'
	goto, done

cont5:
	Catch, ErrorNo5
	if (ErrorNo5 ne 0) then begin
		Catch, /cancel
		on_error, 1
		tlib = 'failed 5'
		goto, done
	endif
	print,' Test:  single-pre, double-post underscore, cdecl=1'
	err = call_external( lib, '_init_maia_32__', /cdecl )
	tlib = 'success 5 -  single-pre, double-post underscore, cdecl=1'
	goto, done

done:
	warning,'Lib_test',[ $
					'Library version = '+str_tidy(libver), $
					'Library file = '+lib, $
					'Test library access = '+tlib ], /info
	return
end
