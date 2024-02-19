pro continuum_source_plugin, title=title

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Photons   (continuum)'			; return the menu title for this plugin
	return							
endif

return
end
