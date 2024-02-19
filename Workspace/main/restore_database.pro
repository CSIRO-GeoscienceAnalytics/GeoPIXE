pro restore_database

; Restore 'geopixe2.sav' databases and arrays to common

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

@database_commons.def

	restore, geopixe_root+'geopixe2.sav'
	return
end
