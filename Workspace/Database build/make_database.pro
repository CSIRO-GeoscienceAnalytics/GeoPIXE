pro make_database, from, to

; Make the database common data and splash page.
; Make sure the dirs 'dat' and 'Ebel' are enabled (i.e. named correctly in main/database/)
; Note overlay text for the splash page is done in the "OnRealize_splash_draw" routine of 'splash'.

@database_commons.def

startupp
;warning,'make_database',['Make sure you enable the paths', $
;						'to "database\dat" and "database\Ebel" first.']

; Remember to add any database commons BOTH here AND in 'restore_database' (via "database_commons.def").

from = fix_path(from)
splash, picture24=from+'geopixe-profile.png', picture8=from+'geopixe-profile.gif', timeout=2.0

temp = get_xsort_threshold()
build_line_list
init_xray_lines
init_xsect
init_dedx
init_edge
init_excess
init_mass
init_mass_density
init_abundance
init_xsect_pige
init_hubbell
init_xrf_lines, /force

file = to+'geopixe2.sav'
save, /comm, /variables, file=file
print,'Database SAVED to file: '+file
return
end
