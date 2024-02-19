pro device_specific, obj,unit, xrange,yrange, first=first, n_guide,progress_file, error=error, $
			ecompress=ecompress, flux=flux, dead_fraction=dead_fraction, $
			charge=charge, suppress=suppress, ic=flux_ic, beam_energy=beam_energy, $
			x_coords=x_coords, y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units, $
			_ref_extra=extra

; Set-up device specific list-mode parameters, and define
; event structure to read.
; 
; First		first file in multi-file
; Suppress	suppress pop-ups for 2nd and subsequent sorts in batch mode
; 
; The following are passed to devices that may need them, via _extra mechanism:
; 	ystep		flags special Y-step scan mode (e.g. MPsys).
; 	

COMPILE_OPT STRICTARR
if n_elements(first) lt 1 then first = 0
if n_elements(suppress) lt 1 then suppress = 0
if n_elements(flux_ic) lt 1 then flux_ic = {mode:0, pv:'', val:0.0, unit:0.0, conversion:1., use_dwell:0, dwell:1.0}

progress_file = obj->multi_files()					; progress bar by file count

; Some read whole flux array here (APS), while others (NSLS) set it up here and
; set pixel values during read_buffer.

if n_elements(flux) lt 1 then flux=0.0
if n_elements(dead_fraction) lt 1 then dead_fraction=0.0
if n_elements(beam_energy) lt 1 then beam_energy=0.0
x_coord_units = ''
y_coord_units = ''
z_coord_units = ''
				
	error = obj->read_setup( unit, xrange,yrange, first=first, n_guide,progress_file, $
			ecompress=ecompress, flux=flux, dead_fraction=dead_fraction, $
			charge=charge, suppress=suppress, ic=flux_ic, beam_energy=beam_energy, $
			x_coords=x_coords, y_coords=y_coords, x_coord_units=x_coord_units, y_coord_units=y_coord_units, $
			_extra=extra)	
	return
end
