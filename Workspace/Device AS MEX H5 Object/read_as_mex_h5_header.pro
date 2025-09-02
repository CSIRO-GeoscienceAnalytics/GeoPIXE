function read_as_mex_h5_header, unit, error=error

; Draft attempt at reading HDF map scan 'header' data from MEX

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'read_as_mex_h5_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
common c_petra_1, min_x, step_x, min_y, pixel_x, pixel_y
common c_mex_1, reference_ts, smooth_ref_time
common c_maia_13, maia_dwell
error = 1

	stat = fstat( unit)
	
;	Data is organized as spectra (4096) for each of detector channel (4), for each sequence step (41496),
;	for each energy (1). The energy dimension is ignored for now.
;	The sequence steps have an 'x' and 'y' each, which do not necessarily follow a perfect rectangle.
;
;	Have ignored energy axis for now. Will need this for XANES maps. Will also need to test if E axis
;	is actually the fast axis.
;
;	It is assumed below that the various parameters are not necessarily sampled at the same rate. Hence,
;	the various time-stamps ('_ts' vectors) may have different steps and lengths. The logic is to first
;	find the average step size in 'x', then determine an equi-spaced X grid for pixels based on this step
;	and the starting value. Then determine the effective pixel-x time-stamp for each pixel. This becomes
;	the 'reference_ts' (stored in common) that all other parameters are sampled to.
;
;	The main routines where all this is applied is 'read_as_mex_h5_header' and 'as_mex_h5_DEVICE::read_setup'.

	file_id = H5F_OPEN(stat.name)

	version = '0.0.0'									; can't read attribute below!
	use_x = 0
	use_y = 0
	dwell = 0.0
	common_dwell = 0.0
	sample = 'unknown'
	n_adcs = 1					; 8
	pv_names = ''
	cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_adcs)
	print,''
	print,'AS MEX H5 device: read_as_mex_h5_header ...'

	timebase = 1000L			; for ms?
	mp = 0
	error = 1

	nm = H5G_get_num_objs( file_id)
	name7 = strarr(nm)
	for j=0,nm-1 do begin
		name7[j] = H5G_get_obj_name_by_idx(file_id, j)
	endfor
	
	q = where( name7 eq 'abs_x', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "abs_x" dataset found.'
		goto, finish
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'abs_x')
		abs_x = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'abs_y', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "abs_y" dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'abs_y')
		abs_y = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'x', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "x" dataset found.'
		goto, finish
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'x')
		x = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'x_ts', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "x_ts" dataset found.'
		goto, finish
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'x_ts')
		x_ts = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'y', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "y" dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'y')
		y = H5D_read(rec_id)
		H5D_close, rec_id
	endelse
	nxy = n_elements(y)
	
	q = where( name7 eq 'y_ts', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "y_ts" dataset found.'
		goto, finish
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'y_ts')
		y_ts = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	sx = x - shift(x,1)									; step change in x
	sx[0] = sx[1]										; fix wrap
	sy = y - shift(y,1)									; step change in y
	sy[0] = sy[1]										; fix wrap
	rx = abs(sx)/(max(sx)-min(sx))						; relative step changes
	ry = abs(sy)/(max(sy)-min(sy))
	qx = where(rx gt 0.1*max(rx), nqx)					; where significant step changes occur
	qy = where(ry gt 0.1*max(ry), nqy)

;	NOTE: The interpolations below do not require that the other tables have the same
;	length as the reference 'reference_ts'.

	if nqy gt nqx then begin
		print,'	Fast axis = Y'
		step_y = mean( median( abs(sy[qy]),5))				; average step sizes

;		Need to check for clusters of X steps. These need to be treated together as a single move.

		q = where( qx eq (shift( qx,-1) -1), nq)			; pairs of moves together
		while nq gt 0 do begin
			sx[qx[q]] = sx[qx[q]] + sx[qx[q+1]]				; combine pairs
			sx[qx[q+1]] = 0.0
			qx[q+1] = -1
			q = where( (qx eq (shift( qx,-1) -1)) and (qx ge 0) and ((shift( qx,-1) -1) ge 0), nq)
		endwhile

		q = where(qx ge 0, nq)
		step_x = mean( median( abs(sx[qx[q]]),5))

		pixel_y = round( (y - min(y)) / step_y )			; find middle of equi-spaced Y pixels
		ny = max(pixel_y) + 1

;		Think about effective time-stamp for pixellated y, as a basis for
;		interpolating corresponding values in the other TS tables (e.g. x_ts, i0_ts).
				
		y_eff = float(pixel_y) * step_y + min(y)			; effective 'y' positions in pixels
		nabs_y = y_eff + abs_y[0] - y[0]					; effective absolute 'y'
	
		offset = lindgen(nxy)								; offset (integer) to make all visit once
															; works if ny > max(y)-min(y)
		if ny le (max(y) - min(y)) then begin
			offset = offset * (round((max(y) - min(y))/ny) > 1) 
		endif
	
		reference_ts = interpol( y_ts, y+offset, y_eff+offset)	; effective time-stamp for each pixel, based on 'y'

		if smooth_ref_time then begin						; smooth out errors/jitter in reference time-stamps
			reference_ts = smooth( reference_ts, 50)
		endif

		x_at_y = interpol( x, x_ts, reference_ts)			; effective 'x' at time of effective 'y' time-stamps
		nabs_x = x_at_y + abs_x[0] - x[0]					; effective absolute 'x'
	
		pixel_x = round( (x_at_y - min(x_at_y)) / step_x )	; effective middle of equi-spaced X pixels
		nx = max(pixel_x) + 1

	endif else begin	
		print,'	Fast axis = X'
		step_x = mean( median( abs(sx[qx]),5))				; average step sizes

;		Need to check for clusters of Y steps. These need to be treated together as a single move.

		q = where( qy eq (shift( qy,-1) -1), nq)			; pairs of moves together
		while nq gt 0 do begin
			sy[qy[q]] = sy[qy[q]] + sy[qy[q+1]]				; combine pairs
			sy[qy[q+1]] = 0.0
			qy[q+1] = -1
			q = where( (qy eq (shift( qy,-1) -1)) and (qy ge 0) and ((shift( qy,-1) -1) ge 0), nq)
		endwhile

		q = where(qy ge 0, nq)
		step_y = mean( median( abs(sy[qy[q]]),5))

		pixel_x = round( (x - min(x)) / step_x )			; find middle of equi-spaced X pixels
		nx = max(pixel_x) + 1

;		Think about effective time-stamp for pixellated x, as a reference TS for
;		interpolating corresponding values in the other TS tables (e.g. y_ts, i0_ts).
				
		x_eff = float(pixel_x) * step_x + min(x)			; effective 'x' positions in pixels
		nabs_x = x_eff + abs_x[0] - x[0]					; effective absolute 'x'

		offset = lindgen(nxy)								; offset (integer) to make all visit once
															; works if nx > max(x)-min(x)
		if nx le (max(x) - min(x)) then begin
			offset = offset * (round((max(x) - min(x))/nx) > 1) 
		endif
	
		reference_ts = interpol( x_ts, x+offset, x_eff+offset)	; effective time-stamp for each pixel, based on 'x'

		if smooth_ref_time then begin						; smooth out errors/jitter in reference time-stamps
			reference_ts = smooth( reference_ts, 50)
		endif

		y_at_x = interpol( y, y_ts, reference_ts)			; effective 'y' at time of effective 'x' time-stamps
		nabs_y = y_at_x + abs_y[0] - y[0]					; effective absolute 'y'
	
		pixel_y = round( (y_at_x - min(y_at_x)) / step_y )	; effective middle of equi-spaced Y pixels
		ny = max(pixel_y) + 1
	endelse
	print, '	Pixel counts X,Y =',nx,ny
	print, '	Effective X,Y step size =', step_x, step_y
	print, '	Smooth reference time-stamp = ', smooth_ref_time

;	Dwell time from steps in x_ts

	t = reference_ts - shift(reference_ts,1)
	t[0] = t[1]											; fix wrap
	dwell_array = t 									; dwell time (s)

	maia_dwell = fltarr(nx,ny)
	maia_dwell[pixel_x,pixel_y] = dwell_array * 1000.	; ms dwell
	
	h = histogram( maia_dwell *300./(max(maia_dwell)>0.001), /NaN, locations=tx)
	q2 = reverse(sort(h))
	q3 = where( tx[q2] ne 0.0, nq3)						; to avoid missed pixels with zero dwell
	common_dwell = 0.0
	if nq3 ne 0 then common_dwell = tx[q2[q3[0]]] *(max(maia_dwell)>0.001)/300.

;	Find energy, sample, instrument name

	q = where( name7 eq 'dcm_energy_ev', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "dcm_energy_ev" dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'dcm_energy_ev')
		energy = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

;	Find flux PVs

	q = where( name7 eq 'i0', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "i0" dataset found.'
		goto, finish
	endif else begin
		pv_names = 'i0'
	endelse
	q = where( name7 eq 'i1', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "i1" dataset found.'
	endif else begin
		pv_names = n_elements(pv_names) gt 0 ? [pv_names,'i1'] : 'i1'
	endelse
	q = where( name7 eq 'i2', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "i2" dataset found.'
	endif else begin
		pv_names = n_elements(pv_names) gt 0 ? [pv_names,'i2'] : 'i2'
	endelse

;	We don't have any embedded energy calibration so far

find_cals:
	for i=0,n_adcs-1 do begin
		cal[i].a = 0.0					; dslope[i]
		cal[i].b = 0.0					; doff[i]
		if cal[i].a gt 1.0e-10 then begin
			cal[i].on = 1
			cal[i].units = 'keV'
		endif
	endfor

	error = 0

;	Setting the nominal pixel dwell to the most common (dwell map) below. 
;
;	Missing from header info at the moment is (also see other possibles in 'update_header_info'):
;		cal					energy calibration
;		comment				comment string for this run
;		sample				sample name
;		IC_sensitivity		ion chamber preamp sensitivity (relative to 1.0 = nA/V). Is this "/scan/instrument/keithley1, keithley2, ..."

	mp = {	version:	version, $					; version (float)
;			timebase:	timebase, $					; timebase count?
;			headings:	headings, $				`	; "Detector" names
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			axisx:		nabs_x, $					; X absolute coords
			axisy:		nabs_y, $					; Y absolute coords
			dwell:		common_dwell, $				; mean dwell 
			energy:		energy[0], $				; beam energy
			sample:		sample, $					; sample name
			facility:	'AS', $
;			endstation:	endstation, $				; endstation
			endstation:	'MEX', $					; endstation
			cal:		cal, $						; energy calibrations
			pv_names:	pv_names $					; PV names
			}
	error = 0

finish:
	H5F_close, file_id
	return, mp
end
