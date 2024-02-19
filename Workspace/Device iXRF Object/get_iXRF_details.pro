function get_iXRF_details, p, njson=njson, error=err

; Read segment lists 'p' and extract details (e.g. parameters from ODB XML/struct).
; 'p' is a pointer to an array of pointers to segment structs.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
common c_geopixe_adcs, geopixe_max_adcs
common c_maia_7, maia_energy_pv
if n_elements(geopixe_max_adcs) lt 1 then startupp
if n_elements(maia_energy_pv) eq 0 then maia_energy_pv='ENERGY'
if maia_energy_pv eq '' then maia_energy_pv='ENERGY'
if n_elements(njson) eq 0 then njson=0

err = 1
if typename(p) ne 'ORDEREDHASH' then return, ''

id = {name:'', file:'', X:0L, Y:0L, Z:0L, noY:1 }
dwell = 0.0
nxpixels = 0
nypixels = 0
nzpixels = 0
xorigin = 0.0
yorigin = 0.0
zorigin = 0.0
xsize = 0.
ysize = 0.
zsize = 0.
xname = 'X'
yname = 'Y'
zname = ''
xon = 1
yon = 1
zon = 0
monitor_time = 0.0
name = ''
val = 0
xmin = 16384
xmax = 0
first = 1
energy = 0.0
sample = ''
grain = ''
IC_sensitivity = 1.0
IC_name = 'iXRF:dwell.time'
conv = 0.0
comment = ''
sample_type = ''
sample_serial = ''
detector_identity = ''
flux_chan0_coeff = 0.0
flux_chan0_unit = ''
flux_chan0_name = ''
flux_chan1_coeff = 0.0
flux_chan1_unit = ''
flux_chan1_name = ''
cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, geopixe_max_adcs)
dt_scale_slope = 0.0
dt_scale_offset = 0.0
pileup_on = 0
pileup_file = ''
throttle_on = 0
throttle_file = ''
pileup_found = 0
throttle_found = 0

if njson eq 0 then return, ''

Catch, ErrorNo
if (ErrorNo ne 0) then begin
	Catch, /cancel
	goto, cont
endif

nxpixels = (p["File Header"])["Xres"]
nypixels = (p["File Header"])["Yres"]
xsize = (p["File Header"])["Width (mm)"] * 1000.
ysize = (p["File Header"])["Height (mm)"] * 1000.
dwell = (p["File Header"])["Dwell (mS)"]
comment = 'Duration (H:M:S) = ' + (p["File Header"])["Duration (H:M:S)"]
ndets = (p["File Header"])["Dets"]
for i=0,ndets-1 do begin
	cal[i].on = 1
	cal[i].a = (p["File Header"])["Gain (eV)"] / 1000.
	cal[i].b = 0.0
	cal[i].units = 'keV'
endfor
energy = (p["X-ray Source"])["KV"]

cont:
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'get_iXRF_details',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, ''
		endif
	endif

details = {id:id, monitor:{time:monitor_time, name:name, val:val}, dwell:dwell, $
	scan:{xrange:nxpixels,yrange:nypixels,zrange:nzpixels, xsize:xsize, $
	ysize:ysize, zsize:zsize, $
	xorigin:xorigin, yorigin:yorigin, zorigin:zorigin, $
	xname:xname, yname:yname, zname:zname, xon:xon, yon:yon, zon:zon }, $
	limits:{x:{min:xmin, max:xmax}}, energy:energy, sample:sample, $
	deadtime_cal: {a:dt_scale_slope, b:dt_scale_offset}, $
	IC_sensitivity:IC_sensitivity, IC_name:IC_name, comment:comment, grain:grain, $
	pileup:{on:pileup_on, file:pileup_file, found:pileup_found}, $
	throttle:{on:throttle_on, file:throttle_file, found:throttle_found}, $
	sample_type:sample_type, sample_serial:sample_serial, detector_identity:detector_identity, $
	cal:cal }

err = 0
return, details
end
