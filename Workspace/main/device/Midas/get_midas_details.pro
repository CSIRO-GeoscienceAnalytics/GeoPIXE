function get_midas_details, p, error=err

; Read segment lists 'p' and extract details (e.g. parameters from ODB XML/struct).
; 'p' is a pointer to an array of pointers to segment structs.

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
		warning,'get_midas_details',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_maia_7, maia_energy_pv
if n_elements(geopixe_max_adcs) lt 1 then startupp
if n_elements(maia_energy_pv) eq 0 then maia_energy_pv='ENERGY'
if maia_energy_pv eq '' then maia_energy_pv='ENERGY'

; Known Midas list-mode Event types ...
;										Hex
;	0	ODB1		ODB begin			8000
;	1	ODB2		ODB end				8001
;	2	NMP		MRD NMP pixel event		1
;	3	PIXL		PIXL bank			101
;	4	ADCx		ADCx bank			102
;	5	STAT		STAT bank			103
;	6	DTME		DT CORR bank		104

@midas_listmode.def

err = 1
if ptr_valid(p) eq 0 then return, ''
if n_elements(*p) lt 1 then return, ''
pr = *p
if ptr_valid(pr[0]) eq 0 then return, ''
n_bytes = 64 * 1024L
n = n_elements(pr)
if (n lt 1) or (size(*(pr[0]),/tname) ne 'STRUCT') then return, ''

id = {name:'', file:'', X:0L, Y:0L, Z:0L, noY:1 }
dwell = 0.0
nxpixels = 0
nypixels = 0
nzpixels = 0
xorigin = 0.0
yorigin = 0.0
zorigin = 0.0
xpixel = 1.
ypixel = 1.
zpixel = 1.
xname = ''
yname = ''
zname = ''
xon = 1
yon = 1
zon = 1
monitor_time = 0.0
name = ''
val = 0
xmin = 16384
xmax = 0
first = 1
energy = 0.0
sample = ''
grain = ''
IC_sensitivity = 0.0
IC_name = ''
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

for i=0L,n-1 do begin

	if (*pr[i]).length gt 0 then begin

		dt = data_type[(*pr[i]).tag]
		case dt of
			odb_begin_event_id: begin										; ODB1
				b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
				odb = string(b)

;				s = XML_to_Hash( odb, /ToStruct, error=err)
;				if err eq 0 then begin
;					comment = s.dir_runinfo.key_stop_time
;					id.name = s.dir_nmp_analyzer.dir_adc0_trigger.dir_common.key_host
;					id.file = s.dir_logger.dir_channels.dir_0.dir_settings.key_current_filename
;					nxpixels = long( s.dir_equipment.dir_nmp.key_steps)
;					nypixels = long( s.dir_equipment.dir_nmp.key_stepsy)
;					cal[0].a = float( s.dir_equipment.dir_nmp.dir_parameters.keyarray_cala[0].value)
;					cal[0].b = float( s.dir_equipment.dir_nmp.dir_parameters.keyarray_calb[0].value)
;					cal[0].on = 1
;					cal[0].units = 'keV'
;					
;					dwell = float(dir_equipment.dir_nmp.dir_parameters.key_dwell)
;				endif

				s = json_parse( odb, /tostruct, /toarray)
				if size(s,/tname) eq 'STRUCT' then begin
					comment = s.runinfo.stop_time
					id.name = s.nmp_analyzer.adc0_trigger._common.host
					id.file = s.logger.channels._0.settings.current_filename
					nxpixels = long( s.equipment.nmp.steps)
					nypixels = long( s.equipment.nmp.stepsy)
					cal[0].a = float( s.equipment.nmp.parameters.cala[0])
					cal[0].b = float( s.equipment.nmp.parameters.calb[0])
					cal[0].on = 1
					cal[0].units = 'keV'
					dwell = float( s.equipment.nmp.parameters.dwell)
				endif
				end
			odb_end_event_id: begin											; ODB2
				b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
				odb = string(b)

;				s = XML_to_Hash( odb, /ToStruct, error=err)
;				if err eq 0 then begin
;;					charge?
;				endif

				s = json_parse( odb, /tostruct, /toarray)
				if size(s,/tname) eq 'STRUCT' then begin
;					charge?
				endif
				end

			else:
		endcase
	endif
endfor

details = {id:id, monitor:{time:monitor_time, name:name, val:val}, dwell:dwell, $
	scan:{xrange:nxpixels,yrange:nypixels,zrange:nzpixels, xsize:float2(nxpixels)*xpixel, $
	ysize:float2(nypixels)*ypixel, zsize:float2(nzpixels)*zpixel, $
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
