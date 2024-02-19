function get_falconx_details, p, header=head, version=version, njson=njson, $
				silent=silent, info=info, tick=tick, error=err

; Read segment lists 'p' and extract details (e.g. Monitor records).
; 'p' is a pointer to an array of pointers to segment structs.
;
; header	is PSON header string.

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
       warning,'get_falconx_details',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, ''
    endif
endif
common c_geopixe_adcs, geopixe_max_adcs
common c_falconx_7, falconx_energy_pv
if n_elements(geopixe_max_adcs) lt 1 then startupp
if n_elements(falconx_energy_pv) eq 0 then falconx_energy_pv='ENERGY'
if falconx_energy_pv eq '' then falconx_energy_pv='ENERGY'
if n_elements(version) eq 0 then version = falconx_version( head)
if n_elements(njson) eq 0 then njson = 0L
if n_elements(info) eq 0 then info = get_falconx_info( /define)
if n_elements(silent) lt 1 then silent=0
if n_elements(tick) eq 0 then tick = 4.0e-6

; List mode data field definitions ...

@falconx_listmode.def

;	This list in read_falconx, get_falconx_details too ... also effects list of tags in main 'fx_browse' routine
;													Hex		Length
;	4	Sync		Sync data type					4		1
;	0	Pulse1		Pulse data type					0		1
;	1	Pulse2		optional 2nd word of pulse		1		1
;	9	Gate		Gate state						9		1
;	10	Gstats		Gate statistics					A		9
;	12	Position	Spatial position				C		7
;	11	Spatial 	Spatial statistics				B		9
;	14	Periodic	Periodic stats					E		8
;	15	Error		Analogue/Overflow status		F		1

	channel = 0
	first = 1
	err = 1
	if ptr_valid(p) eq 0 then goto, done
	if n_elements(*p) lt 1 then goto, done
	pr = *p
	if ptr_good(pr[0]) eq 0 then goto, done
	n_bytes = 1024L
	n = n_elements(pr)
	if (n lt 1) or (size(*(pr[0]),/tname) ne 'STRUCT') then return, ''
	
;	Need to scan the JSON header string 'head' first for metadata ...
;	
;	scan.xon	“spatial.axis_1.enable” (test “spatial.numAxes” for # axes)
;	scan.yon, zon, ...
;	tick		“afe.sampleRate” - sample rate clock in MHz (use to set time-base in/from Fortran)
;	cal[].on	"channel.id" - channel ID; only one detector channel per stream!
;				Need to use this to set all 'ste' to this channel?

;	update these to new list-mode data ...

	for i=0L,n-1 do begin
		dtype = data_type[(*pr[i]).tag]
	    if (*pr[i]).length gt 0 then begin
			case dtype of
				11: begin										; spatial
					b = *(*pr[i]).b
					nb = n_elements(b)
					sample = 0L & erase = 0L & icr=0L & raw=0L
					FC0=0L & FC1=0L & FC2=0L & FC3=0L
					time = 0L
					for k=0,nb-1 do begin
						type = long(ishft( b[k] and spatial1_type_mask, spatial1_type_offset))
						data = long( b[k] and spatial1_data_mask)
						case type of
							spatial1_sample_countl_type: begin
								sample = sample OR data
								end
							spatial1_sample_countm_type: begin
								msb = long( b[k] and spatial1_sample_msb_mask)
								sample = sample OR ishft( msb, spatial1_sample_msb_offset)
								end
							spatial1_erasure_count_type: begin
								erase = data
								end
							spatial1_est_icr_type: begin
								icr = data
								end
							spatial1_raw_icr_type: begin
								raw = data
								end
							spatial1_generic_count1_type: begin
								FC0 = data
								end
							spatial1_generic_count2_type: begin
								FC1 = data
								end
							spatial1_generic_count3_type: begin
								FC2 = data
								end
							spatial1_generic_count4_type: begin
								FC3 = data
								end
							spatial1_time_stamp_type: begin
								time = data
								end
							else:
						endcase
					endfor
					if info.scan_dwell eq 0. then info.scan_dwell = tick * sample
					end
				12: begin										; position
					b = *(*pr[i]).b
					if first then begin
						nb = n_elements(b)
;						axis0=0L & axis1=0L & axis2=0L & time=0L
						axis0=-1L & axis1=-1L & axis2=-1L & time=0L
						for k=0,nb-1 do begin
							type = long(ishft( b[k] and position_type_mask, position_type_offset))
							data = long( b[k] and position_data_mask)
							datas = data
							if (datas and pa_sign_bit_mask4) ne 0 then datas = datas or pa_sign_extend
							case type of
								position_axis0_type: begin
									axis0 = datas
									info.id.noY = 0
									end
								position_axis1_type: begin
									axis1 = datas
									info.id.noY = 0
									end
								position_axis2_type: begin
									axis2 = datas
									info.id.noY = 0
									end
								position_axis3_type: begin
									axis3 = datas
									info.id.noY = 0
									end
								position_time_stamp_type: begin
									time = data
									end
								else:
							endcase
						endfor
						info.id.x = axis0
						info.id.Y = axis1
						info.id.z = axis2
;						info.id.noY = 0
						if info.id.noY eq 0 then first = 0
					endif
					end
				else: 
			endcase
		endif
	endfor
		
done:
	charge_gain_unit_lists, vals, units, vunit

;	Did we see metadata values for IC ...

	if (info.flux_chan0_coeff ne 0.0) and (info.flux_chan0_unit ne '') then begin
		info.IC0_sensitivity = charge_sensitivity(info.flux_chan0_coeff, info.flux_chan0_unit)
		if silent eq 0 then print,'Found metadata IC sensitivity (FC0) = ',info.IC0_sensitivity
		
;		How is this connected to the IC PV selection widget in IC struct?
		
		if info.IC_sensitivity eq 0.0 then begin
			info.IC_sensitivity = info.IC0_sensitivity
			info.IC_name = 'FalconX:scaler.FC0' 
		endif
	endif else info.IC0_sensitivity=0.0

;	Note that these ICn returns are not used elsewhere as yet ...

	if (info.flux_chan1_coeff ne 0.0) and (info.flux_chan1_unit ne '') then begin
		info.IC1_sensitivity = charge_sensitivity(info.flux_chan1_coeff, info.flux_chan1_unit)
		if silent eq 0 then print,'Found metadata IC sensitivity (FC1) = ',info.IC1_sensitivity
	endif else info.IC1_sensitivity=0.0
	if (info.flux_chan2_coeff ne 0.0) and (info.flux_chan2_unit ne '') then begin
		info.IC2_sensitivity = charge_sensitivity(info.flux_chan2_coeff, info.flux_chan2_unit)
		if silent eq 0 then print,'Found metadata IC sensitivity (FC2) = ',info.IC2_sensitivity
	endif else info.IC2_sensitivity=0.0
	if (info.flux_chan3_coeff ne 0.0) and (info.flux_chan3_unit ne '') then begin
		info.IC3_sensitivity = charge_sensitivity(info.flux_chan3_coeff, info.flux_chan3_unit)
		if silent eq 0 then print,'Found metadata IC sensitivity (FC3) = ',info.IC3_sensitivity
	endif else info.IC3_sensitivity=0.0

	info.version = version
	info.njson = njson
	info.channel = channel

	err = 0
	return, info
end
