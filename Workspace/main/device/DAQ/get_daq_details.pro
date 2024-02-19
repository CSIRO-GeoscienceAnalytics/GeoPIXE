function get_daq_details, p, error=err

; Read segment lists 'p' and extract details (e.g. Monitor records).
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
       warning,'get_daq_details',['IDL run-time error caught.', '', $
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

x_mask =	'00007FFF'x			; ET
y_mask =	'3FFF8000'x
x_offset =		0
y_offset = 		-15
x_mask2 =	'00007FFF'x			; ET2		Maia 96
y_mask2 =	'3FFF8000'x
x_offset2 =		0
y_offset2 =		-15
x_mask3 =	'00007FFF'x			; ET3		Maia 384 (temporary)
y_mask3 =	'3FFF8000'x
x_offset3 =		0
y_offset3 =		-15

pa_tag_mask4 =			'E0000000'xul; (ET4) EVENT_1
pa_mask4 =	    		'07FFFFFF'xul
pa_sign_bit_mask4 =	    '04000000'xul
pa_sign_extend = 		'F8000000'xul
pa_tag4 =				'E0000000'xul

pa_offset5 =			0					; ET w/ timestamps		DAQ 36
pa_tag_offset5 = 		-27
pa_tag5 =				'E0000000'xul
pa_mask5 =	    		'0FFFFFFF'xul
tag_mask5 =				'80000000'xul
adr_mask5 =				'7F000000'xul
t_mask5 =				'00FFE000'xul
e_mask5 =				'00001FFF'xul
tag_offset5 = 			-31
adr_offset5 = 			-24
t_offset5 =				-13
e_offset5 =				0
xyz_tag5 = 				'80000000'xul
xyz_axis_mask5 =		'50000000'xul
xyz_pos_offset5 =		0

pa_tag_mask5 =			'F0000000'xul
pa_sign_bit_mask5 =	    '08000000'xul
pa_sign_extend5 = 		'F0000000'xul
xyz_tag_mask5 =			'F0000000'xul
xyz_tag_pa5 =			'F0000000'xul
pa_tag_tf5 =			'F0000000'xul
tf_tag_mask5 = 			'FC000000'xul
tf_tag_bt5 =			'F0000000'xul
tf_bit_mask5 =			'03FFFFFF'xul

;	This list in read_maia, get_daq_details too ... also effects list of tags in main 'blog_browse' routine
;
;	1     id                  identity'
;	2     newrun              new run'
;	3     newseg              new segment file'
;	4     tod                 time of day'
;	5     summary             activity summary'
;	6     comment             comment strings'
;	7     sendnext            request next block'
;	8     et                  ET Maia block'
;	9     xy                  XY position Maia block'
;	10    PA                  PA pixel advance Maia block'
;	11    da_put              DA pile-up init table Maia block'
;	12    da_cal              DA cal init coefficients Maia block'
;	13    da_cal2             DA cal init table Maia block'
;	14    da_mat              DA matrix Maia block'
;	15    maia_da_pixel_1     DA pixel record - pixel ppm-uC contributions'
;	16	  maia_da_init_file_1 DA initialization file name
;	17	  maia_da_element_1	  DA initialization element string
;	18    maia_da_params_1    DA parameters'
;	19    maia_da_matrix_raw_1  DA raw matrix'
;	20    maia_da_cal_1       DA calibration'
;	21    maia_da_throttle_1  Throttle table factors'
;	22    maia_da_enable_1    Dali enable status bits'
;	23    sendprev            client stuff'
;	24    sendprevornext      "  '
;	25    et2                 ET 96 element ET block'
;	26    monitor             Epics stuff'
;	27	  pm_eterr_1		  Wollongong boards ...'
;	28	  id2				  revised ID'
;	29	  endrun			  end of run'
;	30	  maia_rexec_1		  MIRO rexec parameterz
;	31	  et3				  ET 384 Maia block'
;	32	  summary_2			  summary, version 2'
;	33	  setgroup			  set the data storage group dir tree for blog'
;	34	  event_1			  (was ET4) ET 384 Maia block'
;	35*	  da_accum			  DA accumulator
;	36*	  ROI_accum			  ROI accumulator
;	37*	  DT_accum			  Deadtime accumulator
;	38*	  DTpp_accum		  DT per pixel accumulator
;	39*	  activity_accum	  DA accumulator
;	40*	  E_spectra			  spectrum accumulator
;	41*	  ET2D_accum		  ET 2D accumulator
;	42	  maia_scaninfo_1	  new scan information, replaces rexec_1
;	43	  T_spectra			  ToT spectrum accumulator
;	44	  maia_da_info_1	  DA/DT info block
;	45	  var_list_1		  Common library var list
;	46	  var_value_1		  library var values
;	47	  maia_scaninfo_2	  Maia ScanInfo 2
;	48	  pm_event_ts_1		  DAQ ET w/o TS
;	49	  pm_event_nots_1	  DAQ ET no TS
;	50	  pm_activity_1		  DAQ Activity
;	51	  setproject		  set next run project string
;	52	  client			  client connect/disconnect log
;	53	  summary_3			  summary, version 3
;	54	  name				  name used (set by client) to identify a client
;	55	  metadata			  metadata records (key/value pairs, separated by newlines \n)
;	56    summary 4			  summary, version 4
;	57    report			  detailed report summary
;		
;	* Indicates Maia records with sub_header data (these flagged in test below)
;	
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
	nupixels = 0
	nvpixels = 0
	nwpixels = 0
	xorigin = 0.0
	yorigin = 0.0
	zorigin = 0.0
	uorigin = 0.0
	vorigin = 0.0
	worigin = 0.0
	xpixel = 1.25
	ypixel = 1.25
	zpixel = 1.25
	upixel = 1.25
	vpixel = 1.25
	wpixel = 1.25
	xname = ''
	yname = ''
	zname = ''
	uname = ''
	vname = ''
	wname = ''
	xon = 0
	yon = 0
	zon = 0
	uon = 0
	von = 0
	won = 0
	order = ''
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
;	k_pileup = -1
;	k_throttle = -1
;	k_da = -1
	k_deadtime = -1
	pileup_on = 0
	pileup_file = ''
	pileup_found = 0
;	throttle_on = 0
;	throttle_file = ''
;	throttle_found = 0
	
	for i=0L,n-1 do begin
;		if i eq 79 then begin
;			print,'debug'
;		endif
	    if (*pr[i]).length gt 0 then begin

			case (*pr[i]).tag of
				1: begin										; identity
					b = (*(*pr[i]).b)[20:min([n_bytes,(*pr[i]).length])-1]
					q = where((b lt 32) or (b gt 127),nq)
					if nq gt 0 then b[q]=32B
					sc = strtrim(strcompress(string(b)),2)
					id.name = sc
					id.file = (*pr[i]).file
					end
;				26: begin										; monitor
;					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
;					sc = string(b)
;					sep = string([10B,13B])
;					s2 = strsplit(sc,sep,/extract)
;					monitor_time = (*pr[i]).tv_sec
;					if n_elements(monitor_string) ge 1 then begin
;						monitor_string = [monitor_string, s2]
;					endif else begin
;						monitor_string = s2
;					endelse
;					end
				30: begin										; rexec1 info
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					b1 = byte(b,0,4)
					scan_type = b1[0]
					scan_major = b1[1]
					r1 = ulong(b,4,2)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					r2 = float(b,12,8)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					xpixel = r2[2]*1000.
					ypixel = r2[3]*1000.
					rvel = r2[4]
					dwell = r2[5]*1000.
					overrun = r2[6]
					tot_time = r2[7]					
					end
;				8: begin										; ET data
;					b = (*(*pr[i]).b)[0:3]
;					d = ulong(b,0,1)
;					swap_bytes, d, /big_endian_data
;					x = fix(d[0] and x_mask)
;					y = fix(ishft( d[0] and y_mask, -15))
;					if x ge 16*1024 then x=x-32*1024
;					if y ge 16*1024 then y=y-32*1024
;					if first then begin
;						id.x = x
;						id.Y = y
;						id.noY = 0
;						first = 0
;					endif
;					end
;				25: begin										; ET2 data
;					b = (*(*pr[i]).b)[0:3]
;					d = ulong(b,0,1)
;					swap_bytes, d, /big_endian_data
;					x = fix(d[0] and x_mask2)
;					y = fix(ishft( d[0] and y_mask2, y_offset2))
;					if x ge 16*1024 then x=x-32*1024
;					if y ge 16*1024 then y=y-32*1024
;					if first then begin
;						id.x = x
;						id.Y = y
;						id.noY = 0
;						first = 0
;					endif
;					end
;				31: begin										; ET3 data
;					b = (*(*pr[i]).b)[0:3]
;					d = ulong(b,0,1)
;					swap_bytes, d, /big_endian_data
;					x = fix(d[0] and x_mask3)
;					y = fix(ishft( d[0] and y_mask3, y_offset3))
;					if x ge 16*1024 then x=x-32*1024
;					if y ge 16*1024 then y=y-32*1024
;					if first then begin
;						id.x = x
;						id.Y = y
;						id.noY = 0
;						first = 0
;					endif
;					end
;				34: begin										; event_1 (was ET4) data
;					b = (*(*pr[i]).b)[0:11]
;					d = ulong(b,0,3)
;					swap_bytes, d, /big_endian_data
;					q1 = where( d and pa_tag_mask4 ne pa_tag4, nq1)
;					dx = d[0] and pa_mask4
;					dy = d[1] and pa_mask4
;					dz = d[2] and pa_mask4
;					t = d and pa_sign_bit_mask4
;					if t[0] ne 0 then dx = dx or pa_sign_extend
;					if t[1] ne 0 then dy = dy or pa_sign_extend
;					if t[2] ne 0 then dz = dz or pa_sign_extend
;					x = long(dx)
;					y = long(dy)
;					z = long(dz)
;					xmin = xmin < x
;					xmax = xmax > x
;					if first then begin
;						id.x = x
;						id.Y = y
;						id.z = z
;						id.noY = 0
;						first = 0
;					endif
;					end
				42: begin										; scaninfo_1
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					scan_num = ulong(b,0,1)
					swap_bytes, scan_num, /big_endian_data
					scan_mda = ulong(b,4,1)
					swap_bytes, scan_mda, /big_endian_data
					b1 = byte(b,8,4)
					scan_order = b1[0]
					r1 = ulong(b,12,3)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					nzpixels = r1[2]
					r2 = float(b,24,7)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					zorigin = r2[2]
					xpixel = r2[3]*1000.
					ypixel = r2[4]*1000.
					zpixel = r2[5]*1000.
					dwell = r2[6]*1000.
					sinfo = string(b[52:*])					
					end
				44: begin										; DA info
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					dt_scale_offset = float(b,8,1)
					swap_bytes, dt_scale_offset, /big_endian_data
					dt_scale_slope = float(b,12,1)
					swap_bytes, dt_scale_slope, /big_endian_data
					end
				46: begin										; var_val 1
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					str = strsplit( string(b), string(10B), /extract)
					ns = n_elements(str)
					for j=0,ns-1 do begin
						if strmid( str[j], 0,1) eq '#' then continue
						nb = locate(' ',str[j])
						if nb lt 1 then continue
						s1 = strmid( str[j], 0, nb)
						nt = strlen(str[j])
						if nt lt nb+2 then continue
						sarg = strmid( str[j], nb+1, nt-nb-1)			; whole arguments string
						lsarg = strlen(sarg)
						if lsarg gt 2 then begin
							s2 = strmid( sarg, 1, lsarg-2)				; strip off " for a single string argument
						endif else s2=''
						simple = (strmid( s2, 0,1) eq '{') ? 0 : 1 
						case s1 of
;							'cal.det[].ecoeff[]': begin
;								sv = strsplit( sarg, ' 	',/extract)
;								val = float2(sv, error=err)
;								if err eq 0 then begin
;									nv = n_elements(sv)
;									n_detectors = nv/2
;									val = reform(val, 2, n_detectors)
;									cal[0:n_detectors-1].b = reform(val[0,0:n_detectors-1])
;									cal[0:n_detectors-1].a = reform(val[1,0:n_detectors-1])
;									cal[0:n_detectors-1].on = 1
;									cal[0:n_detectors-1].units = 'keV'
;								endif
;								end
;							'config.dam.identity': begin
;								if sarg ne '' then detector_identity = sarg
;								end
;							'deadtime.time.coeff[]': begin
;								sv = strsplit( sarg, ' 	',/extract)
;								val = float2(sv, error=err)
;								if err eq 0 then begin
;									nv = n_elements(sv)
;									if nv ge 2 then begin
;										dt_scale_offset = val[0]
;										dt_scale_slope = val[1]
;									endif
;								endif
;								end
							'metadata.sample.type': begin
								if sarg ne '' then sample_type = sarg
								end
							'metadata.sample.name': begin
								if sarg ne '' then sample = sarg
								end
							'metadata.sample.serial': begin
								if sarg ne '' then sample_serial = sarg
								end
								
							'charge.unit': begin
								sv = strsplit( sarg, ' 	',/extract)
								st = '[' + strjoin( sv, ',') + ']'
								sval = unstringify(st, error=err)
								if err eq 0 then begin
									nv = n_elements(sval)
									if nv ge 2 then begin
										flux_chan0_unit = sval[0]
										flux_chan1_unit = sval[1]
									endif
									if (flux_chan0_unit ne '') and (IC_name eq '') then IC_name='DAQ:scaler.FC0'
								endif
								end
							'charge.coeff': begin
								sv = strsplit( sarg, ' 	',/extract)
								val = float2(sv, error=err)
								if err eq 0 then begin
									nv = n_elements(sv)
									if nv ge 2 then begin									
										flux_chan0_coeff = val[0]
										flux_chan1_coeff = val[1]
									endif
								endif
								end
								
;							'position.dim[].source': begin
;								sv = strsplit( sarg, ' 	',/extract)
;								if err eq 0 then begin
;									nv = n_elements(sv)
;									if nv ge 1 then if sv[0] eq 'none' then xon=0
;									if nv ge 2 then if sv[1] eq 'none' then yon=0
;									if nv ge 3 then if sv[2] ne 'none' then zon=0
;								endif
;								end

							'position.dim[].name': begin
								sv = strsplit( sarg, ' 	',/extract)
								if err eq 0 then begin
									nv = n_elements(sv)
									if nv ge 1 then if sv[0] ne '' then xname=sv[0]
									if nv ge 2 then if sv[1] ne '' then yname=sv[1]
									if nv ge 3 then if sv[2] ne '' then zname=sv[2]
								endif
								end

							'scratch.datum[].key': begin
								sv = strsplit( sarg, ' 	',/extract)
								st = '[' + strjoin( sv, ',') + ']'
								sval = unstringify(st, error=err)
								if err eq 0 then begin
									nv = n_elements(sval)
									for k=0,nv-1 do begin
										case sval[k] of
											'DAQ:deadtime': begin
												k_deadtime = k
												end
;											'Maia:pileup.info': begin
;												k_pileup = k
;												end
;											'Maia:throttle.info': begin
;												k_throttle = k
;												end
;											'Maia:DA.info': begin
;												k_da = k
;												end
											else:
										endcase
									endfor
								endif
								end
							'scratch.datum[].value': begin
								sv = strsplit( sarg, ' 	',/extract)
								st = '[' + strjoin( sv, ',') + ']'
								sval = unstringify(st, error=err)
								if err eq 0 then begin
									nv = n_elements(sval)
									if (k_deadtime ge 0) and (k_deadtime lt nv) then begin
										st = str_escape( sval[k_deadtime],/strip)
										t = unstringify( st, error=err)
										if tag_present('cal',t) then begin
											if tag_present('a',t.cal) then dt_scale_slope = t.cal.a
											if tag_present('b',t.cal) then dt_scale_offset = t.cal.b
										endif
									endif
;									if (k_pileup ge 0) and (k_pileup lt nv) then begin
;										st = str_escape( sval[k_pileup],/strip)
;										t = unstringify( st, error=err)
;										if tag_present('on',t) then begin
;											pileup_found = 1
;											pileup_on = t.on							; most authoratative source
;										endif
;										if tag_present('file',t) then pileup_file = t.file
;										if pileup_file eq '' then pileup_on=0
;									endif
;									if (k_throttle ge 0) and (k_throttle lt nv) then begin
;										st = str_escape( sval[k_throttle],/strip)
;										t = unstringify( st, error=err)
;										if tag_present('on',t) then begin
;											throttle_found = 1
;											throttle_on = t.on							; most authoratative source
;										endif
;										if tag_present('file',t) then throttle_file = t.file
;										if throttle_file eq '' then throttle_on=0
;									endif
								endif
								end			
					
;							'da.info': begin
;								end
;							'pileup.info': begin
;								if (pileup_found eq 0) then begin
;									if simple eq 0 then begin							; old .info stringify structs
;										t = unstringify( str_escape(s2,/strip))
;										if tag_present('on',t) then pileup_on = t.on
;										if tag_present('file',t) then pileup_file = t.file
;										if pileup_file eq '' then pileup_on=0
;										pileup_found = 1
;									endif else begin									; new .info (duplicate of metadata and var_val)
;										if (s2 ne '') then begin						; or older .info, pre-structs
;											pileup_found = 1
;											pileup_on = 1
;											pileup_file = s2
;											if pileup_file eq '' then pileup_on=0
;										endif
;									endelse
;								endif
;								end
							else:
						endcase
					endfor
					end
				47: begin										; scaninfo_2 (detect units later ...)
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					scan_num = ulong(b,0,1)
					swap_bytes, scan_num, /big_endian_data
					scan_mda = ulong(b,4,1)
					swap_bytes, scan_mda, /big_endian_data
					b1 = byte(b,8,4)
					scan_order = b1[0]
					r1 = ulong(b,12,3)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					nzpixels = r1[2]
					r2 = float(b,24,7)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					zorigin = r2[2]
					xpixel = r2[3]*1000.
					ypixel = r2[4]*1000.
					zpixel = r2[5]*1000.
					dwell = r2[6]*1000.
					sinfo = string(b[52:*])
					end
				55: begin										; metadata
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					str = strsplit( string(b), string(10B), /extract)
					ns = n_elements(str)
					for j=0,ns-1 do begin
						nb = locate(' ',str[j])
						s1 = strmid( str[j], 0, nb)
						nt = strlen(str[j])
						if nt ge nb+2 then begin
							s2 = strmid( str[j], nb+1)
						endif else s2 = ''
						case s1 of
							'beam_energy': begin
								if s2 ne '' then energy = float2(s2)
								end
							'detector_identity': begin
								if s2 ne '' then detector_identity = s2
								end
							'sample_info': begin
								if s2 ne '' then comment = s2 + comment
								end
							'sample_serial': begin
								if s2 ne '' then comment = comment + ', ' + s2
								end
							'scan_region': begin
								if s2 ne '' then grain = s2
								end
							'scan_crossref': begin
								if s2 ne '' then comment = comment + ', #' + s2
								end
							'scan_order': begin
								if s2 ne '' then order = s2
								end

							'scan_dim0_name': begin
								if s2 ne '' then xname = s2
								end
							'scan_dim1_name': begin
								if s2 ne '' then yname = s2
								end
							'scan_dim2_name': begin
								if s2 ne '' then zname = s2
								end
							'scan_dim3_name': begin
								if s2 ne '' then uname = s2
								end
							'scan_dim4_name': begin
								if s2 ne '' then vname = s2
								end
							'scan_dim5_name': begin
								if s2 ne '' then wname = s2
								end
								
							'scan_dim0_enable': begin
								if s2 ne '' then xon = fix2(s2)
								end
							'scan_dim1_enable': begin
								if s2 ne '' then yon = fix2(s2)
								end
							'scan_dim2_enable': begin
								if s2 ne '' then zon = fix2(s2)
								end
							'scan_dim3_enable': begin
								if s2 ne '' then uon = fix2(s2)
								end
							'scan_dim4_enable': begin
								if s2 ne '' then von = fix2(s2)
								end
							'scan_dim5_enable': begin
								if s2 ne '' then won = fix2(s2)
								end
							
;							Take care here as this 'origin' is the 6D x,y,z,u,v,w axes, where actually
;							the physical stage XYZ is always ZUV. Get this from scan.info

;							'scan_dim0_origin': begin
;								if s2 ne '' then xorigin = float2(s2)
;								end
;							'scan_dim1_origin': begin
;								if s2 ne '' then yorigin = float2(s2)
;								end
;							'scan_dim2_origin': begin
;								if s2 ne '' then zorigin = float2(s2)
;								end
;							'scan_dim3_origin': begin
;								if s2 ne '' then uorigin = float2(s2)
;								end
;							'scan_dim4_origin': begin
;								if s2 ne '' then vorigin = float2(s2)
;								end
;							'scan_dim5_origin': begin
;								if s2 ne '' then worigin = float2(s2)
;								end

							'scan_info': begin
								t = unstringify(s2)
								if tag_present('origin',t) then begin
									if tag_present('x',t.origin) then zorigin = t.origin.x
									if tag_present('y',t.origin) then uorigin = t.origin.y
									if tag_present('z',t.origin) then vorigin = t.origin.z
								endif
								end

							'scan_dim0_extent': begin
								if s2 ne '' then nxpixels = fix2(s2)
								end
							'scan_dim1_extent': begin
								if s2 ne '' then nypixels = fix2(s2)
								end
							'scan_dim2_extent': begin
								if s2 ne '' then nzpixels = fix2(s2)
								end
							'scan_dim3_extent': begin
								if s2 ne '' then nupixels = fix2(s2)
								end
							'scan_dim4_extent': begin
								if s2 ne '' then nvpixels = fix2(s2)
								end
							'scan_dim5_extent': begin
								if s2 ne '' then nwpixels = fix2(s2)
								end

							'scan_dim0_pitch': begin
								if s2 ne '' then xpixel = 1000. * float2(s2)
								end
							'scan_dim1_pitch': begin
								if s2 ne '' then ypixel = 1000. * float2(s2)
								end
							'scan_dim2_pitch': begin
								if s2 ne '' then zpixel = 1000. * float2(s2)
								end
							'scan_dim3_pitch': begin
								if s2 ne '' then upixel = 1000. * float2(s2)
								end
							'scan_dim4_pitch': begin
								if s2 ne '' then vpixel = 1000. * float2(s2)
								end
							'scan_dim5_pitch': begin
								if s2 ne '' then wpixel = 1000. * float2(s2)
								end

							'charge_coeff': begin
								if s2 ne '' then flux_chan0_coeff = float2(s2)
								end
							'charge_unit': begin
								if s2 ne '' then begin
									flux_chan0_unit = s2
									if (IC_name eq '') then IC_name='DAQ:scaler.FC0'
								endif
								end

;							'throttle_info': begin
;								if throttle_found eq 0 then begin						; should just duplicate scratch
;									throttle_file = s2
;									if lenchr(s2) gt 0 then throttle_on = 1
;									throttle_found = 1
;								endif
;								end
;							'pileup_info': begin
;								if pileup_found eq 0 then begin							; should just duplicate scratch
;									pileup_file = s2
;									if lenchr(s2) gt 0 then pileup_on = 1
;									pileup_found = 1
;								endif
;								end
;							'deadtime_coeff0': begin
;								if s2 ne '' then dt_scale_offset = float2(s2)
;								end
;							'deadtime_coeff1': begin
;								if s2 ne '' then dt_scale_slope = float2(s2)
;								end
							else:
						endcase
					endfor
					end
				else: 
			endcase
		endif
	endfor
	
;	Did we see metadata values for IC ...

	if (flux_chan0_coeff ne 0.0) and (flux_chan0_unit ne '') then begin
		IC_sensitivity0 = charge_sensitivity(flux_chan0_coeff, flux_chan0_unit)
		print,'Found metadata IC sensitivity (FC0) = ',IC_sensitivity0
		
;	How is this connected to the IC PV selection widget in IC struct?
		
		if IC_sensitivity eq 0.0 then begin
			IC_sensitivity = IC_sensitivity0
			IC_name = 'DAQ:scaler.FC0' 
		endif
	endif
;	if (flux_chan1_coeff ne 0.0) and (flux_chan1_unit ne '') then begin
;		IC_sensitivity1 = charge_sensitivity(flux_chan1_coeff, flux_chan1_unit)
;		print,'Found metadata IC sensitivity (FC1) = ',IC_sensitivity1
;	endif
	
	while energy gt 500. do energy = energy/1000.
	
	details = {id:id, dwell:dwell, $
				scan:{xrange:nxpixels,yrange:nypixels,zrange:nzpixels,urange:nupixels,vrange:nvpixels,wrange:nwpixels, $
				xsize:float2(nxpixels)*xpixel, ysize:float2(nypixels)*ypixel, zsize:float2(nzpixels)*zpixel, $
				usize:float2(nupixels)*upixel, vsize:float2(nvpixels)*vpixel, wsize:float2(nwpixels)*wpixel, $
				xorigin:xorigin, yorigin:yorigin, zorigin:zorigin, uorigin:uorigin, vorigin:vorigin, worigin:worigin, $
				xname:xname, yname:yname, zname:zname, uname:uname, vname:vname, wname:wname, $
				xon:xon, yon:yon, zon:zon, uon:uon, von:von, won:won, order:order }, $
				limits:{x:{min:xmin, max:xmax}}, energy:energy, sample:sample, $
				deadtime_cal: {a:dt_scale_slope, b:dt_scale_offset}, $
				IC_sensitivity:IC_sensitivity, IC_name:IC_name, comment:comment, grain:grain, $
				pileup:{on:pileup_on, file:pileup_file, found:pileup_found}, $
;				throttle:{on:throttle_on, file:throttle_file, found:throttle_found}, $
				sample_type:sample_type, sample_serial:sample_serial, detector_identity:detector_identity, $
				cal:cal }
	err = 0
	return, details
end
