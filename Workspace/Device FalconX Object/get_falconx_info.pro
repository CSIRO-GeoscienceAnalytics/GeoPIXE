	function get_falconx_info, file, define=idefine, error=error
;
;	Read in a FalconX INFO file for run details.
;	If not found, then return error=1.
;
	COMPILE_OPT STRICTARR
	if n_elements(file) eq 0 then file=''

	info = {	$
		file:					file, $			; file name
		run:					0L, $			; run #
		version:				0L, $			; JSON version
		njson:					0L, $
		id:						{name:'', X:0L, Y:0L, Z:0L, noY:1 }, $
		channel:				0, $
		IC_sensitivity:			0.0, $			; IC Sensitivity (nA/V)
		IC_name:				'FalconX:scaler.FC0', $ 

		IC0_sensitivity:		0.0, $			; individual sensitivities 
		IC1_sensitivity:		0.0, $			; (not really used yet)
		IC2_sensitivity:		0.0, $
		IC3_sensitivity:		0.0, $ 

;	Specific metadata follow ...

		run_start_time:			'', $			; start date and time
		run_group:				'', $			; group

		sample_name:			'', $			; sample name
		sample_owner:			'', $			; sample owner
		sample_type:			'', $			; sample type (e.g. "standard", "user")
		sample_serial:			'', $			; sample serial number/code
		sample_info:			'', $			; sample info string

		beam_energy:			0.0, $			; energy (eV)
		detector_identity:		'', $			; name of detector

		flux_chan0_name:		'', $			; IC0 name
		flux_chan0_coeff:		0.0, $			; 	gain multiplier
		flux_chan0_unit:		'', $			; 	gain range unit (e.g. "nA/V")
		flux_chan1_name:		'', $			; IC1 name
		flux_chan1_coeff:		0.0, $			; 	gain multiplier
		flux_chan1_unit:		'', $			; 	gain range unit (e.g. "nA/V")
		flux_chan2_name:		'', $			; IC2 name
		flux_chan2_coeff:		0.0, $			; 	gain multiplier
		flux_chan2_unit:		'', $			; 	gain range unit (e.g. "nA/V")
		flux_chan3_name:		'', $			; IC3 name
		flux_chan3_coeff:		0.0, $			; 	gain multiplier
		flux_chan3_unit:		'', $			; 	gain range unit (e.g. "nA/V")

		scan_dwell:				0.0, $			; nominal dwell time per pixel (ms)
		scan_type:				'', $			; type of scan
		scan_crossref:			'', $			; cross-reference code
		scan_info:				'', $			; comment
		scan_region:			'', $			; region/grain
		scan_order:				'', $			; axis order codes (e.g. “012”)
		sequence_number:		0, $			; sequence in a multiscan set
		sequence_total:			0, $			; total in a multiscan set

		scan_dim1_name:			'', $			; 1 axis name
		scan_dim1_extent:		0L, $			; 	# pixels
		scan_dim1_size:			0.0, $			; 	size (mm)
		scan_dim1_origin:		0.0, $			; 	origin (mm)
		scan_dim1_pitch:		0.0, $			; 	pitch (mm)
		scan_dim1_unit:			'', $			; 	units ("mm" usually)
		scan_dim2_name:			'', $			; 2 axis name
		scan_dim2_extent:		0L, $			; 	# pixels
		scan_dim2_size:			0.0, $			; 	size (mm)
		scan_dim2_origin:		0.0, $			; 	origin (mm)
		scan_dim2_pitch:		0.0, $			; 	pitch (mm)
		scan_dim2_unit:			'', $			; 	units ("mm" usually)

		scan_dim3_name:			'', $			; 3 axis name
		scan_dim3_extent:		0L, $			; 	# pixels
		scan_dim3_size:			0.0, $			; 	size (mm)
		scan_dim3_origin:		0.0, $			; 	origin (mm)
		scan_dim3_pitch:		0.0, $			; 	pitch (mm)
		scan_dim3_unit:			'', $			; 	units ("mm" usually)
		scan_dim4_name:			'', $			; 4 axis name
		scan_dim4_extent:		0L, $			; 	# pixels
		scan_dim4_size:			0.0, $			; 	size (mm)
		scan_dim4_origin:		0.0, $			; 	origin (mm)
		scan_dim4_pitch:		0.0, $			; 	pitch (mm)
		scan_dim4_unit:			'', $			; 	units ("mm" usually)

		scan_dim5_name:			'', $			; 5 axis name
		scan_dim5_extent:		0L, $			; 	# pixels
		scan_dim5_size:			0.0, $			; 	size (mm)
		scan_dim5_origin:		0.0, $			; 	origin (mm)
		scan_dim5_pitch:		0.0, $			; 	pitch (mm)
		scan_dim5_unit:			'', $			; 	units ("mm" usually)
		scan_dim6_name:			'', $			; 5 axis name
		scan_dim6_extent:		0L, $			; 	# pixels
		scan_dim6_size:			0.0, $			; 	size (mm)
		scan_dim6_origin:		0.0, $			; 	origin (mm)
		scan_dim6_pitch:		0.0, $			; 	pitch (mm)
		scan_dim6_unit:			'' $			; 	units ("mm" usually)
	}
	error = 0
	if n_elements(idefine) eq 0 then idefine = 0
	if idefine then return, info

	error = 1
	mp = 0
	if n_elements(file) lt 1 then return,0
	if lenchr(file) lt 1 then return,0

	F = strip_file_ext(file)
	i = locate_last('_',F)
	if i gt 0 then F=strmid(F,0,i)
	info.run = long2( strip_path(F))
	F = F + '.info'

	on_ioerror,more
	openr,unit,F,/get_lun

	line = ''
	on_ioerror,err
	while EOF(unit) eq 0 do begin
		readf, unit, line
		if (lenchr(line) eq 0) then continue
		i = locate('#',line)
		if i eq 0 then continue
		if i gt 0 then line = strmid(line,0,i)

		set_separators, ' 	'
		chop_string, line, str, n_str
		if n_str lt 2 then continue
		args = strjoin(str[1:*],' ')

		case str[0] of
			'run_start_time': begin
				info.run_start_time = args
				end
			'run_group': begin
				info.run_group = args
				end
			'sample_name': begin
				info.sample_name = args
				end
			'sample_owner': begin
				info.sample_owner = args
				end
			'sample_type': begin
				info.sample_type = str[1]
				end
			'sample_serial': begin
				info.sample_serial = str[1]
				end

			'sample_info': begin
				info.sample_info = args
				end
			'sample_info_1': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_2': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_3': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_4': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_5': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_6': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_7': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end
			'sample_info_8': begin
				if info.sample_info eq '' then begin
					info.sample_info = args
				endif else begin
					info.sample_info = info.sample_info + ', ' + args
				endelse
				end

			'beam_energy': begin
				info.beam_energy = float2(str[1])
				end
			'detector_identity': begin
				info.detector_identity = str[1]
				end

			'flux_chan0_name': begin
				info.flux_chan0_name = str[1]
				end
			'flux_chan0_coeff': begin
				info.flux_chan0_coeff = float2(str[1])
				end
			'flux_chan0_unit': begin
				info.flux_chan0_unit = str[1]
				end
			'flux_chan1_name': begin
				info.flux_chan1_name = str[1]
				end
			'flux_chan1_coeff': begin
				info.flux_chan1_coeff = float2(str[1])
				end
			'flux_chan1_unit': begin
				info.flux_chan1_unit = str[1]
				end

			'flux_chan2_name': begin
				info.flux_chan2_name = str[1]
				end
			'flux_chan2_coeff': begin
				info.flux_chan2_coeff = float2(str[1])
				end
			'flux_chan2_unit': begin
				info.flux_chan2_unit = str[1]
				end
			'flux_chan3_name': begin
				info.flux_chan3_name = str[1]
				end
			'flux_chan3_coeff': begin
				info.flux_chan3_coeff = float2(str[1])
				end
			'flux_chan3_unit': begin
				info.flux_chan3_unit = str[1]
				end

			'scan_dwell': begin
				info.scan_dwell = float2(str[1])
				end
			'scan_type': begin
				info.scan_type = str[1]
				end
			'scan_crossref': begin
				info.scan_crossref = str[1]
				end
			'scan_info': begin
				info.scan_info = args
				end
			'scan_region': begin
				info.scan_region = str[1]
				end
			'scan_order': begin
				info.scan_order = str[1]
				end

			'sequence_number': begin
				info.sequence_number = fix2(str[1])
				end
			'sequence_total': begin
				info.sequence_total = fix2(str[1])
				end

			'scan_dim1_name': begin
				info.scan_dim1_name = str[1]
				end
			'scan_dim1_extent': begin
				info.scan_dim1_extent = fix2(str[1])
				end
			'scan_dim1_size': begin
				info.scan_dim1_size = float2(str[1])
				end
			'scan_dim1_origin': begin
				info.scan_dim1_origin = float2(str[1])
				end
			'scan_dim1_pitch': begin
				info.scan_dim1_pitch = float2(str[1])
				end
			'scan_dim1_unit': begin
				info.scan_dim1_unit = str[1]
				end

			'scan_dim2_name': begin
				info.scan_dim2_name = str[1]
				end
			'scan_dim2_extent': begin
				info.scan_dim2_extent = fix2(str[1])
				end
			'scan_dim2_size': begin
				info.scan_dim2_size = float2(str[1])
				end
			'scan_dim2_origin': begin
				info.scan_dim2_origin = float2(str[1])
				end
			'scan_dim2_pitch': begin
				info.scan_dim2_pitch = float2(str[1])
				end
			'scan_dim2_unit': begin
				info.scan_dim2_unit = str[1]
				end

			'scan_dim3_name': begin
				info.scan_dim3_name = str[1]
				end
			'scan_dim3_extent': begin
				info.scan_dim3_extent = fix2(str[1])
				end
			'scan_dim3_size': begin
				info.scan_dim3_size = float2(str[1])
				end
			'scan_dim3_origin': begin
				info.scan_dim3_origin = float2(str[1])
				end
			'scan_dim3_pitch': begin
				info.scan_dim3_pitch = float2(str[1])
				end
			'scan_dim3_unit': begin
				info.scan_dim3_unit = str[1]
				end

			'scan_dim4_name': begin
				info.scan_dim4_name = str[1]
				end
			'scan_dim4_extent': begin
				info.scan_dim4_extent = fix2(str[1])
				end
			'scan_dim4_size': begin
				info.scan_dim4_size = float2(str[1])
				end
			'scan_dim4_origin': begin
				info.scan_dim4_origin = float2(str[1])
				end
			'scan_dim4_pitch': begin
				info.scan_dim4_pitch = float2(str[1])
				end
			'scan_dim4_unit': begin
				info.scan_dim4_unit = str[1]
				end

			'scan_dim5_name': begin
				info.scan_dim5_name = str[1]
				end
			'scan_dim5_extent': begin
				info.scan_dim5_extent = fix2(str[1])
				end
			'scan_dim5_size': begin
				info.scan_dim5_size = float2(str[1])
				end
			'scan_dim5_origin': begin
				info.scan_dim5_origin = float2(str[1])
				end
			'scan_dim5_pitch': begin
				info.scan_dim5_pitch = float2(str[1])
				end
			'scan_dim5_unit': begin
				info.scan_dim5_unit = str[1]
				end

			'scan_dim6_name': begin
				info.scan_dim6_name = str[1]
				end
			'scan_dim6_extent': begin
				info.scan_dim6_extent = fix2(str[1])
				end
			'scan_dim6_size': begin
				info.scan_dim6_size = float2(str[1])
				end
			'scan_dim6_origin': begin
				info.scan_dim6_origin = float2(str[1])
				end
			'scan_dim6_pitch': begin
				info.scan_dim6_pitch = float2(str[1])
				end
			'scan_dim6_unit': begin
				info.scan_dim6_unit = str[1]
				end
			else:
		endcase
	endwhile
	error = 0

more:
	close_file, unit
	return, info

err:
	warning, 'get_falconx_info', ['Serious error in INFO file.','Ignoring settings.','',$
			'Check file format, and be sure to','use ASCII mode for FTP transfers.']
	goto, more
	end

