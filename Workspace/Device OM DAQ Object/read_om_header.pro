function read_om_header, device, unit, error=error

; Based on the OM DAQ SDK guide and header info in "Dqp_DataIO.h" and "DataIO.chm".
; (see Oxford Microbeams for further information).
;
; OM DAQ is a 8-channel PC data acquisition and scanning
; system developed by Oxford Microbeams for general nuclear microprobe imaging.

common c_mpsys_1, minx,miny, ste_mask,stx_mask,sty_mask, mdx_mask,mdy_mask
common c_mpsys_2, e_mask,x_mask,y_mask, ste_offset,stx_offset,sty_offset
common c_om_2, lmf_cal
common c_om_3b, c_per_pulse, lmf_charge
common c_om_4, lmf_size
common c_om_6, LMF_version, count_offset, count_mask, lmf_pixels
common c_om_7, LMF_x, LMF_y
if n_elements(lmf_charge) eq 0 then lmf_charge=-1.0
if n_elements(LMF_x) eq 0 then LMF_x=uintarr(256*256L)
if n_elements(LMF_y) eq 0 then LMF_y=uintarr(256*256L)

	error = 0
	on_ioerror, bad_io

	head = {ID:0u, type:0u, size:0u}
	readu, unit, head
	swap_bytes, head, big_endian_data=0

	OM_id = ishft( head.ID and 'FF00'xu, -8)
	header_version = head.ID and '00FF'xu
	LMF_version = ishft( head.type and 'FF00'xu, -8)
	LMF_tag = head.type and '00FF'xu
	block_size = head.size						; a small number may indicate 2^n format?

	if (OM_id ne '42'xu) or (LMF_tag ne 2) then begin
		warning,'read_om_header','Not an OM LMF file format.'
		error = 1
		return, 0
	endif
	if (LMF_version lt 0) or (LMF_version gt 4) then begin
		warning,'read_om_header',['Unsupported LMF version number ='+string(LMF_version), $
						'Get updated file format information from Oxford Microbeams.']
		error = 1
		return, 0
	endif

; Based on header info in OM file: Dqp_DataIO.h
; 
;	5/6/2007:  Header versions are getting complicated.
;	Here is a table showing the RUNDATA and  ADCINFO
;	structure versions associated with each header version
;	
;	HV		RUNDATA	    ADCINFO	   File Struct	Coded here
;	1		  1	  		  1			1
;	2     	  2           1			1
;	3     	  3           1			1
;	4     	  4           2			1			***
;	5     	  5           3			1
;	6     	  6           3			1
;	7     	  6           4			1
;	8     	  7           4			1
;	9     	  8           5			1
;	10    	  8           6			1
;	11    	  8           7			1			***
;	12		  8			  7			2			***
;	13		  8			  8			2			***
;	14		  9			  9			2			***
;
;	The new "File Struct" 2 (LMF tag?) indicates a new Listmode structure.
;

	case header_version of
;		1: begin
;			rundata = bytarr(1043)				; 1044?
;			adcinfo = bytarr(122,8)
;			end
;		2: begin
;			rundata = bytarr(1047)				; 1048?
;			adcinfo = bytarr(122,8)
;			end
;		3: begin
;			rundata = bytarr(1055)				; 1056?
;			adcinfo = bytarr(122,8)
;			end
		4: begin
			rundata = { $						; bytarr(3606)
					week: bytarr(4), $			; week #
					run: bytarr(4), $			; run #
					user: bytarr(5), $			; user name
					op: bytarr(5), $			; operator code
					date: bytarr(13), $			; date
					time: bytarr(6), $			; time
					caption: bytarr(81), $		; caption
					dummy: bytarr(170,5), $
					target: 0S, $				; target number
					mode: bytarr(1), $			; scan mode
					size: uintarr(2), $			; scan size (um)
					pt: intarr(2), $			; point coords (x,y)
					prevmap: bytarr(7), $		; previous map run number
					MeV: 0.0, $					; beam energy
					Z: 0S, $					; Z of particle
					A: 0S, $					; A of particle
					QQ: 0S, $					; Q of particle
					dt: 0UL, $					; elapsed time (sec)
					q: ulonarr(4), $			; Q pulses
					q_factor: 0.0, $			; Q scaling factor
					C_per_pulse: 0.0, $			; Q scaler sensitivity
					Qscaler: intarr(10), $		; Dead-time scaler setup info
					dScanArea: 0UL, $			; total scan area (pixels)
					tgt: fltarr(2), $			; sample stage position (mm,deg)
					sample: bytarr(170,15)}		; sample structure

			adcinfo = replicate( $				; bytarr(128,8)
					{live:0S, $					; 1 if ADC active
					cal: fltarr(3), $			; cal c0,c1,(c2 = # channels?)
					det_name: bytarr(9), $		; detector file name
					sig: 0S, $					; radiation type
					fill: bytarr(103) }, 8)
			ncalsize = 2
			end
		11: begin
;			rundata = bytarr(3606)				; 3606

			rundata = { $						; bytarr(3606)		RUNDATA 8
					week: 	bytarr(4), $		; week #
					run: 	bytarr(4), $		; run #
					user: 	bytarr(16), $		; user name
					op: 	bytarr(5), $		; operator code
					date: 	bytarr(13), $		; date
					time: 	bytarr(6), $		; time
					caption: bytarr(81), $		; caption
					sample: bytarr(170+70,15), $	; 15 sample layer specs				*** fixed July-2014
					target: 0S, $				; target number
					mode:	bytarr(1), $		; scan mode
					prevmap: bytarr(7), $		; previous map run number
					MeV:	0.0, $				; beam energy
					Z:		0S, $				; Z of particle
					A:		0S, $				; A of particle
					QQ: 	0S, $				; Q of particle
					dt:		0UL, $				; elapsed time (sec)
					q:		ulonarr(4), $		; Q pulses
					q_factor: 0.0, $			; Q scaling factor
					C_per_pulse: 0.0, $			; Q scaler sensitivity
					Qscaler: intarr(10), $		; Dead-time scaler setup info
					TgtPos:	fltarr(3), $		; tgt position
					TgtAng:	fltarr(3), $		; tgt angle
					scan: {	ScanType:		0S, $	; scan type
							ScanSource: 	0S, $	; type of scanning (beam/tgt)
							Pixels: 		0S, $	; number of pixels in original scan
							ClockSource: 	0S, $	; source of pixel clocks
							TickPerPixel: 	0S, $	; number of ticks per pixel
							thisPoint: 		0S, $	; sequence number of point in sequence
							PointsInSequence: 0S, $	; number of points in point sequence
							P0: 		intarr(2), $	; start point of line (x,y pixels)
							P1: 		intarr(2), $	; end point of line (x,y pixels)
							ScanSize:	fltarr(2), $	; scan size (x,y um);
							TotalPixels: 	0UL}, $	; total numnber of pixels in selected scan
					scalers: ulonarr(5), $		; scalers
					site:	bytarr(16) }		; site

			adcinfo = replicate( $				; bytarr(230,8)		ADCINFO 7
					{ source:	0S, $			; source type
					HW_chan:	0L,	$			; Hardware channel for this data
					HW_bins:	0L, $			; max number of energy channels
					live:		0S, $			; 1 if ADC active
					cal: 		fltarr(4), $	; cal c0,c1,c2, (c3 = # channels?)
					det_name: 	bytarr(9), $	; detector file name
					det_desc: 	bytarr(80), $	; detector description
					sig: 		0S, $			; radiation type
					fill: 		bytarr(111) }, 8)
			ncalsize = 3
			end
		12: begin
;			rundata = bytarr(3606)				; 3606

			rundata = { $						; bytarr(3606)		RUNDATA 8
					week: 	bytarr(4), $		; week #
					run: 	bytarr(4), $		; run #
					user: 	bytarr(16), $		; user name
					op: 	bytarr(5), $		; operator code
					date: 	bytarr(13), $		; date
					time: 	bytarr(6), $		; time
					caption: bytarr(81), $		; caption
					sample: bytarr(170+70,15), $	; 15 sample layer specs				*** fixed July-2014
					target: 0S, $				; target number
					mode:	bytarr(1), $		; scan mode
					prevmap: bytarr(7), $		; previous map run number
					MeV:	0.0, $				; beam energy
					Z:		0S, $				; Z of particle
					A:		0S, $				; A of particle
					QQ: 	0S, $				; Q of particle
					dt:		0UL, $				; elapsed time (sec)
					q:		ulonarr(4), $		; Q pulses
					q_factor: 0.0, $			; Q scaling factor
					C_per_pulse: 0.0, $			; Q scaler sensitivity
					Qscaler: intarr(10), $		; Dead-time scaler setup info
					TgtPos:	fltarr(3), $		; tgt position
					TgtAng:	fltarr(3), $		; tgt angle
					scan: {	ScanType:		0S, $	; scan type
							ScanSource: 	0S, $	; type of scanning (beam/tgt)
							Pixels: 		0S, $	; number of pixels in original scan
							ClockSource: 	0S, $	; source of pixel clocks
							TickPerPixel: 	0S, $	; number of ticks per pixel
							thisPoint: 		0S, $	; sequence number of point in sequence
							PointsInSequence: 0S, $	; number of points in point sequence
							P0: 		intarr(2), $	; start point of line (x,y pixels)
							P1: 		intarr(2), $	; end point of line (x,y pixels)
							ScanSize:	fltarr(2), $	; scan size (x,y um);
							TotalPixels: 	0UL}, $	; total numnber of pixels in selected scan
					scalers: ulonarr(5), $		; scalers
					site:	bytarr(16) }		; site

			adcinfo = replicate( $				; bytarr(230,8)		ADCINFO 7
					{ source:	0S, $			; source type
					HW_chan:	0L,	$			; Hardware channel for this data
					HW_bins:	0L, $			; max number of energy channels
					live:		0S, $			; 1 if ADC active
					cal: 		fltarr(4), $	; cal c0,c1,c2, (c3 = # channels?)
					det_name: 	bytarr(9), $	; detector file name
					det_desc: 	bytarr(80), $	; detector description
					sig: 		0S, $			; radiation type
					fill: 		bytarr(111) }, 8)
			ncalsize = 3
			end
		13: begin
;			rundata = bytarr(3606)				; 3606 ??

			rundata = { $						; bytarr(3606)		RUNDATA 8
					week: 	bytarr(4), $		; week #
					run: 	bytarr(4), $		; run #
					user: 	bytarr(16), $		; user name
					op: 	bytarr(5), $		; operator code
					date: 	bytarr(13), $		; date
					time: 	bytarr(6), $		; time
					caption: bytarr(81), $		; caption
					sample: bytarr(170+70,15), $	; 15 sample layer specs				*** fixed July-2014
					target: 0S, $				; target number
					mode:	bytarr(1), $		; scan mode
					prevmap: bytarr(7), $		; previous map run number
					MeV:	0.0, $				; beam energy
					Z:		0S, $				; Z of particle
					A:		0S, $				; A of particle
					QQ: 	0S, $				; Q of particle
					dt:		0UL, $				; elapsed time (sec)
					q:		ulonarr(4), $		; Q pulses
					q_factor: 0.0, $			; Q scaling factor
					C_per_pulse: 0.0, $			; Q scaler sensitivity
					Qscaler: intarr(10), $		; Dead-time scaler setup info
					TgtPos:	fltarr(3), $		; tgt position
					TgtAng:	fltarr(3), $		; tgt angle
					scan: {	ScanType:		0S, $	; scan type
							ScanSource: 	0S, $	; type of scanning (beam/tgt)
							Pixels: 		0S, $	; number of pixels in original scan
							ClockSource: 	0S, $	; source of pixel clocks
							TickPerPixel: 	0S, $	; number of ticks per pixel
							thisPoint: 		0S, $	; sequence number of point in sequence
							PointsInSequence: 0S, $	; number of points in point sequence
							P0: 		intarr(2), $	; start point of line (x,y pixels)
							P1: 		intarr(2), $	; end point of line (x,y pixels)
							ScanSize:	fltarr(2), $	; scan size (x,y um);
							TotalPixels: 	0UL}, $	; total numnber of pixels in selected scan
					scalers: ulonarr(5), $		; scalers
					site:	bytarr(16) }		; site

			adcinfo = replicate( $				; bytarr(234,8)		ADCINFO 8
					{ source:	0S, $			; source type
					HW_chan:	0L,	$			; Hardware channel for this data
					HW_bins:	0L, $			; max number of energy channels
					live:		0S, $			; 1 if ADC active
					cal: 		fltarr(4), $	; cal c0,c1,c2, (c3 = # channels?)
					det_name: 	bytarr(9), $	; detector file name
					det_desc: 	bytarr(80), $	; detector description
					sig: 		0S, $			; radiation type
					fill: 		bytarr(115) }, 8)	; added "timingDeltaT" for v8
			ncalsize = 3
			end
		14: begin
;			rundata = bytarr(3606)				; ??

			rundata = { $						; bytarr(?)		RUNDATA/ dataSTRUCT 9
					week: 	bytarr(4), $		; week #
					run: 	bytarr(4), $		; run #
					user: 	bytarr(16), $		; user name
					op: 	bytarr(5), $		; operator code
					date: 	bytarr(13), $		; date
					time: 	bytarr(6), $		; time
					caption: bytarr(81), $		; caption
					sample: replicate( { $		; (170 bytes + eq) 15 sample layer specs SAMPSTRUC (MAX_S_LAYER=15)
						t:		0.0, $			; thickness
						rho:	0.0, $			; density
						z:		intarr(10), $	; Z
						f:		fltarr(10), $	; F
						trace:	bytarr(102), $	; elements
						eqn: { flag: 0UB, $		; (70 bytes)
							z:	intarr(5), $		; (MAX_EQ_EL = 5)
							f:	fltarr(5), $		;
							eqn: 0S, $				;
							par: fltarr(5), $		; (MAX_EQ_PAR = 5)
							vary: 0UB, $			;
							fuzzd: 0.0, $			;
							fuzzs: 0L, $			;
							subThick: 0.0, $		;
							nSub1: 0L}},15), $
					target: 0S, $				; target number
					mode:	bytarr(1), $		; scan mode
					prevmap: bytarr(7), $		; previous map run number
					MeV:	0.0, $				; beam energy
					Z:		0S, $				; Z of particle
					A:		0S, $				; A of particle
					QQ: 	0S, $				; Q of particle
					dt:		0UL, $				; elapsed time (sec)
					q:		ulonarr(4), $		; Q pulses
					q_factor: 0.0, $			; Q scaling factor
					C_per_pulse: 0.0, $			; Q scaler sensitivity
					Qscaler: intarr(10), $		; Dead-time scaler setup info
					TgtPos:	fltarr(3), $		; tgt position
					TgtAng:	fltarr(3), $		; tgt angle
					scan: {	ScanType:		0S, $	; scan type
							ScanSource: 	0S, $	; type of scanning (beam/tgt)
							Pixels: 		0S, $	; number of pixels in original scan
							ClockSource: 	0S, $	; source of pixel clocks
							TickPerPixel: 	0S, $	; number of ticks per pixel
							thisPoint: 		0S, $	; sequence number of point in sequence
							PointsInSequence: 0S, $	; number of points in point sequence
							P0: 		intarr(2), $	; start point of line (x,y pixels)
							P1: 		intarr(2), $	; end point of line (x,y pixels)
							ScanSize:	fltarr(2), $	; scan size (x,y um);
							TotalPixels: 	0UL}, $	; total numnber of pixels in selected scan
					scalers: ulonarr(5), $		; scalers (MAX_BOARDSCALER = 5)
					site:	bytarr(16), $		; site
					QscalerCount:	0ULL }		; long form Q counts.

			adcinfo = replicate( $				; bytarr(234,8)		ADCINFO 9
					{ source:	0S, $			; source type
					HW_chan:	0L,	$			; Hardware channel for this data
					HW_bins:	0L, $			; max number of energy channels
					live:		0S, $			; 1 if ADC active
					cal: 		fltarr(4), $	; cal c0,c1,c2, (c3 = # channels?)
					det_name: 	bytarr(9), $	; detector file name
					det_desc: 	bytarr(80), $	; detector description
					sig: 		0S, $			; radiation type
					geom:		fltarr(6), $	; Geometry (area, beamangle, inplane tilt, normal tilt, distance-offset)
					vary:		intarr(6), $	; varying?
					fw:			fltarr(3), $	; resolution params
					filterable:	0US, $			; Takes filters
					AllowQuadratic: 0US, $		;
					filter: { filt_name: bytarr(9), $	; filename for data
							fz:		intarr(5), $		; filter Z (MAX_F_LAYER = 5)
							ft:		fltarr(5), $		; thickness
							fh:		fltarr(5) }, $		; hole
					timing: { elapsedTime: 0ULL, $		; timing data
							liveTime:	0ULL, $			;
							deadTime:	0ULL, $			;
							normalEvents: 0ULL, $		;
							pileupEvents: 0ULL, $		;
							HwTicksPerSec: 0ULL}, $		;
					DToption:	0L, $			; 0 for Measured DT (default), 1 for collected charge, 2 for custom DT
					CustomDT:	0.0D+0 }, 8)	; Customised DT (0 .. 1)
			ncalsize = 3
			end
		else: begin
			warning,'read_om_header',['Unknown header version number ='+string(header_version), $
							'Need updated file format information from Oxford.','Consult CSIRO.']
			error = 1
			return, 0
			end
	endcase

	readu, unit, rundata
	swap_bytes, rundata, big_endian_data=0

	readu, unit, adcinfo
	swap_bytes, adcinfo, big_endian_data=0
	lmf_cal = adcinfo.cal

	lmf_charge = 0.0

;	For LMF version 4,5, also read pixel table.

	if LMF_version ge 4 then begin
		nxy = 0L
		readu, unit, nxy
		swap_bytes, nxy, big_endian_data=0
		
;		if (nxy lt 1) or (nxy gt 256*256L) then begin
;			warning,'read_om_header',['Strange "nxy" (pixel table length) = '+strtrim(string(nxy),2), '', $
;								'May be a data format issue.']
;			error = 1
;			return, 0
;		endif
;		if (nxy ne 2*(nxy/2)) then begin
;			warning,'read_om_header',['Strange odd "nxy" (pixel table length).', $
;								'May be a data format issue.']
;			error = 1
;			return, 0
;		endif

		table = lonarr(nxy)
		readu, unit, table
		swap_bytes, table, big_endian_data=0
		
		LMF_x = uint( table[0:nxy-1] and '0000FFFF'xUL)
		LMF_y = uint( ishft( table[0:nxy-1], -16) and '0000FFFF'xUL)

	endif else begin
		escale = float(lmf_cal[ncalsize,*]) / 4096
		lmf_cal[0,*] = lmf_cal[0,*] * escale				; ?
		lmf_cal[1,*] = lmf_cal[1,*] * escale
	endelse

	case LMF_version of
		0: begin
			e_mask = '07FF'xu
			ste_mask = '3800'xu
			ste_offset = -11
			end
		1: begin
			e_mask = '0FFF'xu
			ste_mask = '7000'xu
			ste_offset = -12
			end
		2: begin
			e_mask = '0FFF'xu
			ste_mask = '7000'xu
			ste_offset = -12
			end
		3: begin								; still uses X, Y words
			e_mask =     '00FFFFFF'xUL
			ste_mask =   '07000000'xUL
			ste_offset = -24
			count_mask = 'F8000000'xUL
			count_offset = -27
			end
		4: begin								; now uses a 'PixelCount' and
			e_mask =     '00FFFFFF'xUL			; pixel index into XY tables.
			ste_mask =   '07000000'xUL
			ste_offset = -24
			count_mask = 'F8000000'xUL
			count_offset = -27
			lmf_pixels = rundata.scan.pixels
			end
		5: begin								; new Block format using 32-bit "atoms"
			end
		else:
	endcase

	case header_version of
		4: begin
			c_per_pulse = rundata.c_per_pulse * 1.0E6		; microCoulombs per pulse
			lmf_size = float(64*1024L - rundata.size)*0.1	; these values a mystery?
			title = string(rundata.caption)

			x_size = lmf_size[0]							; scan size in um?
			y_size = lmf_size[1]							; scan size in um?
			xsize = fix(round(sqrt(rundata.dScanArea)))
			ysize = fix(round(sqrt(rundata.dScanArea)))
			end
		11: begin
			c_per_pulse = rundata.c_per_pulse * 1.0E6		; microCoulombs per pulse
			title = string(rundata.caption)

			x_size = rundata.scan.ScanSize[0]				; scan size in um?
			y_size = rundata.scan.ScanSize[1]
			lmf_size = [x_size, y_size]		
			xsize = fix(round(sqrt(rundata.scan.TotalPixels)))
			ysize = fix(round(sqrt(rundata.scan.TotalPixels)))			
			end
		12: begin
			c_per_pulse = rundata.c_per_pulse * 1.0E6		; microCoulombs per pulse
			title = string(rundata.caption)

			x_size = rundata.scan.ScanSize[0]				; scan size in um?
			y_size = rundata.scan.ScanSize[1]
			lmf_size = [x_size, y_size]		
			xsize = rundata.scan.Pixels
			ysize = rundata.scan.TotalPixels / rundata.scan.Pixels			
			end
		13: begin
			c_per_pulse = rundata.c_per_pulse * 1.0E6		; microCoulombs per pulse
			title = string(rundata.caption)

			x_size = rundata.scan.ScanSize[0]				; scan size in um?
			y_size = rundata.scan.ScanSize[1]
			lmf_size = [x_size, y_size]		
			xsize = rundata.scan.Pixels
			ysize = rundata.scan.TotalPixels / rundata.scan.Pixels			
			end
		14: begin
			c_per_pulse = rundata.c_per_pulse * 1.0E6		; microCoulombs per pulse
			title = string(rundata.caption)

			x_size = rundata.scan.ScanSize[0]				; scan size in um?
			y_size = rundata.scan.ScanSize[1]
			lmf_size = [x_size, y_size]		
			xsize = rundata.scan.Pixels
			ysize = rundata.scan.TotalPixels / rundata.scan.Pixels			
			end
	endcase

	if c_per_pulse lt 1.0e-8 then begin
		warning,'read_om_header',['Very small "c_per_pulse" = '+strtrim(string(c_per_pulse),2), '', $
					'Check that OM DAQ has been set-up correctly','in order to record integrated charge.']
	endif
	if (xsize lt 2) or (ysize lt 2) then begin
		warning,'read_om_header',['Very small scan "xsize, ysize" = '+strtrim(string(xsize),2)+', '+strtrim(string(ysize),2), '', $
					'Check that OM DAQ has been set-up correctly','for scanning.']
	endif
	
	stat = fstat(unit)

	head = { cal:			lmf_cal, $			; energy cal
			c_per_pulse:	c_per_pulse, $		; charge per pulse
			version:		header_version, $	; header version
			title:			title, $			; run caption
			id:				om_id, $			; OM id
			lmf_version:	lmf_version, $		; LMF file version
			x_size:			x_size, $			; scan size in um?
			y_size:			y_size, $			; scan size in um?
			n_x:			xsize, $			; x pixels
			n_y:			ysize, $			; y pixels
			block_size:		block_size, $		; block size
			file:			stat.name, $		; File name
			bytes:			stat.cur_ptr, $		; number of bytes in header
			size:			stat.size }			; Size of file

	error = 0
	return, head

bad_io:
	error = 1
	return, 0
	end