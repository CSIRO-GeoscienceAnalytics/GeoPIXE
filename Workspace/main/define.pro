function define, image=image, spectrum=spectrum, table=table, $
			cut=cut, old_cut=old_cut, filter=filter, old_filter2=old_filter2, $
			cal=cal, old_filter1=old_filter1, options_image=options_image, $
			maia_channel=maia_channel, maia_struct=maia_struct, maia_port=maia_port, $
			maia_pad=maia_pad, maia_layout=maia_layout, import=import, stack=stack, test=test, $
			maia_control=maia_control, maia_shared1=maia_shared1, maia_version=maia_version, $
			maia_number=maia_number, maia_readout=maia_readout, maia_scan_spec=maia_scan_spec, $
			daq_channel=daq_channel, daq_struct=daq_struct, daq_readout=daq_readout, $
			daq_pad=daq_pad, daq_layout=daq_layout, daq_version=daq_version, daq_number=daq_number, $
			daq_control=daq_control, daq_shared1=daq_shared1, scan_spec=scan_spec, $
			wizard_notify=wizard_notify, old_source1=old_source1, source=source, maia_frame_spec=maia_frame_spec, $
			maia_sample_spec=maia_sample_spec, daq_da_shared1=daq_da_shared1, plot_options=plot_options, $
			correct_yields=correct_yields, operations=operations, old_source2=old_source2, $
			pink=pink

;	Define structures for use in NMP software
;	see "GeoPIXE internal workings.docx"

COMPILE_OPT STRICTARR
common c_null_spec_1, max_history, max_cal,  max_fit
common c_null_image_1, max_image_cal

max_image_cal = 8
max_history = 32
max_cal = 8
max_fit = 32			; 6
def_struct = 0

if n_elements(image) lt 1 then image=0
if n_elements(spectrum) lt 1 then spectrum=0
if n_elements(table) lt 1 then table=0
if n_elements(cut) lt 1 then cut=0
if n_elements(old_cut) lt 1 then old_cut=0
if n_elements(filter) lt 1 then filter=0
if n_elements(old_filter2) lt 1 then old_filter2=0
if n_elements(old_filter1) lt 1 then old_filter1=0
if n_elements(cal) lt 1 then cal=0
if n_elements(options_image) lt 1 then options_image=0
if n_elements(maia_channel) lt 1 then maia_channel=0
if n_elements(maia_control) lt 1 then maia_control=0
if n_elements(maia_version) lt 1 then maia_version=0
if n_elements(maia_shared1) lt 1 then maia_shared1=0
if n_elements(maia_struct) lt 1 then maia_struct=0
if n_elements(maia_port) lt 1 then maia_port=0
if n_elements(maia_layout) lt 1 then maia_layout=0
if n_elements(maia_pad) lt 1 then maia_pad=0
if n_elements(maia_number) lt 1 then maia_number=0
if n_elements(maia_readout) lt 1 then maia_readout=0
if n_elements(maia_scan_spec) lt 1 then maia_scan_spec=0
if n_elements(maia_frame_spec) lt 1 then maia_frame_spec=0
if n_elements(maia_sample_spec) lt 1 then maia_sample_spec=0
if n_elements(import) lt 1 then import=0
if n_elements(stack) lt 1 then stack=0
if n_elements(daq_channel) lt 1 then daq_channel=0
if n_elements(daq_struct) lt 1 then daq_struct=0
if n_elements(daq_readout) lt 1 then daq_readout=0
if n_elements(daq_pad) lt 1 then daq_pad=0
if n_elements(daq_layout) lt 1 then daq_layout=0
if n_elements(daq_version) lt 1 then daq_version=0
if n_elements(daq_number) lt 1 then daq_number=0
if n_elements(daq_control) lt 1 then daq_control=0
if n_elements(daq_shared1) lt 1 then daq_shared1=0
if n_elements(daq_da_shared1) lt 1 then daq_da_shared1=0
if n_elements(scan_spec) lt 1 then scan_spec=0
if n_elements(wizard_notify) lt 1 then wizard_notify=0
if n_elements(source) lt 1 then source=0
if n_elements(pink) lt 1 then pink=0
if n_elements(old_source1) lt 1 then old_source1=0
if n_elements(old_source2) lt 1 then old_source2=0
if n_elements(plot_options) lt 1 then plot_options=0
if n_elements(correct_yields) lt 1 then correct_yields=0
if n_elements(operations) lt 1 then operations=0

;--------------------------------------------------------------------------------------------

if spectrum then begin 

; The SPECTRUM struct for use in Spectrum_Display, etc.

def_struct = { $
		file:'', $							; spec file name
		source:'', $						; source (eg. first .evt file name)
		source2:'', $						; source2 (eg. final .evt file name)
		throttle: '', $						; throttle factors file
		pileup: '', $						; pileup limits file
		linearize: '', $					; linearization function file

		DevObj: obj_new(), $				; device object
		version: 0L, $						; version of read in DAI file.
		label:'', $							; label (eg. X2 spectrum)
		orphan: 0, $						; request to "own me"
		group: 0L, $						; temporary place to store parent widget ID

		sample:'', $						; sample name
		grain:'', $							; grain/point name
		comment:'', $						; user comment
		n_history:0, $						; # history string records
		history: strarr(max_history), $		; history records
	
		cal: {	order:0, $					; energy or distance calibration, poly order
				units:'', $					; calibration units
				poly: fltarr(max_cal)}, $	; polynomial (order+1 terms)
		ecal: {	order:0, $					; energy calibration, poly order
				units:'', $					; calibration units
				poly: fltarr(max_cal)}, $	; polynomial (order+1 terms)
		ecompress: 1L, $					; channel compression of spectrum from original (8192)

		deadtime_correction: 1.0, $			; deadtime correction factor applied.
		pileup_ratio: 0.0, $				; image pileup scaling
		pileup_A4: 1.0, $					; PIXE Fit A[4] multiplier
		FWHM: 0.0, $						; FWHM (eV) @ Mn Ka

		charge: 0.0, $						; integrated charge (uC) (if "live" then set DT correction=1.)
		IC_total: 0.0, $					; total IC count (not deadtime corrected)
		energy: 0.0, $						; beam energy (MeV PIXE; keV SXRF, ...)

		ambient: {	on:		0, $			; ambient conditions detected in spectrum data
					P:		0.0, $			; pressure (mbar)
					T:		0.0 }, $		; temperature (C)
		tube: {	volts:		0.0, $			; lab source tube voltage (kV)
				current:	0.0, $			; lab source tube current (uA)
				time:		0.0 }, $		; acquisition "real" time (use with 'deadtime_correction')

		IC: {	mode:	0, $				; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
				conversion:		0.0, $		; IC count to charge conversion
				pv: {	name:	'', $		; Epics PV name string
						val:	0.0, $		; PV value multipler
						unit:	0.0}}, $	; PV units, range value
		dwell: { on:	0, $				; dwell used (convert IC rate to count in a pixel)
				val:	0.0}, $				; dwell value (ms)
		plist:	ptr_new(), $ 				; ptr to PV list

		x:0.0, $							; X (mm)
		y:0.0, $							; Y (mm)
		z:0.0, $							; Z (mm)
		theta:0.0, $						; theta (degrees)
		phi:0.0, $							; phi (degrees)
		scan: {	x:0.0, $					; X scan (mm)
				y:0.0}, $					; Y scan (mm)
		filter:-1, $						; filter code (-1 means unknown)
		sequence: {	num:0, $				; sequence number, whole
					sub:0}, $			 	; sub sequence fraction

		px_coords:	ptr_new(), $			; X coords array
		x_coord_units: '', $				; X coords units

		processed: 0L, $					; number of events processed
		valid: 0L, $						; number of valid events
		bad_xy: 0L, $						; events with bad X,Y, bits and mismatched station
		clipped: 0L, $						; number of events clipped by DA matrix

		matrix: {	label: '', $			; original DA matrix file label
					file: '', $				; local DA matrix file name
					charge: 0.0, $			; DA matrix fit charge
					mdl: ptr_new() }, $		; pointer to MDL values from DA

		events: 0L, $						; stopped at 'events' events (else zero)
		type:	0L, $						; type of image (0=counts, 1=traverse (conc))
		station:1, $						; station number (start at 1)
		channel:0, $						; station number (redundant?, use station; starts at 0)
		detector: 0L, $						; detector, data type (0=PIXE, 1=PIGE, ..., -1 unknown?)

		array: 0, $							; Flags "Multiple detector" mode.
		multiplicity: 1, $					; detector array multiplicity
		pactive: ptr_new(), $				; pointer to All active ADC channels/stations sorted (only if array=1)

		ystep: 0L, $						; Y step mode used (else X)
		xstep_on: 0L, $						; X,Y step mode used
		xstep: 0L, $						; X (or Y) step count
		step_events: 0L, $					; flags step advance on E events
		step_toggle: 0L, $					; advance position on toggle bit
		toggle_bit: 0L, $					; toggle bit
		toggle_station: 0L, $				; toggle station
		microns: 0.0, $						; traverse step distance

		show: 1, $							; display the spectrum
		showfit: 1, $						; display any overlay fits
		emin: 0.0, $						; minimum energy to display
		emax: 10.0, $						; maximum energy to display

		elow: 0.0, $						; current low energy
		ehigh: 100.0, $						; current high energy
		ylow: 0.0, $						; current low counts
		yhigh: 100.0, $						; current high counts
		log: 1, $							; log/linear mode flag

		xcompress: 1L, $					; x compress on sort
		ycompress: 1L, $					; y compress on sort
		nx: 0L, $							; x pixel range of image / X step
		ny: 0L, $							; y pixel range of image
		show_back:		0, $				; show background DA fit option

		shape: {type:	0, $				; type of region shape (1:Box, 5:Ellipse)
			X: 0.0, $						; X size (microns)
			Y: 0.0 }, $						; Y size (microns)

		size: 0, $							; size of spectrum
		data: ptr_new(), $					; pointer to spectrum fltarr
		has_errors: 0L, $					; flags the presence of errors
		error: ptr_new(), $					; pointer to spectrum error fltarr
		has_mdl: 0L, $						; flags the presence of MDLs
		mdl: ptr_new(), $					; pointer to spectrum MDL fltarr

		max_detectors:	0L, $				; number of detectors in an array (dims for following)
		has_pileup: 0, $					; flags the presence of a pileup per detector histogram
		pileup_loss_det: ptr_new(), $		; pointer to pileup per detector histogram
		has_dead: 0, $						; flags the presence of dead-time per detector histogram
		deadtime_det: ptr_new(), $			; pointer to dead-time per detector histogram

		n_fit: 0, $							; # associated fit spectra
		fit: ptrarr(max_fit) }				; array of pointers to fit spectrum structures
;

;--------------------------------------------------------------------------------------------

endif else if image then begin

; The standard IMAGE struct for Image, Image_Table, etc.

def_struct = { $

; info
		source:'', $						; source (eg. first evt file name)
		source2:'', $						; source2 (eg. final evt file name)
		throttle:'', $						; throttle factors file
		pileup: '', $						; pileup limits file
		linearize: '', $					; linearization function file

		version: 0L, $						; version of read in DAI file.
		file:'', $							; image file name

		sample:'', $						; sample name
		grain:'', $							; grain/point name
		comment:'', $						; user comment
		facility: '', $						; facility string
		endstation: '', $					; endstation name
		energy: 0.0, $						; beam energy (MeV PIXE; keV SXRF, ...)

		n_el: 0L, $							; number of elements/images
		el: ptr_new(), $			; els	; pointer to element names array
		n_attributes: 0L, $					; # additional attribute planes added to element images and n_el

		history: ptr_new(), $		; els	; pointer to an array of pointers (one for each image)
											; which point to string arrays of history records

; device specific
		DevObj: obj_new(), $				; device object

; method
		matrix: {	label: '', $			; original DA matrix file label
					file: '', $				; local DA matrix file name
					charge: 0.0, $			; DA matrix fit charge
					mdl: ptr_new() }, $		; pointer to MDL values from DA
		pda: ptr_new(), $					; pointer to the full DA struct, if present (loaded when needed, e.g. 'analyze_image')

		type:	0L, $						; type of image (0=ppm.uC, 1=mineral fraction, 2=counts, 3=energy)
		mode:	0L, $						; image mode (0=DA, 1=CUTs, 2=CUT energy (STIM), 3=MPDA, 4=cube ... )
		corrected: 0L, $					; flags yield correction
		flatten:	0, $					; indicates that image/flux was "flattened" by flux image

; scanning
		scan: {	x:0.0, $					; X scan size (mm)
				y:0.0, $					; Y scan size (mm)
				origin: { x:0.0, $			; X scan origin (mm)
						y: 0.0 }}, $		; Y scan origin (mm)
		px_coords:	ptr_new(), $			; X coords array
		py_coords:	ptr_new(), $			; Y coords array
		x_coord_units: '', $				; X coords units
		y_coord_units: '', $				; Y coords units

		energy_proxy_axis: 0, $				; axis controlling energy index (0=none, 1=X, 2=Y)
		energies_file:	'', $				; file-name for XANES energies set-up

		dwell: { on:	0, $				; dwell used (convert IC rate to count in a pixel)
				val:	0.0}, $				; dwell value (ms)

		xcompress: 1L, $					; x compressed on sort
		ycompress: 1L, $					; y compressed
		original_xsize: 0L, $				; original (post-compress) size
		original_ysize: 0L, $
		bounds: { valid: 0, $				; bounds valid only for devices that have bounds
				xmin:	0, $				; bounds of image pixel values
				xmax:	0, $                ;  (post-compress, but not offset)
				ymin:	0, $				; these are present regardless of 'valid'
				ymax:	0 }, $
		xsize: 0L, $						; present size of images (may be cut-down if a cluster node)
		ysize: 0L, $						; (post compress) and after scaling
		scaled_x: 1.0, $					; x scaled after sort
		scaled_y: 1.0, $					; y scaled

		sub_region:	0, $					; flags the use of a sub-region
		xoffset:	0L, $					; X offset of this image segment in total image (not compressed)
		yoffset:	0L, $					; Y offset of this image segment ("stripe" in cluster mode)
		x_sub_range: 0L, $					; X size of the full image segment/sub region (not compressed)
		y_sub_range: 0L, $					; Y size of the full image segment/sub region
		
		ystep: 0L, $						; Y step mode
		xstep_on: 0L, $						; X (or Y) step mode used
		xstep: 0L, $						; X (or Y) step count
		step_events: 0L, $					; flags step advance on E events
		step_toggle: 0L, $					; advance position on toggle bit
		toggle_bit: 0L, $					; toggle bit
		toggle_station: 0L, $				; toggle station
		events: 0L, $						; stopped at 'events' events (else zero)
		
; detector
		channel: 0L, $						; ADC channel/station sorted (first active) (start at 0)
		detector: 0L, $						; detector, data type (0=PIXE, 1=PIGE, ...)

		cal: {	order:0, $					; energy calibration, poly order
				units:'', $					; calibration units
				poly: fltarr(max_image_cal+1)}, $	; polynomial (order+1 terms)
		ecompress: 1L, $					; channel compression of spectrum from original (8192)

		array: 0L, $						; Flags "Multiple detector" mode.
		pactive: ptr_new(), $				; pointer to All active ADC channels/stations sorted (only if array=1)
		pcal: ptr_new(), $					; pointer to all active cals in Multiple detector mode (only if array=1)
											; {a:fltarr, b:fltarr, order:1, units:'keV'}

; normalization
		charge: 0.0, $						; integrated charge (uC)

		IC: {	mode:	0, $				; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
				conversion:		0.0, $		; IC count to charge conversion
				pv: {	name:	'', $		; Epics PV name string
						val:	0.0, $		; PV value multiplier
						unit:	0.0}}, $	; PV units, range value
		plist:	ptr_new(), $ 				; ptr to PV list
	
; statistics
		processed: 0LL, $					; number of events processed
		valid: 0LL, $						; number of valid events
		bad_xy: 0LL, $						; events with bad X,Y, bits and mismatched station
		clipped: 0LL, $						; number of events clipped by DA matrix

		hist: ptr_new(), $					; pointer to detector frequency histogram

		has_flux: 0L, $						; flags presence of a flux image array
		flux: ptr_new(), $					; flux array (same size as image)
		raw_flux: ptr_new(), $				; uncorrected flux array

		has_dead: 0L, $						; flags presence of a dead_fraction image array
		dead_fraction: ptr_new(), $			; dead_fraction array (same size as image)
		has_dwell: 0L, $					; flags presence of a dwell image array
		dwell_map: ptr_new(), $				; dwell array (same size as image), if device supports this
		has_pileup: 0L, $					; flags presence of a pileup image array
		pileup_map: ptr_new(), $			; pileup losses array (same size as image), if device supports this
		has_rates: 0L, $					; flags presence of a count-rate (/s) image array
		count_rate_map: ptr_new(), $		; count-rate (/s) array (same size as image), if device supports this

; display
		options: ptr_new(), $		; els	; pointer to an array of image display options structs
		escale: ptr_new(), $		; els	; pointer to an array of error display options structs

		has_preview: 0L, $					; includes a preview image
		preview: ptr_new(), $				; preview image

; maps
		has_phase:	0L, $					; flags presence of a phase map set
		phase: ptr_new(), $					; phase maps, used in MPDA mode
		has_yield:	0L, $					; flags presence of a yield map set
		yield: ptr_new(), $					; yield maps, used in MPDA mode
		
		has_errors: 0L, $					; flags the presence of error images
		error: ptr_new(), $			; els	; pointer to image errors array
		image: ptr_new(), $			; els	; pointer to images array

; internal (the following are part of memory struct, but not read/write to file)

		orphan: 0, $						; request to "own me"
		show_back: 0L, $					; show background option

; modify (used for 'Interelement Operations', 'dynamic_correct')
		modify: {  $
			target:		0, $				; target element index
			source:		0, $				; source element index
			operation:	0, $				; operation (0="off")
			image:		ptr_new(/alloc), $	; filtered source image
			filter:		0, $				; filter selection (0="off")
			anchour:	-1, $				; anchour position
			scale:		1.0, $				; scale factor
			strength:	1.0}, $				; slider strength

; temp
		temp: {	$
			valid: 0, $						; flags temp entries as valid (clear to rebuild temps)
			total_flux: 0.0, $				; temporary store of total flux
			total_pixels: 0L, $				; temporary count of non-zero pixels
			flux_map: ptr_new(), $			; temporary flux map
			charge: 0.0, $					; temporary total charge
			charge_map: ptr_new()}, $		; temporary charge map

; undo (used for image processing for undo)			
		undo: {  $							; undo single element operation buffer
			id:  0L, $						; current element index
			OK: 0L, $						; image buffer is valid and current
			min: 0.0, $						; image minimum
			max: 0.0, $						; image max
			emin: 0.0, $					; error min
			emax: 0.0, $					; error max
			history: ptr_new(), $	; els	; pointer to old history
			has_errors: 0L, $				; flags the presence of error images
			error: ptr_new(), $		; els	; pointer to image errors array
			image: ptr_new() } }	; els	; pointer to images array


;--------------------------------------------------------------------------------------------

endif else if stack then begin


def_struct = {	source:		'', $			; first DAI file in set
		source2:	'', $					; source2 (eg. final evt file name)
		throttle:	'', $					; throttle factors file
		pileup: 	'', $					; pileup limits file
		linearize: 	'', $					; linearization function file

		DevObj: 	obj_new(), $			; device object
		version: 	0L, $					; version of read in DAI file.
		file:		'', $					; image file name
		orphan: 	0, $					; request to "own me"
		
		sample:		'', $					; sample string from first file
		grain:		'', $					; grain string 
		comment:	'', $					; comment string 
		facility: 	'', $					; facility string
		endstation: '', $					; endstation name
		cal:	{	order:0, $				; energy calibration, poly order
					units:'', $				; calibration units
					poly: fltarr(max_image_cal+1)}, $	; polynomial (order+1 terms)
		ecompress: 	1L, $					; channel compression of spectrum from original (8192)

		xanes: 		1, $					; Flags that this struct is some 3D stack (not just XANES now),
											; such as a XANES 3D image stack, tomo ( use 'tag_present()' = 1)
		stack_type:	0, $					; type of stack (0=XANES, 1=Tomo, 2=XANES before collapse)
		el:			'', $					; element for stack
		ixanes:		0, $					; element index for stack element
		energies_file:	'', $				; file-name for XANES energies set-up
		
		scan: 	{	x:0.0, $				; X scan (mm)
					y:0.0, $				; Y scan (mm)
					z:0.0, $				; Z scan
					origin: { x:0.0, $		; X scan origin (mm) in global stage coordinates
							y: 0.0, $		; Y scan origin (mm)
							z: 0.0 }}, $	; Z scan origin
		px_coords:	ptr_new(), $			; X coords array
		py_coords:	ptr_new(), $			; Y coords array
		pz_coords:	ptr_new(), $			; Z coords array (energy: MeV ions, keV X-rays, angles, ...)
		x_coord_units: '', $				; X coords units
		y_coord_units: '', $				; Y coords units
		z_coord_units: '', $				; Z coords units
		pz_label:	ptr_new(), $			; Z additional label info

		processed: 0LL, $					; number of events processed
		valid: 0LL, $						; number of valid events
		bad_xy: 0LL, $						; events with bad X,Y, bits and mismatched station
		clipped: 0LL, $						; number of events clipped by DA matrix

		matrix: {	label: '', $			; original DA matrix file label
					file: '', $				; local DA matrix file name
					charge: 0.0, $			; DA matrix fit charge
					mdl: ptr_new() }, $		; pointer to MDL values from DA

		history: 	ptr_new(), $	; xZ	; pointer to an array of pointers (one for each image)
											; which point to string arrays of history records

		options: 	ptr_new(), $	; xZ	; pointer to an array of image display options structs
		escale: 	ptr_new(), $	; xZ	; pointer to an array of error display options structs

		show_back: 	0L, $					; show background option

		xcompress: 	1L, $					; x compressed on sort
		ycompress: 	1L, $					; y compressed
		zcompress: 	1L, $					; z compressed
		original_xsize: 0L, $				; original (post-compress) size
		original_ysize: 0L, $
		original_zsize: 0L, $
		scaled_x: 	1.0, $					; x scaled after sort
		scaled_y: 	1.0, $					; y scaled
		bounds:	{	valid: 0, $				; bounds valid only for devices that have bounds
					xmin:	0, $			; bounds of image pixel values
					xmax:	0, $               ;  (post-compress)
					ymin:	0, $			; these are present regardless of 'valid'
					ymax:	0 }, $

		xsize: 		0L, $					; present size of images
		ysize: 		0L, $					; (post compress) and after scaling
		zsize: 		0L, $					; number of E stacks

		sub_region:	0, $					; flags the use of a sub-region
		xoffset:	0L, $					; X offset of this image segment in total image (not compressed)
		yoffset:	0L, $					; Y offset of this image segment ("stripe" in cluster mode)
		x_sub_range: 0L, $					; X size of the full image segment/sub region (not compressed)
		y_sub_range: 0L, $					; Y size of the full image segment/sub region
		
		ystep: 		0L, $					; Y step mode
		xstep_on: 	0L, $					; X (or Y) step mode used
		xstep: 		0L, $					; X (or Y) step count
		step_events: 0L, $					; flags step advance on E events
		step_toggle: 0L, $					; advance position on toggle bit
		toggle_bit: 0L, $					; toggle bit
		toggle_station: 0L, $				; toggle station
		events: 	0L, $					; stopped at 'events' events (else zero)

		type:		0L, $					; type of image (0=ppm.uC, 1=mineral fraction, 2=counts, 3=energy)
		mode:		0L, $					; image mode (0=DA, 1=CUTs, 2=CUT energy (STIM), ... )
		channel: 	0L, $					; ADC channel/station sorted (first active) (start at 0)
		detector: 	0L, $					; detector, data type (0=PIXE, 1=PIGE, ...)

		array: 		0L, $					; Flags "Multiple detector" mode.
		pactive: 	ptr_new(), $			; pointer to All active ADC channels/stations sorted (only if array=1)
		pcal: 		ptr_new(), $			; pointer to all active cals in Multiple detector mode (only if array=1)
											; {a:fltarr, b:fltarr, order:1, units:'keV'}

		charge: 	0.0, $					; integrated charge (uC)

		IC:	{	mode:	0, $				; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
				conversion:		0.0, $		; IC count to charge conversion
				pv: {	name:	'', $		; Epics PV name string
						val:	0.0, $		; PV value multipler
						unit:	0.0}}, $	; PV units, range value
		dwell: { on:	0, $				; dwell used (convert IC rate to count in a pixel)
				val:	0.0}, $				; dwell value (ms)
		plist:		ptr_new(), $ 			; ptr to PV list

		temp: {	$
				valid: 0, $					; flags temp entries as valid (clear to rebuild temps)
				total_flux: 0.0, $			; temporary store of total flux
				total_pixels: 0L, $			; temporary count of non-zero pixels
				flux_map: ptr_new(), $		; temporary flux map
				charge: 0.0, $				; temporary total charge
				charge_map: ptr_new()}, $	; temporary charge map
			
		undo: {  $							; undo single element operation buffer
				id:  0L, $					; current element index
				OK: 0L, $					; image buffer is valid and current
				min: 0.0, $					; image minimum
				max: 0.0, $					; image max
				emin: 0.0, $				; error min
				emax: 0.0, $				; error max
				history: ptr_new(), $	; xZ; pointer to old history
				has_errors: 0L, $			; flags the presence of error images
				error: ptr_new(), $		; xZ; pointer to image errors array
				image: ptr_new() }, $	; xZ; pointer to images array

		has_flux: 	0L, $					; flags presence of a flux image array
		flux: 		ptr_new(), $			; flux array (all stack planes)
		flatten:	1, $					; indicates that all images were "flattened" by flux image (and between planes)

		has_dead: 0L, $						; flags presence of a dead_fraction image array
		dead_fraction: ptr_new(), $			; dead_fraction array (same size as image)
		has_dwell: 0L, $					; flags presence of a dwell image array
		dwell_map: ptr_new(), $				; dwell array (same size as image), if device supports this
		has_pileup: 0L, $					; flags presence of a dwell image array
		pileup_map: ptr_new(), $			; pileup losses array (same size as image), if device supports this
		has_rates: 0L, $					; flags presence of a count-rate (/s) image array
		count_rate_map: ptr_new(), $		; count-rate (/s) array (same size as image), if device supports this

		has_phase:	0L, $					; flags presence of a phase map set
		has_yield:	0L, $					; flags presence of a yield map set

		has_preview: 0L, $					; includes a preview image
		preview: 	ptr_new(), $			; preview image
		has_errors: 0L, $					; flags the presence of error images
		error: 		ptr_new(), $			; pointer to image errors array
		image:		ptr_new() }				; poinnter to 3D stack array.


;--------------------------------------------------------------------------------------------

endif else if table then begin

; The TABLE entry struct for Image_Table

def_struct = {	$
		index:			-1L, $			; original index in table (must be set on read/analyze)
		mode:			0, $			; mode (0:image regions, 1:corr splines)
		file:			'', $			; file name for images
		region_file:	'', $			; filename for regions
		source:			'', $			; source (eg. final .evt file name)
		source2:		'', $			; source2 (eg. final .evt file name)
		throttle:		'', $			; throttle factors file
		pileup: 		'', $			; pileup limits file
		linearize: 		'', $			; linearization function file

		DevObj: 		obj_new(), $	; device object

		matrix:			'', $			; file name of DA matrix

		analyze_type:	intarr(2), $	; analyze type
		el_shown:		'', $			; element displayed when sigma pressed
		elx:			'', $			; X corr axis element (mode 1)
		ely:			'', $			; Y corr axis element (mode 1)
		note:			'', $			; Note about region
		n_el:			0, $			; number of elements
		el:				ptr_new(), $	; element names
		conc:			ptr_new(), $	; conc values
		error:			ptr_new(), $	; error values
		mdl:			ptr_new(), $	; MDL values
		
		sd:				ptr_new(), $	; SD values			(not saved, used for Wizards)
		relsd:			ptr_new(), $	; SD/Poisson values	(not saved, used for Wizards)
		
		centroid: 		ptr_new(), $	; centroid (pixels)
		phase:			ptr_new(), $	; phase proportions, if phase maps are present
		ayield:			ptr_new(), $	; average yield within and across pixel in region (keep for old MPDA)
		poverlay:		ptr_new(), $	; pointer to spectra overlays per element	;@3-16
		
		q:				ptr_new(), $	; q array
		nx:				0L, $			; x present size of q,image
		ny:				0L, $			; y
		xoffset:		0L, $			; X offset of this image segment in total image
		yoffset:		0L, $			; Y offset of this image "stripe"
		scanx:			0.0, $			; X scan (mm)
		scany:			0.0, $			; Y scan (mm)
		xcompress:		1, $			; x compressed in sort
		ycompress:		1, $			; y compressed
		original_xsize:		0L, $		; original (post-compress) size
		original_ysize: 	0L, $
		scaled_x:		1.0, $			; x scaled in image
		scaled_y:		1.0, $			; y scaled
		show_back:		0, $			; show background option
		sample:			'', $			; sample name
		grain:			'', $			; grain name
		comment:		'', $			; a comment
		channel:		0, $			; ADC channel (from Image, start at 0)
		detector:		0, $			; detector type (0=PIXE, 1=PIGE)
		ystep: 			0L, $			; Y step mode used (else X)
		xstep_on:		0, $			; was xstep/ystep used
		xstep:			0, $			; xstep counts
		step_events:	0, $			; flags step advance by events
		step_toggle:	0L, $			; flags step advance by toggle bit
		toggle_bit:		0L, $			; toggle bit
		toggle_station:	0L, $			; toggle station
		events:			0L, $			; stopped at 'events'
		cal_a:			1.0, $			; cal_a
		cal_b:			0.0, $			; cal_b
		ecompress:		1L, $			; energy spectrum compression factor
		charge:			0.0, $			; charge for region
		IC_total: 		0.0, $			; IC count for region

		IC: {	mode:	0, $				; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
				conversion:		0.0, $		; IC count to charge conversion
				pv: {	name:	'', $		; Epics PV name string
						val:	0.0, $		; PV value multipler
						unit:	0.0}}, $	; PV units, range value
		dwell: { on:	0, $				; dwell used (convert IC rate to count in a pixel)
				val:	0.0}, $				; dwell value (ms)
		plist:			ptr_new(), $ 		; ptr to PV list
		array:			0, $			; Is this a detector array result
		pactive:		ptr_new(), $	; Pointer to the active channels
		pcal:			ptr_new(), $	; Pointer to the active energy cals
		pmark:			ptrarr(2)}		; points to marker points

;--------------------------------------------------------------------------------------------

endif else if old_cut then begin

; The CUT entry struct for Cuts (old format)

def_struct = {cut,  $
		file:'', $						; cuts file name
		el:'', $						; cut name (element)
		units:'', $						; energy units string
		low:0.0, $						; low energy of cut
		high:0.0, $						; high energy of cut
		blow:0.0, $						; low background cut
		bhigh:0.0 }						; high background energy


;--------------------------------------------------------------------------------------------

endif else if cut then begin

; The CUT entry struct for Cuts

def_struct = {cut2,  $
		file:'', $						; cuts file name
		el:'', $						; cut name (element)
		units:'', $						; energy units string
		cal_a: 0.0, $					; cal_a
		cal_b: 0.0, $					; cal_b

		type: 0, $						; type of cut (0:Cut, 1:X0-X5, 2:old)
		x: intarr(6), $					; actual channel markers
		e: fltarr(6), $					; energies of markers
		dleft: 0.0, $					; lever factor left
		dright: 0.0, $					; lever factor right

		left: 0.0, $					; left (X0-X1) sum
		right: 0.0, $					; right (X4-X5) sum
		sum: 0.0, $						; peak (X2-X3) sum
		back: 0.0, $					; background (X2-X3) sum
		area: 0.0, $					; peak, less background
		error: 0.0 }					; area error

;--------------------------------------------------------------------------------------------

endif else if old_filter1 then begin

; The FILTER struct for filters

def_struct = {filter, $
		N: 0, $							; number of species
		Z: intarr(32), $				; atomic numbers
		F: fltarr(32), $				; atomic fractions
		thick: 0.0, $					; thickness, ALWAYS in mg/cm^2
		pinhole: 0, $					; 1=has a pinhole, 2=Bragg
		pinratio: 0.0, $				; solid-angle ratio (>1)
		name: '' }						; title


;--------------------------------------------------------------------------------------------

endif else if old_filter2 then begin

; The FILTER struct for filters

def_struct = {filter2, $
		N: 0, $							; number of species
		Z: intarr(32), $				; atomic numbers
		F: fltarr(32), $				; atomic fractions
		thick: 0.0, $					; thickness, ALWAYS in mg/cm^2
		pinhole: 0, $					; 1=has a pinhole
		pinratio: 0.0, $				; solid-angle ratio (>1)
		name: '', $						; title

		microns: 0, $					; thick WAS ENTERED in microns
		density: 0.0, $					; density
		formula: '', $					; chemical formula
		weight: 0 }						; 1=wt%, 0=at. fraction outside "()"

;--------------------------------------------------------------------------------------------

endif else if filter then begin

; The FILTER struct for filters

def_struct = {filter3, $
		N: 0, $							; number of species
		Z: intarr(32), $				; atomic numbers
		F: fltarr(32), $				; atomic fractions
		thick: 0.0, $					; thickness, ALWAYS in mg/cm^2
		pinhole: 0, $					; 1=has a pinhole
		pinratio: 0.0, $				; solid-angle ratio (>1)
		name: '', $						; title

		microns: 0, $					; thick WAS ENTERED in microns
		density: 0.0, $					; density (used if /microns on entry)
		formula: '', $					; chemical formula
		weight: 0, $					; 1=wt%, 0=at. fraction outside "()"

		bragg: {bragg, $
				Eopt:	0.0, $			; Bragg: center
				a0:		0.0, $			;		amplitude
				a2a:	0.0, $			; 		left wdith
				a2b:	0.0, $			;		right width
				a3:		0.0, $			;		baseline
				a4:		0.0 } $			; 		ramp slope
		}

;--------------------------------------------------------------------------------------------

endif else if cal then begin

; The CAL struct for use in Spectrum_Display, etc.

def_struct = { $
		order:		1, $					; energy or distance calibration, poly order
		units:		'channel', $			; calibration units
		poly: 		fltarr(max_cal) $		; polynomial (order+1 terms)
	}
def_struct.poly[0:1] = [0.0,1.0]

;--------------------------------------------------------------------------------------------

endif else if options_image then begin

; The OPTIONS struct for use in Image_Display, and used above in Image struct, etc.

def_struct =  {	min:	0.0, $				; minimum value in image
				max:	0.0, $				; maximum value in image
				bottom:	0.0, $				; bottom slider value to view
				top:	100.0, $			; top slider value to view
				interp:	1, $				; interpolate images
				log:	0 }					; log scale ? (0=lin, 1=log, 2= sqrt)

;--------------------------------------------------------------------------------------------

endif else if maia_port then begin

; The Maia command socket port parameters struct for use in Maia_Setup, etc.

def_struct =  {	$
		open:		0, $					; flags port open
		attempts:	0, $					; times opened.
		retries:	5, $					; maximum retries
		token:		'@', $					; command line commencement token
		unit:		0L, $					; IDL unit number
		ip:			'', $					; socket IP address
		client:		'', $					; client name
		port:		0L, $					; port number
		connect_timeout: 0, $				; connect timeout
		write_timeout:	0, $				; write timeout
		read_timeout:	0, $				; read timeout
		current:	0L, $					; current line in executing script
		last:		0L, $					; last line
		time:		0.1, $					; Timer delay time
		version:	0, $					; software version
		n_detectors:	1, $				; number of detectors supported
		last_error:	0, $					; flags a quiet error
		last_command: '', $					; last error command
		last_message: '', $					; last error returned
		pval: ptr_new(ptrarr(100)), $		; script get return target ptr buffer
		pindex: ptr_new(lonarr(100)), $		; script get return target index buffer
		ps: ptr_new(strarr(100)) $			; script buffer
			}

;--------------------------------------------------------------------------------------------

endif else if maia_pad then begin

; The Maia pad layout parameters struct for use in Maia_Setup, etc.

def_struct =  {	$
				index:	0, $		; detector number
				x:		0.0, $		; X coordinate (mm)
				y:		0.0, $		; Y 
				z:		0.0, $		; Z offset from planar
				width:	0.0, $		; width (mm)
				height:	0.0, $		; height
				tilt:	0.0, $		; tilt angle (degrees from normal)
				bad:	0, $		; 0 good, 1 FWHM high, 2 bad
				FWHM:	0.0, $		; FWHM (Mn) eV
				Hermes:	0, $		; Hermes/Scepter ASIC number
				Quadrant:0, $		; Quadrant #
				radial:	0, $		; Radial #
				column:	0, $		; Col # from left
				row:	0}			; Row # from top
				
;--------------------------------------------------------------------------------------------

endif else if maia_layout ge 1 then begin

; The Maia pad layout parameters struct for use in Maia_Setup, etc.

N = maia_layout
def_struct =  {	$
			title:		'', $		; comment string
			file:		'', $		; file name
			N:			N, $		; number of detector pads
			start:		0, $		; detector pad number start/offset (def 0)
			shape:		0, $		; shape of detectors (0=circ, 1=square)
			threshold:	0.0, $		; FWHM threshold (eV)
			symmetry:	0, $		; symmetry modulo number
			mirrorX:	0, $		; mirror in X
			mirrorY:	0, $		; mirror in Y
			reorient:	0, $		; Re-orient/rotate the detector by 90 degrees
			veto:		0, $		; veto high FWHM above threshold
			data:		replicate(define(/maia_pad),N), $		; pad coordinate data (mm)
			ref:		intarr(N+10) }	; cross-reference from detector # to table indx.
									; allow more space for offset 'start'
				
;--------------------------------------------------------------------------------------------

endif else if maia_version ge 1 then begin

; Version numbers for Maia components

	def_struct =  { dam:	2, $					; detector analogue module version
					hermes:	3, $					; Hermes version
					scepter: 7, $					; Scepter version
					ddm:	0, $					; detector digital module version
					dbpm:	0, $					; detector support module version
					software: 0L }					; software version

;--------------------------------------------------------------------------------------------

endif else if maia_number then begin

; Feature numbers for Maia components

	def_struct =  	{	da:		32, $				; number of DA accumulators available
						et2d:	0, $				; number of ET2D accumulators available
						roi:	0, $				; number of ROI accumulators available
						spectra: 0, $				; number of Group Spectra accumulators available
						timer:	2}					; number of Timers available
	
;--------------------------------------------------------------------------------------------

endif else if maia_readout then begin

; Maia readout enables

	def_struct = {	event:			1, $			; event enable
					photon:			1, $			; photon enable
					accum:			1, $			; accumulators enable
					activity:		1, $			; activity enable
					scepter:		1, $			; scepter
					RR_enable:		[1,1,1], $		; RR enable
					Quad_enable:	[1,1,1,1] }		; Quadrant enable
	
;--------------------------------------------------------------------------------------------

endif else if maia_channel ge 1 then begin

; The Maia single channel parameters struct for use in Maia_Setup.
; Channels are in table index order.
; Add new entries within each {} to bottom to preserve 'table[]' par indices,
; which index parameters within these structs, as used in the Maia_setup summary table.

N = maia_channel
def_struct =  replicate( {	$
		disable:		0, $						; channel disable (not used; use enable.ECH from background to *(*pstate).pdisable)
		hermes:	{time:	3, $						; Hermes peaking time (time)
				gain:	0, $						; hermes gain (gain)
				eblk:	0, $						; Hermes bias leakage control (eblk)
				elk:	0}, $						; Hermes leakage monitor (elk)
		scepter: {tdm:	3, $						; scepter timing mode (tdm)
				tds:	0, $						; scepter TAC slope (tds)
				tos:	0, $						; scepter TAC timeout (tos)
				trk:	0, $						; scepter simultaneous catching mode (trk)
				trke:	0, $						; scepter enhanced simultaneous catching mode (trk)
				trim:	0.0, $						; scepter6 trim DAC (4 bits, -150 mV-0) (V)
				thresh:	0.5, $						; scepter threshold DAC (10 bits, 0-2V) (V)
				clock:	3.0, $						; scepter quadrant RR clock (MHz)
				thpd:	0.0, $						; scepter7 PD trim (4 bits, -150 mV-0) (V)
				filt:	0, $						; scepter6 additional filtering enable
				tcm:	0}, $						; scepter6 comparator multi-fire suppression (0=0, 1=100ns, 2=1us, 3=2us)
		cal: {	a:		1.0, $						; energy calibration A (gain) (keV/channel)
				b:		0.0}, $						; energy calibration B (offset) (keV)
		trim: {	E: {a:	1.0, $						; E gain trim A (gain), E' = a*E + b
					b:	0.0}, $						; E gain trim B (offset) 
				T: {a:	1.0, $						; T gain trim A (gain), T' = a*T + b 
					b:	0.0}} $						; T gain trim B (offset)
 			}, N)

;--------------------------------------------------------------------------------------------

endif else if maia_control ge 1 then begin

; The Maia detector control and readback parameters

def_struct = {	peltier:	0.0, $				; Peltier current (A)
				peltier_supply: 0.0, $			; readback of peltier supply voltage
				peltier_monitor: 0.0, $			; readback of peltier current
				peltier_cool_max: 2.0, $		; maximum Peltier setting for cooling
				peltier_bake_max: 1.0, $		; maximum Peltier current in bake-out mode
				guard:		0.0, $				; Guard voltage
				guard_max:	3.0, $				; maximum detector guard (V)
				bias:		0.0, $				; detector set bias (V)
				bias_min:	15.0, $				; minimum detector bias (V)
				bias_max:	160.0, $			; maximum detector bias (V)
				bias_monitor: 0.0, $			; detector readback bias (V)
				bias_rate:	0.5, $				; maximum rate of change of detector bias (V/s)
				leakage:	0.0, $				; detector leakage (uA)
				temp: { detector:	0.0, $		; Wafer T readback (C)
						hermes:		0.0, $		; HERMES temp (C)
						water:		0.0, $		; Water temp (C)
						coldtrap:	0.0, $		; Coldtrap temp (C)
						mosfet:		0.0, $		; Mosfet temp (C)
						hymod_FPGA:	0.0, $		; HYMOD FPGA temp (C)
						hymod_CPU:	0.0, $		; HYMOD CPU temp (C)
						FPGA:		0.0}, $		; FPGA temp (C)
				status: { link:		0, $		; DDM to Hymod Link status
						link_rate:	0L, $		; link rate /s
						event_rate: 0L, $		; event rate /s
						discard_rate: 0L, $		; event discard rate /s
						link_erate:	fltarr(4), $ ; link error rate (estimates per second)
						charge_rate: 0.0, $		; flux0 count rate Q/s
						flux1_rate: 0.0, $		; flux1 count rate /s
						blog:		0, $		; Kandinski blog enabled
						bake:		0, $		; bake enable Peltier status
						bpinterlock_uptime: 0.0D0, $	; Interlock uptime (sec since 1/1/1970)
						bpinterlock_downtime: 0.0D0, $	; Interlock downtime (sec since 1/1/1970)
						link_uptime: 0.0D0, $			; DDM link uptime (sec since 1/1/1970)
						main_uptime: 0.0D0, $			; Kandinski up time (sec UTC)
						fs_time: 	0L, $		; time available in file-system
						fs_free: 	0ULL, $		; available in file-system
						fs_size:	0ULL}, $	; file-system size
				vcc:		0.0, $				; DDM Vcc (V)	
				oan:		0.0, $				; ELK or OAN baseline ADC from Hermes
				linkloss:	0L, $				; Link losses
				interlock:	0, $				; Interlock status
				aux:		0.0, $				; AUX ADC from Scepter
				monitor:	0.0 }				; DAM monitor ADC

;--------------------------------------------------------------------------------------------

endif else if maia_struct ge 1 then begin

; The Maia 384 parameters struct for use in Maia_Setup. 
; In the case of the 'channel' array of structs, these are in CSV table
; order (not [necessarily] detector number order).
; 
; N.B "define" is part of GeoPIXE, and so GeoPIXE needs to be recompiled
; for new additions here.

N = maia_struct
channel_pars = define(maia_channel=N)
control_pars = define(/maia_control)
version_pars = define(/maia_version)
number_pars = define(/maia_number)

group_pars = {	title:	'', $							; Group title string
				table:	bytarr(N), $					; detectors assigned to a group (0, 1 values)
				et_mode: 0, $							; E or T mode for spectra
				pileup:	0, $							; enable pileup rejection for this group
				throttle: 0 }							; enable Throttling for this group
				
def_struct = { file:	'', $							; Maia parameters file
			DevObj:		obj_new(), $					; local Maia device object
			run: {		number:	0L, $					; blog run number
						segment: 0L, $					; segment number
						discard: 0, $					; discard on, not running to disk
						last_run: 0L, $					; previous run number
						last_discard: 0, $				; previous discard state
						error: { bits:	0L, $			; accumulator error bits
								MTC:	0, $			; missed trigger count
								OFV:	0, $			; accumulator overflow flag
								ERR:	0}, $			; accumulator error flag
						group: '', $					; setgroup dir name root
						project: '', $					; setproject dir name root
						X: 0.0, $						; present position
						Y: 0.0, $						;
						Z: 0.0, $
						energy: 0.0, $					; beam energy from Epics (keV)
						rate: 0.0}, $					; byte rate / s
			identity: { dam:	'', $					; detector analogue module identity string
						ddm:	'', $					; detector digital module identity string
						dbpm:	'', $					; detector support module identity
						library:	'', $				; version of Fortran libraries
						software: '' , $				; software identity string
						blog: '' }, $					; blog server HYMOD connected to string
			status: {	blog_error: ''}, $				; blog error
			version:  	version_pars, $					; software version
			number:		number_pars, $					; number of things
			n_detectors:		N, $					; number of detecttors (1-384)
			channel: 	channel_pars, $					; up to 384 channel parameters (CSV index order)
			throttle: { on:		0, $					; throttle on
						factors: bytarr(4096), $		; throttle spectrum
						file:	''}, $					; throttle file
			pileup: { 	on:		0, $					; pileup mode enabled
						limits: { plow:	ptr_new(/allocate_heap), $	; low pileup limits
								phigh: ptr_new(/allocate_heap)}, $	; high pileup limits
						file:	''}, $					; pileup limits file
			Linear: {	on:		0, $					; linearization on
						file:	''}, $					; linearization file
			Trim: {		on:		0, $					; gain trimming ON
						file:	'', $					; gaintrim spec file
						file2:	''}, $					; gaintrim spec file2 (optional)
			Scan: {		on:		0, $					; scan is active
						X:		0L, $					; scan X size
						Y:		0L, $					; scan Y size
						origin: {	X:	0.0, $			; scan origin X
									Y:	0.0, $			; scan origin Y
									Xunit: '', $		; scan X units
									Yunit: ''}, $		; scan Y units
						pitch:	{	X:	0.0, $			; scan pitch X
									Y:	0.0, $			; scan pitch Y
									Xunit: '', $		; scan pitch X units
									Yunit: ''} }, $		; scan pitch Y units
			Deadtime: {	on:		0, $					; deadtime enabled
						auto:	1, $					; set DTcalA based on Scepter TDS and Version
						file:	'', $					; file (not used)?
						cal: {	a:	0.1,  $				; conversion from T to time clicks (ns)
								b:	0.0}}, $
			DA:		{	on: 	0, $					; DA on
						N:		32, $					; number of DA elements
						scale:	fltarr(32), $			; scale factors for each DA record
						name:	strarr(32), $			; element name strings
						parray:	ptr_new(/allocate_heap), $	; pointer to 'matrix.array' struct
						memory: {	X:	0, $			; shared DA memory X size
									Y:	0 }, $			; shared DA memory X size
						axes:	[0,1,2], $				; axis redirection
						file:	'', $					; DA file
						save:	0, $					; enable auto saves
						save_path:	''}, $				; DA auto save path
			IC: {	mode:	1, $						; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
					conversion:		0.0, $				; IC count to charge conversion
					pv: {	name:	'', $				; Epics PV name string
							val:	0.0, $				; PV value multipler
							unit:	0.0}, $				; PV units, range value
					plist:	ptr_new(/allocate_heap), $	; ptr to PV list
					remote:	0}, $						; 0:local, 1:remote, inherits from IC0, etc.
			IC0: {	pv: {	name:	'', $				; scaler name string connected to FC0
							val:	0.0, $				; value multipler
							unit:	0.0}, $				; units, range value
					remote:	0}, $						; 0:local, 1:set from valid Kandinski
			IC1: {	pv: {	name:	'', $				; scaler name string connected to FC0
							val:	0.0, $				; value multipler
							unit:	0.0}, $				; units, range value
					remote:	0}, $						; 0:local, 1:set from valid Kandinski
			ICE: {	pv: {	name:	'', $				; Epics PV name string
							val:	0.0, $				; PV value multipler
							unit:	0.0}}, $			; PV units, range value
			ROI:	{	on:		0, $					; ROI on
						file:	'', $					; ROI limits file
						rate:	0.0 }, $				; ROI count rate
			Cal:	{	file:	'', $					; energy calibration spec file
						mode:	0}, $					; cal mode (0=individual, 1=single for all)
			Groups: {	on:		0, $					; groups ON
						spectra: 12, $					; number of groups in use
						file:	'', $					; groups file
						group:	replicate(group_pars,16)}, $	; detector groups
			control: 	control_pars, $					; control and readback parameters	
			chart: {	on:		1} $					; chart logging enabled
			}

;--------------------------------------------------------------------------------------------

endif else if maia_shared1 ge 1 then begin

	N = maia_shared1
	NDA = 32
	n_file = 512
	n_dainfo = 5000
	n_chips = (N/32) > 1
	channel_pars = define(maia_channel=N)
	control_pars = define(/maia_control)
	version_pars = define(/maia_version)
	number_pars = define(/maia_number)
	readout_pars = define(/maia_readout)
	layout_data = replicate(define(/maia_pad),N)

def_struct = { $
			n_detectors:	N, $						; # detectors
			version:		version_pars, $				; version numbers
			identity: { dam: bytarr(n_file)}, $			; Maia DAM identity string
			number:			number_pars, $				; number of things
			layout_data:	layout_data, $				; pad coordinate data (mm)
			channel: 		channel_pars, $				; up to 384 channel parameters
			control: 		control_pars, $				; control and readback parameters	
			throttle: {	factors: bytarr(4096)}, $		; Throttle factors
			pileup: { limits: {low: intarr(4096), $		; pileup limits
							high: intarr(4096) }}, $
			readout:	 readout_pars, $				; Maia readout enables
			enable: {	synth:	0, $					; synth pulser
						pulser: 0, $					; pulser
						ECH: bytarr(N), $				; Hermes ECH channel disables (CSV index order)
						EAN: bytarr(N), $				; Hermes EAN debug selections (CSV index order)
						EBLK: bytarr(N), $				; Hermes EBLK selections (CSV index order)
						LOCK: bytarr(n_chips), $		; Scepter LOCK
						linear: 0, $					; Linearize
						gaintrim: 0, $					; Gaintrim
						pileup: 0, $					; Pileup
						throttle: 0, $					; Throttle
						pixel: 0, $						; pixel
						deadtime: 0, $					; deadtime blocks
						roi: 0, $						; ROI enable
						groups: 0}, $					; Group spectra enable
			info: 	{	roi: bytarr(n_file), $			; ROI file as byte array
						da: bytarr(n_dainfo), $			; DA info as byte array
						deadtime: bytarr(n_file), $		;
						cal: bytarr(n_file), $			; cal file
						gaintrim: bytarr(2*n_file), $	; gaintrim files
						linear: bytarr(n_file), $		; linearize file
						pileup: bytarr(n_file), $		; pileup file
						throttle: bytarr(n_file), $		; throttle file
						group: bytarr(n_file)}, $		; group file
			DA:	{	on: 	0, $						; DA on
					N:		NDA, $						; number of DA elements
					scale:	fltarr(32), $				; scale factors for each DA record
					bname:	bytarr(32,32), $			; element name strings
					rGamma:	fltarr(N,NDA) }, $			; rGamma array
			Scan: {		X:		0L, $					; scan X size
						Y:		0L, $					; scan Y size
						dwell: { on:	0, $			; dwell mode on
								val:	0.0}, $			; dwell time (ms)
						origin: {	X:	0.0, $			; scan origin X
									Y:	0.0, $			; scan origin Y
									bXunit: bytarr(32), $		; scan X units
									bYunit: bytarr(32)}, $		; scan Y units
						pitch:	{	X:	0.0, $			; scan pitch X
									Y:	0.0, $			; scan pitch Y
									bXunit: bytarr(32), $		; scan pitch X units
									bYunit: bytarr(32)}}, $		; scan pitch Y units
			IC0: {	remote:	0, $						; 0:local, 1:set from valid Kandinski
					pv: {	bname:	bytarr(128), $		; scaler name string connected to FC0
							val:	0.0, $				; value multipler
							unit:	0.0}}, $			; units, range value
			IC1: {	remote:	0, $						; 0:local, 1:set from valid Kandinski
					pv: {	bname:	bytarr(128), $		; scaler name string connected to FC0
							val:	0.0, $				; value multipler
							unit:	0.0}}, $			; units, range value
			run: {		X:		0L, $					; run X pixel position
						Y:		0L, $					; Y pixel position
						Z:		0L }, $					; Z position
			Deadtime: {	cal: {	a:	0.1,  $				; conversion from T clicks (ns)
								b:	0.0}}, $				
			status: 	{	blog_error: bytarr(n_file)} $	; blog error
			}
			
;--------------------------------------------------------------------------------------------

endif else if daq_channel then begin

; The DAQ single channel parameters struct for use in DAQ_Setup.
; add new entries within each {} to bottom to preserve 'table[]' par indices,
; which index parameters within these structs, as used in the DAQ_setup summary table.

def_struct =  {	$
		disable:		0, $						; channel disable (not used; use enable.ECH from background to *(*pstate).pdisable)
		scepter: {tdm:	3, $						; scepter timing mode (tdm)
				tds:	0, $						; scepter TAC slope (tds)
				tos:	0, $						; scepter TAC timeout (tos)
				trk:	0, $						; scepter simultaneous catching mode (trk)
				trke:	0, $						; scepter enhanced simultaneous catching mode (trk)
				trim:	0.0, $						; scepter6 trim DAC (4 bits, -150 mV-0) (V)
				thresh:	0.5, $						; scepter threshold DAC (10 bits, 0-2V) (V)
				clock:	3.0, $						; scepter quadrant RR clock (MHz)
				thpd:	0.0, $						; scepter7 PD trim (4 bits, -150 mV-0) (V)
				filt:	0, $						; scepter6 additional filtering enable
				tcm:	0}, $						; scepter6 comparator multi-fire suppression (0=0, 1=100ns, 2=1us, 3=2us)
		cal: {	a:		1.0, $						; energy calibration A (gain) (keV/channel)
				b:		0.0}, $						; energy calibration B (offset) (keV)
		trim: {	E: {a:	1.0, $						; E gain trim A (gain), E' = a*E + b
					b:	0.0}, $						; E gain trim B (offset) 
				T: {a:	1.0, $						; T gain trim A (gain), T' = a*T + b 
					b:	0.0}} $						; T gain trim B (offset)
 			}

;--------------------------------------------------------------------------------------------

endif else if daq_readout then begin

; Maia readout enables

	def_struct = {	event:			1, $			; event enable
					photon:			1, $			; photon enable
					activity:		1, $			; activity enable
					scepter:		1, $			; scepter
					Quad_enable:	[1] }			; SCEPTER clock enable enable

;--------------------------------------------------------------------------------------------

endif else if daq_pad then begin

; The DAQ pad layout parameters struct for use in DAQ_Setup, etc.

def_struct =  {	$
				index:	0, $		; detector number
				x:		0.0, $		; X coordinate (mm)
				y:		0.0, $		; Y 
				z:		0.0, $		; Z offset from planar
				width:	0.0, $		; width (mm)
				height:	0.0, $		; height
				tilt:	0.0, $		; tilt angle (degrees from normal)
				bad:	0, $		; 0 good, 1 FWHM high, 2 bad
				FWHM:	0.0, $		; FWHM (Mn) eV
				Hermes:	0, $		; Hermes/Scepter ASIC number
				Quadrant:0, $		; Quadrant #
				radial:	0, $		; Radial #
				column:	0, $		; Col # from left
				row:	0}			; Row # from top
				
;--------------------------------------------------------------------------------------------

endif else if daq_layout ge 1 then begin

; The Maia pad layout parameters struct for use in Maia_Setup, etc.

N = maia_layout
def_struct =  {	$
			title:		'', $		; comment string
			file:		'', $		; file name
			N:			N, $		; number of detector pads
			start:		0, $		; detector pad number start/offset (def 0)
			shape:		0, $		; shape of detectors (0=circ, 1=square)
			threshold:	0.0, $		; FWHM threshold (eV)
			symmetry:	0, $		; symmetry modulo number
			mirrorX:	0, $		; mirror in X
			mirrorY:	0, $		; mirror in Y
			reorient:	0, $		; Re-orient/rotate the detector by 90 degrees
			veto:		0, $		; veto high FWHM above threshold
			data:		replicate(define(/daq_pad),N), $		; pad coordinate data (mm)
			ref:		intarr(N+10) }	; cross-reference from detector # to table indx.
									; allow more space for offset 'start'
				
;--------------------------------------------------------------------------------------------

endif else if daq_version ge 1 then begin

; Version numbers for Maia components

	def_struct =  { scepter: 7, $					; Scepter version
					daq:	1, $					; DAQ digital module version
					software: 0L }					; software version

;--------------------------------------------------------------------------------------------

endif else if daq_number then begin

; Feature numbers for Maia components

	def_struct =  	{	da:		32, $				; number of DA accumulators available
						et2d:	0, $				; number of ET2D accumulators available
						roi:	0, $				; number of ROI accumulators available
						timer:	2}					; number of Timers available
	
;--------------------------------------------------------------------------------------------

endif else if daq_control ge 1 then begin

; The DAQ detector control and readback parameters

def_struct = {			temp: { cpu:		0.0, $		; CPU temp (C)
								FPGA:		0.0, $		; CPU temp (C)
								board:		0.0, $		; board temp (C)
								ETH0:		0.0, $		; ETH0 temp (C)
								ETH1:		0.0}, $		; ETh1 temp (C)
						status: { event_rate: 0L, $		; total event rate /s
								discard_rate: 0L, $		; event discard rate /s
								photon_rate: 0L, $		; photon event rate /s
								charge_rate: 0L, $		; flux count rate Q/s
								discard: 0, $			; event discard mode
								blog:		0, $		; Kandinski blog enabled
								blog_rate:	0.0, $		; blog data rate
								raster_status: 0, $		; 0:stop, 1:run, 2:pause, 3:error
								main_uptime: 0.0D0}, $	; Kandinski up time (sec UTC)
						interlock:	0}					; Interlock status (0 bad, 1 good)

;--------------------------------------------------------------------------------------------

endif else if daq_struct ge 1 then begin

; The MDAQ 32 parameters struct for use in DAQ_Setup. 
; In the case of the 'channel' array of structs, these are in CSV table
; order (not [necessarily] detector number order).
; 
; N.B "define" is part of GeoPIXE, and so GeoPIXE needs to be recompiled
; for new additions here.

N = daq_struct[0]
NELS = 32
channel_pars = define(/daq_channel)
control_pars = define(/daq_control)
version_pars = define(/daq_version)
number_pars = define(/daq_number)

def_struct = { file:	'', $							; Maia parameters file
			DevObj:		obj_new(), $					; local DAQ device object
			run: {		number:	0L, $					; blog run number
						segment: 0L, $					; segment number
						discard: 0, $					; discard on, not running to disk
						last_run: 0L, $					; previous run number
						last_discard: 0, $				; previous discard state
						error: { bits:	0L, $			; accumulator error bits
								MTC:	0, $			; missed trigger count
								OFV:	0, $			; accumulator overflow flag
								ERR:	0}, $			; accumulator error flag
						group: '', $					; setgroup dir name root
						project: '', $					; setproject dir name root
						position: fltarr(6), $			; present position (mm)
						pixel: intarr(6), $				; present pixel address
						energy: 0.0, $					; beam energy (MeV)
						rate: 0.0}, $					; byte rate / s
			identity: { daq:	'', $					; detector digital module identity string
						library:	'', $				; version of Fortran libraries
						software: '' , $				; software identity string
						blog: '' }, $					; blog server HYMOD connected to string
			version: 	version_pars, $					; version pars
			number:		number_pars, $					; number pars
			n_detectors:		N, $					; number of detecttors (1-36)
			channel: replicate(channel_pars,N), $		; up to 36 channel parameters
			Trim: {		on:		0, $					; gain trimming ON
						file:	'', $					; gaintrim spec file
						file2:	''}, $					; gaintrim spec file2 (optional)
			Scan: {		on:		0, $					; scan is active
						iX:		0, $					; X axis index
						iY:		0, $					; Y axis
						X:		0L, $					; scan X size
						Y:		0L, $					; scan Y size
						origin: {	X:	0.0, $			; scan origin X
									Y:	0.0}, $			; scan origin Y
						pitch:	{	X:	0.0, $			; scan pitch X
									Y:	0.0}, $			; scan pitch Y
						info:	'' }, $					; scan info struct (stringify)
			Stage: {	max: { velocity:  fltarr(4), $	; stage maximum velocity (mm/s)
							acceleration: fltarr(4)}, $ ; stage axis acceleration (mm/s2)
						velocity:	fltarr(4)}, $		; stage current velocity (mm/s)
			Deflect: {	scale: {	x:	1.0, $			; deflector X DAC scale factor (mm/V)
									y:	1.0}, $			; deflector Y DAC scale factor (mm/V)
						max: {		x:	3.0, $			; deflector X DAC maximum (V)
									y:	3.0}}, $		; deflector Y DAC maximum (V)
			Deadtime: {	on:		0, $					; deadtime enabled
						auto:	1, $					; set DTcalA based on Scepter TDS and Version
						cal: {	a:	0.1,  $				; conversion from ToT time clicks to ns
								b:	0.0}, $				; for SCEPTER channels (0-31)
						nim:	100. }, $				; for NIM channels
			pileup: { 	on:		0, $					; pileup mode enabled
						limits: { plow:	ptr_new(/allocate_heap), $	; low pileup limits
								phigh: ptr_new(/allocate_heap)}, $	; high pileup limits
						file:	''}, $					; pileup limits file
			DA:		{	on: 	0, $					; DA on
						array:	0, $					; array detector
						type:	0, $					; detector type index
						mode:	0, $					; mode: 0=DA, 1=CUTs, ...
						file:	'', $					; DA file
						channel_on: intarr(N), $		; channel enable
						N:		NELS, $					; number of DA elements
						name:	strarr(NELS), $			; element name strings
;						parray:	ptr_new(/allocate_heap), $	; pointer to 'matrix.array' struct
						memory: {	X:	0, $			; shared DA memory X size
									Y:	0 }, $			; shared DA memory X size
						save:	0, $					; enable auto saves
						save_path:	''}, $				; DA auto save path
			DA2:	{	on: 	0, $					; DA on
						array:	0, $					; array detector
						type:	0, $					; detector type index
						mode:	0, $					; mode: 0=DA, 1=CUTs, ...
						file:	'', $					; DA file
						channel_on: intarr(N), $		; channel enable
						N:		NELS, $					; number of DA elements
						name:	strarr(NELS), $			; element name strings
;						parray:	ptr_new(/allocate_heap), $	; pointer to 'matrix.array' struct
						memory: {	X:	0, $			; shared DA memory X size
									Y:	0 }, $			; shared DA memory X size
						save:	0, $					; enable auto saves
						save_path:	''}, $				; DA auto save path
			DA3:	{	on: 	0, $					; DA on
						array:	0, $					; array detector
						type:	0, $					; detector type index
						mode:	0, $					; mode: 0=DA, 1=CUTs, ...
						file:	'', $					; DA file
						channel_on: intarr(N), $		; channel enable
						N:		NELS, $					; number of DA elements
						name:	strarr(NELS), $			; element name strings
;						parray:	ptr_new(/allocate_heap), $	; pointer to 'matrix.array' struct
						memory: {	X:	0, $			; shared DA memory X size
									Y:	0 }, $			; shared DA memory X size
						save:	0, $					; enable auto saves
						save_path:	''}, $				; DA auto save path
			DA4:	{	on: 	0, $					; DA on
						array:	0, $					; array detector
						type:	0, $					; detector type index
						mode:	0, $					; mode: 0=DA, 1=CUTs, ...
						file:	'', $					; DA file
						channel_on: intarr(N), $		; channel enable
						N:		NELS, $					; number of DA elements
						name:	strarr(NELS), $			; element name strings
;						parray:	ptr_new(/allocate_heap), $	; pointer to 'matrix.array' struct
						memory: {	X:	0, $			; shared DA memory X size
									Y:	0 }, $			; shared DA memory X size
						save:	0, $					; enable auto saves
						save_path:	''}, $				; DA auto save path
			Beam: {	energy:		3.0, $					; Beam energy (MeV)
					particle:	'p', $					; Beam particle
					charge: {	scale:	0.01, $			; Charge scale (Klee 'charge.coeff')
								unit:	'pC'}}, $		; Charge pulse unit (Klee 'charge.unit')
			IC: {	mode:	1, $						; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
					conversion:		1.0, $				; IC count to charge conversion
					pv: {	name:	'DAQ:scaler.FC0', $	; PV name string
							val:	1.0e-9, $			; PV value multipler
							unit:	0.01}, $			; PV units, range value
					plist:	ptr_new(/allocate_heap)}, $	; ptr to PV list
			ROI:	{	on:		0, $					; ROI on
						file:	''}, $					; ROI limits file
			Cal:	{	file:	'', $					; energy calibration spec file
						mode:	0}, $					; cal mode (0=individual, 1=single for all)
			control: 	control_pars $					; control_pars	
			}

;--------------------------------------------------------------------------------------------

endif else if daq_shared1 ge 1 then begin

	N = daq_shared1
	NDA = 32
	n_file = 512
	n_dainfo = 5000
	n_info = 50000
	n_chips = 2
	channel_pars = replicate( define(/daq_channel),N)
	control_pars = define(/daq_control)
	version_pars = define(/daq_version)
	number_pars = define(/daq_number)
	readout_pars = define(/daq_readout)
	layout_data = replicate(define(/daq_pad),N)

def_struct = { $
			n_detectors:	N, $						; # detectors
			version:		version_pars, $				; version numbers
			number:			number_pars, $				; number of things
			layout_data:	layout_data, $				; pad coordinate data (mm)
			channel: 		channel_pars, $				; up to 384 channel parameters
			control: 		control_pars, $				; control and readback parameters	
			readout:	 readout_pars, $				; Maia readout enables
			enable: {	synth:	0, $					; synth pulser
						pulser: 0, $					; pulser
						ECH: bytarr(N), $				; photon.chan[].enable OFF
						LOCK: bytarr(n_chips), $		; Scepter LOCK
						pixel: 0, $						; pixel
						deadtime: 0, $					; deadtime blocks
						roi: 0}, $						; ROI enable
			info: 	{	roi: bytarr(n_file), $			; ROI file as byte array
						da1: bytarr(n_dainfo), $		; DA info1 as byte array
						da2: bytarr(n_dainfo), $		; DA info2 as byte array
						da3: bytarr(n_dainfo), $		; DA info3 as byte array
						da4: bytarr(n_dainfo), $		; DA info4 as byte array
						deadtime: bytarr(n_dainfo), $	; DT info as byte array
						pileup: bytarr(n_dainfo), $		; pileup info as byte array
						cal: bytarr(n_file)}, $			; cal file
			Scan: {		extent:	intarr(6), $			; scan pixel size (6 sxes)
						origin: fltarr(6), $			; scan origin (6 axes)
						pitch:	fltarr(6), $			; scan pitch (6 axes)
						info:	bytarr(n_info)}, $		; scan info stringify struct
			Beam: {	energy:		3.0, $					; Beam energy (MeV)
					bparticle:	bytarr(32), $			; Beam particle
					charge: {	scale: 0.01, $			; Charge scale (Klee 'charge.coeff')
								bunit: bytarr(32)}}, $	; Charge pulse unit (Klee 'charge.unit')
			run: {	position: 	fltarr(6), $			; run mm position
					pixel: 		intarr(6)}, $			; run pixel address
			Stage: {	max: { velocity:  fltarr(4), $	; stage maximum velocity (mm/s)
							acceleration: fltarr(4)}, $ ; stage axis acceleration (mm/s2)
						velocity:	fltarr(4)} $		; stage current velocity (mm/s)

;			DA:	{	on: 	0, $						; DA on
;					N:		NDA, $						; number of DA elements
;					scale:	fltarr(32), $				; scale factors for each DA record
;					bname:	bytarr(32,32), $			; element name strings
;					rGamma:	fltarr(N,NDA) }, $			; rGamma array
;			pileup: { limits: {low: intarr(4096), $		; pileup limits
;							high: intarr(4096) }}, $
;			Deadtime: {	cal: {	a:	0.1,  $				; conversion from ToT to 100 ns time clicks
;								b:	0.0}} $				
			}
			
;--------------------------------------------------------------------------------------------

endif else if daq_da_shared1 ge 1 then begin

	N = daq_da_shared1
	NELS = 32
	NDETS = N
	NENERGY = 2048

def_struct = { $
			n_detectors:	NDETS, $					; # detectors
			channel_on:		intarr(NDETS), $			; active channel enables
			cal: replicate( {	a:	0.0,  $				; channel energy cal
								b:	0.0}, NDETS), $
			axis: {	x:		0, $						; X axis indes
					y:		0 }, $						; Y axis index
			DA: {	N:		NELS, $						; # elements used in DA, etc.
					n_energy: NENERGY, $				; # energy bins used in DA
					cal: {	a:	0.0,  $					; DA energy matrix cal.a
							b:	0.0}, $					;    cal.b
					matrix: fltarr(NENERGY,NELS), $		; DA matrix
					MDL:	fltarr(NELS), $				; MDLs
					rGsum:	fltarr(NELS) }, $			; rGamma summed over detectors
			DT: {	cal: {	a:	0.0,  $					; DT time cal
							b:	0.0 }, $					
					nim: 	0.0 }, $					; NIM channel DT cal C
			pileup: intarr(2,NENERGY) $					; pileup field
			}
			
;--------------------------------------------------------------------------------------------

; Scan specification structure for DAQ system

endif else if scan_spec ge 1 then begin

def_struct = {	active:					0, $		; active flag index into ['ON','OFF','START','STOP','REF1','REF2']
				sample:					'', $		; sample name
				grain:					'', $		; grain/point name
				comment:				'', $		; some notes
				blog:					0, $		; blog run number
				crossref:				'', $		; crossref to other system (Maia blog number)
				origin: { x:			0.0, $		; X origin (mm)
						y:				0.0, $		; Y origin (mm)
						z:				0.0}, $		; Z origin (mm)
				raster: { $
					axis: {	x:			0, $		; X axis index ['DAC X','DAC Y','Stage X','Stage Y','Stage Z','Stage A']
							y:			0}, $		; Y
					shape:				0, $		; raster shape index ['Square','Ellipse','?']
					step_scan:			0, $		; fly scan (0), step scan (1)
					overscan:			0, $		; overscan factor (1 = single, no overscan)
					interlace:			0, $		; enable interlace scan
					size: {	x:			0.0, $		; X size (mm)
							y:			0.0}, $		; Y size (mm)
					pixel: { x:			0.0, $		; X pixel size (mm)
							y:			0.0}, $		; Y pixel size (mm)
					charge: {	min:	0.0, $		; min charge (pC)
								max:	0.0}, $		; max charge (pC)
					photons: {	min:	0.0, $		; min photon count
								max:	0.0}, $		; max photon count
					time: {		min:	0.0, $		; min dwell time (s)
								max:	0.0}} $	; max dwell time (s)
			}

;--------------------------------------------------------------------------------------------

; Frame specification structure for Maia Mapper system

endif else if maia_frame_spec ge 1 then begin

def_struct = {	$
			finder: {		ref1: {		x:	0.0, $			; Ref #1 X coord (Finder)
										y:	0.0, $			; Ref #1 Y 
										z:	0.0 }, $		; Ref #1 Z
							ref2: {		x:	0.0, $			; Ref #2 X coord (Finder)
										y:	0.0, $	 		; Ref #2 Y
										z:	0.0 }}, $		; Ref #2 Z
			mapper1: {		ref1: {		x:	0.0, $			; Ref #1 X coord (Mapper1)
										y:	0.0, $			; Ref #1 Y
										z:	0.0 }, $		; Ref #1 Z
							ref2: {		x:	0.0, $			; Ref #2 X coord (Mapper1)
										y:	0.0, $	 		; Ref #2 Y
										z:	0.0 }},  $	 	; Ref #2 Z
			mapper2: {		ref1: {		x:	0.0, $			; Ref #1 X coord (Mapper2)
										y:	0.0, $			; Ref #1 Y
										z:	0.0 }, $		; Ref #1 Z
							ref2: {		x:	0.0, $			; Ref #2 X coord (Mapper2)
										y:	0.0, $	 		; Ref #2 Y
										z:	0.0 }}  $	 	; Ref #2 Z
			}

;--------------------------------------------------------------------------------------------

; Sample specification structure for Maia Mapper system

endif else if maia_sample_spec ge 1 then begin

def_struct = {	$
		reference: {	ref1: {		x:	0.0, $				; Ref #1 X coord 
									y:	0.0, $				; Ref #1 Y 
									z:	0.0 }, $			; Ref #1 Z
						ref2: {		x:	0.0, $				; Ref #2 X coord 
									y:	0.0, $	 			; Ref #2 Y
									z:	0.0 }}, $			; Ref #2 Z
		coordinates:	'finder' ,	$						; “finder” or “mapper1/2”
		IGSN:			'', $								; IGSN code			
		sample:			'' }								; sample record

;--------------------------------------------------------------------------------------------

; Scan specification structure for Maia Mapper system

endif else if maia_scan_spec ge 1 then begin

def_struct = {	active:					0, $		; active flag index into ['ON','OFF','START','STOP','REF1','REF2']
				sample:					'', $		; sample name "X" refers to key "sample.X"
				region:					'', $		; grain/region name
				comment:				'', $		; some notes
				project:				'', $		; project name
				coordinates:			'mapper1', $	; denotes coordinate system "finder" or "mapper1/2"
				origin: { x:			0.0, $		; X origin (mm)
						y:				0.0, $		; Y origin (mm)
						z:				0.0}, $		; Z origin (mm)
				raster: { $
					size: {	x:			0.0, $		; X size (mm)
							y:			0.0}, $		; Y size (mm)
					pixel: { x:			0.0, $		; X pixel pitch (mm)
							y:			0.0}, $		; Y pixel pitch (mm)
					dwell: 				0.0, $		; dwell time (ms)
					shape:				0, $		; raster shape type ['raster','ellipse','?']
					interlace:			1, $		; interlace scan count (1=normal non-interlaced)
					stroke:				0 }, $		; skip this many 'strokes', use "0" for initial scan
				Zsurface:				fltarr(4) $	; Z bilinear surface coeffs (k,y,x,xy)
			}

;--------------------------------------------------------------------------------------------

endif else if import ge 1 then begin

; The details struct used in spectra import

def_struct = {	name:			'', $			; unique name for import
				title:			'', $			; description for import list
				in_ext:			'.spec', $		; input file extension
				request:		'Select Spectrum to Load', $ ; title for requester
				preview:		0, $			; allow spectrum preview
				raw:			0, $			; flags use of separate Raw data path '(*pstate).dpath'
				spec_evt:		0, $			; uses a call to spec_evt to extract from EVT data
				device_name:	'', $			; associated device object name
				multifile:		0, $			; denotes data in a series of more than one file
				separate:		'.', $			; char between file and run #
				use_linear:		0, $			; request linearization file
				use_pileup:		0, $			; request pileup file
				use_throttle:	0, $			; request throttle file
				use_IC:			0, $			; pop-up the flux_select PV selection panel
				IC_mode:		0, $			; default charge/flux mode if pop-up used
				use_tot:		0, $			; collect ToT data too
				xr:				200L, $			; default X range
				yr:				200L $			; default Y range
			}

;--------------------------------------------------------------------------------------------

endif else if old_source1 ge 1 then begin

; The definition struct for a continuum laboratory source, by model or experimental spectrum.
; Only the mono (continuum=0) is used for ion beams so far.
; 'continuum' mode=0 means that no details are saved/loaded except 'continuum' itself.
; 'continuum' must be >0 for saving details.

n_filters = 20												; maxmimum # filters
n_lines = 10												; maximum number of lines per element
n_els = 10													; maximum number of characteristic elements
filter = define( /filter)

; N.B.	If change size of data[], E[], etc. in spectrum, or
;		number of filters (n_filters above), then must fix 'read_source'

def_struct = {	continuum:	0, $							; continuum spectrum (1), or mono source (0)
				energy:		0.0, $							; beam energy (mono), maximum energy (continuum)
				model:		0, $							; model curve (1), or experimental spectrum file (0)
				spectrum: {	N:			0L, $				; number of channels used (<= 300)
							data:		fltarr(300), $		; spectrum (continuum, for model or experimental)
							E:			fltarr(300), $		; energy axis for spectrum (consistent with Cal)
							proportion:	fltarr(300), $		; proportion in each channel due to continuum
							char:		fltarr(300), $		; characteristic line spectrum
							cal:	{	B:		0.0, $		; energy cal offset (keV)
										A:		0.02}}, $	; energy cal gain (keV per channel)
				lines: {	n_lines:	intarr(n_els), $			; number of lines per element/shell
							Z:			intarr(n_els), $			; atomic numbers
							shell:		intarr(n_els), $			; shells of elements
							line:		intarr(n_lines,n_els), $	; line index
							e:			fltarr(n_lines,n_els), $	; line energy
							rel:		fltarr(n_lines,n_els)}, $	; line relative intensity
				file:		'', $							; file name: .model file or experimental data
				title:		'', $							; title string
				modata: {	volts:	0., $					; model data: tube (kV)
							power:	0., $					; tube power (W)
							anode: {	name:		'', $	; name for anode
										formula:	'', $	; formula
										weight:		0}, $	; weight% on formula
							spot:	0.02, $					; effective source spot size (mm)
							phi:	90., $					; incident angle (to surface)
							eps:	5., $					; takeoff angle (to surface)
							bin:	0.02, $					; energy spectrum channel width (keV)
							mono:	[0.0,3.0,0.5]}, $		; optional monochromator spec [E,BW%,Eff] (E=0.0 means OFF)
				beam: {		mode:	0, $					; beam mode (0=side-window, 1=transmission)
							thick:	0.0}, $					; thickness of thin transmission anode (mg/cm2)
				acceptance:	0.001, $						; acceptance from source (msr)
				mono: {		mode:	0, $					; mono mode (0=off, 1=on)
							z:		42, $					; Z of elements energy
							shell:	1}, $					; shell
				n_filters:	0, $							; used filters
				filters:	replicate(filter,n_filters) }	; model filter array	

;--------------------------------------------------------------------------------------------

endif else if old_source2 ge 1 then begin

; The definition struct for a continuum laboratory source, by model or experimental spectrum.
; This version is missing the etrans, trans for transmission, as used in define(/source).

n_filters = 20												; maxmimum # filters
n_lines = 10												; maximum number of lines per element
n_els = 10													; maximum number of characteristic elements
filter = define( /filter)

; N.B.	If change size of data[], E[], etc. in spectrum, or
;		number of filters (n_filters above), then must fix 'read_source'

def_struct = {	continuum:	0, $							; continuum spectrum (1), or mono source (0)
				energy:		0.0, $							; beam energy (mono), maximum energy (continuum)
				model:		0, $							; model curve (1), or experimental spectrum file (0)
				spectrum: {	N:			0L, $				; number of channels used (<= 300)
							data:		fltarr(300), $		; spectrum (continuum, for model or experimental)
							E:			fltarr(300), $		; energy axis for spectrum (consistent with Cal)
							proportion:	fltarr(300), $		; proportion in each channel due to continuum
							char:		fltarr(300), $		; characteristic line spectrum
							cal:	{	B:		0.0, $		; energy cal offset (keV)
										A:		0.02}}, $	; energy cal gain (keV per channel)
				lines: {	n_lines:	intarr(n_els), $			; number of lines per element/shell
							Z:			intarr(n_els), $			; atomic numbers
							shell:		intarr(n_els), $			; shells of elements
							line:		intarr(n_lines,n_els), $	; line index
							e:			fltarr(n_lines,n_els), $	; line energy
							rel:		fltarr(n_lines,n_els)}, $	; line relative intensity
				file:		'', $							; file name: .model file or experimental data
				title:		'', $							; title string
				modata: {	volts:	0., $					; model data: tube (kV)
							power:	0., $					; tube power (W)
							anode: {	name:		'', $	; name for anode
										formula:	'', $	; formula
										weight:		0}, $	; weight% on formula
							spot:	0.02, $					; effective source spot size (mm)
							phi:	90., $					; incident angle (to surface)
							eps:	5., $					; takeoff angle (to surface)
							bin:	0.02, $					; energy spectrum channel width (keV)
							mono:	[0.0,3.0,0.5]}, $		; optional monochromator spec [E,BW%,Eff] (E=0.0 means OFF)
				beam: {		mode:	0, $					; beam mode (0=side-window, 1=transmission)
							thick:	0.0}, $					; thickness of thin transmission anode (mg/cm2)
				acceptance:	0.001, $						; acceptance from source (msr)
				mono: {		mode:	0, $					; mono mode (0=off, 1=on)
							z:		42, $					; Z of elements energy
							shell:	1}, $					; shell
				poly: {		mode:	0, $					; poly mode (0=off, 1=on)
							gain:	21000., $				; flux gain
							energy:	17.4, $					; energy of this flux gain
							model:	'XOS default', $		; polycapillary transmission model (file name)
							diameter:	2.0, $				; diameter of beam at exit (mm)
							focus:		14.0, $				; focal distance (mm)
							spot:	0.03, $					; focus spot size
							pinhole: 0.025, $				; pinhole diameter (at 'distance' mm) as flux gain reference
							distance: 100.}, $				; distance of pinhole
				n_filters:	0, $							; used filters
				filters:	replicate(filter,n_filters) }	; model filter array	

;--------------------------------------------------------------------------------------------

endif else if source ge 1 then begin

; The definition struct for a continuum laboratory source, by model or experimental spectrum.
; Only the mono (continuum=0) is used for ion beams so far.
; 'continuum' model=0 means that no details are saved/loaded except 'continuum' itself.
; 'continuum' must be >0 for saving details.

n_filters = 20												; maxmimum # filters
n_lines = 10												; maximum number of lines per element
n_els = 10													; maximum number of characteristic elements
filter = define( /filter)

; N.B.	If change size of data[], E[], etc. in spectrum, or
;		number of filters (n_filters above), then must fix 'read_source'

def_struct = {	continuum:	0, $							; continuum spectrum (1), or mono source (0)
				energy:		0.0, $							; beam energy (mono), maximum energy (continuum)
				model:		1, $							; model: 1 lab source, 2: pink beam
				spectrum: {	N:			0L, $				; number of channels used (<= 300)
							data:		fltarr(300), $		; spectrum (continuum, for model or experimental)
							E:			fltarr(300), $		; energy axis for spectrum (consistent with Cal)
							proportion:	fltarr(300), $		; proportion in each channel due to continuum
							char:		fltarr(300), $		; characteristic line spectrum
							cal:	{	B:		0.0, $		; energy cal offset (keV)
										A:		0.02}}, $	; energy cal gain (keV per channel)

				lines: {	n_lines:	intarr(n_els), $			; number of lines per element/shell
							Z:			intarr(n_els), $			; atomic numbers
							shell:		intarr(n_els), $			; shells of elements
							line:		intarr(n_lines,n_els), $	; line index
							e:			fltarr(n_lines,n_els), $	; line energy
							rel:		fltarr(n_lines,n_els)}, $	; line relative intensity
				file:		'', $							; file name: .model file or experimental data
				title:		'', $							; title string
				modata: {	volts:	0., $					; model data: tube (kV)
							power:	0., $					; tube power (W)
							anode: {	name:		'', $	; name for anode
										formula:	'', $	; formula
										weight:		0}, $	; weight% on formula
							spot:	0.02, $					; effective source spot size (mm)
							phi:	90., $					; incident angle (to surface)
							eps:	5., $					; takeoff angle (to surface)
							bin:	0.02, $					; energy spectrum channel width (keV)
							mono:	[0.0,3.0,0.5]}, $		; optional monochromator spec [E,BW%,Eff] (E=0.0 means OFF)
				beam: {		mode:	0, $					; beam mode (0=side-window, 1=transmission)
							thick:	0.0}, $					; thickness of thin transmission anode (mg/cm2)

				acceptance:	0.001, $						; acceptance from source (msr)
				mono: {		mode:	0, $					; mono mode (0=off, 1=on)
							z:		42, $					; Z of elements energy
							shell:	1}, $					; shell
				poly: {		mode:	0, $					; poly mode (0=off, 1=on)
							gain:	21000., $				; flux gain
							energy:	17.4, $					; energy of this flux gain
							model:	'XOS default', $		; polycapillary transmission model (file name)
							diameter:	2.0, $				; diameter of beam at exit (mm)
							focus:		14.0, $				; focal distance (mm)
							spot:	0.03, $					; focus spot size
							pinhole: 0.025, $				; pinhole diameter (at 'distance' mm) as flux gain reference
							distance: 100., $				; distance of pinhole
							etrans:	fltarr(300), $			; transmission table energies (keV)
							trans:	fltarr(300)}, $			; transmissions (rel to 'energy')
				n_filters:	0, $							; used filters
				filters:	replicate(filter,n_filters) }	; model filter array	

;--------------------------------------------------------------------------------------------

endif else if pink ge 1 then begin

; The definition struct for a pink beam by model with experimental spectrum.
; 'continuum' must be >0 for saving details.

n_filters = 20												; maxmimum # filters
n_mirrors = 4												; maxmimum # mirrors
filter = define( /filter)
mirror = { title:'', file:''}								; link to CXRO data file

; N.B.	If change size of data[], E[], etc. in spectrum, or
;		number of filters (n_filters above), then must fix 'read_source'

def_struct = {	continuum:	0, $							; continuum spectrum (1), or mono source (0)
				energy:		0.0, $							; beam energy (mono), maximum energy (continuum)
				model:		2, $							; model: 1 lab source, 2: pink beam
				spectrum: {	N:			0L, $				; number of channels used (<= 300)
							data:		fltarr(300), $		; spectrum (continuum, for model or experimental)
							E:			fltarr(300), $		; energy axis for spectrum (consistent with Cal)
;							proportion:	fltarr(300), $		; proportion in each channel due to continuum
;							char:		fltarr(300), $		; characteristic line spectrum
							cal:	{	B:		0.0, $		; energy cal offset (keV)
										A:		0.02}}, $	; energy cal gain (keV per channel)

				file:		'', $							; file name: .model file or experimental data
				title:		'', $							; title string
				fe_spectrum_file:	'', $					; FE spectrum filename
				n_mirrors:	0, $							; used mirrors
				mirrors:	replicate(mirror,n_mirrors), $	; mirrors

				acceptance:	0.001, $						; acceptance from source (msr)
				mono: {		mode:	0, $					; mono mode (0=off, 1=on)
							z:		42, $					; Z of elements energy
							shell:	1}, $					; shell
				modata: {	spot:	0.02, $					; effective source spot size (mm)
							bin:	0.02, $					; energy spectrum channel width (keV)
							mono:	[0.0,3.0,0.5]}, $		; optional monochromator spec [E,BW%,Eff] (E=0.0 means OFF)
				poly: {		mode:	0, $					; poly mode (0=off, 1=on)
							gain:	21000., $				; flux gain
							energy:	17.4, $					; energy of this flux gain
							model:	'XOS default', $		; polycapillary transmission model (file name)
							diameter:	2.0, $				; diameter of beam at exit (mm)
							focus:		14.0, $				; focal distance (mm)
							spot:	0.03, $					; focus spot size
							pinhole: 0.025, $				; pinhole diameter (at 'distance' mm) as flux gain reference
							distance: 100., $				; distance of pinhole
							etrans:	fltarr(300), $			; transmission table energies (keV)
							trans:	fltarr(300)}, $			; transmissions (rel to 'energy')
				n_filters:	0, $							; used filters
				filters:	replicate(filter,n_filters) }	; model filter array	

;--------------------------------------------------------------------------------------------

endif else if wizard_notify ge 1 then begin

; The Wizard Notify communication struct, passed as Notify from Wizard, and then
; returned to the originating Wizard (to be freed there).

def_struct = {	wizard:		'', $				; originating wizard name
				window:		'', $				; specific name label for destination window
				command:	'', $				; command to be executed by window
				qual1:		'', $				; some extra qualifier1
				qual2:		'', $				; some extra qualifier2
				error:		0, $				; error return code, on reply
				top:		0L, $				; top level base of sender
				pdata:		ptr_new(), $		; general data to be transferred
				local:		1, $				; pdata is managed 'local' to Wizard
				callback:	'', $				; name of callback routine
				pnext:		ptr_new() }			; next action in linked list

;--------------------------------------------------------------------------------------------

endif else if plot_options ge 1 then begin

; plot set-up options struct (see "plot_image_select", "plot_images", "plot_corr", "plot_rgb_images", ...)
; Also used for spectra, just ignore irrelevant options.
; Remember to update "load_plot_options".

def_struct = {	$
				Type:			'CGM', $	; CGM, METAFILE, PRINTER, ...
				Crop:			0, $		; Crop to shape
				White:			0, $		; black background
				SymSize:		0.5, $		; symbol size
				CharSize:		1.4, $		; character size
				CharThick:		1.2, $		; character line thickness
				LineThick:		1.2, $		; drawing line thickness
				ColourTable:	5, $		; colour table
				Invert:			0, $		; no invert colour table
				LabelAxes:		1, $		; yes, label XY axes
				ZaxisLegend:	1, $		; yes, place a Z axis legend
				DistLegend:		0, $		; for a distance legend
				ppmOnly:		0, $		; ppm only, no wt%
				Title: { on:	1, $		; for a title
					mode:		0, $		; for filename
					text:		'' }, $		; optional text
				DistPos:		0, $		; for bottom distance position
				DistColour:		0, $		; for bottom distance colour
				LabelPos:		0, $		; for outside element label
				LabelColour:	0, $		; for element label colour
				ConcMaxMode:	0, $		; for auto range, 1-2-5 maxima with 10 step colours
				ManualMax:		100., $		; max conc scale in manual mode
				ShowShape:		1, $		; show the shape
				Absolute:		0, $,		; absolute distance scales with origin
				centroids: { on: 0, $		; plot region centroids for selected element ON
					element:	''}, $		; select centroid element name
				enhance: { $
					spots: { on: 0, $		; enhance hot-spots
						elements: ''}}, $	; select enhance element names (1 to 3, sep by spaces)
				max_area: 0, $				; use a maximum display area, strip borders
				separate:		0, $		; plot separate spectra, or common axes
				landscape:		0, $		; Landscape orientation
				Learn: { on:	0, $		; to use a "Learn" RGB file for planes
					file:		'' }}		; "Learn" filename

;--------------------------------------------------------------------------------------------

endif else if correct_yields ge 1 then begin

; The Wizard Notify communication struct, passed as Notify from Wizard, and then
; returned to the originating Wizard (to be freed there).;--------------------------------------------------------------------------------------------

	def_struct = {	$
			n_comp:		3L, $					; number of components
			max_comp:	16, $					; maximum number of components
			comp:		strarr(3), $			; component elements array
			minerals:	strarr(3), $			; mineral name strings array
			rest:		'', $					; "rest" DA matrix file name
			original:	'', $					; original DA matrix file name
			files:		strarr(3), $			; DA matrix file names for minerals
			R:			fltarr(3,3), $			; end-member conc table (wt%)
			current_mode:	0, $				; current correction mode
			pyield:		ptr_new(), $			; pointer to end-mmeber yields
			plast:		ptr_new(), $			; pointer to pixel yields, last iteration
			pdensity:	ptr_new(), $			; pointer to end-mmeber densities
			pscale:		ptr_new() $				; pointer to absorb scaling, last iteration
			}

;--------------------------------------------------------------------------------------------

endif else if operations ge 1 then begin

; The "operations" list for interelement_operations, dynamic_corrections.

	def_struct = ['Off','Subtract','Add','Multiply','Divide','Correct Y step']

;--------------------------------------------------------------------------------------------

endif

return, def_struct
end
