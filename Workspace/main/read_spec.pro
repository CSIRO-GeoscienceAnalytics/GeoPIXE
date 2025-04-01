function read_spec, file, header=header, find=find, error=error
;
;	Read the spectra file 'file'
;
;	Return 'p', a pointer (or pointer array) pointing to
;	spectrum structs, containing the spectrum details
;	and data.
;
;	If /header then only read header stuff for spectrum 'find',
;	where 'find' is the station number (1,2,3,...), 0 means any.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Read_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

error = 1
if n_elements(file) lt 1 then goto, bad_file
if n_elements(header) lt 1 then header=0
if n_elements(find) lt 1 then find=0

on_ioerror, bad_io
close, 1
openr, 1, file, /XDR

valid = [-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26]			; valid /XDR versions (see "read_text_spec" for older)
version = 0
readu, 1, version
q = where( version eq valid)
if q[0] eq -1 then begin
	close, 1
	error = 0
	return, read_text_spec( file, header=header, find=find)
endif

processed = 0L
valid = 0L
bad_xy = 0L
clipped = 0L
ystep = 0L
xstep_on = 0
xstep = 0L
step_events = 0
step_toggle = 0
toggle_bit = 0
toggle_station = 0
events = 0L
type = 0
detector = 0
xcompress = 1
channel = 0				; redundant ? (use station)
device_name = 'MPSYS_DEVICE'
source2 = ''
throttle = ''
pileup = ''
linearize = ''

if version le -12 then readu,1, ystep

readu,1, processed, valid, bad_xy, clipped
readu,1, xstep_on, xstep, step_events
readu,1, step_toggle, toggle_bit, toggle_station
readu,1, events
readu,1, channel, type, detector, xcompress

ycompress = 1
nx = 0
ny = 0
readu,1, ycompress, nx, ny

microns = 0.0
ecal_a = 0.0
ecal_b = 0.0
readu,1, microns, ecal_a, ecal_b

mfile = ''
mcharge = 0.0
nmdl = 0
readu,1, mfile
readu,1, mcharge
readu,1, nmdl
if nmdl gt 0 then begin
	mdl = fltarr(nmdl)
	readu,1, mdl
endif

if version le -23 then begin
	readu,1, device_name
	print,'version -23: device_name=',device_name
endif else begin
	device = 0
	readu,1, device
	print,'version old: device=',device
	old = device_index_from_old_index( device, name=name, error=err)
	if err then print, '    Error converting old device index.' 
	device_name = name
endelse
print,'all versions: device_name=',device_name

obj = obj_new( device_name)
if obj_valid(obj) eq 0 then begin
	warning,'read_spec','Failed to create device object for: '+device_name
endif
if find ge 1 then print,'Find, offset: ', find, adc_offset_device(obj)

readu, 1, source2

if version le -17 then readu,1, throttle
if version le -19 then readu,1, pileup
if version le -19 then readu,1, linearize

IC = {	mode:			0, $		; 0:ion charge, 1:IC with PV, 2:Ion Chamber no PV)
		conversion:		1.0, $		; IC count to charge conversion
		pv: {	name:	'', $		; Epics PV name string
				val:	0.0, $		; PV value multipler
				unit:	0.0}}		; PV units, range value
IC_mode = 0
dwell = { on:	0, $				; dwell used (convert IC rate to count in a pixel)
		val:	0.0}				; dwell value (ms)
np = 0

if version le -21 then begin
	readu,1, IC_mode
	if IC_mode ne 0 then begin
		readu,1, IC
		print,'version -21: Spec IC parameters= PV: ',IC.pv.name,' = ',IC.pv.val*IC.pv.unit
	endif
	readu,1, np
	if np gt 0 then begin
		sic = strarr(np)
		readu, 1, sic
	endif	
	readu,1, dwell
	print,'version -21: Spec dwell = ',dwell.val
endif

max_det = 0L
has_pileup = 0
has_dead = 0
if version le -25 then begin
	readu,1, max_det
	readu,1, has_pileup
	print,'version -25: Spec has_pileup = ',has_pileup,'  max_dets = ',max_det
	if has_pileup then begin
		pu = fltarr(max_det)
		readu,1, pu
	endif
	readu,1, has_dead
	print,'version -25: Spec has_pileup = ',has_dead
	if has_pileup then begin
		dt = fltarr(max_det)
		readu,1, dt
	endif
endif

n = 0L
readu,1, n
if n lt 1 then goto, error

; 'Find' may select a station to return. If not then find=0.
; 
; Want to make sure that the 'find_offset' matches the Display ADC number (as in Spectrum Select).
; The input 'find' is usually from the "Get" button on the Cal window droplist, so found=0 means "any"
; and the ADC number "0", "1", etc. start at find=1,2, etc. Hence, the "-1" in the fiormula for 
; 'find_offset'. This is modified for device where ADC number start from "0", where 'adc_offset_device'
; has the value "-1".

if find ne 0 then begin
	find_offset = find-1 - adc_offset_device(obj)			; equiv. to: find - start_ADC
	print,'find_offset=',find_offset
endif else find_offset=find

p = 0L
for j=0L,n-1 do begin
	headerj = (j eq 0) ? 0 : header
	pj = get_spec( 1, header=headerj, find=find_offset>0, version=version)
	if ptr_valid(pj) eq 0 then goto, bad_get
	(*pj).file = file

	if j eq 0 then begin
		(*pj).max_detectors = max_det
		(*pj).has_pileup = has_pileup
		(*pj).has_dead = has_dead
		if has_pileup then (*pj).pileup_loss_det = ptr_new( pu, /no_copy)  
		if has_dead then (*pj).deadtime_det = ptr_new( dt, /no_copy)
	endif
	
	(*pj).version = version
	(*pj).processed = processed
	(*pj).valid = valid
	(*pj).bad_xy = bad_xy
	(*pj).clipped = clipped
	(*pj).ystep = ystep
	(*pj).xstep_on = xstep_on
	(*pj).xstep = xstep
	(*pj).step_events = step_events
	(*pj).step_toggle = step_toggle
	(*pj).toggle_bit = toggle_bit
	(*pj).toggle_station = toggle_station
	(*pj).events = events
	(*pj).type = type
	(*pj).channel = channel
	(*pj).detector = detector
	(*pj).xcompress = xcompress

	(*pj).matrix.file = mfile
	(*pj).matrix.charge = mcharge
	(*pj).matrix.mdl = ptr_new(mdl, /no_copy)

	(*pj).ycompress = ycompress
	(*pj).nx = nx
	(*pj).ny = ny

	(*pj).microns = microns
	(*pj).ecal.poly[1] = ecal_a
	(*pj).ecal.poly[0] = ecal_b

	(*pj).DevObj = clone_device_object(obj)
	(*pj).source2 = source2
	(*pj).throttle = throttle
	(*pj).pileup = pileup
	(*pj).linearize = linearize

	(*pj).IC.mode = IC_mode
	if IC_mode ne 0 then (*pj).IC = IC
	(*pj).dwell = dwell
	
	
	if np gt 0 then (*pj).plist = ptr_new(sic)
	
	if header then begin
		if ((*pj).station eq 0) or ((*pj).station eq find_offset) or (find eq 0) then begin
			p = pj
			error = 0
			goto, finish
		endif
		free_spectrum, pj
	endif else begin
		if j eq 0 then begin
			p = pj
		endif else begin
			p = [p, pj]
		endelse
	endelse
endfor
if header then begin
;	warning,'read_spec','failed to find selected station.'
	print,'read_spec, failed to find selected station ',find
endif
error = 0

finish:
	close,1
	if obj_valid(obj) then obj_destroy, obj
	return, p

bad_get:
	print,'read_spec: get_spec error'
	goto, finish
bad_io:
	print,'read_spec: I/O error'
	goto, error
bad_file:
	print,'read_spec: no file name supplied'
	goto, usage
bad_version:
	print,'read_spec: bad version number'
	goto, error

usage:
	print,'read_spec: Usage: p = read_spec(file)'
	print,'		where "p" is pointer to spectrum struct(s)'
	print,'		and "file" is the name of the input file'
	goto, error

error:
	p = 0L
	goto, finish
end
