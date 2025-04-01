function spectrum_details, pspectra, show=showi

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
		warning,'spectrum_details',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif

define_devices
if n_elements(showi) lt 1 then showi=0
list = ['Properties error']
if ptr_good(pspectra[0]) eq 0 then return, list
if ptr_good(pspectra[0],/struct) then begin
	show = (showi > 0) < (n_elements(pspectra) - 1)
	p = pspectra[show]
	if ptr_good( p,/struct) eq 0 then return, list
endif else begin
	show = (showi > 0) < (n_elements(*pspectra) - 1)
	p = (*pspectra)[show]
	if ptr_good( p,/struct) eq 0 then return, list
endelse
obj = (*p).DevObj
if obj_valid(obj) eq 0 then return,''

;------------------------------------------------------------
; Details string array:

ionbeam = obj->ionbeam()
charge_gain_unit_lists, ic_vals, ic_units, ic_vunits, ionbeam=ionbeam
on_off = ['Off','On']

List = [ 'GeoPIXE Spectrum SPEC file', '   '+(*p).file]
List = [List,'   Version: '+ str_tidy((*p).version)]
List = [List,'Data file(s):','   '+(*p).source + ' ...', '   ' + obj->title()]
List = [List,'   Label: '+(*p).label]
if (*p).valid gt 0 then begin
	List = [List,'   Valid: '+ str_tidy((*p).valid) + ', bad XY: ' + str_tidy((*p).bad_xy) + ', clipped: ' + str_tidy((*p).clipped)]
endif

if obj->linear() then List = [List,'   Linearize: '+(*p).linearize]
if obj->pileup() then List = [List,'   Pileup: '+(*p).pileup]
if obj->Throttle() then List = [List,'   Throttle: '+(*p).throttle]

List = [List,'Calibration: A='+str_tidy((*p).cal.poly[1])+'  B='+str_tidy((*p).cal.poly[0])+' '+(*p).cal.units]
List = [List,'   Energy: '+str_tidy((*p).energy)+', DT Corr: '+str_tidy((*p).deadtime_correction)]
if (*p).dwell.on then begin
	list = [list, '   Dwell (nominal): ' + str_tidy((*p).dwell.val) + ' (ms)']
endif
if (*p).IC.mode eq 1 then begin
	l = locate('time', strlowcase((*p).IC.PV.name))
	ival = find_charge_val_unit_index( (*p).IC.PV.val, (*p).IC.PV.unit, iunit=iunit, time=(l ge 0))
;	q = where( abs(((*p).IC.PV.unit - ic_vunits)/(ic_vunits>0.001)) lt 0.01, nq)
;	unit = ic_units[(nq ne 0) ? q[0] : 0]
	unit = ic_units[iunit]
	ic = '   PV: ' + (*p).IC.PV.name + ', sensitivity = ' + str_tidy((*p).IC.PV.val) + ' (' + unit + ')'
	list = [list, 'Ion chamber:', ic]
endif

c = str_tidy((*p).charge)
if (*p).IC_total ne 0. then begin
	c = c + ', Total flux: ' + str_tidy((*p).IC_total)
	c = c + ' (conversion: ' + str_tidy((*p).IC.conversion) + ')'
endif
List = [List, '   Charge: '+c]

if (*p).ambient.on then begin
	List = [List, 'Ambient Conditions:']
	List = [List, '   Pressure: '+str_tidy((*p).ambient.P)+' (mbar)']
	List = [List, '   Temperature: '+str_tidy((*p).ambient.T)+' (C)']
endif

if (*p).tube.volts gt 0. then begin
	List = [List, 'Laboratory Source:']
	List = [List, '   Volts: '+str_tidy((*p).tube.volts)+' (kV)']
	List = [List, '   Current: '+str_tidy((*p).tube.current)+' (uA)']
	List = [List, '   Time: '+str_tidy((*p).tube.time)+' (s)']
endif

List = [List,'Size: ' + str_tidy((*p).size)]
if ptr_good((*p).pactive) then begin
	List = [List,'   Detectors: '+strjoin(str_tidy( (*(*p).pactive)[0:16 < (n_elements(*(*p).pactive)-1) ]),', ')+' ...']
	list = [list,'   Detector array (active detector multiplicity): ' + str_tidy(n_elements(*(*p).pactive))]
endif else begin
	List = [List,'   Detector: '+str_tidy( (*p).station + adc_offset_device(obj))]
endelse
print,'Station, offset=', (*p).station, adc_offset_device(obj)

if ptr_good((*p).px_coords) then begin
	list = [list, '   X axis position array (' + str_tidy(n_elements(*(*p).px_coords)) + '), units: ' + (*p).x_coord_units]
	List = [List,'   X coord range: ' + str_tidy(min(*(*p).px_coords)) + ' to ' + str_tidy(max(*(*p).px_coords)) + ' ' + (*p).x_coord_units ]
endif

; Device objects specific history info here ...
; Spectra do not read/write device parameters yet, only images, so we won't do this yet.
;List = [List, obj->options_legend()]

list = [list,'Notes:','   Sample: ' + (*p).sample, '   Grain: ' + (*p).grain, '   Comment: ' + (*p).comment] 

return, list
end
