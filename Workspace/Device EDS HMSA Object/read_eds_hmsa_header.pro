function read_eds_hmsa_header, unit, error=error


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
       warning,'read_eds_hmsa_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
error = 1

	stat = fstat( unit)
	error = 1
	name = strip_file_ext( stat.name) + '.xml'
	head = xml_to_hash( file=name, /tostruct, error=err)
	if err then begin
		warning,'read_eds_hmsa_header','Failed to read XML file.'
		goto, finish
	endif

	version = 0.0				; need to get at "version" in XML for "AuthorSoftware"
	n_adcs = 1
	mp = 0

	dtype = head.data.ImageRaster_EDS_hypermap.DatumType
	doffset = fix(head.data.ImageRaster_EDS_hypermap.DataOffset)
	comment = head.header.Title

	nenergy = fix(head.data.ImageRaster_EDS_hypermap.DatumDimensions.Dimension_Channel)
	nx = fix(head.data.ImageRaster_EDS_hypermap.CollectionDimensions.Dimension_X)
	ny = fix(head.data.ImageRaster_EDS_hypermap.CollectionDimensions.Dimension_Y)

	cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_adcs)

;	for i=0,n_adcs-1 do begin
	i = 0
		scale = 1.0
		if head.Conditions.Detector.Calibration.Unit eq 'eV' then scale = 0.001
		cal[i].a = scale* float(head.Conditions.Detector.Calibration.Gain)
		cal[i].b = scale* float(head.Conditions.Detector.Calibration.Offset)
		if cal[i].a gt 1.0e-10 then begin
			cal[i].on = 1
			cal[i].units = 'keV'
		endif
;	endfor

	error = 0

;	Assuming no DT and uniform dwell/ flux.

	mp = {	version:	version, $					; version (string)
			comment:	comment, $					; comment
			dtype:		dtype, $					; data type string
			doffset:	doffset, $					; data offset (bytes)
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			cal:		cal, $						; energy calibrations
			nenergy:	nenergy $					; length of spectra
			}
	error = 0

finish:
	return, mp
end
