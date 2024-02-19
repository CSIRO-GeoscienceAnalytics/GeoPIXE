function device_index_from_old_index, index, name=name, silent=silent, error=error

; Return the new Object device list index for an old device
; index 'index'. Use this to convert 'device' index as stored in (now superceded)
; image DAI files to new index, and return object name in 'name'.
;
;	New device Obj			  Old device list:							     old index
;	MPSYS_DEVICE			'MARC MPsys - Linux data acquisition'				; 0
;	LUND_KMAX_DEVICE		'Lund KMax - MAC data acquisition'					; 1
;	NAC_XSYS_DEVICE			'NAC XSYS - VAX data acquisition'					; 2
;	MPSYS_UNIX_DEVICE		'MARC MPsys - Unix data acquisition'				; 3
;	OM_DAQ_DEVICE			'OM DAQ - PC data acquisition'						; 4
;	SANDIA_EVT_DEVICE		'Sandia EVT - PC data acquisition'					; 5
;	SANDIA_MPAWIN_DEVICE	'Sandia MPAWIN - PC data acquisition'				; 6
;	FASTCOM_MPA3_DEVICE 	'Fastcom MPA3 - PC data acquisiion'					; 7
;	APS_LST_DEVICE			'APS Data Cube - LST reformatted on PC'				; 8
;	SANDIA_BD12_DEVICE		'Sandia Rontec BD12 - PC data acquisition'			; 9
;	NSLS_MCA_DEVICE			'NSLS MCA - VME data acquisition'					; 10
;	TOHOKU_LABO_DEVICE		'Tohoku Labo - PC data acquisition'					; 11
;	PRIMECORE_U48_DEVICE	'Sandia Primecore U48 - PC data acquisition'		; 12
;	ZAGREB_LST_DEVICE		'Zagreb LST - PC data acquisition'					; 13
;	GSE_CARS_MCA_DEVICE		'GSE-CARS MCA - VME data acquisition'				; 14
;	HASYLAB_FIO_DEVICE		'Hasylab FIO - ASCII pixel data'					; 15
;	MAIA_DEVICE				'Maia 384/96 - HYMOD data acquisition'				; 16
;	NSLS_HDF_DEVICE			'NSLS HDF - SXRF image data file'					; 17
;	SLS_MCA_DEVICE			'SLS MCA - SXRF image data file'					; 18
;	ESRF_EDF_DEVICE			'ESRF EDF - SXRF image data file'					; 19
;	CSIRO_MIN_DEVICE		'CSIRO minerals - EMP image data file'				; 20
;	WAKASA_UNIDAQ_DEVICE	'Wakasa UniDAQ - PC data acquisition'				; 21
;	HORIBA_RAW_DEVICE		'Horiba raw - XRF image data file'					; 22
;	NSLS_NETCDF_DEVICE		'NSLS NetCDF - SXRF image data file'				; 23

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0
error = 1
name = ''

old_list = ['MPSYS','LUND_KMAX','NAC_XSYS','MPSYS_UNIX','OM_DAQ','SANDIA_EVT','SANDIA_MPAWIN', $
			'FASTCOM_MPA3','APS_LST','SANDIA_BD12','NSLS_MCA','TOHOKU_LABO','PRIMECORE_U48', $
			'ZAGREB_LST','GSE_CARS_MCA','HASYLAB_FIO','MAIA','NSLS_HDF','SLS_MCA','ESRF_EDF', $
			'CSIRO_MIN','WAKASA_UNIDAQ','HORIBA_RAW','NSLS_NETCDF'] + '_DEVICE'
			
n = n_elements(old_list)
if (index lt 0) or (index ge n) then goto, bad_index
name = old_list[index]

error = 0
return, -1

bad_index:
	if silent eq 0 then begin
		warning,'device_index_from_old_index','Old device index "'+str_tidy(index)+'" not found in old table.'
	endif else begin
		print,'device_index_from_old_index: Old device index "'+str_tidy(index)+'" not found in old table.'
	endelse
	return, 0L
end