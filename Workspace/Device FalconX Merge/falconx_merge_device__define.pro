;
; GeoPIXE Device Object for FalconX detector array data where the detector channel data
; files are not merged. Merge them in this device.
; Inherits all functionality from FalconX device. 
;
; Uses 'self.embed_detector'=1 to flag expectation to merge detector channel data files. This is
; detected in FalconX device and used to alter the assumptions about file names, etc.
;
; File are assumed to take form: 
;	xx_00_0.silist, xx_00_1.silist, xx_00_2.silist, ... for det #0 
;	xx_01_0.silist, xx_01_1.silist, xx_01_2.silist, ... for det #1 
;	xx_02_0.silist, xx_02_1.silist, xx_02_2.silist, ... for det #2, etc. 
; 
; FalconX (SI-Torro XIA) is a FPGA-based pulse processor and data acquisition 
; system developed by Southern Innovation (Melbourne) and XIA. 
; Data is written by a Unix front-end processor in big-endian byte order.
;
; Initialization:
; When all device classes are detected and loaded into GeoPIXE (all files of the
; form "xxx_device__define.sav" or "xxx_device__define.pro" in the "interface"
; sub-directory of the GeoPIXE main directory, excluding 'BASE_DEVICE"), an 
; Object reference is created for each using the IDL call "Obj_New('xxx_device')".
; This executes the "Init" method in the class.
; 
; init()			initialize the class definition and also defines the main 
; 					object parameter struct in the "xxx_device__define" routine.
; 					DO NOT RUN THIS METHOD (it is done by IDL).
;-------------------------------------------------------------------
;
; Return the index 'q' to 'files' selected for cluster 'cluster_index' out of 'cluster_total'
; keeping all detectors for each sequence number together.

function falconx_merge_device::cluster_files, files, cluster_total, cluster_index, nq=nq4

	COMPILE_OPT STRICTARR

	if n_elements(files) eq 0 then goto, bad
	if n_elements(cluster_total) eq 0 then goto, bad
	if n_elements(cluster_index) eq 0 then goto, bad

	name = strip_path( strip_file_ext(files))
	nf = n_elements(name)
	i = locate_last('_',name)
	seq = lonarr(nf)
	for j=0,nf-1 do seq[j] = long( strmid( name[j], i[j]+1))		; sequence number
	q = sort(seq)
	seq2 = seq[q]
	unique = seq2[ uniq(seq2)]										; unique sequence numbers

	q1 = indgen(cluster_total)
	q1 = congrid( q1, n_elements(unique), /center)
	q2 = where(q1 eq cluster_index, nq2)							; uniques selected by cluster_index

	mask = intarr(nf)
	for i=0L,nq2-1 do begin
		q3 = where( seq eq unique[q2[i]], nq3)
		if nq3 gt 0 then mask[q3]=1									; all files with these uniques
	endfor
	q4 = where( mask eq 1, nq4)
	return, q4

bad:
	nq4 = 0L
	return, -1L
end

;----------------------------------------------------------------------------------------------------

function falconx_merge_device::init

COMPILE_OPT STRICTARR
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
		warning,'falconx_merge_device::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
	 
	i = self->FALCONX_DEVICE::init()	
		
	self.name = 'FALCONX_MERGE_DEVICE'
	self.title = 'FalconX (merge) - XIA data acquisition'

	self.embed_detector = 1			; used to flag merge of single detector channel data files
	return, i						; detector # now embedded in file-name
end

;-------------------------------------------------------------------

pro falconx_merge_device__define

; Define FalconX Merge device object internal data structure.
; Inherits all structure etails from FALCONX_DEVICE.
; Only called using obj = obj_new('FALCONX_MERGE_DEVICE')

COMPILE_OPT STRICTARR

falconx = { FALCONX_MERGE_DEVICE,  $

		INHERITS FALCONX_DEVICE $							; mandatory base device parameters
	}
	return
end
