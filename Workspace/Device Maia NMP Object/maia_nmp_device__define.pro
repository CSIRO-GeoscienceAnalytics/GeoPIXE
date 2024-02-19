;
; GeoPIXE Device Object for Maia detector array data on the NMP
; Inherits all functionality from Maia device.
; 
; Maia is a 384 (96) channel detector array and data acquisition and scanning
; system developed by CSIRO-BNL for high definition, high-throughput
; X-ray fluorescence imaging. Data is written by a Unix front-end processor in 
; big-endian byte order.
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
;----------------------------------------------------------------------------------------------------

function maia_nmp_device::init

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
		warning,'maia_nmp_device::init',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
	 
	i = self->MAIA_DEVICE::init()	
		
	self.name = 'MAIA_NMP_DEVICE'
	self.title = 'Maia 384 (NMP) - HYMOD data acquisition'
	self.ionbeam = 1
	self.synchrotron = 0
	return, i
end

;-------------------------------------------------------------------

pro maia_nmp_device__define

; Define Maia NMP device object internal data structure.
; Inherits all structure etails from MAIA_DEVICE.
; Only called using obj = obj_new('MAIA_NMP_DEVICE')

COMPILE_OPT STRICTARR

maia = { MAIA_NMP_DEVICE,  $

		INHERITS MAIA_DEVICE $								; mandatory base device parameters
	}
	return
end
