function adc_offset_device, obj

; 'start_adc' subtleties:
;	0	Means ADC channel number is the same in the hardware and binary raw data files
;		as displayed in droplists, etc, in GeoPIXE.
;	1	Means that ADC channel displayed as "1" occurs in raw data files as "0".
;
; 'station' versus 'channel'
; 	channel		is ADC number as read in from raw data.
; 	station		is this plus 1 (e.g. 1 for channel=0)
; 	
; channel	station		start_adc	offset		station+offset
; 	0		  1			  0			 -1				0
;	1		  2			  0			 -1				1
; 	0		  1			  1			  0				1
;	1		  2			  1			  0				2
; 	
; Offset from 0 to start of real ADCs.
; The offsets for ADC numbers are to their "start_adc" init value.
;
; An offset of 0 means ADC start at	"1"  (default) but in raw data is "0"
;				2						"3"
;				-1						"0"
; 'obj' is a new Device Object.

common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=16
if n_elements(max_adcs) lt 1 then max_adcs=geopixe_max_adcs
define_devices

if obj_valid(obj) eq 0 then return, 0
offset = obj->start_adc() - 1

return, offset
end
