pro read_buffer, obj, unit, x1,y1,e, channel,n, xcompress,ycompress, $
		station_e=ste, time=t, ecompress=ecompress, multiple=multiple, $
		xoffset=xoffset, yoffset=yoffset, title=title, file=file, error=error, $
		flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
		total_processed=processed, processed=count1, valid=good, total_bad_xy=bad_xy, raw_xy=raw_xy, $
		_ref_extra=extra

;   Device specific list-mode (event-by-event) data file reading routine.
;   Remember, channel starts at 0 (-1 means any channel/ADC).
;
;	NOTE: DO NOT add extra key-words here. If they are needed in parent routine, and in device,
;		they can be passed via '_extra' mechanism. For example, the Z axis keywords pass through
;		via the 'extra' mechanism.
;		
; input:
;   unit		read unit number
;   file		filename passed for multi-file XY checking
;   channel		desired ADC channel(s) (these start at zero, after an optional offset)
;   			(note: this is converted to a on/off vector for device method call)
;   xcompress	desired X axis compression
;   ycompress	desired Y axis compression
;   ecompress	desired E axis compression (needs to match DA energy calibration)
;   xoffset		offset X by this (i.e. subtract this)
;   yoffset		offset Y by this
;   /raw_xy		suppresses X,Y compression and offset
;   flux		optional array that comes in to be updated with pixel flux
;   			2D: extra planes are for added 'attributes', as defined by 'get_attributes()' method.
;   			3D: multiple planes for all Z pixels.
;   dead_fraction for some this is an array that comes in to be updated with pixel dead_fraction.
;   			3D: multiple planes for all Z pixels; 2D: 1 plane only.
;				If flux is already DT corrected, "live" flux, then set dead-fraction zero.
;				'dead_fraction' returns dead-fraction, unless 'dwell' is also returned from device
;				(via 'get_dwell' method), then 'dead_fraction' returns dead-time per pixel.
;
; The following are passed to devices that may need them, via _extra mechanism:
;	veto		vector (uintarr) indicates a vetoed event (use this as events are rejected)
; 	by_odd		only accept odd Y rows				; e.g. Maia
; 	by_even		only accept even Y rows
; 	support_odd_even	returns 1 for a device that supports odd/even selection.
;   zcompress	desired Z axis compression
; 	
;   /step_toggle advance step by toggle bit			; these are for X step flagged by
;   toggle_bit	bit # for toggle bit (starts at 0)	; toggling bit in E data (e.g. MPsys)
;   toggle_adc	ADC # for toggle bit
;   /ystep		Y step mode, else X
;   /step_events advance step by event count		; toggle by set # events in toggle_adc	
;   step_count	count for step_event				; # events to use with step_events
; 	
; return:
;   e			energy vector (uintarr) returned
;   t			(optional) Time-over-threshold vector (uintarr), for some DAQs (e.g. Maia)
;   x1			X vector (uintarr) return
;   y1			Y vector (uintarr) return
;   z1=z1		Z vector (uintarr) return
;   ste			(optional) ADC number vector (uintarr) for each returned event (less offset, starting at 0)
;	n			number of (x,y,e,t,ste) events returned (may include veto=1 pseudo events for BT, FC0, FC1)
;   multiple	(optional) if this has same dimensions as e, then it indicates multiple
;          		events with the same x1,y1,e.
;   beam_energy	beam energy, if it's available (MeV=ionbeam, keV=synchrotron)
;   valid		number of good events, or zero
;   count1		number of events processes in this buffer
;   bad_xy		increment passed in value of total event with bad X,Y codes
;   processed	increment total number of events processed.
;   title		run title
;   error		error=1 flags an error to abort

COMPILE_OPT STRICTARR
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384

if n_elements(raw_xy) lt 1 then raw_xy=0
if n_elements(ecompress) lt 1 then ecompress=1L
if n_elements(xcompress) lt 1 then xcompress=1L
if n_elements(ycompress) lt 1 then ycompress=1L
if n_elements(processed) lt 1 then processed=0L
if n_elements(bad_xy) lt 1 then bad_xy=0L
if n_elements(channel) lt 1 then channel=-1L
if n_elements(flux) lt 1 then flux=0.0
if n_elements(dead_fraction) lt 1 then dead_fraction=fltarr(384)
if n_elements(xoffset) lt 1 then xoffset = 0
if n_elements(yoffset) lt 1 then yoffset = 0
if n_elements(beam_energy) lt 1 then beam_energy = 0.0

;	Take care with vectors that some devices do not set, such as veto, ste, tot, multiple.
;	The test here (or in da_evt, etc.) will detect if they are not set-up. But after that we
;	need to	clear them for each buffer. The easiest way to ensure that is to set them to "0US" or "-1L"
;	here (for ste, t, multiple) or in 'da_evt', etc. (for veto).

ecompress = ecompress > 1L
multiple = -1L
ste = 0US
t = 0US
n = 0L

channel_on = intarr(geopixe_max_adcs)
if channel[0] eq -1 then begin
    channel_on[*] = 1
endif else begin
    q = where( (channel ge 0) and (channel lt geopixe_max_adcs))
    if q[0] ne -1 then begin
       channel_on[channel[q]] = 1
    endif else begin
    	print,'read_buffer: bad channel values input'
       goto, bad_io
    endelse
endelse

	error = obj->read_buffer( unit, x1,y1,e, channel_on,n, xcompress,ycompress, $
		station_e=ste, time=t, ecompress=ecompress, multiple=multiple, $
		xoffset=xoffset, yoffset=yoffset, title=title, file=file, error=error, $
		flux=flux, dead_fraction=dead_fraction, beam_energy=beam_energy, $
		total_processed=processed, processed=count1, valid=good, total_bad_xy=bad_xy, raw_xy=raw_xy, $
		_extra=extra)
		
	if n eq 0 then return
	if n_elements(x1) ne n then begin
		print,'read_buffer: Length of "x1" does not match "n". Return Error.'
		goto, bad_io
	endif
	if n_elements(ste) ne n then ste = uintarr(n)
	if n_elements(t) ne n then t = uintarr(n)	
	if multiple[0] eq -1 then multiple = replicate(1L, n)
	return
	
bad_io:
	error = 1
	return
end



