function tos_real, amp, threshold=threshold, tau=tau, $
			resolution=resolution, cal_a=cal_a, germanium=germanium

; Simulate time-over-threshold (uS) for a given ADC value(s) 'amp'

; amp			amplitude, i.e. ADC output channel number/peak height
; threshold		threshold channel number
; tau			shaping time in microseconds
; resolution	Mn Ka resolution in eV
; cal_a			Cal A coefficient in keV/channel
; /germanium	flags a Ge detector

if n_elements(cal_a) lt 1 then cal_a = 0.01						; A keV/channel
if n_elements(amp) lt 1 then begin
	max_amp = 2000												; up to ~20 keV
	amp = findgen(max_amp) > 5
endif else max_amp = max(amp)
if n_elements(threshold) lt 1 then threshold = max_amp * 0.01	; 1%, ~0.2 keV
if n_elements(tau) lt 1 then tau = 1.0							; 1 microsec
if n_elements(resolution) lt 1 then resolution = 170.			; FWHM @ 5.85 keV
if n_elements(germanium) lt 1 then germanium = 0				; Ge detector?

f = 115.
if germanium then f=100.
noise = sqrt(resolution*resolution - f*f)/(1000.*cal_a)			; ~111 eV noise equiv.
fano = f/(1000.*cal_a)											; for 115 Si Fano term

noise = noise /  2.355											; convert FWHM to 1sigma
fano = fano /  2.355

; Time over threshold

t = tos(threshold, tau, amp)

; Noise components:
; 1. shot noise at threshold crossing
; 2. Fano 'noise' on amplitude

; shot noise amplitude:
; change in threshold crossing t for a small noise amplitude
; i.e. determine gradient of Gaussian at threshold crossing;
; inverse of this is change in t with noise.

dn = noise * randomn(seed, n_elements(amp))

; convert to amp contribution using Gaussian gradient

dt1 = dn * dt_noise( threshold, tau, amp)

; Fano term
; scales with sqrt(amp), normalized to 115 eV at 5.850 keV

fa = fano * sqrt( amp/(5.85/cal_a) ) * randomn(seed, n_elements(amp))
dt2 = fa * dtos_da( threshold, tau, amp)

; combine both terms (noise dominates):

dt = sqrt(dt1*dt1 + dt2*dt2)
q = where(dt1 lt 0.)
if q[0] ne -1 then dt[q] = -dt[q]

q = where( finite(dt) eq 0)
if q[0] ne -1 then dt[q] = 0.0

; scale by root-2 to include noise in negative part of Gaussian tail

t1 = (t + sqrt(2.)*dt) > 0.
return, t1
end
