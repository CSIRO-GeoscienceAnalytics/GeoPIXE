function tos_pu, amp1, amp2, threshold1, tau1, delta1, short=short, $
						twin=twin, peak=peak

; Time over threshold for 2 pulse of amplitude 'amp1' and 'amp2'
; separated by delta (uS)
; 'amp1' can be a vector, 'amp2' is only a scalar.
;
; amp			amplitude, i.e. ADC output channel number/peak height
; threshold		threshold channel number
; tau			shaping time in microseconds
; delta			time between pulses 1 and 2
;				may be a vector too.
; /short		search for short-time crossing
;
; Returns:
; twin			an array returns if the two pulses are separated, and then
;				the time-over-threshold is the length of only the first pulse.
; peak			peak amplitude of first pulse (at zero T).
;
; assume a Gaussian pulse of time-constant 'tau' microseconds
; of the form: threshold = amp1*exp(-t*t/(2.*tau*tau)) +    $
;						amp2*exp(-(t-delta)*(t-delta)/(2.*tau*tau))

common c_tos_1, pulse1, pulse2, tau, delta, threshold

if n_elements(short) lt 1 then short=0
n = n_elements(amp1)
n2 = n_elements(amp2)
m = n_elements(delta1)
t = fltarr(max([n,m]))
twin = intarr(max([n,m]))
peak = fltarr(max([n,m]))

tau = tau1
threshold = threshold1

for i=0L,max([n,m])-1 do begin
	pulse1 = amp1[i < (n-1)]
	pulse2 = amp2[i < (n2-1)]
	delta = delta1[i < (m-1)]
	t1 = 0.5*tos(threshold, tau, pulse1)

	Peak[i] = amp_fun(0.0)					; maximum combined pulses at zero time

;	Long threshold crossing ...

	Xl = 1.2*t1+delta
	Resultl = NEWTON( Xl, 'amp_fun', CHECK=check, TOLX=1.0e-6, tolmin=1.0e-5, stepmax=tau/5., /double )
	Al = amp_fun(resultl > 0.0)

;	Short 'crossing' or perhaps only an inflection ...

	Xs = 0.9*t1
	Results = NEWTON( Xs, 'amp_fun', CHECK=check, TOLX=1.0e-6, tolmin=1.0e-5, stepmax=tau/5., /double )
	As = amp_fun(results > 0.0)

	if short then begin
		result = results
		if As gt 1.0e-4 then begin
			twin[i] = 0
			result = resultl
		endif else begin
			if (abs(resultl-results) gt 0.05) and (results gt 0.005*tau) then begin
				twin[i] = 1
			endif
		endelse
	endif else begin
		result = resultl
		if As gt 1.0e-4 then begin
			twin[i] = 0
		endif else begin
			if (abs(resultl-results) gt 0.05) and (results gt 0.005*tau) then begin
				twin[i] = 1
			endif
		endelse
	endelse

	t[i] = result + t1
endfor

	return, t

bad:
	warning,'tos_pu','bad arguments.'
	return, 0.
end