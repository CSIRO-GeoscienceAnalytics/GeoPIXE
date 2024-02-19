function amp_fun, t

; assume a Gaussian pulse of time-constant 'tau' microseconds
; of the form: threshold = amp1*exp(-t*t/(2.*tau*tau)) +    $
;						amp2*exp(-(t-delta)*(t-delta)/(2.*tau*tau))

common c_tos_1, pulse1, pulse2, tau, delta, threshold

y = pulse1*exp(-t*t/(2.*tau*tau)) +    $
				pulse2*exp(-(t-delta)*(t-delta)/(2.*tau*tau)) - threshold

return, y
end
