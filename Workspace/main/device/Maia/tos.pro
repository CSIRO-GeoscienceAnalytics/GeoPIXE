function tos, threshold, tau, amp

; assume a Gaussian pulse of time-constant 'tau' microseconds
; of the form: threshold = amp*exp(-t*t/(2.*tau*tau))

t = sqrt(-2.*tau*tau*alog(threshold/(amp>threshold)))

; scale x2 to add negative part of Gaussian fumnction

return, 2.*t
end
