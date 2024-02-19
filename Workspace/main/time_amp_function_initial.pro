pro time_amp_function_initial, x,y, a

; Initial parameters values for the CURVEFIT function defined by 'fname'

a = dblarr(3)
a[0] = (y[2]-y[0]) / (alog10(x[2]) - alog10(x[0]))
a[1] = 0.1
a[2] = y[0]/a[0] - alog10(x[0])

return
end

