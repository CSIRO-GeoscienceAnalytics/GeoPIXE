function angle_lines, xa,ya, xb,yb, xc,yc, degrees=degrees

; Return angle at point A (xa,ya).
; Angle increasing from pt b to c.

if n_elements(degrees) lt 1 then degrees=0

dx1 = float(xb-xa)
dx2 = float(xc-xa)
dy1 = float(yb-ya)
dy2 = float(yc-ya)

theta1 = atan(dy1,dx1)
theta2 = atan(dy2,dx2)
theta = theta2 - theta1

q = where( theta lt -!pi)
if q[0] ne -1 then theta[q] = theta[q] + 2.*!pi

if degrees then theta = 180. * theta / !pi
return, theta
end

;if (abs(dx1) lt small) and (abs(dx2) lt small) then begin
;	return, ( (dy1*dy2 lt 0.0) ? !pi : 0.0 )
;
;endif else if abs(dx1) lt small then begin
;	m2 = dy2/dx2
;	theta = atan( -1. / m2)
;
;endif else if abs(dx2) lt small then begin
;	m1 = dy1/dx1
;	theta = atan( 1. / m1)
;
;endif else begin
;	m1 = dy1/dx1
;	m2 = dy2/dx2
;	theta = atan( m2-m1, 1.+m1*m2)
;endelse
;
;;print,'pa=',xa,ya,' pb=',xb,yb,' pc=',xc,yc,' theta=',theta
;return, theta
;end
