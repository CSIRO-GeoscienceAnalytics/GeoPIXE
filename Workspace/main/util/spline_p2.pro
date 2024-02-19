; $Id: spline_p.pro,v 1.8 1999/01/16 01:26:12 scottm Exp $
;
; Copyright (c) 1993-1999, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	SPLINE_P
;
; PURPOSE:
;	This procedure performs parameteric cubic spline interpolation.
;
; CATEGORY:
;	Interpolation - E1.
;
; CALLING SEQUENCE:
;	spline_p2, X, Y, Xr, Yr
;
; INPUTS:
;	X:	  The abcissa vector (should be floating or double).
;	Y:	  The vector of ordinate values corresponding to X.
;	Neither X or Y need be monotonic.
;
; KEYWORD PARAMETERS:
;	INTERVAL: The interval in XY space between interpolants. If
;		  omitted, approximately 8 interpolants per XY segment
;		  will result.
;	TAN0:	  The tangent to the spline curve at X[0], Y[0]. If omitted,
;		  the tangent is calculated to make the curvature of the
;		  result zero at the beginning. This is a two element vector,
;		  containing the X and Y components of the tangent.
;	TAN1:	  The tangent to the spline curve at X[N-1], Y[N-1].If omitted,
;		  the tangent is calculated to make the curvature of the
;		  result zero at the end. This is a two element vector,
;		  containing the X and Y components of the tangent.
;
; OUTPUTS:
;	XR:	  The abcissa values of the interpolated function. This
;		  may NOT be the same variable as either X or Y.
;	YR:	  The ordinate values of the interpolated function. This
;		  may NOT be the same variable as either X or Y.
;
; RESTRICTIONS:
;	X and Y should be floating or double.
;
; PROCEDURE:
;	Cubic spline interpolation with relaxed or clamped end conditions
;	as used in the Numerical Recipes.
;
;	This routine is both more general and faster than the
;	user's library function SPLINE. One call to SPLINE_P is equivalent
;	to two calls to SPLINE, as both the X and Y are interpolated with
;	splines. It is suited for interpolating between randomly
;	placed points, and the abcissae	values need not be monotonic.
;	In addition, the end conditions may be optionally specified via
;	tangents.
;
; EXAMPLE:
;	The commands below show a typical use of SPLINE_P:
;	  X = [0.,1,0,-1,0]	  ;Abcissae for square with a vertical diagonal
;	  Y = [0.,1,2,1,0]	  ;Ordinates
;	  spline_p2, X, Y, XR, YR  ;Interpolate with relaxed end conditions
;	  PLOT, XR, YR		  ;Show it
;
; 	As above, but with setting both the beginning and end tangents:
; 	  spline_p2, X, Y, XR, YR, TAN0=[1,0], TAN1=[1,0]
;
; 	This yields approximately 32 interpolants.
;
; 	As above, but with setting the interval to 0.05, making more
;	interpolants, closer together:
; 	  spline_p2, X, Y, XR, YR, TAN0=[1,0], TAN1=[1,0], INTERVAL=0.05
;
; 	This yields 116 interpolants and looks close to a circle.
;
; MODIFICATION HISTORY:
;	DMS, RSI.	August, 1993.	Written.
;	DMS, RSI.	Jan, 1994.  Modified to use NR_ spline routines.
;-

PRO SPLINE_P2, x, y, xr, yr, $
	INTERVAL=interval, TAN0=tan0, TAN1=tan1

n = n_elements(x)
if n ne n_elements(y) then $
	message,'X and Y must have the same number of points'

ni = n-1								;Number of intervals

dx = x - shift(x,1)						;Delta x and y
dy = y - shift(y,1)						;dx(i) = x(i) - x(i-1)
dx[0] = 0.
dy[0] = 0.

t = sqrt(dx^2 + dy^2)					;interpoint Distance

big = 2.0e30

; Default interval = approx 8 points per interval....
if n_elements(interval) le 0 then interval = total(t) / (8*ni)

r = ceil(t/interval)					;# of elements in each interval

q = where( r ge 1)
if q[0] eq -1 then begin
	xr = [x[0],x[0]]
	yr = [y[0],y[0]]
	return
endif else begin
	q = [0,q]
	x2 = x[q]
	y2 = y[q]
	dx = x2 - shift(x2,1)				;Delta x and y
	dy = y2 - shift(y2,1)				;dx(i) = x(i) - x(i-1)
	dx[0] = 0.0
	dy[0] = 0.0
	t = sqrt(dx^2 + dy^2)				;interpoint Distance
	r = ceil(t/interval)				;# of elements in each interval
endelse

n = n_elements(x2)
ni = n-1
nr = long(total(r))						;# of elements in result

tt = fltarr(nr+1, /nozero)
j = 0L

for int = 0, ni-1 do begin				;Each interval
    i1 = int+1
    nn = r[i1]							;# pnts in this interval
    tt[j] = t[i1] / nn * findgen(nn) + t[int]
    t[i1] = t[i1] + t[int]
    j = j + nn
    endfor
tt[nr] = t[int]

; Use end tangents, or use relaxed condition.
if n_elements(tan0) ge 2 then begin		;Clamped on left?
    xp0 = tan0[0]
    yp0 = tan0[1]
endif else begin						;Relaxed
    xp0 = big
    yp0 = big
endelse

if n_elements(tan1) ge 2 then begin		;Clamped on right?
    xpn = tan1[0]
    ypn = tan1[1]
endif else begin
    xpn = big
    ypn = big
endelse

; Compute result & quit
xr = SPL_INTERP(t,x2, SPL_INIT(t,x2, yp1 = xp0, ypn = xpn), tt)
yr = SPL_INTERP(t,y2, SPL_INIT(t,y2, yp1 = yp0, ypn = ypn), tt)
end
