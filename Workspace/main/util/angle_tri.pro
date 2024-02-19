function angle_tri, xa,ya, xb,yb, xc,yc

a = sqrt( float(xb-xc)*float(xb-xc) + float(yb-yc)*float(yb-yc) )
b = sqrt( float(xa-xc)*float(xa-xc) + float(ya-yc)*float(ya-yc) )
c = sqrt( float(xb-xa)*float(xb-xa) + float(yb-ya)*float(yb-ya) )

s = 0.5*(a + b + c)
theta = asin( 2.0*sqrt(s*(s-a)*(s-b)*(s-c))/(b*c) )

if float(xc-xb)*float(yc-yb) * float(xb-xa)*float(yb-ya) gt 0.0 then begin
	theta = -theta
endif

print,'pa=',xa,ya,' pb=',xb,yb,' pc=',xc,yc,' theta=',theta,' a,b,c=',a,b,c
return, theta
end
