function pinhole, t1, t2, ratioi, quiet=quiet, aspy=aspy, cuas=cuas, cp=cp

; Pin-hole t1, ratio
; extra absorber t2
;
; all t1 are in microns of Al.
; ratio is ratio of detector solid-angle to hole solid-angle.

if n_params(0) lt 2 then begin
	print,'pinhole: usage - "pinhole, thick-normal, thick-pinhole, pinhole-ratio"'
	print,'	where thickness are microns of Al, ratio is ratio of detector to hole solid-angles.'
	print,'	omit "pinhole-ratio" to optimize the ratio for a total Fe+As count rate of 1000.'
	return, 0
endif
opt_ratio = 0
if n_params(0) lt 3 then begin
	opt_ratio = 1
endif else if ratioi lt 1.0 then begin
	print,'pinhole: ratio must be >1'
	return, 0
endif else ratio = ratioi
if n_elements(quiet) lt 1 then quiet = 0
if n_elements(aspy) lt 1 then aspy = 0
if n_elements(cuas) lt 1 then cuas = 0
if n_elements(cp) lt 1 then cp = 0
if aspy+cuas+cp eq 0 then aspy=1

if opt_ratio then begin
	iratio = 1.1 + findgen(300)
	small = 100000.
	for i=0L,n_elements(iratio)-1 do begin
		r = pinhole(t1,t2,iratio[i],/quiet)
		if abs(r[0]+r[1]-1000.) lt small then begin
			ratio = iratio[i]
			small = abs(r[0]+r[1]-1000.)
		endif
	endfor
;	print,'  final ratio = ',ratio,' range=[',iratio[0],iratio[n_elements(iratio)-1],'], small',small

	iratio = ratio * (1. + 0.2 * (findgen(100)*0.01 - 0.5))
	for i=0L,n_elements(iratio)-1 do begin
		r = pinhole(t1,t2,iratio[i],/quiet)
		if abs(r[0]+r[1]-1000.) lt small then begin
			ratio = iratio[i]
			small = abs(r[0]+r[1]-1000.)
		endif
	endfor
;	print,'  final ratio = ',ratio,' range=[',iratio[0],iratio[n_elements(iratio)-1],'], small',small

	iratio = ratio * (1. + 0.005 * (findgen(100)*0.01 - 0.5))
	for i=0L,n_elements(iratio)-1 do begin
		r = pinhole(t1,t2,iratio[i],/quiet)
		if abs(r[0]+r[1]-1000.) lt small then begin
			ratio = iratio[i]
			small = abs(r[0]+r[1]-1000.)
		endif
	endfor
	print,'  final ratio = ',ratio,' range=[',iratio[0],iratio[n_elements(iratio)-1],'], small',small
endif

mfe = 96.0		; mass absorption for Al
mcu = 50.8
mau = 30.0
mas = 23.9
mag = 3.02

rfe = 345000.	; count rates, no filters, 1 nA 3 MeV p
rcu = 116000.
ras = 69000.
rau = 0.11
rag = 0.014

dal = 2.7

; transmission through normal filter

t2fe = exp( -(mfe/1000.) * (t2*dal/10.))
;print, 'tfe=',t2fe
t2cu = exp( -(mcu/1000.) * (t2*dal/10.))
;print, 'tcu=',t2cu
t2as = exp( -(mas/1000.) * (t2*dal/10.))
;print, 'tas=',t2as
t2au = exp( -(mau/1000.) * (t2*dal/10.))
;print, 'tau=',t2au
t2ag = exp( -(mag/1000.) * (t2*dal/10.))
;print, 'tag=',t2ag

; transmission through pinhole

t1fe = exp( -(mfe/1000.) * (t1*dal/10.))
t1fe = 1./ratio + (1.-1./ratio)* t1fe
;print, 'tfe=',t1fe
t1cu = exp( -(mcu/1000.) * (t1*dal/10.))
t1cu = 1./ratio + (1.-1./ratio)* t1cu
;print, 'tcu=',t1cu
t1as = exp( -(mas/1000.) * (t1*dal/10.))
t1as = 1./ratio + (1.-1./ratio)* t1as
;print, 'tas=',t1as
t1au = exp( -(mau/1000.) * (t1*dal/10.))
t1au = 1./ratio + (1.-1./ratio)* t1au
;print, 'tau=',t1au
t1ag = exp( -(mag/1000.) * (t1*dal/10.))
t1ag = 1./ratio + (1.-1./ratio)* t1ag
;print, 'tag=',t1ag

; count rates

r12fe = rfe * t1fe * t2fe
r12cu = rcu * t1cu * t2cu
r12as = ras * t1as * t2as
r12au = rau * t1au * t2au * 10000.
r12ag = rag * t1ag * t2ag * 10000.

if(quiet eq 0) then begin
	if aspy then print,'count rates in aspy @ 1 nA (Fe,As,Au[1%],Ag[1%]) - ',r12fe,r12as,r12au,r12ag
	if cuas then print,'count rates in CuAs @ 1 nA (Cu,As,Au[1%],Ag[1%]) - ',r12cu,r12as,r12au,r12ag
	if cp then print,'count rates in chalcopyrite @ 1 nA (Fe,Cu,Au[1%],Ag[1%]) - ',r12fe,r12cu,r12au,r12ag
endif

if aspy then return, [r12fe,r12as,r12au,r12ag]
if cuas then return, [r12cu,r12as,r12au,r12ag]
if cp then return, [r12fe,r12cu,r12au,r12ag]
end
