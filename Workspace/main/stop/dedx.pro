;
; function DEDX:   de = dedx( Z1, A1, Z2, E_MeV)
;
;	Z1		integer		atomic number of ion
;	A1		real		mass of ion (amu)
;	Z2     	integer		atomic mass of target atom
;	E_MEV   real		Ion energy (MeV)
;
;	DEDX	real		de/dx in MeV / mg/cm**2

;------------------------------------------------------------------

   function DEDXP, EPROT,A,BETASQ

;	Hydrogen stopping powers (eV/10**15 atoms/cm**2)

	nen = n_elements(eprot)
	sp = fltarr(nen)

	q = where( eprot lt 10.)
	if q[0] ne -1 then begin
		SP[q] = A[0,q] * SQRT(EPROT[q])
	endif

	q = where( eprot gt 1000.)
	if q[0] ne -1 then begin
		SUM = fltarr(n_elements(q))
		for i=1L,5 do begin
			II = I+7
			SUM = SUM + A[II-1,q] * ALOG(EPROT[q])^(I-1)
		endfor
		SP[q] = (A[5,q]/BETASQ[q])*(ALOG(A[6,q]*BETASQ[q]/(1.-BETASQ[q]))-BETASQ[q]-SUM)
	endif

	q = where( (eprot ge 10.) and (eprot le 1000.))
	if q[0] ne -1 then begin
		SLOW = A[1,q] * EPROT[q]^0.45
		SHIGH = (A[2,q]/EPROT[q]) * ALOG(1.+(A[3,q]/EPROT[q])+A[4,q]*EPROT[q])
		SP[q] = 1./((1./SLOW)+(1./SHIGH))
	endif

	RETURN, SP
	END

;------------------------------------------------------------------

	function DEDXN, Z1,Z2,A1,A2,E

;	Nuclear stopping power (eV/10**15 atoms/cm**2)

	nen = n_elements(e)
	sn = fltarr(nen)

	DUM1 = (FLOAT(Z1)^0.667+FLOAT(Z2)^0.667)
	DUM2 = A1+A2
	DUM3 = FLOAT(Z1)*FLOAT(Z2)
	DUM4 = SQRT(DUM1)

	EPS = 32.53*A2*E/(DUM3*DUM2*DUM4)
	CONVF = 8.462*DUM3*A1/(DUM2*DUM4)

	q = where(eps lt 0.01)
	if q[0] ne -1 then begin
		SN[q] = 1.593 * CONVF[q] * SQRT(EPS[q])
	endif

	q = where((EPS GE 0.01) AND (EPS LE 10.0))
	if q[0] ne -1 then begin
		SN[q] = 1.7*CONVF[q] * SQRT(EPS[q])*ALOG(EPS[q]+EXP(1.))  $
						/ (1.+6.8*EPS[q] + 3.4*EPS[q]^1.5)
	endif

	q = where(EPS GT 10.0)
	if q[0] ne -1 then begin
		SN[q] = CONVF[q] * ALOG(0.47*EPS[q])/(2.*EPS[q])
	endif

	RETURN, SN
	END

;------------------------------------------------------------------

	function DEDXA, EK,A

;	Alpha stopping powers

	SMALL = 1.0E-10

	E = EK/1000.
	nen = n_elements(e)
	se = fltarr(nen)

	SL = A[0,*] * EK^A[1,*]
	SH = A[2,*] * ALOG(1.+A[3,*]/E + A[4,*]*E) / E

	q = where( SL lt small)
	if q[0] ne -1 then begin
		SE[q] = SL[q]
	endif

	q = where( SH lt small)
	if q[0] ne -1 then begin
		SE[q] = SH[q]
	endif

	q = where( (SH ge small) and (SL ge small))
	if q[0] ne -1 then begin
		SE[q] = 1. / (1./SL[q] + 1./SH[q])
	endif

	RETURN, SE
	END

;------------------------------------------------------------------

function dedx, z1, a1i, z2i, E_MeV

;	Z1		integer		scalar	atomic number of ion
;	A1		real		scalar	mass of ion (amu)
;	Z2     	integer		vector	atomic mass of target atom
;	E_MEV   real		vector	Ion energy (MeV)
;
;	DEDX	real		de/dx in MeV / mg/cm^2

common c_dedx, proton, alpha, proton_ok, alpha_ok

E = 1000.*E_MEV
z2 = z2i
a1 = round(a1i) + excess(z1,a1i)/931.481

nz = n_elements(z2)
nen = n_elements(e)
nt = nz

if (nz lt 1) or (nen lt 1) then return, 0.0
if nz eq 1 then begin
	nt = nen
	z2 = replicate(Z2,nt)
endif else if nen eq 1 then begin
	nt = nz
	e = replicate(e,nt)
endif else if nz ne nen then begin
	print,'Dedx: Z2,E can only both be vectors if they have same length.'
	de_dx = fltarr(nt)
	goto, done
endif

de_dx = fltarr(nt)
A2 = mass(Z2)

IF(Z1 EQ 1) THEN begin                              ; protons

	if( n_elements(proton_OK) eq 0) then proton_OK = 0
	if( proton_OK ne 1) then init_dedx
	if( proton_OK ne 1) then return, de_dx

	EX = E/A1
	BETASQ = EX/465740.5
	SE = DEDXP(EX,proton[*,z2],BETASQ)

endif ELSE IF(Z1 EQ 2) THEN begin                   ; alpha particles

	if( n_elements(alpha_OK) eq 0) then alpha_OK = 0
	if( alpha_OK ne 1) then init_dedx
	if( alpha_OK ne 1) then return, de_dx

	SE = DEDXA(A1*E/4.0026,alpha[*,z2])

endif ELSE begin                                    ; heavy ions

	if( n_elements(proton_OK) eq 0) then proton_OK = 0
	if( proton_OK ne 1) then init_dedx
	if( proton_OK ne 1) then return, de_dx

	EX = E/A1
	BETASQ = EX/465740.5
	VV0 = 137.039*SQRT(BETASQ)

	V1 = 0.886*VV0/(FLOAT(Z1)^0.6666667)
	V2 = V1+0.0378*SIN(V1*1.570796)
	SR = 1.-EXP(-V2)*(1.034-0.1777*EXP(-0.08114*FLOAT(Z1)))
	SRED = SR*SR

	SP = DEDXP(EX,proton[*,z2],BETASQ)
	SE = FLOAT(Z1)*FLOAT(Z1)*SP*SRED

ENDelse

SN = DEDXN(Z1,Z2,A1,A2,E)

de_dx = (SE+SN)*0.60225/A2

done:
	s = size(de_dx)
	if s[0] gt 0 then de_dx = reform(de_dx)
	if n_elements(de_dx) eq 1 then de_dx = de_dx[0]
	return, de_dx
end

;------------------------------------------------------------------

