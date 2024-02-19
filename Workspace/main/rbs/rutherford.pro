function rutherford, z1,a1, z2,a2, psi, e, more=more, alt=alt

;	Rutherford cross-section (lab.)
;		mb/str.
;				THETA	scattering angle (c.m.)
;				 PSI		"	 (lab.)
;				  E	beam energy (MeV).
;
;	alternate solution for recoil case :
;
;				MORE	flags alternate solution
;				ALT	recoil case scattering cross-section

RAD=57.29578
PI=3.1415927

	MORE=0
	IF E LT 1.0E-10 then GOTO, erre
	P=ABS(PSI/RAD)
	X=A1/A2
	G=X*SIN(P)

	PSI_MID=ATAN(1./X)
	IF X GT 1.0 THEN begin
		PMAX=ASIN(1./X)
		IF P GT PMAX then GOTO, errang
	endif ELSE IF 1.-X LT 1.E-6 THEN begin
		PMAX=0.5*PI
		IF P GT PMAX then GOTO, errang
	ENDIF
	GAM=SQRT(1.-G*G)

	ZE=FLOAT(Z1*Z2)/E
	DUM1=(X*COS(P)+GAM)*(1.+X)
	T=P+ASIN(G)
	IF T LT 1.0E-7 THEN begin
		RUTHER=1.0E+30
	endif ELSE begin
		ANG=DUM1*DUM1/(GAM* SIN(T/2.)^4)
		RUTHER=1.296*ZE*ZE*ANG
	endelse

;----------------------------------------------------------------------------
;	recoil solution

	IF X LE 1.0 then RETURN, ruther
	MORE=1
	T=PI-T+2.*P
	DUM1=(X*COS(P)-GAM)*(1.+X)
	ANG=DUM1*DUM1/(GAM* SIN(T/2.)^4)

	ALT=1.296*ZE*ZE*ANG

	RETURN, ruther

errang:
	ERROR,'RUTHERFORD','un-physical scattering angle.'
	GOTO, done
erre:
	ERROR,'RUTHERFORD','zero energy !'

done:
	RUTHER=0.
	RETURN, ruther

end
