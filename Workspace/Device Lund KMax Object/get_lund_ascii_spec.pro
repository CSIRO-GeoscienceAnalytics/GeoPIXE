	pro get_lund_ascii_spec, p, name
; -----------------------------------------------------
;restore, 'C:\Users\Jan\Geopixe_old\Geopixe.sav'
;compile  Compiled module: GET_LUND_ASCII_SPEC.
;save, /routines, filename='C:\Users\Jan\Geopixe_old\GeoPIXE.sav'
; -----------------------------------------------------
;	Read in a spectrum
;
	on_ioerror,err
	openr,unit,name,/get_lun
		openr,unit,name,/get_lun


	line = ''
	label_new= ''
	readf, unit, line
	bit = strsplit( line, ',', /extract)

	lbl = strsplit( name, '\',count=l_count,/ extract) ; remove directry info

	no_dot = strsplit( lbl[l_count-1],'.',/extract ) ; remove .txt
	lbl[l_count-1]= no_dot[0]
	label_new = strjoin (lbl[l_count-2:l_count-1],'\');just last level directory + spect no



	n = n_elements(bit)

h5 = 390	    ; total time in s
h6 = 100.100	; Q in nC
h7 = 20			; total counts in spectrum
h8 = 400		; spectrum size

h10=0.2			; GUPIX H-value
h11=0.2			; reference dist
h12=0.2			; actual distance
h16=1.0
h17=1.0
h19=1.0
h20=1.0
reads,bit[5],h5
reads,bit[6],h6
reads,bit[7],h7
reads,bit[10],h10
reads,bit[11],h11
reads,bit[12],h12
reads,bit[16],h16
reads,bit[17],h17
reads,bit[19],h19
reads,bit[20],h20

;print,bit[5],h5
;print,bit[6],h6
;print,bit[8],h8
;print,bit[10],h10
;print,bit[11],h11
;print,bit[12],h12
;print,bit[16],h16		; Cal A1
;print,bit[17],h17		; Cal A2
;print,bit[19],h19
;print,bit[20],h20

spectrum = fltarr(2048)
bitar = bit[67:*]

nb = n_elements(bitar)
if nb lt 1 then goto, err
spectrum[0:nb-1] = long(bitar)
;reads,bitar, spectrum

	siz = n_elements(spectrum)

	spec2 = define(/spectrum)
;	spec2.file = label_new
	spec2.file = name
	spec2.source = bit[0]
	spec2.label = label_new
		;spec2.label = bit[1]

	spec2.cal.units = 'keV'
	spec2.cal.order = 1
	spec2.cal.poly[0] = h16/h17		; ????
	spec2.cal.poly[1] = 1./h17		; h17 is channels per keV

	spec2.charge = h6/1000.			; laddning (nC) till charge
;;;		spec2.charge = h6/100.			; SPECIAL laddning (nC) till charge GUPIXvariant

;	spec2.station =					; get from ADC # in bit[0] ?
;	spec2.channel = 				; station-1 (starts at 0)

	spec2.size = siz
	spec2.data = ptr_new( spectrum )

	p = ptr_new( spec2)

more:
	if n_elements(unit) ge 1 then free_lun, unit
	return

err:
	warning,'get_lund_ascii_spec','error opening file: '+name
	p = 0L
	return
	end
