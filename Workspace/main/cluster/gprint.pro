pro gprint, mi1,mi2,mi3,mi4,mi5,mi6,mi7,mi8,mi9,mi10,mi11,mi12,mi13,mi14,mi15,mi16,mi17,mi18,mi19,mi20, $
			output=out, active=active, level=level, _extra=extra

; Print or printf to 'out'
; 'out' is unit to use. If out=0, then try common block default unit
; 'default_gprint_unit'. If it's zero, then fall back to print.
; 'active' enable or disable gprint using this flag.

COMPILE_OPT STRICTARR
common c_debug_print1, default_gprint_unit, gprint_active
if n_elements(default_gprint_unit) lt 1 then default_gprint_unit=-1	; stdout
if n_elements(gprint_active) lt 1 then gprint_active=2
if n_elements(active) eq 1 then begin
	gprint_active = active
	return
endif
if gprint_active eq 0 then return
if n_elements(level) lt 1 then level=2
if level lt gprint_active then return

m1 = n_elements(mi1) lt 1 ? 'undefined' : mi1
m2 = n_elements(mi2) lt 1 ? 'undefined' : mi2
m3 = n_elements(mi3) lt 1 ? 'undefined' : mi3
m4 = n_elements(mi4) lt 1 ? 'undefined' : mi4
m5 = n_elements(mi5) lt 1 ? 'undefined' : mi5
m6 = n_elements(mi6) lt 1 ? 'undefined' : mi6
m7 = n_elements(mi7) lt 1 ? 'undefined' : mi7
m8 = n_elements(mi8) lt 1 ? 'undefined' : mi8
m9 = n_elements(mi9) lt 1 ? 'undefined' : mi9
m10 = n_elements(mi10) lt 1 ? 'undefined' : mi10
m11 = n_elements(mi11) lt 1 ? 'undefined' : mi11
m12 = n_elements(mi12) lt 1 ? 'undefined' : mi12
m13 = n_elements(mi13) lt 1 ? 'undefined' : mi13
m14 = n_elements(mi14) lt 1 ? 'undefined' : mi14
m15 = n_elements(mi15) lt 1 ? 'undefined' : mi15
m16 = n_elements(mi16) lt 1 ? 'undefined' : mi16
m17 = n_elements(mi17) lt 1 ? 'undefined' : mi17
m18 = n_elements(mi18) lt 1 ? 'undefined' : mi18
m19 = n_elements(mi19) lt 1 ? 'undefined' : mi19
m20 = n_elements(mi20) lt 1 ? 'undefined' : mi20

if n_elements(out) lt 1 then out = default_gprint_unit
if out le 0 then goto, do_print
default_gprint_unit = out

	case (n_params() < 20) of
		0: return
		1: printf, out,  _extra=extra, m1
		2: printf, out,  _extra=extra, m1,m2
		3: printf, out,  _extra=extra, m1,m2,m3
		4: printf, out,  _extra=extra, m1,m2,m3,m4
		5: printf, out,  _extra=extra, m1,m2,m3,m4,m5
		6: printf, out,  _extra=extra, m1,m2,m3,m4,m5,m6
		7: printf, out,  _extra=extra, m1,m2,m3,m4,m5,m6,m7
		8: printf, out,  _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8
		9: printf, out,  _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9
		10: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10
		11: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11
		12: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12
		13: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13
		14: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14
		15: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15
		16: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16
		17: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17
		18: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18
		19: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19
		20: printf, out, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20
		else: 
	endcase
	flush, out
	return
	
do_print:
	case (n_params() < 20) of
		0: return
		1: print,  _extra=extra, m1
		2: print,  _extra=extra, m1,m2
		3: print,  _extra=extra, m1,m2,m3
		4: print,  _extra=extra, m1,m2,m3,m4
		5: print,  _extra=extra, m1,m2,m3,m4,m5
		6: print,  _extra=extra, m1,m2,m3,m4,m5,m6
		7: print,  _extra=extra, m1,m2,m3,m4,m5,m6,m7
		8: print,  _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8
		9: print,  _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9
		10: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10
		11: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11
		12: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12
		13: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13
		14: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14
		15: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15
		16: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16
		17: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17
		18: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18
		19: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19
		20: print, _extra=extra, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20
		else: 
	endcase
	return
end
