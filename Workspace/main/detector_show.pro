PRO detector_show, pin, group=group, tlb=tlb

; 'p' is the layer-setup data struct containing details of gemoetry, such as
; alpha, beta for the sample and theta, phi, tilt for the detector as a whole, and
; pointers to the detector and layout structures.

; Define classes used by Xobjview
IDLexObjViewWid__define
IDLexViewManip__define
IDLexWidget__define
IDLexViewGroup__define
IDLexObjView__define
IDLexInscribingView__define
IDLexModelManip__define

white_back = 1										; enable white background
if n_elements(group) lt 1 then group=1L
if n_elements(white_back) eq 0 then white_back=0	; default to black background

black = [0,0,0]
white = [255,255,255]
if white_back then begin
	text_colour = black
	text_colour2 = white
	back_colour = [255,255,255]
	target_colour = [111,55,180]
	beam_colour = [0,240,0]
	detector_colour = byte([245,192,31]*0.7)
	text_detector_colour = black
endif else begin
	text_colour = white
	text_colour2 = black
	back_colour = [0,0,0]
	target_colour = [111,55,180]
	beam_colour = [0,240,0]
	detector_colour = [245,192,31]
	text_detector_colour = black
endelse

; as in 'layer_setup'

if n_elements(pin) lt 1 then begin
	p = ptr_new( {	$
			title:				'', $				; calculation title
			lcm_file:			'', $				; LCM file name
			output_file:		'', $				; output yield file name
			beam:	{beam, mode:	0, $			; beam particle mode
					z1:			1, $				; beam Z
					a1:			1, $				; beam A
					state:		1.0, $				; beam charge state
					state_factor:	1.0, $			; charge state fraction scaling (e.g. for molecular H2)
					e_factor:	1.0, $				; energy scaling (e.g. for molecular H2)
					energy:		3.0 }, $			; beam energy
			source:				source, $			; source beam struct (supercedes simple mono energy for continuum beams)
			detector: {detector2, theta: 135., $	; detector angle
					phi:		0.0 }, $			; out of plane, azimuthal angle
			array: 				0, $				; flags an array detector
			detector_mode:		0, $				; detector droplist index
			detector_list:		ptr_new(), $		; pointer to list of detector file names
			pdetector:			ptr_new(), $		; pointer to detector struct (kept in fit-setup)
			playout:			ptr_new(), $		; pointer to detector layout struct (kept in fit-setup)
			target: {target, alpha:	0.0, $			; target roration angle (vertical axis)
					beta:		0.0 }, $			; target title (about centre horizontal)
			Emin:				2.0, $				; minimum X-ray energy
			Emax:				48.0, $				; maximum X-ray energy
			n_layers:			1, $				; # layers
			unknown:			1, $				; unknown layer
			layer:				0, $				; layer details

			many:				0, $				; multi-thick mode (0: off, 1:on-linked, 2:on-independent)
			thick_min:			0.0, $				; array of multi-thicknesses minima for "many" thickness mode
			thick_max:			0.0, $				; array of multi-thicknesses maxima for "many" thickness mode
			thick_step:			0.0, $				; array of multi-thicknesses steps for "many" thickness mode

			peaks:				ptr_new(), $		; pointer to peaks results struct
			peaks2:				ptr_new() $			; pointer to extra stuff for plotting
	} )
endif else p = pin
if ptr_good(p, /struct) eq 0 then return

theta = (*p).detector.theta						; detector geometry
phi = (*p).detector.phi

alpha = (*p).target.alpha						; target geometry
beta = (*p).target.beta

array = (*p).array								; flag an array detector
if array then begin
	pd = (*p).pdetector
	pl = (*p).playout
	if ptr_valid(pd) eq 0 then return
	if ptr_valid(pl) eq 0 then return

	tilt = (*pd).tilt
	distance = (*pd).distance
	thick = (*pd).crystal.thick / (100. * (*pd).density)

	n_det = (*pl).N								; number of pads
	shape = (*pl).shape							; shape (0=round, 1=square)
	symmetry = (*pl).symmetry					; symmetry (360/symmetry reorient steps)
	reorient = (*pl).reorient					; reorient steps
	mirrorX = (*pl).mirrorX						; mirror in X before reorient
	mirrorY = (*pl).mirrorY						; mirror in Y before reorient

	pad = (*pl).data							; detector pad data (n_det)
	veto = (*pl).veto							; veto by FWHM (ignore for now)
	threshold = (*pl).threshold					; FWHM threshold
endif else begin
	tilt = 0.0
	distance = 50.
	thick = 0.5

	n_det = 1									; number of pads
	shape = 0									; shape (0=round, 1=square)
	symmetry = 2								; symmetry (360/symmetry reorient steps)
	reorient = 0								; reorient steps
	mirrorX = 0									; mirror in X before reorient
	mirrorY = 0									; mirror in Y before reorient

	pad = {index:0, x:0.0, y:0.0, z:0.0, width:10., height:10., tilt: 0.0, $
					bad:0, FWHM:0.0}
	veto = 0									; veto by FWHM (ignore for now)
	threshold = 1000.							; FWHM threshold
endelse

r_theta = distance*0.6
r_beta = distance*0.6
siz_tgt = 25. < distance*0.8
siz_beam = siz_tgt/15. < 1.
char_size = 7.0 * (distance/40.) < 20.

;-------------------------------------------------------------------------------------

; Main model and coordinate frame

oModel0 = OBJ_NEW('IDLgrModel')

oModel0->SetProperty, $
    HIDE= 0

oModel1 = OBJ_NEW('IDLgrModel')

; Set an initial viewing direction, upstream looking at target ...

oModel1->SetProperty, $
    TRANSFORM= [[0.88250990, 0.47029294, -0.0013496904, 0.00000000], $
   		[-0.22820426, 0.43073363, 0.87315055, 0.00000000], $
   		[0.41121754, -0.77025572, 0.48744940, 0.00000000], $
   		[0.00000000, 0.00000000, 0.00000000, 1.0000000]], $
    HIDE= 0

oModel0->Add, oModel1

; X axis

if (theta gt 30.) then begin
	s = 1.
	text = '-X'
	x_reverse = 1
endif else begin
	s = -1.
	text = 'X'
	x_reverse = 0
endelse

oPolyline_tgt_axisX = OBJ_NEW('IDLgrPolyline', [[0.,0.,0.],[s*0.8*distance,0.,0.]])

oPolyline_tgt_axisX->SetProperty, $
    COLOR= [0,240,0], $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel1->Add, oPolyline_tgt_axisX

; X axis label

oModel_X_text = OBJ_NEW('IDLgrModel')

oModel_X_text->SetProperty, $
    HIDE= 0

oModel_X_text->Rotate, [1,0,0], 90.
oModel_X_text->Translate, s*0.9*distance, 0., -0.5*char_size

oModel1->Add, oModel_X_text

oFont_axes = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_X = OBJ_NEW('IDLgrText')

oText_X->SetProperty, $
    ALIGNMENT= 0.500000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
    COLOR= text_colour, $
    FONT= oFont_axes, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= text, $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_X_text->Add, oText_X

; -X axis

if ((x_reverse eq 0) and ((abs(phi) gt 1.) or (alpha lt -1.))) or $
				((x_reverse eq 1) and (theta gt 1.) and (abs(phi) gt 1.)) then begin
	oPolyline_tgt_axisX = OBJ_NEW('IDLgrPolyline', [[0.,0.,0.],[-s*0.8*distance,0.,0.]])

	oPolyline_tgt_axisX->SetProperty, $
	    COLOR= [0,240,0], LINESTYLE=[2, '5555'X], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel1->Add, oPolyline_tgt_axisX
endif

; Y axis

oPolyline_tgt_axisY = OBJ_NEW('IDLgrPolyline', [[0.,0.,0.],[0.,0.,0.8*distance]])

oPolyline_tgt_axisY->SetProperty, $
    COLOR= [0,240,0], $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel1->Add, oPolyline_tgt_axisY

; Y axis label

oModel_Y_text = OBJ_NEW('IDLgrModel')

oModel_Y_text->SetProperty, $
    HIDE= 0

oModel_Y_text->Rotate, [1,0,0], 90.
oModel_Y_text->Translate, 0., 0., 0.85*distance

oModel1->Add, oModel_Y_text

oFont_axes = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_Y = OBJ_NEW('IDLgrText')

oText_Y->SetProperty, $
    ALIGNMENT= 0.50000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
    COLOR= text_colour, $
    FONT= oFont_axes, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= 'Y', $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_Y_text->Add, oText_Y

;-----------------------------------------------------------------------------

; Target

makeCube, verts_tgt, conn_tgt, sy=0.1*siz_tgt, sx=siz_tgt, sz=siz_tgt
;t = compute_mesh_normals( verts_tgt, conn_tgt)

oModel_tgt = OBJ_NEW('IDLgrModel')

oModel_tgt->SetProperty, $
    HIDE= 0

oModel_tgt->Rotate, [0,0,1], alpha

oModel1->Add, oModel_tgt

oModel_tgt2 = OBJ_NEW('IDLgrModel')

oModel_tgt2->SetProperty, $
    HIDE= 0

oModel_tgt2->Rotate, [1,0,0], -beta

oModel_tgt->Add, oModel_tgt2

oModel_tgt3 = OBJ_NEW('IDLgrModel')

oModel_tgt3->SetProperty, $
    HIDE= 0

oModel_tgt3->Translate, 0.0, 0.05*siz_tgt, 0.0

oModel_tgt2->Add, oModel_tgt3

oPolygon_tgt = OBJ_NEW('IDLgrPolygon', verts_tgt)

oPolygon_tgt->SetProperty, $
    BOTTOM= [98,64,94], $
    COLOR= target_colour, $
    POLYGONS= conn_tgt, $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]
;    XCOORD_CONV=[0.00000000,0.38153376], YCOORD_CONV=[0.00000000,0.38153376], ZCOORD_CONV=[0.00000000,0.38153376]

oModel_tgt3->Add, oPolygon_tgt

; Target normal

if (abs(alpha) gt 1.) or (abs(beta) gt 1.) then begin
	oPolyline_tgt_normal = OBJ_NEW('IDLgrPolyline', [[0.,-0.3*distance,0.],[0.,0.,0.]])

	oPolyline_tgt_normal->SetProperty, $
	    COLOR= text_colour, LINESTYLE=[2, '5555'X], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_tgt2->Add, oPolyline_tgt_normal
endif

; Target tangentX

if abs(alpha) gt 1. then begin
	stanx = ((x_reverse eq 0) and (alpha gt 1.)) ? -1. : +1.

	oPolyline_tgt_tangentx = OBJ_NEW('IDLgrPolyline', [[0.,0.,0.],[stanx*0.8*distance,0.,0.]])

	oPolyline_tgt_tangentx->SetProperty, $
		COLOR= [111,55,180], $
    	XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_tgt->Add, oPolyline_tgt_tangentx
endif

; Target tangentY

if abs(beta) gt 1. then begin
	oPolyline_tgt_tangentY = OBJ_NEW('IDLgrPolyline', [[0.,0.,0.],[0.,0.,0.8*distance]])

	oPolyline_tgt_tangentY->SetProperty, $
	    COLOR= [111,55,180], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_tgt2->Add, oPolyline_tgt_tangentY
endif

; Target label

oModel_Target_text = OBJ_NEW('IDLgrModel')

oModel_Target_text->SetProperty, $
    HIDE= 0

oModel_Target_text->Rotate, [1,0,0], 90.
oModel_Target_text->Translate, 0., 0.0, -0.5*siz_tgt - 1.2*char_size

oModel_Tgt3->Add, oModel_Target_text

oFont_axes = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_Target = OBJ_NEW('IDLgrText')

oText_Target->SetProperty, $
    ALIGNMENT= 0.500000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
    COLOR= text_colour, $
    FONT= oFont_axes, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= 'Target', $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_Target_text->Add, oText_Target

; Target front label

oModel_Target_text2 = OBJ_NEW('IDLgrModel')

oModel_Target_text2->SetProperty, $
    HIDE= 0

oModel_Target_text2->Rotate, [1,0,0], 90.
oModel_Target_text2->Translate, 0., -0.1*siz_tgt, -1.2*char_size*0.6

oModel_Tgt3->Add, oModel_Target_text2

oFont_axes2 = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_Target2 = OBJ_NEW('IDLgrText')

oText_Target2->SetProperty, $
    ALIGNMENT= 0.500000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size*0.6, $
    COLOR= white, $				;text_colour, $
    FONT= oFont_axes2, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= 'Front', $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_Target_text2->Add, oText_Target2

; beta arc

if abs(beta) gt 1. then begin
	first = 1
	for t=min([beta,0.]),max([beta,0.]),1. do begin
		if first then begin
			beta_arc = [0.,sin(t/!radeg),cos(t/!radeg)]
			first = 0
		endif else begin
			beta_arc = [[beta_arc],[0.,sin(t/!radeg),cos(t/!radeg)]]
		endelse
	endfor

	oPolyline_beta_arc = OBJ_NEW('IDLgrPolyline', r_beta*beta_arc)

	oPolyline_beta_arc->SetProperty, $
	    COLOR= [111,55,180], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_tgt->Add, oPolyline_beta_arc

; Beta label

	oModel_beta_text = OBJ_NEW('IDLgrModel')

	oModel_beta_text->SetProperty, $
	    HIDE= 0

	oModel_beta_text->Rotate, [1,0,0], 90.
	oModel_beta_text->Translate, 0.2*char_size, 0., 0.6*distance+0.3*char_size
	oModel_beta_text->Rotate, [0,0,1], 90.
	oModel_beta_text->Rotate, [1,0,0], -beta/2.

	oModel_tgt->Add, oModel_beta_text

	oFont_axes = obj_new('IDLgrFont', 'Symbol')

	oText_beta = OBJ_NEW('IDLgrText')

	oText_beta->SetProperty, $
	    ALIGNMENT= 0.50000, $
	    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
	    COLOR= text_colour, $
	    FONT= oFont_axes, $
	    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
	    STRINGS= 'b', $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_beta_text->Add, oText_beta
endif

; alpha arc

if abs(alpha) gt 1. then begin
	first = 1
	sa = (alpha gt 0.) ? -1. : +1.
	for t=min([alpha,0.]),max([alpha,0.]),1. do begin
		if first then begin
			alpha_arc = [stanx*cos(t/!radeg),stanx*sin(t/!radeg),0.]
			first = 0
		endif else begin
			alpha_arc = [[alpha_arc],[stanx*cos(t/!radeg),stanx*sin(t/!radeg),0.]]
		endelse
	endfor

	oPolyline_alpha_arc = OBJ_NEW('IDLgrPolyline', r_beta*alpha_arc)

	oPolyline_alpha_arc->SetProperty, $
	    COLOR= [111,55,180], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel1->Add, oPolyline_alpha_arc

; Alpha label

	oModel_Alpha_text = OBJ_NEW('IDLgrModel')

	oModel_Alpha_text->SetProperty, $
	    HIDE= 0

	oModel_Alpha_text->Rotate, [1,0,0], 90.
	oModel_Alpha_text->Rotate, [0,0,1], 90.
	oModel_Alpha_text->Translate, stanx*r_beta,0., 0.2*char_size
	oModel_Alpha_text->Rotate, [0,0,1], alpha/2.

	oModel1->Add, oModel_Alpha_text

	oFont_axes = obj_new('IDLgrFont', 'Symbol')

	oText_Alpha = OBJ_NEW('IDLgrText')

	oText_Alpha->SetProperty, $
	    ALIGNMENT= 0.500000, $
	    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
	    COLOR= text_colour, $
	    FONT= oFont_axes, $
	    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
	    STRINGS= 'a', $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_Alpha_text->Add, oText_Alpha
endif

;-----------------------------------------------------------------------------

; Beam

oModel_beam = OBJ_NEW('IDLgrModel')

oModel_beam->SetProperty, $
    HIDE= 0

oModel1->Add, oModel_beam

makeCylinder, verts_beam, conn_beam, sy=distance*2.2, sx=siz_beam, sz=siz_beam, rX=90.

oPolygon_beam = OBJ_NEW('IDLgrPolygon', verts_beam)

oModel_beam->Translate, 0.0, -0.4*distance, 0.0

oPolygon_beam->SetProperty, $
    BOTTOM= [98,64,94], $
    COLOR= beam_colour, $
    POLYGONS= conn_beam, $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_beam->Add, oPolygon_beam

; Beam axis label

oModel_Beam_text = OBJ_NEW('IDLgrModel')

oModel_Beam_text->SetProperty, $
    HIDE= 0

oModel_Beam_text->Rotate, [1,0,0], 90.
oModel_Beam_text->Rotate, [0,0,1], 90.
oModel_Beam_text->Translate, 0., -1.3*distance, 0.6*char_size

oModel1->Add, oModel_Beam_text

oFont_axes = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_Beam = OBJ_NEW('IDLgrText')

oText_Beam->SetProperty, $
    ALIGNMENT= 0.500000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
    COLOR= text_colour, $
;    COLOR= white, $
    FONT= oFont_axes, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= 'Beam', $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_Beam_text->Add, oText_Beam

; Z axis label

oModel_Z_text = OBJ_NEW('IDLgrModel')

oModel_Z_text->SetProperty, $
    HIDE= 0

oModel_Z_text->Rotate, [1,0,0], 90.
oModel_Z_text->Rotate, [0,0,1], 90.
oModel_Z_text->Translate, 0., 0.8*distance, -0.5*char_size

oModel1->Add, oModel_Z_text

oFont_axes = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_Z = OBJ_NEW('IDLgrText')

oText_Z->SetProperty, $
    ALIGNMENT= 0.500000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
    COLOR= text_colour, $
    FONT= oFont_axes, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= 'Z', $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_Z_text->Add, oText_Z

;-----------------------------------------------------------------------------

; Detector

oModel_det = OBJ_NEW('IDLgrModel')

oModel_det->SetProperty, $
    HIDE= 0

oModel_det->Rotate, [0,1,0], phi

oModel1->Add, oModel_det

oModel_det2 = OBJ_NEW('IDLgrModel')

oModel_det2->SetProperty, $
    HIDE= 0

oModel_det2->Rotate, [0,0,1], theta

oModel_det->Add, oModel_det2

oModel_det3 = OBJ_NEW('IDLgrModel')

oModel_det3->SetProperty, $
    HIDE= 0

oModel_det3->Rotate, [0,0,1], tilt
oModel_det3->Translate, 0.0, distance, 0.0

oModel_det2->Add, oModel_det3

oModel_det4 = OBJ_NEW('IDLgrModel')

oModel_det4->SetProperty, $
    HIDE= 0

;	do we need to rotate about the detector axis for symmetry rotations?

sX = 1.
sY = 1.
if mirrorX then sX=-1.
if mirrorY then sY=-1.

if symmetry eq 0 then symmetry=4
delta = (360./symmetry) * (reorient mod symmetry)

oModel_det4->Rotate, [0,1,0], -delta

oModel_det3->Add, oModel_det4

;........................................................................................

; Detector pad array

oModel_pad = objarr(n_det)
oPolygon_pad = objarr(n_det)
oModel_pad_label = objarr(n_det)
oModel_pad_text = objarr(n_det)
oText_pad = objarr(n_det)
oFont_pads = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')
pad_char_size = 0.7 * mean(pad.height)

for i=0L,n_det-1 do begin

	oModel_pad[i] = OBJ_NEW('IDLgrModel')

	oModel_pad[i]->SetProperty, $
	    HIDE= 0

	oModel_pad[i]->Translate, 0., thick/2., 0.

;	'tilt' is a radial tilt, so need to calcuate the X,Y projection tilts

	r = sqrt( pad[i].x*pad[i].x + pad[i].y*pad[i].y)
	tiltx = pad[i].tilt * pad[i].y / r
	tilty = pad[i].tilt * pad[i].x / r
	
	oModel_pad[i]->Rotate, [1,0,0], tiltx
	oModel_pad[i]->Rotate, [0,0,1], -tilty

;	The sX, sY affect mirror reflections, if needed

	oModel_pad[i]->Translate, sX*pad[i].x, -pad[i].z, sY*pad[i].y

	oModel_det4->Add, oModel_pad[i]

; detector pads

	if shape eq 1 then begin
		makeCube, verts_det, conn_det, sy=thick, sx=pad[i].width, sz=pad[i].height
	endif else begin
		makeCylinder, verts_det, conn_det, sx=pad[i].width, sy=thick, sz=pad[i].height, rX=90.
	endelse

	oPolygon_pad[i] = OBJ_NEW('IDLgrPolygon', verts_det)

	oPolygon_pad[i]->SetProperty, $
	    BOTTOM= [98,64,94], $
	    COLOR= detector_colour, $
	    POLYGONS= conn_det, $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_pad[i]->Add, oPolygon_pad[i]

; Pad labels

	if n_det gt 1 then begin
		oModel_pad_text[i] = OBJ_NEW('IDLgrModel')

		oModel_pad_text[i]->SetProperty, HIDE= 0

;		Offset the label by -0.5*thick, to be above pad surface

		oModel_pad_text[i]->Rotate, [1,0,0], 90.
		oModel_pad_text[i]->Translate, 0., -thick*0.5 -pad[i].width*0.1, -0.35*pad_char_size

		oModel_pad[i]->Add, oModel_pad_text[i]

		oText_pad[i] = OBJ_NEW('IDLgrText')

		oText_pad[i]->SetProperty, $
		    ALIGNMENT= 0.500000, $
	    	CHAR_DIMENSIONS= [0.75,1.00]*pad_char_size, $
		    COLOR= text_detector_colour, $
		    FONT= oFont_pads, $
		    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
		    STRINGS= str_tidy(pad[i].index), $
		    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

		oModel_pad_text[i]->Add, oText_pad[i]
	endif
endfor

;........................................................................................

; Detector normal

if (abs(tilt) gt 1.) then begin
	oPolyline_detector_normal = OBJ_NEW('IDLgrPolyline', [[0.,-0.3*distance,0.],[0.,0.,0.]])

	oPolyline_detector_normal->SetProperty, $
	    COLOR= text_colour, LINESTYLE=[2, '5555'X], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_det3->Add, oPolyline_detector_normal
endif

; Detector ray

oModel_det_axis = OBJ_NEW('IDLgrModel')

oPolyline_det = OBJ_NEW('IDLgrPolyline', [[0.,0.,0.],[0.,distance,0.]])

oPolyline_det->SetProperty, $
    COLOR= [240,12,12], $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_det2->Add, oPolyline_det

; Detector label

oModel_Detector_text = OBJ_NEW('IDLgrModel')

oModel_Detector_text->SetProperty, $
    HIDE= 0

oModel_Detector_text->Rotate, [1,0,0], 90.
oModel_Detector_text->Translate, 0., 0.0, max([pad.y,pad.x])+0.6*mean(pad.height) + 0.1*char_size

oModel_det3->Add, oModel_Detector_text

oFont_axes = obj_new('IDLgrFont', 'Helvetica*Bold*Italic')

oText_Detector = OBJ_NEW('IDLgrText')

oText_Detector->SetProperty, $
    ALIGNMENT= 0.500000, $
    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
    COLOR= text_colour, $
    FONT= oFont_axes, $
    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
    STRINGS= 'Detector', $
    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

oModel_Detector_text->Add, oText_Detector

; Theta arc

if abs(theta) gt 1. then begin
	first = 1
	for t=min([theta,0.]),max([theta,0.]),1. do begin
		if first then begin
			theta_arc = [-sin(t/!radeg),cos(t/!radeg),0.]
			first = 0
		endif else begin
			theta_arc = [[theta_arc],[-sin(t/!radeg),cos(t/!radeg),0.]]
		endelse
	endfor

	oPolyline_theta_arc = OBJ_NEW('IDLgrPolyline', r_theta*theta_arc)

	oPolyline_theta_arc->SetProperty, $
	    COLOR= [240,12,12], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_det->Add, oPolyline_theta_arc

; Theta label

	oModel_Theta_text = OBJ_NEW('IDLgrModel')

	oModel_Theta_text->SetProperty, $
	    HIDE= 0

	oModel_Theta_text->Rotate, [1,0,0], 90.
;	oModel_Theta_text->Rotate, [0,1,0], -90.
	oModel_Theta_text->Translate, 0., r_theta, 0.2*char_size
	oModel_Theta_text->Rotate, [0,0,1], Theta/2.

	oModel_det->Add, oModel_Theta_text

	oFont_axes = obj_new('IDLgrFont', 'Symbol')

	oText_Theta = OBJ_NEW('IDLgrText')

	oText_Theta->SetProperty, $
	    ALIGNMENT= 0.500000, $
	    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
	    COLOR= text_colour, $
	    FONT= oFont_axes, $
	    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
	    STRINGS= 'q', $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_Theta_text->Add, oText_Theta
endif

; Theta arc extension (if theta is less than 90)

if (abs(theta) lt 90.) and (abs(phi) gt 1.) and (abs(theta) gt 1) then begin
	first = 1
	s = (theta lt 0.) ? -1. : +1.
	for t=min([theta,s*90.]),max([theta,s*90.]),1. do begin
		if first then begin
			theta_arc = [-sin(t/!radeg),cos(t/!radeg),0.]
			first = 0
		endif else begin
			theta_arc = [[theta_arc],[-sin(t/!radeg),cos(t/!radeg),0.]]
		endelse
	endfor

	oPolyline_theta_arc = OBJ_NEW('IDLgrPolyline', r_theta*theta_arc)

	oPolyline_theta_arc->SetProperty, $
	    COLOR= [240,12,12], LINESTYLE=[2, '5555'X], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_det->Add, oPolyline_theta_arc
endif

; phi arc

if (abs(phi) gt 1.) and (abs(theta) gt 1.) then begin
	first = 1
	s = (theta gt 0.) ? +1. : -1.
	for t=min([phi,0.]),max([phi,0.]),1. do begin
		if first then begin
			phi_arc = [-s*cos(t/!radeg),0.,s*sin(t/!radeg)]
			first = 0
		endif else begin
			phi_arc = [[phi_arc],[-s*cos(t/!radeg),0.,s*sin(t/!radeg)]]
		endelse
	endfor

	oPolyline_phi_arc = OBJ_NEW('IDLgrPolyline', r_theta*phi_arc)

	oPolyline_phi_arc->SetProperty, $
	    COLOR= [240,0,0], $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel1->Add, oPolyline_phi_arc

; Phi label

	oModel_Phi_text = OBJ_NEW('IDLgrModel')

	oModel_Phi_text->SetProperty, $
	    HIDE= 0

	oModel_Phi_text->Rotate, [1,0,0], 90.
	oModel_Phi_text->Rotate, [0,1,0], 90.
	oModel_Phi_text->Translate, (r_beta+0.5*char_size),0., 0.
	oModel_Phi_text->Rotate, [0,1,0], (1.+s)*90.
	oModel_Phi_text->Rotate, [0,1,0], Phi/2.

	oModel1->Add, oModel_Phi_text

	oFont_axes = obj_new('IDLgrFont', 'Symbol')

	oText_Phi = OBJ_NEW('IDLgrText')

	oText_Phi->SetProperty, $
	    ALIGNMENT= 0.500000, $
	    CHAR_DIMENSIONS= [0.75,1.00]*char_size, $
	    COLOR= text_colour, $
	    FONT= oFont_axes, $
	    LOCATIONS= [0.00000000,0.00000000,0.00000000], $
	    STRINGS= 'f', $
	    XCOORD_CONV=[0.0,1.0], YCOORD_CONV=[0.0,1.0], ZCOORD_CONV=[0.0,1.0]

	oModel_Phi_text->Add, oText_Phi
endif

;------------------------------------------------------------------------------

oLight0 = OBJ_NEW('IDLgrLight')

oLight0->SetProperty, $
    INTENSITY= 0.500000, $
    LOCATION= [0.0000000, 2.0000000, 5.0000000], $
    TYPE= 2

oModel1->Add, oLight0


oLight1 = OBJ_NEW('IDLgrLight')

oLight1->SetProperty, $
    INTENSITY= 0.500000, $
    TYPE= 0

oModel1->Add, oLight1

if widget_info( group, /valid) then begin
	XOBJVIEW, oModel0, XSIZE=700, YSIZE=700, background=back_colour, scale=1.2, $
			title='Target Detector Beam Geometry', group=group, tlb=tlb
endif else begin
	XOBJVIEW, oModel0, XSIZE=700, YSIZE=700, background=back_colour, scale=1.2, $
			title='Target Detector Beam Geometry', tlb=tlb
endelse

return
END
