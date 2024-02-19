pro build_raster_list4, pm, plist, ii, pk, ignore_no_stage=no_stage, error=error, set_origin=set_origin

; Build a simple X-dominant zig-zag raster move list for Klee, based on scan 
; item struct parameters in (*plist)[ii]. Send it to port Klee 'pk'.
; 
; Fly scan: Move a whole line in a step, by setting velocity first.
; Use only Time max mode in this test case. Do overscan.
;
; Step scan: Move each pixel in a line, set max velocity, but do 'onpos' wait.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'build_raster_list4',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(no_stage) eq 0 then no_stage=1					; make this "1" to ignore stage errors
if n_elements(set_origin) eq 0 then set_origin=0
;common c_stage_list_velocity, vel_stage_save

error = 1
if ptr_good(pm) eq 0 then begin
	warning,'build_raster_list4','DAQ struct pointer is invalid.'
	return
endif
if (ii lt 0) or (ii ge n_elements( *plist)) then begin
	warning,'build_raster_list4','Selected scan spec index is out of range.'
	return
endif
p = (*plist)[ii]
if ptr_good(p) eq 0 then begin
	warning,'build_raster_list4','Selected scan spec pointer is invalid.'
	return
endif

; axes = ['DAC X','DAC Y','Stage X','Stage Y','Stage Z','Stage A']
; shapes = ['Rectangular','Ellipse']

DAC_axis = [0,1]			; x,y
Stage_axis = [2,3,4,5]		; x,y,z,a

; Use an X-dominant (fastest) scan raster, unless Y is DAC and X is not ...

qdacx = where( (*p).raster.axis.x eq dac_axis, nqdacx)
qdacy = where( (*p).raster.axis.y eq dac_axis, nqdacy)
use_dacx = nqdacx gt 0
use_dacy = nqdacy gt 0
x_dominant = 1
if use_dacy and (NOT use_dacx) then x_dominant = 0

; Overscan count is repeated/doubled, so an overscan=1 does 2 passes across a line.

overscan = (*p).raster.overscan / 2

xorg = (*p).origin.x									; Origin for scan area,
yorg = (*p).origin.y									; defined as bottom-left corner.
zorg = (*p).origin.z									; Offset from here for DAC axes.
org = [xorg,yorg,zorg]

;	Move to origin first ...
;	Note later for use with Maia, this will need to offset to lower DAC range as well for DAC axes.
;	Assume only stage axes for Maia for now.

use_z_axis = 0											; no Z axis at CSIRO for testing
if use_z_axis then begin
	axes = [0,1,2]
endif else begin
	axes = [0,1]
endelse

;	Now move to origin as a prliminary 'raster' spec (see below) ...

;if set_origin then begin
;	socket_command_set, pk, 'position.target', org[axes], class='stage.axis', chip=axes, error=error
;	if error then begin
;		warning,'build_raster_list4','Error moving to origin.'
;		return
;	endif
;	stage_list_wait_on_move, pk, axes=axes
;	return
;endif

if x_dominant then begin
	af = (*p).raster.axis.x								; fast axis selection = axis.x
	as = (*p).raster.axis.y
	df = (nqdacx gt 0)									; indicates a DAC axis
	ds = (nqdacy gt 0)
	sf = (*p).raster.size.x								; scan size (mm)
	ss = (*p).raster.size.y
	pf = (*p).raster.pixel.x							; pixel size (mm)
	ps = (*p).raster.pixel.y
endif else begin
	af = (*p).raster.axis.y								; fast axis selection = axis.y
	as = (*p).raster.axis.x
	df = (nqdacy gt 0)									; indicates a DAC axis
	ds = (nqdacx gt 0)
	sf = (*p).raster.size.y								; scan size (mm)
	ss = (*p).raster.size.x
	pf = (*p).raster.pixel.y							; pixel size (mm)
	ps = (*p).raster.pixel.x
endelse

; This 'mixed_dac_stage' as used so that in mxed DAC-stage motion, only the fast DAC was overscanned.
; Otherwise, the whole frame as overscanned. Now we want to overscan the fast axis all the time, even for
; stage-stage motion, so that we can still use the YLUT mechanism (possibly needing to swap X-Y). 

;mixed_dac_stage = (df and NOT ds)
mixed_dac_stage = 1										; overscan fast axis always now.

nf = round( sf/pf) > 1									; Fast,Slow pixel count
ns = round( ss/ps) > 1
nf2 = round( 0.5*sf/pf) > 1								; Half axis count, for DAC axis initial positiing
ns2 = round( 0.5*ss/ps) > 1
;qmin = long(100.*(*p).raster.charge.min)				; convert pC charge to 0.01 pC count
;qmax = long(100.*(*p).raster.charge.max)
qmin = (*p).raster.charge.min							; using charge units throughout
qmax = (*p).raster.charge.max
pmin = long((*p).raster.photons.min)					; photons as int
pmax = long((*p).raster.photons.max)
tmin = (*p).raster.time.min								; time in (s) already
tmax = (*p).raster.time.max								; we'll use 'tmax' and ignore 'tmin' in fly mode

line_tmin = 0.											; since velocity is set, we just need to bracket
;line_tmax = tmax*nf									; time for a line related to velocity?
line_tmax = 0.											; No, in fly-scan mode, set these to zero.

;	Firstly, readback key parameters from Klee ...

v = socket_command_get( pk, 'velocity.max', class='stage.axis', chip=-1, n_chip=3, error=error)
if error then begin
	warning,'build_raster_list4','Error reading "stage velocity.max" from Klee.'
	return
endif
v = [v,2.] < 2.

; Not velocity, as this is done via scratch storage now ...

;v1 = socket_command_get( pk, 'velocity', class='stage.axis', chip=-1, n_chip=4, error=error)
;if error then begin
;	warning,'build_raster_list4','Error reading "stage velocity" from Klee.'
;	return
;endif
;if set_origin then vel_stage_save = v1

socket_command_set, pk, 'velocity', 10000., class='deflect.axis', chip=-1, n_chip=2, error=error
v2 = socket_command_get( pk, 'velocity', class='deflect.axis', chip=-1, n_chip=2, error=error)
if error then begin
	warning,'build_raster_list4','Error reading "deflect velocity" from Klee.'
	return
endif
vel_max = [v2,v]										; max velocities for DAC and stage axes

stage_list_check_bounds, pm, p, pk, error=error
if error then return

vf = (sf/( (tmin>tmax) * nf )) < vel_max[af]			; velocity along line
vs = vel_max[as]										; between lines

;	Build the raster.step[] sequence ...

step = intarr(200)
com = strarr(200)
arg = strarr(200)
type = strarr(200)
i = 0
is = 0

; Absolute move start of sequence ...
 
abs_start = is
step[i] = is		&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'abs'						& i += 1
step[i] = is		&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= '2'							& i += 1
step[i] = is		&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
step[i] = is		&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(xorg,places=4)		& i += 1
step[i] = is		&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'none' 						& i += 1
step[i] = is		&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
step[i] = is		&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
is += 1

step[i] = is		&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'abs'						& i += 1
step[i] = is		&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= '3'							& i += 1
step[i] = is		&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
step[i] = is		&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(yorg,places=4)		& i += 1
step[i] = is		&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'allonpos'					& i += 1
step[i] = is		&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
step[i] = is		&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
is += 1

if use_z_axis then begin
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'abs'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= '4'							& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(zorg,places=4)		& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'allonpos'					& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	is += 1
endif

; If any DAC axes are used, do the 'up' by Stage, 'back' by DAC offset ...
; For each DAC axis, the corresponding stage axis is offset by +2 in index.
; This assumes that the DAC axes are using a 'target' of mm, just as the corresponding stage axis.
 
if df then begin

;	Stage rel move first to centre ...
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af+2)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(pf*nf2,places=4)	& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'none'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	is += 1

;	DAC abs move back to origin ...
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'abs'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(-pf*nf2,places=4)	& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'allonpos'					& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	is += 1
endif

if ds then begin

;	Stage rel move first to centre ...
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as+2)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(ps*ns2,places=4)	& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'none'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	is += 1

;	DAC abs move back to origin ...
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'abs'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pos'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(-ps*ns2,places=4)	& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'allonpos'					& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	is += 1
endif

if set_origin eq 0 then begin		; ***************************************************************************************

; Relative part of sequence ...
; Assume rectangular scan for now. Elliptical scan will need much more raster.step allocation in Klee.

; Interlace needs to do an offset that varies each time through 16x loop.
; Overscan varies according to whether one or both axes are fast (DAC). If mixed DAC/stage, then only
; the fast axis is overscanned, else the whole frame is overscanned.

snake_start = is
fast_start1 = is
if (*p).raster.step_scan then begin
	mf = 1
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmin)				& i += 1
	step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmax)				& i += 1
	step[i] = is	&	com[i] = 'dwell.charge.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmin)				& i += 1
	step[i] = is	&	com[i] = 'dwell.charge.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmax)				& i += 1
	step[i] = is	&	com[i] = 'dwell.photon.min'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmin)				& i += 1
	step[i] = is	&	com[i] = 'dwell.photon.max'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmax)				& i += 1
	step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(is)				& i += 1
	step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(nf)				& i += 1
	is += 1
endif else begin
	mf = nf			; nf-1
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmin)			& i += 1
	step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmax)			& i += 1
	is += 1
endelse

if (overscan ge 1) and mixed_dac_stage then begin

;	If mixed DAC/stage axes, in overscan mode, just overscan the fast DAC axis ...

	if (*p).raster.step_scan then begin
		mf = -1
		step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
		step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
		step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
		step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
		step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
		step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
		step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
		step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmin)				& i += 1
		step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmax)				& i += 1
		step[i] = is	&	com[i] = 'dwell.charge.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmin)				& i += 1
		step[i] = is	&	com[i] = 'dwell.charge.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmax)				& i += 1
		step[i] = is	&	com[i] = 'dwell.photon.min'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmin)				& i += 1
		step[i] = is	&	com[i] = 'dwell.photon.max'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmax)				& i += 1
		step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(is)				& i += 1
		step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(nf)				& i += 1
		is += 1
	endif else begin
		mf = -nf		;	-(nf-1)
		step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
		step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
		step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
		step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
		step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
		step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
		step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
		step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmin)			& i += 1
		step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmax)			& i += 1
		is += 1
	endelse

	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(fast_start1)		& i += 1
	step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(overscan)			& i += 1
	is += 1
endif

if (*p).raster.interlace then begin
	nis = ceil(0.5*ns / 16)
	pis = ps * 16
	ms = 16
endif else begin
	nis = (ns+1)/2										; two snake runs per 'nis'
	pis = ps
	ms = 1
endelse

step[i] = is		&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
step[i] = is		&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
step[i] = is		&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
step[i] = is		&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(ms,places=4)		& i += 1
step[i] = is		&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
step[i] = is		&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
step[i] = is		&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
is += 1

fast_start2 = is
if (*p).raster.step_scan then begin
	mf = ((overscan ge 1) and mixed_dac_stage) ? 1 : -1
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmin)				& i += 1
	step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmax)				& i += 1
	step[i] = is	&	com[i] = 'dwell.charge.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmin)				& i += 1
	step[i] = is	&	com[i] = 'dwell.charge.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmax)				& i += 1
	step[i] = is	&	com[i] = 'dwell.photon.min'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmin)				& i += 1
	step[i] = is	&	com[i] = 'dwell.photon.max'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmax)				& i += 1
	step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(is)				& i += 1
	step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(nf)				& i += 1
	is += 1
endif else begin
	mf = ((overscan ge 1) and mixed_dac_stage) ? nf : -nf
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmin)			& i += 1
	step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmax)			& i += 1
	is += 1
endelse

if (overscan ge 1) and mixed_dac_stage then begin

;	If mixed DAC/stage axes, in overscan mode, just overscan the fast DAC axis ...

	if (*p).raster.step_scan then begin
		mf = -1
		step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
		step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
		step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
		step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
		step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
		step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
		step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
		step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmin)				& i += 1
		step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(tmax)				& i += 1
		step[i] = is	&	com[i] = 'dwell.charge.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmin)				& i += 1
		step[i] = is	&	com[i] = 'dwell.charge.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(qmax)				& i += 1
		step[i] = is	&	com[i] = 'dwell.photon.min'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmin)				& i += 1
		step[i] = is	&	com[i] = 'dwell.photon.max'		&	type[i] = 'int'	&	arg[i]= str_tidy(pmax)				& i += 1
		step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(is)				& i += 1
		step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(nf)				& i += 1
		is += 1
	endif else begin
		mf = -nf		;	-(nf-1)
		step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
		step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
		step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
		step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(mf,places=4)		& i += 1
		step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
		step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
		step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
		step[i] = is	&	com[i] = 'dwell.time.min'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmin)			& i += 1
		step[i] = is	&	com[i] = 'dwell.time.max'		&	type[i] = 'flt'	&	arg[i]= str_tidy(line_tmax)			& i += 1
		is += 1
	endelse

	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(af)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(fast_start2)		& i += 1
	step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(overscan)			& i += 1
	is += 1
endif

step[i] = is		&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
step[i] = is		&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
step[i] = is		&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
step[i] = is		&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(ms,places=4)		& i += 1
step[i] = is		&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
step[i] = is		&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
step[i] = is		&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
step[i] = is		&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(snake_start)		& i += 1
step[i] = is		&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(nis)				& i += 1
is += 1

;	Fly-back in preparation for interlace or overscan ...

step[i] = is		&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
step[i] = is		&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
step[i] = is		&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
step[i] = is		&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(-2*ms * nis,places=4)	& i += 1
step[i] = is		&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
step[i] = is		&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
step[i] = is		&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
is += 1

if (*p).raster.interlace then begin

;	Here we need to do a Rel move to offset to +ps, so that we start next interlaced frame offset correctly by one step.

	ms = 1
	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(ms,places=4)		& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(snake_start)		& i += 1
	step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(16)				& i += 1
	is += 1	

	;	Here we need to do a Rel move to offset back the 16*ps, so that we start next frame correctly.

	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= str_tidy(-16*ms,places=4)	& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	is += 1
endif

if (overscan ge 1) and NOT mixed_dac_stage then begin

;	If 100x scan, then repeat all 100x. 
;	If NOT mixed DAC/stage axes, in overscan mode, overscan the full frame here ...

	step[i] = is	&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'rel'						& i += 1
	step[i] = is	&	com[i] = 'move.dim'				&	type[i] = 'int'	&	arg[i]= str_tidy(as)				& i += 1
	step[i] = is	&	com[i] = 'move.measure'			&	type[i] = 'str'	&	arg[i]= 'pixel'						& i += 1
	step[i] = is	&	com[i] = 'move.target'			&	type[i] = 'flt'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'move.wait'			&	type[i] = 'str'	&	arg[i]= 'onpos'						& i += 1
	step[i] = is	&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '0'							& i += 1
	step[i] = is	&	com[i] = 'loop.step'			&	type[i] = 'int'	&	arg[i]= str_tidy(snake_start)		& i += 1
	step[i] = is	&	com[i] = 'loop.repeat'			&	type[i] = 'int'	&	arg[i]= str_tidy(overscan)			& i += 1
	is += 1
endif

endif					; *** end only for relative scan, not set_origin *****************************************************

step[i] = is		&	com[i] = 'move.type'			&	type[i] = 'str'	&	arg[i]= 'stop'						& i += 1
step[i] = is		&	com[i] = 'move.beam.enable'		&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
step[i] = is		&	com[i] = 'dwell.beam.enable'	&	type[i] = 'int'	&	arg[i]= '1'							& i += 1
is += 1

nis = is
n = i
error = 0
n_steps = 32
if nis gt n_steps then begin
	warning,'build_raster_list4',['Number of "raster.step"s ('+str_tidy(nis)+') exceeds Klee allocation.', $
				'Cannot execute this style of raster without more Klee resources.']
	return
endif

show_raster_list, step, com, arg, type

socket_command_set, pk, 'move.type', 'stop', class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'move.dim', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'move.measure', 'pos', class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'move.target', 0.0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'move.wait', 'none', class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'move.beam.enable', 1, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.charge.min', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.charge.max', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.photon.min', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.photon.max', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.time.min', 0.0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.time.max', 0.0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'dwell.beam.enable', 1, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'loop.repeat', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'loop.step', 0, class='raster.step', chip=-1, n_chip=n_steps, error=err & error=(error or err)
socket_command_set, pk, 'step', 0, class='raster.start', error=err & error = error or err
socket_command_set, pk, 'enable', 0, class='pixel.dim', chip=[0,1,2,3,4], n_chip=5, error=err & error = error or err
if error then begin
	warning,'build_raster_list4','Error sending initial "socket_command_get" commands to Klee.'
	return
endif

for i=0,n-1 do begin
	case type[i] of
		'int': begin
			val = long(arg[i])
			end
		'str': begin
			val = arg[i]
			end
		'flt': begin
			val = float(arg[i])
			end
	endcase
	
	socket_command_set, pk, com[i], val, class='raster.step', chip=step[i], error=err
	if err then begin
		warning,'build_raster_list4',['Error sending "socket_command_get" "raster.step" command to Klee:', $
					'command='+com[i], 'arg='+arg[i], 'type='+type[i], 'step='+str_tidy(step[i])] 
		error = error or err
		return
	endif
endfor	

if set_origin eq 0 then begin		; ***************************************************************************************

	if (*p).raster.step_scan eq 0 then begin
		if af ge 2 then begin
			socket_command_set, pk, 'velocity', vf, class='stage.axis', chip=af-2, error=err & error=(error or err)
		endif else begin
			socket_command_set, pk, 'velocity', vf, class='deflect.axis', chip=af, error=err & error=(error or err)
		endelse
		if as ge 2 then begin
			socket_command_set, pk, 'velocity', vs, class='stage.axis', chip=as-2, error=err & error=(error or err)
		endif else begin
			socket_command_set, pk, 'velocity', vs, class='deflect.axis', chip=as, error=err & error=(error or err)
		endelse
	endif else begin
		if af ge 2 then begin
			socket_command_set, pk, 'velocity', vel_max[af], class='stage.axis', chip=af-2, error=err & error=(error or err)
		endif else begin
			socket_command_set, pk, 'velocity', vel_max[af], class='deflect.axis', chip=af, error=err & error=(error or err)
		endelse
		if as ge 2 then begin
			socket_command_set, pk, 'velocity', vel_max[as], class='stage.axis', chip=as-2, error=err & error=(error or err)
		endif else begin
			socket_command_set, pk, 'velocity', vel_max[as], class='deflect.axis', chip=as, error=err & error=(error or err)
		endelse
	endelse

;	Note that the crossref is not valid yet. The Maia blog has just started, but the run-number has not
;	been collected yet. This is done in 'next' routine. Perhaps needs to be sooner too.

;	Save whole scan struct ...
	
	s = stringify( *p)
;	pointer_display, p
	print,'info string=',s
	socket_command_set, pk, 'info', s, class='metadata.scan', error=err & error = error or err

	s = '"' + str_tidy(af) + ' ' + str_tidy(as) + '"'
	socket_command_set, pk, 'order', s, class='metadata.scan', error=err & error = error or err

	socket_command_set, pk, 'name', '"'+(*p).sample+'"', class='metadata.sample', error=err & error = error or err
;	socket_command_set, pk, 'type', (*p).?, class='metadata.sample.type', error=err & error = error or err
;	socket_command_set, pk, 'serial', (*p).?, class='metadata.sample.serial', error=err & error = error or err
	socket_command_set, pk, 'region', '"'+(*p).grain+'"', class='metadata.scan', error=err & error = error or err
	socket_command_set, pk, 'info', '"'+(*p).comment+'"', class='metadata.sample', error=err & error = error or err
;	socket_command_set, pk, 'crossref', '"'+(*p).crossref+'"', class='metadata.scan', error=err & error = error or err

	socket_command_set, pk, 'energy', (*pm).beam.energy*1.0e+6, class='metadata.beam', error=err & error = error or err
	socket_command_set, pk, 'particle', (*pm).beam.particle, class='metadata.beam', error=err & error = error or err

	socket_command_set, pk, 'coeff', (*pm).beam.charge.scale, class='charge', error=err & error = error or err
	socket_command_set, pk, 'unit', (*pm).beam.charge.unit, class='charge', error=err & error = error or err

	if use_dacx or use_dacy then begin
		socket_command_set, pk, 'source', 'deflect', class='fdac', error=err & error = error or err
	endif
	socket_command_set, pk, 'source', (*p).raster.axis.x, class='stepdir.chan', chip=0, error=err & error = error or err
	socket_command_set, pk, 'source', (*p).raster.axis.y, class='stepdir.chan', chip=1, error=err & error = error or err

	socket_command_set, pk, 'origin', org, class='pixel.dim', chip=[2,3,4], n_chip=3, error=err & error = error or err

	socket_command_set, pk, 'pitch', pf, class='pixel.dim', chip=af, error=err & error = error or err
	socket_command_set, pk, 'pitch', ps, class='pixel.dim', chip=as, error=err & error = error or err
	if df then begin
		socket_command_set, pk, 'origin', -sf/2., class='pixel.dim', chip=af, error=err & error = error or err
	endif
	socket_command_set, pk, 'position.target', zorg, class='stage.axis', chip=2, quiet=no_stage, error=err & error = error or (err and (no_stage eq 0))
	socket_command_set, pk, 'coord.extent', nf, class='pixel.dim', chip=af, error=err & error = error or err
	socket_command_set, pk, 'enable', 1, class='pixel.dim', chip=af, error=err & error = error or err
	if ds then begin
		socket_command_set, pk, 'origin', -ss/2., class='pixel.dim', chip=as, error=err & error = error or err
	endif
	socket_command_set, pk, 'coord.extent', ns, class='pixel.dim', chip=as, error=err & error = error or err
	socket_command_set, pk, 'enable', 1, class='pixel.dim', chip=as, error=err & error = error or err
	
	socket_command_set, pk, 'select', 1, class='raster.photon.chan', chip=-1, n_chip=36, error=err & error = error or err
	socket_command_set, pk, 'enable', 1, class='pixel', error=err & error = error or err
	socket_command_set, pk, 'enable', 1, class='photon', error=err & error = error or err
	socket_command_set, pk, 'enable', 1, class='beam.photon', error=err & error = error or err
	socket_command_set, pk, 'enable', 1, class='beam', error=err & error = error or err
	if error then begin
		warning,'build_raster_list4','Error setting up raster sequence.'
		return
	endif
	
	socket_command_set, pk, 'newrun', 1, class='blog', error=err & error = error or err

endif					; *** end only for relative scan, not set_origin *****************************************************

;	Just does move to origin via raster spec, if /set_origin
;	Start raster scan, if set_origin=0

	socket_command_set, pk, 'enable', 1, class='raster', error=err & error = error or err	
	socket_command_set, pk, 'step', 0, class='raster.start', error=err & error = error or err
	if error then begin
		warning,'build_raster_list4','Error starting raster sequence.'
		return
	endif
	
	(*pm).DevObj->set_options, x_axis = (*p).raster.axis.x, y_axis = (*p).raster.axis.y

return
end
	