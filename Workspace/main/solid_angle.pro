function solid_angle, distance, diameter, tilt=tilt, array=array, shape=shape, $
			layout_file=layout, playout=playout

; Solid angle (msr)
;	Calculate single solid-angle for a single (non-array) detector using 'diameter'
;	and 'distance'. 'Shape' modifies this for a square detector.
;
;	For an array, the width and height of each element come from the layout file.
;	Pass this either as the file name, or a pre-loaded layout in 'playout' pointer.
;	The solid-angle includes the effects of 'vetoed' channels in the layout file.
;
;	distance	distance to single or array detector (mm)
;	diameter	diameter of 'generic' detector (mm) - not used for an array
;	tilt		overall tilt angle of single or array detector (degree)
;	shape=1		a square detector (0=round)
;	/array		a detector array (width, height, tilt come from layout)
;	layout		layout file name
;	playout		pointer to layout struct (alt to file approach)

	solid_angle = 0.0
	if (distance gt 0.001) then begin
		if array then begin
			if (n_elements(playout) eq 0) then begin
				if n_elements(layout) eq 0 then begin
					warning,'solid_angle','array, but no file or structure supplied.'
					goto, done
				endif
				d = read_detector_layout(layout, error=error)
				if error then begin
					warning,'solid_angle','bad read of detector layout file.'
					goto, done
				endif
				playout = ptr_new(d, /no_copy)
			endif
			if (n_elements(*playout) eq 0) then begin
;				warning,'solid_angle','layout pointer empty.'
				goto, done
			endif

			g = detector_geometry( playout, distance, 180., 0.0, tilt=tilt, error=error)
			if error then begin
				warning,'solid_angle','error from "detector_geometry"'
				goto, done
			endif

			area = (*playout).data.width * (*playout).data.height
			if shape eq 0 then area = area * !pi/4.
			if (*playout).veto then begin
				q = where( (*playout).data.bad eq 0)
			endif else begin
				q = where( (*playout).data.bad le 1)
			endelse
			solid_angle = 1000. * total ( area[q]*cos(g[q].tilt/!radeg)/(g[q].R*g[q].R) )

		endif else begin

;			This uses the small solid-angle approximation: area/d^2
;			When should we switch to the full formula?
;				 2.* !pi * (1. - cos(angle)), where angle is cone half-angle (tan(angle)=r/d)

			area = diameter * diameter
			if shape eq 0 then area = area * !pi/4.
			solid_angle = 1000.*cos(tilt/!radeg) * area/(distance * distance)
		endelse
	endif

done:
	return, solid_angle
end
