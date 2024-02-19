pro maia_update_da2, pstate, update=update, error=error

; Triggered by a timer event on the "Images" button, scan DA image
; shared memory for new DA image. This assumes increment done in blog client.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0

	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'maia_update_da2',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	
error = 1
update = 0
tin = systime(/seconds)
busy = 0.0
buffers_lost = 0.0

if ptr_valid(pstate) eq 0 then goto, fin
ps = (*pstate).psocket
if ptr_valid(ps) eq 0 then goto, fin
if (*(*pstate).psocket).open eq 0 then goto, fin

; '(*pm).control' and 'data' tables are in CSV layout file order.
; 'data.index' returns the detector number for each member of these tables. Use this to
; index returned data 'v' in detector order to give CSV table order for assigns.
; 'data.hermes' returns the Hermes/Scepter chip number for each member of these tables.
 
pm = (*pstate).pmaia
play = (*pstate).playout
pr = (*pstate).preadout
pimage = (*pstate).pimage
n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32
ref = (*play).ref[ (*play).start + indgen((*play).N) ]	; index back from CSV table to detector order

;	par array:	0	n_buffers
;				1	free
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-8	Redirect indices for x,y,z axes
;				9	Flip X axis
;				10	Scan X size in pixels
;			 11-12	free
;			 13-15	buffer_size (1-3 dimensions)
;	                                                               Redirection
;	  incoming (from maia_launch ...):   reset kill                 x   y   z
;	  outgoing (to maia_launch ...):                kill running                    buffer_size
;	                   0           1       2    3    4     5        6   7   8  ...   13

pshrmem_pars = (*pstate).pshrmem_pars			; pointer to Maia parameters shared memory
if (*pshrmem_pars).error then goto, fin

maia_launch_read_da, ps, pm, pimage, /scan, /deadtime, pshrmem=pshrmem_pars

;	pf	0	% busy				pl	0	blog port			pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval				2	N DA in use
;		3	IC rate (from Epics)	3	X compress
;		4	sensitivity				4	Y compress
;		5	X						5	Dwell index
;		6	Y						6	total used pixels
;		7	energy (from Epics)		7	max X		
;		8	total flux				8	max Y	
;		9							9	activate blog_da_file
;		10+	scale					10+	maia_IC_name (as bytes)

pshrmem = (*pstate).pshrmem_da
pimage = (*pstate).pimage
scale_offset = 10
if (*pshrmem).error eq 0 then begin
	ppar = (*pshrmem).ppar
	pl = (*pshrmem).plong[0]
	pd = (*pshrmem).pdat[0]
	pf = (*pshrmem).pfloat[0]
	(*pshrmem).loop = 0
	pv = (*pshrmem).pvalid[0]
	valid = (*pv)[0]
	(*pv)[0] = 0
	(*pl)[9] = 0					; set to one to activate blog_file_da2, DA from blog files,
									; or set break point in blog_file_da2
	busy = (*pf)[0]
	buffers_lost = (*pf)[1]
	(*pm).run.energy = (*pf)[7]		; beam energy from blog_client_epics
	
	(*pm).DA.memory.X = n_elements( (*pd)[*,0,0])
	(*pm).DA.memory.Y = n_elements( (*pd)[0,*,0])
	if (*pm).DA.N eq 0 then (*pm).DA.on=0
	if (*pm).DA.on then begin
	
;		Copy the scale factors to the blog_client_da2 process.
;		Scale each by the 1/rG sum of the Gamma geometry factors, summed over
;		active detector channels (as in *(*pimage).pactive)
;		(N.B. The rG factors are in detector number order)

		scale = (*pm).DA.scale
		if ptr_good( (*pm).DA.parray) then begin
			if (*(*pm).DA.parray).On then begin
				q = *(*pimage).pactive
				nel = n_elements((*(*pm).DA.parray).rGamma[0,*]) < (*pm).DA.N
				for i=0,nel-1 do begin
					rG = total( (*(*pm).DA.parray).rGamma[ q, i] )
					scale[i] = scale[i] / rG
				endfor
			endif
		endif
		
		(*pl)[2] = (*pm).DA.N
		(*pf)[scale_offset:scale_offset+(*pm).DA.N-1] = scale[0:(*pm).DA.N-1]

		b = byte( (*pm).IC.pv.name)
		nb = n_elements(b)
		if nb gt 0 then (*pl)[10:10+nb-1] = long(b)
		(*pf)[4] = (*pm).IC.pv.val * (*pm).IC.pv.unit
	endif
	if (*pm).scan.on then begin
		(*ppar)[6:8] = (*pm).DA.axes				; set axis redirection
		(*ppar)[10] = (*pm).scan.X					; set X axis scan size

		rx = ceil(float((*pm).scan.X) / float((*pm).DA.memory.X)) > 1
		ry = ceil(float((*pm).scan.Y) / float((*pm).DA.memory.Y)) > 1
		r = rx > ry
		if r ne (*pimage).xcompress then begin
			print, 'maia_update_da2: DA image compression changed to = ',r
			log_message, (*pstate).comms, type='INFO', 'maia_launch maia_update_da2, DA image compression changed to = '+str_tidy(r)
		endif
		(*pimage).xcompress = r
		(*pimage).ycompress = r
		if (*pm).DA.on then begin
			maia_launch_update_DA_images, pimage, pm
			(*pl)[5] = (*pm).DA.N
		endif
		(*pl)[3] = r
		(*pl)[4] = r
		(*pimage).original_xsize = (*pm).scan.X / r	; actual scan sized saved in image here
		(*pimage).original_ysize = (*pm).scan.Y / r	; xsize,ysize is buffer size for now.
		(*pimage).bounds.valid = 1
		(*pimage).bounds.xmax = (*pl)[7]			; max X,Y (compressed) observed in data
		(*pimage).bounds.ymax = (*pl)[8]
		(*pimage).energy = (*pf)[7]					; beam energy from blog_client_epics
		(*pimage).temp.total_flux = (*pf)[8]		; total flux from blog_client_da2
		(*pimage).temp.total_pixels = (*pl)[6]		; total pixels used from blog_client_da2
		(*pimage).temp.valid = 0					; flag need to update charge_map, etc.
		
		maia_launch_update_image_cal, pimage, pm, play		; update image *pcal, cal
	endif
endif

	; DA image memory in shared memory directly updated by backgnd process.
	
fin:
	t = systime(/seconds)
	t_interval = t - (*pstate).time.update.da
	(*pstate).time.update.da = t
	percent = 100. * (t - tin) / t_interval
	
	(*pstate).activity.process.da = percent + busy
	(*pstate).activity.buffers.da = buffers_lost
	if percent gt 10. then (*pstate).time_da = (2.*(*pstate).time_da) < 30. 
	if percent lt 5. then (*pstate).time_da = (0.5*(*pstate).time_da) > 2. 
	if percent gt 10. then print,'Update DA: percent=',percent,'  t-tin=',t-tin,'  busy=',busy,' lost=',buffers_lost,' time_da=',t_interval

	t_interval2 = t - (*pstate).time.display.da
	if t_interval2 gt (*pstate).time_da_update then begin
		(*pstate).time.display.da = t
		update = 1
	endif
	error = 0
	return
end
