pro evt_start, pstate, group=group, notify=notify, error=evt_error, gencom=gencom, $
				export=export, suppress=suppress, pprefs=pprefs, verify=verify, file_return=stret

; Perform sorts for Sort EVT. Calls 'cluster_client' in cluster mode ...
; Assumes a valid 'pstate' from Sort EVT.
;
; /Notify	update widgets and notify of new images, path, ...
; /Suppress suppress pop-ups
; /verify	verify all filenames by testing or prompting via file_requester()
;
; /Gencom	generate a "GeoPIXE command file" (.gcf) instead of execution.
; /Export	translate input EVT into Maia output format.

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
		warning,'evt_start',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

common c_debug_dummy, dummy_write
if n_elements(dummy_write) lt 1 then dummy_write=0

file_ext = [[['*.dam','*.cuts','*.cuts','*.mpdam','',''],['*.dam','*.region','','','',''],['*.dam','*.cuts','*.cuts','','','']], $
		[['*.damg','*.cuts','*.cuts','','',''],['*.damg','*.cuts','*.cuts','','',''],['*.damg','*.cuts','*.cuts','','','']], $
		[['*.damr','*.cuts','*.cuts','','',''],['*.damr','*.cuts','*.cuts','','',''],['*.damr','*.cuts','*.cuts','','','']], $
		[['*.dame','*.cuts','*.cuts','','',''],['*.dame','*.cuts','*.cuts','','',''],['*.dame','*.cuts','*.cuts','','','']], $
		[['*.dams','*.cuts','*.cuts','','',''],['*.dams','*.cuts','*.cuts','','',''],['*.dams','*.cuts','*.cuts','','','']], $
		[['*.damc','*.cuts','*.cuts','','',''],['*.damc','*.cuts','*.cuts','','',''],['*.damc','*.cuts','*.cuts','','','']], $
		[['*.damd','*.cuts','*.cuts','','',''],['*.damd','*.cuts','*.cuts','','',''],['*.damd','*.cuts','*.cuts','','','']], $
		[['*.damx','*.cuts','*.cuts','*.mpdam','',''],['*.damx','*.cuts','*.cuts','','',''],['*.damx','*.cuts','*.cuts','','','']]]

file_title = [['Select DA Matrix file','Select CUTS specification file','Select STIM energy CUTS file','Select MPDA MPDAM file','',''], $
		['Select DA Matrix file','Select CUTS specification file','Select STIM energy CUTS file','Select ?','',''], $
		['Select DA Matrix file','Select CUTS specification file','Select STIM energy CUTS file','Select ?','','']]

	evt_error = 1
	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr
	if n_elements(notify) lt 1 then notify=1
	if n_elements(suppress) lt 1 then suppress=0
	if n_elements(verify) lt 1 then verify=0
	if n_elements(group) lt 1 then begin
		group = 0L
		notify=0
	endif
	if n_elements(export) lt 1 then export=0
	if n_elements(gencom) lt 1 then gencom=0

	DevObj = (*(*p).pDevObjList)[(*p).device]
	
	if ptr_good( pprefs,/struct) eq 0 then begin
	  	prefs = geopixe_defaults( error=err, source='evt_start')
		pprefs = ptr_new(prefs)
	endif
	cluster_prefs = (*pprefs).cluster
	mpdam_string = ''

	file = (*p).file[(*p).station]
	if extract_extension( file) eq 'mpdam' then mpda = 1 else mpda=0
	raw = (*p).evt_file

	if verify eq 0 then goto, cont

;....................................................................................

;	This section tests all input files, searches for them if necessary, and prepares
;	a return struct of the updated file paths.

	blog = {old:'',new:''}
	pileup = {old:'',new:''}
	throttle = {old:'',new:''}
	linear = {old:'',new:''}
	dam = {old:'',new:''}
	if file_test( *(*pstate).dpath, /dir) eq 0 then *(*pstate).dpath = *(*pstate).path

	dpath = *(*pstate).dpath
	path = *(*pstate).path
	file = (*p).evt_file
	dir_mode = (((*p).sort_mode eq 1) and (*p).XANES_dir)
	F = file_requester( /read, filter = ((*p).sort_mode eq 1) ? '*' : ('*'+DevObj->extension()), $
		path=[dpath,path], group=group, file=file, dir=dir_mode, $
		title='Select the source list-mode data', fix_filter=0, /translate, updir=5, $
		numeric=(DevObj->multi_files() and DevObj->extension() eq ''), /skip_if_exists )
	if F[0] eq '' then goto, finish
	raw = F[0]
	if F[0] ne (*p).evt_file then begin
		blog.new = F[0]
		blog.old = (*p).evt_file
		evt_set_evt_file, pstate, F[0], group, /no_output, /no_cal_adopt
	endif

	dpath = *(*pstate).dpath
	file = (*p).evt2_file
	if file[0] ne '' then begin
		F = file_requester( /read, filter = ((*p).sort_mode eq 1) ? '*' : '*'+DevObj->extension(), $
			path=dpath, group=group, file=file, dir=(((*p).sort_mode eq 1) and (*p).XANES_dir), $
			title='Select the last list-mode data', fix_filter=0, /latest, $
			numeric=(DevObj->multi_files() and (DevObj->extension() eq '')), /skip_if_exists)
		evt_set_evt2_file, pstate, F[0]
	endif

	file = (*p).throttle_file
	if file[0] ne '' then begin
		F = file_requester( /read, filter = ['*.throttle.var','*.txt'], file=file[0], $
			path=path, group=group, /translate, updir=4, $
			title='Select the throttle factors file', fix_filter=0, /skip_if_exists)
		if F[0] eq '' then goto, finish
		if F[0] ne (*p).throttle_file then begin
			throttle.new = F[0]
			throttle.old = (*p).throttle_file
			evt_set_throttle_file, pstate, F[0]
		endif
	endif

	if ((*p).sort_mode eq 1) and ((*p).XANES_dir eq 0) then begin
		file1 = (*p).xanes_energies_file
		title = 'Select the XANES energies file'
		fext = ['*.csv','*.txt']
	endif else begin
		file1 = (*p).pileup_file
		title = 'Select the pileup time limits file'
		fext = ['*.pileup.var','*.txt']
	endelse
	file = file1
	path = *(*pstate).path
	if file[0] ne '' then begin
		F = file_requester( /read, filter=fext, file=file[0], /translate, updir=4, $
			path=path, group=group, title=title, fix_filter=0, /skip_if_exists)
		if F[0] eq '' then goto, finish
		if F[0] ne file1 then begin
			pileup.new = F[0]
			pileup.old = file1
			evt_set_pileup_file, pstate, F[0]
		endif
	endif
	
	file = (*p).linearize_file
	path = *(*pstate).path
	if file[0] ne '' then begin
		F = file_requester( /read, filter = ['*.linear.var','*.linear'], file=file[0], $
			path=path, group=group, /translate, updir=4, $
			title='Select the linearization function file', fix_filter=0, /skip_if_exists)
		if F[0] eq '' then goto, finish
		if F[0] ne (*p).linearize_file then begin
			linear.new = F[0]
			linear.old = (*p).linearize_file
			evt_set_linear_file, pstate, F[0]
		endif
	endif
		
;	DO NOT test output_file here, as it is done in evt_start ...

	if (*p).mode[(*p).station] ne 4 then begin
		file = (*p).file[(*p).station]
		path = *(*pstate).path
		filt = file_ext[(*p).mode[(*p).station],(*p).sort_mode,(*p).type[(*p).station]]
		F = file_requester( /read, filter=filt, path=path, group=group, file=file[0], /translate, updir=4, $
				title=file_title[(*p).mode[(*p).station],(*p).sort_mode], fix_filter=0, /skip_if_exists)
		if F[0] eq '' then goto, finish
		if F[0] ne (*p).file[(*p).station] then begin
			dam.new = F[0]
			dam.old = (*p).file[(*p).station]
			evt_set_proj_file, pstate, F[0]
			file = F[0]
		endif
	endif

;	Flag any changes during 'verify' for return (e.g. to Wizard that needs to update files/paths).

	stret = { blog:blog, pileup:pileup, throttle:throttle, linear:linear, dam:dam }

;...............................................................................................................
	
cont:
;	In order to test all files in mpdam (and its referenced .correct and all DA matrix files), these are
;	done in 'evt_start' and put in an updated mpdam struct, stringified into string 'mpdam_string'.
;	This way all file-names passed to 'da_evt' are good, but passed in 'mpdam_string' argument to 'da_evt',
;	which will work in a cluster mode sub-process where files must be found.

	mpda = check_mpdam( file, DevObj, raw=raw, stringed=mpdam_string, phase_good=phase_good, original=original, $
								verify=verify, error=err)
	if err then goto, finish
	
		active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file, /alert)

		if (mode eq 3) and (mpda eq 0) then goto, bad_sort_mode
		if (mode eq 3) then mode=0											; make MPDA mode (mode=3) use DA routines.

		if strlen((*p).output_file) lt 1 then (*p).output_file='default'		
		print,'EVT start: test file write = ',(*p).output_file
		if file_write_test( (*p).output_file) eq 0 then goto, finish		; test/create dir and test file writeable
		
start_more:
		pileup = ''		
		throttle = ''		
		linearize = ''	
		if DevObj->pileup() then pileup = (*p).pileup_file
		if DevObj->throttle() then throttle = (*p).throttle_file
		if DevObj->linear() then linearize = (*p).linearize_file
;		if DevObj->name() eq 'MAIA_DEVICE' then energies = (*p).xanes_energies_file
		(*p).flux = 0.0
		charge = (*p).charge

		xcompress = (*p).xcompress + 1			; (*p).xcompress is the droplist index, add one for compress
		ycompress = (*p).ycompress + 1
		zcompress = (*p).zcompress + 1
		bit = (*pstate).bit_numbers[(*p).step_bit]
		steps = (*p).step_size_index + 1
		scanx = (*p).xsize
		scany = (*p).ysize
		scanz = (*p).zsize
		xorigin = (*p).xorigin
		yorigin = (*p).yorigin
		zorigin = (*p).zorigin
		xrange = (*p).xrange > 2
		yrange = (*p).yrange > 2
		zrange = (*p).zrange > 2

;		xoffset = ((*p).xoffset > 0) < (xrange-1)
;		yoffset = ((*p).yoffset > 0) < (yrange-1)
;		xoffset = (*p).xoffset < (xrange-1)										;@23-5-18
;		yoffset = (*p).yoffset < (yrange-1)										;@23-5-18

		xoffset = (*p).xoffset													;@23-5-18
		yoffset = (*p).yoffset													;@23-5-18
		if (*p).image_mode eq 1 then begin
;			x_sub_range = ((*p).x_sub_range > 1) < (xrange - xoffset)			;@23-5-18
;			y_sub_range = ((*p).y_sub_range > 1) < (yrange - yoffset)			;@23-5-18

			x_sub_range = ((*p).x_sub_range > 1)								;@23-5-18
			y_sub_range = ((*p).y_sub_range > 1) 								;@23-5-18
			if (xoffset gt 0) and (xrange lt x_sub_range+xoffset) then begin	;@23-5-18
				scanx = scanx * (x_sub_range+xoffset)/xrange					;@23-5-18
				xrange = x_sub_range+xoffset									;@23-5-18
			endif
			if (yoffset gt 0) and (yrange lt y_sub_range+yoffset) then begin	;@23-5-18
				scany = scany * (y_sub_range+yoffset)/yrange					;@23-5-18
				yrange = y_sub_range+yoffset									;@23-5-18
			endif

;			dx = scanx*0.001 / (xrange-1)
;			dy = scany*0.001 / (yrange-1)
;			scanx = dx * (xrange-1) * 1000.
;			scany = dy * (yrange-1) * 1000.
;			xorigin = xorigin + xoffset * dx
;			yorigin = yorigin + yoffset * dx

			xcompress = 1
			ycompress = 1
		endif else begin
			xoffset = 0L
			yoffset = 0L
			x_sub_range = xrange
			y_sub_range = yrange
;			dx = 0.0
;			dy = 0.0
		endelse
		collapse_energy = (*p).collapse_energy
		print,'EVT_start: XYZ range, XYZ compress - ',xrange,yrange,zrange, xcompress,ycompress,zcompress
;		pointer_display, p

		dir_mode = ((*p).sort_mode eq 1) and ((*p).XANES_dir eq 1)
		
		evt_files = select_evt_files( (*p).evt_file, (*p).evt2_file, $
						DevObj->multi_files(), DevObj->multi_char(), $
						DevObj->extension(), embed_detector=DevObj->embed_detector(), $
						test=dummy_write, dir=dir_mode)

		print,'EVT start time = ',systime()
		tic
		
; Devices with numeric file extensions can have a YLUT to make sorting for region spectra quicker.
; These are built in DA_evt (or here in cluster mode), but not for special incremental modes that must
; be sorted fully, such as the Maia 'Correct Y Encoder' mode. These are built during DA_EVT processing.
; If a YLUT exists it is retrieved, else a new one is built. Delete bad YLUT files.

		if (*p).cluster and (((*p).sort_mode eq 0) or ((*p).sort_mode eq 2)) then begin
			if DevObj->ylut() then begin
				print,'EVT start: build YLUT first, based on: ', evt_files[0]
				ylut = DevObj->build_ylut( evt_files, output=(*p).output_file, error=err)
				print,'EVT start: build YLUT finished.'
				if err then goto, bad_ylut_build
			endif else begin
				warning,'EVT start',['Device, Slow Axis choices do not support a YLUT test.', $
						'Cluster mode will use excessive memory in this case.', $
						'Disable "Cluster" mode to proceed.']
				return
			endelse
		endif else begin
			if (*p).cluster then print,'EVT start: Sort Mode does not support a YLUT test.'
		endelse

; If a limited yoffset, yrange have been used, we may need to further prune down the list of EVT files.
; But if a negative yoffset is used, we assume this is to fix bad X,Y and veto further pruning.

		if DevObj->ylut() and (yoffset ge 0) and (((*p).sort_mode eq 0) or ((*p).sort_mode eq 2)) then begin
			ylut = DevObj->get_ylut( evt_files, output=(*p).output_file, error=err)
			if (err eq 0) then begin
				if (n_elements(Ylut) gt 1) then begin
					pYlut = ptr_new(ylut, /no_copy)
					evt_files = DevObj->trim_evt_files( evt_files, yoffset=yoffset, yrange=y_sub_range, pYlut=pYlut)
				endif
			endif
		endif
;		widget_control, /hourglass
		devpars = DevObj->get_options(error=error)

		print,'EVT files: # = ',n_elements(evt_files)
		n_files = n_elements(evt_files)
		if n_files eq 0 then goto, bad_no_evt
		print, evt_files

		cluster = (*p).cluster
		if n_files le 3 then cluster=0

		if export then begin
				case (*p).xy_mode of

					0: begin								; normal XY scan
;print,'normal XY scan:'
;print,'    active=',active
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file

						cmit_evt, evt_files, $
							device = DevObj->name(), $
							channel = active, $					; vector in "Multiple detector" mode
							detector = type, $
							charge = charge, $
							cal_a = cal_a, $					; vector in "Multiple detector" mode
							cal_b = cal_b, $					; vector in "Multiple detector" mode
							xrange = xrange, $
							yrange = yrange, $
							xcompress = xcompress, $
							ycompress = ycompress, $
							events = 0L, $
							scanx = scanx, $
							scany = scany, $
							sample = (*p).sample, $
							grain = (*p).grain, $
							comment = (*p).comment, $
							throttle = throttle, $
							output = (*p).output_file, $
							group = group, $
							/progress
						end

					1: begin								; X step mode
;print,'X step image scan:'
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    steps=',steps,' step_size=',(*p).step_size
;print,'    step-mode=',(*p).step_mode,' station=',(*p).step_station,' bit=',bit
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file
;print,'    active=',active, ' mode=',mode
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b

						xstep_count = (*p).step_count

						cmit_xstep_evt, evt_files, xstep_count, $
							device = DevObj->name(), $
							channel = active, $
							detector = type, $
							charge = charge, $
							cal_a = cal_a, $
							cal_b = cal_b, $
							xrange = xrange, $
							yrange = yrange, $
							xcompress = xcompress, $
							ycompress = ycompress, $
							events = 0L, $
							step_toggle = ( (*p).step_mode eq 0), $
							step_events = ( (*p).step_mode eq 2), $
							toggle_bit = bit, $
							step_station = (*p).step_station, $
							scanx = scanx, $
							scany = scany, $
							sample = (*p).sample, $
							grain = (*p).grain, $
							comment = (*p).comment, $
							throttle = throttle, $
							output = (*p).output_file, $
							group = group, $
							/progress
						end
					3: begin								; Y step mode
;print,'Y step image scan:'
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    steps=',steps,' step_size=',(*p).step_size
;print,'    step-mode=',(*p).step_mode,' station=',(*p).step_station,' bit=',bit
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file
;print,'    active=',active, ' mode=',mode
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b

						xstep_count = (*p).step_count

						cmit_xstep_evt, evt_files, xstep_count, /ystep, $
							device = DevObj->name(), $
							channel = active, $
							detector = type, $
							charge = charge, $
							cal_a = cal_a, $
							cal_b = cal_b, $
							xrange = xrange, $
							yrange = yrange, $
							xcompress = xcompress, $
							ycompress = ycompress, $
							events = 0L, $
							step_toggle = ( (*p).step_mode eq 0), $
							step_events = ( (*p).step_mode eq 2), $
							toggle_bit = bit, $
							step_station = (*p).step_station, $
							scanx = scanx, $
							scany = scany, $
							sample = (*p).sample, $
							grain = (*p).grain, $
							comment = (*p).comment, $
							throttle = throttle, $
							output = (*p).output_file, $
							group = group, $
							/progress
						end
					else:
				endcase

			goto, finish
		endif

		case (*p).sort_mode of
			0: begin									; image
				case (*p).xy_mode of

					0: begin								; normal XY scan
;print,'normal XY scan:'
;print,'    active=',active
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file

						case mode of
							0: begin									; DA

								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, $
									conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0
								title = 'GeoPIXE Cluster DA Imaging'
 								
								args = ['da_evt', $
									'files=' + stringify(evt_files), $
									'matrix=' + stringify(file), $
									'mpdam_string=' + stringify(mpdam_string), $
									'translate_file=' + stringify((*p).xanes_energies_file), $
									'proxy_axis=' + stringify((*p).energy_proxy_axis), $
									'device=' + stringify(DevObj->name()), $
									'devpars=' + stringify(devpars), $
									'channel=' + stringify(active), $		
									'detector=' + stringify(type), $
									'charge=' + stringify(charge), $
									'ic=' + stringify(ic), $				
									'pv_list=' + stringify(*(*p).pic_list), $				
									'flatten=' + stringify( (cluster and not gencom) ? 0 : (*p).flatten), $	
									'cal_a=' + stringify(cal_a), $	
									'cal_b=' + stringify(cal_b), $	
									'x_sub_range=' + stringify(x_sub_range), $
									'y_sub_range=' + stringify(y_sub_range), $
									'xoffset=' + stringify(xoffset), $
									'yoffset=' + stringify(yoffset), $
									'xrange=' + stringify(xrange), $
									'yrange=' + stringify(yrange), $
									'xcompress=' + stringify(xcompress), $
									'ycompress=' + stringify(ycompress), $
;										'events=0L', $
									'scanx=' + stringify(scanx), $
									'scany=' + stringify(scany), $
									'xorigin=' + stringify(xorigin), $
									'yorigin=' + stringify(yorigin), $
									'sample=' + stringify((*p).sample), $
									'grain=' + stringify((*p).grain), $
									'comment=' + stringify((*p).comment), $
									'facility=' + stringify((*p).facility), $
									'endstation=' + stringify((*p).endstation), $
									'throttle=' + stringify(throttle), $
									'pileup=' + stringify(pileup), $
									'linearize=' + stringify(linearize), $
									'output=' + stringify((*p).output_file) ]

								if cluster or gencom then begin

									if gencom then begin
										F = file_requester( /write, filter='*.gcf', path=path, group=group, $
											title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
										if F eq '' then return
										args = [args,'cluster='+str_tidy((*p).cluster)]
										geopixe_gen_commands, F, args
										return
									endif else begin
										if mpda and (phase_good eq 0) then begin

;											If MPDA and phase maps not found (see 'check_mpdam' above), then go
;											and do normal DA pass, correct images, project phases and make a new
;											MPDAM file. Modify 'args' to reflect these changes and continue MPDA (below).

											geopixe_do_DA, args, DevObj, raw=evt_files, stringed=mpdam_string, $
													path=*(*pstate).path, original=original, cluster=1, mpdam=file, error=error
							
											title = title +' (MPDA)'
										endif
										args = strjoin( args, ', ')
									endelse

									if error eq 0 then begin
										cluster_client, title = title, $
											subtitle = strip_path((*p).output_file), $
											args=args, $
											n_files = n_elements(evt_files), $
											group_leader = group, $
											presult = presults, $
											prefs = cluster_prefs, $
											error = error
										print,'evt_start: error code return from DA cluster client = ',error
									endif
										
									;	Results is an array containing the result strings from each slave.
									;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
									;	Read each image and combine into one. Also combine charge, flux, etc.
	
									if (error eq 0) and ptr_good(presults) then begin
										cluster_merge_images, presults, images=pp, input_file1=(*p).evt_file, input_file2=(*p).evt2_file, $
													output_file=(*p).output_file, path=path, flatten=(*p).flatten, error=error

										if error eq 0 then begin
											charge = (*pp).charge				; these may have been modified
											scanx = 1000. * (*pp).scan.x
											scany = 1000. * (*pp).scan.y
											xorigin = (*pp).scan.origin.x
											yorigin = (*pp).scan.origin.y
										endif
									endif
								
								endif else begin
									if mpda and (phase_good eq 0) then begin

;										If MPDA and phase maps not found (see 'check_mpdam' above), then go
;										and do normal DA pass, correct images, project phases and make a new
;										MPDAM file. Modify 'args' to reflect these changes and continue MPDA (below).
;										This uses 'execute' and so will not work in IDL VM mode.

										geopixe_do_DA, args, DevObj, raw=evt_files, stringed=mpdam_string, $
												path=*(*pstate).path, original=original, cluster=0, mpdam=file, error=error
							
;										Now return to do the MPDA sort as requested. This uses 'execute' and so will not work
;										in IDL VM mode. However, the code below will work for non-cluster.

										title = title +' (MPDA)'
										args = strjoin( [args, 'group='+stringify(group), 'suppress='+stringify(suppress)], ', ')
										geopixe_execute, args, error=error, /progress

										if error eq 0 then begin
											pp = read_geopixe_image( (*p).output_file, error=error)
										endif

									endif else begin
										da_evt, evt_files, matrix=file, $		; change to use 'file' 28/3/12
											mpdam_string = mpdam_string, $
											translate_file = (*p).xanes_energies_file, $
											proxy_axis = (*p).energy_proxy_axis, $
											device = DevObj->name(), $
											devpars = devpars, $				; all device information
											channel = active, $					; vector in "Multiple detector" mode
											detector = type, $
											charge = charge, $
											ic = ic, $
											pv_list = *(*p).pic_list, $
											flatten = (*p).flatten, $
											cal_a = cal_a, $					; vector in "Multiple detector" mode
											cal_b = cal_b, $					; vector in "Multiple detector" mode
											xoffset = xoffset, $
											yoffset = yoffset, $
											x_sub_range = x_sub_range, $
											y_sub_range = y_sub_range, $
											xrange = xrange, $
											yrange = yrange, $
											xcompress = xcompress, $
											ycompress = ycompress, $
											events = 0L, $
											scanx = scanx, $
											scany = scany, $
											xorigin = xorigin, $
											yorigin = yorigin, $
											sample = (*p).sample, $
											grain = (*p).grain, $
											comment = (*p).comment, $
											facility = (*p).facility, $
											endstation = (*p).endstation, $
											throttle = throttle, $
											pileup = pileup, $
											linearize = linearize, $
											output = (*p).output_file, $
											images = pp, $
											group = group, $
											suppress = suppress, $				; suppress pop-ups
											/progress
									endelse
								endelse

								if (error eq 0) and ptr_good(pp, /struct) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin
										(*p).file[(*p).station] = file
;									endif
									if ptr_good((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
										widget_control, (*pstate).file, set_value=(*p).file[(*p).station]

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							1: begin									; cuts
;print,'		CUTs file=',file
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif
					;			widget_control, /hourglass
					
								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0

								if cluster or gencom then begin
									args = ['cut_evt', $
										'files=' + stringify(evt_files), $
										'cuts=' + stringify(cuts, /embed_ptr), $
										'device=' + stringify(DevObj->name()), $
										'channel=' + stringify(active), $		
										'detector=' + stringify(type), $
										'charge=' + stringify(charge), $
										'ic=' + stringify(ic), $				
										'pv_list=' + stringify(*(*p).pic_list), $
										'flatten=' + stringify( (cluster and not gencom) ? 0 : (*p).flatten), $	
										'cal_a=' + stringify(cal_a), $	
										'cal_b=' + stringify(cal_b), $	
										'x_sub_range=' + stringify(x_sub_range), $
										'y_sub_range=' + stringify(y_sub_range), $
										'ecompress=' + stringify(ecompress), $
										'xoffset=' + stringify(xoffset), $
										'yoffset=' + stringify(yoffset), $
										'xrange=' + stringify(xrange), $
										'yrange=' + stringify(yrange), $
										'xcompress=' + stringify(xcompress), $
										'ycompress=' + stringify(ycompress), $
;										'events=0L', $
										'scanx=' + stringify(scanx), $
										'scany=' + stringify(scany), $
										'xorigin=' + stringify(xorigin), $
										'yorigin=' + stringify(yorigin), $
										'devpars=' + stringify(devpars), $
										'sample=' + stringify((*p).sample), $
										'grain=' + stringify((*p).grain), $
										'comment=' + stringify((*p).comment), $
										'facility=' + stringify((*p).facility), $
										'endstation=' + stringify((*p).endstation), $
										'throttle=' + stringify(throttle), $
										'pileup=' + stringify(pileup), $
										'linearize=' + stringify(linearize), $
										'output=' + stringify((*p).output_file) ]

									if gencom then begin
										F = file_requester( /write, filter='*.gcf', path=path, group=group, $
											title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
										if F eq '' then return
										args = [args,'cluster='+str_tidy((*p).cluster)]
										geopixe_gen_commands, F, args
										return
									endif else begin
										args = strjoin( args, ', ')
									endelse

									cluster_client, title = 'GeoPIXE Cluster CUTs Imaging', $
										subtitle = strip_path((*p).output_file), $
										args=args, $
										n_files = n_elements(evt_files), $
										group_leader = group, $
										presult = presults, $
										prefs = cluster_prefs, $
										error = error
										
									;	Results is an array containing the result strings from each slave.
									;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
									;	Read each image and combine into one. Also combine charge, flux, etc.
	
									if (error eq 0) and ptr_good(presults) then begin
										cluster_merge_images, presults, images=pp, input_file1=(*p).evt_file, input_file2=(*p).evt2_file, $
													output_file=(*p).output_file, path=path, flatten=(*p).flatten, error=error

										if error eq 0 then begin
											charge = (*pp).charge				; these may have been modified
											scanx = 1000. * (*pp).scan.x
											scany = 1000. * (*pp).scan.y
											xorigin = (*pp).scan.origin.x
											yorigin = (*pp).scan.origin.y
										endif
									endif
								
								endif else begin
									cut_evt, evt_files, cuts, $
										device = DevObj->name(), $
										channel = active, $
										detector = type, $
										charge = charge, $
										ic = ic, $
										pv_list = *(*p).pic_list, $
										flatten = (*p).flatten, $
										cal_a = cal_a, $
										cal_b = cal_b, $
										ecompress = ecompress, $
										xoffset = xoffset, $
										yoffset = yoffset, $
										x_sub_range = x_sub_range, $
										y_sub_range = y_sub_range, $
										xrange = xrange, $
										yrange = yrange, $
										xcompress = xcompress, $
										ycompress = ycompress, $
										events = 0L, $
										scanx = scanx, $
										scany = scany, $
										xorigin = xorigin, $
										yorigin = yorigin, $
										devpars = devpars, $
										sample = (*p).sample, $
										grain = (*p).grain, $
										comment = (*p).comment, $
										facility = (*p).facility, $
										endstation = (*p).endstation, $
										throttle = throttle, $
										pileup = pileup, $
										linearize = linearize, $
										output = (*p).output_file, $
										images = pp, $
										group = group, $
										suppress = suppress, $				; suppress pop-ups
										/progress
								endelse

								if (error eq 0) and ptr_good(pp, /struct) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin 
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							2: begin									; mean energy (STIM) cuts
;print,'		CUTs file=',file
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif
					;			widget_control, /hourglass
					
								cut_evt, evt_files, cuts, $
									/stim_mean, $
									device = DevObj->name(), $
									devpars = devpars, $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									ecompress = ecompress, $
									xoffset = xoffset, $
									yoffset = yoffset, $
									x_sub_range = x_sub_range, $
									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									scanx = scanx, $
									scany = scany, $
									xorigin = xorigin, $
									yorigin = yorigin, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									facility = (*p).facility, $
									endstation = (*p).endstation, $
									throttle = throttle, $
									pileup = pileup, $
									linearize = linearize, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							4: begin									; XYE (compressed E) data cube

								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, $
									conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0
								title = 'GeoPIXE Cluster XYE data cube'
								
								args = ['cube_evt', $
									'files=' + stringify(evt_files), $
									'device=' + stringify(DevObj->name()), $
									'devpars=' + stringify(devpars), $
									'channel=' + stringify(active), $		
									'detector=' + stringify(type), $
									'charge=' + stringify(charge), $
									'ic=' + stringify(ic), $				
									'pv_list=' + stringify(*(*p).pic_list), $				
									'flatten=' + stringify( (cluster and not gencom) ? 0 : (*p).flatten), $	
									'cal_a=' + stringify(cal_a), $	
									'cal_b=' + stringify(cal_b), $	
									'x_sub_range=' + stringify(x_sub_range), $
									'y_sub_range=' + stringify(y_sub_range), $
									'xoffset=' + stringify(xoffset), $
									'yoffset=' + stringify(yoffset), $
									'xrange=' + stringify(xrange), $
									'yrange=' + stringify(yrange), $
									'xcompress=' + stringify(xcompress), $
									'ycompress=' + stringify(ycompress), $
;										'events=0L', $
									'scanx=' + stringify(scanx), $
									'scany=' + stringify(scany), $
									'xorigin=' + stringify(xorigin), $
									'yorigin=' + stringify(yorigin), $
									'sample=' + stringify((*p).sample), $
									'grain=' + stringify((*p).grain), $
									'comment=' + stringify((*p).comment), $
									'facility=' + stringify((*p).facility), $
									'endstation=' + stringify((*p).endstation), $
									'throttle=' + stringify(throttle), $
									'pileup=' + stringify(pileup), $
									'linearize=' + stringify(linearize), $
									'output=' + stringify((*p).output_file) ]

								if cluster or gencom then begin

									if gencom then begin
										F = file_requester( /write, filter='*.gcf', path=path, group=group, $
											title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
										if F eq '' then return
										args = [args,'cluster='+str_tidy((*p).cluster)]
										geopixe_gen_commands, F, args
										return
									endif else begin
										args = strjoin( args, ', ')
									endelse

									if error eq 0 then begin
										cluster_client, title = title, $
											subtitle = strip_path((*p).output_file), $
											args=args, $
											n_files = n_elements(evt_files), $
											group_leader = group, $
											presult = presults, $
											prefs = cluster_prefs, $
											error = error
										print,'evt_start: error code return from DA cluster client = ',error
									endif
										
									;	Results is an array containing the result strings from each slave.
									;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
									;	Read each image and combine into one. Also combine charge, flux, etc.
	
									if (error eq 0) and ptr_good(presults) then begin
										cluster_merge_images, presults, images=pp, input_file1=(*p).evt_file, input_file2=(*p).evt2_file, $
													output_file=(*p).output_file, path=path, flatten=(*p).flatten, error=error

										if error eq 0 then begin
											charge = (*pp).charge				; these may have been modified
											scanx = 1000. * (*pp).scan.x
											scany = 1000. * (*pp).scan.y
											xorigin = (*pp).scan.origin.x
											yorigin = (*pp).scan.origin.y
										endif
									endif
								
								endif else begin
									cube_evt, evt_files, $					; 
										device = DevObj->name(), $
										devpars = devpars, $				; all device information
										channel = active, $					; vector in "Multiple detector" mode
										detector = type, $
										charge = charge, $
										ic = ic, $
										pv_list = *(*p).pic_list, $
										flatten = (*p).flatten, $
										cal_a = cal_a, $					; vector in "Multiple detector" mode
										cal_b = cal_b, $					; vector in "Multiple detector" mode
										xoffset = xoffset, $
										yoffset = yoffset, $
										x_sub_range = x_sub_range, $
										y_sub_range = y_sub_range, $
										xrange = xrange, $
										yrange = yrange, $
										xcompress = xcompress, $
										ycompress = ycompress, $
										events = 0L, $
										scanx = scanx, $
										scany = scany, $
										xorigin = xorigin, $
										yorigin = yorigin, $
										sample = (*p).sample, $
										grain = (*p).grain, $
										comment = (*p).comment, $
										facility = (*p).facility, $
										endstation = (*p).endstation, $
										throttle = throttle, $
										pileup = pileup, $
										linearize = linearize, $
										output = (*p).output_file, $
										images = pp, $
										group = group, $
										suppress = suppress, $				; suppress pop-ups
										/progress
								endelse

								if (error eq 0) and ptr_good(pp, /struct) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin
										(*p).file[(*p).station] = file
;									endif
									if ptr_good((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
										widget_control, (*pstate).file, set_value=(*p).file[(*p).station]

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							5: begin									; cuts (/all full spectrum single CUT)
;print,'		CUTs file=',file
					;			widget_control, /hourglass
					
								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0

								if cluster or gencom then begin
									args = ['cut_evt', $
										'files=' + stringify(evt_files), $
										'all=1', $
										'device=' + stringify(DevObj->name()), $
										'channel=' + stringify(active), $		
										'detector=' + stringify(type), $
										'charge=' + stringify(charge), $
										'ic=' + stringify(ic), $				
										'pv_list=' + stringify(*(*p).pic_list), $
										'flatten=' + stringify( (cluster and not gencom) ? 0 : (*p).flatten), $	
										'cal_a=' + stringify(cal_a), $	
										'cal_b=' + stringify(cal_b), $	
										'x_sub_range=' + stringify(x_sub_range), $
										'y_sub_range=' + stringify(y_sub_range), $
										'ecompress=' + stringify(ecompress), $
										'xoffset=' + stringify(xoffset), $
										'yoffset=' + stringify(yoffset), $
										'xrange=' + stringify(xrange), $
										'yrange=' + stringify(yrange), $
										'xcompress=' + stringify(xcompress), $
										'ycompress=' + stringify(ycompress), $
;										'events=0L', $
										'scanx=' + stringify(scanx), $
										'scany=' + stringify(scany), $
										'xorigin=' + stringify(xorigin), $
										'yorigin=' + stringify(yorigin), $
										'devpars=' + stringify(devpars), $
										'sample=' + stringify((*p).sample), $
										'grain=' + stringify((*p).grain), $
										'comment=' + stringify((*p).comment), $
										'facility=' + stringify((*p).facility), $
										'endstation=' + stringify((*p).endstation), $
										'throttle=' + stringify(throttle), $
										'pileup=' + stringify(pileup), $
										'linearize=' + stringify(linearize), $
										'output=' + stringify((*p).output_file) ]

									if gencom then begin
										F = file_requester( /write, filter='*.gcf', path=path, group=group, $
											title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
										if F eq '' then return
										args = [args,'cluster='+str_tidy((*p).cluster)]
										geopixe_gen_commands, F, args
										return
									endif else begin
										args = strjoin( args, ', ')
									endelse

									cluster_client, title = 'GeoPIXE Cluster CUTs Imaging (/All)', $
										subtitle = strip_path((*p).output_file), $
										args=args, $
										n_files = n_elements(evt_files), $
										group_leader = group, $
										presult = presults, $
										prefs = cluster_prefs, $
										error = error
										
									;	Results is an array containing the result strings from each slave.
									;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
									;	Read each image and combine into one. Also combine charge, flux, etc.
	
									if (error eq 0) and ptr_good(presults) then begin
										cluster_merge_images, presults, images=pp, input_file1=(*p).evt_file, input_file2=(*p).evt2_file, $
													output_file=(*p).output_file, path=path, flatten=(*p).flatten, error=error

										if error eq 0 then begin
											charge = (*pp).charge				; these may have been modified
											scanx = 1000. * (*pp).scan.x
											scany = 1000. * (*pp).scan.y
											xorigin = (*pp).scan.origin.x
											yorigin = (*pp).scan.origin.y
										endif
									endif
								
								endif else begin
									cut_evt, evt_files, /all, $
										device = DevObj->name(), $
										channel = active, $
										detector = type, $
										charge = charge, $
										ic = ic, $
										pv_list = *(*p).pic_list, $
										flatten = (*p).flatten, $
										cal_a = cal_a, $
										cal_b = cal_b, $
										ecompress = ecompress, $
										xoffset = xoffset, $
										yoffset = yoffset, $
										x_sub_range = x_sub_range, $
										y_sub_range = y_sub_range, $
										xrange = xrange, $
										yrange = yrange, $
										xcompress = xcompress, $
										ycompress = ycompress, $
										events = 0L, $
										scanx = scanx, $
										scany = scany, $
										xorigin = xorigin, $
										yorigin = yorigin, $
										devpars = devpars, $
										sample = (*p).sample, $
										grain = (*p).grain, $
										comment = (*p).comment, $
										facility = (*p).facility, $
										endstation = (*p).endstation, $
										throttle = throttle, $
										pileup = pileup, $
										linearize = linearize, $
										output = (*p).output_file, $
										images = pp, $
										group = group, $
										suppress = suppress, $				; suppress pop-ups
										/progress
								endelse

								if (error eq 0) and ptr_good(pp, /struct) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin 
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end
							else:
						endcase
						end

					1: begin								; X step mode
;print,'X step image scan:'
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    steps=',steps,' step_size=',(*p).step_size
;print,'    step-mode=',(*p).step_mode,' station=',(*p).step_station,' bit=',bit
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file
;print,'    active=',active, ' mode=',mode
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b

						case mode of
							0: begin							; DA
;print,'		DA matrix=',file
;								matrix = read_da(file, error=error)
;								if error then begin
;									file = *(*pstate).path + strip_path( file)
;									matrix = read_da( file, error=error)
;									if error eq 0 then begin
;										set_active, pstate, active, file
;									endif
;								endif
;								if error then begin
;									warning, 'EVT', ['Error reading DA matrix file:',file]
;									goto, finish
;								endif
								xstep_count = (*p).step_count
					;			widget_control, /hourglass

								da_xstep_evt2, evt_files, matrix=file, xstep_count, $
									device = DevObj->name(), $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									xoffset = xoffset, $
									yoffset = yoffset, $
									x_sub_range = x_sub_range, $
									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									step_toggle = ( (*p).step_mode eq 0), $
									step_events = ( (*p).step_mode eq 2), $
									toggle_bit = bit, $
									step_station = (*p).step_station, $
									scanx = scanx, $
									scany = scany, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									throttle = throttle, $
									pileup = pileup, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							1: begin							; cuts
;print,'		CUTs file=',file
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif
								xstep_count = (*p).step_count
					;			widget_control, /hourglass

								cut_xstep_evt, evt_files, cuts, xstep_count, $
									device = DevObj->name(), $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									ecompress = ecompress, $
;									xoffset = xoffset, $
;									yoffset = yoffset, $
;									x_sub_range = x_sub_range, $
;									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									step_toggle = ( (*p).step_mode eq 0), $
									step_events = ( (*p).step_mode eq 2), $
									toggle_bit = bit, $
									step_station = (*p).step_station, $
									scanx = scanx, $
									scany = scany, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									throttle = throttle, $
									pileup = pileup, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							2: begin							; mean energy (STIM) cuts
;print,'		CUTs file=',file
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif
								xstep_count = (*p).step_count
					;			widget_control, /hourglass

								cut_xstep_evt, evt_files, cuts, xstep_count, $
									/stim_mean, $
									device = DevObj->name(), $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									ecompress = ecompress, $
;									xoffset = xoffset, $
;									yoffset = yoffset, $
;									x_sub_range = x_sub_range, $
;									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									step_toggle = ( (*p).step_mode eq 0), $
									step_events = ( (*p).step_mode eq 2), $
									toggle_bit = bit, $
									step_station = (*p).step_station, $
									scanx = scanx, $
									scany = scany, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									throttle = throttle, $
									pileup = pileup, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end
							else:
						endcase
						end
					3: begin								; Y step mode
;print,'Y step image scan:'
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    steps=',steps,' step_size=',(*p).step_size
;print,'    step-mode=',(*p).step_mode,' station=',(*p).step_station,' bit=',bit
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file
;print,'    active=',active, ' mode=',mode
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b

						case mode of
							0: begin							; DA
;print,'		DA matrix=',file
;								matrix = read_da(file, error=error)
;								if error then begin
;									file = *(*pstate).path + strip_path( file)
;									matrix = read_da( file, error=error)
;									if error eq 0 then begin
;										set_active, pstate, active, file
;									endif
;								endif
;								if error then begin
;									warning, 'EVT', ['Error reading DA matrix file:',file]
;									goto, finish
;								endif
								xstep_count = (*p).step_count
					;			widget_control, /hourglass

								da_xstep_evt2, evt_files, matrix=file, xstep_count, /ystep, $
									device = DevObj->name(), $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									xoffset = xoffset, $
									yoffset = yoffset, $
									x_sub_range = x_sub_range, $
									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									step_toggle = ( (*p).step_mode eq 0), $
									step_events = ( (*p).step_mode eq 2), $
									toggle_bit = bit, $
									step_station = (*p).step_station, $
									scanx = scanx, $
									scany = scany, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									throttle = throttle, $
									pileup = pileup, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=string((*p).charge)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							1: begin							; cuts
;print,'		CUTs file=',file
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif
								xstep_count = (*p).step_count
					;			widget_control, /hourglass

								cut_xstep_evt, evt_files, cuts, xstep_count, /ystep, $
									device = DevObj->name(), $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									ecompress = ecompress, $
;									xoffset = xoffset, $
;									yoffset = yoffset, $
;									x_sub_range = x_sub_range, $
;									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									step_toggle = ( (*p).step_mode eq 0), $
									step_events = ( (*p).step_mode eq 2), $
									toggle_bit = bit, $
									step_station = (*p).step_station, $
									scanx = scanx, $
									scany = scany, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									throttle = throttle, $
									pileup = pileup, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=string((*p).charge)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							2: begin							; mean energy (STIM) cuts
;print,'		CUTs file=',file
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif
								xstep_count = (*p).step_count
					;			widget_control, /hourglass

								cut_xstep_evt, evt_files, cuts, xstep_count, /ystep, $
									/stim_mean, $
									device = DevObj->name(), $
									channel = active, $
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $
									cal_b = cal_b, $
									ecompress = ecompress, $
;									xoffset = xoffset, $
;									yoffset = yoffset, $
;									x_sub_range = x_sub_range, $
;									y_sub_range = y_sub_range, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									events = 0L, $
									step_toggle = ( (*p).step_mode eq 0), $
									step_events = ( (*p).step_mode eq 2), $
									toggle_bit = bit, $
									step_station = (*p).step_station, $
									scanx = scanx, $
									scany = scany, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
									throttle = throttle, $
									pileup = pileup, $
									output = (*p).output_file, $
									images = pp, $
									group = group, $
									/progress

								if ptr_valid(pp) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
;									endif
									if ptr_valid((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=string((*p).charge)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end
							else:
						endcase
						end
					else:
				endcase
				end
			1: begin									; EXAFS spectra
				case (*p).xy_mode of

					0: begin								; normal XY scan
;print,'normal XY scan:'
;print,'    active=',active
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    charge=',(*p).charge
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file

						case mode of
							0: begin									; DA
;								matrix = read_da(file, error=error)
;								if error then begin
;									file = *(*pstate).path + strip_path( file)
;									matrix = read_da( file, error=error)
;									if error eq 0 then begin
;										set_active, pstate, active, file
;									endif
;								endif
;								if error then begin
;									warning, 'EVT', ['Error reading DA matrix file:',file]
;									goto, finish
;								endif
						;		widget_control, /hourglass

								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0

;		Remember to also set "dir=((*p).sort_mode eq 1)" in select_evt_files above.

								xanes_da_evt, evt_files, matrix=file, $
									device = DevObj->name(), $
									devpars = devpars, $
									channel = active, $					; vector in "Multiple detector" mode
									detector = type, $
									charge = charge, $
									ic = ic, $
									pv_list = *(*p).pic_list, $
									flatten = (*p).flatten, $
									cal_a = cal_a, $					; vector in "Multiple detector" mode
									cal_b = cal_b, $					; vector in "Multiple detector" mode
									events = 0L, $
									sample = (*p).sample, $
									grain = (*p).grain, $
									comment = (*p).comment, $
;									facility = (*p).facility, $
;									endstation = (*p).endstation, $
									throttle = throttle, $
									pileup = pileup, $
									linearize = linearize, $
									output = (*p).output_file, $
									xrange = xrange, $
									yrange = yrange, $
									xcompress = xcompress, $
									ycompress = ycompress, $
									scanx = scanx, $
									scany = scany, $
									group = group, $
									suppress = suppress, $				; suppress pop-ups
									/progress

;		Remember to also set "dir=0" in select_evt_files above.
;					
;								xanes_single_da_evt, evt_files, matrix=file, $
;									device = DevObj->name(), $
;									channel = active, $					; vector in "Multiple detector" mode
;									detector = type, $
;									charge = charge, $
;									cal_a = cal_a, $					; vector in "Multiple detector" mode
;									cal_b = cal_b, $					; vector in "Multiple detector" mode
;									events = 0L, $
;									throttle = throttle, $
;									pileup = pileup, $
;									energies = (*p).xanes_energies_file, $				; filename for energies table
;									linearize = linearize, $
;									output = (*p).output_file, $
;									group = group, $
;									suppress = suppress, $				; suppress pop-ups
;									/progress
								end
								
							1: begin									; CUTs
								cuts = read_cuts(file, error=error)
								if error then begin
									file = *(*pstate).path + strip_path( file)
									cuts = read_cuts( file, error=error)
									if error eq 0 then begin
										set_active, pstate, active, file
									endif
								endif
								if error then begin
									warning, 'EVT', 'Error reading CUTs'
									goto, finish
								endif

;		Remember to also set "dir=0" in select_evt_files above.
					
								xanes_single_da_evt, evt_files, cuts, /cuts, $
									device = DevObj->name(), $
									channel = active, $					; vector in "Multiple detector" mode
									detector = type, $
									charge = charge, $
									cal_a = cal_a, $					; vector in "Multiple detector" mode
									cal_b = cal_b, $					; vector in "Multiple detector" mode
									events = 0L, $
									throttle = throttle, $
									pileup = pileup, $
									energies = (*p).xanes_energies_file, $				; filename for energies table
									linearize = linearize, $
									output = (*p).output_file, $
									group = group, $
									suppress = suppress, $				; suppress pop-ups
									/progress
								end
						endcase
						end
				endcase
				end
				
			2: begin									; stack 3D (was 'traverse')
				case (*p).xy_mode of

					4: begin								; normal XYZ (XANES E) scan 
;print,'normal XY scan:'
;print,'    active=',active
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file

						case mode of
							0: begin									; DA XYE XANES stack
;print,'		DA matrix=',file

								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0
								
								if cluster or gencom then begin
									args = ['da_evt_stack', $
										'files=' + stringify(evt_files), $
										'select=' + stringify((*p).el_select), $
										'matrix=' + stringify(file), $
										'device=' + stringify(DevObj->name()), $
										'devpars=' + stringify(devpars), $
										'translate_file=' + stringify((*p).xanes_energies_file), $
										'collapse_energy=' + stringify(collapse_energy), $
										'channel=' + stringify(active), $		
										'detector=' + stringify(type), $
										'charge=' + stringify(charge), $
										'ic=' + stringify(ic), $				
										'pv_list=' + stringify(*(*p).pic_list), $				
;										'flatten=' + stringify( (cluster and not gencom) ? 0 : (*p).flatten), $	
										'flatten=0', $	
										'cal_a=' + stringify(cal_a), $	
										'cal_b=' + stringify(cal_b), $	
										'x_sub_range=' + stringify(x_sub_range), $
										'y_sub_range=' + stringify(y_sub_range), $
										'xoffset=' + stringify(xoffset), $
										'yoffset=' + stringify(yoffset), $
										'xrange=' + stringify(xrange), $
										'yrange=' + stringify(yrange), $
										'zrange=' + stringify(zrange), $
										'xcompress=' + stringify(xcompress), $
										'ycompress=' + stringify(ycompress), $
;										'events=0L', $
										'scanx=' + stringify(scanx), $
										'scany=' + stringify(scany), $
										'scanz=' + stringify(scanz), $
										'xorigin=' + stringify(xorigin), $
										'yorigin=' + stringify(yorigin), $
										'zorigin=' + stringify(zorigin), $
										'sample=' + stringify((*p).sample), $
										'grain=' + stringify((*p).grain), $
										'comment=' + stringify((*p).comment), $
										'facility=' + stringify((*p).facility), $
										'endstation=' + stringify((*p).endstation), $
										'throttle=' + stringify(throttle), $
										'pileup=' + stringify(pileup), $
										'linearize=' + stringify(linearize), $
										'output=' + stringify((*p).output_file) ]

									if gencom then begin
										F = file_requester( /write, filter='*.gcf', path=path, group=group, $
											title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
										if F eq '' then return
										args = [args,'cluster='+str_tidy((*p).cluster)]
										geopixe_gen_commands, F, args
										return
									endif else begin
										args = strjoin( args, ', ')
									endelse

									cluster_client, title = 'GeoPIXE Cluster DA 3D Imaging', $
										subtitle = strip_path((*p).output_file), $
										args=args, $
										n_files = n_elements(evt_files), $
										group_leader = group, $
										presult = presults, $
										prefs = cluster_prefs, $
										error = error
									print,'evt_start: error code return from DA cluster client = ',error
										
									;	Results is an array containing the result strings from each slave.
									;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
									;	Read each image and combine into one. Also combine charge, flux, etc.
	
									if (error eq 0) and ptr_good(presults) then begin
										cluster_merge_images, presults, images=pp, input_file1=(*p).evt_file, input_file2=(*p).evt2_file, $
													output_file=(*p).output_file, path=path, flatten=(*p).flatten, error=error

										if error eq 0 then begin
											charge = (*pp).charge				; these may have been modified
											scanx = 1000. * (*pp).scan.x
											scany = 1000. * (*pp).scan.y
											xorigin = (*pp).scan.origin.x
											yorigin = (*pp).scan.origin.y
										endif
									endif
								
								endif else begin
									da_evt_stack, evt_files, (*p).el_select, $
										matrix = file, $	
										device = DevObj->name(), $
										devpars = devpars, $				; all device information
										translate_file = (*p).xanes_energies_file, $
										collapse_energy = collapse_energy, $
										channel = active, $					; vector in "Multiple detector" mode
										detector = type, $
										charge = charge, $
										ic = ic, $
										pv_list = *(*p).pic_list, $
										flatten = (*p).flatten, $
										cal_a = cal_a, $					; vector in "Multiple detector" mode
										cal_b = cal_b, $					; vector in "Multiple detector" mode
										xoffset = xoffset, $
										yoffset = yoffset, $
										x_sub_range = x_sub_range, $
										y_sub_range = y_sub_range, $
										xrange = xrange, $
										yrange = yrange, $
										zrange = zrange, $
										xcompress = xcompress, $
										ycompress = ycompress, $
										events = 0L, $
										scanx = scanx, $
										scany = scany, $
										scanz = scanz, $
										xorigin = xorigin, $
										yorigin = yorigin, $
										zorigin = zorigin, $
										sample = (*p).sample, $
										grain = (*p).grain, $
										comment = (*p).comment, $
										facility = (*p).facility, $
										endstation = (*p).endstation, $
										throttle = throttle, $
										pileup = pileup, $
										linearize = linearize, $
										output = (*p).output_file, $
										images = pp, $
										group = group, $
										suppress = suppress, $				; suppress pop-ups
										/progress
								endelse

								if (error eq 0) and ptr_good(pp, /struct) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin
;									endif
									if ptr_good((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end

							else:
						endcase
						end

					5: begin								; normal XYZ (tomo) scan 
;print,'normal XY scan:'
;print,'    active=',active
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file

						case mode of
							0: begin									; DA XYZ (tomo angle) stack
;print,'		DA matrix=',file

								ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, unit:(*p).preamp.unit, conversion:(*p).charge_conversion, use_dwell:(*p).use_dwell, dwell:(*p).dwell}
								error = 0
								
								if cluster or gencom then begin
									args = ['da_evt_tomo', $
										'files=' + stringify(evt_files), $
										'matrix=' + stringify(file), $
										'device=' + stringify(DevObj->name()), $
										'devpars=' + stringify(devpars), $
										'channel=' + stringify(active), $		
										'detector=' + stringify(type), $
										'charge=' + stringify(charge), $
										'ic=' + stringify(ic), $				
										'pv_list=' + stringify(*(*p).pic_list), $				
;										'flatten=' + stringify( (cluster and not gencom) ? 0 : (*p).flatten), $	
										'flatten=0', $	
										'cal_a=' + stringify(cal_a), $	
										'cal_b=' + stringify(cal_b), $	
										'x_sub_range=' + stringify(x_sub_range), $
										'y_sub_range=' + stringify(y_sub_range), $
										'xoffset=' + stringify(xoffset), $
										'yoffset=' + stringify(yoffset), $
										'xrange=' + stringify(xrange), $
										'yrange=' + stringify(yrange), $
										'zrange=' + stringify(zrange), $
										'xcompress=' + stringify(xcompress), $
										'ycompress=' + stringify(ycompress), $
										'zcompress=' + stringify(zcompress), $
;										'events=0L', $
										'scanx=' + stringify(scanx), $
										'scany=' + stringify(scany), $
										'scanz=' + stringify(scanz), $
										'xorigin=' + stringify(xorigin), $
										'yorigin=' + stringify(yorigin), $
										'zorigin=' + stringify(zorigin), $
										'sample=' + stringify((*p).sample), $
										'grain=' + stringify((*p).grain), $
										'comment=' + stringify((*p).comment), $
										'facility=' + stringify((*p).facility), $
										'endstation=' + stringify((*p).endstation), $
										'throttle=' + stringify(throttle), $
										'pileup=' + stringify(pileup), $
										'linearize=' + stringify(linearize), $
										'output=' + stringify((*p).output_file) ]

									if gencom then begin
										F = file_requester( /write, filter='*.gcf', path=path, group=group, $
											title='Write a "GeoPIXE Command File"', /fix_filter, /latest)
										if F eq '' then return
										args = [args,'cluster='+str_tidy((*p).cluster)]
										geopixe_gen_commands, F, args
										return
									endif else begin
										args = strjoin( args, ', ')
									endelse

									cluster_client, title = 'GeoPIXE Cluster DA 3D Imaging', $
										subtitle = strip_path((*p).output_file), $
										args=args, $
										n_files = n_elements(evt_files), $
										group_leader = group, $
										presult = presults, $
										prefs = cluster_prefs, $
										error = error
									print,'evt_start: error code return from DA cluster client = ',error
										
									;	Results is an array containing the result strings from each slave.
									;	This is an array of 'xxx.DAI.nn' file names of resulting image (stripes).
									;	Read each image and combine into one. Also combine charge, flux, etc.
	
									if (error eq 0) and ptr_good(presults) then begin
										cluster_merge_images, presults, images=pp, input_file1=(*p).evt_file, input_file2=(*p).evt2_file, $
													output_file=(*p).output_file, path=path, flatten=(*p).flatten, error=error

										if error eq 0 then begin
											charge = (*pp).charge				; these may have been modified
											scanx = 1000. * (*pp).scan.x
											scany = 1000. * (*pp).scan.y
											xorigin = (*pp).scan.origin.x
											yorigin = (*pp).scan.origin.y
										endif
									endif
								
								endif else begin
									da_evt_tomo, evt_files, $
										matrix = file, $	
										device = DevObj->name(), $
										devpars = devpars, $				; all device information
										channel = active, $					; vector in "Multiple detector" mode
										detector = type, $
										charge = charge, $
										ic = ic, $
										pv_list = *(*p).pic_list, $
										flatten = (*p).flatten, $
										cal_a = cal_a, $					; vector in "Multiple detector" mode
										cal_b = cal_b, $					; vector in "Multiple detector" mode
										xoffset = xoffset, $
										yoffset = yoffset, $
										x_sub_range = x_sub_range, $
										y_sub_range = y_sub_range, $
										xrange = xrange, $
										yrange = yrange, $
										zrange = zrange, $
										xcompress = xcompress, $
										ycompress = ycompress, $
										zcompress = zcompress, $
										events = 0L, $
										scanx = scanx, $
										scany = scany, $
										scanz = scanz, $
										xorigin = xorigin, $
										yorigin = yorigin, $
										zorigin = zorigin, $
										sample = (*p).sample, $
										grain = (*p).grain, $
										comment = (*p).comment, $
										facility = (*p).facility, $
										endstation = (*p).endstation, $
										throttle = throttle, $
										pileup = pileup, $
										linearize = linearize, $
										output = (*p).output_file, $
										images = pp, $
										group = group, $
										suppress = suppress, $				; suppress pop-ups
										/progress
								endelse

								if (error eq 0) and ptr_good(pp, /struct) then begin
									evt_error = 0
									if ptr_valid( (*pstate).pimage) then begin
										if (*(*pstate).pimage).orphan eq 1 then begin
											(*pstate).local_images = 1
											(*(*pstate).pimage).orphan = 0
										endif
										if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
									endif
									(*p).charge = charge
;									if (*p).image_mode eq 0 then begin
										(*p).xsize = scanx
										(*p).ysize = scany
										(*p).xorigin = xorigin
										(*p).yorigin = yorigin
;									endif
									if ptr_good((*pp).flux) then (*p).flux = total( *(*pp).flux)

									if notify then begin
										widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
										widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
										widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

										(*pstate).local_images = 0			; 'image' will own this
										(*pp).orphan = 1
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=group
										notify, 'images', (*pstate).pimage, from=group
									endif
								endif
								end
								
							else:
						endcase
						end

					2: begin								; traverse Xstep mode
;print,'X step image scan: DA matrix=',file
;print,'    sample=',(*p).sample,' grain=',(*p).grain,' comment=',(*p).comment
;print,'    xrange=',(*p).xrange,' yrange=',(*p).yrange,' charge=',(*p).charge
;print,'    xcompress=',xcompress,' ycompress=',ycompress
;print,'    xsize=',(*p).xsize,' ysize=',(*p).ysize
;print,'    steps=',steps,' step_size=',(*p).step_size
;print,'    step-mode=',(*p).step_mode,' station=',(*p).step_station,' bit=',bit
;print,'    EVT file=',evt_files
;print,'    output file=',(*p).output_file
;print,'    active=',active, ' mode=',mode
;print,'    type=',type,' cal_a=',cal_a,' cal_b=',cal_b

				;		widget_control, /hourglass
;						matrix = read_da(file, error=error)
;						if error then begin
;							file = *(*pstate).path + strip_path( file)
;							matrix = read_da( file, error=error)
;							if error eq 0 then begin
;								set_active, pstate, active, file
;							endif
;						endif
;						if error then begin
;							warning, 'EVT', ['Error reading DA matrix file:',file]
;							goto, finish
;						endif
						xstep_count = (*p).step_count
				;		widget_control, /hourglass

						da_xstep_trav_evt, evt_files, matrix=file, xstep_count, $
							device = DevObj->name(), $
							channel = active, $
							detector = type, $
							charge = charge, $
							cal_a = cal_a, $
							cal_b = cal_b, $
							xrange = xrange, $
							xcompress = xcompress, $
							scanx = scanx, $
							microns = (*p).step_size, $			; redundant for scanning?
							events = 0L, $
							step_toggle = ( (*p).step_mode eq 0), $
							step_events = ( (*p).step_mode eq 2), $
							toggle_bit = bit, $
							step_station = (*p).step_station, $
							sample = (*p).sample, $
							grain = (*p).grain, $
							comment = (*p).comment, $
							output = (*p).output_file, $
							spectra = pp, $
							group = group, $
							/progress

						if ptr_valid(pp) then begin
							evt_error = 0
							if ptr_valid( (*pstate).pspec) then begin
								if (*(*(*pstate).pspec)[0]).orphan eq 1 then begin
									(*pstate).local_spectra = 1
									(*(*(*pstate).pspec)[0]).orphan = 0
								endif
								if ((*pstate).pspec ne pp) and ((*pstate).local_spectra eq 1) then free_spectra, (*pstate).pspec
							endif
							(*p).charge = charge
							if notify then begin
								widget_control, (*pstate).charge, set_value=string((*p).charge)

								(*pstate).pspec = pp
								(*pstate).local_spectra = 0
								(*(*pp)[0]).orphan = 1
								*(*pstate).path = extract_path( (*p).output_file)
								notify, 'path', (*pstate).path, from=group
								notify, 'spectra', pp, from=group
							endif
						endif
						end
					else:
				endcase
				end
			else:
		endcase

finish:
	print,'EVT finish time = ',systime()
	return

bad_ptr:
	warning, 'evt_start', 'Bad pointers or (*pstate).'
	return
bad_ylut_build:
    warning, 'evt_start', 'Error building Y Lookup Table.'
    return
bad_no_evt:
    warning, 'evt_start', 'No EVT file selected.'
    return
bad_sort_mode:
    warning, 'evt_start', 'MPDA projection mode requires a valid .mpdam file.'
    return
end
