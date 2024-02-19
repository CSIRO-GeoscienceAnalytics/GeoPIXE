pro Spectrum_Load_do, pstate, F, F2, device=obj, opt=opt, group=group, throttle=throttle, pileup=pileup, linearize=linear, $
			sensitivity=sensitivity, charge_mode=charge_mode, flux_pv=flux_pv, conv=conv, $
			Q=charge, verify=verify, file_return=sret, append=append, error=err

; Load spectra using the Import Select 'opt' details.
; Execute by passing import selection 'name' to device.
; Need to treat extraction of spectra from raw list-mode data differently,
; flagged by the spec_evt=1 in the import options struct 'opt'. This calls
; the 'spec_evt' routine, passing the device.

COMPILE_OPT STRICTARR
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

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
       warning,'Spectrum_Load_do',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

err = 1
if n_elements(F) lt 1 then return
if n_elements(opt) lt 1 then return
if n_elements(old) lt 1 then old=0
if n_elements(F2) lt 1 then F2=''
if n_elements(append) lt 1 then append=0
if n_elements(pileup) lt 1 then pileup=''
if n_elements(throttle) lt 1 then throttle=''
if n_elements(linear) lt 1 then linear=''
if n_elements(sensitivity) lt 1 then sensitivity=0.0
if n_elements(charge) lt 1 then charge=0.0
if n_elements(verify) lt 1 then verify=0

if obj_valid(obj) eq 0 then begin
	obj = obj_new( opt.device_name)
endif
if obj->name() ne opt.device_name then begin
	obj_destroy, obj
	obj = obj_new( opt.device_name)
endif
if obj_valid(obj) eq 0 then return

got_one = 0
pa = ptr_new()
append_on = 0
if append then begin
	if ptr_valid((*pstate).p) then begin
		if ptr_valid( (*(*pstate).p)[0]) then begin
			got_one = 1
			pa = *(*pstate).p
			append_on = 1
		endif
	endif
endif

if verify and opt.spec_evt then begin
	vpileup = {old:'',new:''}
	vthrottle = {old:'',new:''}
	vlinear = {old:'',new:''}
	path = *(*pstate).path
	if opt.use_linear and (linear ne '') then begin
		Fl = file_requester( /read, filter = ['*.linear.var','*.linear'], title='Select (re-)linearization function file', $
					file=linear, path=path, dialog_parent=group, fix_filter=1, /skip_if_exists)
		if Fl[0] ne linear then begin
			vlinear.new = Fl[0]
			vlinear.old = linear
			linear = Fl[0]
		endif
	endif
	if opt.use_pileup and (pileup ne '') then begin
		Fp = file_requester( /read, filter = ['*.pileup.var','*.txt'], title='Select pileup limits file', $
					path=path, file=pileup, dialog_parent=group, fix_filter=1, /skip_if_exists)
		if Fp[0] ne pileup then begin
			vpileup.new = Fp[0]
			vpileup.old = pileup
			pileup = Fp[0]
		endif
	endif
	if opt.use_throttle and (throttle ne '') then begin
		Ft = file_requester( /read, filter = ['*.throttle.var','*.txt'], title='Select throttle factors file', $
					path=path, file=throttle, dialog_parent=group, fix_filter=1, /skip_if_exists)
		if Ft[0] ne throttle then begin
			vthrottle.new = Ft[0]
			vthrottle.old = throttle
			throttle = Ft[0]
		endif
	endif
	sret = { pileup:vpileup, throttle:vthrottle, linear:vlinear }
endif

; Last data file (for multifile sets, only using spec_evt) ...

mp_error = 1
if opt.spec_evt then begin
	if opt.multifile then begin
		if n_elements(F) eq 1 then begin
			files = select_evt_files( F, F2, 1, opt.separate, opt.in_ext)
		endif else begin
;			files = select_evt_files( F, '', 1, opt.separate, opt.in_ext)
			files = F
		endelse
	endif
	
	mp = get_header_info( obj, F[0], group=group, /silent, error=mp_error)
	if mp_error eq 0 then begin
		opt.xr = mp.scan.x_pixels
		opt.yr = mp.scan.y_pixels
		if sensitivity eq 0.0 then sensitivity = mp.sensitivity
	endif
endif

; If indirect flux is used, gather conversion, PVs, etc. ...

if opt.use_ic ne 0 then begin
	p = (*pstate).pflux
	if ptr_good(p) eq 0 then goto, finish
	if (*p).error then goto, finish

	if sensitivity ne 0.0 then begin
		val = charge_gain_units( sensitivity, units=unit)
		(*p).preamp.val = val
		(*p).preamp.unit = unit
		print,'Spectrum_Load_do: found sensitivity = ',sensitivity,', val,unit=',val,unit
	endif
	if n_elements(charge_mode) gt 0 then if charge_mode ge 0 then (*p).charge_mode=charge_mode
	if n_elements(flux_pv) gt 0 then if flux_pv ne '' then (*p).preamp.pv=flux_pv
	if n_elements(conv) gt 0 then if conv ne 0.0 then (*p).charge_conversion=conv

;	use_dwell=0 so that flux is by integration over time, not dwell*rate
	flux_ic = {mode:(*p).charge_mode, pv:(*p).preamp.pv, val:(*p).preamp.val, $
				unit:(*p).preamp.unit, conversion:(*p).charge_conversion, $
				use_dwell:0, dwell:(*p).dwell}
	pv_list = *(*p).pic_list
	q = where( pv_list eq (*p).preamp.pv, nq)
	if nq eq 0 then pv_list = [(*p).preamp.pv, pv_list]
endif

widget_control, /hourglass
Fo = F
break_for = 0
cluster = 0
if ptr_good((*pstate).pflux) then cluster = (*(*pstate).pflux).cluster

; Loop through multiply selected files, or a single multifile data set ...
; Now that some device parameters may have been set in Flux_select, are now in 'obj'

devpars = obj->get_options()

; There is a problem with (non list-mode) spectra import here, as it does not get the 'flux_ic'
; information. And the 'obj' internal settings have not been updated with these (e.g. PV name).
; Probably should have a method to update 'obj' internal setting relevant for flux here.
; In the meantime, the spectra import may be lacking complete flux information (unlike 'spec_evt' mode).

for i=0L,n_elements(F)-1 do begin

	if opt.spec_evt then begin
		break_for = 1
		try_mp = 1
		
		if cluster then begin					; Compose spec_evt command for cluster processing	

			if opt.multifile then begin
				command = 'spec_evt, ' + stringify(files)
				nf = n_elements(files)
			endif else begin
				command = 'spec_evt, ' + stringify(F[i])
				nf = 1
			endelse
			args =	command + ', device=' + stringify(opt.device_name) + ', ' + $
					'devpars=' + stringify(devpars) + ', ' + $
					'do_tot=' + stringify(opt.use_tot) + ', ' + $
					'ic=' + stringify(flux_ic) + ', ' + $
					'pv_list=' + stringify(pv_list) + ', ' + $
					'pileup=' + stringify(pileup) + ', ' + $
					'linearize=' + stringify(linear) + ', ' + $
					'throttle=' + stringify(throttle) + ', ' + $
					'xrange=' + stringify(opt.xr) + ', ' + $
					'yrange=' + stringify(opt.yr) + ', ' + $
					'output=' + stringify(*(*pstate).path)
					
			cluster_client, title = 'GeoPIXE Cluster Spectrum Extract', $
								subtitle = *(*pstate).path, $
								args=args, $
								n_files = nf, $
								group_leader = group, $
								presult = presults, $
								error = error
										
		;	Results is an array containing the result strings from each slave.
		;	This is an array of 'xxx.SPEC.nn' file names of resulting spectra.
		;	Read each spectrum file and combine spectra into one. 
		;	Also combine charge, flux, etc.
	
			q = where( (*presults ne '') and (*presults ne 'null'), nq)
			if (error eq 0) and (nq gt 0) then begin
				widget_control, /hourglass
				p = read_spec( (*presults)[q[0]], error=error)
				if error eq 0 then begin
					pp = ptr_new(p)
					if nq gt 1 then begin
						for j=1L, nq-1 do begin
							p2 = read_spec( (*presults)[q[j]], error=err)
							pp2 = ptr_new(p2, /no_copy)
							if err eq 0 then begin
								spectrum_load_spectrum_increment, pp, pp2, /free
							endif
						endfor
;						if ptr_valid(pp) then p = *pp		
					endif
					for j=0L, n_elements(p)-1 do begin
						(*p[j]).file = strip_file_ext( (*p[j]).file, /double) + '.spec'
					endfor
					write_spec, p, (*p[0]).file
				endif
			endif else if nq eq 0 then begin
				print,'spectrum_do_load: no results returned from Cluster processing.'
				err = 1
				return
			endif else if error then begin
				print,'spectrum_do_load: error encountered during Cluster processing.'
				err = 1
				return
			endif
			
		endif else begin

		;	Scan data set(s) using 'spec_evt' ...
		;	For multifile sets, break FOR loop after first set.

			if opt.multifile then begin
				spec_evt, files, group=group, spectra=pp, device=opt.device_name, do_tot=opt.use_tot, /progress, ic=flux_ic, pv_list=pv_list, $
			         				pileup=pileup, linearize=linear, throttle=throttle, xrange=opt.xr, yrange=opt.yr, $
			         				output=*(*pstate).path, devpars=devpars, /suppress
				break_for = 1
			endif else begin
				spec_evt, F[i], group=group, spectra=pp, device=opt.device_name, do_tot=opt.use_tot, /progress, ic=flux_ic, pv_list=pv_list, $
			         				pileup=pileup, linearize=linear, throttle=throttle, xrange=opt.xr, yrange=opt.yr, $
			         				output=*(*pstate).path, devpars=devpars, /suppress
			endelse
			if ptr_valid(pp) then p = *pp		
		endelse
		
	endif else begin
		try_mp = 0
		print,'spectrum_new_load: import spec, device=',obj->name(),', mode="',opt.name,'"'
		p = obj->import_spec( opt.name, F[i], group=group)
		
		for j=0,n_elements(p)-1 do begin
			if ptr_valid(p[j]) then begin
				if obj_valid((*p[j]).DevObj) eq 0 then (*p[j]).DevObj = clone_device_object(obj)
			endif
		endfor
	endelse

	if (n_elements(p) ge 1) then begin
		if ptr_valid(p[0]) then begin
			if try_mp and (n_elements(mp) eq 0) then begin
				mp = get_header_info( obj, F[i], group=group, /silent, error=mp_error)
			endif else begin
				if (n_elements(mp_error) eq 0) or (n_elements(mp) eq 0) then mp_error=1
			endelse
			if (mp_error eq 0) then begin
				done = intarr(geopixe_max_adcs)
				for j=0L,n_elements(p)-1 do begin
					adc = clip((*p[j]).station-1,0,geopixe_max_adcs-1)
					(*p[j]).detector = mp.detector[adc]
					if (done[adc] eq 0) and mp.cal[adc].on then begin
						(*p[j]).cal.order = 1
						(*p[j]).cal.units = mp.cal[adc].units
						(*p[j]).cal.poly[0] = mp.cal[adc].b
						(*p[j]).cal.poly[1] = mp.cal[adc].a
					endif
					(*p[j]).comment = mp.title
					(*p[j]).sample = mp.sample
					(*p[j]).grain = mp.grain
					(*p[j]).energy = mp.energy
					done[adc] = 1
					
					if (mp.charge gt 1.0e-6) and (opt.use_ic eq 0) then (*p[j]).charge = mp.charge
					if (charge gt 1.0e-6) and (opt.use_ic eq 0) then (*p[j]).charge = charge
					if (*p[j]).charge gt 1.0e-6 then charge = (*p[j]).charge
				endfor
			endif
			if got_one then begin
				pa = [pa,p]
			endif else begin
				pa = p
				got_one = 1
			endelse
		endif else warning,'Spectrum_Load_do','Error reading spectrum file: '+F[i]
	endif else warning,'Spectrum_Load_do','Error reading spectrum file: '+F[i]

    if break_for then break
endfor

if n_elements(pa) lt 1 then goto, done
if ptr_valid(pa[0]) then begin
	if ptr_valid((*pstate).p) then begin
		if ptr_valid( (*(*pstate).p)[0] ) then begin
			if (*(*(*pstate).p)[0]).orphan eq 1 then begin
				(*pstate).local = 1
				(*(*(*pstate).p)[0]).orphan = 0
			endif
		endif
		if (*pstate).local and ((*(*pstate).p)[0] ne pa[0]) and (append_on eq 0) then begin
			free_spectra, (*pstate).p
		endif
	endif
	(*pstate).p = ptr_new( pa)								; !!!pspec
	*(*pstate).pshow = replicate(1,n_elements(pa))
	for i=0L,n_elements(*(*pstate).pshow)-1 do (*(*(*pstate).p)[i]).show = (*(*pstate).pshow)[i]
	(*pstate).local = 1
	(*(*(*pstate).p)[0]).orphan = 0
	(*pstate).file = F[0]
		
	init_spectra, (*pstate).p, (*pstate).width,(*pstate).view, /new, negative=(*pstate).show_negative

	j = current_plot( pstate)
	p = (*((*pstate).p))[j]
	(*pstate).elow = (*p).elow
	(*pstate).ehigh = (*p).ehigh

	(*pstate).vlow = 0
	(*pstate).vhigh = (*pstate).view-1
	widget_control, (*pstate).draw2, set_draw_view=[(*pstate).vlow,0]
	draw_spectra, pstate
	notify, 'spectra-changed', (*pstate).p, from=group

	if ptr_valid(p) then begin
		(*(*pstate).pf).pspec = p
		(*(*pstate).pf).pall = (*pstate).p
		notify, 'spectrum-fit', (*pstate).pf, from=group
	endif
endif

done:
	print,'Paths out: raw:',*(*pstate).dpath,', output:',*(*pstate).path
	notify, 'path', (*pstate).path, from=(*pstate).tlb
;	(*pstate).file = ''
	err = 0

finish:
;	obj_destroy, obj		; keep the object for another load ...
							; needs to be explicitly destroyed elsewhere
	return
end
