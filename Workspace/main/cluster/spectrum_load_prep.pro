pro spectrum_load_prep, pstate, F, F2, device=obj, opt=opt, group=group, throttle=throttle, pileup=pileup, linearize=linearize, $
		sensitivity=sensitivity, append=append

; Do file preparation for a spectrum files load/import. The actual read is done in 'spectrum_load_do'.
; The 'opt' struct contains the details of the desired load.
; The arguments 'sensitivity' (and 'throttle', 'pileup', 'linearize') are passed back for 
; use in 'spectrum_load_do' routine.

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
       warning,'spectrum_load_prep',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, finish
    endif
endif

spectrum_preview
if n_elements(opt) lt 1 then return
if n_elements(old) lt 1 then old=0
if n_elements(append) lt 1 then append=0
do_pileup = 0
do_throttle = 0
linearize = ''
pileup = ''
throttle = ''
sensitivity = 0.0

if obj_valid(obj) eq 0 then begin
	obj = obj_new( opt.device_name)
endif
if obj->name() ne opt.device_name then begin
	obj_destroy, obj
	obj = obj_new( opt.device_name)
endif
if obj_valid(obj) eq 0 then return

;print,'Paths in: raw:',*(*pstate).dpath,', output:',*(*pstate).path
path = opt.raw ? *(*pstate).dpath : *(*pstate).path 

; Select first data file (or multiply select several to append) ...

F = file_requester(filter = '*'+opt.in_ext, title=opt.request, path=path, $	
		dialog_parent=group, fix_filter=0, /multiple, preview_routine=(opt.preview ? 'spectrum_preview' : ''))
if F[0] eq '' then goto, finish

; Update paths (so far) ...

if opt.raw then begin
	*(*pstate).dpath = extract_path(F[0])
endif else begin
	*(*pstate).path = extract_path(F[0])
endelse

; Last data file (for multifile sets, only using spec_evt) ...

mp_error = 1
if opt.spec_evt then begin
	if opt.multifile then begin
		if n_elements(F) eq 1 then begin
			F2 = file_requester(filter = '*'+opt.in_ext, title='Select final list-mode file', $
					path=extract_path(F[0]), dialog_parent=group, fix_filter=0)
		endif
	endif
endif

; Select output path, if this is being read from 'raw' data path 'dpath' ...

if opt.raw then begin
	path = file_requester( title='select Output path', path=*(*pstate).path, $	
								dialog_parent=group, /dir)
	if path eq '' then goto, finish
	*(*pstate).path = path
endif
print,'Paths now: raw:',*(*pstate).dpath,', output:',*(*pstate).path

; Read file header info ...

if opt.spec_evt then begin
	out = *(*pstate).path + strip_path(F[0])
	mp = get_header_info( obj, F[0], group=group, output=out, /silent, error=mp_error)
	if mp_error eq 0 then begin
		opt.xr = mp.scan.x_pixels
		opt.yr = mp.scan.y_pixels
		sensitivity = mp.sensitivity

;		Deadtime cal is one Device parameter set in both header and device sort_options ...
		obj->update_device_from_header
	endif
endif

; If indirect flux is used, pop-up flux_select requester to gather conversion, PVs, etc. ...
; Note: If 'sensitivity' changes, it appears in the preamp variables, so need to update the
; returned sensitivity result, which may be used by 'Spectrum_Load_do'.

if opt.use_ic ne 0 then begin
	flux_select, group_leader=group, TLB=tlb, device=obj, $
				pars=(*pstate).pflux, path=path, debug=0, charge_mode=opt.ic_mode, $
				dpath=*(*pstate).dpath, evt_file=F[0], sensitivity=sensitivity
	sensitivity = (*(*pstate).pflux).preamp.val * (*(*pstate).pflux).preamp.unit
	if (*(*pstate).pflux).error then goto, finish
endif

; For pileup and throttle, 'found' means the field was found in the header, 'on' means it was also
; enabled in the run and the 'file' is not blank. Very old data may have found=0. New data will have
; found=1 and a separate 'on' and 'file'.

if opt.spec_evt then begin
	path = *(*pstate).path
	do_pileup = 1
	do_throttle = 1
	if mp_error eq 0 then begin
		if mp.pileup.on then pileup = mp.pileup.file
		if mp.throttle.on then throttle = mp.throttle.file
		do_pileup = (mp.pileup.found eq 0) or mp.pileup.on 					; popup a file-requester if either
		do_throttle = (mp.throttle.found eq 0) or mp.throttle.on			; no field in header (old) or on=1.
	endif
	
	if opt.use_linear then begin
		Fl = file_requester( /read, filter = ['*.linear.var','*.linear'], title='Select (re-)linearization function file', $
					file=linearize, path=path, dialog_parent=group, fix_filter=1, updir=3)	;, /skip_if_exists)
		linearize = Fl[0]
	endif
	if opt.use_pileup and do_pileup then begin
		Fp = file_requester( /read, filter = ['*.pileup.var','*.txt'], title='Select pileup limits file', $
					path=path, file=pileup, dialog_parent=group, fix_filter=1, updir=3)		;, /skip_if_exists)
;		if Fp[0] ne '' then pileup = Fp[0]
		pileup = Fp[0]
	endif
	if opt.use_throttle and do_throttle then begin
		Ft = file_requester( /read, filter = ['*.throttle.var','*.txt'], title='Select throttle factors file', $
					path=path, file=throttle, dialog_parent=group, fix_filter=1, updir=3)	;, /skip_if_exists)
;		if Ft[0] ne '' then throttle=Ft[0]
		throttle = Ft[0]
	endif
endif

	if file_write_test( *(*pstate).path, /dir) eq 0 then begin
		warning,'spectrum_load_prep',['Cannot create output file in this dir:',*(*pstate).path]
		goto, finish
	endif

	if do_pileup eq 0 then pileup=''
	if do_throttle eq 0 then throttle=''
	
finish:
;	obj_destroy, obj		; keep this for 'spectrum_load_do'
	return
end
