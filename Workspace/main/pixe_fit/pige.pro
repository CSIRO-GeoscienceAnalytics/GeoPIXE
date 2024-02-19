function pige, a, org, peaks, n_channels, x, detector, $
		dfa=df, mask=maski, do_step=do_step, e_low=e_low, e_high=e_high, $
		initial=initiali, show_df=show_dfi, scale_df=scale_dfi, dff=dff, $
		pure=pure

;	'x' is the vector of channels to fit (less cuts, etc.).
;	'n_channels' is the maximum number of channels.
;
;	'a' is the vector of ALL parameters.
;	'org' is the offset to the start of peaks.
;	'mask' enables the varying of selected parameters.
;	'peaks' is a pointer to a structure containing all X-ray details:
;		{n_el, z, shell, n_lines, lines, e, intensity, yield}
;	'detector' is a pointer to a detector struct.
;
;	Increment the function vector 'signal', and the partial differivative 'df'.
;	Both of these run over ALL spectrum channels.
;
;	Note that the 'df' matrix is also over ALL parameters, even those
;	that are 'mask'ed off.
;
;	'do_step' 	a vector to enable a step for each line.
;	'e_low, e_high' are nominally the fitting range.
;
;	'initial'	if set, then don't do DFs (unless show_df).
;	'show_df'	select a DF to show for debugging.
;			Then return finite difference in DFF too.
;	'scale_df'	scale DF (and dff) by this factor.
;	'pure'		return just the pure component 'pure',
;			0-n_els-1 elements, n_els-* background.
;
;	Remember that arrays 'signal' and 'df' are over the full spectrum range.
;	However, the calculation here is limited to the range of channels
;	given by 'x_low', 'x_high'.
;
;	See Method/software log 13, page 98.
;
;	Parameters:	0 width		2 cal		4 free		5 steps amp		7 free
;				1 width		3 cal					6 steps slope	8,9 free
;
;-------------------------------------------------------------------------

COMPILE_OPT STRICTARR

	if n_elements(initiali) lt 1 then initiali=0
	if n_elements(scale_dfi) lt 1 then scale_dfi=[1.0,2.0]
	if n_elements(show_dfi) lt 1 then show_dfi=-1

	scale_df = scale_dfi
	if n_elements(scale_df) eq 1 then scale_df = [scale_df,2.0]
	show_df = show_dfi < (n_elements(a)-1)
	no_df = 0
	initial = initiali
	if initial and (show_df lt 0) then no_df=1
	mask = maski
	mask_save = mask
	a_save = a
	if show_df ge 0 then mask[show_df] = 1
	if n_elements(pure) gt 0 then begin
		no_df = 1
		show_df = -1
		a[org:*] = 0.0
		a[org+pure] = 1.0
	endif

	na = n_elements(a)
	signal = fltarr(n_channels)
	df = dblarr(n_channels,na)

	for i=0L,(*peaks).n_els-1 do begin
		if (*peaks).n_lines[i] gt 0 then begin
			for k=0L,(*peaks).n_lines[i]-1 do begin

				line_gamma, org+i, org+(*peaks).n_els+i, (*peaks).e[k,i], $
					(*peaks).intensity[k,i], x, $
					a, signal, df, detector,  mask=mask, $
					do_step=do_step[i], e_low=e_low, e_high=e_high, $
					no_df=no_df
			endfor
		endif
	endfor

	result = signal

	if show_df ge 0 then begin
		da = fltarr(na)
		da[show_df] = a[show_df] * 0.001

		f2 = pige( a+da, org, peaks, n_channels, x, detector, $
				mask=mask,  do_step=do_step, e_low=e_low, e_high=e_high, $
				/initial)

		dff = scale_df[1]*scale_df[0] * (f2 - result) / da[show_df]
		df[*,show_df] = scale_df[0] * df[*,show_df]
	endif

	mask = mask_save
	a = a_save

	return, result
	end
