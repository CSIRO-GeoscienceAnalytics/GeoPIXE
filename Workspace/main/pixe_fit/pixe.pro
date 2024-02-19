function pixe, a, org, rorg, peaks, n_channels, x, background1, background2, detector, $
		dfa=df, mask=maski, do_tail=do_tail, e_low=e_low, e_high=e_high, $
		initial=initiali, show_df=show_dfi, scale_df=scale_dfi, dff=dff, $
		pure=pure, pileup_mode=pileup_mode, pileup_spec=pileup, $
		tweek_el=tweek_el, tweek_lines=tweek_lines, compton=compton

;	'x' is the vector of channels to fit (less cuts, etc.).
;	'n_channels' is the maximum number of channels.
;
;	'a' 			is the vector of ALL parameters.
;	'org' 			is the offset to the start of peaks.
;	'rorg' 			is the offset to the start of tweeks.
;	'mask' 			enables the varying of selected parameters.
;	'peaks' 		is a pointer to a structure containing all X-ray details:
;					{n_el, z, shell, n_lines, lines, e, intensity, yield}
;	'background'	is the actual background spectrum.
;	'detector' 		is a pointer to a detector struct.
;
;	Increment the function vector 'signal', and the partial differivative 'df'.
;	Both of these run over ALL spectrum channels.
;
;	Note that the 'df' matrix is also over ALL parameters, even those
;	that are 'mask'ed off.
;
;	'do_tail' 		a vector to enable a tail for each line.
;	'compton'		pointer to the Compton parameter struct (see 'fit-setup')
;	'e_low, e_high' are nominally the fitting range.
;
;	'initial'		if set, then don't do DFs (unless show_df).
;	'pileup_mode'	use the 'pileup' spectrum instead of sum_peak.
;	'show_df'		select a DF to show for debugging.
;					Then return finite difference in DFF too.
;	'scale_df'		scale DF (and dff) by this factor. If 2 element vector, then 2nd scales DFF.
;	'pure'			return just the pure component 'pure',
;					0-* elements, -3 background, -6 sum.
;
;	tweek_el=n		Select number of element to tweek line intensities for (n=-1 means OFF)
;	tweek_lines=tweek_lines	Array over lines; each element selects tweek parameter for a line (-1 means OFF).

;	Remember that arrays 'signal' and 'df' are over the full spectrum range.
;	However, the calculation here is limited to the range of channels
;	given by 'x_low', 'x_high'.
;
;	See Method/software log 8, page 260.
;
;	Parameters:	0 Noise		2 cal B		4 pileup	5 tail amp		7  backgnd 1	8 Compton tail amp
;				1 Fano		3 cal A					6 tail length	10 backgnd 2	9 Compton tail length
;
;	(ORG - RORG) should always to 10 (for line Tweeks)
;
;	ORG			start index for element intensity parameters
;	RORG		start index for line intensity tweaks
;				These are set in 'pixe_setup'.
;
;	for RORG=11, ORG=21
;						11-20	tweek lines
;						21-*	element peak area
;
;	Note: The legal values of ORG and RORG are such that ORG-RORG equals 10.
;-------------------------------------------------------------------------

COMPILE_OPT STRICTARR

	if n_elements(initiali) lt 1 then initiali=0
	if n_elements(scale_dfi) lt 1 then scale_dfi=[1.0,1]
	if n_elements(show_dfi) lt 1 then show_dfi=-1
	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if n_elements(pileup) lt 1 then pileup=0.0
	if n_elements(tweek_el) lt 1 then tweek_el=-1

	scale_df = scale_dfi
	if n_elements(scale_df) eq 1 then scale_df = [scale_df,1]
	show_df = show_dfi < (n_elements(a)-1)
	no_df = 0
	initial = initiali
	if initial and (show_df lt 0) then no_df=1

	mask = maski
	mask_save = mask
	a_save = a
	if n_elements(pure) gt 0 then begin
		no_df = 1
		show_df = -1
		a[org:*] = 0.0
		a[7] = 0.0
		a[10] = 0.0
		a[4] = 0.0
		if pure ge 0 then begin
			a[org+pure] = 1.0
		endif else if pure eq -2 then begin
			a[7] = 1.0
		endif else if pure eq -1 then begin				;@9-19
			a[10] = 1.0
		endif else if pure+org eq 4 then begin
			a[4] = 1.0
		endif
	endif

	na = n_elements(a)
	signal = fltarr(n_channels)
	df = dblarr(n_channels,na)

	tail_amp = [tail_amplitude( detector, (*peaks).e)]
	tail_len = [tail_length( detector, (*peaks).e)]
	nk = n_elements( (*peaks).e[*,0])
	ni = n_elements( (*peaks).e[0,*])
	tail_amp = reform( tail_amp, nk,ni, /overwrite)
	tail_len = reform( tail_len, nk,ni, /overwrite)
	Camp = (*compton).tail.amp
	Clen = (*compton).tail.len
	comp_shift = (*compton).shift
	comp_spread = (*compton).spread

	comp = (*peaks).lines
	comp[*] = 0
	q = where( (*peaks).lines eq line_index('Compton'))					; Compton lines
	if q[0] ne -1 then comp[q]=1
	q = where( (*peaks).lines eq line_index('Compton2'))				; Compton+Compton pileup
	if q[0] ne -1 then comp[q]=2
	auger = (*peaks).lines
	auger[*] = 0
	q = where( ((*peaks).lines eq line_index('K-LL')) or ((*peaks).lines eq line_index('K-MM')))
	if q[0] ne -1 then auger[q]=1

	beta_tail = (*peaks).intensity
	beta_tail[*] = 1.

;	K beta enhancement ...

	q = where( ((*peaks).z lt 50) and ((((*peaks).lines ge 4) and ((*peaks).lines le 8))  or  $
						 (((*peaks).lines ge 10) and ((*peaks).lines le 12))) )
	if q[0] ne -1 then beta_tail[q] = 2.

	for i=0L,(*peaks).n_els-1 do begin
		if (*peaks).n_lines[i] gt 0 then begin
			for k=0L,(*peaks).n_lines[i]-1 do begin

				tweek = -1
				if (i eq tweek_el) then begin
					if tweek_lines[k] ne -1 then tweek = rorg+tweek_lines[k]
				endif

				line, i+org, (*peaks).e[k,i], (*peaks).intensity[k,i], x, $
					a, signal, df, detector, tail_amp[k,i], tail_len[k,i], mask=mask, $
					do_tail=do_tail[i], e_low=e_low, e_high=e_high, beta_tail=beta_tail[k,i], $
					no_df=no_df, auger=auger[k,i], tweek=tweek, compton=comp[k,i], $
					e_beam=(*peaks).e_beam, comp_tail_amp=Camp, comp_tail_len=Clen, $
					comp_shift=comp_shift, comp_spread=comp_spread
			endfor
		endif
	endfor

	if mask[7] and (no_df eq 0) then begin
		df[x,7] = double(background1[x])
	endif
	if mask[10] and (no_df eq 0) then begin
		df[x,10] = double(background2[x])
	endif
	if mask[4] and (no_df eq 0) then begin
		df[x,4] = double(pileup[x])
	endif

	result = signal + a[7]*background1 + a[10]*background2 + a[4]*pileup

	if show_df ge 0 then begin
		da = dblarr(na)
		da[show_df] = a[show_df] * 0.01D+0

		f2 = pixe( a+da, org, rorg, peaks, n_channels, x, background1, background2, detector, $
				mask=mask, do_tail=do_tail, e_low=e_low, e_high=e_high, $
				/initial, pileup_mode=pileup_mode, pileup_spec=pileup, $
				tweek_el=tweek_el, tweek_lines=tweek_lines, compton=compton )

		df[*,show_df] = scale_df[0] * df[*,show_df]
		dff = scale_df[0] * (f2 - result) / da[show_df]
		case round(scale_df[1]) of
			1: begin
				dff = 2.0 * dff
				end
			2: begin
				dff = 10.0 * dff
				end
			3: begin
				dff = 100.0 * dff
				end
			4: begin
				dff = 1.0 + dff
				end
			5: begin
				dff = 10.0 + dff
				end
			6: begin
				dff = 100.0 + dff
				end
			7: begin
				dff = 1000.0 + dff
				end
			else:
		endcase
	endif

	mask = mask_save
	a = a_save

	return, result
	end
