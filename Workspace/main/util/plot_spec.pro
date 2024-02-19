	pro plot_spec, pspec, xl,xh, window=w, top=top,bot=bot, noerase=noerase, $
		noaxes=noaxes, charsize=csize, ylog=ylog, noxlabel=noxlabel, noylabel=noylabel, $
		noyaxis=noyaxis, noxaxis=noxaxis, select=select,ticklen=ticklen, noxnames=noxnames, $
		thick=thick, charthick=cthick, axisthick=athick, no_overlays=no_overlays, $
		noynames=noynames, colour=colour, xticklen=xticklen, yticklen=yticklen, $
		xlabel=xlabel, ylabel=ylabel, plot_number=plot_number, bw=bw, interp=interp
;
;	plot a spectrum, pointed to by spectrum pointer 'pspec',
;	'pspec' can be an array of pointers.
;	see "null_spectrum.pro" for details of spectrum.
;
	if n_elements(w) gt 0 then begin
		wxl = w[0]
		wyl = w[1]
		wxh = w[2]
		wyh = w[3]
	endif

	default_plot, dthick, dathick, dcsize, dcthick

	if n_elements(colour) eq 0 then colour=-1
	if n_elements(noaxes) eq 0 then noaxes=0
	if n_elements(no_overlays) eq 0 then no_overlays=0
	if n_elements(noerase) eq 0 then noerase=0
	if n_elements(ylog) eq 0 then ylog=0
	if n_elements(noylabel) eq 0 then noylabel=0
	if n_elements(noxlabel) eq 0 then noxlabel=0
	if n_elements(noxaxis) eq 0 then noxaxis=0
	if n_elements(noyaxis) eq 0 then noyaxis=0
	if n_elements(noxnames) eq 0 then noxnames=0
	if n_elements(noynames) eq 0 then noynames=0
	if n_elements(csize) eq 0 then csize=dcsize
	if n_elements(ticklen) eq 0 then ticklen=0.02
	if n_elements(xticklen) eq 0 then xticklen=ticklen
	if n_elements(yticklen) eq 0 then yticklen=ticklen
	if n_elements(thick) eq 0 then thick=dthick
	if n_elements(cthick) eq 0 then cthick=dcthick
	if n_elements(athick) eq 0 then athick=dathick
	if n_elements(xlabel) eq 0 then xlabel='X-ray Energy (keV)'
	if n_elements(ylabel) eq 0 then ylabel='Counts Per Channel'
	if n_elements(plot_number) eq 0 then plot_number=-1
	if n_elements(bw) eq 0 then bw=0
	if n_elements(interp) eq 0 then interp=0

	white = 12
	black = 0
	!y.type = 0
	if ylog then !y.type=1

	if noerase eq 0 then erase

	n_actual = n_elements(pspec)
	p = pspec[0]
	n = (*p).size
	a = (*p).cal.poly[1]
	b = (*p).cal.poly[0]
	x = a * findgen(n) + b
	y = *((*p).data)
	low = 0
	high = n-1

	lx = min(x)
	hx = max(x)
	if n_params(0) gt 1 then begin
		if n_elements(select) gt 0 then begin
			lx = xl
			hx = xh
		endif else begin
		    lx = clip( xl, lx,hx)
		    hx = clip( xh, lx,hx)
		endelse
	endif else begin
		xl = lx
		xh = hx
	endelse

    l = clip( fix((lx-b)/a), 0, (*p).size-1 )
    h = clip( fix((hx-b)/a), 0, (*p).size-1 )
	if n_elements(top) eq 0 then begin
	    top = 1.2*max(y(l:h))
	endif
	if n_elements(bot) eq 0 then begin
		if ylog eq 1 then begin
		    bot = 0.3*min(y(l:h))
		    if bot lt 0.0001 then bot = 0.5
		endif else begin
		    bot = 1.2*min(y(l:h))
		    if bot gt 0.0 then bot = 0.0
		endelse
	endif
	if ylog then bot = bot > 0.5
	top = top > bot + 0.5
;	print,'Plot_spec: bot = ',bot,'  top = ',top
;
	if noxlabel then begin
		!x.title = ''
	endif else begin
		!x.title = xlabel
	endelse
	if noylabel then begin
		!y.title = ''
	endif else begin
		!y.title = ylabel
	endelse

	if noaxes or (noxaxis and noyaxis) then begin
		!x.style = 13
		!y.style = 13
	endif else begin
		if noxaxis or noyaxis then begin
			if noxaxis then begin
				!x.style = 13
			endif else begin
				!x.style = 9
			endelse
			if noyaxis then begin
				!y.style = 13
			endif else begin
				!y.style = 9
			endelse
		endif else begin
			!x.style = 1
			!y.style = 1
		endelse
	endelse

;print,'!p.color=',!p.color

if noerase and (!p.multi[0] eq 0) then begin
	if n_elements(w) gt 0 then begin
		if noxnames then begin
			plot,[0,0],[0,0],/nodata,/noerase,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30)
		endif else if noynames then begin
			plot,[0,0],[0,0],/nodata,/noerase,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				ytickname=replicate(' ',30)
		endif else if noxnames and noynames then begin
			plot,[0,0],[0,0],/nodata,/noerase,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30), ytickname=replicate(' ',30)
		endif else begin
			plot,[0,0],[0,0],/nodata,/noerase,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen
		endelse
	endif else begin
		if noxnames then begin
			plot,[0,0],[0,0],/nodata,/noerase, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30)
		endif else if noynames then begin
			plot,[0,0],[0,0],/nodata,/noerase, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				ytickname=replicate(' ',30)
		endif else if noxnames and noynames then begin
			plot,[0,0],[0,0],/nodata,/noerase, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30), ytickname=replicate(' ',30)
		endif else begin
			plot,[0,0],[0,0],/nodata,/noerase, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen
		endelse
	endelse
endif else begin
	if n_elements(w) gt 0 then begin
		if (noaxes eq 0) then begin
			polyfill,[wxl-0.05,wxh,wxh,wxl-0.05],[wyl,wyl,wyh,wyh],/norm, $
				color=!p.background
		endif
		if noxnames then begin
			plot,[0,0],[0,0],/nodata ,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30)
		endif else if noynames then begin
			plot,[0,0],[0,0],/nodata ,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				ytickname=replicate(' ',30)
		endif else if noxnames and noynames then begin
			plot,[0,0],[0,0],/nodata ,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30), ytickname=replicate(' ',30)
		endif else begin
			plot,[0,0],[0,0],/nodata ,position=w, xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen
		endelse
	endif else begin
		if noxnames then begin
			plot,[0,0],[0,0],/nodata , xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30)
		endif else if noynames then begin
			plot,[0,0],[0,0],/nodata , xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				ytickname=replicate(' ',30)
		endif else if noxnames and noynames then begin
			plot,[0,0],[0,0],/nodata , xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen, $
				xtickname=replicate(' ',30), ytickname=replicate(' ',30)
		endif else begin
			plot,[0,0],[0,0],/nodata , xlog=0,ylog=ylog, $
				xrange=[xl,xh], yrange=[bot,top], xthick=athick,ythick=athick, $
				charthick=cthick, thick=thick, charsize=csize, yticklen=xticklen, xticklen=yticklen
		endelse
	endelse
endelse
	if(bot lt 0.0) then begin
	    plots,[xl,xh],[0.0,0.0],linestyle=1,thick=thick
	endif

	col = (bw eq 1) ? !p.color : ((colour ge 0) ? colour : spec_colour(0))
	if !p.background eq black then begin
		if col eq black then col=white
	endif
	if !p.background eq white then begin
		if col eq white then col=black
	endif

	if n_elements(select) ge 2 then begin
	    l = clip( fix((select[0]-b)/a), 0, (*p).size-2 )
	    h = clip( fix((select[1]-b)/a), 1, (*p).size-1 )
	endif
	if (*p).has_errors then begin
		for i=l,h do begin
			t = [x[i],x[i]]
			y1 = ylog ? 10.0^!y.crange[0] : !y.crange[0]
			y2 = ylog ? 10.0^!y.crange[1] : !y.crange[1]
			y = [(*(*p).data)[i] - (*(*p).error)[i], (*(*p).data)[i] + (*(*p).error)[i]]
			y = clip(y, y1,y2)
			plots, t,y, color=col, thick=thick
	 	endfor
		oplot, x[l:h], (*(*p).data)[l:h], psym=6, color=col, thick=thick, symsize=!p.symsize
		if interp then oplot, x[l:h], (*(*p).data)[l:h], psym=0, color=col, thick=thick, symsize=!p.symsize
	endif else begin
		oplot, x[l:h], (*(*p).data)[l:h], psym=10, color=col, thick=thick
	endelse

	if (*p).n_fit gt 0 then begin
		for i=0L,(*p).n_fit-1 do begin
			pf = (*p).fit[i]
			n = (*pf).size
			a = (*pf).cal.poly[1]
			b = (*pf).cal.poly[0]
			x = a * findgen(n) + b
			y = *((*pf).data)
			col = (bw eq 1) ? !p.color : ((colour ge 0) ? colour : spec_colour(i+1))
			if !p.background eq black then begin
				if col eq black then col=white
			endif
			if !p.background eq white then begin
				if col eq white then col=black
			endif
			oplot, x,y, color=col, thick=1.3*thick, psym=0
		endfor
	endif

	if (n_actual le 1) or no_overlays or (plot_number eq 1) then return
	na = n_actual
	if plot_number ge 1 then na = na < plot_number

	for j=1L,na-1 do begin
		p = pspec[j]
		n = (*p).size
		a = (*p).cal.poly[1]
		b = (*p).cal.poly[0]
		x = a * findgen(n) + b
		y = *((*p).data)

		col = ((colour ge 0) ? colour : spec_colour(j))
		if !p.background eq black then begin
			if col eq black then col=white
		endif
		if !p.background eq white then begin
			if col eq white then col=black
		endif
		oplot, x,y, color=col, thick=thick, psym=10

		if (*p).n_fit gt 0 then begin
			for i=0L,(*p).n_fit-1 do begin
				pf = (*p).fit[i]
				n = (*pf).size
				a = (*pf).cal.poly[1]
				b = (*pf).cal.poly[0]
				x = a * findgen(n) + b
				y = *((*pf).data)

				col = ((colour ge 0) ? colour : spec_colour(i+1))
				if !p.background eq black then begin
					if col eq black then col=white
				endif
				if !p.background eq white then begin
					if col eq white then col=black
				endif
				oplot, x,y, color=col, thick=thick, psym=0
			endfor
		endif
	endfor

	return
	end

