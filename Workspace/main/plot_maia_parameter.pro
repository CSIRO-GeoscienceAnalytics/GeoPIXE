pro plot_maia_parameter, id, par, cgm=cgm, ps=ps, white=white, bw=bw, title=title, true=true, $
			min=ymin, max=ymax, screen=screen, layout=file1

; Plot a parameter 'par' on the detector map, with labels 'id' on each detector
;
; /true		Use actual detector pad sizes, else fill out map
;			show axes only for /true
; layout	layout file (e.g. "Maia_384C.csv")
; min, max	set display range
; /cgm		CGM plot output
; /ps		postscript output
; /screen	screen output 
;			else defaults to printer output, which will fallback to screen
; /white	on white background, else black

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
common Colors, rrr,ggg,bbb, rr,gg,bb
catch_errors_on=0

sz = 0.95	; 2.2
!p.charsize=sz
startupp, /colours, /database

if n_elements(screen) lt 1 then screen = 0
if n_elements(cgm) lt 1 then cgm = 0
if n_elements(ps) lt 1 then ps = 0
if n_elements(white) lt 1 then white = 0	;1
if n_elements(id) lt 1 then return
if n_elements(id) ne n_elements(par) then return
if n_elements(title) lt 1 then title = ''
if n_elements(true) lt 1 then true = 0
if n_elements(file1) lt 1 then file1 = "Maia_384C.csv"
if file1 eq '' then file1 = "Maia_384C.csv"

; Note: 'file_search2' in 'file_requester' below may have set a draw id for progress bar.
; So don't call 'file_requester' after opening window, before plotting to it.

path1 = extract_path( file1)
file1 = file_requester(/read, filter='*.csv', updir=2, file=file1, /skip_if_exists)
d = read_detector_layout( file1, maia=maia, error=error)
if error or (maia eq 0) then return

case !version.os_family of
	'MacOS': begin
		retain = 2
		end
	'unix': begin
		retain = 2
		end
	else: begin
		retain = 1
		end
endcase

if cgm then begin
	set_device, 'CGM', file=strip_file_ext(file1)+'.cgm'
	used_printer = 1
endif else if ps then begin
	set_device, 'PS', /portrait, file=strip_file_ext(file1)+'.eps', /square
	used_printer = 1
endif else if screen then begin
	used_printer = 0
	window, 0, xsize=800, ysize=800, retain=retain
endif else begin
	if new_dialog_printersetup() then begin
		used_printer = 1
		white = 1
	endif else begin
		used_printer = 0
		window, 0, xsize=800, ysize=800, retain=retain
	endelse
endelse

default_plot, thick, athick, csize, cthick, thick_scale=1.2

if true then begin
	wlo = 0.10
	base = 0.08
	whi = 0.97
	top = 0.95
endif else begin
	wlo = 0.08
	base = 0.06
	whi = 0.99
	top = 0.97
endelse

;if (!d.name eq 'CGM') then begin
;	wlo = wlo+0.1
;	whi = whi
;	base = base
;	top = top
;endif

if white then begin
	!p.color = spec_colour('black')
	!p.background = spec_colour('white')
endif

;----------------------------------------------------------------------------

!p.title = title
!x.title = 'X (mm)'
!y.title = 'Y (mm)'

g = 10.				; plot box size
yof = 0.1			; Y offset for index label
delta = 0.02		; increase size so that hole is within lines

if true then begin
	c = !p.color
endif else begin
	c = !p.background
endelse

if n_elements(ymin) eq 0 then ymin = min(par)
if n_elements(ymax) eq 0 then ymax = max(par)

plot, [0,0],[0,0], /nodata, xrange=[-g,g],yrange=[-g,g], $
    position=[wlo,base,whi,top], xstyle=1, thick=thick, charthick=cthick*1.2, $
    charsize=csize*1.4, xthick=athick, ythick=athick, color=c

xyouts, 0.0,1.0, 'Max = '+str_tidy(max(par)), color=!p.color, align=0.5, charsize=1.2*csize, charthick=cthick
xyouts, 0.0,-1.0, 'Min = '+str_tidy(min(par)), color=!p.color, align=0.5, charsize=1.2*csize, charthick=cthick

col = 16B + bytscl( par, min=ymin, max=ymax, top=99)
n = n_elements(par)

if true eq 0 then begin
	w = max(d.data.width)
	h = max(d.data.height)
	yt = max(d.data.y) + h/2 + 0.3
	xyouts, 0.0,yt, title, color=!p.color, align=0.5, charsize=csize*1.6, charthick=cthick*1.2
	csz = 1.1 * csize
endif else csz = csize

for k=0,n-1 do begin
	q = where( id[k] eq d.data.index, nq)
	if nq eq 0 then continue
	i = q[0]									; CSV index that matches detector number

	if true then begin
		w = d.data[i].width + delta
		h = d.data[i].height + delta
	endif
	x = d.data[i].x
	y = d.data[i].y
;	j = d.data[i].index
	j = id[k]
	boxx = [-w,w,w,-w,-w]/2
	boxy = [-h,-h,h,h,-h]/2

	polyfill, x+boxx, y+boxy, color=col[k], /data

	if true then plots, x+boxx, y+boxy, thick=thick*0.6
	if fix(rr[col[k]])+fix(gg[col[k]])+fix(bb[col[k]]) gt 3*128 then begin
		c = spec_colour('black')
	endif else begin
		c = spec_colour('white')
	endelse
	xyouts, x,y-yof,/data, str_tidy(j), color=c, align=0.5, charsize=csz, charthick=cthick
endfor

; Legend bar

if true then begin
	w = 0.7
	h = 7.0/100.
	x = -11.4
	y0 = 2.5
endif else begin
	w = 0.9
	h = 9.0/100.
	x = -10.7
	y0 = -1.0
endelse
boxx = [-w,w,w,-w,-w]/2
boxy = [-h,-h,h,h,-h]/2
y = y0

for k=0,100-1 do begin
	polyfill, x+boxx, y+boxy, color=16+k, /data
	y = y+h
endfor
boxy = 100.*boxy
y = y0 + 50*h
plots, x+boxx, y+boxy, /data, thick=thick*0.7

if true eq 0 then begin
	xyouts, x,y0+100.*h+0.2, str_tidy(ymax,places=1,length=6), color=!p.color, align=0.5, charsize=1.2*csize, charthick=cthick
	xyouts, x,y0-0.5, str_tidy(ymin,places=1,length=6), color=!p.color, align=0.5, charsize=1.2*csize, charthick=cthick
endif

;----------------------------------------------------------------------------

if used_printer then begin
	device,/close
	set_plot, 'WIN'
endif
return
end
