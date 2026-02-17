pro geopixe_browser_event, event

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	
	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

	types = ['BeforePopup','LoadStart','LoadEnd','LoadError','AddressChange','TitleChange', $
			'StatusMessage','FaviconChange','ConsoleMessage','Notify','OnDownloadBegin','OnDownloadEnd']

	case tag_names( event,/structure) of
		'NOTIFY': begin
;			OnNotify_time_amp, event
			return
			end
		'WIDGET_BROWSER': begin
;			print, types[event.type], ' = ', event.value
;			return
			end
		'WIDGET_TRACKING': begin
;			OnTracking_time_amp, Event
			return
			end
		'WIDGET_KILL_REQUEST': begin
			print,'Kill request ...'
			goto, kill
			end
		else:
	endcase

	uname = widget_info( event.id, /uname)
	case uname of

		'browser-tlb': begin
			case Tag_Names(Event, /STRUCTURE_NAME) of
				'WIDGET_BASE': begin
;					print,event.x,event.y
					x = (event.x > 400) - 6
					y = (event.y > 300) - 30
					print,'Browser: x,y=',x,y
					widget_control, (*pstate).browser, scr_xsize=x, scr_ysize=y
					end
				else:
			endcase
			end

		'browser': begin
			end

		'back-button': begin
			widget_control, (*pstate).browser, /BROWSER_GO_BACK
			end
		'forward-button': begin
			widget_control, (*pstate).browser, /BROWSER_GO_FORWARD
			end
		'geopixe_overview': begin
			geopixe_browser, 'Help/GeoPIXE Overview.htm', title='GeoPIXE Overview', group=event.top
			end
		'geopixe_ref_button': begin
			geopixe_browser, 'Help/GeoPIXE-Users-Guide.htm', title='GeoPIXE Users Guide', group=event.top, key='Table of Contents'
			end
		'geopixe_worked_examples_button': begin
			geopixe_browser, 'Help/GeoPIXE Worked Examples Open Notes.htm', title='GeoPIXE Worked Examples', group=event.top
			end

		else:
	endcase
	return

bad_state:
	warning,'geopixe_browser_event',['STATE variable has become ill-defined.','GeoPIXE Browser needs to be closed.'],/error
	goto, kill

kill:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) eq 'STRUCT' then begin
		if ptr_valid( (*pstate).ptoc) then ptr_free, (*pstate).ptoc
	endif
	ptr_free, pstate

die:
	widget_control, event.top, /destroy
	return
end

;-------------------------------------------------------------------------------------------------

function gbrowser_toc, file, error=error, count=nn

;	Build a table of contents with matching "#" links.
;
;	Scan HTML file for all <a href="link">key</a> links
;	Return array of nodes {key:'', link: ''} with these.
;	All keys are compressed (single whitespace only).
;	Note that these "keys" need to be unique in a HTML document.

	r = 0
	nn = 0L
	error = 1
	if n_elements(file) eq 0 then return, r
	if file_test(file) eq 0 then return, r
	
	s = strarr(100000L)
	on_ioerror, bad
	openr, lun, file, /get_lun
	on_ioerror, cont
	readf, lun, s
cont:
	close_file, lun
	q = where( strlen(s) gt 0, nq)
	if nq eq 0 then return, r
	s = s[q]
	
	node = {key:'', link: ''}

	for i=0,nq-1 do begin
		j = locate( '<a href=', s[i])
		if j le -1 then continue

		s2 = strjoin( s[i:i+3], ' ')
		t = strmid( s2, j+9)
		k = locate( '">', t)
		if k le -1 then begin
			print,'Bad k: i=',i,', j=',j,', k=',k,', text=',strmid(s2,j,20)
			continue
		endif
		link = strmid( t, 0, k)

		t = strmid( t, k+2)
		m = locate( '<', t)
		if m le -1 then begin
			print,'Bad m: i=',i,', j=',j,', k=',k,', m=',m,', text=',strmid(s2,j,20)
			continue
		endif
		key = strcompress( strip_non_print( strmid( t, 0, m)))

		node.key = key
		node.link = link
		if nn eq 0 then begin
			table = node
			nn = 1L
		endif else begin
			table = [table, node]
			nn = nn+1
		endelse
	endfor

	print,nn
	error = 0
	return, table
bad:
	close_file, lun
	warning,'gbrowser_toc','Error opening file:'+file
	return, r
end

;---------------------------------------------------------------------------------------------------------

pro geopixe_browser, file, group=group, title=title, key=key

; Open an embedded browser to display 'file'.
; Store parameters back in '*pars' (from parent), if present.
; If a valid window is still open, then reuse it.
;
; If 'key' supplied, search a table of contents in file for any hits to 'key' key, and 
; if found, display that section. For now the whole key must be given.
; Note that these "keys" need to be unique in a HTML document.

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	if n_elements(geopixe_root) lt 1 then startupp
	if n_elements(group) lt 1 then group = 0L
	if n_elements(Title) lt 1 then Title = 'GeoPIXE Worked Examples'
	if n_elements(file) lt 1 then file = 'Help/' + 'GeoPIXE Worked Examples Open Notes.htm'
;	if n_elements(key) lt 1 then key = 'H.  XANES Imaging'

	filename = geopixe_root + file
;	print, filename
	s = strsplit( filename, ' ', /extract, count=n)
	if n gt 1 then s = strjoin( s, '%20')

	toc = gbrowser_toc( filename, error=err, count=nn)					; look for a table of contents

	note = ''
	if (err eq 0) and (n_elements(key) ne 0) then begin
		q = where( toc.key eq strcompress( strip_non_print(key)), nq)	; look for requested 'key' in toc
		if nq gt 0 then begin
			link = toc[q[0]].link										; find matching # link
			s = s+link
		endif else begin
			note = ' (tag "'+key+'" not found)'
		endelse
	endif
	htmlfile = 'file:///' + s	

	tlb = WIDGET_BASE(Title = Title+note[0], uname='browser-tlb', /column, $
						/TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS, group=group)
	base1 = WIDGET_BASE( tlb)
	
	browser = WIDGET_BROWSER(base1, VALUE=htmlfile[0], uname='browser', XSIZE=740, YSIZE=800)

	bbase = widget_base( tlb, /row, /base_align_center, ypad=0, xpad=0, space=5)
	button = widget_button( bbase, value='Back', uname='back-button')
	button = widget_button( bbase, value='Forward', uname='forward-button')

	button = widget_button( bbase, value='GeoPIXE Overview', uname='geopixe_overview')
	button = widget_button( bbase, value='GeoPIXE Manual', uname='geopixe_ref_button')
	button = widget_button( bbase, value='Worked Examples', uname='geopixe_worked_examples_button')

	state = { $
			file:		filename, $						; filename
			html:		htmlfile, $						; HTML file
			ptoc:		ptr_new( toc, /no_copy), $		; table of contents, keys versus links
			tlb:		tlb, $							; TLB
			browser:	browser $						; browset ID
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	WIDGET_CONTROL, tlb, /REALIZE

	xManager, 'geopixe_browser', tlb, /NO_BLOCK
	return
end