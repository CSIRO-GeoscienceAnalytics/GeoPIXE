
pro midas_browse_event, Event

COMPILE_OPT STRICTARR

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'midas_browser_event',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   free_record, pseg
       return
    endif
endif

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			widget_control, event.id, get_uvalue=s
			if event.enter eq 1 then begin
				if size(s,/tname) eq 'STRING' then begin
					widget_control, (*pstate).help, set_value=s
				endif else if tag_present('help',s) then begin
					if size(s.Help,/tname) eq 'STRING' then begin
						widget_control, (*pstate).help, set_value=s.Help
					endif
				endif
			endif else begin
				widget_control, (*pstate).help, set_value='Select data class and file format in the lists above.'
			endelse
			return
			end
		'WIDGET_TIMER': begin
;			timer_on = (*pstate).timer_on
;			blog_read_next, (*pstate).ps, (*pstate).p, (*pstate).tags, (*pstate).tag_select, timer_on=timer_on
;			(*pstate).timer_on = timer_on
;			update_midas_records, pstate
;			if (*pstate).timer_on then widget_control, event.id, timer=(*pstate).time
;			return
			end
		'WIDGET_KILL_REQUEST': begin
			goto, kill
			end
		else:
	endcase

	if (*pstate).realtime then begin
		xoff = 26
		yoff = 137
		xmin = 720
		ymin = 200
		dh = 0
	endif else begin
		xoff = 130
		yoff = 175
		xmin = 720
		ymin = 300
		dh = 105
	endelse
	
	uname = widget_info( event.id, /uname)
	case uname of

		'midas_browse_TLB': begin
			w = event.x - xoff > xmin
			h = event.y - yoff > ymin
			h1 = h/2
			h2 = h/2
			widget_control, (*pstate).record_title, scr_xsize=w
			widget_control, (*pstate).record_list, scr_ysize=h1, scr_xsize=w
			widget_control, (*pstate).detail_list, scr_ysize=h2, scr_xsize=w
			widget_control, (*pstate).help, scr_xsize=w+dh
			if (*pstate).realtime eq 0 then begin
				widget_control, (*pstate).segment_list, scr_ysize=h+67
			endif
			end

		'run': begin
			F = file_requester( /read, $
					title='Select a member of the desired "Run"', path=(*pstate).path, $
					filter='*.mid', group=event.top)
			if F ne '' then begin
;				t = strip_file_ext(strip_path(F))
;				l = locate_last( '_', t) > 1
;				root = strmid(t,0,l)
				root = ''
				(*pstate).run = 0
;				(*pstate).run = fix(strip_file_ext(strip_path(F)))
				(*pstate).path = extract_path(F)
				widget_control, (*pstate).dir_text, set_value=(*pstate).path
				F = find_file2( (*pstate).path + root +'*.mid')
				g = strip_file_ext(strip_path(F))
;				l = locate_last( '_', g)
;				for i=0,n_elements(g)-1 do g[i] = strmid(g[i],l[i]+1)
;				q = sort( long2(g))
				q = sort( g)
				g = g[q]
				*(*pstate).pfiles = F[q]
				widget_control, (*pstate).segment_list, set_value=g
				*(*pstate).psegment = 0
				widget_control, (*pstate).record_list, set_value=''
				widget_control, (*pstate).detail_list, set_value=''
				
;				ylut = get_maia_ylut( F[q[0]], error=err)
;				if err eq 0 then begin
;					*(*pstate).pylut = ylut
;					(*pstate).ylut_ok = 1
;				endif else (*pstate).ylut_ok = 0
			endif
			end

		'run-text': begin
			widget_control, (*pstate).run_text, get_value=s
			print, 'Datatype for "run" return is - ', size(s, /tname)
			s = str_tidy(s)
			if s ne '' then begin
;				t = strip_file_ext(strip_path(strtrim(s,2)))
;				l = locate_last( '_', t) > 1
;				root = strmid(t,0,l)
				root = ''
				(*pstate).run = 0
;				(*pstate).run = fix(strip_file_ext(strip_path(F)))
				(*pstate).path = extract_path(F)
				widget_control, (*pstate).dir_text, set_value=(*pstate).path
				F = find_file2( (*pstate).path + root +'*.mid')
				g = strip_file_ext(strip_path(F))
;				l = locate_last( '_', g)
;				for i=0,n_elements(g)-1 do g[i] = strmid(g[i],l[i]+1)
;				q = sort( long2(g))
				q = sort( g)
				g = g[q]
				*(*pstate).pfiles = F[q]
				widget_control, (*pstate).segment_list, set_value=g
				*(*pstate).psegment = 0
				widget_control, (*pstate).record_list, set_value=''
				widget_control, (*pstate).detail_list, set_value=''
			endif
			end

		'dir': begin
			F = file_requester( /read, /dir, $
					title='Select the working directory', path=(*pstate).path, $
					group=event.top)
			if F ne '' then begin
				(*pstate).path = F
				widget_control, (*pstate).dir_text, set_value=(*pstate).path
			endif
			end

		'dir-text': begin
			widget_control, (*pstate).dir_text, get_value=s
			if s ne '' then begin
				(*pstate).path = s
				widget_control, (*pstate).dir_text, set_value=(*pstate).path
			endif
			end

		'option-old': begin
			(*pstate).old = event.select
			update_midas_records, pstate
			end

		'segment-list': begin
			index = widget_info( (*pstate).segment_list, /list_select)
			*(*pstate).psegment = (*(*pstate).pfiles)[index]
			free_midas_record, (*pstate).p
			if widget_info( (*pstate).skip_text, /valid) then begin
				widget_control, (*pstate).skip_text, get_value=s
				(*pstate).skip = long(s)
			endif else (*pstate).skip=0
			pseg = read_midas_segments( *(*pstate).psegment, type=0, $
							n_buffer=(*pstate).n_buffer, skip=(*pstate).skip)	;, state=state)
			(*pstate).p = ptr_new(pseg, /no_copy)
			
			spec = midas_spectrum( (*pstate).p)				;, icr=icr, dtp=dtp)
			if n_elements(spec) le 1 then goto, finish
			!p.title = 'Total Energy Spectrum (this buffer)'
			!x.title = 'Channel'
			!y.title = 'Counts per channel'
			window,1,xsize=600,ysize=300
			top = max(spec) > 10
			plot,spec,/nodata,xstyle=1,/ylog,yrange=[0.5,top*1.05],ystyle=1
			oplot,spec,psym=10,color=spec_colour('green')
;			xyouts, 0.7,0.85,'ICR = '+str_tidy(icr)+' c/s',/norm
;			xyouts, 0.7,0.77,'DT = '+str_tidy(dtp, places=-3)+' %',/norm
			
			update_midas_records, pstate
			
;			if (*pstate).ylut_ok and (index lt n_elements(*(*pstate).pylut))then begin
;				s = ['','Lookup table Y origin for this segment = '+ str_tidy((*(*pstate).pylut)[index])]
;				widget_control, (*pstate).detail_list, set_value = s
;				*(*pstate).pdetail = s
;			endif
			end

		'record-list': begin
			(*pstate).record = (*(*pstate).pback)[event.index]
			update_midas_details, pstate
			
			spec = midas_spectrum( (*pstate).p, select=(*pstate).record)				;, icr=icr, dtp=dtp)
			if n_elements(spec) le 1 then goto, finish
			!p.title = 'Total Energy Spectrum (this record)'
			!x.title = 'Channel'
			!y.title = 'Counts per channel'
			window,1,xsize=600,ysize=300
			top = max(spec) > 10
			plot,spec,/nodata,xstyle=1,/ylog,yrange=[0.5,top*1.05],ystyle=1
			oplot,spec,psym=10,color=spec_colour('green')
			;			xyouts, 0.7,0.85,'ICR = '+str_tidy(icr)+' c/s',/norm
			;			xyouts, 0.7,0.77,'DT = '+str_tidy(dtp, places=-3)+' %',/norm
			end

		'details-list': begin
			pr1 = *(*pstate).p
			q = *(*pstate).precord
			pr = pr1[q]
			n = n_elements(pr)
			i = (*pstate).record
			tag = (*pr[i]).tag
			type = (*pr[i]).type
;			case tag of 
;				26: begin						; monitor
;					s0 = *(*pstate).pdetail
;					if (event.index ge 4) and (n_elements(s0) gt event.index) then begin
;						s = s0[event.index]
;						s3 = strsplit(s,' ',/extract)
;						if strlen(s3[0]) gt 0 then begin
;							maia_IC_name = s3[0]
;							val = fltarr(n)
;							sep = string([10B,13B])
;							k = 0
;							for j=0L,n-1 do begin
;								if (*pr[j]).tag eq 26 then begin
;									pd = pr[j]							
;									b = *(*pd).b
;									sc = string(b)
;									s1 = strsplit(sc,sep,/extract)
;									ns1 = n_elements(s1)
;									if ns1 ge 1 then begin
;										for i=0L,ns1-1 do begin
;											s2 = strsplit(s1[i],' ',/extract)
;											ns2 = n_elements(s2)
;											if (s2[0] eq maia_IC_name) and (ns2 gt 3) then begin
;												val[k] = float2(s2[3])
;												k = k+1
;											endif
;										endfor
;									endif
;								endif
;							endfor
;							val = val[0:k-1]
;							q = where(val ne 0.0, nq)
;							if nq gt 2 then begin
;								window,1,xsize=600,ysize=350
;								bs = min([0.01*max(val),0.1*(max(val)-min(val))])>0.000001*max(val)
;								h = histogram(val,binsize=bs,omin=omin,omax=omax)
;								n = n_elements(h)
;								x = omin + (omax-omin)*findgen(n)/n
;								print,'min=',omin,' max=',omax
;								dx = (omax-omin)*0.05 > 0.000002*max(val)
;								!p.title = 'Selected Monitor PV histogram'
;								!x.title = maia_IC_name + ' (s.d. = ' + str_tidy(stddev(val)) + ', ' + str_tidy(100.*stddev(val)/mean(val))+ '%)'
;								plot,[x[0],x,max(x)+bs],[0,h,0],psym=10, xrange=[omin-bs-dx,omax+bs+dx], xstyle=1, yrange=[0,1.05*max(h)], ystyle=1
;							endif
;						endif
;					endif
;					end
;				else:
;			endcase
			end

		'clear': begin
			free_midas_record, (*pstate).p
			update_midas_records, pstate
			end
			
		'read': begin
;			blog_read_next, (*pstate).ps, (*pstate).p, (*pstate).tags, (*pstate).tag_select
;			update_midas_records, pstate
			end
			
		'start': begin
			(*pstate).timer_on = 1
			widget_control, event.id, timer=(*pstate).time
			end
			
		'stop': begin
			(*pstate).timer_on = 0
			end
			
		'select': begin
			q = where( (*pstate).tags eq '', nq)
			if nq gt 0 then (*pstate).tag_select[q] = 0
			select = element_select( (*pstate).tlb, (*pstate).tags, old_select=(*pstate).tag_select, $
					path=(*pstate).path, title='Select Record Tags to Display')
			q = where( (*pstate).tag_select ne select, nq)
			if nq gt 0 then begin
				(*pstate).tag_select = select
				update_midas_records, pstate
			endif
			end

		'options': begin
			(*pstate).options[event.value] = event.select
			update_midas_records, pstate
			end

		'buffer-size': begin
			mega_byte = 1024LL * 1024LL
			sizes = [100LL*1024LL, 200LL*1024LL, 500LL*1024LL, [1LL, 2LL, 3LL, 4LL, 6LL, 8LL, 10LL, 12LL, 15LL, 20LL, 30LL, 40LL, 50LL, 60LL, 80LL, 101LL]*mega_byte]
			(*pstate).n_buffer = sizes[event.index]
			end

		'skip-text': begin
			widget_control, (*pstate).skip_text, get_value=s
			(*pstate).skip = long(s)
			end

		'detail-list': begin
			end

		else:
	endcase
finish:
	return

kill:
	free_midas_record, (*pstate).p				; full ptr to ptr array of record data
	ptr_free, (*pstate).precord				; the subset index list or records to display
	ptr_free, (*pstate).pback				; back index list
	ptr_free, (*pstate).pfiles				; list of blog files
	ptr_free, (*pstate).psegment			; selected blog segment file
	if (*pstate).realtime then close_file, (*(*pstate).ps).unit

	widget_control, event.top, /destroy
	return
end

;-----------------------------------------------------------------

pro update_midas_records, pstate

COMPILE_OPT STRICTARR

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'update_midas_records',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pdj
       return
    endif
endif

; List mode data field definitions ...

@midas_listmode.def

;xlast = -1000
;ylast = -1000
n_bytes = 1024L
k = 0

	s = ''
	if ptr_valid((*pstate).p) eq 0 then return
	if n_elements(*(*pstate).p) lt 1 then return
	pr1 = *(*pstate).p
	if ptr_good(pr1[0], /struct) eq 0 then begin
		s = ''
		goto, done
	endif
	n = n_elements(pr1)
	good = intarr(n)
	for i=0L,n-1 do begin
		if (*pstate).tag_select[(*pr1[i]).tag < (n_elements((*pstate).tag_select)-1) ] then good[i]=1
	endfor
	q = where( good eq 1,nq)
	*(*pstate).precord = q
	if nq lt 1 then begin
		s = ''
		goto, done
	endif
	pr = pr1[q]

	n = n_elements(pr)
	close, 2
	home = geopixe_environment( temp=temp_dir)
	temp_file = temp_dir + 'browse.txt'
	openw, 2, temp_file
	j = 0
	back = 0L

	for i=0L,n-1 do begin
;		if i ge 23654 then begin
;			print,'debug'
;		endif
;	    if (*pr[i]).length eq 0 then begin
;	    	case data_type of
;				3: begin										; newseg
;					b = strip_path((*pr[i]).file)
;					sc = strtrim(strcompress(string(b)),2)
;					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
;	        				sc, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "File= ",A)'
;	        		back = [back,i]
;					end
;
;				else: begin
;					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
;	    		     		format='(I7,I4,2x, A7, I7, I11, I11)'
;	        		back = [back,i]
;					end
;	    	endcase
;	    endif else begin

			data_type = (*pstate).data_type[(*pr[i]).tag]
			case data_type of
				'8000'x: begin									; ODB1
					b = *(*pr[i]).b
					sml = replace(10B,' ',string(b[0:(n_elements(b) < 50)-1]))
					sc = 'ODB = '+sml
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
	        				sc, format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, A)'
	        		back = [back,i]
					end
				'8001'x: begin									; ODB2
					b = *(*pr[i]).b					
					sml = replace(10B,' ',string(b[0:(n_elements(b) < 50)-1]))
					sc = 'ODB = '+sml
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
	        				sc, format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, A)'
	        		back = [back,i]
					end
				1: begin										; NMP
					b = *(*pr[i]).b
					header = ulong( b, 0, size_midas_header)
;					swap_bytes, header, big_endian_data=0		; this one has been swapped already
					trigger = header[0] and trigger_mask
					serial = header[1]
					time = header[2]
					flags = header[3]
					sc = 'Trigger = '+str_tidy(trigger)
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
	        				'Trigger = ', trigger, format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, A, Z10)'
	        		back = [back,i]
					end
				'0101'x: begin									; PIXL
					b = *(*pr[i]).b
					nb = (*pr[i]).length/2
					if nb ge 4 then begin
						data = uint( b, 0, nb)
						swap_bytes, data, big_endian_data=0
					endif else data=0
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
	        				data, format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, "DAC x,y =",2I5,5x,"Pixel x,y =",2I5)'
	        		back = [back,i]
					end
				'0102'x: begin									; ADCx
					b = *(*pr[i]).b
					sc = '# events = '+str_tidy((*pr[i]).length/4)
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
						sc, format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, A)'
					back = [back,i]
					end
				'0103'x: begin									; STAT
					b = *(*pr[i]).b
					nb = (*pr[i]).length/4
					if nb ge 4 then begin
						data = ulong( b, 0, nb)
						swap_bytes, data, big_endian_data=0
					endif else data=0
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
	        				data, format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, "Q =",I5,5x,"ICR =",I5,5x,"Events =",I5,5x,"Dwell =",I5)'
	        		back = [back,i]
					end
				'0104'x: begin									; DTME
					b = *(*pr[i]).b
					nb = (*pr[i]).length/4
					if nb ge 1 then begin
						data = float( b, 0, nb)
						swap_bytes, data, big_endian_data=0
					endif else data=0
					printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, b[0], $
						data[0:(nb<4)-1], format='(I7,Z5,2x, A6, I7, 2x, Z10, 2x, 10(f7.3,2x))'
					back = [back,i]
					end

				else: begin
					if (*pr[i]).length eq 0 then begin
						printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, $
	         				format='(I7,Z5,2x, A6, I7, 1x, "No payload")'
					endif else begin
						printf, 2, (*pr[i]).index, data_type, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, $
	         				(*(*pr[i]).b)[0:1<(((*pr[i]).length-1)>0)], format='(I7,Z5,2x, A6, I7, 1x, 2(Z10))'
					endelse
	        		back = [back,i]
					end
			endcase
;		endelse
	endfor
	close, 2
	*(*pstate).pback = n_elements(back) gt 1 ? back[1:*] : 0L

	openr, 2, temp_file
	s = strarr(2*n)
	k = 0L
	t = ''
	on_ioerror, fin
	for i=0L,2*n-1 do begin
		readf, 2, t
		s[i] = t
		k = k+1
	endfor
fin:
	s = (k gt 0) ? s[0:k-1] : ''
	close, 2

done:
	widget_control, (*pstate).record_list, set_value = s
	if k gt 0 then widget_control, (*pstate).record_list, set_list_select = k-1
;	widget_control, (*pstate).detail_list, set_value = ''
	return
end

;-----------------------------------------------------------------

pro update_midas_details, pstate

COMPILE_OPT STRICTARR

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'update_midas_details',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pd
       free_record, pdj
       return
    endif
endif

; List mode data field definitions ...

@midas_listmode.def

;xlast = -1000
;ylast = -1000
n_bytes = 64*1024L	

	if ptr_valid((*pstate).p) eq 0 then return
	if n_elements(*(*pstate).p) lt 1 then return
	pr1 = *(*pstate).p
	n = n_elements(pr1)
	if (n lt 1) or (size(*(pr1[0]),/tname) ne 'STRUCT') then begin
		s = ''
		goto, done
	endif
	q = *(*pstate).precord
	pr = pr1[q]

	i = (*pstate).record						; record number
	if (*pstate).realtime then begin
		pd = pr[i]
		goto, more
	endif
	
	file = (*pr[i]).file
	ptr = (*pr[i]).ptr
	type = (*pr[i]).type

	pd = read_midas_segments( file, type=type, select=ptr)	;, state=state)
	if ptr_valid(pd[0]) eq 0 then return
	(*pd).index = (*pr[i]).index
	(*pd).file = (*pr[i]).file
	(*pd).ptr = (*pr[i]).ptr

more:
	close, 2
	home = geopixe_environment( temp=temp_dir)
	temp_file = temp_dir + 'browse.txt'
	openw, 2, temp_file
;	in = (*pd).tv_sec
;    d = in/86400	&	in = in mod 86400
;    h = in/3600		&	in = in mod 3600
;    m = in/60		&	in = in mod 60
;    s = in
;    day = str_tidy(d)+' '+str_tidy(h)+':'+str_tidy(m)+':'+str_tidy(s)
	day = 0
	data_type = (*pstate).data_type[(*pd).tag]
	
	printf, 2, data_type, day, strip_path((*pd).file), $
	        				format='(3x,"index",T14,"tag:",1x, Z5, T33,"Time:",A, T65,"File: ",A)'
	printf, 2, (*pd).index, (*pstate).tags[(*pd).tag], (*pd).ptr, (*pd).length, $
	        				format='(I8,T10,A14, T29,"File ptr: ",I10,T63,"Length:",I10)'
	printf, 2, ''
	n = 3

; Known Midas list-mode Event types ...
;								Hex
;	0	ODB1		ODB begin			8000
;	1	ODB2		ODB end				8001
;	2	NMP		MRD NMP pixel event		1
;	3	PIXL		PIXL bank			101
;	4	ADCx		ADCx bank			102
;	5	STAT		STAT bank			103
;	6	DTME		DT CORR bank		104

; 	    if (*pd).length eq 0 then begin
;	    	case (*pd).tag of
;				3: begin										; newseg
;					b = strip_path((*pd).file)
;					sc = strtrim(strcompress(string(b)),2)
;					printf, 2, sc, format='( "new segment: File= ",A)'
;					end
;
;				else: begin
;			         printf, 2, 'No Payload (length = 0)'
;					end
;	    	endcase
;	    endif else begin

			data_type = (*pstate).data_type[(*pd).tag]
			case data_type of
				'8000'x: begin									; ODB1
					b = *(*pd).b
					odb = string(b)
					s = XML_to_Hash( odb, /ToStruct)
					printf, 2, odb, format='(T2,A)'
					n = n + n_elements(b)/40
					end
				'8001'x: begin									; ODB2
					b = *(*pd).b
					odb = string(b)
					printf, 2, odb, format='(T2,A)'
					n = n + n_elements(b)/40
					end
				1: begin										; NMP
					b = *(*pd).b
					if (*pd).length lt 4 then goto, cont
					header = ulong( b, 0, size_midas_header)
;					swap_bytes, header, big_endian_data=0		; this one has been swapped already
					trigger = header[0] and trigger_mask
					serial = header[1]
					time = header[2]
					flags = header[3]
					printf, 2, trigger, serial, format='(T6,"Trigger: ",I10,T42,"Serial: ",I10)'
					printf, 2, time, flags, format='(T9,"Time: ",Z10,T43,"Flags: ",Z10)'
					n = n+2
					end
				'0101'x: begin									; PIXL
					b = *(*pd).b
					nb = (*pd).length/2
					if nb lt 4 then goto, cont
					data = uint( b, 0, nb)
					swap_bytes, data, big_endian_data=0
					printf, 2, format='(T8,"PIXL data: ")'
					printf, 2, data, format='(T8,"DAC x,y =",2I5,5x,"Pixel x,y =",2I5)'
					n = n+ 2
					end
				'0102'x: begin									; ADCx
					b = *(*pd).b
					nb = (*pd).length/2
					if nb lt 1 then goto, cont
					data = uint( b, 0, nb)
					swap_bytes, data, big_endian_data=0
					printf, 2, format='(T8,"ADCx data:  (E / n)")'
					printf, 2, data, format='(10(I5,"/",I3,1x))'
					n = n+1 + nb/10
					end
				'0103'x: begin									; STAT
					b = *(*pd).b
					nb = (*pd).length/4
					if nb lt 4 then goto, cont
					data = ulong( b, 0, nb)
					swap_bytes, data, big_endian_data=0
					printf, 2, format='(T8,"STAT data: ")'
					printf, 2, data, format='(T8,"Q =",I5,5x,"ICR =",I5,5x,"Events =",I5,5x,"Dwell =",I5)'
					n = n+2 
					end
				'0104'x: begin									; DTME
					b = *(*pd).b
					nb = (*pd).length/4
					if nb lt 1 then goto, cont
					data = float( b, 0, nb)
					swap_bytes, data, big_endian_data=0
					printf, 2, format='(T8,"DTME data (DT corr factor per detector): ")'
					printf, 2, data, format='(T8,10(f7.3,2x))'
					n = n+2
					end
					
				else: begin
					n_tags = n_elements((*pstate).tags)
					if (*pd).length eq 0 then begin
						printf, 2, (*pd).index, (*pd).tag, (*pstate).tags[(*pd).tag], (*pd).length, $
	         				format='(I7,Z5,2x, A6, I7, 1x, "No payload")'
					endif else begin
						printf, 2, (*pd).index, (*pd).tag, (*pstate).tags[(*pd).tag <(n_tags-1)], (*pd).length, $
	         				(*(*pd).b)[0:min([20,min([n_bytes,(*pd).length])-1])], format='(I7,Z5,2x, A6, I7, 1x, 5(4Z11,3x))'
					endelse
					end
			endcase
;		endelse

cont:
	if (*pstate).realtime eq 0 then ptr_free, pd
	n = n[0]
	n = n + 100
	close, 2
	openr, 2, temp_file
	s = strarr(n)
	k = 0L
	t = ''
	on_ioerror, fin
	for i=0L,n-1 do begin
		readf, 2, t
		if strlen(t) gt 1000 then t=strmid(t,0,10000)
		s[i] = t
		k = k+1
	endfor
fin:
	s = s[0:k-1]
	close, 2

done:
	widget_control, (*pstate).detail_list, set_value = s
	*(*pstate).pdetail = s
	return
end

;-----------------------------------------------------------------

pro midas_browser, _EXTRA=extra

; blogbrowse (midas_browser.sav) loads routines from GeoPIXE.sav.
; These routines are NOT compiled into midas_browser.sav.
; First look locally (e.g. if this launched from runtime SAV file),
; then try a directory "geopixe" at same dir level nearby.

	found = 0
	file = 'GeoPIXE.sav'
	if file_test(file) eq 0 then begin
		file = '../geopixe/GeoPIXE.sav'
		if file_test(file) eq 0 then begin
			a = dialog_message('midas_browser: Failed to restore GeoPIXE.sav.',/error)
		endif else found=1
	endif else found = 1
	if found then restore, file
	startupp, /colours

	midas_browse, _EXTRA=extra
	return
end

;-----------------------------------------------------------------

pro midas_browse, group=group, realtime=realtime, ip=ip, port=port, $
					dt=dt, version=version, debug=debug

;	Browse Maia midas files and records.

COMPILE_OPT STRICTARR
if n_elements(realtime) lt 1 then realtime=0
if n_elements(ip) lt 1 then ip='138.194.25.203'			; blog2
if n_elements(port) lt 1 then port=9000
if n_elements(debug) lt 1 then debug=0
if n_elements(dt) lt 1 then dt=16.7

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if debug then catch_errors_on = 0
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'midas_browser',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
startupp

	version = geopixe_version()		; Version text
	n_detectors = 384				; detector max
	n_elements = 32					; element # max, DA row max

  case !version.os_family of
	'MacOS': begin
		fnt = 'COURIER*BOLD*10'
		xlist = 920
		end
	'unix': begin
		fnt = '6x10'
		xlist = 920
		end
	else: begin
		fnt = 'COURIER*10'
		xlist = 920
 		end
  endcase

	ps = 0L
	timeout = 0.1
	if realtime then begin
		sock = open_socket( ip=ip, port=port, error=error, read_timeout=timeout )
		if error eq 0 then ps = ptr_new(sock)
	endif
	
;	NOTE: This list in 'midas_browse' too ...
;	See also list of tags in 'read_midas', 'get_midas_details', and 'update_midas_records', 'update_midas_details' too ...

@midas_listmode.def

	define_devices
	
	xsize = xlist + 105 + 20
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0) ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])
	toggle_old = 0L
	title = 'iThemba Labs Midas ListMode Browser'
	if realtime then title = title + '  ' + version
	
	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
				group_leader=group, uname='midas_browse_TLB', /base_align_center, /TLB_SIZE_EVENTS, $
				xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_left)

	if realtime eq 0 then begin
		row0base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
		label = widget_label( row0base, value='Midas Browser '+version)
		label = widget_label( row0base, value='   ')
		button = widget_button( row0base, value='Run:', uname='run',/tracking, uvalue='Select the current "Run" number, by selecting one of its files in the file requester.')
		dir_text = widget_text( row0base, value='', uname='dir-text', /tracking, $
					uvalue='Select a member of the desired run using the "Run" button to bring up a file selector.',scr_xsize=500)
		label = widget_label( row0base, value='   ')
	
		rowbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_top)
		col1base = widget_base( rowbase, /column, xpad=0, ypad=0, space=2, /base_align_left)
		label = widget_label( col1base, value='Select Segment(s):')
		segment_List = Widget_List(col1base, UNAME='segment-list', /multiple,  $
					value = labs, /tracking, uvalue = 'Select a "Segment", or multiply select several segments (use "click and drag" select), to read and display in the right panels. Chose a "Run" number above first.', $
					scr_xsize=100 ,scr_ysize=500)
	endif else begin
		rowbase = tbase
		segment_list = 0L
		dir_text = 0L
	endelse
	
	col2base = widget_base( rowbase, /column, xpad=0, ypad=0, space=2, /base_align_left)

	if realtime eq 0 then label = widget_label( col2base, value='Records in selected Segment(s):')
	
	close, 2
	home = geopixe_environment( temp=temp_dir)
	temp_file = temp_dir + 'browse.txt'
	openw, 2, temp_file
	printf, 2, format='(T7,"i",T10,"tag", T17, "name", T25, "len", T37, "Hex", T42, "Payload")'
	close, 2
	openr, 2, temp_file
	rtitle = ''
	readf, 2, rtitle
	close, 2

	record_title = Widget_List(col2base, UNAME='record-title',  $
				value = rtitle, scr_xsize=xlist ,scr_ysize=22, font=fnt, /tracking, uvalue = 'Record component title heading, showing "tag" number and name, "len" length of record in bytes, ' + $
				'"payload" record contents specific to this tag.')
	record_List = Widget_List(col2base, UNAME='record-list',  $
				value = '', /tracking, uvalue = 'Shows all selected record types for the selected Segment(s). Click on one, to select a record from these and display its details in the panel below. ' + $
				'Select record types to display using the "Select Record Tags" button below.', $
				scr_xsize=xlist ,scr_ysize=278, font=fnt)

	row2base = widget_base( col2base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
	button = widget_button( row2base, value='Select Record Tags', uname='select',/tracking, uvalue='Select which record type(s) to display in the above panel.')

	label = widget_label( row2base, value=' ')
;	toggle = cw_bgroup2( row2base, ['Show First Occurences','ET only with XY change','Show all monitor logs'], /row, ypad=0, $
;					/nonexclusive, /return_index, /tracking, uname='options', set_value=[0,0,1], $
;					uvalue=['Only shows the first of a sequence of the same TAG. (Not implemented yet.)', $
;							'Only show ET events after a change in XY. (Not implemented yet.)','Show all lines of Monitor reports for each monitor record (up to a maximum of 2048 bytes), else just show the first one.'])
	toggle = 0L
	
	if realtime eq 0 then begin
		label = widget_label( row2base, value='  Buffer size:')
		buffer_mode = widget_combobox( row2base, value=['100 Kbytes','200 Kbytes','500 Kbytes','1 Mbytes','2 Mbytes','3 Mbytes','4 Mbytes','6 Mbytes','8 Mbytes','10 Mbytes','12 Mbytes','15 Mbytes','20 Mbytes','30 Mbytes','40 Mbytes','50 Mbytes','60 Mbytes','80 Mbytes','100 Mbytes'], uname='buffer-size', /tracking, $
						uvalue='Number of bytes to read from each segment file. Only the first records, up to this many bytes, will be displayed for each segment. ' + $
						'Use a buffer size of 100 Mbytes to see all records. However, read times will be ~10-20 seconds per segment.')
	
;		label = widget_label( row2base, value=' Skip:')
;		skip_text = widget_text( row2base, value='0', uname='skip-text', /tracking, /editable, $
;					uvalue='Optionally, skip this number of bytes before processing records to display.',scr_xsize=100)
		skip_text = 0L
	endif else begin
		buffer_mode = 0L
		skip_text = 0L
;		button = widget_button( row2base, value='Read', uname='read',/tracking, uvalue='Read another record.')
		button = widget_button( row2base, value='Start', uname='start',/tracking, uvalue='Start adding records to the list from the midasd server.')
		button = widget_button( row2base, value='Stop', uname='stop',/tracking, uvalue='Stop adding records to the list from the midasd server.')
		button = widget_button( row2base, value='Clear', uname='clear',/tracking, uvalue='Clear all records.')
	endelse
		
	label = widget_label( col2base, value='More Record details:')
	detail_List = Widget_List(col2base, UNAME='details-list',  $
			value = labs, /tracking, uvalue = 'Display details of the record selected in the above panel.', $
			scr_xsize=xlist ,scr_ysize=150, font=fnt)

	help = widget_text( tbase, scr_xsize=xlist + (realtime ? 0: 105), ysize=2, /wrap, uname='HELP', /tracking, $
				uvalue='Help window - displays information about widgets. Move cursor over each widget to get help.',frame=0)
	
	state = {	$
			segment_list:	segment_list, $		; segment list ID
			record_list:	record_list, $		; record list ID
			record_title:	record_title, $		; record list title ID
			detail_list:	detail_list, $		; details list ID
			options_id:		toggle, $			; options ID
			option_old:		toggle_old, $		; 'old' file option ID
			dir_text:		dir_text, $			; dir text ID
;			run_text:		run_text, $			; run text ID
			skip_text:		skip_text, $		; skip text ID
			buffer_mode:	buffer_mode, $		; buffer size ID
			tlb:			tlb, $				; TLB ID

			path:			'', $				; working directory
			run:			0L, $				; run number
			old:			0, $				; old format segment files
			record:			0L, $				; record number
			skip:			0L, $				; bytes to skip first
			options:		[0,0,1], $			; options
			realtime:		realtime, $			; realtime socket mode
			deadtime_cal:	dt, $				; deadtime cal factor
			tags:			data_tags, $		; list of tags by index
			data_type:		data_type, $		; data type tag codes
			da_cal:			fltarr(3), $		; DA matrix cal
			cal:			fltarr(3,n_detectors), $	; ptr to detector cals
			el:				strarr(n_elements), $	; ptr to element list
			n_els:			0, $				; number of elements
			pdam:			ptrarr(n_elements,/allocate_heap), $	; ptr to DA matrix
			tag_select:		replicate(1,n_data_tags), $	; tag enables
			psegment:		ptr_new(/allocate_heap), $	; pointer to selected segment file list
			precord:		ptr_new(/allocate_heap), $	; pointer to selected subset of records
			pback:			ptr_new(/allocate_heap), $	; pointers from list line back to selected subset of records
			pdetail:		ptr_new(/allocate_heap), $	; pointer to current details text lines
			p:				ptr_new(/allocate_heap), $	; data area - array of pointers to data
			ps:				ps, $				; pointer to midas socket (if /realtime)
			pfiles:			ptr_new(/allocate_heap), $	; file names list
			pylut:			ptr_new(/allocate_heap), $	; ptr to Y lookup table
			ylut_ok:		0, $				; flags a good LUT
			n_buffer:		100000L, $			; read buffer size (bytes)
			timer_on:		0, $				; realtime record read timer
			time:			0.3, $				; realtime record read interval
			timeout:		timeout, $			; midasd socket read timeout	
			help:			help $				; context help widget ID
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'midas_browse', tlb, /no_block
	return
end
