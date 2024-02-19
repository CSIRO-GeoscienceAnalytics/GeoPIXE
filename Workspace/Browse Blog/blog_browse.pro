
pro blog_browse_event, Event

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
       warning,'blog_browser_event',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   free_record, pseg
       return
    endif
endif

x_mask =	'00007FFF'x			; ET
y_mask =	'3FFF8000'x
tag_mask =	'80000000'x
e_mask =	'0001FFE0'x
t_mask =	'1FFE0000'x
adr_mask =	'0000001F'x
pdf_mask =	'20000000'x
x_offset =		0
y_offset = 		-15
tag_offset = 	-30
e_offset = 		-5
t_offset = 		-17
adr_offset = 	0
pdf_offset2 = 	-29

x_mask2 =	'00007FFF'x			; ET2		Maia 96
y_mask2 =	'3FFF8000'x
tag_mask2 =	'80000000'x
e_mask2 =	'0007FF80'x
t_mask2 =	'1FF80000'x
adr_mask2 =	'0000007F'x
pdf_mask2 =	'20000000'x
x_offset2 =		0
y_offset2 =		-15
tag_offset2 = 	-30
e_offset2 =		-7
t_offset2 =		-19
adr_offset2 = 	0
pdf_offset2 = 	-29

x_mask3 =	'00007FFF'x			; ET3		Maia 384 (temporary)
y_mask3 =	'3FFF8000'x
x_offset3 =		0
y_offset3 =		-15
tag_mask3 =	'80000000'x
t_mask3 =	'7FE00000'x
e_mask3 =	'001FFE00'x
adr_mask3 =	'000001FF'x
tag_offset3 = 	-31
t_offset3 =		-21
e_offset3 =		-9
adr_offset3 = 	0
xy_axis_mask3 =	'18000000'x	
xy_pos_mask3 =	'07FFFFFF'x
xy_axis_offset3 =		-27
xy_pos_offset3 =		0

pa_offset4 =			0					; event_1 (was ET4)		Maia 384
pa_tag_offset4 = 		-27
pa_tag4 =				'E0000000'xul
pa_mask4 =	    		'07FFFFFF'xul
tag_mask4 =				'80000000'xul
adr_mask4 =				'7FC00000'xul
t_mask4 =				'003FF000'xul
e_mask4 =				'00000FFF'xul
tag_offset4 = 			-31
adr_offset4 = 			-22
t_offset4 =				-12
e_offset4 =				0
xyz_tag4 = 				'80000000'xul
xyz_axis_mask4 =		'50000000'xul	
xyz_pos_offset4 =		0

pa_tag_mask =			'E0000000'xul
pa_tag_mask4 =			'F8000000'xul
pa_sign_bit_mask4 =	    '04000000'xul
pa_sign_extend = 		'F8000000'xul
xyz_tag_mask4 =			'E0000000'xul
xyz_tag_pa4 =			'E0000000'xul
pa_tag_tf4 =			'F8000000'xul
tf_tag_mask4 = 			'FE000000'xul
tf_tag_bt4 =			'F8000000'xul
tf_bit_mask4 =			'01FFFFFF'xul

adr_mask9 =		'0000000000000FFF'x			; pm_eterr_1
rr_mask9 =		'0000000FFFFFF000'x
t_mask9 =		'0000FFF000000000'x
e_mask9 =		'0FFF000000000000'x
pdf_mask9 =		'1000000000000000'x
conseq_mask9 =	'2000000000000000'x
adr_offset9 =		0
rr_offset9 =		-12
t_offset9 =			-36
e_offset9 =			-48
pdf_offset9 =		-60
conseq_offset9 =	-61

pa_offset5 =			0					; ET w/ timestamps		DAQ 36
pa_tag_offset5 = 		-27
pa_tag5 =				'E0000000'xul
pa_mask5 =	    		'0FFFFFFF'xul
tag_mask5 =				'80000000'xul
adr_mask5 =				'7F000000'xul
t_mask5 =				'00FFE000'xul
e_mask5 =				'00001FFF'xul
tag_offset5 = 			-31
adr_offset5 = 			-24
t_offset5 =				-13
e_offset5 =				0
xyz_tag5 = 				'80000000'xul
xyz_axis_mask5 =		'50000000'xul	
xyz_pos_offset5 =		0

pa_tag_mask5 =			'F0000000'xul
pa_sign_bit_mask5 =	    '08000000'xul
pa_sign_extend5 = 		'F0000000'xul
xyz_tag_mask5 =			'F0000000'xul
xyz_tag_pa5 =			'F0000000'xul
pa_tag_tf5 =			'F0000000'xul
tf_tag_mask5 = 			'FC000000'xul
tf_tag_bt5 =			'F0000000'xul
tf_bit_mask5 =			'03FFFFFF'xul

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
			if (*pstate).realtime then begin
				timer_on = (*pstate).timer_on
				blog_read_next, (*pstate).ps, (*pstate).p, (*pstate).tags, (*pstate).tag_select, timer_on=timer_on
				(*pstate).timer_on = timer_on
				update_maia_records, pstate
				if (*pstate).timer_on then widget_control, event.id, timer=(*pstate).time
			endif
			return
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

		'blog_browse_TLB': begin
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
			F = file_requester( title='Select a member of the desired "Run"', path=(*pstate).path, $
						/numeric, group=event.top)
			if F ne '' then begin
				(*pstate).run = fix(strip_file_ext(strip_path(F)))
				(*pstate).path = extract_path(F)
;				widget_control, (*pstate).run_text, set_value=string((*pstate).run)
				widget_control, (*pstate).dir_text, set_value=(*pstate).path
				F = find_file2(strip_file_ext(strtrim(F,2))+'.*', /extension_numeric)
				g = strip_path(F)
				q = sort_file_numeric(g,/ext)
				g = g[q]
				*(*pstate).pfiles = F[q]
				widget_control, (*pstate).segment_list, set_value=g
				*(*pstate).psegment = 0
				widget_control, (*pstate).record_list, set_value=''
				widget_control, (*pstate).detail_list, set_value=''
				
				ylut = get_maia_ylut( F[q[0]], error=err)
				if err eq 0 then begin
					*(*pstate).pylut = ylut
					(*pstate).ylut_ok = 1
				endif else (*pstate).ylut_ok = 0
			endif
			end

		'run-text': begin
			widget_control, (*pstate).run_text, get_value=s
			print, 'Datatype for "run" return is - ', size(s, /tname)
			s = str_tidy(s)
			if s ne '' then begin
				(*pstate).run = fix(s)
				F = find_file2((*pstate).path+strip_file_ext(strtrim(s,2))+'.*', /extension_numeric)
				g = strip_path(F)
				q = sort_file_numeric(g,/ext)
				g = g[q]
				*(*pstate).pfiles = F[q]
				widget_control, (*pstate).segment_list, set_value=g
				*(*pstate).psegment = 0
				widget_control, (*pstate).record_list, set_value=''
				widget_control, (*pstate).detail_list, set_value=''
			endif
			end

		'dir': begin
			F = file_requester( /dir, title='Select the working directory', path=(*pstate).path, $
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
			update_maia_records, pstate
			end

		'segment-list': begin
			index = widget_info( (*pstate).segment_list, /list_select)
			*(*pstate).psegment = (*(*pstate).pfiles)[index]
			free_record, (*pstate).p
			widget_control, (*pstate).skip_text, get_value=s
			(*pstate).skip = long(s)
			pseg = read_maia_segments( *(*pstate).psegment, $
							n_buffer=(*pstate).n_buffer, skip=(*pstate).skip)	;, state=state)
			(*pstate).p = ptr_new(pseg, /no_copy)
			
			rates = add_maia_activity( (*pstate).p)								; only uses tag=39
			nd = n_elements(rates)
			if nd gt 1 then begin
				!p.title = 'Average Detector activity counts'
				!x.title = 'Detector #'
				!y.title = 'Frequency'
				!p.charsize = 1.0
				!p.charthick = 1.0
				!p.thick = 1.0
				window,1,xsize=600,ysize=300
				plot,[0,nd-1],[0,1.1*max(rates)],/nodata,xstyle=1
				oplot,rates,psym=10,color=spec_colour('green')
			endif
			
			update_maia_records, pstate
			
			if (*pstate).ylut_ok and (index lt n_elements(*(*pstate).pylut))then begin
				s = ['','Lookup table Y origin for this segment = '+ str_tidy((*(*pstate).pylut)[index])]
				widget_control, (*pstate).detail_list, set_value = s
				*(*pstate).pdetail = s
			endif
			end

		'record-list': begin
			(*pstate).record = (*(*pstate).pback)[event.index]
			update_maia_details, pstate
			end

		'details-list': begin
			pr1 = *(*pstate).p
			q = *(*pstate).precord
			pr = pr1[q]
			n = n_elements(pr)
			i = (*pstate).record
			tag = (*pr[i]).tag
			case tag of 
				26: begin						; monitor
					s0 = *(*pstate).pdetail
					if (event.index ge 4) and (n_elements(s0) gt event.index) then begin
						s = s0[event.index]
						s3 = strsplit(s,' ',/extract)
						if strlen(s3[0]) gt 0 then begin
							maia_IC_name = s3[0]
							val = fltarr(n)
							sep = string([10B,13B])
							k = 0
							for j=0L,n-1 do begin
								if (*pr[j]).tag eq 26 then begin
									pd = pr[j]							
									b = *(*pd).b
									sc = string(b)
									s1 = strsplit(sc,sep,/extract)
									ns1 = n_elements(s1)
									if ns1 ge 1 then begin
										for i=0L,ns1-1 do begin
											s2 = strsplit(s1[i],' ',/extract)
											ns2 = n_elements(s2)
											if (s2[0] eq maia_IC_name) and (ns2 gt 3) then begin
												val[k] = float2(s2[3])
												k = k+1
											endif
										endfor
									endif
								endif
							endfor
							val = val[0:k-1]
							q = where(val ne 0.0, nq)
							if nq gt 2 then begin
								window,1,xsize=600,ysize=350
								bs = min([0.01*max(val),0.1*(max(val)-min(val))])>0.000001*max(val)
								h = histogram(val,binsize=bs,omin=omin,omax=omax)
								n = n_elements(h)
								x = omin + (omax-omin)*findgen(n)/n
								print,'min=',omin,' max=',omax
								dx = (omax-omin)*0.05 > 0.000002*max(val)
								!p.title = 'Selected Monitor PV histogram'
								!x.title = maia_IC_name + ' (s.d. = ' + str_tidy(stddev(val)) + ', ' + str_tidy(100.*stddev(val)/mean(val))+ '%)'
								!p.charsize = 1.0
								!p.charthick = 1.0
								!p.thick = 1.0
								plot,[x[0],x,max(x)+bs],[0,h,0],psym=10, xrange=[omin-bs-dx,omax+bs+dx], xstyle=1, yrange=[0,1.05*max(h)], ystyle=1
							endif
						endif
					endif
					end
				34: begin						; event_1
					s0 = *(*pstate).pdetail
					if (strmid( s0[event.index],0,7) eq 'Time BT') or (strmid( s0[event.index-1],0,7) eq 'Time BT') then begin
						val = fltarr(n)
						k = 0
						for j=0L,n-1 do begin
							if (*pr[j]).tag eq 34 then begin
								pd = pr[j]							
								b = *(*pd).b
								nd = n_elements(b)/4
								d = ulong(b,0,nd)
								swap_bytes, d, /big_endian_data
								q_non_et = where( (d and tag_mask4) ne 0, nq_non_et) 
								if nq_non_et gt 0 then begin
									q = where( (d[q_non_et] and xyz_tag_mask4) eq xyz_tag_pa4, nq_xyz_pa) 
									if nq_xyz_pa gt 0 then begin
										q_pa = q_non_et[q]
										q = where( (d[q_pa] and pa_tag_mask4) eq pa_tag_tf4, nq_pa_tf) 
										if nq_pa_tf gt 0 then begin
											q_pa_tf = q_pa[q]
											q = where( (d[q_pa_tf] and tf_tag_mask4) eq tf_tag_bt4, nq_tf_bt) 
											if nq_tf_bt gt 0 then begin
												q_tf_bt = q_pa_tf[q]
												val[k] = float(d[q_tf_bt] and tf_bit_mask4) / 10000.
												k = k+1
											endif
										endif
									endif
								endif
							endif
						endfor
						val2 = val[0:k-1]
						q1 = where( val2 lt 200., nq1)
						if nq1 lt 2 then goto, finish
						q = qsample( val2[q1], nq, veto_low=1., veto_high=1., /non_zero, /positive)
						val = val2[q1[q]]
						if nq gt 2 then begin
							window,1,xsize=600,ysize=350
							bs = min([0.01*max(val),0.1*(max(val)-min(val))])>0.000001*max(val)
							h = histogram(val,binsize=bs,omin=omin,omax=omax)
							n = n_elements(h)
							x = omin + (omax-omin)*findgen(n)/n
							print,'min=',omin,' max=',omax
							dx = (omax-omin)*0.05 > 0.000002*max(val)
							!p.title = 'Dwell time (ms)'
							!x.title = 'Dwell time (ms)' + ' (s.d. = ' + str_tidy(stddev(val)) + ', ' + str_tidy(100.*stddev(val)/mean(val))+ '%)'
							!p.charsize = 1.0
							!p.charthick = 1.0
							!p.thick = 1.0
							plot,[x[0],x,max(x)+bs],[0,h,0],psym=10, xrange=[omin-bs-dx,omax+bs+dx], xstyle=1, yrange=[0,1.05*max(h)], ystyle=1
						endif
					endif
					end
				else:
			endcase
			end

		'clear': begin
			free_record, (*pstate).p
			(*pstate).p = ptr_new(/alloc)
			update_maia_records, pstate
			end
			
		'read': begin
			blog_read_next, (*pstate).ps, (*pstate).p, (*pstate).tags, (*pstate).tag_select
			update_maia_records, pstate
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
				print,'new:',select
				print,'old:',(*pstate).tag_select
				old = strjoin( str_tidy(select), ' ')
				new = strjoin( str_tidy((*pstate).tag_select), ' ')
;				warning,'blog_browse',['Old:',old,'New:',new]

				(*pstate).tag_select = select
				update_maia_records, pstate
			endif
			end

		'options': begin
			(*pstate).options[event.value] = event.select
			update_maia_records, pstate
			end

		'buffer-size': begin
			mega_byte = 1024LL * 1024LL
			sizes = [500LL*1024LL, [1LL, 2LL, 3LL, 4LL, 6LL, 8LL, 10LL, 12LL, 15LL, 20LL, 30LL, 40LL, 50LL, 60LL, 80LL, 101LL]*mega_byte]
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
	free_record, (*pstate).p				; full ptr to ptr array of record data
	ptr_free, (*pstate).precord				; the subset index list or records to display
	ptr_free, (*pstate).pback				; back index list
	ptr_free, (*pstate).pfiles				; list of blog files
	ptr_free, (*pstate).psegment			; selected blog segment file
	if (*pstate).realtime then close_file, (*(*pstate).ps).unit

	widget_control, event.top, /destroy
	file_delete, (*pstate).temp_file
	return
end

;-----------------------------------------------------------------

pro update_maia_records, pstate

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
       warning,'update_maia_records',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pdj
       return
    endif
endif

@maia_listmode.def

;xlast = -1000
;ylast = -1000
n_bytes = 64 * 1024L
k = 0

	s = ''
	widget_control, (*pstate).record_list, set_value = s
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
	temp_file = (*pstate).temp_file
	openw, 2, temp_file
	j = 0
	back = 0L

	for i=0L,n-1 do begin
		if i ge 23654 then begin
;			print,'debug'
		endif
	    if (*pr[i]).length eq 0 then begin
	    	case (*pr[i]).tag of
				3: begin										; newseg
					b = strip_path((*pr[i]).file)
					sc = strtrim(strcompress(string(b)),2)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sc, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "File= ",A)'
	        		back = [back,i]
					end

				else: begin
					tags = [(*pstate).tags, '??']
					ntgs = n_elements(tags)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, tags[(*pr[i]).tag < (ntgs-1)], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	    		     		format='(I7,I4,2x, A7, I7, I11, I11)'
	        		back = [back,i]
					end
	    	endcase
	    endif else begin

			case (*pr[i]).tag of
				1: begin										; identity
					b = (*(*pr[i]).b)[20:*]
					q = where((b lt 32) or (b gt 127),nq)
					if nq gt 0 then b[q]=32B
					sc = strtrim(strcompress(string(b)),2)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sc, format='(I7,I4,1x, A8, I7, I11, I11, 2x, A)'
	        		back = [back,i]
					end
				5: begin										; summary
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					rt = ulong(b,8,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nbyt = ulong(b,32,1)
					swap_bytes, nbyt, /big_endian_data
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Run-time=",I10,4x,"Total bytes=",'
					if nbyt gt 1000000. then begin
						nbyt = float(nbyt)/1000000.
						sformat = sformat + 'F10.2,1x,"Mbytes")'
					endif else sformat = sformat + 'I11)'
					tbytes = str_tidy(nbyt)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				runtime, tbytes, format=sformat
	        		back = [back,i]
					end
				6: begin										; comment
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					sc = strsplit(string(b), string([10B,13B]), /extract)
					sc = sc[0]
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sc, format='(I7,I4,2x, A7, I7, I11, I11, 2x, A)'
	        		back = [back,i]
					end
				8: begin										; ET data
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)

					if (*pr[i]).length gt 4 then begin
						b = (*(*pr[i]).b)[4:min([n_bytes,(*pr[i]).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						t = d and tag_mask
						bad = where( t ne 0, nbad)
						de = ishft( d and e_mask, e_offset)
						dt = ishft( d and t_mask, t_offset)
						adr = ishft( d and adr_mask, adr_offset)
						no = min([10,nd])
						out = intarr(2,no)
						out[1,*] = adr[0:no-1]
						out[0,*] = de[0:no-1]
						if nbad gt 0 then begin
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"bad tag bit(s) set")'
						endif else begin
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				sx,sy, out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"E(n)=",10(I5,"(",I3,")"))'
						endelse
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"no ET data in payload")'
					endelse
					back = [back,i]
					end
				9: begin										; XY data
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "stage encoder x=",A,1x,"y=",A)'
	        		back = [back,i]
					end
				10: begin										; PA data
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "pixel x=",A,1x,"y=",A)'
	        		back = [back,i]
					end
				12: begin										; DA detector cals
					b = (*(*pr[i]).b)[0:((*pr[i]).length < 20)-1]
					d = float(b,0,3,n_elements(b)/12)
					swap_bytes, d, /big_endian_data
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				d, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "Cals: ",5(G11.4))'
	        		back = [back,i]
	        		
					filej = (*pr[i]).file
					ptrj = (*pr[i]).ptr
					pdj = read_maia_segments( filej, select=ptrj)
					if ptr_valid(pdj[0]) then begin
						(*pdj).index = (*pr[i]).index
						(*pdj).file = (*pr[i]).file
						(*pdj).ptr = (*pr[i]).ptr
						b = (*(*pdj).b)[0:(*pdj).length-1]
						d = float(b,0,3,n_elements(b)/12)
						swap_bytes, d, /big_endian_data
						(*pstate).cal[*,0:n_elements(d[0,*])-1] = d
					endif
					end
				14: begin										; DA matrix row
					b = (*(*pr[i]).b)[0:3]
					nr = ulong(b,0,1)
					swap_bytes, nr, /big_endian_data
					b = (*(*pr[i]).b)[4:((*pr[i]).length < 16)-1]
					d = float(b,0,n_elements(b)/4)
					swap_bytes, d, /big_endian_data
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				nr,d, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "DA row ",I,": ",4(G11.4))'
	        		back = [back,i]
	        		
					filej = (*pr[i]).file
					ptrj = (*pr[i]).ptr
					pdj = read_maia_segments( filej, select=ptrj)
					if ptr_valid(pdj[0]) then begin
						(*pdj).index = (*pr[i]).index
						(*pdj).file = (*pr[i]).file
						(*pdj).ptr = (*pr[i]).ptr
						b = (*(*pdj).b)[4:(*pdj).length-1]
						d = float(b,0,n_elements(b)/4-1)
						swap_bytes, d, /big_endian_data
						*(*pstate).pdam[nr[0]] = d
					endif
					end
				15: begin										; DA pixel
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					net = min([(*pr[i]).length/4 - 1,8])
					d = float((*(*pr[i]).b)[4:*],0,net)
					swap_bytes, d, /big_endian_data
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy,d, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "DA event for x = ",A,", y = ",A,": ",8(G11.4))'
	        		back = [back,i]
					end
				16: begin										; DA init file1
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					sc = string(b)
					sep = string([10B,13B])
					s2 = strsplit(sc,sep,/extract)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				s2, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "DA: Init File1: ",A)'
	        		back = [back,i]
					end
				17: begin										; DA element
					b = (*(*pr[i]).b)[0:1]
					d = uint(b,0,1)
					swap_bytes, d, /big_endian_data
					if (*pr[i]).length gt 2 then begin
						s = str_tidy( (*(*pr[i]).b)[2:*])
					endif else s=''
					(*pstate).el[d] = s
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				d,s, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "element ",I," = ",A)'
	        		back = [back,i]
					end
				18: begin										; DA params1
					b = (*(*pr[i]).b)[0:11]
					d = fix(b,0,6)
					swap_bytes, d, /big_endian_data
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				d, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "DA pars = ",6I5)'
	        		back = [back,i]
	        		(*pstate).n_els = d[1]
					end
				20: begin										; DA DA cal
					b = (*(*pr[i]).b)[0:11]
					d = float(b,0,3)
					swap_bytes, d, /big_endian_data
					(*pstate).da_cal = d
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				d, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "DA Cal: ",3(G11.4))'
	        		back = [back,i]
					end
				21: begin										; DA Throttle
					net = 20
					b = (*(*pr[i]).b)[0:net-1]
					d = uint(b,0,net/2)
					swap_bytes, d, /big_endian_data
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				d, format='(I7,I4,2x, A7, I7, I11, I11, 2x, 10I5)'
	        		back = [back,i]
					end
				22: begin										; Dali enable
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					bits = bit_split(d)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				reverse(bits), format='(I7,I4,2x, A7, I7, I11, I11, 1x, 4(8I2,2x))'
	        		back = [back,i]
					end
				25: begin										; ET2 data
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask2)
					y = fix(ishft( d[0] and y_mask2, y_offset2))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)

					if (*pr[i]).length gt 4 then begin
						b = (*(*pr[i]).b)[4:min([n_bytes,(*pr[i]).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						t = d and tag_mask2
						bad = where( t ne 0, nbad)
						de = ishft( d and e_mask2, e_offset2)
						dt = ishft( d and t_mask2, t_offset2)
						adr = ishft( d and adr_mask2, adr_offset2)
						no = min([10,nd])
						out = intarr(2,no)
						out[1,*] = adr[0:no-1]
						out[0,*] = de[0:no-1]
						if nbad gt 0 then begin
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"bad tag bit(s) set")'
						endif else begin
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				sx,sy, out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"E(n)=",10(I5,"(",I3,")"))'
						endelse
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"no ET data in payload")'
					endelse
					back = [back,i]
					end
				26: begin										; monitor
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					s2 = strsplit(string(b), string([10B,13B]), /extract)
					s3 = (*pstate).options[2] ? s2 : s2[0]
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				s3, format='(I7,I4,2x, A7, I7, I11, I11, 2x, (T52,A))'
	        		back = [back,replicate(i,n_elements(s3))]
					end
				27: begin										; pm_eterr_1 records
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					nd = n_elements(b)/8
					d = ulong64(b,0,nd)
					swap_bytes, d, /big_endian_data
					adr = ishft( d and adr_mask9, adr_offset9)
					dt = ishft( d and t_mask9, t_offset9)
					de = ishft( d and e_mask9, e_offset9)
					pdf = ishft( d and pdf_mask9, pdf_offset9)
					rr = ishft( d and rr_mask9, rr_offset9)
					conseq = ishft( d and conseq_mask9, conseq_offset9)
					net = 5 < min([n_bytes/8,nd])
					out = intarr(2,net)
					out[0,*] = de[0:net-1]
					out[1,*] = adr[0:net-1]
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        			out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "E(n)=",5(I5,"(",I3,")"))'
	        		back = [back,i]
					end
				28: begin										; id2
					b = (*(*pr[i]).b)[20:min([n_bytes,(*pr[i]).length])-1]
					q = where((b lt 32) or (b gt 127),nq)
					if nq gt 0 then b[q]=32B
					sc = strtrim(strcompress(string(b)),2)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sc, format='(I7,I4,2x, A7, I7, I11, I11, 2x, A)'
	        		back = [back,i]
					end
				30: begin										; rexec info
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					b1 = byte(b,0,4)
					scan_type = b1[0]
					scan_major = b1[1]
					r1 = ulong(b,4,2)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					r2 = float(b,12,8)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					xpixel = r2[2]
					ypixel = r2[3]
					rvel = r2[4]
					dwell = r2[5]
					overrun = r2[6]
					tot_time = r2[7]					
					sformat = '(I7,I4,2x, A7, I7, I11, I11, 2x, "Image size=",2I6,4x,"Velocity=",G10.3," mm/s")'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				nxpixels,nypixels, rvel, format=sformat
	        		back = [back,i]
					end
				31: begin										; ET3 data
					b = (*(*pr[i]).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask3)
					y = fix(ishft( d[0] and y_mask3, y_offset3))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)

					if (*pr[i]).length gt 4 then begin
						b = (*(*pr[i]).b)[4:min([n_bytes,(*pr[i]).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						t = d and tag_mask3
						good = where( t eq 0, ngood)
						if ngood gt 0 then begin
							de = ishft( d[good] and e_mask3, e_offset3)
							dt = ishft( d[good] and t_mask3, t_offset3)
							adr = ishft( d[good] and adr_mask3, adr_offset3)
							no = min([10,ngood])
							out = intarr(2,no)
							out[1,*] = adr[0:no-1]
							out[0,*] = de[0:no-1]
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
								sx,sy, out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"E(n)=",10(I5,"(",I3,")"))'
						endif else begin
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
								sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"no E(T) found.")'
						endelse
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "x=",A,1x,"y=",A,2x,"no ET data in payload")'
					endelse
					back = [back,i]
					end
				32: begin										; summary2
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					rt = ulong(b,12,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nbyt = ulong64(b,28,1)
					swap_bytes, nbyt, /big_endian_data
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Run-time=",I10,4x,"Total bytes=",'
					if nbyt gt 1000000. then begin
						nbyt = float(nbyt)/1000000.
						sformat = sformat + 'F10.2,1x,"Mbytes")'
					endif else sformat = sformat + 'I11)'
					tbytes = str_tidy(nbyt)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				runtime, tbytes, format=sformat
	        		back = [back,i]
					end
				33: begin										; setgroup
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					group = string(b[0:*])
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Group = ",A)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				group, format=sformat
	        		back = [back,i]
					end
				34: begin										; event_1 (was ET4) data
					sx = '?'
					sy = '?'
					sz = '?'
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					nd = n_elements(b)/4
					d = ulong(b,0,nd)
					swap_bytes, d, /big_endian_data
					dx = d[0] and pa_mask4
					dy = d[1] and pa_mask4
					dz = d[2] and pa_mask4
					t = d and pa_sign_bit_mask4
					if t[0] ne 0 then dx = dx or pa_sign_extend
					if t[1] ne 0 then dy = dy or pa_sign_extend
					if t[2] ne 0 then dz = dz or pa_sign_extend
					x = long(dx)
					y = long(dy)
					z = long(dz)
					sx = str_tidy( x)
					sy = str_tidy( y)
					sz = str_tidy( z)
					dtime = 0.0
					q_non_et = where( (d and tag_mask4) ne 0, nq_non_et) 
					if nq_non_et gt 0 then begin
						q = where( (d[q_non_et] and xyz_tag_mask4) eq xyz_tag_pa4, nq_xyz_pa) 
						if nq_xyz_pa gt 0 then begin
							q_pa = q_non_et[q]
							q = where( (d[q_pa] and pa_tag_mask4) eq pa_tag_tf4, nq_pa_tf) 
							if nq_pa_tf gt 0 then begin
								q_pa_tf = q_pa[q]
								q = where( (d[q_pa_tf] and tf_tag_mask4) eq tf_tag_bt4, nq_tf_bt) 
								if nq_tf_bt gt 0 then begin
									q_tf_bt = q_pa_tf[q]
									dtime = float(d[q_tf_bt] and tf_bit_mask4) / 10000.
								endif
							endif
						endif
					endif
					q1 = where( d and pa_tag_mask ne pa_tag4, nq1)

					if ((*pr[i]).length gt 12) and (nq1 eq 0) then begin
						b = (*(*pr[i]).b)[12:min([n_bytes,(*pr[i]).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
												
						t = d and tag_mask4
						good = where( t eq 0, ngood)
						if ngood gt 0 then begin
							de = ishft( d[good] and e_mask4, e_offset4)
							dt = ishft( d[good] and t_mask4, t_offset4)
							adr = ishft( d[good] and adr_mask4, adr_offset4)
							no = min([10,ngood])
							out = intarr(2,no)
							out[1,*] = adr[0:no-1]
							out[0,*] = de[0:no-1]
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
								sx,sy,sz, str_tidy(dtime), out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "0=",A,1x,"1=",A,1x,"2=",A,1x,A,2x,"E(n)=",10(I5,"(",I3,")"))'
						endif else begin
							printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
								sx,sy,sz, str_tidy(dtime), format='(I7,I4,2x, A7, I7, I11, I11, 2x, "0=",A,1x,"1=",A,1x,"2=",A,1x,A,2x,"no E(T) found.")'
						endelse
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sx,sy,sz, str_tidy(dtime), format='(I7,I4,2x, A7, I7, I11, I11, 2x, "0=",A,1x,"1=",A,1x,"2=",A,1x,A,2x,"no ET data in payload")'
					endelse
					back = [back,i]
					end
				35: begin										; DA accum
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "XY=",2I7,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				(*pr[i]).sub_header.X, (*pr[i]).sub_header.Y, float((*pr[i]).sub_header.duration)*1.e-4, (*pr[i]).sub_header.error, format=sformat
	        		back = [back,i]
					end
				37: begin										; DT accum
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "XY=",2I7,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				(*pr[i]).sub_header.X, (*pr[i]).sub_header.Y, float((*pr[i]).sub_header.duration)*1.e-4, (*pr[i]).sub_header.error, format=sformat
	        		back = [back,i]
					end
				39: begin										; activity accum
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				(*pr[i]).sub_header.trigger, float((*pr[i]).sub_header.duration)*1.e-4, (*pr[i]).sub_header.error, format=sformat
	        		back = [back,i]
					end
				40: begin										; energy spectrum accum
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				(*pr[i]).sub_header.trigger, float((*pr[i]).sub_header.duration)*1.e-4, (*pr[i]).sub_header.error, format=sformat
	        		back = [back,i]
					end
				42: begin										; scan info 1
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					scan_num = ulong(b,0,1)
					swap_bytes, scan_num, /big_endian_data
					scan_mda = ulong(b,4,1)
					swap_bytes, scan_mda, /big_endian_data
					b1 = byte(b,8,4)
					scan_order = b1[0]
					r1 = ulong(b,12,3)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					nzpixels = r1[2]
					r2 = float(b,24,7)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					zorigin = r2[2]
					xpixel = r2[3]
					ypixel = r2[4]
					zpixel = r2[5]
					dwell = r2[6]
					sinfo = string(b[52:*])
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Image size=",2I6,4x,"Dwell=",G10.3," ms")'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				nxpixels,nypixels, dwell*1000., format=sformat
	        		back = [back,i]
					end
				43: begin										; time spectrum accum
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				(*pr[i]).sub_header.trigger, float((*pr[i]).sub_header.duration)*1.e-4, (*pr[i]).sub_header.error, format=sformat
	        		back = [back,i]
					end
				44: begin										; DA info
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					n_da_elements = ulong(b,0,1)
					swap_bytes, n_da_elements, /big_endian_data
					n_active = ulong(b,4,1)
					swap_bytes, n_active, /big_endian_data
					sformat = '(I7,I4,2x, A7, I7, I11, I11, 2x, "# DA elements=",I3,", # active=",I3)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				n_da_elements, n_active, format=sformat
	        		back = [back,i]
					end
				45: begin										; Var list
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					sinfo = string(b)
					str = strsplit(sinfo,string([0B,11B,13B]),/extract)
					ns = n_elements(str)
					sformat = '(I7,I4,2x, A7, I7, I11, I11, 2x, "Var list=",A)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				str[0], format=sformat
	        		back = [back,i]
					end
				46: begin										; Var values
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					sinfo = string(b)
					str = strsplit(sinfo,string([10B,13B]),/extract)
					q = where( (strmid(str,0,1) ne '#') and (str ne ''), nq)
					if nq gt 0 then begin
						sformat = '(I7,I4,2x, A7, I7, I11, I11, 2x, "Var values= ",A)'
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				strmid(str[q[0]],0,100), format=sformat
					endif else begin
						sformat = '(I7,I4,2x, A7, I7, I11, I11, 2x, "Var values (none)")'
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				format=sformat
					endelse
	        		back = [back,i]
					end
				47: begin										; scan info 2 (need to detect units later ...)
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					scan_num = ulong(b,0,1)
					swap_bytes, scan_num, /big_endian_data
					scan_mda = ulong(b,4,1)
					swap_bytes, scan_mda, /big_endian_data
					b1 = byte(b,8,4)
					scan_order = b1[0]
					r1 = ulong(b,12,3)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					nzpixels = r1[2]
					r2 = float(b,24,7)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					zorigin = r2[2]
					xpixel = r2[3]
					ypixel = r2[4]
					zpixel = r2[5]
					dwell = r2[6]
					sinfo = string(b[52:*])
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Image size=",3I6,4x,"Dwell=",G10.3," ms")'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				nxpixels,nypixels,nzpixels, dwell*1000., format=sformat
	        		back = [back,i]
					end
				48: begin										; DAQ ET with timestamps
					sdX = '?'
					sdY = '?'
					ssX = '?'
					ssY = '?'
					ssZ = '?'
					ssA = '?'
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					nd = n_elements(b)/8
					d = ulong(b,0,2,nd)
					swap_bytes, d, /big_endian_data
					ddx = d[0,0] and pa_mask5
					ddy = d[0,1] and pa_mask5
					dsx = d[0,2] and pa_mask5
					dsy = d[0,3] and pa_mask5
					dsz = d[0,4] and pa_mask5
;					dsa = d[0,5] and pa_mask5
					t = d[0,*] and pa_sign_bit_mask5
					if t[0] ne 0 then ddx = ddx or pa_sign_extend5
					if t[1] ne 0 then ddy = ddy or pa_sign_extend5
					if t[2] ne 0 then dsx = dsx or pa_sign_extend5
					if t[3] ne 0 then dsy = dsy or pa_sign_extend5
					if t[4] ne 0 then dsz = dsz or pa_sign_extend5
;					if t[5] ne 0 then dsa = dsa or pa_sign_extend5
					dx = long(ddx)
					dy = long(ddy)
					sx = long(dsx)
					sy = long(dsy)
					sz = long(dsz)
;					sa = long(dsa)
					sdX = str_tidy( dx)
					sdY = str_tidy( dy)
					ssX = str_tidy( sx)
					ssY = str_tidy( sy)
					ssZ = str_tidy( sz)
;					ssA = str_tidy( sa)
					dtime = 0.0
					q_non_et = where( (d[0,*] and tag_mask5) ne 0, nq_non_et) 
					if nq_non_et gt 0 then begin
						q = where( (d[0,q_non_et] and xyz_tag_mask5) eq xyz_tag_pa5, nq_xyz_pa) 
						if nq_xyz_pa gt 0 then begin
							q_pa = q_non_et[q]
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_tf5, nq_pa_tf) 
							if nq_pa_tf gt 0 then begin
								q_pa_tf = q_pa[q]
								q = where( (d[0,q_pa_tf] and tf_tag_mask5) eq tf_tag_bt5, nq_tf_bt) 
								if nq_tf_bt gt 0 then begin
									q_tf_bt = q_pa_tf[q]
									dtime = float(d[0,q_tf_bt] and tf_bit_mask5) / 10000.
								endif
							endif
						endif
					endif
					q = where( (d[0,*] and tag_mask5) eq 0, nq_et) 

					if (nq_et gt 0) then begin
						de = ishft( d[0,q] and e_mask5, e_offset5)
						dt = ishft( d[0,q] and t_mask5, t_offset5)
						adr = ishft( d[0,q] and adr_mask5, adr_offset5)
						no = min([10,nq_et])
						out = intarr(2,no)
						out[1,*] = adr[0:no-1]
						out[0,*] = de[0:no-1]
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
							sdX,sdY,ssX,ssY, out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "dX=",A,1x,"dY=",A,1x,"sX=",A,1x,"sY=",A,2x,"E(n)=",10(I5,"(",I3,")"))'
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sdX,sdY,ssX,ssY, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "dX=",A,1x,"dY=",A,1x,"sX=",A,1x,"sY=",A,2x,"no ET data in payload")'
					endelse
					back = [back,i]
					end
				49: begin										; DAQ ET with NO timestamps
					sdX = '?'
					sdY = '?'
					ssX = '?'
					ssY = '?'
					ssZ = '?'
					ssA = '?'
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					nd = n_elements(b)/4
					d = ulong(b,0,1,nd)
					swap_bytes, d, /big_endian_data
					ddx = d[0,0] and pa_mask5
					ddy = d[0,1] and pa_mask5
					dsx = d[0,2] and pa_mask5
					dsy = d[0,3] and pa_mask5
					dsz = d[0,4] and pa_mask5
					dsa = d[0,5] and pa_mask5
					t = d[0,*] and pa_sign_bit_mask5
					if t[0] ne 0 then ddx = ddx or pa_sign_extend5
					if t[1] ne 0 then ddy = ddy or pa_sign_extend5
					if t[2] ne 0 then dsx = dsx or pa_sign_extend5
					if t[3] ne 0 then dsy = dsy or pa_sign_extend5
					if t[4] ne 0 then dsz = dsz or pa_sign_extend5
					if t[5] ne 0 then dsa = dsa or pa_sign_extend5
					dx = long(ddx)
					dy = long(ddy)
					sx = long(dsx)
					sy = long(dsy)
					sz = long(dsz)
					sa = long(dsa)
					sdX = str_tidy( dx)
					sdY = str_tidy( dy)
					ssX = str_tidy( sx)
					ssY = str_tidy( sy)
					ssZ = str_tidy( sz)
					ssA = str_tidy( sa)
					dtime = 0.0
					q_non_et = where( (d[0,*] and tag_mask5) ne 0, nq_non_et) 
					if nq_non_et gt 0 then begin
						q = where( (d[0,q_non_et] and xyz_tag_mask5) eq xyz_tag_pa5, nq_xyz_pa) 
						if nq_xyz_pa gt 0 then begin
							q_pa = q_non_et[q]
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_tf5, nq_pa_tf) 
							if nq_pa_tf gt 0 then begin
								q_pa_tf = q_pa[q]
								q = where( (d[0,q_pa_tf] and tf_tag_mask5) eq tf_tag_bt5, nq_tf_bt) 
								if nq_tf_bt gt 0 then begin
									q_tf_bt = q_pa_tf[q]
									dtime = float(d[0,q_tf_bt] and tf_bit_mask5) / 10000.
								endif
							endif
						endif
					endif
					q = where( (d[0,*] and tag_mask5) eq 0, nq_et) 

					if (nq_et gt 0) then begin
						de = ishft( d[0,q] and e_mask5, e_offset5)
						dt = ishft( d[0,q] and t_mask5, t_offset5)
						adr = ishft( d[0,q] and adr_mask5, adr_offset5)
						no = min([10,nq_et])
						out = intarr(2,no)
						out[1,*] = adr[0:no-1]
						out[0,*] = de[0:no-1]
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
							sdX,sdY,ssX,ssY, out, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "dX=",A,1x,"dY=",A,1x,"sX=",A,1x,"sY=",A,2x,"E(n)=",10(I5,"(",I3,")"))'
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				sdX,sdY,ssX,ssY, format='(I7,I4,2x, A7, I7, I11, I11, 2x, "dX=",A,1x,"dY=",A,1x,"sX=",A,1x,"sY=",A,2x,"no ET data in payload")'
					endelse
					back = [back,i]
					end
				50: begin										; DAQ activity
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					nd = n_elements(b)/4
					d = ulong(b,0,nd)
					swap_bytes, d, /big_endian_data
					n_det = d[0]
					rate = d[1:n_det]
					time = float(d[n_det+1]) * 1.0e-7
					flux = d[n_det+2]
					tot = d[n_det+3]
				 
					sformat = '(I7,I4,2x, A7, I7, I11, I11, 2x, "Duration=",G," (s)",4x,"Flux=",I,4x,"Total=",I)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				time, flux, tot, format=sformat
	        		back = [back,i]
					end
				52: begin										; client action
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					uint4 = ulong(b,0,4)
					swap_bytes, uint4, /big_endian_data
					serial = uint4[0]
					action = uint4[1]
					number = uint4[2]
					tot = uint4[3]
					b2 = b[16:*]
					q = where( b2 eq 0B, nq)
					if nq gt 0 then b2[q]=byte('|')
					str = strsplit( string(b2), '|', /extract)
					ns = n_elements(str)
					if ns ge 3 then begin
						sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, A, 2x,"IP:",A,"  port=",A)'
						printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
		        				str[2], str[0], str[1], format=sformat
		        		back = [back,i]
					endif
					end
				53: begin										; summary3
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					rt = ulong(b,12,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nbyt = ulong64(b,28,1)
					swap_bytes, nbyt, /big_endian_data
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Run-time=",I10,4x,"Total bytes=",'
					if nbyt gt 1000000. then begin
						nbyt = float(nbyt)/1000000.
						sformat = sformat + 'F10.2,1x,"Mbytes")'
					endif else sformat = sformat + 'I11)'
					tbytes = str_tidy(nbyt)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				runtime, tbytes, format=sformat
	        		back = [back,i]
					end
				54: begin										; client name
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Client name: ",A)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				string(b), format=sformat
	        		back = [back,i]
					end
				55: begin										; Metadata
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					sinfo = string(b)
					str = strsplit(sinfo,string([10B,13B]),/extract)
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Metadata= ",A)'
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	        				str[0], format=sformat
	        		back = [back,i]
					end
				56: begin           					        ; summary4
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					rt = ulong(b,12,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nbyt = ulong64(b,28,1)
					swap_bytes, nbyt, /big_endian_data
					sformat = '(I7,I4,1x, A8, I7, I11, I11, 2x, "Run-time=",I10,4x,"Total bytes=",'
					if nbyt gt 1000000. then begin
						nbyt = float(nbyt)/1000000.
						sformat = sformat + 'F10.2,1x,"Mbytes")'
					endif else sformat = sformat + 'I11)'
					tbytes = str_tidy(nbyt)
					printf, 2, (*pr[i]).index, (*pr[i]).tag, (*pstate).tags[(*pr[i]).tag], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
							runtime, tbytes, format=sformat
					back = [back,i]
					end

				else: begin
					tags = [(*pstate).tags, '??']
					ntgs = n_elements(tags)
					if (*pr[i]).length eq 0 then begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, tags[(*pr[i]).tag < (ntgs-1)], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	         				format='(I7,I4,2x, A7, I7, I11, I11, 1x, "No payload")'
					endif else begin
						printf, 2, (*pr[i]).index, (*pr[i]).tag, tags[(*pr[i]).tag < (ntgs-1)], (*pr[i]).length, (*pr[i]).seq, (*pr[i]).tseq, $
	         				(*(*pr[i]).b)[0:min([15,min([n_bytes,(*pr[i]).length])-1])], format='(I7,I4,2x, A7, I7, I11, I11, 1x, 16(4Z3,3x))'
					endelse
	        		back = [back,i]
					end
			endcase
		endelse
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

pro update_maia_details, pstate

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
       warning,'update_maia_details',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pd
       free_record, pdj
       return
    endif
endif

@maia_listmode.def

;xlast = -1000
;ylast = -1000
n_bytes = 64 * 1024L		; 100000L

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

	pd = read_maia_segments( file, select=ptr)	;, state=state)
	if ptr_valid(pd[0]) eq 0 then return
	(*pd).index = (*pr[i]).index
	(*pd).file = (*pr[i]).file
	(*pd).ptr = (*pr[i]).ptr

more:
	close, 2
	temp_file = (*pstate).temp_file
	openw, 2, temp_file
	in = (*pd).tv_sec
    d = in/86400	&	in = in mod 86400
    h = in/3600		&	in = in mod 3600
    m = in/60		&	in = in mod 60
    s = in
    day = str_tidy(d)+' '+str_tidy(h)+':'+str_tidy(m)+':'+str_tidy(s)

    tags = [(*pstate).tags, '??']
    ntgs = n_elements(tags)
	printf, 2, (*pd).tag, (*pd).seq, day, strip_path((*pd).file), $
	        				format='(3x,"index",T15,"tag:",1x, I3, T33,"Seq:",I10, T51,"Time:",A, T76,"Segment File: ",A)'
	printf, 2, (*pd).index, tags[(*pd).tag < (ntgs-1)], (*pd).tseq, (*pd).tv_us, (*pd).ptr, $
	        				format='(I8,T11,A14, T30,"TagSeq:",I10, T51,"(us):",I10, T76,"File ptr: ",I10)'
	printf, 2, ''
	n = 3

;	This list in read_maia, get_maia_details too ... also effects list of tags in main 'blog_browse' routine
;
;	1     id                  identity'
;	2     newrun              new run'
;	3     newseg              new segment file'
;	4     tod                 time of day'
;	5     summary             activity summary'
;	6     comment             comment strings'
;	7     sendnext            request next block'
;	8     et                  ET Maia block'
;	9     xy                  XY position Maia block'
;	10    PA                  PA pixel advance Maia block'
;	11    da_put              DA pile-up init table Maia block'
;	12    da_cal              DA cal init coefficients Maia block'
;	13    da_cal2             DA cal init table Maia block'
;	14    da_mat              DA matrix Maia block'
;	15    maia_da_pixel_1     DA pixel record - pixel ppm-uC contributions'
;	16	  maia_da_init_file_1 DA initialization file name
;	17	  maia_da_element_1	  DA initialization element string
;	18    maia_da_params_1    DA parameters'
;	19    maia_da_matrix_raw_1  DA raw matrix'
;	20    maia_da_cal_1       DA calibration'
;	21    maia_da_throttle_1  Throttle table factors'
;	22    maia_da_enable_1    Dali enable status bits'
;	23    sendprev            client stuff'
;	24    sendprevornext      "  '
;	25    et2                 ET 96 element ET block'
;	26    monitor             Epics stuff'
;	27	  pm_eterr_1		  Wollongong boards ...'
;	28	  id2				  revised ID'
;	29	  endrun			  end of run'
;	30	  maia_rexec_1		  MIRO rexec parameterz
;	31	  et3				  ET 384 Maia block'
;	32	  summary_2			  summary, version 2'
;	33	  setgroup			  set the data storage group dir tree for blog'
;	34	  event_1			  (was ET4) ET 384 Maia block'
;	35	  da_accum			  DA accumulator
;	36	  ROI_accum			  ROI accumulator
;	37	  DT_accum			  Deadtime accumulator
;	38	  DTpp_accum		  DT per pixel accumulator
;	39	  activity_accum	  Activity accumulator
;	40	  E_spectrum		  energy spectrum accumulator
;	41	  ET2D_accum		  ET 2D accumulator
;	42	  maia_scaninfo_1	  new scan information, replaces rexec_1
;	43	  T_spectrum		  time spectrum accumulator
;	44	  maia_da_info_1	  DA/DT info block
;	45	  var_list_1		  Common library var list
;	46	  var_value_1		  library var values
;	47	  maia_scaninfo_2	  Maia ScanInfo 2
;	48	  pm_event_ts_1		  DAQ ET w/ TS
;	49	  pm_event_nots_1	  DAQ ET no TS
;	50	  pm_activity_1		  DAQ Activity
;	51	  setproject		  set next run project string
;	52	  client			  client connect/disconnect log
;	53	  summary_3			  summary, version 3
;	54	  name				  name used (set by client) to identify a client
;	55	  metadata			  metadata records (key/value pairs, separated by newlines \n)
;	56    summary 4			  summary, version 4
;	57    report   			  detailed report summary

 	    if (*pd).length eq 0 then begin
	    	case (*pd).tag of
				3: begin										; newseg
					b = strip_path((*pd).file)
					sc = strtrim(strcompress(string(b)),2)
					printf, 2, sc, format='( "new segment: File= ",A)'
					end

				else: begin
			         printf, 2, 'No Payload (length = 0)'
					end
	    	endcase
	    endif else begin

			case (*pd).tag of
				1: begin										; identity
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					v = long(b,0,1)
					swap_bytes, v, /big_endian_data
					version = str_tidy(v)
					r = ulong(b,4,1)
					swap_bytes, r, /big_endian_data
					run = str_tidy(r)
					s = ulong(b,8,1)
					swap_bytes, s, /big_endian_data
					segment = str_tidy(s)
					c = ulong(b,16,1)
					swap_bytes, c, /big_endian_data
					ctime = str_tidy(c)
					b2 = b[20:*]
					q = where(b2 eq 0B,nq)
					if nq gt 0 then b2[q]=127B
					str = strsplit(string(b2),string(127B),/extract,/preserve_null)
					timezone = str[0]
					reference = str[1]
					experiment = str[2]
					equipment = str[3]
					location = str[4]
					personnel = str[5]
					printf, 2, equipment, timezone, experiment, format='(T2,"Equipment: ",A,T31,"Timezone: ",A,T72," Experiment: ",A)'
					printf, 2, location, reference, personnel, format='(T3,"Location: ",A,T30,"Reference: ",A,T73," Personnel: ",A)'
					printf, 2, version, run, segment, ctime, format='(T4,"Version= ",A,T29,"Run= ",A,T47,"segment= ",A,T78,"ctime= ",A)'
					n = n+2
					end
				5: begin										; summary
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					ru = ulong(b,0,1)
					swap_bytes, ru, /big_endian_data
					run = str_tidy(ru)
					s = ulong(b,4,1)
					swap_bytes, s, /big_endian_data
					segment = str_tidy(s)
					
					rt = ulong(b,8,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					
					nc = ulong(b,12,1)
					swap_bytes, nc, /big_endian_data
					clients = str_tidy(nc)

					nct = ulong(b,16,1)
					swap_bytes, nct, /big_endian_data
					tclients = str_tidy(nct)
					nbt = ulong(b,20,1)
					swap_bytes, nbt, /big_endian_data
					tblocks = str_tidy(nbt)
					r = double(b,24,1)
					swap_bytes, r, /big_endian_data
					rate = str_tidy(r)
					nbyt = (ulong(b,32,1))[0]
					swap_bytes, nbyt, /big_endian_data
					use_mb = 0
					if nbyt gt 1000000L then begin
						nbyt = float(nbyt)/1000000.
						use_mb = 1
					endif
					tbytes = str_tidy(nbyt)
					rb = double(b,36,1)
					swap_bytes, rb, /big_endian_data
					brate = str_tidy(rb)

					printf, 2, run, tblocks, rate, format='(T8,"Run: ",I6,T29,"Total blocks: ",I10,T66,"Rate: ",F12.3)'
					if use_mb then begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",F10.2,T54,"Mbytes",T66,"Rate: ",F12.3)'
					endif else begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",I10,T66,"Rate: ",F12.3)'
					endelse
					printf, 2, runtime, clients, tclients, format='(T3,"Run time: ",I10,T34,"Clients: ",I10,T65,"Total: ",I10)'
					n = n+2
					end
				6: begin										; comment
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					sc = string(b)
					sep = string([10B,13B])
					s2 = strsplit(sc,sep,/extract)
					printf, 2, s2, format='(A)'
					if n_elements(s2) gt 1 then n = n+n_elements(s2)-1
					end
				8: begin										; ET data
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					printf, 2, sx, sy, format='("ET type 1 event; pixel address: X = ",A,", Y = ",A)'

					if (*pd).length gt 4 then begin
						b = (*(*pd).b)[4:min([n_bytes,(*pd).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						t = d and tag_mask
						bad = where( t ne 0, nbad)
						de = ishft( d and e_mask, e_offset)
						dt = ishft( d and t_mask, t_offset)
						adr = ishft( d and adr_mask, adr_offset)
						net = 480 < min([n_bytes/4,nd])
						out = intarr(3,net)
						out[0,*] = de[0:net-1]
						out[1,*] = dt[0:net-1]
						out[2,*] = adr[0:net-1]
						printf, 2, ''
						printf, 2, format='("Energy, Time data  E / T (n) (where E=pulse-height, T=time-over-threshold, n=detector #):")'
						n = n+2
						if nbad gt 0 then begin
							printf, 2, format='("bad tag bit(s) set")'
							n = n+1
						endif else begin
							printf, 2, out, format='(6(I5,"/",I5,"(",I3,")",1x))'
							n = n + (1 > net/6)
						endelse
					endif else begin
						printf, 2, format='("no ET data in payload")'
						n = n+1
					endelse
					end
				9: begin										; XY data
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					printf, 2, sx, sy, format='("stage encoder advance to X = ",A,", Y = ",A)'
					end
				10: begin										; PA data
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					printf, 2, sx, sy, format='("pixel advance to X = ",A,", Y = ",A)'
					end
				12: begin										; DA cal1
					b = (*(*pd).b)
					d = float(b,0,3,n_elements(b)/12)
					swap_bytes, d, /big_endian_data
					net = n_elements(d[0,*])
					printf, 2, 'DA: Cal 1 coefficients'
					for j=0L,net-1 do begin
						printf, 2, d[*,j], format='(3(G11.4))'
					endfor
					n = n+net					
					end
				14: begin										; DA table
					b1 = (*(*pd).b)[0:3]
					d1 = ulong(b1,0,1)
					swap_bytes, d1, /big_endian_data
					b2 = (*(*pd).b)[4:*]
					d2 = float(b2,0,n_elements(b2)/4)
					swap_bytes, d2, /big_endian_data
					net = n_elements(d2)
					printf, 2, 'DA: DA table for element ', d1
					printf, 2, d2, format='(10(G11.4))'
					n = n+ceil(float(net)/10)
					end
				15: begin										; DA pixel
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask)
					y = fix(ishft( d[0] and y_mask, -15))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					net = (*pd).length/4 - 1
					d = float((*(*pd).b)[4:*],0,net)
					swap_bytes, d, /big_endian_data
					printf, 2, sx, sy, format='("DA event for X = ",A,", Y = ",A)'
					printf, 2, d, format='(10(G11.4))'
					n = n+ceil(float(net)/10)
					
					if (*pstate).n_els lt 1 then goto, cont
					sum = fltarr((*pstate).n_els)
					warn = ''
					j = i-1
					if (j ge 0) and (j lt n_elements(pr)) then begin
						different = 0
						repeat begin
							pdj = read_maia_segments( (*pr[j]).file, select=(*pr[j]).ptr)		;, state=state)
							if ptr_valid(pdj[0]) eq 0 then return
							print,'Try record j = ',j,'  tag = ',(*pdj).tag
							if (*pdj).tag eq 15 then different=1
							
							if (*pdj).tag eq 25 then begin
								bj = (*(*pdj).b)[0:3]
								dj = ulong(bj,0,1)
								swap_bytes, dj, /big_endian_data
								xj = fix(dj[0] and x_mask)
								yj = fix(ishft( dj[0] and y_mask, -15))
								if xj ge 16*1024 then xj=xj-32*1024
								if yj ge 16*1024 then yj=yj-32*1024
								if (xj ne x) or (yj ne y) then different=1					; correct
								if not different and ( (*pdj).length gt 4) then begin
									b = (*(*pdj).b)[4:(*pdj).length-1]
									nd = n_elements(b)/4
									d = ulong(b,0,nd)
									swap_bytes, d, /big_endian_data
									de = ishft( d and e_mask2, e_offset2)
									dt = ishft( d and t_mask2, t_offset2)
									adr = ishft( d and adr_mask2, adr_offset2)
									e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr] 
									col = (e - (*pstate).da_cal[0]) / (*pstate).da_cal[1] 
									q = where((col gt 0) and (col lt n_elements(*(*pstate).pdam[0])), nq)
									if nq gt 0 then begin
										for k=0L,(*pstate).n_els-1 do begin								
											sum[k] = sum[k] + total( (*(*pstate).pdam[k])[col[q]], /NaN )
										endfor
									endif
								endif
							endif
							j = j-1
							if j lt 0 then begin
								different = 1
								warn = 'Some ET2 records may be missed past start of segment.'
							endif
						endrep until different
					endif
					j = i+1
					if (j ge 0) and (j lt n_elements(pr)) then begin
						different = 0
						repeat begin
							pdj = read_maia_segments( (*pr[j]).file, select=(*pr[j]).ptr)		;, state=state)
							if ptr_valid(pdj[0]) eq 0 then return
							print,'Try record j = ',j,'  tag = ',(*pdj).tag
							if (*pdj).tag eq 15 then different=1
							
							if (*pdj).tag eq 25 then begin
								bj = (*(*pdj).b)[0:3]
								dj = ulong(bj,0,1)
								swap_bytes, dj, /big_endian_data
								xj = fix(dj[0] and x_mask)
								yj = fix(ishft( dj[0] and y_mask, -15))
								if xj ge 16*1024 then xj=xj-32*1024
								if yj ge 16*1024 then yj=yj-32*1024
								if (xj ne x) or (yj ne y) then different=1					; correct
								if not different and ( (*pdj).length gt 4) then begin
									b = (*(*pdj).b)[4:(*pdj).length-1]
									nd = n_elements(b)/4
									d = ulong(b,0,nd)
									swap_bytes, d, /big_endian_data
									de = ishft( d and e_mask2, e_offset2)
									dt = ishft( d and t_mask2, t_offset2)
									adr = ishft( d and adr_mask2, adr_offset2)
									e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr] 
									col = (e - (*pstate).da_cal[0]) / (*pstate).da_cal[1] 
									q = where((col gt 0) and (col lt n_elements(*(*pstate).pdam[0])), nq)
									if nq gt 0 then begin
										for k=0L,(*pstate).n_els-1 do begin								
											sum[k] = sum[k] + total( (*(*pstate).pdam[k])[col[q]], /NaN )
										endfor
									endif
								endif
							endif
							j = j+1
							if j ge n_elements(pr) then begin
								different = 1
								warn = 'Some ET2 records may be missed past end of segment.'
							endif
						endrep until different
					endif
					printf, 2
					printf, 2, 'DA sum for ET2 events in this pixel:'
					printf, 2, sum, format='(10(G11.4))'
					if strlen(warn) gt 0 then begin
						printf, 2, 'Warning: ', warn
						n = n+1
					endif
					n = n + 2 + ceil(float((*pstate).n_els)/10)
					end
				16: begin										; DA init file1
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					sc = string(b)
					sep = string([10B,13B])
					s2 = strsplit(sc,sep,/extract)
					printf, 2, s2, format='("DA: Init File1 = ",A)'
					if n_elements(s2) gt 1 then n = n+n_elements(s2)-1
					end
				17: begin										; DA element1
					b1 = (*(*pd).b)[0:1]
					d = fix(b1,0,1)
					swap_bytes, d, /big_endian_data
					sc = ''
					if (*pd).length gt 2 then begin
						b2 = (*(*pd).b)[2:min([n_bytes,(*pd).length])-1]
						sc = string(b2)
					endif
					printf, 2, d, sc, format='("DA: Element1 = ",I,3x,A)'
					n = n+1
					end
				18: begin										; DA params1
					b = (*(*pd).b)[0:11]
					d = fix(b,0,6)
					swap_bytes, d, /big_endian_data
					net = 1
					printf, 2, 'DA: Parameters'
					printf, 2, '#elements active #detectors E-bits  T-bits DA-bits'
					printf, 2, d, format='(6(I8))'
					n = n+3					
					end
				20: begin										; DA DA cal
					b = (*(*pd).b)[0:11]
					d = float(b,0,3)
					swap_bytes, d, /big_endian_data
					net = 1
					printf, 2, 'DA: DA Cal coefficients'
					printf, 2, d, format='(3(G11.4))'
					n = n+2					
					end
				21: begin										; DA throttle vector
					net = min([n_bytes,(*pd).length])
					b = (*(*pd).b)[0:net-1]
					d = fix(b,0,net/2)
					swap_bytes, d, /big_endian_data
					printf, 2, format='("Throttle spectrum:")'
					printf, 2, d, format='(20(I5))'
					n = n+ceil(float(net)/20)
					end
				22: begin										; Dali enable bits
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					b = bit_split(d)
					bits = str_tidy(indgen(32))
					printf, 2, format='("Dali Enable bits:")'
					printf, 2, reverse(bits), format='(2x,32A3)'
					printf, 2, reverse(b), format='(2x,32(I3))'
					n = n+2
					end
				25: begin										; ET2 data
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask2)
					y = fix(ishft( d[0] and y_mask2, y_offset2))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)

					if (*pd).length gt 4 then begin
						b = (*(*pd).b)[4:min([n_bytes,(*pd).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						t = d and tag_mask2
						bad = where( t ne 0, nbad)
						de = ishft( d and e_mask2, e_offset2)
						dt = ishft( d and t_mask2, t_offset2)
						adr = ishft( d and adr_mask2, adr_offset2)
						pdf = ishft( d and pdf_mask2, pdf_offset2)
						qpdf = where(pdf ne 0, n_pdf)
						e = de
						q1 = where(e gt 0, nq1)
						q2 = where(e le 0, nq2)
						!p.charsize = 1.0
						!p.charthick = 1.0
						!p.thick = 1.0
						if nq1 gt 0 then begin
							!p.title = 'E - T plot for this record'
							!x.title = 'E (Channel Number)'
							!y.title = 'T (Time over Threshold)'
							if (*pstate).n_els ge 1 then begin
								e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr]
								!x.title = 'E (energy [keV])'
							endif
							window,0,xsize=700,ysize=400
							plot,e[q1],dt[q1],psym=3
						endif
						!p.title = 'Detector activity - for this record'
						!x.title = 'Detector #'
						!y.title = 'Frequency'
						window,1,xsize=400,ysize=300
						plot,[0,95],[0,max(adr)],/nodata
						if nq2 gt 0 then oplot,histogram(adr[q2],binsize=1,min=0,max=95),psym=10,color=spec_colour('red')
						if nq1 gt 0 then oplot,histogram(adr[q1],binsize=1,min=0,max=95),psym=10,color=spec_colour('green')
						printf, 2, sx, sy, n_pdf, format='("ET type 2 event; pixel address: X = ",A,", Y = ",A,",  # pdf bits set =",I5)'
						net = 640 < min([n_bytes/4,nd])
						out = intarr(3,net)
						out[0,*] = de[0:net-1]
						out[1,*] = dt[0:net-1]
						out[2,*] = adr[0:net-1]
						printf, 2, ''
						printf, 2, format='("Energy, Time data:  E / T (n)   (where E=pulse-height, T=time-over-threshold, n=detector #):")'
						n = n+2
						if nbad gt 0 then begin
							printf, 2, format='("bad tag bit(s) set")'
							n = n+1
						endif else begin
							printf, 2, out, format='(8(I5,"/",I5,"(",I3,")",1x))'
							n = n + (1 > net/8)
						endelse
					endif else begin
						printf, 2, format='("no ET data in payload")'
						n = n+1
					endelse
					end
				26: begin										; monitor
					printf, 2, 'Time stamp = ', (*pd).tv_sec,' (sec), ',(*pd).tv_us,' (usec)'
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					sc = string(b)
					sep = string([10B,13B])
					s2 = strsplit(sc,sep,/extract)
					printf, 2, s2, format='(A)'
					if n_elements(s2) gt 1 then n = n+n_elements(s2)-1
					n = n+1
					end
				27: begin										; pm_eterr_1 records
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					nd = n_elements(b)/8
					d = ulong64(b,0,nd)
					swap_bytes, d, /big_endian_data
					adr = ishft( d and adr_mask9, adr_offset9)
					dt = ishft( d and t_mask9, t_offset9)
					de = ishft( d and e_mask9, e_offset9)
					pdf = ishft( d and pdf_mask9, pdf_offset9)
					rr = ishft( d and rr_mask9, rr_offset9)
					conseq = ishft( d and conseq_mask9, conseq_offset9)
					qpdf = where(pdf ne 0, n_pdf)
					printf, 2, n_pdf, format='("pm_eterr_1 event:  # pdf bits set =",I5)'
					net = 500 < min([n_bytes/8,nd])
					out = ulonarr(4,net)
					out[0,*] = de[0:net-1]
					out[1,*] = dt[0:net-1]
					out[2,*] = adr[0:net-1]
					out[3,*] = rr[0:net-1]
					printf, 2, ''
					printf, 2, format='("Energy, Time data:  E/T (adr;RR)  (where E=energy, T=time, adr=address, RR=read request):")'
					n = n+2
					printf, 2, out, format='(5(I5,"/",I5," (",I4,";",I8,")",2x))'
					n = n + (1 > net/5)
					end
				28: begin										; id2
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					v = long(b,0,1)
					swap_bytes, v, /big_endian_data
					version = str_tidy(v)
					r = ulong(b,4,1)
					swap_bytes, r, /big_endian_data
					run = str_tidy(r)
					s = ulong(b,8,1)
					swap_bytes, s, /big_endian_data
					segment = str_tidy(s)
					c = ulong(b,16,1)
					swap_bytes, c, /big_endian_data
					ctime = str_tidy(c)
					b2 = b[20:*]
					q = where(b2 eq 0B,nq)
					if nq gt 0 then b2[q]=127B
					q = where(b2 lt 31B,nq)
					if nq gt 0 then b2[q]=32B
					str = strsplit(string(b2),string(127B),/extract,/preserve_null)
					timezone = str[0]
					blogd_revision = str[1]
					logger_hostname = str[2]
					facility = str[3]
					blogd_working = str[4]
					blogd_path = str[5]
					printf, 2, facility, timezone, format='(T2,"Facility: ",A,T36,"Timezone: ",A)'
					printf, 2, logger_hostname, format='(T29,"Logger hostname: ",A)'
					printf, 2, blogd_working, blogd_revision, blogd_path, format='(T2,"Location: ",A,T36,"Revision: ",A,T69,"Data Group: ",A)'
					printf, 2, version, run, segment, ctime, format='(T3,"Version= ",A,T29,"Run= ",A,T47,"segment= ",A,T74,"ctime= ",A)'
					n = n+4
					end
				30: begin										; rexec1 info
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					b1 = byte(b,0,4)
					scan_type = b1[0]
					scan_major = b1[1]
					r1 = ulong(b,4,2)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					r2 = float(b,12,8)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					xpixel = r2[2]
					ypixel = r2[3]
					rvel = r2[4]
					dwell = r2[5]
					overrun = r2[6]
					tot_time = ulong(r2[7],0,1)					

					printf, 2, float(nxpixels)*xpixel,float(nypixels)*ypixel, nxpixels,nypixels, rvel, $
								format='(T8,"Image size:",2F9.3,1x,"(mm)",4x,"(",I6,"  x",I6," pixels)",4x,"Velocity:",G10.3," (mm/s)")'
					printf, 2, xorigin,yorigin, xpixel*1000.,ypixel*1000., format='(T12,"Origin:",2F9.2,1x,"(mm)",3x,"Pixel pitch:",F8.2,"  x",F8.2," (microns/pixel)")'
					printf, 2, dwell*1000., overrun, format='(T8,"Dwell time:",F9.2,1x,"(ms/pixel)",10x,"Overrun: ",F8.3,1x,"(mm)")'
					printf, 2, tot_time,float(tot_time)/60.,float(tot_time)/3600., format='(T3,"Total scan time: ",I10,1x,"(s)",1x,"(",F6.1,1x,"minutes",1x,F6.3,1x,"hours)")'
					n = n+4
					end
				31: begin										; ET3 data
					b = (*(*pd).b)[0:3]
					d = ulong(b,0,1)
					swap_bytes, d, /big_endian_data
					x = fix(d[0] and x_mask3)
					y = fix(ishft( d[0] and y_mask3, y_offset3))
					if x ge 16*1024 then x=x-32*1024
					if y ge 16*1024 then y=y-32*1024
					sx = str_tidy( x)
					sy = str_tidy( y)
					printf, 2, sx, sy, format='("ET type 3 event; pixel address: X = ",A,", Y = ",A)'

					if (*pd).length gt 4 then begin
						b = (*(*pd).b)[4:min([n_bytes,(*pd).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						t = d and tag_mask3
						xy_tag = where( t ne 0, n_xy)
						if n_xy gt 0 then begin
							daxis = ishft( d[xy_tag] and xy_axis_mask3, xy_axis_offset3)
							dpos = ishft( d[xy_tag] and xy_pos_mask3, xy_pos_offset3)
							net = 640 < min([n_xy,nd])
							out = intarr(2,net)
							out[0,*] = daxis[0:net-1]
							out[1,*] = dpos[0:net-1]
							printf, 2, ''
							printf, 2, format='("Encoder values: Value (axis):")'
							n = n+2
							printf, 2, out, format='(8(I5,"(",I3,")",1x))'
							n = n + (1 > net/8)						
						endif
						et_tag = where( t eq 0, n_et)
						if n_et gt 0 then begin
							de = ishft( d[et_tag] and e_mask3, e_offset3)
							dt = ishft( d[et_tag] and t_mask3, t_offset3)
							adr = ishft( d[et_tag] and adr_mask3, adr_offset3)
							e = de
							!p.title = 'E - T plot for this record'
							!x.title = 'E (Channel Number)'
							!y.title = 'T (Time over Threshold)'
							!p.charsize = 1.0
							!p.charthick = 1.0
							!p.thick = 1.0
							if (*pstate).n_els ge 1 then begin
								e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr]
								!x.title = 'E (energy [keV])'
							endif
							q1 = where(e gt 0, nq1)
							q2 = where(e le 0, nq2)
							if nq1 gt 0 then begin
								window,0,xsize=700,ysize=400
								plot,e[q1],dt[q1],psym=3
								!p.title = 'Detector activity - for this record'
								!x.title = 'Detector #'
								!y.title = 'Frequency'
							endif
							window,1,xsize=600,ysize=300
							plot,[0,383],[0,max(adr)],/nodata
							if nq2 gt 0 then oplot,histogram(adr[q2],binsize=1,min=0,max=383),psym=10,color=spec_colour('red')
							if nq1 gt 0 then oplot,histogram(adr[q1],binsize=1,min=0,max=383),psym=10,color=spec_colour('green')
							net = 640 < min([n_et,nd])
							out = intarr(3,net)
							out[0,*] = de[0:net-1]
							out[1,*] = dt[0:net-1]
							out[2,*] = adr[0:net-1]
							printf, 2, ''
							printf, 2, format='("Energy, Time data:  E / T (n)   (where E=pulse-height, T=time-over-threshold, n=detector #):")'
							n = n+2
							printf, 2, out, format='(8(I5,"/",I5,"(",I3,")",1x))'
							n = n + (1 > net/8)
						endif
					endif else begin
						printf, 2, format='("no ET data in payload")'
						n = n+1
					endelse
					end
				32: begin										; summary2
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					ru = ulong(b,0,1)
					swap_bytes, ru, /big_endian_data
					run = str_tidy(ru)
					s = ulong(b,4,1)
					swap_bytes, s, /big_endian_data
					segment = str_tidy(s)
					
					di = ulong(b,8,1)
					swap_bytes, di, /big_endian_data
					discard = str_tidy(di)
					
					rt = ulong(b,12,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nc = ulong(b,16,1)
					swap_bytes, nc, /big_endian_data
					clients = str_tidy(nc)
					nct = ulong(b,20,1)
					swap_bytes, nct, /big_endian_data
					tclients = str_tidy(nct)
					nbt = ulong(b,24,1)
					swap_bytes, nbt, /big_endian_data
					tblocks = str_tidy(nbt)

					nbyt = ulong64(b,28,1)
					swap_bytes, nbyt, /big_endian_data
					use_mb = 0
					if nbyt gt 1000000L then begin
						nbyt = float(nbyt)/1000000.
						use_mb = 1
					endif
					tbytes = str_tidy(nbyt)
					
					r = ulong(b,36,1)
					swap_bytes, r, /big_endian_data
					rate = str_tidy(r)
					rb = ulong(b,40,1)
					swap_bytes, rb, /big_endian_data
					brate = str_tidy(rb)
					
					group = string(b[44:*])

					printf, 2, run, tblocks, rate, group, format='(T8,"Run: ",I6,T29,"Total blocks: ",I10,T66,"Rate: ",F12.3,T92,"Group: ",A)'
					if use_mb then begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",F10.2,T54,"Mbytes",T66,"Rate: ",F12.3)'
					endif else begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",I10,T66,"Rate: ",F12.3)'
					endelse
					printf, 2, runtime, clients, tclients, format='(T3,"Run time: ",I10,T34,"Clients: ",I10,T65,"Total: ",I10)'
					n = n+2
					end
				33: begin										; setgroup
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					group = string(b[0:*])

					printf, 2, group, format='(T8,"Group: ",A)'
					n = n+1
					end
				34: begin										; event_1 (was ET4) data
					if ((*pd).length gt 0) then begin
						b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
						nd = n_elements(b)/4
						d = ulong(b,0,nd)
						swap_bytes, d, /big_endian_data
						q_non_et = where( (d and tag_mask4) ne 0, nq_non_et) 
						if nq_non_et gt 0 then begin
							print,'Non-ET:', nq_non_et
							for ij=0,nq_non_et-1 do print, bit_split(d[q_non_et[ij]],/reverse)
							q = where( (d[q_non_et] and xyz_tag_mask4) eq xyz_tag_x4, nq_xyz_x) 
							if nq_xyz_x gt 0 then begin
								q_xyz_x = q_non_et[q]
								dpos = d[q_xyz_x] and xyz_pos_mask4
								net = 640 < min([nq_xyz_x,nd])
								out = intarr(net)
								out[*] = dpos[0:net-1]
								printf, 2, format='("Encoder 0 values:")'
								n = n+1
								printf, 2, out, format='(10(I8,1x))'
								n = n + (1 > ceil(float(net)/10.))						
							endif
							q = where( (d[q_non_et] and xyz_tag_mask4) eq xyz_tag_y4, nq_xyz_y) 
							if nq_xyz_y gt 0 then begin
								q_xyz_y = q_non_et[q]
								dpos = d[q_xyz_y] and xyz_pos_mask4
								net = 640 < min([nq_xyz_y,nd])
								out = intarr(net)
								out[*] = dpos[0:net-1]
								printf, 2, format='("Encoder 1 values:")'
								n = n+1
								printf, 2, out, format='(10(I8,1x))'
								n = n + (1 > ceil(float(net)/10.))						
							endif
							q = where( (d[q_non_et] and xyz_tag_mask4) eq xyz_tag_z4, nq_xyz_z) 
							if nq_xyz_z gt 0 then begin
								q_xyz_z = q_non_et[q]
								dpos = d[q_xyz_z] and xyz_pos_mask4
								net = 640 < min([nq_xyz_z,nd])
								out = intarr(net)
								out[*] = dpos[0:net-1]
								printf, 2, format='("Encoder 2 values:")'
								n = n+1
								printf, 2, out, format='(10(I8,1x))'
								n = n + (1 > ceil(float(net)/10.))						
							endif
							
							q = where( (d[q_non_et] and xyz_tag_mask4) eq xyz_tag_pa4, nq_xyz_pa) 
							if nq_xyz_pa gt 0 then begin
								q_pa = q_non_et[q]
								sx = '?'
								sy = '?'
								sz = '?'
								strange_PA = 0
								ssx = '?'
								ssy = '?'
								ssz = '?'
								q = where( (d[q_pa] and pa_tag_mask4) eq pa_tag_x4, nq_pa_x) 
								if nq_pa_x gt 0 then begin
									q_pa_x = q_pa[q]
									if q_pa_x ne 0 then strange_PA=1
									dx = d[q_pa_x] and pa_mask4
									if (d[q_pa_x] and pa_sign_bit_mask4) ne 0 then dx = dx or pa_sign_extend
									x = long(dx)
									sx = str_tidy( x)
								endif
								q = where( (d[q_pa] and pa_tag_mask4) eq pa_tag_y4, nq_pa_y) 
								if nq_pa_y gt 0 then begin
									q_pa_y = q_pa[q]
									if q_pa_y ne 1 then strange_PA=1
									dy = d[q_pa_y] and pa_mask4
									if (d[q_pa_y] and pa_sign_bit_mask4) ne 0 then dy = dy or pa_sign_extend
									y = long(dy)
									sy = str_tidy( y)
								endif
								q = where( (d[q_pa] and pa_tag_mask4) eq pa_tag_z4, nq_pa_z) 
								if nq_pa_z gt 0 then begin
									q_pa_z = q_pa[q]
									if q_pa_z ne 2 then strange_PA=1
									dz = d[q_pa_z] and pa_mask4
									if (d[q_pa_z] and pa_sign_bit_mask4) ne 0 then dz = dz or pa_sign_extend
									z = long(dz)
									sz = str_tidy( z)
								endif
								s = string( [q_pa_x,q_pa_y,q_pa_z])
								s = strjoin(str_tidy(s),',')
								printf, 2, sx, sy, sz, s, format='("Pixel address: 0 = ",A,", 1 = ",A,", 2 = ",A,T76,"[",A,"]")'
								n = n+1
								if strange_PA then begin
									if n_elements(q_pa_x) gt 0 then ssx = str_tidy( q_pa_x)
									if n_elements(q_pa_y) gt 0 then ssy = str_tidy( q_pa_y)
									if n_elements(q_pa_z) gt 0 then ssz = str_tidy( q_pa_z)
									printf, 2, ssx, ssy, ssz, format='(20x,"Strange pixel address positions: 0 = ",A,", 1 = ",A,", 2 = ",A)'
									n = n+1
								endif

								q = where( (d[q_pa] and pa_tag_mask4) eq pa_tag_tf4, nq_pa_tf) 
								if nq_pa_tf gt 0 then begin
									q_pa_tf = q_pa[q]
									q = where( (d[q_pa_tf] and tf_tag_mask4) eq tf_tag_bt4, nq_tf_bt) 
									if nq_tf_bt gt 0 then begin
										q_tf_bt = q_pa_tf[q]
										dtime = double(d[q_tf_bt] and tf_bit_mask4) / 10000.
										net = 640 < min([nq_tf_bt,nd])
										out = dblarr(net)
										out[*] = dtime[0:net-1]
										s = string( [q_tf_bt[0:net-1]])
										s = strjoin(str_tidy(s),',')
										printf, 2, s, format='("Time BT values (ms):",T76,"[",A,"]")'
										n = n+1
										printf, 2, out, format='(6(F12.4,1x))'
										n = n + (1 > ceil(float(net)/6.))						
									endif
									q = where( (d[q_pa_tf] and tf_tag_mask4) eq tf_tag_fc1, nq_tf_fc1) 
									if nq_tf_fc1 gt 0 then begin
										q_tf_fc1 = q_pa_tf[q]
										dtime = d[q_tf_fc1] and tf_bit_mask4
										net = 640 < min([nq_tf_fc1,nd])
										out = lonarr(net)
										out[*] = dtime[0:net-1]
										s = string( [q_tf_fc1[0:net-1]])
										s = strjoin(str_tidy(s),',')
										printf, 2, s, format='("FC0 values (counts):",T76,"[",A,"]")'
										n = n+1
										printf, 2, out, format='(6(I12,1x))'
										n = n + (1 > ceil(float(net)/6.))						
									endif
									q = where( (d[q_pa_tf] and tf_tag_mask4) eq tf_tag_fc2, nq_tf_fc2) 
									if nq_tf_fc2 gt 0 then begin
										q_tf_fc2 = q_pa_tf[q]
										dtime = d[q_tf_fc2] and tf_bit_mask4
										net = 640 < min([nq_tf_fc2,nd])
										out = lonarr(net)
										out[*] = dtime[0:net-1]
										s = string( [q_tf_fc2[0:net-1]])
										s = strjoin(str_tidy(s),',')
										printf, 2, s, format='("FC1 values (counts):",T76,"[",A,"]")'
										n = n+1
										printf, 2, out, format='(6(I12,1x))'
										n = n + (1 > ceil(float(net)/6.))						
									endif

									q = where( (d[q_pa_tf] and tf_tag_mask4) eq tf_tag_mk4, nq_tf_mk) 
									if nq_tf_mk gt 0 then begin
										q_tf_mk = q_pa_tf[q]
										q = where( (d[q_tf_mk] and mk_tag_mask4) eq mk_tag_mk0, nq_tf_mk0) 
										if nq_tf_mk0 gt 0 then begin
											q_tf_mk0 = q_tf_mk[q]
											enc = d[q_tf_mk0] and mk_bit_mask4
											net = 640 < min([nq_tf_mk0,nd])
											out = lonarr(net)
											out[*] = enc[0:net-1]
											s = string( [q_tf_mk0[0:net-1]])
											s = strjoin(str_tidy(s),',')
											printf, 2, s, format='("MK0 values:",T76,"[",A,"]")'
											n = n+1
											printf, 2, out, format='(6(I12,1x))'
											n = n + (1 > ceil(float(net)/6.))
										endif
										q = where( (d[q_tf_mk] and mk_tag_mask4) eq mk_tag_mk1, nq_tf_mk1) 
										if nq_tf_mk1 gt 0 then begin
											q_tf_mk1 = q_tf_mk[q]
											enc = d[q_tf_mk1] and mk_bit_mask4
											net = 640 < min([nq_tf_mk1,nd])
											out = lonarr(net)
											out[*] = enc[0:net-1]
											s = string( [q_tf_mk1[0:net-1]])
											s = strjoin(str_tidy(s),',')
											printf, 2, s, format='("MK1 values:",T76,"[",A,"]")'
											n = n+1
											printf, 2, out, format='(6(I12,1x))'
											n = n + (1 > ceil(float(net)/6.))
										endif
										q = where( (d[q_tf_mk] and mk_tag_mask4) eq mk_tag_mk2, nq_tf_mk2) 
										if nq_tf_mk2 gt 0 then begin
											q_tf_mk2 = q_tf_mk[q]
											enc = d[q_tf_mk2] and mk_bit_mask4
											net = 640 < min([nq_tf_mk2,nd])
											out = lonarr(net)
											out[*] = enc[0:net-1]
											s = string( [q_tf_mk2[0:net-1]])
											s = strjoin(str_tidy(s),',')
											printf, 2, s, format='("MK2 values:",T76,"[",A,"]")'
											n = n+1
											printf, 2, out, format='(6(I12,1x))'
											n = n + (1 > ceil(float(net)/6.))
										endif
									endif
								endif
							endif
						endif
						
						et_tag = where( (d and tag_mask4) eq 0, n_et)
						if n_et gt 0 then begin
							de = ishft( d[et_tag] and e_mask4, e_offset4)
							dt = ishft( d[et_tag] and t_mask4, t_offset4)
							adr = ishft( d[et_tag] and adr_mask4, adr_offset4)
							e = de
							!p.title = 'E - T plot for this record'
							!x.title = 'E (Channel Number)'
							!y.title = 'T (Time over Threshold)'
							!p.charsize = 1.0
							!p.charthick = 1.0
							!p.thick = 1.0
							if (*pstate).n_els ge 1 then begin
								e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr]
								!x.title = 'E (energy [keV])'
							endif
							q1 = where(e gt 0, nq1)
							q2 = where(e le 0, nq2)
							if nq1 gt 0 then begin
								window,0,xsize=700,ysize=400
								plot,e[q1],dt[q1],psym=3
							endif
							!p.title = 'Detector activity - for this record'
							!x.title = 'Detector #'
							!y.title = 'Frequency'
							window,1,xsize=600,ysize=300
							h1 = 0
							h2 = 0
							if nq2 gt 0 then h2 = histogram(adr[q2],binsize=1,min=0,max=383)
							if nq1 gt 0 then h1 = histogram(adr[q1],binsize=1,min=0,max=383)
							plot,[0,383],[0,1.2*max([h1,h2])],/nodata, xstyle=1
							if nq2 gt 0 then oplot,h2,psym=10,color=spec_colour('red')
							if nq1 gt 0 then oplot,h1,psym=10,color=spec_colour('green')
							xyouts,0.6,0.80,/norm,'Events with E=0',color=spec_colour('red')
							xyouts,0.6,0.86,/norm,'Events with E>0',color=spec_colour('green')
							net = (65536/4) < min([n_et,nd])
							out = intarr(3,net)
							out[0,*] = de[0:net-1]
							out[1,*] = dt[0:net-1]
							out[2,*] = adr[0:net-1]
							s = string( [et_tag[0]])
							s = str_tidy(s)
							printf, 2, ''
							printf, 2, s, format='("Energy, Time data:  E / T (n)   (where E=pulse-height, T=time-over-threshold, n=detector #):",T95,"[",A,", ...]")'
							n = n+2
							printf, 2, out, format='(8(I5,"/",I5,"(",I3,")",1x))'
							n = n + (1 > ceil(float(net)/8.))
						endif else begin
							printf, 2, format='("no ET data in payload")'
							n = n+1
						endelse
					endif
					end
				35: begin										; DA accum
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					DTpp = ulong(b,0,3)
					swap_bytes, DTpp, /big_endian_data
					nda = ((*pd).sub_header.count - 3)/2
					da = long64(b,12,nda)
					swap_bytes, da, /big_endian_data
					fshift = (2.) ^ (-24)
					fda = float(da) * 	fshift
					sformat = '(6x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pd).sub_header.trigger, float((*pd).sub_header.duration)*1.0e-4, (*pd).sub_header.error, format=sformat
					sformat = '(6x, "X=",I7,2x,"Y=",I7,4x)'
					printf, 2, (*pd).sub_header.X,(*pd).sub_header.Y, format=sformat
					if (*pd).sub_header.error ne 0 then begin
						ERR = uint( ishft((*pd).sub_header.error and err_mask, err_offset))
						OFV = uint( ishft((*pd).sub_header.error and ofv_mask, ofv_offset))
						MTC = uint( ishft((*pd).sub_header.error and mtc_mask, mtc_offset))
						printf, 2, ''
						printf, 2, 'Activity accumulator error found:', format='(T4,A)'
						n = n+2
						if ERR ne 0 then begin
							printf, 2,'ERR bit set (error)', format='(T8,A)'
							n = n+1
						endif
						if OFV ne 0 then begin
							printf, 2,'OFV bit set (overflow)', format='(T8,A)'
							n = n+1
						endif
						if MTC ne 0 then begin
							printf, 2,'MTC set, "missing trigger count" = ',MTC, format='(T8,A,3x,I)'
							n = n+1
						endif
					endif
					printf, 2, ''
					printf, 2, DTpp, format='(T4,"DTpp:        NN:",I10,5x,"NNpu:",I10,5x,"TT:",I10)'
					printf, 2, fda, format='(T4,"DA accumulators:",8(/,T6,5G))'
					n = n+4 + (1 > ceil(float(nda)/5.))
					end
				37: begin										; DT accum
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					ndet = ((*pd).sub_header.count)/3
					d = long(b,0,3*ndet)
					swap_bytes, d, /big_endian_data
					q = 3*indgen(ndet)
					dnn = d[q]
					dnnpu = d[q+1]
					dtt = d[q+2]
					dt = (*pstate).deadtime_cal * 1.0e-6 * float(dtt) / (float((*pd).sub_header.duration)*1.0e-4)
					sformat = '(6x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pd).sub_header.trigger, float((*pd).sub_header.duration)*1.0e-4, (*pd).sub_header.error, format=sformat
					sformat = '(6x, "X=",I6,2x,"Y=",I6,4x)'
					printf, 2, (*pd).sub_header.X,(*pd).sub_header.Y, format=sformat
					if (*pd).sub_header.error ne 0 then begin
						ERR = uint( ishft((*pd).sub_header.error and err_mask, err_offset))
						OFV = uint( ishft((*pd).sub_header.error and ofv_mask, ofv_offset))
						MTC = uint( ishft((*pd).sub_header.error and mtc_mask, mtc_offset))
						printf, 2, ''
						printf, 2, 'Activity accumulator error found:', format='(T4,A)'
						n = n+2
						if ERR ne 0 then begin
							printf, 2,'ERR bit set (error)', format='(T8,A)'
							n = n+1
						endif
						if OFV ne 0 then begin
							printf, 2,'OFV bit set (overflow)', format='(T8,A)'
							n = n+1
						endif
						if MTC ne 0 then begin
							printf, 2,'MTC set, "missing trigger count" = ',MTC, format='(T8,A,3x,I)'
							n = n+1
						endif
					endif
					pu = float(dnnpu)/float(dnn)
					q = where( finite(pu),nq)
					if nq gt 0 then begin
						!p.title = 'DT Record - Pile-up loss fraction for this record'
						!x.title = 'Detector #'
						!y.title = 'Pile-up loss fraction (%)'
						!p.charsize = 1.0
						!p.charthick = 1.0
						!p.thick = 1.0
						window,0,xsize=800,ysize=400
						plot,100.*pu,psym=10,/nodata
						oplot,100.*pu,psym=10,color=spec_colour('green')
					endif
					!p.title = 'DT Record - Dead-time for this record'
					!x.title = 'Detector #'
					!y.title = 'Dead-time fraction (%)'
					sdt = str_tidy((*pstate).deadtime_cal)
					window,1,xsize=800,ysize=400
					plot,100.*dt,psym=10,/nodata
					oplot,100.*dt,psym=10,color=spec_colour('green')
					printf, 2, ''
					printf, 2, dnn, format='(T4,"DT accumulators NN:",40(/,T6,15I7))'
					printf, 2, dnnpu, format='(T4,"DT accumulators NNPU:",40(/,T6,15I7))'
					printf, 2, dtt, format='(T4,"DT accumulators TT:",40(/,T6,10I12))'
					printf, 2, pu, format='(T4,"Pile-up loss fraction:",40(/,T6,15F7.4))'
					printf, 2, dt, format='(T4,"Dead-time fraction (for cal='+sdt+' ns):",40(/,T6,15F7.4))'
					n = n+3 + (1 > 4*ceil(float(ndet)/15.))
					n = n + (1 > ceil(float(ndet)/10.))
					end
				39: begin										; activity accum
					trig = (*pd).sub_header.trigger
					subj = ishft( trig and trigger_subj_mask, trigger_subj_offset)
					index = ishft( trig and trigger_index_mask, trigger_index_offset)
					trig = ishft( trig and trigger_trig_mask, trigger_trig_offset)
					groups = ishft( trig and trigger_groups_mask, trigger_groups_offset)
					pu = ishft( trig and trigger_pu_mask, trigger_pu_offset)
					d = ishft( trig and trigger_d_mask, trigger_d_offset)
					
					nd = (*pd).sub_header.count - 16
					b = (*(*pd).b)[0:min([n_bytes,4*nd-1])]
					rates = ulong(b,0,nd)
					swap_bytes, rates, /big_endian_data
					!p.title = 'Detector activity accumulator'
					!x.title = 'Detector #'
					!y.title = 'Frequency'
					!p.charsize = 1.0
					!p.charthick = 1.0
					!p.thick = 1.0
					window,1,xsize=600,ysize=300
					plot,[0,nd-1],[0,1.1*max(rates)],/nodata,xstyle=1
					oplot,rates,psym=10,color=spec_colour('green')
					
					sformat = '(6x, "Trigger=",Z9,3x,"Duration=",G," ms",3x,"Flux1=",I9,3x,"Flux2=",I9,3x,"Error=",Z9)'
					printf, 2, (*pd).sub_header.trigger, float((*pd).sub_header.duration)*1.0e-4, $
								(*pd).sub_header.flux1, (*pd).sub_header.flux2, (*pd).sub_header.error, format=sformat
					n = n+1
					if (*pd).sub_header.error ne 0 then begin
						ERR = uint( ishft((*pd).sub_header.error and err_mask, err_offset))
						OFV = uint( ishft((*pd).sub_header.error and ofv_mask, ofv_offset))
						MTC = uint( ishft((*pd).sub_header.error and mtc_mask, mtc_offset))
						printf, 2, ''
						printf, 2, 'Activity accumulator error found:', format='(T4,A)'
						n = n+2
						if ERR ne 0 then begin
							printf, 2,'ERR bit set (error)', format='(T8,A)'
							n = n+1
						endif
						if OFV ne 0 then begin
							printf, 2,'OFV bit set (overflow)', format='(T8,A)'
							n = n+1
						endif
						if MTC ne 0 then begin
							printf, 2,'MTC set, "missing trigger count" = ',MTC, format='(T8,A,3x,I)'
							n = n+1
						endif
					endif
					printf, 2, ''
					sformat2 = '(6x,"Throttle discard=",I2,3x,"PU reject=",I2,3x,"Group=",I5,3x,"Trig=",Z3,"x",3x,"Index=",I2,3x,"Subj=",I3)'
					printf, 2, d,pu,groups,trig,index,subj, format=sformat2
					n = n+2

					out = lonarr(2,nd)
					out[0,*] = lindgen(nd)
					out[1,*] = rates
					printf, 2, ''
					printf, 2, format='("Channel data:  n / Rate   (where n=detector number, Rate=count recorded in Duration):")'
					n = n+2
					printf, 2, out, format='(8(I4,"/",I8,1x))'
					n = n + (1 > ceil(nd/8.))
					end
				40: begin										; energy spectrum accum
					trig = (*pd).sub_header.trigger
					subj = ishft( trig and trigger_subj_mask, trigger_subj_offset)
					index = ishft( trig and trigger_index_mask, trigger_index_offset)
					trig = ishft( trig and trigger_trig_mask, trigger_trig_offset)
					groups = ishft( trig and trigger_groups_mask, trigger_groups_offset)
					pu = ishft( trig and trigger_pu_mask, trigger_pu_offset)
					d = ishft( trig and trigger_d_mask, trigger_d_offset)
					
					nd = (*pd).sub_header.count
					b = (*(*pd).b)[0:min([n_bytes,4*nd-1])]
					rates = ulong(b,0,nd)
					swap_bytes, rates, /big_endian_data
					!p.title = 'Energy Spectrum Accumulator #' + str_tidy(index)
					!x.title = 'Energy bin #'
					!y.title = 'Counts'
					!p.charsize = 1.0
					!p.charthick = 1.0
					!p.thick = 1.0
					window,1,xsize=600,ysize=300
					plot,[0,nd-1],[0,1.1*max(rates)],/nodata,xstyle=1
					oplot,rates,psym=10,color=spec_colour('green')
					
					sformat = '(6x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pd).sub_header.trigger, float((*pd).sub_header.duration)*1.e-4, (*pd).sub_header.error, format=sformat
					if (*pd).sub_header.error ne 0 then begin
						ERR = uint( ishft((*pd).sub_header.error and err_mask, err_offset))
						OFV = uint( ishft((*pd).sub_header.error and ofv_mask, ofv_offset))
						MTC = uint( ishft((*pd).sub_header.error and mtc_mask, mtc_offset))
						printf, 2, ''
						printf, 2, 'Activity accumulator error found:', format='(T4,A)'
						n = n+2
						if ERR ne 0 then begin
							printf, 2,'ERR bit set (error)', format='(T8,A)'
							n = n+1
						endif
						if OFV ne 0 then begin
							printf, 2,'OFV bit set (overflow)', format='(T8,A)'
							n = n+1
						endif
						if MTC ne 0 then begin
							printf, 2,'MTC set, "missing trigger count" = ',MTC, format='(T8,A,3x,I)'
							n = n+1
						endif
					endif
					printf, 2, ''
					sformat2 = '(6x,"Throttle discard=",I2,3x,"PU reject=",I2,3x,"Group=",I5,3x,"Trig=",Z3,"x",3x,"Index=",I2,3x,"Subj=",I3)'
					printf, 2, d,pu,groups,trig,index,subj, format=sformat2
					n = n+3

					out = intarr(2,nd)
					out[0,*] = indgen(nd)
					out[1,*] = rates
					printf, 2, ''
					printf, 2, format='("Spectrum data:  n / Counts   (where n=E bin number, Rate=count recorded in Duration * 100 ns):")'
					n = n+2
					printf, 2, out, format='(8(I4,"/",I8,1x))'
					n = n + (1 > ceil(nd/8.))
					end
				42: begin										; scan info 1
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					scan_num = ulong(b,0,1)
					swap_bytes, scan_num, /big_endian_data
					scan_mda = ulong(b,4,1)
					swap_bytes, scan_mda, /big_endian_data
					b1 = byte(b,8,4)
					scan_order = b1[0]
					r1 = ulong(b,12,3)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					nzpixels = r1[2]
					r2 = float(b,24,7)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					zorigin = r2[2]
					xpixel = r2[3]
					ypixel = r2[4]
					zpixel = r2[5]
					dwell = r2[6]
					sinfo = string(b[52:*])
					printf, 2, float(nxpixels)*xpixel,float(nypixels)*ypixel, nxpixels,nypixels, xpixel/dwell, $
								format='(T8,"Image size:",2F9.3,1x,"(mm)",4x,"(",I6,"  x",I6," pixels)",4x,"Velocity:",G10.3," (mm/s)")'
					printf, 2, xorigin,yorigin, xpixel*1000.,ypixel*1000., format='(T12,"Origin:",2F9.2,1x,"(mm)",3x,"Pixel pitch:",F8.2,"  x",F8.2," (microns/pixel)")'
					printf, 2, dwell*1000., format='(T8,"Dwell time:",F9.2,1x,"(ms/pixel)")'
					n = n+3
					end					
				43: begin										; time spectrum accum
					trig = (*pd).sub_header.trigger
					subj = ishft( trig and trigger_subj_mask, trigger_subj_offset)
					index = ishft( trig and trigger_index_mask, trigger_index_offset)
					trig = ishft( trig and trigger_trig_mask, trigger_trig_offset)
					groups = ishft( trig and trigger_groups_mask, trigger_groups_offset)
					pu = ishft( trig and trigger_pu_mask, trigger_pu_offset)
					d = ishft( trig and trigger_d_mask, trigger_d_offset)
					
					nd = (*pd).sub_header.count
					if nd gt 0 then begin
						b = (*(*pd).b)[0:min([n_bytes,4*nd-1])]
						rates = ulong(b,0,nd)
						swap_bytes, rates, /big_endian_data
						!p.title = 'Time Spectrum Accumulator #' + str_tidy(index)
						!x.title = 'Time bin #'
						!y.title = 'Counts'
						!p.charsize = 1.0
						!p.charthick = 1.0
						!p.thick = 1.0
						window,1,xsize=600,ysize=300
						plot,[0,nd-1],[0,1.1*max(rates)],/nodata,xstyle=1
						oplot,rates,psym=10,color=spec_colour('green')
					endif
					sformat = '(6x, "Trigger=",Z9,4x,"Duration=",G," ms",4x,"Error=",Z9)'
					printf, 2, (*pd).sub_header.trigger, float((*pd).sub_header.duration)*1.e-4, (*pd).sub_header.error, format=sformat
					if (*pd).sub_header.error ne 0 then begin
						ERR = uint( ishft((*pd).sub_header.error and err_mask, err_offset))
						OFV = uint( ishft((*pd).sub_header.error and ofv_mask, ofv_offset))
						MTC = uint( ishft((*pd).sub_header.error and mtc_mask, mtc_offset))
						printf, 2, ''
						printf, 2, 'Activity accumulator error found:', format='(T4,A)'
						n = n+2
						if ERR ne 0 then begin
							printf, 2,'ERR bit set (error)', format='(T8,A)'
							n = n+1
						endif
						if OFV ne 0 then begin
							printf, 2,'OFV bit set (overflow)', format='(T8,A)'
							n = n+1
						endif
						if MTC ne 0 then begin
							printf, 2,'MTC set, "missing trigger count" = ',MTC, format='(T8,A,3x,I)'
							n = n+1
						endif
					endif
					printf, 2, ''
					sformat2 = '(6x,"Throttle discard=",I2,3x,"PU reject=",I2,3x,"Group=",I5,3x,"Trig=",Z3,"x",3x,"Index=",I2,3x,"Subj=",I3)'
					printf, 2, d,pu,groups,trig,index,subj, format=sformat2
					n = n+3

					printf, 2, ''
					printf, 2, format='("Spectrum data:  n / Counts   (where n=E bin number, Rate=count recorded in Duration * 100 ns):")'
					n = n+2
					if nd gt 0 then begin
						out = intarr(2,nd)
						out[0,*] = indgen(nd)
						out[1,*] = rates
						printf, 2, out, format='(8(I4,"/",I8,1x))'
						n = n + (1 > ceil(nd/8.))
					endif else begin
						printf, 2, 'Zero header count', format='(T8,A)'
						n = n + 1
					endelse
					end
				44: begin										; DA info
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					n_da_elements = ulong(b,0,1)
					swap_bytes, n_da_elements, /big_endian_data
					n_active = ulong(b,4,1)
					swap_bytes, n_active, /big_endian_data
					dt_scale_offset = float(b,8,1)
					swap_bytes, dt_scale_offset, /big_endian_data
					dt_scale_slope = float(b,12,1)
					swap_bytes, dt_scale_slope, /big_endian_data
					scale_factor = float(b,16,n_da_elements)
					swap_bytes, scale_factor, /big_endian_data
					printf, 2, n_da_elements,n_active, format='(T8,"# DA elements:",I3,2x,"(# active:",I3,")")'
					printf, 2, dt_scale_slope,dt_scale_offset, format='(T6,"Deadtime Cal A (slope):",F9.3,2x,", B (offset):",F9.2,2x,"(ns)")'
					printf, 2, ''
					printf, 2, scale_factor, format='(T4,"DA scale factors:",8(/,T6,5G))'
					n = n+4 + (1 > ceil(n_da_elements/5.))

					b2 = b[16+4*n_da_elements:*]
					q = where(b2 eq 0B, nq)
					if nq gt 0 then b2[q] = 10B
					sinfo = string(b2)
					str = strsplit(sinfo,string([10B,13B]),/extract)
					ns = n_elements(str)
					if ns ge n_active then begin
						printf, 2, str[0:n_active-1], format='(T4,"DA elements:",65(/,T6,10A8))'
						n = n + 1 + (1 > ceil(n_active/10.))
					endif else begin
						printf, 2, format='(T4,"DA elements:",65(/,T6,"No element list found"))'
					endelse
					if ns gt n_active then begin
						printf, 2, str[n_active:*], format='(T4,"DA info:",65(/,T6,A))'
						n = n + 1 + (1 > ceil( (ns-n_active)/10.))
					endif else begin
						printf, 2, format='(T4,"DA info:",65(/,T6,"No info strings found"))'
					endelse
					end
				45: begin										; Var list
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					q = where(b eq 0B, nq)
					if nq gt 0 then b[q] = 10B
					sinfo = string(b)
					str = strsplit(sinfo,string([10B,13B]),/extract)
					ns = n_elements(str)
					printf, 2, str, format='(T4,"Var list:",65(/,T6,A))'
					n = n + 1 + (1 > ns)
					end
				46: begin										; Var values
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					q = where(b eq 0B, nq)
					if nq gt 0 then b[q] = 10B
					sinfo = string(b)
					str = strsplit(sinfo,string([10B,13B]),/extract)
					ns = n_elements(str)
					printf, 2, str, format='(T4,"Var values:",65(/,T6,A))'
					n = n + 1 + (1 > ns)
					end
				47: begin										; scan info 2 (need to detect units later [mm assumed below] ...)
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					scan_num = ulong(b,0,1)
					swap_bytes, scan_num, /big_endian_data
					scan_mda = ulong(b,4,1)
					swap_bytes, scan_mda, /big_endian_data
					b1 = byte(b,8,4)
					scan_order = b1[0]
					r1 = ulong(b,12,3)
					swap_bytes, r1, /big_endian_data
					nxpixels = r1[0]
					nypixels = r1[1]
					nzpixels = r1[2]
					r2 = float(b,24,7)
					swap_bytes, r2, /big_endian_data
					xorigin = r2[0]
					yorigin = r2[1]
					zorigin = r2[2]
					xpixel = r2[3]
					ypixel = r2[4]
					zpixel = r2[5]
					dwell = r2[6]
					sinfo = string(b[52:*])
					printf, 2, float(nxpixels)*xpixel,float(nypixels)*ypixel,float(nzpixels)*zpixel, nxpixels,nypixels,nzpixels, $
								format='(T8,"Image size:",3F9.3,3x,"(",I6," x",I6," x",I6," pixels)")'
					printf, 2, xorigin,yorigin,zorigin, xpixel*1000.,ypixel*1000.,zpixel*1000., format='(T12,"Origin:",3F9.2,5x,"Pixel pitch:",F8.2," x",F8.2," x",F8.2)'
					printf, 2, dwell*1000., xpixel/dwell, format='(T8,"Dwell time:",F9.2,1x,"(ms/pixel)",15x,"Velocity:",G10.3," (mm/s)")'
					n = n+3
					if lenchr(sinfo) ne 0 then begin
						printf, 2, ''
						printf, 2, '  Scan Info:'
						printf, 2, sinfo
					endif 
					end					
				48: begin										; DAQ ET with timestamps
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					nd = n_elements(b)/8
					d = ulong(b,0,2,nd)
					swap_bytes, d, /big_endian_data
					dtime = 0.0

					q_non_et = where( (d[0,*] and tag_mask5) ne 0, nq_non_et) 
					if nq_non_et gt 0 then begin
						q = where( (d[0,q_non_et] and xyz_tag_mask5) eq xyz_tag_pa5, nq_xyz_pa) 
						if nq_xyz_pa gt 0 then begin
							q_pa = q_non_et[q]
;							sx = '?'
;							sy = '?'
;							sz = '?'
;							su = '?'
;							sv = '?'
;							sw = '?'
							strange_PA = 0
							sdX = ''
							sdY = ''
							ssX = ''
							ssY = ''
							ssZ = ''
							ssA = ''
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_dX5, nq_pa_dx) 
							if nq_pa_dx gt 0 then begin
								q_pa_dx = q_pa[q]
								if q_pa_dx ne 0 then strange_PA=1
								dx = d[0,q_pa_dx] and pa_mask5
								if (d[0,q_pa_dx] and pa_sign_bit_mask5) ne 0 then dx = dx or pa_sign_extend5
								x = long(dx)
								sdX = str_tidy( x)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_dY5, nq_pa_dy) 
							if nq_pa_dy gt 0 then begin
								q_pa_dy = q_pa[q]
								if q_pa_dy ne 1 then strange_PA=1
								dy = d[0,q_pa_dy] and pa_mask5
								if (d[0,q_pa_dy] and pa_sign_bit_mask5) ne 0 then dy = dy or pa_sign_extend5
								y = long(dy)
								sdY = str_tidy( y)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sX5, nq_pa_sx) 
							if nq_pa_sx gt 0 then begin
								q_pa_sx = q_pa[q]
								if q_pa_sx ne 2 then strange_PA=1
								dz = d[0,q_pa_sx] and pa_mask5
								if (d[0,q_pa_sx] and pa_sign_bit_mask5) ne 0 then dz = dz or pa_sign_extend5
								x = long(dz)
								ssX = str_tidy( x)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sY5, nq_pa_sy) 
							if nq_pa_sy gt 0 then begin
								q_pa_sy = q_pa[q]
								if q_pa_sy ne 3 then strange_PA=1
								du = d[0,q_pa_sy] and pa_mask5
								if (d[0,q_pa_sy] and pa_sign_bit_mask5) ne 0 then du = du or pa_sign_extend5
								u = long(du)
								ssY = str_tidy( u)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sZ5, nq_pa_sz) 
							if nq_pa_sz gt 0 then begin
								q_pa_sz = q_pa[q]
								if q_pa_sz ne 4 then strange_PA=1
								dv = d[0,q_pa_sz] and pa_mask5
								if (d[0,q_pa_sz] and pa_sign_bit_mask5) ne 0 then dv = dv or pa_sign_extend5
								v = long(dv)
								ssZ = str_tidy( v)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sA5, nq_pa_sa) 
							if nq_pa_sa gt 0 then begin
								q_pa_sa = q_pa[q]
								if q_pa_sa ne 5 then strange_PA=1
								dw = d[0,q_pa_sa] and pa_mask5
								if (d[0,q_pa_sa] and pa_sign_bit_mask5) ne 0 then dw = dw or pa_sign_extend5
								w = long(dw)
								ssA = str_tidy( w)
							endif
							printf, 2, sdX, sdY, ssX, ssY, ssZ, ssA, format='(T11,"Pixel address:",T28,"dX = ",A,", dY = ",A,", sX = ",A,", sY = ",A,", sZ = ",A,", sA = ",A)'
							n = n+1
							if strange_PA then begin
								if n_elements(q_pa_dx) gt 0 then sdX = str_tidy( q_pa_dx)
								if n_elements(q_pa_dy) gt 0 then sdY = str_tidy( q_pa_dy)
								if n_elements(q_pa_sx) gt 0 then ssX = str_tidy( q_pa_sx)
								if n_elements(q_pa_sy) gt 0 then ssY = str_tidy( q_pa_sy)
								if n_elements(q_pa_sz) gt 0 then ssZ = str_tidy( q_pa_sz)
								if n_elements(q_pa_sa) gt 0 then ssA = str_tidy( q_pa_sa)
								printf, 2, sdX, sdY, ssX, ssY, ssZ, ssA, format='(20x,"Strange pixel address positions: dX = ",A,", dY = ",A,", sX = ",A,", sY = ",A,", sZ = ",A,", sA = ",A)'
								n = n+1
							endif

							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_tf5, nq_pa_tf) 
							if nq_pa_tf gt 0 then begin
								q_pa_tf = q_pa[q]
								q = where( (d[0,q_pa_tf] and tf_tag_mask5) eq tf_tag_bt5, nq_tf_bt) 
								if nq_tf_bt gt 0 then begin
									q_tf_bt = q_pa_tf[q]
									dtime = double(d[0,q_tf_bt] and tf_bit_mask5) / 10000.
									net = 6 < min([nq_tf_bt,nd])
									out = dblarr(net)
									out[*] = dtime[0:net-1]
									printf, 2, format='(T5,"Time BT values (ms):",T28,6(F12.4,1x))', out
									n = n+1
								endif
								q = where( (d[0,q_pa_tf] and tf_tag_mask5) eq tf_tag_fc5, nq_tf_fc1) 
								if nq_tf_fc1 gt 0 then begin
									q_tf_fc1 = q_pa_tf[q]
									dtime = d[0,q_tf_fc1] and tf_bit_mask5
									net = 6 < min([nq_tf_fc1,nd])
									out = lonarr(net)
									out[*] = dtime[0:net-1]
									printf, 2, format='(T6,"FC values (counts):",T28,6(I12,1x))', out
									n = n+1
								endif
							endif
						endif
					endif
					
					et_tag = where( (d[0,*] and tag_mask5) eq 0, n_et)
					if n_et gt 0 then begin
						de = reform(ishft( d[0,et_tag] and e_mask5, e_offset5))
						dt = reform(ishft( d[0,et_tag] and t_mask5, t_offset5))
						adr = reform(ishft( d[0,et_tag] and adr_mask5, adr_offset5))
						ts = reform(d[1,et_tag])
						ts0 = ts[0]
						tq = where( (long(ts) - long(shift(ts,1))) lt 0, wraps)
						printf, 2, format='("Initial Event Timestamp: ",T28,I,T50,"Wrap arounds: ",I)', ts0, wraps
						n = n+1
						e = de
						!p.title = 'E - T plot for this record'
						!x.title = 'E (Channel Number)'
						!y.title = 'T (Time over Threshold)'
						!p.charsize = 1.0
						!p.charthick = 1.0
						!p.thick = 1.0
						if (*pstate).n_els ge 1 then begin
							e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr]
							!x.title = 'E (energy [keV])'
						endif
						q1 = where(e gt 0, nq1)
						q2 = where(e le 0, nq2)
						if nq1 gt 0 then begin
							window,0,xsize=700,ysize=400
							plot,e[q1],dt[q1],xstyle=0,ystyle=0,/nodata
							oplot,e[q1],dt[q1],psym=3,color=spec_colour('green')
						endif
						!p.title = 'Detector activity - for this record'
						!x.title = 'Detector #'
						!y.title = 'Frequency'
						window,1,xsize=600,ysize=300
						h1 = 0
						h2 = 0
						if nq2 gt 0 then h2 = histogram(adr[q2],binsize=1,min=0,max=383)
						if nq1 gt 0 then h1 = histogram(adr[q1],binsize=1,min=0,max=383)
						plot,[0,36],[0,1.2*max([h1,h2])],/nodata, xstyle=1
						if nq2 gt 0 then oplot,h2,psym=10,color=spec_colour('red')
						if nq1 gt 0 then oplot,h1,psym=10,color=spec_colour('green')
						net = (65536/4) < min([n_et,nd])
						out = lonarr(4,net)
						out[0,*] = de[0:net-1]
						out[1,*] = dt[0:net-1]
						out[2,*] = adr[0:net-1]
						out[3,*] = ts[0:net-1]; - ts0
						printf, 2, ''
						printf, 2, format='("Energy/ Time data:  E / T (n, TS)   (where E=pulse-height, T=time-over-threshold, n=detector #, TS=timestamp [wraps @ 10M]):")'
						n = n+2
						printf, 2, out, format='(5(I5,"/",I5,"(",I3,",",I8,")",1x))'
						n = n + (1 > ceil(float(net)/5.))
					endif else begin
						printf, 2, format='("no ET data in payload")'
						n = n+1
					endelse
					end
				49: begin										; DAQ ET with NO timestamps
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					nd = n_elements(b)/4
					d = ulong(b,0,1,nd)
					swap_bytes, d, /big_endian_data
					dtime = 0.0

					q_non_et = where( (d[0,*] and tag_mask5) ne 0, nq_non_et) 
					if nq_non_et gt 0 then begin
						q = where( (d[0,q_non_et] and xyz_tag_mask5) eq xyz_tag_pa5, nq_xyz_pa) 
						if nq_xyz_pa gt 0 then begin
							q_pa = q_non_et[q]
;							sx = '?'
;							sy = '?'
;							sz = '?'
;							su = '?'
;							sv = '?'
;							sw = '?'
							strange_PA = 0
							sdX = ''
							sdY = ''
							ssX = ''
							ssY = ''
							ssZ = ''
							ssA = ''
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_dX5, nq_pa_dx) 
							if nq_pa_dx gt 0 then begin
								q_pa_dx = q_pa[q]
								if q_pa_dx ne 0 then strange_PA=1
								dx = d[0,q_pa_dx] and pa_mask5
								if (d[0,q_pa_dx] and pa_sign_bit_mask5) ne 0 then dx = dx or pa_sign_extend5
								x = long(dx)
								sdX = str_tidy( x)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_dY5, nq_pa_dy) 
							if nq_pa_dy gt 0 then begin
								q_pa_dy = q_pa[q]
								if q_pa_dy ne 1 then strange_PA=1
								dy = d[0,q_pa_dy] and pa_mask5
								if (d[0,q_pa_dy] and pa_sign_bit_mask5) ne 0 then dy = dy or pa_sign_extend5
								y = long(dy)
								sdY = str_tidy( y)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sX5, nq_pa_sx) 
							if nq_pa_sx gt 0 then begin
								q_pa_sx = q_pa[q]
								if q_pa_sx ne 2 then strange_PA=1
								dz = d[0,q_pa_sx] and pa_mask5
								if (d[0,q_pa_sx] and pa_sign_bit_mask5) ne 0 then dz = dz or pa_sign_extend5
								x = long(dz)
								ssX = str_tidy( x)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sY5, nq_pa_sy) 
							if nq_pa_sy gt 0 then begin
								q_pa_sy = q_pa[q]
								if q_pa_sy ne 3 then strange_PA=1
								du = d[0,q_pa_sy] and pa_mask5
								if (d[0,q_pa_sy] and pa_sign_bit_mask5) ne 0 then du = du or pa_sign_extend5
								u = long(du)
								ssY = str_tidy( u)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sZ5, nq_pa_sz) 
							if nq_pa_sz gt 0 then begin
								q_pa_sz = q_pa[q]
								if q_pa_sz ne 4 then strange_PA=1
								dv = d[0,q_pa_sz] and pa_mask5
								if (d[0,q_pa_sz] and pa_sign_bit_mask5) ne 0 then dv = dv or pa_sign_extend5
								v = long(dv)
								ssZ = str_tidy( v)
							endif
							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_sA5, nq_pa_sa) 
							if nq_pa_sa gt 0 then begin
								q_pa_sa = q_pa[q]
								if q_pa_sa ne 5 then strange_PA=1
								dw = d[0,q_pa_sa] and pa_mask5
								if (d[0,q_pa_sa] and pa_sign_bit_mask5) ne 0 then dw = dw or pa_sign_extend5
								w = long(dw)
								ssA = str_tidy( w)
							endif
							printf, 2, sdX, sdY, ssX, ssY, ssZ, ssA, format='(T11,"Pixel address:",T28,"dX = ",A,", dY = ",A,", sX = ",A,", sY = ",A,", sZ = ",A,", sA = ",A)'
							n = n+1
							if strange_PA then begin
								if n_elements(q_pa_dx) gt 0 then sdX = str_tidy( q_pa_dx)
								if n_elements(q_pa_dy) gt 0 then sdY = str_tidy( q_pa_dy)
								if n_elements(q_pa_sx) gt 0 then ssX = str_tidy( q_pa_sx)
								if n_elements(q_pa_sy) gt 0 then ssY = str_tidy( q_pa_sy)
								if n_elements(q_pa_sz) gt 0 then ssZ = str_tidy( q_pa_sz)
								if n_elements(q_pa_sa) gt 0 then ssA = str_tidy( q_pa_sa)
								printf, 2, sdX, sdY, ssX, ssY, ssZ, ssA, format='(20x,"Strange pixel address positions: dX = ",A,", dY = ",A,", sX = ",A,", sY = ",A,", sZ = ",A,", sA = ",A)'
								n = n+1
							endif

							q = where( (d[0,q_pa] and pa_tag_mask5) eq pa_tag_tf5, nq_pa_tf) 
							if nq_pa_tf gt 0 then begin
								q_pa_tf = q_pa[q]
								q = where( (d[0,q_pa_tf] and tf_tag_mask5) eq tf_tag_bt5, nq_tf_bt) 
								if nq_tf_bt gt 0 then begin
									q_tf_bt = q_pa_tf[q]
									dtime = double(d[0,q_tf_bt] and tf_bit_mask5) / 10000.
									net = 6 < min([nq_tf_bt,nd])
									out = dblarr(net)
									out[*] = dtime[0:net-1]
									printf, 2, format='(T5,"Time BT values (ms):",T28,6(F12.4,1x))', out
									n = n+1
								endif
								q = where( (d[0,q_pa_tf] and tf_tag_mask5) eq tf_tag_fc5, nq_tf_fc1) 
								if nq_tf_fc1 gt 0 then begin
									q_tf_fc1 = q_pa_tf[q]
									dtime = d[0,q_tf_fc1] and tf_bit_mask5
									net = 6 < min([nq_tf_fc1,nd])
									out = lonarr(net)
									out[*] = dtime[0:net-1]
									printf, 2, format='(T6,"FC values (counts):",T28,6(I12,1x))', out
									n = n+1
								endif
							endif
						endif
					endif
					
					et_tag = where( (d[0,*] and tag_mask5) eq 0, n_et)
					if n_et gt 0 then begin
						de = ishft( d[0,et_tag] and e_mask5, e_offset5)
						dt = ishft( d[0,et_tag] and t_mask5, t_offset5)
						adr = ishft( d[0,et_tag] and adr_mask5, adr_offset5)
						e = de
						!p.title = 'E - T plot for this record'
						!x.title = 'E (Channel Number)'
						!y.title = 'T (Time over Threshold)'
						!p.charsize = 1.0
						!p.charthick = 1.0
						!p.thick = 1.0
						if (*pstate).n_els ge 1 then begin
							e = de * (*pstate).cal[1,adr] + (*pstate).cal[0,adr]
							!x.title = 'E (energy [keV])'
						endif
						q1 = where(e gt 0, nq1)
						q2 = where(e le 0, nq2)
						if nq1 gt 0 then begin
							window,0,xsize=700,ysize=400
							plot,e[q1],dt[q1],xstyle=0,ystyle=0,/nodata
							oplot,e[q1],dt[q1],psym=3,color=spec_colour('green')
						endif
						!p.title = 'Detector activity - for this record'
						!x.title = 'Detector #'
						!y.title = 'Frequency'
						window,1,xsize=600,ysize=300
						h1 = 0
						h2 = 0
						if nq2 gt 0 then h2 = histogram(adr[q2],binsize=1,min=0,max=383)
						if nq1 gt 0 then h1 = histogram(adr[q1],binsize=1,min=0,max=383)
						plot,[0,36],[0,1.2*max([h1,h2])],/nodata, xstyle=1
						if nq2 gt 0 then oplot,h2,psym=10,color=spec_colour('red')
						if nq1 gt 0 then oplot,h1,psym=10,color=spec_colour('green')
						net = (65536/4) < min([n_et,nd])
						out = intarr(3,net)
						out[0,*] = de[0:net-1]
						out[1,*] = dt[0:net-1]
						out[2,*] = adr[0:net-1]
						printf, 2, ''
						printf, 2, format='("Energy, Time data:  E / T (n)   (where E=pulse-height, T=time-over-threshold, n=detector #):")'
						n = n+2
						printf, 2, out, format='(10(I5,"/",I5,"(",I3,")",1x))'
						n = n + (1 > ceil(float(net)/10.))
					endif else begin
						printf, 2, format='("no ET data in payload")'
						n = n+1
					endelse
					end
				50: begin										; DAQ activity
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					nd = n_elements(b)/4
					d = ulong(b,0,nd)
					swap_bytes, d, /big_endian_data
					n_det = d[0]
					rates = d[1:n_det]
					!p.title = 'Detector activity accumulator'
					!x.title = 'Detector #'
					!y.title = 'Frequency'
					!p.charsize = 1.0
					!p.charthick = 1.0
					!p.thick = 1.0
					window,1,xsize=600,ysize=300
					plot,[0,n_det-1],[0,1.1*max(rates)],/nodata,xstyle=1
					oplot,rates,psym=10,color=spec_colour('green')

					time = float(d[n_det+1]) * 1.0e-7
					flux = d[n_det+2]
					tot = d[n_det+3]
				 
					sformat = '(6x, "Duration=",G," (s)",4x,"Flux=",I,4x,"Total=",I)'
					printf, 2, time, flux, tot, format=sformat
	        		n = n+1

					out = intarr(2,n_det)
					out[0,*] = indgen(n_det)
					out[1,*] = rates
					printf, 2, ''
					printf, 2, format='("Channel data:  n / Rate   (where n=detector number, Rate=count recorded in Duration):")'
					n = n+2
					printf, 2, out, format='(8(I4,"/",I8,1x))'
					n = n + (1 > ceil(n_det/8.))
					end
				52: begin										; client action
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					uint4 = ulong(b,0,4)
					swap_bytes, uint4, /big_endian_data
					serial = uint4[0]
					action = uint4[1]
					number = uint4[2]
					actions = ['connected','new connection','recently disconnected']
					tot = uint4[3]
					b2 = b[16:*]
					q = where( b2 eq 0B, nq)
					if nq gt 0 then b2[q]=byte('|')
					str = strsplit( string(b2), '|', /extract)
					ns = n_elements(str)
					printf, 2, tot, serial, actions[action<(n_elements(actions))], $
								format='(T8,"Total clients: ",I6,T35,"Serial #: ",I6,T56,"Action: ",A)'
					n = n+1
					if ns ge 3 then begin
						printf, 2, str[2], str[0], str[1], format='(/,T8,"Name: ",A,3x,"IP: ",A,3x,"Port: ",A)'
						n = n+1
					endif
					end
				53: begin										; summary3
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					ru = ulong(b,0,1)
					swap_bytes, ru, /big_endian_data
					run = str_tidy(ru)
					s = ulong(b,4,1)
					swap_bytes, s, /big_endian_data
					segment = str_tidy(s)
					
					di = ulong(b,8,1)
					swap_bytes, di, /big_endian_data
					discard = str_tidy(di)
					
					rt = ulong(b,12,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nc = ulong(b,16,1)
					swap_bytes, nc, /big_endian_data
					clients = str_tidy(nc)
					nct = ulong(b,20,1)
					swap_bytes, nct, /big_endian_data
					tclients = str_tidy(nct)
					nbt = ulong(b,24,1)
					swap_bytes, nbt, /big_endian_data
					tblocks = str_tidy(nbt)

					nbyt = ulong64(b,28,1)
					swap_bytes, nbyt, /big_endian_data
					use_mb = 0
					if nbyt gt 1000000L then begin
						nbyt = float(nbyt)/1000000.
						use_mb = 1
					endif
					tbytes = str_tidy(nbyt)
					
					r = ulong(b,36,1)
					swap_bytes, r, /big_endian_data
					rate = str_tidy(r)
					rb = ulong(b,40,1)
					swap_bytes, rb, /big_endian_data
					brate = str_tidy(rb)
					printf, 2, run, tblocks, rate, format='(T8,"Run: ",I6,T29,"Total blocks: ",I10,T66,"Rate: ",F12.3)'
					if use_mb then begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",F10.2,T54,"Mbytes",T66,"Rate: ",F12.3)'
					endif else begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",I10,T66,"Rate: ",F12.3)'
					endelse
					printf, 2, runtime, clients, tclients, format='(T3,"Run time: ",I10,T34,"Clients: ",I10,T65,"Total: ",I10)'
					
					bg = b[44:*]
					q = where(bg eq 0B, nq)
					if nq gt 0 then bg[q]=127B
					str = strsplit( string(bg), string(127B), /extract, /preserve_null)
					ns = n_elements(str)
					group_next = str[0]
					if ns ge 4 then begin
						group = str[1]
						project_next = str[2]
						project = str[3]
						printf, 2, project_next,project, group_next,group, format='(T4,"Project: Next=",A," Current=",A,", Group: Next=",A," Current=",A)'
						n = n+1
					endif
					n = n+3
					end
				54: begin										; client name
					b = (*(*pr[i]).b)[0:min([n_bytes,(*pr[i]).length])-1]
					printf, 2, string(b), format='(T8,"Client name: ",A)'
					n = n+1
					end
				55: begin										; Metadata
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					q = where(b eq 0B, nq)
					if nq gt 0 then b[q] = 10B
					sinfo = string(b)
					str = strsplit(sinfo,string([10B,13B]),/extract)
					ns = n_elements(str)
					printf, 2, str, format='(T4,"Metadata:",65(/,T6,A))'
					n = n + 1 + (1 > ns)
					end
				56: begin               					    ; summary4
					b = (*(*pd).b)[0:min([n_bytes,(*pd).length])-1]
					ru = ulong(b,0,1)
					swap_bytes, ru, /big_endian_data
					run = str_tidy(ru)
					s = ulong(b,4,1)
					swap_bytes, s, /big_endian_data
					segment = str_tidy(s)

					di = ulong(b,8,1)
					swap_bytes, di, /big_endian_data
					discard = str_tidy(di)

					rt = ulong(b,12,1)
					swap_bytes, rt, /big_endian_data
					runtime = str_tidy(rt)
					nc = ulong(b,16,1)
					swap_bytes, nc, /big_endian_data
					clients = str_tidy(nc)
					nct = ulong(b,20,1)
					swap_bytes, nct, /big_endian_data
					tclients = str_tidy(nct)
					nbt = ulong(b,24,1)
					swap_bytes, nbt, /big_endian_data
					tblocks = str_tidy(nbt)

					nbyt = ulong64(b,28,1)
					swap_bytes, nbyt, /big_endian_data
					use_mb = 0
					if nbyt gt 1000000L then begin
						nbyt = float(nbyt)/1000000.
						use_mb = 1
					endif
					tbytes = str_tidy(nbyt)

					r = ulong(b,36,1)
					swap_bytes, r, /big_endian_data
					rate = str_tidy(r)
					rb = ulong(b,40,1)
					swap_bytes, rb, /big_endian_data
					brate = str_tidy(rb)
					printf, 2, run, tblocks, rate, format='(T8,"Run: ",I6,T29,"Total blocks: ",I10,T66,"Rate: ",F13.3)'
					if use_mb then begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",F10.2,T54,"Mbytes",T66,"Rate: ",F13.3)'
					endif else begin
						printf, 2, segment, tbytes, brate, format='(T4,"Segment: ",I6,T30,"Total bytes: ",I10,T66,"Rate: ",F13.3)'
					endelse
					printf, 2, runtime, clients, tclients, format='(T3,"Run time: ",I10,T34,"Clients: ",I10,T65,"Total: ",I13)'

					bg = b[44:*]
					q = where(bg eq 0B, nq)
					if nq gt 0 then bg[q]=127B
					str = strsplit( string(bg), string(127B), /extract, /preserve_null)
					ns = n_elements(str)
					group_next = str[0]
					if ns ge 5 then begin
						group = str[1]
						project_next = str[2]
						project = str[3]
						errmess = str[4]
						printf, 2, project_next,project, group_next,group, format='(T4,"Project: Next=",A," Current=",A,", Group: Next=",A," Current=",A)'
						printf, 2, errmess, format='(T10,"Current error message=",A)'
						n = n+2
					endif
					n = n+3
					end
					
				else: begin
					tags = [(*pstate).tags, '??']
					ntgs = n_elements(tags)
					if (*pd).length eq 0 then begin
						printf, 2, (*pd).index, (*pd).tag, tags[(*pr[i]).tag < (ntgs-1)], (*pd).length, (*pd).seq, (*pd).tseq, $
	         				format='(I7,I4,2x, A7, I7, I8, I8, 1x, "No payload")'
					endif else begin
						printf, 2, (*pd).index, (*pd).tag, tags[(*pr[i]).tag < (ntgs-1)], (*pd).length, (*pd).seq, (*pd).tseq, $
	         				(*(*pd).b)[0:min([15,min([n_bytes,(*pd).length])-1])], format='(I7,I4,2x, A7, I7, I8, I8, 1x, 16(4Z3,3x))'
					endelse
					end
			endcase
		endelse

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

pro blog_browser, _EXTRA=extra

; blogbrowse (blog_browser.sav) loads routines from GeoPIXE.sav.
; These routines are NOT compiled into blog_browser.sav.
; First look locally (e.g. if this launched from runtime SAV file),
; then try a directory "geopixe" at same dir level nearby.

	found = 0
	file = 'GeoPIXE.sav'
	if file_test(file) eq 0 then begin
		file = '../geopixe/GeoPIXE.sav'
		if file_test(file) eq 0 then begin
			a = dialog_message('blog_browser: Failed to restore GeoPIXE.sav.',/error)
		endif else found=1
	endif else found = 1
	if found then restore, file
	startupp, /colours

	blog_browse, _EXTRA=extra
	return
end

;-----------------------------------------------------------------

pro blog_browse, group=group, realtime=realtime, ip=ip, port=port, $
					dt=dt, version=version, debug=debug

;	Browse Maia Blog files and records.

COMPILE_OPT STRICTARR
if n_elements(realtime) lt 1 then realtime=0
if n_elements(port) lt 1 then port=9000
if n_elements(debug) lt 1 then debug=0
if n_elements(dt) lt 1 then dt=16.7
common c_debug_linux_bug, first							;@8-18
if n_elements(first) lt 1 then first=3					;@8-18

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
       warning,'blog_browser',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET

;	Due to an odd bug with very first pop-up, we try again ...		;@8-18

		if first gt 0 then begin									;@8-18
loop:
			first = (first-1) >0
			Catch, ErrorNo
			if (ErrorNo ne 0) then begin
				Catch, /cancel
				on_error, 1
		       help, calls = s
		       n = n_elements(s)
		       c = 'Call stack: '
		       if n gt 2 then c = [c, s[1:n-2]]
		       warning,'blog_browser',['IDL run-time error caught.', '', $
		          'Error:  '+strtrim(!error_state.name,2), $
		          !Error_state.msg,'',c], /error
		       MESSAGE, /RESET
				if first gt 0 then goto, loop
				return
			endif
		endif else return
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
	
	define_devices
	obj = obj_new('MAIA_DEVICE')
	
	xsize = xlist + 105 + 20
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0) ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])
	toggle_old = 0L
	title = 'Maia Blog Browser'
	if realtime then title = title + '  ' + version
	
	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
				group_leader=group, uname='blog_browse_TLB', /base_align_center, /TLB_SIZE_EVENTS, $
				xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_left)

	if realtime eq 0 then begin
		row0base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
		label = widget_label( row0base, value='Blog Browser '+version)
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
	temp_file = temp_dir + temp_name()
	openw, 2, temp_file
	printf, 2, format='(T7,"i",T9,"tag", T17, "name", T25, "len", T36, "Seq", T44, "TagSeq", T52, "Payload")'
	close, 2
	openr, 2, temp_file
	rtitle = ''
	readf, 2, rtitle
	close, 2

	record_title = Widget_List(col2base, UNAME='record-title',  $
				value = rtitle, scr_xsize=xlist ,scr_ysize=22, font=fnt, /tracking, uvalue = 'Record component title heading, showing "tag" number and name, "len" length of record in bytes, ' + $
				'"Seq" record sequence number, "TagSeq" sequence for this particular tag, "payload" record contents specific to this tag.')
	record_List = Widget_List(col2base, UNAME='record-list',  $
				value = '', /tracking, uvalue = 'Shows all selected record types for the selected Segment(s). Click on one, to select a record from these and display its details in the panel below. ' + $
				'Select record types to display using the "Select Record Tags" button below.', $
				scr_xsize=xlist ,scr_ysize=278, font=fnt)

	row2base = widget_base( col2base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
	button = widget_button( row2base, value='Select Record Tags', uname='select',/tracking, uvalue='Select which record type(s) to display in the above panel.')

	label = widget_label( row2base, value=' ')
	toggle = cw_bgroup2( row2base, ['Show First Occurences','ET only with XY change','Show all monitor logs'], /row, ypad=0, $
					/nonexclusive, /return_index, /tracking, uname='options', set_value=[0,0,1], $
					uvalue=['Only shows the first of a sequence of the same TAG. (Not implemented yet.)', $
							'Only show ET events after a change in XY. (Not implemented yet.)','Show all lines of Monitor reports for each monitor record (up to a maximum of 2048 bytes), else just show the first one.'])

	if realtime eq 0 then begin
		label = widget_label( row2base, value='  Buffer size:')
		buffer_mode = widget_combobox( row2base, value=['500 Kbytes','1 Mbytes','2 Mbytes','3 Mbytes','4 Mbytes','6 Mbytes','8 Mbytes','10 Mbytes','12 Mbytes','15 Mbytes','20 Mbytes','30 Mbytes','40 Mbytes','50 Mbytes','60 Mbytes','80 Mbytes','100 Mbytes'], uname='buffer-size', /tracking, $
						uvalue='Number of bytes to read from each segment file. Only the first records, up to this many bytes, will be displayed for each segment. ' + $
						'Use a buffer size of 100 Mbytes to see all records. However, read times will be ~10-20 seconds per segment.')
	
		label = widget_label( row2base, value=' Skip:')
		skip_text = widget_text( row2base, value='0', uname='skip-text', /tracking, /editable, $
					uvalue='Optionally, skip this number of bytes before processing records to display.',scr_xsize=100)
	endif else begin
		buffer_mode = 0L
		skip_text = 0L
;		button = widget_button( row2base, value='Read', uname='read',/tracking, uvalue='Read another record.')
		button = widget_button( row2base, value='Start', uname='start',/tracking, uvalue='Start adding records to the list from the blogd server.')
		button = widget_button( row2base, value='Stop', uname='stop',/tracking, uvalue='Stop adding records to the list from the blogd server.')
		button = widget_button( row2base, value='Clear', uname='clear',/tracking, uvalue='Clear all records.')
	endelse
		
	label = widget_label( col2base, value='More Record details:')
	detail_List = Widget_List(col2base, UNAME='details-list',  $
			value = labs, /tracking, uvalue = 'Display details of the record selected in the above panel.', $
			scr_xsize=xlist ,scr_ysize=150, font=fnt)

	help = widget_text( tbase, scr_xsize=xlist + (realtime ? 0: 105), ysize=2, /wrap, uname='HELP', /tracking, $
				uvalue='Help window - displays information about widgets. Move cursor over each widget to get help.',frame=0)
	
;	NOTE: This list in 'read_maia" too.
;	See also list of tags in 'read_maia', 'get_maia_details', and 'update_maia_records', 'update_maia_details' too ...
	tags = ['ignore','id','newrun','newseg','tod','summary','comment','send_next','et','xy', $
		'pa','DA_put', 'DA_cal','DA_cal2','DA_mat','DA_pixel1','DA_init_file1','DA_element1','DA_params1', 'DA_raw1', $
		'DA_cal1','DA_throttle1','DA_enable1','send_prev','prev/next','et2','monitor', 'pm_eterr_1', $
		'id2','endrun','rexec1','et3','summary2','setgroup','event_1','DA_accum','ROI_accum','DT_accum','DTpp_accum', $
		'activity','E_spectra','ET2D','scaninfo_1','T_spectra','DA_info','Var_List_1','Var_Value_1','Maia scan Info 2', $
		'DAQ ET w/ TimeStamp','DAQ ET no TimeStamp','DAQ Activity','set-project','client','summary3','client-name', $
		'Metadata','summary4','report']
	ntags = n_elements(tags)

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
			tags:			tags, $				; list of tags by index
			da_cal:			fltarr(3), $		; DA matrix cal
			cal:			fltarr(3,n_detectors), $	; ptr to detector cals
			el:				strarr(n_elements), $	; ptr to element list
			n_els:			0, $				; number of elements
			pdam:			ptrarr(n_elements,/allocate_heap), $	; ptr to DA matrix
			tag_select:		replicate(1,ntags), $	; tag enables
			psegment:		ptr_new(/allocate_heap), $	; pointer to selected segment file list
			precord:		ptr_new(/allocate_heap), $	; pointer to selected subset of records
			pback:			ptr_new(/allocate_heap), $	; pointers from list line back to selected subset of records
			pdetail:		ptr_new(/allocate_heap), $	; pointer to current details text lines
			p:				ptr_new(/allocate_heap), $	; data area - array of pointers to data
			ps:				ps, $				; pointer to blog socket (if /realtime)
			pfiles:			ptr_new(/allocate_heap), $	; file names list
			pylut:			ptr_new(/allocate_heap), $	; ptr to Y lookup table
			ylut_ok:		0, $				; flags a good LUT
			n_buffer:		500000L, $			; read buffer size (bytes)
			timer_on:		0, $				; realtime record read timer
			time:			0.3, $				; realtime record read interval
			timeout:		timeout, $			; blogd socket read timeout	
			temp_file:		temp_file, $		; temp file name
			help:			help $				; context help widget ID
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'blog_browse', tlb, /no_block
	return
end
