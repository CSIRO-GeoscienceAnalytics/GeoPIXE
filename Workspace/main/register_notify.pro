;-------------------------------------------------------------------------------
;
; These routines manage global messaging in IDL applications.
;
; register_notify, id, tags
;
;	Registers that widget 'id' wants to hear about the events in 'tags'.
;	'tags' is just a string name (or array), with the name(s) of the
;	events that 'id' wants to know about. Optionally specifying a specifc 
;	'from' ID
;
; notify, tag
;
;	Sends out notification events to all widgets registered to hear about a
;	'tag' event, optionally specifying a data pointer and a specifc 'from' ID.
;
; cancel_notify, id
;
;	Cancels all notifications that were requested by 'id'. They also get
;	cancelled if an id is found to be invalid.
;
; N.B. Check_notify will kill any notifications that are to/from widgets that
; are not realized. Hence, do not use any register_notifys, etc. between creating
; widgets and the /realize step. Only register them after /realized.
;
;-------------------------------------------------------------------------------
;
; Return an index into the list of registered tags for a single notify string 'name'

function notify_bits, name

; notify_tag_name	unique list of registered tag names
; n_notify_mask		total number of registered tag names

common c_notify_2, n_notify_mask, notify_tag_name

	if n_elements(name) lt 1 then return, -1
	if n_elements(n_notify_mask) lt 1 then n_notify_mask = 0

	s = strlowcase( name[0])
	n = n_notify_mask
	if n gt 0 then begin
		q = where( s eq notify_tag_name, nq)
		if nq ge 1 then return, q[0]
	endif
	
	n_notify_mask = n+1
	if n eq 0 then begin
		notify_tag_name = s
	endif else begin
		notify_tag_name = [notify_tag_name,s]
	endelse
	
	return, n
end

;-------------------------------------------------------------------------------
;
; The 'mask' is a vector/index into the list of registered notify tags. 
; Return an index (or vector of indices) for notify string array 'tags'

function notify_mask, tags

	if n_elements(tags) lt 1 then return, -1
	if size(tags,/tname) ne 'STRING' then return, -1

	for i=0L,n_elements(tags)-1 do begin
		j = notify_bits( tags[i])
		if j ge 0 then begin
			if i eq 0 then begin
				mask = j
			endif else begin
				mask = [mask,j]
			endelse
		endif
	endfor
	
	return, mask
end

;-------------------------------------------------------------------------------
;
; Return a string array of names for mask vector 'mask'

function notify_tags, mask

; notify_tag_name	unique list of registered tag names
; n_notify_mask		total number of registered tag names

common c_notify_2, n_notify_mask, notify_tag_name

	n = 0
	if n_elements(n_notify_mask) lt 1 then n_notify_mask = 0
	if n_elements(mask) lt 1 then return, ''

	for i=0L,n_elements(mask)-1 do begin
		if i eq 0 then begin
			tags = notify_tag_name[mask[i]]
		endif else begin
			tags = [tags,notify_tag_name[mask[i]]]
		endelse
	endfor
	
	return, tags
end

;--------------------------------------------------------------------------
;
; Cancel any notification that was registered to 'id';
; ie. any notify that was registered to go to 'id' or from 'id'.

pro cancel_notify, id

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

	if n_elements( n_notify_id) lt 1 then return
	if n_notify_id lt 1 then return
	
	for i=0L, n_notify_id-1 do begin
		if (id eq notify_id[i]) or (id eq notify_from[i]) then begin
			notify_id[i] = 0L
			notify_flags[i] = 0
			notify_from[i] = 0L
			notify_to[i] = 0
		endif
	endfor
	for i=0L, n_notify_id-1 do begin
		if widget_info( notify_id[i],/valid_id) eq 0 then begin
			notify_id[i] = 0L
			notify_flags[i] = 0
			notify_from[i] = 0L
			notify_to[i] = 0
		endif
		if notify_from[i] ne 0L then begin
			if widget_info( notify_from[i],/valid_id) eq 0 then begin
				notify_id[i] = 0L
				notify_flags[i] = 0
				notify_from[i] = 0L
				notify_to[i] = 0
			endif
		endif
	endfor
	
	return
end

;--------------------------------------------------------------------------
;
; Test each registered entry for this 'from' source. If found, continue
; search using found id as new from. This recursive process sets the
; notify_to vector to flag which ids to send notifys to.

pro test_notify, itag, from

; Recursively find all notify indices that expect 'itag' from ID 'from'
; Flag those found by setting 'notify_to' = 1.

; notify_id		widget ID of a target widget
; n_notify_id	total number of notifys
; notify_from	widget ID of source 'from' widget
; notify_to		logical flag to flag a valid destination ID for a notify
; notify_flags	index of tag into list of unique registered tags

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

	if n_elements( n_notify_id) lt 1 then begin
		n_notify_id = 0
		return
	endif
	if n_notify_id lt 1 then return
	
	mask = notify_mask( itag)				; returns vector of indices to notifys expecting 'itag'
	
	for i=0L,n_notify_id-1 do begin
		q = where( notify_to eq 1)			; indices for flagged destinations
		found = 0							; 'found' means aleady done
		if q[0] ne -1 then begin
			q = where( notify_id[i] eq notify_id[q])
			if q[0] ne -1 then found=1
		endif
		if (mask[0] eq notify_flags[i]) and (notify_to[i] eq 0) and (found eq 0) then begin
			if from eq 0 then begin
				notify_to[i] = 1
			endif else if (from eq notify_from[i]) then begin
				notify_to[i] = 1
				test_notify, itag, notify_id[i]		; propogate/chain notify
			endif
		endif
	endfor
	
	return
end

;--------------------------------------------------------------------------
;
; Notify any registered ids waiting on one particular tag string 'itag',
; and pass on the 'pointer'. Note only one 'itag' string per call.
;
; 'From' specifies that this notify was registered requesting only notifications
; from a particular widget id. We supply this id using 'from'. By convention,
; the from id is the top-level-base id of the current widget program.
;
; This only really makes sense if we pass the id of a group_leader. Then if
; the from widget is detroyed (the group leader), which would make this notify
; invalid, then our destination widget is destroyed too.
;
; As well as notifying any ids that specify this 'from' as their requested from,
; we also search for the resulting id as a requested 'from' for other notifies,
; as so forth, so that all connected ids get notified.

pro notify, itag, pointer, from=from

; n_notify_id	total number of notifys
; notify_id		widget ID of a target widget
; notify_from	widget ID of source 'from' widget
; notify_to		logical flag to flag a valid destination ID for a notify
; notify_flags	index of tag into list of unique registered tags

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to
common c_notify_4, track_notify

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
			warning,'Notify',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

;	A first use/define of {NOTIFY} could set incorrect data-types, so we do it here first
;	to make sure it is correct for all uses.

	notify_event = {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:'', POINTER:ptr_new(), FROM:0L }

	if n_elements(track_notify) lt 1 then track_notify=0
;	if track_notify then print,'---> enter NOTIFY: itag=',itag,' FROM=',from
	if n_elements(from) lt 1 then from=0L

	if n_elements( n_notify_id) lt 1 then begin
		n_notify_id = 0
		return
	endif
	if n_notify_id lt 1 then return
	if n_elements(pointer) lt 1 then pointer=ptr_new()
	if size(pointer,/tname) ne 'POINTER' then begin
		print,'Notify (itag=',itag,', from=',from,'): bad pointer'
		return
	endif
	
	mask = notify_mask( itag)		; only mask[0] gets used below
	check_notify

;	Test each registered entry for this 'from' source. If found, continue
;	search using found id as new from. This recursive process sets the
;	'notify_to' vector to flag which ids to send notifys to.

	notify_to[*] = 0
	test_notify, itag, from			; sets the 'notify_to' flag for all that should receive 'itag'

;	Send a NOTIFY event to all IDs with 'notify_to' true.

	j = 0
	for i=0L,n_notify_id-1 do begin
		if (notify_to[i] eq 1) then begin
			if widget_info( notify_id[i],/valid_id) then begin
				top = tlb_id( notify_id[i])
				tag = notify_tags( mask[0])
				notify_event = {NOTIFY, ID:notify_id[i], TOP:top, HANDLER:0L, $
							TAG:tag, POINTER:pointer, FROM:from }
				if track_notify then begin
					s0 = '### NOTIFY: tag=  '
					s1 = '   FROM='
					sw = widget_info( from, /valid) ? widget_info( from, /uname) : 'none'
					s2 = ' ( ' + sw + ' ),    TO='
					s3 = ' ( ' + widget_info( notify_id[i], /uname) + ' )'
					print,s0,tag,s1,from,s2,notify_id[i],s3
				endif
				if widget_info( notify_id[i],/realized) then begin
					widget_control, notify_id[i], send_event=notify_event
				endif
			endif
		endif
	endfor
	
	return
end

;--------------------------------------------------------------------------
;
; Like notify, but just prints all IDs that should receive notification.

pro debug_notify, itag, from=from

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

	if n_elements(from) lt 1 then from=0L
	
	if n_elements( n_notify_id) lt 1 then begin
		n_notify_id = 0
		return
	endif
	if n_notify_id lt 1 then return
	if n_elements(pointer) lt 1 then pointer=ptr_new()
	
	mask = notify_mask( itag)
	check_notify
	
	notify_to[*] = 0
	test_notify, itag, from
	
	for i=0L,n_notify_id-1 do begin
		if (notify_to[i] eq 1) and (notify_id[i] ne from) and (notify_from[i] eq from) then begin
			if widget_info( notify_id[i], /valid_id) then begin
				uname = widget_info( notify_id[i], /uname)
			endif else begin
				uname = '-invalid-'
			endelse
			if widget_info( from, /valid_id) then begin
				ufrom = widget_info( from, /uname)
			endif else begin
				ufrom = '-invalid-'
			endelse
			if widget_info( notify_id[i],/valid_id) then begin
				top = tlb_id( notify_id[i])
			endif else begin
				top = -1
			endelse
			tag = notify_tags( mask[0])
			print,'Notify   "',tag,'"     to ',notify_id[i],' (',uname,')     from =',from,' (',ufrom,')'
		endif
	endfor
	
	return
end

;-------------------------------------------------------------------------------

; Check all registered notifys for any referencing invalid widget IDs.
; If invalid found, set their 'notify_id' (as well as from, flags, to) to zero.

pro check_notify

; n_notify_id	total number of notifys
; notify_id		widget ID of a target widget
; notify_from	widget ID of source 'from' widget
; notify_to		logical flag to flag a valid destination ID for a notify
; notify_flags	index of tag into list of unique registered tags

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

	if n_elements(n_notify_id) lt 1 then n_notify_id = 0
	
	test_realized_too = 0
	
;	Test for any invalid widget IDs. They are just zeroed, and left in list, to keep
;	the 'notify_flags' indices correct.

	for i=0L,n_notify_id-1 do begin

;		Test for invaid target ID ...

		if widget_info( notify_id[i],/valid_id) eq 0 then begin
			notify_id[i] = 0L
			notify_flags[i] = 0
			notify_from[i] = 0L
			notify_to[i] = 0
		endif else begin
			if test_realized_too and (widget_info( notify_id[i],/realized) eq 0) then begin
				notify_id[i] = 0L
				notify_flags[i] = 0
				notify_from[i] = 0L
				notify_to[i] = 0
			endif
		endelse
	
;		Test for invaid from ID ...

		if notify_from[i] ne 0L then begin
			if widget_info( notify_from[i],/valid_id) eq 0 then begin
				notify_id[i] = 0L
				notify_flags[i] = 0
				notify_from[i] = 0L
				notify_to[i] = 0
			endif else begin
				if test_realized_too and (widget_info( notify_from[i],/realized) eq 0) then begin
					notify_id[i] = 0L
					notify_flags[i] = 0
					notify_from[i] = 0L
					notify_to[i] = 0
				endif
			endelse
		endif
	endfor
	
;	Eliminate duplicates ...
;	They are just zeroed, and left in list, to keep the 'notify_flags' indices correct.

	for i=0L,n_notify_id-1 do begin
		if notify_id[i] ne 0 then begin
			q = where( (notify_id eq notify_id[i]) and (notify_from eq notify_from[i]) $
					and (notify_flags eq notify_flags[i]) and (notify_id[i] ne 0) )
	
			if q[0] ne -1 then begin
				for j=0L,n_elements(q)-1 do begin
					if (q[j] ne i) then begin
						notify_id[q[j]] = 0L			; zero duplicates
						notify_flags[q[j]] = 0
						notify_from[q[j]] = 0L
						notify_to[q[j]] = 0
					endif
				endfor
			endif
		endif
	endfor

	return
end

;-------------------------------------------------------------------------------
;
; Clear all notifications arrays and flags

pro clear_notify

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_2, n_notify_mask, notify_tag_name
common c_notify_3, notify_from, notify_to

	n_notify_id = 0
	n_notify_mask = 0
	
	notify_id = 0L
	notify_flags = 0
	notify_tag_name = ''
	notify_from = 0L
	notify_to = 0
	
	return
end

;-------------------------------------------------------------------------------
;
; Show all register 'id' to receive notifications on any of 'tags' events.

pro show_notify

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

	if n_elements(n_notify_id) lt 1 then n_notify_id = 0
	
	for i=0L, n_notify_id-1 do begin
		if notify_id[i] ne 0L then begin
			if widget_info( notify_id[i], /valid_id) then begin
				uname = widget_info( notify_id[i], /uname)
			endif else begin
				uname = '-invalid-'
			endelse
			if notify_from[i] ne 0L then begin
				if widget_info( notify_from[i], /valid_id) then begin
					ufrom = widget_info( notify_from[i], /uname)
				endif else begin
					ufrom = '-invalid-'
				endelse
				print, notify_id[i],' (',uname,')   ', notify_tags( notify_flags[i]), $
					'           from ',notify_from[i],' (',ufrom,')'
			endif else begin
				print, notify_id[i],' (',uname,')   ', notify_tags( notify_flags[i])
			endelse
		endif
	endfor
	
	check_notify
	return
end

;--------------------------------------------------------------------------

pro track_notify, on=on, off=off

common c_notify_4, track_notify

	if (n_elements(on) lt 1) and (n_elements(off) lt 1) then on=1
	if n_elements(off) lt 1 then off=0
	if n_elements(on) lt 1 then on=0
	if off then on=0
	
	track_notify = on
	return
end

;-------------------------------------------------------------------------------
;
; Redirect a notification.
; If a TLB needs to be remade, then need to exchange all occurences of it in
; the notify tables.
;
; N.B  Do not have any other Notify-type routines between destroying the original TLB,
; creating the new one and calling this redirection routine. Otherwise the cleanup routine
; Check_Notify will clear out invalid IDs.

pro redirect_notify, from=from, to=to

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

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
			warning,'Redirect_notify',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	
	if n_elements(from) lt 1 then return
	if n_elements(to) lt 1 then return
	if from eq 0L then return
	if to eq 0L then return
	if n_elements(n_notify_id) lt 1 then n_notify_id = 0
	
	if n_notify_id lt 1 then return
	
	q = where( from eq notify_id)
	if q[0] ne -1 then notify_id[q] = to
	q = where( from eq notify_from)
	if q[0] ne -1 then notify_from[q] = to
	
	check_notify
	return
end

;-------------------------------------------------------------------------------
;
; Register 'id' to receive notifications on any of 'tags' events.
;
; If 'from' is specified, then we only want to get notification from notify's
; listed that specify this 'from' id in a from keyword.

pro register_notify, id, tags, from=from

; n_notify_id	total number of notifys
; notify_id		widget ID of a target widget
; notify_from	widget ID of source 'from' widget
; notify_to		logical flag to flag a valid destination ID for a notify
; notify_flags	index of tag into list of unique registered tags

common c_notify_1, n_notify_id, notify_id, notify_flags
common c_notify_3, notify_from, notify_to

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
			warning,'Register_notify',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	
	if n_params(0) lt 2 then return
	if n_elements(id) lt 1 then begin
		warning,'register_notify',['TLB id is undefined; tags = ',tags]
		return
	endif
	if n_elements(from) lt 1 then from=0L
	
	if n_elements(n_notify_id) lt 1 then n_notify_id = 0
	valid = widget_info( long(id), /valid_id)
	check_notify
	if not valid then begin
		print,'register_notify: ID [',id,'] is invalid'
		return
	endif

;	Mask is now just an index into a list of registered notify string names.
;	If there are more than one registered, then mask is an int vector.

	mask = notify_mask( tags)
	if mask[0] eq -1 then return
	
	j = 0
	nmask = n_elements(mask)
	if n_notify_id gt 0 then begin						; reuse an id=0 entry first
		for i=0L, n_notify_id-1 do begin
			if notify_id[i] eq 0L then begin
				notify_id[i] = id
				notify_flags[i] = mask[j]
				notify_from[i] = long(from)
				notify_to[i] = 0
				j = j+1
				if j ge nmask then goto, finish
			endif
		endfor
	endif
	
	while j lt nmask do begin
		n_notify_id = n_notify_id+1						; make a new entry
		if n_notify_id eq 1 then begin
			notify_id = id
			notify_flags = mask[j]
			notify_from = long(from)
			notify_to = 0
		endif else begin
			notify_id = [notify_id,id]
			notify_flags = [notify_flags,mask[j]]
			notify_from = [notify_from,from]
			notify_to = [notify_to,0]
		endelse
		j = j+1
	endwhile

finish:
	check_notify
	return
end

