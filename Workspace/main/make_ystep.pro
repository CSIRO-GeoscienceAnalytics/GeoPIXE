pro make_ystep, filei, progress=do_progress, device=obj
;
;   Read an .evt file.
;   and translate it into a Y step file (swap X and Y)

COMPILE_OPT STRICTARR
common c_evt_last, last

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
       warning,'make_ystep',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

images = 0L
define_devices
if n_elements(obj) lt 1 then obj=obj_new('MPSYS_DEVICE')

if (n_params() lt 1) then begin
    print,'make_ystep: missing arguments'
    return
endif
file = strtrim(filei,2)
if strlen(file[0]) lt 1 then begin
    if n_elements(last) ge 1 then begin
       path = extract_path(last)
    endif else begin
       path = 'g:\nmp'
    endelse

    file = file_requester(/read, path=path, $
       title='Select EVT file to sort', filter='*'+obj->extension(), $
       /fix_filter)
endif else begin
    path = extract_path( file[0])
    if path eq file[0] then begin
       file = file_requester(/read, path=path, file=file[0], $
         title='Select EVT file to sort', filter='*'+obj->extension(), $
         /fix_filter)
    endif
endelse
if strlen(file) lt 1 then goto, bad_file
last = file[0]

if n_elements(do_progress) lt 1 then do_progress=0

j = 0L
nj = n_elements(file)
evt_file = strip_file_ext( file) + obj->extension()
out_file = strip_file_ext( file[0]) + '-Y' + obj->extension()

n_buffer = 50000L          ; ~50K x 2 x 3 byte buffers (0.3 MBytes)

on_ioerror, bad_out_file
close,2
openw, 2, out_file, bufsiz=1500*1024L
event_array = uintarr( 3, n_buffer, /nozero)
out_buffer = assoc( 2, event_array)
i_out = 0

loop_file:
    on_ioerror, bad_file
    close,1
    openr, 1, evt_file[j], bufsiz=1500*1024L
    on_ioerror, finish

    n_guide = n_buffer
    i_buffer = 0L
    progress_file = 0

    event_array = uintarr( 3, n_buffer, /nozero)
    buffer = assoc( 1, event_array)

    nprogress = ((100000L / n_guide) > 1L) < 500L

    if j eq 0 then begin
       if do_progress then begin
         progress, tlb=progress_tlb, title='Sort X Step XY EVT file'

         iprogress = 0L
         if progress_file then begin
          p = { unit:0, current:0L, size:nj, file:evt_file[j], value:0L}
         endif else begin
          p = { unit:1, value:0L}
         endelse
       endif
    endif

    while ~ EOF(1) do begin

       ev = buffer[i_buffer]
       i_buffer = i_buffer+1

       t = (ev[2,*] and '3FFF'xu) or '8000'xu
       ev[2,*] = (ev[1,*] and '3FFF'xu) or '4000'xu
       ev[1,*] = t

       out_buffer[i_out] = ev
       i_out = i_out+1

       if do_progress then begin
         iprogress = iprogress + 1
         if iprogress ge nprogress then begin
          if progress_file then begin
              p.current = j
              p.file = evt_file[j]
          endif
          progress, /update, progress_tlb, p, cancel=cancel
          if cancel then goto, finish
          iprogress = 0L
         endif
       endif

    endwhile

    j = j+1
    if j lt nj then goto, loop_file

finish:
    if n_elements(p) gt 0 then begin
       if do_progress then begin
         progress, /update, progress_tlb, p
       endif
    endif
close, 1
on_ioerror, bad_close
if do_progress then begin
    progress, /complete, progress_tlb, 'X step --> Y step complete'
endif

close, 2

if do_progress then progress, /ending, progress_tlb
return

bad_file:
warning, 'make_ystep', 'EVT File not found.'
return
bad_out_file:
warning, 'make_ystep', 'Could not open output file.'
return
bad_close:
warning, 'make_ystep', 'Error closing file (disk space?).'
return
end
