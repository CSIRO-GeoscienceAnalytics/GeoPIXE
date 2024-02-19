function get_img, file, mpsys_unix=mpsys_unix
;
;   Reading an .img file.
;   Return the spectra
;
;   The spectra are returned as an array of pointers to
;   new spectrum structures.
;
;   The default is MPsys/Linux format.
;   Use /MPsys_Unix to select MPsys/Unix format.
;
; if n_elements(file) lt 1 then file = 'b13.img'

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
       warning,'Get_IMG',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, bad_io
    endif
endif

if n_elements(mpsys_unix) lt 1 then mpsys_unix=0
device_name = 'MPSYS_DEVICE'
if mpsys_unix then device_name='MPSYS_UNIX_DEVICE'
obj = obj_new(device_name)

on_ioerror, bad_io

s = strip_file_ext( file) + '.img'
openr, lun, s, /get_lun

station_struct = { e:lonarr(8192), x:lonarr(4096), y:lonarr(4096)}
buffer = assoc( lun, station_struct)

output = strip_file_ext( file) + 'i.spec'
i = 0
found = 0
first = 1
use_xy = 1
on_ioerror, bad_format

while ~ EOF(lun) do begin

try_again:
    s = buffer[i]
    goto, cont

bad_format:
    if first eq 0 then goto, bad_io
    first = 0
    station_struct = { e:lonarr(8192)}           ; try without X,Y
    buffer = assoc( lun, station_struct)
    use_xy = 0
    goto, try_again

cont:
    swap_bytes, s, big_endian_data=obj->big_endian()

    if total( s.e) lt 1 then goto, more

    spec = define(/spectrum)
    spec.file = output
    spec.DevObj = clone_device_object(obj)
    spec.source = file
    spec.label = file + ' ' + strtrim(string(i+1),2) + '/E'
    spec.size = 8192
    spec.data = ptr_new(s.e,/no_copy)
    spec.station = i+1
    spec.log = 1
    if found eq 0 then begin
       p = ptr_new( spec, /no_copy)
    endif else begin
       p = [ p, ptr_new(spec, /no_copy)]
    endelse
    found = 1

if use_xy then begin
    spec = define(/spectrum)
    spec.source = file
    spec.DevObj = clone_device_object(obj)
    spec.label = file + ' ' + strtrim(string(i),2) + '/X'
    spec.size = 4096
    spec.data = ptr_new(s.x,/no_copy)
    spec.station = i+1
    p = [ p, ptr_new(spec, /no_copy)]

    spec = define(/spectrum)
    spec.source = file
    spec.label = file + ' ' + strtrim(string(i),2) + '/Y'
    spec.size = 4096
    spec.data = ptr_new(s.y,/no_copy)
    spec.station = i+1
    p = [ p, ptr_new(spec, /no_copy)]
endif

  more:
    spec = 0
    i = i+1
endwhile

finish:
	if obj_valid(obj) then obj_destroy, obj
	close_file, lun
	return, p

bad_io:
    print,'get_img: I/O error'
    p = 0
    goto, finish
end
