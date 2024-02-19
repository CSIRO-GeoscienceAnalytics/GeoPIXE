pro concat

files = file_requester( filter='*.evt', /read, /must_exist, /multiple, $
          title='Select EVT files to concatenate')
if n_elements(files) lt 2 then return
if lenchr(files[0]) lt 1 then return

	q = sort(files)

    name = strip_file_ext(files[q[0]]) + '-new.evt'
    openw, 1, name
    buffer = uintarr(512*1024L)

    for i=0L,n_elements(files)-1 do begin
       openr, 2, files[q[i]]
       on_ioerror, err
       while ~ EOF(2) do begin
         readu,2,buffer
         writeu,1,buffer
       endwhile
       goto, cont
err:
       stat = fstat(2)
       n = stat.transfer_count
       if n gt 0 then writeu,1,buffer[0:n-1]
cont:
       close, 2
    endfor

    close, 1
    return
end
