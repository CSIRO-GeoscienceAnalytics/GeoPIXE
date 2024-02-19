    pro get_nac_spec, p, file, group=group
;
;   Read in a NAC XSYS (VAX) binary spectrum
;
    p = 0L
    define_devices

    if n_params(0) lt 2 then return
    if n_elements(group) lt 1 then group=0L
    if file eq '' then return

    n = 0
    got_pixe = -1
    charge = 0.0
    events = 0L

    xinfo = { area:0L, oldtype:0, type:0B, class:0B, lwlen:0L, plen:0L, relp:0L, $
         anam:bytarr(8), xsiz:0L, ysiz:0L, xoffs:0, yoffs:0, xgain:0, ygain:0, $
         xss:0, yss:0, evnum:0B, flags: 0B, xssold:0B, yssold:0B, $
         cond:0, fill1:0, glow:0, ghig:0, back:0, disp:0, cal1:0.0, cal2:0.0, cal3:0.0, $
         flp:0.0, fill2:bytarr(16) }

    header = { chrtyp:bytarr(4), cxkey:bytarr(4), xvers:0L, nrun:0L, hbdate:lonarr(2), $
         hsvers:0, hdtype:0, hdblks:0L, ksum:0L, isum:0L, rsum:0L, fill1:lonarr(4), $
         eclflgs:0L, inf:xinfo, ctitle:bytarr(80), cuser:bytarr(12), $
         fill2:bytarr(4), fill3:lonarr(64) }

    on_ioerror, bad_io
    openr, unit, file, bufsiz=100*1024L, /get_lun

    readu, unit, header
    swap_bytes, header, big_endian_data=0, /vax_data

;   print, 'CHRtyp=',string(header.chrtyp)
;   print, 'HDtype=',header.hdtype, '  HDblks=',header.hdblks
;   print, 'Nrun=',header.nrun, '  ctitle=',string(header.ctitle)
;   print, 'Cuser=',string(header.cuser)
;   print, ''
    run_num = header.nrun
    if string(header.chrtyp) ne 'RUNH' then goto, bad_run

    while ~ EOF(unit) do begin
       readu, unit, header
       swap_bytes, header, big_endian_data=0, /vax_data

;     print, 'CHRtyp=',string(header.chrtyp)
;     print, 'HDtype=',header.hdtype, '  HDblks=',header.hdblks
;     print, 'Type=',header.inf.type, '  Class=',header.inf.class
;     print, 'LWlength=',header.inf.lwlen, '  Anam=',string(header.inf.anam)
;     print, 'xsiz=',header.inf.xsiz, '  ysiz=',header.inf.ysiz
;     print, 'xgain=',header.inf.xgain, '  ygain=',header.inf.ygain
;     print, 'area=',header.inf.area, '  evnum=',header.inf.evnum, ' flags=',header.inf.flags
;     print, 'cal1=',header.inf.cal1, '  cal2=',header.inf.cal2, '  cal3=',header.inf.cal3

       if (string(header.chrtyp) ne 'DATA') then goto, done
       len = header.inf.xsiz * header.inf.ysiz
       channel = header.inf.area - 1
       if header.inf.area eq 1 then got_pixe=n             ; XRAY
       label = strtrim( string(header.inf.anam), 2)
;     print,'Read data block, size=',len,'  Event number=',channel+1,'  label=',label

       if (header.HDtype ne 0) and (header.HDblks gt 0) then begin
         case header.HDtype of
          2: data = bytarr(len)
          3: data = uintarr(len)
          4: data = ulonarr(len)
          5: data = fltarr(len)
          6: data = dblarr(len)
          else: goto, bad_type
         endcase

         readu, unit, data
         swap_bytes, data, big_endian_data=0, /vax_data

         if (header.inf.class eq 1) and (header.inf.ysiz eq 1) and  $
              (header.HDtype ne 0) and (header.HDblks gt 0) then begin

		device_name = 'NAC_XSYS_DEVICE'
		obj = obj_new(device_name)

          spec2 = define(/spectrum)
          spec2.file = file
          spec2.source = ''
          spec2.DevObj = clone_device_object(obj)
          spec2.label = strtrim(strip_file_ext(file),2) + '  ' + strtrim(label,2)
          spec2.sequence.num = run_num
          spec2.station = channel+1
          spec2.channel = channel
          spec2.ecompress = 1
          case label of
              'XRAY': spec2.detector = 0
              'PIGE': spec2.detector = 1
              'RBS': spec2.detector = 2
              'ERDA': spec2.detector = 3
              'STIM': spec2.detector = 4
              'CHARGE': spec2.detector = 5
              'DEADTIME': spec2.detector = 6
              else:
          endcase

          spec2.cal.order = 1
          if abs(header.inf.cal3) gt 1.0e-6 then spec2.cal.order=2
          if abs(header.inf.cal2) lt 1.0e-6 then header.inf.cal2=1.0
          if (abs(header.inf.cal2 - 1.0) gt 0.0001) then begin
              case label of
                 'XRAY':   spec2.cal.units = 'keV'
                 'PIGE':   spec2.cal.units = 'MeV'
                 'RBS':    spec2.cal.units = 'MeV'
                 'STIM':   spec2.cal.units = 'MeV'
                 'ERDA':   spec2.cal.units = 'MeV'
                 else: spec2.cal.units = 'MeV'
              endcase
          endif else begin
              spec2.cal.units = 'Channel'
          endelse
          spec2.cal.poly[0] = header.inf.cal1
          spec2.cal.poly[1] = header.inf.cal2
          spec2.cal.poly[2] = header.inf.cal3

          spec2.size = len
          spec2.data = ptr_new(float(data))

          if (n_elements(p) ge 1) and ptr_valid(p[0]) then begin
              p = [p, ptr_new(spec2,/no_copy)]
          endif else begin
              p = ptr_new(spec2,/no_copy)
          endelse
          n = n+1

         endif else if (header.inf.class eq 3) and (header.inf.area eq 990) and  $
              (header.HDtype ne 0) and (header.HDblks gt 0) then begin

          charge = float(data[3])/1000.0
          events = data[2]
         endif
       endif
    endwhile

done:
    if n gt 0 then begin
       for k=0L,n-1 do (*p[k]).charge = charge

       if got_pixe ge 0 then begin
         sum = total( *(*p[got_pixe]).data)
         dt = float(events) / sum
         if (dt lt 1.0) then begin
          warning,'get_nac_spec','Unrealistic dead-time correction <1.0; ignored.'
         endif else if (dt gt 2.0) then begin
          warning,'get_nac_spec','Very large dead-time correction >1.0; ignored.'
         endif else begin
          *(*p[got_pixe]).data =  *(*p[got_pixe]).data * dt
          (*p[got_pixe]).deadtime_correction =  dt
         endelse
       endif
    endif

    close,unit
    free_lun,unit
    return

bad_run:
    warning,'get_nac_spec','Bad initial header, not of type RUNH'
    goto, done
bad_header:
    warning,'get_nac_spec','Bad header, not of type DATA'
    goto, done
bad_spectrum:
    warning,'get_nac_spec','Not spectrum class'
    goto, done
bad_type:
    warning,'get_nac_spec','Bad data type: ',header.HDtype
    goto, done
bad_io:
    warning,'get_nac_spec','I/O error'
    goto, done
    end
