!
! Image Fortran routines for DLL or shared lib to serve IDL with
! matrix routines for DA image accumulation and image processing
! and fiddly bit twiddling for certain input data devices.
!
! Windows and Linux source differ:
!	Windows: use !!DLL_EXPORT lines.
!	Linux: comment out the !!DLL_EXPORT lines
!
! 64 bit libraries: edit address passing wrappers ...
!	integer*8 argc, argv(*)		! pass 64 bit addresses by reference
!
!...............................................................................
! Windows: DLL (image_dll.f90) is compiled and linked like so:
! 		using Lahey Fortran (c:\LF9555\Bin\lf95)
!
!  lf95 -dll -winconsole -lst image_dll.f90
!
!...............................................................................
! Linux: Library (image_lib.f) is compiled and linked like so:
!
!  g77 -fno-pedantic -Wno-globals -ff90 -ffree-form fPIC -o image_lib.o -c image_lib.f
!  ld -shared -o image_lib.so  image_lib.o
!
!  Remember in the call_external call to include /CDECL,
!  and TWO underscores after the global name.
!
!-------------------------------------------------------------------------------

! default to 32-bit

#ifdef __64BIT__
#define INTEGER_ARCH integer*8
#else
#define INTEGER_ARCH integer*4
#endif

! ensure integer arguments to iand() and ior() match in "kind"
! https://stackoverflow.com/questions/59756525/iand-with-different-kind-parameters-using-new-gfortran-version

#define empty()
#define fixkind(f, i, j)	f empty() (int(i, kind(j)), j)
#define iand(i, j)		fixkind(iand, i, j)
#define ior(i, j)		fixkind(ior, i, j)

!  !DLL_EXPORT geopixe_lib_version
!  INTEGER_ARCH argc, argv(*)

!-------------------------------------------------------------------------------

integer function geopixe_lib_version( argc, argv)

!DLL_EXPORT geopixe_lib_version
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.1) then          ! needed # args
	geopixe_lib_version= 1
	return
endif

call geopixe_lib_version_b( %val(argv(1)) )

geopixe_lib_version = 0
return
end

!------------------------------------------------------------

SUBROUTINE geopixe_lib_version_b( version)

INTEGER*4 version

version = 53
return
end

!-------------------------------------------------------------------------------

integer function image_accumulate( argc, argv)

!DLL_EXPORT image_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.9) then          ! needed # args
	image_accumulate = 1
	return
endif

call image_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)) )

image_accumulate = 0
return
end

!------------------------------------------------------------

SUBROUTINE image_accumulate_b( x,y,e,d,n, image,nx,ny,nel)

INTEGER*4 n,nx,ny,nel
INTEGER*4 x(0:n-1),y(0:n-1),e(0:n-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), d(0:n-1)

do j=0,n-1
	if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
			((y(j).ge.0).and.(y(j).lt.ny)).and. &
			((e(j).ge.0).and.(e(j).lt.nel)) ) then

		image(x(j),y(j),e(j)) = image(x(j),y(j),e(j)) + d(j)
	endif
enddo
return
end

!-------------------------------------------------------------------------------

SUBROUTINE get_maia_32( ev, i, n_buffer, buff, nbuff, jbuff, incomplete, swap, err)

! Read nbuff bytes from ev, starting at i. Do not exceed n_buffer.
! Load them into buff, starting at jbuff.
! If ev runs out, return incomplete with jbuff recording how
! many actually returned (i.e. next position to fill).
! If swap is set, then rearrange first 4(8) bytes of long buff.

INTEGER*4 i,j, n_buffer, nbuff,jbuff, swap, err
INTEGER*1 ev(0:n_buffer-1), buff(0:nbuff-1), it
LOGICAL incomplete

	err = 0
	if(jbuff.ge.nbuff) then
	   err = 1
	   return
	endif

	do j=jbuff,nbuff-1
	   if(i.ge.n_buffer) then
		 incomplete = .true.
		 jbuff = j
		 return
	   endif
	   buff(j) = ev(i)
	   i = i+1
	enddo

	incomplete = .false.
	jbuff = 0

	if(swap.eq.1) then  
	    if(nbuff.ge.4) then 	         ! swap first 4 bytes, as in long
		it = buff(0)
		buff(0) = buff(3)
		buff(3) = it
		it = buff(1)
		buff(1) = buff(2)
		buff(2) = it
	    endif
	    if(nbuff.ge.8) then 	         ! swap second 4 bytes, as in long
		it = buff(4)
		buff(4) = buff(7)
		buff(7) = it
		it = buff(5)
		buff(5) = buff(6)
		buff(6) = it
	    endif
	endif
	return
end

!-------------------------------------------------------------------------------

integer function init_maia_32( argc, argv)

!DLL_EXPORT init_maia_32
INTEGER_ARCH argc, argv(*)

integer*4 remain, mbuff, n_mon_buff
INTEGER*1 bev(0:3)
logical first
parameter (n_mon_buff = 20000)
INTEGER*1 mon_buff(0:n_mon_buff-1)
Integer*2 ldone, rdone, ldone2, rdone2
REAL*4 dtsum, raw, dwell_last, dwell_first
integer*4 xlast, ylast, xlastp, ylastp

common /c_maia_1/ jbuff, bev, remain, first
common /c_maia_3/ mbuff, mon_buff
common /c_maia_4/ ldone, rdone, ldone2, rdone2
common /c_maia_5/ xlast, ylast, xlastp, ylastp, dwell_last, dwell_first, dtsum, raw
common /c_maia_7/ xy_on

jbuff = 0
remain = 0
mbuff = 0
ldone = 1
rdone = 1
ldone2 = 1
rdone2 = 1

xlast = -1
ylast = -1
xlastp = -1
ylastp = -1
dwell_last = 0.0
dwell_first = 0.0
dtsum = 0.0
raw = 0.0
xy_on = 0

init_maia_32 = 0
return
end

!-------------------------------------------------------------------------------

integer function unidaq_32_events( argc, argv)

!DLL_EXPORT unidaq_32_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.18) then          ! needed # args
	unidaq_32_events = 1
	return
endif

call unidaq_32_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)) )

unidaq_32_events = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE unidaq_32_events_b( ev,n_buffer, channel_on,nc, e,x,y,n,ste, &
		 ibranch,swap, ecount,length,skip, x0,y0, bad,idebug)

INTEGER*4 i,j,n,nc, n_buffer, n_max, bad, swap, tev(0:24), jbuff, err, scaler_data_type
INTEGER*4 interrupt, ADC0_mask, ADC1_mask, ADC6_mask, ADC7_mask, adc_data_type, unidaq_events
INTEGER*4 ADC0_pu_mask, ADC6_pu_mask, idebug, type, run, sequence, mode, was_jbuff
INTEGER*2 ecount, length, ibranch, x0,y0, skip, remain, byte_mask
INTEGER*2 e(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*2 ste(0:n_buffer-1), channel_on(0:nc-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:99)
LOGICAL incomplete

common /c_unidaq_1/ jbuff, bev
equivalence (bev(0), tev(0))

parameter ( unidaq_events = 0, ADC0_mask = z'00000002', adc_data_type = z'0000EEEE'  )
parameter ( ADC1_mask = z'00000004', ADC6_mask = z'00000008', ADC7_mask = z'00000080')
parameter ( ADC0_pu_mask = z'00000020', ADC6_pu_mask = z'00000040')
parameter ( byte_mask = z'0FFF', scaler_data_type = z'0000DDDD' )

n_max = n_buffer/4
n = 0
i = 0
goto (10,20,30,40,50,60,70,80,90,100), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (length)
	length = 4*tev(0)
	ibranch = 2
	if( length.gt.8041*4) then
		idebug = 7
		bad = bad+1
		return
	endif

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (type)
	type = tev(0)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (run)
	run = tev(0)
	ibranch = 4

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (seq)
	sequence = tev(0)
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (mode)
	mode = tev(0)
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (reserved)
	reserved = tev(0)
	ibranch = 7
	idebug = -1

!	e(n) = length/4
!	x(n) = type
!	n = n+1

70  if(type.eq.unidaq_events) then				 ! Event block type
		remain = length - 4*6
		idebug = 1
		ibranch = 9
		ecount = 0								! events in 801 record stream, then scalers
		goto 90
	endif
	skip = length - 4*6

80 if(i+skip.gt.n_buffer-1) then                ! skip unwanted data
		skip = (i+skip) - n_buffer
		idebug = 2
		ibranch = 8
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'unidaq_events' event ...

90 if(n.ge.n_max-4) return
	if( ecount.ge.801) then
		idebug = 3
		ibranch = 10
		goto 100
	endif
!	if(remain.lt.4) then         		       	     ! skip insufficient data
!		i = i+remain				! remain not defined on reentry!
!		ibranch = 1
!		idebug = 4
!		goto 10
!	endif
	was_jbuff = jbuff

!   Process payload data up to 801 ADC blocks ...

	call get_maia_32( ev, i, n_buffer, bev, 40, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
!		do j = 0,9
!			ste(j) = tev(j)
!		enddo
!		y(0) = was_jbuff
!		y(1) = jbuff
!		y(2) = i
		idebug = 8
		return
	endif
!	if( was_jbuff.ne.0) then
!		do j = 0,9
!			ste(j) = tev(j)
!		enddo
!		y(0) = was_jbuff
!		y(1) = jbuff
!		y(2) = i
!		n = 10
!		idebug = 9
!		return	
!	endif

	if( tev(0).eq.scaler_data_type) then
		idebug = 10
		err = 1
		return
!		ibranch = 10
!		goto 100
	endif
	if( tev(0).ne.adc_data_type) then
		idebug = 5
		err = 1
		return
	endif
	interrupt = tev(1)

!	e(n) = length/4
!	y(n) = interrupt
!	n = n+1
!	goto 111

	x0 = iand( tev(4), byte_mask)				! ADC 2		relative XY
	y0 = iand( tev(5), byte_mask)				! ADC 3
	if( channel_on(0).eq.1) then
		if( iand( interrupt, ADC0_mask).ne.0) then
			if( iand( interrupt, ADC0_pu_mask).eq.0) then
				x(n) = x0
				y(n) = y0
				e(n) = iand( tev(2), byte_mask)
				ste(n) = 0
				n = n+1
			endif
		endif
	endif
	if( channel_on(1).eq.1) then
		if( iand( interrupt, ADC1_mask).ne.0) then
!			if( iand( interrupt, ADC1_pu_mask).eq.0) then
				x(n) = x0
				y(n) = y0
				e(n) = iand( tev(3), byte_mask)
				ste(n) = 1
				n = n+1
!			endif
		endif
	endif
	if( channel_on(2).eq.1) then
		if( iand( interrupt, ADC6_mask).ne.0) then
			if( iand( interrupt, ADC6_pu_mask).eq.0) then
				x(n) = x0
				y(n) = y0
				e(n) = iand( tev(8), byte_mask)
				ste(n) = 2
				n = n+1
			endif
		endif
	endif
	if( channel_on(3).eq.1) then
		if( iand( interrupt, ADC7_mask).ne.0) then
!			if( iand( interrupt, ADC6_pu_mask).eq.0) then
				x(n) = x0
				y(n) = y0
				e(n) = iand( tev(9), byte_mask)
				ste(n) = 3
				n = n+1
			endif
!		endif
	endif

111	continue
	ecount = ecount+1
	remain = remain-40
	goto 90

!   Process payload data Scaler block  ...

100	call get_maia_32( ev, i, n_buffer, bev, 100, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return

!	Do scaler count here later (scaler #3) for charge

	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function init_unidaq_32( argc, argv)

!DLL_EXPORT init_unidaq_32
INTEGER_ARCH argc, argv(*)

integer*4 jbuff
INTEGER*1 bev(0:99)

common /c_unidaq_1/ jbuff, bev
jbuff = 0

init_unidaq_32 = 0
return
end

!-------------------------------------------------------------------------------

integer function daq_36_events( argc, argv)

!DLL_EXPORT daq_36_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.33) then 		         ! needed # args
	daq_36_events = 1
	return
endif

call daq_36_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), &
	 %val(argv(29)), %val(argv(30)), %val(argv(31)), %val(argv(32)), &
	 %val(argv(33)) )

daq_36_events = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of DAQ 36 events handler modelled on Maia (includes flux, dwell, dead-time, ...)
! and used for off-line processing of streamed event data.
! Uses the addition of pseudo events to hold new values of BT, FC0, FC1).
!
!	flux_mode	0	  ??		fx index 0	flux selected (PV or FCn)
!			1	FC1 h/w			 1	FC1
!			2	(FC2 ?)			 2	(FC2 ?)
!							 3	Dwell
!
!	xaxis	0	X	yaxis	0	X	select X,Y axes from XYZUVW
!		1	Y		1	Y
!		2	Z		2	Z
!		3	U		3	U
!		4	V		4	V
!		5	W		5	W

SUBROUTINE daq_36_events_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for selected flux, FC0, FC1, Dwell)

INTEGER*4 n,nc, n_buffer, bad, swap, idebug, length
INTEGER*4 n_events, n_fx, flux_mode, skip
INTEGER*2 tag, ibranch, x0,y0,z0,u0,v0,w0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1)
INTEGER*2 z(0:n_events-1), u(0:n_events-1), v(0:n_events-1), w(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1)
REAL*4 fx(0:n_fx-1,0:n_events-1)

INTEGER*1 a_tag, b_tag, bev(0:7)
INTEGER*4 i,j,k,l,m, nb, n_max, sequence, jbuff, tx,ty,tz, tv,tu,tw, tev(0:1), err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, len2
INTEGER*2 de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, word_mask
INTEGER*2 maia_events_ts, maia_events_nots
INTEGER*2 pa_offset5, de_offset5, dt_offset5, pdf_offset5, tag_offset5, adr_offset5
INTEGER*4 adr_mask5, de_mask5, dt_mask5, pdf_mask5, tag_mask5, pa_mask5
INTEGER*4 pa_sign_bit_mask5, pa_sign_extend5, pa_tag_offset5
INTEGER*4 pa_tag_mask5, pa_tag_x5, pa_tag_y5, pa_tag_z5, pa_tag_u5, pa_tag_v5, pa_tag_w5, pa_tag_tf5
INTEGER*4 tf_tag_mask5, tf_tag_bt5, tf_tag5_fc1, tf_tag5_fc2, tf_bit_mask5, tf_tag_fc5  
Integer*2 ldone, rdone
REAL*8 time_first
LOGICAL incomplete, first, whole

common /c_daq_5/ jbuff, bev, remain, first				! init w/ init_daq_32
common /c_daq_4/ ldone, rdone
common /c_daq_6/ tv_sec, tv_micro
equivalence (bev(0), tev(0))

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_ts = 48, maia_events_nots = 49 )

! Specific to the ET format 'pm_event_ts_1' and 'pm_event_nots_1' ...
parameter ( adr_mask5 = z'7F000000', de_mask5 = z'00001FFF', dt_mask5 = z'00FFE000' )
parameter ( pdf_mask5 = z'80000000', tag_mask5 = z'80000000', adr_offset5 = -24 )
parameter ( de_offset5 = 0, dt_offset5 = -13, pdf_offset5 = -31, tag_offset5 = -31 )
parameter ( pa_offset5 = 0, pa_mask5 = z'0FFFFFFF', pa_tag_offset5 = -27)
parameter ( pa_sign_bit_mask5 = z'08000000', pa_sign_extend5 = z'F0000000')
parameter ( pa_tag_mask5 = z'F0000000', pa_tag_x5 = z'80000000', pa_tag_y5 = z'90000000', pa_tag_z5 = z'A0000000' )
parameter ( pa_tag_u5 = z'B0000000', pa_tag_v5 = z'C0000000', pa_tag_w5 = z'D0000000', tf_tag_fc5 = z'F4000000' )
parameter ( pa_tag_tf5 = z'F0000000', tf_tag_mask5 = z'FC000000', tf_tag_bt5 =z'F0000000' )
parameter ( tf_bit_mask5 = z'03FFFFFF' )

n_max = n_events
n = 0
i = 0
goto (10,20,30,40,50,60,70,80,90,100, 110,120), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		idebug = 91
		return
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	length = iand(length,byte_mask)
	len2 = bev(1)
	len2 = iand(len2,byte_mask)
	length = ior( ishft( length, 8), len2)
	if(length.gt.65536) then
		idebug = 95
		return
	endif
	if(length.lt.0) then
		idebug = 96
		return
	endif
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev(0)
	ibranch = 4

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev(0)
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev(0)
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev(0)
	if( time_first.eq.0.0) time_first = dble(tv_sec) + dble(tv_micro)/1000000.
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev(0)
	ibranch = 8

80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_nots) then			     ! DAQ not time-stamped, they contain XYZUVW too
		remain = length
		first = .true.
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_events_ts) then				     ! DAQ time-stamped, they contain XYZUVW too
		remain = length
		first = .true.
		ibranch = 11
		goto 110
	endif
	skip = length	

100	if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
		idebug = 90
		return
	endif
	if(i+skip.lt.0) then
		skip = 0
		ibranch = 10
		idebug = 94
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'pm_event_nots_1' event ... (normal 32-bit event data)

120	if(remain.le.0) then						! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 92
		goto 10
	endif

!   Process payload data while it lasts ...
!	First 6 words of ET events is PA pixel XYZUVW information, but we won't assume this (use masks).
!   Also need to assume that PA Y always follows PA X, and PA X,Y precedes all ET data.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words of payload

	tx = iand( tev(0), tag_mask5)
	if(tx.eq.0) then							! ET

!		These are assumed to follow AFTER x0,y0 are set from PA ...

		dtag = 0
		dt = ishft( iand( tev(0), dt_mask5), dt_offset5)
		de = ishft( iand( tev(0), de_mask5), de_offset5)
		adr = ishft( iand( tev(0), adr_mask5), adr_offset5)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = de
				t(n) = dt
!				time(n) = 0
				ste(n) = adr
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
!				if (flux_mode.eq.0) then			! use (Epics PV?)
!					fx(0,n) = flux_pv				! put current val in each event
!				endif
				n = n+1
			endif
		endif
	else
		tx = iand( tev(0), pa_tag_mask5)
		if(tx.eq.pa_tag_x5) then				! PA X
			tx =  iand( tev(0), pa_mask5)
			if( iand(tx,pa_sign_bit_mask5).ne.0) tx = ior(tx,pa_sign_extend5)
			x0 = tx

		else if(tx.eq.pa_tag_y5) then				! PA Y
			ty =  iand( tev(0), pa_mask5)
			if( iand(ty,pa_sign_bit_mask5).ne.0) ty = ior(ty,pa_sign_extend5)
			y0 = ty

		else if(tx.eq.pa_tag_z5) then				! PA Z
			tz =  iand( tev(0), pa_mask5)
			if( iand(tz,pa_sign_bit_mask5).ne.0) tz = ior(tz,pa_sign_extend5)
			z0 = tz
	
		else if(tx.eq.pa_tag_u5) then				! PA U
			tu =  iand( tev(0), pa_mask5)
			if( iand(tu,pa_sign_bit_mask5).ne.0) tu = ior(tu,pa_sign_extend5)
			u0 = tu
	
		else if(tx.eq.pa_tag_v5) then				! PA V
			tv =  iand( tev(0), pa_mask5)
			if( iand(tv,pa_sign_bit_mask5).ne.0) tv = ior(tv,pa_sign_extend5)
			v0 = tv
	
		else if(tx.eq.pa_tag_w5) then				! PA W
			tw =  iand( tev(0), pa_mask5)
			if( iand(tw,pa_sign_bit_mask5).ne.0) tw = ior(tw,pa_sign_extend5)
			w0 = tw
	
		else if(tx.eq.pa_tag_tf5) then				! Time-Flux

!			These are assumed to follow AFTER x0,y0 are set from PA ...

			tx = iand( tev(0), tf_tag_mask5)
			if((tx.eq.tf_tag_bt5).and.(n.lt.n_events)) then				! BT (ms)
				ty = iand( tev(0), tf_bit_mask5)
				x(n) = x0
				y(n) = y0							! add a pseudo event
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = 0
				t(n) = 0
!				time(n) = 0
				ste(n) = 0
				tags(n) = 0
				veto(n) = 1							! will veto adding to images/spectra
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				fx(3,n) = ty * 0.0001				! dwell (ms)
				n = n+1

			else if((tx.eq.tf_tag_fc5).and.(n.lt.n_events)) then			! Flux
				ty = iand( tev(0), tf_bit_mask5)
				x(n) = x0
				y(n) = y0							! add a pseudo event
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = 0
				t(n) = 0
!				time(n) = 0
				ste(n) = 0
				tags(n) = 0
				veto(n) = 1							! will veto adding to images/spectra
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				fx(1,n) = ty
!				if (flux_mode.eq.0) then			! use Epics PV
!					fx(0,n) = flux_pv				! put current val in each event
!				else if (flux_mode.eq.1) then
					fx(0,n) = ty					! h/w will be added to pseudo event
!				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'pm_event_ts_1' event with time-stamps (64-bit event data) ...

110	if(remain.le.0) then						! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 93
		goto 10
	endif

!   Process payload data while it lasts ...
!   First words of ET events is PA pixel XYZUVW information, but we won't assume this (use masks).
!   Also need to assume that PA Y always follows PA X, and PA X,Y precedes all ET data.

	call get_maia_32( ev, i, n_buffer, bev, 8, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words x2 of payload

	tx = iand( tev(0), tag_mask5)
	if(tx.eq.0) then							! ET

!		These are assumed to follow AFTER x0,y0 are set from PA ...

		dtag = 0
		dt = ishft( iand( tev(0), dt_mask5), dt_offset5)
		de = ishft( iand( tev(0), de_mask5), de_offset5)
		adr = ishft( iand( tev(0), adr_mask5), adr_offset5)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = de
				t(n) = dt
!				time(n) = tev(1)					! time-stamp
				ste(n) = adr
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
!				if (flux_mode.eq.0) then			! use (Epics PV?)
!					fx(0,n) = flux_pv				! put current val in each event
!				endif
				n = n+1
			endif
		endif
	else
		tx = iand( tev(0), pa_tag_mask5)
		if(tx.eq.pa_tag_x5) then				! PA X
			tx =  iand( tev(0), pa_mask5)
			if( iand(tx,pa_sign_bit_mask5).ne.0) tx = ior(tx,pa_sign_extend5)
			x0 = tx

		else if(tx.eq.pa_tag_y5) then				! PA Y
			ty =  iand( tev(0), pa_mask5)
			if( iand(ty,pa_sign_bit_mask5).ne.0) ty = ior(ty,pa_sign_extend5)
			y0 = ty

		else if(tx.eq.pa_tag_z5) then				! PA Z
			tz =  iand( tev(0), pa_mask5)
			if( iand(tz,pa_sign_bit_mask5).ne.0) tz = ior(tz,pa_sign_extend5)
			z0 = tz
	
		else if(tx.eq.pa_tag_u5) then				! PA U
			tu =  iand( tev(0), pa_mask5)
			if( iand(tu,pa_sign_bit_mask5).ne.0) tu = ior(tu,pa_sign_extend5)
			u0 = tu
	
		else if(tx.eq.pa_tag_v5) then				! PA V
			tv =  iand( tev(0), pa_mask5)
			if( iand(tv,pa_sign_bit_mask5).ne.0) tv = ior(tv,pa_sign_extend5)
			v0 = tv
	
		else if(tx.eq.pa_tag_w5) then				! PA W
			tw =  iand( tev(0), pa_mask5)
			if( iand(tw,pa_sign_bit_mask5).ne.0) tw = ior(tw,pa_sign_extend5)
			w0 = tw
	
		else if(tx.eq.pa_tag_tf5) then				! Time-Flux

!			These are assumed to follow AFTER x0,y0 are set from PA ...

			tx = iand( tev(0), tf_tag_mask5)
			if((tx.eq.tf_tag_bt5).and.(n.lt.n_events)) then				! BT (ms)
				ty = iand( tev(0), tf_bit_mask5)
				x(n) = x0
				y(n) = y0							! add a pseudo event
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = 0
				t(n) = 0
!				time(n) = tev(1)
				ste(n) = 0
				tags(n) = 0
				veto(n) = 1							! will veto adding to images/spectra
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				fx(3,n) = ty * 0.0001				! dwell (ms)
				n = n+1

			else if((tx.eq.tf_tag_fc5).and.(n.lt.n_events)) then			! Flux
				ty = iand( tev(0), tf_bit_mask5)
				x(n) = x0
				y(n) = y0							! add a pseudo event
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = 0
				t(n) = 0
!				time(n) = tev(1)
				ste(n) = 0
				tags(n) = 0
				veto(n) = 1							! will veto adding to images/spectra
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				fx(1,n) = ty
!				if (flux_mode.eq.0) then			! use Epics PV
!					fx(0,n) = flux_pv				! put current val in each event
!				else if (flux_mode.eq.1) then
					fx(0,n) = ty					! h/w will be added to pseudo event
!				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-8
	first = .false.
	goto 110
end
	
!-------------------------------------------------------------------------------

integer function init_daq_32( argc, argv)

!DLL_EXPORT init_daq_32
INTEGER_ARCH argc, argv(*)
integer*4 remain, jbuff
INTEGER*1 bev(0:7)
logical first
Integer*2 ldone, rdone

common /c_daq_5/ jbuff, bev, remain, first
common /c_daq_4/ ldone, rdone

jbuff = 0
remain = 0
ldone = 1
rdone = 1

init_daq_32 = 0
return
end

!-------------------------------------------------------------------------------

integer function daq_accumulate_dtfx( argc, argv)

!DLL_EXPORT daq_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.23) then       		   ! needed # args
	daq_accumulate_dtfx = 1
	return
endif

call daq_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)), %val(argv(23)) )

daq_accumulate_dtfx = 0
return
end

!------------------------------------------------------------

SUBROUTINE daq_accumulate_dtfx_b( image_mode, t,x,y,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,n_flux, dta,dtb,dtc, flux, dead, dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action.
!	For older record types (w/ no BT, Flux1, etc. words) also catch PV in fx[0,*] for veto=0 real events.
!
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!				(hangover from Maia, and may not use flux_mode=0 for DAQ device)
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] (if not zero) to set Dwell in daq_dwell
!
!	Dead_fraction:
!		Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
!		Only accept good (veto=0) events.
!		Later (in da_evt) norm this using the dead_fraction_norm(/image) method, which returns dwell (ms) array.

INTEGER*4 image_mode, flux_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),t(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, dta,dtb,dtc, flux(0:xrange-1,0:yrange-1,0:n_flux)
REAL*4 dead(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt

if(n.le.0) return
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

!  Note that 'pseudo=1' is usually the same as 'veto=1'. However, borders, etc. can be trimmedfor these
!  pixels pseudo=0 and veto=1, which fail both tests below.
!
!  This approach follows Maia, which works for channels (ste) 0-31. For channels 32-35, we do the
!  same thing but with a fixed 10 MHz clock, reflected in 'dtc'.

do i=0,n-1
	if( ste(i).ge.32) then
		dt = dtc * t(i)
	else
		dt = dta * t(i) + dtb
	endif
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),0) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),0) = flux(x(i),y(i),0) + fx(0,i) * flux_scale2
				endif
				flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
				flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
				endif
			endif
			if( veto(i).eq.0) then
				if( dt.gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + dt
				endif
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),0) = fx(0,i) * flux_scale2
					endif
				endif
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in Maia device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.0) then
				if( dt.gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + dt
				endif
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

SUBROUTINE get_ixrf( ev, i, n_buffer, buff, nbuff, jbuff, incomplete, swap, err)

! Read nbuff bytes from ev, starting at i. Do not exceed n_buffer.
! Load them into buff, starting at jbuff.
! If ev runs out, return incomplete with jbuff recording how
! many actually returned (i.e. next position to fill).
! If swap is set, then rearrange first 4 (2) bytes of long (short half) buff.

INTEGER*4 i,j, n_buffer, nbuff,jbuff, swap, err
INTEGER*1 ev(0:n_buffer-1), buff(0:nbuff-1), it
LOGICAL incomplete

	err = 0
	if(jbuff.ge.nbuff) then
	   err = 1
	   return
	endif

	do j=jbuff,nbuff-1
	   if(i.ge.n_buffer) then
		 incomplete = .true.
		 jbuff = j
		 return
	   endif
	   buff(j) = ev(i)
	   i = i+1
	enddo

	incomplete = .false.
	jbuff = 0
	if(swap.eq.0) return

	if(nbuff.ge.4) then          ! swap first 4 bytes, as in long
	   it = buff(0)
	   buff(0) = buff(3)
	   buff(3) = it
	   it = buff(1)
	   buff(1) = buff(2)
	   buff(2) = it
	else if(nbuff.ge.2) then     ! swap first 2 bytes, as in int
	   it = buff(0)
	   buff(0) = buff(1)
	   buff(1) = it
	endif
	return
end

!-------------------------------------------------------------------------------

integer function init_ixrf( argc, argv)

!DLL_EXPORT init_ixrf
INTEGER_ARCH argc, argv(*)

integer*4 remain, jbuff, x0_last, y0_last, z0_last 
INTEGER*1 bev(0:3)
Integer*2 ldone, rdone, ldone2, rdone2

common /c_ixrf_1/ jbuff, bev, remain
common /c_ixrf_4/ ldone, rdone, ldone2, rdone2
common /c_ixrf_7/ x0_last, y0_last, z0_last

jbuff = 0
remain = 0
ldone = 1
rdone = 1
ldone2 = 1
rdone2 = 1
x0_last = 0
y0_last = 0
z0_last = 0

init_ixrf = 0
return
end

!-------------------------------------------------------------------------------

integer function ixrf_events1( argc, argv)

!DLL_EXPORT ixrf_events1
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.24) then 		         ! needed # args
	ixrf_events1 = 1
	return
endif

call ixrf_events1_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)) )

ixrf_events1 = 0
return
end

!-------------------------------------------------------------------------------
!
! First Version of iXRF_events for UQ iXRF List-mode.
!
!	flux_mode	0		-- not used? --		fx index 0		Q
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!									 3		Dwell
!									 4		DT corr

SUBROUTINE ixrf_events1_b( ev,n_buffer, channel_on,nc, e,count,x,y,ste,veto,tags,n_events,n, &
		fx,n_fx, x0,y0, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0
INTEGER*2 e(0:n_events-1), count(0:n_events-1), x(0:n_events-1), y(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1)
REAL*4 fx(0:n_fx-1,0:n_events-1)
LOGICAL incomplete

INTEGER*2 dp_event_id
PARAMETER (dp_event_id = z'5044')
!INTEGER*4 dp_event_id					! hack for 16-bit chars
!PARAMETER (dp_event_id = z'00500044')

INTEGER*4 tx, ty, tz, tt, tev, tag4
INTEGER*2 de, dtag, pdf, adr, sev, i_channel, i_detector
REAL*4 fev, dt
integer*4 jbuff, remain, x0_last, y0_last, z0_last
integer*2 ldone, rdone, ldone2, rdone2
INTEGER*1 bev(0:3)
logical first

common /c_ixrf_1/ jbuff, bev, remain
common /c_ixrf_4/ ldone, rdone, ldone2, rdone2
common /c_ixrf_6/ tv_sec, tv_micro
common /c_ixrf_7/ x0_last, y0_last, z0_last
common /c_ixrf_8/ first, dt, i_channel, i_detector, tag4
equivalence (bev(0), tev), (bev(0), sev), (bev(0), fev)

n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,50,60,70,80,90,100), ibranch

10	call get_ixrf( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! ID (get_ixrf advances 'i' +2)
	tag = sev

	if(tag.ne.dp_event_id) go to 10			! ONLY works if "DP" is ONLY known tag!
	ibranch = 2	

!10	call get_ixrf( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
!	if(incomplete.or.(err.eq.1)) return		! ID (get_ixrf advances 'i' +4)
!	ibranch = 2	
!	tag4 = tev

20	call get_ixrf( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! length
	ibranch = 3
	length = tev

30	call get_ixrf( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! X
	ibranch = 4
	x0 = sev

40	call get_ixrf( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! Y
	ibranch = 5
	y0 = sev

50	call get_ixrf( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! detector
	ibranch = 6
	i_detector = sev

60	call get_ixrf( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! dead-time
	ibranch = 7
	dt = fev

70	if(tag.eq.dp_event_id) then
!!70	if(tag4.eq.dp_event_id) then			! only for old 16-bit char tag
		remain = length - 16
!!		remain = length - 18
		ibranch = 8
		first = .true.
		goto 80					! DP event
	endif

	skip = length - 16				! length includes header bytes
!!	skip = length - 18
	if(skip.lt.0) then				! should never go negative
		bad = 1
		idebug = i-16
		return
	endif
	goto 100

!...................................................................................

100	if(i+skip.gt.n_buffer-1) then			! skip unwanted Event
		skip = (i+skip) - n_buffer
		ibranch = 10				! end of buffer, come back to here (100)
		idebug = 92
		return
	endif
	i = i+skip
	ibranch = 1					! next Event block
	goto 10

!...................................................................................
!   Process the 'DP' record ...

80	idebug = length

	if(remain.le.0) then 
!		i = i+remain
		if(remain.ne.0) idebug = 93
		ibranch = 1
		goto 10					! next record
	endif

!   Process payload data (DP)...

	call get_ixrf( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! channel
	ibranch = 9
	i_channel = sev

90	call get_ixrf( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return		! count, multiplicity
	tt = sev

	if(first) then
		x(n) = x0
		y(n) = y0
		e(n) = 0
		count(n) = 0
		ste(n) = i_detector
		tags(n) = 0
		veto(n) = 1
		do k=0,n_fx-1
			fx(k,n) = 0.0
		enddo
		fx(4,n) = dt
		first = .false.
		n = n+1
	endif
	if((channel_on(i_detector).eq.1).and.(n.lt.n_events)) then
		x(n) = x0
		y(n) = y0
		e(n) = i_channel			! all event/channel data follows AFTER
		count(n) = tt				! X0,Y0 set from header
		ste(n) = i_detector
		tags(n) = 0
		veto(n) = 0
		do k=0,n_fx-1
			fx(k,n) = 0.0
		enddo
		n = n+1
	endif

	remain = remain-4
	ibranch = 8
	goto 80
end

!-------------------------------------------------------------------------------

integer function ixrf_accumulate_dtfx( argc, argv)

!DLL_EXPORT ixrf_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.20) then       		   ! needed # args
	ixrf_accumulate_dtfx = 1
	return
endif

call ixrf_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)) )

ixrf_accumulate_dtfx = 0
return
end

!------------------------------------------------------------

SUBROUTINE ixrf_accumulate_dtfx_b( image_mode, x,y,ste,veto,pseudo,fx,n,n_fx, flux_scale, &
		 			xcompress,ycompress, xrange,yrange,n_flux, flux, dwell, &
					dwell_nominal, dead_fraction, weight )

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Only use flux_mode 0:
!
!	Flux:	simply accumulate dwell_nominal as a dwell "flux" map 
!		here in flux[*,*,0] (scaled by: nsls_flux_scale ?)
!				
!	Dwell:	Use Dwell in iXRF_dwell as the assumed constant dwell_nominal (can pass by ref, better than self.dwell)
!
!	Dead_fraction:
!		Use DT in iXRF_fx[4,*] to set dead_fraction[*,*] weighted by dwell (in wight)
!		Later (in da_evt) norm this using the get_dwell() method, which returns the dwell (ms) array.

INTEGER*4 image_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, flux(0:xrange-1,0:yrange-1,0:n_flux), dt, dwell_nominal
REAL*4 dwell(0:xrange-1,0:yrange-1), dead_fraction(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1)

if(n.le.0) return
flux_scale2 = flux_scale

!	Uses simple dwell weighting for multipass. But Perhaps should use estimated counts weights for 
!	multipass merge (as in DAQ device)?

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
			if(pseudo(i).eq.1) then
				flux(x(i),y(i),0) = flux(x(i),y(i),0) + dwell_nominal * flux_scale2
				dwell(x(i),y(i)) = dwell(x(i),y(i)) + dwell_nominal
				dead_fraction(x(i),y(i)) = dead_fraction(x(i),y(i)) + fx(4,i) * dwell_nominal / 100.
				weight(x(i),y(i)) = weight(x(i),y(i)) + dwell_nominal
			endif
		endif
	else
		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.1) then
				flux(0,0,0) = flux(0,0,0) + dwell_nominal * flux_scale2
				dwell(0,0) = dwell(0,0) + dwell_nominal
				dead_fraction(ste(i),0) = dead_fraction(ste(i),0) + fx(4,i) * dwell_nominal / 100.
				weight(ste(i),0) = weight(ste(i),0) + dwell_nominal
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

SUBROUTINE get_midas( ev, i, n_buffer, buff, nbuff, jbuff, incomplete, swap, err)

! Read nbuff bytes from ev, starting at i. Do not exceed n_buffer.
! Load them into buff, starting at jbuff.
! If ev runs out, return incomplete with jbuff recording how
! many actually returned (i.e. next position to fill).
! If swap is set, then rearrange first 4 (2) bytes of long (short half) buff.

INTEGER*4 i,j, n_buffer, nbuff,jbuff, swap, err
INTEGER*1 ev(0:n_buffer-1), buff(0:nbuff-1), it
LOGICAL incomplete

	err = 0
	if(jbuff.ge.nbuff) then
	   err = 1
	   return
	endif

	do j=jbuff,nbuff-1
	   if(i.ge.n_buffer) then
		 incomplete = .true.
		 jbuff = j
		 return
	   endif
	   buff(j) = ev(i)
	   i = i+1
	enddo

	incomplete = .false.
	jbuff = 0
	if(swap.eq.0) return

	if(nbuff.ge.4) then          ! swap first 4 bytes, as in long
	   it = buff(0)
	   buff(0) = buff(3)
	   buff(3) = it
	   it = buff(1)
	   buff(1) = buff(2)
	   buff(2) = it
	else if(nbuff.ge.2) then     ! swap first 2 bytes, as in int
	   it = buff(0)
	   buff(0) = buff(1)
	   buff(1) = it
	endif
	return
end

!-------------------------------------------------------------------------------

integer function init_midas( argc, argv)

!DLL_EXPORT init_midas
INTEGER_ARCH argc, argv(*)

integer*4 remain, jbuff, x0_last, y0_last, z0_last, remain_global 
integer*4 size_bank, temp_energy
INTEGER*1 bev(0:3)
Integer*2 ldone, rdone, ldone2, rdone2

common /c_midas_1/ jbuff, bev, remain, remain_global 
common /c_midas_4/ ldone, rdone, ldone2, rdone2
common /c_midas_7/ x0_last, y0_last, z0_last, temp_energy, size_bank

jbuff = 0
remain = 0
remain_global = 0
ldone = 1
rdone = 1
ldone2 = 1
rdone2 = 1
x0_last = 0
y0_last = 0
z0_last = 0
temp_energy = 0
size_bank = 0

init_midas = 0
return
end

!-------------------------------------------------------------------------------

integer function midas_events1( argc, argv)

!DLL_EXPORT midas_events1
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.24) then 		         ! needed # args
	midas_events1 = 1
	return
endif

call midas_events1_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)) )

midas_events1 = 0
return
end

!-------------------------------------------------------------------------------
!
! First Version of midas_events for midas List-mode, prior to additions for flux.
!
!	flux_mode	0		-- not used? --		fx index 0		Q
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!									 3		Dwell
!									 4		DT corr

SUBROUTINE midas_events1_b( ev,n_buffer, channel_on,nc, e,t,x,y,ste,veto,tags,n_events,n, &
		fx,n_fx, x0,y0, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for: DT, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1)
REAL*4 fx(0:n_fx-1,0:n_events-1)
LOGICAL incomplete

INTEGER*2 nmp_event_id, odb_begin_event_id, odb_end_event_id, back_pixl_id, back_adcx_id, bank_stat_id
PARAMETER (nmp_event_id = 1, odb_begin_event_id = z'8000', odb_end_event_id = z'8001')
PARAMETER (back_pixl_id = z'101', back_adcx_id = z'102', bank_stat_id = z'103')

INTEGER*4 tx, ty, tz, tt, tev
INTEGER*2 de, dtag, pdf, adr, sev, i_channel
REAL*4 fev
integer*4 jbuff, remain, x0_last, y0_last, z0_last, remain_global 
integer*4 size_bank, temp_energy
integer*2 ldone, rdone, ldone2, rdone2
INTEGER*1 bev(0:3)

common /c_midas_1/ jbuff, bev, remain, remain_global 
common /c_midas_4/ ldone, rdone, ldone2, rdone2
common /c_midas_6/ tv_sec, tv_micro
common /c_midas_7/ x0_last, y0_last, z0_last, temp_energy, size_bank
common /c_midas_8/ i_channel
equivalence (bev(0), tev), (bev(0), sev), (bev(0), fev)

n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280), ibranch

10	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! ID (get_midas advances 'i' +2)
	ibranch = 2	
	tag = sev

20	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! trigger
	ibranch = 3	

30	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! serial
	ibranch = 4	

40	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! time
	ibranch = 5

50	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! size
	ibranch = 6
	length = tev

60	if(tag.eq.nmp_event_id) goto 65			! NMP event

	skip = length
	goto 210

!...................................................................................
!   Process the 'NMP' event ...

65	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return         	 ! global bank all size
	ibranch = 7
	length = tev
	remain_global = length

70	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return         	 ! flags
	ibranch = 8
!	flags = tev

80	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return        	 ! bank name (no swap bytes in name)
	ibranch = 9
		
90	if((bev(0).ne.65).or.(bev(1).ne.68).or.(bev(2).ne.67).or.(bev(3).ne.120)) goto 101

	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)		! "ADCx" bank
	if(incomplete.or.(err.eq.1)) return       ! type
	ibranch = 10

100	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! bank size
	ibranch = 11
	length = sev
	size_bank = length 
	tt = modulo(length,8)
	if(tt.ne.0) size_bank = length + (8 - tt)		! round up to 8-byte boundary
	remain = length 
	goto 110

101	if((bev(0).ne.83).or.(bev(1).ne.84).or.(bev(2).ne.65).or.(bev(3).ne.84)) goto 102

	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)		! "STAT" bank
	if(incomplete.or.(err.eq.1)) return       ! type
	ibranch = 13

130	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! bank size
	ibranch = 14
	length = sev
	size_bank = length 
	tt = modulo(length,8)
	if(tt.ne.0) size_bank = length + (8 - tt)		! round up to 8-byte boundary
	remain = length 
	goto 140

102	if((bev(0).ne.80).or.(bev(1).ne.73).or.(bev(2).ne.88).or.(bev(3).ne.76)) goto 103

	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)		! "PIXL" bank
	if(incomplete.or.(err.eq.1)) return       ! type
	ibranch = 22

220	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! bank size
	ibranch = 23
	length = sev
	size_bank = length 
	tt = modulo(length,8)
	if(tt.ne.0) size_bank = length + (8 - tt)		! round up to 8-byte boundary
	remain = length 
	goto 230

103	if((bev(0).ne.68).or.(bev(1).ne.84).or.(bev(2).ne.77).or.(bev(3).ne.69)) goto 180

	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)		! "DTME" bank
	if(incomplete.or.(err.eq.1)) return       ! type
	ibranch = 27

270	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! bank size
	ibranch = 28
	i_channel = 1						! ADC # starts at 1
	length = sev
	size_bank = length 
	tt = modulo(length,8)
	if(tt.ne.0) size_bank = length + (8 - tt)		! round up to 8-byte boundary
	remain = length 
	goto 280

!...................................................................................
!    Read unknown Bank header

180	ibranch = 18
	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! type
	ibranch = 19

190	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! bank size
	length = sev
	size_bank = length 
	tt = modulo(length,8)
	if(tt.ne.0) size_bank = length + (8 - tt)		! round up to 8-byte boundary
	remain = length 
	skip = size_bank

!...................................................................................

200	if(i+skip.gt.n_buffer-1) then			! skip unknown Bank
		skip = (i+skip) - n_buffer
		ibranch = 20
		idebug = 91
		return
	endif
	i = i+skip
	remain_global = remain_global - size_bank - 8
	if(remain_global.lt.8) then
		skip = remain_global
		ibranch = 21
		goto 210				! next Event
	endif
	ibranch = 8
	goto 80						! next Bank

!...................................................................................

210	if(i+skip.gt.n_buffer-1) then			! skip unwanted Event
		skip = (i+skip) - n_buffer
		ibranch = 21				! end of buffer, come back to here (210)
		idebug = 92
		return
	endif
	i = i+skip
	ibranch = 1					! next Event block
	goto 10

!...................................................................................
!   Process the 'ADCx' bank ...

110	if(remain.le.0) then 
		if(remain.ne.0) idebug = 93
		remain_global = remain_global - size_bank - 8
		skip = size_bank - length
		i = i+skip				! skip nulls at end
		if(remain_global.lt.8) then
			skip = remain_global
			ibranch = 21
			goto 210			! next Event
		endif
		ibranch = 8
		goto 80					! next Bank
	endif

!   Process payload data (ADCx)...

	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! energy
	ibranch = 12
	temp_energy = sev

120	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! channel
	adr = sev

	if((temp_energy.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
		if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
			x(n) = x0
			y(n) = y0
			e(n) = temp_energy
			t(n) = 0
			ste(n) = adr
			tags(n) = 0
			veto(n) = 0
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			n = n+1
		endif
	endif

	remain = remain-4
	ibranch = 11
	goto 110

!...................................................................................
!   Process the 'STAT' bank ...

140	if(remain.le.0) then
		if(remain.ne.0) idebug = 94
		remain_global = remain_global - size_bank - 8
		skip = size_bank - length
		i = i+skip				! skip nulls at end
		if(remain_global.lt.8) then
			skip = remain_global
			ibranch = 21
			goto 210			! next Event
		endif
		ibranch = 8
		goto 80					! next Bank
	endif

!   Process payload data (DACx)...

	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! Q
	ibranch = 15
	z0_last = tev

150	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! ICR
	ibranch = 16

160	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! events
	ibranch = 17

170	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! dwell

	if((n.lt.n_events)) then
		x(n) = x0
		y(n) = y0
		e(n) = 0
		t(n) = 0
		ste(n) = 0
		tags(n) = 0
		veto(n) = 1
		do k=1,n_fx-1
			fx(k,n) = 0.0
		enddo
		fx(0,n) = z0_last			! Q
		fx(3,n) = tev				! dwell
		n = n+1
	endif

	remain = remain-16
	ibranch = 14
	goto 140

!...................................................................................
!   Process the 'PIXL' bank ...

230	if(remain.le.0) then
		if(remain.ne.0) idebug = 94
		remain_global = remain_global - size_bank - 8
		skip = size_bank - length
		i = i+skip				! skip nulls at end
		if(remain_global.lt.8) then
			skip = remain_global
			ibranch = 21
			goto 210			! next Event
		endif
		ibranch = 8
		goto 80					! next Bank
	endif

!   Process payload data (STAT)...

	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! DAX X
	ibranch = 24

240	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! DAC Y
	ibranch = 25

250	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! pixel X
	ibranch = 26
	x0 = sev

260	call get_midas( ev, i, n_buffer, bev, 2, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! pixel Y
	y0 = sev

	remain = remain-8
	ibranch = 23
	goto 230

!...................................................................................
!   Process the 'DTME' bank ...

280	if(remain.le.0) then 
		if(remain.ne.0) idebug = 93
		remain_global = remain_global - size_bank - 8
		skip = size_bank - length
		i = i+skip				! skip nulls at end
		if(remain_global.lt.8) then
			skip = remain_global
			ibranch = 21
			goto 210			! next Event
		endif
		ibranch = 8
		goto 80					! next Bank
	endif

!   Process payload data (DTME)...

	call get_midas( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return       ! DT corr
	adr = i_channel
	i_channel = i_channel+1

	if((adr.ge.0).and.(adr.lt.nc)) then
		if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
			x(n) = x0
			y(n) = y0
			e(n) = 0
			t(n) = 0
			ste(n) = adr
			tags(n) = 0
			veto(n) = 1
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			fx(4,n) = fev			! DT corr factor
			n = n+1
		endif
	endif

	remain = remain-4
	ibranch = 28
	goto 280
end

!-------------------------------------------------------------------------------

integer function midas_accumulate_dtfx( argc, argv)

!DLL_EXPORT midas_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.17) then       		   ! needed # args
	midas_accumulate_dtfx = 1
	return
endif

call midas_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)) )

midas_accumulate_dtfx = 0
return
end

!------------------------------------------------------------

SUBROUTINE midas_accumulate_dtfx_b( image_mode, x,y,ste,veto,pseudo,fx,n,n_fx, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,n_flux, flux, dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Only use flux_mode 0:
!
!	Flux:	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale
!				
!	Dwell:	Use fx[3,*] (if not zero) to set Dwell in midas_dwell (can pass by ref, better than self.dwell)

INTEGER*4 image_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, flux(0:xrange-1,0:yrange-1,0:n_flux)
REAL*4 dwell(0:xrange-1,0:yrange-1), dt

if(n.le.0) return
flux_scale2 = flux_scale

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
			if(pseudo(i).eq.1) then
				flux(x(i),y(i),0) = flux(x(i),y(i),0) + fx(0,i) * flux_scale2
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
				endif
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in midas device in spectrum_mode
	endif
enddo

return
end

!-------------------------------------------------------------------------------

SUBROUTINE get_fx( ev, i, n_buffer, buff, nbuff, jbuff, incomplete, swap, err)

! Read nbuff bytes from ev, starting at i. Do not exceed n_buffer.
! Load them into buff, starting at jbuff.
! If ev runs out, return incomplete with jbuff recording how
! many actually returned (i.e. next position to fill).
! If swap is set, then rearrange first 4 bytes of long buff.

INTEGER*4 i,j, n_buffer, nbuff,jbuff, swap, err
INTEGER*1 ev(0:n_buffer-1), buff(0:nbuff-1), it
LOGICAL incomplete

	err = 0
	if(jbuff.ge.nbuff) then
	   err = 1
	   return
	endif

	do j=jbuff,nbuff-1
	   if(i.ge.n_buffer) then
		 incomplete = .true.
		 jbuff = j
		 return
	   endif
	   buff(j) = ev(i)
	   i = i+1
	enddo

	incomplete = .false.
	jbuff = 0

	if((swap.eq.1).and.(nbuff.ge.4)) then          ! swap first 4 bytes, as in long
	   it = buff(0)
	   buff(0) = buff(3)
	   buff(3) = it
	   it = buff(1)
	   buff(1) = buff(2)
	   buff(2) = it
	endif
	return
end

!-------------------------------------------------------------------------------

integer function init_fx( argc, argv)

!DLL_EXPORT init_fx
INTEGER_ARCH argc, argv(*)

integer*4 remain, x0_last, y0_last, z0_last, tcount
integer*4 stats_type, stats_ovf, jbuff, i
INTEGER*1 bev(0:3)
Integer*2 ldone, rdone, ldone2, rdone2, busym
integer*4 u0_last, v0_last, w0_last, fc0, fc1, fc2, fc3, icr, raw, busy, icrm, rawm
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
common /c_fx_6/ dwell_last
common /c_fx_8/ fc0, fc1, fc2, fc3, u0_last, v0_last, w0_last
common /c_fx_9/ icr, raw
common /c_fx_10/ busy
common /c_fx_11/ icrm(0:15), rawm(0:15), busym(0:15)

jbuff = 0
remain = 0
ldone = 1
rdone = 1
ldone2 = 1
rdone2 = 1
x0_last = 0
y0_last = 0
z0_last = 0
u0_last = 0
v0_last = 0
w0_last = 0
stats_type = 0
stats_ovf = 0
tcount = 0
dwell_last = 0.0
fc0 = 0
fc1 = 0
fc2 = 0
fc3 = 0
raw = 0
busy = 0
icr = 0
do i=0,15
	icrm(i) = 0
	rawm(i) = 0
enddo

init_fx = 0
return
end

!-------------------------------------------------------------------------------

integer function falconx_test1( argc, argv)

!DLL_EXPORT falconx_test1
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.3) then 		         ! needed # args
	falconx_test1 = 1
	return
endif

call falconx_test1_b( %val(argv(1)), %val(argv(2)), %val(argv(3)) )

falconx_test1 = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE falconx_test1_b( a,b,c )

INTEGER*4 a,b,c
integer*4 x0_last, y0_last, z0_last, tcount
integer*4 stats_type, stats_ovf

common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount

x0_last = a
y0_last = b
z0_last = c
return
end

!-------------------------------------------------------------------------------

integer function falconx_test2( argc, argv)

!DLL_EXPORT falconx_test2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.6) then 		         ! needed # args
	falconx_test2 = 1
	return
endif

call falconx_test2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
		 %val(argv(4)), %val(argv(5)), %val(argv(6)) )

falconx_test2 = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE falconx_test2_b( a,b,c, d,e,f )

INTEGER*4 a,b,c, d,e,f

integer*4 remain, x0_last, y0_last, z0_last, tcount
integer*4 stats_type, stats_ovf, jbuff, tev
INTEGER*1 bev(0:3)
Integer*2 ldone, rdone, ldone2, rdone2

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
equivalence (bev(0), tev)

a = x0_last
b = y0_last
c = z0_last
d = jbuff
e = tev
f = remain

return
end

!-------------------------------------------------------------------------------

integer function falconx_events4( argc, argv)

!DLL_EXPORT falconx_events4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.35) then 		         ! needed # args
	falconx_events4 = 1
	return
endif

call falconx_events4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), %val(argv(29)), %val(argv(30)), &
	 %val(argv(31)), %val(argv(32)), %val(argv(33)), %val(argv(34)), %val(argv(35)) )

falconx_events4 = 0
return
end

!-------------------------------------------------------------------------------
!
! First Version of falconx_events for FalconX List-mode, after additions for flux.
!
!	flux_mode	0		-- not used? --		fx index 0		selected FCn
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!			3		FC2 h/w				 3		Dwell
!			4		FC3 h/w				 4		lost counts
!									 5		raw counts

SUBROUTINE falconx_events4_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,idebug, version,tick )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for: DT, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, jbuff, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx, flux_mode, version, as_version, si_version
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0,z0,u0,v0,w0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1), z(0:n_events-1), u(0:n_events-1), v(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1), w(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
REAL*4 fx(0:n_fx-1,0:n_events-1), tick
LOGICAL incomplete

INTEGER*4 data_type_mask, data_type_offset
INTEGER*2 pulse1_data_type, pulse2_data_type, position_data_type, spatial1_data_type, error_data_type
INTEGER*4 clock1_length, gstats1_length, position_length, spatial1_length, periodic_length
INTEGER*4 gate1_length, error_length, pulse1_length, pulse2_length
INTEGER*4 pulse1_valid_mask, pulse1_valid_offset, pulse1_energy_mask, pulse1_energy_offset
INTEGER*4 pulse1_adr_offset, pulse1_adr_mask
INTEGER*4 pulse2_toa_mask
INTEGER*4 position_type_mask, position_type_offset, position_data_mask, spatial1_rate_bit
INTEGER*4 spatial1_type_mask, spatial1_type_offset, spatial1_data_mask, spatial1_rate_mask
INTEGER*4 spatial1_sample_msb_offset, spatial1_rate_offset
INTEGER*4 spatial1_sample_countl_type, spatial1_sample_countm_type
INTEGER*4 error_type_mask, error_type_offset, error_analogue_status_type, error_overflow_type
INTEGER*4 error_saturate_mask, error_saturate_offset, error_time_stamp_mask

INTEGER*4 position_axis0_type, position_axis1_type, position_axis2_type, position_axis3_type 
INTEGER*4 position_axis4_type, position_axis5_type, spatial1_sample_msb_mask
INTEGER*4 spatial1_time_stamp_type, spatial1_erasure_count_type, spatial1_raw_icr_type, spatial1_est_icr_type
INTEGER*4 spatial1_generic_count1_type, spatial1_generic_count2_type, spatial1_generic_count3_type, spatial1_generic_count4_type
INTEGER*4 pa_sign_bit_mask4, pa_sign_extend

! NOTE: for now the "E" value is defined as top 13 bits of 24 bit 'amplitude' (for raw FalconX data)
!       for AS/XFM data, this will be shifted down 4 bits, so E becomes top 13 bits of 20 bit data. 

parameter (data_type_mask = z'F0000000', data_type_offset=-28)
parameter (pulse1_data_type=0, pulse2_data_type=1, position_data_type=12, spatial1_data_type=11, error_data_type=15)
parameter (pulse1_valid_mask = z'08000000', pulse1_valid_offset = -27, spatial1_data_mask = z'00FFFFFF')
parameter (pulse2_toa_mask = z'000FFFFF')
parameter (position_type_mask =	z'0F000000', position_type_offset = -24, position_data_mask = z'00FFFFFF')
parameter (spatial1_type_mask = z'0F000000', spatial1_type_offset = -24)
parameter (spatial1_sample_msb_offset = 24)
parameter (spatial1_sample_countl_type = 0, spatial1_sample_countm_type = 1)
parameter (error_type_mask = z'0F000000', error_type_offset = -24, error_analogue_status_type = 0, error_overflow_type = 1)
parameter (error_saturate_mask = z'000C0000', error_saturate_offset = -18, error_time_stamp_mask = z'00FFFFFF')
parameter (pa_sign_bit_mask4 = z'00800000', pa_sign_extend = z'FF000000')

! Version 0.6.0 (use A postfix) --> SI version 600

INTEGER*4 pulse1_energy_maskA, pulse1_energy_offsetA, pulse1_adr_offsetA, pulse1_adr_maskA
INTEGER*4 position_lengthA, spatial1_lengthA, spatial1_rate_maskA, spatial1_sample_msb_maskA
INTEGER*4 position_axis0_typeA, position_axis1_typeA, position_axis2_typeA, position_axis3_typeA 
INTEGER*4 position_axis4_typeA, position_axis5_typeA, spatial1_rate_bitA
INTEGER*4 spatial1_time_stamp_typeA, spatial1_erasure_count_typeA
INTEGER*4 spatial1_est_icr_typeA, spatial1_raw_icr_typeA, spatial1_rate_offsetA
INTEGER*4 spatial1_generic_count1_typeA, spatial1_generic_count2_typeA, spatial1_generic_count3_typeA, spatial1_generic_count4_typeA

parameter (position_lengthA = 7, spatial1_lengthA = 9, spatial1_sample_msb_maskA = z'000000FF')
parameter (pulse1_energy_maskA = z'00FFF800', pulse1_energy_offsetA = -11)
parameter (pulse1_adr_offsetA = 0, pulse1_adr_maskA = 0, spatial1_rate_maskA = z'00FFFFFF')
parameter (position_axis0_typeA = 0, position_axis1_typeA = 1, position_axis2_typeA = 2, position_axis3_typeA = 3)
parameter (position_axis4_typeA = 4, position_axis5_typeA = 5, spatial1_rate_bitA = z'00000000')
parameter (spatial1_erasure_count_typeA = 2, spatial1_raw_icr_typeA = 14, spatial1_rate_offsetA = 0)
parameter (spatial1_generic_count1_typeA = 4, spatial1_generic_count2_typeA = 5, spatial1_generic_count3_typeA = 6)
parameter (spatial1_est_icr_typeA = 3, spatial1_generic_count4_typeA = 7, spatial1_time_stamp_typeA = 15)

! Version 0.8.1 (use B postfix) --> version 801

INTEGER*4 position_lengthB, spatial1_lengthB
INTEGER*4 gate1_lengthB, error_lengthB, pulse1_lengthB, pulse2_lengthB, spatial1_sample_msb_maskB
INTEGER*4 position_axis0_typeB, position_axis1_typeB, position_axis2_typeB, position_axis3_typeB 
INTEGER*4 position_axis4_typeB, position_axis5_typeB
INTEGER*4 spatial1_time_stamp_typeB, spatial1_erasure_count_typeB, spatial1_est_icr_typeB, spatial1_raw_icr_typeB
INTEGER*4 spatial1_generic_count1_typeB, spatial1_generic_count2_typeB, spatial1_generic_count3_typeB, spatial1_generic_count4_typeB

parameter (position_lengthB = 7, spatial1_lengthB = 12, spatial1_sample_msb_maskB = z'000000FF')
parameter (gate1_lengthB = 1, error_lengthB = 1, pulse1_lengthB = 1, pulse2_lengthB = 1)
parameter (position_axis0_typeB = 0, position_axis1_typeB = 1, position_axis2_typeB = 2, position_axis3_typeB = 3)
parameter (position_axis4_typeB = 4, position_axis5_typeB = 5)
parameter (spatial1_erasure_count_typeB = 2, spatial1_est_icr_typeB = 4, spatial1_raw_icr_typeB = 5)
parameter (spatial1_generic_count1_typeB = 6, spatial1_generic_count2_typeB = 7, spatial1_generic_count3_typeB = 8)
parameter (spatial1_generic_count4_typeB = 9, spatial1_time_stamp_typeB = 15)

! Version 0.8.7 (use C postfix) --> version 807, increased 'spatial1_length' from 11 to 12, and added test of tag bits

INTEGER*4 position_lengthC, spatial1_lengthC
INTEGER*4 gate1_lengthC, error_lengthC, pulse1_lengthC, pulse2_lengthC, spatial1_sample_msb_maskC
INTEGER*4 position_axis0_typeC, position_axis1_typeC, position_axis2_typeC, position_axis3_typeC 
INTEGER*4 position_axis4_typeC, position_axis5_typeC
INTEGER*4 spatial1_time_stamp_typeC, spatial1_erasure_count_typeC, spatial1_est_icr_typeC, spatial1_raw_icr_typeC
INTEGER*4 spatial1_generic_count1_typeC, spatial1_generic_count2_typeC, spatial1_generic_count3_typeC, spatial1_generic_count4_typeC

parameter (position_lengthC = 7, spatial1_lengthC = 12, spatial1_sample_msb_maskC = z'00FFFFFF')
parameter (gate1_lengthC = 1, error_lengthC = 1, pulse1_lengthC = 1, pulse2_lengthC = 1)
parameter (position_axis0_typeC = 0, position_axis1_typeC = 1, position_axis2_typeC = 2, position_axis3_typeC = 3)
parameter (position_axis4_typeC = 4, position_axis5_typeC = 5)
parameter (spatial1_erasure_count_typeC = 2, spatial1_est_icr_typeC = 4, spatial1_raw_icr_typeC = 5)
parameter (spatial1_generic_count1_typeC = 6, spatial1_generic_count2_typeC = 7, spatial1_generic_count3_typeC = 8)
parameter (spatial1_generic_count4_typeC = 9, spatial1_time_stamp_typeC = 15)

! Version AS/XFM (use P postfix) --> AS version 100
!			increased 'spatial1_length' from 12 to 74 to allow up to 16 detectors
!			reduced OCR, ICR mask to 19 bits, cater for LSB, MSB and 4 'adr' bits
!			erased, saturated, veto only using single word for now.

INTEGER*4 pulse1_energy_maskP, pulse1_energy_offsetP, pulse1_adr_offsetP, pulse1_adr_maskP
INTEGER*4 spatial1_rate_maskP, spatial1_rate_bitP, spatial1_lengthP, spatial1_adr_maskP, spatial1_rate_offsetP

parameter (pulse1_energy_maskP = z'000FFF80', pulse1_energy_offsetP = -7, spatial1_rate_bitP = z'00080000')
parameter (pulse1_adr_offsetP = -20, pulse1_adr_maskP = z'00F00000', spatial1_rate_maskP = z'0007FFFF')
parameter (spatial1_lengthP = 74, spatial1_adr_maskP = z'00F00000', spatial1_rate_offsetP = 19)

! --------------

INTEGER*4 tx, ty, tz, tt, tev, tcount
INTEGER*2 de, dtag, pdf, adr, busym
integer*4 remain, x0_last, y0_last, z0_last, u0_last, v0_last, w0_last, fc0, fc1, fc2, fc3
integer*4 stats_type, stats_ovf, icr, raw, busy, icrm, rawm
integer*2 ldone, rdone, ldone2, rdone2
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_6/ dwell_last
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
common /c_fx_8/ fc0, fc1, fc2, fc3, u0_last, v0_last, w0_last
common /c_fx_9/ icr, raw
common /c_fx_10/ busy
common /c_fx_11/ icrm(0:15), rawm(0:15), busym(0:15)
equivalence (bev(0), tev)

as_version = version / 1000			! AS version part (truncate off SI part)
si_version = version - 1000*as_version		! SI version part

if(si_version.ge.807) then
	position_length = position_lengthC
	spatial1_length = spatial1_lengthC 
	spatial1_sample_msb_mask = spatial1_sample_msb_maskC
	position_axis0_type = position_axis0_typeC
	position_axis1_type = position_axis1_typeC
	position_axis2_type = position_axis2_typeC
	position_axis3_type = position_axis3_typeC
	position_axis4_type = position_axis4_typeC
	position_axis5_type = position_axis5_typeC
	spatial1_est_icr_type = spatial1_est_icr_typeC
	spatial1_raw_icr_type = spatial1_raw_icr_typeC
	spatial1_erasure_count_type = spatial1_erasure_count_typeC
	spatial1_generic_count1_type = spatial1_generic_count1_typeC
	spatial1_generic_count2_type = spatial1_generic_count2_typeC
	spatial1_generic_count3_type = spatial1_generic_count3_typeC
	spatial1_generic_count4_type = spatial1_generic_count4_typeC
	spatial1_est_icr_type = spatial1_est_icr_typeC
	spatial1_time_stamp_type = spatial1_time_stamp_typeC 
else if(si_version.ge.801) then
	position_length = position_lengthB 
	spatial1_length = spatial1_lengthB 
	spatial1_sample_msb_mask = spatial1_sample_msb_maskB
	position_axis0_type = position_axis0_typeB
	position_axis1_type = position_axis1_typeB
	position_axis2_type = position_axis2_typeB
	position_axis3_type = position_axis3_typeB
	position_axis4_type = position_axis4_typeB
	position_axis5_type = position_axis5_typeB
	spatial1_est_icr_type = spatial1_est_icr_typeB
	spatial1_raw_icr_type = spatial1_raw_icr_typeB
	spatial1_erasure_count_type = spatial1_erasure_count_typeB
	spatial1_generic_count1_type = spatial1_generic_count1_typeB
	spatial1_generic_count2_type = spatial1_generic_count2_typeB
	spatial1_generic_count3_type = spatial1_generic_count3_typeB
	spatial1_generic_count4_type = spatial1_generic_count4_typeB
	spatial1_est_icr_type = spatial1_est_icr_typeB
	spatial1_time_stamp_type = spatial1_time_stamp_typeB 
else
	position_length = position_lengthA 
	spatial1_length = spatial1_lengthA
	spatial1_sample_msb_mask = spatial1_sample_msb_maskA
	position_axis0_type = position_axis0_typeA
	position_axis1_type = position_axis1_typeA
	position_axis2_type = position_axis2_typeA
	position_axis3_type = position_axis3_typeA
	position_axis4_type = position_axis4_typeA
	position_axis5_type = position_axis5_typeA
	spatial1_est_icr_type = spatial1_est_icr_typeA
	spatial1_raw_icr_type = spatial1_raw_icr_typeA
	spatial1_erasure_count_type = spatial1_erasure_count_typeA
	spatial1_generic_count1_type = spatial1_generic_count1_typeA
	spatial1_generic_count2_type = spatial1_generic_count2_typeA
	spatial1_generic_count3_type = spatial1_generic_count3_typeA
	spatial1_generic_count4_type = spatial1_generic_count4_typeA
	spatial1_est_icr_type = spatial1_est_icr_typeA
	spatial1_time_stamp_type = spatial1_time_stamp_typeA 
endif

if(as_version.ge.100) then
	pulse1_energy_mask = pulse1_energy_maskP
	pulse1_energy_offset = pulse1_energy_offsetP
	pulse1_adr_offset = pulse1_adr_offsetP
	pulse1_adr_mask = pulse1_adr_maskP
	spatial1_rate_mask = spatial1_rate_maskP
	spatial1_rate_bit = spatial1_rate_bitP
	spatial1_length = spatial1_lengthP
	spatial1_rate_offset = spatial1_rate_offsetP
else
	pulse1_energy_mask = pulse1_energy_maskA
	pulse1_energy_offset = pulse1_energy_offsetA
	pulse1_adr_offset = pulse1_adr_offsetA
	pulse1_adr_mask = pulse1_adr_maskA
	spatial1_rate_mask = spatial1_rate_maskA
	spatial1_rate_bit = spatial1_rate_bitA
	spatial1_length = spatial1_lengthA
	spatial1_rate_offset = spatial1_rate_offsetA
endif

if(n_fx.lt.6) return
n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,10,10,10,10,10,10,100), ibranch

10  call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word (get_fx advances 'i' +4)
	
	tag = ishft( iand( tev, data_type_mask), data_type_offset)

11	if(tag.eq.pulse1_data_type) then		! Pulse 1 event

		pdf = ishft( iand( tev, pulse1_valid_mask ), pulse1_valid_offset )	! invalid
		de = ishft( iand( tev, pulse1_energy_mask ), pulse1_energy_offset )	! energy
		adr = ishft( iand( tev, pulse1_adr_mask), pulse1_adr_offset)		! channel

		if((pdf.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = de
				t(n) = 0
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor valid bit
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				n = n+1
			endif
		endif

		skip = 0
		goto 100
	endif

	if(tag.eq.position_data_type) then		! position event
		length = (position_length-1)*4						! remaining length
		remain = length

		x0_last = x0
		x0 = iand( tev, position_data_mask )					! x

		ibranch = 2
		goto 20
	endif

	if(tag.eq.spatial1_data_type) then		! spatial event
		length = (spatial1_length-1) *4
		remain = length

		tcount = iand( tev, spatial1_data_mask)					! sample LSB

		ibranch = 3
		goto 30
	endif

	if(tag.eq.error_data_type) then			! error1 event

		tx = ishft( iand( tev, error_type_mask ), error_type_offset )		! type
		if(tx.eq.error_analogue_status_type) then
			tt = ishft( iand( tev, error_saturate_mask), error_saturate_offset )	! saturation (not used from 0.8.1)
			bad = bad + 1								! sat'n data now in stats
			idebug = 1000 + tt
		else if(tx.eq.error_overflow_type) then
			tt = iand( tev, error_time_stamp_mask)					! overflow
			bad = bad + 1
			idebug = 1000
		endif

		skip = 0
		goto 100
	endif

	skip = 0					! skip done already for 1 word (in get_fx)

100	if(i+skip.gt.n_buffer-1) then			! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 91
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'position' event ...

20	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 92
		goto 10
	endif

!   Process payload data (encoder) ...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tag = ishft( iand( tev, data_type_mask), data_type_offset)
	if(tag.ne.position_data_type) goto 11						! not a position event

	tx = ishft( iand( tev, position_type_mask ), position_type_offset )		! type
	if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)

	if(tx.eq.position_axis1_type) then
		y0_last = y0
		y0 = iand( tev, position_data_mask )					! y

	else if(tx.eq.position_axis2_type) then
		z0_last = z0
		z0 = iand( tev, position_data_mask )					! z

	else if(tx.eq.position_axis3_type) then
		u0_last = u0
		u0 = iand( tev, position_data_mask )					! u

	else if(tx.eq.position_axis4_type) then
		v0_last = v0
		v0 = iand( tev, position_data_mask )					! v

	else if(tx.eq.position_axis5_type) then
		w0_last = w0
		w0 = iand( tev, position_data_mask )					! w
	endif	
	remain = remain-4
	goto 20

!...................................................................................
!   Process the 'spatial' event ...

30	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 93
		goto 10
	endif

!   Process payload data (stats)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tag = ishft( iand( tev, data_type_mask), data_type_offset)
	if(tag.ne.spatial1_data_type) goto 11						! not a spatial event

	tx = ishft( iand( tev, spatial1_type_mask ), spatial1_type_offset )		! type

	if(tx.eq.spatial1_sample_countm_type) then
!		tcount = ior( tcount, ishft( iand( tev, spatial1_sample_msb_mask), spatial1_sample_msb_offset))  ! samples 
		dwell_last = tcount * tick
		tcount = iand( tev, spatial1_sample_msb_mask)				! samples 
		dwell_last = dwell_last + tcount * tick * 2.**spatial1_sample_msb_offset

	else if(tx.eq.spatial1_generic_count1_type) then
		fc0 = iand( tev, spatial1_data_mask)					! FC0

	else if(tx.eq.spatial1_generic_count2_type) then
		fc1 = iand( tev, spatial1_data_mask)					! FC1

	else if(tx.eq.spatial1_generic_count3_type) then
		fc2 = iand( tev, spatial1_data_mask)					! FC2

	else if(tx.eq.spatial1_generic_count4_type) then
		fc3 = iand( tev, spatial1_data_mask)					! FC3

	else if(tx.eq.spatial1_est_icr_type) then
		adr = ishft( iand( tev, pulse1_adr_mask), pulse1_adr_offset)
		m = iand( tev, spatial1_rate_bit)
		if((adr.ge.0).and.(adr.lt.15)) then
			j = iand( tev, spatial1_rate_mask)				! ICR (estimated ICR count)
			if(m.ne.0) j = ishft( j, spatial1_rate_offset)
			icrm(adr) = ior( icrm(adr), j)
			busym(adr) = 1
		endif

	else if(tx.eq.spatial1_raw_icr_type) then
		adr = ishft( iand( tev, pulse1_adr_mask), pulse1_adr_offset)
		m = iand( tev, spatial1_rate_bit)
		if((adr.ge.0).and.(adr.lt.15)) then
			j = iand( tev, spatial1_rate_mask)				! Raw (actual OCR count)
			if(m.ne.0) j = ishft( j, spatial1_rate_offset)
			rawm(adr) = ior( rawm(adr), j)
			busym(adr) = 1
		endif

	else if(tx.eq.spatial1_time_stamp_type) then

		do k=0,15
		    if((n.lt.n_events).and.((k.eq.0).or.(busym(k).eq.1))) then
			do j=0,n_fx-1
				fx(j,n) = 0.0
			enddo
			x(n) = x0_last	
			y(n) = y0_last							! add a pseudo event
			z(n) = z0_last							! for previous pixel address
			u(n) = u0_last
			v(n) = v0_last
			w(n) = w0_last
			e(n) = 0
			t(n) = 0
			tags(n) = 0
			veto(n) = 1							! a pseudo event for counters, losses
			if(k.eq.0) then
				if(flux_mode.eq.1) fx(0,n) = fc0
				if(flux_mode.eq.2) fx(0,n) = fc1
				if(flux_mode.eq.3) fx(0,n) = fc2
				if(flux_mode.eq.4) fx(0,n) = fc3
				fx(1,n) = fc0
				fx(2,n) = fc1
				if(dwell_last.gt.0) then
					fx(3,n) = dwell_last				! dwell (ms), assumes 4 ns clock
				endif
			endif
			if(busym(k).eq.1) then
				ste(n) = k
				if((icrm(k).gt.0).and.(icrm(k).ge.rawm(k))) then
					fx(4,n) = float(icrm(k)) - float(rawm(k))	! lost counts
					fx(5,n) = float(rawm(k))			! raw (OCR counts weight)
				endif
			endif
			busym(k) = 0
			rawm(k)= 0
			icrm(k) = 0
			n = n+1
		    endif
		enddo
	endif
	remain = remain-4
	goto 30
end

!-------------------------------------------------------------------------------

integer function falconx_events3( argc, argv)

!DLL_EXPORT falconx_events3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.34) then 		         ! needed # args
	falconx_events3 = 1
	return
endif

call falconx_events3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), %val(argv(29)), %val(argv(30)), &
	 %val(argv(31)), %val(argv(32)), %val(argv(33)), %val(argv(34)) )

falconx_events3 = 0
return
end

!-------------------------------------------------------------------------------
!
! First Version of falconx_events for FalconX List-mode, after additions for flux.
!
!	flux_mode	0		-- not used? --		fx index 0		selected FCn
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!			3		FC2 h/w				 3		Dwell
!			4		FC3 h/w				 4		lost counts
!									 5		raw counts

SUBROUTINE falconx_events3_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,idebug, version )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for: DT, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, jbuff, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx, flux_mode, version 
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0,z0,u0,v0,w0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1), z(0:n_events-1), u(0:n_events-1), v(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1), w(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
REAL*4 fx(0:n_fx-1,0:n_events-1), tick
LOGICAL incomplete

INTEGER*4 data_type_mask, data_type_offset
INTEGER*2 pulse1_data_type, pulse2_data_type, position_data_type, spatial1_data_type, error_data_type
INTEGER*4 clock1_length, gstats1_length, position_length, spatial1_length, periodic_length
INTEGER*4 gate1_length, error_length, pulse1_length, pulse2_length
INTEGER*4 pulse1_valid_mask, pulse1_valid_offset, pulse1_energy_mask, pulse1_energy_offset
INTEGER*4 pulse1_adr_offset, pulse1_adr_mask
INTEGER*4 pulse2_toa_mask
INTEGER*4 position_type_mask, position_type_offset, position_data_mask
INTEGER*4 spatial1_type_mask, spatial1_type_offset, spatial1_data_mask, spatial1_rate_mask
INTEGER*4 spatial1_sample_msb_mask, spatial1_sample_msb_offset
INTEGER*4 spatial1_sample_countl_type, spatial1_sample_countm_type
INTEGER*4 error_type_mask, error_type_offset, error_analogue_status_type, error_overflow_type
INTEGER*4 error_saturate_mask, error_saturate_offset, error_time_stamp_mask

INTEGER*4 position_axis0_type, position_axis1_type, position_axis2_type, position_axis3_type 
INTEGER*4 position_axis4_type, position_axis5_type
INTEGER*4 spatial1_time_stamp_type, spatial1_erasure_count_type, spatial1_raw_icr_type, spatial1_est_icr_type
INTEGER*4 spatial1_generic_count1_type, spatial1_generic_count2_type, spatial1_generic_count3_type, spatial1_generic_count4_type
INTEGER*4 pa_sign_bit_mask4, pa_sign_extend

! NOTE: for now the "E" value is defined as top 13 bits of 24 bit 'amplitude' (for raw FalconX data)
!       for AS/XFM data, this will be shifted down 4 bits, so E becomes top 13 bits of 20 bit data. 

parameter (data_type_mask = z'F0000000', data_type_offset=-28)
parameter (pulse1_data_type=0, pulse2_data_type=1, position_data_type=12, spatial1_data_type=11, error_data_type=15)
parameter (pulse1_valid_mask = z'08000000', pulse1_valid_offset = -27, spatial1_data_mask = z'00FFFFFF')
parameter (pulse2_toa_mask = z'000FFFFF')
parameter (position_type_mask =	z'0F000000', position_type_offset = -24, position_data_mask = z'00FFFFFF')
parameter (spatial1_type_mask = z'0F000000', spatial1_type_offset = -24)
parameter (spatial1_sample_msb_mask = z'00FFFFFF', spatial1_sample_msb_offset = 24)
parameter (spatial1_sample_countl_type = 0, spatial1_sample_countm_type = 1)
parameter (error_type_mask = z'0F000000', error_type_offset = -24, error_analogue_status_type = 0, error_overflow_type = 1)
parameter (error_saturate_mask = z'000C0000', error_saturate_offset = -18, error_time_stamp_mask = z'00FFFFFF')
parameter (pa_sign_bit_mask4 = z'00800000', pa_sign_extend = z'FF000000')

! Version AS/XFM (use C postfix) --> version 100,000

INTEGER*4 pulse1_energy_maskC, pulse1_energy_offsetC, pulse1_adr_offsetC, pulse1_adr_maskC
INTEGER*4 spatial1_rate_maskC

parameter (pulse1_energy_maskC = z'000FFF80', pulse1_energy_offsetC = -7)
parameter (pulse1_adr_offsetC = -20, pulse1_adr_maskC = z'00F00000', spatial1_rate_maskC = z'000FFFFF')

! Version 0.6.0 (use A postfix) --> version 600

INTEGER*4 pulse1_energy_maskA, pulse1_energy_offsetA, pulse1_adr_offsetA, pulse1_adr_maskA
INTEGER*4 position_lengthA, spatial1_lengthA, spatial1_rate_maskA
INTEGER*4 position_axis0_typeA, position_axis1_typeA, position_axis2_typeA, position_axis3_typeA 
INTEGER*4 position_axis4_typeA, position_axis5_typeA
INTEGER*4 spatial1_time_stamp_typeA, spatial1_erasure_count_typeA
INTEGER*4 spatial1_est_icr_typeA, spatial1_raw_icr_typeA
INTEGER*4 spatial1_generic_count1_typeA, spatial1_generic_count2_typeA, spatial1_generic_count3_typeA, spatial1_generic_count4_typeA

parameter (position_lengthA = 7, spatial1_lengthA = 9)
parameter (pulse1_energy_maskA = z'00FFF800', pulse1_energy_offsetA = -11)
parameter (pulse1_adr_offsetA = 0, pulse1_adr_maskA = 0, spatial1_rate_maskA = z'00FFFFFF')
parameter (position_axis0_typeA = 0, position_axis1_typeA = 1, position_axis2_typeA = 2, position_axis3_typeA = 3)
parameter (position_axis4_typeA = 4, position_axis5_typeA = 5)
parameter (spatial1_erasure_count_typeA = 2, spatial1_raw_icr_typeA = 14)
parameter (spatial1_generic_count1_typeA = 4, spatial1_generic_count2_typeA = 5, spatial1_generic_count3_typeA = 6)
parameter (spatial1_est_icr_typeA = 3, spatial1_generic_count4_typeA = 7, spatial1_time_stamp_typeA = 15)

! Version 0.8.3 (use B postfix) --> version 803
! Version 0.9.0 (use B postfix) --> version 900, increased 'spatial1_lengthB' from 11 to 12, and added test of tag bits
!                                                increased 'spatial1_lengthB' from 12 to 42 to allow up to 16 detectors
!                if OCR, ICR get split into multiple word-pairs for slow dwell/high rates, then this needs to be bigger

INTEGER*4 position_lengthB, spatial1_lengthB
INTEGER*4 gate1_lengthB, error_lengthB, pulse1_lengthB, pulse2_lengthB
INTEGER*4 position_axis0_typeB, position_axis1_typeB, position_axis2_typeB, position_axis3_typeB 
INTEGER*4 position_axis4_typeB, position_axis5_typeB
INTEGER*4 spatial1_time_stamp_typeB, spatial1_erasure_count_typeB, spatial1_est_icr_typeB, spatial1_raw_icr_typeB
INTEGER*4 spatial1_generic_count1_typeB, spatial1_generic_count2_typeB, spatial1_generic_count3_typeB, spatial1_generic_count4_typeB

parameter (position_lengthB = 7, spatial1_lengthB = 42)
parameter (gate1_lengthB = 1, error_lengthB = 1, pulse1_lengthB = 1, pulse2_lengthB = 1)
parameter (position_axis0_typeB = 0, position_axis1_typeB = 1, position_axis2_typeB = 2, position_axis3_typeB = 3)
parameter (position_axis4_typeB = 4, position_axis5_typeB = 5)
parameter (spatial1_erasure_count_typeB = 2, spatial1_est_icr_typeB = 4, spatial1_raw_icr_typeB = 5)
parameter (spatial1_generic_count1_typeB = 6, spatial1_generic_count2_typeB = 7, spatial1_generic_count3_typeB = 8)
parameter (spatial1_generic_count4_typeB = 9, spatial1_time_stamp_typeB = 15)

! --------------

INTEGER*4 tx, ty, tz, tt, tev, tcount
INTEGER*2 de, dtag, pdf, adr, busym
integer*4 remain, x0_last, y0_last, z0_last, u0_last, v0_last, w0_last, fc0, fc1, fc2, fc3
integer*4 stats_type, stats_ovf, icr, raw, busy, icrm, rawm
integer*2 ldone, rdone, ldone2, rdone2
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_6/ dwell_last
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
common /c_fx_8/ fc0, fc1, fc2, fc3, u0_last, v0_last, w0_last
common /c_fx_9/ icr, raw
common /c_fx_10/ busy
common /c_fx_11/ icrm(0:15), rawm(0:15), busym(0:15)
equivalence (bev(0), tev)

tick = 4.0e-6

if(version.ge.100000) then
	pulse1_energy_mask = pulse1_energy_maskC
	pulse1_energy_offset = pulse1_energy_offsetC
	pulse1_adr_offset = pulse1_adr_offsetC
	pulse1_adr_mask = pulse1_adr_maskC
	spatial1_rate_mask = spatial1_rate_maskC
else
	pulse1_energy_mask = pulse1_energy_maskA
	pulse1_energy_offset = pulse1_energy_offsetA
	pulse1_adr_offset = pulse1_adr_offsetA
	pulse1_adr_mask = pulse1_adr_maskA
	spatial1_rate_mask = spatial1_rate_maskA
endif

if(version.ge.800) then
	position_length = position_lengthB 
	spatial1_length = spatial1_lengthB 
	position_axis0_type = position_axis0_typeB
	position_axis1_type = position_axis1_typeB
	position_axis2_type = position_axis2_typeB
	position_axis3_type = position_axis3_typeB
	position_axis4_type = position_axis4_typeB
	position_axis5_type = position_axis5_typeB
	spatial1_est_icr_type = spatial1_est_icr_typeB
	spatial1_raw_icr_type = spatial1_raw_icr_typeB
	spatial1_erasure_count_type = spatial1_erasure_count_typeB
	spatial1_generic_count1_type = spatial1_generic_count1_typeB
	spatial1_generic_count2_type = spatial1_generic_count2_typeB
	spatial1_generic_count3_type = spatial1_generic_count3_typeB
	spatial1_generic_count4_type = spatial1_generic_count4_typeB
	spatial1_est_icr_type = spatial1_est_icr_typeB
	spatial1_time_stamp_type = spatial1_time_stamp_typeB 
else
	position_length = position_lengthA 
	spatial1_length = spatial1_lengthA
	position_axis0_type = position_axis0_typeA
	position_axis1_type = position_axis1_typeA
	position_axis2_type = position_axis2_typeA
	position_axis3_type = position_axis3_typeA
	position_axis4_type = position_axis4_typeA
	position_axis5_type = position_axis5_typeA
	spatial1_est_icr_type = spatial1_est_icr_typeA
	spatial1_raw_icr_type = spatial1_raw_icr_typeA
	spatial1_erasure_count_type = spatial1_erasure_count_typeA
	spatial1_generic_count1_type = spatial1_generic_count1_typeA
	spatial1_generic_count2_type = spatial1_generic_count2_typeA
	spatial1_generic_count3_type = spatial1_generic_count3_typeA
	spatial1_generic_count4_type = spatial1_generic_count4_typeA
	spatial1_est_icr_type = spatial1_est_icr_typeA
	spatial1_time_stamp_type = spatial1_time_stamp_typeA 
endif

if(n_fx.lt.6) return
n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,10,10,10,10,10,10,100), ibranch

10  call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word (get_fx advances 'i' +4)
	
	tag = ishft( iand( tev, data_type_mask), data_type_offset)

11	if(tag.eq.pulse1_data_type) then		! Pulse 1 event

		pdf = ishft( iand( tev, pulse1_valid_mask ), pulse1_valid_offset )	! invalid
		de = ishft( iand( tev, pulse1_energy_mask ), pulse1_energy_offset )	! energy
		adr = ishft( iand( tev, pulse1_adr_mask), pulse1_adr_offset)		! channel

		if((pdf.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = de
				t(n) = 0
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor valid bit
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				n = n+1
			endif
		endif

		skip = 0
		goto 100
	endif

	if(tag.eq.position_data_type) then		! position event
		length = (position_length-1)*4						! remaining length
		remain = length

		x0_last = x0
		x0 = iand( tev, position_data_mask )					! x

		ibranch = 2
		goto 20
	endif

	if(tag.eq.spatial1_data_type) then		! spatial event
		length = (spatial1_length-1) *4
		remain = length

		tcount = iand( tev, spatial1_data_mask)					! sample LSB

		ibranch = 3
		goto 30
	endif

	if(tag.eq.error_data_type) then			! error1 event

		tx = ishft( iand( tev, error_type_mask ), error_type_offset )		! type
		if(tx.eq.error_analogue_status_type) then
			tt = ishft( iand( tev, error_saturate_mask), error_saturate_offset )	! saturation
			bad = bad + 1
			idebug = 1000 + tt
		else if(tx.eq.error_overflow_type) then
			tt = iand( tev, error_time_stamp_mask)					! overflow
			bad = bad + 1
			idebug = 1000
		endif

		skip = 0
		goto 100
	endif

	skip = 0					! skip done already for 1 word (in get_fx)

100	if(i+skip.gt.n_buffer-1) then			! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 91
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'position' event ...

20	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 92
		goto 10
	endif

!   Process payload data (encoder) ...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tag = ishft( iand( tev, data_type_mask), data_type_offset)
	if(tag.ne.position_data_type) goto 11						! not a position event

	tx = ishft( iand( tev, position_type_mask ), position_type_offset )		! type
	if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)

	if(tx.eq.position_axis1_type) then
		y0_last = y0
		y0 = iand( tev, position_data_mask )					! y

	else if(tx.eq.position_axis2_type) then
		z0_last = z0
		z0 = iand( tev, position_data_mask )					! z

	else if(tx.eq.position_axis3_type) then
		u0_last = u0
		u0 = iand( tev, position_data_mask )					! u

	else if(tx.eq.position_axis4_type) then
		v0_last = v0
		v0 = iand( tev, position_data_mask )					! v

	else if(tx.eq.position_axis5_type) then
		w0_last = w0
		w0 = iand( tev, position_data_mask )					! w
	endif	
	remain = remain-4
	goto 20

!...................................................................................
!   Process the 'spatial' event ...

30	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 93
		goto 10
	endif

!   Process payload data (stats)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tag = ishft( iand( tev, data_type_mask), data_type_offset)
	if(tag.ne.spatial1_data_type) goto 11						! not a spatial event

	tx = ishft( iand( tev, spatial1_type_mask ), spatial1_type_offset )		! type

	if(tx.eq.spatial1_sample_countm_type) then
!		tcount = ior( tcount, ishft( iand( tev, spatial1_sample_msb_mask), spatial1_sample_msb_offset))  ! samples 
		dwell_last = tcount * tick
		tcount = iand( tev, spatial1_sample_msb_mask)				! samples 
		dwell_last = dwell_last + tcount * tick * 2.**spatial1_sample_msb_offset

	else if(tx.eq.spatial1_generic_count1_type) then
		fc0 = iand( tev, spatial1_data_mask)					! FC0

	else if(tx.eq.spatial1_generic_count2_type) then
		fc1 = iand( tev, spatial1_data_mask)					! FC1

	else if(tx.eq.spatial1_generic_count3_type) then
		fc2 = iand( tev, spatial1_data_mask)					! FC2

	else if(tx.eq.spatial1_generic_count4_type) then
		fc3 = iand( tev, spatial1_data_mask)					! FC3

	else if(tx.eq.spatial1_est_icr_type) then
		adr = ishft( iand( tev, pulse1_adr_mask), pulse1_adr_offset)
		if((adr.ge.0).and.(adr.lt.15)) then
			icrm(adr) = icrm(adr) + iand( tev, spatial1_rate_mask)		! ICR (estimated ICR count)
			busym(adr) = 1
		endif

	else if(tx.eq.spatial1_raw_icr_type) then
		adr = ishft( iand( tev, pulse1_adr_mask), pulse1_adr_offset)
		if((adr.ge.0).and.(adr.lt.15)) then
			rawm(adr) = rawm(adr) + iand( tev, spatial1_rate_mask)		! Raw (actual OCR count)
			busym(adr) = 1
		endif

	else if(tx.eq.spatial1_time_stamp_type) then

		do k=0,15
		    if((n.lt.n_events).and.((k.eq.0).or.(busym(k).eq.1))) then
			do j=0,n_fx-1
				fx(j,n) = 0.0
			enddo
			x(n) = x0_last	
			y(n) = y0_last							! add a pseudo event
			z(n) = z0_last							! for previous pixel address
			u(n) = u0_last
			v(n) = v0_last
			w(n) = w0_last
			e(n) = 0
			t(n) = 0
			tags(n) = 0
			veto(n) = 1							! a pseudo event for counters, losses
			if(k.eq.0) then
				if(flux_mode.eq.1) fx(0,n) = fc0
				if(flux_mode.eq.2) fx(0,n) = fc1
				if(flux_mode.eq.3) fx(0,n) = fc2
				if(flux_mode.eq.4) fx(0,n) = fc3
				fx(1,n) = fc0
				fx(2,n) = fc1
				if(dwell_last.gt.0) then
					fx(3,n) = dwell_last				! dwell (ms), assumes 4 ns clock
				endif
			endif
			if(busym(k).eq.1) then
				ste(n) = k
				if((icrm(k).gt.0).and.(icrm(k).ge.rawm(k))) then
					fx(4,n) = float(icrm(k)) - float(rawm(k))	! lost counts
					fx(5,n) = float(rawm(k))			! raw (OCR counts weight)
				endif
			endif
			busym(k) = 0
			rawm(k)= 0
			icrm(k) = 0
			n = n+1
		    endif
		enddo
	endif
	remain = remain-4
	goto 30
end

!-------------------------------------------------------------------------------

integer function falconx_events2( argc, argv)

!DLL_EXPORT falconx_events2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.33) then 		         ! needed # args
	falconx_events2 = 1
	return
endif

call falconx_events2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), %val(argv(29)), %val(argv(30)), &
	 %val(argv(31)), %val(argv(32)), %val(argv(33)) )

falconx_events2 = 0
return
end

!-------------------------------------------------------------------------------
!
! First Version of falconx_events for FalconX List-mode, after additions for flux.
!
!	flux_mode	0		-- not used? --		fx index 0		-- later -- select FCn?
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!									 3		Dwell
!									 4		dead-time (%)

SUBROUTINE falconx_events2_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for: DT, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, jbuff, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx, flux_mode
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0,z0,u0,v0,w0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1), z(0:n_events-1), u(0:n_events-1), v(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1), w(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
REAL*4 fx(0:n_fx-1,0:n_events-1)
LOGICAL incomplete

INTEGER*4 data_type_mask, data_type_offset
INTEGER*2 pulse1_data_type, pulse2_data_type, position_data_type, spatial1_data_type, error_data_type
INTEGER*4 clock1_length, gstats1_length, position_length, spatial1_length, periodic_length
INTEGER*4 gate1_length, error_length, pulse1_length, pulse2_length
INTEGER*4 pulse1_valid_mask, pulse1_valid_offset, pulse1_energy_mask, pulse1_energy_offset
INTEGER*4 pulse2_toa_mask
INTEGER*4 position_type_mask, position_type_offset, position_data_mask
INTEGER*4 position_axis0_type, position_axis1_type, position_axis2_type, position_axis3_type 
INTEGER*4 position_axis4_type, position_axis5_type
INTEGER*4 spatial1_type_mask, spatial1_type_offset, spatial1_data_mask
INTEGER*4 spatial1_sample_msb_mask, spatial1_sample_msb_offset, spatial1_time_stamp_type
INTEGER*4 spatial1_sample_countl_type, spatial1_sample_countm_type, spatial1_erasure_count_type, spatial1_est_icr_type
INTEGER*4 spatial1_generic_count1_type, spatial1_generic_count2_type, spatial1_generic_count3_type, spatial1_generic_count4_type
INTEGER*4 error_type_mask, error_type_offset, error_analogue_status_type, error_overflow_type
INTEGER*4 error_saturate_mask, error_saturate_offset, error_time_stamp_mask

! NOTE: for now the "E" value is defined as top 12 bits of 24 bit 'amplitude'

parameter (data_type_mask = z'F0000000', data_type_offset=-28)
parameter (pulse1_data_type=0, pulse2_data_type=1, position_data_type=12, spatial1_data_type=11, error_data_type=15)
parameter (clock1_length = 1, gstats1_length = 9, position_length = 7, spatial1_length = 9, periodic_length = 8)
parameter (gate1_length = 1, error_length = 1, pulse1_length = 1, pulse2_length = 1)
parameter (pulse1_valid_mask = z'08000000', pulse1_valid_offset = -27, pulse1_energy_mask = z'00FFF000')
parameter (pulse1_energy_offset = -12)
parameter (pulse2_toa_mask = z'000FFFFF')
parameter (position_type_mask =	z'0F000000', position_type_offset = -24, position_data_mask = z'00FFFFFF')
parameter (spatial1_type_mask = z'0F000000', spatial1_type_offset = -24, spatial1_data_mask = z'00FFFFFF')
parameter (spatial1_sample_msb_mask = z'00000FF', spatial1_sample_msb_offset = 24)
parameter (spatial1_sample_countl_type = 0, spatial1_sample_countm_type = 1)
parameter (error_type_mask = z'0F000000', error_type_offset = -24, error_analogue_status_type = 0, error_overflow_type = 1)
parameter (error_saturate_mask = z'000C0000', error_saturate_offset = -18, error_time_stamp_mask = z'00FFFFFF')

parameter (position_axis0_type = 0, position_axis1_type = 1, position_axis2_type = 2, position_axis3_type = 3)
parameter (position_axis4_type = 4, position_axis5_type = 5)
parameter (spatial1_erasure_count_type = 2)
parameter (spatial1_generic_count1_type = 4, spatial1_generic_count2_type = 5, spatial1_generic_count3_type = 6)
parameter (spatial1_est_icr_type = 3, spatial1_generic_count4_type = 7, spatial1_time_stamp_type = 15)

INTEGER*4 tx, ty, tz, tt, tev, tcount
INTEGER*2 de, dtag, pdf, adr
integer*4 remain, x0_last, y0_last, z0_last, u0_last, v0_last, w0_last, fc0, fc1, fc2, fc3
integer*4 stats_type, stats_ovf
integer*2 ldone, rdone, ldone2, rdone2
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_6/ dwell_last
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
common /c_fx_8/ fc0, fc1, fc2, fc3, u0_last, v0_last, w0_last
equivalence (bev(0), tev)

if(n_fx.lt.5) return
n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,10,10,10,10,10,10,100), ibranch

10  call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word (get_fx advances 'i' +4)
	
	tag = ishft( iand( tev, data_type_mask), data_type_offset)

	if(tag.eq.pulse1_data_type) then		! Pulse 1 event

		pdf = ishft( iand( tev, pulse1_valid_mask ), pulse1_valid_offset )	! invalid
		de = ishft( iand( tev, pulse1_energy_mask ), pulse1_energy_offset )	! energy
!		adr = ishft( iand( tev, adr_mask), adr_offset)				! channel
		adr = 0

		if((pdf.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = de
				t(n) = 0
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor valid bit
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				n = n+1
			endif
		endif

		skip = 0
		goto 100
	endif

	if(tag.eq.position_data_type) then		! position event
		length = (position_length-1)*4						! remaining length
		remain = length

		x0_last = x0
		x0 = iand( tev, position_data_mask )					! x

		ibranch = 2
		goto 20
	endif

	if(tag.eq.spatial1_data_type) then		! spatial event
		length = (spatial1_length-1) *4
		remain = length

		tcount = iand( tev, spatial1_data_mask)					! sample LSB

		ibranch = 3
		goto 30
	endif

	if(tag.eq.error_data_type) then			! error1 event

		tx = ishft( iand( tev, error_type_mask ), error_type_offset )		! type
		if(tx.eq.error_analogue_status_type) then
			tt = ishft( iand( tev, error_saturate_mask), error_saturate_offset )	! saturation
			bad = bad + 1
			idebug = 1000 + tt
		else if(tx.eq.error_overflow_type) then
			tt = iand( tev, error_time_stamp_mask)					! overflow
			bad = bad + 1
			idebug = 1000
		endif

		skip = 0
		goto 100
	endif

	skip = 0					! skip done already for 1 word (in get_fx)

100	if(i+skip.gt.n_buffer-1) then			! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 91
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'position' event ...

20	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 92
		goto 10
	endif

!   Process payload data (encoder) ...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tx = ishft( iand( tev, position_type_mask ), position_type_offset )		! type
	if(tx.eq.position_axis1_type) then
		y0_last = y0
		y0 = iand( tev, position_data_mask )					! y
	else if(tx.eq.position_axis2_type) then
		z0_last = z0
		z0 = iand( tev, position_data_mask )					! z
	else if(tx.eq.position_axis3_type) then
		u0_last = u0
		u0 = iand( tev, position_data_mask )					! u
	else if(tx.eq.position_axis4_type) then
		v0_last = v0
		v0 = iand( tev, position_data_mask )					! v
	else if(tx.eq.position_axis5_type) then
		w0_last = w0
		w0 = iand( tev, position_data_mask )					! w
	endif	
	remain = remain-4
	goto 20

!...................................................................................
!   Process the 'spatial' event ...

30	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 93
		goto 10
	endif

!   Process payload data (stats)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tx = ishft( iand( tev, spatial1_type_mask ), spatial1_type_offset )		! type

	if(tx.eq.spatial1_sample_countm_type) then
		tcount = ior( tcount, ishft( iand( tev, spatial1_sample_msb_mask), spatial1_sample_msb_offset))  ! sample 
		dwell_last = tcount * 4.0e-6
	else if(tx.eq.spatial1_generic_count1_type) then
		fc0 = iand( tev, spatial1_data_mask)					! FC0
	else if(tx.eq.spatial1_generic_count2_type) then
		fc1 = iand( tev, spatial1_data_mask)					! FC1
	else if(tx.eq.spatial1_time_stamp_type) then
		x(n) = x0_last	
		y(n) = y0_last								! add a pseudo event
		z(n) = z0_last
		u(n) = u0_last
		v(n) = v0_last
		w(n) = w0_last
		e(n) = 0
		t(n) = 0
		ste(n) = 0
		tags(n) = 0
		veto(n) = 1								! a pseudo event for counters
		do k=0,n_fx-1
			fx(k,n) = 0.0
		enddo
		fx(1,n) = fc0
		fx(2,n) = fc1
		fx(3,n) = dwell_last							! dwell (ms), assumes 4 ns clock
!		fx(4,n) = dt_last * 1.0e-2						! dead-time ?
		if(flux_mode.eq.1) fx(0,n) = fc0
		if(flux_mode.eq.2) fx(0,n) = fc1
		n = n+1
	endif
	remain = remain-4
	goto 30
end

!-------------------------------------------------------------------------------

integer function falconx_events1( argc, argv)

!DLL_EXPORT falconx_events1
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.27) then 		         ! needed # args
	falconx_events1 = 1
	return
endif

call falconx_events1_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)) )

falconx_events1 = 0
return
end

!-------------------------------------------------------------------------------
!
! First Version of falconx_events for FalconX List-mode, prior to additions for flux.
!
!	flux_mode	0		-- not used? --		fx index 0		-- later -- select FCn?
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!									 3		Dwell
!									 4		dead-time (%)

SUBROUTINE falconx_events1_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0,z0, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for: DT, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, jbuff, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx, flux_mode
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0,z0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1), z(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
REAL*4 fx(0:n_fx-1,0:n_events-1)
LOGICAL incomplete

INTEGER*4 data_type_mask
INTEGER*2 fx_pulse1, fx_pulse2, fx_spatial1, fx_stats1, fx_stats2, fx_error1, fx_sync1, fx_sync2
INTEGER*4 stats1_type_mask, stats1_type_offset, stats1_ovf_mask, stats1_ovf_offset
INTEGER*4 stats1_sample_count_index, stats1_erasure_count_index, stats1_pulse_event_count_index, stats1_pulse_accepted_count_index
INTEGER*4 stats1_est_icr_index, stats1_ocr_index, stats1_dt_percent_index, stats1_type_gate_triggered
INTEGER*4 stats1_type_pixel_triggered, stats1_type_periodic
INTEGER*4 stats2_type_mask, stats2_type_offset, stats2_ovf_mask, stats2_ovf_offset
INTEGER*4 stats2_count_offset, stats2_sample_countl_index, stats2_sample_countm_index, stats2_erasure_countl_index
INTEGER*4 stats2_erasure_countm_index, stats2_pulse_event_countl_index, stats2_pulse_event_countm_index
INTEGER*4 stats2_pulse_accepted_countm_index, stats2_est_icr_index, stats2_ocr_index, stats2_dt_percent_index
INTEGER*4 stats2_type_gate_triggered, stats2_type_pixel_triggered, stats2_type_periodic, stats2_pulse_accepted_countl_index
INTEGER*4 spatial1_axis0_index, spatial1_axis1_index, spatial1_axis2_index
INTEGER*4 gate1_state_mask, gate1_state_offset
INTEGER*4 error_status_mask, error_status_offset
INTEGER*4 pulse1_valid_mask, pulse1_valid_offset, pulse1_energy_mask, pulse1_energy_offset
INTEGER*4 pulse1_toa_index, pulse1_toa_mask, pulse1_toa_offset
INTEGER*4 pulse2_valid_mask, pulse2_valid_offset, pulse2_state_mask
INTEGER*4 pulse2_state_offset, pulse2_energy_mask, pulse2_energy_offset
INTEGER*4 clock1_length, clock2_length, stats1_length, stats2_length, spatial1_length
INTEGER*4 gate1_length, error_length, pulse1_length, pulse2_length

parameter (data_type_mask = z'0000007F')
parameter (fx_pulse1=16, fx_pulse2=17, fx_spatial1=4, fx_stats1=2, fx_stats2=3, fx_error1=64, fx_sync1=0, fx_sync2=1)
parameter (stats1_type_mask = z'00000F00', stats1_type_offset = -8, stats1_ovf_mask = z'00001000', stats1_ovf_offset = -9)
parameter (stats1_sample_count_index = 1, stats1_erasure_count_index = 2, stats1_pulse_event_count_index = 3)
parameter (stats1_pulse_accepted_count_index = 4)
parameter (stats1_est_icr_index = 5, stats1_ocr_index = 6, stats1_dt_percent_index = 7, stats1_type_gate_triggered = 0)
parameter (stats1_type_pixel_triggered = 1, stats1_type_periodic = 2)
parameter (stats2_type_mask = z'00000F00', stats2_type_offset = -8, stats2_ovf_mask = z'00001000', stats2_ovf_offset = -9)
parameter (stats2_count_offset = 24, stats2_sample_countl_index = 1, stats2_sample_countm_index = 2)
parameter (stats2_erasure_countl_index = 3)
parameter (stats2_erasure_countm_index = 4, stats2_pulse_event_countl_index = 5, stats2_pulse_event_countm_index = 6)
parameter (stats2_pulse_accepted_countl_index = 7)
parameter (stats2_pulse_accepted_countm_index = 8, stats2_est_icr_index = 9, stats2_ocr_index = 10)
parameter (stats2_dt_percent_index = 11)
parameter (stats2_type_gate_triggered = 0, stats2_type_pixel_triggered = 1, stats2_type_periodic = 2)
parameter (spatial1_axis0_index = 1, spatial1_axis1_index = 2, spatial1_axis2_index = 3)
parameter (gate1_state_mask = z'00000300', gate1_state_offset = -8)
parameter (error_status_mask = z'0000FF00', error_status_offset = -8)
parameter (pulse1_valid_mask = z'00000100', pulse1_valid_offset = -8, pulse1_energy_mask = z'07FFF800')
parameter (pulse1_energy_offset = -11)
parameter (pulse1_toa_index = 1, pulse1_toa_mask = z'3FFFFFFC0', pulse1_toa_offset = -6)
parameter (pulse2_valid_mask = z'00000100', pulse2_valid_offset = -8, pulse2_state_mask = z'00000600')
parameter (pulse2_state_offset = -9, pulse2_energy_mask = z'07FFF800', pulse2_energy_offset = -11)
parameter (clock1_length = 1, clock2_length = 2, stats1_length = 8, stats2_length = 12, spatial1_length = 4)
parameter (gate1_length = 1, error_length = 1, pulse1_length = 2, pulse2_length = 1)

INTEGER*4 tx, ty, tz, tt, tev
INTEGER*2 de, dtag, pdf, adr
integer*4 remain, x0_last, y0_last, z0_last, tcount
integer*4 stats_type, stats_ovf
integer*2 ldone, rdone, ldone2, rdone2
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_6/ dwell_last
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
equivalence (bev(0), tev)

if(n_fx.lt.5) return
n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,50,10,10,10,10,100), ibranch

10  call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word (get_fx advances 'i' +4)
	
	tag = iand( tev, data_type_mask)

	if(tag.eq.fx_pulse1) then		     ! Pulse 1 event
		length = (pulse1_length-1)*4						! remaining length
		remain = length

		pdf = ishft( iand( tev, pulse1_valid_mask ), pulse1_valid_offset )	! invalid
		de = ishft( iand( tev, pulse1_energy_mask ), pulse1_energy_offset )	! energy
!		adr = ishft( iand( tev, adr_mask), adr_offset)				! channel
		adr = 0

		if((pdf.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				e(n) = de
				t(n) = 0
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor valid bit
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				n = n+1
			endif
		endif

		ibranch = 2
		goto 20
	endif

	if(tag.eq.fx_pulse2) then		     ! Pulse 2 event
		length = 0								! remaining length
		remain = length

		pdf = ishft( iand( tev, pulse2_valid_mask ), pulse2_valid_offset )	! invalid
		de = ishft( iand( tev, pulse2_energy_mask ), pulse2_energy_offset )	! energy
!		adr = ishft( iand( tev, adr_mask), adr_offset)				! channel
		adr = 0

		if((pdf.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				e(n) = de
				t(n) = 0
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor valid bit
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				n = n+1
			endif
		endif

		skip = length
		goto 100
	endif

	if(tag.eq.fx_spatial1) then		     ! Spatial 1 event
		length = (spatial1_length-1)*4						! remaining length
		remain = length

		ibranch = 3
		goto 30
	endif

	if(tag.eq.fx_stats1) then		     ! Stats1 event
		length = (stats1_length-1) *4
		remain = length

		stats_type = ishft( iand( tev, stats1_type_mask ), stats1_type_offset )		! type
		stats_ovf = ishft( iand( tev, stats1_ovf_mask ), stats1_ovf_offset )		! OVF

		ibranch = 4
		goto 40
	endif

	if(tag.eq.fx_stats2) then		     ! Stats2 event
		length = (stats2_length-1)*4						! remaining length
		remain = length

		stats_type = ishft( iand( tev, stats2_type_mask ), stats2_type_offset )		! type
		stats_ovf = ishft( iand( tev, stats2_ovf_mask ), stats2_ovf_offset )		! OVF

		ibranch = 5
		goto 50
	endif

	if(tag.eq.fx_error1) then		     ! error1 event
		length = 0								! remaining length
		remain = length

		tx = ishft( iand( tev, error_status_mask ), error_status_offset )		! error
		idebug = 1000 + tx

		skip = length
		goto 100
	endif

	if(tag.eq.fx_sync1) then		     ! sync1 event
		length = 0								! remaining length
		remain = length

		skip = length
		goto 100
	endif

	if(tag.eq.fx_sync2) then		     ! sync2 event
		length = (clock2_length-1)*4						! remaining length
		remain = length

		skip = length
		goto 100
	endif

	skip = 0					! skip done already for 1 word (in get_fx)

100	if(i+skip.gt.n_buffer-1) then			! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 91
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'fx_pulse1' event ...

20	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 92
		goto 10
	endif

!   Process payload data (TOA)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return  

	tt = ishft( iand( tev, pulse1_toa_mask ), pulse1_toa_offset )	! Time of arrival
	remain = remain-4
	goto 20

!...................................................................................
!   Process the 'fx_spatial1' event ...

30	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 93
		goto 10
	endif

!   Process payload data (TOA)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	if(remain.eq.(spatial1_length - spatial1_axis0_index)*4) then
		x0_last = x0
		x0 = tev
	else if(remain.eq.(spatial1_length - spatial1_axis1_index)*4) then
		y0_last = y0
		y0 = tev
	else if(remain.eq.(spatial1_length - spatial1_axis2_index)*4) then
		z0_last = z0
		z0 = tev
	endif	
	remain = remain-4
	goto 30

!...................................................................................
!   Process the 'fx_stats1' event ...

40	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif

!   Process payload data (stats)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	if(stats_type.eq.stats1_type_pixel_triggered) then	! triggered by pixel change
		if(remain.eq.(stats1_length - stats1_sample_count_index)*4) then
			tt = tev				! Sample count
			x(n) = x0_last
			y(n) = y0_last							! add a pseudo event
			z(n) = z0_last
			e(n) = 0
			t(n) = 0
			ste(n) = 0
			tags(n) = stats_ovf 
			veto(n) = 1
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			dwell_last = tt * 4.0e-6
			fx(3,n) = dwell_last			! dwell (ms)
			n = n+1
!		else if(remain.eq.(stats1_length - ?)*4) then
!			tt = tev				! FC0 count would look like this ...
!			x(n) = x0_last
!			y(n) = y0_last							! add a pseudo event
!			z(n) = z0_last
!			e(n) = 0
!			t(n) = 0
!			ste(n) = 0
!			tags(n) = stats_ovf 
!			veto(n) = 1
!			do k=0,n_fx-1
!				fx(k,n) = 0.0
!			enddo
!			fx(1,n) = tt				! FC0
!			if(flux_mode.eq.1) fx(0,n) = tt
!			n = n+1
!		else if(remain.eq.(stats1_length - ?)*4) then
!			tt = tev				! FC1 count would look like this ...
!			x(n) = x0_last
!			y(n) = y0_last							! add a pseudo event
!			z(n) = z0_last
!			e(n) = 0
!			t(n) = 0
!			ste(n) = 0
!			tags(n) = stats_ovf 
!			veto(n) = 1
!			do k=0,n_fx-1
!				fx(k,n) = 0.0
!			enddo
!			fx(2,n) = tt				! FC1
!			if(flux_mode.eq.2) fx(0,n) = tt
!			n = n+1
		else if(remain.eq.(stats1_length - stats1_dt_percent_index)*4) then
			tt = tev				! DT count
			x(n) = x0_last
			y(n) = y0_last							! add a pseudo event
			z(n) = z0_last
			e(n) = 0
			t(n) = 0
			ste(n) = 0
			tags(n) = stats_ovf 
			veto(n) = 1
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			fx(4,n) = tt * 1.0e-2			! DT (%)
			n = n+1
		endif	
	endif
	remain = remain-4
	goto 40

!...................................................................................
!   Process the 'fx_stats2' event ...

50	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 95
		goto 10
	endif

!   Process payload data (stats)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	if(stats_type.eq.stats2_type_pixel_triggered) then	! triggered by pixel change
		if(remain.eq.(stats2_length - stats2_sample_countl_index)*4) then
			tcount = tev				! Sample count (LSB)
		else if(remain.eq.(stats2_length - stats2_sample_countm_index)*4) then
			dev = tev
			dev = ior( ishft( dev, stats2_count_offset), tcount)		! Sample count
			x(n) = x0_last
			y(n) = y0_last							! add a pseudo event
			z(n) = z0_last
			e(n) = 0
			t(n) = 0
			ste(n) = 0
			tags(n) = stats_ovf 
			veto(n) = 1
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			dwell_last = tt * 4.0e-6
			fx(3,n) = dwell_last			! dwell (ms)
			n = n+1
!		else if(remain.eq.(stats2_length - ?)*4) then
!			tt = tev				! FC0 count would look like this ...
!			x(n) = x0_last
!			y(n) = y0_last							! add a pseudo event
!			z(n) = z0_last
!			e(n) = 0
!			t(n) = 0
!			ste(n) = 0
!			tags(n) = stats_ovf 
!			veto(n) = 1
!			do k=0,n_fx-1
!				fx(k,n) = 0.0
!			enddo
!			fx(1,n) = tt				! FC0
!			if(flux_mode.eq.1) fx(0,n) = tt
!			n = n+1
!		else if(remain.eq.(stats2_length - ?)*4) then
!			tt = tev				! FC1 count would look like this ...
!			x(n) = x0_last
!			y(n) = y0_last							! add a pseudo event
!			z(n) = z0_last
!			e(n) = 0
!			t(n) = 0
!			ste(n) = 0
!			tags(n) = stats_ovf 
!			veto(n) = 1
!			do k=0,n_fx-1
!				fx(k,n) = 0.0
!			enddo
!			fx(2,n) = tt				! FC1
!			if(flux_mode.eq.2) fx(0,n) = tt
!			n = n+1
		else if(remain.eq.(stats2_length - stats2_dt_percent_index)*4) then
			tt = tev				! DT count
			x(n) = x0_last
			y(n) = y0_last							! add a pseudo event
			z(n) = z0_last
			e(n) = 0
			t(n) = 0
			ste(n) = 0
			tags(n) = stats_ovf 
			veto(n) = 1
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			fx(4,n) = tt * 1.0e-2			! DT (%)
			n = n+1
		endif	
	endif
	remain = remain-4
	goto 50
end

!-------------------------------------------------------------------------------

integer function falconx_accumulate_dtfx_3D( argc, argv)

!DLL_EXPORT falconx_accumulate_dtfx_3D
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.21) then       		   ! needed # args
	falconx_accumulate_dtfx_3D = 1
	return
endif

call falconx_accumulate_dtfx_3D_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)) )

falconx_accumulate_dtfx_3D = 0
return
end

!------------------------------------------------------------

SUBROUTINE falconx_accumulate_dtfx_3D_b( image_mode, x,y,z,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,zrange, flux,dead,weight,dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action:
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!				(not used for FalconX as yet) 
!		1-4	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale, when not in 3D mode
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] to set Dwell in maia_dwell (ms)
!
!	Dead:
!		Use fx[4,*] to accumulate total lost count in 'dead'.
!		And weights raw count in 'weight' (fx[5,*]).
!		Later (in da_evt) norm this using the weight array (not the dwell array).

INTEGER*4 image_mode, flux_mode, n,n_fx, xcompress,ycompress, xrange,yrange,zrange, i
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, flux(0:xrange-1,0:yrange-1,0:zrange-1)
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt
LOGICAL mode_3D

if(n.le.0) return
if(n_fx.lt.5) return
mode_3D = (zrange.ge.2)
dwell_last = 0.
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange).and.(z(i).ge.0).and.(z(i).lt.zrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),z(i)) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),z(i)) = flux(x(i),y(i),z(i)) + fx(0,i) * flux_scale2
				endif
				if( .not.mode_3D) then
					flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
					flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				endif
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
					dwell_last = fx(3,i)
				endif
				if( fx(4,i).gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + fx(4,i)
				endif
				weight(x(i),y(i)) = weight(x(i),y(i)) + fx(5,i)
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in FalconX device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if(veto(i).eq.1) then
				if( fx(4,i).gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + fx(4,i)
				endif
				weight(ste(i),0) = weight(ste(i),0) + fx(5,i)
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function falconx_accumulate_dtfx( argc, argv)

!DLL_EXPORT falconx_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.20) then       		   ! needed # args
	falconx_accumulate_dtfx = 1
	return
endif

call falconx_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)) )

falconx_accumulate_dtfx = 0
return
end

!------------------------------------------------------------------------------------------------


SUBROUTINE falconx_accumulate_dtfx_b( image_mode, x,y,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,n_flux, flux, dead, weight, dwell)

!	Accumulate flux (including attributes), dead-time and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use 'flux_mode' to select action:
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!				(not used for FalconX as yet) 
!		1-4	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] to set 'dwell' (ms)
!
!	Dead:
!		Use fx[4,*] to accumulate total lost count in 'dead'.
!		And weights raw count in 'weight' (fx[5,*]).
!		Later (in da_evt) norm this using the weight array (not the dwell array).

INTEGER*4 image_mode, flux_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, flux(0:xrange-1,0:yrange-1,0:n_flux), dt, dwell_last
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1)

if(n.le.0) return
if(n_fx.lt.6) return
dwell_last = 0.
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),0) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),0) = flux(x(i),y(i),0) + fx(0,i) * flux_scale2
				endif
				flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
				flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
					dwell_last = fx(3,i)
				endif
				if( fx(4,i).gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + fx(4,i)
				endif
				weight(x(i),y(i)) = weight(x(i),y(i)) + fx(5,i)
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in FalconX device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if(veto(i).eq.1) then
				if( fx(4,i).gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + fx(4,i)
				endif
				weight(ste(i),0) = weight(ste(i),0) + fx(5,i)
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function maia_384_events6( argc, argv)

!DLL_EXPORT maia_384_events6
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.43) then 		         ! needed # args
	maia_384_events6 = 1
	return
endif

call maia_384_events6_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), &
	 %val(argv(29)), %val(argv(30)), %val(argv(31)), &
	 %val(argv(32)), %val(argv(33)), %val(argv(34)), &
	 %val(argv(35)), %val(argv(36)), %val(argv(37)), %val(argv(38)), &
	 %val(argv(39)), %val(argv(40)), %val(argv(41)), %val(argv(42)), %val(argv(43)) )

maia_384_events6 = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of maia_384_events for 3D data (x,y,z) and using simpler vector events extended to include flux,
! dwell, dead-time, ...  using the addition of pseudo events to hold new values of BT, FC0, FC1.
! Now with SKIP as a long. Unlike many devices with explicit dead-time sums per pixel, dead-time here is
! handled via the time-over-threshold values per event returned in 't'.
!
! monitor returns:
!	monitor	strings			nm (returned length)
!
!	xy_correct			0 normal, 1 Y encoder advance mode, 2 filter +-16 glitches, 2 YZ encoder mode
!	xmargin (ymargin)		pixel X (Y) margin boundary width
!	height				height of scan needed for Z encoder correction
!	width				width of scan needed for Y encoder mode
!					number of detector channels (spectrum mode) ???
!
!	beam_energy			current value of beam energy (zero externally before sort)
!	flux_pv				current value of flux Epics PV (zero externally)
!	time_pv				time-stamp for time_pv
!
!	flux_mode	0		epics PV		fx index 0		flux selected (PV or FCn)
!			1		FC1 h/w				 1		FC1
!			2		FC2 h/w				 2		FC2
!			3		Dwell time			 3		Dwell

SUBROUTINE maia_384_events6_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0,z0, monitor,n_monitor,nm, &
		energy_string,n_es, beam_energy, flux_string,n_fs, flux_pv, time_pv, time_first, &
		xy_correct,xmargin,ymargin,width,height, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for selected flux, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, n_es, n_fs, n_events, n_fx, flux_mode
INTEGER*4 length, len2, width,height, skip, x0,y0,z0
INTEGER*4 n_mon_buff, mbuff, x(0:n_events-1), y(0:n_events-1), z(0:n_events-1)
INTEGER*2 tag, ibranch, word_mask, xy_on
INTEGER*2 e(0:n_events-1), t(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
INTEGER*1 energy_string(0:n_es-1), flux_string(0:n_fs-1)
REAL*4 factor, val, val2, fx(0:n_fx-1,0:n_events-1), flux_pv, beam_energy
REAL*8 time_pv, time_first
LOGICAL incomplete, first, whole
Integer*4 xmargin,ymargin, xy_correct
INTEGER*1 monitor(0:n_monitor-1)
INTEGER*4 nm, n_monitor

parameter (n_mon_buff = 20000)
INTEGER*1 a_tag, b_tag, mon_buff(0:n_mon_buff-1)
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask, tx, ty, tz
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4
INTEGER*4 pa_sign_bit_mask4, pa_sign_extend, pa_tag4, pa_tag_offset4
INTEGER*2 maia_monitor, maia_events_1
Integer*2 ldone, rdone, ldone2, rdone2
INTEGER*4 xyz_tag_mask4, xyz_tag_x4, xyz_tag_y4, xyz_tag_z4, xyz_pos_mask4, xyz_tag_pa4 
INTEGER*4 pa_tag_mask4, pa_tag_x4, pa_tag_y4, pa_tag_z4, pa_tag_tf4
INTEGER*4 tf_tag_mask4, tf_tag_bt4, tf_tag_fc1, tf_tag_fc2, tf_bit_mask4 

common /c_maia_1/ jbuff, bev, remain, first
common /c_maia_3/ mbuff, mon_buff
common /c_maia_4/ ldone, rdone, ldone2, rdone2
common /c_maia_7/ xy_on
common /c_maia_6/ tv_sec, tv_micro
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_1 = 34, maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET2 format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET3 format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -31, tag_offset3 = -31 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31 )
parameter ( pa_mask4 = z'07FFFFFF', pa_sign_bit_mask4 = z'04000000', pa_sign_extend = z'F8000000')
parameter ( xyz_tag_mask4 = z'E0000000', xyz_tag_x4 = z'80000000', xyz_tag_y4 =	z'A0000000', xyz_tag_z4 = z'C0000000' )
parameter ( xyz_pos_mask4 = z'07FFFFFF', xyz_tag_pa4 = z'E0000000' )
parameter ( pa_tag_mask4 = z'F8000000', pa_tag_x4 = z'E0000000', pa_tag_y4 = z'E8000000', pa_tag_z4 = z'F0000000' )
parameter ( pa_tag_tf4 = z'F8000000', tf_tag_mask4 = z'FE000000', tf_tag_bt4 =z'F8000000', tf_tag_fc1 =	z'FA000000' )
parameter ( tf_tag_fc2 = z'FC000000', tf_bit_mask4 = z'01FFFFFF' )

n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		idebug = 91
!		return
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length, first 16 bits unsigned)
	length = bev(0)
	length = iand(length,byte_mask)
	len2 = bev(1)
	len2 = iand(len2,byte_mask)
	length = ior( ishft( length, 8), len2)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (20 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	if( time_first.eq.0.0) time_first = dble(tv_sec) + dble(tv_micro)/1000000.
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8

80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_1) then		     ! event_1 (ET event, type #4), they contain XYZ too
		remain = length
		first = .true.
		ibranch = 15
		goto 150
	endif
	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 98
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 97
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          								! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor PDF overflow bit
!				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then		! use Epics PV
					fx(0,n) = flux_pv		! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then				! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 96
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return			! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf				! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then		! use Epics PV
					fx(0,n) = flux_pv		! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 95
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          		! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3) 	! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then		! use Epics PV
					fx(0,n) = flux_pv		! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'maia_events_1' event (was maia_ET_events_4) ...

150	if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif

!   Process payload data while it lasts ...
!   First 3 words of ET4 events is PA pixel XYZ information. This is assumed for tagging event E,T data with X,Y,Z.
!   Also need to assume that PA Z always follows PA Y, always follows PA X.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words of payload

	tx = iand( tev, tag_mask4)
	if(tx.eq.0) then							! ET

		dtag = 0
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv			! put current val in each event
				endif
				n = n+1
			endif
		endif
	else
		tx = iand( tev, xyz_tag_mask4)
		if(tx.eq.xyz_tag_x4) then					! Encoder X
			tx =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_y4) then					! Encoder Y
			ty =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_z4) then					! Encoder Z
			tz =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_pa4) then
			tx = iand( tev, pa_tag_mask4)
			if(tx.eq.pa_tag_x4) then				! PA X
				tx =  iand( tev, pa_mask4)
				if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
				if(xy_correct.eq.2) then
					if(tx.ne.x0) then
						if((tx.eq.x0-15).or.(tx.eq.x0+17)) then
							x0 = x0+1
						else if((tx.eq.x0-17).or.(tx.eq.x0+15)) then
							x0 = x0-1
						else if((tx.eq.x0-16).or.(tx.eq.x0+16)) then
							x0 = x0
						else 
							x0 = tx
						endif
					endif
				else
					x0 = tx
				endif
			else if(tx.eq.pa_tag_y4) then				! PA Y
				ty =  iand( tev, pa_mask4)
				if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
				if((xy_correct.eq.1).or.(xy_correct.eq.3)) then
					idebug = ty
				!	if(y0.eq.0) then
				!		if(ty.gt.0) y0=ty
				!	endif
					if((x0.lt.xmargin).and.(x0.ge.0)) then
						if((ty.ge.0).and.(ty.le.2)) then
							if(xy_on.eq.0) then
								y0 = ty
								xy_on = 1
								ldone = 1
								rdone = 0
							endif
						endif
						if(xy_on.eq.1) then
							if(ldone.eq.0) y0 = y0+1
							ldone = 1
							rdone = 0
						endif

					!	if(ty.gt.y0) then
					!		if(ldone.eq.0) y0 = y0+1
					!		ldone = 1
					!		rdone = 0
					!	else if(ty.le.0) then
					!		y0 = 0
					!		ldone = 1
					!		rdone = 0
					!	endif
					else if((x0.gt.width-1-xmargin).and.(x0.lt.width)) then
						if((ty.ge.0).and.(ty.le.2)) then
							if(xy_on.eq.0) then
								y0 = ty
								xy_on = 1
								ldone = 0
								rdone = 1
							endif
						endif
						if(xy_on.eq.1) then
							if(rdone.eq.0) y0 = y0+1
							ldone = 0
							rdone = 1
						endif

					!	if(ty.gt.y0) then
					!		if(rdone.eq.0) y0 = y0+1
					!		rdone = 1
					!		ldone = 0
					!	else if(ty.le.0) then
					!		y0 = 0
					!		rdone = 1
					!		ldone = 0
					!	endif
					! else if((x0.le.width-1-xmargin).and.(x0.ge.xmargin)) then
					else if((x0.lt.width-1-xmargin-1).and.(x0.gt.xmargin+1)) then
						rdone = 0
						ldone = 0						
					endif
				else
					if(xy_correct.eq.2) then
						if(ty.ne.y0) then
							if((ty.ne.y0-16).and.(ty.ne.y0+16)) y0 = ty
						endif
					else
						y0 = ty
					endif
				endif

			else if(tx.eq.pa_tag_z4) then				! PA Z
				tz =  iand( tev, pa_mask4)
				if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
				idebug = tz
				if(xy_correct.eq.3) then
					if(z0.eq.0) then
						if(tz.gt.0) z0=tz
					endif
					if((y0.lt.ymargin).and.(y0.ge.0)) then
						if(tz.gt.z0) then
							if(ldone2.eq.0) z0 = z0+1
							ldone2 = 1
							rdone2 = 0
						else if(tz.le.0) then
							z0 = 0
							ldone2 = 1
							rdone2 = 0
						endif
					else if((y0.gt.height-1-ymargin).and.(y0.lt.height)) then
						if(tz.gt.z0) then
							if(rdone2.eq.0) z0 = z0+1
							rdone2 = 1
							ldone2 = 0
						else if(tz.le.0) then
							z0 = 0
							rdone2 = 1
							ldone2 = 0
						endif
					else if((y0.le.height-1-ymargin).and.(y0.ge.ymargin)) then
						rdone2 = 0
						ldone2 = 0						
					endif
				else
!					if(xy_correct.eq.2) then
!						if(tz.ne.z0) then
!							if((tz.ne.z0-16).and.(tz.ne.z0+16)) z0 = tz
!						endif
!					else
						z0 = tz
!					endif
				endif
		
			else if(tx.eq.pa_tag_tf4) then				! Time-Flux

!				These are assumed to follow AFTER x0,y0 are set from PA ...

				tx = iand( tev, tf_tag_mask4)
				if(tx.eq.tf_tag_bt4) then			! BT (ms)
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0				! add a pseudo event
					z(n) = z0
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1				! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(3,n) = ty * 0.0001			! dwell (ms)
					if (flux_mode.eq.0) then		! use Epics PV
						fx(0,n) = flux_pv		! put current val in each event
					else if (flux_mode.eq.3) then
						fx(0,n) = fx(3,n)		! h/w will be added to pseudo event
					endif
					n = n+1

				else if(tx.eq.tf_tag_fc1) then			! Flux 1
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0				! add a pseudo event
					z(n) = z0
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1				! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(1,n) = ty
					if (flux_mode.eq.0) then		! use Epics PV
						fx(0,n) = flux_pv		! put current val in each event
					else if (flux_mode.eq.1) then
						fx(0,n) = fx(1,n)		! h/w will be added to pseudo event
					endif
					n = n+1

				else if(tx.eq.tf_tag_fc2) then			! Flux 2
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0				! add a pseudo event
					z(n) = z0
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1				! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(2,n) = ty
					if (flux_mode.eq.0) then		! use Epics PV
						fx(0,n) = flux_pv		! put current val in each event
					else if (flux_mode.eq.2) then
						fx(0,n) = fx(2,n)		! h/w will be added to pseudo event
					endif
					n = n+1

				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 150

!...................................................................................
!   Process the 'monitor' event ...

130	if(length.gt.n_mon_buff) then				! monitor too long
		err = 1						! for mon_buff buffer
		idebug = 1301
		return
	endif
 	j = length		         			! data long words of payload

	call get_maia_32( ev, i, n_buffer, mon_buff, j, mbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return 

	do k=0,j-1						! completed all monitor
		monitor(nm+k) = mon_buff(k)			! copy buffer to 'monitor'
	enddo

	if((j.lt.n_es+4).or.(n_es.eq.0)) goto 132		! skip if short
	val = 0.0
	val2 = 0.0
	do 131 k=nm,nm+j-n_es					! check for "Energy" monitor
		do l=0,n_es-1
			if(energy_string(l).ne.monitor(k+l)) goto 131
		enddo
		l = k+n_es					! found energy monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 132				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val = 10.*val + monitor(m)-48
				else
					val = val + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
		beam_energy = val
131	continue

132	if((j.lt.n_fs+4).or.(n_fs.eq.0)) goto 137		! skip if short
	do 133 k=nm,nm+j-n_fs					! check for "Flux" monitor
		do l=0,n_fs-1
			if(flux_string(l).ne.monitor(k+l)) goto 133
		enddo
		l = k+n_fs					! found flux monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 137				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val2 = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val2 = 10.*val2 + monitor(m)-48
				else
					val2 = val2 + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
		flux_pv = val2
		time_pv = dble(tv_sec) + dble(tv_micro)/1000000.
133	continue

137	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_384_events5( argc, argv)

!DLL_EXPORT maia_384_events5
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.39) then 		         ! needed # args
	maia_384_events5 = 1
	return
endif

call maia_384_events5_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), &
	 %val(argv(29)), %val(argv(30)), %val(argv(31)), &
	 %val(argv(32)), %val(argv(33)), %val(argv(34)), &
	 %val(argv(35)), %val(argv(36)), %val(argv(37)), %val(argv(38)), &
	 %val(argv(39)) )

maia_384_events5 = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of maia_384_events with simpler vector events extended to include flux, dwell, dead-time, ...
! using the addition of pseudo events to hold new values of BT, FC0, FC1.
! Now with SKIP as a long.
!
! monitor returns:
!	monitor	strings		nm (returned length)
!
!	xy_correct			0 normal, 1 Y encoder advance mode, 2 filter +-16 glitches
!	xmargin				pixel X margin boundary width
!	width				width of scan needed for Y encoder mode
!					number of detector channels (spectrum mode) ???
!
!	beam_energy			current value of beam energy (zero externally before sort)
!	flux_pv				current value of flux Epics PV (zero externally)
!	time_pv				time-stamp for time_pv
!
!	flux_mode	0		epics PV		fx index 0		flux selected (PV or FCn)
!			1		FC1 h/w				1		FC1
!			2		FC2 h/w				2		FC2
!									3		Dwell

SUBROUTINE maia_384_events5_b( ev,n_buffer, channel_on,nc, e,t,x,y,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0, monitor,n_monitor,nm, &
		energy_string,n_es, beam_energy, flux_string,n_fs, flux_pv, time_pv, time_first, &
		xy_correct,xmargin,width, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for selected flux, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, n_es, n_fs, n_events, n_fx, flux_mode
INTEGER*4 length, len2, width, skip
INTEGER*4 n_mon_buff, mbuff
INTEGER*2 tag, ibranch, x0,y0, word_mask
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
INTEGER*1 energy_string(0:n_es-1), flux_string(0:n_fs-1)
REAL*4 factor, val, val2, fx(0:n_fx-1,0:n_events-1), flux_pv, beam_energy
REAL*8 time_pv, time_first
LOGICAL incomplete, first, whole
Integer*4 xmargin, xy_correct
INTEGER*1 monitor(0:n_monitor-1)
INTEGER*4 nm, n_monitor

parameter (n_mon_buff = 20000)
INTEGER*1 a_tag, b_tag, mon_buff(0:n_mon_buff-1)
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask, tx, ty, tz
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4
INTEGER*4 pa_sign_bit_mask4, pa_sign_extend, pa_tag4, pa_tag_offset4
INTEGER*2 maia_monitor, maia_events_1
Integer*2 ldone, rdone, ldone2, rdone2
INTEGER*4 xyz_tag_mask4, xyz_tag_x4, xyz_tag_y4, xyz_tag_z4, xyz_pos_mask4, xyz_tag_pa4 
INTEGER*4 pa_tag_mask4, pa_tag_x4, pa_tag_y4, pa_tag_z4, pa_tag_tf4
INTEGER*4 tf_tag_mask4, tf_tag_bt4, tf_tag_fc1, tf_tag_fc2, tf_bit_mask4 

common /c_maia_1/ jbuff, bev, remain, first
common /c_maia_3/ mbuff, mon_buff
common /c_maia_4/ ldone, rdone, ldone2, rdone2
common /c_maia_6/ tv_sec, tv_micro
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_1 = 34, maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET2 format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET3 format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -31, tag_offset3 = -31 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31 )
parameter ( pa_mask4 = z'07FFFFFF', pa_sign_bit_mask4 = z'04000000', pa_sign_extend = z'F8000000')
parameter ( xyz_tag_mask4 = z'E0000000', xyz_tag_x4 = z'80000000', xyz_tag_y4 =	z'A0000000', xyz_tag_z4 = z'C0000000' )
parameter ( xyz_pos_mask4 = z'07FFFFFF', xyz_tag_pa4 = z'E0000000' )
parameter ( pa_tag_mask4 = z'F8000000', pa_tag_x4 = z'E0000000', pa_tag_y4 = z'E8000000', pa_tag_z4 = z'F0000000' )
parameter ( pa_tag_tf4 = z'F8000000', tf_tag_mask4 = z'FE000000', tf_tag_bt4 =z'F8000000', tf_tag_fc1 =	z'FA000000' )
parameter ( tf_tag_fc2 = z'FC000000', tf_bit_mask4 = z'01FFFFFF' )

n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		idebug = 91
!		return
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length, first 16 bits unsigned)
	length = bev(0)
	length = iand(length,byte_mask)
	len2 = bev(1)
	len2 = iand(len2,byte_mask)
	length = ior( ishft( length, 8), len2)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (20 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	if( time_first.eq.0.0) time_first = dble(tv_sec) + dble(tv_micro)/1000000.
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8

80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_1) then		     ! event_1 (ET event, type #4), they contain XYZ too
		remain = length
		first = .true.
		ibranch = 15
		goto 150
	endif
	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 98
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 97
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          								! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDF overflow bit
!				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 96
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return			! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 95
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          		! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3) 	! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'maia_events_1' event (was maia_ET_events_4) ...

150	if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif

!   Process payload data while it lasts ...
!   First 3 words of ET4 events is PA pixel XYZ information. This is assumed for tagging event E,T data with X,Y.
!   Also need to assume that PA Y always follows PA X.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words of payload

	tx = iand( tev, tag_mask4)
	if(tx.eq.0) then							! ET

		dtag = 0
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv			! put current val in each event
				endif
				n = n+1
			endif
		endif
	else
		tx = iand( tev, xyz_tag_mask4)
		if(tx.eq.xyz_tag_x4) then					! Encoder X
			tx =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_y4) then					! Encoder Y
			ty =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_z4) then					! Encoder Z
			tz =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_pa4) then
			tx = iand( tev, pa_tag_mask4)
			if(tx.eq.pa_tag_x4) then				! PA X
				tx =  iand( tev, pa_mask4)
				if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
				if(xy_correct.eq.2) then
					if(tx.ne.x0) then
						if((tx.eq.x0-15).or.(tx.eq.x0+17)) then
							x0 = x0+1
						else if((tx.eq.x0-17).or.(tx.eq.x0+15)) then
							x0 = x0-1
						else if((tx.eq.x0-16).or.(tx.eq.x0+16)) then
							x0 = x0
						else 
							x0 = tx
						endif
					endif
				else
					x0 = tx
				endif
			else if(tx.eq.pa_tag_y4) then				! PA Y
				ty =  iand( tev, pa_mask4)
				if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
				if(xy_correct.eq.1) then
					idebug = ty
					if(x0.lt.0) x0 = -x0
					if(x0.lt.xmargin) then
						if(ty.gt.y0) then
							if(ldone.eq.0) y0 = y0+1
							ldone = 1
							rdone = 0
						endif
					endif
					if(width-1-x0.lt.xmargin) then
						if(ty.gt.y0) then
							if(rdone.eq.0) y0 = y0+1
							rdone = 1
							ldone = 0
						endif
					endif
				else
					if(xy_correct.eq.2) then
						if(ty.ne.y0) then
							if((ty.ne.y0-16).and.(ty.ne.y0+16)) y0 = ty
						endif
					else
						y0 = ty
					endif
				endif

			else if(tx.eq.pa_tag_z4) then				! PA Z
				tz =  iand( tev, pa_mask4)
				if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
		
			else if(tx.eq.pa_tag_tf4) then				! Time-Flux

!				These are assumed to follow AFTER x0,y0 are set from PA ...

				tx = iand( tev, tf_tag_mask4)
				if(tx.eq.tf_tag_bt4) then			! BT (ms)
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0							! add a pseudo event
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1							! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(3,n) = ty * 0.0001			! dwell (ms)
					n = n+1

				else if(tx.eq.tf_tag_fc1) then			! Flux 1
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0							! add a pseudo event
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1							! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(1,n) = ty
					if (flux_mode.eq.0) then		! use Epics PV
						fx(0,n) = flux_pv		! put current val in each event
					else if (flux_mode.eq.1) then
						fx(0,n) = ty			! h/w will be added to pseudo event
					endif
					n = n+1

				else if(tx.eq.tf_tag_fc2) then			! Flux 2
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0							! add a pseudo event
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1							! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(2,n) = ty
					if (flux_mode.eq.0) then			! use Epics PV
						fx(0,n) = flux_pv				! put current val in each event
					else if (flux_mode.eq.2) then
						fx(0,n) = ty					! h/w will be added to pseudo event
					endif
					n = n+1

				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 150

!...................................................................................
!   Process the 'monitor' event ...

130	if(length.gt.n_mon_buff) then				! monitor too long
		err = 1						! for mon_buff buffer
		idebug = 1301
		return
	endif
 	j = length		         			! data long words of payload

	call get_maia_32( ev, i, n_buffer, mon_buff, j, mbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return 

	do k=0,j-1						! completed all monitor
		monitor(nm+k) = mon_buff(k)			! copy buffer to 'monitor'
	enddo

	if((j.lt.n_es+4).or.(n_es.eq.0)) goto 132		! skip if short
	val = 0.0
	val2 = 0.0
	do 131 k=nm,nm+j-n_es					! check for "Energy" monitor
		do l=0,n_es-1
			if(energy_string(l).ne.monitor(k+l)) goto 131
		enddo
		l = k+n_es					! found energy monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 132				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val = 10.*val + monitor(m)-48
				else
					val = val + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
		beam_energy = val
131	continue

132	if((j.lt.n_fs+4).or.(n_fs.eq.0)) goto 137		! skip if short
	do 133 k=nm,nm+j-n_fs					! check for "Flux" monitor
		do l=0,n_fs-1
			if(flux_string(l).ne.monitor(k+l)) goto 133
		enddo
		l = k+n_fs					! found flux monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 137				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val2 = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val2 = 10.*val2 + monitor(m)-48
				else
					val2 = val2 + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
		flux_pv = val2
		time_pv = dble(tv_sec) + dble(tv_micro)/1000000.
133	continue

137	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_384_events4( argc, argv)

!DLL_EXPORT maia_384_events4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.39) then 		         ! needed # args
	maia_384_events4 = 1
	return
endif

call maia_384_events4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), &
	 %val(argv(29)), %val(argv(30)), %val(argv(31)), &
	 %val(argv(32)), %val(argv(33)), %val(argv(34)), &
	 %val(argv(35)), %val(argv(36)), %val(argv(37)), %val(argv(38)), &
	 %val(argv(39)) )

maia_384_events4 = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of maia_384_events with simpler vector events extended to include flux, dwell, dead-time, ...
! using the addition of pseudo events to hold new values of BT, FC0, FC1.
!
! monitor returns:
!	monitor	strings		nm (returned length)
!
!	xy_correct			0 normal, 1 Y encoder advance mode, 2 filter +-16 glitches
!	xmargin				pixel X margin boundary width
!	width				width of scan needed for Y encoder mode
!					number of detector channels (spectrum mode) ???
!
!	beam_energy			current value of bweam energy (zero externally before sort)
!	flux_pv				current value of flux Epics PV (zero externally)
!	time_pv				time-stamp for time_pv
!
!	flux_mode	0		epics PV		fx index 0		flux selected (PV or FCn)
!				1		FC1 h/w					1		FC1
!				2		FC2 h/w					2		FC2
!												3		Dwell

SUBROUTINE maia_384_events4_b( ev,n_buffer, channel_on,nc, e,t,x,y,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode, x0,y0, monitor,n_monitor,nm, &
		energy_string,n_es, beam_energy, flux_string,n_fs, flux_pv, time_pv, time_first, &
		xy_correct,xmargin,width, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 4 (at least, for selected flux, FC0, FC1, Dwell)

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, n_es, n_fs, n_events, n_fx, flux_mode
INTEGER*4 length, len2, width
INTEGER*4 n_mon_buff, mbuff
INTEGER*2 tag, ibranch, x0,y0, skip, word_mask
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
INTEGER*1 energy_string(0:n_es-1), flux_string(0:n_fs-1)
REAL*4 factor, val, val2, fx(0:n_fx-1,0:n_events-1), flux_pv, beam_energy
REAL*8 time_pv, time_first
LOGICAL incomplete, first, whole
Integer*4 xmargin, xy_correct
INTEGER*1 monitor(0:n_monitor-1)
INTEGER*4 nm, n_monitor

parameter (n_mon_buff = 20000)
INTEGER*1 a_tag, b_tag, mon_buff(0:n_mon_buff-1)
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask, tx, ty, tz
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4
INTEGER*4 pa_sign_bit_mask4, pa_sign_extend, pa_tag4, pa_tag_offset4
INTEGER*2 maia_monitor, maia_events_1
Integer*2 ldone, rdone, ldone2, rdone2
INTEGER*4 xyz_tag_mask4, xyz_tag_x4, xyz_tag_y4, xyz_tag_z4, xyz_pos_mask4, xyz_tag_pa4 
INTEGER*4 pa_tag_mask4, pa_tag_x4, pa_tag_y4, pa_tag_z4, pa_tag_tf4
INTEGER*4 tf_tag_mask4, tf_tag_bt4, tf_tag_fc1, tf_tag_fc2, tf_bit_mask4 

common /c_maia_1/ jbuff, bev, remain, first
common /c_maia_3/ mbuff, mon_buff
common /c_maia_4/ ldone, rdone, ldone2, rdone2
common /c_maia_6/ tv_sec, tv_micro
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_1 = 34, maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET2 format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET3 format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -31, tag_offset3 = -31 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31 )
parameter ( pa_mask4 = z'07FFFFFF', pa_sign_bit_mask4 = z'04000000', pa_sign_extend = z'F8000000')
parameter ( xyz_tag_mask4 = z'E0000000', xyz_tag_x4 = z'80000000', xyz_tag_y4 =	z'A0000000', xyz_tag_z4 = z'C0000000' )
parameter ( xyz_pos_mask4 = z'07FFFFFF', xyz_tag_pa4 = z'E0000000' )
parameter ( pa_tag_mask4 = z'F8000000', pa_tag_x4 = z'E0000000', pa_tag_y4 = z'E8000000', pa_tag_z4 = z'F0000000' )
parameter ( pa_tag_tf4 = z'F8000000', tf_tag_mask4 = z'FE000000', tf_tag_bt4 =z'F8000000', tf_tag_fc1 =	z'FA000000' )
parameter ( tf_tag_fc2 = z'FC000000', tf_bit_mask4 = z'01FFFFFF' )

n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		idebug = 91
!		return
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	length = iand(length,byte_mask)
	len2 = bev(1)
	len2 = iand(len2,byte_mask)
	length = ior( ishft( length, 8), len2)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (20 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	if( time_first.eq.0.0) time_first = dble(tv_sec) + dble(tv_micro)/1000000.
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8

80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_1) then		     ! event_1 (ET event, type #4), they contain XYZ too
		remain = length
		first = .true.
		ibranch = 15
		goto 150
	endif
	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 98
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 97
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          								! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDF overflow bit
!				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 96
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return			! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 95
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          		! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3) 	! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'maia_events_1' event (was maia_ET_events_4) ...

150	if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif

!   Process payload data while it lasts ...
!   First 3 words of ET4 events is PA pixel XYZ information. This is assumed for tagging event E,T data with X,Y.
!   Also need to assume that PA Y always follows PA X.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words of payload

	tx = iand( tev, tag_mask4)
	if(tx.eq.0) then							! ET

!		These are assumed to follow AFTER x0,y0 are set from PA ...

		dtag = 0
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = dtag
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				if (flux_mode.eq.0) then			! use Epics PV
					fx(0,n) = flux_pv				! put current val in each event
				endif
				n = n+1
			endif
		endif
	else
		tx = iand( tev, xyz_tag_mask4)
		if(tx.eq.xyz_tag_x4) then					! Encoder X
			tx =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_y4) then					! Encoder Y
			ty =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_z4) then					! Encoder Z
			tz =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_pa4) then
			tx = iand( tev, pa_tag_mask4)
			if(tx.eq.pa_tag_x4) then				! PA X
				tx =  iand( tev, pa_mask4)
				if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
				if(xy_correct.eq.2) then
					if(tx.ne.x0) then
						if((tx.eq.x0-15).or.(tx.eq.x0+17)) then
							x0 = x0+1
						else if((tx.eq.x0-17).or.(tx.eq.x0+15)) then
							x0 = x0-1
						else if((tx.eq.x0-16).or.(tx.eq.x0+16)) then
							x0 = x0
						else 
							x0 = tx
						endif
					endif
				else
					x0 = tx
				endif
			else if(tx.eq.pa_tag_y4) then				! PA Y
				ty =  iand( tev, pa_mask4)
				if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
				if(xy_correct.eq.1) then
					idebug = ty
					if(x0.lt.0) x0 = -x0
					if(x0.lt.xmargin) then
						if(ty.gt.y0) then
							if(ldone.eq.0) y0 = y0+1
							ldone = 1
							rdone = 0
						endif
					endif
					if(width-1-x0.lt.xmargin) then
						if(ty.gt.y0) then
							if(rdone.eq.0) y0 = y0+1
							rdone = 1
							ldone = 0
						endif
					endif
				else
					if(xy_correct.eq.2) then
						if(ty.ne.y0) then
							if((ty.ne.y0-16).and.(ty.ne.y0+16)) y0 = ty
						endif
					else
						y0 = ty
					endif
				endif

			else if(tx.eq.pa_tag_z4) then				! PA Z
				tz =  iand( tev, pa_mask4)
				if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
		
			else if(tx.eq.pa_tag_tf4) then				! Time-Flux

!				These are assumed to follow AFTER x0,y0 are set from PA ...

				tx = iand( tev, tf_tag_mask4)
				if(tx.eq.tf_tag_bt4) then			! BT (ms)
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0							! add a pseudo event
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1							! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(3,n) = ty * 0.0001				! dwell (ms)
					n = n+1

				else if(tx.eq.tf_tag_fc1) then			! Flux 1
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0							! add a pseudo event
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1							! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(1,n) = ty
					if (flux_mode.eq.0) then			! use Epics PV
						fx(0,n) = flux_pv				! put current val in each event
					else if (flux_mode.eq.1) then
						fx(0,n) = ty					! h/w will be added to pseudo event
					endif
					n = n+1

				else if(tx.eq.tf_tag_fc2) then			! Flux 2
					ty = iand( tev, tf_bit_mask4)
					x(n) = x0
					y(n) = y0							! add a pseudo event
					e(n) = 0
					t(n) = 0
					ste(n) = 0
					tags(n) = 0
					veto(n) = 1							! will veto adding to images/spectra
					do k=0,n_fx-1
						fx(k,n) = 0.0
					enddo
					fx(2,n) = ty
					if (flux_mode.eq.0) then			! use Epics PV
						fx(0,n) = flux_pv				! put current val in each event
					else if (flux_mode.eq.2) then
						fx(0,n) = ty					! h/w will be added to pseudo event
					endif
					n = n+1

				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 150

!...................................................................................
!   Process the 'monitor' event ...

130	if(length.gt.n_mon_buff) then				! monitor too long
		err = 1						! for mon_buff buffer
		idebug = 1301
		return
	endif
 	j = length		         			! data long words of payload

	call get_maia_32( ev, i, n_buffer, mon_buff, j, mbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return 

	do k=0,j-1						! completed all monitor
		monitor(nm+k) = mon_buff(k)			! copy buffer to 'monitor'
	enddo

	if((j.lt.n_es+4).or.(n_es.eq.0)) goto 132		! skip if short
	val = 0.0
	val2 = 0.0
	do 131 k=nm,nm+j-n_es					! check for "Energy" monitor
		do l=0,n_es-1
			if(energy_string(l).ne.monitor(k+l)) goto 131
		enddo
		l = k+n_es					! found energy monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 132				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val = 10.*val + monitor(m)-48
				else
					val = val + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
		beam_energy = val
131	continue

132	if((j.lt.n_fs+4).or.(n_fs.eq.0)) goto 137		! skip if short
	do 133 k=nm,nm+j-n_fs					! check for "Flux" monitor
		do l=0,n_fs-1
			if(flux_string(l).ne.monitor(k+l)) goto 133
		enddo
		l = k+n_fs					! found flux monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 137				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val2 = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val2 = 10.*val2 + monitor(m)-48
				else
					val2 = val2 + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
		flux_pv = val2
		time_pv = dble(tv_sec) + dble(tv_micro)/1000000.
133	continue

137	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_384_events3( argc, argv)

!DLL_EXPORT maia_384_events3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.42) then 		         ! needed # args
	maia_384_events3 = 1
	return
endif

call maia_384_events3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), &
	 %val(argv(29)), %val(argv(30)), %val(argv(31)), &
	 %val(argv(32)), %val(argv(33)), %val(argv(34)), &
	 %val(argv(35)), %val(argv(36)), %val(argv(37)), %val(argv(38)), &
	 %val(argv(39)), %val(argv(40)), %val(argv(41)), %val(argv(42)) )

maia_384_events3 = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of maia_384_events that h/w Flux counters, dead-time (TT) and dwell
!
! monitor returns (indexed by nt):
!	time_sec, time_micro		time stamp of monitor event
!	beam, flux			energy, flux IC value in monitor
!	index(x,dy)			index by x,dy into monitor list (nt)
!
! partial table accumulators (indexed by x,dy offset by ybase [y0 on entry])
!	dwell(x,dy)			dwell (ms) partial table (x,dy)
!	fc1(x,dy)			Flux 1 partial table (x,dy)
!	fc2(x,dy)			Flux 2 partial table (x,dy)
!	tt(x,dy)			TT sum partial table (x,dy)
!
!	xy_correct			0 normal, 1 Y encoder advance mode, 2 filter +-16 glitches
!	xmargin				pixel X margin boundary width
!	width				width of X scan (and index, dwell, fc1, fc2 arrays)
!					number of detector channels (spectrum mode)
!	dheight				height of index, dwell, fc1, fc2 buffers only
!	width=ndet,dheight=1		scalar spectrum mode

SUBROUTINE maia_384_events3_b( ev,n_buffer, channel_on,nc, e,t,x,y,n,ste,tags, &
		ibranch,swap, tag,length,skip, x0,y0, monitor,n_monitor,nm, &
		time_sec,time_micro,beam,flux,n_times,nt, bad,idebug, &
		energy_string,n_es, flux_string,n_fs, index, &
		xy_correct, xmargin, width, dheight, fc1, fc2, dwell, tt )

! assumes that n_buffer IS divisable by 4

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, nm, n_monitor, n_es, n_fs
INTEGER*4 length, len2, n_times,nt
INTEGER*4 time_sec(0:n_times-1),time_micro(0:n_times-1)
INTEGER*4 n_mon_buff, mbuff
REAL*4 beam(0:n_times-1), flux(0:n_times-1)
INTEGER*2 tag, ibranch, x0,y0, skip, word_mask
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*2 ste(0:n_buffer/4-1), tags(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3), monitor(0:n_monitor-1)
INTEGER*1 energy_string(0:n_es-1), flux_string(0:n_fs-1)
REAL*4 factor, val, val2
LOGICAL incomplete, first, whole, spectrum_mode
Integer*4 xmargin, xy_correct, width, dheight, index(0:width-1,0:dheight-1), ybase
Integer*4 fc1(0:width-1,0:dheight-1), fc2(0:width-1,0:dheight-1)
REAL*4 dwell(0:width-1,0:dheight-1), tt(0:width-1,0:dheight-1)

parameter (n_mon_buff = 20000)
INTEGER*1 a_tag, b_tag, mon_buff(0:n_mon_buff-1)
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask, tx, ty, tz
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4
INTEGER*4 pa_sign_bit_mask4, pa_sign_extend, pa_tag4, pa_tag_offset4
INTEGER*2 maia_monitor, maia_events_1
Integer*2 ldone, rdone, ldone2, rdone2
INTEGER*4 xyz_tag_mask4, xyz_tag_x4, xyz_tag_y4, xyz_tag_z4, xyz_pos_mask4, xyz_tag_pa4 
INTEGER*4 pa_tag_mask4, pa_tag_x4, pa_tag_y4, pa_tag_z4, pa_tag_tf4
INTEGER*4 tf_tag_mask4, tf_tag_bt4, tf_tag_fc1, tf_tag_fc2, tf_bit_mask4 

common /c_maia_1/ jbuff, bev, remain, first
common /c_maia_3/ mbuff, mon_buff
common /c_maia_4/ ldone, rdone, ldone2, rdone2
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_1 = 34, maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -31, tag_offset3 = -31 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31 )
parameter ( pa_mask4 = z'07FFFFFF', pa_sign_bit_mask4 = z'04000000', pa_sign_extend = z'F8000000')
parameter ( xyz_tag_mask4 = z'E0000000', xyz_tag_x4 = z'80000000', xyz_tag_y4 =	z'A0000000', xyz_tag_z4 = z'C0000000' )
parameter ( xyz_pos_mask4 = z'07FFFFFF', xyz_tag_pa4 = z'E0000000' )
parameter ( pa_tag_mask4 = z'F8000000', pa_tag_x4 = z'E0000000', pa_tag_y4 = z'E8000000', pa_tag_z4 = z'F0000000' )
parameter ( pa_tag_tf4 = z'F8000000', tf_tag_mask4 = z'FE000000', tf_tag_bt4 =z'F8000000', tf_tag_fc1 =	z'FA000000' )
parameter ( tf_tag_fc2 = z'FC000000', tf_bit_mask4 = z'01FFFFFF' )

n_max = n_buffer/4
n = 0
i = 0
nm = 0
nt = 0
spectrum_mode = .false.
if(dheight.eq.1) then
	spectrum_mode = .true.
	xy_correct = 0
endif
ybase = y0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		idebug = 91
!		return
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	length = iand(length,byte_mask)
	len2 = bev(1)
	len2 = iand(len2,byte_mask)
	length = ior( ishft( length, 8), len2)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (16 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8


80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_1) then		     ! event_1 (ET event, type #4), they contain XYZ too
		remain = length
		first = .true.
		ibranch = 15
		goto 150
	endif
	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 98
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 97
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          								! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
		if(spectrum_mode) then
			index(0,0) = nt-1			! cross-reference last nt
		else
			if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
				index(x0,y0-ybase) = nt-1	! cross-reference last nt for each x0,y0
			endif
		endif
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
				if(spectrum_mode.and.(adr.ge.0).and.(adr.lt.width)) then
					tt(adr,0) = tt(adr,0) + dt	! T sum in spectrum mode
				else
					if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
						tt(x0,y0-ybase) = tt(x0,y0-ybase) + dt
					endif
				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 96
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return			! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
		if(spectrum_mode) then
			index(0,0) = nt-1			! cross-reference last nt
		else
			if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
				index(x0,y0-ybase) = nt-1	! cross-reference last nt for each x0,y0
			endif
		endif
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
				if(spectrum_mode.and.(adr.ge.0).and.(adr.lt.width)) then
					tt(adr,0) = tt(adr,0) + dt	! T sum in spectrum mode
				else
					if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
						tt(x0,y0-ybase) = tt(x0,y0-ybase) + dt
					endif
				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 95
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          		! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
		if(spectrum_mode) then
			index(0,0) = nt-1				! cross-reference last nt
		else
			if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
				index(x0,y0-ybase) = nt-1	! cross-reference last nt for each x0,y0
			endif
		endif
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3) 	! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				n = n+1
				if(spectrum_mode.and.(adr.ge.0).and.(adr.lt.width)) then
					tt(adr,0) = tt(adr,0) + dt	! T sum in spectrum mode
				else
					if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
						tt(x0,y0-ybase) = tt(x0,y0-ybase) + dt
					endif
				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'maia_events_1' event (was maia_ET_events_4) ...

150	if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif

!   Process payload data while it lasts ...
!   First 3 words of ET4 events is PA pixel XYZ information, but we won't assume this (use masks).
!   However, need to assume that PA Y always follows PA X.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words of payload

	tx = iand( tev, tag_mask4)
	if(tx.eq.0) then							! ET

!		These are assumed to follow AFTER x0,y0 are set from PA ...

		dtag = 0
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = dtag
				n = n+1
				if(spectrum_mode.and.(adr.ge.0).and.(adr.lt.width)) then
					tt(adr,0) = tt(adr,0) + dt	! T sum in spectrum mode
				else
					if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
						tt(x0,y0-ybase) = tt(x0,y0-ybase) + dt
					endif
				endif
			endif
		endif
	else
		tx = iand( tev, xyz_tag_mask4)
		if(tx.eq.xyz_tag_x4) then					! Encoder X
			tx =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_y4) then					! Encoder Y
			ty =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_z4) then					! Encoder Z
			tz =  iand( tev, xyz_pos_mask4 )

		else if(tx.eq.xyz_tag_pa4) then
			tx = iand( tev, pa_tag_mask4)
			if(tx.eq.pa_tag_x4) then				! PA X
				tx =  iand( tev, pa_mask4)
				if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
				if(xy_correct.eq.2) then
					if(tx.ne.x0) then
						if((tx.eq.x0-15).or.(tx.eq.x0+17)) then
							x0 = x0+1
						else if((tx.eq.x0-17).or.(tx.eq.x0+15)) then
							x0 = x0-1
						else if((tx.eq.x0-16).or.(tx.eq.x0+16)) then
							x0 = x0
						else 
							x0 = tx
						endif
					endif
				else
					x0 = tx
				endif
			else if(tx.eq.pa_tag_y4) then				! PA Y
				ty =  iand( tev, pa_mask4)
				if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
				if(xy_correct.eq.1) then
					idebug = ty
					if(x0.lt.0) x0 = -x0
					if(x0.lt.xmargin) then
						if(ty.gt.y0) then
							if(ldone.eq.0) y0 = y0+1
							ldone = 1
							rdone = 0
						endif
					endif
					if(width-1-x0.lt.xmargin) then
						if(ty.gt.y0) then
							if(rdone.eq.0) y0 = y0+1
							rdone = 1
							ldone = 0
						endif
					endif
				else
					if(xy_correct.eq.2) then
						if(ty.ne.y0) then
							if((ty.ne.y0-16).and.(ty.ne.y0+16)) y0 = ty
						endif
					else
						y0 = ty
					endif
				endif

!				Assume that Y always follows X to set the index ...

				if(spectrum_mode) then
					index(0,0) = nt-1			! cross-reference last nt
				else
					if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
						index(x0,y0-ybase) = nt-1	! cross-reference last nt for each x0,y0
					endif
				endif
			else if(tx.eq.pa_tag_z4) then				! PA Z
				tz =  iand( tev, pa_mask4)
				if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
		
			else if(tx.eq.pa_tag_tf4) then				! Time-Flux

!				These are assumed to follow AFTER x0,y0 are set from PA ...

				tx = iand( tev, tf_tag_mask4)
				if(tx.eq.tf_tag_bt4) then			! BT (ms)
					ty = iand( tev, tf_bit_mask4)
					if(spectrum_mode) then
						dwell(0,0) = dwell(0,0) + (ty / 10000.)
					else
						if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
							dwell(x0,y0-ybase) = dwell(x0,y0-ybase) + (ty / 10000.)
						endif
					endif

				else if(tx.eq.tf_tag_fc1) then			! Flux 1
					ty = iand( tev, tf_bit_mask4)
					if(spectrum_mode) then
						fc1(0,0) = fc1(0,0) + ty
					else
						if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
							fc1(x0,y0-ybase) = fc1(x0,y0-ybase) + ty
						endif
					endif

				else if(tx.eq.tf_tag_fc2) then			! Flux 2
					ty = iand( tev, tf_bit_mask4)
					if(spectrum_mode) then
						fc2(0,0) = fc2(0,0) + ty
					else
						if((x0.ge.0).and.(x0.lt.width).and.(y0-ybase.ge.0).and.(y0-ybase.lt.dheight)) then
							fc2(x0,y0-ybase) = fc2(x0,y0-ybase) + ty
						endif
					endif
				endif
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 150

!...................................................................................
!   Process the 'monitor' event ...

130	if(length.gt.n_mon_buff) then				! monitor too long
		err = 1						! for mon_buff buffer
		idebug = 1301
		return
	endif
 	j = length		         			! data long words of payload

	call get_maia_32( ev, i, n_buffer, mon_buff, j, mbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return 

	do k=0,j-1						! completed all monitor
		monitor(nm+k) = mon_buff(k)			! copy buffer to 'monitor'
	enddo

	if((j.lt.n_es+4).or.(n_es.eq.0)) goto 132		! skip if short
	val = 0.0
	val2 = 0.0
	do 131 k=nm,nm+j-n_es					! check for "Energy" monitor
		do l=0,n_es-1
			if(energy_string(l).ne.monitor(k+l)) goto 131
		enddo
		l = k+n_es					! found energy monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 132				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val = 10.*val + monitor(m)-48
				else
					val = val + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
131	continue


132	if((j.lt.n_fs+4).or.(n_fs.eq.0)) goto 137		! skip if short
	do 133 k=nm,nm+j-n_fs					! check for "Flux" monitor
		do l=0,n_fs-1
			if(flux_string(l).ne.monitor(k+l)) goto 133
		enddo
		l = k+n_fs					! found flux monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 137				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val2 = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val2 = 10.*val2 + monitor(m)-48
				else
					val2 = val2 + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
133	continue


137	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif
	if(nt.lt.n_times) then
		time_sec(nt) = tv_sec				! save time stamps
		if(tv_sec.lt.1266400000) idebug=11
		time_micro(nt) = tv_micro
		beam(nt) = val					! beam energy
		flux(nt) = val2					! flux rate
		nt = nt+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_384_events2( argc, argv)

!DLL_EXPORT maia_384_events2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.42) then 		         ! needed # args
	maia_384_events2 = 1
	return
endif

call maia_384_events2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), &
	 %val(argv(29)), %val(argv(30)), %val(argv(31)), &
	 %val(argv(32)), %val(argv(33)), %val(argv(34)), &
	 %val(argv(35)), %val(argv(36)), %val(argv(37)), %val(argv(38)), &
	 %val(argv(39)), %val(argv(40)), %val(argv(41)), %val(argv(42)) )

maia_384_events2 = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of maia_384_events that adds XANES energy "boundaries" array "energy".
!
! If n_energy > 0 then advance x0 on n_record of ET events, else ignore XANES.
! 
! monitor returns (indexed by nt):
!	time_sec, time_micro		time stamp of monitor event
!	beam, flux			energy, flux IC value in monitor
!	index(x)			index by x (ET record) into monitor nt
!	n_record			total number of ET records (in this block)
!
! energy, n_energy	energy lookup-table (not used now)
!
!	xy_correct			0 normal, 1 Y encoder advance mode, 2 filter +-16 glitches
!	xmargin				pixel X margin boundary width
!	width				width of X scan

SUBROUTINE maia_384_events2_b( ev,n_buffer, channel_on,nc, e,t,x,y,n,ste,tags, &
		ibranch,swap, tag,length,skip, x0,y0, monitor,n_monitor,nm, &
		time_sec,time_micro,beam,flux,n_times,nt, bad,idebug, energy,n_energy, &
		energy_string,n_es, flux_string,n_fs, index,n_index, n_record, &
		et_time, xy_correct, xmargin, width)

! assumes that n_buffer IS divisable by 4

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, nm, n_monitor, n_es, n_fs, n_energy
INTEGER*4 length, len2, n_times,nt
INTEGER*4 time_sec(0:n_times-1),time_micro(0:n_times-1)
INTEGER*4 n_index, n_record, index(0:n_index-1), n_mon_buff, mbuff
REAL*4 beam(0:n_times-1), flux(0:n_times-1)
REAL*8 et_time(0:n_index-1), time_stamp
INTEGER*2 tag, ibranch, x0,y0, skip, word_mask
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*2 ste(0:n_buffer/4-1), tags(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3), monitor(0:n_monitor-1)
INTEGER*1 energy_string(0:n_es-1), flux_string(0:n_fs-1)
REAL*4 energy(0:n_energy-1), factor, val, val2
LOGICAL incomplete, first, whole
Integer*4 xmargin, xy_correct, width

parameter (n_mon_buff = 20000)
INTEGER*1 a_tag, b_tag, mon_buff(0:n_mon_buff-1)
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask, tx, ty, tz
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4
INTEGER*4 pa_tag_mask4, pa_sign_bit_mask4, pa_sign_extend, pa_tag4, pa_tag_offset4
INTEGER*2 maia_monitor, maia_events_1, xyz_count 
Integer*2 ldone, rdone, ldone2, rdone2

common /c_maia_1/ jbuff, bev, remain, first
common /c_maia_2/ time_stamp
common /c_maia_3/ mbuff, mon_buff
common /c_maia_4/ ldone, rdone, ldone2, rdone2
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_1 = 34, maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -31, tag_offset3 = -31 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22, pa_sign_bit_mask4 = z'04000000' )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31, pa_tag4 = z'E0000000' )
parameter ( pa_offset4 = 0, pa_mask4 = z'07FFFFFF', pa_tag_offset4 = -27, pa_tag_mask4 = z'E0000000', pa_sign_extend = z'F8000000')

n_max = n_buffer/4
n = 0
i = 0
nm = 0
nt = 0
n_record = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		idebug = 91
!		return
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	length = iand(length,byte_mask)
	len2 = bev(1)
	len2 = iand(len2,byte_mask)
	length = ior( ishft( length, 8), len2)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (16 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8


80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_1) then		     ! event_1 (ET event, type #4), they contain XYZ too
		remain = length
		first = .true.
		xyz_count = 1
		n_record = n_record+1
		ibranch = 15
		goto 150
	endif
	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		n_record = n_record+1
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		n_record = n_record+1
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		n_record = n_record+1
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 98
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 97
		goto 10
	endif
	if((n_energy.ne.0).and.(n_record.lt.32768)) then
		if(first) then
			time_stamp = tv_sec + tv_micro*0.000001		! ET time stamp
		endif			
		if(n_record.gt.0) then
			x0 = n_record-1
		endif
		if((x0.ge.0).and.(x0.lt.n_index)) then
			index(x0) = nt-1				! cross-reference last nt for each x0
			et_time(x0) = time_stamp			! ET time stamp
		endif
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          								! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		if(n_energy.eq.0) x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 96
		goto 10
	endif
	if((n_energy.ne.0).and.(n_record.lt.32768)) then
		if(first) then
			time_stamp = tv_sec + tv_micro*0.000001		! ET time stamp
		endif			
		if(n_record.gt.0) then
			x0 = n_record-1
		endif
		if((x0.ge.0).and.(x0.lt.n_index)) then
			index(x0) = nt-1				! cross-reference last nt for each x0
			et_time(x0) = time_stamp			! ET time stamp
		endif
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return			! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		if(n_energy.eq.0) x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 95
		goto 10
	endif
	if((n_energy.ne.0).and.(n_record.lt.32768)) then
		if(first) then
			time_stamp = tv_sec + tv_micro*0.000001		! ET time stamp
		endif			
		if(n_record.gt.0) then
			x0 = n_record-1
		endif
		if((x0.ge.0).and.(x0.lt.n_index)) then
			index(x0) = nt-1				! cross-reference last nt for each x0
			et_time(x0) = time_stamp			! ET time stamp
		endif
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          		! data long words of payload

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		if(n_energy.eq.0) x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3)   ! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'maia_events_1' event (was maia_ET_events_4) ...

150	if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif
	if((n_energy.ne.0).and.(n_record.lt.32768)) then
		if(first) then
			time_stamp = tv_sec + tv_micro*0.000001		! ET time stamp
		endif			
		if(n_record.gt.0) then
			x0 = n_record-1
		endif
		if((x0.ge.0).and.(x0.lt.n_index)) then
			index(x0) = nt-1				! cross-reference last nt for each x0
			et_time(x0) = time_stamp			! ET time stamp
		endif
	endif

!   Process payload data while it lasts ...
!   First 3 words of ET4 events is PA pixel XYZ information (keep count using 'xyz_count').

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          			! data long words of payload

	tx = iand( tev, pa_tag_mask4)					! PA data at start of payload
	ty = iand( tev, tag_mask4)
	if(tx.eq.pa_tag4) then
!		idebug = idebug-1
		goto (151,152,153), xyz_count				! strictly, xyz_count could get lost if
		goto 155						! buffer emptied amid xyz words

151		tx =  iand( tev, pa_mask4)				! X  
		if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
		if(n_energy.eq.0) then
			if(xy_correct.eq.2) then
				if(tx.ne.x0) then
					if((tx.eq.x0-15).or.(tx.eq.x0+17)) then
						x0 = x0+1
					else if((tx.eq.x0-17).or.(tx.eq.x0+15)) then
						x0 = x0-1
					else if((tx.eq.x0-16).or.(tx.eq.x0+16)) then
						x0 = x0
					else 
						x0 = tx
					endif
				endif
			else
				x0 = tx
			endif
		endif
		xyz_count = xyz_count+1
		goto 155
152		ty =  iand( tev, pa_mask4)				! Y
		if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
		if(xy_correct.eq.1) then
			idebug = ty
			if(x0.lt.0) x0 = -x0
			if(x0.lt.xmargin) then
				if(ty.gt.y0) then
					if(ldone.eq.0) y0 = y0+1
					ldone = 1
					rdone = 0
				endif
			endif
			if(width-1-x0.lt.xmargin) then
				if(ty.gt.y0) then
					if(rdone.eq.0) y0 = y0+1
					rdone = 1
					ldone = 0
				endif
			endif
		else
			if(xy_correct.eq.2) then
				if(ty.ne.y0) then
					if((ty.ne.y0-16).and.(ty.ne.y0+16)) y0 = ty
				endif
			else
				y0 = ty
			endif
		endif
		xyz_count = xyz_count+1
		goto 155
153		tz =  iand( tev, pa_mask4)				! Z
		if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
		xyz_count = xyz_count+1
		goto 155
	else if(ty.eq.0) then
		dtag = ishft( iand( tev, tag_mask4), tag_offset4)	! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask4), pdf_offset4)	! same as tag now in #4
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				n = n+1
			endif
		endif
!		idebug = idebug+1
	else							! must be an encoder value
!		idebug = idebug+1
	endif
155	remain = remain-4
	first = .false.
	goto 150

!...................................................................................
!   Process the 'monitor' event ...
!   If 'n_energy' is not zero, look for energy PV and position in XANES energy array.

130	if(length.gt.n_mon_buff) then				! monitor too long
		err = 1						! for mon_buff buffer
		idebug = 1301
		return
	endif
 	j = length		         			! data long words of payload

	call get_maia_32( ev, i, n_buffer, mon_buff, j, mbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return 

	do k=0,j-1						! completed all monitor
		monitor(nm+k) = mon_buff(k)			! copy buffer to 'monitor'
	enddo

	if((n_energy.eq.0).or.(j.lt.n_es+4).or.(n_es.eq.0)) goto 132	! skip if not XANES or short
	val = 0.0
	val2 = 0.0
	do 131 k=nm,nm+j-n_es					! check for "Energy" monitor
		do l=0,n_es-1
			if(energy_string(l).ne.monitor(k+l)) goto 131
		enddo
		l = k+n_es					! found energy monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 132				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val = 10.*val + monitor(m)-48
				else
					val = val + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
!		do l=0,n_energy-1
!			if(val.lt.energy(l)) then		! found Energy transition
!				x0 = l				! set x0 as energy bin
!				goto 132
!			endif
!		enddo			
131	continue


132	if((j.lt.n_fs+4).or.(n_fs.eq.0)) goto 137		! skip if short
	do 133 k=nm,nm+j-n_fs					! check for "Flux" monitor
		do l=0,n_fs-1
			if(flux_string(l).ne.monitor(k+l)) goto 133
		enddo
		l = k+n_fs					! found flux monitor
		nb = 0
		do while((nb.lt.3).and.(l.lt.nm+j-1))
			if(monitor(l).eq.32) nb=nb+1		! skip 3 blanks
			l = l+1
		enddo
		m = l						! start of value
		if(m.ge.nm+j) goto 137				! no room for value
		do while((monitor(l).gt.32).and.(l.lt.nm+j-1))
			l = l+1
		enddo
		val2 = monitor(m)-48				! value in range: m,l-1
		whole = .true.					! 48 is "0"
		factor = 0.1
		do while(m.lt.l-1)				! if m < l-1, more chars
			m = m+1
			if(monitor(m).eq.46) then		! decimal point
				whole = .false.
			else
				if(whole) then
					val2 = 10.*val2 + monitor(m)-48
				else
					val2 = val2 + (monitor(m)-48)*factor
					factor = 0.1*factor
				endif
			endif	
		enddo
133	continue


137	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif
	if(nt.lt.n_times) then
		time_sec(nt) = tv_sec				! save time stamps
		if(tv_sec.lt.1266400000) idebug=n_record
		time_micro(nt) = tv_micro
		beam(nt) = val					! beam energy
		flux(nt) = val2					! flux rate
		nt = nt+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_384_events( argc, argv)

!DLL_EXPORT maia_384_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.27) then 		         ! needed # args
	maia_384_events = 1
	return
endif

call maia_384_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)) )

maia_384_events = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE maia_384_events_b( ev,n_buffer, channel_on,nc, e,t,x,y,n,ste,tags, &
		ibranch,swap, tag,length,skip, x0,y0, monitor,n_monitor,nm, &
		time_sec,time_micro,n_times,nt, bad,idebug)

! assumes that n_buffer IS divisable by 4

INTEGER*4 i,j,n,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, nm, n_monitor
INTEGER*4 n_times, nt
INTEGER*4 time_sec(0:n_times-1),time_micro(0:n_times-1)
INTEGER*2 tag, length, ibranch, x0,y0, skip, len2, word_mask
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*2 ste(0:n_buffer/4-1), tags(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3), monitor(0:n_monitor-1)
LOGICAL incomplete, first

INTEGER*1 a_tag, b_tag
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dz, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4, pa_tag_offset4, pa_tag_mask4
INTEGER*2 maia_monitor, maia_events_1, xyz_count, pa_tag4 

common /c_maia_1/ jbuff, bev, remain, first
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_events_1 = 34, maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -31, tag_offset3 = -31 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31, pa_tag4 = z'001C' )
parameter ( pa_offset4 = 0, pa_mask4 = z'07FFFFFF', pa_tag_offset4 = -27, pa_tag_mask4 = z'F8000000')

n_max = n_buffer/4
n = 0
i = 0
nm = 0
nt = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	len2 = bev(1)
	length = iand( ior( ishft( iand(length,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (16 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8


80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_events_1) then		     ! event_1 (ET event, type #4), they contain XYZ too
		remain = length
		xyz_count = 1
		idebug = idebug+1
		ibranch = 15
		goto 150
	endif
	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 117
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -i
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
!a		if(err.eq.1) idebug = -1
!		if(incomplete) idebug = i
		return          								! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -5678
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	idebug = 1717
	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
		if(err.eq.1) idebug = -1234
		if(incomplete) idebug = 9876
		return          				! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -5677
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	idebug = 1716
	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
		if(err.eq.1) idebug = -1233
		if(incomplete) idebug = 9875
		return          				! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3)   ! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'maia_events_1' event (was maia_ET_events_4) ...

150	if(remain.le.0) then					! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -5679
		goto 10
	endif

!   Process payload data while it lasts ...
!   First 3 words of ET4 events is PA pixel XYZ information (keep count using 'xyz_count').

	idebug = 1717
	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
		if(err.eq.1) idebug = -1234
		if(incomplete) idebug = 9876
		return          				! data long words of payload
	endif

	dx = ishft( iand( tev, pa_tag_mask4), pa_tag_offset4)		! PA data at start of payload
	dy = ishft( iand( tev, tag_mask4), tag_offset4)
	if(dx.eq.pa_tag4) then
		idebug = 1910
		goto (151,152,153), xyz_count				! strictly, xyz_count could get lost if
		idebug = 1911
		goto 155						! buffer emptied amid xyz words

151		dx =  ishft( iand( tev, pa_mask4), pa_offset4)		! X  
		x0 = dx
		xyz_count = xyz_count+1
		goto 155
152		dy =  ishft( iand( tev, pa_mask4), pa_offset4)		! Y
		y0 = dy
		xyz_count = xyz_count+1
		goto 155
153		dz =  ishft( iand( tev, pa_mask4), pa_offset4)		! Z
		xyz_count = xyz_count+1
		goto 155
	else if(dy.eq.0) then
		dtag = ishft( iand( tev, tag_mask4), tag_offset4)	! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask4), pdf_offset4)	! same as tag now in #4
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				n = n+1
			endif
		endif
	else								! must be an encoder value

	endif
155	remain = remain-4
	goto 150

!...................................................................................
!   Process the 'monitor' event ...

130 	j = min(length, n_monitor-nm)

	call get_maia_32( ev, i, n_buffer, monitor(nm), j, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) then
		return          				! data long words of payload
	endif
	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif
	if(nt.lt.n_times) then
		time_sec(nt) = tv_sec				! save time stamps
		time_micro(nt) = tv_micro
		nt = nt+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_32_events2( argc, argv)

!DLL_EXPORT maia_32_events2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.27) then 		         ! needed # args
	maia_32_events2 = 1
	return
endif

call maia_32_events2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)) )

maia_32_events2 = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE maia_32_events2_b( ev,n_buffer, channel_on,nc, e,t,x,y,n,ste,tags, &
		ibranch,swap, tag,length,skip, x0,y0, monitor,n_monitor,nm, &
		time_sec,time_micro,n_times,nt, bad,idebug)

! assumes that n_buffer IS divisable by 4

INTEGER*4 i,j,n,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain, nm, n_monitor
INTEGER*4 n_times, nt
INTEGER*4 time_sec(0:n_times-1),time_micro(0:n_times-1)
INTEGER*2 tag, length, ibranch, x0,y0, skip, len2, word_mask
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*2 ste(0:n_buffer/4-1), tags(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3), monitor(0:n_monitor-1)
LOGICAL incomplete, first

INTEGER*1 a_tag, b_tag
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dtag, pdf, adr, byte_mask, maia_et_events_2, maia_et_events_3
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 maia_monitor

common /c_maia_1/ jbuff, bev, remain, first
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_et_events_1 = 8, maia_et_events_2 = 25, maia_et_events_3 = 31, maia_monitor = 26 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -30, tag_offset3 = -30 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

n_max = n_buffer/4
n = 0
i = 0
nm = 0
nt = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120,130,140), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	len2 = bev(1)
	length = iand( ior( ishft( iand(length,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (16 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8


80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90	if(tag.eq.maia_et_events_3) then	     ! ET event, type #3, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 14
		goto 140
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 12
		goto 120
	endif
	if(tag.eq.maia_et_events_1) then	     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_monitor) then		     ! Monitor event
		ibranch = 13
		goto 130
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -i
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
!a		if(err.eq.1) idebug = -1
!		if(incomplete) idebug = i
		return          								! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -5678
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	idebug = 1717
	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
		if(err.eq.1) idebug = -1234
		if(incomplete) idebug = 9876
		return          				! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120

!...................................................................................
!   Process the 'maia_et_events_3' event ...

140 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -5677
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	idebug = 1716
	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
		if(err.eq.1) idebug = -1233
		if(incomplete) idebug = 9875
		return          				! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask3), tag_offset3)   ! rest of data is all ET words
!		pdf = ishft( iand( tev, pdf_mask3), pdf_offset3)	! same as tag now in #3
		dt = ishft( iand( tev, dt_mask3), dt_offset3)
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
!				tags(n) = pdf
				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 140

!...................................................................................
!   Process the 'monitor' event ...

130 	j = min(length, n_monitor-nm)

	call get_maia_32( ev, i, n_buffer, monitor(nm), j, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) then
		return          								! data long words of payload
	endif
	nm = nm+j
	if(nm.lt.n_monitor) then
		monitor(nm) = 1					! add separator character
		nm = nm+1
	endif
	if(nt.lt.n_times) then
		time_sec(nt) = tv_sec				! save time stamps
		time_micro(nt) = tv_micro
		nt = nt+1
	endif

	if( j.lt.length) then
		skip = length-j
		goto 100
	endif
	ibranch = 1
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_32_events( argc, argv)

!DLL_EXPORT maia_32_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.20) then          ! needed # args
	maia_32_events = 1
	return
endif

call maia_32_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)) )

maia_32_events = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE maia_32_events_b( ev,n_buffer, channel_on,nc, e,t,x,y,n,ste,tags, &
		 ibranch,swap, tag,length,skip, x0,y0, bad,idebug)

! assumes that n_buffer IS divisable by 4

INTEGER*4 i,n,nc, n_buffer, n_max, bad, swap, sequence, idebug, tev, jbuff, err
INTEGER*4 tag_seq, tv_sec, tv_micro, client, remain
INTEGER*2 tag, length, ibranch, x0,y0, skip, len2, word_mask
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*2 ste(0:n_buffer/4-1), tags(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
LOGICAL incomplete, first

INTEGER*1 a_tag, b_tag
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask
INTEGER*2 maia_et_events_1, de, dt, dx, dy, dtag, pdf, adr, byte_mask, maia_et_events_2
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2

common /c_maia_1/ jbuff, bev, remain, first
equivalence (bev(0), tev)

parameter ( a_tag = z'AA', b_tag = z'BB', byte_mask = z'00FF', word_mask = z'7FFF' )
parameter ( maia_et_events_1 = 8, maia_et_events_2 = 25 )

! Specific to the ET format 'maia_et_events_1' ...
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = -5, dt_offset = -17, pdf_offset = -29, tag_offset = -30 )
parameter ( dy_offset = -15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

n_max = n_buffer/4
n = 0
i = 0
goto (10,20,30,40,50,60,70,80,90,100,110,120), ibranch

10  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word of header (tags)
	if((bev(0).eq.a_tag).and.(bev(3).eq.b_tag)) then
		tag = bev(1)
		len2 = bev(2)
		tag = iand( ior( ishft( iand(tag,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	else
		bad = bad+1
		i = max(i+1,0)         ! try next byte (was backtrack 3 and test byte)
		goto 10
	endif
	ibranch = 2

20  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, 0, err)
	if(incomplete.or.(err.eq.1)) return          ! second long word of header (length)
	length = bev(0)
	len2 = bev(1)
	length = iand( ior( ishft( iand(length,byte_mask), 8), iand(len2,byte_mask)), word_mask)
	ibranch = 3

30  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! third long word of header (sequence)
	sequence = tev
	ibranch = 4


!   New format long words (16 bytes more)

40  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! forth long word of header (tag seq)
	tag_seq = tev
	ibranch = 5

50  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! fifth long word of header (time sec)
	tv_sec = tev
	ibranch = 6

60  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! sixth long word of header (time us)
	tv_micro = tev
	ibranch = 7

70  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! seventh long word of header (client)
	client = tev
	ibranch = 8


80  call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! eigth word of header (unused)
	ibranch = 9

90  if(tag.eq.maia_et_events_1) then		     ! ET event, type #1, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 11
		goto 110
	endif
	if(tag.eq.maia_et_events_2) then	     ! ET event, type #2, they contain XY too
		remain = length
		first = .true.
		idebug = idebug+1
		ibranch = 12
		goto 120
	endif
	skip = length

100 if(i+skip.gt.n_buffer-1) then                    ! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'maia_et_events_1' event ...

110 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -i
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
!a		if(err.eq.1) idebug = -1
!		if(incomplete) idebug = i
		return          								! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask), dy_offset)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask), dx_offset)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask), tag_offset)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask), pdf_offset)
		dt = ishft( iand( tev, dt_mask), dt_offset)
		de = ishft( iand( tev, de_mask), de_offset)
		adr = ishft( iand( tev, adr_mask), adr_offset)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 110

!...................................................................................
!   Process the 'maia_et_events_2' event ...

120 if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = -5678
		goto 10
	endif

!   Process payload data while it lasts ...
!   First word of ET events is PA pixel XY information.

	idebug = 1717
	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) then
		if(err.eq.1) idebug = -1234
		if(incomplete) idebug = 9876
		return          				! data long words of payload
	endif

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)   ! rest of data is all ET words
		pdf = ishft( iand( tev, pdf_mask2), pdf_offset2)
		dt = ishft( iand( tev, dt_mask2), dt_offset2)
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)
!a		if((idebug.ge.0).and.(dtag.ne.0)) idebug=idebug+1

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
!		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				tags(n) = pdf			! hijack "tags" to monitor PDD overflow bit
!				tags(n) = dtag
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	first = .false.
	goto 120
end

!-------------------------------------------------------------------------------

integer function maia_et2_events2( argc, argv)

!DLL_EXPORT maia_et2_events2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.16) then          ! needed # args
	maia_et2_events2 = 1
	return
endif

call maia_et2_events2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)) )

maia_et2_events2 = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE maia_et2_events2_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,n,ste, swap, x0,y0,z0, tag3)

! 'ev' assumed to be an ET Maia Blog event 'payload' only.
! Assumes that n_buffer IS divisable by 4
! tag3 = ET block tag index (0=ET2, 1=ET3, 2=ET4)
! Added 'z' axis (only for ET4 records)

INTEGER*4 i,n,nc, n_buffer, n_max, swap, tev, jbuff, err, remain, tag3
INTEGER*2 x0,y0,z0, ste(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1), z(0:n_buffer/4-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
LOGICAL incomplete, first

INTEGER*2 de, dt, dx, dy, adr, dtag, maia_et_events_2, maia_et_events_3, maia_events_1, xyz_count 
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4, pa_tag4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4, pa_tag_offset4, pa_tag_mask4

common /c_maia_1/ jbuff, bev, remain, first
equivalence (bev(0), tev)

parameter ( maia_events_1 = 34, maia_et_events_2 = 25, maia_et_events_3 = 31 )

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -30, tag_offset3 = -30 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31, pa_tag4 = z'001C' )
parameter ( pa_offset4 = 0, pa_mask4 = z'07FFFFFF', pa_tag_offset4 = -27, pa_tag_mask4 = z'F8000000')

n_max = n_buffer/4
n = 0
i = 0
xyz_count = 1
first = .true.
if((tag3.lt.0).or.(tag3.gt.2)) return


10	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return
	
    if(tag3.eq.2) then							! et4 events

!	Process the 'maia_et_events_4' event ...
!	Process payload data while it lasts ...
!	First 3 words of ET4 events is PA pixel XYZ information.

	dx = ishft( iand( tev, pa_tag_mask4), pa_tag_offset4)		! PA data at start of payload
	dy = ishft( iand( tev, tag_mask4), tag_offset4)
	if(dx.eq.pa_tag4) then
		goto (151,152,153), xyz_count				! strictly, xyz_count could get lost if
		goto 155						! buffer emptied amid xyz words

151		dx =  ishft( iand( tev, pa_mask4), pa_offset4)		! X  
		x0 = dx
		xyz_count = xyz_count+1
		goto 155
152		dy =  ishft( iand( tev, pa_mask4), pa_offset4)		! Y
		y0 = dy
		xyz_count = xyz_count+1
		goto 155
153		dz =  ishft( iand( tev, pa_mask4), pa_offset4)		! Z
		z0 = dz
		xyz_count = xyz_count+1
		goto 155

	else if(dy.eq.0) then
		dtag = ishft( iand( tev, tag_mask4), tag_offset4)	! rest of data is all ET words
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				n = n+1
			endif
		endif
	else								! must be an encoder value

	endif

    else if(tag3.eq.1) then						! et3 events

!	Process the 'maia_et_events_2' event ...
!	Process payload data while it lasts ...
!	First word of ET2 events is PA pixel XY information.

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dt = ishft( iand( tev, dt_mask3), dt_offset3)	   ! rest of data is all ET words
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)
		dtag = ishft( iand( tev, tag_mask3), tag_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				n = n+1
			endif
		endif
	endif
    else								! et2 events

!	Process the 'maia_et_events_2' event ...
!	Process payload data while it lasts ...
!	First word of ET2 events is PA pixel XY information.

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dt = ishft( iand( tev, dt_mask2), dt_offset2)	   ! rest of data is all ET words
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				n = n+1
			endif
		endif
	endif
    endif

155	if(i.gt.n_buffer-4) return
	first = .false.
	goto 10
end

!-------------------------------------------------------------------------------

integer function maia_et2_events( argc, argv)

!DLL_EXPORT maia_et2_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	maia_et2_events = 1
	return
endif

call maia_et2_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)) )

maia_et2_events = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE maia_et2_events_b( ev,n_buffer, channel_on,nc, e,t,x,y,n,ste, swap, x0,y0, tag3)

! 'ev' assumed to be an ET Maia Blog event 'payload' only.
! Assumes that n_buffer IS divisable by 4
! tag3 = ET block tag index (0=ET2, 1=ET3, 2=ET4)

INTEGER*4 i,n,nc, n_buffer, n_max, swap, tev, jbuff, err, remain, tag3
INTEGER*2 x0,y0, ste(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
LOGICAL incomplete, first

INTEGER*2 de, dt, dx, dy, adr, dtag, maia_et_events_2, maia_et_events_3, maia_events_1, xyz_count 
INTEGER*2 dy_offset2, dx_offset2, de_offset2, dt_offset2, pdf_offset2, tag_offset2, adr_offset2
INTEGER*4 adr_mask2, de_mask2, dt_mask2, pdf_mask2, tag_mask2, dx_mask2, dy_mask2
INTEGER*2 dy_offset3, dx_offset3, de_offset3, dt_offset3, pdf_offset3, tag_offset3, adr_offset3
INTEGER*4 adr_mask3, de_mask3, dt_mask3, pdf_mask3, tag_mask3, dx_mask3, dy_mask3
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4, pa_tag4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4, pa_tag_offset4, pa_tag_mask4

common /c_maia_1/ jbuff, bev, remain, first
equivalence (bev(0), tev)

parameter ( maia_events_1 = 34, maia_et_events_2 = 25, maia_et_events_3 = 31 )

! Specific to the ET format 'maia_et_events_2' ...
parameter ( adr_mask2 = z'0000007F', de_mask2 = z'0007FF80', dt_mask2 = z'1FF80000' )
parameter ( pdf_mask2 = z'20000000', tag_mask2 = z'C0000000', adr_offset2 = 0 )
parameter ( de_offset2 = -7, dt_offset2 = -19, pdf_offset2 = -29, tag_offset2 = -30 )
parameter ( dy_offset2 = -15, dx_offset2 = 0, dx_mask2 = z'00007FFF', dy_mask2 = z'3FFF8000')

! Specific to the ET format 'maia_et_events_3' ...
parameter ( adr_mask3 = z'000001FF', de_mask3 = z'001FFE00', dt_mask3 = z'7FE00000' )
parameter ( pdf_mask3 = z'80000000', tag_mask3 = z'80000000', adr_offset3 = 0 )
parameter ( de_offset3 = -9, dt_offset3 = -21, pdf_offset3 = -30, tag_offset3 = -30 )
parameter ( dy_offset3 = -15, dx_offset3 = 0, dx_mask3 = z'00007FFF', dy_mask3 = z'3FFF8000')

! Specific to the ET/XYZ format 'maia_events_1' (was ET4) ...
parameter ( adr_mask4 = z'7FC00000', de_mask4 = z'00000FFF', dt_mask4 = z'003FF000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -22 )
parameter ( de_offset4 = 0, dt_offset4 = -12, pdf_offset4 = -31, tag_offset4 = -31, pa_tag4 = z'001C' )
parameter ( pa_offset4 = 0, pa_mask4 = z'07FFFFFF', pa_tag_offset4 = -27, pa_tag_mask4 = z'F8000000')

n_max = n_buffer/4
n = 0
i = 0
xyz_count = 1
first = .true.
if((tag3.lt.0).or.(tag3.gt.2)) return


10	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return
	
    if(tag3.eq.2) then							! et4 events

!	Process the 'maia_et_events_4' event ...
!	Process payload data while it lasts ...
!	First 3 words of ET4 events is PA pixel XYZ information.

	dx = ishft( iand( tev, pa_tag_mask4), pa_tag_offset4)		! PA data at start of payload
	dy = ishft( iand( tev, tag_mask4), tag_offset4)
	if(dx.eq.pa_tag4) then
		goto (151,152,153), xyz_count				! strictly, xyz_count could get lost if
		goto 155						! buffer emptied amid xyz words

151		dx =  ishft( iand( tev, pa_mask4), pa_offset4)		! X  
		x0 = dx
		xyz_count = xyz_count+1
		goto 155
152		dy =  ishft( iand( tev, pa_mask4), pa_offset4)		! Y
		y0 = dy
		xyz_count = xyz_count+1
		goto 155
153		dz =  ishft( iand( tev, pa_mask4), pa_offset4)		! Z
		xyz_count = xyz_count+1
		goto 155

	else if(dy.eq.0) then
		dtag = ishft( iand( tev, tag_mask4), tag_offset4)	! rest of data is all ET words
		dt = ishft( iand( tev, dt_mask4), dt_offset4)
		de = ishft( iand( tev, de_mask4), de_offset4)
		adr = ishft( iand( tev, adr_mask4), adr_offset4)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				n = n+1
			endif
		endif
	else								! must be an encoder value

	endif

    else if(tag3.eq.1) then						! et3 events

!	Process the 'maia_et_events_2' event ...
!	Process payload data while it lasts ...
!	First word of ET2 events is PA pixel XY information.

	if(first) then
		dy = ishft( iand( tev, dy_mask3), dy_offset3)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask3), dx_offset3)

		x0 = dx
		y0 = dy
	else
		dt = ishft( iand( tev, dt_mask3), dt_offset3)	   ! rest of data is all ET words
		de = ishft( iand( tev, de_mask3), de_offset3)
		adr = ishft( iand( tev, adr_mask3), adr_offset3)
		dtag = ishft( iand( tev, tag_mask3), tag_offset3)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				n = n+1
			endif
		endif
	endif
    else								! et2 events

!	Process the 'maia_et_events_2' event ...
!	Process payload data while it lasts ...
!	First word of ET2 events is PA pixel XY information.

	if(first) then
		dy = ishft( iand( tev, dy_mask2), dy_offset2)       ! PA data at start of payload
		dx = ishft( iand( tev, dx_mask2), dx_offset2)

		x0 = dx
		y0 = dy
	else
		dt = ishft( iand( tev, dt_mask2), dt_offset2)	   ! rest of data is all ET words
		de = ishft( iand( tev, de_mask2), de_offset2)
		adr = ishft( iand( tev, adr_mask2), adr_offset2)
		dtag = ishft( iand( tev, tag_mask2), tag_offset2)

		if((dtag.eq.0).and.(de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				ste(n) = adr
				n = n+1
			endif
		endif
	endif
    endif

155	if(i.gt.n_buffer-4) return
	first = .false.
	goto 10
end

!-------------------------------------------------------------------------------

integer function daq_et_events( argc, argv)

!DLL_EXPORT daq_et_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.15) then          ! needed # args
	daq_et_events = 1
	return
endif

call daq_et_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)) )

daq_et_events = 0
return
end

!-------------------------------------------------------------------------------

SUBROUTINE daq_et_events_b( ev,n_buffer, channel_on,nc, e,t,x,y,time,n,ste, swap, x0,y0, tag3)

! Called from background daq client process.
!		'ev' assumed to be a DAQ ET Blog event 'payload' only.
!		Assumes that n_buffer IS divisable by 4
!		tag3 = ET block tag index (0=ET with TimeStamps, 1=ET without TS)

INTEGER*4 i,n,nc, n_buffer, n_max, swap, tev(0:1), jbuff, err, remain, tag3, tx, ty, tz
INTEGER*4 tu, tv, tw, time(0:n_buffer/4-1)
INTEGER*2 x0,y0, ste(0:n_buffer/4-1), channel_on(0:nc-1)
INTEGER*2 e(0:n_buffer/4-1), t(0:n_buffer/4-1), x(0:n_buffer/4-1), y(0:n_buffer/4-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:7)
LOGICAL incomplete, first

INTEGER*2 de, dt, dx, dy, adr, dtag 
INTEGER*2 pa_offset4, de_offset4, dt_offset4, pdf_offset4, tag_offset4, adr_offset4
INTEGER*4 adr_mask4, de_mask4, dt_mask4, pdf_mask4, tag_mask4, pa_mask4, pa_tag_offset4
INTEGER*4 pa_tag_mask4, pa_tag_x4, pa_tag_y4, pa_tag_z4, pa_tag_tf4
INTEGER*4 pa_tag_u4, pa_tag_v4, pa_tag_w4, pa_sign_bit_mask4, pa_sign_extend
INTEGER*4 tf_tag_mask4, tf_tag_bt4, tf_tag_fc1, tf_bit_mask4 

common /c_daq_5/ jbuff, bev, remain, first			! init w/ init_daq_32_TS
equivalence (bev(0), tev(0))

! Specific to the ET format 'pm_event_ts_1' and 'pm_event_nots_1' ...
parameter ( adr_mask4 = z'7F000000', de_mask4 = z'00001FFF', dt_mask4 = z'00FFE000' )
parameter ( pdf_mask4 = z'80000000', tag_mask4 = z'80000000', adr_offset4 = -24 )
parameter ( de_offset4 = 0, dt_offset4 = -13, pdf_offset4 = -31, tag_offset4 = -31, pa_tag4 = z'001C' )
parameter ( pa_offset4 = 0, pa_mask4 = z'0FFFFFFF', pa_tag_offset4 = -27)
parameter ( pa_sign_bit_mask4 = z'08000000', pa_sign_extend = z'F0000000')
parameter ( pa_tag_mask4 = z'F0000000', pa_tag_x4 = z'80000000', pa_tag_y4 = z'90000000', pa_tag_z4 = z'A0000000' )
parameter ( pa_tag_u4 = z'B0000000', pa_tag_v4 = z'C0000000', pa_tag_w4 = z'D0000000' )
parameter ( pa_tag_tf4 = z'F0000000', tf_tag_mask4 = z'FC000000', tf_tag_bt4 =z'F0000000', tf_tag_fc1 =	z'F4000000' )
parameter ( tf_bit_mask4 = z'03FFFFFF' )

n_max = n_buffer/4
n = 0
i = 0
first = .true.
if((tag3.ne.0).and.(tag3.ne.1)) return

10  if(tag3.eq.0) then							! et events with time stamps

	call get_maia_32( ev, i, n_buffer, bev, 8, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return
	
!	Process the 'pm_event_ts_1' event ...
!	Process payload data while it lasts ...
!	First 6 words of ET events is PA pixel XYZUVW information, but we won't assume this (use masks).
!	However, need to assume that PA Y always follows PA X (later if use partial arrays).

	tx = iand( tev(0), tag_mask4)
	if(tx.eq.0) then						! ET (w/ timestamp)

		dt = ishft( iand( tev(0), dt_mask4), dt_offset4)
		de = ishft( iand( tev(0), de_mask4), de_offset4)
		adr = ishft( iand( tev(0), adr_mask4), adr_offset4)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				time(n) = tev(1)
				ste(n) = adr
				n = n+1
			endif
		endif
	else
		tx = iand( tev(0), pa_tag_mask4)
		if(tx.eq.pa_tag_x4) then				! PA X
			tx =  iand( tev(0), pa_mask4)
			if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
			x0 = tx

		else if(tx.eq.pa_tag_y4) then				! PA Y
			ty =  iand( tev(0), pa_mask4)
			if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
			y0 = ty

		else if(tx.eq.pa_tag_z4) then				! PA Z
			tz =  iand( tev(0), pa_mask4)
			if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
	
		else if(tx.eq.pa_tag_u4) then				! PA U
			tu =  iand( tev(0), pa_mask4)
			if( iand(tu,pa_sign_bit_mask4).ne.0) tu = ior(tu,pa_sign_extend)
	
		else if(tx.eq.pa_tag_v4) then				! PA V
			tv =  iand( tev(0), pa_mask4)
			if( iand(tv,pa_sign_bit_mask4).ne.0) tv = ior(tv,pa_sign_extend)
	
		else if(tx.eq.pa_tag_w4) then				! PA W
			tw =  iand( tev(0), pa_mask4)
			if( iand(tw,pa_sign_bit_mask4).ne.0) tw = ior(tw,pa_sign_extend)
	
		else if(tx.eq.pa_tag_tf4) then				! Time-Flux
			tx = iand( tev(0), tf_tag_mask4)
			if(tx.eq.tf_tag_bt4) then			! BT (ms)
				ty = iand( tev(0), tf_bit_mask4)
			else if(tx.eq.tf_tag_fc1) then			! Flux
				ty = iand( tev(0), tf_bit_mask4)
			endif

		endif
	endif

    else if(tag3.eq.1) then						! et events without TS

	call get_maia_32( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return
	
!	Process the 'pm_event_nots_1' event ...

	tx = iand( tev(0), tag_mask4)
	if(tx.eq.0) then						! ET

		dt = ishft( iand( tev(0), dt_mask4), dt_offset4)
		de = ishft( iand( tev(0), de_mask4), de_offset4)
		adr = ishft( iand( tev(0), adr_mask4), adr_offset4)

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_max)) then
				x(n) = x0
				y(n) = y0
				e(n) = de
				t(n) = dt
				time(n) = 0
				ste(n) = adr
				n = n+1
			endif
		endif
	else
		tx = iand( tev(0), pa_tag_mask4)
		if(tx.eq.pa_tag_x4) then				! PA X
			tx =  iand( tev(0), pa_mask4)
			if( iand(tx,pa_sign_bit_mask4).ne.0) tx = ior(tx,pa_sign_extend)
			x0 = tx

		else if(tx.eq.pa_tag_y4) then				! PA Y
			ty =  iand( tev(0), pa_mask4)
			if( iand(ty,pa_sign_bit_mask4).ne.0) ty = ior(ty,pa_sign_extend)
			y0 = ty

		else if(tx.eq.pa_tag_z4) then				! PA Z
			tz =  iand( tev(0), pa_mask4)
			if( iand(tz,pa_sign_bit_mask4).ne.0) tz = ior(tz,pa_sign_extend)
	
		else if(tx.eq.pa_tag_u4) then				! PA U
			tu =  iand( tev(0), pa_mask4)
			if( iand(tu,pa_sign_bit_mask4).ne.0) tu = ior(tu,pa_sign_extend)
	
		else if(tx.eq.pa_tag_v4) then				! PA V
			tv =  iand( tev(0), pa_mask4)
			if( iand(tv,pa_sign_bit_mask4).ne.0) tv = ior(tv,pa_sign_extend)
	
		else if(tx.eq.pa_tag_w4) then				! PA W
			tw =  iand( tev(0), pa_mask4)
			if( iand(tw,pa_sign_bit_mask4).ne.0) tw = ior(tw,pa_sign_extend)
	
		else if(tx.eq.pa_tag_tf4) then				! Time-Flux
			tx = iand( tev(0), tf_tag_mask4)
			if(tx.eq.tf_tag_bt4) then			! BT (ms)
				ty = iand( tev(0), tf_bit_mask4)
			else if(tx.eq.tf_tag_fc1) then			! Flux
				ty = iand( tev(0), tf_bit_mask4)
			endif

		endif
	endif
    endif

    if(tag3.eq.0) then
	if(i.gt.n_buffer-8) return
    else
	if(i.gt.n_buffer-4) return
    endif
    first = .false.
    goto 10
end

!-------------------------------------------------------------------------------

integer function cmit_event_maia( argc, argv)

!DLL_EXPORT cmit_event_maia
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	cmit_event_maia = 1
	return
endif

call cmit_event_maia_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)), %val(argv(13)), %val(argv(14)) )

cmit_event_maia = 0
return
end

!------------------------------------------------------------

SUBROUTINE cmit_event_maia_b( ste,e,time, x,y, multiple,n_buffer, do_time, &
		out, n_out_max, debug,n_out, x0,y0 )

INTEGER*4 n_out_max, n_out, n_buffer, do_time, debug
INTEGER*4 multiple(0:n_buffer-1)
INTEGER*2 e(0:n_buffer-1), x(0:n_buffer-1), y(0:n_buffer-1), ste(0:n_buffer-1), time(0:n_buffer-1)
INTEGER*1 out(0:n_out_max-1)

INTEGER*4 i,k,m,i_step,long,seq,tlong0,tlong1,tlong2,tlong3,length,total, xy_long
INTEGER*1 a_tag, b_tag, bytes(0:3)
INTEGER*2 maia_et_events_1, maia_pa_events_1, word,x0,y0,word_mask
INTEGER*2 dy_offset, dx_offset, de_offset, dt_offset, pdf_offset, tag_offset, adr_offset
INTEGER*4 adr_mask, de_mask, dt_mask, pdf_mask, tag_mask, dx_mask, dy_mask, pa_tags

parameter ( a_tag = z'AA', b_tag = z'BB', word_mask = z'FFFF', byte_mask = z'00FF' )
parameter ( maia_et_events_1 = 8, maia_pa_events_1 = 10, pa_tags = z'80000000' )
parameter ( adr_mask = z'0000001F', de_mask = z'0001FFE0', dt_mask = z'1FFE0000' )
parameter ( pdf_mask = z'20000000', tag_mask = z'C0000000', adr_offset = 0 )
parameter ( de_offset = 5, dt_offset = 17, pdf_offset = 29, tag_offset = 30 )
parameter ( dy_offset = 15, dx_offset = 0, dx_mask = z'00007FFF', dy_mask = z'3FFF8000')

equivalence (word, bytes(0)), (long, bytes(0))

seq = 0
i_step = 1000
debug = 0
if( n_buffer.lt.1) return

! Initial X,Y position ...

tlong0 = x(0)
tlong0 = ishft(tlong0, dx_offset)
tlong1 = y(0)
long = ior( iand(tlong0,dx_mask), iand(ishft(tlong1, dy_offset),dy_mask))
long = ior( long, pa_tags)
xy_long = long

if((x(0).eq.x0).and.(y(0).eq.y0)) goto 2

x0 = x(0)
y0 = y(0)
out(n_out) = a_tag   ; n_out=n_out+1;   if( n_out.ge.n_out_max) return
word =  maia_pa_events_1
out(n_out) = bytes(1)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(0)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = b_tag       ; n_out=n_out+1;   if( n_out.ge.n_out_max) return

word = 8
out(n_out) = bytes(1)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(0)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return

out(n_out) = 0          ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
out(n_out) = 0          ; n_out=n_out+1;  if( n_out.ge.n_out_max) return

long = seq
seq = seq+1
out(n_out) = bytes(3)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(2)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(1)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(0)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return

do k=1,20
	out(n_out) = 0          ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
enddo

out(n_out) = bytes(3)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(2)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(1)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(0)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(3)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(2)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(1)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
out(n_out) = bytes(0)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return

2 i = 0
5 if(i.ge.n_buffer) goto 90
	length = min(i_step, n_buffer-i)
	do j=0,length-1
	   if((x0.ne.x(i+j)).or.(y0.ne.y(i+j))) then
		 length = j
		 goto 10
	   endif
	enddo
10  if(length.eq.0) then
	   debug = 7
	   goto 90
	endif
	total = length
	if(multiple(0).ge.0) then
	   total = 0
	   do j=0,length-1
		 if(multiple(i+j).ge.1) total = total + multiple(i+j)
	   enddo
	   if(total.gt.8000) then
		 i_step = i_step/2
		 if(i_step.lt.1) then
		  debug = 8               ! some data may be lost in this case
		  goto 20
		 endif
		 debug = 6
		 goto 5
	   endif
	endif
20  if(length.eq.0) goto 90

	out(n_out) = a_tag        ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
	word =  maia_et_events_1
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = b_tag        ; n_out=n_out+1;  if( n_out.ge.n_out_max) return

	word = iand( (total+1)*4,word_mask)
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return

	out(n_out) = 0          ; n_out=n_out+1; if( n_out.ge.n_out_max) return
	out(n_out) = 0          ; n_out=n_out+1; if( n_out.ge.n_out_max) return

	long = seq
	seq = seq+1
	out(n_out) = bytes(3)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(2)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return

	do k=1,20
	   out(n_out) = 0          ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
	enddo

!   Always has PA data word at start of ET payload ...

	long = xy_long
	out(n_out) = bytes(3)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(2)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return

	m = 0
	do j=0,length-1
	   if( multiple(0).lt.0) then
		 tlong0 = 0
		 if(do_time.eq.1) tlong0 = time(i+j)
		 tlong0 = ishft(tlong0, dt_offset)
		 tlong1 = e(i+j)
		 tlong1 = ishft(tlong1, de_offset)
		 tlong2 = ste(i+j)
		 tlong2 = ishft(tlong2, adr_offset)
		 long = ior(iand(tlong0,dt_mask),ior(iand(tlong1,de_mask),iand(tlong2,adr_mask)))
		 out(n_out) = bytes(3)       ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
		 out(n_out) = bytes(2)       ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
		 out(n_out) = bytes(1)       ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
		 out(n_out) = bytes(0)       ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
		 m = m+1
		 if((m.ge.total).and.(j.lt.length-1)) then
		  debug = 5
		  goto 40
		 endif
	   else
		 if( multiple(i+j).ge.1) then
		  do k=1,multiple(i+j)
			  tlong0 = 0
			  if(do_time.eq.1) tlong0 = time(i+j)
			  tlong0 = ishft(tlong0, dt_offset)
			  tlong1 = e(i+j)
			  tlong1 = ishft(tlong1, de_offset)
			  tlong2 = ste(i+j)
			  tlong2 = ishft(tlong2, adr_offset)
			  long = ior(iand(tlong0,dt_mask),ior(iand(tlong1,de_mask),iand(tlong2,adr_mask)))
			  out(n_out) = bytes(3)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
			  out(n_out) = bytes(2)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
			  out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
			  out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
			  m = m+1
			  if((m.ge.total).and.((j.lt.length-1).or.(k.lt.multiple(i+j)-1))) then
				 debug = 4
				 goto 40
			  endif
		  enddo
		 endif
	   endif
	enddo

40	if((x(i+length).eq.x0).and.(y(i+length).eq.y0)) goto 50

 	out(n_out) = a_tag      ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	word =  maia_pa_events_1
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = b_tag        ; n_out=n_out+1;  if( n_out.ge.n_out_max) return

	word = 8
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return

	out(n_out) = 0          ; n_out=n_out+1; if( n_out.ge.n_out_max) return
	out(n_out) = 0          ; n_out=n_out+1; if( n_out.ge.n_out_max) return

	long = seq
	seq = seq+1
	out(n_out) = bytes(3)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(2)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return

	do k=1,20
	   out(n_out) = 0          ; n_out=n_out+1;  if( n_out.ge.n_out_max) return
	enddo

	x0 = x(i+length)
	y0 = y(i+length)
	tlong0 = x0
	tlong0 = ishft(tlong0, dx_offset)
	tlong1 = y0
	long = ior( iand(tlong0,dx_mask), iand(ishft(tlong1, dy_offset),dy_mask))
	long = ior( long, pa_tags)
	out(n_out) = bytes(3)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(2)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(1)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)   ; n_out=n_out+1;    if( n_out.ge.n_out_max) return
	out(n_out) = bytes(3)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
	out(n_out) = bytes(2)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
	out(n_out) = bytes(1)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
	out(n_out) = bytes(0)     ; n_out=n_out+1; if( n_out.ge.n_out_max) return
	xy_long = long

50	i = i+length
	goto 5

90 return
end

!-------------------------------------------------------------------------------

integer function cmit_event_build( argc, argv)

!DLL_EXPORT cmit_event_build
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.15) then          ! needed # args
	cmit_event_build = 1
	return
endif

call cmit_event_build_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)) )

cmit_event_build = 0
return
end

!------------------------------------------------------------

SUBROUTINE cmit_event_build_b( ste,e,time, x,y, multiple,n_buffer, do_time, &
			out, n_out_max,n_out_dim,n_out, throttle, tbuff, nt )

INTEGER*4 n_out_max, n_out, n_buffer, do_time, n_out_dim
INTEGER*4 i,k,nt
INTEGER*4 multiple(0:n_buffer-1)
INTEGER*2 e(0:n_buffer-1), x(0:n_buffer-1), y(0:n_buffer-1), ste(0:n_buffer-1), time(0:n_buffer-1)
INTEGER*2 out(0:n_out_dim-1, 0:n_out_max-1)
INTEGER*2 throttle(0:nt-1), tbuff(0:nt-1)

if( n_buffer.lt.1) return
if((do_time.eq.1).and.(n_out_dim.lt.5)) return

do i=0,n_buffer-1
  if( (e(i).ge.0).and.(e(i).lt.nt)) then
	if( multiple(0).lt.0) then
		if(tbuff(e(i)).le.1) then
			tbuff(e(i)) = throttle(e(i))
			out(0,n_out) = ste(i)
			out(1,n_out) = e(i)
			out(2,n_out) = x(i)
			out(3,n_out) = y(i)
			if(do_time.eq.1) out(4,n_out) = time(i)
			n_out = n_out+1
			if( n_out.ge.n_out_max) return
		else
			tbuff(e(i)) = tbuff(e(i))-1
		endif
	else
	   if( multiple(i).ge.1) then
		 do k=1,multiple(i)
	    	if(tbuff(e(i)).le.1) then
	    		tbuff(e(i)) = throttle(e(i))
				out(0,n_out) = ste(i)
				out(1,n_out) = e(i)
				out(2,n_out) = x(i)
				out(3,n_out) = y(i)
				if(do_time.eq.1) out(4,n_out) = time(i)
				n_out = n_out+1
				if( n_out.ge.n_out_max) return
			else
				tbuff(e(i)) = tbuff(e(i))-1
			endif
		 enddo
	   endif
	endif
  endif
enddo

	return
end

!-------------------------------------------------------------------------------

integer function throttle_q( argc, argv)

!DLL_EXPORT throttle_q
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.7) then          ! needed # args
	throttle_q = 1
	return
endif

call throttle_q_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)) )

throttle_q = 0
return
end

!------------------------------------------------------------

SUBROUTINE throttle_q_b( e,multiple,n_buffer, throttle, tbuff, nt, mask )

INTEGER*4 n_buffer
INTEGER*4 i,k,nt
INTEGER*4 multiple(0:n_buffer-1)
INTEGER*2 e(0:n_buffer-1), mask(0:n_buffer-1)
INTEGER*2 throttle(0:nt-1), tbuff(0:nt-1)

if( n_buffer.lt.1) return
if( multiple(0).ge.0) return

do i=0,n_buffer-1
  if( (e(i).ge.0).and.(e(i).lt.nt)) then
   	if(tbuff(e(i)).le.1) then
   		tbuff(e(i)) = throttle(e(i))
		mask(i) = 1
	else
		tbuff(e(i)) = tbuff(e(i))-1
	endif
  endif
enddo

	return
end

!-------------------------------------------------------------------------------

integer function low_stats_filter( argc, argv)

!DLL_EXPORT low_stats_filter
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.7) then           ! needed # args
	low_stats_filter = 1
	return
endif

call low_stats_filter_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)))

low_stats_filter = 0
return
end

!------------------------------------------------------------

subroutine low_stats_filter_b( spec1,spec2, n_spec, lowi,highi, AF,BF )

INTEGER*4 n_spec, low, high, i,j,k, lowi, highi, clip, NW
REAL*4 BIG_A, BIG_B, TOO_SMALL, RATIO, SCALE, LEFT, RIGHT, W, BIG
REAL*4 sl, sr, Y, Z1, Z2
REAL*4 spec1(0:n_spec-1), spec2(0:n_spec-1), AF, BF, FWHM, FILTER

clip(k) = MAX( 0, MIN( k, n_spec-1))

!   Perhaps do veto-ends and extend-ends in here too later ...

low = lowi
high = highi
if(low.lt.0) low=0
if(low.gt.n_spec-2) low=n_spec-2
if(high.gt.n_spec-1) high=n_spec-1

do i=0, n_spec-1
	spec2(i) = spec1(i)
enddo

if(low.gt.high-2) return

	BIG_A = 75.              ! desired sum slope
	BIG_B = 10.              ! desired sum min
	TOO_SMALL = 10.          ! minimum sum
	RATIO = 1.3              ! maximum slope
	SCALE = 1.5              ! FWHM scaling

do i=low,high

	FWHM = sqrt( AF*float(i) + BF)

	FILTER = spec1(i)
	BIG = BIG_A * SQRT(ABS(FILTER))          ! desired sum
	IF(BIG.LT.BIG_B) BIG=BIG_B           ! minimum sum

	NW = NINT(SCALE*FWHM)
	IF(NW.LT.2) NW=2

	LEFT = 0.
	RIGHT = 0.
	W = 1.
	DO j=1,NW                      ! full side sums
		LEFT = LEFT + spec1(CLIP(i-j))
		RIGHT = RIGHT + spec1(CLIP(i+j))
		W = W+2.
	enddo

	Y = FILTER + LEFT+RIGHT                 ! reduce span for good
	DO j=NW,1,-1                    ! statistics on peaks

		sr = ABS(RIGHT)+1.
		sl = ABS(LEFT)+1.
		Z1 = (sr-sqrt(sr))/(sl+sqrt(sl))
		Z2 = (sl-sqrt(sl))/(sr+sqrt(sr))
		IF((Y.LE.BIG).AND.(Z1.LE.RATIO).AND.(Z2.LE.RATIO)) GOTO 79

		LEFT = LEFT - spec1(CLIP(I-J))
		RIGHT = RIGHT - spec1(CLIP(I+J))
		IF(FILTER+LEFT+RIGHT.LE.TOO_SMALL) GOTO 79

		Y = FILTER + LEFT+RIGHT
		W = W-2.
	enddo
79  spec2(i) = Y/W

enddo

return
end

!------------------------------------------------------------

integer function bd12_events( argc, argv)

!DLL_EXPORT bd12_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.11) then          ! needed # args
	bd12_events = 1
	return
endif

call bd12_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)) )

bd12_events = 0
return
end

!------------------------------------------------------------

SUBROUTINE bd12_events_b( ev,n_buffer, x0,y0, channel_on,nc, e,x,y,n,adc )

! channel_on    array indicates (1) whether a channel is selected

INTEGER*4 n_buffer, n, nc
INTEGER*2 e(0:n_buffer-1), x(0:n_buffer-1), y(0:n_buffer-1), channel_on(0:nc-1)
INTEGER*2 ev(0:n_buffer-1), adc(0:n_buffer-1), x0,y0

INTEGER*4 i,j,tnc
INTEGER*2 e_mask, ste_mask
INTEGER*2 ste_offset
INTEGER*2 ste, te

parameter ( e_mask = z'0FFF', ste_mask = z'F000')
parameter ( ste_offset = -12)

! The data uses the bottom bits for ADC data (12 bits for e, x, y),
! and the top 4 bits tag the ADC station and to identify X,Y.
! ADC tag zero is reserved for BD12 'control info'?
! Various data integrity checks are made, and the
! ADC data is accessed via bit masks.

j = 0
n = 0
if( n_buffer.lt.1) return
tnc = min(nc,12)

do i=0,n_buffer-1
	ste = ishft( iand( ev(i), ste_mask), ste_offset)
	te = iand( ev(i), e_mask)

	if( ste.eq.14) then                   ! X
	   x0 = te
	else if( ste.eq.15) then             ! Y
	   y0 = te
	endif

	if((ste.ge.1).and.(ste.le.tnc)) then     ! E
	   if( channel_on( ste-1).eq.1) then
		 e(j) = te
		 x(j) = x0
		 y(j) = y0
		 adc(j) = ste
		 j = j+1
	   endif
	endif
enddo

	n = j
	return
end

!-------------------------------------------------------------------------------

integer function tohoku_events( argc, argv)

!DLL_EXPORT tohoku_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.21) then          ! needed # args
	tohoku_events = 1
	return
endif

call tohoku_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	 %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), &
	 %val(argv(20)), %val(argv(21)) )

tohoku_events = 0
return
end

!------------------------------------------------------------

! This routine reads Tohoku list-mode data. If it needs to get another buffer
! to continue building an event, it will set ibranch, to branch on return.
! Many local variables are passed here in order to continue after this branch.
! Each pass the current e,x1,y1,ste arrays are returned.

SUBROUTINE tohoku_events_b( ev,j_mpa,ibranch,n_buffer,n_actual, &
	   e,x,y,ste, n,n_max, mpa_x_adc,mpa_y_adc, &
	   word, ADCindex, ADCpntr, adc,tag, k, adcs, bad_xy )

INTEGER*4 i,j, k,n, ibranch, j_mpa, n_buffer,n_actual,bad_xy
INTEGER*4 adcs, n_max
INTEGER*2 ev(0:n_buffer-1), word,bit(0:15), adc(0:15)
INTEGER*2 TAGmask, DataMask
INTEGER*2 ADCindex(0:15), ADCpntr(0:15), tag(0:15), xt,yt
INTEGER*2 x(0:n_max-1),y(0:n_max-1),e(0:n_max-1),ste(0:n_max-1)
INTEGER*2 mpa_x_adc,mpa_y_adc
LOGICAL bad_x, bad_y

parameter ( TAGmask = z'8000', DataMask = z'1FFF')

	do i=0,15
	   bit(i) = ishft(1,i)
	enddo
	if(j_mpa.lt.0) j_mpa=0
	if(k.lt.0) k=0
	if(mpa_x_adc.lt.0) mpa_x_adc=0
	if(mpa_x_adc.gt.7) mpa_x_adc=7
	if(mpa_y_adc.lt.0) mpa_y_adc=0
	if(mpa_y_adc.gt.7) mpa_y_adc=7
	if(n_actual.gt.n_buffer) n_actual=n_buffer

	n = 0
	if(n_actual.lt.1) goto 90

	goto (10,20,30,90), ibranch

10  if(j_mpa.ge.n_actual) goto 90
	word = ev(j_mpa)

	if (iand(TAGmask, word).eq.0) then
	   j_mpa = j_mpa+1
	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 1
		 goto 90
	   endif
	   goto 85                    ! loop back to 10
	endif

15  do i=0,15                        ! found a TAG word
	   adc(i) = 0
	enddo
	adcs = 0
	do i=0,15
	   ADCindex(i) = 0
	   ADCpntr(i) = -1
	enddo
	do i=0,7                       ! up to 8 ADCs
	   if( iand(bit(i),word).ne.0) then
		 ADCindex(adcs) = i             ! index to real ADC #
		 ADCpntr(i) = adcs
		 adcs = adcs+1                 ! number of ADCs in this event
	   endif
	enddo
	k = 0
	j_mpa = j_mpa+1
	if( j_mpa.ge.n_actual) then
	   j_mpa = 0
	   ibranch = 2
	   goto 90
	endif

20  if( k.lt.adcs) then
	   word = ev(j_mpa)
	   if (iand(TAGmask, word).ne.0) then
		 bad_xy = bad_xy+1
		 goto 15
	   endif

	   j = ADCindex(k)
	   if((j.ge.0).and.(j.lt.16)) then
		 adc(k) = iand( word, DataMask)
		 tag(k) = j
	   endif
	   k = k+1
	   j_mpa = j_mpa+1
	   if( k.ge.adcs) goto 30

	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 2
		 goto 90
	   endif
	endif
	if( k.lt.adcs) goto 20

30  xt = 0
	yt = 0
	bad_x = .false.
	bad_y = .false.
	i = ADCpntr(mpa_x_adc)
	if((i.ge.0).and.(i.lt.adcs)) then
	   xt = adc(i)
	   adc(i) = -1
	else
	   bad_x = .true.
	endif
	i = ADCpntr(mpa_y_adc)
	if((i.ge.0).and.(i.lt.adcs)) then
	   yt = adc(i)
	   adc(i) = -1
	else
	   bad_y = .true.
	endif
	if(bad_x.or.bad_y) then
	   bad_xy = bad_xy+1
	else
	   do k=0,adcs-1
		 if( adc(k).ge.0) then
		  e(n) = adc(k)
		  x(n) = xt
		  y(n) = yt
		  ste(n) = tag(k)
		  n = n+1
		 endif
	   enddo
	endif

85  if(j_mpa.lt.n_actual) goto 10
	ibranch = 1
	j_mpa = 0

90  return
end

!-------------------------------------------------------------------------------

integer function mpa4_accumulate_dtfx( argc, argv)

!DLL_EXPORT mpa4_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.16) then       		   ! needed # args
	mpa4_accumulate_dtfx= 1
	return
endif

call mpa4_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)) )

mpa4_accumulate_dtfx= 0
return
end

!------------------------------------------------------------

SUBROUTINE mpa4_accumulate_dtfx_b( image_mode, x,y,ste,veto,pseudo,n, fx,n_fx, channel_on,nc, xrange,yrange, flux, dead, weight)

! Accumulate busy, count for dead-time estimation, and the charge map.

INTEGER*4 image_mode, n,n_fx,nc, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1), channel_on(0:nc-1)
INTEGER*4 fx(0:n_fx-1,0:n-1)
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), flux(0:xrange-1,0:yrange-1)

if(n.le.0) return
if(nc.le.0) return
if((xrange.le.0).or.(yrange.le.0)) return
if(n_fx.lt.3) return

if(image_mode.eq.1) then
	do i=0,n-1
		if(pseudo(i).eq.1) then
			if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
				if( (ste(i).ge.0).and.(ste(i).lt.nc)) then
					if (channel_on(ste(i)).eq.1) then
						dead(x(i),y(i)) = dead(x(i),y(i)) + fx(0,i)		! busy per pixel
						weight(x(i),y(i)) = weight(x(i),y(i)) + fx(1,i)		! counts per pixel
					endif
				endif
				flux(x(i),y(i)) = flux(x(i),y(i)) + fx(2,i)				! charge pulses
			endif
		endif
	enddo
else
	do i=0,n-1
		if(pseudo(i).eq.1) then
			if( (ste(i).ge.0).and.(ste(i).lt.xrange)) then
				dead(ste(i),0) = dead(ste(i),0) + fx(0,i)				! busy per channel
				weight(ste(i),0) = weight(ste(i),0) + fx(1,i)				! counts per channel
			endif
			flux(0,0) = flux(0,0) + fx(2,i)							! charge pulses
		endif
	enddo
endif
return
end

!-------------------------------------------------------------------------------

integer function mpa4_events2( argc, argv)

!DLL_EXPORT mpa4_events2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.20) then          ! needed # args
	mpa4_events2 = 1
	return
endif

call mpa4_events2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	 %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), &
	 %val(argv(20)) )

mpa4_events2 = 0
return
end

!------------------------------------------------------------

! This routine reads MPA4 list-mode data. If it needs to get another buffer
! to continue building an event, it will set ibranch, to branch on return.
! Many local variables are passed here in order to continue after this branch.
! Each pass the current e,x1,y1,ste arrays are returned.
!
! Byte order access to 'ev' assumes little endian byte order.
!
! fx used for pseudo events:
!	0: busy, weight - for dead_fraction
!	1: charge - for charge counts

SUBROUTINE mpa4_events2_b( ev,n_buffer,n_actual, e,x,y,ste, veto,fx,n_fx, n_max,n, X0,Y0, charge_adc, &
			channel_on,nc, j_mpa,ibranch, bad_xy)

INTEGER*8 ev(0:n_buffer-1)
INTEGER*4 i,j,k,n, ibranch,j_mpa, n_buffer,n_actual,n_max,n_fx, bad_xy, X0,Y0, charge_adc, nc
INTEGER*4 x(0:n_max-1),y(0:n_max-1), fx(0:n_fx-1,0:n_max-1)
INTEGER*2 e(0:n_max-1),ste(0:n_max-1),veto(0:n_max-1), channel_on(0:nc-1)

INTEGER*1 bit(0:7), type_mask, bit6, bit3, bit4
INTEGER*4 byte_mask, word_mask, byte3_mask
parameter ( type_mask = z'0F', bit6 = z'40', bit3 = z'08', bit4 = z'10' )
parameter ( byte_mask = z'FF', word_mask = z'FFFF', byte3_mask = z'FFFFFF' )

INTEGER*8 fev
INTEGER*4 counter3, counter4, temp, Xold,Yold, busy(0:7), count(0:7), charge
INTEGER*2 i_bit, k_byte, i_adc, adcs, ADCindex(0:7), adc(0:7)
INTEGER*1 adc_active, adc_not_busy, bev(0:7)
equivalence (fev, bev(0))

common /c_mpa4_1b/ i_bit, k_byte, i_adc, ADCindex, adcs, counter3, counter4, fev, busy, count
common /c_mpa4_2b/ adc_active, adc_not_busy, adc, charge, Xold,Yold

	j_mpa = 0
	n = 0

	if(n_buffer.lt.1) return
	if(n_actual.lt.1) return
	if(n_fx.ne.3) return
	if(n_max.lt.1) return
	if( n_actual.gt.n_buffer) n_actual=n_buffer

	if( ibranch.le.0) then						! use ibranch=0 to INIT
		ibranch = 1
		do i=0,7
			busy(i) = 0
			count(i) = 0
		enddo
		charge = 0
		Xold = -10000
		Yold = -10000
		X0 = -10000
		Y0 = -10000
	endif

	do i=0,7
	   bit(i) = ishft(1,i)
	enddo

1	fev = ev(j_mpa)
	goto (10,20,30,40,50,60,70,80,90), ibranch

10	if(j_mpa.ge.n_actual) goto 99

	if( iand( bev(0),type_mask).eq.8) then				! 1ms timer event
		adc_not_busy = bev(1)
		i_adc = 0
		ibranch = 2
		goto 20
	endif
	if( iand( bev(0),type_mask).eq.7) then				! ADC or Coinc event
		if( iand( bev(0),bit6).ne.0) then	
			adc_active = bev(1)
			k_byte = 2
			adcs = 0
			i_adc = 0
			do i=0,7
				ADCindex(i) = -1
				adc(i) = 0
			enddo
			ibranch = 3					! Coinc event
			goto 30
		else
			ibranch = 9					! ADC singles event
			goto 90
		endif
	endif

20	do i=0,7
		if( iand(bit(i),adc_not_busy).eq.0) then		! 1ms timer event
			busy(i) = busy(i)+1
		endif
		count(i) = count(i)+1
	enddo
	j_mpa = j_mpa+1
	ibranch = 1
	goto 99

30	if( i_adc.ge.7) ibranch = 4
	if( iand(bit(i_adc),adc_active).ne.0) then			! Coinc event
		ADCindex(adcs) = i_adc
		adc(i_adc) = bev(k_byte)				! save ADC values
		k_byte = k_byte+1					! move to next ADC byte
		temp = bev(k_byte)
		adc(i_adc) = ior( ishft(temp,8), iand(adc(i_adc),byte_mask))
		k_byte = k_byte+1					! move to next ADC byte
		adcs = adcs+1
		if(k_byte.ge.8) then
			j_mpa = j_mpa+1					! move to next line
			k_byte = 0
			i_adc = i_adc+1
			goto 99
		endif
	endif
	i_adc = i_adc+1
	if( i_adc.lt.8) goto 30						! loop on all 8 ADC bits

40	counter3 = bev(k_byte)						! get byte 0 counter3
	k_byte = k_byte+1						! move to next byte
	temp = bev(k_byte)						! assumes little endian order
	counter3 = ior( ishft(temp,8), iand(counter3,byte_mask))	! get byte 1 counter3
	k_byte = k_byte+1						! move to next byte
	ibranch = 5
	if(k_byte.ge.8) then
		j_mpa = j_mpa+1						! move to next line
		k_byte = 0
		goto 99
	endif

50	temp = bev(k_byte)						! assumes little endian order
	counter3 = ior( ishft(temp,16), iand(counter3,word_mask))	! get byte 2 counter3
	k_byte = k_byte+1						! move to next byte
	temp = bev(k_byte)						! assumes little endian order
	counter3 = ior( ishft(temp,24), iand(counter3,byte3_mask))	! get byte 3 counter3
	X0 = counter3
	k_byte = k_byte+1						! move to next byte
	ibranch = 6
	if(k_byte.ge.8) then
		j_mpa = j_mpa+1						! move to next line
		k_byte = 0
		goto 99
	endif

60	counter4 = bev(k_byte)						! get byte 0 counter4
	k_byte = k_byte+1						! move to next word
	temp = bev(k_byte)						! assumes little endian order
	counter4 = ior( ishft(temp,8), iand(counter4,byte_mask))	! get byte 1 counter4
	k_byte = k_byte+1						! move to next byte
	ibranch = 7
	if(k_byte.ge.8) then
		j_mpa = j_mpa+1						! move to next line
		k_byte = 0
		goto 99
	endif

70	temp = bev(k_byte)
	counter4 = ior( ishft(temp,16), iand(counter4,word_mask))	! get byte 2 counter4
	temp = bev(k_byte)						! assumes little endian order
	counter4 = ior( ishft(temp,24), iand(counter4,byte3_mask))	! get byte 3 counter4
	Y0 = counter4
	ibranch = 8
	i_adc = 0

	if((X0.ne.Xold).or.(Y0.ne.Yold)) then
		if( n.ge.n_max-8) then					! output full, error!
			bad_xy = bad_xy + 8
			goto 80
		endif		

		do i=0,7
			veto(n) = 1					! output pseudo event
			x(n) = Xold
			y(n) = Yold					! save X,Y (previous)
			e(n) = 0
			ste(n) = i
			fx(0,n) = busy(i)				! ADCs busy
			fx(1,n) = count(i)				! total 1ms timers
			if(i.eq.charge_adc) then
				fx(2,n) = charge			! total charge for this pixel
				charge = 0
			else
				fx(2,n) = 0
			endif
			n = n+1
			busy(i) = 0					! clear counters
			count(i) = 0
		enddo
		Xold = X0						! save old X,Y
		Yold = Y0
	endif

80	if( n.ge.n_max-8) then						! output full, error!
		bad_xy = bad_xy + 8
		goto 85
	endif		

	do i=0,7
	    k = ADCindex(i)
	    if( (k.ge.0).and.(k.lt.nc)) then				! output X-ray event
		if( channel_on(k).eq.1) then
			veto(n) = 0
			x(n) = X0					! new X,Y
			y(n) = Y0
			e(n) = adc(k)
			ste(n) = k
			fx(0,n) = 0
			fx(1,n) = 0
			fx(2,n) = 0
			n = n+1
		endif
		if(k.eq.charge_adc) then
			if(e(n).gt.3) charge = charge + 1		! increment charge pulses
		endif 
	    endif
	enddo

85	j_mpa = j_mpa+1							! next event
	ibranch = 1
	goto 99  

90	j_mpa = j_mpa+1							! singles event
	ibranch = 1
	goto 99	

99	if(j_mpa.lt.n_actual) goto 1
100 	return
end

!-------------------------------------------------------------------------------

integer function mpa4_events( argc, argv)

!DLL_EXPORT mpa4_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	mpa4_events = 1
	return
endif

call mpa4_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)), %val(argv(13)), %val(argv(14)) )

mpa4_events = 0
return
end

!------------------------------------------------------------

! This routine reads MPA4 list-mode data. If it needs to get another buffer
! to continue building an event, it will set ibranch, to branch on return.
! Many local variables are passed here in order to continue after this branch.
! Each pass the current e,x1,y1,ste arrays are returned.
!
! Equivalence of 16,32 words to 'ev' assumes little endian byte order.

SUBROUTINE mpa4_events_b( ev,n_buffer,n_actual, e,x,y,ste, veto,weight, n,n_max, j_mpa,ibranch, bad_xy)

INTEGER*8 ev(0:n_buffer-1)
INTEGER*4 i,j,k,n, ibranch,j_mpa, n_buffer,n_actual,n_max, bad_xy
INTEGER*1 bit(0:7)
INTEGER*2 x(0:n_max-1),y(0:n_max-1),e(0:n_max-1),ste(0:n_max-1),veto(0:n_max-1),weight(0:n_max-1)

INTEGER*1 type_mask, bit6, bit3, bit4
INTEGER*4 byte_mask, word_mask, byte3_mask
parameter ( type_mask = z'0F', bit6 = z'40', bit3 = z'08', bit4 = z'10' )
parameter ( byte_mask = z'FF', word_mask = z'FFFF', byte3_mask = z'FFFFFF' )

INTEGER*8 fev
INTEGER*4 counter3, counter4, temp
INTEGER*2 i_bit, k_byte, i_adc, adcs, ADCindex(0:7), adc(0:7), busy(0:7), count(0:7), X0,Y0
INTEGER*1 adc_active, adc_not_busy, bev(0:7)
equivalence (fev, bev(0))

common /c_mpa4_1/ i_bit, k_byte, i_adc, ADCindex, adcs, counter3, counter4, fev, busy, count, X0,Y0
common /c_mpa4_2/ adc_active, adc_not_busy, adc

	j_mpa = 0
	if( ibranch.le.0) then						! use ibranch=0 to INIT
		ibranch = 1
		do i=0,7
			busy(i) = 0
			count(i) = 0
			X0 = -10000
			Y0 = -10000
		enddo
	endif

	n = 0
	if(n_actual.lt.1) goto 100
	if( n_actual.gt.n_buffer) n_actual=n_buffer

	do i=0,7
	   bit(i) = ishft(1,i)
	enddo

1	fev = ev(j_mpa)
	goto (10,20,30,40,50,60,70,80,90), ibranch

10	if(j_mpa.ge.n_actual) goto 99

	if( iand( bev(0),type_mask).eq.8) then				! 1ms timer event
		adc_not_busy = bev(1)
		i_adc = 0
		ibranch = 2
		goto 20
	endif
	if( iand( bev(0),type_mask).eq.7) then				! ADC or Coinc event
		if( iand( bev(0),bit6).ne.0) then	
			k_byte = 2
			adc_active = bev(1)
			adcs = 0
			i_adc = 0
			do i=0,7
				ADCindex(i) = -1
				adc(i) = 0
			enddo
			ibranch = 3					! Coinc event
			goto 30
		else
			ibranch = 9					! ADC singles event
			goto 90
		endif
	endif

20	do i=0,7
		if( iand(bit(i),adc_not_busy).eq.0) then		! 1ms timer event
			busy(i) = busy(i)+1
		endif
		count(i) = count(i)+1
	enddo
	j_mpa = j_mpa+1
	ibranch = 1
	goto 99

30	if( i_adc.ge.7) ibranch = 4
	if( iand(bit(i_adc),adc_active).ne.0) then			! Coinc event
		ADCindex(adcs) = i_adc
		adc(i_adc) = bev(k_byte)				! save ADC values
		k_byte = k_byte+1					! move to next ADC byte
		temp = bev(k_byte)
		adc(i_adc) = ior( ishft(temp,8), iand(adc(i_adc),byte_mask))
		k_byte = k_byte+1					! move to next ADC byte
		adcs = adcs+1
		if(k_byte.ge.8) then
			j_mpa = j_mpa+1					! move to next line
			k_byte = 0
			i_adc = i_adc+1
			goto 99
		endif
	endif
	i_adc = i_adc+1
	if( i_adc.lt.8) goto 30						! loop on all 8 ADC bits

40	counter3 = bev(k_byte)						! get byte 0 counter3
	k_byte = k_byte+1						! move to next byte
	temp = bev(k_byte)						! assumes little endian order
	counter3 = ior( ishft(temp,8), iand(counter3,byte_mask))	! get byte 1 counter3
	k_byte = k_byte+1						! move to next byte
	ibranch = 5
	if(k_byte.ge.8) then
		j_mpa = j_mpa+1						! move to next line
		k_byte = 0
		goto 99
	endif

50	temp = bev(k_byte)						! assumes little endian order
	counter3 = ior( ishft(temp,16), iand(counter3,word_mask))	! get byte 2 counter3
	k_byte = k_byte+1						! move to next byte
	temp = bev(k_byte)						! assumes little endian order
	counter3 = ior( ishft(temp,24), iand(counter3,byte3_mask))	! get byte 3 counter3
	k_byte = k_byte+1						! move to next byte
	ibranch = 6
	if(k_byte.ge.8) then
		j_mpa = j_mpa+1						! move to next line
		k_byte = 0
		goto 99
	endif

60	counter4 = bev(k_byte)						! get byte 0 counter4
	k_byte = k_byte+1						! move to next word
	temp = bev(k_byte)						! assumes little endian order
	counter4 = ior( ishft(temp,8), iand(counter4,byte_mask))	! get byte 1 counter4
	k_byte = k_byte+1						! move to next byte
	ibranch = 7
	if(k_byte.ge.8) then
		j_mpa = j_mpa+1						! move to next line
		k_byte = 0
		goto 99
	endif

70	temp = bev(k_byte)
	counter4 = ior( ishft(temp,16), iand(counter4,word_mask))	! get byte 2 counter4
	temp = bev(k_byte)						! assumes little endian order
	counter4 = ior( ishft(temp,24), iand(counter4,byte3_mask))	! get byte 3 counter4
	ibranch = 8
	i_adc = 0

	if( (counter3.ne.X0).or.(counter4.ne.Y0)) then
		if( n.ge.n_max-8) then					! output full, error!
			bad_xy = bad_xy + 8
			goto 75
		endif		

		do i=0,7
			if( busy(i).gt.0) then
				veto(n) = 1				! output pseudo event
				e(n) = busy(i)				! ADCs busy
				weight(n) = count(i)			! total 1ms timers
				x(n) = 1000 + X0
				y(n) = 1000 + Y0			! save previous X,Y
				ste(n) = i
				n = n+1
			endif
			busy(i) = 0
			count(i) = 0
		enddo
75		X0 = counter3
		Y0 = counter4
	endif

80	if( n.ge.n_max-8) then						! output full, error!
		bad_xy = bad_xy + 8
		goto 85
	endif		

	do i=0,7
	    if( ADCindex(i).ge.0) then
		k = ADCindex(i)
		if( k.ge.0) then					! output X-ray event
			veto(n) = 0
			e(n) = adc(k)
			weight(n) = 1
			x(n) = 1000 + X0				! new X,Y
			y(n) = 1000 + Y0
			ste(n) = k
			n = n+1
		endif
	    endif
	enddo

85	j_mpa = j_mpa+1							! next event
	ibranch = 1
	goto 99  

90	j_mpa = j_mpa+1							! singles event
	ibranch = 1
	goto 99	

99	if(j_mpa.lt.n_actual) goto 1
100 	return
end

!-------------------------------------------------------------------------------

integer function mpa3_events( argc, argv)

!DLL_EXPORT mpa3_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.27) then          ! needed # args
	mpa3_events = 1
	return
endif

call mpa3_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	 %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), &
	 %val(argv(20)), %val(argv(21)), %val(argv(22)), %val(argv(23)), &
	 %val(argv(24)), %val(argv(25)), %val(argv(26)), %val(argv(27)) )

mpa3_events = 0
return
end

!------------------------------------------------------------
! This routine reads MPA3 list-mode data. If it needs to get another buffer
! to continue building an event, it will set ibranch, to branch on return.
! Many local variables are passed here in order to continue after this branch.
! Each pass the current e,x1,y1,ste arrays are returned.

SUBROUTINE mpa3_events_b( ev,j_mpa,ibranch,n_buffer,n_actual, &
	   e,x,y,ste, n,n_max, mpa_x_adc,mpa_y_adc, &
	   hword,lword, rtcrec,dummyrec, ADCindex, ADCpntr, &
	   adc,tag, k,maxADCs, adcs, bad_xy, rt, rtadc )          ! rtc?

INTEGER*4 i,j, k,n, ibranch, j_mpa, n_buffer,n_actual,bad_xy
INTEGER*4 rt, rtadc(0:15), maxADCs, adcs, n_max
INTEGER*2 ev(0:1,0:n_buffer-1), hword,lword,bit(0:15), adc(0:maxADCs-1)
INTEGER*2 TimerEvent, RTCmask, DummyMask, sync, RTCrec, dummyrec
INTEGER*2 ADCindex(0:15), ADCpntr(0:15), tag(0:maxADCs-1), xt,yt
INTEGER*2 x(0:n_max-1),y(0:n_max-1),e(0:n_max-1),ste(0:n_max-1)
INTEGER*2 mpa_x_adc,mpa_y_adc
integer*4 rtc0, rtc1, rtc2
! integer*4 rtc(0:n_max-1)

parameter ( TimerEvent= z'4000', RTCmask = z'1000', DummyMask = z'8000')
parameter ( sync = z'FFFF')

	do i=0,15
	   bit(i) = ishft(1,i)
	enddo
	if(j_mpa.lt.0) j_mpa=0
	if(k.lt.0) k=0
	if(mpa_x_adc.lt.0) mpa_x_adc=0
	if(mpa_x_adc.gt.15) mpa_x_adc=15
	if(mpa_y_adc.lt.0) mpa_y_adc=0
	if(mpa_y_adc.gt.15) mpa_y_adc=15
	if(n_actual.gt.n_buffer) n_actual=n_buffer

	n = 0
	if(n_actual.lt.1) goto 90

	goto (10,20,30,40,50,60,70,80,90), ibranch

10  if(j_mpa.ge.n_actual) goto 90
	hword = ev(1,j_mpa)
	lword = ev(0,j_mpa)

	if( hword.eq.TimerEvent) then
	   rt = rt+1
	   if( lword.ne.0) then
		 do i=0,15
		  if( iand(bit(i),lword).ne.0) then
			  rtadc(i) = rtadc(i)+1
		  endif
		 enddo
	   endif
	   j_mpa = j_mpa+1
	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 1
		 goto 90
	   endif
	   goto 85             ! loop back to 10
	endif

20  if((lword.eq.sync).and.(hword.eq.sync)) then
	   j_mpa = j_mpa+1
	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 1
		 goto 90
	   endif
	   goto 85             ! loop back to 10
	endif

30  rtcrec = iand( hword, RTCmask)
	dummyrec = iand( hword, DummyMask)
	do i=0,maxADCs-1
	   adc(i) = 0
	enddo

	adcs = 0
	do i=0,15
	   ADCindex(i) = 0
	enddo
	do i=0,15
	   if( iand(bit(i),lword).ne.0) then
		 ADCindex(adcs) = i
		 adcs = adcs+1
	   endif
	enddo
	k = 0
	j_mpa = j_mpa+1
	if( j_mpa.ge.n_actual) then
	   j_mpa = 0
	   ibranch = 4
	   goto 90
	endif

40  hword = ev(1,j_mpa)
	lword = ev(0,j_mpa)

	if( rtcrec.ne.0) then
	   rtc0 = lword
	   rtc1 = hword
	   j_mpa = j_mpa+1
	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 5
		 goto 90
	   endif
	else
	   goto 60
	endif

50  hword = ev(1,j_mpa)
	lword = ev(0,j_mpa)
	rtc2 = lword
!   rtc(n-1) = (rtc2*65536+rtc1)*65536+rtc0

	if( dummyrec.eq.0) then
	   if(k.lt.16) then
		 j = ADCindex(k)
		 if((j.ge.0).and.(j.lt.16)) then
		  i = ADCpntr(j)
		  if((i.ge.0).and.(i.lt.maxADCs)) then
			  adc(i) = hword
			  tag(i) = j
		  endif
		 endif
	   endif
	   k = k+1
	endif
	j_mpa = j_mpa+1
	if( j_mpa.ge.n_actual) then
	   j_mpa = 0
	   ibranch = 7
	   goto 90
	endif
	goto 70

60  if( dummyrec.ne.0) then
	   if(k.lt.16) then
		 j = ADCindex(k)
		 if((j.ge.0).and.(j.lt.16)) then
		  i = ADCpntr(j)
		  if((i.ge.0).and.(i.lt.maxADCs)) then
			  adc(i) = hword
			  tag(i) = j
		  endif
		 endif
	   endif
	   k = k+1
	   j_mpa = j_mpa+1
	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 7
		 goto 90
	   endif
	endif

70  if( k.lt.adcs) then
	   hword = ev(1,j_mpa)
	   lword = ev(0,j_mpa)
	   if(k.lt.16) then
		 j = ADCindex(k)
		 if((j.ge.0).and.(j.lt.16)) then
		  i = ADCpntr(j)
		  if((i.ge.0).and.(i.lt.maxADCs)) then
			  adc(i) = lword
			  tag(i) = j
		  endif
		 endif
	   endif
	   if(k+1.lt.16) then
		 j = ADCindex(k+1)
		 if((j.ge.0).and.(j.lt.16)) then
		  i = ADCpntr(j)
		  if((i.ge.0).and.(i.lt.maxADCs)) then
			  adc(i) = hword
			  tag(i) = j
		  endif
		 endif
	   endif
	   k = k+2
	   j_mpa = j_mpa+1
	   if( j_mpa.ge.n_actual) then
		 j_mpa = 0
		 ibranch = 7
		 goto 90
	   endif
	endif
	if( k.lt.adcs) goto 70

80  xt = 0
	yt = 0
	i = ADCpntr(mpa_x_adc)
	if((i.ge.0).and.(i.lt.maxADCs)) then
	   xt = adc(i)
	   adc(i) = 0
	endif
	i = ADCpntr(mpa_y_adc)
	if((i.ge.0).and.(i.lt.maxADCs)) then
	   yt = adc(i)
	   adc(i) = 0
	endif
	if((xt.eq.0).and.(yt.eq.0)) bad_xy=bad_xy+1
	do k=0,maxADCs-1
	   if( adc(k).ne.0) then
		 e(n) = adc(k)
		 x(n) = xt
		 y(n) = yt
		 ste(n) = tag(k)
		 n = n+1
	   endif
	enddo

85  if(j_mpa.lt.n_actual) goto 10
	ibranch = 1

90  return
end

!-------------------------------------------------------------------------------

integer function mdaq2_accumulate_dtfx_3D( argc, argv)

!DLL_EXPORT mdaq2_accumulate_dtfx_3D
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.22) then       		   ! needed # args
	mdaq2_accumulate_dtfx_3D = 1
	return
endif

call mdaq2_accumulate_dtfx_3D_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)) )

mdaq2_accumulate_dtfx_3D = 0
return
end

!------------------------------------------------------------

SUBROUTINE mdaq2_accumulate_dtfx_3D_b( image_mode, x,y,z,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 			xcompress,ycompress, xrange,yrange,zrange, dta, flux,dead,weight,dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action:
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale, when not in 3D mode
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] to set 'dwell' (ms)
!
!	Dead:
!		Use fx[4,*] to set DT (ms) in 'dead'.
!		Weighted by raw count in 'weight' (fx[5,*]), only in image mode.
!		Later (in da_evt) norm this using both the weight array and the dwell (ms) array.

INTEGER*4 image_mode, flux_mode, n,n_fx, xcompress,ycompress, xrange,yrange,zrange, i
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, flux(0:xrange-1,0:yrange-1,0:zrange-1)
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt, dta
LOGICAL mode_3D

if(n.le.0) return
if(n_fx.lt.6) return
mode_3D = (zrange.ge.2)
dwell_last = 0.
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange).and.(z(i).ge.0).and.(z(i).lt.zrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),z(i)) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),z(i)) = flux(x(i),y(i),z(i)) + fx(0,i) * flux_scale2
				endif
				if( .not.mode_3D) then
					flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
					flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				endif
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
				endif
				if( fx(4,i).gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + fx(4,i)
				endif
				weight(x(i),y(i)) = weight(x(i),y(i)) + fx(5,i)
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in mdaq2 device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.1) then
				if( fx(4,i).gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + fx(4,i)
				endif
				weight(ste(i),0) = weight(ste(i),0) + fx(5,i)
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function mdaq2_accumulate_dtfx( argc, argv)

!DLL_EXPORT mdaq2_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.21) then       		   ! needed # args
	mdaq2_accumulate_dtfx = 1
	return
endif

call mdaq2_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)) )

mdaq2_accumulate_dtfx = 0
return
end

!------------------------------------------------------------------------------------------------


SUBROUTINE mdaq2_accumulate_dtfx_b( image_mode, x,y,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 			xcompress,ycompress, xrange,yrange,n_flux, dta, flux, dead, weight, dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action:
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!				(not used for MDAQ2 as yet) 
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] to set 'dwell' (ms)
!
!	Dead:
!		Use fx[4,*] to set DT (ms) in 'dead'.
!		Weighted by raw count in 'weight' (fx[5,*]), only in image mode.
!		Later (in da_evt) norm this using both the weight array and the dwell (ms) array.

INTEGER*4 image_mode, flux_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, flux(0:xrange-1,0:yrange-1,0:n_flux), dt, dwell_last, dta
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1)

if(n.le.0) return
if(n_fx.lt.6) return
dwell_last = 0.
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),0) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),0) = flux(x(i),y(i),0) + fx(0,i) * flux_scale2
				endif
				flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
				flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
				endif
				if( fx(4,i).gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + fx(4,i)
				endif
				weight(x(i),y(i)) = weight(x(i),y(i)) + fx(5,i)
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in mdaq2 device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.1) then
				if( fx(4,i).gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + fx(4,i)
				endif
				weight(ste(i),0) = weight(ste(i),0) + fx(5,i)
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function init_mdaq2( argc, argv)

!DLL_EXPORT init_mdaq2
INTEGER_ARCH argc, argv(*)

integer*4 remain, x0_last, y0_last, z0_last, tcount
integer*4 stats_type, stats_ovf, jbuff
INTEGER*1 bev(0:3)
Integer*2 ldone, rdone, ldone2, rdone2
integer*4 u0_last, v0_last, w0_last, fc0, fc1, fc2, fc3
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
common /c_fx_6/ dwell_last
common /c_fx_8/ fc0, fc1, fc2, fc3, u0_last, v0_last, w0_last

jbuff = 0
remain = 0
ldone = 1
rdone = 1
ldone2 = 1
rdone2 = 1
x0_last = 0
y0_last = 0
z0_last = 0
u0_last = 0
v0_last = 0
w0_last = 0
stats_type = 0
stats_ovf = 0
tcount = 0
dwell_last = 0.0
fc0 = 0
fc1 = 0
fc2 = 0
fc3 = 0

init_mdaq2 = 0
return
end

!-------------------------------------------------------------------------------

integer function mdaq2_events( argc, argv)

!DLL_EXPORT mdaq2_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.34) then 		         ! needed # args
	mdaq2_events = 1
	return
endif

call mdaq2_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), &
	 %val(argv(13)), %val(argv(14)), %val(argv(15)), %val(argv(16)), &
	 %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), &
	 %val(argv(25)), %val(argv(26)), %val(argv(27)), %val(argv(28)), %val(argv(29)), %val(argv(30)), &
	 %val(argv(31)), %val(argv(32)), %val(argv(33)), %val(argv(34)) )

mdaq2_events = 0
return
end

!-------------------------------------------------------------------------------
!
! Version of MicroDAQ using a variant of the FalconX List-mode.
!
!	flux_mode	0		-- not used? --		fx index 0		-- later -- select FCn?
!			1		FC0 h/w				 1		FC0
!			2		FC1 h/w				 2		FC1
!			3		FC2 h/w				 3		Dwell (ms)
!			4		FC3 h/w				 4		dead-time (ms)
!									 5		raw counts

SUBROUTINE mdaq2_events_b( ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, &
		fx,n_fx, flux_mode,clock, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,idebug )

! assumes that n_buffer IS divisable by 4
! assume that n_fx is 6 (at least, for: DT, FC0, FC1, Dwell, DT, raw)
! tags used for now to hold ev[] index 'i'

INTEGER*4 i,j,k,l,m,n,nb,nc, n_buffer, n_max, jbuff, err
INTEGER*8 dev
INTEGER*4 n_events, n_fx, flux_mode
INTEGER*4 length, len2, skip, bad, swap, idebug
INTEGER*2 tag, ibranch, x0,y0,z0,u0,v0,w0
INTEGER*2 e(0:n_events-1), t(0:n_events-1), x(0:n_events-1), y(0:n_events-1), z(0:n_events-1), u(0:n_events-1), v(0:n_events-1)
INTEGER*2 ste(0:n_events-1), tags(0:n_events-1), channel_on(0:nc-1), veto(0:n_events-1), w(0:n_events-1)
INTEGER*1 ev(0:n_buffer-1), bev(0:3)
REAL*4 fx(0:n_fx-1,0:n_events-1), tick, clock, small, temp
LOGICAL incomplete

INTEGER*4 data_type_mask, data_type_offset
INTEGER*2 pulse1_data_type, position_data_type, spatial1_data_type, error_data_type
INTEGER*4 position_length, spatial1_length
INTEGER*4 error_length, pulse1_length, busy_data_type, busy2_data_type, busy3_data_type
INTEGER*4 pulse1_energy_mask, pulse1_energy_offset, busy_busy_mask, busy_channel_mask, busy_channel_offset
INTEGER*4 pulse1_channel_mask, pulse1_channel_offset, busy_length, busy3_length
INTEGER*4 busy2_busy_mask, busy2_channel_mask, busy2_channel_offset, busy2_length, busy3_type_mask
INTEGER*4 busy3_busy_lsb_type, busy3_busy_msb_type, busy3_raw_lsb_type, busy3_raw_msb_type, busy3_data_mask
INTEGER*4 position_type_mask, position_type_offset, position_data_mask, busy3_channel_offset, busy3_type_offset
INTEGER*4 position_axis0_type, position_axis1_type, position_axis2_type, position_axis3_type 
INTEGER*4 position_axis4_type, position_axis5_type, spatial1_busy_type, busy3_channel_mask
INTEGER*4 spatial1_type_mask, spatial1_type_offset, spatial1_data_mask
INTEGER*4 spatial1_sample_msb_mask, spatial1_sample_msb_offset, spatial1_busy_mask, spatial1_busy_offset
INTEGER*4 spatial1_sample_countl_type, spatial1_sample_countm_type, spatial1_generic_count4_type
INTEGER*4 spatial1_generic_count1_type, spatial1_generic_count2_type, spatial1_generic_count3_type
INTEGER*4 error_type_mask, error_type_offset, error_analogue_status_type, error_overflow_type
INTEGER*4 error_saturate_mask, error_saturate_offset, error_time_stamp_mask

parameter (data_type_mask = z'F0000000', data_type_offset=-28, busy2_data_type=3, busy3_data_type=6)
parameter (pulse1_data_type=0, position_data_type=12, spatial1_data_type=11, error_data_type=15, busy_data_type=2)
parameter (position_length = 6, spatial1_length = 6)
parameter (error_length = 1, pulse1_length = 1, busy_length = 1, busy2_length = 1, busy3_length = 4)
parameter (pulse1_energy_mask = z'0000FFFF')
parameter (pulse1_energy_offset = 0, pulse1_channel_mask = z'0F000000', pulse1_channel_offset = -24)
parameter (busy_busy_mask = z'00FFFFFF', busy_channel_mask = z'0F000000', busy_channel_offset = -24)
parameter (busy2_busy_mask = z'000000FF', busy2_channel_mask = z'0F000000', busy2_channel_offset = -24)
parameter (busy3_type_mask = z'0F000000', busy3_type_offset = -24, busy3_channel_mask = z'00F00000')
parameter (busy3_channel_offset = -20, busy3_data_mask = z'000FFFFF', busy3_busy_lsb_type = 0)
parameter (busy3_busy_msb_type = 1, busy3_raw_lsb_type = 2, busy3_raw_msb_type = 3)
parameter (position_type_mask =	z'0F000000', position_type_offset = -24, position_data_mask = z'00FFFFFF')
parameter (position_axis0_type = 0, position_axis1_type = 1, position_axis2_type = 2, position_axis3_type = 3)
parameter (position_axis4_type = 4, position_axis5_type = 5)
parameter (spatial1_type_mask = z'0F000000', spatial1_type_offset = -24, spatial1_data_mask = z'00FFFFFF')
parameter (spatial1_sample_msb_mask = z'000000FF', spatial1_sample_msb_offset = 24)
parameter (spatial1_sample_countl_type = 0, spatial1_sample_countm_type = 1, spatial1_generic_count4_type = 5)
parameter (spatial1_generic_count1_type = 2, spatial1_generic_count2_type = 3, spatial1_generic_count3_type = 4)
parameter (error_type_mask = z'0F000000', error_type_offset = -24, error_analogue_status_type = 0, error_overflow_type = 1)
parameter (error_saturate_mask = z'000C0000', error_saturate_offset = -18, error_time_stamp_mask = z'00FFFFFF')

INTEGER*4 de, tx, ty, tz, tt, tev, tcount
INTEGER*2 dtag, pdf, adr
integer*4 remain, x0_last, y0_last, z0_last, u0_last, v0_last, w0_last, fc0, fc1, fc2, fc3
integer*4 stats_type, stats_ovf, icr, raw, busy
integer*2 ldone, rdone, ldone2, rdone2
real*4 dwell_last

common /c_fx_1/ jbuff, bev, remain
common /c_fx_4/ ldone, rdone, ldone2, rdone2
common /c_fx_6/ dwell_last
common /c_fx_7/ x0_last, y0_last, z0_last, stats_type, stats_ovf, tcount
common /c_fx_8/ fc0, fc1, fc2, fc3, u0_last, v0_last, w0_last
common /c_fx_9/ icr, raw
common /c_fx_10/ busy
equivalence (bev(0), tev)

if(n_fx.lt.6) return
!tick = 1000./80.e+6		! tick (ms) for 80 MHz FPGA clock (will need to scale these later)
tick = 1.0e-3 / clock		! clock in MHz.
small = 1.0e-5
n_max = n_events
n = 0
i = 0
nm = 0
goto (10,20,30,40,10,10,10,10,10,100), ibranch

10  call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return          ! first long word (get_fx advances 'i' +4)
	
	tag = ishft( iand( tev, data_type_mask), data_type_offset)

	if(tag.eq.pulse1_data_type) then		! Pulse 1 event

		de = ishft( iand( tev, pulse1_energy_mask ), pulse1_energy_offset )	! energy
		adr = ishft( iand( tev, pulse1_channel_mask), pulse1_channel_offset)	! channel
!		if(adr.gt.0) adr = adr-1

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = de
				t(n) = 0
				ste(n) = adr
				tags(n) = i-4
				veto(n) = 0
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				n = n+1
			endif
		endif

		skip = 0
		goto 100
	endif

	if(tag.eq.busy_data_type) then			! Busy event

		de = iand( tev, busy_busy_mask )					! busy
		adr = ishft( iand( tev, busy_channel_mask), busy_channel_offset)	! channel
!		if(adr.gt.0) adr = adr-1

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				dt_last = de * tick					! DT from busy counts

				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = 0
				t(n) = 0						! busy count
				ste(n) = adr
				tags(n) = i-4
				veto(n) = 1						! a pseudo event for DT
				do k=0,n_fx-1
					fx(k,n) = 0.0					! no weights using old BUSY
				enddo
				fx(4,n) = dt_last					! dead-time (ms), assumes same clock
				n = n+1
			endif
		endif

		skip = 0
		goto 100
	endif

	if(tag.eq.busy2_data_type) then			! Busy 2 MSB event

		de = iand( tev, busy2_busy_mask )					! busy MSB
		adr = ishft( iand( tev, busy2_channel_mask), busy2_channel_offset)	! channel
!		if(adr.gt.0) adr = adr-1

		if((de.gt.0).and.(adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then
				de = ishft( de, -busy_channel_offset)			! shift it UP by the LSB 24 bits
				dt_last = de * tick					! DT from busy counts

				x(n) = x0
				y(n) = y0
				z(n) = z0
				u(n) = u0
				v(n) = v0
				w(n) = w0
				e(n) = 0
				t(n) = 0						! busy count
				ste(n) = adr
				tags(n) = i-4
				veto(n) = 1						! a pseudo event for DT
				do k=0,n_fx-1
					fx(k,n) = 0.0					! no weights using old BUSY2
				enddo
				fx(4,n) = dt_last					! dead-time (ms), assumes same clock
				n = n+1
			endif
		endif

		skip = 0
		goto 100
	endif

	if(tag.eq.busy3_data_type) then			! busy3 event
		length = (busy3_length-1) *4						! remaining length
		remain = length

		tx = ishft( iand( tev, busy3_type_mask ), busy3_type_offset )		! type
		if(tx.eq.busy3_busy_lsb_type) then
			busy = iand( tev, busy3_data_mask )				! busy LSB
		endif

		ibranch = 4
		goto 40
	endif

	if(tag.eq.position_data_type) then		! position event
		length = (position_length-1) *4						! remaining length
		remain = length

		tx = ishft( iand( tev, position_type_mask ), position_type_offset )	! type
		if(tx.eq.position_axis0_type) then
			x0_last = x0
			x0 = iand( tev, position_data_mask )				! x
		endif

		ibranch = 2
		goto 20
	endif

	if(tag.eq.spatial1_data_type) then		! spatial event
		length = (spatial1_length-1) *4
		remain = length

		tx = ishft( iand( tev, spatial1_type_mask ), spatial1_type_offset )	! type
		if(tx.eq.spatial1_sample_countl_type) then
			tcount = iand( tev, spatial1_data_mask)				! sample LSB
		endif

		ibranch = 3
		goto 30
	endif

	if(tag.eq.error_data_type) then			! error1 event

		tx = ishft( iand( tev, error_type_mask ), error_type_offset )		! type
		if(tx.eq.error_analogue_status_type) then
			tt = ishft( iand( tev, error_saturate_mask), error_saturate_offset )	! saturation
			bad = bad + 1
			idebug = 1000 + tt
		else if(tx.eq.error_overflow_type) then
			tt = iand( tev, error_time_stamp_mask)				! overflow
			bad = bad + 1
			idebug = 1000
		endif

		skip = 0
		goto 100
	endif

	skip = 0					! skip done already for 1 word (in get_fx)

100	if(i+skip.gt.n_buffer-1) then			! skip unwanted data
		skip = (i+skip) - n_buffer
		ibranch = 10
!		idebug = 91
		return
	endif
	i = i+skip
	ibranch = 1
	goto 10

!...................................................................................
!   Process the 'position' event ...

20	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 92
		goto 10
	endif

!   Process payload data (encoder) ...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tx = ishft( iand( tev, position_type_mask ), position_type_offset )		! type

	if(tx.eq.position_axis1_type) then
		y0_last = y0
		y0 = iand( tev, position_data_mask )					! y

	else if(tx.eq.position_axis2_type) then
		z0_last = z0
		z0 = iand( tev, position_data_mask )					! z

	else if(tx.eq.position_axis3_type) then
		u0_last = u0
		u0 = iand( tev, position_data_mask )					! u

	else if(tx.eq.position_axis4_type) then
		v0_last = v0
		v0 = iand( tev, position_data_mask )					! v

	else if(tx.eq.position_axis5_type) then
		w0_last = w0
		w0 = iand( tev, position_data_mask )					! w
	endif	
	remain = remain-4
	goto 20

!...................................................................................
!   Process the 'spatial' event ...

30	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 93
		goto 10
	endif

!   Process payload data (stats)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tx = ishft( iand( tev, spatial1_type_mask ), spatial1_type_offset )		! type

	if(tx.eq.spatial1_sample_countm_type) then
		tcount = ior( tcount, ishft( iand( tev, spatial1_sample_msb_mask), spatial1_sample_msb_offset))  ! sample 
		dwell_last = tcount * tick

	else if(tx.eq.spatial1_generic_count1_type) then
		fc0 = iand( tev, spatial1_data_mask)					! FC0

	else if(tx.eq.spatial1_generic_count2_type) then
		fc1 = iand( tev, spatial1_data_mask)					! FC1

	else if(tx.eq.spatial1_generic_count3_type) then
		fc2 = iand( tev, spatial1_data_mask)					! FC2

	else if(tx.eq.spatial1_generic_count4_type) then
		fc3 = iand( tev, spatial1_data_mask)					! FC3

		if(n.lt.n_events) then
			x(n) = x0	!x0_last
			y(n) = y0	!y0_last					! add a pseudo event
			z(n) = z0	!z0_last							
			u(n) = u0	!u0_last
			v(n) = v0	!v0_last
			w(n) = w0	!w0_last
			e(n) = 0
			t(n) = 0
			ste(n) = 0
			tags(n) = i-(spatial1_length*4)
			veto(n) = 1							! a pseudo event for counters
			do k=0,n_fx-1
				fx(k,n) = 0.0
			enddo
			fx(1,n) = fc0
			fx(2,n) = fc1
			fx(3,n) = dwell_last						! dwell (ms), assumes 10 ns clock
			if(flux_mode.eq.1) fx(0,n) = fc0
			if(flux_mode.eq.2) fx(0,n) = fc1
			n = n+1
		endif
	endif
	remain = remain-4
	goto 30

!...................................................................................
!   Process the 'busy3' event ...

40	if(remain.le.0) then                 	     ! skip insufficient data
!		i = i+remain
		ibranch = 1
		if(remain.ne.0) idebug = 94
		goto 10
	endif

!   Process payload data (busy)...

	call get_fx( ev, i, n_buffer, bev, 4, jbuff, incomplete, swap, err)
	if(incomplete.or.(err.eq.1)) return 

	tx = ishft( iand( tev, busy3_type_mask ), busy3_type_offset )			! type

	if(tx.eq.busy3_busy_msb_type) then
		ty = iand( tev, busy3_data_mask)		  			! busy MSB
		busy = ior( busy, ishft( ty, -busy3_channel_offset)) 

	else if(tx.eq.busy3_raw_lsb_type) then
		raw = iand( tev, busy3_data_mask)					! raw LSB

	else if(tx.eq.busy3_raw_msb_type) then
		ty = iand( tev, busy3_data_mask)
		raw = ior( raw, ishft( ty, -busy3_channel_offset)) 			! raw MSB

		adr = ishft( iand( tev, busy3_channel_mask), busy3_channel_offset)	! channel
!		if(adr.gt.0) adr = adr-1

		if((adr.ge.0).and.(adr.lt.nc)) then
			if((channel_on(adr).eq.1).and.(n.lt.n_events)) then

				x(n) = x0	!x0_last
				y(n) = y0	!y0_last				! add a pseudo event
				z(n) = z0	!z0_last							
				u(n) = u0	!u0_last
				v(n) = v0	!v0_last
				w(n) = w0	!w0_last
				e(n) = 0
				t(n) = 0
				ste(n) = adr
				tags(n) = i-(busy3_length*4)
				veto(n) = 1						! a pseudo event for counters
				do k=0,n_fx-1
					fx(k,n) = 0.0
				enddo
				fx(5,n) = float(raw)					! raw pulse count
				if( dwell_last.gt.small) then
					temp = float(busy) * tick / dwell_last		! dead-fraction
					if( temp.gt.0.95) temp = 0.95
					fx(4,n) = fx(5,n) * temp / (1. - temp)		! lost count
				endif
				n = n+1
			endif
		endif
	endif
	remain = remain-4
	goto 40
end

!-------------------------------------------------------------------------------

integer function mdaq_events( argc, argv)

!DLL_EXPORT mdaq_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.10) then          ! needed # args
	mdaq_events = 1
	return
endif

call mdaq_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)) )

mdaq_events = 0
return
end

!------------------------------------------------------------

SUBROUTINE mdaq_events_b( ev,n_buffer,channel_on,nc, e,x,y,n,adc, bad_xy )


! channel_on    indicates (1) whether a channel is selected


INTEGER*4 n_buffer, n, bad_xy
INTEGER*4 nc
INTEGER*2 e(0:n_buffer-1), x(0:n_buffer-1), y(0:n_buffer-1), channel_on(0:nc-1)
INTEGER*2 ev(0:2,0:n_buffer-1), adc(0:n_buffer-1)

INTEGER*4 i,j
INTEGER*2 e_mask, x_mask, y_mask, ste_mask, stx_mask, sty_mask, mdx_mask
INTEGER*2 mdy_mask, ste_offset, stx_offset, sty_offset
INTEGER*2 ste, stx, sty, correct_mdx, correct_mdy, one

parameter ( e_mask = z'0FFF', x_mask = z'0FFF', y_mask = z'0FFF')
parameter ( ste_mask = z'C000', stx_mask = z'3000', sty_mask = z'3000')
parameter ( mdx_mask = z'C000', mdy_mask = z'C000')
parameter ( ste_offset = -14, stx_offset = -12, sty_offset = -12)
parameter ( correct_mdx = z'8000', correct_mdy = z'4000', one=z'0001')

! The data uses the bottom bits for ADC data (13 bits e, 12 bits
! for x,y), and the top bits to tag the ADC station and to identify X,Y.
! Various data integrity checks are made, and the
! ADC data is accessed via bit masks.

j = 0
n = 0
if( n_buffer.lt.1) return

do i=0,n_buffer-1

	ste = ishft( iand( ev(0,i), ste_mask), ste_offset)
	stx = ishft( iand( ev(1,i), stx_mask), stx_offset)
	sty = ishft( iand( ev(2,i), sty_mask), sty_offset)

	if( (iand(ev(1,i),mdx_mask).eq.correct_mdx).and. &
			(iand(ev(2,i),mdy_mask).eq.correct_mdy).and. &
			(ste.eq.stx).and.(stx.eq.sty) ) then

		if( (ste.ge.0).and.(ste.lt.nc)) then
			if( channel_on(ste).eq.1) then       ! Accept any channel in list
				x(j) = iand( ev(1,i), x_mask)
				e(j) = iand( ev(0,i), e_mask)
				y(j) = iand( ev(2,i), y_mask)
				adc(j) = ste
				j = j+1
			endif
		endif
	else
		bad_xy = bad_xy+1
	endif
enddo

20 continue
	n = j
	return
end

!-------------------------------------------------------------------------------

integer function mpsys_events( argc, argv)

!DLL_EXPORT mpsys_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.19) then          ! needed # args
	mpsys_events = 1
	return
endif

call mpsys_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	 %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)) )

mpsys_events = 0
return
end

!------------------------------------------------------------

SUBROUTINE mpsys_events_b( ev,n_buffer,channel_on,nc, e,x,y,n,adc, bad_xy, &
		  xstep,xlast,xmax,step_count,xcount, toggle_adc,toggle_bit,toggle_last, &
		  ystep )

! xstep =   0 normal XY scan
!      1   X or Y step mode using toggle bit using /step_toggle
!      2   X or Y step using step_count in toggle_adc
!      3   X or Y step using /step_events (step_count in PIXE ADC)
!
! ystep =   0 X step mode
!      1   Y step mode
!
! xlast        cummulative X position
! xcount       cummulative X count
! channel_on    indicates (1) whether a channel is selected

INTEGER*4 n_buffer, n, bad_xy, xstep, xmax, xcount, toggle_adc, toggle_bit
INTEGER*4 step_count, ystep, nc
INTEGER*2 e(0:n_buffer-1), x(0:n_buffer-1), y(0:n_buffer-1), channel_on(0:nc-1)
INTEGER*2 ev(0:2,0:n_buffer-1), adc(0:n_buffer-1), toggle_last, xlast

INTEGER*4 i,j
INTEGER*2 e_mask0, e_mask, x_mask, y_mask, ste_mask, stx_mask, sty_mask, mdx_mask
INTEGER*2 mdy_mask, ste_offset, stx_offset, sty_offset
INTEGER*2 ste, stx, sty, correct_mdx, correct_mdy, one, toggle_mask, toggle

parameter ( e_mask0 = z'0FFF', x_mask = z'0FFF', y_mask = z'0FFF')
parameter ( ste_mask = z'C000', stx_mask = z'3000', sty_mask = z'3000')
parameter ( mdx_mask = z'C000', mdy_mask = z'C000')
parameter ( ste_offset = -14, stx_offset = -12, sty_offset = -12)
parameter ( correct_mdx = z'8000', correct_mdy = z'4000', one=z'0001')

! The data uses the bottom bits for ADC data (13 bits e, 12 bits
! for x,y), and the top bits to tag the ADC station and to identify X,Y.
! Various data integrity checks are made, and the
! ADC data is accessed via bit masks.
!
! Due to a problem with MicroDAS, which jams all external high-bits (as used for
! x-step toggle) across all ADC channels, the use of the high bit on station 1
! forces us to mask out this bit on all ADCs, even when not in step-mode.
! Hence the change to e_mask0 above to only 12 bits.
!
! Simlarly, if another bit is used temporarily, it needs to be DISCONNECTED when not
! in use (e.g. stereo bit #11).

e_mask = e_mask0
if(xstep.eq.1) then
	toggle_mask = ishft( one, toggle_bit)
	e_mask = iand( e_mask, (toggle_mask-one))
endif

j = 0
n = 0
if( n_buffer.lt.1) return

do i=0,n_buffer-1
	ste = ishft( iand( ev(0,i), ste_mask), ste_offset)
	stx = ishft( iand( ev(1,i), stx_mask), stx_offset)
	sty = ishft( iand( ev(2,i), sty_mask), sty_offset)

	if( (iand(ev(1,i),mdx_mask).eq.correct_mdx).and. &
		 (iand(ev(2,i),mdy_mask).eq.correct_mdy).and. &
		 (ste.eq.stx).and.(stx.eq.sty) ) then

	   if(xstep.eq.1) then                    ! toggle bit X step
		 if( ste.eq.toggle_adc) then
		  toggle = iand( ev(0,i), toggle_mask)
		  if( toggle.ne.toggle_last) then
			  xlast = xlast+1
			  if( xlast.gt.xmax) goto 20
			  toggle_last = toggle
		  endif
		 endif
	   else if(xstep.eq.2) then             ! X step_count in toggle_adc
		 if( ste.eq.toggle_adc) then
		  xcount = xcount + 1
		  if( xcount.gt.step_count) then
			  xlast = xlast + 1
			  xcount = xcount - step_count
			  if( xlast.gt.xmax) goto 20
		  endif
		 endif
	   else if(xstep.eq.3) then             ! X step_count in PIXE adc
		 if( (ste.ge.0).and.(ste.lt.nc)) then
		  if( channel_on(ste).eq.1) then       ! Advances on total count in
			  xcount = xcount + 1              ! all enabled channels
			  if( xcount.gt.step_count) then
				 xlast = xlast + 1
				 xcount = xcount - step_count
				 if( xlast.gt.xmax) goto 20
			  endif
		  endif
		 endif
	   endif
	   if( xstep.ne.0) then                 ! any X step mode
		 if( (ste.ge.0).and.(ste.lt.nc)) then
		  if( channel_on(ste).eq.1) then
			  if( ystep.eq.1) then
				 x(j) = iand( ev(1,i), x_mask)
				 y(j) = xlast
			  else
				 x(j) = xlast
				 y(j) = iand( ev(2,i), y_mask)
			  endif
			  e(j) = iand( ev(0,i), e_mask)
			  adc(j) = ste
			  j = j+1
		  endif
		 endif
	   else                           ! normal XY scan
		 if( (ste.ge.0).and.(ste.lt.nc)) then
		  if( channel_on(ste).eq.1) then       ! Accept any channel in list
			  x(j) = iand( ev(1,i), x_mask)
			  e(j) = iand( ev(0,i), e_mask)
			  y(j) = iand( ev(2,i), y_mask)
			  adc(j) = ste
			  j = j+1
		  endif
		 endif
	   endif
	else
	   bad_xy = bad_xy+1
	endif
enddo

20 continue
	n = j
	return
end

!-------------------------------------------------------------------------------

integer function xsys_events( argc, argv)

!DLL_EXPORT xsys_events
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	xsys_events = 1
	return
endif

call xsys_events_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14))  )

xsys_events = 0
return
end

!------------------------------------------------------------

SUBROUTINE xsys_events_b( ev,i,n,n_buffer, channel_on,nc,e,x,y,ste,  &
		  run_num, title_start,title_end, unknown)

INTEGER*4 n_buffer, i,n, nc, unknown, title_start, title_end, run_num
INTEGER*2 e(0:n_buffer-1), x(0:n_buffer-1), y(0:n_buffer-1)
INTEGER*2 ev(0:n_buffer-1), ste(0:n_buffer-1), channel_on(0:nc-1)

INTEGER*4 length, j
INTEGER*2 event_tag, e_mask, x_mask, y_mask, buffer_id_mask
INTEGER*2 event_id_mask, event_type_mask, id, type

parameter ( e_mask = z'1FFF', x_mask = z'0FFF', y_mask = z'0FFF')
parameter ( buffer_id_mask = z'FC00', event_id_mask = z'C000', event_type_mask = z'00FF')
parameter ( event_tag = z'8000')

j = 0
n = 0
if( i.ge.n_buffer) return

10  length = ev(i) / 2
	if( length.ge.(n_buffer-i)) length=(n_buffer-i)
	if( length.le.0) goto 20

	id = iand( ev(i+1), event_id_mask)
	type = iand( ev(i+1), event_type_mask)

	if( id.eq.event_tag) then
	   if( ((type-1).ge.0).and.((type-1).lt.nc).and.(type.lt.10)) then
		 if( channel_on(type-1).eq.1) then         ! Accept any channel in list
		  e(j) = iand( ev(i+2), e_mask)
		  x(j) = iand( ev(i+3), x_mask)
		  y(j) = iand( ev(i+4), y_mask)
		  ste(j) = type - 1
		  j = j+1
		 endif
	   else if( type.eq.254) then
		 run_num = ev(i+8)
		 title_start = i+16
		 title_end = i+length-1
	   else if( type.eq.255) then
		 goto 20
	   else
		 unknown = unknown+1
	   endif
	endif
	i = i+length
if( i.lt.n_buffer) goto 10

20  n = j
	return
	end

!-------------------------------------------------------------------------------

integer function cut_accumulate5( argc, argv)

!DLL_EXPORT cut_accumulate5
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.24) then        	  ! needed # args
	cut_accumulate5 = 1
	return
endif

call cut_accumulate5_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), &
	 %val(argv(15)), %val(argv(16)), %val(argv(17)), %val(argv(18)), &
	 %val(argv(19)), %val(argv(20)), %val(argv(21)), %val(argv(22)), &
	 %val(argv(23)), %val(argv(24)) )

cut_accumulate5 = 0
return
end

!------------------------------------------------------------

SUBROUTINE cut_accumulate5_b( x,y,e,pu,veto, n,good, image,nx,ny, nnpu,nn, error,nxe,nye, &
		 cut,type,dleft,dright, nel, multiple, stim_mean,image_count,error_count)

INTEGER*4 n,m,good, nx,ny,nxe,nye, nel, multiple(0:n-1), stim_mean, nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
INTEGER*4 image_count(0:nx-1,0:ny-1,0:nel-1),error_count(0:nxe-1,0:nye-1,0:nel-1)
INTEGER*2 x(0:n-1),y(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye, type
REAL*4 e(0:n-1), cut(0:5,0:nel-1), dleft(0:nel-1),dright(0:nel-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1)
REAL*4 error(0:nxe-1,0:nye-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30

do j=0,n-1
    if( multiple(0).eq.-1) then
	m = 1
    else
	m = multiple(j)
    endif

    if( veto(j).eq.0) then
	 if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
	    xe = x(j)/2
	    ye = y(j)/2

	    if(pu(j).eq.0) then
		do k=0,nel-1
			if( stim_mean.eq.1) then
			    if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + m*e(j)
				  image_count(x(j),y(j),k) = image_count(x(j),y(j),k) + m
				  error(xe,ye,k) = error(xe,ye,k) + m
				  error_count(xe,ye,k) = error_count(xe,ye,k) + m
			    endif
			else
			    if ( ((e(j).ge.cut(0,k)).and.(e(j).le.cut(1,k))) .and. (type.eq.1) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dleft(k) *m
				  error(xe,ye,k) = error(xe,ye,k) + dleft(k)*dleft(k) *m
			    endif

			    if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) +  m
				  error(xe,ye,k) = error(xe,ye,k) +  m
			    endif

			    if ( ((e(j).ge.cut(4,k)).and.(e(j).le.cut(5,k))) .and. ((type.eq.1).or.(type.eq.2)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dright(k) *m
				  error(xe,ye,k) = error(xe,ye,k) + dright(k)*dright(k) *m
			    endif
			endif
		enddo
	    endif

	    if(nn(x(j),y(j)).lt.(big-m)) nn(x(j),y(j)) = nn(x(j),y(j)) + m
	    if(pu(j).eq.1) then
		if(nnpu(x(j),y(j)).lt.(big-m)) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + m
	    endif
	 endif
    endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function cut_accumulate4( argc, argv)

!DLL_EXPORT cut_accumulate4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.22) then        	  ! needed # args
	cut_accumulate4 = 1
	return
endif

call cut_accumulate4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), &
	 %val(argv(15)), %val(argv(16)), %val(argv(17)), %val(argv(18)), &
	 %val(argv(19)), %val(argv(20)), %val(argv(21)), %val(argv(22)) )

cut_accumulate4 = 0
return
end

!------------------------------------------------------------

SUBROUTINE cut_accumulate4_b( x,y,e,pu,n, image,nx,ny, nnpu,nn, error,nxe,nye, &
		 cut,type,dleft,dright, nel, multiple, stim_mean,image_count,error_count)

INTEGER*4 n, nx,ny,nxe,nye, nel, multiple(0:n-1), stim_mean, nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
INTEGER*4 image_count(0:nx-1,0:ny-1,0:nel-1),error_count(0:nxe-1,0:nye-1,0:nel-1)
INTEGER*2 x(0:n-1),y(0:n-1),pu(0:n-1), xe,ye, type
REAL*4 e(0:n-1), cut(0:5,0:nel-1), dleft(0:nel-1),dright(0:nel-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1)
REAL*4 error(0:nxe-1,0:nye-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(n.le.0) return
big = 2**30 - 1
big = big + 2**30

if( multiple(0).eq.-1) then
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
		 xe = x(j)/2
		 ye = y(j)/2

		if(pu(j).eq.0) then
		 do k=0,nel-1
			if( stim_mean.eq.1) then
			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + e(j)
				  image_count(x(j),y(j),k) = image_count(x(j),y(j),k) + 1
				  error(xe,ye,k) = error(xe,ye,k) + 1.0
				  error_count(xe,ye,k) = error_count(xe,ye,k) + 1
			  endif
			else
			  if ( ((e(j).ge.cut(0,k)).and.(e(j).le.cut(1,k))) .and. (type.eq.1) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dleft(k)
				  error(xe,ye,k) = error(xe,ye,k) + dleft(k)*dleft(k)
			  endif

			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + 1.0
				  error(xe,ye,k) = error(xe,ye,k) + 1.0
			  endif

			  if ( ((e(j).ge.cut(4,k)).and.(e(j).le.cut(5,k))) .and. ((type.eq.1).or.(type.eq.2)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dright(k)
				  error(xe,ye,k) = error(xe,ye,k) + dright(k)*dright(k)
			  endif
			endif
		 enddo
		endif

		if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
		if(pu(j).eq.1) then
			if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
		endif
	   endif
	enddo
else
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
		 xe = x(j)/2
		 ye = y(j)/2

		if(pu(j).eq.0) then
		 do k=0,nel-1
			if( stim_mean.eq.1) then
			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + multiple(j)*e(j)
				  image_count(x(j),y(j),k) = image_count(x(j),y(j),k) + multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) + multiple(j)
				  error_count(xe,ye,k) = error_count(xe,ye,k) + multiple(j)
			  endif
			else
			  if ( ((e(j).ge.cut(0,k)).and.(e(j).le.cut(1,k))) .and. (type.eq.1) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dleft(k) *multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) + dleft(k)*dleft(k) *multiple(j)
			  endif

			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) +  multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) +  multiple(j)
			  endif

			  if ( ((e(j).ge.cut(4,k)).and.(e(j).le.cut(5,k))) .and. ((type.eq.1).or.(type.eq.2)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dright(k) *multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) + dright(k)*dright(k) *multiple(j)
			  endif
			endif
		 enddo
		endif

		if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
		if(pu(j).eq.1) then
			if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
		endif
	   endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function cut_accumulate3( argc, argv)

!DLL_EXPORT cut_accumulate3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.19) then          ! needed # args
	cut_accumulate3 = 1
	return
endif

call cut_accumulate3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), &
	 %val(argv(15)), %val(argv(16)), %val(argv(17)), %val(argv(18)), &
	 %val(argv(19)) )

cut_accumulate3 = 0
return
end

!------------------------------------------------------------

SUBROUTINE cut_accumulate3_b( x,y,e,n, image,nx,ny, error,nxe,nye, &
		 cut,type,dleft,dright, nel, multiple, stim_mean,image_count,error_count)

INTEGER*4 n, nx,ny,nxe,nye, nel, multiple(0:n-1), stim_mean
INTEGER*4 image_count(0:nx-1,0:ny-1,0:nel-1),error_count(0:nxe-1,0:nye-1,0:nel-1)
INTEGER*2 x(0:n-1),y(0:n-1), xe,ye, type
REAL*4 e(0:n-1), cut(0:5,0:nel-1), dleft(0:nel-1),dright(0:nel-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1)
REAL*4 error(0:nxe-1,0:nye-1,0:nel-1)

if( multiple(0).eq.-1) then
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
		 xe = x(j)/2
		 ye = y(j)/2

		 do k=0,nel-1
			if( stim_mean.eq.1) then
			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + e(j)
				  image_count(x(j),y(j),k) = image_count(x(j),y(j),k) + 1
				  error(xe,ye,k) = error(xe,ye,k) + 1.0
				  error_count(xe,ye,k) = error_count(xe,ye,k) + 1
			  endif
			else
			  if ( ((e(j).ge.cut(0,k)).and.(e(j).le.cut(1,k))) .and. (type.eq.1) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dleft(k)
				  error(xe,ye,k) = error(xe,ye,k) + dleft(k)*dleft(k)
			  endif

			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + 1.0
				  error(xe,ye,k) = error(xe,ye,k) + 1.0
			  endif

			  if ( ((e(j).ge.cut(4,k)).and.(e(j).le.cut(5,k))) .and. ((type.eq.1).or.(type.eq.2)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dright(k)
				  error(xe,ye,k) = error(xe,ye,k) + dright(k)*dright(k)
			  endif
			endif
		 enddo
	   endif
	enddo
else
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
		 xe = x(j)/2
		 ye = y(j)/2

		 do k=0,nel-1
			if( stim_mean.eq.1) then
			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + multiple(j)*e(j)
				  image_count(x(j),y(j),k) = image_count(x(j),y(j),k) + multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) + multiple(j)
				  error_count(xe,ye,k) = error_count(xe,ye,k) + multiple(j)
			  endif
			else
			  if ( ((e(j).ge.cut(0,k)).and.(e(j).le.cut(1,k))) .and. (type.eq.1) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dleft(k) *multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) + dleft(k)*dleft(k) *multiple(j)
			  endif

			  if ( (e(j).ge.cut(2,k)).and.(e(j).le.cut(3,k)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) +  multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) +  multiple(j)
			  endif

			  if ( ((e(j).ge.cut(4,k)).and.(e(j).le.cut(5,k))) .and. ((type.eq.1).or.(type.eq.2)) ) then
				  image(x(j),y(j),k) = image(x(j),y(j),k) + dright(k) *multiple(j)
				  error(xe,ye,k) = error(xe,ye,k) + dright(k)*dright(k) *multiple(j)
			  endif
			endif
		 enddo
	   endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------
! THIS DOESN'T SEEM TO BE IN USE ...

integer function cut_accumulate2( argc, argv)

!DLL_EXPORT cut_accumulate2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.15) then          ! needed # args
	cut_accumulate2 = 1
	return
endif

call cut_accumulate2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), &
	 %val(argv(15)) )

cut_accumulate2 = 0
return
end

!------------------------------------------------------------
! THIS DOESN'T SEEM TO BE IN USE ...

SUBROUTINE cut_accumulate2_b( x,y,e,n, image,nx,ny, error,nxe,nye, low,high,blow,bhigh,nel)

INTEGER*4 n,nx,ny,nxe,nye,nel
INTEGER*2 x(0:n-1),y(0:n-1),xe,ye
REAL*4 e(0:n-1),low(0:nel-1),high(0:nel-1),blow(0:nel-1),bhigh(0:nel-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1)
REAL*4 error(0:nxe-1,0:nye-1,0:nel-1)

do k=0,nel-1
	do j=0,n-1
	   if ( ((e(j).ge.low(k)).and.(e(j).le.high(k))) .or.  &
				 ((e(j).ge.blow(k)).and.(e(j).le.bhigh(k)))) then
		 if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
			  ((y(j).ge.0).and.(y(j).lt.ny)) ) then
		  if ((e(j).ge.low(k)).and.(e(j).le.high(k))) then
			  image(x(j),y(j),k) = image(x(j),y(j),k) + 1.0
		  endif
		  if ((e(j).ge.blow(k)).and.(e(j).le.bhigh(k))) then
			  image(x(j),y(j),k) = image(x(j),y(j),k) - 1.0
		  endif
		 endif
		 xe = x(j)/2
		 ye = y(j)/2
		 if( ((xe.ge.0).and.(xe.lt.nxe)).and. &
					((ye.ge.0).and.(ye.lt.nye)) ) then
		  error(xe,ye,k) = error(xe,ye,k) + 1.0
		 endif
	   endif
	enddo
enddo

return
end

!-------------------------------------------------------------------------------
! THIS DOESN'T SEEM TO BE IN USE ...

integer function cut_accumulate( argc, argv)

!DLL_EXPORT cut_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.12) then          ! needed # args
	cut_accumulate = 1
	return
endif

call cut_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), %val(argv(7)), &
	 %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), &
	 %val(argv(12)) )

cut_accumulate = 0
return
end

!------------------------------------------------------------
! THIS DOESN'T SEEM TO BE IN USE ...

SUBROUTINE cut_accumulate_b( x,y,e,n, image,nx,ny, low,high,blow,bhigh,nel)

INTEGER*4 n,nx,ny,nel
INTEGER*2 x(0:n-1),y(0:n-1)
REAL*4 e(0:n-1),low(0:nel-1),high(0:nel-1),blow(0:nel-1),bhigh(0:nel-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1)

do k=0,nel-1
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		  ((y(j).ge.0).and.(y(j).lt.ny)) ) then
		 if ((e(j).ge.low(k)).and.(e(j).le.high(k))) then
		  image(x(j),y(j),k) = image(x(j),y(j),k) + 1.0
		 endif
		 if ((e(j).ge.blow(k)).and.(e(j).le.bhigh(k))) then
		  image(x(j),y(j),k) = image(x(j),y(j),k) - 1.0
		 endif
	   endif
	enddo
enddo

return
end

!-------------------------------------------------------------------------------

integer function xanes_accumulate3( argc, argv)

!DLL_EXPORT xanes_accumulate3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	xanes_accumulate3 = 1
	return
endif

call xanes_accumulate3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)) )

xanes_accumulate3 = 0
return
end

!------------------------------------------------------------

SUBROUTINE xanes_accumulate3_b( x,y,e,n, image,nx,ny, image_error,nxe,nye, &
			  nel, matrix,size, multiple)

INTEGER*4 n, nx,ny, nxe,nye, nel,size, j,k
INTEGER*2 y(0:n-1),e(0:n-1), xe,ye
INTEGER*4 x(0:n-1), multiple(0:n-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.

if( multiple(0).eq.-1) then
	do k=0,nel-1
	   do j=0,n-1
		 if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
		  ((e(j).ge.0).and.(e(j).lt.size)) ) then

			  image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)

			  if(halve) then
				 xe = x(j)/2
				 ye = y(j)/2
			  else
				 xe = x(j)
				 ye = y(j)
			  endif
			  if( ((xe.ge.0).and.(xe.lt.nxe)).and. &
				 ((ye.ge.0).and.(ye.lt.nye)).and. &
				 ((e(j).ge.0).and.(e(j).lt.size)) ) then
				   image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k)	* matrix(e(j),k)
			  endif
		 endif
	   enddo
	enddo
else
	do k=0,nel-1
	   do j=0,n-1
		 if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
		  ((e(j).ge.0).and.(e(j).lt.size)) ) then

			  image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)*multiple(j)

			  if(halve) then
				 xe = x(j)/2
				 ye = y(j)/2
			  else
				 xe = x(j)
				 ye = y(j)
			  endif
			  if( ((xe.ge.0).and.(xe.lt.nxe)).and. &
				 ((ye.ge.0).and.(ye.lt.nye)).and. &
				 ((e(j).ge.0).and.(e(j).lt.size)) ) then
				   image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k)*matrix(e(j),k)*multiple(j)
			  endif
		 endif
	   enddo
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function maia_accumulate_dtfx2_3D( argc, argv)

!DLL_EXPORT maia_accumulate_dtfx2_3D
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.24) then       		   ! needed # args
	maia_accumulate_dtfx2_3D = 1
	return
endif

call maia_accumulate_dtfx2_3D_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)), %val(argv(23)), %val(argv(24)) )

maia_accumulate_dtfx2_3D = 0
return
end

!------------------------------------------------------------

SUBROUTINE maia_accumulate_dtfx2_3D_b( image_mode, t,x,y,z,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,zrange, dta,dtb, flux,dead,weight,dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action.
!	For older record types (w/ no BT, Flux1, etc. words) also catch PV in fx[0,*] for veto=0 real events.
!
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale, when not in 3D mode
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] (if not zero) to set Dwell in maia_dwell (can pass by ref, better than self.dwell)
!
!	Dead_fraction:
!		Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
!		Only accept good (veto=0) events.
!		Later (in da_evt) norm this using the dead_fraction_norm(/image) method, which returns dwell (ms) array.
!
!	dta, dtb	dead-time calibration (divided by number of active detector channels)

INTEGER*4 image_mode, flux_mode, n,n_fx, xcompress,ycompress, xrange,yrange,zrange, i, k64, xl,yl
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),t(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, dta,dtb, flux(0:xrange-1,0:yrange-1,0:zrange-1)
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt, df, lost, small
LOGICAL mode_3D

REAL*4 dtsum, raw, dwell_last, dwell_first
integer*4 xlast, ylast, xlastp, ylastp
common /c_maia_5/ xlast, ylast, xlastp, ylastp, dwell_last, dwell_first, dtsum, raw

if(n.le.0) return
if(n_fx.lt.4) return
small = 1.0e-6
mode_3D = (zrange.ge.2)
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)
k64 = 64*1024

do i=0,n-1
	if( image_mode.eq.1) then
	    xl = x(i)
	    if(xl.lt.0) xl=xl+k64
	    yl = y(i)
	    if(yl.lt.0) yl=yl+k64
		if((xl.ge.0).and.(xl.lt.xrange).and.(yl.ge.0).and.(yl.lt.yrange).and.(z(i).ge.0).and.(z(i).lt.zrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(xl,yl,z(i)) = fx(0,i) * flux_scale2
					endif
				else
					flux(xl,yl,z(i)) = flux(xl,yl,z(i)) + fx(0,i) * flux_scale2
				endif
				if( .not.mode_3D) then
					flux(xl,yl,1) = flux(xl,yl,1) + fx(1,i)
					flux(xl,yl,2) = flux(xl,yl,2) + fx(2,i)
				endif

				if( fx(3,i).gt.0.0) then
					dwell(xl,yl) = dwell(xl,yl) + fx(3,i)

					if((xl.eq.xlastp).and.(yl.eq.ylastp)) then
						dwell_last = dwell_last + fx(3,i)
					else
						xlastp = xl
						ylastp = yl

						if((xlast.ge.0).and.(ylast.ge.0)) then
							if(dwell_last.gt.small) then
								df = dtsum / dwell_last				! dead-fraction (for this pass)
								if(df.gt.0.95) df = 0.95
								lost = raw * df/(1.-df)				! lost counts
								dead(xlast,ylast) = dead(xlast,ylast) + lost
							endif
							weight(xlast,ylast) = weight(xlast,ylast) + raw
							xlast = -1
							ylast = -1
						endif
						dwell_last = fx(3,i)
					endif
				endif
			endif
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if((xl.eq.xlast).and.(yl.eq.ylast)) then
					dtsum = dtsum + dt							! DT in pixel pass
					raw = raw + 1.								! raw counts
				else 
					dtsum = dt								! DT in pixel pass
					raw = 1.								! raw counts
					xlast = xl
					ylast = yl
				endif

				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(xl,yl,z(i)) = fx(0,i) * flux_scale2
					endif
				endif
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in Maia device in spectrum_mode.
!		In spectrum mode, do not use weight and lost approach, just time.

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if( dt.gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + dt
				endif
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function maia_accumulate_dtfx2( argc, argv)

!DLL_EXPORT maia_accumulate_dtfx2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.23) then       		   ! needed # args
	maia_accumulate_dtfx2 = 1
	return
endif

call maia_accumulate_dtfx2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)), %val(argv(23)) )

maia_accumulate_dtfx2 = 0
return
end

!------------------------------------------------------------

SUBROUTINE maia_accumulate_dtfx2_b( image_mode, t,x,y,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,n_flux, dta,dtb, flux, dead, weight, dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action.
!	For older record types (w/ no BT, Flux1, etc. words) also catch PV in fx[0,*] for veto=0 real events.
!
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] (if not zero) to set Dwell in maia_dwell (can pass by ref, better than self.dwell)
!
!	Dead_fraction:
!		Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
!		Only accept good (veto=0) events.
!		Later (in da_evt) norm this using the dead_fraction_norm(/image) method, which returns dwell (ms) array.
!
!	dta, dtb	dead-time calibration (divided by number of active detector channels)

INTEGER*4 image_mode, flux_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i, k64, xl,yl
INTEGER*2 x(0:n-1),y(0:n-1),t(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, dta,dtb, flux(0:xrange-1,0:yrange-1,0:n_flux)
REAL*4 dead(0:xrange-1,0:yrange-1), weight(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt, df, lost, small

REAL*4 dtsum, raw, dwell_last, dwell_first
integer*4 xlast, ylast, xlastp, ylastp
common /c_maia_5/ xlast, ylast, xlastp, ylastp, dwell_last, dwell_first, dtsum, raw

if(n.le.0) return
if(n_fx.lt.4) return
small = 1.0e-6
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)
k64 = 64*1024

!  Note that 'pseudo=1' is usually the same as 'veto=1'. However, borders, etc. can be trimmed for these
!  pixels pseudo=0 and veto=1, which fail both tests below.

do i=0,n-1
	if( image_mode.eq.1) then
	    xl = x(i)
	    if(xl.lt.0) xl=xl+k64
	    yl = y(i)
	    if(yl.lt.0) yl=yl+k64
		if((xl.ge.0).and.(xl.lt.xrange).and.(yl.ge.0).and.(yl.lt.yrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then					! also not needed
						flux(xl,yl,0) = fx(0,i) * flux_scale2
					endif
				else
					flux(xl,yl,0) = flux(xl,yl,0) + fx(0,i) * flux_scale2
				endif
				flux(xl,yl,1) = flux(xl,yl,1) + fx(1,i)
				flux(xl,yl,2) = flux(xl,yl,2) + fx(2,i)

				if( fx(3,i).gt.0.0) then						! this looks wrong!
					dwell(xl,yl) = dwell(xl,yl) + fx(3,i)

					if((xl.eq.xlastp).and.(yl.eq.ylastp)) then
						dwell_last = dwell_last + fx(3,i)
					else
						xlastp = xl
						ylastp = yl

						if((xlast.ge.0).and.(ylast.ge.0)) then
							if(dwell_last.gt.small) then
								df = dtsum / dwell_last				! dead-fraction (for this pass)
								if(df.gt.0.95) df = 0.95
								lost = raw * df/(1.-df)				! lost counts
								dead(xlast,ylast) = dead(xlast,ylast) + lost
							endif
							weight(xlast,ylast) = weight(xlast,ylast) + raw
							xlast = -1
							ylast = -1
						endif
						dwell_last = fx(3,i)
					endif
				endif
			endif
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if((xl.eq.xlast).and.(yl.eq.ylast)) then
					dtsum = dtsum + dt							! DT in pixel pass
					raw = raw + 1.								! raw counts
				else 
					dtsum = dt								! DT in pixel pass
					raw = 1.								! raw counts
					xlast = xl
					ylast = yl
				endif

				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then					! also not needed
						flux(xl,yl,0) = fx(0,i) * flux_scale2
					endif
				endif
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in Maia device in spectrum_mode.
!		In spectrum mode, do not use weight and lost approach, just time.

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if( dt.gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + dt
				endif
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function maia_accumulate_dtfx_3D( argc, argv)

!DLL_EXPORT maia_accumulate_dtfx_3D
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.23) then       		   ! needed # args
	maia_accumulate_dtfx_3D = 1
	return
endif

call maia_accumulate_dtfx_3D_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)), %val(argv(23)) )

maia_accumulate_dtfx_3D = 0
return
end

!------------------------------------------------------------

SUBROUTINE maia_accumulate_dtfx_3D_b( image_mode, t,x,y,z,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,zrange, dta,dtb, flux,dead,dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action.
!	For older record types (w/ no BT, Flux1, etc. words) also catch PV in fx[0,*] for veto=0 real events.
!
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale, when not in 3D mode
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] (if not zero) to set Dwell in maia_dwell (can pass by ref, better than self.dwell)
!
!	Dead_fraction:
!		Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
!		Only accept good (veto=0) events.
!		Later (in da_evt) norm this using the dead_fraction_norm(/image) method, which returns dwell (ms) array.

INTEGER*4 image_mode, flux_mode, n,n_fx, xcompress,ycompress, xrange,yrange,zrange, i
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),t(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, dta,dtb, flux(0:xrange-1,0:yrange-1,0:zrange-1)
REAL*4 dead(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt
LOGICAL mode_3D

if(n.le.0) return
if(n_fx.lt.4) return
mode_3D = (zrange.ge.2)
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange).and.(z(i).ge.0).and.(z(i).lt.zrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),z(i)) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),z(i)) = flux(x(i),y(i),z(i)) + fx(0,i) * flux_scale2
				endif
				if( .not.mode_3D) then
					flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
					flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				endif
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
				endif
			endif
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if( dt.gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + dt
				endif
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),z(i)) = fx(0,i) * flux_scale2
					endif
				endif
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in Maia device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if( dt.gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + dt
				endif
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function maia_accumulate_dtfx( argc, argv)

!DLL_EXPORT maia_accumulate_dtfx
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.22) then       		   ! needed # args
	maia_accumulate_dtfx = 1
	return
endif

call maia_accumulate_dtfx_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)) )

maia_accumulate_dtfx = 0
return
end

!------------------------------------------------------------

SUBROUTINE maia_accumulate_dtfx_b( image_mode, t,x,y,ste,veto,pseudo,fx,n,n_fx, flux_mode, flux_scale, &
		 				xcompress,ycompress, xrange,yrange,n_flux, dta,dtb, flux, dead, dwell)

!	Accumulate flux (including attributes), dead_fraction and dwell ...
!	Process only pseudo=1 'events' for flux, dwell. Use flux_mode to select action.
!	For older record types (w/ no BT, Flux1, etc. words) also catch PV in fx[0,*] for veto=0 real events.
!
!		0	Epics PV as a rate, so set the flux in a pixel to fx[0,*], if it is not zero
!				scaled by: nsls_flux_scale * float(xcompress)*float(ycompress)
!		1,2	H/W flux counter used and accumulated in fx[0,*] in Fortran, so accumulate fx[0,*] 
!				here in flux[*,*,0] scaled by: nsls_flux_scale
!				
!			Use FC0 in fx[1,*] to accumulate into flux[*,*,1]
!			Use FC1 in fx[2,*] to accumulate into flux[*,*,2]
!			
!		* Note that nsls_flux_scale is already scaled by fixed dwell in setup method for Epics case.
!
!	Dwell:
!		Use fx[3,*] (if not zero) to set Dwell in maia_dwell (can pass by ref, better than self.dwell)
!
!	Dead_fraction:
!		Accumulate 't' into dead_fraction (image array), using cal to make it in ms units.
!		Only accept good (veto=0) events.
!		Later (in da_evt) norm this using the dead_fraction_norm(/image) method, which returns dwell (ms) array.

INTEGER*4 image_mode, flux_mode, n,n_fx,n_flux, xcompress,ycompress, xrange,yrange, i
INTEGER*2 x(0:n-1),y(0:n-1),t(0:n-1),veto(0:n-1),ste(0:n-1),pseudo(0:n-1)
REAL*4 fx(0:n_fx-1,0:n-1), flux_scale,flux_scale2, dta,dtb, flux(0:xrange-1,0:yrange-1,0:n_flux)
REAL*4 dead(0:xrange-1,0:yrange-1), dwell(0:xrange-1,0:yrange-1), dt

if(n.le.0) return
if(n_fx.lt.4) return
flux_scale2 = flux_scale
if( flux_mode.eq.0) flux_scale2 = flux_scale2 * float(xcompress)*float(ycompress)

!  Note that 'pseudo=1' is usually the same as 'veto=1'. However, borders, etc. can be trimmedfor these
!  pixels pseudo=0 and veto=1, which fail both tests below.

do i=0,n-1
	if( image_mode.eq.1) then
		if((x(i).ge.0).and.(x(i).lt.xrange).and.(y(i).ge.0).and.(y(i).lt.yrange)) then
			if(pseudo(i).eq.1) then
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),0) = fx(0,i) * flux_scale2
					endif
				else
					flux(x(i),y(i),0) = flux(x(i),y(i),0) + fx(0,i) * flux_scale2
				endif
				flux(x(i),y(i),1) = flux(x(i),y(i),1) + fx(1,i)
				flux(x(i),y(i),2) = flux(x(i),y(i),2) + fx(2,i)
				if( fx(3,i).gt.0.0) then
					dwell(x(i),y(i)) = dwell(x(i),y(i)) + fx(3,i)
				endif
			endif
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if( dt.gt.0.0) then
					dead(x(i),y(i)) = dead(x(i),y(i)) + dt
				endif
				if( flux_mode.eq.0) then
					if( fx(0,i).gt.0.0) then
						flux(x(i),y(i),0) = fx(0,i) * flux_scale2
					endif
				endif
			endif
		endif
	else
!		Don't do flux or dwell here. These are done in Maia device in spectrum_mode

		if((ste(i).ge.0).and.(ste(i).lt.xrange)) then
			if( veto(i).eq.0) then
				dt = dta * t(i) + dtb
				if( dt.gt.0.0) then
					dead(ste(i),0) = dead(ste(i),0) + dt
				endif
			endif
		endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function da_build_mpda_table( argc, argv)

!DLL_EXPORT da_build_mpda_table
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.7) then       		   ! needed # args
	da_build_mpda_table= 1
	return
endif

call da_build_mpda_table_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)) )

da_build_mpda_table= 0
return
end

!------------------------------------------------------------

SUBROUTINE da_build_mpda_table_b( table, spec, phase, nx, ny, n_comp, n_energy)

REAL*4 table(0:n_energy-1,0:n_comp-1,0:nx-1,0:ny-1), spec(0:n_energy-1,0:n_comp-1), phase(0:nx-1,0:ny-1,0:n_comp-1)
INTEGER*4 n_energy, n_comp, nx, ny
INTEGER*4 ix, iy, j, k
REAL*4 small, sum

small = 1.0e-6			! sets smallest peaks in 'spec' visible

do ix=0,nx-1
	do iy=0,ny-1
		do k=0,n_energy-1
			sum = 0.
			do j=0,n_comp-1
				table(k,j,ix,iy) = spec(k,j) * phase(ix,iy,j)
				sum = sum + table(k,j,ix,iy)
			enddo
			if(sum.gt.small) then
				do j=0,n_comp-1
					table(k,j,ix,iy) = table(k,j,ix,iy) / sum
				enddo
			endif
		enddo
	enddo
enddo

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate_tomo( argc, argv)

!DLL_EXPORT da_accumulate_tomo
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.22) then       		   ! needed # args
	da_accumulate_tomo = 1
	return
endif

call da_accumulate_tomo_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), %val(argv(22)) )

da_accumulate_tomo = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate_tomo_b( x,y,z,e,pu,veto, n,good, image,nx,ny,nz, nnpu,nn, image_error,nxe,nye,nze, &
			  nel, matrix,size, multiple)

INTEGER*4 n,nx,ny,nz,nxe,nye,nze,nel,size, big, good
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1,0:nz-1), image_error(0:nxe-1,0:nye-1,0:nel-1,0:nze-1)
REAL*4 matrix(0:size-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30

if( multiple(0).eq.-1) then
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)).and.((z(j).ge.0).and.(z(j).lt.nz)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
			    do k=0,nel-1
					if((e(j).ge.0).and.(e(j).lt.size)) then
		
					    image(x(j),y(j),k,z(j)) = image(x(j),y(j),k,z(j)) + matrix(e(j),k)
		
					    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
							image_error(xe,ye,k,z(j)) = image_error(xe,ye,k,z(j)) + matrix(e(j),k) * matrix(e(j),k)
					    endif
					endif
			    enddo
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	  endif
	enddo
else
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)).and.((z(j).ge.0).and.(z(j).lt.nz)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
			    do k=0,nel-1
					if((e(j).ge.0).and.(e(j).lt.size)) then
		
					    image(x(j),y(j),k,z(j)) = image(x(j),y(j),k,z(j)) + matrix(e(j),k)*multiple(j)
		
					    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
						image_error(xe,ye,k,z(j)) = image_error(xe,ye,k,z(j)) + matrix(e(j),k) * matrix(e(j),k)*multiple(j)
					    endif
					endif
			    enddo
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	  endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate_stack( argc, argv)

!DLL_EXPORT da_accumulate_stack
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.24) then       		   ! needed # args
	da_accumulate_stack = 1
	return
endif

call da_accumulate_stack_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)), %val(argv(23)), %val(argv(24)) )

da_accumulate_stack = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate_stack_b( x,y,z,z2,e,pu,veto, n,good, image,nx,ny,nz, nnpu,nn, image_error,nxe,nye,nze, &
			  matrix,size,nee, multiple, compress)

! If 'compress' = 1, then use 'z2' instead of 'z' for 'image' Z/E axis.

INTEGER*4 n,nx,ny,nz,nxe,nye,nze,nee,size, big,good, compress
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),z2(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye,zt
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nz-1), image_error(0:nxe-1,0:nye-1,0:nze-1)
REAL*4 matrix(0:size-1,0:nee-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30

if( multiple(0).eq.-1) then
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    zt = z(j)
	    if(compress.eq.1) zt = z2(j)

	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)).and. &
					((zt.ge.0).and.(zt.lt.nz)).and.((z2(j).ge.0).and.(z2(j).lt.nee)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
				if((e(j).ge.0).and.(e(j).lt.size)) then
		
				    image(x(j),y(j),zt) = image(x(j),y(j),zt) + matrix(e(j),z2(j))
		
				    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
						image_error(xe,ye,zt) = image_error(xe,ye,zt) + matrix(e(j),z2(j)) * matrix(e(j),z2(j))
				    endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	  endif
	enddo
else
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    zt = z(j)
	    if(compress.eq.1) zt = z2(j)

	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)).and. &
					((zt.ge.0).and.(zt.lt.nz)).and.((z2(j).ge.0).and.(z2(j).lt.nee)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
				if((e(j).ge.0).and.(e(j).lt.size)) then
		
				    image(x(j),y(j),zt) = image(x(j),y(j),zt) + matrix(e(j),z2(j)) * multiple(j)
		
				    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
					image_error(xe,ye,zt) = image_error(xe,ye,zt) + matrix(e(j),z2(j)) * matrix(e(j),z2(j)) * multiple(j)
				    endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	  endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate_cube( argc, argv)

!DLL_EXPORT da_accumulate_cube
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then       		   ! needed # args
	da_accumulate_cube = 1
	return
endif

call da_accumulate_cube_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)) )

da_accumulate_cube = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate_cube_b( x,y,z,pu,veto, n,good, image,nx,ny,nz, nnpu,nn, multiple)

INTEGER*4 n,nx,ny,nz,size, big, good, k64, xl,yl
INTEGER*2 x(0:n-1),y(0:n-1),z(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nz-1)
LOGICAL halve

if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
k64 = 64*1024

if( multiple(0).eq.-1) then
    do j=0,n-1
	if( veto(j).eq.0) then 
	    xl = x(j)
	    if(xl.lt.0) xl=xl+k64
	    yl = y(j)
	    if(yl.lt.0) yl=yl+k64

	    if( ((xl.ge.0).and.(xl.lt.nx)).and.((yl.ge.0).and.(yl.lt.ny)).and.((z(j).ge.0).and.(z(j).lt.nz)) ) then
		if(pu(j).eq.0) then
		    image(xl,yl,z(j)) = image(xl,yl,z(j)) + 1
		else
		    if(nnpu(xl,yl).lt.big) nnpu(xl,yl) = nnpu(xl,yl) + 1
		endif
		if(nn(xl,yl).lt.big) nn(xl,yl) = nn(xl,yl) + 1
	    endif
	endif
    enddo
else
    do j=0,n-1
	if( veto(j).eq.0) then 
	    xl = x(j)
	    if(xl.lt.0) xl=xl+k64
	    yl = y(j)
	    if(yl.lt.0) yl=yl+k64

	    if( ((xl.ge.0).and.(xl.lt.nx)).and.((yl.ge.0).and.(yl.lt.ny)).and.((z(j).ge.0).and.(z(j).lt.nz)) ) then
		if(pu(j).eq.0) then
		    image(xl,yl,z(j)) = image(xl,yl,z(j)) + multiple(j)
		else
		    if(nnpu(xl,yl).lt.big) nnpu(xl,yl) = nnpu(xl,yl) + multiple(j)
		endif
		if(nn(xl,yl).lt.big) nn(xl,yl) = nn(xl,yl) + multiple(j)
	    endif
	endif
    enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate11( argc, argv)

!DLL_EXPORT da_accumulate11
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.22) then       		   ! needed # args
	da_accumulate11 = 1
	return
endif

call da_accumulate11_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)) )

da_accumulate11 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate11_b( x,y,e,pu,veto,z, n,good, image,nx,ny, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size,n_planes,n_energy, multiple)

INTEGER*4 n,nx,ny,nel,size, big, good, n_energy,n_planes, k64, xl,yl, xe,ye
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), z(0:n-1)
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:n_planes-1,0:n_energy-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
k64 = 64*1024

if( multiple(0).eq.-1) then
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    xl = x(j)
	    if(xl.lt.0) xl=xl+k64
	    yl = y(j)
	    if(yl.lt.0) yl=yl+k64
	    if( ((xl.ge.0).and.(xl.lt.nx)).and.((yl.ge.0).and.(yl.lt.ny)).and.((z(j).ge.0).and.(z(j).lt.n_energy)) ) then
			if(halve) then
			    xe = xl/2
			    ye = yl/2
			else
			    xe = xl
			    ye = yl
			endif
	
			if(pu(j).eq.0) then
			    do k=0,nel-1
				if((e(j).ge.0).and.(e(j).lt.size).and.(k.lt.n_planes)) then
		
				    image(xl,yl,k) = image(xl,yl,k) + matrix(e(j),k,z(j))
		
				    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
					image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k,z(j)) * matrix(e(j),k,z(j))
				    endif
				endif
			    enddo
			endif
	
			if(nn(xl,yl).lt.big) nn(xl,yl) = nn(xl,yl) + 1
			if(pu(j).eq.1) then
				if(nnpu(xl,yl).lt.big) nnpu(xl,yl) = nnpu(xl,yl) + 1
			endif
		endif
	  endif
	enddo
else
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    xl = x(j)
	    if(xl.lt.0) xl=xl+k64
	    yl = y(j)
	    if(yl.lt.0) yl=yl+k64
	    if( ((xl.ge.0).and.(xl.lt.nx)).and.((yl.ge.0).and.(yl.lt.ny)).and.((z(j).ge.0).and.(z(j).lt.n_energy)) ) then
			if(halve) then
			    xe = xl/2
			    ye = yl/2
			else
			    xe = xl
			    ye = yl
			endif
	
			if(pu(j).eq.0) then
			    do k=0,nel-1
				if((e(j).ge.0).and.(e(j).lt.size).and.(k.lt.n_planes)) then
		
				    image(xl,yl,k) = image(xl,yl,k) + matrix(e(j),k,z(j)) *multiple(j)
		
				    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
					image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k,z(j)) * matrix(e(j),k,z(j)) *multiple(j)
				    endif
				endif
			    enddo
			endif
	
			if(nn(xl,yl).lt.(big-multiple(j))) nn(xl,yl) = nn(xl,yl) + multiple(j)
			if(pu(j).eq.1) then
			    if(nnpu(xl,yl).lt.(big-multiple(j))) nnpu(xl,yl) = nnpu(xl,yl) + multiple(j)
			endif
		endif
	  endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate10( argc, argv)

!DLL_EXPORT da_accumulate10
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.25) then       		   ! needed # args
	da_accumulate10 = 1
	return
endif

call da_accumulate10_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)), &
	 %val(argv(22)), %val(argv(23)), %val(argv(24)), %val(argv(25)) )

da_accumulate10 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate10_b( x,y,e,pu,veto, n,good, image,nx,ny,nel2, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size, phase,ncomp, inv_yield, invy, fny, multiple)

INTEGER*4 i,j,k,n,nx,ny,nel,nel2,size,ncomp, big, good, k64, xl,yl, xe,ye
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel2-1), image_error(0:nxe-1,0:nye-1,0:nel2-1)
REAL*4 inv_yield(0:nel-1,0:ncomp-1), invy(0:nxe-1,0:nye-1,0:nel-1), fny(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1,0:ncomp-1), phase(0:nx-1,0:ny-1,0:ncomp-1), small, t, te, ty, sum
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
small = 1.0e-10
k64 = 64*1024

if( multiple(0).eq.-1) then
	do j=0,n-1
	    if( veto(j).eq.0) then 
		xl = x(j)
		if(xl.lt.0) xl=xl+k64
		yl = y(j)
		if(yl.lt.0) yl=yl+k64
		if( ((xl.ge.0).and.(xl.lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(halve) then
			    xe = xl/2
			    ye = y(j)/2
			else
			    xe = xl
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
				if((e(j).ge.0).and.(e(j).lt.size)) then
					sum = 0.
					do i=0,ncomp-1
						sum = sum + phase(xl,y(j),i)
					enddo
					if(sum.gt.small) then
						do k=0,nel-1
							t = 0.
							te = 0.
							ty = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * phase(xl,y(j),i)
			
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + matrix(e(j),k,i)*matrix(e(j),k,i) * phase(xl,y(j),i)
									if( inv_yield(k,i).gt.small) then
										ty = ty + inv_yield(k,i) * phase(xl,y(j),i)
									endif
								endif
							enddo
							image(xl,y(j),k) = image(xl,y(j),k) + t/sum
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te/sum
								fny(xe,ye,k) = fny(xe,ye,k) + sum
								invy(xe,ye,k) = invy(xe,ye,k) + ty
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(xl,y(j)).lt.big) nn(xl,y(j)) = nn(xl,y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(xl,y(j)).lt.big) nnpu(xl,y(j)) = nnpu(xl,y(j)) + 1
			endif
		endif
	    endif
	enddo
else
	do j=0,n-1
	    if( veto(j).eq.0) then 
		xl = x(j)
		if(xl.lt.0) xl=xl+k64
		yl = y(j)
		if(yl.lt.0) yl=yl+k64
		if( ((xl.ge.0).and.(xl.lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(halve) then
			    xe = xl/2
			    ye = y(j)/2
			else
			    xe = xl
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
				if((e(j).ge.0).and.(e(j).lt.size)) then
					sum = 0.
					do i=0,ncomp-1
						sum = sum + phase(xl,y(j),i)
					enddo
					if(sum.gt.small) then
						do k=0,nel-1
							t = 0.
							te = 0.
							ty = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * phase(xl,y(j),i)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + (matrix(e(j),k,i) * matrix(e(j),k,i)) * phase(xl,y(j),i)
									if( inv_yield(k,i).gt.small) then
										ty = ty + inv_yield(k,i) * phase(xl,y(j),i)
									endif
								endif
							enddo
							image(xl,y(j),k) = image(xl,y(j),k) + (t/sum) * multiple(j)
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + (te/sum) * multiple(j)
								fny(xe,ye,k) = fny(xe,ye,k) + sum * multiple(j)
								invy(xe,ye,k) = invy(xe,ye,k) + ty * multiple(j)
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(xl,y(j)).lt.(big-multiple(j))) nn(xl,y(j)) = nn(xl,y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(xl,y(j)).lt.(big-multiple(j))) nnpu(xl,y(j)) = nnpu(xl,y(j)) + multiple(j)
			endif
		endif
	    endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate9( argc, argv)

!DLL_EXPORT da_accumulate9
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.31) then       		   ! needed # args
	da_accumulate9 = 1
	return
endif

call da_accumulate9_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), %val(argv(25)), %val(argv(26)), %val(argv(27)), &
	 %val(argv(28)), %val(argv(29)), %val(argv(30)), %val(argv(31)) )

da_accumulate9 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate9_b( x,y,e,pu,veto, n,good, image,nx,ny,nel2, nnpu,nn, image_error,nxe,nye, &
			nel, matrix,size, table, q, net, ncomp, nxt, nyt, inv_yield, invy, fny, compress, multiple, &
			phase_weight)

! Assumes that Table is normalized to total one for each e,x,y
! NOTE: This 'compress' is additional compress for Table (x,y may have already been compressed)

INTEGER*4 i,j,k,n,nx,ny,nxe,nye,nel,nel2,size,ncomp,net,nxt,nyt, big, good, compress, q1
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye, xt,yt
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1), q(0:size-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel2-1), image_error(0:nxe-1,0:nye-1,0:nel2-1)
REAL*4 matrix(0:size-1,0:nel-1,0:ncomp-1), table(0:net-1,0:ncomp-1,0:nxt-1,0:nyt-1)
REAL*4 inv_yield(0:nel-1,0:ncomp-1), invy(0:nxe-1,0:nye-1,0:nel-1), fny(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 small, t, te, ty, w, sum, phase_weight(0:nx-1,0:ny-1,0:ncomp-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
small = 1.0e-10

if( multiple(0).eq.-1) then
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(pu(j).eq.0) then
				if(halve) then
				    xe = x(j)/2
				    ye = y(j)/2
				else
				    xe = x(j)
				    ye = y(j)
				endif
				xt = x(j) / compress
				yt = y(j) / compress
	
				if((e(j).ge.0).and.(e(j).lt.size)) then
					q1 = q(e(j))
					if( ((q1.ge.0).and.(q1.lt.net)) .and. ((xt.ge.0).and.(xt.lt.nxt).and.(yt.ge.0).and.(yt.lt.nyt)) ) then
						do k=0,nel-1
							t = 0.
							te = 0.
							ty = 0.
							do i=0,ncomp-1
								w = matrix(e(j),k,i) * table(q1,i,xt,yt)
								t = t + w
								phase_weight(x(j),y(j),i) = phase_weight(x(j),y(j),i) + w
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + w * matrix(e(j),k,i)
									if( inv_yield(k,i).gt.small) then
										ty = ty + inv_yield(k,i) * table(q1,i,xt,yt)
										fny(xe,ye,k) = fny(xe,ye,k) + table(q1,i,xt,yt)
									endif
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te
								invy(xe,ye,k) = invy(xe,ye,k) + ty
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	    endif
	enddo
else
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(pu(j).eq.0) then
				if(halve) then
				    xe = x(j)/2
				    ye = y(j)/2
				else
				    xe = x(j)
				    ye = y(j)
				endif
				xt = x(j) / compress
				yt = y(j) / compress
	
				if((e(j).ge.0).and.(e(j).lt.size)) then
					q1 = q(e(j))
					if( ((q1.ge.0).and.(q1.lt.net)) .and. ((xt.ge.0).and.(xt.lt.nxt).and.(yt.ge.0).and.(yt.lt.nyt)) ) then
						do k=0,nel-1
							t = 0.
							te = 0.
							ty = 0.
							do i=0,ncomp-1
								w = matrix(e(j),k,i) * table(q1,i,xt,yt)
								t = t + w
								phase_weight(x(j),y(j),i) = phase_weight(x(j),y(j),i) + w * multiple(j)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + w * matrix(e(j),k,i)
									if( inv_yield(k,i).gt.small) then
										ty = ty + inv_yield(k,i) * table(q1,i,xt,yt)
										fny(xe,ye,k) = fny(xe,ye,k) + multiple(j)*table(q1,i,xt,yt)
									endif
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t * multiple(j)
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te * multiple(j)
								invy(xe,ye,k) = invy(xe,ye,k) + ty * multiple(j)
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	    endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate8( argc, argv)

!DLL_EXPORT da_accumulate8
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.30) then       		   ! needed # args
	da_accumulate8 = 1
	return
endif

call da_accumulate8_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), %val(argv(25)), %val(argv(26)), %val(argv(27)), &
	 %val(argv(28)), %val(argv(29)), %val(argv(30)) )

da_accumulate8 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate8_b( x,y,e,pu,veto, n,good, image,nx,ny,nel2, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size, table, q, net, ncomp, nxt, nyt, inv_yield, invy, fny, compress, multiple)

! Assumes that Table is normalized to total one for each e,x,y
! NOTE: This 'compress' is additional compress for Table (x,y may have already been compressed)

INTEGER*4 i,j,k,n,nx,ny,nxe,nye,nel,nel2,size,ncomp,net,nxt,nyt, big, good, compress, q1
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye, xt,yt
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1), q(0:size-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel2-1), image_error(0:nxe-1,0:nye-1,0:nel2-1)
REAL*4 matrix(0:size-1,0:nel-1,0:ncomp-1), table(0:net-1,0:ncomp-1,0:nxt-1,0:nyt-1)
REAL*4 inv_yield(0:nel-1,0:ncomp-1), invy(0:nxe-1,0:nye-1,0:nel-1), fny(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 small, t, te, ty, sum
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
small = 1.0e-10

if( multiple(0).eq.-1) then
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(pu(j).eq.0) then
				if(halve) then
				    xe = x(j)/2
				    ye = y(j)/2
				else
				    xe = x(j)
				    ye = y(j)
				endif
				xt = x(j) / compress
				yt = y(j) / compress
	
				if((e(j).ge.0).and.(e(j).lt.size)) then
					q1 = q(e(j))
					if( ((q1.ge.0).and.(q1.lt.net)) .and. ((xt.ge.0).and.(xt.lt.nxt).and.(yt.ge.0).and.(yt.lt.nyt)) ) then
						do k=0,nel-1
							t = 0.
							te = 0.
							ty = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * table(q1,i,xt,yt)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + matrix(e(j),k,i)*matrix(e(j),k,i) * table(q1,i,xt,yt)

									if( inv_yield(k,i).gt.small) then
										ty = ty + inv_yield(k,i) * table(q1,i,xt,yt)
										fny(xe,ye,k) = fny(xe,ye,k) + table(q1,i,xt,yt)
									endif
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te
								invy(xe,ye,k) = invy(xe,ye,k) + ty
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	    endif
	enddo
else
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(pu(j).eq.0) then
				if(halve) then
				    xe = x(j)/2
				    ye = y(j)/2
				else
				    xe = x(j)
				    ye = y(j)
				endif
				xt = x(j) / compress
				yt = y(j) / compress
	
				if((e(j).ge.0).and.(e(j).lt.size)) then
					q1 = q(e(j))
					if( ((q1.ge.0).and.(q1.lt.net)) .and. ((xt.ge.0).and.(xt.lt.nxt).and.(yt.ge.0).and.(yt.lt.nyt)) ) then
						do k=0,nel-1
							t = 0.
							te = 0.
							ty = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * table(q1,i,xt,yt)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + (matrix(e(j),k,i) * matrix(e(j),k,i)) * table(q1,i,xt,yt)

									if( inv_yield(k,i).gt.small) then
										ty = ty + inv_yield(k,i) * table(q1,i,xt,yt)
										fny(xe,ye,k) = fny(xe,ye,k) + multiple(j)*table(q1,i,xt,yt)
									endif
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t * multiple(j)
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te * multiple(j)
								invy(xe,ye,k) = invy(xe,ye,k) + ty * multiple(j)
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	    endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate7( argc, argv)

!DLL_EXPORT da_accumulate7
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.27) then       		   ! needed # args
	da_accumulate7 = 1
	return
endif

call da_accumulate7_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), %val(argv(25)), %val(argv(26)), %val(argv(27)) )

da_accumulate7 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate7_b( x,y,e,pu,veto, n,good, image,nx,ny,nel2, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size, table, q, net, ncomp, nxt, nyt, compress, multiple)

INTEGER*4 i,j,k,n,nx,ny,nxe,nye,nel,nel2,size,ncomp,net,nxt,nyt, big, good, compress, q1
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye, xt,yt
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1), q(0:size-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel2-1), image_error(0:nxe-1,0:nye-1,0:nel2-1)
REAL*4 matrix(0:size-1,0:nel-1,0:ncomp-1), table(0:net-1,0:ncomp-1,0:nxt-1,0:nyt-1), small, t, te, sum
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
small = 1.0e-10

if( multiple(0).eq.-1) then
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(pu(j).eq.0) then
				if(halve) then
				    xe = x(j)/2
				    ye = y(j)/2
				else
				    xe = x(j)
				    ye = y(j)
				endif
				xt = x(j) / compress
				yt = y(j) / compress
	
				if((e(j).ge.0).and.(e(j).lt.size)) then
					q1 = q(e(j))
					if( ((q1.ge.0).and.(q1.lt.net)) .and. ((xt.ge.0).and.(xt.lt.nxt).and.(yt.ge.0).and.(yt.lt.nyt)) ) then
						do k=0,nel-1
							t = 0.
							te = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * table(q1,i,xt,yt)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + matrix(e(j),k,i)*matrix(e(j),k,i) * table(q1,i,xt,yt)
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	    endif
	enddo
else
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(pu(j).eq.0) then
				if(halve) then
				    xe = x(j)/2
				    ye = y(j)/2
				else
				    xe = x(j)
				    ye = y(j)
				endif
				xt = x(j) / compress
				yt = y(j) / compress
	
				if((e(j).ge.0).and.(e(j).lt.size)) then
					q1 = q(e(j))
					if( ((q1.ge.0).and.(q1.lt.net)) .and. ((xt.ge.0).and.(xt.lt.nxt).and.(yt.ge.0).and.(yt.lt.nyt)) ) then
						do k=0,nel-1
							t = 0.
							te = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * table(q1,i,xt,yt)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + (matrix(e(j),k,i) * matrix(e(j),k,i)) * table(q1,i,xt,yt)
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t * multiple(j)
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te * multiple(j)
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	    endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate6( argc, argv)

!DLL_EXPORT da_accumulate6
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.21) then       		   ! needed # args
	da_accumulate6 = 1
	return
endif

call da_accumulate6_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)), %val(argv(20)), %val(argv(21)) )

da_accumulate6 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate6_b( x,y,e,pu,veto, n,good, image,nx,ny, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size, phase,ncomp, multiple)

INTEGER*4 i,j,k,n,nx,ny,nel,size,ncomp, big, good
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1,0:ncomp-1), phase(0:nx-1,0:ny-1,0:ncomp-1), small, t, te, sum
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30
small = 1.0e-10

if( multiple(0).eq.-1) then
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
				if((e(j).ge.0).and.(e(j).lt.size)) then
					sum = 0.
					do i=0,ncomp-1
						sum = sum + phase(x(j),y(j),i)
					enddo
					if(sum.gt.small) then
						do k=0,nel-1
							t = 0.
							te = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * phase(x(j),y(j),i)
			
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + matrix(e(j),k,i)*matrix(e(j),k,i) * phase(x(j),y(j),i)
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + t/sum
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + te/sum
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	    endif
	enddo
else
	do j=0,n-1
	    if( veto(j).eq.0) then 
		if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
				if((e(j).ge.0).and.(e(j).lt.size)) then
					sum = 0.
					do i=0,ncomp-1
						sum = sum + phase(x(j),y(j),i)
					enddo
					if(sum.gt.small) then
						do k=0,nel-1
							t = 0.
							te = 0.
							do i=0,ncomp-1
								t = t + matrix(e(j),k,i) * phase(x(j),y(j),i)
		
								if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
									te = te + (matrix(e(j),k,i) * matrix(e(j),k,i)) * phase(x(j),y(j),i)
								endif
							enddo
							image(x(j),y(j),k) = image(x(j),y(j),k) + (t/sum) * multiple(j)
							if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
								image_error(xe,ye,k) = image_error(xe,ye,k) + (te/sum) * multiple(j)
							endif
						enddo
					endif
				endif
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	    endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate5( argc, argv)

!DLL_EXPORT da_accumulate5
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.19) then       		   ! needed # args
	da_accumulate5 = 1
	return
endif

call da_accumulate5_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)) )

da_accumulate5 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate5_b( x,y,e,pu,veto, n,good, image,nx,ny, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size, multiple)

INTEGER*4 n,nx,ny,nel,size, big, good
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1), xe,ye
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(good.le.0) return
big = 2**30 - 1
big = big + 2**30

if( multiple(0).eq.-1) then
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
			    do k=0,nel-1
					if((e(j).ge.0).and.(e(j).lt.size)) then
		
					    image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)
		
					    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
							image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k) * matrix(e(j),k)
					    endif
					endif
			    enddo
			endif
	
			if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
			endif
		endif
	  endif
	enddo
else
	do j=0,n-1
	  if( veto(j).eq.0) then 
	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and.((y(j).ge.0).and.(y(j).lt.ny)) ) then
			if(halve) then
			    xe = x(j)/2
			    ye = y(j)/2
			else
			    xe = x(j)
			    ye = y(j)
			endif
	
			if(pu(j).eq.0) then
			    do k=0,nel-1
					if((e(j).ge.0).and.(e(j).lt.size)) then
		
					    image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)*multiple(j)
		
					    if( ((xe.ge.0).and.(xe.lt.nxe)).and. ((ye.ge.0).and.(ye.lt.nye)) ) then
						image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k) * matrix(e(j),k)*multiple(j)
					    endif
					endif
			    enddo
			endif
	
			if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
			if(pu(j).eq.1) then
				if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
			endif
		endif
	  endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate4( argc, argv)

!DLL_EXPORT da_accumulate4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.17) then       		   ! needed # args
	da_accumulate4 = 1
	return
endif

call da_accumulate4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)) )

da_accumulate4 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate4_b( x,y,e,pu,n, image,nx,ny, nnpu,nn, image_error,nxe,nye, &
			  nel, matrix,size, multiple)

INTEGER*4 n,nx,ny,nel,size, big
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1), xe,ye
INTEGER*4 multiple(0:n-1), nnpu(0:nx-1,0:ny-1), nn(0:nx-1,0:ny-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
if(n.le.0) return
big = 2**30 - 1
big = big + 2**30

if( multiple(0).eq.-1) then
	do j=0,n-1
	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and. ((y(j).ge.0).and.(y(j).lt.ny)) ) then
		if(halve) then
		    xe = x(j)/2
		    ye = y(j)/2
		else
		    xe = x(j)
		    ye = y(j)
		endif

		if(pu(j).eq.0) then
		    do k=0,nel-1
			if((e(j).ge.0).and.(e(j).lt.size)) then

			    image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)

			    if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
				image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k) * matrix(e(j),k)
			    endif
			endif
		    enddo
		endif

		if(nn(x(j),y(j)).lt.big) nn(x(j),y(j)) = nn(x(j),y(j)) + 1
		if(pu(j).eq.1) then
			if(nnpu(x(j),y(j)).lt.big) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + 1
		endif
	    endif
	enddo
else
	do j=0,n-1
	    if( ((x(j).ge.0).and.(x(j).lt.nx)).and. ((y(j).ge.0).and.(y(j).lt.ny)) ) then
		if(halve) then
		    xe = x(j)/2
		    ye = y(j)/2
		else
		    xe = x(j)
		    ye = y(j)
		endif

		if(pu(j).eq.0) then
		    do k=0,nel-1
			if((e(j).ge.0).and.(e(j).lt.size)) then

			    image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)*multiple(j)

			    if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)) ) then
				image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k) * matrix(e(j),k)*multiple(j)
			    endif
			endif
		    enddo
		endif

		if(nn(x(j),y(j)).lt.(big-multiple(j))) nn(x(j),y(j)) = nn(x(j),y(j)) + multiple(j)
		if(pu(j).eq.1) then
			if(nnpu(x(j),y(j)).lt.(big-multiple(j))) nnpu(x(j),y(j)) = nnpu(x(j),y(j)) + multiple(j)
		endif
	    endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate3( argc, argv)

!DLL_EXPORT da_accumulate3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	da_accumulate3 = 1
	return
endif

call da_accumulate3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), &
	 %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)) )

da_accumulate3 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate3_b( x,y,e,n, image,nx,ny, image_error,nxe,nye, &
			  nel, matrix,size, multiple)

INTEGER*4 n,nx,ny,nel,size
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1), xe,ye
INTEGER*4 multiple(0:n-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1)
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.

if( multiple(0).eq.-1) then
	do k=0,nel-1
	   do j=0,n-1
		 if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
		  ((e(j).ge.0).and.(e(j).lt.size)) ) then

			  image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)

			  if(halve) then
				 xe = x(j)/2
				 ye = y(j)/2
			  else
				 xe = x(j)
				 ye = y(j)
			  endif
			  if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)).and.((e(j).ge.0).and.(e(j).lt.size)) ) then
				   image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k)	* matrix(e(j),k)
			  endif
		 endif
	   enddo
	enddo
else
	do k=0,nel-1
	   do j=0,n-1
		 if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
		  ((e(j).ge.0).and.(e(j).lt.size)) ) then

			  image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)*multiple(j)

			  if(halve) then
				 xe = x(j)/2
				 ye = y(j)/2
			  else
				 xe = x(j)
				 ye = y(j)
			  endif
			  if( ((xe.ge.0).and.(xe.lt.nxe)).and.((ye.ge.0).and.(ye.lt.nye)).and.((e(j).ge.0).and.(e(j).lt.size)) ) then
				   image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k)*matrix(e(j),k)*multiple(j)
			  endif
		 endif
	   enddo
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function da_accumulate2( argc, argv)

!DLL_EXPORT da_accumulate2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.15) then          ! needed # args
    da_accumulate2 = 1
    return
endif

call da_accumulate2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
     %val(argv(4)), %val(argv(5)), %val(argv(6)), &
     %val(argv(7)), %val(argv(8)), %val(argv(9)), &
     %val(argv(10)), %val(argv(11)), %val(argv(12)), %val(argv(13)), &
     %val(argv(14)), %val(argv(15)) )

da_accumulate2 = 0
return
end

!------------------------------------------------------------

SUBROUTINE da_accumulate2_b( x,y,e,n, image,nx,ny, image_error,nxe,nye, &
              nel, matrix,size, multiple, n_det)

INTEGER*4 n,nx,ny,nel,size, n_det
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1), xe,ye
INTEGER*4 multiple(0:n-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), image_error(0:nxe-1,0:nye-1,0:nel-1)
REAL*4 matrix(0:size-1,0:nel-1), xd
LOGICAL halve

halve = .true.
if(ny.eq.1) halve=.false.
xd = 1./float(n_det)
if( n_det.lt.1) xd=1.0

if( multiple(0).eq.-1) then
    do k=0,nel-1
       do j=0,n-1
         if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
          ((y(j).ge.0).and.(y(j).lt.ny)).and. &
          ((e(j).ge.0).and.(e(j).lt.size)) ) then

              image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)*xd

              if(halve) then
                 xe = x(j)/2
                 ye = y(j)/2
              else
                 xe = x(j)
                 ye = y(j)
              endif
              if( ((xe.ge.0).and.(xe.lt.nxe)).and. &
                 ((ye.ge.0).and.(ye.lt.nye)).and. &
                 ((e(j).ge.0).and.(e(j).lt.size)) ) then
                   image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k)*matrix(e(j),k)*xd*xd
              endif
         endif
       enddo
    enddo
else
    do k=0,nel-1
       do j=0,n-1
         if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
          ((y(j).ge.0).and.(y(j).lt.ny)).and. &
          ((e(j).ge.0).and.(e(j).lt.size)) ) then

              image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)*multiple(j)*xd

              if(halve) then
                 xe = x(j)/2
                 ye = y(j)/2
              else
                 xe = x(j)
                 ye = y(j)
              endif
              if( ((xe.ge.0).and.(xe.lt.nxe)).and. &
                 ((ye.ge.0).and.(ye.lt.nye)).and. &
                 ((e(j).ge.0).and.(e(j).lt.size)) ) then
                   image_error(xe,ye,k) = image_error(xe,ye,k) + matrix(e(j),k)*matrix(e(j),k)*multiple(j)*xd*xd
              endif
         endif
       enddo
    enddo
endif

return
end

!------------------------------------------------------------
! THIS DOESN'T SEEM TO BE IN USE ...

integer function da_accumulate( argc, argv)

!DLL_EXPORT da_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.10) then          ! needed # args
	da_accumulate = 1
	return
endif

call da_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) )

da_accumulate = 0
return
end

!------------------------------------------------------------
! THIS DOESN'T SEEM TO BE IN USE ...

SUBROUTINE da_accumulate_b( x,y,e,n, image,nx,ny,nel, matrix,size)

INTEGER*4 n,nx,ny,nel,size
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1)
REAL*4 image(0:nx-1,0:ny-1,0:nel-1), matrix(0:size-1,0:nel-1)

do k=0,nel-1
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		 ((y(j).ge.0).and.(y(j).lt.ny)).and. &
		 ((e(j).ge.0).and.(e(j).lt.size)) ) then
		  image(x(j),y(j),k) = image(x(j),y(j),k) + matrix(e(j),k)
	   endif
	enddo
enddo

return
end

!-------------------------------------------------------------------------------

integer function spec_accumulate4( argc, argv)

!DLL_EXPORT spec_accumulate4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.17) then          ! needed # args
	spec_accumulate4 = 1
	return
endif

call spec_accumulate4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), &
	 %val(argv(15)), %val(argv(16)), %val(argv(17)) )

spec_accumulate4 = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_accumulate4_b( e,x,y,pu,veto,n, spec,ns,found, mask,nq,nx,ny,yoff, multiple, use_master, master)

INTEGER*4 n,nx,ny,nq,ns,j,k,yoff
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 spec(0:ns-1,0:nq-1),found(0:nq-1),multiple(0:n-1)
LOGICAL*1 mask(0:nq-1,0:nx-1,0:ny-1), master(0:nx-1,0:ny-1)
LOGICAL*4 use_master

if( multiple(0).eq.-1) then
	do j=0,n-1
		if( (pu(j).eq.0) .and. (veto(j).eq.0) .and. &
				((x(j).ge.0).and.(x(j).lt.nx)).and. &
				((y(j)-yoff.ge.0).and.(y(j)-yoff.lt.ny)).and. &
				((e(j).ge.0).and.(e(j).lt.ns)) ) then
			if(use_master) then
				if( master(x(j),y(j)-yoff)) then
					do k=0,nq-1
						if( mask(k,x(j),y(j)-yoff)) then
							spec(e(j),k) = spec(e(j),k) + 1
							found(k) = found(k) + 1
						endif
					enddo
				endif
			else
				do k=0,nq-1
					if( mask(k,x(j),y(j)-yoff)) then
						spec(e(j),k) = spec(e(j),k) + 1
						found(k) = found(k) + 1
					endif
				enddo
			endif
		endif
	enddo
else
	do j=0,n-1
		if( (pu(j).eq.0) .and. (veto(j).eq.0) .and. &
				((x(j).ge.0).and.(x(j).lt.nx)).and. &
				((y(j)-yoff.ge.0).and.(y(j)-yoff.lt.ny)).and. &
				((e(j).ge.0).and.(e(j).lt.ns)) ) then
			if(use_master) then
				if( master(x(j),y(j)-yoff)) then
					do k=0,nq-1
						if( mask(k,x(j),y(j)-yoff)) then
							spec(e(j),k) = spec(e(j),k) + multiple(j)
							found(k) = found(k) + multiple(j)
						endif
					enddo
				endif
			else
				do k=0,nq-1
					if( mask(k,x(j),y(j)-yoff)) then
						spec(e(j),k) = spec(e(j),k) + multiple(j)
						found(k) = found(k) + multiple(j)
					endif
				enddo
			endif
		endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function spec_accumulate3( argc, argv)

!DLL_EXPORT spec_accumulate3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.14) then          ! needed # args
	spec_accumulate3 = 1
	return
endif

call spec_accumulate3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)) )

spec_accumulate3 = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_accumulate3_b( e,x,y,pu,veto,n, spec,ns,found, mask,nx,ny,nq, multiple)

INTEGER*4 n,nx,ny,nq,ns,j,k
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 spec(0:ns-1,0:nq-1),found(0:nq-1),multiple(0:n-1)
LOGICAL*1 mask(0:nx-1,0:ny-1,0:nq-1)

if( multiple(0).eq.-1) then
	do j=0,n-1
		if( (pu(j).eq.0) .and. (veto(j).eq.0) .and. &
				((x(j).ge.0).and.(x(j).lt.nx)).and. &
				((y(j).ge.0).and.(y(j).lt.ny)).and. &
				((e(j).ge.0).and.(e(j).lt.ns)) ) then
			do k=0,nq-1
				if( mask(x(j),y(j),k)) then
					spec(e(j),k) = spec(e(j),k) + 1
					found(k) = found(k) + 1
				endif
			enddo
		endif
	enddo
else
	do j=0,n-1
		if( (pu(j).eq.0) .and. (veto(j).eq.0) .and. &
				((x(j).ge.0).and.(x(j).lt.nx)).and. &
				((y(j).ge.0).and.(y(j).lt.ny)).and. &
				((e(j).ge.0).and.(e(j).lt.ns)) ) then
			do k=0,nq-1
				if( mask(x(j),y(j),k)) then
					spec(e(j),k) = spec(e(j),k) + multiple(j)
					found(k) = found(k) + multiple(j)
				endif
			enddo
		endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function spec_accumulate2( argc, argv)

!DLL_EXPORT spec_accumulate2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.13) then          ! needed # args
	spec_accumulate2 = 1
	return
endif

call spec_accumulate2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)) )

spec_accumulate2 = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_accumulate2_b( e,x,y,pu,n, spec,ns,found, mask,nx,ny,nq, multiple)

INTEGER*4 n,nx,ny,nq,ns,j,k
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),pu(0:n-1)
INTEGER*4 spec(0:ns-1,0:nq-1),found(0:nq-1),multiple(0:n-1)
LOGICAL*1 mask(0:nx-1,0:ny-1,0:nq-1)

if( multiple(0).eq.-1) then
	do j=0,n-1
	   if( (pu(j).eq.0) .and. ((x(j).ge.0).and.(x(j).lt.nx)).and. &
			  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
			  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		 do k=0,nq-1
		  if( mask(x(j),y(j),k)) then
			  spec(e(j),k) = spec(e(j),k) + 1
			  found(k) = found(k) + 1
		  endif
		 enddo
	   endif
	enddo
else
	do j=0,n-1
	   if( (pu(j).eq.0) .and. ((x(j).ge.0).and.(x(j).lt.nx)).and. &
			  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
			  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		 do k=0,nq-1
		  if( mask(x(j),y(j),k)) then
			  spec(e(j),k) = spec(e(j),k) + multiple(j)
			  found(k) = found(k) + multiple(j)
		  endif
		 enddo
	   endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function spec_accumulate( argc, argv)

!DLL_EXPORT spec_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.12) then          ! needed # args
	spec_accumulate = 1
	return
endif

call spec_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)))

spec_accumulate = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_accumulate_b( e,x,y,n, spec,ns,found, mask,nx,ny,nq, multiple)

INTEGER*4 n,nx,ny,nq,ns,j,k
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1)
INTEGER*4 spec(0:ns-1,0:nq-1),found(0:nq-1),multiple(0:n-1)
LOGICAL*1 mask(0:nx-1,0:ny-1,0:nq-1)

if( multiple(0).eq.-1) then
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
			  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
			  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		do k=0,nq-1
		 	if( mask(x(j),y(j),k)) then
				spec(e(j),k) = spec(e(j),k) + 1
				found(k) = found(k) + 1
			endif
		enddo
	   endif
	enddo
else
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
			  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
			  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		do k=0,nq-1
			if( mask(x(j),y(j),k)) then
				spec(e(j),k) = spec(e(j),k) + multiple(j)
				found(k) = found(k) + multiple(j)
			endif
		enddo
	   endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function spec_det_accumulate4( argc, argv)

!DLL_EXPORT spec_det_accumulate4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.19) then          ! needed # args
	spec_det_accumulate4 = 1
	return
endif

call spec_det_accumulate4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)), &
	 %val(argv(18)), %val(argv(19)) )

spec_det_accumulate4 = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_det_accumulate4_b( e,x,y,ste,pu,veto,n, spec,ns,nd,found, nnpu,nn, mask,nq,nx,ny,yoff, multiple)

INTEGER*4 n,nq,nx,ny,yoff,nd,ns,j, m
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 spec(0:ns-1,0:nd-1),found(0:nd-1),multiple(0:n-1),nnpu(0:nd-1),nn(0:nd-1)
LOGICAL*1 mask(0:nq-1,0:nx-1,0:ny-1)

do j=0,n-1
	if( multiple(0).eq.-1) then
		m = 1
	else
		m = multiple(j)
	endif

	if((ste(j).ge.0).and.(ste(j).lt.nd).and.(veto(j).eq.0)) then
		if( pu(j).eq.0) then
			if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
					((y(j)-yoff.ge.0).and.(y(j)-yoff.lt.ny)).and. &
					((e(j).ge.0).and.(e(j).lt.ns)) ) then
				if( mask(0,x(j),y(j)-yoff)) then
					spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
					found(ste(j)) = found(ste(j)) + m
				endif
			endif
		else
			nnpu(ste(j)) = nnpu(ste(j)) + m
		endif
		nn(ste(j)) = nn(ste(j)) + m
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function spec_det_accumulate3( argc, argv)

!DLL_EXPORT spec_det_accumulate3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.17) then          ! needed # args
	spec_det_accumulate3 = 1
	return
endif

call spec_det_accumulate3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)), %val(argv(17)) )

spec_det_accumulate3 = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_det_accumulate3_b( e,x,y,ste,pu,veto,n, spec,ns,nd,found, nnpu,nn, mask,nx,ny, multiple)

INTEGER*4 n,nx,ny,nd,ns,j, m
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 spec(0:ns-1,0:nd-1),found(0:nd-1),multiple(0:n-1),nnpu(0:nd-1),nn(0:nd-1)
LOGICAL*1 mask(0:nx-1,0:ny-1)

do j=0,n-1
	if( multiple(0).eq.-1) then
		m = 1
	else
		m = multiple(j)
	endif

	if((ste(j).ge.0).and.(ste(j).lt.nd).and.(veto(j).eq.0)) then
		if( pu(j).eq.0) then
			if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
						((y(j).ge.0).and.(y(j).lt.ny)).and. &
						((e(j).ge.0).and.(e(j).lt.ns)) ) then
				if( mask(x(j),y(j))) then
					spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
					found(ste(j)) = found(ste(j)) + m
				endif
			endif
		else
			nnpu(ste(j)) = nnpu(ste(j)) + m
		endif
		nn(ste(j)) = nn(ste(j)) + m
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function spec_det_accumulate2( argc, argv)

!DLL_EXPORT spec_det_accumulate2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.16) then          ! needed # args
	spec_det_accumulate2 = 1
	return
endif

call spec_det_accumulate2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)) , &
	 %val(argv(14)), %val(argv(15)), %val(argv(16)) )

spec_det_accumulate2 = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_det_accumulate2_b( e,x,y,ste,pu,n, spec,ns,nd,found, nnpu,nn, mask,nx,ny, multiple)

INTEGER*4 n,nx,ny,nd,ns,j, m
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),pu(0:n-1)
INTEGER*4 spec(0:ns-1,0:nd-1),found(0:nd-1),multiple(0:n-1),nnpu(0:nd-1),nn(0:nd-1)
LOGICAL*1 mask(0:nx-1,0:ny-1)

do j=0,n-1
    if( multiple(0).eq.-1) then
	m = 1
    else
	m = multiple(j)
    endif

    if((ste(j).ge.0).and.(ste(j).lt.nd)) then
	if( pu(j).eq.0) then
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
				  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
				  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		  if( mask(x(j),y(j))) then
			  spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
			  found(ste(j)) = found(ste(j)) + m
		  endif
	   endif
	else
	    nnpu(ste(j)) = nnpu(ste(j)) + m
	endif
	nn(ste(j)) = nn(ste(j)) + m
    endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function spec_det_accumulate( argc, argv)

!DLL_EXPORT spec_det_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.13) then          ! needed # args
	spec_det_accumulate = 1
	return
endif

call spec_det_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)) , &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)))

spec_det_accumulate = 0
return
end

!------------------------------------------------------------

SUBROUTINE spec_det_accumulate_b( e,x,y,ste,n, spec,ns,nd,found, mask,nx,ny, multiple)

INTEGER*4 n,nx,ny,nd,ns,j
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1)
INTEGER*4 spec(0:ns-1,0:nd-1),found(0:nd-1),multiple(0:n-1)
LOGICAL*1 mask(0:nx-1,0:ny-1)

if( multiple(0).eq.-1) then
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
				  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
				  ((ste(j).ge.0).and.(ste(j).lt.nd)).and. &
				  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		  if( mask(x(j),y(j))) then
			  spec(e(j),ste(j)) = spec(e(j),ste(j)) + 1
			  found(ste(j)) = found(ste(j)) + 1
		  endif
	   endif
	enddo
else
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
				  ((y(j).ge.0).and.(y(j).lt.ny)).and. &
				  ((ste(j).ge.0).and.(ste(j).lt.nd)).and. &
				  ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		  if( mask(x(j),y(j))) then
			  spec(e(j),ste(j)) = spec(e(j),ste(j)) + multiple(j)
			  found(ste(j)) = found(ste(j)) + multiple(j)
		  endif
	   endif
	enddo
endif

return
end

!-------------------------------------------------------------------------------

integer function hist_accumulate4( argc, argv)

!DLL_EXPORT hist_accumulate4
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.26) then 	         ! needed # args
	hist_accumulate4 = 1
	return
endif

call hist_accumulate4_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	%val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), %val(argv(25)), &
	 %val(argv(26)) )

hist_accumulate4 = 0
return
end

!------------------------------------------------------------

SUBROUTINE hist_accumulate4_b( e,tot,x,y,ste,pu,veto,n, spec,specx,specy,ns,nc,found, nnpu,nn, multiple, &
		  do_tot, spect,nt, do_et2d, et2d,n_energy,n_time, ecompress,tcompress )

INTEGER*4 n,nc,ns,nt, m, do_et2d,do_tot
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),tot(0:n-1),pu(0:n-1),veto(0:n-1)
INTEGER*4 spec(0:ns-1,0:nc-1),found(0:nc-1),multiple(0:n-1),nnpu(0:nc-1),nn(0:nc-1)
INTEGER*4 specx(0:ns-1,0:nc-1),specy(0:ns-1,0:nc-1),spect(0:nt-1,0:nc-1)
INTEGER*4 n_energy,n_time, ecompress,tcompress, et2d(0:n_energy-1,0:n_time-1)
INTEGER*4 ie,it

do j=0,n-1
	if( multiple(0).eq.-1) then
		m = 1
	else
		m = multiple(j)
	endif

	if((ste(j).ge.0).and.(ste(j).lt.nc).and.(veto(j).eq.0)) then
		if( (pu(j)).eq.0) then
			if( ((e(j).ge.0).and.(e(j).lt.ns)) ) then
				spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
				found(ste(j)) = found(ste(j)) + m
			endif
			if( ((x(j).ge.0).and.(x(j).lt.ns)) ) then
				specx(x(j),ste(j)) = specx(x(j),ste(j)) + m
			endif
			if( ((y(j).ge.0).and.(y(j).lt.ns)) ) then
				specy(y(j),ste(j)) = specy(y(j),ste(j)) + m
			endif
			if(do_tot.eq.1) then
				if( ((tot(j).ge.0).and.(tot(j).lt.nt)) ) then
					spect(tot(j),ste(j)) = spect(tot(j),ste(j)) + m
				endif
			endif
			if(do_et2d.eq.1) then
				ie = e(j) / ecompress
				it = tot(j) / tcompress
				if( ((it.ge.0).and.(it.lt.n_time)).and.((ie.ge.0).and.(ie.lt.n_energy)) ) then
					et2d(ie,it) = et2d(ie,it) + m
				endif
			endif
		else
			nnpu(ste(j)) = nnpu(ste(j)) + m
		endif
		nn(ste(j)) = nn(ste(j)) + m
    endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function hist_accumulate3( argc, argv)

!DLL_EXPORT hist_accumulate3
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.25) then 	         ! needed # args
	hist_accumulate3 = 1
	return
endif

call hist_accumulate3_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	%val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)), %val(argv(24)), %val(argv(25)) )

hist_accumulate3 = 0
return
end

!------------------------------------------------------------

SUBROUTINE hist_accumulate3_b( e,x,y,ste,pu,n, spec,specx,specy,ns,nc,found, nnpu,nn, multiple, &
		  do_tot, tot,spect,nt, do_et2d, et2d,n_energy,n_time, ecompress,tcompress )

INTEGER*4 n,nc,ns,nt, do_tot,m
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),tot(0:n-1),pu(0:n-1)
INTEGER*4 spec(0:ns-1,0:nc-1),found(0:nc-1),multiple(0:n-1),nnpu(0:nc-1),nn(0:nc-1)
INTEGER*4 specx(0:ns-1,0:nc-1),specy(0:ns-1,0:nc-1),spect(0:nt-1,0:nc-1)
INTEGER*4 n_energy,n_time,do_et2d, ecompress,tcompress, et2d(0:n_energy-1,0:n_time-1)
INTEGER*2 ie,it


do j=0,n-1
    if( multiple(0).eq.-1) then
	m = 1
    else
	m = multiple(j)
    endif

    if((ste(j).ge.0).and.(ste(j).lt.nc)) then
	if( (pu(j)).eq.0) then
	    if( ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
		found(ste(j)) = found(ste(j)) + m
	    endif
	    if( ((x(j).ge.0).and.(x(j).lt.ns)) ) then
		specx(x(j),ste(j)) = specx(x(j),ste(j)) + m
	    endif
	    if( ((y(j).ge.0).and.(y(j).lt.ns)) ) then
		specy(y(j),ste(j)) = specy(y(j),ste(j)) + m
	    endif
	    if(do_tot.eq.1) then
		if( ((tot(j).ge.0).and.(tot(j).lt.nt)) ) then
		    spect(tot(j),ste(j)) = spect(tot(j),ste(j)) + m
		endif
	    endif
	    if(do_et2d.eq.1) then
		ie = e(j) / ecompress
		it = tot(j) / tcompress
		if( ((it.ge.0).and.(it.lt.n_time)).and.((ie.ge.0).and.(ie.lt.n_energy)) ) then
		    et2d(ie,it) = et2d(ie,it) + m
		endif
	    endif
	else
	    nnpu(ste(j)) = nnpu(ste(j)) + m
	endif
	nn(ste(j)) = nn(ste(j)) + m
    endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function hist_accumulate2( argc, argv)

!DLL_EXPORT hist_accumulate2
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.23) then 	         ! needed # args
	hist_accumulate2 = 1
	return
endif

call hist_accumulate2_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	%val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)), %val(argv(22)), %val(argv(23)) )

hist_accumulate2 = 0
return
end

!------------------------------------------------------------

SUBROUTINE hist_accumulate2_b( e,x,y,ste,n, spec,specx,specy,ns,nc,found, multiple, &
		  do_tot, tot,spect,nt, do_et2d, et2d,n_energy,n_time,n_et2d, ecompress,tcompress )

INTEGER*4 n,nc,ns,nt, do_tot, do_et2d, m
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),tot(0:n-1)
INTEGER*4 spec(0:ns-1,0:nc-1),found(0:nc-1),multiple(0:n-1)
INTEGER*4 specx(0:ns-1,0:nc-1),specy(0:ns-1,0:nc-1),spect(0:nt-1,0:nc-1)
INTEGER*4 n_energy,n_time,n_et2d, ecompress,tcompress, et2d(0:n_energy-1,0:n_time-1,0:n_et2d-1)
INTEGER*4 ee,tt

do j=0,n-1
	if( multiple(0).eq.-1) then
	   m = 1
	else
	   m = multiple(j)
	endif

	if( ((ste(j).ge.0).and.(ste(j).lt.nc)) ) then
	   if( ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
		found(ste(j)) = found(ste(j)) + m
	   endif
	   if( ((x(j).ge.0).and.(x(j).lt.ns)) ) then
		specx(x(j),ste(j)) = specx(x(j),ste(j)) + m
	   endif
	   if( ((y(j).ge.0).and.(y(j).lt.ns)) ) then
		specy(y(j),ste(j)) = specy(y(j),ste(j)) + m
	   endif
	   if(do_tot.eq.1) then
		if( ((tot(j).ge.0).and.(tot(j).lt.nt)) ) then
		   spect(tot(j),ste(j)) = spect(tot(j),ste(j)) + m
		endif
	   endif
	   if(do_et2d.eq.1) then
		ee = e(j) / ecompress
		tt = tot(j) / tcompress
		if( ((tt.ge.0).and.(tt.lt.n_time)).and.((ee.ge.0).and.(ee.lt.n_energy)) ) then
		    if(n_et2d.gt.1) then
			if((ste(j).ge.0).and.(ste(j).lt.n_et2d-1)) then
			    if(et2d(ee,tt,ste(j)+1) + m.le.2147483647) then
				et2d(ee,tt,ste(j)+1) = et2d(ee,tt,ste(j)+1) + m
			    endif
			endif
		    endif
		    et2d(ee,tt,0) = et2d(ee,tt,0) + m
		endif
	   endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function hist_accumulate( argc, argv)

!DLL_EXPORT hist_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.21) then 	         ! needed # args
	hist_accumulate = 1
	return
endif

call hist_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)), %val(argv(10)), &
	 %val(argv(11)), %val(argv(12)), %val(argv(13)), %val(argv(14)), %val(argv(15)), &
	%val(argv(16)), %val(argv(17)), %val(argv(18)), %val(argv(19)), %val(argv(20)), &
	 %val(argv(21)) )

hist_accumulate = 0
return
end

!------------------------------------------------------------

SUBROUTINE hist_accumulate_b( e,x,y,ste,n, spec,specx,specy,ns,nc,found, multiple, &
		  do_tot, tot, spect, do_et2d, et2d,n_energy,n_time, ecompress,tcompress )

INTEGER*4 n,nc,ns, do_tot,m
INTEGER*2 x(0:n-1),y(0:n-1),e(0:n-1),ste(0:n-1),tot(0:n-1)
INTEGER*4 spec(0:ns-1,0:nc-1),found(0:nc-1),multiple(0:n-1)
INTEGER*4 specx(0:ns-1,0:nc-1),specy(0:ns-1,0:nc-1),spect(0:ns-1,0:nc-1)
INTEGER*4 n_energy,n_time,do_et2d, ecompress,tcompress, et2d(0:n_energy-1,0:n_time-1)
INTEGER*4 ee,tt

do j=0,n-1
	if( multiple(0).eq.-1) then
	   m = 1
	else
	   m = multiple(j)
	endif

	if( ((ste(j).ge.0).and.(ste(j).lt.nc)) ) then
	   if( ((e(j).ge.0).and.(e(j).lt.ns)) ) then
		spec(e(j),ste(j)) = spec(e(j),ste(j)) + m
		found(ste(j)) = found(ste(j)) + m
	   endif
	   if( ((x(j).ge.0).and.(x(j).lt.ns)) ) then
		specx(x(j),ste(j)) = specx(x(j),ste(j)) + m
	   endif
	   if( ((y(j).ge.0).and.(y(j).lt.ns)) ) then
		specy(y(j),ste(j)) = specy(y(j),ste(j)) + m
	   endif
	   if(do_tot.eq.1) then
		if( ((tot(j).ge.0).and.(tot(j).lt.ns)) ) then
		   spect(tot(j),ste(j)) = spect(tot(j),ste(j)) + m
		endif
	   endif
	   if(do_et2d.eq.1) then
		ee = e(j) / ecompress
		tt = tot(j) / tcompress
		if( ((tt.ge.0).and.(tt.lt.n_time)).and.((ee.ge.0).and.(ee.lt.n_energy)) ) then
		   et2d(ee,tt) = et2d(ee,tt) + m
		endif
	   endif
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function time_accumulate( argc, argv)

!DLL_EXPORT time_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.9) then          ! needed # args
	time_accumulate = 1
	return
endif

call time_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)), %val(argv(9)))

time_accumulate = 0
return
end

!------------------------------------------------------------

SUBROUTINE time_accumulate_b( e,x,stn,n, time_stn, spec,ns,found,nq)

INTEGER*4 n,time_stn,nq,ns
INTEGER*2 x(0:n-1),e(0:n-1),stn(0:n-1)
INTEGER*4 spec(0:ns-1,0:nq-1),found(0:nq-1)

do j=0,n-1
	if(((stn(j).ge.0).and.(stn(j).lt.nq)) .and. ((x(j).ge.0).and.(x(j).lt.ns))) then
	   if( stn(j) .eq. time_stn) then
		 spec(x(j),time_stn) = spec(x(j),time_stn) + e(j)
	   else
		 spec(x(j),stn(j)) = spec(x(j),stn(j)) + 1
	   endif

	   found(stn(j)) = found(stn(j)) + 1
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function phase_accumulate( argc, argv)

!DLL_EXPORT phase_accumulate
INTEGER_ARCH argc, argv(*)

j = LOC(argc)
if(j.lt.8) then          ! needed # args
	phase_accumulate = 1
	return
endif

call phase_accumulate_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), &
	 %val(argv(4)), %val(argv(5)), %val(argv(6)), &
	 %val(argv(7)), %val(argv(8)))

phase_accumulate = 0
return
end

!------------------------------------------------------------

SUBROUTINE phase_accumulate_b( e,x,stn,n, spec,ns,found,nq)

INTEGER*4 n,nq,ns
INTEGER*2 x(0:n-1),e(0:n-1),stn(0:n-1)
INTEGER*4 spec(0:ns-1,0:nq-1),found(0:nq-1)

do j=0,n-1
	if(((stn(j).ge.0).and.(stn(j).lt.nq)) .and. ((x(j).ge.0).and.(x(j).lt.ns)) &
				   .and. (e(j).gt.0)) then
	   spec(x(j),stn(j)) = spec(x(j),stn(j)) + 1
	   found(stn(j)) = found(stn(j)) + 1
	endif
enddo

return
end

!-------------------------------------------------------------------------------

integer function hist_xy( argc, argv)

! All parameters are passed by reference

!DLL_EXPORT hist_xy
INTEGER_ARCH argc, argv(*)

j = loc(argc)
if(j.lt.11) then
	hist_xy = 1
	return
endif

call hist_xy_b( %val(argv(1)), %val(argv(2)), %val(argv(3)), %val(argv(4)), &
	%val(argv(5)), %val(argv(6)), %val(argv(7)), %val(argv(8)), &
	%val(argv(9)), %val(argv(10)), %val(argv(11)) )

hist_xy = 0
return
end

!--------------------------------------------------------------------------------

subroutine hist_xy_b( x,y,z,n, image,nx,ny,nel, hist,nhist,nh )

!   x,y     int*4    vectors of x,y coords in image array
!   z     int*4  corresponding projected coordinate in hist array
!   n     int*4  number of x,y,z elements
!
!   image float*4 image array
!   nx,ny int*4   x,y dimensions of image array(nx,ny,nel)
!   nel     int*4    number of element planes in image array
!
!   hist  float*4  histogram array to accumulate (return)
!   nhist int*4   histogram frequency (return)
!   nh       int*4 size if hist array
!
!   all arrays start at zero.
!   all parameters are passed by reference

integer*4 x,y,z
integer*4 n,nx,ny,nel,nh, nhist(0:nh-1)
real*4 image(0:nx-1,0:ny-1,0:nel-1), hist(0:nh-1,0:nel-1)

do i=0,nel-1
	do j=0,n-1
	   if( ((x(j).ge.0).and.(x(j).lt.nx)).and. &
		 ((y(j).ge.0).and.(y(j).lt.ny)).and. &
		 ((z(j).ge.0).and.(z(j).lt.nh)) ) then
		  nhist(z(j)) = nhist(z(j)) + 1
		  hist(z(j),i) = hist(z(j),i) + image(x(j),y(j),i)
	   endif
	enddo
enddo

return
end

!--------------------------------------------------------------------------------
