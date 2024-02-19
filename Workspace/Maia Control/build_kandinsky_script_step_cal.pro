pro build_kandinsky_script_step_cal, ps, n, time=time, rate=rate, all=all, ndet=ndet, $
			low=low, high=high, count=count

; Build a Kandinsky script to do the step-cal-20 run for linearization
;   [n]		detector channel(s)
;   /all	all detectors
;   low		low amp
;   high	high amp
;   count	number of peaks
;   rate	count rate
;
; return string array of commands.

COMPILE_OPT STRICTARR
if ptr_valid(ps) eq 0 then return
if n_elements(n) lt 1 then n=0						; [14,60,80]
if n_elements(time) lt 1 then time=20				; 300
if n_elements(rate) lt 1 then rate=2000
if n_elements(count) lt 1 then count=20
if n_elements(low) lt 1 then low=0.05
if n_elements(high) lt 1 then high=3.3
if n_elements(all) lt 1 then all=0					; 0
if n_elements(ndet) lt 1 then ndet=384				; 384
low = low[0]
high = high[0]
count = count[0]
rate = rate[0]
time = time[0]
n_chips = (*ps).n_detectors/32

warning,'build_kandinski_script_step_cal',['Remember to clear "linearize enable" to','collect data to fit linearization parameters.']

; Check low and high with count to ensure even DAC spacing (10 bits assumed)

v_dac_bit = 3.0 / 1024								; 10 bit DAC increment for 3V range
ilow = round(low/v_dac_bit)							; start DAC
ihigh = round(high/v_dac_bit)						; end DAC
iinc = (long((ihigh - ilow) / long(count-1))) > 1	; nearest (floor) DAC increment
count = (ihigh - ilow + 1)/iinc						; compensate count to get high back up
ihigh = ilow + long(count-1)*iinc					; revise ihigh for whole incs
low = ilow * v_dac_bit								; rounded low
high = ihigh * v_dac_bit							; whole number of incs to high

inc = (high-low)/float(count-1)						; new float inc
print,'build_kandinsky_script_step_cal: new LOW, HIGH =', low, high

if all then begin
	nloop = 32
	n = 32*indgen(ndet/32)
	q = where(n lt ndet)
	n = n[q]
endif else begin
	nloop = 1
endelse
nn = n_elements(n)
tot = 0.0
(*ps).current = 0
(*(*ps).ps)[*]=''

socket_command_set, ps, /queue, 'enable',0, class='pulser' , error=error
socket_command_set, ps, /queue, 'enable',0, class='photon' , error=error
socket_command_set, ps, /queue, 'EBLK',1, class='hermes', chip=-1, n_chips=n_chips, error=error
socket_command_set, ps, /queue, 'newrun',1, class='blog' , error=error
socket_command_set, ps, /queue, 'rate',rate, class='pulser' , error=error

for k=0L,nloop-1 do begin
	h = n / 32
	c = n mod 32
	socket_command_set, ps, /queue, 'newseg',1, class='blog' , error=error
	socket_command_set, ps, /queue, 'ecal',1, class='hermes', chip=h, channel=c , error=error
	(*(*ps).ps)[(*ps).current] = 'sleep 10.0'
	(*ps).current =(*ps).current+1
	socket_command_set, ps, /queue, 'enable',1, class='pulser' , error=error
	socket_command_set, ps, /queue, 'enable',1, class='photon' , error=error
	tot = tot + 3.0
		
	v = low
	for i=0L,count-1 do begin
		socket_command_set, ps, /queue, 'voltage',v, class='pulser' , error=error
		(*(*ps).ps)[(*ps).current] = 'sleep ' + str_tidy(time)
		(*ps).current =(*ps).current+1
		v = v+inc
		tot = tot + time
	endfor

	socket_command_set, ps, /queue, 'enable',0, class='pulser' , error=error
	socket_command_set, ps, /queue, 'enable',0, class='photon' , error=error
	socket_command_set, ps, /queue, 'ecal',0, class='hermes', chip=h, channel=c , error=error
	n = n+1
endfor

socket_command_set, ps, /queue, 'enable',0, class='pulser' , error=error
socket_command_set, ps, /queue, 'endrun',1, class='blog' , error=error
socket_command_set, ps, /queue, 'EBLK',0, class='hermes', chip=-1, n_chips=n_chips, error=error
socket_command_set, ps, /queue, 'enable',1, class='photon' , error=error
print,'Total time = ',tot,' sec (',tot/60.,' mins, ',tot/3600.,' hours)'
return
end
