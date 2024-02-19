pro build_kandinsky_script_gain_trim, ps, time=time, rate=rate, $
			low=low, high=high, n_channels=n_channels, n_chips=n_chips

; Build a Kandinsky script to do the gain-trim run for gain trimming of E,T
;   low		low amp
;   high	high amp
;   rate	count rate
;   time	time on each peak
;
; return string array of commands.

COMPILE_OPT STRICTARR
if ptr_valid(ps) eq 0 then return
if n_elements(time) lt 1 then time=10				; 300
if n_elements(rate) lt 1 then rate=2000
if n_elements(low) lt 1 then low=0.6
if n_elements(high) lt 1 then high=3.0
if n_elements(n_chips) lt 1 then n_chips=12
if n_elements(n_channels) lt 1 then n_channels=32
low = low[0]
high = high[0]
rate = rate[0]
time = time[0]
n_chips = (*ps).n_detectors/32

warning,'build_kandinski_script_gain_trim',['Remember to clear "gaintrim enable" to','collect data to fit gaintrim parameters.']

tot = 0.0
(*ps).current = 0
(*(*ps).ps)[*]=''

socket_command_set, ps, /queue, 'enable',0, class='pulser' , error=error
socket_command_set, ps, /queue, 'enable',0, class='photon' , error=error
socket_command_set, ps, /queue, 'EBLK',1, class='hermes', chip=-1, n_chips=n_chips, error=error
socket_command_set, ps, /queue, 'newrun',1, class='blog' , error=error
socket_command_set, ps, /queue, 'rate',rate, class='pulser' , error=error
socket_command_set, ps, /queue, 'enable',1, class='pulser' , error=error

one_chip_per_quadrant = [0,3,6,9]

for c=0,n_channels-1 do begin
	socket_command_set, ps, /queue, 'ecal',1, class='hermes', chip=-1, channel=c, n_chips=n_chips , error=error
	socket_command_set, ps, /queue, 'voltage',low, class='pulser' , error=error
	(*(*ps).ps)[(*ps).current] = 'sleep 10.0'
	(*ps).current =(*ps).current+1
	socket_command_set, ps, /queue, 'enable',1, class='photon' , error=error
	(*(*ps).ps)[(*ps).current] = 'sleep ' + str_tidy(time)
	(*ps).current =(*ps).current+1
	socket_command_set, ps, /queue, 'enable',0, class='photon' , error=error
	socket_command_set, ps, /queue, 'voltage',high, class='pulser' , error=error
	(*(*ps).ps)[(*ps).current] = 'sleep 1.0'
	(*ps).current =(*ps).current+1
	socket_command_set, ps, /queue, 'enable',1, class='photon' , error=error
	(*(*ps).ps)[(*ps).current] = 'sleep ' + str_tidy(time)
	(*ps).current =(*ps).current+1
	socket_command_set, ps, /queue, 'enable',0, class='photon' , error=error
	socket_command_set, ps, /queue, 'ecal',0, class='hermes', chip=-1, channel=c, n_chips=n_chips , error=error
	tot = tot+time+20.
endfor

(*(*ps).ps)[(*ps).current] = 'sleep 10.0'
(*ps).current =(*ps).current+1
socket_command_set, ps, /queue, 'endrun',1, class='blog' , error=error
socket_command_set, ps, /queue, 'EBLK',0, class='hermes', chip=-1, n_chips=n_chips, error=error
socket_command_set, ps, /queue, 'enable',0, class='pulser' , error=error
socket_command_set, ps, /queue, 'enable',1, class='photon' , error=error
print,'Total time = ',tot,' sec (',tot/60.,' mins, ',tot/3600.,' hours)'
return
end
