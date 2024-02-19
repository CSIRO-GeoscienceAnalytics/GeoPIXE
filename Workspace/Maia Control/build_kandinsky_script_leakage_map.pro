pro build_kandinsky_script_leakage_map, ps, n, time=time, n_channels=n_channels, n_chips=n_chips, map=pelk, ean=ean, all=all

; Build a Kandinsky script to scan all detectors using ELK and sample OAK for each 
;   [n]		detector channel(s)
;   /all	all detectors
;   time	time on each detector
;	map		pointer to where results will be stored
;	
; return string array of commands.

COMPILE_OPT STRICTARR
if ptr_valid(ps) eq 0 then return
if n_elements(n) lt 1 then n=0						; [14,60,80]
if n_elements(all) lt 1 then all=0					; 0
if n_elements(time) lt 1 then time=10				; 300
if n_elements(n_chips) lt 1 then n_chips=12
if n_elements(n_channels) lt 1 then n_channels=32
if n_elements(ean) lt 1 then ean=0
time = time[0]

if all then begin
	nloop = n_channels
	nn = n_channels*indgen(n_chips)
	q = indgen(nn)
	n = q
endif else begin
	nloop = 1
endelse
nn = n_elements(n)

tot = 0.0
(*ps).current = 0
(*(*ps).ps)[*]=''

(*pelk)[*] = 0.0
elk = 'elk'
if ean then elk = 'ean'

for i=0,nn-1 do begin
	h = n[i] / n_channels
	c = n[i] - h*n_channels
	
	socket_command_set, ps, /queue, elk,1, class='hermes', chip=h, channel=c, error=error
	
	(*(*ps).ps)[(*ps).current] = 'sleep ' + str_tidy(time)
	(*ps).current =(*ps).current+1

	socket_command_set, ps, /queue, get=pelk, index=n[i], 'oan', class='status.hermes' , error=error

	socket_command_set, ps, /queue, elk,0, class='hermes', chip=h, channel=c, error=error
	tot = tot+time
endfor
print,'Total time = ',tot,' sec (',tot/60.,' mins, ',tot/3600.,' hours)'
return
end
