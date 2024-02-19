pro build_miro_script_gain_trim, n=n, time=time, rate=rate, vlow=vlow, vhigh=vhigh

; Build a Miro script to do the gain trimming pulser data collection

if n_elements(n) lt 1 then n=96
if n_elements(time) lt 1 then time=10
if n_elements(rate) lt 1 then rate=2000
if n_elements(vlow) lt 1 then vlow=0.08
if n_elements(vhigh) lt 1 then vhigh=0.28

openw, 1, 'gain-trim-cal'

printf, 1, 'disable etev'
printf, 1, 'newrun'
printf, 1, 'sleep 0.25'

for i=0L,n-1 do begin
	printf, 1, 'hermes ' + str_tidy(i) + ' ecal 1'
	printf, 1, 'pulser ' + str_tidy(vlow) + ' ' + str_tidy(vlow) + ' ' + str_tidy(rate)
	printf, 1, 'sleep 0.25'
	printf, 1, 'enable etev'
	printf, 1, 'sleep ' + str_tidy(time)
	printf, 1, 'disable etev'
	printf, 1, 'pulser ' + str_tidy(vhigh) + ' ' + str_tidy(vhigh) + ' ' + str_tidy(rate)
	printf, 1, 'sleep 0.25'
	printf, 1, 'enable etev'
	printf, 1, 'sleep ' + str_tidy(time)
	printf, 1, 'disable etev'
	printf, 1, 'hermes ' + str_tidy(i) + ' ecal 0'
endfor

printf, 1, 'pulser 0 0 0'
printf, 1, 'newseg'
close,1
return
end
