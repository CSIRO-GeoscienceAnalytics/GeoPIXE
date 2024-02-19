pro build_miro_script_step_cal, n=n, time=time, rate=rate, inc=inc, all=all, ndet=ndet

; Build a Miro script to do the step-cal-20 run for linearization
; Use 'inc' of integers for DAC values, float for volts.
; If /all then build step-cal for all detector channels, in groups of 3.

if n_elements(n) lt 3 then n=0						; [14,60,80]
if n_elements(time) lt 1 then time=20				; 300
if n_elements(rate) lt 1 then rate=2000
if n_elements(all) lt 1 then all=1					; 0
if n_elements(ndet) lt 1 then ndet=96				; 384
if n_elements(inc) lt 1 then inc=200				; 0.08

if all then begin
	nloop = 32
	n = 32*indgen(20)
	q = where(n lt ndet)
	n = n[q]
	file = 'step-cal-all-20-binary'
endif else begin
	nloop = 1
	file = 'step-cal-20-binary'
endelse
nn = n_elements(n)
tot = 0.0

openw, 1, file
printf, 1, 'disable etev'
printf, 1, 'newrun'

for k=0L,nloop-1 do begin
	printf, 1, 'newseg'
	
	for j=0L,nn-1 do begin
		s = str_tidy(n[j])
		printf, 1, 'hermes ' + s + ' ecal 1'
	endfor
	printf, 1, 'sleep 0.25'
	printf, 1, 'enable etev'
	
	nv = 5000 / inc
	v = inc
	for i=0L,nv-1 do begin
		printf, 1, 'pulser ' + str_tidy(v) + ' ' + str_tidy(v) + ' ' + str_tidy(rate)
		printf, 1, 'sleep ' + str_tidy(time)
		v = v+inc
		tot = tot + time
	endfor

	printf, 1, 'disable etev'
	for j=0L,nn-1 do begin
		s = str_tidy(n[j])
		printf, 1, 'hermes ' + s + ' ecal 0'
	endfor

	n = n+1
endfor

printf, 1, 'disable etev'
printf, 1, 'pulser 0 0 0'
printf, 1, 'endrun'
close,1
print,'Total time = ',tot,' sec (',tot/60.,' mins, ',tot/3600.,' hours)'
return
end
