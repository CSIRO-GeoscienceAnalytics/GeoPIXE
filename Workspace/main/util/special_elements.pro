function special_elements, arb=arb, counts=counts, time=time, maia=maia

; Special reserved element names
;
;	/counts		report as counts (no units) and report average in regions
;	/arb		report as arbitrary units divided by charge/flux
;	/time		report as time units
;	/maia		Maia on-line GUI image names

if n_elements(arb) eq 0 then arb=0
if n_elements(counts) eq 0 then counts=0
if n_elements(time) eq 0 then time=0
if n_elements(maia) eq 0 then maia=0

if counts then begin
	return, ['Flux','Flux0','Flux1','NNPU','NN','TT','Dwell','DT','PU','rFlux','Rate','c/s']
endif
if arb then begin
	return, ['Back','Compton','elastic','sum','Back1','Back2','Back3','Back4','Back5','Back6','scatter']
endif
if time then begin
	return, ['Dwell']
endif

; For Maia keep the Flux pars at end as "attributes"

if maia then begin
	return, ['Dwell','Flux','NN','NNPU','TT','Flux0','Flux1']
endif

return, ['Back','Flux','Flux0','Flux1','NNPU','NN','TT','Dwell','Compton','elastic','sum','DT','PU','rFlux','Rate', $
			'c/s','Back1','Back2','Back3','Back4','Back5','Back6','scatter']
end
