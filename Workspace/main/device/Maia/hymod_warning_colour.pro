function hymod_warning_colour, debug

if debug.pulser or debug.synth or debug.scepter then begin
	c = 2
endif else begin
	c = 0
endelse

return, c
end
