	function clip, x, low, high
;
; Clip x into range [low,high]
;
	return, (x > low) < high
	end
