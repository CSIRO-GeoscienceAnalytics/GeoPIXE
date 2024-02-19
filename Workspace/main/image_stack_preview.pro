pro image_stack_preview, file, preview=preview, ignore=ignore

; Provide preview image and data for 'file_requester'

COMPILE_OPT STRICTARR
define_devices

preview = 0L
if n_elements(file) lt 1 then return

	image_geopixe_preview, file, preview=preview, ignore=ignore, /xanes
	return

end
