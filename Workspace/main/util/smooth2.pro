function smooth2, imgi, n

COMPILE_OPT STRICTARR

	img = reform(imgi)
	s = size(img)
	if (s[0] ne 2) and (s[0] ne 1) then goto, done
	nx = s[1]
	ny = (s[0] eq 2) ? s[2]: 1
	nx2 = nx + 2*n
	ny2 = ny + 2*n

;	Extend image

	img2 = extend_image( img, n)

;	Smooth

	img3 = smooth( img2, n)

;	Select original image area

	img = img3[n:nx2-n-1,n:ny2-n-1]

done:
	return, img
end