function extend_image, img, n

COMPILE_OPT STRICTARR

	s = size(img)
	img2 = img
	if (s[0] ne 2) and (s[0] ne 1) then goto, done
	nx = s[1]
	ny = (s[0] eq 2) ? s[2]: 1
	nx2 = nx + 2*n
	ny2 = ny + 2*n

	img2 = replicate( img[0], nx2, ny2)

;	Extend corner areas

	img2[0:n,0:n] = img[0,0]								; bottom left
	img2[nx2-n-1:nx2-1,0:n] = img[nx-1,0]					; bottom right
	img2[0:n,ny2-n-1:ny2-1] = img[0,ny-1]					; top left
	img2[nx2-n-1:nx2-1,ny2-n-1:ny2-1] = img[nx-1,ny-1]		; top right

;	Extend sides

	for i=0L,n-1 do begin
		img2[i,n:ny2-n-1] = img[0,*]						; left
		img2[nx2-i-1,n:ny2-n-1] = img[nx-1,*]				; right
		img2[n:nx2-n-1,i] = img[*,0]						; bottom
		img2[n:nx2-n-1,ny2-i-1] = img[*,ny-1]				; top
	endfor

;	Copy image

	img2[n:nx2-n-1,n:ny2-n-1] = img

done:
	return, img2
end
