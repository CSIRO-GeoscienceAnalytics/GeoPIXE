function shift_image, img, dx,dy

; Shift an image by dx in X and dy in Y

	nx = floor(dx)				; whole pixel parts (fix() later, when move fractional part too)
	ny = floor(dy)
	fx = dx - nx				; fractional shifts (should be always positive)
	fy = dy - ny
	
	img1 = shift( img, nx,ny) 

	img2 = ( img1*(1-fx)*(1-fy) + shift(img1,1,0)*fx*(1-fy) + shift(img1,0,1)*(1-fx)*fy + shift(img1,1,1)*fx*fy)
	
	return, img2
end
