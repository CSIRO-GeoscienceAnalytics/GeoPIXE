pro lund_header, d_header

; Define a Lund histogram (1D, 2D) file header struct

d_header = {  $
	dummy1:			bytarr(7), $
	btitle:			bytarr(16), $
	dummy1b:		bytarr(47), $
	FileRefNum:		2S, $
	PlotWindow:		4L, $
	PlotType:		2S, $
	LiveTime:		4L, $
	DataXLength:	4L, $
	DataYLength:	4L, $
	DataMatrix:		4L, $
	DisplayMatrix:	4L, $
	dummy2:			bytarr(50), $
	Xmin:			4L, $
	XMax:			4L, $
	YMin:			4L, $
	YMax:			4L, $
	ZMin:			4L, $
	ZMax:			4L, $
	dummy3:			bytarr(66),  $
	AutoDraw:		1B,  $
	AxesOn:			1B,  $
	GridOn:			1B,  $
	XCalibOn:		1B,  $
	XGain:			0.0,  $
	XOffset:		0.0,  $
	XUnits:			bytarr(16),  $
	dummy4:			bytarr(24),  $
	YGain:			0.0,  $
	YOffset:		0.0,  $
	YUnits:			bytarr(16),  $
	dummy5:			bytarr(150),  $
	YCalibOn:		1B,  $
	dummy6:			bytarr(47) }

	return
	end

