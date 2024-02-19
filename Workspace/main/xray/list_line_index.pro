function list_line_index, z, shell, nosort=nosort, photo=photo

; Return a list of line indices for this 'shell' and 'z',
; sorted in order of relative-intensity.
;
; Both arguments must be scalars
; Shells are:  K=1, L=2, M=3, all=0
;
; Return relative-intensity order, unless /nosort used.

@line_split_definitions.def

if n_params() lt 2 then return, 0
if (n_elements(z) gt 1) or (n_elements(shell) gt 1) then begin
	print,'list_line_index: illegal argument mixed array lengths'
	return, 0
endif
if n_elements(nosort) lt 1 then nosort=0
if (z lt 1) or (z gt 94) then return, 0

list = 0

if shell le 1 then begin

;	K-alpha

	if (z le Ka_s) then begin
		list = [list,Ka_lo]
	endif else begin
		list = [list,[1,2,3]]
	endelse

;	K beta

	if (z le Kb_s) then begin
		list = [list,Kb_lo]
	endif else begin
		list = [list,[4,5,6,7,8]]
	endelse

;	K-LL and K-MM Auger lines

	if ( (z ge 12) and (z le 30)) then begin
		list = [list,[38,39]]
	endif
endif

if (shell eq 0) or (shell eq 2) then begin

;	L alpha

	if (z le La_s) then begin
		list = [list,La_lo]
	endif else begin
		list = [list,[15,16]]
	endelse

;	L beta

	list = [list,[17,18,19,20,21,22]]


;	L gamma, eta, l

	list = [list,[23,24,25,26,27,28,  14,13]]

;	Lt, Lb9

	list = [list,[51,52]]

endif

if (shell eq 0) or (shell eq 3) then begin

;	M lines

	list = [list,[32,33,34,35,36,37]]

;	M minor lines

	list = [list,[48,49,50]]
endif

if n_elements(list) gt 1 then list = list[1:*]

rel = relative_intensity( z,list, photo=photo)
energy = e_line( z,list)

q = where( (rel gt 0.000001) and (energy gt 0.1))		; eliminate duds
if q[0] eq -1 then return, 0

list = list[q]
rel = rel[q]

if nosort then return, list

; Sort by relative intensity

q = sort(rel)							; sort by rel-int
list = list[reverse(q)]					; into descending order

return, list
end
