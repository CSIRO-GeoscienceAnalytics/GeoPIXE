function str_tidy, x, places=places, length=len
 
; Convert to string and strip leading and trailing spaces.
; Then trim off any redundant zeroes after decimal points or leading zeroes on exponents.
; places=places		only keep this many places maximum after decimal "."
;					if 'places' is negative, relax this to keep significant digits
; length=len		set an absolute limit on length
;
; Chris Ryan (CSIRO), revised 2012

COMPILE_OPT STRICTARR
if n_elements(places) eq 0 then places=-6
if n_elements(len) eq 0 then len=0
if n_elements(x) eq 0 then return, ''

s = strip_trail_zero( string(x), places=places)

if len gt 0 then s = strmid( s, 0, len)
return, s
end
