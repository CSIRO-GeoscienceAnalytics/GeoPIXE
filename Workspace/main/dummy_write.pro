pro dummy_write, on=on, off=off

common c_debug_dummy, dummy_write
if n_elements(dummy_write) lt 1 then dummy_write=0

if n_elements(on) lt 1 then on=1
if n_elements(off) lt 1 then off=0
if off then on=0

dummy_write = on
return
end

