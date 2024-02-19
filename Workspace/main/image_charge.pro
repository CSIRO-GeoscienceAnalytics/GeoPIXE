pro image_charge, file, q

; Ammend an image file's charge

p = read_geopixe_image(file)
if ptr_valid(p) eq 0 then return

print,'Modify charge from ',(*p).charge,'  to  ',q
(*p).charge = q

write_geopixe_image, p, file
return
end

