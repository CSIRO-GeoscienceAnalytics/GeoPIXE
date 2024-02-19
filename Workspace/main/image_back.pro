pro image_back, file, q

; Ammend an image file's show_back

p = read_geopixe_image(file)
if ptr_valid(p) eq 0 then return

print,'Modify show from ',(*p).show_back,'  to  ',long(q)
(*p).show_back = long(q)

write_geopixe_image, p, file
return
end

