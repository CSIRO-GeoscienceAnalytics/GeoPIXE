pro grab24

file = file_requester(title='Save as GIF file',filter='*.gif',/write)
if strlen(file) lt 1 then return

img = tvrd(true=3)
img = color_quan( img, 3, r,g,b, colors=128)

write_gif, file, img, r,g,b

return
end
