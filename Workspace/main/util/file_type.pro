function file_type, file

j = locate_last('.', file)
if j lt 0 then return, ''

type = extract(file,j+1,lenchr(file)-1)
return, type
end
