FUNCTION TYPEVAR, variable

   ; Use SIZE function to get variable info.

;dataTypes = ['UNDEFINED', 'BYTE', 'INTEGER', 'LONG', 'FLOAT', $
;     'DOUBLE', 'COMPLEX', 'STRING', 'STRUCT', 'DCOMPLEX', $
;     'POINTER', 'OBJREF',' UINT',' ULONG',' LONG64',' ULONG64' ]

RETURN, size( variable, /Tname)
END