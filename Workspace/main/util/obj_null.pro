function obj_null, obj

COMPILE_OPT STRICTARR

	help, obj, output=s
	if strpos( s, '<NullObject>') ge 0 then return, 1

	return, 0
end
