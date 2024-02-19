pro error_catch_off

COMPILE_OPT STRICTARR
common c_debug_warnings, enable_warning_popup
common c_errors_1, catch_errors_on

catch_errors_on = 0							; enable error CATCHing
enable_warning_popup = 1					; enable popup warnings

return
end
