function run_time, n

; Is it run-time mode, or not.
; Call this routine in Main routine before compiling anything else.

return, lmgr(/runtime)
end
