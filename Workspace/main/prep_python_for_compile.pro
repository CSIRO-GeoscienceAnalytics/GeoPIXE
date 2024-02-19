pro prep_python_for_compile

sys = python.import('sys')
s = sys.version
print, s

;a = dialog_message(s, /info)
;wait, 1
return
end