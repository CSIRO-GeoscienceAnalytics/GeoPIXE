pro set_widget_text, id, s

; Set widget text and right align it.

n = lenchr(s)
k = lenchr(strip_path(s))
widget_control, id, set_value=s, set_text_select=[n-k,k]
widget_control, id, set_value=s, set_text_select=[n-1,0]

return
end
