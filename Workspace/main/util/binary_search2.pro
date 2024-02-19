function bin2_search2, list, e, l,h,r,done

; This does not seem to be used. Only calls to 'binary_search()'
; are seen in GeoPIXE.

q = where(done eq 0, count)
if count eq 0 then return,r

m = (l[q]+h[q])/2
q1 = where(m le l[q],count1)
if count1 gt 0 then begin
    done[q[q1]] = 1
    em = 0.5*( list[l[q[q1]]] + list[h[q[q1]]])
    r[q[q1]] = l[q[q1]]
    q2 = where( e[q[q1]] ge em, count2)
    if count2 gt 0 then r[q[q1[q2]]]=h[q[q1[q2]]]
endif

q1 = where(e[q] lt list[m], count1)
if count1 gt 0 then h[q[q1]] = m[q1]
q1 = where(e[q] ge list[m], count1)
if count1 gt 0 then l[q[q1]] = m[q1]

return, bin2_search2( list, e, l,h,r,done)
end

;----------------------------------------------------------------

function binary_search2, list, e

; Binary search routine (recursive)
; search for closest match to 'e' in 'list' and return closest index into list.
; 'e' can be a vector.
;
; This does not seem to be used. Only calls to 'binary_search()'
; are seen in GeoPIXE.

n = n_elements(e)
nl = n_elements(list)
if nl lt 1 then return, 0
if n lt 1 then return, 0

l = intarr(n)
h = replicate(nl-1,n)
done = intarr(n)
r = intarr(n)

return, bin2_search2( list, e, l,h,r,done)
end
