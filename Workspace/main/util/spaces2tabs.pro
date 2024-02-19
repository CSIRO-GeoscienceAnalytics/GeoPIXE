;+
; Program: spaces2tabs
;
; Purpose:
;   Example program to convert beginning line spaces
;   of specified length to tabs.
;
; Results:
;   Resulting converted text file is saved in current
;   directory with ".tabs" appended to file name.
;
; User modifications:
;   Change the "tabsize" variable definition to the
;   the length of space characters that should be
;   converted to tabs.
;
; Author: ju, ITT Technical Support
; Date: 10.30.06
;
;-
PRO spaces2tabs

    tabsize = 4 ; spaces (modify value to suit needs)
    maxindents = '8'

    stabsize = STRCOMPRESS(tabsize, /REMOVE_ALL)

    fname = DIALOG_PICKFILE(path='', FILTER='*.pro')

    PRINT
    PRINT, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
    PRINT, '%%% Converting spaces at beginning of %%%
    PRINT, '%%% line of length ' + stabsize + ' to tabs. %%%'
    PRINT, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

    PRINT, 'Source file name: '
    PRINT, '>> ' + fname


    sarray = STRARR(FILE_LINES(fname))

    OPENR, lun, fname, /GET_LUN
    READF, lun, sarray
    FREE_LUN, lun

    FOR i = 0, N_ELEMENTS(sarray) - 1 DO BEGIN
       pos = STREGEX(sarray[i], $
         '^( {' + stabsize + '}){1,' + $
         maxindents +'}', LENGTH = len)
       IF pos NE -1 THEN BEGIN
    ;         PRINT, sarray[i]
         stabs = ''
         sremain = STRMID(sarray[i], len)
         FOR j = 1, (len / tabsize) DO $
          stabs += STRING(9B)
         sarray[i] = stabs + sremain
    ;        PRINT, sarray[i]
       ENDIF
    ENDFOR

    outfname = fname + '.tabs'
    OPENW, lun, outfname, /GET_LUN
    PRINTF, lun, sarray, FORMAT = '(a)'
    FREE_LUN, lun

    PRINT, 'Converted file name: '
    PRINT, '>> ' + outfname
    PRINT, 'Done converting begin line spaces to tabs.'
    PRINT

END

