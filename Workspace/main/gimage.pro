
pro gImage_event, Event

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'Image_Event',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

  wWidget =  Event.top
  widget_control, hourglass=0

  child = widget_info( event.top, /child)
  widget_control, child, get_uvalue=pstate

  if ptr_good(pstate,/struct) eq 0 then goto, bad_state

  test = (*pstate).test
  debug = (*pstate).debug

    case tag_names( event,/structure) of
       'NOTIFY': begin
         OnNotify_Image, event
         goto, finish
         end
       'WIDGET_TRACKING': begin
         OnTracking_Image, Event
         goto, finish
         end
       else:
    endcase

  uname = widget_info( event.id, /uname)

  case uname of

    'Image_TLB': begin
       case Tag_Names(Event, /STRUCTURE_NAME) of
         'WIDGET_KILL_REQUEST': begin
          OnKill_Image, Event
          end
         'WIDGET_BASE': begin
;         help,event,/str
          OnSize_Image, Event
          end
         'WIDGET_TIMER': begin
          OnTimer_Image, Event
          end
         else:
       endcase
       end

    'Image_Draw': begin
       if( Event.type eq 3 )then begin
         OnViewport_Image, Event
       endif else begin
         OnButton_Image, Event
       endelse
       end

    'Element_combobox': begin
       OnSelect_Image_Element, Event
       end

    'Mode_combobox': begin
       OnSelect_Image_Mode, Event
       end

    'Full_Button': begin
       OnButton_Image_Full, Event
       end

    'Zoom_In_Button': begin
       OnButton_Image_Zoom_In, Event
       end

    'Zoom_Out_Button': begin
       OnButton_Image_Zoom_Out, Event
       end

    'Analyze_Type_combobox': begin
       OnSelect_Image_Analyze_Type, Event
       end

    'Analyze_Mode_combobox': begin
       OnSelect_Image_Analyze_Mode, Event
       end

	'interp_button': begin
       OnSelect_Image_Interp, Event
		end
		
    'Zscale_Mode_combobox': begin
       OnSelect_Image_Zscale_Mode, Event
       end

    'Analyze_Button': begin
       OnButton_Image_Analyze, Event
       end

    'Bottom_Slider': begin
       OnMove_Image_Bottom, Event
       end

    'Top_Slider': begin
       OnMove_Image_Top, Event
       end

    'Load_Menu': begin
       Image_Load, Event
       end

    'Load_Menu_Ignore': begin
       Image_Load, Event, /ignore
       end

    'Save_Menu': begin
       Image_Save, Event
       end

    'Clear_Menu': begin
       Image_Clear, Event
       end

    'Save_GIF_Menu': begin
       Image_Save_GIF, Event
       end

	'Save_PNG_Colour_Menu': begin
       Image_Save_GIF, Event, /PNG, /RGB
       end

    'Save_PNG_Menu': begin
       Image_Save_GIF, Event, /PNG
       end

    'Save_JPEG_Menu': begin
       Image_Save_GIF, Event, /JPEG
       end

    'Save_TIFF_Menu': begin
       Image_Save_GIF, Event, /TIFF
       end

    'Save_GIF_Shape_Menu': begin
       Image_Save_GIF, Event, /only_shape
       end

    'Save_PNG_Shape_Menu': begin
       Image_Save_GIF, Event, /PNG, /only_shape
       end

    'Save_GIF_Highlight_Menu': begin
       Image_Save_GIF, Event, /only_Highlight
       end

    'Save_PNG_Highlight_Menu': begin
       Image_Save_GIF, Event, /PNG, /only_Highlight
       end

    'Save_PNG_Highlight_Menu_pink': begin
       Image_Save_GIF, Event, /PNG, /only_Highlight, colour=spec_colour('pink')
       end

    'Save_PNG_Highlight_Menu_red': begin
       Image_Save_GIF, Event, /PNG, /only_Highlight, colour=spec_colour('red')
       end

    'Save_PNG_Highlight_Menu_yellow': begin
       Image_Save_GIF, Event, /PNG, /only_Highlight, colour=spec_colour('yellow')
       end

    'Save_PNG_Highlight_Menu_lblue': begin
       Image_Save_GIF, Event, /PNG, /only_Highlight, colour=spec_colour('l.blue')
       end

    'Save_PNG_Highlight_Menu_orange': begin
       Image_Save_GIF, Event, /PNG, /only_Highlight, colour=spec_colour('orange')
       end

    'Save_Chimage_Menu': begin
       Image_Save_Chimage, Event
       end
       
	'Save_Zarr_Menu': begin
       Image_Save_Zarr, Event
		end

	'Export_Zarr_GCF_Menu': begin
       Image_Export_Zarr_GCF, Event
		end

	'dump_binary_Menu': begin
       Image_dump_binary, Event
       end
       
    'Save_All_HTML_Menu': begin
       Image_Save_all_HTML, Event, /GIF
       end

    'Save_All_BW_HTML_Menu': begin
       Image_Save_all_HTML, Event, /bw, /GIF
       end

    'Save_All_HTML_PNG_Menu': begin
       Image_Save_all_HTML, Event, /PNG
       end

    'Save_All_BW_HTML_PNG_Menu': begin
       Image_Save_all_HTML, Event, /bw, /PNG
       end

    'Import_Stack_Menu': begin
       Image_Import_stack, Event
       end

    'Import_Stack_Dir_Menu': begin
       Image_Import_stack, Event, /dir
       end

    'Import_Stack_Flatten_Menu': begin
       Image_Import_stack, Event, /flatten
       end

    'Import_Stack_Dir_Flatten_Menu': begin
       Image_Import_stack, Event, /dir, /flatten
       end

	'Import_Stack_Energies_Menu': begin
		Image_Import_Stack_energies, Event
		end
		
    'Import_BMP_Menu': begin
       Image_Import, Event, type=0
       end

    'Import_PNC_Menu': begin
       Image_Import, Event, type=1
       end

    'Import_PEF_Menu': begin
       Image_Import, Event, type=2
       end

    'Import_PNG_Menu': begin
       Image_Import, Event, type=3
       end

    'Import_TIF_Menu': begin
       Image_Import, Event, type=4
       end

    'Save_All_HTML_TIFF0_Menu': begin
       Image_Save_all_TIFF, Event, mode=0
       end
    'Save_All_HTML_TIFF0_VAR_Menu': begin
       Image_Save_all_TIFF, Event, mode=0, /var
       end
    'Save_All_HTML_TIFF0_TXM_Menu': begin
       Image_Save_all_TIFF, Event, mode=0, /txm
       end
    'Save_All_HTML_TIFF1_Menu': begin
       Image_Save_all_TIFF, Event, mode=1
       end
    'Save_All_HTML_TIFF2_Menu': begin
       Image_Save_all_TIFF, Event, mode=2
       end

    'Export_ASCII_Menu': begin
       Image_Export_CSV, Event
       end

;    'Export_CSV_Region_Menu': begin
;       Image_Export_CSV, Event, /region
;       end
;
;    'Export_TAB_Menu': begin
;       Image_Export_CSV, Event, /tab
;       end
;
;    'Export_TAB_Region_Menu': begin
;       Image_Export_CSV, Event, /region, /tab
;       end
;
;    'Export_XY_CSV_Menu': begin
;       Image_Export_CSV, Event, /xy
;       end
;
;    'Export_XY_CSV_Region_zero_Menu': begin
;       Image_Export_CSV, Event, /region, /xy, clip_zero=0
;       end
;
;    'Export_XY_CSV_Region_Menu': begin
;       Image_Export_CSV, Event, /region, /xy
;       end
;
;    'Export_XY_TAB_Menu': begin
;       Image_Export_CSV, Event, /tab, /xy
;       end
;
;    'Export_XY_TAB_Region_Menu': begin
;       Image_Export_CSV, Event, /region, /tab, /xy
;       end
;
;    'Export_XY_TAB_Region_zero_Menu': begin
;       Image_Export_CSV, Event, /region, /tab, /xy, clip_zero=0
;       end

	'XANES_energy_list_Menu': begin
       Image_Export_XANES_energies, Event
		end
	
	'retry_image_increment_Menu': begin
		retry_image_increment, group=event.top, path=(*pstate).path
		end
		
    'Save_CGM_Menu': begin
       Image_Export, Event, /CGM
       end

    'Plot_PNG_Menu': begin
       Image_Export, Event, /PNG
       end

    'Save_WMF_Menu': begin
       Image_Export, Event, /WMF
       end

    'Save_All_CGM_Menu': begin
       Image_Export, Event, /CGM, /All
       end

    'Plot_All_PNG_Menu': begin
       Image_Export, Event, /PNG, /All
       end

    'Save_All_WMF_Menu': begin
       Image_Export, Event, /WMF, /All
       end

    'Plot_JPEG_Menu': begin
       Image_Export, Event, /JPEG
       end

    'Convert_MDA_Menu': begin
       aps_to_list, group=event.top
       end

    'Plot_Menu': begin
       Image_results_Plot, Event
       end

    'Print_Menu': begin
       Image_Export, Event
       end

    'Print_All_Menu': begin
       Image_Export, Event, /All
       end

    'Exit_Menu': begin
       Image_Exit, Event
       end

    'Clone_Menu': begin
       Image_Clone, Event
       end

    'Clone_Menu_1': begin
       Image_Clone, Event, 1
       end

    'Clone_Menu_3': begin
       Image_Clone, Event, 3
       end

    'Clone_Menu_7': begin
       Image_Clone, Event, 7
       end

    'Clone_Menu_11': begin
       Image_Clone, Event, 11
       end

    'Multi_Image_Menu': begin
       Image_Multi_Image, Event
       end

    'Xanes_Image_Menu': begin
       Image_xanes, Event
       end

    'Select_Menu': begin
       Image_Select, Event
       end

    'Colour_Table': begin
       Image_Colours, Event
       end

    'Default_Colour_Table': begin
       Image_Colours, Event, 5
       end

    'TenStep_Colour_Table': begin
       Image_Colours, Event, -1
       end

    'Grey_Scale': begin
       Image_Colours, Event, 0
       end

    'Invert_Colour_Table': begin
       Image_Invert_Colours, Event
       end

	'Red_Colour_Blind_On': begin
		Image_Colour_blind, Event, 1
		end
		
	'Red_Colour_Blind_Off': begin
		Image_Colour_blind, Event, 0
		end
		
    'Linear_Luminance': begin
       Image_Linear_Luminance, Event
       end

    'Clear_All_Marks_Menu': begin
       Clear_All_Image_Marks, Event
       end

    'Clear_Corr_Menu': begin
       Clear_Image_Corr, Event
       end

    'Include_Menu': begin
       Image_Include, Event
       end

    'Exclude_Menu': begin
       Image_Exclude, Event
       end

    'Box_Menu': begin
       image_shape, shape=1, Event
       end

    'Circle_Menu': begin
       image_shape, shape=2, Event
       end

    'Curve_Menu': begin
       image_shape, shape=3, Event
       end

    'Line_Menu': begin
       image_shape, shape=4, Event
       end

    'Ellipse_Menu': begin
       image_shape, shape=5, Event
       end

    'Spline_10_Menu': begin
       image_shape, shape=6, Event
       end

    'Spline_32_Menu': begin
       image_shape, shape=7, Event
       end

    'Project_X_Menu': begin
       image_shape, shape=8, Event
       end

    'Project_Y_Menu': begin
       image_shape, shape=9, Event
       end

    'Spline_100_Menu': begin
       image_shape, shape=10, Event
       end

    'Single_pixel_Menu': begin
       image_shape, shape=11, Event
       end

	'reset_display_range': begin
		image_reset_display_range, event
		end
		
    'Analyze_Menu': begin
       Image_Analyze, Event
       end

    'Throttle_Menu': begin
       Image_Analyze, Event, /throttle
       end

    'Median_Menu-2': begin
       Image_Process_Median, Event, 2
       end

    'Median_Menu-3': begin
       Image_Process_Median, Event, 3
       end

    'Median_Menu-5': begin
       Image_Process_Median, Event, 5
       end

    'Median_Menu-10': begin
       Image_Process_Median, Event, 10
       end

    'Boxcar_Menu-2': begin
       Image_Process_Boxcar, Event, 2
       end

    'Boxcar_Menu-3': begin
       Image_Process_Boxcar, Event, 3
       end

    'Boxcar_Menu-5': begin
       Image_Process_Boxcar, Event, 5
       end

    'Boxcar_Menu-10': begin
       Image_Process_Boxcar, Event, 10
       end

    'Gaussian_Menu-1': begin
       Image_Process_Gaussian, Event, 1.0
       end

    'Gaussian_Menu-1.5': begin
       Image_Process_Gaussian, Event, 1.5
       end

    'Gaussian_Menu-2': begin
       Image_Process_Gaussian, Event, 2.0
       end

    'Gaussian_Menu-3': begin
       Image_Process_Gaussian, Event, 3.0
       end

    'Gaussian_Menu-5': begin
       Image_Process_Gaussian, Event, 5.0
       end

    'Gaussian_Menu-10': begin
       Image_Process_Gaussian, Event, 10.0
       end

    'Roberts_Menu': begin
       Image_Process_Roberts, Event
       end

    'Sobel_Menu': begin
       Image_Process_Sobel, Event
       end

    'Dilate_Menu-2': begin
       Image_Process_Dilate, Event, 2
       end

    'Dilate_Menu-5': begin
       Image_Process_Dilate, Event, 5
       end

    'Dilate_Menu-10': begin
       Image_Process_Dilate, Event, 10
       end

    'Clear_Menu-1': begin
       Image_Process_Clear, Event, 1, /no_undo, select=-1
       end

    'Clear_Menu-2': begin
       Image_Process_Clear, Event, 2, /no_undo, select=-1
       end

    'Clear_Menu-5': begin
       Image_Process_Clear, Event, 5, /no_undo, select=-1
       end

    'Clear_Menu-10': begin
       Image_Process_Clear, Event, 10, /no_undo, select=-1
       end

    'Clear_Menu-sides-1': begin
       Image_Process_Clear_sides, Event, 1, /no_undo, select=-1
       end

    'Clear_Menu-sides-2': begin
       Image_Process_Clear_sides, Event, 2, /no_undo, select=-1
       end

    'Clear_Menu-sides-5': begin
       Image_Process_Clear_sides, Event, 5, /no_undo, select=-1
       end

    'Clear_Menu-sides-10': begin
       Image_Process_Clear_sides, Event, 10, /no_undo, select=-1
       end

    'Erode_Menu-2': begin
       Image_Process_Erode, Event, 2
       end

    'Erode_Menu-5': begin
       Image_Process_Erode, Event, 5
       end

    'Erode_Menu-10': begin
       Image_Process_Erode, Event, 10
       end

    'Scale_X-50': begin
       Image_Process_Scale, Event, 0, 0.5
       end

    'Scale_X-75': begin
       Image_Process_Scale, Event, 0, 0.75
       end

    'Scale_X-90': begin
       Image_Process_Scale, Event, 0, 0.9
       end

    'Scale_X-110': begin
       Image_Process_Scale, Event, 0, 1.1
       end

    'Scale_X-150': begin
       Image_Process_Scale, Event, 0, 1.5
       end

    'Scale_X-200': begin
       Image_Process_Scale, Event, 0, 2.0
       end

    'Scale_X-300': begin
       Image_Process_Scale, Event, 0, 3.0
       end

    'Scale_Y-90': begin
       Image_Process_Scale, Event, 1, 0.9
       end

    'Scale_Y-50': begin
       Image_Process_Scale, Event, 1, 0.5
       end

    'Scale_Y-75': begin
       Image_Process_Scale, Event, 1, 0.75
       end

    'Scale_Y-110': begin
       Image_Process_Scale, Event, 1, 1.1
       end

    'Scale_Y-150': begin
       Image_Process_Scale, Event, 1, 1.5
       end

    'Scale_Y-200': begin
       Image_Process_Scale, Event, 1, 2.0
       end

    'Scale_Y-300': begin
       Image_Process_Scale, Event, 1, 3.0
       end

    'Scale_XY-110': begin
       Image_Process_Scale, Event, 2, 1.1
       end

    'Scale_XY-150': begin
       Image_Process_Scale, Event, 2, 1.5
       end

    'Scale_XY-200': begin
       Image_Process_Scale, Event, 2, 2.0
       end

    'Scale_XY-300': begin
       Image_Process_Scale, Event, 2, 3.0
       end

    'Scale_XY-50': begin
       Image_Process_Scale, Event, 2, 0.5
       end

    'Scale_XY-90': begin
       Image_Process_Scale, Event, 2, 0.9
       end


	'Shift-even+0.5': begin
       Image_Process_Shift_rows, Event, +0.5, /x, /even
       end

	'Shift-even-0.5': begin
       Image_Process_Shift_rows, Event, -0.5, /x, /even
       end

	'Shift-odd+0.5': begin
       Image_Process_Shift_rows, Event, +0.5, /x, /odd
       end

	'Shift-odd-0.5': begin
       Image_Process_Shift_rows, Event, -0.5, /x, /odd
       end

    'Shift+1': begin
       Image_Process_Shift_rows, Event, +1.0, /x, /odd
       end

    'Shift+2': begin
       Image_Process_Shift_rows, Event, +2.0, /x, /odd
       end

    'Shift+5': begin
       Image_Process_Shift_rows, Event, +5.0, /x, /odd
       end

    'Shift-1': begin
       Image_Process_Shift_rows, Event, -1.0, /x, /odd
       end

    'Shift-2': begin
       Image_Process_Shift_rows, Event, -2.0, /x, /odd
       end

    'Shift-5': begin
       Image_Process_Shift_rows, Event, -5.0, /x, /odd
       end



	'Shift-ytest-even+0.5': begin
       Image_Process_Shift_rows, Event, +0.5, /x, /even, /test_ysize
       end

	'Shift-ytest-even-0.5': begin
       Image_Process_Shift_rows, Event, -0.5, /x, /even, /test_ysize
       end

	'Shift-ytest-odd+0.5': begin
       Image_Process_Shift_rows, Event, +0.5, /x, /odd, /test_ysize
       end

	'Shift-ytest-odd-0.5': begin
       Image_Process_Shift_rows, Event, -0.5, /x, /odd, /test_ysize
       end

    'Shift-ytest+1': begin
       Image_Process_Shift_rows, Event, +1, /x, /odd, /test_ysize
       end

    'Shift-ytest+2': begin
       Image_Process_Shift_rows, Event, +2, /x, /odd, /test_ysize
       end

    'Shift-ytest+5': begin
       Image_Process_Shift_rows, Event, +5, /x, /odd, /test_ysize
       end

    'Shift-ytest-1': begin
       Image_Process_Shift_rows, Event, -1, /x, /odd, /test_ysize
       end

    'Shift-ytest-2': begin
       Image_Process_Shift_rows, Event, -2, /x, /odd, /test_ysize
       end

    'Shift-ytest-5': begin
       Image_Process_Shift_rows, Event, -5, /x, /odd, /test_ysize
       end


    'Shift-all+1': begin
       Image_Process_Shift, Event, +1, /all
       end

    'Shift-all+2': begin
       Image_Process_Shift, Event, +2, /all
       end

    'Shift-all+5': begin
       Image_Process_Shift, Event, +5, /all
       end

    'Shift-all-1': begin
       Image_Process_Shift, Event, -1, /all
       end

    'Shift-all-2': begin
       Image_Process_Shift, Event, -2, /all
       end

    'Shift-all-5': begin
       Image_Process_Shift, Event, -5, /all
       end

    'Shift-columns+1': begin
       Image_Process_Shift_rows, Event, +1.0, /y, /odd
       end

    'Shift-columns-1': begin
       Image_Process_Shift_rows, Event, -1.0, /y, /odd
       end


    'Flip-X': begin
       Image_Process_Rotate, Event, /flipX
       end

    'Flip-Y': begin
       Image_Process_Rotate, Event, /flipY
       end

    'Rotate+90': begin
       Image_Process_Rotate, Event, 90.0
       end

    'Rotate-90': begin
       Image_Process_Rotate, Event, -90.0
       end

    'Clip-Right': begin
       Image_Process_Clip, Event, /right
       end

    'Clip-Top': begin
       Image_Process_Clip, Event, /top
       end

    'Clip-Crop': begin
       Image_Process_Crop, Event
       end

    'Clip-Crop-Zero': begin
       Image_Process_Crop, Event, /zero
       end

    'Correct-X-Current': begin
       Image_Process_Correct_Current, Event, 0
       end

	'Correct-X-Current-file': begin
       Image_Process_Correct_Current_file, Event, 0
       end

    'Correct-Y-Current': begin
       Image_Process_Correct_Current, Event, 1
       end

	'Correct-Y-Current-file': begin
       Image_Process_Correct_Current_file, Event, 1
       end

	'Correct-Y-offset': begin
       Image_Process_Correct_Ydrift, Event
		end
		
    'Correct-Image-Pileup': begin
       notify, 'correct-image-pileup', from=event.top
       end

    'Missing-Rows': begin
       Image_Process_Missing_Lines, Event, 0
       end

    'Double-Rows': begin
       Image_Process_Missing_Lines, Event, 1
       end

    'Missing-Cols': begin
       Image_Process_Missing_Lines, Event, 0, /vertical
       end

    'Double-Cols': begin
       Image_Process_Missing_Lines, Event, 1, /vertical
       end

    'Zeroes': begin
       Image_Process_Zeroes, Event
       end

    'Ghost-30': begin
       Image_Process_Ghost, Event, -0.3
       end

    'Ghost-10': begin
       Image_Process_Ghost, Event, -0.1
       end

    'Ghost-5': begin
       Image_Process_Ghost, Event, -0.05
       end

    'Ghost-3': begin
       Image_Process_Ghost, Event, -0.03
       end

    'Ghost-1': begin
       Image_Process_Ghost, Event, -0.01
       end

    'Ghost+1': begin
       Image_Process_Ghost, Event, +0.01
       end

    'Suppress-Region': begin
       Image_Process_Suppress_Region, Event, 0.7
       end

    'Kill-Region': begin
       Image_Process_Suppress_Region, Event, 0.0
       end

    'Kill-Region-all': begin
       Image_Process_Suppress_Region, Event, 0.0, /all
       end

    'Kill-All-Region-all': begin
       Image_Process_Kill_All_Region, Event
       end

    'Enhance-Region': begin
       Image_Process_Suppress_Region, Event, 1.2
       end

    'flux-remove': begin
       Image_Process_Flux_remove, Event
       end

    'flux-apply': begin
       Image_Process_Flux_apply, Event
       end

    'Merge_Gamma_Menu': begin
       Image_Process_Merge_Gamma, Event
       end

    'Plugin': begin
       Image_Process_Plugin, Event
       end

    'Reload-Plugins': begin
       Image_Reload_Plugins, Event
    	end

    'Wizard': begin
       Image_Process_Wizard, Event
       end

    'Undo_Menu': begin
       Image_Restore_undo, Event
       end

	'Reset_Menu': begin
		parallel_reset
		end
		
    'Test_Menu': begin
       Image_Process_Test, Event
       end

    'Snapshot_Menu': begin
       notify, 'snapshot'
       end

    'Plot_Results_Menu': begin
       Image_Plot_Results, Event
       end

    'Results_Table_Menu': begin
       Image_Results_Table, Event
       end

    'Select_Image_Menu': begin
       Image_Select_Image, Event
       end

    'Image_Operations_Menu': begin
       Image_Image_Operations, Event
       end

    'Image_History_Menu': begin
       Image_History_Window, Event
       end

    'Image_History_Stats_Menu': begin
       Image_History_Window, Event, /stats
       end

    'RGB_Menu': begin
       Image_Image_RGB, Event
       end

    'Spectrum_Display_Menu': begin
       Image_Spectrum_Display, Event
       end

    'Corr_Menu': begin
       Image_Corr_Display, Event
       end

    'Energy_Corr_Menu': begin
       Image_Corr_Display, Event, /energy
       end

    'PCA_cluster_Menu': begin
       Image_pca_cluster_Display, Event
       end

	'Interelement_Menu': begin
       Image_interelement_operations, Event
		end

	'Correct-interelement': begin
       Image_interelement_operations, Event
		end

	'Correct-Y-ripples-5': begin
       Image_process_correct_Y_ripples, Event, 5
		end

	'Correct-Y-ripples-10': begin
       Image_process_correct_Y_ripples, Event, 10
		end

	'Correct-Y-ripples-20': begin
       Image_process_correct_Y_ripples, Event, 20
		end

	'Correct-Y-ripples-50': begin
       Image_process_correct_Y_ripples, Event, 50
		end

	'Correct-Y-ripples-100': begin
       Image_process_correct_Y_ripples, Event, 100
		end

    'Sim_Setup_Menu': begin
		restore, geopixe_root+'sim_pixe_xrf.sav'
       Sim_Setup_geopixe, group=event.top
       end

    'Command_Menu': begin
       Command, group=event.top
       end

    'EVT_Menu': begin
       Image_EVT, event
       end

    'Correct_Menu': begin
       Image_Correct_Yield, event, /small
       end

    'Correct_Big_Menu': begin
       Image_Correct_Yield, event, /big
       end

    'Project_Menu': begin
       Image_Correct_Yield, event
       end

    'Edit_Filters_Menu': begin
       Image_Edit_Filters, event
       end

    'Edit_Detectors_Menu': begin
       Image_Edit_Detectors, event
       end

    'New_Image': begin
       gimage, /noprefs, test=test, debug=debug
       end
    'New_Spectrum': begin
       spectrum_display, test=test
       end

    'Blog_Browser_Menu': begin
       Image_Blog_Browser, event
       end

	'Compare_yields_Menu': begin
		compare_yields
		end
		
	'Compare_sources_Menu': begin
		compare_source
		end

	'Compare_pink_Menu': begin
		compare_pink
		end

	'Compare_fits_Menu': begin
		compare_fits
		end

   'Help_User': begin
	   	warning,'image',['','GeoPIXE Information (see the "Help" directory):', $
	   		'','   Reference and users guide:', $
	   		'        See the "GeoPIXE Users Guide.pdf".', $
	   		'        New features: "Update Notes.pdf".', $
	   		'','   Workshop notes and Worked examples:', $
	   		'        "GeoPIXE Worked Examples.pdf".', $
	   		'','   View the workshop talk slides:', $
			'        "GeoPIXE-...-wshop1.pdf", ', $
	   		'        "GeoPIXE-...-wshop2.pdf".', $
	   		'','   Technical notes (see "Help" dir):', $
	   		'','Maia detector information:', $
	   		'','   Technical description and set-up:', $
	   		'        See "Maia-384-user-help.pdf".'], /info
;       case !version.os_family of
;	   	'Windows': begin
;			hfile2 = geopixe_root+'Help'+path_sep()+'GeoPIXE Users Guide.htm'
;			hfile = geopixe_root+'Help'+path_sep()+'GeoPIXE-Users-Guide.htm'
;			if file_test(hfile) then begin
;				s = 'Browser.lnk "' + hfile + '"'
;				spawn, s, /nowait, /hide
;			endif else begin
;				s = 'Browser.lnk "' + hfile2 + '"'
;				spawn, s, /nowait, /hide
;			endelse
;			end
;		'unix': begin
;			hfile2 = geopixe_root+'Help'+path_sep()+'GeoPIXE-Users-Guide.htm'
;			s = 'Browser "' + hfile2 + '" &'
;			spawn, s
;			end
;         else: begin
;		 	warning,'image_event',['Spawn not supported on this platform.','Open help file manually.']
;			end
;       endcase
       end

   'Help_Maia_User': begin
	   	warning,'image',['','Maia detector information:', $
	   		'     Technical description, set-up and calibration:', $
	   		'          See "Maia-384-user-help.pdf" in the Help directory.'], /info
;       case !version.os_family of
;	   	'Windows': begin
;			hfile = geopixe_root+'Help'+path_sep()+'Maia-384-user-help.htm'
;			if file_test(hfile) then begin
;				s = 'Browser.lnk "' + hfile + '"'
;				spawn, s, /nowait, /hide
;			endif
;			end
;		'unix': begin
;			hfile = geopixe_root+'Help'+path_sep()+'Maia-384-user-help.htm'
;			s = 'Browser "' + hfile + '" &'
;			spawn, s
;			end
;         else: begin
;		 	warning,'image_event',['Spawn not supported on this platform.','Open help file manually.']
;			end
;       endcase
       end

	'Update_GeoPIXE': begin
		Image_update_geopixe, event
		end
			
    'Help_Query': begin
		restore, geopixe_root+'idl_query.sav'
       IDL_Query_geopixe
       end

    'About': begin
       warning,'GeoPIXE','Version: '+(*pstate).version, /info
       end

    else:
  endcase

finish:
  widget_control, hourglass=0
  return

bad_state:
    warning,'Image_Event','State variable undefined, exit ...'
    image_Exit, Event
    return
end

;-----------------------------------------------------------------------------------

;pro main
;
;if !version.os_family eq 'Windows' then device, decomposed=0
;if !version.os_family eq 'unix' then device, true_color=24, decomposed=0
;if !version.os_family eq 'MacOS' then device, decomposed=0
;
;   gimage
;   return
;end

;-----------------------------------------------------------------------------------

pro runtime

;	See comments in "geopixe" below ...

	argv = command_line_args( count=argc)
	if argc ge 1 then begin

		startupp, /database, /colours, /devices

		geopixe_do_commands, argc=argc, argv=argv
		return
	endif

	startupp, /error
    gimage
    return
end

;-----------------------------------------------------------------------------------

pro test_geopixe_loaded

    return
end

;-----------------------------------------------------------------------------------

; When called from "geopixe.sav" at runtime.

pro geopixe

; GeoPIXE data-flow module.
;
; Pass a command file-name to execute it and return.
; See "geopixe_do_commands".
;
; Read GCF command file and execute the enclosed command and arguments.
; 
; argv[0]	command file (.gcf), else a simple command
; argv[1]	optional file list to replace "files=" line in GCF file.
;			preceed with "@" to supply a file-name containing the file-list,
;			with one input file-name per line.
; argv[2]	optional 'output' file-name to replace "output=" line in GCF file.
;			usually, both argv[1] and argv[2] will be supplied together.
;
; File list should be in stringify format. Will there be a character limit?
; If so, then may need the alternate approach (e.g. "@file", where "@" signifies
; reading the file list from this "file").
;
; If the first argument is not a GCF file (no “.gcf” extension found), 
; then the argument will be interpreted as a GeoPIXE procedure and execute it, 
; passing any supplied parameters.


	argv = command_line_args( count=argc)
	if argc ge 1 then begin
		if argv[0] ne '' then begin

;			Choice here is minimum to get process going.

			startupp, /database, /colours, /devices

			geopixe_do_commands, argc=argc, argv=argv
			return
		endif
	endif

    gimage
    return
end

;-----------------------------------------------------------------------------------

pro geopixe_new

	startupp, /error

    gimage
    return
end

;-----------------------------------------------------------------------------------

pro geopixe_test

	startupp, /error

    gimage, /test
    return
end

;-----------------------------------------------------------------------------------

pro gimage, GROUP_LEADER=wGroup, TLB=Image_TLB, clone=clone, path=path, _EXTRA=_VWBExtra_, $
         debug=debug, test=test, nosav=nosav, noprefs=noprefs, $
         dpath=dpath, realtime=realtime, ppercent=ppercent, xanes=xanes, no_database=no_database

;   /debug       enable debug mode, stop execution at errors, suppress popup warnings
;   /nosav       do not load sav files for plugins/wizards. Use this to build initial GeoPIXE.sav.
;   /test     	 enable features under test, else they remain hidden.
;   /no_database	do not load geopixe2.sav database. Force reading all database source files
;					(make sure database folder path names are correct - not "off").
;	/xanes		 3D stack mode (XANES images, tomo, ...)

COMPILE_OPT STRICTARR
common c_image_start, image_compiled
common c_debug_warnings, enable_warning_popup
common c_errors_1, catch_errors_on
common c_test, c_test
common c_gif, use_gif
common c_531, idl_531
common c_pige, use_PIGE_yields
common c_working_dir, geopixe_root
common c_geopixe_adcs, geopixe_max_adcs
common c_geopixe_vm, geopixe_enable_vm
common c_fit_memory_1, fit_memory_on

if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384
if n_elements(c_test) lt 1 then c_test=0
if n_elements(geopixe_enable_vm) lt 1 then geopixe_enable_vm=1
if n_elements(wGroup) lt 1 then wGroup=0L
if n_elements(clone) lt 1 then clone=0
if n_elements(debug) lt 1 then debug=0
if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=path
if n_elements(nosav) lt 1 then nosav=0
if n_elements(noprefs) lt 1 then noprefs=0
if n_elements(realtime) lt 1 then realtime=0
if n_elements(ppercent) eq 0 then ppercent=ptr_new(0.0)
if n_elements(xanes) lt 1 then xanes=0
if xanes then noprefs=1
if n_elements(no_database) lt 1 then no_database=0

geopixe_enable_vm = 1               ; enable use of VM
use_PIGE_yields = 1                 ; set for empirical CSIRO PIGE yields factors.
enable_warning_popup = 1            ; enable popup warnings
geopixe_max_adcs = 384				; 
fit_memory_on = 0					; enable fit parameter memory
run_tim = 0
version = geopixe_version()			; Version text

if clone eq 0 then begin

	startupp, /colours, /database					  ; setup IDL graphics and database
	if (database_alive() ne 1) and (no_database eq 0) then begin
		warning,'GeoPIXE','failed to load "geopixe2.sav"'
	endif
	if realtime eq 0 then begin
	    catch_errors_on = 1                           ; enable error CATCHing
	    if debug then catch_errors_on = 0             ; disable error CATCHing
	    define_devices
	endif
endif
if n_elements(test) lt 1 then test=c_test

ErrorNo = 0
debug_line = 'start'					; helps to see how far we got when a fatal crash occurs
										; as it gets displayed in the error catch warning popup.
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'GeoPIXE',['IDL run-time error caught (init).', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.','','Debug line: '+debug_line], /error
       MESSAGE, /RESET
       return
    endif
endif else on_error,0

if (clone eq 0) and (realtime eq 0) then splash, title='GeoPIXE - Quantitative PIXE/SXRF Imaging and Analysis', timeout=4

lib = geopixe_library(version=lib_ver)
print,'Startup GeoPIXE "gimage" version = ',version + ' (Library version:' +str_tidy(lib_ver) + ')'

note_on = 0
if note_on and debug then warning,'GeoPIXE',['Hey! You wanted to remember this:', $
                        '...']

use_gif = 0
idl_531 = 0
new_idl = 1
if float(!version.release) lt 5.4 then begin
    use_gif = 1                   ; enable GIF output, suppress PNG
    idl_531 = 1                   ; use PNG only with IDL 5.4 (mirror bug)
    new_idl = 0
endif
if new_idl eq 0 then warning,'GeoPIXE',['Some features of GeoPIXE not supported', $
                        'in versions of IDL prior to 5.6.']

;	if n_elements( image_compiled) lt 1 then begin
	image_routines							; Load support routines
	image_eventcb							; Load event callback routines
	register_notify							; notification routines
	interelement_operations					; to define 'INTERELEMENT_TRANSFORM'
;	image_compiled = 1
;	endif

  case !version.os_family of
    'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
       draw_trim = 15
       scr_trim = 21
       help1_xsize = 339
       mode_xsize = 63
       slide_xsize = 95			; 83
       button_height = 21
       xsize_element = 80
       xsize_analyze_type = 124
       xsize_analyze_mode = 48
       xsize_loglin = 73
       help2_scr_ysize = 61
       end
    'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
       draw_trim = 0
       scr_trim = 0			; 15
       help1_xsize = 361
       mode_xsize = 63
       slide_xsize = 95			; 85
       button_height = 21
       xsize_element = 80
       xsize_analyze_type = 132
       xsize_analyze_mode = 54
       xsize_loglin = 79
       help2_scr_ysize = 60
       end
    else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
       draw_trim = 0
       scr_trim = 15			; 15
       help1_xsize = 359		; 261
       mode_xsize = 55
       slide_xsize = 101			; 92			; 83
       button_height = 21
       xsize_element = 73		; 44
       xsize_analyze_type = 124
       xsize_analyze_mode = 48
       xsize_loglin = 63
       help2_scr_ysize = 58
      end
  endcase
;@2  widget_control, default_font=def_font        ; set font for all windows (no, let system manage this)

  debug_line = 'load plugins ...'
  add_plugins = 0
  plugins = Image_Load_Plugins( error=error)
  if error eq 0 then add_plugins = 1
  add_wizards = 0
  wizards = wizard_load_plugins( error=error)
  if error eq 0 then add_wizards = 1
  debug_line = 'plugins done'

;	If KVS enabled in the "geopixe.conf" file, then connect to the key-value store (KVS) using
;	the MM ZMQ library tools. This is for Maia Mapper support.

  kvs = 0
  kvs_prefix = 'off'
  kvs2 = 0
  kvs_prefix2 = 'off'
  comms = 0L
  comms2 = 0L
  debug_line = 'KVS ...'
  shape_file = ''
  if (clone eq 0) and (noprefs eq 0) then begin
  	debug_line = 'KVS geopixe_prefs ...'
  	prefs = geopixe_defaults( error=err, source='image')
  	debug_line = 'KVS geopixe_prefs done.'
	if err eq 0 then begin
		if strlen(prefs.path.analysis) gt 0 then path = prefs.path.analysis
		if strlen(prefs.path.data) gt 0 then dpath = prefs.path.data

		if prefs.shape.enable then begin
			shape_file = fix_path( prefs.shape.path) + prefs.shape.file
		endif

		if prefs.kvs.enable then begin
			if python_valid( message=mess) eq 0 then begin
				warning,'gimage',['KVS has been enabled for Maia Mapper in your "geopixe.conf" file.', $
					'However, python version found is not compatible with IDL.', $
					'Maia Mapper Libs need python 2.7 with IDL 8.5.1, or '+ mess, $
					'so configure for correct IDL + python combination or disable KVS in the "geopixe.conf" file.','', $
					'Continue without KVS and MM Libs ...']
				prefs.kvs.enable = 0
			endif
		endif

		if prefs.kvs.enable then begin
			print,'Open KVS using endpoint: ', prefs.kvs.endpoint
			comms = open_comms( error=err)
			if err then begin
				warning,'image',['Failed to open a MM comms object.']
			endif
			kvs = open_kvs(prefs.kvs.endpoint, comms=comms, error=err)
			if err then begin
				warning,'image',['Failed to open the KVS endpoint: ',prefs.kvs.endpoint]
			endif
			kvs_prefix = prefs.kvs.prefix
			
			check_kvs, kvs, error=err
			if err then begin
				warning,'image',['Check of KVS failed.','Will close and ignore KVS.']
				close_kvs, kvs
			endif			

			if (err eq 0) and (prefs.kvs.endpoint2 ne '') then begin
				print,'Open KVS 2 using endpoint: ', prefs.kvs.endpoint2

				print,'Open comms 2 object ...'
				comms2 = open_comms( error=err)
				if err then begin
					warning,'image',['Failed to open a comms 2 object.']
				endif
				print,'Open KVS 2  ...'
				kvs2 = open_kvs(prefs.kvs.endpoint2, comms=comms2, error=err)
				if err then begin
					warning,'image',['Failed to open the KVS 2 endpoint: ',prefs.kvs.endpoint2]
				endif
				kvs_prefix2 = prefs.kvs.prefix2
				
				print,'Check KVS 2 ...'
				check_kvs, kvs2, error=err
				if err then begin
					warning,'image',['Check of KVS 2 failed.','Will close and ignore KVS 2.']
					close_kvs, kvs2
				endif			
			endif
		endif else begin
			debug_line = 'null KVS ...'
			kvs = open_kvs( 'null')			; get it compiled, but just return 0L
		endelse
	endif
	prefs.test = test
	prefs.debug = debug
	prefs.slave = realtime
  endif
  debug_line = 'KVS done'
  print,'KVS done, start GeoPIXE GUI ...'

  title = 'GeoPIXE ' + version + ' (Lib: ' +str_tidy(lib_ver) + ')'
  if realtime then title='Maia Image Display  (GeoPIXE ' + version + ')'
  if xanes then title='3D Image Stack'
  Image_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Image_TLB' ,/TLB_KILL_REQUEST_EVENTS,  $
  		/TLB_SIZE_EVENTS ,TITLE=title ,SPACE=2 ,XPAD=1 ,YPAD=1, /base_align_center,  $
		/COLUMN ,MBAR=Image_TLB_MBAR, _EXTRA=_VWBExtra_)


  Image_Draw_Base = Widget_Base(Image_TLB, UNAME='Image_Draw_Base' ,SPACE=0  $
      ,XPAD=0 ,YPAD=0 ,ROW=1)

  if !version.os_family eq 'unix' then begin
    retain=2
  endif else begin
    retain=1
  endelse

  Image_Draw = Widget_Draw(Image_Draw_Base, UNAME='Image_Draw'  $
      ,SCR_XSIZE=360+scr_trim ,SCR_YSIZE=360+scr_trim  $
      ,NOTIFY_REALIZE='OnRealize_Image'  $
      ,KILL_NOTIFY='OnDestroy_Image' ,/SCROLL ,XSIZE=360+draw_trim  $
      ,YSIZE=360+draw_trim, retain=retain, /BUTTON_EVENTS,/VIEWPORT_EVENTS)


  Image_Button_Base = Widget_Base(Image_TLB, UNAME='Image_Button_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,column=1, /align_center, /base_align_center)


  Image_Button_Base1 = Widget_Base(Image_Button_Base, UNAME='Image_Button_Base1'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /align_center, /base_align_center)


  Image_Button_Base2 = Widget_Base(Image_Button_Base1, UNAME='Image_Button_Base2'  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/column, /align_center, /base_align_center)


  Image_Button_Top_Base = Widget_Base(Image_Button_Base2, UNAME='Image_Button_Top_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_center)


  Image_Button_Bot_Base = Widget_Base(Image_Button_Base2, UNAME='Image_Button_Bot_Base'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_bottom, /base_align_bottom)


  Image_Help1_Base = Widget_Base(Image_Button_Base, UNAME='Image_Help1_Base', map=1  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center, /base_align_center)


  Image_Help2_Base = Widget_Base(Image_Button_Base1, UNAME='Image_Help2_Base', map=0  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  debug_line = 'Postcreate ...'
  PostCreate_Image_Base, Image_Draw_Base, Image_Help1_Base, Image_Help2_Base, path=path, $
       parent=wGroup, clone=clone, test=test, debug=debug, prefs=prefs, dpath=dpath, $
       _EXTRA=_VWBExtra_, wizards=wizards, plugins=plugins, realtime=realtime, ppercent=ppercent, $
       version=version, xanes=xanes, kvs_obj1=kvs, kvs_prefix1=kvs_prefix, kvs_obj2=kvs2, $
	   kvs_prefix2=kvs_prefix2, shape_file=shape_file

  PostCreate_Image, Image_Draw, _EXTRA=_VWBExtra_
  debug_line = 'Postcreate done'

  if xanes then begin
	 els = ['9.0','10.0']
	 ehelp = 'Droplist to select the Stack (E,angle) image plane to display.'
  endif else begin
	  els = [ '  ', ' Sm ' ]
	  ehelp = 'Droplist to select the element image to display.'
  endelse
  
  Element_combobox = widget_combobox(Image_Button_Top_Base,  $
      UNAME='Element_combobox' ,NOTIFY_REALIZE='OnRealize_Element',  $
      VALUE=els, tracking_events=1, xsize=xsize_element, $
      uvalue=ehelp)

  space1 = widget_base(Image_Button_Top_Base, xsize=1)

  Full_Button = Widget_Button(Image_Button_Top_Base, UNAME='Full_Button'  $
      ,/ALIGN_CENTER ,VALUE='0', tracking_events=1, font=large_font, $
      uvalue='Show original size of image in window.',xsize=20, ysize=button_height)


  Zoom_In_Button = Widget_Button(Image_Button_Top_Base, UNAME='Zoom_In_Button'  $
      ,/ALIGN_CENTER ,VALUE='+', tracking_events=1, font=large_font, $
      uvalue='Magnify image view x2.',xsize=20, ysize=button_height)


  Zoom_Out_Button = Widget_Button(Image_Button_Top_Base, UNAME='Zoom_Out_Button'  $
      ,/ALIGN_CENTER ,VALUE='-', tracking_events=1, font=large_font, $
      uvalue='Demagnify image view x2.',xsize=20, ysize=button_height)


  space2 = widget_base(Image_Button_Top_Base, xsize=1)


  Analyze_Type = widget_combobox(Image_Button_Top_Base, xsize=xsize_analyze_type,  $
      UNAME='Analyze_Type_combobox' ,NOTIFY_REALIZE='OnRealize_Analyze_Type'  $
      ,VALUE=[ 'Distance', 'Box', 'Circle', 'Curve 8', 'Traverse', 'Ellipse', 'Spline 10', 'Spline 32', 'Project X', 'Project Y', 'Spline 100', 'S pixel' ], tracking_events=1, $
      uvalue='Select area analyze type. Use mouse to drag out a shape, ' + $
      'or move/adjust an existing shape. Clear all shapes using menu.')


  Analyze_Mode = widget_combobox(Image_Button_Top_Base, xsize=xsize_analyze_mode,  $
      UNAME='Analyze_Mode_combobox' ,NOTIFY_REALIZE='OnRealize_Analyze_Mode'  $
      ,VALUE=[ '+', '-' ], tracking_events=1, $
      uvalue='Select area analyze mode, to INCLUDE an area (+), or EXCLUDE it (-) from a prior include.')


  Analyze_Button = Widget_Button( Image_Button_Top_Base, UNAME='Analyze_Button'  $
      ,/ALIGN_CENTER ,VALUE='S', xsize=24, tracking_events=1, ysize=button_height, font=symbol, $
      uvalue='Use analyze type and mode to calculate average concentrations. '+ $
      'See "Image Regions" window for results.')


  Mode_combobox = widget_combobox( Image_Button_Bot_Base, sensitive=(realtime eq 0), $
      UNAME='Mode_combobox' ,NOTIFY_REALIZE='OnRealize_Image_Mode',  $
      VALUE=[ 'Conc', 'Var' ], tracking_events=1, xsize=mode_xsize, $
      uvalue='Droplist to select the data to display: concentration, variance.')


  interp_base = widget_base(Image_Button_Bot_Base, /nonexclusive, xsize=20, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=2)
  interp_button = widget_button( interp_base, value='', /tracking, uvalue='Enable/disable Interpolation of pixels when zoomed in.', $
  				uname='interp_button', xsize=20, ysize=20, /align_top, $
  				NOTIFY_REALIZE='OnRealize_Image_Interp')


  Bottom_Slider = widget_slider( Image_Button_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
    uvalue='Adjust minimum image value to display, as a % of max.', uname='Bottom_Slider', $
    xsize=slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_Image_Bottom_Slider')


  Zscale_Mode = widget_combobox(Image_Button_Bot_Base, xsize=xsize_loglin,  $
      UNAME='Zscale_Mode_combobox' ,NOTIFY_REALIZE='OnRealize_Image_Zscale_Mode'  $
      ,VALUE=[ 'L linear  ', 'G log', 'S sqrt' ], tracking_events=1, $
      uvalue='Select image Z scale display mode between Linear, Log and Sqrt.')


  Top_Slider = widget_slider( Image_Button_Bot_Base, minimum=0, maximum=300, /drag, /tracking, $
    uvalue='Adjust maximum image value to display, as a % of max.', uname='Top_Slider', $
    xsize=slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_Image_Top_Slider')


  Help_Text1 = Widget_Text(Image_Help1_Base, UNAME='Help_Text1', /wrap $
      ,NOTIFY_REALIZE='OnRealize_Image_Help1',scr_XSIZE=help1_xsize, frame=0 ,YSIZE=3, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.', /align_center)


  Help_Text2 = Widget_Text(Image_Help2_Base, UNAME='Help_Text2', /wrap $
      ,NOTIFY_REALIZE='OnRealize_Image_Help2',scr_XSIZE=1, frame=0 ,scr_YSIZE=help2_scr_ysize, ysize=3, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')


 ; File menus

  debug_line = 'Menus ...'
  W_MENU_0 = Widget_Button(Image_TLB_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File', font=def_font)

	if realtime eq 0 then begin
	  W_MENU_1 = Widget_Button(W_MENU_0, UNAME='Load_Menu' ,VALUE='Load', font=def_font)
	endif

  W_MENU_2 = Widget_Button(W_MENU_0, UNAME='Save_Menu' ,VALUE='Save', font=def_font)

  W_MENU_2b = Widget_Button(W_MENU_0, UNAME='Clear_Menu' ,VALUE='Clear', font=def_font)

	if realtime eq 0 then begin
	  W_MENU_2c = Widget_Button(W_MENU_0, UNAME='Load_Menu_Ignore' ,VALUE='Load (ignore null)', /separator, font=def_font)
	endif

	if use_gif then begin
	    W_MENU_3c = Widget_Button(W_MENU_0, UNAME='Save_All_HTML_Menu' , /separator $
	          ,VALUE='Save ALL as HTML (GIF)', font=def_font)
	
	    W_MENU_3d = Widget_Button(W_MENU_0, UNAME='Save_All_BW_HTML_Menu'   $
	          ,VALUE='Black and white HTML (GIF)', font=def_font)
	endif else begin
	    W_MENU_3e = Widget_Button(W_MENU_0, UNAME='Save_All_HTML_PNG_Menu', /separator   $
	          ,VALUE='Save ALL as HTML (PNG)', font=def_font)
	
	    W_MENU_3f = Widget_Button(W_MENU_0, UNAME='Save_All_BW_HTML_PNG_Menu'   $
	          ,VALUE='Black and white HTML (PNG)', font=def_font)
	endelse

	if realtime eq 0 then begin
	    W_MENU_3x = Widget_Button(W_MENU_0, UNAME='Import_Menu' ,VALUE='Import', /menu, /separator, font=def_font)

		if xanes then begin
		    W_MENU_31x = Widget_Button(W_MENU_3x, UNAME='Import_Stack_Menu', VALUE='XANES image stack (DAI files)', font=def_font)

		    W_MENU_31x2 = Widget_Button(W_MENU_3x, UNAME='Import_Stack_Dir_Menu', VALUE='XANES image stack (Directories)', font=def_font)

		    W_MENU_31x3 = Widget_Button(W_MENU_3x, UNAME='Import_Stack_Flatten_Menu', VALUE='XANES image stack (DAI files) w/ Flatten', font=def_font)

		    W_MENU_31x4 = Widget_Button(W_MENU_3x, UNAME='Import_Stack_Dir_Flatten_Menu', VALUE='XANES image stack (Directories) w/ Flatten', font=def_font)

		    W_MENU_31x5 = Widget_Button(W_MENU_3x, UNAME='Import_Stack_Energies_Menu', VALUE='XANES image plane energies', font=def_font)
		endif else begin
		    W_MENU_31x = Widget_Button(W_MENU_3x, UNAME='Import_BMP_Menu', VALUE='BMP element images', font=def_font)
		    W_MENU_31x1 = Widget_Button(W_MENU_3x, UNAME='Import_PNG_Menu', VALUE='PNG element images', font=def_font)
		    W_MENU_31x4 = Widget_Button(W_MENU_3x, UNAME='Import_TIF_Menu', VALUE='TIF element images', font=def_font)

		    W_MENU_31x2 = Widget_Button(W_MENU_3x, UNAME='Import_PNC_Menu', VALUE='PNC-CAT image file', font=def_font)
	
		    W_MENU_31x3 = Widget_Button(W_MENU_3x, UNAME='Import_PEF_Menu', VALUE='PEF image file', font=def_font)
		endelse
	endif

	if (realtime eq 0) and (xanes eq 0) then begin
		W_MENU_3c = Widget_Button(W_MENU_0, UNAME='Convert_Menu' ,VALUE='Convert', /menu, font=def_font)

		W_MENU_3c1 = Widget_Button(W_MENU_3c, UNAME='Convert_MDA_Menu', VALUE='APS MDA to LST', font=def_font)
    endif

    W_MENU_3 = Widget_Button(W_MENU_0, UNAME='Export_Menu' ,VALUE='Export', /menu, /separator, font=def_font)

    W_MENU_3y = Widget_Button(W_MENU_3, UNAME='Simple_Menu' ,VALUE='Simple Image', /menu, /separator, font=def_font)
    W_MENU_3z = Widget_Button(W_MENU_3, UNAME='Plot_Menu' ,VALUE='Image plot', /menu, /separator, font=def_font)
    W_MENU_3x = Widget_Button(W_MENU_3, UNAME='Data_Menu' ,VALUE='Image data', /menu, /separator, font=def_font)


	if xanes then begin
		W_MENU_31e = Widget_Button(W_MENU_3x, UNAME='XANES_energy_list_Menu', VALUE='XANES energy list', font=def_font)
	endif
	if use_gif then begin
	    W_MENU_31 = Widget_Button(W_MENU_3y, UNAME='Save_GIF_Menu', VALUE='Save Image as GIF', /separator, font=def_font)
	
	    W_MENU_31b = Widget_Button(W_MENU_3y, UNAME='Save_GIF_Shape_Menu', VALUE='Save Shape as GIF', font=def_font)
	
	    W_MENU_31c = Widget_Button(W_MENU_3y, UNAME='Save_GIF_Highlight_Menu', VALUE='Current Assoc. Highlight as GIF', font=def_font)
	endif else begin
	    W_MENU_31 = Widget_Button(W_MENU_3y, UNAME='Save_JPEG_Menu', VALUE='Save Image as JPEG', /separator, font=def_font)
	    W_MENU_31a = Widget_Button(W_MENU_3y, UNAME='Save_PNG_Menu', VALUE='Save Image as PNG', font=def_font)
	    W_MENU_31d = Widget_Button(W_MENU_3y, UNAME='Save_TIFF_Menu', VALUE='Save Image as TIFF', font=def_font)
	    if test then begin
			W_MENU_31ab = Widget_Button(W_MENU_3y, UNAME='Save_PNG_Colour_Menu', VALUE='Save Colour Image as PNG', font=def_font)
		endif
	
	    W_MENU_31b = Widget_Button(W_MENU_3y, UNAME='Save_PNG_Shape_Menu', VALUE='Current Shape as transparent PNG', /separator, font=def_font)
	
		W_MENU_31c = Widget_Button(W_MENU_3y, UNAME='Highlight_save_Menu' ,VALUE='Current Assoc. Highlight as PNG', /menu, font=def_font)
	    W_MENU_31c1 = Widget_Button(W_MENU_31c, UNAME='Save_PNG_Highlight_Menu', VALUE='green', font=def_font)
	    W_MENU_31c1 = Widget_Button(W_MENU_31c, UNAME='Save_PNG_Highlight_Menu_pink', VALUE='pink', font=def_font)
	    W_MENU_31c1 = Widget_Button(W_MENU_31c, UNAME='Save_PNG_Highlight_Menu_lblue', VALUE='l.blue', font=def_font)
	    W_MENU_31c1 = Widget_Button(W_MENU_31c, UNAME='Save_PNG_Highlight_Menu_red', VALUE='red', font=def_font)
	    W_MENU_31c1 = Widget_Button(W_MENU_31c, UNAME='Save_PNG_Highlight_Menu_yellow', VALUE='yellow', font=def_font)
	    W_MENU_31c1 = Widget_Button(W_MENU_31c, UNAME='Save_PNG_Highlight_Menu_orange', VALUE='orange', font=def_font)
	endelse

   W_MENU_31a = Widget_Button(W_MENU_3x, UNAME='Export_ASCII_Menu', VALUE='Selected Images as ASCII tables', font=def_font)

;   W_MENU_311 = Widget_Button(W_MENU_31a, UNAME='Export_CSV_Menu', VALUE='Whole images, comma separated', font=def_font)
;
;   W_MENU_312 = Widget_Button(W_MENU_31a, UNAME='Export_TAB_Menu', VALUE='Whole images, tab separated', font=def_font)
;
;   W_MENU_313 = Widget_Button(W_MENU_31a, UNAME='Export_CSV_Region_Menu', VALUE='Selected Region, comma separated', font=def_font)
;
;   W_MENU_314 = Widget_Button(W_MENU_31a, UNAME='Export_TAB_Region_Menu', VALUE='Selected Region, tab separated', font=def_font)
;
;   W_MENU_31b = Widget_Button(W_MENU_3x, UNAME='Export_XY_ASCII_Menu', VALUE='Selected Images as XY conc tables', /menu, font=def_font)

;   W_MENU_31b1 = Widget_Button(W_MENU_31b, UNAME='Export_XY_CSV_Menu', VALUE='Whole images, comma separated', font=def_font)

;   W_MENU_31b2 = Widget_Button(W_MENU_31b, UNAME='Export_XY_TAB_Menu', VALUE='Whole images, tab separated', font=def_font)

;   W_MENU_31b3 = Widget_Button(W_MENU_31b, UNAME='Export_XY_CSV_Region_Menu', VALUE='Selected Region, comma separated (clip negatives to zero)', font=def_font)
;
;   W_MENU_31b4 = Widget_Button(W_MENU_31b, UNAME='Export_XY_TAB_Region_Menu', VALUE='Selected Region, tab separated (clip negatives to zero)', font=def_font)
;
;   W_MENU_31b5 = Widget_Button(W_MENU_31b, UNAME='Export_XY_CSV_Region_zero_Menu', VALUE='Selected Region, comma separated (include negatives)', font=def_font)
;
;   W_MENU_31b6 = Widget_Button(W_MENU_31b, UNAME='Export_XY_TAB_Region_zero_Menu', VALUE='Selected Region, tab separated (include negatives)', font=def_font)

	if xanes eq 0 then begin
		W_MENU_36 = Widget_Button(W_MENU_3x, UNAME='Save_Chimage_Menu' ,VALUE='Selected Images as Chimage', /separator, font=def_font)

		W_MENU_36b = Widget_Button(W_MENU_3x, UNAME='Save_Zarr_Menu' ,VALUE='Export as ZARR', /separator, font=def_font)

		W_MENU_36c = Widget_Button(W_MENU_3x, UNAME='Export_Zarr_GCF_Menu' ,VALUE='Create GeoPIXE Command File to Export as ZARR (C*)', font=def_font)
	endif

	if xanes eq 0 then begin
		W_MENU_31d = Widget_Button(W_MENU_0, UNAME='retry_image_increment_Menu', VALUE='Retry cluster image combine', font=def_font)
	endif
	W_MENU_31e = Widget_Button(W_MENU_0, UNAME='dump_binary_Menu', VALUE='Dump binary', font=def_font)

   W_MENU_34c = Widget_Button(W_MENU_3z, UNAME='Plot_JPEG_Menu' ,VALUE='Current Image as JPEG plot', font=def_font)

   W_MENU_35b = Widget_Button(W_MENU_3z, UNAME='Plot_PNG_Menu' ,VALUE='Current Image as PNG plot', font=def_font)

   W_MENU_32 = Widget_Button(W_MENU_3z, UNAME='Save_CGM_Menu' ,VALUE='Current Image as CGM plot', /separator, font=def_font)

;   W_MENU_33 = Widget_Button(W_MENU_3z, UNAME='Save_All_CGM_Menu' ,VALUE='Selected Images as CGM', font=def_font)

   W_MENU_34 = Widget_Button(W_MENU_3z, UNAME='Save_WMF_Menu' ,VALUE='Current Image as WMF plot', font=def_font)

;   W_MENU_35 = Widget_Button(W_MENU_3z, UNAME='Save_All_WMF_Menu' ,VALUE='Selected Images as WMF', font=def_font)

;   W_MENU_35c = Widget_Button(W_MENU_3z, UNAME='Plot_All_PNG_Menu' ,VALUE='Selected Images as PNG plots', font=def_font)

   W_MENU_37 = Widget_Button(W_MENU_3y, UNAME='Export_TIFF_Menu', VALUE='Selected Images as TIFF', /separator, /menu, font=def_font)

   W_MENU_37_ = Widget_Button(W_MENU_37, UNAME='Save_All_HTML_TIFF0_Menu' ,VALUE='Concentration', font=def_font)

   W_MENU_37a = Widget_Button(W_MENU_37, UNAME='Save_All_HTML_TIFF0_VAR_Menu' ,VALUE='Concentration (w/ Variance)', font=def_font)

  if xanes then  W_MENU_37b = Widget_Button(W_MENU_37, UNAME='Save_All_HTML_TIFF0_TXM_Menu' ,VALUE='XANES for TXM Wizard', font=def_font)

   W_MENU_37c = Widget_Button(W_MENU_37, UNAME='Save_All_HTML_TIFF1_Menu' ,VALUE='Counts', font=def_font)

   W_MENU_37d = Widget_Button(W_MENU_37, UNAME='Save_All_HTML_TIFF2_Menu' ,VALUE='Areal density (ng/cm**2)', font=def_font)


;  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='Plot_Menu' ,/SEPARATOR  $
;      ,VALUE='Plot', font=def_font)


  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='Print_Menu' ,VALUE='Print' ,/SEPARATOR, font=def_font)

  W_MENU_4b = Widget_Button(W_MENU_0, UNAME='Print_All_Menu' ,VALUE='Print All', font=def_font)

;	if noprefs eq 0 then begin
;		W_MENU_6 = Widget_Button(W_MENU_0, UNAME='Prefs_Menu' ,/SEPARATOR    ,VALUE='Preferences', font=def_font)
;	endif
  W_MENU_6b = Widget_Button(W_MENU_0, UNAME='Image_History_Menu' ,VALUE='Image Properties')
  W_MENU_6c = Widget_Button(W_MENU_0, UNAME='Image_History_Stats_Menu' ,VALUE='Image Properties and Pixel Statistics')

  W_MENU_7 = Widget_Button(W_MENU_0, UNAME='Exit_Menu'    ,VALUE='Exit', font=def_font)


; Display menus

  W_MENU_20 = Widget_Button(Image_TLB_MBAR, UNAME='W_MENU_20'  $
      ,/MENU ,VALUE='Display', font=def_font)


  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='Clone_Menu', VALUE='Clone', /menu, font=def_font)

  W_MENU_211 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_1', VALUE='1 Clone', font=def_font)

  W_MENU_212 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_3', VALUE='3 Clone', font=def_font)

  W_MENU_213 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_7', VALUE='7 Clone', font=def_font)

;  W_MENU_214 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_11', VALUE='11 Clone', font=def_font)


  W_MENU_214b = Widget_Button(W_MENU_20, UNAME='Multi_Image_Menu', VALUE='Multi Image', font=def_font)

	if (xanes eq 0) and (realtime eq 0) then begin
		W_MENU_214c = Widget_Button(W_MENU_20, UNAME='Xanes_Image_Menu', VALUE='3D Stack Image', font=def_font)
	endif

;  W_MENU_22 = Widget_Button(W_MENU_20, UNAME='Select_Menu', VALUE='Select' , font=def_font)


  W_MENU_23 = Widget_Button(W_MENU_20, UNAME='Colours_Menu', VALUE='Colours',/SEPARATOR, /menu, font=def_font )

  W_MENU_2310 = Widget_Button(W_MENU_23, UNAME='Colour_Table', VALUE='Colour Table', font=def_font )

  W_MENU_231 = Widget_Button(W_MENU_23, UNAME='Default_Colour_Table', VALUE='Default Colours' , font=def_font)

  W_MENU_231b = Widget_Button(W_MENU_23, UNAME='TenStep_Colour_Table', VALUE='10 Step Colours' )

  W_MENU_232 = Widget_Button(W_MENU_23, UNAME='Grey_Scale', VALUE='Grey Scale')

  W_MENU_232b = Widget_Button(W_MENU_23, UNAME='Red_Colour_Blind_Menu', VALUE='Red Colour Blind', /menu, font=def_font )

  W_MENU_232b1 = Widget_Button(W_MENU_232b, UNAME='Red_Colour_Blind_On', VALUE='On')
  W_MENU_232b2 = Widget_Button(W_MENU_232b, UNAME='Red_Colour_Blind_Off', VALUE='Off')

  W_MENU_233 = Widget_Button(W_MENU_23, UNAME='Invert_Colour_Table', VALUE='Invert Colour Table',/SEPARATOR  )

  W_MENU_234 = Widget_Button(W_MENU_23, UNAME='Linear_Luminance', VALUE='Linear Luminance' )


  W_MENU_24 = Widget_Button(W_MENU_20, UNAME='Clear_All_Marks_Menu', VALUE='Clear ALL Shapes',/SEPARATOR )

  W_MENU_24b = Widget_Button(W_MENU_20, UNAME='Clear_Corr_Menu', VALUE='Clear Association/PCA Highlights' )

  W_MENU_25 = Widget_Button(W_MENU_20, UNAME='reset_display_range', VALUE='Reset display min/max' )

; Analyze menus

	if realtime eq 0 then begin
	  W_MENU_40 = Widget_Button(Image_TLB_MBAR, UNAME='W_MENU_40'  $
	      ,/MENU ,VALUE='Analyze')


	  W_MENU_41 = Widget_Button(W_MENU_40, UNAME='W_MENU_41', /menu  $
	      ,VALUE='Mode')

	  W_MENU_411 = Widget_Button(W_MENU_41, UNAME='Include_Menu', VALUE='Include' )

	  W_MENU_412 = Widget_Button(W_MENU_41, UNAME='Exclude_Menu', VALUE='Exclude' )


	  W_MENU_42 = Widget_Button(W_MENU_40, UNAME='W_MENU_42', /menu  $
	      ,VALUE='Type')

	  W_MENU_421 = Widget_Button(W_MENU_42, UNAME='Box_Menu', VALUE='Box' )

	  W_MENU_422 = Widget_Button(W_MENU_42, UNAME='Circle_Menu', VALUE='Circle' )

	  W_MENU_425 = Widget_Button(W_MENU_42, UNAME='Ellipse_Menu', VALUE='Ellipse' )

	  W_MENU_424 = Widget_Button(W_MENU_42, UNAME='Line_Menu', VALUE='Linear Traverse' )

	  W_MENU_423 = Widget_Button(W_MENU_42, UNAME='Curve_Menu', VALUE='Curve 8 Traverse' )

	  W_MENU_426 = Widget_Button(W_MENU_42, UNAME='Spline_10_Menu', VALUE='Spline 10' )

	  W_MENU_427 = Widget_Button(W_MENU_42, UNAME='Spline_32_Menu', VALUE='Spline 32' )

	  W_MENU_428 = Widget_Button(W_MENU_42, UNAME='Project_X_Menu', VALUE='Project X' )

	  W_MENU_429 = Widget_Button(W_MENU_42, UNAME='Project_Y_Menu', VALUE='Project Y' )

	  W_MENU_429a = Widget_Button(W_MENU_42, UNAME='Spline_100_Menu', VALUE='Spline 100' )

	  W_MENU_429b = Widget_Button(W_MENU_42, UNAME='Single_pixel_Menu', VALUE='Single Pixel' )


	  W_MENU_43 = Widget_Button(W_MENU_40, UNAME='Analyze_Menu' ,VALUE='Region')

	if xanes eq 0 then begin
		W_MENU_44 = Widget_Button(W_MENU_40, UNAME='Throttle_Menu' ,VALUE='Throttle', /separator)
	endif
  endif
  debug_line = 'Main Menus done.'

; Image Processing menus

  debug_line = 'processing menus ...'
  plugin_menus_root = Widget_Button(Image_TLB_MBAR, UNAME='plugin_menus_root'  $
      ,/MENU ,VALUE='Process')


  W_MENU_514b = Widget_Button(plugin_menus_root, UNAME='W_MENU_514', /menu, VALUE='Clear' )

  W_MENU_514b1 = Widget_Button(W_MENU_514b, UNAME='W_MENU_514', /menu, VALUE='Border' )

  W_MENU_5141 = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-1', VALUE='* 1' )
  W_MENU_5142 = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-2', VALUE='* 2' )
  W_MENU_5143 = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-5', VALUE='* 5' )
  W_MENU_5143 = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-10', VALUE='* 10' )

  W_MENU_5141b = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-sides-1', VALUE='* sides 1' )
  W_MENU_5142b = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-sides-2', VALUE='* sides 2' )
  W_MENU_5143b = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-sides-5', VALUE='* sides 5' )
  W_MENU_5143b = Widget_Button(W_MENU_514b1, UNAME='Clear_Menu-sides-10', VALUE='* sides 10' )

  W_menu_554b = Widget_Button(W_menu_514b, UNAME='Kill-Region', VALUE='Kill this Region in this plane' )
  W_menu_554c = Widget_Button(W_menu_514b, UNAME='Kill-Region-all', VALUE='Kill this Region in all planes' )
  W_menu_554d = Widget_Button(W_menu_514b, UNAME='Kill-All-Region-all', VALUE='Kill all Regions in all planes' )

  W_MENU_51 = Widget_Button(plugin_menus_root, UNAME='W_MENU_51', /menu  $
      ,VALUE='Filter')

  W_MENU_510 = Widget_Button(W_MENU_51, UNAME='W_MENU_510', /menu  $
      ,VALUE='Smooth')

  W_MENU_513 = Widget_Button(W_MENU_510, UNAME='W_MENU_513', /menu, VALUE='Gaussian' )

  W_MENU_5131 = Widget_Button(W_MENU_513, UNAME='Gaussian_Menu-1', VALUE='1' )
  W_MENU_51312 = Widget_Button(W_MENU_513, UNAME='Gaussian_Menu-1.5', VALUE='1.5' )
  W_MENU_5132 = Widget_Button(W_MENU_513, UNAME='Gaussian_Menu-2', VALUE='2' )
  W_MENU_5133 = Widget_Button(W_MENU_513, UNAME='Gaussian_Menu-3', VALUE='3' )
  W_MENU_5134 = Widget_Button(W_MENU_513, UNAME='Gaussian_Menu-5', VALUE='5' )
  W_MENU_5135 = Widget_Button(W_MENU_513, UNAME='Gaussian_Menu-10', VALUE='10' )

  W_MENU_512 = Widget_Button(W_MENU_510, UNAME='W_MENU_512', /menu, VALUE='Boxcar' )

  W_MENU_5121 = Widget_Button(W_MENU_512, UNAME='Boxcar_Menu-2', VALUE='2' )
  W_MENU_5122 = Widget_Button(W_MENU_512, UNAME='Boxcar_Menu-3', VALUE='3' )
  W_MENU_5123 = Widget_Button(W_MENU_512, UNAME='Boxcar_Menu-5', VALUE='5' )
  W_MENU_5124 = Widget_Button(W_MENU_512, UNAME='Boxcar_Menu-10', VALUE='10' )

  W_MENU_511 = Widget_Button(W_MENU_510, UNAME='W_MENU_511', /menu, VALUE='Median' )

  W_MENU_5111 = Widget_Button(W_MENU_511, UNAME='Median_Menu-2', VALUE='2' )
  W_MENU_5112 = Widget_Button(W_MENU_511, UNAME='Median_Menu-3', VALUE='3' )
  W_MENU_5113 = Widget_Button(W_MENU_511, UNAME='Median_Menu-5', VALUE='5' )
  W_MENU_5114 = Widget_Button(W_MENU_511, UNAME='Median_Menu-10', VALUE='10' )

  W_MENU_520 = Widget_Button(W_MENU_51, UNAME='W_MENU_520', /menu  $
      ,VALUE='Edge Enhance')

  W_MENU_521 = Widget_Button(W_MENU_520, UNAME='Roberts_Menu', VALUE='Roberts' )

  W_MENU_522 = Widget_Button(W_MENU_520, UNAME='Sobel_Menu', VALUE='Sobel' )


  W_MENU_514 = Widget_Button(W_MENU_51, UNAME='W_MENU_514', /menu, VALUE='Erode' )

  W_MENU_5141 = Widget_Button(W_MENU_514, UNAME='Erode_Menu-2', VALUE='2' )
  W_MENU_5142 = Widget_Button(W_MENU_514, UNAME='Erode_Menu-5', VALUE='5' )
  W_MENU_5143 = Widget_Button(W_MENU_514, UNAME='Erode_Menu-10', VALUE='10' )

  W_MENU_515 = Widget_Button(W_MENU_51, UNAME='W_MENU_515', /menu, VALUE='Dilate' )

  W_MENU_5151 = Widget_Button(W_MENU_515, UNAME='Dilate_Menu-2', VALUE='2' )
  W_MENU_5152 = Widget_Button(W_MENU_515, UNAME='Dilate_Menu-5', VALUE='5' )
  W_MENU_5153 = Widget_Button(W_MENU_515, UNAME='Dilate_Menu-10', VALUE='10' )

  W_menu_53 = Widget_Button(plugin_menus_root, UNAME='W_menu_53', /menu, VALUE='Scale' )

  W_menu_531 = Widget_Button(W_menu_53, UNAME='Scale_X-50', VALUE='X x 0.5' )
  W_menu_532 = Widget_Button(W_menu_53, UNAME='Scale_X-75', VALUE='X x 0.75' )
  W_menu_533 = Widget_Button(W_menu_53, UNAME='Scale_X-90', VALUE='X x 0.9' )
  W_menu_534 = Widget_Button(W_menu_53, UNAME='Scale_X-110', VALUE='X x 1.1' )
  W_menu_534b = Widget_Button(W_menu_53, UNAME='Scale_X-150', VALUE='X x 1.5' )
  W_menu_535 = Widget_Button(W_menu_53, UNAME='Scale_X-200', VALUE='X x 2.0' )
  W_menu_535b = Widget_Button(W_menu_53, UNAME='Scale_X-300', VALUE='X x 3.0' )
  W_menu_536 = Widget_Button(W_menu_53, UNAME='Scale_Y-50', VALUE='Y x 0.5' )
  W_menu_537 = Widget_Button(W_menu_53, UNAME='Scale_Y-75', VALUE='Y x 0.75' )
  W_menu_538 = Widget_Button(W_menu_53, UNAME='Scale_Y-90', VALUE='Y x 0.9' )
  W_menu_539 = Widget_Button(W_menu_53, UNAME='Scale_Y-110', VALUE='Y x 1.1' )
  W_menu_539b = Widget_Button(W_menu_53, UNAME='Scale_Y-150', VALUE='Y x 1.5' )
  W_menu_5310 = Widget_Button(W_menu_53, UNAME='Scale_Y-200', VALUE='Y x 2.0' )
  W_menu_539c = Widget_Button(W_menu_53, UNAME='Scale_Y-300', VALUE='Y x 3.0' )
  W_menu_5311 = Widget_Button(W_menu_53, UNAME='Scale_XY-110', VALUE='X,Y x 1.1' )
  W_menu_5311 = Widget_Button(W_menu_53, UNAME='Scale_XY-150', VALUE='X,Y x 1.5' )
  W_menu_5311 = Widget_Button(W_menu_53, UNAME='Scale_XY-200', VALUE='X,Y x 2.0' )
  W_menu_5311 = Widget_Button(W_menu_53, UNAME='Scale_XY-300', VALUE='X,Y x 3.0' )
  W_menu_5312 = Widget_Button(W_menu_53, UNAME='Scale_XY-50', VALUE='X,Y x 0.5' )
  W_menu_5312 = Widget_Button(W_menu_53, UNAME='Scale_XY-90', VALUE='X,Y x 0.9' )

  W_menu_54 = Widget_Button(plugin_menus_root, UNAME='W_menu_54', /menu, VALUE='Rotate' )

  W_menu_541 = Widget_Button(W_menu_54, UNAME='Rotate+90', VALUE='+90 degrees' )
  W_menu_542 = Widget_Button(W_menu_54, UNAME='Rotate-90', VALUE='-90 degrees' )
  W_menu_543 = Widget_Button(W_menu_54, UNAME='Flip-X', VALUE='Mirror X' )
  W_menu_544 = Widget_Button(W_menu_54, UNAME='Flip-Y', VALUE='Mirror Y' )

  W_menu_53c = Widget_Button(plugin_menus_root, UNAME='W_menu_53', /menu, VALUE='Shift' )

  if xanes then begin
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-even+0.5', VALUE='even rows +0.5 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-even-0.5', VALUE='even rows -0.5 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-odd+0.5', VALUE='odd rows +0.5 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-odd-0.5', VALUE='odd rows -0.5 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest+1', VALUE='odd rows +1 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-1', VALUE='odd rows -1 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest+2', VALUE='odd rows +2 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-2', VALUE='odd rows -2 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest+5', VALUE='odd rows +5 (toggle w/ odd Y rows)' )
	  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-ytest-5', VALUE='odd rows -5 (toggle w/ odd Y rows)' )
  endif
  
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-even+0.5', VALUE='even rows +0.5', /separator )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-even-0.5', VALUE='even rows -0.5' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-odd+0.5', VALUE='odd rows +0.5' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-odd-0.5', VALUE='odd rows -0.5' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift+1', VALUE='odd rows +1' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-1', VALUE='odd rows -1' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift+2', VALUE='odd rows +2' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-2', VALUE='odd rows -2' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift+5', VALUE='odd rows +5' )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-5', VALUE='odd rows -5' )

  W_menu_531b = Widget_Button(W_menu_53c, UNAME='Shift-columns+1', VALUE='odd columns +1' )
  W_menu_531c = Widget_Button(W_menu_53c, UNAME='Shift-columns-1', VALUE='odd columns -1' )

  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-all+1', VALUE='all rows +1', /separator )
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-all-1', VALUE='all rows -1')
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-all+2', VALUE='all rows +2')
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-all-2', VALUE='all rows -2')
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-all+5', VALUE='all rows +5')
  W_menu_531 = Widget_Button(W_menu_53c, UNAME='Shift-all-5', VALUE='all rows -5')

  W_menu_54b = Widget_Button(plugin_menus_root, UNAME='Crop', /menu, VALUE='Crop' )

;  W_menu_54b1 = Widget_Button(W_menu_54b, UNAME='Clip-Right', VALUE='Clip Right' )
;  W_menu_54b2 = Widget_Button(W_menu_54b, UNAME='Clip-Top', VALUE='Clip Top' )
  W_menu_54b3 = Widget_Button(W_menu_54b, UNAME='Clip-Crop', VALUE='Crop to Shape (enclosing rectangle)' )
  W_menu_54b4 = Widget_Button(W_menu_54b, UNAME='Clip-Crop-Zero', VALUE='Crop to Shape (zero outside shape)' )

  W_menu_55 = Widget_Button(plugin_menus_root, UNAME='W_menu_55', /menu, VALUE='Correct' )

  W_menu_551 = Widget_Button(W_menu_55, UNAME='Correct-X-Current', VALUE='* Xstep Current' )
  W_menu_551b = Widget_Button(W_menu_55, UNAME='Correct-X-Current-file', VALUE='CorrectX from file' )
  W_menu_552 = Widget_Button(W_menu_55, UNAME='Correct-Y-Current', VALUE='* Ystep Current' )
  W_menu_552b = Widget_Button(W_menu_55, UNAME='Correct-Y-Current-file', VALUE='CorrectY from file' )
  W_menu_553 = Widget_Button(W_menu_55, UNAME='Suppress-Region', VALUE='Suppress Region x0.7' )
  W_menu_554 = Widget_Button(W_menu_55, UNAME='Enhance-Region', VALUE='Enhance Region x1.2' )
  W_menu_555 = Widget_Button(W_menu_55, UNAME='Correct-Image-Pileup', VALUE='Image Pile-up' )
  W_menu_555b = Widget_Button(W_menu_55, UNAME='Missing-Rows', VALUE='* Missing rows' )
  W_menu_555c = Widget_Button(W_menu_55, UNAME='Double-Rows', VALUE='* Double counted rows' )
  W_menu_555d = Widget_Button(W_menu_55, UNAME='Missing-Cols', VALUE='* Missing columns' )
  W_menu_555e = Widget_Button(W_menu_55, UNAME='Double-Cols', VALUE='* Double counted columns' )
  W_menu_555e2 = Widget_Button(W_menu_55, UNAME='Zeroes', VALUE='* Zero pixels' )
  W_menu_555e3 = Widget_Button(W_menu_55, UNAME='Correct-Y-offset', VALUE='Correct Y Drift' )
  
  W_menu_555f = Widget_Button(W_MENU_55, UNAME='W_menu_555f', /menu, VALUE='Ghost' )
  W_menu_555f2 = Widget_Button(W_menu_555f, UNAME='Ghost-30', VALUE='-30 %' )
  W_menu_555f1 = Widget_Button(W_menu_555f, UNAME='Ghost-10', VALUE='-10 %' )
  W_menu_555f2 = Widget_Button(W_menu_555f, UNAME='Ghost-5', VALUE='-5 %' )
  W_menu_555f2 = Widget_Button(W_menu_555f, UNAME='Ghost-3', VALUE='-3 %' )
  W_menu_555f2 = Widget_Button(W_menu_555f, UNAME='Ghost-1', VALUE='-1 %' )
  W_menu_555f3 = Widget_Button(W_menu_555f, UNAME='Ghost+1', VALUE='+1 %' )

  if (realtime eq 0) and (xanes eq 0) then begin
  	W_menu_556 = Widget_Button(W_menu_55, UNAME='Correct-interelement', VALUE='Inter Element effects' )
  endif
  
;  W_menu_556 = Widget_Button(W_MENU_55, UNAME='W_menu_556', /menu, VALUE='Flux' )
;  W_menu_556a = Widget_Button(W_menu_556, UNAME='flux-remove', VALUE='Remove flatten correction' )
;  W_menu_556c = Widget_Button(W_menu_556, UNAME='flux-apply', VALUE='Apply flatten correction' )

  W_menu_557 = Widget_Button(W_menu_55, UNAME='Correct-Y-ripples', /menu, VALUE='Y Ripples' )
  
  W_MENU_5571 = Widget_Button(W_menu_557, UNAME='Correct-Y-ripples-5', VALUE='5' )
  W_MENU_5572 = Widget_Button(W_menu_557, UNAME='Correct-Y-ripples-10', VALUE='10' )
  W_MENU_5573 = Widget_Button(W_menu_557, UNAME='Correct-Y-ripples-20', VALUE='20' )
  W_MENU_5574 = Widget_Button(W_menu_557, UNAME='Correct-Y-ripples-50', VALUE='50' )
  W_MENU_5575 = Widget_Button(W_menu_557, UNAME='Correct-Y-ripples-100', VALUE='100' )

  W_MENU_55 = Widget_Button(plugin_menus_root, UNAME='Undo_Menu', VALUE='Undo Operation', /separator )

  if (realtime eq 0) and (xanes eq 0) then begin
  	W_MENU_54 = Widget_Button(plugin_menus_root, UNAME='Merge_Gamma_Menu', VALUE='Merge PIGE', /separator )
  endif

  W_MENU_59 = Widget_Button(plugin_menus_root, UNAME='Reset_Menu', VALUE='Reset process shared memory', /separator )

  if test then W_MENU_56 = Widget_Button(plugin_menus_root, UNAME='Test_Menu', VALUE='Test', /separator )

	W_MENU_57 = Widget_Button(plugin_menus_root, UNAME='Reload-Plugins', VALUE='Reload User Plugins', /separator )
	
  plugin_menus = Widget_Button(plugin_menus_root, UNAME='W_MENU_57', /menu, VALUE='User Plugins' )

  debug_line = 'plugin menus ...'
if add_plugins then begin
  for i=0L,n_elements((*plugins).title)-1 do begin
    W_MENU_571 = Widget_Button(plugin_menus, UNAME='Plugin', VALUE=(*plugins).title[i], $
         uvalue=(*plugins).list[i] )
  endfor
endif else plugin_menus=0L

  wizard_menus = Widget_Button(plugin_menus_root, UNAME='W_MENU_58', /menu, VALUE='Wizards', /separator )

  debug_line = 'wizard menus ...'
if add_wizards then begin
  for i=0L,n_elements((*wizards).title)-1 do begin
    W_MENU_581 = Widget_Button(wizard_menus, UNAME='Wizard', VALUE=(*wizards).title[i], $
         uvalue=(*wizards).list[i] )
  endfor
endif else wizard_menus=0L

; Window menus

  debug_line = 'other menus ...'

  W_MENU_60 = Widget_Button(Image_TLB_MBAR, UNAME='W_MENU_60', /MENU ,VALUE='Window')


;  W_MENU_61 = Widget_Button(W_MENU_60, UNAME='Snapshot_Menu' ,VALUE='Snapshot Windows')


;  W_MENU_61a = Widget_Button(W_MENU_60, UNAME='Plot_Results_Menu' ,VALUE='Plot Results')


	if (xanes eq 0) then begin
		W_MENU_62 = Widget_Button(W_MENU_60, UNAME='Results_Table_Menu' ,VALUE='Image Regions', /separator)
	endif else begin
		W_MENU_62 = Widget_Button(W_MENU_60, UNAME='Results_Table_Menu' ,VALUE='XANES Stack Regions', /separator)
	endelse

;  W_MENU_63 = Widget_Button(W_MENU_60, UNAME='Select_Image_Menu' ,VALUE='Select Image')


  W_MENU_64 = Widget_Button(W_MENU_60, UNAME='Image_Operations_Menu' ,VALUE='Image Operations')

  W_MENU_64b = Widget_Button(W_MENU_60, UNAME='Image_History_Menu' ,VALUE='Image Properties and History')
  W_MENU_64c = Widget_Button(W_MENU_60, UNAME='Image_History_Stats_Menu' ,VALUE='Image Properties and Pixel Statistics')


	if (xanes eq 0) then begin
		W_MENU_65b = Widget_Button(W_MENU_60, UNAME='Corr_Menu' ,VALUE='Element Associations', /separator)
	endif else begin
		W_MENU_65b = Widget_Button(W_MENU_60, UNAME='Corr_Menu' ,VALUE='Energy Associations', /separator)
	endelse

  W_MENU_65b2 = Widget_Button(W_MENU_60, UNAME='PCA_cluster_Menu' ,VALUE='PCA, Clusters')

  W_MENU_65c = Widget_Button(W_MENU_60, UNAME='RGB_Menu' ,VALUE='3 Element RGB Images')

  if (realtime eq 0) and (xanes eq 0) then begin
  	W_MENU_65d = Widget_Button(W_MENU_60, UNAME='Interelement_Menu' ,VALUE='Inter Element Operations')
  endif

;  W_MENU_66 = Widget_Button(W_MENU_60, UNAME='Command_Menu' ,VALUE='Command', /separator)

	if (realtime eq 0) then begin
	  W_MENU_67b= Widget_Button(W_MENU_60, UNAME='Spectrum_Display_Menu' ,VALUE='Spectrum Display', /separator)
	endif

	if (realtime eq 0) and (xanes eq 0) then begin
	  W_MENU_67 = Widget_Button(W_MENU_60, UNAME='EVT_Menu' ,VALUE='Sort EVT File')

	  W_MENU_67c = Widget_Button(W_MENU_60, UNAME='Xanes_Image_Menu', VALUE='XANES Images')

      W_MENU_65b1 = Widget_Button(W_MENU_60, UNAME='Energy_Corr_Menu' ,VALUE='Energy Associations')

	  W_MENU_68 = Widget_Button(W_MENU_60, UNAME='Correct_Menu' ,VALUE='Correct Yields (N=6)', /separator)

	  W_MENU_68b = Widget_Button(W_MENU_60, UNAME='Correct_Big_Menu' ,VALUE='Correct Yields (N=16)')

	  W_MENU_69 = Widget_Button(W_MENU_60, UNAME='Project_Menu' ,VALUE='Project Minerals')
	endif

  W_MENU_69a = Widget_Button(W_MENU_60, UNAME='Edit_Filters_Menu' ,VALUE='Edit Filters', /separator)

  W_MENU_69b = Widget_Button(W_MENU_60, UNAME='Edit_Detectors_Menu' ,VALUE='Edit Detectors')

	if xanes eq 0 then begin
	  W_MENU_69c = Widget_Button(W_MENU_60, UNAME='New_Image' ,VALUE='Image Display (unlinked)', /separator)
	
	  W_MENU_69d = Widget_Button(W_MENU_60, UNAME='New_Spectrum' ,VALUE='Spectrum Display (unlinked)')
	
	  W_MENU_69e = Widget_Button(W_MENU_60, UNAME='Sim_Setup_Menu' ,VALUE='PIXE/SXRF Simulator (unlinked)')
	
	  W_MENU_69f = Widget_Button(W_MENU_60, UNAME='Blog_Browser_Menu' ,VALUE='Blog Browser (unlinked)')
	endif

; Test menus

  W_MENU_80 = Widget_Button(Image_TLB_MBAR, UNAME='W_MENU_80', /MENU ,VALUE='Tests')

  W_MENU_81 = Widget_Button(W_MENU_80, UNAME='Compare_yields_Menu' ,VALUE='Compare Yields')
  W_MENU_82 = Widget_Button(W_MENU_80, UNAME='Compare_sources_Menu' ,VALUE='Compare Sources')
  W_MENU_83 = Widget_Button(W_MENU_80, UNAME='Compare_pink_Menu' ,VALUE='Compare Pink Beam Sources')
  W_MENU_84 = Widget_Button(W_MENU_80, UNAME='Compare_fits_Menu' ,VALUE='Compare Fits')

; Help menus

  W_MENU_70 = Widget_Button(Image_TLB_MBAR, UNAME='W_MENU_70', /MENU ,VALUE='Help')

  W_MENU_71 = Widget_Button(W_MENU_70, UNAME='Help_User' ,VALUE="GeoPIXE User's Guide")

  W_MENU_71b = Widget_Button(W_MENU_70, UNAME='Help_Maia_User' ,VALUE="Maia User's Guide")

  W_MENU_72 = Widget_Button(W_MENU_70, UNAME='Help_Query' ,VALUE="Query IDL Environment")

  W_MENU_74 = Widget_Button(W_MENU_70, UNAME='Update_GeoPIXE' ,VALUE="Update GeoPIXE")

  W_MENU_73 = Widget_Button(W_MENU_70, UNAME='About' ,VALUE="About GeoPIXE")


  debug_line = 'realize ...'
  Widget_Control, /REALIZE, Image_TLB
  if (clone eq 0) and (noprefs eq 0) then widget_control, Image_TLB, timer=0.1

  set_map_help, Image_TLB
child = widget_info( Image_TLB, /child)
widget_control, child, get_uvalue=pstate

  debug_line = 'image scale ...'
if ptr_valid((*pstate).p) then begin
	if n_elements(*(*pstate).p) gt 0 then begin
       set_image_view, pstate, top, clone=clone
       pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
       if ptr_valid( opt) then begin
         widget_control, (*pstate).top_slider, set_value = (*opt)[(*pstate).image].top
         widget_control, (*pstate).bottom_slider, set_value = (*opt)[(*pstate).image].bottom
         widget_control, (*pstate).Zscale_mode_id, set_combobox_select = (((*opt)[(*pstate).image].log >0)<2)
       endif
	endif
endif

  debug_line = 'register notify ...'
if wGroup ne 0 then begin
	register_notify, Image_TLB, $
		[	'path', $               ; new path
			'dpath' $               ; new raw data path
		], from=wGroup
	if xanes then begin
		register_notify, Image_TLB, $
		[	'image-clone', $          ; new image from Image clone
			'images' $				; new images loaded somewhere
;			'image-corr-q', $          ; pass on notify of qc set by corr
;			'image-corr-clear', $      ; clear corr pixels in other images
;			'image-display', $          ; image display needs updating (smooth...)
;			'image-elements', $			; refresh element droplist for new elements
;			'image-results', $          ; new conc results (pass on)
;			'image-update-time', $       ; q region vector --> TimeAmp
;			'image-analyze-type', $      ; analyze type changed
;			'image-analyze-mode', $      ; analyze mode chanhged
;			'image-analyze-clear', $   ; shape cleared off screen
;			'image-analyze-mark', $      ; shape has changed
;			'image-analyze-q', $       ; q region vector has changed
;			'image-analyze-all-clear', $  ; clear all marker settings and droplists
;			'image-region-clear', $      ; clear current marker, just prior to select
;			'image-region-select', $      ; pass on notify of image_table region-select
;			'image-region-throttle' $   ; pass on notify of image region throttle to image_table
		], from=wGroup
	endif else begin
		register_notify, Image_TLB, $
		[	'mark-e', $				; mark line energy from Identify (pass to setup-filter)
			'image-clone', $          ; new image from Image clone
			'images', $				; new images loaded somewhere
			'spectrum-display', $		; new spectra display chaange
			'correct-image-pileup', $     ; subtract pileup from images
			'corr-analyze-clear', $        ; pass on notify of qc clear
			'image-corr-q', $          ; pass on notify of qc set by corr
			'image-corr-clear', $      ; clear corr pixels in other images
			'image-display', $          ; image display needs updating (smooth...)
			'image-line', $             ; new line results (pass on)
			'image-elements', $			; refresh element droplist for new elements
			'image-results', $          ; new conc results (pass on)
			'image-update-time', $       ; q region vector --> TimeAmp
			'image-analyze-type', $      ; analyze type changed
			'image-analyze-mode', $      ; analyze mode chanhged
			'image-analyze-clear', $   ; shape cleared off screen
			'image-analyze-mark', $      ; shape has changed
			'image-analyze-q', $       ; q region vector has changed
			'image-analyze-all-clear', $  ; clear all marker settings and droplists (from clone)
			'image-clear-all-marks', $		; clear all markers (from external)
			'image-region-clear', $      ; clear current marker, just prior to select
			'image-region-select', $      ; pass on notify of image_table region-select
			'image-region-delete', $	; to delete a selected spectrum (region) row
			'image-regions', $				; regions pointers, if valid w/ spectra
			'image-region-throttle', $   ; pass on notify of image region throttle to image_table
			'image-spectrum-throttle' $   ; pass on notify of image_table spectrum throttle to spectrum_display
		], from=wGroup
	endelse
  endif

  register_notify, Image_TLB, 'snapshot'            ; snapshot windows
  register_notify, Image_TLB, ['wizard-action']		; global notify from a wizard

  debug_line = 'xmanager ...'
  print,debug_line
  xmanager, 'gImage', Image_TLB, /no_block

	child = widget_info( Image_TLB, /child)
	widget_control, child, get_uvalue=pstate
	
	(*pstate).plugin_menus = plugin_menus
	(*pstate).plugin_menus_root = plugin_menus_root
	(*pstate).wizard_menus = wizard_menus
	(*pstate).wizard_menus_root = plugin_menus_root
	debug_line = 'return ...'
	print,debug_line
	return
end
