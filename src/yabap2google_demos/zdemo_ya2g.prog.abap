*&---------------------------------------------------------------------*
*& Report  SAPHTML_DEMO1                                               *
*&---------------------------------------------------------------------*
REPORT  zdemo_ya2g.

DATA: html_control TYPE REF TO cl_gui_html_viewer,
      my_container TYPE REF TO cl_gui_custom_container,
      fcode        LIKE sy-ucomm,
      myevent_tab  TYPE cntl_simple_events,
      myevent      TYPE cntl_simple_event,
      edurl(2048),
      alignment    TYPE i.

DATA: gif_google_api_creator TYPE REF TO yif_a2g_google_api_creator.
DATA: gs_spread TYPE ycl_a2g_spreadsheet_api=>ty_s_sheet_api.

DATA: lv_string TYPE string.
DATA: sheet TYPE string.



SELECTION-SCREEN: BEGIN OF TABBED BLOCK sub FOR 6 LINES,
                  END OF BLOCK sub.

SELECTION-SCREEN: skip.

PARAMETERS: p_lifnr TYPE lfa1-lifnr.
PARAMETERS: pinplace TYPE oax.

*SELECTION-SCREEN: skip.
*SELECTION-SCREEN: skip.
*SELECTION-SCREEN: skip.
*
*SELECTION-SCREEN: BEGIN OF TABBED BLOCK sub FOR 6 LINES,
*                  END OF BLOCK sub.


*****************************************************
*              CLASS cl_myevent_handler             *
*****************************************************
CLASS cl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_navigate_complete
                FOR EVENT navigate_complete OF cl_gui_html_viewer
      IMPORTING url.
ENDCLASS.

DATA: evt_receiver TYPE REF TO cl_myevent_handler.

INITIALIZATION.
  sub-prog = sy-repid.
  sub-dynnr = 110.



START-OF-SELECTION.


*  sheet = '1IS2AR-EKVne8Mw9uxkLoJkiF7OtKm8ob-r5zW8pkqL0'.

* get message manager
  DATA(go_message) = ycl_a2g_msg_manager=>create_init_msg_manager( ).
  gif_google_api_creator = ycl_a2g_api_creator=>get_api_creator_instance( i_clsname = 'YCL_A2G_SPREADSHEET_API_CREATE'
                                                                          i_msg_manager = go_message ).
  gs_spread-spreadsheet_id = sheet.
  DATA(gif_google_api) = gif_google_api_creator->create_instance( ).


  DATA(gif_context)  = gif_google_api->get_context(  ).
  gif_context->write_data( REF #( gs_spread ) ).

  DATA(gif_command)  = gif_google_api->get_command(  ).

  PERFORM create_sheet USING gif_google_api.


* Execute the request via command Execute
  gif_command->execute( ycl_a2g_spreadsheet_api=>gc_fcode_spsheet_new ).

* get json class for response
  DATA(gif_a2g_json_res) = gif_google_api->get_jclass_for_response( method = 'CREATE_NEW_SPREADSHEET' ).


  DATA(lv_json) = gif_a2g_json_res->yif_a2g_json_context~read_json_data(  ).

  DATA(result) = gif_a2g_json_res->get_attribute( i_name = ycl_a2g_json_spreadsheet=>gc_fnam_spreadsheeturl ).


  SET SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'TESTHTM1'.
  SET TITLEBAR '001'.

  IF my_container IS INITIAL.

    CREATE OBJECT my_container
      EXPORTING
        container_name = 'HTML'
      EXCEPTIONS
        OTHERS         = 1.
    CASE sy-subrc.
      WHEN 0.
*
      WHEN OTHERS.
        RAISE cntl_error.
    ENDCASE.
  ENDIF.

  IF html_control IS INITIAL.
    CREATE OBJECT html_control
      EXPORTING
        parent = my_container.
    IF sy-subrc NE 0.
      RAISE cntl_error.
    ENDIF.

* register event

*************************************************************
* DON'T USE the NAVIGATE_COMPLETE event in application logic
*************************************************************
    myevent-eventid = html_control->m_id_navigate_complete.
    myevent-appl_event = 'X'.
    APPEND myevent TO myevent_tab.
    CALL METHOD html_control->set_registered_events
      EXPORTING
        events = myevent_tab.

    CREATE OBJECT evt_receiver.

    SET HANDLER evt_receiver->on_navigate_complete
                FOR html_control.

    PERFORM load_home_page.
  ENDIF.
ENDMODULE.                             " STATUS_0100  OUTPUT




*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE fcode.
    WHEN 'BACK'.
      IF NOT html_control IS INITIAL.
        CALL METHOD html_control->free.
        FREE html_control.
      ENDIF.
      IF NOT my_container IS INITIAL.
        CALL METHOD my_container->free
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
        ENDIF.
        FREE my_container.
      ENDIF.

      LEAVE PROGRAM.
    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CLEAR fcode.
ENDMODULE.                             " USER_COMMAND_0100  INPUT

* Homepage form
FORM load_home_page.
  DATA: doc_url(150).
  ASSIGN result->* TO FIELD-SYMBOL(<fs>).
  doc_url = <fs>.
  CALL METHOD html_control->show_url
    EXPORTING
      in_place = pinplace
      url      = doc_url.

ENDFORM.                               " LOAD_HOME_PAGE

****************************************************
*    cl_myevent_handler implementation             *
****************************************************
CLASS cl_myevent_handler IMPLEMENTATION.

*************************************************************
* DON'T USE the NAVIGATE_COMPLETE event in application logic
*************************************************************
  METHOD on_navigate_complete.
    edurl = url.
  ENDMETHOD.

ENDCLASS.


FORM create_sheet USING uif_google_api TYPE REF TO  yif_a2g_google_api .

  DATA: lv_title TYPE string.
  DATA: lv_col TYPE i.

  DATA(lif_spreadsheet) = uif_google_api->new_jclass_for_request( 'CREATE_NEW_SPREADSHEET' ).

  DATA(lif_spread_prop) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_properties ).
  lv_title = 'Test mio'.
  lif_spreadsheet->set_attribute( i_name = ycl_a2g_json_spreadsheet_prop=>gc_fnam_title
                                  i_value = REF #( lv_title ) ).
  lif_spreadsheet->set_default( ).


  DATA(lif_sheets) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_sheets ).
  DATA(lif_sheets_prop) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_properties  ).

  lv_title = 'Header'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_title
                                  i_value = REF #( lv_title ) ).

  lv_title = 'GRID'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_sheettype
                                  i_value = REF #( lv_title ) ).

  lv_title = '0'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_index
                                  i_value = REF #( lv_title ) ).

  lv_title = '1'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_sheetid
                                  i_value = REF #( lv_title ) ).



  DATA(lif_sheets_data) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_data ).

  DATA: ls_sheet_data TYPE ycl_a2g_json_griddata=>ty_s_json_griddata.
  DO 20 TIMES.
    APPEND INITIAL LINE TO ls_sheet_data-row_data.
  ENDDO.

  READ TABLE ls_sheet_data-row_data ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX 6.
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_value-string_value = 'ABAP 2 GSHEET Project'.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.
  <fs_cell>-user_entered_format-text_format-font_size = '18'.
  <fs_cell>-user_entered_format-horizontal_alignment = 'CENTER'.

  READ TABLE ls_sheet_data-row_data ASSIGNING <fs_row> INDEX 9.
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = 'Documento'.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.
  <fs_cell>-user_entered_format-text_format-font_size = '16' .
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = 'Scheda Fornitore'.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.
  <fs_cell>-user_entered_format-text_format-font_size = '16' .
  <fs_cell>-user_entered_format-horizontal_alignment = 'CENTER'.


  READ TABLE ls_sheet_data-row_data ASSIGNING <fs_row> INDEX 15.
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  <fs_cell>-user_entered_value-string_value = 'Nominativo'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  <fs_cell>-user_entered_value-string_value = 'Data'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.

  READ TABLE ls_sheet_data-row_data ASSIGNING <fs_row> INDEX 16.
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = 'Creato Da'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = 'Techedge S.p.A'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = '10/10/2019'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.

  READ TABLE ls_sheet_data-row_data ASSIGNING <fs_row> INDEX 17.
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = 'Verificato Da'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = ''.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = '00/00/0000'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  READ TABLE ls_sheet_data-row_data ASSIGNING <fs_row> INDEX 18.
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = 'Approvato Da'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = ''.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = '00/00/0000'.
  <fs_cell>-user_entered_format-borders-bottom-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-bottom-width = 1.
  <fs_cell>-user_entered_format-borders-top-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-top-width = 1.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-right-width = 1.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID'.
  <fs_cell>-user_entered_format-borders-left-width = 1.


  DO 8 TIMES.
    APPEND INITIAL LINE TO ls_sheet_data-column_metadata ASSIGNING FIELD-SYMBOL(<fs_col>).
    <fs_col>-pixel_size = '100'.
  ENDDO.
  READ TABLE ls_sheet_data-column_metadata ASSIGNING <fs_col> INDEX 3.
  <fs_col>-pixel_size = '150'.
  READ TABLE ls_sheet_data-column_metadata ASSIGNING <fs_col> INDEX 4.
  <fs_col>-pixel_size = '300'.
  READ TABLE ls_sheet_data-column_metadata ASSIGNING <fs_col> INDEX 5.
  <fs_col>-pixel_size = '150'.

  lif_sheets_data->yif_a2g_context~write_data( REF #( ls_sheet_data ) ).


  DATA(lif_sheets_merge) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).
  DATA lv_idx TYPE i.

  lv_idx = 1.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).


  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).

  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).

  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).

  lv_idx = 6.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).




  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).

  lv_idx = 1.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).


  lv_idx = 3.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).

  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).

  lv_idx = 8.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).

  lv_idx = 9.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).

  PERFORM create_sheet2 USING lif_spreadsheet.

ENDFORM.



FORM create_sheet2 USING lif_spreadsheet TYPE REF TO  yif_a2g_json  .
  DATA: lv_title TYPE string.
  DATA: lv_col TYPE i.

  DATA(lif_sheets) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_sheets ).
  DATA(lif_sheets_prop) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_properties  ).

  lv_title = 'Vendor Data'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_title
                                  i_value = REF #( lv_title ) ).


  lv_title = 'GRID'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_sheettype
                                  i_value = REF #( lv_title ) ).

  lv_title = '0'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_index
                                  i_value = REF #( lv_title ) ).

  lv_title = '2'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_sheetid
                                  i_value = REF #( lv_title ) ).



  DATA(lif_sheets_data) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_data ).

  SELECT SINGLE * FROM lfa1 INTO @DATA(ls_lfa1) WHERE lifnr = @p_lifnr.
  DATA: ls_sheet_data TYPE ycl_a2g_json_griddata=>ty_s_json_griddata.
  PERFORM add_head0 CHANGING ls_sheet_data-row_data.
  PERFORM add_head0 CHANGING ls_sheet_data-row_data.

  PERFORM add_head1 USING 'Vendor'
                          ls_lfa1-name1
                    CHANGING ls_sheet_data-row_data.

  PERFORM add_head1 USING 'Vendor ID'
                          ls_lfa1-lifnr
                    CHANGING ls_sheet_data-row_data.

  PERFORM add_head0 CHANGING ls_sheet_data-row_data.

  PERFORM add_head1 USING 'Street'
                          ls_lfa1-stras
                    CHANGING ls_sheet_data-row_data.

  PERFORM add_head1 USING 'District'
                          ls_lfa1-ort01
                    CHANGING ls_sheet_data-row_data.

  PERFORM add_head1 USING 'Postal code'
                          ls_lfa1-pstlz
                    CHANGING ls_sheet_data-row_data.

  PERFORM add_head2 USING 'Country'
                          ls_lfa1-land1
                          'Region'
                          ls_lfa1-regio
                    CHANGING ls_sheet_data-row_data.

  PERFORM add_head0 CHANGING ls_sheet_data-row_data.
  PERFORM add_head0 CHANGING ls_sheet_data-row_data.
  PERFORM add_head0 CHANGING ls_sheet_data-row_data.

  PERFORM add_headline1 USING 'Order list update at'
                              'Customer ID'  ls_lfa1-lifnr
                    CHANGING ls_sheet_data-row_data.

  SELECT ekpo~ebeln, ekpo~ebelp, ekpo~matnr, ekpo~txz01, ekpo~matkl, ekpo~menge, ekpo~meins, ekpo~brtwr, ekko~waers
   INTO TABLE @DATA(lt_data)
      FROM ekko
           INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln
           WHERE ekko~lifnr = @ls_lfa1-lifnr..

  PERFORM add_headline2 USING 'Order No'
                              'Position'
                              'Material No'
                              'Description'
                              'Material Group'
                              'Quantity'
                              'UM'
                              'Gross value'
                              'Currency'
                    CHANGING ls_sheet_data-row_data.

  LOOP AT lt_data INTO DATA(ls_data).
    PERFORM add_item USING
    ls_data-ebeln ls_data-ebelp ls_data-matnr ls_data-txz01
    ls_data-matkl ls_data-menge ls_data-meins ls_data-brtwr
    ls_data-waers
                      CHANGING ls_sheet_data-row_data.
  ENDLOOP.


  DO 9 TIMES.
    APPEND INITIAL LINE TO ls_sheet_data-column_metadata ASSIGNING FIELD-SYMBOL(<fs_col>).
    <fs_col>-pixel_size = '150'.
  ENDDO.
  lif_sheets_data->yif_a2g_context~write_data( REF #( ls_sheet_data ) ).



  DATA(lif_sheets_merge) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).
  DATA lv_idx TYPE i.
  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 3.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).


  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).
  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 3.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 4.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).


  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).
  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 6.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).

  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).
  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 6.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 7.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).


  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).
  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 5.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 7.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 8.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).

  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).

  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 7.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 8.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 9.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).
  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).

  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 0.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 3.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 12.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 13.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).
  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).

  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 6.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 10.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 12.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 13.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).

  lif_sheets_merge = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_merges ).


  lv_idx = 2.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_sheetid
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 0.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 1.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endcolumnindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = 13.
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_startrowindex
                                  i_value = REF #( lv_idx ) ).
  lv_idx = lines( lt_data ).
  lif_sheets_merge->set_attribute( i_name =  ycl_a2g_json_gridrange=>gc_fnam_endrowindex
                                  i_value = REF #( lv_idx ) ).

*** add chart
  DATA(lif_sheets_chart) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_charts ).
*check 1 = 2.
  DATA: ls_chart TYPE ycl_a2g_json_embeddedchart=>ty_s_json_embeddedchart.
  ls_chart-chart_id = '1'.
  ls_chart-spec-basic_chart-chart_type = 'COLUMN'.
  APPEND INITIAL LINE TO ls_chart-spec-basic_chart-series ASSIGNING FIELD-SYMBOL(<fs_serie>).
  APPEND INITIAL LINE TO <fs_serie>-series-source_range-sources ASSIGNING FIELD-SYMBOL(<fs_source>).
  <fs_source>-sheet_id = '2'.
  <fs_source>-start_row_index = 14.
  <fs_source>-end_row_index = lines( lt_data ).
  <fs_source>-start_column_index = 8.
  <fs_source>-end_column_index = 9.



  APPEND INITIAL LINE TO ls_chart-spec-basic_chart-domains ASSIGNING FIELD-SYMBOL(<fs_ax>).
*-domain-source_range-sources
  APPEND INITIAL LINE TO <fs_ax>-domain-source_range-sources ASSIGNING <fs_source>.
  <fs_source>-sheet_id = '2'.
  <fs_source>-start_row_index = 14.
  <fs_source>-end_row_index = lines( lt_data )..
  <fs_source>-start_column_index = 5.
  <fs_source>-end_column_index = 6.

  ls_chart-position-overlay_position-anchor_cell-sheet_id = 2.
  ls_chart-position-overlay_position-offset_x_pixels = 10.
  ls_chart-position-overlay_position-offset_y_pixels = 10.
  ls_chart-position-overlay_position-width_pixels = 514.
  ls_chart-position-overlay_position-height_pixels = 230.

  lif_sheets_chart->yif_a2g_context~write_data(  REF #( ls_chart ) ).


ENDFORM.



FORM add_head0 CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  DO 4 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).

  <fs_cell>-user_entered_format-borders-left-style = 'SOLID_MEDIUM'.
  <fs_cell>-user_entered_format-borders-left-width = 2.

  DO 4 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID_MEDIUM'.
  <fs_cell>-user_entered_format-borders-right-width = 2.



ENDFORM.



FORM add_head1 USING title
                     value
               CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  DO 4 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_value-string_value = title.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID_MEDIUM'.
  <fs_cell>-user_entered_format-borders-left-width = 2.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = value.
  <fs_cell>-user_entered_format-background_color-red = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-green = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8509804'.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID_MEDIUM'.
  <fs_cell>-user_entered_format-borders-right-width = 2.



ENDFORM.



FORM add_head2 USING title1
                     value1
                     title2
                     value2
               CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  DO 4 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_value-string_value = title1.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  <fs_cell>-user_entered_format-borders-left-style = 'SOLID_MEDIUM'.
  <fs_cell>-user_entered_format-borders-left-width = 2.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = value1.
  <fs_cell>-user_entered_format-background_color-red = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-green = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8509804'.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title2.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.


  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = value2.
  <fs_cell>-user_entered_format-background_color-red = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-green = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8509804'.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-borders-right-style = 'SOLID_MEDIUM'.
  <fs_cell>-user_entered_format-borders-right-width = 2.


ENDFORM.


FORM add_headline1 USING title1 title2 title3
               CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_value-string_value = title1 && sy-datum.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.


  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title2.
  <fs_cell>-user_entered_format-background_color-red = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-green = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8509804'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title3.
  <fs_cell>-user_entered_format-background_color-red = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-green = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8509804'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_format-background_color-red = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-green = '0.8509804'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8509804'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.



ENDFORM.


FORM add_headline2 USING title1 title2 title3 title4 title5 title6 title7 title8 title9
               CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_format-background_color-red = '0.5529412'.
  <fs_cell>-user_entered_format-background_color-green = '0.7058824'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8862745'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title1.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title2.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title3.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title4.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title5.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title6.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title7.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title8.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title9.
  <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
  <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.


ENDFORM.



FORM add_item USING title1 title2 title3 title4 title5 title6 title7 title8 title9
               CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_format-background_color-red = '0.5529412'.
  <fs_cell>-user_entered_format-background_color-green = '0.7058824'.
  <fs_cell>-user_entered_format-background_color-blue = '0.8862745'.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title1.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title2.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title3.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title4.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title5.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title6.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title7.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-number_value = title8.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING <fs_cell>.
  <fs_cell>-user_entered_value-string_value = title9.
  PERFORM border CHANGING <fs_cell>-user_entered_format-borders.


ENDFORM.


FORM border CHANGING cborder TYPE ycl_a2g_json_borders=>ty_s_json_borders.

  cborder-bottom-style = 'SOLID'.
  cborder-bottom-width = 1.
  cborder-top-style = 'SOLID'.
  cborder-top-width = 1.
  cborder-right-style = 'SOLID'.
  cborder-right-width = 1.
  cborder-left-style = 'SOLID'.
  cborder-left-width = 1.
ENDFORM.



MODULE pbo_0110 OUTPUT.
  DATA go_cust_cont TYPE REF TO cl_gui_custom_container.
  DATA:picture TYPE REF TO cl_gui_picture.

  IF go_cust_cont IS INITIAL.
    CREATE OBJECT go_cust_cont
      EXPORTING
        container_name = 'CUSTOM_CONTAINER'.
    CREATE OBJECT picture
      EXPORTING
        parent = go_cust_cont
      EXCEPTIONS
        error  = 1.

    DATA url(255).
    CLEAR url.
    PERFORM load_pic_from_db CHANGING url.

* load picture
    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    picture->set_display_mode( display_mode = picture->DISPLAY_MODE_FIT ).


  ENDIF.
ENDMODULE.

FORM load_pic_from_db CHANGING url.
  DATA query_table LIKE w3query OCCURS 1 WITH HEADER LINE.
  DATA html_table LIKE w3html OCCURS 1.
  DATA return_code LIKE  w3param-ret_code.
  DATA content_type LIKE  w3param-cont_type.
  DATA content_length LIKE  w3param-cont_len.
  DATA pic_data LIKE w3mime OCCURS 0.
  DATA pic_size TYPE i.

  REFRESH query_table.
  query_table-name = '_OBJECT_ID'.
  query_table-value = 'ZABAP2GSHEET'.
*  query_table-value = 'ZTECHEDGE'.
  APPEND query_table.

  ##FM_OLDED
  CALL FUNCTION 'WWW_GET_MIME_OBJECT'
    TABLES
      query_string        = query_table
      html                = html_table
      mime                = pic_data
    CHANGING
      return_code         = return_code
      content_type        = content_type
      content_length      = content_length
    EXCEPTIONS
      object_not_found    = 1
      parameter_not_found = 2
      OTHERS              = 3.
  IF sy-subrc = 0.
    pic_size = content_length.
  ENDIF.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      ##NO_TEXT
      type     = 'image'
      subtype  = cndp_sap_tab_unknown
      size     = pic_size
      lifetime = cndp_lifetime_transaction
    TABLES
      data     = pic_data
    CHANGING
      url      = url
      ##FM_SUBRC_OK
    EXCEPTIONS
      OTHERS   = 1.


ENDFORM.                               " LOAD_PIC_FROM_DB
