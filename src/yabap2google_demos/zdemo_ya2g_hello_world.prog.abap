*&---------------------------------------------------------------------*
*& Report  SAPHTML_DEMO1                                               *
*&---------------------------------------------------------------------*
REPORT  zdemo_ya2g_hello_world.

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
* get message manager
  DATA(go_message) = ycl_a2g_msg_manager=>create_init_msg_manager( ).
  gif_google_api_creator = ycl_a2g_api_creator=>get_api_creator_instance( i_clsname = 'YCL_A2G_SPREADSHEET_API_CREATE'
                                                                          i_msg_manager = go_message ).
  gs_spread-spreadsheet_id = sheet.
  DATA(gif_google_api) = gif_google_api_creator->create_instance( ).
  DATA(gif_context)  = gif_google_api->get_context(  ).
  gif_context->write_data( REF #( gs_spread ) ).

*  PERFORM create_sheet USING gif_google_api.

  DATA: lv_title TYPE string.
  DATA: lv_col TYPE i.

  DATA(lif_spreadsheet) = gif_google_api->new_jclass_for_request( 'CREATE_NEW_SPREADSHEET' ).

  DATA(lif_spread_prop) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_properties ).
  lv_title = 'Test mio'.
  lif_spreadsheet->set_attribute( i_name = ycl_a2g_json_spreadsheet_prop=>gc_fnam_title
                                  i_value = REF #( lv_title ) ).
  lif_spreadsheet->set_default( ).

  DATA(lif_sheets) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_sheets ).
  DATA(lif_sheets_prop) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_properties  ).

  lv_title = 'Header'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_title     i_value = REF #( lv_title ) ).
  lv_title = 'GRID'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_sheettype i_value = REF #( lv_title ) ).
  lv_title = '0'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_index     i_value = REF #( lv_title ) ).
  lv_title = '1'.
  lif_sheets_prop->set_attribute( i_name = ycl_a2g_json_sheet_prop=>gc_fnam_sheetid   i_value = REF #( lv_title ) ).

  DATA(lif_sheets_data) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_data ).
  DATA: ls_sheet_data TYPE ycl_a2g_json_griddata=>ty_s_json_griddata.

  APPEND INITIAL LINE TO ls_sheet_data-row_data ASSIGNING FIELD-SYMBOL(<fs_row>).
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_value-string_value = 'Hey Google.. Toc Toc..'.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.
  <fs_cell>-user_entered_format-text_format-font_size = '18'.
  <fs_cell>-user_entered_format-horizontal_alignment = 'CENTER'.
  lif_sheets_data->yif_a2g_context~write_data( REF #( ls_sheet_data ) ).

* Execute the request via command Execute
  gif_google_api->get_command(  )->execute( ycl_a2g_spreadsheet_api=>gc_fcode_spsheet_new ).

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

  APPEND INITIAL LINE TO ls_sheet_data-row_data ASSIGNING FIELD-SYMBOL(<fs_row>).
  DO 2 TIMES.
    APPEND INITIAL LINE TO <fs_row>-values.
  ENDDO.
  APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
  <fs_cell>-user_entered_value-string_value = 'ABAP 2 GSHEET Hello World'.
  <fs_cell>-user_entered_format-text_format-italic = 'true'.
  <fs_cell>-user_entered_format-text_format-bold = 'true'.
  <fs_cell>-user_entered_format-text_format-font_size = '18'.
  <fs_cell>-user_entered_format-horizontal_alignment = 'CENTER'.


ENDFORM.


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
