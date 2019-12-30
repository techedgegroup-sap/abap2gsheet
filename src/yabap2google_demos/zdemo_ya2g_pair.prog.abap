PROGRAM zdemo_ya2g_pair.

DATA: gv_time TYPE i.

DATA: ok_code            LIKE sy-ucomm,
      save_ok            LIKE sy-ucomm,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_DEMO_0100_CONT1',
      grid1              TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      gs_layout          TYPE lvc_s_layo,
      g_max              TYPE i VALUE 100.
DATA: html_control   TYPE REF TO cl_gui_html_viewer,
      html_container TYPE REF TO cl_gui_docking_container,
      fcode          LIKE sy-ucomm,
      myevent_tab    TYPE cntl_simple_events,
      myevent        TYPE cntl_simple_event,
      edurl(2048),
      alignment      TYPE i.
DATA: lt_sflight TYPE TABLE OF sflight WITH HEADER LINE.
DATA: gv_spreadsheetid TYPE string.
DATA: gif_google_api_creator TYPE REF TO yif_a2g_google_api_creator.
DATA: gs_spread TYPE ycl_a2g_spreadsheet_api=>ty_s_sheet_api.
DATA: gs_sheetvalues TYPE ycl_a2g_sheetvalues_api=>ty_s_sheetvalues_api.
DATA: gv_sync TYPE oax.
DATA: lv_string TYPE string.
DATA: sheet TYPE string.

PARAMETERS pinplace TYPE oax.
SELECTION-SCREEN: SKIP.
SELECTION-SCREEN: SKIP.
SELECTION-SCREEN: SKIP.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK sub FOR 6 LINES,
                  END OF BLOCK sub.





DATA: BEGIN OF gt_outtab OCCURS 0.  "with header line
        INCLUDE STRUCTURE sflight.
        DATA: celltab TYPE lvc_t_styl.
DATA: END OF gt_outtab.

CLASS cl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_navigate_complete
                FOR EVENT navigate_complete OF cl_gui_html_viewer
      IMPORTING url.
    METHODS on_data
                FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.

DATA: lo_handler TYPE REF TO cl_myevent_handler.

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

  METHOD on_data.
    DATA: lt_param TYPE tihttpnvp,
          ls_param TYPE ihttpnvp.

    DATA(go_message) = ycl_a2g_msg_manager=>create_init_msg_manager( ).

    gif_google_api_creator = ycl_a2g_api_creator=>get_api_creator_instance( i_clsname = 'YCL_A2G_SHEETVALUES_APICREATOR'
                                                                        i_msg_manager = go_message ).
    gs_sheetvalues-spreadsheet_id = gv_spreadsheetid.
    DATA(gif_google_api) = gif_google_api_creator->create_instance( ).

    DATA(gif_context)  = gif_google_api->get_context(  ).

    gif_context->write_data( REF #( gs_sheetvalues ) ).
    DATA(gif_command)  = gif_google_api->get_command(  ).

* Adding parameter
    ls_param-name = 'valueInputOption'.
    ls_param-value = 'USER_ENTERED'.
    APPEND ls_param TO lt_param.
    gif_google_api->set_query_parameter( lt_param ).

    DATA: lv_ci TYPE numc2.

    DATA(gif_a2g_json) = gif_google_api->new_jclass_for_request( method = 'batchUpdate' ).
    DATA(lif_data) = gif_a2g_json->new_element( ycl_a2g_json_batchupdate_req=>gc_fnam_data ).


    LOOP AT er_data_changed->mt_mod_cells INTO DATA(ls_mod).
      lv_ci = ls_mod-row_id + 1.
      lv_string = 'E' && lv_ci && ':E'.
      ADD 1 TO lv_ci.
      lv_string = lv_string && lv_ci.
      gs_sheetvalues-range = lv_string.

      gif_context->write_data( REF #( gs_sheetvalues ) ).


      lif_data->set_attribute( i_name = ycl_a2g_json_valuerange=>gc_fnam_range
                                   i_value = REF #( lv_string ) ).

      DATA: lt_strings TYPE stringtab.
      APPEND ls_mod-value TO lt_strings.
      lif_data->set_attribute( i_name = ycl_a2g_json_valuerange=>gc_fnam_values
                                   i_value = REF #( lt_strings ) ).

    ENDLOOP.

*    PERFORM append USING gif_google_api.

* Execute the request via command Execute
    gif_command->execute( ycl_a2g_sheetvalues_api=>gc_fcode_batch_update ).

* get json class for response
    DATA(gif_a2g_json_res) = gif_google_api->get_jclass_for_response( method = 'batchUpdate' ).


  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  sub-dynnr = 110.
  sub-prog = sy-repid.


START-OF-SELECTION.
*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*

  lo_handler = NEW #( ).
  CALL SCREEN 100.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAIN101'.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT grid1
      EXPORTING
        i_parent = g_custom_container.
    PERFORM select_data_and_init_style.

    gs_layout-stylefname = 'CELLTAB'.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        i_structure_name = 'SFLIGHT'
        is_layout        = gs_layout
      CHANGING
        it_outtab        = gt_outtab[].

  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE pai INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT'.
      PERFORM exit_program.
    WHEN 'SWITCH'.
      PERFORM switch_edit_mode.
    WHEN 'GSHEET'.
      PERFORM gsheet_active.
    WHEN 'SYNC'.
      IF     gv_sync = 'X'.
        gv_sync = ' '.
      ELSE.
        gv_sync = 'X'.
      ENDIF.
      PERFORM adjust_alv USING 'DEM' .
    WHEN OTHERS.
*     do nothing
  ENDCASE.
ENDMODULE.
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM exit_program.
  LEAVE PROGRAM.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_and_init_style.
  DATA: lt_celltab TYPE lvc_t_styl,
        l_index    TYPE i.

  SELECT * FROM sflight INTO TABLE lt_sflight UP TO 20 ROWS.
  LOOP AT lt_sflight.
    MOVE-CORRESPONDING lt_sflight TO gt_outtab.
    APPEND gt_outtab.
  ENDLOOP.

  LOOP AT gt_outtab.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF gt_outtab-seatsmax GE 300.
      PERFORM fill_celltab USING 'RW'
                           CHANGING lt_celltab.
    ELSE.
      PERFORM fill_celltab USING 'RO'
                           CHANGING lt_celltab.
    ENDIF.
    INSERT LINES OF lt_celltab INTO TABLE gt_outtab-celltab.
    MODIFY gt_outtab INDEX l_index.
  ENDLOOP.
ENDFORM.                               " SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PT_CELLTAB  text
*----------------------------------------------------------------------*
FORM fill_celltab USING VALUE(p_mode)
                  CHANGING pt_celltab TYPE lvc_t_styl.
  DATA: ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  IF p_mode EQ 'RW'.
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "p_mode eq 'RO'
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  ls_celltab-fieldname = 'CARRID'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'CONNID'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'FLDATE'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'PRICE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'CURRENCY'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'PLANETYPE'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'SEATSMAX'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'SEATSOCC'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'PAYMENTSUM'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_celltab INTO TABLE pt_celltab.

ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.

  IF grid1->is_ready_for_input( ) EQ 0.
* set edit enabled cells ready for input
    CALL METHOD grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
    SET HANDLER lo_handler->on_data FOR grid1.

  ELSE.
* lock edit enabled cells against input
    CALL METHOD grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.
  ENDIF.
ENDFORM.                               " SWITCH_EDIT_MODE



FORM gsheet_active.


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
  DATA(gv_spread) = gif_a2g_json_res->get_attribute( i_name = ycl_a2g_json_spreadsheet=>gc_fnam_spreadsheetid ).

  ASSIGN gv_spread->* TO FIELD-SYMBOL(<fs_spread>).
  gv_spreadsheetid = <fs_spread>.

*** Authoirizzation chech
*  DATA(lt_message) = go_message->get( ).
*  READ TABLE lt_message INTO DATA(ls_message) WITH KEY number = '003'.
*  IF sy-subrc = 0.
*    DATA: l_client_redirection_uri     TYPE string.
*    l_client_redirection_uri = 'https://s4hanalab.r53.techedgegroup.com/sap/bc/webdynpro/sap/oa2c_grant_app?sap-client=100' .
*
*  ENDIF.

**************
  DATA: lv_ext TYPE i.
  IF html_container IS INITIAL.
    IF pinplace = 'X'.
      lv_ext = '700'.
    ELSE.
      lv_ext = '10'.
    ENDIF.

    CREATE OBJECT html_container
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_right
        extension = 500
*       container_name = 'HTML'
      EXCEPTIONS
        OTHERS    = 1.
    CREATE OBJECT html_control
      EXPORTING
        parent = html_container.

    DATA: doc_url(150).
    ASSIGN result->* TO FIELD-SYMBOL(<fs>).
*    IF l_client_redirection_uri IS INITIAL.
      doc_url = <fs>.
*    ELSE.
*      doc_url =  l_client_redirection_uri.
*    ENDIF.
    CALL METHOD html_control->show_url
      EXPORTING
        in_place = pinplace
        url      = doc_url.
  ENDIF.
ENDFORM.



FORM create_sheet USING uif_google_api TYPE REF TO  yif_a2g_google_api .

  DATA: lv_title TYPE string.
  DATA: lv_col TYPE i.

  DATA(lif_spreadsheet) = uif_google_api->new_jclass_for_request( 'CREATE_NEW_SPREADSHEET' ).

  DATA(lif_spread_prop) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_properties ).
  lv_title = 'Abap2Gsheet Demo'.
  lif_spreadsheet->set_attribute( i_name = ycl_a2g_json_spreadsheet_prop=>gc_fnam_title
                                  i_value = REF #( lv_title ) ).
  lif_spreadsheet->set_default( ).


  DATA(lif_sheets) = lif_spreadsheet->new_element(  ycl_a2g_json_spreadsheet=>gc_fnam_sheets ).
  DATA(lif_sheets_prop) = lif_sheets->new_element(  ycl_a2g_json_sheets=>gc_fnam_properties  ).

  lv_title = 'Plane Data'.
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

  DATA: ls_sheet_data TYPE ycl_a2g_json_griddata=>ty_s_json_griddata.


  PERFORM add_headline2 CHANGING ls_sheet_data-row_data.

  LOOP AT lt_sflight INTO DATA(ls_data).
    PERFORM add_item USING ls_data
    CHANGING ls_sheet_data-row_data.
  ENDLOOP.


  DO 9 TIMES.
    APPEND INITIAL LINE TO ls_sheet_data-column_metadata ASSIGNING FIELD-SYMBOL(<fs_col>).
    <fs_col>-pixel_size = '150'.
  ENDDO.
  lif_sheets_data->yif_a2g_context~write_data( REF #( ls_sheet_data ) ).



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


FORM add_headline2 CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .


  SELECT dd03l~fieldname, dd03l~position, dd04t~scrtext_m INTO TABLE @DATA(lt_dd03l)
           FROM dd03l AS dd03l
           INNER JOIN dd04t AS dd04t ON  dd04t~rollname = dd03l~rollname
                                     AND dd04t~ddlanguage = @sy-langu
  WHERE tabname = 'SFLIGHT'.

  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  SORT lt_dd03l BY position ASCENDING.
  LOOP AT lt_dd03l INTO DATA(ls_dd03l).

    APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).
    <fs_cell>-user_entered_value-string_value = ls_dd03l-scrtext_m.
    <fs_cell>-user_entered_format-background_color-red = '0.7490196'.
    <fs_cell>-user_entered_format-background_color-green = '0.7490196'.
    <fs_cell>-user_entered_format-background_color-blue = '0.7490196'.
    PERFORM border CHANGING <fs_cell>-user_entered_format-borders.
  ENDLOOP.

ENDFORM.



FORM add_item USING us_data STRUCTURE sflight
               CHANGING cdata TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata .
  DATA: lv_char_im(10) TYPE c.

  APPEND INITIAL LINE TO cdata ASSIGNING FIELD-SYMBOL(<fs_row>).

  DO.
    CLEAR lv_char_im.
    ASSIGN COMPONENT sy-index OF STRUCTURE us_data TO FIELD-SYMBOL(<fs>).
    IF sy-subrc <> 0. EXIT. ENDIF.
    APPEND INITIAL LINE TO <fs_row>-values ASSIGNING FIELD-SYMBOL(<fs_cell>).

    IF sy-index = 5.
      <fs_cell>-user_entered_value-number_value = <fs>.
    ELSEIF sy-index = 4.
      WRITE <fs> TO lv_char_im USING EDIT MASK '__.__.____'.
      <fs_cell>-user_entered_value-string_value = lv_char_im.
    ELSE.
      <fs_cell>-user_entered_value-string_value = <fs>.
    ENDIF.
    PERFORM border CHANGING <fs_cell>-user_entered_format-borders.

  ENDDO.

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


FORM adjust_alv USING taskname.
  DATA: lv_change TYPE oax.

*  IF taskname = 'GSHEET'.
*    RECEIVE RESULTS FROM FUNCTION 'ZDEMO_TIMER_ALV'
*    IMPORTING ex_change = lv_change.
*    IF lv_change = 'X'.
*    grid1->refresh_table_display( i_soft_refresh = 'X' ).
*    ENDIF.
*  ENDIF.
*  ADD 1 TO gv_time.
*  IF gv_sync = 'X'.
*    CHECK gv_time < 5.
*  BREAK-POINT.
  CALL FUNCTION 'ZDEMO_TIMER_ALV'
*      STARTING NEW TASK 'GSHEET'
*      DESTINATION 'NONE'
*      PERFORMING adjust_alv ON END OF TASK
    EXPORTING
      url    = gv_spreadsheetid
    TABLES
      outtab = gt_outtab.

  grid1->refresh_table_display( i_soft_refresh = 'X' ).


*  ENDIF.
ENDFORM.



FORM append USING uif_google_api TYPE REF TO  yif_a2g_google_api .
  DATA: lt_param TYPE tihttpnvp,
        ls_param TYPE ihttpnvp.

* get json class for request parameter
  DATA(gif_a2g_json) = uif_google_api->new_jclass_for_request( method = 'append' ).


* Adding parameter
  ls_param-name = 'valueInputOption'.
  ls_param-value = 'USER_ENTERED'.
  APPEND ls_param TO lt_param.
  uif_google_api->set_query_parameter( lt_param ).


  lv_string = 'E2:E20'.
  gif_a2g_json->set_attribute( i_name = ycl_a2g_json_valuerange=>gc_fnam_range
                               i_value = REF #( lv_string ) ).

  DATA: lt_strings TYPE stringtab.
  APPEND 'Sample Cell1' TO lt_strings.
  APPEND 'Sample Cell2' TO lt_strings.
  gif_a2g_json->set_attribute( i_name = ycl_a2g_json_valuerange=>gc_fnam_values
                               i_value = REF #( lt_strings ) ).


  CLEAR  lt_strings .
  APPEND 'Sample Cell3' TO lt_strings.
  gif_a2g_json->set_attribute( i_name = ycl_a2g_json_valuerange=>gc_fnam_values
                               i_value = REF #( lt_strings ) ).


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

    picture->set_display_mode( display_mode = picture->display_mode_fit ).


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
