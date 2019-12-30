CLASS ycl_a2g_sheet_api DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_http_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_fcode_copy   TYPE syst_ucomm VALUE 'COPY_TO'           ##NO_TEXT.
    CONSTANTS gc_endpoint_url TYPE string VALUE 'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheetId}/sheets/{sheetId}' ##NO_TEXT.

    TYPES: BEGIN OF ty_s_sheet_api,
             spreadsheet_id TYPE string,
             sheet_id       TYPE i,
           END OF ty_s_sheet_api.

    DATA:
      gs_sheet_api      TYPE ty_s_sheet_api,
      gv_spreadsheet_id TYPE string,
      gv_sheet_id       TYPE string.

    "! Constructor
    "! @parameter if_msg_manager  | Cmp. Appl. - Abap 2 Google - Message Manager interface
    "! @parameter im_target       | Target
    "! @parameter im_param_kind   | Param Kind
    METHODS constructor
      IMPORTING !if_msg_manager TYPE REF TO yif_a2g_msg_manager
                !im_target      TYPE string OPTIONAL
                !im_param_kind  TYPE string OPTIONAL.

    METHODS yif_a2g_command~execute REDEFINITION.
    METHODS rebuild_data REDEFINITION.

    METHODS set_spreadsheet_id
      IMPORTING !i_spreadsheet_id TYPE string.

    METHODS set_sheet_id
      IMPORTING !i_sheet_id TYPE i.

    METHODS yif_a2g_google_api~new_jclass_for_request  REDEFINITION.
    METHODS yif_a2g_google_api~get_jclass_for_response REDEFINITION.
    METHODS yif_a2g_google_api~set_query_parameter REDEFINITION.

  PROTECTED SECTION.
    DATA: gt_parameters TYPE tihttpnvp.
    METHODS generate_rules REDEFINITION.
    METHODS: copy_to.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_sheet_api IMPLEMENTATION.

  METHOD yif_a2g_google_api~set_query_parameter.
    CLEAR me->gt_parameters.
    me->gt_parameters = i_parameters.
  ENDMETHOD.


  METHOD yif_a2g_google_api~new_jclass_for_request.
* do nothing
    DATA: lo_object    TYPE REF TO object,
          lv_classname TYPE string,
          lv_name      TYPE syst_ucomm.
    DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
    CASE method.
      WHEN gc_fcode_copy.
        lv_classname =  'YCL_A2G_JSON_COPYTO_REQ'.
        lv_name = me->gc_fcode_copy.
    ENDCASE.
    IF NOT lv_name IS INITIAL.
      return = lo_json_factory->build_json_instance( lv_classname ).
      lo_object = return.
      me->go_sub_json->setinstance( im_name = lv_name && me->gc_suffix_req
                                    im_object = lo_object ).
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_google_api~get_jclass_for_response.
* to be defined
    CASE method.
      WHEN gc_fcode_copy.             return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_copy           && me->gc_suffix_res ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager = if_msg_manager
                        im_target      = im_target
                        im_param_kind  = im_param_kind ).

    me->gv_data = REF #( me->gs_sheet_api ).

  ENDMETHOD.


  METHOD rebuild_data.
    me->set_spreadsheet_id( me->gs_sheet_api-spreadsheet_id  ).
    me->set_sheet_id( me->gs_sheet_api-sheet_id  ).
  ENDMETHOD .

  METHOD generate_rules.
* do nothing
  ENDMETHOD.

  METHOD set_spreadsheet_id.
    "&  Source Part
    CHECK me->gv_spreadsheet_id <> i_spreadsheet_id.
    me->gv_spreadsheet_id = i_spreadsheet_id.
  ENDMETHOD.

  METHOD set_sheet_id.
    "&  Source Part
    CHECK me->gv_sheet_id <> i_sheet_id.
    me->gv_sheet_id = i_sheet_id.
  ENDMETHOD.

  METHOD yif_a2g_command~execute.
    "&  Source Part
    super->yif_a2g_command~execute( im_fcode ).

    CASE im_fcode.
      WHEN gc_fcode_copy.   me->copy_to( ).
    ENDCASE.

  ENDMETHOD.

  METHOD copy_to.
    DATA: lv_target    TYPE string VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':copyTo'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '{sheetId}' WITH me->gv_sheet_id  INTO lv_target.
    CONDENSE lv_target NO-GAPS.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_copy && me->gc_suffix_req ).

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_params( me->gt_parameters  ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_COPYTO_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data  ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_copy && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.



ENDCLASS.
