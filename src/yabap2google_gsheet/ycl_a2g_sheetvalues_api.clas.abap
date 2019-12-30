CLASS ycl_a2g_sheetvalues_api DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_http_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_fcode_append           TYPE syst_ucomm VALUE 'append'                  ##NO_TEXT.
    CONSTANTS gc_fcode_batch_clear      TYPE syst_ucomm VALUE 'batchClear'              ##NO_TEXT.
    CONSTANTS gc_fcode_batch_clear_flt  TYPE syst_ucomm VALUE 'batchClearByDataFilter'  ##NO_TEXT.
    CONSTANTS gc_fcode_batch_get        TYPE syst_ucomm VALUE 'batchGet'  ##NO_TEXT.
    CONSTANTS gc_fcode_batch_get_flt    TYPE syst_ucomm VALUE 'batchGetByDataFilter'  ##NO_TEXT.
    CONSTANTS gc_fcode_batch_update     TYPE syst_ucomm VALUE 'batchUpdate'  ##NO_TEXT.
    CONSTANTS gc_fcode_batch_update_flt TYPE syst_ucomm VALUE 'batchUpdateByDataFilter'  ##NO_TEXT.
    CONSTANTS gc_fcode_clear            TYPE syst_ucomm VALUE 'clear'  ##NO_TEXT.
    CONSTANTS gc_fcode_get              TYPE syst_ucomm VALUE 'get'  ##NO_TEXT.
    CONSTANTS gc_fcode_update           TYPE syst_ucomm VALUE 'update'  ##NO_TEXT.
    CONSTANTS gc_endpoint_url           TYPE string     VALUE 'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheetId}/values/{Range}' ##NO_TEXT.



    TYPES: BEGIN OF ty_s_sheetvalues_api,
             spreadsheet_id TYPE string,
             range          TYPE string,
           END OF ty_s_sheetvalues_api.

    DATA:
      gs_sheetvalues_api  TYPE ty_s_sheetvalues_api,
      gv_spreadsheet_id   TYPE string,
      gv_range            TYPE string.

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

    METHODS set_range
      IMPORTING !i_range TYPE string.

    METHODS yif_a2g_google_api~new_jclass_for_request  REDEFINITION.
    METHODS yif_a2g_google_api~get_jclass_for_response REDEFINITION.
    METHODS yif_a2g_google_api~set_query_parameter REDEFINITION.

  PROTECTED SECTION.
    DATA: gt_parameters TYPE tihttpnvp.

    METHODS generate_rules REDEFINITION.
    METHODS append.
    METHODS batch_clear.
    METHODS batch_clear_flt.
    METHODS batch_get.
    METHODS batch_get_flt.
    METHODS batch_update.
    METHODS batch_update_flt.
    METHODS clear.
    METHODS get.
    METHODS update.

  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_A2G_SHEETVALUES_API IMPLEMENTATION.


  METHOD append.
    DATA: lv_target   TYPE string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':append'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '{Range}' WITH me->gv_range  INTO lv_target.


    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_append && me->gc_suffix_req ).


    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_params( me->gt_parameters  ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json)  =  lo_json_factory->build_json_instance( 'YCL_A2G_JSON_APPEND_RESPONSE' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data  ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_append && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD batch_clear.
    DATA: lv_target   TYPE        string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.


    lv_target = me->gc_endpoint_url && ':batchClear'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '/{Range}' WITH ''  INTO lv_target.
    CONDENSE lv_target NO-GAPS.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_clear && me->gc_suffix_req ).


    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_ip_json_request( lif_context->read_json_data( ) ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHCLEAR_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_batch_clear && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD batch_clear_flt.
    DATA: lv_target   TYPE string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':batchClearByDataFilter'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '/{Range}' WITH ''  INTO lv_target.
    CONDENSE lv_target NO-GAPS.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_clear_flt && me->gc_suffix_req ).

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).
        me->close_client(  ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHCLEAR_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_batch_clear_flt && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD batch_get.
    DATA: lv_target TYPE string     VALUE gc_endpoint_url,
          lo_object TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':batchGet'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '/{Range}' WITH ''  INTO lv_target.
    CONDENSE lv_target NO-GAPS.

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_method( me->gc_method_get_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_get_request ).
        me->close_client(  ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHGET_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_batch_get && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD batch_get_flt.
    DATA: lv_target   TYPE string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':batchClearByDataFilter'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '/{Range}' WITH ''  INTO lv_target.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_get_flt && me->gc_suffix_req ).


    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_method( me->gc_method_get_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_get_request ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).

        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHGET_DFLT_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_batch_get_flt && me->gc_suffix_res
                                      im_object = lo_object ).
      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD batch_update.
    DATA: lv_target   TYPE string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':batchUpdate'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '/{Range}' WITH ''  INTO lv_target.
    condense lv_target NO-GAPS.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_update && me->gc_suffix_req ).

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).
        me->close_client(  ).
        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHUPDATE_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_batch_update && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD batch_update_flt.
    DATA: lv_target   TYPE string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':batchUpdateByDataFilter'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '/{Range}' WITH ''  INTO lv_target.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_update_flt && me->gc_suffix_req ).

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).
        me->close_client(  ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHUPDATE_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_batch_update_flt && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD clear.
    DATA: lv_target TYPE string     VALUE gc_endpoint_url,
          lo_object TYPE REF TO object.

    lv_target = me->gc_endpoint_url && ':clear'.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '{Range}' WITH me->gv_range  INTO lv_target.

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).

        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_BATCHCLEAR_RES' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_clear && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( if_msg_manager = if_msg_manager
                        im_target      = im_target
                        im_param_kind  = im_param_kind ).

    me->gv_data = REF #( me->gs_sheetvalues_api ).

  ENDMETHOD.


  METHOD generate_rules.
* do nothing
  ENDMETHOD.


  METHOD get.
    DATA: lv_target TYPE string     VALUE gc_endpoint_url,
          lo_object TYPE REF TO object.


    lv_target = me->gc_endpoint_url.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '{Range}' WITH me->gv_range  INTO lv_target.

    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_method( me->gc_method_get_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_get_request ).
        me->close_client(  ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).

        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_VALUERANGE' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_clear && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD rebuild_data.
* do nothing
    me->set_spreadsheet_id( me->gs_sheetvalues_api-spreadsheet_id  ).
    me->set_range( me->gs_sheetvalues_api-range  ).
  ENDMETHOD .


  METHOD set_range.
    "&  Source Part
    CHECK me->gv_range <> i_range.
    me->gv_range = i_range.
  ENDMETHOD.


  METHOD set_spreadsheet_id.
    "&  Source Part
    CHECK me->gv_spreadsheet_id <> i_spreadsheet_id.
    me->gv_spreadsheet_id = i_spreadsheet_id.
  ENDMETHOD.


  METHOD update.
    DATA: lv_target   TYPE string     VALUE gc_endpoint_url,
          lif_context TYPE REF TO yif_a2g_json_context,
          lo_object   TYPE REF TO object.


    lv_target = me->gc_endpoint_url.
    REPLACE '{spreadsheetId}' WITH me->gv_spreadsheet_id  INTO lv_target.
    REPLACE '{Range}' WITH me->gv_range  INTO lv_target.

    lif_context ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_update && me->gc_suffix_req ).


    TRY.

        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_params( me->gt_parameters  ).
        me->set_ip_json_request( lif_context->read_json_data(  ) ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).
        me->close_client(  ).

        DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).

        DATA(lif_json) = lo_json_factory->build_json_instance( 'YCL_A2G_JSON_UPD_VAL_RESPONSE' ).

        lif_json->yif_a2g_json_context~write_json_data( me->gv_response_data ).
        lo_object ?= lif_json.
        me->go_sub_json->setinstance( im_name = me->gc_fcode_clear && me->gc_suffix_res
                                      im_object = lo_object ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD yif_a2g_command~execute.
    "&  Source Part
    super->yif_a2g_command~execute( im_fcode ).

    CASE im_fcode.
      WHEN gc_fcode_append.           me->append( ).
      WHEN gc_fcode_batch_clear.      me->batch_clear( ).
      WHEN gc_fcode_batch_clear_flt.  me->batch_clear_flt( ).
      WHEN gc_fcode_batch_get.        me->batch_get( ).
      WHEN gc_fcode_batch_get_flt.    me->batch_get_flt( ).
      WHEN gc_fcode_batch_update.     me->batch_update( ).
      WHEN gc_fcode_batch_update_flt. me->batch_update_flt( ).
      WHEN gc_fcode_clear.            me->clear( ).
      WHEN gc_fcode_get.              me->get( ).
      WHEN gc_fcode_update.           me->update( ).
    ENDCASE.

  ENDMETHOD.


  METHOD yif_a2g_google_api~get_jclass_for_response.
* to be defined
    CASE method.
      WHEN gc_fcode_append.           return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_append           && me->gc_suffix_res ).
      WHEN gc_fcode_batch_clear.      return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_clear      && me->gc_suffix_res ).
      WHEN gc_fcode_batch_clear_flt.  return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_clear_flt  && me->gc_suffix_res ).
      WHEN gc_fcode_batch_get.        return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_get        && me->gc_suffix_res ).
      WHEN gc_fcode_batch_get_flt.    return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_get_flt    && me->gc_suffix_res ).
      WHEN gc_fcode_batch_update.     return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_update     && me->gc_suffix_res ).
      WHEN gc_fcode_batch_update_flt. return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_batch_update_flt && me->gc_suffix_res ).
      WHEN gc_fcode_clear.            return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_clear            && me->gc_suffix_res ).
      WHEN gc_fcode_get.              return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_get              && me->gc_suffix_res ).
      WHEN gc_fcode_update.           return ?= me->go_sub_json->getinstance( im_name = me->gc_fcode_update           && me->gc_suffix_res ).
    ENDCASE.

  ENDMETHOD.


  METHOD yif_a2g_google_api~new_jclass_for_request.
    DATA: lo_object    TYPE REF TO object,
          lv_classname TYPE string,
          lv_name      TYPE syst_ucomm.
    DATA(lo_json_factory) = NEW ycl_a2g_json_factory( me->go_msg_manager ).
    CASE method.
      WHEN gc_fcode_append.
        lv_classname =  'YCL_A2G_JSON_VALUERANGE'.
        lv_name = me->gc_fcode_append.
      WHEN gc_fcode_batch_clear.
        lv_classname =  'YCL_A2G_JSON_BATCHCLEAR_REQ'.
        lv_name = me->gc_fcode_batch_clear.
      WHEN gc_fcode_batch_clear_flt.
        lv_classname =  'YCL_A2G_JSON_DATAFILTER'.
        lv_name = me->gc_fcode_batch_clear_flt.
      WHEN gc_fcode_batch_get_flt.
        lv_classname =  'YCL_A2G_JSON_BATCHGET_DFLT_REQ'.
        lv_name = me->gc_fcode_batch_get_flt.
      WHEN gc_fcode_batch_update.
        lv_classname =  'YCL_A2G_JSON_BATCHUPDATE_REQ'.
        lv_name = me->gc_fcode_batch_update.
      WHEN gc_fcode_batch_update_flt.
        lv_classname =  'YCL_A2G_JSON_BATCHUPDATE_FLT_REQ'.
        lv_name = me->gc_fcode_batch_update_flt.
      WHEN gc_fcode_batch_update_flt.
        lv_classname =  'YCL_A2G_JSON_VALUERANGE'.
        lv_name = me->gc_fcode_batch_update_flt.
      WHEN gc_fcode_update.
        lv_classname =  'YCL_A2G_JSON_VALUERANGE'.
        lv_name = me->gc_fcode_update.
    ENDCASE.
    IF NOT lv_name IS INITIAL.
      return = lo_json_factory->build_json_instance( lv_classname ).
      lo_object = return.
      me->go_sub_json->setinstance( im_name = lv_name && me->gc_suffix_req
                                    im_object = lo_object ).
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_google_api~set_query_parameter.
    CLEAR me->gt_parameters.
    me->gt_parameters = i_parameters.
  ENDMETHOD.
ENDCLASS.
