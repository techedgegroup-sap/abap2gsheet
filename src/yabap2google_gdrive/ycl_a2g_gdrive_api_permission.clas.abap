CLASS ycl_a2g_gdrive_api_permission DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_http_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_endpoint_url TYPE string VALUE 'https://www.googleapis.com/drive/v3/files/fileId/permissions' ##NO_TEXT.
    CONSTANTS gc_fcode_perm_create  TYPE syst_ucomm VALUE 'PERM_CREATE'       ##NO_TEXT.
    CONSTANTS gc_fcode_perm_delete  TYPE syst_ucomm VALUE 'PERM_DELETE'       ##NO_TEXT.
    CONSTANTS gc_fcode_perm_get     TYPE syst_ucomm VALUE 'PERM_GET'          ##NO_TEXT.
    CONSTANTS gc_fcode_post         TYPE syst_ucomm VALUE 'POST'          ##NO_TEXT.

    DATA: gv_file_id TYPE string.
    METHODS yif_a2g_command~execute REDEFINITION.


    "! Setter method for target
    "! @parameter im_file          | target file
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_file
      IMPORTING !im_file TYPE string
      RAISING   ycx_a2g_core_api.

    METHODS rebuild_data REDEFINITION.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.

    METHODS permission_create.
    METHODS permisison_delete.
    METHODS permission_get_all.
  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_a2g_gdrive_api_permission IMPLEMENTATION.

  METHOD rebuild_data.
  ENDMETHOD .

  METHOD generate_rules.
  ENDMETHOD.


  METHOD set_file.

    "&  Source Part
    CHECK me->gv_file_id <> im_file.
    me->gv_file_id = im_file.
  ENDMETHOD.

  METHOD yif_a2g_command~execute.

    "&  Source Part
    super->yif_a2g_command~execute( im_fcode ).
    IF NOT me->gs_http_api-method IS INITIAL.

      me->gif_http_client->request->set_method( method = me->gs_http_api-method ).

      CASE im_fcode.
        WHEN me->gc_fcode_perm_create.  me->permission_create( ).
        WHEN me->gc_fcode_perm_delete.  me->permisison_delete( ).
        WHEN me->gc_fcode_perm_get.      me->permission_get_all( ).
      ENDCASE.

    ENDIF.

  ENDMETHOD.                    "yif_a2g_command~execute


  METHOD permission_create.

    DATA: lt_param    TYPE tihttpnvp,
          ls_param    TYPE ihttpnvp,
          lv_response TYPE string,
          lv_target   TYPE string,
          lv_json_req TYPE string.


*    zcl_google_http_api=>decode_abap2json( IMPORTING ep_json      = lv_json_req
*                                           CHANGING  cp_abap_data = ip_permission ).

    lv_target = me->gc_endpoint_url.
    REPLACE 'fileId' WITH me->gv_file_id INTO lv_target .
    TRY.
        me->set_target( lv_target ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post ).
      CATCH ycx_a2g_core_api.
    ENDTRY.
  ENDMETHOD.                                             "#EC CI_VALPAR
  METHOD permisison_delete.
  ENDMETHOD.

  METHOD permission_get_all.
  ENDMETHOD.
ENDCLASS.
