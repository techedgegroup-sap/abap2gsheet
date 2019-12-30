CLASS ycl_a2g_gdrive_file_api DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_http_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_endpoint_url    TYPE string VALUE 'https://www.googleapis.com/drive/v3/files' ##NO_TEXT.
    CONSTANTS gc_upload_endpoint TYPE string VALUE 'https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart' ##NO_TEXT.
    CONSTANTS gc_fcode_update      TYPE string VALUE 'UPDATE'       ##NO_TEXT.
    CONSTANTS gc_fcode_copy        TYPE string VALUE 'COPY'       ##NO_TEXT.
    CONSTANTS gc_fcode_delete      TYPE string VALUE 'DELETE'          ##NO_TEXT.
    CONSTANTS gc_fcode_list_filter TYPE syst_ucomm VALUE 'LIST_FILTER'          ##NO_TEXT.
    CONSTANTS gc_fcode_upload      TYPE syst_ucomm VALUE 'UPLOAD'          ##NO_TEXT.
    CONSTANTS gc_fname_string TYPE string VALUE '{fileName}'.
    CONSTANTS gc_mime_string TYPE string VALUE '{mime-type}'.
    CONSTANTS gc_content TYPE string VALUE 'Content-Disposition'.
    CONSTANTS gc_form TYPE string VALUE 'form-data; name=&quot;metadata&quot;'.
    CONSTANTS gc_json TYPE string VALUE 'application/json'.
    CONSTANTS gc_name_mime TYPE string VALUE '{ name: &apos;{fileName}&apos;, mimeType: &apos;{mime-type}&apos; }'.

    DATA: gv_file_id TYPE string.
    DATA: gv_folder_id TYPE string.
    METHODS yif_a2g_command~execute REDEFINITION.
*    METHODS init_default REDEFINITION.

    "! Setter method for file id
    "! @parameter im_fileid   | target
    METHODS set_fileid
      IMPORTING !im_fileid TYPE string.

    METHODS set_folder_id
      IMPORTING !im_folder_id TYPE string.

    METHODS rebuild_data REDEFINITION.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.

    METHODS file_update.
    METHODS file_copy.
    METHODS file_delete.
    METHODS file_list_filter.
    METHODS file_upload.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_a2g_gdrive_file_api IMPLEMENTATION.

  METHOD yif_a2g_command~execute.

    "&  Source Part
    super->yif_a2g_command~execute( im_fcode ).

    CASE im_fcode.
      WHEN gc_fcode_update.       me->file_update( ).
      WHEN  gc_fcode_copy.        me->file_copy( ).
      WHEN  gc_fcode_delete.      me->file_delete( ).
      WHEN  gc_fcode_list_filter. me->file_list_filter( ).
      WHEN  gc_fcode_upload.      me->file_upload( ).
    ENDCASE.


  ENDMETHOD.                    "yif_a2g_command~execute

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD set_fileid.
    "&  Source Part
    CHECK me->gv_file_id <> im_fileid.
    me->gv_file_id = im_fileid.
  ENDMETHOD.

  METHOD set_folder_id.
    "&  Source Part
    CHECK me->gv_folder_id <> im_folder_id.
    me->gv_folder_id = im_folder_id.
  ENDMETHOD.

  METHOD file_copy.
    DATA: lv_json_file TYPE string,
          ip_file      TYPE string,
          lv_target    TYPE string VALUE gc_endpoint_url.

    lv_target = me->gc_endpoint_url && '/' && me->gv_file_id && '/copy'.

    me->decode_abap2json( IMPORTING ep_json      = lv_json_file
                          CHANGING  cp_abap_data = ip_file ).
    TRY.
        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_ip_json_request( lv_json_file ).
        me->set_method( me->gc_method_post_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_post_request ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.

  METHOD file_delete.
    DATA: lt_param  TYPE tihttpnvp,
          ls_param  TYPE ihttpnvp,
          lv_target TYPE string VALUE gc_endpoint_url.

    lv_target = me->gc_endpoint_url && '/' && me->gv_file_id.
    TRY.
        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_method( me->gc_method_delete_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_delete_request ).

      CATCH ycx_a2g_core_api.
    ENDTRY.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD file_list_filter.

    TRY.
        me->set_param_kind( 'H' ).
        me->set_target( me->gc_endpoint_url ).
        me->set_method( me->gc_method_get_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_get_request ).

      CATCH ycx_a2g_core_api.
    ENDTRY.


  ENDMETHOD.


  METHOD file_update.

    DATA: lv_target TYPE string.

    lv_target = me->gc_endpoint_url && '/' && me->gv_file_id && '?addParents=' && me->gv_folder_id.

    TRY.
        me->set_param_kind( 'H' ).
        me->set_target( lv_target ).
        me->set_method( me->gc_method_get_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_get_request ).

      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.

  METHOD file_upload.

    DATA: "lv_target       TYPE string VALUE gc_upload_endpoint,
      lv_resp_string  TYPE string,
      lv_param_kind   TYPE string,
      lt_multipart    TYPE ya2google_t_mltprt,
      ls_part         TYPE ya2google_s_mltprt,
      ls_param        TYPE ihttpnvp,
      lv_json_request TYPE string VALUE ''.
*          lv_mime_string  TYPE string,
*          lv_fname_string TYPE string.



*    ls_param-name =  me->gc_content.
*
*    ls_param-value = me->gc_form. "
*    ls_part-content_type = me->gc_json.
*    ls_part-cdata =  me->gc_name_mime.
*
*    REPLACE me->gc_fname_string WITH ip_filename INTO ls_part-cdata.
*
*    IF ip_destination_mime_type IS NOT INITIAL.
*      REPLACE gc_mime_string WITH ip_destination_mime_type INTO ls_part-cdata.
*    ELSE.
*      REPLACE gc_mime_string WITH ip_original_mime_type INTO ls_part-cdata.
*    ENDIF.
*
*    APPEND ls_param TO ls_part-header_fields.
*    APPEND ls_part TO lt_multipart.
*    CLEAR: ls_part,
*          ls_param.
*
*
*    ls_part-content_type = ip_original_mime_type.
*    ls_param-name =  TEXT-002.
*
*    APPEND ls_param TO ls_part-header_fields.
*    ls_part-data = ip_file_xstring.
*    APPEND ls_part TO lt_multipart.
*    CLEAR: ls_part, ls_param.


    TRY.
        me->set_param_kind( 'H' ).
        me->set_target( me->gc_upload_endpoint ).
        me->set_method( me->gc_method_get_request ).
        me->client_open(  ).
        me->yif_a2g_command~execute( me->gc_fcode_get_request ).

      CATCH ycx_a2g_core_api.
    ENDTRY.



  ENDMETHOD.


ENDCLASS.
