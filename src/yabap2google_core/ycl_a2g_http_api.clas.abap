"!<h1>YIF_A2G_http_api</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - http API base abstract</p>
"! <p>This class is the abstaction of base used to all component element. Not all operations are used
"! in all classes but each class reimplement anc use only the necessary operation</p>
CLASS ycl_a2g_http_api DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM ycl_a2g_objbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_a2g_google_api.

    METHODS yif_a2g_command~execute REDEFINITION.

    "! DELETE Method for HTTP POST operation
    CONSTANTS gc_method_delete_request    TYPE string VALUE 'DELETE' ##NO_TEXT.
    "! PATCH Method for HTTP POST operation
    CONSTANTS gc_method_patch_request     TYPE string VALUE 'PATCH'  ##NO_TEXT.
    "! POST Method for HTTP POST operation
    CONSTANTS gc_method_post_request      TYPE string VALUE 'POST'   ##NO_TEXT.
    "! GET Method for HTTP POST operation
    CONSTANTS gc_method_get_request       TYPE string VALUE 'GET'    ##NO_TEXT.

    "! DELETE Method for HTTP POST operation
    CONSTANTS gc_fcode_delete_request    TYPE syst_ucomm VALUE 'DELETE' ##NO_TEXT.
    "! PATCH Method for HTTP POST operation
    CONSTANTS gc_fcode_patch_request     TYPE syst_ucomm VALUE 'PATCH'  ##NO_TEXT.
    "! POST Method for HTTP POST operation
    CONSTANTS gc_fcode_post_request      TYPE syst_ucomm VALUE 'POST'   ##NO_TEXT.
    "! GET Method for HTTP POST operation
    CONSTANTS gc_fcode_get_request       TYPE syst_ucomm VALUE 'GET'    ##NO_TEXT.

    "! Suffix for store request json instance
    CONSTANTS gc_suffix_req       TYPE string VALUE 'REQ'    ##NO_TEXT.
    "! Suffix for store response json instance
    CONSTANTS gc_suffix_res       TYPE string VALUE 'RES'    ##NO_TEXT.

    "! Speadsheets url prefix
    CONSTANTS gc_spreadsheet_url_prefix TYPE string VALUE 'https://docs.google.com/spreadsheets/d/' ##NO_TEXT.

    "! Google profile to logon
    CONSTANTS google_profile TYPE oa2c_profile VALUE 'ZGOOGLE_SHEETS' ##NO_TEXT.  "--> trasformare in configurazione


    DATA:
      "! Structure with base data
      gs_http_api      TYPE ya2google_s_httpapi READ-ONLY,
      "! response data in string format
      gv_response_data TYPE string READ-ONLY.


    "! Constructor
    "! @parameter if_msg_manager  | Cmp. Appl. - Abap 2 Google - Message Manager interface
    "! @parameter im_target       | Target
    "! @parameter im_param_kind   | Param Kind
    METHODS constructor
      IMPORTING !if_msg_manager TYPE REF TO yif_a2g_msg_manager
                !im_target      TYPE string OPTIONAL
                !im_param_kind  TYPE string OPTIONAL.

    "! Setter method for target
    "! @parameter im_target        | target
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_target
      IMPORTING !im_target TYPE string
      RAISING   ycx_a2g_core_api.

    "! Setter method for method to call
    "! @parameter im_method        | method to execute
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_method
      IMPORTING !im_method TYPE string
      RAISING   ycx_a2g_core_api.

    "! Setter param kind
    "! @parameter im_param_kind    | param kind
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_param_kind
      IMPORTING !im_param_kind TYPE string
      RAISING   ycx_a2g_core_api.

    "! Setter params
    "! @parameter im_params        | Param name & value tab
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_params
      IMPORTING !im_params TYPE tihttpnvp
      RAISING   ycx_a2g_core_api.

    "! Setter json request
    "! @parameter im_json_req      | request in json format
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_ip_json_request
      IMPORTING !im_json_req TYPE string
      RAISING   ycx_a2g_core_api.

    "! This method Setter multiparts fragmet
    "! @parameter im_multiparts    | multiparts fragment
    "! @raising  ycx_a2g_core_api  | Http API Exception if some error found
    METHODS set_multiparts
      IMPORTING !im_multiparts TYPE ya2google_t_mltprt
      RAISING   ycx_a2g_core_api.

    "! display the response via cl_demo "testing only
    METHODS display_response.


    METHODS client_open.
    METHODS init_default
      IMPORTING !im_target     TYPE string
                !im_param_kind TYPE string.

  PROTECTED SECTION.

    "! http client instace
    DATA gif_http_client TYPE REF TO if_http_client.
    "! response instance
    DATA gif_response    TYPE REF TO if_http_response.
    "! array form json metadata subcalss
    DATA: go_sub_json TYPE REF TO ycl_a2g_array.


    "! send & receive request
    METHODS send_receive_request.
    "! generate oauth2 authorizzation
    METHODS set_oauth2.
    "! transform http response in string format
    METHODS trasform_response_string.
    "! att the multipart fragment to the post http request
    METHODS register_multipart.

    "! Orchestration for execution operation Delete
    METHODS send_delete_request.
    "! Orchestration for execution operation Pathc
    METHODS send_patch_request.
    "! Orchestration for execution operation Post
    METHODS send_post_request.
    "! Orchestration for execution operation Get
    METHODS send_get_request.
    "! Close http Client
    METHODS close_client.

    METHODS check_scope REDEFINITION.

    DATA: go_methods TYPE REF TO ycl_a2g_array.

  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_A2G_HTTP_API IMPLEMENTATION.


  METHOD check_scope.
* do nothing
  ENDMETHOD.


  METHOD client_open.
    "&  Declaration Part
    DATA: ls_t100key     TYPE scx_t100key.

    "& Create HTTP client
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = me->gs_http_api-target
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = me->gif_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.

      ls_t100key-msgid = sy-msgid.
      ls_t100key-msgno = sy-msgno.
      ls_t100key-attr1 = sy-msgv1.
      ls_t100key-attr2 = sy-msgv2.
      ls_t100key-attr2 = sy-msgv3.
      ls_t100key-attr2 = sy-msgv4.
      me->go_msg_manager->register( im_t100key = ls_t100key
                                    im_msgty   = sy-msgty ).
    ENDIF.
* Turn off logon popup. Detect authentication errors.
    me->gif_http_client->propertytype_logon_popup = 0.

  ENDMETHOD.


  METHOD close_client.
    "&  Declaration Part
    DATA: ls_t100key     TYPE scx_t100key.

    "&  Source Part
    CALL METHOD me->gif_http_client->close
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.

      ls_t100key-msgid = sy-msgid.
      ls_t100key-msgno = sy-msgno.
      ls_t100key-attr1 = sy-msgv1.
      ls_t100key-attr2 = sy-msgv2.
      ls_t100key-attr2 = sy-msgv3.
      ls_t100key-attr2 = sy-msgv4.
      me->go_msg_manager->register( im_t100key = ls_t100key
                                    im_msgty   = sy-msgty ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    "&  Source Part
    super->constructor( if_msg_manager ).

    me->init_default( im_target     = im_target
                      im_param_kind = im_param_kind ).

    me->go_methods = NEW #( ).
    me->go_sub_json = NEW #( ).
  ENDMETHOD.


  METHOD display_response.
    "&  Declaration Part
    DATA:lt_fields      TYPE tihttpnvp,
         lv_status_code TYPE i,
         ls_t100key     TYPE scx_t100key.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    "&  Source Part
    me->gif_response->get_status( IMPORTING code = lv_status_code ).

    IF lv_status_code = 200.
      me->trasform_response_string( ).

      DATA(l_content_type) = me->gif_response->get_content_type( ).
      IF l_content_type CP `text/html*`.
      ELSEIF l_content_type CP `text/xml*`.
        cl_demo_output=>display_xml( xml = me->gv_response_data ).
      ELSEIF l_content_type CP `application/json*`.
        cl_demo_output=>display_json( json = me->gv_response_data ).
      ENDIF.

    ELSE.
      me->gif_response->get_header_fields( CHANGING fields = lt_fields ).

      LOOP AT lt_fields ASSIGNING <ls_field>.
        ls_t100key-msgid = 'YA2G_MESSAGE'.
        ls_t100key-msgno = '000'.
        ls_t100key-attr1 = <ls_field>-name.
        ls_t100key-attr2 = <ls_field>-value.
        me->go_msg_manager->register( im_t100key = ls_t100key
                                      im_msgty   = me->go_msg_manager->gc_error ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD init_default.

    GET REFERENCE OF me->gs_http_api INTO  me->gv_data.
    CREATE DATA me->gv_datax TYPE ya2google_s_httpapix.

    TRY.
        me->set_target( im_target ).
        me->set_param_kind( im_param_kind ).
      CATCH ycx_a2g_core_api.
    ENDTRY.

  ENDMETHOD.


  METHOD register_multipart.
    DATA: ls_header_field TYPE ihttpnvp,
          lv_part         TYPE REF TO if_http_entity.

    IF me->gs_http_api-multiparts IS NOT INITIAL.

      me->gif_http_client->request->set_content_type( content_type = 'multipart/form-data' ).
      LOOP AT me->gs_http_api-multiparts INTO DATA(ls_multipart).
        lv_part = me->gif_http_client->request->if_http_entity~add_multipart( ).
        lv_part->set_content_type( content_type = ls_multipart-content_type ).
        LOOP AT ls_multipart-header_fields INTO ls_header_field.
          lv_part->set_header_field( name  = ls_header_field-name     " Name of the header field
                                     value = ls_header_field-value ). " HTTP header field value
          IF ls_multipart-cdata IS NOT INITIAL.
            lv_part->set_cdata( ls_multipart-cdata ).
          ENDIF.
        ENDLOOP.

        IF ls_multipart-data IS NOT INITIAL.
          lv_part->set_data( EXPORTING data =   ls_multipart-data ).
        ENDIF.

        CLEAR lv_part.
      ENDLOOP.
    ELSE.
      me->gif_http_client->request->set_content_type( content_type = 'application/json' ).
    ENDIF.
  ENDMETHOD.


  METHOD send_delete_request.
    "&  Declaration Part
    "&  Source Part

    LOOP AT me->gs_http_api-params INTO DATA(ls_param).
      me->gif_http_client->request->set_form_field( name  = ls_param-name
                                                    value = ls_param-value ).
    ENDLOOP.

    "& Set OAuth 2.0 Token
    me->set_oauth2( ).

    "& Send / Receive Request
    me->send_receive_request( ).
    me->trasform_response_string( ).
    me->close_client( ).

  ENDMETHOD.


  METHOD send_get_request.
    "&  Source Part
    LOOP AT me->gs_http_api-params INTO DATA(ls_param).
      me->gif_http_client->request->set_form_field( name  = ls_param-name
                                                    value = ls_param-value ).
    ENDLOOP.

* Set OAuth 2.0 Token
    me->set_oauth2( ).

* Send / Receive Request
    me->send_receive_request( ).
    me->trasform_response_string( ).
    me->close_client( ).

  ENDMETHOD.


  METHOD send_patch_request.
    "&  Declaration Part
    DATA: out      TYPE xstring.

    "&  Source Part
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = me->gs_http_api-ip_json_req
      IMPORTING
        buffer = out.

    "& Multiparts
    me->register_multipart(  ).

    "& body
    me->gif_http_client->request->set_data( EXPORTING data = out ).

    "& Query parameters
    LOOP AT me->gs_http_api-params INTO DATA(ls_param).
      me->gif_http_client->request->set_form_field( name  = ls_param-name
                                                    value = ls_param-value ).
    ENDLOOP.

    "& Set OAuth 2.0 Token
    me->set_oauth2( ).

    "& Send / Receive Request
    me->send_receive_request( ).
    me->trasform_response_string( ).
    me->close_client( ).

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD send_post_request.
    "&  Declaration Part
    DATA: out      TYPE xstring.

    "&  Source Part
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = me->gs_http_api-ip_json_req
      IMPORTING
        buffer = out.

    "& Multiparts
    me->register_multipart(  ).

    "& body
    me->gif_http_client->request->set_data( EXPORTING data = out ).

    "& Query parameters
    LOOP AT me->gs_http_api-params INTO DATA(ls_param).
      me->gif_http_client->request->set_form_field( name  = ls_param-name
                                                    value = ls_param-value ).
    ENDLOOP.

    "& Set OAuth 2.0 Token
    me->set_oauth2( ).

    "& Send / Receive Request
    me->send_receive_request( ).
    me->trasform_response_string( ).
    me->close_client( ).

  ENDMETHOD.


  METHOD send_receive_request.
    "&  Declaration Part
    DATA: ls_t100key     TYPE scx_t100key.
*    data: lt_fields type TIHTTPNVP.
    "&  Source Part

    me->gif_http_client->request->get_header_fields( CHANGING fields = me->gs_http_api-params ). "lt_fields ).

    CALL METHOD me->gif_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0.
      ls_t100key-msgid = sy-msgid.
      ls_t100key-msgno = sy-msgno.
      ls_t100key-attr1 = sy-msgv1.
      ls_t100key-attr2 = sy-msgv2.
      ls_t100key-attr2 = sy-msgv3.
      ls_t100key-attr2 = sy-msgv4.
      me->go_msg_manager->register( im_t100key = ls_t100key
                                    im_msgty   = sy-msgty ).
    ENDIF.

    CALL METHOD me->gif_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      ls_t100key-msgid = sy-msgid.
      ls_t100key-msgno = sy-msgno.
      ls_t100key-attr1 = sy-msgv1.
      ls_t100key-attr2 = sy-msgv2.
      ls_t100key-attr2 = sy-msgv3.
      ls_t100key-attr2 = sy-msgv4.
      me->go_msg_manager->register( im_t100key = ls_t100key
                                    im_msgty   = sy-msgty ).
    ENDIF.

    me->gif_response = me->gif_http_client->response.

  ENDMETHOD.


  METHOD set_ip_json_request.

    "&  Source Part
    CHECK me->gs_http_api-ip_json_req <> im_json_req.
    me->gs_http_api-ip_json_req = im_json_req.
  ENDMETHOD.


  METHOD set_method.
    "&  Declaration Part
    DATA: lv_dref_value TYPE REF TO data.
    "&  Source Part
    TRY.
        CHECK me->gs_http_api-method <> im_method.
        me->gs_http_api-method = im_method.
        GET REFERENCE OF me->gs_http_api-method INTO lv_dref_value.
        me->update_field( i_fieldname = 'METHOD'
                          i_value     = lv_dref_value ).
        me->check_field( 'METHOD' ).
      CATCH ycx_a2g_objbase INTO DATA(ox_objbase).
        RAISE EXCEPTION TYPE ycx_a2g_core_api
          EXPORTING
            textid = ox_objbase->if_t100_message~t100key.
    ENDTRY.
  ENDMETHOD.


  METHOD set_multiparts.
    "&  Source Part
    CHECK me->gs_http_api-multiparts <> im_multiparts.
    me->gs_http_api-multiparts = im_multiparts.
  ENDMETHOD.


  METHOD set_oauth2.
    "&  Declaration Part
    DATA: ls_t100key     TYPE scx_t100key.

    "&  Source Part
    TRY.
        DATA(lo_oa2c_client) = cl_oauth2_client=>create( i_profile = ycl_a2g_http_api=>google_profile ).

      CATCH cx_oa2c INTO DATA(lx_oa2c).
        ls_t100key-msgid = 'YA2G_MESSAGE'.
        ls_t100key-msgno = '001'. "Error calling CREATE.
        me->go_msg_manager->register( im_t100key = ls_t100key
                                      im_msgty   = me->go_msg_manager->gc_error ).

        ls_t100key-msgid = 'YA2G_MESSAGE'.
        ls_t100key-msgno = '000'.
        ls_t100key-attr1 = lx_oa2c->get_text( ).
        me->go_msg_manager->register( im_t100key = ls_t100key
                                      im_msgty   = me->go_msg_manager->gc_error ).

    ENDTRY.

    TRY.
        lo_oa2c_client->set_token( io_http_client = me->gif_http_client
                                   i_param_kind   = me->gs_http_api-param_kind ).

      CATCH cx_oa2c INTO lx_oa2c.
        TRY.
            CALL METHOD lo_oa2c_client->execute_refresh_flow.
          CATCH cx_oa2c INTO lx_oa2c.

            ls_t100key-msgid = 'YA2G_MESSAGE'.
            ls_t100key-msgno = '003'. "Error calling EXECUTE_REFRESH_FLOW.
            me->go_msg_manager->register( im_t100key = ls_t100key
                                          im_msgty   = me->go_msg_manager->gc_error ).

            ls_t100key-msgid = 'YA2G_MESSAGE'.
            ls_t100key-msgno = '000'.
            ls_t100key-attr1 = lx_oa2c->get_text( ).
            me->go_msg_manager->register( im_t100key = ls_t100key
                                          im_msgty   = me->go_msg_manager->gc_error ).

        ENDTRY.
        TRY.
            lo_oa2c_client->set_token( io_http_client = me->gif_http_client
                                       i_param_kind   = me->gs_http_api-param_kind ).
          CATCH cx_oa2c INTO lx_oa2c.
            ls_t100key-msgid = 'YA2G_MESSAGE'.
            ls_t100key-msgno = '004'. "Error calling SET_TOKEN.
            me->go_msg_manager->register( im_t100key = ls_t100key
                                          im_msgty   = me->go_msg_manager->gc_error ).


            ls_t100key-msgid = 'YA2G_MESSAGE'.
            ls_t100key-msgno = '000'.
            ls_t100key-attr1 = lx_oa2c->get_text( ).
            me->go_msg_manager->register( im_t100key = ls_t100key
                                          im_msgty   = me->go_msg_manager->gc_error ).

        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD set_params.

    "&  Source Part
    CHECK me->gs_http_api-params <> im_params.
    me->gs_http_api-params = im_params.
  ENDMETHOD.


  METHOD set_param_kind.

    "&  Source Part
    CHECK me->gs_http_api-param_kind <> im_param_kind.
    me->gs_http_api-param_kind = im_param_kind.
  ENDMETHOD.


  METHOD set_target.
    "&  Source Part
    CHECK me->gs_http_api-target <> im_target.
    me->gs_http_api-target = im_target.
  ENDMETHOD.


  METHOD trasform_response_string.
    "&  Declaration Part
    DATA: l_status_code TYPE i.

    "&  Source Part
    me->gif_response->get_status( IMPORTING code = l_status_code ).
*    IF l_status_code = 200.
    me->gv_response_data = me->gif_response->get_cdata( ).
*    ENDIF.

  ENDMETHOD.


  METHOD yif_a2g_command~execute.
    "&  Declaration Part
    DATA: ls_t100key     TYPE scx_t100key.

    "&  Source Part
    super->yif_a2g_command~execute( im_fcode ).
    IF NOT me->gs_http_api-method IS INITIAL.

      me->gif_http_client->request->set_method( method = me->gs_http_api-method ).

      CASE im_fcode.
        WHEN me->gc_method_delete_request.  me->send_delete_request( ).
        WHEN me->gc_method_patch_request.   me->send_patch_request( ).
        WHEN me->gc_method_post_request.    me->send_post_request( ).
        WHEN me->gc_method_get_request.    me->send_get_request( ).
      ENDCASE.

    ELSE.

      ls_t100key-msgid = 'YA2G_MESSAGE'.
      ls_t100key-msgno = '005'. "No method is defined to post request
      me->go_msg_manager->register( im_t100key = ls_t100key
                                    im_msgty   = me->go_msg_manager->gc_error ).

    ENDIF.

  ENDMETHOD.                    "yif_a2g_command~execute


  METHOD yif_a2g_google_api~get_command.
    return ?= me.
  ENDMETHOD.


  METHOD yif_a2g_google_api~get_context.
    return ?= me.
  ENDMETHOD.


  METHOD yif_a2g_google_api~get_jclass_for_response.
* do nothing implemented into subclasses
  ENDMETHOD.


  METHOD yif_a2g_google_api~new_jclass_for_request.
* do nothing implemented into subclasses
  ENDMETHOD.
ENDCLASS.
