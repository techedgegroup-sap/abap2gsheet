class YCL_OA2C_SPECIFICS_ZGOOGLE definition
  public
  inheriting from CL_OA2C_SPECIFICS_ABSTRACT
  final
  create public .

public section.

  methods IF_OA2C_SPECIFICS~GET_AC_AUTH_REQU_PARAM_NAMES
    redefinition .
  methods IF_OA2C_SPECIFICS~GET_CONFIG_EXTENSION
    redefinition .
  methods IF_OA2C_SPECIFICS~GET_ENDPOINT_SETTINGS
    redefinition .
  methods IF_OA2C_SPECIFICS~GET_SUPPORTED_GRANT_TYPES
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS YCL_OA2C_SPECIFICS_ZGOOGLE IMPLEMENTATION.


  METHOD IF_OA2C_SPECIFICS~GET_AC_AUTH_REQU_PARAM_NAMES.
    DATA: ls_add_param TYPE if_oa2c_specifics~ty_s_add_param.

    CALL METHOD super->if_oa2c_specifics~get_ac_auth_requ_param_names
      IMPORTING
        e_client_id           = e_client_id
        e_redirect_uri        = e_redirect_uri
        e_response_type       = e_response_type
        e_response_type_value =
                                e_response_type_value
        e_scope               = e_scope.

    ls_add_param-name = `access_type`.
    INSERT ls_add_param INTO TABLE et_add_param_names.

    ls_add_param-name = `approval_prompt`.
    INSERT ls_add_param INTO TABLE et_add_param_names.

    ls_add_param-name = `login_hint`.
    INSERT ls_add_param INTO TABLE et_add_param_names.

  ENDMETHOD.


  METHOD IF_OA2C_SPECIFICS~GET_CONFIG_EXTENSION.
    r_config_extension = `ZGOOGLE`.
  ENDMETHOD.


  METHOD IF_OA2C_SPECIFICS~GET_ENDPOINT_SETTINGS.
    e_changeable = abap_false.
    e_authorization_endpoint_path = `accounts.google.com/o/oauth2/auth`.
    e_token_endpoint_path = `accounts.google.com/o/oauth2/token`.
    e_revocation_endpoint_path = `accounts.google.com/o/oauth2/revoke`.
  ENDMETHOD.


  METHOD IF_OA2C_SPECIFICS~GET_SUPPORTED_GRANT_TYPES.

    e_authorization_code = abap_true.
    e_saml20_assertion = abap_true.
    e_refresh = abap_true.
    e_revocation = abap_true.

  ENDMETHOD.
ENDCLASS.
