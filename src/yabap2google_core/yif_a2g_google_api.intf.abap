INTERFACE yif_a2g_google_api
  PUBLIC .

  METHODS: get_command
    RETURNING VALUE(return) TYPE REF TO yif_a2g_command.

  METHODS: get_context
    RETURNING VALUE(return) TYPE REF TO yif_a2g_context.

  METHODS: new_jclass_for_request
    IMPORTING method        TYPE string
    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  METHODS: get_jclass_for_response
    IMPORTING method        TYPE string
    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  METHODS set_query_parameter IMPORTING i_parameters TYPE tihttpnvp.

ENDINTERFACE.
