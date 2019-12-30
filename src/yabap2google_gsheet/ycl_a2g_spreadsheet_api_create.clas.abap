CLASS ycl_a2g_spreadsheet_api_create DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_api_creator
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS yif_a2g_google_api_creator~create_instance REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_spreadsheet_api_create IMPLEMENTATION.
  METHOD yif_a2g_google_api_creator~create_instance.

    DATA: lo_spreadsheet_api TYPE REF TO ycl_a2g_spreadsheet_api.

    lo_spreadsheet_api = NEW #( if_msg_manager = me->gif_msg_manager ).

    return ?= lo_spreadsheet_api.

  ENDMETHOD.
ENDCLASS.
