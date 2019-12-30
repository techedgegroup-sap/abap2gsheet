CLASS ycl_a2g_sheet_api_creator DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_api_creator
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS yif_a2g_google_api_creator~create_instance REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_sheet_api_creator IMPLEMENTATION.
  METHOD yif_a2g_google_api_creator~create_instance.

    DATA: lo_sheet_api TYPE REF TO ycl_a2g_sheet_api.

    lo_sheet_api = NEW #( if_msg_manager = me->gif_msg_manager ).

    return ?= lo_sheet_api.

  ENDMETHOD.
ENDCLASS.
