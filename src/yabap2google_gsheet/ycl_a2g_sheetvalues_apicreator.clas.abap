CLASS ycl_a2g_sheetvalues_apicreator DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_api_creator
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS yif_a2g_google_api_creator~create_instance REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_sheetvalues_apicreator IMPLEMENTATION.

  METHOD yif_a2g_google_api_creator~create_instance.

    DATA: lo_sheetvalues_api TYPE REF TO ycl_a2g_sheetvalues_api.

    lo_sheetvalues_api = NEW #( if_msg_manager = me->gif_msg_manager ).

    return ?= lo_sheetvalues_api.

  ENDMETHOD.

ENDCLASS.
