CLASS ycl_a2g_api_creator DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_a2g_google_api_creator.

    METHODS constructor
      IMPORTING i_msg_manager TYPE REF TO yif_a2g_msg_manager.

    CLASS-METHODS: get_api_creator_instance
      IMPORTING i_clsname     TYPE string
                i_msg_manager TYPE REF TO yif_a2g_msg_manager
      RETURNING VALUE(return) TYPE REF TO yif_a2g_google_api_creator.

  PROTECTED SECTION.
    DATA gif_msg_manager TYPE REF TO yif_a2g_msg_manager.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_api_creator IMPLEMENTATION.

  METHOD constructor.

    "&  Source Part
    me->gif_msg_manager = i_msg_manager.

  ENDMETHOD.



  METHOD get_api_creator_instance.

    "&  Declaration Part
    DATA: lv_classname TYPE string.
    DATA: lo_object TYPE REF TO ycl_a2g_api_creator.

    "&  Source Part
    lv_classname = i_clsname.
    TRANSLATE lv_classname TO UPPER CASE.

    CREATE OBJECT lo_object TYPE (lv_classname)
      EXPORTING
        I_MSG_MANAGER = i_msg_manager.

    return ?= lo_object.


  ENDMETHOD.

ENDCLASS.
