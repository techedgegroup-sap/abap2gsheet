CLASS ycl_a2g_json_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS    constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS    build_json_instance
      IMPORTING i_classname   TYPE        string
      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PROTECTED SECTION.
    DATA: gif_msg_manager TYPE REF TO yif_a2g_msg_manager.

  PRIVATE SECTION.

ENDCLASS.

CLASS ycl_a2g_json_factory IMPLEMENTATION.

  METHOD  constructor.
    me->gif_msg_manager ?= if_msg_manager.
  ENDMETHOD.

  METHOD build_json_instance.
    "&  Declaration Part
    DATA: lv_classname TYPE string.

    "&  Source Part
    lv_classname = i_classname.
    TRANSLATE lv_classname TO UPPER CASE.

    CREATE OBJECT return TYPE (lv_classname)
      EXPORTING
        if_msg_manager = me->gif_msg_manager.
  ENDMETHOD.

ENDCLASS.
