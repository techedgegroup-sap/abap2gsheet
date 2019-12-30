CLASS ycl_a2g_json_numberformat DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_numberformat,
             type    TYPE string,
             pattern TYPE string,
           END OF ty_s_json_numberformat.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_numberformat  TYPE ty_s_json_numberformat.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_numberformat IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_numberformat  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

ENDCLASS.
