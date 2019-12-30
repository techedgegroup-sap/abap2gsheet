CLASS ycl_a2g_json_booleanrule DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_booleanrule,
             condition TYPE ycl_a2g_json_booleancondition=>ty_s_json_booleancondition,
             format    TYPE ycl_a2g_json_cellformat=>ty_s_json_cellformat,
           END OF ty_s_json_booleanrule.
    TYPES ty_t_json_booleanrule TYPE STANDARD TABLE OF ty_s_json_booleanrule WITH NON-UNIQUE KEY condition.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_booleanrule  TYPE ty_s_json_booleanrule.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_booleanrule IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_booleanrule  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
