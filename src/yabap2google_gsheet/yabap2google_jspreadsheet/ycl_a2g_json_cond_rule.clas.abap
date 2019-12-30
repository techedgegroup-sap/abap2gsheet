CLASS ycl_a2g_json_cond_rule DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_cond_rule,
             ranges         TYPE ycl_a2g_json_gridrange=>ty_t_json_gridrange,
             booleanRule type ycl_a2g_json_booleanrule=>ty_s_json_booleanrule,
             gradientRule type ycl_a2g_json_gradientRule=>ty_s_json_gradientRule,
           END OF ty_s_json_cond_rule.
    TYPES ty_t_json_cond_rule TYPE STANDARD TABLE OF ty_s_json_cond_rule WITH NON-UNIQUE KEY gradientRule.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_cond_rule  TYPE ty_s_json_cond_rule.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_cond_rule IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_cond_rule  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
