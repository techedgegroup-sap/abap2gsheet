CLASS ycl_a2g_json_pivottable DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_pivottable,
             source      TYPE ycl_a2g_json_gridrange=>ty_s_json_gridrange,
             rows        TYPE ycl_a2g_json_pivotgroup=>ty_t_json_pivotgroup,
             columns     TYPE ycl_a2g_json_pivotgroup=>ty_t_json_pivotgroup,
             criteria    TYPE ycl_a2g_json_pivotfiltercrite=>ty_s_json_pivotfiltercrite,
             values      TYPE ycl_a2g_json_pivotvalue=>ty_t_json_pivotvalue,
             valuelayout TYPE string,
           END OF ty_s_json_pivottable.
    TYPES ty_t_json_pivottable TYPE STANDARD TABLE OF ty_s_json_pivottable WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_pivottable  TYPE ty_s_json_pivottable.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_pivottable IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_pivottable  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
