CLASS ycl_a2g_json_pivotgrpvalmeta DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_pivotgrpvalmeta,
             value     TYPE ycl_a2g_json_extendedvalue=>ty_s_json_extendedvalue,
             collapsed TYPE string,
           END OF ty_s_json_pivotgrpvalmeta.
    TYPES ty_t_json_pivotgrpvalmeta TYPE STANDARD TABLE OF ty_s_json_pivotgrpvalmeta WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_pivotgrpvalmeta  TYPE ty_s_json_pivotgrpvalmeta.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_pivotgrpvalmeta IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_pivotgrpvalmeta  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
