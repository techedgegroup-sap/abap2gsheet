CLASS ycl_a2g_json_datavalidrule DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_datavalidrule,
             condition  TYPE ycl_a2g_json_BooleanCondition=>ty_s_json_BooleanCondition,
             inputMessage type string,
             strict type string,
             showCustomUi type string,
           END OF ty_s_json_datavalidrule.
    TYPES ty_t_json_datavalidrule TYPE STANDARD TABLE OF ty_s_json_datavalidrule WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_datavalidrule  TYPE ty_s_json_datavalidrule.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_datavalidrule IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_datavalidrule  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
