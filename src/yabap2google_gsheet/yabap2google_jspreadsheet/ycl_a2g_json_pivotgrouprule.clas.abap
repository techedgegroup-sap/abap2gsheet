CLASS ycl_a2g_json_pivotgrouprule DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_pivotgrouprule,
             manualrule    TYPE ycl_a2g_json_manualrule=>ty_s_json_manualrule,
             histogramrule TYPE ycl_a2g_json_histogramrule=>ty_s_json_histogramrule,
             datetimerule  TYPE ycl_a2g_json_datetimerule=>ty_s_json_datetimerule,
           END OF ty_s_json_pivotgrouprule.
    TYPES ty_t_json_pivotgrouprule TYPE STANDARD TABLE OF ty_s_json_pivotgrouprule WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_pivotgrouprule  TYPE ty_s_json_pivotgrouprule.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_pivotgrouprule IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_pivotgrouprule  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
