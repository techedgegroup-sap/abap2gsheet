CLASS ycl_a2g_json_wfchartcolstyle DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_wfchartcolstyle,
             label TYPE string,
             color TYPE ycl_a2g_json_color=>ty_s_json_color,
           END OF ty_s_json_wfchartcolstyle.
    TYPES ty_t_json_wfchartcolstyle TYPE STANDARD TABLE OF ty_s_json_wfchartcolstyle WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_wfchartcolstyle  TYPE ty_s_json_wfchartcolstyle.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_wfchartcolstyle IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_wfchartcolstyle  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
