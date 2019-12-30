CLASS ycl_a2g_json_basicchartdomain DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_basicchartdomain,
             domain   TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             reversed TYPE string,
           END OF ty_s_json_basicchartdomain.
    TYPES ty_t_json_basicchartdomain TYPE STANDARD TABLE OF ty_s_json_basicchartdomain WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_basicchartdomain  TYPE ty_s_json_basicchartdomain.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_basicchartdomain IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_basicchartdomain  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

ENDCLASS.
