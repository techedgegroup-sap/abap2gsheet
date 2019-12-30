CLASS ycl_a2g_json_histogramrule DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_histogramrule,
             interval TYPE i,
             start    TYPE i,
             end      TYPE i,
           END OF ty_s_json_histogramrule.
    TYPES ty_t_json_histogramrule TYPE STANDARD TABLE OF ty_s_json_histogramrule WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_histogramrule  TYPE ty_s_json_histogramrule.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_histogramrule IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_histogramrule  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
