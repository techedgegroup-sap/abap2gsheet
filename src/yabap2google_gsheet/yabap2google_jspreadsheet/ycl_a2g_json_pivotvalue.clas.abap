CLASS ycl_a2g_json_pivotvalue DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_pivotvalue,
             summarizefunction     TYPE string,
             name                  TYPE string,
             calculateddisplaytype TYPE string,
             sourcecolumnoffset    TYPE i,
             formula               TYPE string,
           END OF ty_s_json_pivotvalue.
    TYPES ty_t_json_pivotvalue TYPE STANDARD TABLE OF ty_s_json_pivotvalue WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_pivotvalue  TYPE ty_s_json_pivotvalue.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_pivotvalue IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_pivotvalue  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
