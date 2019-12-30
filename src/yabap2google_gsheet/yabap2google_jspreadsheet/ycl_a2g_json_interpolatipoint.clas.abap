CLASS ycl_a2g_json_interpolatipoint DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_interpolatipoint,
             color TYPE ycl_a2g_json_color=>ty_s_json_color,
             type  TYPE string,
             value TYPE string,
           END OF ty_s_json_interpolatipoint.
    TYPES ty_t_json_interpolatipoint TYPE STANDARD TABLE OF ty_s_json_interpolatipoint WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_interpolatipoint  TYPE ty_s_json_interpolatipoint.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_interpolatipoint IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_interpolatipoint  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
