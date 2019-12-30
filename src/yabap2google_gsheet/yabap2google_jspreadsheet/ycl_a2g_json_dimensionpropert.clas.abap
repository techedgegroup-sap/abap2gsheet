CLASS ycl_a2g_json_dimensionpropert DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_dimensionpropert,
             hidden_by_filter    TYPE string,
             hidden_by_user      TYPE string,
             pixel_size         TYPE i,
             developer_metadata TYPE ycl_a2g_json_developermetadata=>ty_t_json_devrmeta,
           END OF ty_s_json_dimensionpropert.
    TYPES ty_t_json_dimensionpropert TYPE STANDARD TABLE OF ty_s_json_dimensionpropert WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_dimensionpropert  TYPE ty_s_json_dimensionpropert.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_dimensionpropert IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_dimensionpropert  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
