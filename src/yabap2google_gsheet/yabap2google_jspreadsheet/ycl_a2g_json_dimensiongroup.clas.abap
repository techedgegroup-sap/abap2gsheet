CLASS ycl_a2g_json_dimensiongroup DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_dimensiongroup,
             range     TYPE ycl_a2g_json_dimensionrange=>ty_s_json_dimensionrange,
             depth     TYPE i,
             collapsed TYPE string,
           END OF ty_s_json_dimensiongroup.
    TYPES ty_t_json_dimensiongroup TYPE STANDARD TABLE OF ty_s_json_dimensiongroup WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_dimensiongroup  TYPE ty_s_json_dimensiongroup.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_dimensiongroup IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_dimensiongroup  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
