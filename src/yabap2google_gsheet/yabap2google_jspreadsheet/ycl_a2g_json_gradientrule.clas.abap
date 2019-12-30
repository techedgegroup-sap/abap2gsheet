CLASS ycl_a2g_json_gradientrule DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_gradientrule,
             minpoint         TYPE ycl_a2g_json_InterpolatiPoint=>ty_s_json_InterpolatiPoint,
             midpoint         TYPE ycl_a2g_json_InterpolatiPoint=>ty_s_json_InterpolatiPoint,
             maxpoint         TYPE ycl_a2g_json_InterpolatiPoint=>ty_s_json_InterpolatiPoint,
           END OF ty_s_json_gradientrule.
    TYPES ty_t_json_gradientrule TYPE STANDARD TABLE OF ty_s_json_gradientrule WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_gradientrule  TYPE ty_s_json_gradientrule.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_gradientrule IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_gradientrule  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
