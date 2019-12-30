CLASS ycl_a2g_json_bandedrange DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_bandedrange,
             bandedrangeid    TYPE i,
             range            TYPE ycl_a2g_json_gridrange=>ty_s_json_gridrange,
             rowproperties    TYPE ycl_a2g_json_bandingprop=>ty_s_json_bandingprop,
             columnproperties TYPE ycl_a2g_json_bandingprop=>ty_s_json_bandingprop,
           END OF ty_s_json_bandedrange.
    TYPES ty_t_json_bandedrange TYPE STANDARD TABLE OF ty_s_json_bandedrange WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_bandedrange  TYPE ty_s_json_bandedrange.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_bandedrange IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_bandedrange  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
