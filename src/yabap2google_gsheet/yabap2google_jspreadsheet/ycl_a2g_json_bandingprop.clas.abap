CLASS ycl_a2g_json_bandingprop DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_bandingprop,
             headercolor     TYPE ycl_a2g_json_color=>ty_s_json_color,
             firstbandcolor  TYPE ycl_a2g_json_color=>ty_s_json_color,
             secondbandcolor TYPE ycl_a2g_json_color=>ty_s_json_color,
             footercolor     TYPE ycl_a2g_json_color=>ty_s_json_color,
           END OF ty_s_json_bandingprop.
    TYPES ty_t_json_bandingprop TYPE STANDARD TABLE OF ty_s_json_bandingprop WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_bandingprop  TYPE ty_s_json_bandingprop.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_bandingprop IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_bandingprop  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
