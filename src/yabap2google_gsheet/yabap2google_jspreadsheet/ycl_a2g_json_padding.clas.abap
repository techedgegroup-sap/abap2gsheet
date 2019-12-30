CLASS ycl_a2g_json_padding DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_padding,
             top    TYPE i,
             right  TYPE i,
             bottom TYPE i,
             left   TYPE i,
           END OF ty_s_json_padding.


    CONSTANTS gc_fnam_top             TYPE string VALUE 'TOP'.
    CONSTANTS gc_fnam_right           TYPE string VALUE 'RIGHT'.
    CONSTANTS gc_fnam_bottom          TYPE string VALUE 'BOTTOM'.
    CONSTANTS gc_fnam_left            TYPE string VALUE 'LEFT'.


    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.



    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_padding  TYPE ty_s_json_padding.

    METHODS set_top           IMPORTING !i_value TYPE REF TO data.
    METHODS set_right         IMPORTING !i_value TYPE REF TO data.
    METHODS set_bottom        IMPORTING !i_value TYPE REF TO data.
    METHODS set_left          IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_padding IMPLEMENTATION.
  METHOD yif_a2g_json~set_default.


    DATA: lv_val TYPE i.
    lv_val = 2.
    me->set_bottom( REF #( lv_val )  ).
    me->set_top( REF #( lv_val )  ).

    lv_val = 3.
    me->set_left( REF #( lv_val )  ).
    me->set_right( REF #( lv_val )  ).


  ENDMETHOD.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_padding  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_top    . me->set_top( i_value ).
      WHEN gc_fnam_right  . me->set_right( i_value ).
      WHEN gc_fnam_bottom . me->set_bottom( i_value ).
      WHEN gc_fnam_left   . me->set_left( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_top   . return = REF #( me->gs_padding-top   ).
      WHEN gc_fnam_right . return = REF #( me->gs_padding-right  ).
      WHEN gc_fnam_bottom. return = REF #( me->gs_padding-bottom ).
      WHEN gc_fnam_left  . return = REF #( me->gs_padding-left   ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_top.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_padding-top <> <fs_value>.
      me->gs_padding-top = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_right.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_padding-right <> <fs_value>.
      me->gs_padding-right = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_left.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_padding-left <> <fs_value>.
      me->gs_padding-left = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_bottom.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_padding-bottom <> <fs_value>.
      me->gs_padding-bottom = <fs_value>.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
