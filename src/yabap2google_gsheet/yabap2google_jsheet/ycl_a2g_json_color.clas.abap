CLASS ycl_a2g_json_color DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.


    TYPES: BEGIN OF ty_s_json_color ,
             red   TYPE string,
             green TYPE string,
             blue  TYPE string,
             alpha TYPE string,
           END OF ty_s_json_color.

    CONSTANTS: gc_fnam_red     TYPE string VALUE 'RED'.
    CONSTANTS: gc_fnam_green   TYPE string VALUE 'GREEN'.
    CONSTANTS: gc_fnam_blue    TYPE string VALUE 'BLUE'.
    CONSTANTS: gc_fnam_alpha   TYPE string VALUE 'ALPHA'.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_color TYPE ty_s_json_color.

    METHODS set_green    IMPORTING !i_value TYPE REF TO data.
    METHODS set_blue     IMPORTING !i_value TYPE REF TO data.
    METHODS set_alpha    IMPORTING !i_value TYPE REF TO data.
    METHODS set_red      IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_color IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.

    DATA:lv_col TYPE string.
    lv_col = 1.
    me->set_blue( REF #( lv_col )  ).
    me->set_green( REF #( lv_col )  ).
    me->set_red( REF #( lv_col ) ).

  ENDMETHOD.

  METHOD set_red .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_color-red  <> <fs_value>.
      me->gs_color-red  = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_alpha .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_color-alpha  <> <fs_value>.
      me->gs_color-alpha  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_blue .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_color-blue  <> <fs_value>.
      me->gs_color-blue  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_green.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_color-green <> <fs_value>.
      me->gs_color-green = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_red      . me->set_red( i_value ).
      WHEN gc_fnam_green        . me->set_green( i_value  ).
      WHEN gc_fnam_blue        . me->set_blue( i_value  ).
      WHEN gc_fnam_alpha    . me->set_alpha( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_red      . return = REF #( me->gs_color-red ).
      WHEN gc_fnam_green        . return = REF #( me->gs_color-green ).
      WHEN gc_fnam_blue        . return = REF #( me->gs_color-blue ).
      WHEN gc_fnam_alpha    . return = REF #( me->gs_color-alpha ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_color ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


ENDCLASS.
