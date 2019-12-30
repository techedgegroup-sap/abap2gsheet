CLASS ycl_a2g_json_valuerange DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_value_range,
             range          TYPE string,
             majordimension TYPE string,
             values         TYPE YA2GOOGLE_T_JSTRING,
           END OF ty_s_json_value_range.

TYPES: ty_t_json_value_range TYPE STANDARD TABLE OF ty_s_json_value_range WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_range            TYPE string VALUE 'RANGE'.
    CONSTANTS: gc_fnam_majordimension   TYPE string VALUE 'MAJORDIMENSION'.
    CONSTANTS: gc_fnam_values           TYPE string VALUE 'VALUES'.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

  PROTECTED SECTION.
    DATA: gs_value_range TYPE ty_s_json_value_range.

    METHODS: set_range
      IMPORTING !i_value TYPE REF TO data.

    METHODS: set_majordimension
      IMPORTING !i_value TYPE REF TO data.

    METHODS: set_values
      IMPORTING !i_value TYPE REF TO data.

    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.


  PRIVATE SECTION.
ENDCLASS.

CLASS ycl_a2g_json_valuerange IMPLEMENTATION.

  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_value_range ).
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_range.           me->set_range( i_value ).
      WHEN gc_fnam_majordimension.  me->set_majordimension( i_value ).
      WHEN gc_fnam_values.          me->set_values( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_range.           return = REF #( me->gs_value_range-range ).
      WHEN gc_fnam_majordimension.  return = REF #( me->gs_value_range-majordimension ).
      WHEN gc_fnam_values.          return = REF #( me->gs_value_range-values ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_majordimension.
    DATA: ls_t100key    TYPE scx_t100key.
    FIELD-SYMBOLS <fs_value> TYPE string.

    TRY.
        ASSIGN i_value->* TO <fs_value>.
        IF <fs_value> IS ASSIGNED.

          CHECK me->gs_value_range-majordimension <> <fs_value>.
          me->check_field( i_fieldname = me->gc_fnam_majordimension ).
          me->gs_value_range-majordimension = <fs_value>.
        ENDIF.
      CATCH ycx_a2g_objbase INTO DATA(ox_objbase).

    ENDTRY.
  ENDMETHOD.

  METHOD set_range.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_value_range-range <> <fs_value>.
      me->gs_value_range-range = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_values.

    FIELD-SYMBOLS <fs_value> TYPE stringtab.
    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      append <fs_value> to me->gs_value_range-values.
    ENDIF.
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

ENDCLASS.
