CLASS ycl_a2g_json_gridrange DEFINITION
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

    TYPES: BEGIN OF ty_s_json_gridrange,
             sheet_id          TYPE i,
             start_row_index    TYPE i,
             end_row_index      TYPE i,
             start_column_index TYPE i,
             end_column_index   TYPE i,
           END OF ty_s_json_gridrange.
    types ty_t_json_gridrange type STANDARD TABLE OF ty_s_json_gridrange WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_sheetid           TYPE string VALUE 'SHEETID'.
    CONSTANTS: gc_fnam_startrowindex     TYPE string VALUE 'STARTROWINDEX'.
    CONSTANTS: gc_fnam_endrowindex       TYPE string VALUE 'ENDROWINDEX'.
    CONSTANTS: gc_fnam_startcolumnindex  TYPE string VALUE 'STARTCOLUMNINDEX'.
    CONSTANTS: gc_fnam_endcolumnindex    TYPE string VALUE 'ENDCOLUMNINDEX'.

  PROTECTED SECTION.

    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_gridrange TYPE ty_s_json_gridrange.

    METHODS set_sheetid           IMPORTING !i_value TYPE REF TO data.
    METHODS set_startrowindex     IMPORTING !i_value TYPE REF TO data.
    METHODS set_endrowindex       IMPORTING !i_value TYPE REF TO data.
    METHODS set_startcolumnindex  IMPORTING !i_value TYPE REF TO data.
    METHODS set_endcolumnindex    IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_gridrange IMPLEMENTATION.

  METHOD set_sheetid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridrange-sheet_id <> <fs_value>.
      me->gs_gridrange-sheet_id = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_startrowindex.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridrange-start_row_index <> <fs_value>.
      me->gs_gridrange-start_row_index = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_endrowindex.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridrange-end_row_index <> <fs_value>.
      me->gs_gridrange-end_row_index = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_startcolumnindex.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridrange-start_column_index <> <fs_value>.
      me->gs_gridrange-start_column_index = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_endcolumnindex.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridrange-end_column_index <> <fs_value>.
      me->gs_gridrange-end_column_index = <fs_value>.
    ENDIF.
  ENDMETHOD.




  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid          . me->set_sheetid( i_value ).
      WHEN gc_fnam_startrowindex    . me->set_startrowindex( i_value ).
      WHEN gc_fnam_endrowindex      . me->set_endrowindex( i_value ).
      WHEN gc_fnam_startcolumnindex . me->set_startcolumnindex( i_value ).
      WHEN gc_fnam_endcolumnindex   . me->set_endcolumnindex( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.

    CASE  i_name.
      WHEN gc_fnam_sheetid          . return = REF #( me->gs_gridrange-sheet_id ).
      WHEN gc_fnam_startrowindex    . return = REF #( me->gs_gridrange-start_row_index ).
      WHEN gc_fnam_endrowindex      . return = REF #( me->gs_gridrange-end_row_index ).
      WHEN gc_fnam_startcolumnindex . return = REF #( me->gs_gridrange-start_column_index ).
      WHEN gc_fnam_endcolumnindex   . return = REF #( me->gs_gridrange-end_column_index ).
    ENDCASE.

  ENDMETHOD.

  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_gridrange ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
