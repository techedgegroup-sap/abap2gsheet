CLASS ycl_a2g_json_dimensionrange DEFINITION
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

    TYPES: BEGIN OF ty_s_json_DimensionRange,
             sheetid    TYPE i,
             dimension  TYPE i,
             startindex TYPE i,
             endindex   TYPE i,
           END OF ty_s_json_DimensionRange.

    CONSTANTS: gc_fnam_sheetid     TYPE string VALUE 'SHEETID'.
    CONSTANTS: gc_fnam_dimension   TYPE string VALUE 'DIMENSION'.
    CONSTANTS: gc_fnam_startindex  TYPE string VALUE 'STARTINDEX'.
    CONSTANTS: gc_fnam_endindex    TYPE string VALUE 'ENDINDEX'.

  PROTECTED SECTION.

    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_DimensionRange TYPE ty_s_json_DimensionRange.

    METHODS set_sheetid     IMPORTING !i_value TYPE REF TO data.
    METHODS set_DIMENSION   IMPORTING !i_value TYPE REF TO data.
    METHODS set_STARTINDEX  IMPORTING !i_value TYPE REF TO data.
    METHODS set_ENDINDEX    IMPORTING !i_value TYPE REF TO data.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_dimensionrange IMPLEMENTATION.


  METHOD set_sheetid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_DimensionRange-sheetid <> <fs_value>.
      me->gs_DimensionRange-sheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_dimension.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_DimensionRange-dimension <> <fs_value>.
      me->gs_DimensionRange-dimension = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_startindex.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_DimensionRange-startindex <> <fs_value>.
      me->gs_DimensionRange-startindex = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_endindex.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_DimensionRange-endindex <> <fs_value>.
      me->gs_DimensionRange-endindex = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid    . me->set_sheetid( i_value ).
      WHEN gc_fnam_dimension  . me->set_dimension( i_value ).
      WHEN gc_fnam_startindex . me->set_startindex( i_value ).
      WHEN gc_fnam_endindex   . me->set_endindex( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid     . return = REF #( me->gs_DimensionRange-sheetid ).
      WHEN gc_fnam_dimension   . return = REF #( me->gs_DimensionRange-dimension ).
      WHEN gc_fnam_startindex  . return = REF #( me->gs_DimensionRange-startindex ).
      WHEN gc_fnam_endindex    . return = REF #( me->gs_DimensionRange-endindex ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_DimensionRange ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

ENDCLASS.
