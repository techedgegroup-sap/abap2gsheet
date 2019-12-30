CLASS ycl_a2g_json_gridproperties DEFINITION
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

    TYPES: BEGIN OF ty_s_json_gridproperties ,
             rowcount                TYPE i,
             columncount             TYPE i,
             frozenrowcount          TYPE i,
             frozencolumncount       TYPE i,
             hidegridlines           TYPE string,
             rowgroupcontrolafter    TYPE string,
             columngroupcontrolafter TYPE string,
           END OF ty_s_json_gridproperties.

    CONSTANTS: gc_fnam_rowcount             TYPE string VALUE 'ROWCOUNT'.
    CONSTANTS: gc_fnam_columncount          TYPE string VALUE 'COLCOUNT'.
    CONSTANTS: gc_fnam_frozenrowcount       TYPE string VALUE 'FROZENROW'.
    CONSTANTS: gc_fnam_frozencolumncount    TYPE string VALUE 'FROZENCOL'.
    CONSTANTS: gc_fnam_hidegridlines        TYPE string VALUE 'HIDEGRIDLINES'.
    CONSTANTS: gc_fnam_rowgroupcontrolafter TYPE string VALUE 'ROWGROUP'.
    CONSTANTS: gc_fnam_colgroupcontrolafter TYPE string VALUE 'COLGROUP'.



  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_gridproperties TYPE ty_s_json_gridproperties.

    METHODS set_rowcount               IMPORTING !i_value TYPE REF TO data.
    METHODS set_columncount            IMPORTING !i_value TYPE REF TO data.
    METHODS set_frozenrowcount         IMPORTING !i_value TYPE REF TO data.
    METHODS set_frozencolumncount      IMPORTING !i_value TYPE REF TO data.
    METHODS set_hidegridlines          IMPORTING !i_value TYPE REF TO data.
    METHODS set_rowgroupcontrolafter   IMPORTING !i_value TYPE REF TO data.
    METHODS set_colgroupcontrolafter   IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_a2g_json_gridproperties IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.

    DATA: lv_val TYPE i.

    lv_val = '1000'.
    me->set_rowCount( REF #( lv_val ) ).

    lv_val = '30'.
    me->set_columnCount( REF #( lv_val ) ).

  ENDMETHOD.

  METHOD set_COLgroupcontrolafter  .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-columngroupcontrolafter  <> <fs_value>.
      me->gs_gridproperties-columngroupcontrolafter  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_rowgroupcontrolafter  .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-rowgroupcontrolafter  <> <fs_value>.
      me->gs_gridproperties-rowgroupcontrolafter  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_hidegridlines .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-hidegridlines  <> <fs_value>.
      me->gs_gridproperties-hidegridlines  = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_frozencolumncount .

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-frozencolumncount  <> <fs_value>.
      me->gs_gridproperties-frozencolumncount  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_frozenrowcount .

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-frozenrowcount  <> <fs_value>.
      me->gs_gridproperties-frozenrowcount  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_columncount.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-columncount <> <fs_value>.
      me->gs_gridproperties-columncount = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_rowcount.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_gridproperties-rowcount <> <fs_value>.
      me->gs_gridproperties-rowcount = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_rowcount             . me->set_rowcount( i_value ).
      WHEN gc_fnam_columncount          . me->set_columncount( i_value ).
      WHEN gc_fnam_frozenrowcount       . me->set_frozenrowcount( i_value ).
      WHEN gc_fnam_frozencolumncount    . me->set_frozencolumncount( i_value ).
      WHEN gc_fnam_hidegridlines        . me->set_hidegridlines( i_value ).
      WHEN gc_fnam_rowgroupcontrolafter . me->set_rowgroupcontrolafter( i_value ).
      WHEN gc_fnam_colgroupcontrolafter . me->set_colgroupcontrolafter( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_rowcount             . return = REF #( me->gs_gridproperties-rowcount ).
      WHEN gc_fnam_columncount          . return = REF #( me->gs_gridproperties-columncount ).
      WHEN gc_fnam_frozenrowcount       . return = REF #( me->gs_gridproperties-frozenrowcount ).
      WHEN gc_fnam_frozencolumncount    . return = REF #( me->gs_gridproperties-frozencolumncount ).
      WHEN gc_fnam_hidegridlines        . return = REF #( me->gs_gridproperties-hidegridlines ).
      WHEN gc_fnam_rowgroupcontrolafter . return = REF #( me->gs_gridproperties-rowgroupcontrolafter ).
      WHEN gc_fnam_colgroupcontrolafter . return = REF #( me->gs_gridproperties-columngroupcontrolafter ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

  ENDMETHOD.

  METHOD rebuild_data.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_gridproperties ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


ENDCLASS.
