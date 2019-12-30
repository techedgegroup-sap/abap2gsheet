CLASS ycl_a2g_json_devrmetaloc DEFINITION
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
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.


    TYPES: BEGIN OF ty_s_json_devrmetaloc,
             locationtype   TYPE string,
             spreadsheet    TYPE string,
             sheetid        TYPE i,
             dimensionrange TYPE ycl_a2g_json_dimensionrange=>ty_s_json_dimensionrange,
           END OF ty_s_json_devrmetaloc.

    CONSTANTS: gc_fnam_sheetid        TYPE string VALUE 'SHEETID'.
    CONSTANTS: gc_fnam_locationtype   TYPE string VALUE 'LOCATIONTYPE'.
    CONSTANTS: gc_fnam_spreadsheet    TYPE string VALUE 'SPREADSHEET'.
    CONSTANTS: gc_fnam_dimensionrange TYPE string VALUE 'DIMENSIONRANGE'.


  PROTECTED SECTION.

    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_devrmetaloc TYPE ty_s_json_devrmetaloc.

    METHODS set_sheetid        IMPORTING !i_value TYPE REF TO data.
    METHODS set_locationtype   IMPORTING !i_value TYPE REF TO data.
    METHODS set_spreadsheet    IMPORTING !i_value TYPE REF TO data.

    METHODS new_dimensionrange RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_devrmetaloc IMPLEMENTATION.


  METHOD set_sheetid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmetaloc-sheetid <> <fs_value>.
      me->gs_devrmetaloc-sheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_locationtype.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmetaloc-locationtype <> <fs_value>.
      me->gs_devrmetaloc-locationtype = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_spreadsheet.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmetaloc-spreadsheet <> <fs_value>.
      me->gs_devrmetaloc-spreadsheet = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid       . me->set_sheetid( i_value ).
      WHEN gc_fnam_locationtype  . me->set_locationtype( i_value ).
      WHEN gc_fnam_spreadsheet   . me->set_spreadsheet( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid        . return = REF #( me->gs_devrmetaloc-sheetid ).
      WHEN gc_fnam_locationtype   . return = REF #( me->gs_devrmetaloc-locationtype ).
      WHEN gc_fnam_spreadsheet    . return = REF #( me->gs_devrmetaloc-spreadsheet ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
    DATA(lif_a2g_json) = me->new_dimensionrange(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_devrmetaloc-dimensionrange ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_dimensionrange ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_devrmetaloc-dimensionrange = <fs_value_range>.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_devrmetaloc ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_dimensionrange. return = me->new_dimensionrange( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_dimensionrange.
        return ?= me->go_json_array->getinstance( me->gc_fnam_dimensionrange ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_dimensionrange.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DIMENSIONRANGE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_dimensionrange
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
