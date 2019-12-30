CLASS ycl_a2g_json_sheetproperties DEFINITION
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

    TYPES: BEGIN OF ty_s_json_sheetproperties ,
             sheetid        TYPE i,
             title          TYPE string,
             index          TYPE i,
             sheettype      TYPE string,
             gridproperties TYPE ycl_a2g_json_gridproperties=>ty_s_json_gridproperties,
             hidden         TYPE string,
             tabcolor       TYPE ycl_a2g_json_color=>ty_s_json_color,
             righttoleft    TYPE string,
           END OF ty_s_json_sheetproperties.

    CONSTANTS: gc_fnam_sheetid        TYPE string VALUE 'SHEETID'.
    CONSTANTS: gc_fnam_title          TYPE string VALUE 'TITLE'.
    CONSTANTS: gc_fnam_index          TYPE string VALUE 'INDEX'.
    CONSTANTS: gc_fnam_sheettype      TYPE string VALUE 'SHEETTYPE'.
    CONSTANTS: gc_fnam_hidden         TYPE string VALUE 'HIDDEN'.
    CONSTANTS: gc_fnam_righttoleft    TYPE string VALUE 'RIGHTTOLEFT'.

    CONSTANTS: gc_fnam_gridproperties TYPE string VALUE 'GRIDPROPERTIES'.
    CONSTANTS: gc_fnam_tabcolor       TYPE string VALUE 'TABCOLOR'.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_sheetproperties TYPE ty_s_json_sheetproperties.

    METHODS set_sheetid      IMPORTING !i_value TYPE REF TO data.
    METHODS set_title        IMPORTING !i_value TYPE REF TO data.
    METHODS set_index        IMPORTING !i_value TYPE REF TO data.
    METHODS set_sheettype    IMPORTING !i_value TYPE REF TO data.
    METHODS set_hidden       IMPORTING !i_value TYPE REF TO data.
    METHODS set_righttoleft  IMPORTING !i_value TYPE REF TO data.

    METHODS new_gridproperties  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_tabcolor        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_a2g_json_sheetproperties IMPLEMENTATION.

  METHOD set_righttoleft .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheetproperties-righttoleft  <> <fs_value>.
      me->gs_sheetproperties-righttoleft  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_hidden .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheetproperties-hidden  <> <fs_value>.
      me->gs_sheetproperties-hidden  = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_sheettype .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheetproperties-sheettype  <> <fs_value>.
      me->gs_sheetproperties-sheettype  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_index .

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheetproperties-index  <> <fs_value>.
      me->gs_sheetproperties-index  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_title.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheetproperties-title <> <fs_value>.
      me->gs_sheetproperties-title = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_sheetid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheetproperties-sheetid <> <fs_value>.
      me->gs_sheetproperties-sheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid      . me->set_sheetid( i_value ).
      WHEN gc_fnam_title        . me->set_title( i_value  ).
      WHEN gc_fnam_index        . me->set_index( i_value  ).
      WHEN gc_fnam_sheettype    . me->set_sheettype( i_value  ).
      WHEN gc_fnam_hidden       . me->set_hidden( i_value  ).
      WHEN gc_fnam_righttoleft  . me->set_righttoleft( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid      . return = REF #( me->gs_sheetproperties-sheetid ).
      WHEN gc_fnam_title        . return = REF #( me->gs_sheetproperties-title ).
      WHEN gc_fnam_index        . return = REF #( me->gs_sheetproperties-index ).
      WHEN gc_fnam_sheettype    . return = REF #( me->gs_sheetproperties-sheettype ).
      WHEN gc_fnam_hidden       . return = REF #( me->gs_sheetproperties-hidden ).
      WHEN gc_fnam_righttoleft  . return = REF #( me->gs_sheetproperties-righttoleft ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

    DATA(lif_a2g_json_grid) = me->new_gridproperties(  ).
    lif_a2g_json_grid->yif_a2g_context~write_data( REF #( me->gs_sheetproperties-gridproperties ) ).

    DATA(lif_a2g_json_color) = me->new_tabcolor(  ).
    lif_a2g_json_color->yif_a2g_context~write_data( REF #( me->gs_sheetproperties-tabcolor ) ).

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_gridproperties ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_sheetproperties-gridproperties =  <fs_value_range>.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_tabcolor ).
    dref_data = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_sheetproperties-tabcolor =  <fs_value_range>.


  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_sheetproperties ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_gridproperties  . return = me->new_gridproperties( ).
      WHEN gc_fnam_tabcolor        . return = me->new_tabcolor( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_gridproperties. return ?= me->go_json_array->getinstance( gc_fnam_gridproperties ).
      WHEN gc_fnam_tabcolor.       return ?= me->go_json_array->getinstance( gc_fnam_tabcolor ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_gridproperties.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDPROPERTIES' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_gridproperties
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_tabcolor.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_tabcolor
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
