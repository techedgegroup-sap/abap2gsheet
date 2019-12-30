CLASS ycl_a2g_json_sheet_prop DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

     TYPES: BEGIN OF ty_s_json_sheet_prop,
             sheet_id        TYPE i,
             title          TYPE string,
             index          TYPE i,
             sheet_type      TYPE string,
             grid_properties  TYPE YCL_A2G_JSON_GRIDPROPERTIES=>ty_s_json_gridproperties ,
             hidden         TYPE string,
             tab_color       TYPE YCL_A2G_JSON_COLOR=>ty_s_json_color,
             right_to_left    TYPE string,
           END OF ty_s_json_sheet_prop.
    TYPES ty_t_json_sheet_prop TYPE STANDARD TABLE OF ty_s_json_sheet_prop WITH NON-UNIQUE DEFAULT KEY.


      CONSTANTS: gc_fnam_sheetid          TYPE string VALUE 'sheetid       '.
      CONSTANTS: gc_fnam_title            TYPE string VALUE 'title         '.
      CONSTANTS: gc_fnam_index            TYPE string VALUE 'index         '.
      CONSTANTS: gc_fnam_sheettype        TYPE string VALUE 'sheettype     '.
      CONSTANTS: gc_fnam_gridproperties    TYPE string VALUE 'gridproperties '.
      CONSTANTS: gc_fnam_hidden           TYPE string VALUE 'hidden        '.
      CONSTANTS: gc_fnam_tabcolor         TYPE string VALUE 'tabcolor      '.
      CONSTANTS: gc_fnam_righttoleft      TYPE string VALUE 'righttoleft   '.


    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.


    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_sheet_prop  TYPE ty_s_json_sheet_prop.

    METHODS set_sheetid         IMPORTING !i_value TYPE REF TO data.
    METHODS set_title           IMPORTING !i_value TYPE REF TO data.
    METHODS set_index           IMPORTING !i_value TYPE REF TO data.
    METHODS set_sheettype       IMPORTING !i_value TYPE REF TO data.
    METHODS set_hidden          IMPORTING !i_value TYPE REF TO data.
    METHODS set_righttoleft     IMPORTING !i_value TYPE REF TO data.

    METHODS new_gridproperties   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_tabcolor        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_sheet_prop IMPLEMENTATION.
  METHOD yif_a2g_json~set_default.

    DATA: lv_string TYPE string.

    DATA(lif_gridProperties) = me->yif_a2g_json~new_element( gc_fnam_gridproperties ).
    lif_gridProperties->set_default( ).

    lv_string = 'GRID'.
    me->set_sheetType( REF #( lv_string ) ).

  ENDMETHOD.
  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid       . me->set_sheetid( i_value  ).
      WHEN gc_fnam_title         . me->set_title( i_value  ).
      WHEN gc_fnam_index         . me->set_index( i_value  ).
      WHEN gc_fnam_sheettype     . me->set_sheettype( i_value  ).
      WHEN gc_fnam_hidden        . me->set_hidden( i_value  ).
      WHEN gc_fnam_righttoleft   . me->set_righttoleft( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid        . return = REF #( me->gs_sheet_prop-sheet_id  ).
      WHEN gc_fnam_title          . return = REF #( me->gs_sheet_prop-title  ).
      WHEN gc_fnam_index          . return = REF #( me->gs_sheet_prop-index  ).
      WHEN gc_fnam_sheettype      . return = REF #( me->gs_sheet_prop-sheet_type  ).
      WHEN gc_fnam_hidden         . return = REF #( me->gs_sheet_prop-hidden  ).
      WHEN gc_fnam_righttoleft    . return = REF #( me->gs_sheet_prop-right_to_left  ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_righttoleft.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheet_prop-right_to_left <> <fs_value>.
      me->gs_sheet_prop-right_to_left = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_hidden.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheet_prop-hidden <> <fs_value>.
      me->gs_sheet_prop-hidden = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_sheettype.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheet_prop-sheet_type <> <fs_value>.
      me->gs_sheet_prop-sheet_type = <fs_value>.
    ENDIF.
  ENDMETHOD.
  METHOD set_index.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheet_prop-index <> <fs_value>.
      me->gs_sheet_prop-index = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_title.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheet_prop-title <> <fs_value>.
      me->gs_sheet_prop-title = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_sheetid.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_sheet_prop-sheet_id <> <fs_value>.
      me->gs_sheet_prop-sheet_id = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD push_data.

    DATA(lif_a2g_json) = me->new_gridproperties(   ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_sheet_prop-grid_properties  )  ).

    lif_a2g_json = me->new_tabcolor(   ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_sheet_prop-tab_color )  ).


    ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(   ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name  ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data(  ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_gridproperties .
        me->gs_sheet_prop-grid_properties   =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_tabcolor .
        me->gs_sheet_prop-tab_color   =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_sheet_prop ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_gridproperties   . return = me->new_gridproperties(  ).
      WHEN gc_fnam_tabcolor  . return = me->new_tabcolor(  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_gridproperties . return ?= me->go_json_array->getinstance( gc_fnam_gridproperties   ).
      WHEN gc_fnam_tabcolor. return ?= me->go_json_array->getinstance( gc_fnam_tabcolor  ).

    ENDCASE.
  ENDMETHOD.

  METHOD new_gridproperties .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDPROPERTIES'  ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_gridproperties
                                     im_object = lo_object  ).
  ENDMETHOD.

  METHOD new_tabcolor.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR'  ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_tabcolor
                                     im_object = lo_object  ).
  ENDMETHOD.

ENDCLASS.
