CLASS ycl_a2g_json_border DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_fnam_style TYPE string VALUE 'STYLE'.
    CONSTANTS gc_fnam_width TYPE string VALUE 'WIDTH'.
    CONSTANTS gc_fnam_color TYPE string VALUE 'COLOR'.


    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.

    TYPES: BEGIN OF ty_s_json_border,
             style TYPE string,
             width TYPE i,
             color TYPE ycl_a2g_json_color=>ty_s_json_color,
           END OF ty_s_json_border.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_border  TYPE ty_s_json_border.

    METHODS set_style  IMPORTING !i_value TYPE REF TO data.
    METHODS set_width  IMPORTING !i_value TYPE REF TO data.

    METHODS new_color RETURNING VALUE(return) TYPE REF TO yif_a2g_json.


  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_a2g_json_border IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.
    DATA: lv_style TYPE string.
    DATA: lv_width TYPE i.
    lv_style = 'SOLID'.
    me->set_style( REF #( lv_style ) ).
    lv_width = 1.
    me->set_width( REF #( lv_width ) ).
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_color   . return = me->new_color( ).
    ENDCASE.

  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_color   . return ?= me->go_json_array->getinstance( me->gc_fnam_color    ).
    ENDCASE.

  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_style . me->set_style( i_value ).
      WHEN gc_fnam_width . me->set_width( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_style . return = REF #( me->gs_border-style ).
      WHEN gc_fnam_width . return = REF #( me->gs_border-width  ).

    ENDCASE.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_color( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_border-color  ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.

      IF lv_name CS me->gc_fnam_color .
        me->gs_border-color   =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_border  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD set_style.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_border-style <> <fs_value>.
      me->gs_border-style = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_width.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_border-width <> <fs_value>.
      me->gs_border-width = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD new_color.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_color
                                     im_object = lo_object ).
  ENDMETHOD.


ENDCLASS.
