CLASS ycl_a2g_json_borders DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    CONSTANTS gc_fnam_top    TYPE string VALUE 'TOP'   .
    CONSTANTS gc_fnam_bottom TYPE string VALUE 'BOTTOM'.
    CONSTANTS gc_fnam_left   TYPE string VALUE 'LEFT'  .
    CONSTANTS gc_fnam_right  TYPE string VALUE 'RIGHT' .

    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.

    TYPES: BEGIN OF ty_s_json_borders,
             top    TYPE ycl_a2g_json_border=>ty_s_json_border,
             bottom TYPE ycl_a2g_json_border=>ty_s_json_border,
             left   TYPE ycl_a2g_json_border=>ty_s_json_border,
             right  TYPE ycl_a2g_json_border=>ty_s_json_border,
           END OF ty_s_json_borders.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_borders  TYPE ty_s_json_borders.

    METHODS new_top    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_bottom RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_left   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_right  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_borders IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.

    DATA(lif_border) = me->yif_a2g_json~new_element( gc_fnam_top ).
    lif_border->set_default( ).

    lif_border = me->yif_a2g_json~new_element( gc_fnam_bottom ).
    lif_border->set_default( ).

    lif_border = me->yif_a2g_json~new_element( gc_fnam_left ).
    lif_border->set_default( ).

    lif_border = me->yif_a2g_json~new_element( gc_fnam_right ).
    lif_border->set_default( ).

  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_top   . return = me->new_top( ).
      WHEN gc_fnam_bottom. return = me->new_bottom( ).
      WHEN gc_fnam_left  . return = me->new_left( ).
      WHEN gc_fnam_right . return = me->new_right( ).
    ENDCASE.

  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_top   . return ?= me->go_json_array->getinstance( me->gc_fnam_top    ).
      WHEN gc_fnam_bottom. return ?= me->go_json_array->getinstance( me->gc_fnam_bottom ).
      WHEN gc_fnam_left  . return ?= me->go_json_array->getinstance( me->gc_fnam_left   ).
      WHEN gc_fnam_right . return ?= me->go_json_array->getinstance( me->gc_fnam_right  ).
    ENDCASE.

  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_top( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_borders-top    ) ).

    lif_a2g_json = me->new_bottom( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_borders-bottom ) ).

    lif_a2g_json = me->new_left( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_borders-left   ) ).

    lif_a2g_json = me->new_right( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_borders-right  ) ).

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.


      IF lv_name CS me->gc_fnam_top        .
        me->gs_borders-top   =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_bottom .
        me->gs_borders-bottom =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_left   .
        me->gs_borders-left   =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_right  .
        me->gs_borders-right  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_borders  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD new_top.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BORDER' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_top
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_bottom.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BORDER' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_bottom
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_left.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BORDER' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_left
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_right.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BORDER' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_right
                                     im_object = lo_object ).
  ENDMETHOD.




ENDCLASS.
