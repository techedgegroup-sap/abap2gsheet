CLASS ycl_a2g_json_overlayposition DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_json_overlayposition,
             anchor_cell    TYPE ycl_a2g_json_gridcoordinate=>ty_s_json_gridcoordinate,
             offset_x_pixels TYPE i,
             offset_y_pixels TYPE i,
             width_pixels   TYPE i,
             height_pixels  TYPE i,
           END OF ty_s_json_overlayposition.
    TYPES ty_t_json_overlayposition TYPE STANDARD TABLE OF ty_s_json_overlayposition WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_anchorcell     TYPE string VALUE 'ANCHORCELL   '.
    CONSTANTS: gc_fnam_offsetxpixels  TYPE string VALUE 'OFFSETXPIXELS'.
    CONSTANTS: gc_fnam_offsetypixels  TYPE string VALUE 'OFFSETYPIXELS'.
    CONSTANTS: gc_fnam_widthpixels    TYPE string VALUE 'WIDTHPIXELS  '.
    CONSTANTS: gc_fnam_heightpixels   TYPE string VALUE 'HEIGHTPIXELS '.


    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_overlayposition  TYPE ty_s_json_overlayposition.

    METHODS set_offsetxpixels IMPORTING !i_value TYPE REF TO data.
    METHODS set_offsetypixels IMPORTING !i_value TYPE REF TO data.
    METHODS set_widthpixels   IMPORTING !i_value TYPE REF TO data.
    METHODS set_heightpixels  IMPORTING !i_value TYPE REF TO data.
    METHODS new_anchorcell    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.


  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_overlayposition IMPLEMENTATION.

  METHOD set_heightpixels.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_overlayposition-height_pixels <> <fs_value>.
      me->gs_overlayposition-height_pixels = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_widthpixels.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_overlayposition-width_pixels <> <fs_value>.
      me->gs_overlayposition-width_pixels = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_offsetypixels.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_overlayposition-offset_y_pixels <> <fs_value>.
      me->gs_overlayposition-offset_y_pixels = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_offsetxpixels.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_overlayposition-offset_x_pixels <> <fs_value>.
      me->gs_overlayposition-offset_x_pixels = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_offsetxpixels  . me->set_offsetxpixels( i_value ).
      WHEN gc_fnam_offsetypixels  . me->set_offsetypixels( i_value ).
      WHEN gc_fnam_widthpixels  . me->set_widthpixels( i_value ).
      WHEN gc_fnam_heightpixels  . me->set_heightpixels( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_offsetxpixels   . return = REF #( me->gs_overlayposition-offset_x_pixels ).
      WHEN gc_fnam_offsetypixels   . return = REF #( me->gs_overlayposition-offset_y_pixels ).
      WHEN gc_fnam_widthpixels   . return = REF #( me->gs_overlayposition-width_pixels ).
      WHEN gc_fnam_heightpixels   . return = REF #( me->gs_overlayposition-height_pixels ).
    ENDCASE.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json_prop) = me->new_anchorcell(  ).
    lif_a2g_json_prop->yif_a2g_context~write_data( REF #( me->gs_overlayposition-anchor_cell  ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_anchorcell .
        me->gs_overlayposition-anchor_cell   =  <fs_value_range>.
      ENDIF.

    ENDLOOP..
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_overlayposition   ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_anchorcell   . return = me->new_anchorcell( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_anchorcell . return ?= me->go_json_array->getinstance( gc_fnam_anchorcell  ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_anchorcell .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDCOORDINATE ' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_anchorcell
                                     im_object = lo_object ).
  ENDMETHOD.
ENDCLASS.
