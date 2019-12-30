CLASS ycl_a2g_json_datafilter DEFINITION
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

    TYPES: BEGIN OF ty_s_json_datafilter,
             developermetadatalookup TYPE ycl_a2g_json_devmetadatalookup=>ty_s_json_devrmetalook,
             a1range                 TYPE string,
             gridrange               TYPE ycl_a2g_json_gridrange=>ty_s_json_gridrange,
           END OF ty_s_json_datafilter.

    TYPES: ty_t_json_datafilter type STANDARD TABLE OF ty_s_json_datafilter WITH NON-UNIQUE DEFAULT KEY.


    CONSTANTS: gc_fnam_devmetalookup    TYPE string VALUE 'DEVMETALOOKUP'.
    CONSTANTS: gc_fnam_a1range          TYPE string VALUE 'A1RANGE'.
    CONSTANTS: gc_fnam_gridrange        TYPE string VALUE 'GRIDRANGE'.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_datafilter TYPE ty_s_json_datafilter.

    METHODS set_a1range         IMPORTING !i_value TYPE REF TO data.

    METHODS new_devmetalookup RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_gridrange     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_datafilter IMPLEMENTATION.


  METHOD set_a1range.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_datafilter-a1range <> <fs_value>.
      me->gs_datafilter-a1range = <fs_value>.
    ENDIF.
  ENDMETHOD.



  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_a1range      . me->set_a1range( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_a1range       . return = REF #( me->gs_datafilter-a1range ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
    DATA(lif_a2g_json) = me->new_devmetalookup(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_datafilter-developermetadatalookup ) ).

    DATA(lif_a2g_json_grid) = me->new_gridrange(  ).
    lif_a2g_json_grid->yif_a2g_context~write_data( REF #( me->gs_datafilter-gridrange ) ).

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_devmetalookup ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_datafilter-developermetadatalookup = <fs_value_range>.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_gridrange ).
    dref_data = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_datafilter-gridrange = <fs_value_range>.


  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_datafilter ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_devmetalookup. return = me->new_devmetalookup( ).
      WHEN gc_fnam_gridrange    . return = me->new_gridrange( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_devmetalookup.
        return ?= me->go_json_array->getinstance( me->gc_fnam_devmetalookup ).
      WHEN gc_fnam_gridrange.
        return ?= me->go_json_array->getinstance( me->gc_fnam_gridrange ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_devmetalookup.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DEVMETADATALOOKUP' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_devmetalookup
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_gridrange.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDRANGE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_gridrange
                                     im_object = lo_object ).
  ENDMETHOD.


ENDCLASS.
