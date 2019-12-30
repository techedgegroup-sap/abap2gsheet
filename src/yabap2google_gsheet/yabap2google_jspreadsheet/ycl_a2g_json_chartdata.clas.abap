CLASS ycl_a2g_json_chartdata DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES: BEGIN OF ty_s_json_chartdata,
             SOURCE_RANGE   TYPE YCL_A2G_JSON_CHARTSOURCERANGE=>ty_s_json_ChartSourceRange,
           END OF ty_s_json_chartdata.
    TYPES ty_t_json_chartdata TYPE STANDARD TABLE OF ty_s_json_chartdata WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_SOURCERANGE   TYPE string VALUE 'SOURCERANGE'.


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

    DATA: gs_chartdata  TYPE ty_s_json_chartdata.

    METHODS new_SOURCERANGE          RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_chartdata IMPLEMENTATION.
  METHOD push_data.
    DATA(lif_a2g_json_prop) = me->new_SOURCERANGE(  ).
    lif_a2g_json_prop->yif_a2g_context~write_data( REF #( me->gs_chartdata-SOURCE_RANGE ) ).
  ENDMETHOD.

  METHOD rebuild_data.

    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_SOURCERANGE.
        me->gs_chartdata-SOURCE_RANGE  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_chartdata  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_SOURCERANGE  . return = me->new_SOURCERANGE( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_SOURCERANGE. return ?= me->go_json_array->getinstance( gc_fnam_SOURCERANGE ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_SOURCERANGE.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CHARTSOURCERANGE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_SOURCERANGE
                                     im_object = lo_object ).
  ENDMETHOD.

  ENDCLASS.
