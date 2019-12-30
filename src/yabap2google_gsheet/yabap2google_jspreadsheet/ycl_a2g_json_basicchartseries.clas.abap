CLASS ycl_a2g_json_basicchartseries DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF TY_S_JSON_BASICCHARTSERIES,
             SERIES     TYPE  YCL_A2G_JSON_CHARTDATA=>TY_S_JSON_CHARTDATA,
             TARGETAXIS TYPE STRING,
             TYPE       TYPE STRING,
             LINESTYLE  TYPE YCL_A2G_JSON_LINESTYLE=>TY_S_JSON_LINESTYLE,
             COLOR      TYPE YCL_A2G_JSON_COLOR=>TY_S_JSON_COLOR,
           END OF TY_S_JSON_BASICCHARTSERIES.
    TYPES ty_t_json_basicchartseries TYPE STANDARD TABLE OF ty_s_json_basicchartseries WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_SERIES         TYPE string VALUE 'SERIES'.
    CONSTANTS: gc_fnam_TARGETAXIS     TYPE string VALUE 'TARGETAXIS'.
    CONSTANTS: gc_fnam_TYPE           TYPE string VALUE 'TYPE'.
    CONSTANTS: gc_fnam_LINESTYLE      TYPE string VALUE 'LINESTYLE'.
    CONSTANTS: gc_fnam_COLOR          TYPE string VALUE 'COLOR'.


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

    DATA: gs_basicchartseries  TYPE ty_s_json_basicchartseries.

    METHODS set_TARGETAXIS       IMPORTING !i_value TYPE REF TO data.
    METHODS set_TYPE             IMPORTING !i_value TYPE REF TO data.

    METHODS new_SERIES     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_LINESTYLE  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_COLOR      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_basicchartseries IMPLEMENTATION.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_TARGETAXIS  . me->set_TARGETAXIS( i_value ).
      WHEN gc_fnam_TYPE  . me->set_TYPE( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_TARGETAXIS   . return = REF #( me->gs_basicchartseries-TARGETAXIS ).
      WHEN gc_fnam_TYPE  . return = REF #( me->gs_basicchartseries-TYPE ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_TYPE.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartseries-TYPE <> <fs_value>.
      me->gs_basicchartseries-TYPE = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_TARGETAXIS.

    FIELD-SYMBOLS <fs_value> TYPE STRING.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartseries-TARGETAXIS <> <fs_value>.
      me->gs_basicchartseries-TARGETAXIS = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD push_data.
    DATA(lif_a2g_json) = me->new_SERIES(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_basicchartseries-SERIES   ) ).
    lif_a2g_json = me->new_LINESTYLE(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_basicchartseries-LINESTYLE ) ).
    lif_a2g_json = me->new_COLOR(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_basicchartseries-COLOR    ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_SERIES   .      me->gs_basicchartseries-SERIES     =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_LINESTYLE.      me->gs_basicchartseries-LINESTYLE  =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_COLOR    .      me->gs_basicchartseries-COLOR      =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_basicchartseries  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_SERIES    . return = me->new_SERIES( ).
      WHEN gc_fnam_LINESTYLE . return = me->new_LINESTYLE( ).
      WHEN gc_fnam_COLOR     . return = me->new_COLOR( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_SERIES   . return ?= me->go_json_array->getinstance( gc_fnam_SERIES    ).
      WHEN gc_fnam_LINESTYLE. return ?= me->go_json_array->getinstance( gc_fnam_LINESTYLE ).
      WHEN gc_fnam_COLOR    . return ?= me->go_json_array->getinstance( gc_fnam_COLOR     ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_COLOR.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_COLOR
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_LINESTYLE.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_LINESTYLE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_LINESTYLE
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_SERIES.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CHARTDATA' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_SERIES
                                     im_object = lo_object ).
  ENDMETHOD.



ENDCLASS.
