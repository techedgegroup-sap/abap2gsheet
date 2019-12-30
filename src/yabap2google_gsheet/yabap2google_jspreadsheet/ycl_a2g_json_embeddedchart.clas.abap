CLASS ycl_a2g_json_embeddedchart DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_embeddedchart,
             CHART_ID  TYPE I,
             SPEC     TYPE YCL_A2G_JSON_CHARTSPEC=>TY_S_JSON_CHARTSPEC,
             POSITION TYPE YCL_A2G_JSON_EMBEDDEDOBJPOS=>TY_S_JSON_EMBEDDEDOBJPOS,
           END OF ty_s_json_embeddedchart.
    TYPES ty_t_json_embeddedchart TYPE STANDARD TABLE OF ty_s_json_embeddedchart WITH NON-UNIQUE KEY CHART_ID.

    CONSTANTS: gc_fnam_CHARTID   TYPE string VALUE 'CHARTID'.
    CONSTANTS: gc_fnam_SPEC      TYPE string VALUE 'SPEC'.
    CONSTANTS: gc_fnam_POSITION  TYPE string VALUE 'POSITION'.


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

    DATA: gs_embeddedchart  TYPE ty_s_json_embeddedchart.

   METHODS set_CHARTID        IMPORTING !i_value TYPE REF TO data.

    METHODS new_SPEC          RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_POSITION      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_embeddedchart IMPLEMENTATION.

  METHOD set_CHARTID.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_embeddedchart-CHART_ID <> <fs_value>.
      me->gs_embeddedchart-CHART_ID = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_CHARTID  . me->set_CHARTID( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_CHARTID   . return = REF #( me->gs_embeddedchart-CHART_ID ).
    ENDCASE.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_SPEC(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_embeddedchart-SPEC ) ).

    lif_a2g_json = me->new_POSITION(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_embeddedchart-POSITION ) ).
  ENDMETHOD.

  METHOD rebuild_data.
   DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_SPEC    . me->gs_embeddedchart-SPEC      =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_POSITION. me->gs_embeddedchart-POSITION  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_embeddedchart  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.



  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_SPEC     . return = me->new_SPEC( ).
      WHEN gc_fnam_POSITION . return = me->new_POSITION( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_SPEC    . return ?= me->go_json_array->getinstance( gc_fnam_SPEC     ).
      WHEN gc_fnam_POSITION. return ?= me->go_json_array->getinstance( gc_fnam_POSITION ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_SPEC.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CHARTSPEC' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_SPEC
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_POSITION.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_EMBEDDEDOBJPOS' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_POSITION
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
