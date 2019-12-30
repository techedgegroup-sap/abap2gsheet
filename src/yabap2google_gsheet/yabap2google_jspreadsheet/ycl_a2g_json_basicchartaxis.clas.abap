CLASS ycl_a2g_json_basicchartaxis DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_basicchartaxis,
             POSITION          TYPE STRING,
             TITLE             TYPE STRING,
             FORMAT            TYPE YCL_A2G_JSON_TEXTFORMAT=>TY_S_JSON_TEXTFORMAT,
             TITLETEXTPOSITION TYPE YCL_A2G_JSON_TEXTPOS=>TY_S_JSON_TEXTPOS,
             VIEWWINDOWOPTIONS TYPE YCL_A2G_JSON_VIEWWINDOWOPT=>TY_S_JSON_VIEWWINDOWOPT,
           END OF ty_s_json_basicchartaxis.
    TYPES ty_t_json_basicchartaxis TYPE STANDARD TABLE OF ty_s_json_basicchartaxis WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_POSITION           TYPE string VALUE 'POSITION'.
    CONSTANTS: gc_fnam_TITLE              TYPE string VALUE 'TITLE'.
    CONSTANTS: gc_fnam_FORMAT             TYPE string VALUE 'FORMAT'.
    CONSTANTS: gc_fnam_TITLETEXTPOSITION  TYPE string VALUE 'TITLETEXTPOSITION'.
    CONSTANTS: gc_fnam_VIEWWINDOWOPTIONS  TYPE string VALUE 'VIEWWINDOWOPTIONS'.

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

    DATA: gs_basicchartaxis  TYPE ty_s_json_basicchartaxis.

    METHODS set_POSITION         IMPORTING !i_value TYPE REF TO data.
    METHODS set_TITLE            IMPORTING !i_value TYPE REF TO data.

    METHODS new_FORMAT             RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_TITLETEXTPOSITION  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_VIEWWINDOWOPTIONS  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_basicchartaxis IMPLEMENTATION.

 METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_POSITION  . me->set_POSITION( i_value ).
      WHEN gc_fnam_TITLE  . me->set_TITLE( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_POSITION   . return = REF #( me->gs_basicchartaxis-POSITION ).
      WHEN gc_fnam_TITLE  . return = REF #( me->gs_basicchartaxis-TITLE ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_TITLE.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartaxis-TITLE <> <fs_value>.
      me->gs_basicchartaxis-TITLE = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_POSITION.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartaxis-POSITION <> <fs_value>.
      me->gs_basicchartaxis-POSITION = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_FORMAT(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_basicchartaxis-FORMAT            ) ).

    lif_a2g_json = me->new_TITLETEXTPOSITION(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_basicchartaxis-TITLETEXTPOSITION ) ).

    lif_a2g_json = me->new_VIEWWINDOWOPTIONS(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_basicchartaxis-VIEWWINDOWOPTIONS ) ).

    ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_FORMAT            .    me->gs_basicchartaxis-FORMAT             =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_TITLETEXTPOSITION .    me->gs_basicchartaxis-TITLETEXTPOSITION  =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_VIEWWINDOWOPTIONS .    me->gs_basicchartaxis-VIEWWINDOWOPTIONS  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_basicchartaxis  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_FORMAT             . return = me->new_FORMAT( ).
      WHEN gc_fnam_TITLETEXTPOSITION  . return = me->new_TITLETEXTPOSITION( ).
      WHEN gc_fnam_VIEWWINDOWOPTIONS  . return = me->new_VIEWWINDOWOPTIONS( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_FORMAT           . return ?= me->go_json_array->getinstance( gc_fnam_FORMAT            ).
      WHEN gc_fnam_TITLETEXTPOSITION. return ?= me->go_json_array->getinstance( gc_fnam_TITLETEXTPOSITION ).
      WHEN gc_fnam_VIEWWINDOWOPTIONS. return ?= me->go_json_array->getinstance( gc_fnam_VIEWWINDOWOPTIONS ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_VIEWWINDOWOPTIONS.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_VIEWWINDOWOPT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_VIEWWINDOWOPTIONS
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_TITLETEXTPOSITION.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTPOS' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_TITLETEXTPOSITION
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_FORMAT.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_FORMAT
                                     im_object = lo_object ).
  ENDMETHOD.
ENDCLASS.
