CLASS ycl_a2g_json_matchedvaluerange DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.


    TYPES: BEGIN OF ty_s_json_matchedvaluerange,
             valuerange  TYPE ycl_a2g_json_devmetadatalookup=>ty_s_json_devrmetalook,
             datafilters TYPE ycl_a2g_json_datafilter=>ty_t_json_datafilter,
           END OF ty_s_json_matchedvaluerange.
TYPES: ty_t_json_matchedvaluerange TYPE STANDARD TABLE OF ty_s_json_matchedvaluerange WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS: gc_fnam_valuerange    TYPE string VALUE 'VALUERANGE'.
    CONSTANTS: gc_fnam_datafilters   TYPE string VALUE 'DATAFILTERS'.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_matchedvaluerange TYPE ty_s_json_matchedvaluerange.

    METHODS new_datafilters RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_valuerange  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_matchedvaluerange IMPLEMENTATION.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_valuerange(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_matchedvaluerange-valuerange ) ).

    LOOP AT me->gs_matchedvaluerange-datafilters INTO DATA(ls_datafilter).
      DATA(lif_a2g_json_grid) = me->new_datafilters(  ).
      lif_a2g_json_grid->yif_a2g_context~write_data( REF #( ls_datafilter ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_valuerange ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_matchedvaluerange-valuerange = <fs_value_range>.

    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_datafilters ).
    LOOP AT lt_names INTO DATA(lv_name).
      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      dref_data = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      APPEND <fs_value_range> TO me->gs_matchedvaluerange-datafilters.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_matchedvaluerange ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_valuerange   . return = me->new_valuerange( ).
      WHEN gc_fnam_datafilters  . return = me->new_datafilters( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_valuerange.
        return ?= me->go_json_array->getinstance( me->gc_fnam_valuerange ).
      WHEN gc_fnam_datafilters.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_datafilters ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD new_valuerange.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_VALUERANGE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_valuerange
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_datafilters.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DATAFILTER' ).

    lo_object ?= return.
    data: lv_line type numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_datafilters ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_datafilters && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
