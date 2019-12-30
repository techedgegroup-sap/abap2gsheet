CLASS ycl_a2g_json_chartsourcerange DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_chartsourcerange,
             SOURCES TYPE YCL_A2G_JSON_GRIDRANGE=>TY_T_json_gridrange,
           END OF ty_s_json_chartsourcerange.
    TYPES ty_t_json_chartsourcerange TYPE STANDARD TABLE OF ty_s_json_chartsourcerange WITH NON-UNIQUE DEFAULT KEY.

        CONSTANTS: gc_fnam_sources   TYPE string VALUE 'SOURCES'.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_chartsourcerange  TYPE ty_s_json_chartsourcerange.

    METHODS new_SOURCES          RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_chartsourcerange IMPLEMENTATION.

  METHOD push_data.

    LOOP AT me->gs_chartsourcerange-SOURCES INTO DATA(ls_namedrange).
      DATA(lif_a2g_json_namedrange) = me->new_SOURCES(  ).
      lif_a2g_json_namedrange->yif_a2g_context~write_data( REF #( ls_namedrange ) ).
    ENDLOOP.


  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

clear me->gs_chartsourcerange-SOURCES.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_SOURCES.
        APPEND <fs_value_range> TO me->gs_chartsourcerange-SOURCES.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_chartsourcerange  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

    METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_SOURCES        . return = me->new_SOURCES( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_SOURCES .
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_SOURCES ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD new_SOURCES.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDRANGE' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_SOURCES ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_SOURCES && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.
ENDCLASS.
