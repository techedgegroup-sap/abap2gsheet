CLASS ycl_a2g_json_rowdata DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_rowdata,
             values TYPE ycl_a2g_json_celldata=>ty_t_json_celldata,
           END OF ty_s_json_rowdata.
    TYPES ty_t_json_rowdata TYPE STANDARD TABLE OF ty_s_json_rowdata WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_values   TYPE string VALUE 'VALUES'.


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

    DATA: gs_rowdata  TYPE ty_s_json_rowdata.

    METHODS new_values     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_rowdata IMPLEMENTATION.

  METHOD push_data.

    LOOP AT me->gs_rowdata-values INTO DATA(ls_value).
      DATA(lif_a2g_json_metadata) = me->new_values(  ).
      lif_a2g_json_metadata->yif_a2g_context~write_data( REF #( ls_value ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.
    CLEAR me->gs_rowdata-values.
    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_values.
        APPEND <fs_value_range> TO me->gs_rowdata-values.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_rowdata  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_values   . return = me->new_values( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_values.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_values ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD new_values.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CELLDATA' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_values ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_values && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
