CLASS ycl_a2g_json_batchget_dflt_res DEFINITION
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

    TYPES: BEGIN OF ty_s_json_batchget,
             spreadsheetid TYPE string,
             valueranges   TYPE ycl_a2g_json_valuerange=>ty_t_json_value_range,
           END OF ty_s_json_batchget.

    CONSTANTS: gc_fnam_spreadsheetid   TYPE string VALUE 'SPREADSHEETID'.
    CONSTANTS: gc_fnam_valuerange        TYPE string VALUE 'VALUERANGE'.
  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_batchget TYPE ty_s_json_batchget.

    METHODS set_spreadsheetid         IMPORTING !i_value TYPE REF TO data.

    METHODS new_valueranges     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_batchget_dflt_res IMPLEMENTATION.

  METHOD set_spreadsheetid.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchget-spreadsheetid <> <fs_value>.
      me->gs_batchget-spreadsheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.



  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid      . me->set_spreadsheetid( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid       . return = REF #( me->gs_batchget-spreadsheetid ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
    LOOP AT me->gs_batchget-valueranges INTO DATA(lv_valuerange).
      DATA(lif_a2g_json_grid) = me->new_valueranges(  ).
      lif_a2g_json_grid->yif_a2g_context~write_data( REF #( lv_valuerange ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_valuerange ).
    LOOP AT lt_names INTO DATA(lv_name).
      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      APPEND <fs_value_range> TO me->gs_batchget-valueranges.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_batchget ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_valuerange    . return = me->new_valueranges( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_valuerange.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_valuerange ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


  METHOD new_valueranges.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_VALUERANGE' ).

    lo_object ?= return.
    data: lv_line type numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_valuerange ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_valuerange  && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.
ENDCLASS.
