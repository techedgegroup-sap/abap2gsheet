CLASS ycl_a2g_json_batchget_dflt_req DEFINITION
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


    TYPES: BEGIN OF ty_s_json_batchgetdflt,
             datafilters          TYPE ycl_a2g_json_datafilter=>ty_t_json_datafilter,
             majordimension       TYPE string,
             valuerenderoption    TYPE string,
             datetimerenderoption TYPE string,
           END OF ty_s_json_batchgetdflt.

    CONSTANTS: gc_fnam_majordimension       TYPE string VALUE 'MAJORDIMENSION'.
    CONSTANTS: gc_fnam_valuerenderoption    TYPE string VALUE 'VALUERENDEROPTION'.
    CONSTANTS: gc_fnam_datafilters          TYPE string VALUE 'DATAFILTERS'.
    CONSTANTS: gc_fnam_datetimerenderoption TYPE string VALUE 'DATETIMERENDEROPTION'.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_batchgetdflt TYPE ty_s_json_batchgetdflt.

    METHODS set_majordimension    IMPORTING !i_value TYPE REF TO data.
    METHODS set_valuerenderoption  IMPORTING !i_value TYPE REF TO data.
    METHODS set_datetimerenderoption     IMPORTING !i_value TYPE REF TO data.

    METHODS new_datafilters RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_batchget_dflt_req IMPLEMENTATION.



  METHOD set_majordimension .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchgetdflt-majordimension <> <fs_value>.
      me->gs_batchgetdflt-majordimension = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_valuerenderoption.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchgetdflt-valuerenderoption <> <fs_value>.
      me->gs_batchgetdflt-valuerenderoption = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_datetimerenderoption.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchgetdflt-datetimerenderoption <> <fs_value>.
      me->gs_batchgetdflt-datetimerenderoption = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_majordimension     . me->set_majordimension( i_value ).
      WHEN gc_fnam_valuerenderoption   . me->set_valuerenderoption( i_value ).
      WHEN gc_fnam_datetimerenderoption      . me->set_datetimerenderoption( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_majordimension      . return = REF #( me->gs_batchgetdflt-majordimension ).
      WHEN gc_fnam_valuerenderoption    . return = REF #( me->gs_batchgetdflt-valuerenderoption ).
      WHEN gc_fnam_datetimerenderoption       . return = REF #( me->gs_batchgetdflt-datetimerenderoption ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
    LOOP AT me->gs_batchgetdflt-datafilters INTO DATA(ls_datafilter).
      DATA(lif_a2g_json_grid) = me->new_datafilters(  ).
      lif_a2g_json_grid->yif_a2g_context~write_data( REF #( ls_datafilter ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_datafilters ).
    LOOP AT lt_names INTO DATA(lv_name).
      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      data(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      APPEND <fs_value_range> TO me->gs_batchgetdflt-datafilters.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_batchgetdflt ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_datafilters. return = me->new_datafilters( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_datafilters.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_datafilters ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
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
