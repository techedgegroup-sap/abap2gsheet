CLASS ycl_a2g_json_griddata DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_griddata,
             start_row       TYPE i,
             start_column    TYPE i,
             row_data        TYPE ycl_a2g_json_rowdata=>ty_t_json_rowdata,
             row_metadata    TYPE ycl_a2g_json_dimensionpropert=>ty_t_json_dimensionpropert,
             column_metadata TYPE ycl_a2g_json_dimensionpropert=>ty_t_json_dimensionpropert,
           END OF ty_s_json_griddata.
    TYPES ty_t_json_griddata TYPE STANDARD TABLE OF ty_s_json_griddata WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_startrow         TYPE string VALUE 'STARTROW      '.
    CONSTANTS: gc_fnam_startcolumn      TYPE string VALUE 'STARTCOLUMN   '.
    CONSTANTS: gc_fnam_rowdata          TYPE string VALUE 'ROWDATA       '.
    CONSTANTS: gc_fnam_rowmetadata      TYPE string VALUE 'ROWMETADATA   '.
    CONSTANTS: gc_fnam_columnmetadata   TYPE string VALUE 'COLUMNMETADATA'.


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

    DATA: gs_griddata  TYPE ty_s_json_griddata.

    METHODS set_startrow            IMPORTING !i_value TYPE REF TO data.
    METHODS set_startcolumn         IMPORTING !i_value TYPE REF TO data.

    METHODS new_rowdata         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_rowmetadata     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_columnmetadata  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_griddata IMPLEMENTATION.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_startrow    . me->set_startrow( i_value ).
      WHEN gc_fnam_startcolumn . me->set_startcolumn( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_startrow     . return = REF #( me->gs_griddata-start_row   ).
      WHEN gc_fnam_startcolumn  . return = REF #( me->gs_griddata-start_column ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_startcolumn.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_griddata-start_column <> <fs_value>.
      me->gs_griddata-start_column = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_startrow.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_griddata-start_row <> <fs_value>.
      me->gs_griddata-start_row = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD push_data.
    LOOP AT me->gs_griddata-row_data INTO DATA(ls_rowdata).
      DATA(lif_a2g_json_rowdata) = me->new_rowdata(  ).
      lif_a2g_json_rowdata->yif_a2g_context~write_data( REF #( ls_rowdata ) ).
    ENDLOOP.

    LOOP AT me->gs_griddata-row_metadata INTO DATA(ls_rowmetadata).
      DATA(lif_a2g_json_rowmetadata) = me->new_rowmetadata(  ).
      lif_a2g_json_rowmetadata->yif_a2g_context~write_data( REF #( ls_rowmetadata ) ).
    ENDLOOP.

    LOOP AT me->gs_griddata-column_metadata INTO DATA(ls_columnmetadata).
      DATA(lif_a2g_json_metadata) = me->new_columnmetadata(  ).
      lif_a2g_json_metadata->yif_a2g_context~write_data( REF #( ls_columnmetadata ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.
clear  me->gs_griddata-row_data.
    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_rowdata.
        APPEND <fs_value_range> TO me->gs_griddata-row_data.
      ELSEIF lv_name CS me->gc_fnam_rowmetadata.
        APPEND <fs_value_range> TO me->gs_griddata-row_metadata.
      ELSEIF lv_name CS me->gc_fnam_columnmetadata.
        APPEND <fs_value_range> TO me->gs_griddata-column_metadata.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_griddata  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_rowdata       . return = me->new_rowdata( ).
      WHEN gc_fnam_rowmetadata   . return = me->new_rowmetadata( ).
      WHEN gc_fnam_columnmetadata. return = me->new_columnmetadata( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_rowdata.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_rowdata ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      WHEN gc_fnam_rowmetadata .
        lt_names =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_rowmetadata ).
        TRY.
            lv_name = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      WHEN gc_fnam_columnmetadata .
        lt_names =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_columnmetadata ).
        TRY.
            lv_name = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD new_rowdata.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_ROWDATA' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_rowdata ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_rowdata && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_rowmetadata.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DIMENSIONPROPERT' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_rowmetadata ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_rowmetadata && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_columnmetadata.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DIMENSIONPROPERT' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_columnmetadata ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_columnmetadata && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
