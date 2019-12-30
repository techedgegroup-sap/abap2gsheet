CLASS ycl_a2g_json_spreadsheet DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_spreadsheet ,
             spreadsheet_id     TYPE string,
             properties        TYPE ycl_a2g_json_spreadsheet_prop=>ty_s_json_spreadsheet_prop,
             sheets            TYPE ycl_a2g_json_sheets=>ty_t_json_sheets,
             named_ranges       TYPE ycl_a2g_json_namedrange=>ty_t_json_namedrange,
             spreadsheet_url    TYPE string,
             developer_metadata TYPE ycl_a2g_json_developermetadata=>ty_t_json_devrmeta,
           END OF ty_s_json_spreadsheet.

    CONSTANTS: gc_fnam_spreadsheetid   TYPE string VALUE 'SPREADSHEETID'.
    CONSTANTS: gc_fnam_spreadsheeturl  TYPE string VALUE 'SPREADSHEETURL'.

    CONSTANTS: gc_fnam_properties  TYPE string VALUE 'PROPERTIES'.
    CONSTANTS: gc_fnam_sheets      TYPE string VALUE 'SHEETS'.
    CONSTANTS: gc_fnam_namedranges TYPE string VALUE 'NAMEDRANGES'.
    CONSTANTS: gc_fnam_devmetadata TYPE string VALUE 'DEVELOPERMETADATA'.

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

    DATA: gs_spreadsheet  TYPE ty_s_json_spreadsheet.

    METHODS set_spreadsheetid      IMPORTING !i_value TYPE REF TO data.
    METHODS set_spreadsheeturl        IMPORTING !i_value TYPE REF TO data.

    METHODS new_properties          RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_sheets              RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_namedranges         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_developermetadata   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.

CLASS ycl_a2g_json_spreadsheet IMPLEMENTATION.

  METHOD set_spreadsheeturl.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_spreadsheet-spreadsheet_url <> <fs_value>.
      me->gs_spreadsheet-spreadsheet_url = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_spreadsheetid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_spreadsheet-spreadsheet_id <> <fs_value>.
      me->gs_spreadsheet-spreadsheet_id = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid  . me->set_spreadsheetid( i_value ).
      WHEN gc_fnam_spreadsheeturl  . me->set_spreadsheeturl( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid   . return = REF #( me->gs_spreadsheet-spreadsheet_id ).
      WHEN gc_fnam_spreadsheeturl  . return = REF #( me->gs_spreadsheet-spreadsheet_url ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

    LOOP AT me->gs_spreadsheet-sheets INTO DATA(ls_sheet).
      DATA(lif_a2g_json_sheet) = me->new_sheets(  ).
      lif_a2g_json_sheet->yif_a2g_context~write_data( REF #( ls_sheet ) ).
    ENDLOOP.

    LOOP AT me->gs_spreadsheet-named_ranges INTO DATA(ls_namedrange).
      DATA(lif_a2g_json_namedrange) = me->new_namedranges(  ).
      lif_a2g_json_namedrange->yif_a2g_context~write_data( REF #( ls_namedrange ) ).
    ENDLOOP.

    LOOP AT me->gs_spreadsheet-developer_metadata INTO DATA(ls_metadata).
      DATA(lif_a2g_json_metadata) = me->new_developermetadata(  ).
      lif_a2g_json_metadata->yif_a2g_context~write_data( REF #( ls_metadata ) ).
    ENDLOOP.

    DATA(lif_a2g_json_prop) = me->new_properties(  ).
    lif_a2g_json_prop->yif_a2g_context~write_data( REF #( me->gs_spreadsheet-properties ) ).

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_properties.
        me->gs_spreadsheet-properties  =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_sheets.
        APPEND <fs_value_range> TO me->gs_spreadsheet-sheets.
      ELSEIF lv_name CS me->gc_fnam_namedranges.
        APPEND <fs_value_range> TO me->gs_spreadsheet-named_ranges.
      ELSEIF lv_name CS me->gc_fnam_devmetadata.
        APPEND <fs_value_range> TO me->gs_spreadsheet-developer_metadata.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_spreadsheet  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_properties  . return = me->new_properties( ).
      WHEN gc_fnam_sheets        . return = me->new_sheets( ).
      WHEN gc_fnam_namedranges        . return = me->new_namedranges( ).
      WHEN gc_fnam_devmetadata        . return = me->new_developermetadata( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_properties. return ?= me->go_json_array->getinstance( gc_fnam_properties ).
      WHEN gc_fnam_sheets.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_sheets ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      WHEN gc_fnam_namedranges .
        lt_names =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_namedranges ).
        TRY.
            lv_name = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      WHEN gc_fnam_devmetadata .
        lt_names =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_devmetadata ).
        TRY.
            lv_name = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD new_properties.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_SPREADSHEET_PROP' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_properties
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_sheets.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_SHEETS' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_sheets ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_sheets && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_developermetadata.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DEVELOPERMETADATA' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_devmetadata ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_devmetadata && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_namedranges.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_NAMEDRANGE' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_namedranges ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_namedranges && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.



ENDCLASS.
