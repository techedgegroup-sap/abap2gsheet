CLASS ycl_a2g_json_celldata DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_celldata,
             user_entered_value  TYPE ycl_a2g_json_extendedvalue=>ty_s_json_extendedvalue,
             effectivevalue    TYPE ycl_a2g_json_extendedvalue=>ty_s_json_extendedvalue,
             formattedvalue    TYPE string,
             user_entered_format TYPE ycl_a2g_json_cellformat=>ty_s_json_cellformat,
             effectiveformat   TYPE ycl_a2g_json_cellformat=>ty_s_json_cellformat,
             hyperlink         TYPE string,
             note              TYPE string,
             text_formatruns    TYPE ycl_a2g_json_textformatrun=>ty_t_json_textformatrun,
             datavalidation    TYPE ycl_a2g_json_datavalidrule=>ty_s_json_datavalidrule,
             pivottable        TYPE ycl_a2g_json_pivottable=>ty_s_json_pivottable,
           END OF ty_s_json_celldata.
    TYPES ty_t_json_celldata TYPE STANDARD TABLE OF ty_s_json_celldata WITH NON-UNIQUE KEY user_entered_value .

    CONSTANTS: gc_fnam_userenteredvalue    TYPE string VALUE 'user_entered_value'.
    CONSTANTS: gc_fnam_effectivevalue      TYPE string VALUE 'EFFECTIVEVALUE'.
    CONSTANTS: gc_fnam_formattedvalue      TYPE string VALUE 'FORMATTEDVALUE'.
    CONSTANTS: gc_fnam_userenteredformat   TYPE string VALUE 'USERENTEREDFORMAT'.
    CONSTANTS: gc_fnam_effectiveformat     TYPE string VALUE 'EFFECTIVEFORMAT'.
    CONSTANTS: gc_fnam_hyperlink           TYPE string VALUE 'HYPERLINK'.
    CONSTANTS: gc_fnam_note                TYPE string VALUE 'NOTE'.
    CONSTANTS: gc_fnam_textformatruns      TYPE string VALUE 'TEXTFORMATRUNS'.
    CONSTANTS: gc_fnam_datavalidation      TYPE string VALUE 'DATAVALIDATION'.
    CONSTANTS: gc_fnam_pivottable          TYPE string VALUE 'PIVOTTABLE'.


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

    DATA: gs_celldata  TYPE ty_s_json_celldata.

    METHODS set_hyperlink        IMPORTING !i_value TYPE REF TO data.
    METHODS set_note             IMPORTING !i_value TYPE REF TO data.
    METHODS set_formattedvalue   IMPORTING !i_value TYPE REF TO data.

    METHODS new_user_entered_value   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_effectivevalue     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_userenteredformat  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_effectiveformat    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_textformatruns     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_datavalidation     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_pivottable         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_celldata IMPLEMENTATION.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_hyperlink       . me->set_hyperlink( i_value ).
      WHEN gc_fnam_note            . me->set_note( i_value ).
      WHEN gc_fnam_formattedvalue  . me->set_formattedvalue( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_hyperlink       . return = REF #( me->gs_celldata-hyperlink      ).
      WHEN gc_fnam_note            . return = REF #( me->gs_celldata-note           ).
      WHEN gc_fnam_formattedvalue  . return = REF #( me->gs_celldata-formattedvalue ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_note.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_celldata-note <> <fs_value>.
      me->gs_celldata-note = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_hyperlink.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_celldata-hyperlink <> <fs_value>.
      me->gs_celldata-hyperlink = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_formattedvalue.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_celldata-formattedvalue <> <fs_value>.
      me->gs_celldata-formattedvalue = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_user_entered_value(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_celldata-user_entered_value ) ).

    lif_a2g_json = me->new_effectivevalue(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_celldata-effectivevalue ) ).

    lif_a2g_json = me->new_userenteredformat(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_celldata-user_entered_format ) ).

    lif_a2g_json = me->new_effectiveformat(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_celldata-effectiveformat ) ).

    lif_a2g_json = me->new_datavalidation(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_celldata-datavalidation ) ).

    lif_a2g_json = me->new_pivottable(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_celldata-pivottable ) ).

    LOOP AT me->gs_celldata-text_formatruns INTO DATA(ls_textformatruns).
      DATA(lif_a2g_json_namedrange) = me->new_textformatruns(  ).
      lif_a2g_json_namedrange->yif_a2g_context~write_data( REF #( ls_textformatruns ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.
clear  me->gs_celldata-text_formatruns.
    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS     me->gc_fnam_userenteredvalue .
        me->gs_celldata-user_entered_value   =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_effectivevalue   .
        me->gs_celldata-effectivevalue     =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_userenteredformat.
        me->gs_celldata-user_entered_format  =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_effectiveformat  .
        me->gs_celldata-effectiveformat    =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_textformatruns   .
        APPEND <fs_value_range> TO me->gs_celldata-text_formatruns.
      ELSEIF lv_name CS me->gc_fnam_datavalidation   .
        me->gs_celldata-datavalidation     =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_pivottable       .
        me->gs_celldata-pivottable         =  <fs_value_range>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_celldata  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_userenteredvalue  . return = me->new_user_entered_value( ).
      WHEN gc_fnam_effectivevalue    . return = me->new_effectivevalue( ).
      WHEN gc_fnam_userenteredformat . return = me->new_userenteredformat( ).
      WHEN gc_fnam_effectiveformat   . return = me->new_effectiveformat( ).
      WHEN gc_fnam_textformatruns    . return = me->new_textformatruns( ).
      WHEN gc_fnam_datavalidation    . return = me->new_datavalidation( ).
      WHEN gc_fnam_pivottable        . return = me->new_pivottable( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_userenteredvalue. return ?= me->go_json_array->getinstance( gc_fnam_userenteredvalue ).
      WHEN gc_fnam_effectivevalue  . return ?= me->go_json_array->getinstance( gc_fnam_effectivevalue   ).
      WHEN gc_fnam_userenteredformat. return ?= me->go_json_array->getinstance( gc_fnam_userenteredformat ).
      WHEN gc_fnam_effectiveformat . return ?= me->go_json_array->getinstance( gc_fnam_effectiveformat  ).
      WHEN gc_fnam_textformatruns  .
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_textformatruns ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      WHEN gc_fnam_datavalidation  . return ?= me->go_json_array->getinstance( gc_fnam_datavalidation   ).
      WHEN gc_fnam_pivottable      . return ?= me->go_json_array->getinstance( gc_fnam_pivottable       ).

    ENDCASE.
  ENDMETHOD.

  METHOD new_textformatruns.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTFORMATRUN' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_textformatruns ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_textformatruns && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_datavalidation .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DATAVALIDRULE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_datavalidation
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_pivottable .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_PIVOTTABLE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_pivottable
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_effectiveformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CELLFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_effectiveformat
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_userenteredformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CELLFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_userenteredformat
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_effectivevalue.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_EXTENDEDVALUE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_effectivevalue
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_user_entered_value.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_EXTENDEDVALUE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_userenteredvalue
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
