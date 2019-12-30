CLASS ycl_a2g_json_batchupdate_res DEFINITION
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
    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.

    TYPES: BEGIN OF ty_s_json_batchupdate_res,
             spreadsheetid       TYPE string,
             totalupdatedrows    TYPE i,
             totalupdatedcolumns TYPE i,
             totalupdatedcells   TYPE i,
             totalupdatedsheets  TYPE i,
             responses           TYPE ycl_a2g_json_upd_val_response=>ty_t_json_updatevaluesresponse,
           END OF ty_s_json_batchupdate_res.

    CONSTANTS: gc_fnam_spreadsheetid       TYPE string VALUE 'SPREADSHEETID'.
    CONSTANTS: gc_fnam_totalupdatedrows    TYPE string VALUE 'TOTALUPDATEDROWS'.
    CONSTANTS: gc_fnam_totalupdatedcolumns TYPE string VALUE 'TOTALUPDATEDCOLUMNS'.
    CONSTANTS: gc_fnam_totalupdatedcells   TYPE string VALUE 'TOTALUPDATEDCELLS'.
    CONSTANTS: gc_fnam_totalupdatedsheets  TYPE string VALUE 'TOTALUPDATEDSHEETS'.
    CONSTANTS: gc_fnam_responses           TYPE string VALUE 'RESPONSES'.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_batchupdate_res TYPE ty_s_json_batchupdate_res.

    METHODS set_spreadsheetid       IMPORTING !i_value TYPE REF TO data.
    METHODS set_totalupdatedrows    IMPORTING !i_value TYPE REF TO data.
    METHODS set_totalupdatedcolumns IMPORTING !i_value TYPE REF TO data.
    METHODS set_totalupdatedcells   IMPORTING !i_value TYPE REF TO data.
    METHODS set_totalupdatedsheets  IMPORTING !i_value TYPE REF TO data.

    METHODS NEW_responses  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_batchupdate_res IMPLEMENTATION.

  METHOD push_data.

    LOOP AT me->gs_batchupdate_res-responses INTO DATA(ls_response).
      DATA(lif_a2g_json_grid) = me->NEW_responses(  ).
      lif_a2g_json_grid->yif_a2g_context~write_data( REF #( ls_response ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.


    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_responses ).
    LOOP AT lt_names INTO DATA(lv_name).
      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      APPEND <fs_value_range> TO me->gs_batchupdate_res-responses.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_batchupdate_res ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_responses  . return = me->NEW_responses( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_responses.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_responses ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


  METHOD NEW_responses .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_UPD_VAL_RESPONSE' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_responses ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_responses && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD set_spreadsheetid.

    FIELD-SYMBOLS <fs_value> TYPE STRING.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_res-spreadsheetid <> <fs_value>.
      me->gs_batchupdate_res-spreadsheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_totalupdatedrows.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_res-totalupdatedrows <> <fs_value>.
      me->gs_batchupdate_res-totalupdatedrows = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_totalupdatedcolumns.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_res-totalupdatedcolumns <> <fs_value>.
      me->gs_batchupdate_res-totalupdatedcolumns = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_totalupdatedcells .

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_res-totalupdatedcells <> <fs_value>.
      me->gs_batchupdate_res-totalupdatedcells = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_totalupdatedsheets .

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_res-totalupdatedsheets <> <fs_value>.
      me->gs_batchupdate_res-totalupdatedsheets = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid     . me->set_spreadsheetid( i_value ).
      WHEN gc_fnam_totalupdatedrows   . me->set_totalupdatedrows( i_value ).
      WHEN gc_fnam_totalupdatedcells . me->set_totalupdatedcolumns( i_value ).
      WHEN gc_fnam_totalupdatedcells . me->set_totalupdatedcells( i_value ).
      WHEN gc_fnam_totalupdatedsheets . me->set_totalupdatedsheets( i_value ).

    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.

    CASE  i_name.
      WHEN gc_fnam_spreadsheetid     . return = REF #( me->gs_batchupdate_res-spreadsheetid ).
      WHEN gc_fnam_totalupdatedrows   . return = REF #( me->gs_batchupdate_res-totalupdatedrows ).
      WHEN gc_fnam_totalupdatedcolumns . return = REF #( me->gs_batchupdate_res-totalupdatedcolumns ).
      WHEN gc_fnam_totalupdatedcells . return = REF #( me->gs_batchupdate_res-totalupdatedcells ).
      WHEN gc_fnam_totalupdatedsheets . return = REF #( me->gs_batchupdate_res-totalupdatedsheets ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
