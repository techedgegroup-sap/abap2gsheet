CLASS ycl_a2g_json_append_response DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_jsonbase
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_json_appendresponse,
             spreadsheetid TYPE string,
             tablerange    TYPE string,
             updates       TYPE ycl_a2g_json_upd_val_response=>ty_s_json_updatevaluesresponse,
           END OF ty_s_json_appendresponse.
    CONSTANTS: gc_fnam_spreadsheetid  TYPE string VALUE 'SPREADSHEETID'.
    CONSTANTS: gc_fnam_tablerange     TYPE string VALUE 'TABLERANGE'.
    CONSTANTS: gc_fnam_updates        TYPE string VALUE 'UPDATES'.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.


  PROTECTED SECTION.
    DATA: gs_json_appendresp TYPE ty_s_json_appendresponse.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data REDEFINITION.

    METHODS set_spreadsheetid   IMPORTING !i_value TYPE REF TO data.
    METHODS set_tablerange      IMPORTING !i_value TYPE REF TO data.
    METHODS new_updates         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_append_response IMPLEMENTATION.
  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_json_appendresp ).
  ENDMETHOD.

  METHOD push_data.

    DATA(lif_a2g_json) = me->new_updates(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_json_appendresp-updates ) ).

  ENDMETHOD.


  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_updates ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_json_appendresp-updates = <fs_value_range>.

  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE i_name .
      WHEN gc_fnam_spreadsheetid  . me->set_spreadsheetid( i_value  ).
      WHEN gc_fnam_tablerange     . me->set_tablerange( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE i_name .
      WHEN gc_fnam_spreadsheetid  . return = REF #( me->gs_json_appendresp-spreadsheetid ).
      WHEN gc_fnam_tablerange     . return = REF #( me->gs_json_appendresp-tablerange ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_updates. return = me->new_updates( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_updates.
        return ?= me->go_json_array->getinstance( me->gc_fnam_updates ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_spreadsheetid.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_appendresp-spreadsheetid <> <fs_value>.
      me->gs_json_appendresp-spreadsheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_tablerange.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_appendresp-tablerange <> <fs_value>.
      me->gs_json_appendresp-tablerange = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD new_updates.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_UPD_VAL_RESPONSE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_updates
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD generate_rules.
  ENDMETHOD.


ENDCLASS.
