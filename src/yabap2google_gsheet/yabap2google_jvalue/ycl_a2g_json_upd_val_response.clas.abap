CLASS ycl_a2g_json_upd_val_response DEFINITION
  PUBLIC
INHERITING FROM ycl_a2g_jsonbase
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_json_updatevaluesresponse,
             spreadsheetid  TYPE string,
             updatedrange   TYPE string,
             updatedrows    TYPE i,
             updatedcolumns TYPE i,
             updatedcells   TYPE i,
             updateddata    TYPE ycl_a2g_json_valuerange=>ty_s_json_value_range,
           END OF ty_s_json_updatevaluesresponse.
    TYPES: ty_t_json_updatevaluesresponse TYPE STANDARD TABLE OF ty_s_json_updatevaluesresponse WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_spreadsheetid  TYPE string VALUE 'SPREADSHEETID'.
    CONSTANTS: gc_fnam_updatedrange   TYPE string VALUE 'UPDATEDRANGE'.
    CONSTANTS: gc_fnam_updatedrows    TYPE string VALUE 'UDATEDROWS'.
    CONSTANTS: gc_fnam_updatedcolumns TYPE string VALUE 'UPDATEDCOLUMNS'.
    CONSTANTS: gc_fnam_updatedcells   TYPE string VALUE 'UPDATEDCELLS'.
    CONSTANTS: gc_fnam_updateddata    TYPE string VALUE 'UPDATEDDATA'.


    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.


    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.


  PROTECTED SECTION.
    DATA: gs_json_updval_res TYPE ty_s_json_updatevaluesresponse.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data REDEFINITION.

    METHODS set_spreadsheetid   IMPORTING !i_value TYPE REF TO data.
    METHODS set_updatedrange    IMPORTING !i_value TYPE REF TO data.
    METHODS set_updatedcells    IMPORTING !i_value TYPE REF TO data.
    METHODS set_updatedcolumns  IMPORTING !i_value TYPE REF TO data.
    METHODS set_updatedrows     IMPORTING !i_value TYPE REF TO data.

    METHODS new_updateddata RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_a2g_json_upd_val_response IMPLEMENTATION.
  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_json_updval_res ).
  ENDMETHOD.

  METHOD push_data.

    DATA(lif_a2g_json) = me->new_updateddata(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_json_updval_res-updateddata ) ).

  ENDMETHOD.



  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_updateddata ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_json_updval_res-updateddata = <fs_value_range>.

  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE i_name .
      WHEN gc_fnam_spreadsheetid  . me->set_spreadsheetid( i_value  ).
      WHEN gc_fnam_updatedrange   . me->set_updatedrange( i_value  ).
      WHEN gc_fnam_updatedrows    . me->set_updatedrows( i_value  ).
      WHEN gc_fnam_updatedcolumns . me->set_updatedcolumns( i_value  ).
      WHEN gc_fnam_updatedcells   . me->set_updatedcells( i_value  ).
*    WHEN gc_fnam_updateddata    .TYPE string VALUE 'UPDATEDDATA'.
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE i_name .
      WHEN gc_fnam_spreadsheetid  . return = REF #( me->gs_json_updval_res-spreadsheetid ).
      WHEN gc_fnam_updatedrange   . return = REF #( me->gs_json_updval_res-updatedrange ).
      WHEN gc_fnam_updatedrows    . return = REF #( me->gs_json_updval_res-updatedrows ).
      WHEN gc_fnam_updatedcolumns . return = REF #( me->gs_json_updval_res-updatedcolumns ).
      WHEN gc_fnam_updatedcells   . return = REF #( me->gs_json_updval_res-updatedcells ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_updateddata. return = me->new_updateddata( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_updateddata.
        return ?= me->go_json_array->getinstance( me->gc_fnam_updateddata ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_spreadsheetid.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_updval_res-spreadsheetid <> <fs_value>.
      me->gs_json_updval_res-spreadsheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_updatedrange.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_updval_res-updatedrange <> <fs_value>.
      me->gs_json_updval_res-updatedrange = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_updatedcells.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_updval_res-updatedcells <> <fs_value>.
      me->gs_json_updval_res-updatedcells = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_updatedcolumns.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_updval_res-updatedcolumns <> <fs_value>.
      me->gs_json_updval_res-updatedcolumns = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_updatedrows.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_json_updval_res-updatedrows <> <fs_value>.
      me->gs_json_updval_res-updatedrows = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD new_updateddata.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_VALUERANGE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_updateddata
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.






