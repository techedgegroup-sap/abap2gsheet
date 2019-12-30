CLASS ycl_a2g_json_developermetadata DEFINITION
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


    TYPES: BEGIN OF ty_s_json_devrmeta,
             metadataid    TYPE i,
             metadatakey   TYPE string,
             metadatavalue TYPE string,
             location      TYPE ycl_a2g_json_devrmetaloc=>ty_s_json_devrmetaloc,
             visibility    TYPE string,
           END OF ty_s_json_devrmeta.

    types ty_t_json_devrmeta type STANDARD TABLE OF ty_s_json_devrmeta WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_metadataid    TYPE string VALUE 'METADATAID'.
    CONSTANTS: gc_fnam_metadatakey   TYPE string VALUE 'METADATAKEY'.
    CONSTANTS: gc_fnam_metadatavalue TYPE string VALUE 'METADATAVALUE'.
    CONSTANTS: gc_fnam_location      TYPE string VALUE 'LOCATION'.
    CONSTANTS: gc_fnam_visibility    TYPE string VALUE 'VISIBILITY'.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_devrmeta TYPE ty_s_json_devrmeta.

    METHODS set_metadataid     IMPORTING !i_value TYPE REF TO data.
    METHODS set_metadatakey    IMPORTING !i_value TYPE REF TO data.
    METHODS set_metadatavalue  IMPORTING !i_value TYPE REF TO data.
    METHODS set_visibility     IMPORTING !i_value TYPE REF TO data.

    METHODS new_location RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_developermetadata IMPLEMENTATION.

  METHOD set_metadataid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmeta-metadataid <> <fs_value>.
      me->gs_devrmeta-metadataid = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_metadatakey .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmeta-metadatakey <> <fs_value>.
      me->gs_devrmeta-metadatakey = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_metadatavalue.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmeta-metadatavalue <> <fs_value>.
      me->gs_devrmeta-metadatavalue = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_visibility.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_devrmeta-visibility <> <fs_value>.
      me->gs_devrmeta-visibility = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_metadataid      . me->set_metadataid( i_value ).
      WHEN gc_fnam_metadatakey     . me->set_metadatakey( i_value ).
      WHEN gc_fnam_metadatavalue   . me->set_metadatavalue( i_value ).
      WHEN gc_fnam_visibility      . me->set_visibility( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_metadataid       . return = REF #( me->gs_devrmeta-metadataid ).
      WHEN gc_fnam_metadatakey      . return = REF #( me->gs_devrmeta-metadatakey ).
      WHEN gc_fnam_metadatavalue    . return = REF #( me->gs_devrmeta-metadatavalue ).
      WHEN gc_fnam_visibility       . return = REF #( me->gs_devrmeta-visibility ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
    DATA(lif_a2g_json) = me->new_location(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_devrmeta-location ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    lif_a2g_json ?= me->go_json_array->getinstance( me->gc_fnam_location ).
    DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
    ASSIGN dref_data->* TO <fs_value_range>.
    me->gs_devrmeta-location = <fs_value_range>.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_devrmeta ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_location. return = me->new_location( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_location.
        return ?= me->go_json_array->getinstance( me->gc_fnam_location ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_location.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DEVRMETALOC' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_location
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
