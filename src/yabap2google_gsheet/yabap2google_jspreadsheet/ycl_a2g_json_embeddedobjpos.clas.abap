CLASS ycl_a2g_json_embeddedobjpos DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_embeddedobjpos,
             sheetid         TYPE i,
             overlay_position TYPE ycl_a2g_json_overlayposition=>ty_s_json_overlayposition,
             newsheet        TYPE string,
           END OF ty_s_json_embeddedobjpos.
    TYPES ty_t_json_embeddedobjpos TYPE STANDARD TABLE OF ty_s_json_embeddedobjpos WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_sheetid          TYPE string VALUE 'SHEETID'.
    CONSTANTS: gc_fnam_overlayposition  TYPE string VALUE 'OVERLAYPOSITION'.
    CONSTANTS: gc_fnam_newsheet         TYPE string VALUE 'NEWSHEET'.


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

    DATA: gs_embeddedobjpos  TYPE ty_s_json_embeddedobjpos.

    METHODS set_sheetid          IMPORTING !i_value TYPE REF TO data.
    METHODS set_newsheet         IMPORTING !i_value TYPE REF TO data.

    METHODS new_overlayposition  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_embeddedobjpos IMPLEMENTATION.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid  . me->set_sheetid( i_value ).
      WHEN gc_fnam_newsheet  . me->set_newsheet( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_sheetid   . return = REF #( me->gs_embeddedobjpos-sheetid ).
      WHEN gc_fnam_newsheet  . return = REF #( me->gs_embeddedobjpos-newsheet ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_newsheet.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_embeddedobjpos-newsheet <> <fs_value>.
      me->gs_embeddedobjpos-newsheet = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_sheetid.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_embeddedobjpos-sheetid <> <fs_value>.
      me->gs_embeddedobjpos-sheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json_prop) = me->new_overlayposition(  ).
    lif_a2g_json_prop->yif_a2g_context~write_data( REF #( me->gs_embeddedobjpos-overlay_position ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_overlayposition.
        me->gs_embeddedobjpos-overlay_position  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_embeddedobjpos  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_overlayposition  . return = me->new_overlayposition( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_overlayposition. return ?= me->go_json_array->getinstance( gc_fnam_overlayposition ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_overlayposition.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_OVERLAYPOSITION' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_overlayposition
                                     im_object = lo_object ).
  ENDMETHOD.



ENDCLASS.
