CLASS ycl_a2g_json_cellformat DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: gc_fnam_numberformat         TYPE string VALUE 'NUMBERFORMAT'.
    CONSTANTS: gc_fnam_backgroundcolor      TYPE string VALUE 'BACKGROUNDCOLOR'.
    CONSTANTS: gc_fnam_borders              TYPE string VALUE 'BORDERS'.
    CONSTANTS: gc_fnam_padding              TYPE string VALUE 'PADDING'.
    CONSTANTS: gc_fnam_textformat           TYPE string VALUE 'TEXTFORMAT'.
    CONSTANTS: gc_fnam_textrotation         TYPE string VALUE 'TEXTROTATION'.

    CONSTANTS: gc_fnam_horizontalalignment  TYPE string VALUE 'HORIZONTALALIGNMENT'.
    CONSTANTS: gc_fnam_verticalalignment    TYPE string VALUE 'VERTICALALIGNMENT'.
    CONSTANTS: gc_fnam_wrapstrategy         TYPE string VALUE 'WRAPSTRATEGY'.
    CONSTANTS: gc_fnam_textdirection        TYPE string VALUE 'TEXTDIRECTION'.
    CONSTANTS: gc_fnam_hyperlinkdisplaytype TYPE string VALUE 'HYPERLINKDISPLAYTYPE'.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.

    TYPES: BEGIN OF ty_s_json_cellformat,
             numberformat         TYPE ycl_a2g_json_numberformat=>ty_s_json_numberformat,
             background_color     TYPE ycl_a2g_json_color=>ty_s_json_color,
             borders              TYPE ycl_a2g_json_borders=>ty_s_json_borders,
             padding              TYPE ycl_a2g_json_padding=>ty_s_json_padding,
             horizontal_alignment  TYPE string,
             verticalalignment    TYPE string,
             wrapstrategy         TYPE string,
             textdirection        TYPE string,
             text_format           TYPE ycl_a2g_json_textformat=>ty_s_json_textformat,
             hyperlinkdisplaytype TYPE string,
             textrotation         TYPE ycl_a2g_json_textrotation=>ty_s_json_textrotation,
           END OF ty_s_json_cellformat.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_cellformat  TYPE ty_s_json_cellformat.

    METHODS set_horizontalalignment   IMPORTING !i_value TYPE REF TO data.
    METHODS set_verticalalignment     IMPORTING !i_value TYPE REF TO data.
    METHODS set_wrapstrategy          IMPORTING !i_value TYPE REF TO data.
    METHODS set_textdirection         IMPORTING !i_value TYPE REF TO data.
    METHODS set_hyperlinkdisplaytype  IMPORTING !i_value TYPE REF TO data.

    METHODS new_numberformat    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_backgroundcolor RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_borders         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_padding         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_textformat      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_textrotation    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_cellformat IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.

    DATA(lif_color) = me->yif_a2g_json~new_element( gc_fnam_backgroundcolor ).
    lif_color->set_default( ).

    DATA(lif_padding) = me->yif_a2g_json~new_element( gc_fnam_padding ).
    lif_padding->set_default( ).

    DATA(lif_text) = me->yif_a2g_json~new_element( gc_fnam_textformat ).
    lif_text->set_default( ).

    DATA(lif_border) = me->yif_a2g_json~new_element( gc_fnam_borders ).
    lif_border->set_default( ).


    DATA: lv_string TYPE string.
    lv_string = 'BOTTOM'.
    me->set_verticalalignment( REF #( lv_string ) ).
    lv_string = 'OVERFLOW_CELL'.
    me->set_wrapstrategy( REF #( lv_string )  ).

  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_horizontalalignment   . me->set_horizontalalignment( i_value ).
      WHEN gc_fnam_verticalalignment     . me->set_verticalalignment( i_value ).
      WHEN gc_fnam_wrapstrategy          . me->set_wrapstrategy( i_value ).
      WHEN gc_fnam_textdirection         . me->set_textdirection( i_value ).
      WHEN gc_fnam_hyperlinkdisplaytype  . me->set_hyperlinkdisplaytype( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_horizontalalignment  . return = REF #( me->gs_cellformat-horizontal_alignment ).
      WHEN gc_fnam_verticalalignment    . return = REF #( me->gs_cellformat-verticalalignment    ).
      WHEN gc_fnam_wrapstrategy         . return = REF #( me->gs_cellformat-wrapstrategy        ).
      WHEN gc_fnam_textdirection        . return = REF #( me->gs_cellformat-textdirection       ).
      WHEN gc_fnam_hyperlinkdisplaytype . return = REF #( me->gs_cellformat-hyperlinkdisplaytype ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_horizontalalignment.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_cellformat-horizontal_alignment <> <fs_value>.
      me->gs_cellformat-horizontal_alignment = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_verticalalignment.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_cellformat-verticalalignment <> <fs_value>.
      me->gs_cellformat-verticalalignment = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_wrapstrategy.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_cellformat-wrapstrategy <> <fs_value>.
      me->gs_cellformat-wrapstrategy = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_textdirection.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_cellformat-textdirection <> <fs_value>.
      me->gs_cellformat-textdirection = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_hyperlinkdisplaytype.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_cellformat-hyperlinkdisplaytype <> <fs_value>.
      me->gs_cellformat-hyperlinkdisplaytype = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD push_data.

    DATA(lif_a2g_json) = me->new_numberformat( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_cellformat-numberformat ) ).

    lif_a2g_json = me->new_backgroundcolor( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_cellformat-background_color ) ).

    lif_a2g_json = me->new_borders( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_cellformat-borders ) ).

    lif_a2g_json = me->new_padding( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_cellformat-padding ) ).

    lif_a2g_json = me->new_textformat( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_cellformat-text_format ) ).

    lif_a2g_json = me->new_textrotation( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_cellformat-textrotation  ) ).

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.


      IF lv_name CS me->gc_fnam_numberformat        .
        me->gs_cellformat-numberformat    =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_backgroundcolor .
        me->gs_cellformat-background_color =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_borders         .
        me->gs_cellformat-borders         =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_padding         .
        me->gs_cellformat-padding         =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_textformat      .
        me->gs_cellformat-text_format      =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_textrotation    .
        me->gs_cellformat-textrotation    =  <fs_value_range>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_cellformat  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN me->gc_fnam_numberformat     . return = me->new_numberformat( ).
      WHEN me->gc_fnam_backgroundcolor  . return = me->new_backgroundcolor( ).
      WHEN me->gc_fnam_borders          . return = me->new_borders( ).
      WHEN me->gc_fnam_padding          . return = me->new_padding( ).
      WHEN me->gc_fnam_textformat       . return = me->new_textformat( ).
      WHEN me->gc_fnam_textrotation     . return = me->new_textrotation( ).
    ENDCASE.

  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN me->gc_fnam_numberformat     . return ?= me->go_json_array->getinstance( me->gc_fnam_numberformat    ).
      WHEN me->gc_fnam_backgroundcolor  . return ?= me->go_json_array->getinstance( me->gc_fnam_backgroundcolor ).
      WHEN me->gc_fnam_borders          . return ?= me->go_json_array->getinstance( me->gc_fnam_borders         ).
      WHEN me->gc_fnam_padding          . return ?= me->go_json_array->getinstance( me->gc_fnam_padding         ).
      WHEN me->gc_fnam_textformat       . return ?= me->go_json_array->getinstance( me->gc_fnam_textformat      ).
      WHEN me->gc_fnam_textrotation     . return ?= me->go_json_array->getinstance( me->gc_fnam_textrotation    ).
    ENDCASE.

  ENDMETHOD.


  METHOD new_textformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_textformat
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_textrotation.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTROTATION' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_textrotation
                                     im_object = lo_object ).
  ENDMETHOD.



  METHOD new_numberformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_NUMBERFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_numberformat
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_backgroundcolor.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_backgroundcolor
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_borders.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BORDERS' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_borders
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_padding.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_PADDING' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_padding
                                     im_object = lo_object ).
  ENDMETHOD.



ENDCLASS.
