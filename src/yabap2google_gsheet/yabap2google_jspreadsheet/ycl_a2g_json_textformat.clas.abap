CLASS ycl_a2g_json_textformat DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_json_textformat,
             foreground_color TYPE ycl_a2g_json_color=>ty_s_json_color,
             font_family      TYPE string,
             font_size        TYPE i,
             bold            TYPE string,
             italic          TYPE string,
             strikethrough   TYPE string,
             underline       TYPE string,
           END OF ty_s_json_textformat.

    CONSTANTS gc_fnam_foregroundcolor  TYPE string VALUE 'FOREGROUNDCOLOR'.
    CONSTANTS gc_fnam_fontfamily       TYPE string VALUE 'FONTFAMILY'.
    CONSTANTS gc_fnam_fontsize         TYPE string VALUE 'FONTSIZE'.
    CONSTANTS gc_fnam_bold             TYPE string VALUE 'BOLD'.
    CONSTANTS gc_fnam_italic           TYPE string VALUE 'ITALIC'.
    CONSTANTS gc_fnam_strikethrough    TYPE string VALUE 'STRIKETHROUGH'.
    CONSTANTS gc_fnam_underline        TYPE string VALUE 'UNDERLINE'.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_textformat  TYPE ty_s_json_textformat.

    METHODS set_fontfamily        IMPORTING !i_value TYPE REF TO data.
    METHODS set_fontsize          IMPORTING !i_value TYPE REF TO data.
    METHODS set_bold              IMPORTING !i_value TYPE REF TO data.
    METHODS set_italic            IMPORTING !i_value TYPE REF TO data.
    METHODS set_strikethrough     IMPORTING !i_value TYPE REF TO data.
    METHODS set_underline         IMPORTING !i_value TYPE REF TO data.

    METHODS new_foregroundcolor   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_textformat IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.
    DATA: lv_string TYPE string.
    DATA: lv_val TYPE i.

    lv_val = 10.
    me->set_fontsize( REF #( lv_val )  ).

    lv_string = 'arial,sans,sans-serif'.
    me->set_fontfamily( REF #( lv_string )  ).

    lv_string = 'false'.
    me->set_bold( REF #( lv_string )  ).
    me->set_italic( REF #( lv_string )  ).
    me->set_strikethrough( REF #( lv_string )  ).
    me->set_underline( REF #( lv_string )  ).


  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_foregroundcolor  . return = me->new_foregroundcolor( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_foregroundcolor. return ?= me->go_json_array->getinstance( gc_fnam_foregroundcolor ).
    ENDCASE.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_fontfamily     . me->set_fontfamily( i_value ).
      WHEN gc_fnam_fontsize       . me->set_fontsize( i_value ).
      WHEN gc_fnam_bold           . me->set_bold( i_value ).
      WHEN gc_fnam_italic         . me->set_italic( i_value ).
      WHEN gc_fnam_strikethrough  . me->set_strikethrough( i_value ).
      WHEN gc_fnam_underline      . me->set_underline( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_fontfamily      . return = REF #( me->gs_textformat-font_family    ).
      WHEN gc_fnam_fontsize        . return = REF #( me->gs_textformat-font_size      ).
      WHEN gc_fnam_bold            . return = REF #( me->gs_textformat-bold          ).
      WHEN gc_fnam_italic          . return = REF #( me->gs_textformat-italic        ).
      WHEN gc_fnam_strikethrough   . return = REF #( me->gs_textformat-strikethrough ).
      WHEN gc_fnam_underline       . return = REF #( me->gs_textformat-underline     ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_foregroundcolor.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_foregroundcolor ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_foregroundcolor && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD set_strikethrough.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_textformat-strikethrough <> <fs_value>.
      me->gs_textformat-strikethrough = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_italic.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_textformat-italic <> <fs_value>.
      me->gs_textformat-italic = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_bold.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_textformat-bold <> <fs_value>.
      me->gs_textformat-bold = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_fontsize.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_textformat-font_size <> <fs_value>.
      me->gs_textformat-font_size = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_fontfamily.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_textformat-font_family <> <fs_value>.
      me->gs_textformat-font_family = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_underline.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_textformat-underline <> <fs_value>.
      me->gs_textformat-underline = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD push_data.
    DATA(lif_a2g_json) = me->new_foregroundcolor(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_textformat-foreground_color ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_foregroundcolor.
        me->gs_textformat-foreground_color  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_textformat  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.


