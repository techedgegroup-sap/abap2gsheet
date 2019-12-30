CLASS ycl_a2g_json_chartspec DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES: BEGIN OF ty_s_json_chartspec,
             title                   TYPE string,
             alttext                 TYPE string,
             titletextformat         TYPE ycl_a2g_json_textformat=>ty_s_json_textformat,
             titletextposition       TYPE ycl_a2g_json_textpos=>ty_s_json_textpos,
             subtitle                TYPE string,
             subtitletextformat      TYPE ycl_a2g_json_textformat=>ty_s_json_textformat,
             subtitletextposition    TYPE ycl_a2g_json_textpos=>ty_s_json_textpos,
             fontname                TYPE string,
             maximized               TYPE string,
             backgroundcolor         TYPE ycl_a2g_json_color=>ty_s_json_color,
             hiddendimensionstrategy TYPE string,
             basic_chart             TYPE ycl_a2g_json_basicchartspec=>ty_s_json_basicchartspec,
             piechart                TYPE ycl_a2g_json_piechart=>ty_s_json_piechart,
             bubblechart             TYPE ycl_a2g_json_bubblechart=>ty_s_json_bubblechart,
             candlestickchart        TYPE ycl_a2g_json_candlestickchart=>ty_s_json_candlestickchart,
             orgchart                TYPE ycl_a2g_json_orgchart=>ty_s_json_orgchart,
             histogramchart          TYPE ycl_a2g_json_histogramchart=>ty_s_json_histogramchart,
             waterfallchart          TYPE ycl_a2g_json_waterfallchart=>ty_s_json_waterfallchart,
             treemapchart            TYPE ycl_a2g_json_treemapchart=>ty_s_json_treemapchart,
           END OF ty_s_json_chartspec.
    TYPES ty_t_json_chartspec TYPE STANDARD TABLE OF ty_s_json_chartspec WITH NON-UNIQUE KEY title.

    CONSTANTS: gc_fnam_title                     TYPE string VALUE 'TITLE'.
    CONSTANTS: gc_fnam_alttext                   TYPE string VALUE 'ALTTEXT'.
    CONSTANTS: gc_fnam_subtitle                  TYPE string VALUE 'SUBTITLE'.
    CONSTANTS: gc_fnam_fontname                  TYPE string VALUE 'FONTNAME'.
    CONSTANTS: gc_fnam_maximized                 TYPE string VALUE 'MAXIMIZED'.
    CONSTANTS: gc_fnam_hiddendimensionstrateg    TYPE string VALUE 'HIDDENDIMENSIONSTRATEG'.


    CONSTANTS: gc_fnam_titletextformat           TYPE string VALUE 'TITLETEXTFORMAT'.
    CONSTANTS: gc_fnam_titletextposition         TYPE string VALUE 'TITLETEXTPOSITION'.
    CONSTANTS: gc_fnam_subtitletextformat        TYPE string VALUE 'SUBTITLETEXTFORMAT'.
    CONSTANTS: gc_fnam_subtitletextposition      TYPE string VALUE 'SUBTITLETEXTPOSITION'.
    CONSTANTS: gc_fnam_backgroundcolor           TYPE string VALUE 'BACKGROUNDCOLOR'.
    CONSTANTS: gc_fnam_basicchart                TYPE string VALUE 'BASICCHART'.
    CONSTANTS: gc_fnam_piechart                  TYPE string VALUE 'PIECHART'.
    CONSTANTS: gc_fnam_bubblechart               TYPE string VALUE 'BUBBLECHART'.
    CONSTANTS: gc_fnam_candlestickchart          TYPE string VALUE 'CANDLESTICKCHART'.
    CONSTANTS: gc_fnam_orgchart                  TYPE string VALUE 'ORGCHART'.
    CONSTANTS: gc_fnam_histogramchart            TYPE string VALUE 'HISTOGRAMCHART'.
    CONSTANTS: gc_fnam_waterfallchart            TYPE string VALUE 'WATERFALLCHART'.
    CONSTANTS: gc_fnam_treemapchart              TYPE string VALUE 'TREEMAPCHART'.


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

    DATA: gs_chartspec  TYPE ty_s_json_chartspec.

    METHODS set_title                  IMPORTING !i_value TYPE REF TO data.
    METHODS set_alttext                IMPORTING !i_value TYPE REF TO data.
    METHODS set_subtitle               IMPORTING !i_value TYPE REF TO data.
    METHODS set_fontname               IMPORTING !i_value TYPE REF TO data.
    METHODS set_maximized              IMPORTING !i_value TYPE REF TO data.
    METHODS set_hiddendimensionstrateg IMPORTING !i_value TYPE REF TO data.

    METHODS new_titletextformat       RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_titletextposition     RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_subtitletextformat    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_subtitletextposition  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_backgroundcolor       RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_basicchart            RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_piechart              RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_bubblechart           RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_candlestickchart      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_orgchart              RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_histogramchart        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_waterfallchart        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_treemapchart          RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_chartspec IMPLEMENTATION.

  METHOD set_hiddendimensionstrateg.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_chartspec-hiddendimensionstrategy <> <fs_value>.
      me->gs_chartspec-hiddendimensionstrategy = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_maximized.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_chartspec-maximized <> <fs_value>.
      me->gs_chartspec-maximized = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_fontname.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_chartspec-fontname <> <fs_value>.
      me->gs_chartspec-fontname = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_subtitle.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_chartspec-subtitle <> <fs_value>.
      me->gs_chartspec-subtitle = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_alttext .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_chartspec-alttext  <> <fs_value>.
      me->gs_chartspec-alttext  = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_title .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_chartspec-title  <> <fs_value>.
      me->gs_chartspec-title  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_title                  . me->set_title( i_value ).
      WHEN gc_fnam_alttext                . me->set_alttext( i_value  ).
      WHEN gc_fnam_subtitle               . me->set_subtitle( i_value  ).
      WHEN gc_fnam_fontname               . me->set_fontname( i_value  ).
      WHEN gc_fnam_maximized              . me->set_maximized( i_value  ).
      WHEN gc_fnam_hiddendimensionstrateg . me->set_hiddendimensionstrateg( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_title                   . return = REF #( me->gs_chartspec-title                 ).
      WHEN gc_fnam_alttext                 . return = REF #( me->gs_chartspec-alttext                ).
      WHEN gc_fnam_subtitle                . return = REF #( me->gs_chartspec-subtitle               ).
      WHEN gc_fnam_fontname                . return = REF #( me->gs_chartspec-fontname               ).
      WHEN gc_fnam_maximized               . return = REF #( me->gs_chartspec-maximized              ).
      WHEN gc_fnam_hiddendimensionstrateg  . return = REF #( me->gs_chartspec-hiddendimensionstrategy ).
    ENDCASE.
  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_titletextformat(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-titletextformat ) ).

    lif_a2g_json = me->new_titletextposition(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-titletextposition    ) ).

    lif_a2g_json = me->new_subtitletextformat(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-subtitletextformat   ) ).

    lif_a2g_json = me->new_subtitletextposition(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-subtitletextposition ) ).

    lif_a2g_json = me->new_backgroundcolor(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-backgroundcolor      ) ).

    lif_a2g_json = me->new_basicchart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-basic_chart           ) ).

    lif_a2g_json = me->new_piechart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-piechart             ) ).

    lif_a2g_json = me->new_bubblechart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-bubblechart          ) ).

    lif_a2g_json = me->new_candlestickchart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-candlestickchart     ) ).

    lif_a2g_json = me->new_orgchart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-orgchart             ) ).

    lif_a2g_json = me->new_histogramchart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-histogramchart       ) ).

    lif_a2g_json = me->new_waterfallchart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-waterfallchart       ) ).

    lif_a2g_json = me->new_treemapchart(  ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_chartspec-treemapchart         ) ).



  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_titletextformat     .
        me->gs_chartspec-titletextformat       =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_titletextposition   .
        me->gs_chartspec-titletextposition     =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_subtitletextformat  .
        me->gs_chartspec-subtitletextformat    =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_subtitletextposition.
        me->gs_chartspec-subtitletextposition  =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_backgroundcolor     .
        me->gs_chartspec-backgroundcolor       =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_basicchart          .
        me->gs_chartspec-basic_chart            =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_piechart            .
        me->gs_chartspec-piechart              =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_bubblechart         .
        me->gs_chartspec-bubblechart           =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_candlestickchart    .
        me->gs_chartspec-candlestickchart      =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_orgchart            .
        me->gs_chartspec-orgchart              =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_histogramchart      .
        me->gs_chartspec-histogramchart        =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_waterfallchart      .
        me->gs_chartspec-waterfallchart        =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_treemapchart        .
        me->gs_chartspec-treemapchart          =  <fs_value_range>.
      ENDIF.

    ENDLOOP..
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_chartspec  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_titletextformat       . return = me->new_titletextformat( ).
      WHEN gc_fnam_titletextposition     . return = me->new_titletextposition( ).
      WHEN gc_fnam_subtitletextformat    . return = me->new_subtitletextformat( ).
      WHEN gc_fnam_subtitletextposition  . return = me->new_subtitletextposition( ).
      WHEN gc_fnam_backgroundcolor       . return = me->new_backgroundcolor( ).
      WHEN gc_fnam_basicchart            . return = me->new_basicchart( ).
      WHEN gc_fnam_piechart              . return = me->new_piechart( ).
      WHEN gc_fnam_bubblechart           . return = me->new_bubblechart( ).
      WHEN gc_fnam_candlestickchart      . return = me->new_candlestickchart( ).
      WHEN gc_fnam_orgchart              . return = me->new_orgchart( ).
      WHEN gc_fnam_histogramchart        . return = me->new_histogramchart( ).
      WHEN gc_fnam_waterfallchart        . return = me->new_waterfallchart( ).
      WHEN gc_fnam_treemapchart          . return = me->new_treemapchart( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_titletextformat     . return ?= me->go_json_array->getinstance( gc_fnam_titletextformat      ).
      WHEN gc_fnam_titletextposition   . return ?= me->go_json_array->getinstance( gc_fnam_titletextposition    ).
      WHEN gc_fnam_subtitletextformat  . return ?= me->go_json_array->getinstance( gc_fnam_subtitletextformat   ).
      WHEN gc_fnam_subtitletextposition. return ?= me->go_json_array->getinstance( gc_fnam_subtitletextposition ).
      WHEN gc_fnam_backgroundcolor     . return ?= me->go_json_array->getinstance( gc_fnam_backgroundcolor      ).
      WHEN gc_fnam_basicchart          . return ?= me->go_json_array->getinstance( gc_fnam_basicchart           ).
      WHEN gc_fnam_piechart            . return ?= me->go_json_array->getinstance( gc_fnam_piechart             ).
      WHEN gc_fnam_bubblechart         . return ?= me->go_json_array->getinstance( gc_fnam_bubblechart          ).
      WHEN gc_fnam_candlestickchart    . return ?= me->go_json_array->getinstance( gc_fnam_candlestickchart     ).
      WHEN gc_fnam_orgchart            . return ?= me->go_json_array->getinstance( gc_fnam_orgchart             ).
      WHEN gc_fnam_histogramchart      . return ?= me->go_json_array->getinstance( gc_fnam_histogramchart       ).
      WHEN gc_fnam_waterfallchart      . return ?= me->go_json_array->getinstance( gc_fnam_waterfallchart       ).
      WHEN gc_fnam_treemapchart        . return ?= me->go_json_array->getinstance( gc_fnam_treemapchart         ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_treemapchart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TREEMAPCHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_treemapchart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_waterfallchart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_WATERFALLCHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_waterfallchart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_histogramchart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_HISTOGRAMCHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_histogramchart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_orgchart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_ORGCHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_orgchart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_candlestickchart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CANDLESTICKCHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_candlestickchart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_bubblechart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BUBBLECHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_bubblechart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_piechart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_PIECHART' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_piechart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_basicchart.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BASICCHARTSPEC' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_basicchart
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_backgroundcolor .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COLOR' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_backgroundcolor
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_subtitletextposition.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTPOS' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_subtitletextposition
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_subtitletextformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_subtitletextformat
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_titletextposition .
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTPOS ' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_titletextposition
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_titletextformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_TEXTFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_titletextformat
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
