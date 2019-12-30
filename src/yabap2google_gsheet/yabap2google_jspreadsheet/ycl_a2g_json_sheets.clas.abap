CLASS ycl_a2g_json_sheets DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_json_sheets,
             properties         TYPE ycl_a2g_json_sheet_prop=>ty_s_json_sheet_prop,
             data               TYPE ycl_a2g_json_griddata=>ty_t_json_griddata,
             merges             TYPE ycl_a2g_json_gridrange=>ty_t_json_gridrange,
             conditionalformats TYPE ycl_a2g_json_cond_rule=>ty_t_json_cond_rule,
             filterviews        TYPE ycl_a2g_json_filterview=>ty_t_json_filterview,
             protectedranges    TYPE ycl_a2g_json_prot_range=>ty_t_json_prot_range,
             basicfilter        TYPE ycl_a2g_json_basicfilter=>ty_s_json_basicfilter,
             charts             TYPE ycl_a2g_json_embeddedchart=>ty_t_json_embeddedchart,
             bandedranges       TYPE ycl_a2g_json_bandedrange=>ty_t_json_bandedrange,
             developermetadata  TYPE ycl_a2g_json_developermetadata=>ty_t_json_devrmeta,
             rowgroups          TYPE ycl_a2g_json_dimensiongroup=>ty_t_json_dimensiongroup,
             columngroups       TYPE ycl_a2g_json_dimensiongroup=>ty_t_json_dimensiongroup,
           END OF ty_s_json_sheets.
    TYPES ty_t_json_sheets TYPE STANDARD TABLE OF ty_s_json_sheets WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS gc_fnam_properties         TYPE string VALUE 'PROPERTIES'.
    CONSTANTS gc_fnam_data               TYPE string VALUE 'DATA'.
    CONSTANTS gc_fnam_merges             TYPE string VALUE 'MERGES'.
    CONSTANTS gc_fnam_conditionalformats TYPE string VALUE 'CONDITIONALFORMATS'.
    CONSTANTS gc_fnam_filterviews        TYPE string VALUE 'FILTERVIEWS'.
    CONSTANTS gc_fnam_protectedranges    TYPE string VALUE 'PROTECTEDRANGES'.
    CONSTANTS gc_fnam_basicfilter        TYPE string VALUE 'BASICFILTER'.
    CONSTANTS gc_fnam_charts             TYPE string VALUE 'CHARTS'.
    CONSTANTS gc_fnam_bandedranges       TYPE string VALUE 'BANDEDRANGES'.
    CONSTANTS gc_fnam_developermetadata  TYPE string VALUE 'DEVELOPERMETADATA'.
    CONSTANTS gc_fnam_rowgroups          TYPE string VALUE 'ROWGROUPS'.
    CONSTANTS gc_fnam_columngroups       TYPE string VALUE 'COLUMNGROUPS'.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.


    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_sheets  TYPE ty_s_json_sheets.

    METHODS new_properties         RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_data               RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_merges             RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_conditionalformats RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_filterviews        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_protectedranges    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_basicfilter        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_charts             RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_bandedranges       RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_developermetadata  RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_rowgroups          RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_columngroups       RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_sheets IMPLEMENTATION.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_properties        . return = me->new_properties( ).
      WHEN gc_fnam_data              . return = me->new_data( ).
      WHEN gc_fnam_merges            . return = me->new_merges( ).
      WHEN gc_fnam_conditionalformats. return = me->new_conditionalformats( ).
      WHEN gc_fnam_filterviews       . return = me->new_filterviews( ).
      WHEN gc_fnam_protectedranges   . return = me->new_protectedranges( ).
      WHEN gc_fnam_basicfilter       . return = me->new_basicfilter( ).
      WHEN gc_fnam_charts            . return = me->new_charts( ).
      WHEN gc_fnam_bandedranges      . return = me->new_bandedranges( ).
      WHEN gc_fnam_developermetadata . return = me->new_developermetadata( ).
      WHEN gc_fnam_rowgroups         . return = me->new_rowgroups( ).
      WHEN gc_fnam_columngroups      . return = me->new_columngroups( ).
    ENDCASE.

  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_properties         . return ?= me->go_json_array->getinstance( me->gc_fnam_properties         ).
      WHEN gc_fnam_data               . return ?= me->go_json_array->getinstance( me->gc_fnam_data               ).
      WHEN gc_fnam_merges             . return ?= me->go_json_array->getinstance( me->gc_fnam_merges             ).
      WHEN gc_fnam_conditionalformats . return ?= me->go_json_array->getinstance( me->gc_fnam_conditionalformats ).
      WHEN gc_fnam_filterviews        . return ?= me->go_json_array->getinstance( me->gc_fnam_filterviews        ).
      WHEN gc_fnam_protectedranges    . return ?= me->go_json_array->getinstance( me->gc_fnam_protectedranges    ).
      WHEN gc_fnam_basicfilter        . return ?= me->go_json_array->getinstance( me->gc_fnam_basicfilter        ).
      WHEN gc_fnam_charts             . return ?= me->go_json_array->getinstance( me->gc_fnam_charts             ).
      WHEN gc_fnam_bandedranges       . return ?= me->go_json_array->getinstance( me->gc_fnam_bandedranges       ).
      WHEN gc_fnam_developermetadata  . return ?= me->go_json_array->getinstance( me->gc_fnam_developermetadata  ).
      WHEN gc_fnam_rowgroups          . return ?= me->go_json_array->getinstance( me->gc_fnam_rowgroups          ).
      WHEN gc_fnam_columngroups       . return ?= me->go_json_array->getinstance( me->gc_fnam_columngroups       ).
    ENDCASE.

  ENDMETHOD.


  METHOD push_data.
    DATA(lif_a2g_json) = me->new_properties( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_sheets-properties          ) ).

    lif_a2g_json = me->new_basicfilter( ).
    lif_a2g_json->yif_a2g_context~write_data( REF #( me->gs_sheets-basicfilter         ) ).


    LOOP AT me->gs_sheets-data INTO DATA(ls_metadata).
      lif_a2g_json = me->new_data( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata                ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-merges INTO DATA(ls_metadata_merge).
      lif_a2g_json = me->new_merges( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_merge              ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-conditionalformats INTO DATA(ls_metadata_conditionalformats).
      lif_a2g_json = me->new_conditionalformats( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_conditionalformats  ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-filterviews INTO DATA(ls_metadata_filterviews).
      lif_a2g_json = me->new_filterviews( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_filterviews         ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-protectedranges INTO DATA(ls_metadata_protectedranges).
      lif_a2g_json = me->new_protectedranges( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_protectedranges     ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-charts INTO DATA(ls_metadata_charts).
      lif_a2g_json = me->new_charts( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_charts              ) ).
    ENDLOOP.


    LOOP AT me->gs_sheets-bandedranges INTO DATA(ls_metadata_bandedranges).
      lif_a2g_json = me->new_bandedranges( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_bandedranges        ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-developermetadata INTO DATA(ls_metadata_developermetadata).
      lif_a2g_json = me->new_developermetadata( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_developermetadata   ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-rowgroups INTO DATA(ls_metadata_rowgroups).
      lif_a2g_json = me->new_rowgroups( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_rowgroups           ) ).
    ENDLOOP.

    LOOP AT me->gs_sheets-columngroups INTO DATA(ls_metadata_columngroups).
      lif_a2g_json = me->new_columngroups( ).
      lif_a2g_json->yif_a2g_context~write_data( REF #( ls_metadata_columngroups       ) ).
    ENDLOOP.



  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.
clear:  me->gs_sheets-data, me->gs_sheets-merges .
    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.

      IF     lv_name CS me->gc_fnam_properties          .
        me->gs_sheets-properties  =  <fs_value_range>.              .
      ELSEIF lv_name CS me->gc_fnam_basicfilter         .
        me->gs_sheets-basicfilter =  <fs_value_range>.              .
      ELSEIF lv_name CS me->gc_fnam_data                .
        APPEND <fs_value_range> TO me->gs_sheets-data               .
      ELSEIF lv_name CS me->gc_fnam_merges              .
        APPEND <fs_value_range> TO me->gs_sheets-merges             .
      ELSEIF lv_name CS me->gc_fnam_conditionalformats  .
        APPEND <fs_value_range> TO me->gs_sheets-conditionalformats .
      ELSEIF lv_name CS me->gc_fnam_filterviews         .
        APPEND <fs_value_range> TO me->gs_sheets-filterviews        .
      ELSEIF lv_name CS me->gc_fnam_protectedranges     .
        APPEND <fs_value_range> TO me->gs_sheets-protectedranges    .
      ELSEIF lv_name CS me->gc_fnam_charts              .
        APPEND <fs_value_range> TO me->gs_sheets-charts             .
      ELSEIF lv_name CS me->gc_fnam_bandedranges        .
        APPEND <fs_value_range> TO me->gs_sheets-bandedranges       .
      ELSEIF lv_name CS me->gc_fnam_developermetadata   .
        APPEND <fs_value_range> TO me->gs_sheets-developermetadata  .
      ELSEIF lv_name CS me->gc_fnam_rowgroups           .
        APPEND <fs_value_range> TO me->gs_sheets-rowgroups          .
      ELSEIF lv_name CS me->gc_fnam_columngroups        .
        APPEND <fs_value_range> TO me->gs_sheets-columngroups       .
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_sheets  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


  METHOD new_properties.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_SHEET_PROP' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_properties
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_data.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDDATA' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_data ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_data && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_merges.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_GRIDRANGE' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_merges ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_merges && lv_line
                                     im_object = lo_object ).

  ENDMETHOD.

  METHOD new_conditionalformats.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_COND_RULE' ).


    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_conditionalformats ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_conditionalformats && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_filterviews.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_FILTERVIEW' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_filterviews ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_filterviews && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_protectedranges.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_PROT_RANGE' ).


    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_protectedranges ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_protectedranges && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_basicfilter.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BASICFILTER' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_basicfilter
    im_object = lo_object ).
  ENDMETHOD.

  METHOD new_charts.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_EMBEDDEDCHART' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_charts ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_charts && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_bandedranges.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BANDEDRANGE' ).


    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_bandedranges ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_bandedranges && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_developermetadata.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DEVELOPERMETADATA' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_developermetadata ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_developermetadata && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_rowgroups.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DIMENSIONGROUP' ).


    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_rowgroups ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_rowgroups && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_columngroups.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_DIMENSIONGROUP' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_columngroups ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_columngroups && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.



ENDCLASS.
