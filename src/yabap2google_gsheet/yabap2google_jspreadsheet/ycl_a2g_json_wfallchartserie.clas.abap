CLASS ycl_a2g_json_wfallchartserie DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_wfallchartserie,
             data TYPE ycl_a2g_json_ChartData=>ty_s_json_chartdata,
             positiveColumnsStyle TYPE ycl_a2g_json_WfChartColStyle=>ty_s_json_WfChartColStyle,
             negativeColumnsStyle type ycl_a2g_json_WfChartColStyle=>ty_s_json_WfChartColStyle,
             subtotalColumnsStyle type ycl_a2g_json_WfChartColStyle=>ty_s_json_WfChartColStyle,
             hideTrailingSubtotal type string,
             customSubtotals type ycl_a2g_json_linestyle=>ty_s_json_linestyle,
           END OF ty_s_json_wfallchartserie.
    TYPES ty_t_json_wfallchartserie TYPE STANDARD TABLE OF ty_s_json_wfallchartserie WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_wfallchartserie  TYPE ty_s_json_wfallchartserie.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_wfallchartserie IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_wfallchartserie  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
