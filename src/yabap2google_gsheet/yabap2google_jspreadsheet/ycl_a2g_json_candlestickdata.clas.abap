CLASS ycl_a2g_json_candlestickdata DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_candlestickdata,
             lowseries   TYPE ycl_a2g_json_candlestickserie=>ty_s_json_candlestickserie,
             openseries  TYPE ycl_a2g_json_candlestickserie=>ty_s_json_candlestickserie,
             closeseries TYPE ycl_a2g_json_candlestickserie=>ty_s_json_candlestickserie,
             highseries  TYPE ycl_a2g_json_candlestickserie=>ty_s_json_candlestickserie,
           END OF ty_s_json_candlestickdata.
    TYPES ty_t_json_candlestickdata TYPE STANDARD TABLE OF ty_s_json_candlestickdata WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_candlestickdata  TYPE ty_s_json_candlestickdata.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_candlestickdata IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_candlestickdata  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
