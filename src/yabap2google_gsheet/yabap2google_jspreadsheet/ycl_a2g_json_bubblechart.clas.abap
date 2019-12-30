CLASS ycl_a2g_json_bubblechart DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_bubblechart,
             legendposition      TYPE string,
             bubblelabels        TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             domain              TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             series              TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             groupids            TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             bubbleopacity       TYPE i,
             bubblebordercolor   TYPE ycl_a2g_json_color=>ty_s_json_color,
             bubblemaxradiussize TYPE i,
             bubbleminradiussize TYPE i,
             bubbletextstyle     TYPE ycl_a2g_json_textformat=>ty_s_json_textformat,
           END OF ty_s_json_bubblechart.
    TYPES ty_t_json_bubblechart TYPE STANDARD TABLE OF ty_s_json_bubblechart WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_bubblechart  TYPE ty_s_json_bubblechart.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_bubblechart IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_bubblechart  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
