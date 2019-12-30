"!<h1>YIF_A2G_MSG_MANAGER</h1>
"! Cmp. Appl. - Abap 2 Google - Message Manager interface
"!<p>This class is used to manage the various message of an application.</p>
CLASS ycl_a2g_msg_manager DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES yif_a2g_msg_manager .

    "! This Event start every times a message is stored
    EVENTS stored .

    "! This Method build and init instance of message manager class external creation
    "! of an instance of message manager are forbidden
    "! @parameter Return     | Cmp. Appl. - Abap 2 Google - Message Manager interface
    CLASS-METHODS create_init_msg_manager
      RETURNING
        VALUE(return) TYPE REF TO yif_a2g_msg_manager .

    "! This Method build and instance of message manager class based on a list
    "! of message registered externally of this component
    "! @parameter im_message  | Error messages
    "! @parameter Return      | Cmp. Appl. - Abap 2 Google - Message Manager interface
    CLASS-METHODS create_msg_manager_from_tab
      IMPORTING
        !im_message   TYPE bapiret2_tab
      RETURNING
        VALUE(return) TYPE REF TO yif_a2g_msg_manager .

  PROTECTED SECTION.

    "! This Method costructor messages manager
    METHODS constructor .


    "! This Method display all messages
    "! @parameter im_messages | output Error messages
    METHODS message_show
      IMPORTING
        !im_messages TYPE ya2google_t_msg_show.

  PRIVATE SECTION.

    "! instance of dialogbox container
    DATA go_dial TYPE REF TO cl_gui_dialogbox_container .
    "! array of messages
    DATA go_msg_array TYPE REF TO ycl_a2g_array.

    "! This Method handler the on_close event of the dialog box container
    METHODS on_close FOR EVENT close OF cl_gui_dialogbox_container
      IMPORTING !sender .

    "! This Method handler the stored event of this class
    METHODS on_store FOR EVENT stored OF ycl_a2g_msg_manager .

ENDCLASS.


CLASS ycl_a2g_msg_manager IMPLEMENTATION.

  METHOD constructor.
    "&  Declaration Part
    "&  Source Part
    me->go_msg_array = NEW #( ).
    SET HANDLER me->on_store FOR me.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD create_msg_manager_from_tab.
    "&  Declaration Part
    DATA: ls_t100key TYPE scx_t100key.
    DATA: lo_msg_manager TYPE REF TO ycl_a2g_msg_manager.

    "&  Source Part
    lo_msg_manager = NEW #( ).
    return ?= lo_msg_manager.

    LOOP AT im_message INTO DATA(ls_bapiret2).

      ls_t100key-msgid = ls_bapiret2-id.
      ls_t100key-msgno = ls_bapiret2-number.
      ls_t100key-attr1 = ls_bapiret2-message_v1.
      ls_t100key-attr2 = ls_bapiret2-message_v2.
      ls_t100key-attr3 = ls_bapiret2-message_v3.
      ls_t100key-attr4 = ls_bapiret2-message_v4.

      return->register( im_t100key = ls_t100key
                        im_msgty   = ls_bapiret2-type ).

    ENDLOOP.
  ENDMETHOD.


  METHOD create_init_msg_manager.
    "&  Declaration Part
    DATA: lo_msg_manager TYPE REF TO ycl_a2g_msg_manager.

    "&  Source Part
    lo_msg_manager = NEW #( ).
    return ?= lo_msg_manager.
  ENDMETHOD.                    "CREATE_INIT_msg_manager


  METHOD message_show.
    "&  Declaration Part
    DATA: lo_alv         TYPE REF TO cl_gui_alv_grid,
          lt_msg_display TYPE        ya2google_t_msg_show,
          ls_layo        TYPE        lvc_s_layo.

    "&  Source Part
    lt_msg_display =  im_messages .
    me->go_dial = NEW #( width  = 600
                         height = 180
                         top    = 100
                         left   = 100 ).

    SET HANDLER on_close FOR me->go_dial.

    lo_alv = NEW #( i_parent = go_dial ).

    ls_layo-zebra      = 'X'.
    ls_layo-cwidth_opt = 'X'.
    ls_layo-no_rowmark = 'X'.
    ls_layo-no_toolbar = 'X'.
    ls_layo-grid_title = TEXT-msg.

    lo_alv->set_table_for_first_display( EXPORTING i_structure_name = |YA2GOOGLE_S_MSG_SHOW|
                                                   is_layout        = ls_layo
                                         CHANGING  it_outtab        = lt_msg_display ).

  ENDMETHOD.                    "message_show


  METHOD on_close.
    "&  Declaration Part
    "&  Source Part
    sender->set_visible( space ).
  ENDMETHOD.                    "on_close


  METHOD on_store.
    "&  Declaration Part
    "&  Source Part
    " Do Nothing ...
  ENDMETHOD.                    "ON_STORE


  METHOD yif_a2g_msg_manager~check_existence.
    "&  Declaration Part
    "&  Source Part
    DATA(lt_return) = me->yif_a2g_msg_manager~get( ).
    TRY.
        DATA(ls_msg) = lt_return[ type = 'E' ].
        return = 'X'.

      CATCH cx_sy_itab_line_not_found.
        TRY.
            ls_msg = lt_return[ type = 'W' ].
            return = 'Y'.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.

  METHOD yif_a2g_msg_manager~get.

    DATA: lo_object       TYPE REF TO object,
          lo_meta_message TYPE REF TO ycl_a2g_meta_message.

    FREE lo_object.
    LOOP AT me->go_msg_array->getallnamesofinstances( ) INTO  data(lv_name).
      lo_object =  me->go_msg_array->getinstance( lv_name ).
      lo_meta_message ?= lo_object.
      data(ls_bapiret2) = lo_meta_message->get_bapiret_message( ).
      APPEND ls_bapiret2 TO return.
    ENDLOOP.
  ENDMETHOD.

  METHOD yif_a2g_msg_manager~show.
    "&  Declaration Part
    DATA: lo_object       TYPE REF TO object,
          lo_meta_message TYPE REF TO ycl_a2g_meta_message,
          lt_msg_display  TYPE        ya2google_t_msg_show.

    "&  Source Part
    LOOP AT me->go_msg_array->getallnamesofinstances( ) INTO  DATA(lv_name).
      lo_object =  me->go_msg_array->getinstance( lv_name ).
      lo_meta_message ?=  lo_object.
      APPEND lo_meta_message->get_message_display( ) TO lt_msg_display.
    ENDLOOP.
    SORT  lt_msg_display.
    DELETE ADJACENT DUPLICATES FROM lt_msg_display COMPARING ALL FIELDS.
    me->message_show( lt_msg_display ).
  ENDMETHOD.


  METHOD yif_a2g_msg_manager~save.
    "&  Declaration Part
    "&  Source Part
    "&  Do Nothing
  ENDMETHOD.


  METHOD yif_a2g_msg_manager~delete.
    "&  Declaration Part
    "&  Source Part
    me->go_msg_array->deleteinstances( ).
  ENDMETHOD.


  METHOD yif_a2g_msg_manager~register.
    "&  Declaration Part
    DATA: lv_name    TYPE string.
    DATA: lv_line    TYPE balmnr.

    "&  Source Part
    DATA(lo_meta_message) = ycl_a2g_meta_message=>create_from_t100key( i_t100key = im_t100key
                                                                       i_msgty   = im_msgty ).

    lv_line = lines( me->go_msg_array->getallnamesofinstances( ) ).
    lv_name = lv_line + 1.

    me->go_msg_array->setinstance( im_name   = lv_name
                                   im_object = lo_meta_message ).

    RAISE EVENT stored.
  ENDMETHOD.                    "YIF_A2G_msg_manager~MESSAGE_STORE
ENDCLASS.
