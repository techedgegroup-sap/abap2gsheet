"!<h1>YIF_A2G_MSG_MANAGER</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Message manager Balog</p>
"!<p>This class is used as a model to manage the various message of an application using the balog function.</p>
CLASS ycl_a2g_bal_msg_manager DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_msg_manager
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    "! Application log: Object name (Application code)
    DATA gv_object    TYPE balhdr-object    READ-ONLY.
    "! Application Log: Subobject
    DATA gv_subobject TYPE balhdr-subobject READ-ONLY.

    "! This Method build and init instance of message manager Application log class
    "! of an instance of message manager are forbidden
    "! @parameter i_object     | Application log: Object name (Application code)
    "! @parameter i_subobject  | Application Log: Subobject
    "! @parameter Return     | Cmp. Appl. - Abap 2 Google - Message Manager interface
    CLASS-METHODS create_init_msg_manager_bal
      IMPORTING !i_object     TYPE        balhdr-object
                !i_subobject  TYPE        balhdr-subobject OPTIONAL
      RETURNING VALUE(return) TYPE REF TO yif_a2g_msg_manager .

    METHODS yif_a2g_msg_manager~show        REDEFINITION .
    METHODS yif_a2g_msg_manager~register    REDEFINITION .
    METHODS yif_a2g_msg_manager~save        REDEFINITION .

  PROTECTED SECTION.

    "! Application Log: Log Handle
    DATA gv_log_handle TYPE balloghndl .

    METHODS message_show REDEFINITION .

    "! Costructor protected of class redefinition changing firma
    "! @parameter i_object     | Application log: Object name (Application code)
    "! @parameter i_subobject  | Application Log: Subobject
    METHODS constructor
      IMPORTING !i_object    TYPE balhdr-object
                !i_subobject TYPE balhdr-subobject .

  PRIVATE SECTION.
    "! T100 Key with Parameters Mapped to Attribute Names
    DATA gs_t100key TYPE scx_t100key.
    "! Message Type
    DATA gv_msgty   TYPE symsgty.
    "! Application log: log number
    DATA gv_msg_key TYPE balognr.

    "! Handler fo ron store event of the class
    METHODS on_store
        FOR EVENT stored OF ycl_a2g_bal_msg_manager .
ENDCLASS.


CLASS ycl_a2g_bal_msg_manager IMPLEMENTATION.


  METHOD constructor.
    "&  Declaration Part
    DATA: ls_log        TYPE bal_s_log.    "Log header data
    "&  Source Part
    super->constructor( ).
    me->gv_object = i_object.
    me->gv_subobject = i_subobject.

    ls_log-extnumber  = 'Abap 2 Google Application Log'.
    ls_log-object     = me->gv_object.
    ls_log-subobject  = me->gv_subobject.
    ls_log-aldate     = sy-datum.
    ls_log-altime     = sy-uzeit.
    ls_log-aluser     = sy-uname.
    ls_log-alprog     = sy-cprog.
    ls_log-aldate_del = sy-datum + 5.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = me->gv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    SET HANDLER me->on_store FOR me.
  ENDMETHOD.


  METHOD create_init_msg_manager_bal.
    "&  Declaration Part
    DATA: lo_msg_manager TYPE REF TO ycl_a2g_bal_msg_manager.

    "&  Source Part
    lo_msg_manager = NEW #( i_object    = i_object
                            i_subobject = i_subobject ).
    return ?= lo_msg_manager.

  ENDMETHOD.


  METHOD message_show.
    "&  Declaration Part
    DATA: lt_logh TYPE bal_t_logh.

    "&  Source Part
    APPEND me->gv_log_handle TO lt_logh.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = lt_logh
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

  ENDMETHOD.


  METHOD on_store.
    "&  Declaration Part
    DATA: ls_msg TYPE bal_s_msg.

    "&  Source Part
    ls_msg-msgty     = me->gv_msgty.
    ls_msg-msgid     = me->gs_t100key-msgid.
    ls_msg-msgno     = me->gs_t100key-msgno.
    ls_msg-msgv1     = me->gs_t100key-attr1.
    ls_msg-msgv2     = me->gs_t100key-attr2.
    ls_msg-msgv3     = me->gs_t100key-attr3.
    ls_msg-msgv4     = me->gs_t100key-attr4.

    CASE  me->gv_msgty.
      WHEN me->yif_a2g_msg_manager~gc_error  . ls_msg-probclass = '1'.
      WHEN me->yif_a2g_msg_manager~gc_success. ls_msg-probclass = '4'.
      WHEN me->yif_a2g_msg_manager~gc_warning. ls_msg-probclass = '2'.
    ENDCASE.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = me->gv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

  ENDMETHOD.


  METHOD yif_a2g_msg_manager~save.
    "&  Declaration Part
    "&  Source Part

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all       = 'X'
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD yif_a2g_msg_manager~show.
    "&  Declaration Part
    DATA: lt_msg_display TYPE ya2google_t_msg_show.

    "&  Source Part
    me->message_show( lt_msg_display ).
  ENDMETHOD.


  METHOD yif_a2g_msg_manager~register.
    "&  Declaration Part
    "&  Source Part

    me->gs_t100key = im_t100key.
    me->gv_msgty = im_msgty.

    super->yif_a2g_msg_manager~register( im_t100key = im_t100key
                                         im_msgty   = im_msgty ).

  ENDMETHOD.
ENDCLASS.
