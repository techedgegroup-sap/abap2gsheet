"!<h1>YIF_A2G_MSG_MANAGER</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Message Metadata complete</p>
"!<p>This class is used to manage the various message of an application.</p>
CLASS ycl_a2g_meta_message DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_meta_msg
  CREATE PROTECTED .

  PUBLIC SECTION.

    "! This Method is responsible to create an instance correctly
    "! @parameter i_t100key   | T100 Key with Parameters Mapped to Attribute Names
    "! @parameter i_msgty     | Message Type
    "! @parameter return      | Instance of Message Metadata complete
    CLASS-METHODS create_from_t100key
      IMPORTING !i_t100key    TYPE scx_t100key
                !i_msgty      TYPE symsgty
      RETURNING VALUE(return) TYPE REF TO ycl_a2g_meta_message .

    "! This Method return the message stored
    "! @parameter return      | message
    METHODS get_message_display
      RETURNING VALUE(return) TYPE ya2google_s_msg_show.

    "! This Method return the message stored in bapiret2 structure
    "! @parameter return      | Return Parameter
    METHODS get_bapiret_message
      RETURNING VALUE(return) TYPE bapiret2 .

  PROTECTED SECTION.

    "! This Method build the message stored in bapiret2 structure
    "! @parameter return      | Return Parameter
    METHODS build_bapiret
      RETURNING VALUE(return) TYPE bapiret2 .
  PRIVATE SECTION.

    "! This Method convert the type of the single message for display information
    "! @parameter return      | Return Parameter
    METHODS convert_type
      IMPORTING !type_in      TYPE bapiret2-type
      RETURNING VALUE(return) TYPE ya2google_s_msg_show-type .
ENDCLASS.



CLASS ycl_a2g_meta_message IMPLEMENTATION.


  METHOD build_bapiret.
  "&  Declaration Part
  "&  Source Part
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = me->gv_msgty
        cl     = me->gv_msgid
        number = me->gv_msgno
        par1   = me->gv_attr1
        par2   = me->gv_attr2
        par3   = me->gv_attr3
        par4   = me->gv_attr4
      IMPORTING
        return = return.
  ENDMETHOD.


  METHOD convert_type.
  "&  Declaration Part
  "&  Source Part
    CASE type_in.
      WHEN 'E'. return = '@5C@'.
      WHEN 'W'. return = '@5D@'.
      WHEN 'S'. return = '@5B@'.
    ENDCASE.
  ENDMETHOD.                    "convert_type


  METHOD create_from_t100key.
  "&  Declaration Part
    DATA: lv_attrx TYPE symsgv.

  "&  Source Part
    return = NEW #(  ).

    return->set_msgid( i_t100key-msgid ).
    return->set_msgno( i_t100key-msgno ).
    return->set_msgty( i_msgty ).
    lv_attrx = i_t100key-attr1.
    return->set_attr1( lv_attrx ).
    lv_attrx = i_t100key-attr2.
    return->set_attr2( lv_attrx ).
    lv_attrx = i_t100key-attr3.
    return->set_attr3( lv_attrx ).
    lv_attrx = i_t100key-attr4.
    return->set_attr4( lv_attrx ).
  ENDMETHOD.                    "create_from_t100key


  METHOD get_bapiret_message.
  "&  Declaration Part
  "&  Source Part

    return = me->build_bapiret( ).
  ENDMETHOD.                    "GET_BAPIRET_MESSAGE


  METHOD get_message_display.
  "&  Declaration Part
  "&  Source Part
    data(ls_bapiret2) = me->get_bapiret_message( ).

    return-type    = me->convert_type( ls_bapiret2-type ).
    return-msgid   = ls_bapiret2-id.
    return-msgno   = ls_bapiret2-number.
    return-message = ls_bapiret2-message.

  ENDMETHOD.                    "get_message_display
ENDCLASS.
