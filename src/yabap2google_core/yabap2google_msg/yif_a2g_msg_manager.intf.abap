"!<h1>YIF_A2G_MSG_MANAGER</h1>
"! Cmp. Appl. - Abap 2 Google - Message Manager interface
"!<p>This class is used to manage the various message of an application.</p>
INTERFACE yif_a2g_msg_manager
  PUBLIC .

  "! Success Message
  CONSTANTS gc_success TYPE symsgty VALUE 'S' ##NO_TEXT.
  "! Warning Message
  CONSTANTS gc_warning TYPE symsgty VALUE 'W' ##NO_TEXT.
  "! Error Message
  CONSTANTS gc_error   TYPE symsgty VALUE 'E' ##NO_TEXT.

  "! This Method store into the message array a specific message
  "! @parameter im_t100key   | T100 Key with Parameters Mapped to Attribute Names
  "! @parameter Im_MSGTY     | Message Type
  METHODS register
    IMPORTING !im_t100key TYPE scx_t100key
              !im_msgty   TYPE symsgty..

  "! This Method delete all messages
  METHODS delete.

  "! This Method give back the list of message registered
  "! @parameter Return   | Error Messages
  METHODS get
    RETURNING
      VALUE(return) TYPE bapiret2_tab .

  "! This Method send in output all message registeres
  METHODS show.

  "! This Method check if an error or warning message exists
  "! @parameter return   | error message exists
  METHODS check_existence
    RETURNING
      VALUE(return) TYPE oax .

  "! This Method save all the error registered
  METHODS save .
ENDINTERFACE.
