"!<h1>yif_a2g_trtyp</h1>
"!<p>This interface is used to manage the transaction type of each class.</p>
INTERFACE yif_a2g_trtyp
  PUBLIC .

  "! Transaction type - Display
  CONSTANTS gc_trtyp_display TYPE trtyp VALUE 'A' ##NO_TEXT.
  "! Transaction type - Create
  CONSTANTS gc_trtyp_create  TYPE trtyp VALUE 'H' ##NO_TEXT.
  "! Transaction type - Change
  CONSTANTS gc_trtyp_change  TYPE trtyp VALUE 'V' ##NO_TEXT.
  "! Transaction type - Delete
  CONSTANTS gc_trtyp_delete  TYPE trtyp VALUE 'L' ##NO_TEXT.
  "! Transaction type - Init transaction
  CONSTANTS gc_trtyp_init    TYPE trtyp VALUE 'X' ##NO_TEXT.

  DATA gv_trtyp TYPE trtyp .

  "! This Event is user to manage subsequent action on change value of transaction type
  "! @parameter im_trtyp       | Transaction tyep Value
  EVENTS trtyp_changed
    EXPORTING VALUE(im_trtyp) TYPE trtyp .

  "! This Method set the new value of transaction type
  "! @parameter im_trtyp       | Transaction tyep Value
  METHODS set_trtyp
    IMPORTING !im_trtyp TYPE trtyp .

  "! This Method initialize the transaction type value
  METHODS init .

ENDINTERFACE.
