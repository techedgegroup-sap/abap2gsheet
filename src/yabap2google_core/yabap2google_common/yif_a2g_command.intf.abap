"!<h1>YIF_A2G_MSG_MANAGER</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl. - Abap 2 Google - Command</p>
"!<p>This class is used to execute the command (command pattern).</p>
INTERFACE yif_a2g_command
  PUBLIC .
  "! Function Code executed
  DATA gv_fcode TYPE syst_ucomm .

  "! This Method execute a specific command
  "! @parameter im_fcode   | command
  METHODS execute
    IMPORTING
      !im_fcode TYPE syst_ucomm.

  "! This Method set the command to execute
  "! @parameter im_fcode   | command
  METHODS set_fcode
    CHANGING
      !im_fcode TYPE syst_ucomm .
ENDINTERFACE.
