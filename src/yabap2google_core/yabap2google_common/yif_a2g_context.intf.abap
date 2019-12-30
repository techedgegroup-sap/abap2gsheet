"!<h1>yif_a2g_context</h1>
"!<p>This interface is used to manage context of the main data into metadata class.</p>
interface YIF_A2G_CONTEXT
  public .

  "! This Method is used to read the data stored into the class
  "! @parameter RETURN       | Data content
  methods READ_DATA
    returning value(RETURN) type ref to DATA .

  "! This Method is used to store the data stored into the class
  "! @parameter INPUT       | Data content
  methods WRITE_DATA
    importing !INPUT type ref to DATA .

  "! This Method is used to read the data stored into the class
  "! @parameter RETURN       | Message Manager class fot error handling
  methods GET_PROTOCOL
    returning value(RETURN) type ref to YIF_A2G_MSG_Manager .
endinterface.
