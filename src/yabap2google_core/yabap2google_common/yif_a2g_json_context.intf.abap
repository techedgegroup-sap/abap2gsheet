"!<h1>YIF_A2G_JSON_CONTEXT</h1>
"!<p>This interface is used to manage context of the main data and json data into metadata class.</p>
interface YIF_A2G_JSON_CONTEXT
  public .

interfaces YIF_A2G_CONTEXT.


  "! This Method is used to read the data stored into the class
  "! @parameter RETURN       | Data content
  methods READ_json_DATA
    returning value(RETURN) type string.

  "! This Method is used to store the data stored into the class
  "! @parameter INPUT       | Data content
  methods WRITE_json_DATA
    importing !INPUT type string.

endinterface.
