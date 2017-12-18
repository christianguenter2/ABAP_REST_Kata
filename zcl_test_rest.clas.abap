CLASS zcl_test_rest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PRIVATE SECTION.
    DATA: mo_request       TYPE REF TO if_http_request,
          mo_response      TYPE REF TO if_http_response,
          mt_header_fields TYPE tihttpnvp,
          mt_form_fields   TYPE tihttpnvp.

    METHODS:
      delete,
      post,
      get
        RAISING
          lcx_error,

      write_data_to_repsonse
        IMPORTING
          i_data TYPE any,

      get_table_type_description
        IMPORTING
          i_table_name                TYPE string
        RETURNING
          VALUE(ro_table_description) TYPE REF TO cl_abap_tabledescr,

      get_paginantion_fields
        EXPORTING
          e_top  TYPE i
          e_skip TYPE i
        RAISING
          lcx_error,

      get_navigation
        EXPORTING
          e_table_name TYPE string
          e_index      TYPE i
        RAISING
          lcx_error,

      string_to_integer
        IMPORTING
          i_string         TYPE string
        RETURNING
          VALUE(r_integer) TYPE i
        RAISING
          lcx_error.

ENDCLASS.


CLASS zcl_test_rest IMPLEMENTATION.

  METHOD if_http_extension~handle_request.

    mo_request = server->request.
    mo_response = server->response.

    mo_request->get_header_fields(
      CHANGING
        fields = mt_header_fields ).

    mo_request->get_form_fields(
      CHANGING
        fields = mt_form_fields ).

    TRY.
        CASE mo_request->get_method( ).
          WHEN 'GET'.

            get( ).

          WHEN 'POST'.

            post( ).

          WHEN 'DELETE'.

            delete( ).

          WHEN OTHERS.

            mo_response->set_status( code   = 405
                                     reason = |Method { mo_request->get_method( ) } not supported| ).

        ENDCASE.

      CATCH lcx_error INTO DATA(error).

        mo_response->set_status( code   = 404
                                 reason = error->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD get.

    DATA: tab_ref TYPE REF TO data.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    get_navigation(
      IMPORTING
        e_table_name = DATA(table_name)
        e_index      = DATA(index) ).

    get_paginantion_fields(
      IMPORTING
        e_top  = DATA(top)
        e_skip = DATA(skip) ).

    IF index IS NOT INITIAL AND ( top IS NOT INITIAL OR skip IS NOT INITIAL ).
      lcx_error=>raise( |It isn't allowed to mix up index with skip or top| ).
    ENDIF.

    DATA(table_descr) = get_table_type_description( table_name ).

    CREATE DATA tab_ref TYPE HANDLE table_descr.
    ASSIGN tab_ref->* TO <table>.
    ASSERT sy-subrc = 0.

    SELECT FROM (table_name)
           FIELDS *
           ORDER BY PRIMARY KEY
           INTO TABLE @<table>
           UP TO @top ROWS
           OFFSET @skip.

    IF index IS INITIAL.

      write_data_to_repsonse( <table> ).

    ELSEIF line_exists( <table>[ index ] ).

      write_data_to_repsonse( <table>[ index ] ).

    ELSE.

      lcx_error=>raise( |No data found| ).

    ENDIF.

  ENDMETHOD.

  METHOD post.

  ENDMETHOD.

  METHOD delete.

  ENDMETHOD.

  METHOD write_data_to_repsonse.

    DATA: writer TYPE REF TO cl_sxml_string_writer.

    DATA(content_type) = mo_request->get_content_type( ).

    CASE content_type.
      WHEN 'application/xml'.

        writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_xml10 ).

      WHEN 'application/json'.

        writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).

      WHEN OTHERS.

        mo_response->set_status( code   = 404
                                 reason = |Content-Type { content_type } not supported| ).

    ENDCASE.

    CALL TRANSFORMATION id SOURCE table = i_data
                           RESULT XML writer.

    mo_response->set_data( writer->get_output( ) ).
    mo_response->set_content_type( content_type ).
    mo_response->set_status( code   = 200
                             reason = |ok| ).

  ENDMETHOD.

  METHOD get_table_type_description.

    ro_table_description = cl_abap_tabledescr=>create( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( i_table_name ) ) ).

  ENDMETHOD.


  METHOD get_paginantion_fields.

    CLEAR: e_skip, e_top.

    DATA(skip_string) = mo_request->get_form_field( name = '$skip' ).
    DATA(top_string) = mo_request->get_form_field( name = '$top' ).

    IF skip_string IS NOT INITIAL.

      e_skip = string_to_integer( skip_string ).

    ENDIF.

    IF top_string IS NOT INITIAL.

      e_top = string_to_integer( top_string ).

    ENDIF.

  ENDMETHOD.


  METHOD get_navigation.

    DATA(path_info) = mo_request->get_header_field( name = '~path_info' ).

    IF path_info IS INITIAL.
      lcx_error=>raise( |Please supply entity| ).
    ENDIF.

    SPLIT path_info AT '/'
                    INTO DATA(dummy) e_table_name DATA(index).

    TRY.
        e_table_name = cl_abap_dyn_prg=>check_table_name_str( val      = to_upper( e_table_name )
                                                              packages = '' ).

      CATCH cx_root INTO DATA(error).
        lcx_error=>raise_exception( error ).
    ENDTRY.

    e_index = string_to_integer( index ).

  ENDMETHOD.


  METHOD string_to_integer.

    IF i_string CN '0123456789'.
      lcx_error=>raise( |{ i_string } contains invalid characters| ).
    ENDIF.

    r_integer = i_string.

  ENDMETHOD.

ENDCLASS.
