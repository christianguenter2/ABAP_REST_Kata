class zcl_test_rest definition
  public
  final
  create public .

  public section.
    interfaces: if_http_extension.

  private section.
    constants: co_max_rows type i value 500.

    data: mo_request       type ref to if_http_request,
          mo_response      type ref to if_http_response,
          mt_header_fields type tihttpnvp,
          mt_form_fields   type tihttpnvp.

    methods:
      delete,
      post,
      get
        raising
          lcx_error,

      write_table_to_repsonse
        importing
          i_data type any,

      get_table_type_description
        importing
          i_table_name                type string
        returning
          value(ro_table_description) type ref to cl_abap_tabledescr,

      get_paginantion_fields
        exporting
          e_top  type i
          e_skip type i
        raising
          lcx_error,

      get_navigation
        exporting
          e_table_name type string
          e_index      type i
        raising
          lcx_error,

      string_to_integer
        importing
          i_string         type string
        returning
          value(r_integer) type i
        raising
          lcx_error,

      map_form_fields_to_where_cls
        importing
          i_table_name   type string
        exporting
          e_where_clause type string,

      get_count
        returning
          value(r_count) type abap_bool,

      write_count_to_response
        importing
          i_count type i,

      write_struct_to_repsonse
        importing
          is_struct type any.

endclass.



class zcl_test_rest implementation.


  method delete.

  endmethod.


  method get.

    data: tab_ref      type ref to data,
          count_result type i.

    field-symbols: <table> type standard table.

    get_navigation(
      importing
        e_table_name = data(table_name)
        e_index      = data(index) ).

    get_paginantion_fields(
      importing
        e_top  = data(top)
        e_skip = data(skip) ).

    if index is not initial and ( top is not initial or skip is not initial ).
      lcx_error=>raise( |It isn't allowed to mix up index with skip or top| ).
    endif.

    data(count) = get_count( ).

    if top is not initial and skip is not initial and count is initial.
      lcx_error=>raise( |It isn't allowed to mix up skip or top with count| ).
    endif.

    if index is not initial.
      top = index.
    endif.

    if top is initial.
      top = co_max_rows.
    endif.

    map_form_fields_to_where_cls(
      exporting
        i_table_name   = table_name
      importing
        e_where_clause = data(where_clause) ).

    data(table_descr) = get_table_type_description( table_name ).

    create data tab_ref type handle table_descr.
    assign tab_ref->* to <table>.
    assert sy-subrc = 0.

    if count = abap_true.

      select from (table_name)
             fields count(*)
             where (where_clause)
             into @count_result.

    else.

      select from (table_name)
             fields *
             where (where_clause)
             order by primary key
             into table @<table>
             up to @top rows
             offset @skip.

    endif.

    if count is not initial.

      write_count_to_response( count_result ).

    elseif index is initial.

      write_table_to_repsonse( <table> ).

    elseif line_exists( <table>[ index ] ).

      write_struct_to_repsonse( <table>[ index ] ).

    else.

      lcx_error=>raise( |No data found| ).

    endif.

  endmethod.


  method get_count.

    r_count = boolc( line_exists( mt_form_fields[ name = '$count' ] ) ).

  endmethod.


  method get_navigation.

    data(path_info) = mo_request->get_header_field( name = '~path_info' ).

    if path_info is initial.
      lcx_error=>raise( |Please supply entity| ).
    endif.

    split path_info at '/'
                    into data(dummy) e_table_name data(index).

    try.
        e_table_name = cl_abap_dyn_prg=>check_table_name_str( val      = to_upper( e_table_name )
                                                              packages = '' ).

      catch cx_root into data(error).
        lcx_error=>raise_exception( error ).
    endtry.

    e_index = string_to_integer( index ).

  endmethod.


  method get_paginantion_fields.

    clear: e_skip, e_top.

    data(skip_string) = mo_request->get_form_field( name = '$skip' ).
    data(top_string) = mo_request->get_form_field( name = '$top' ).

    if skip_string is not initial.

      e_skip = string_to_integer( skip_string ).

    endif.

    if top_string is not initial.

      e_top = string_to_integer( top_string ).

    endif.

  endmethod.


  method get_table_type_description.

    ro_table_description = cl_abap_tabledescr=>create( cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( i_table_name ) ) ).

  endmethod.


  method if_http_extension~handle_request.

    mo_request = server->request.
    mo_response = server->response.

    mo_request->get_header_fields(
      changing
        fields = mt_header_fields ).

    mo_request->get_form_fields(
      changing
        fields = mt_form_fields ).

    try.
        case mo_request->get_method( ).
          when 'GET'.

            get( ).

          when 'POST'.

            post( ).

          when 'DELETE'.

            delete( ).

          when others.

            mo_response->set_status( code   = 405
                                     reason = |Method { mo_request->get_method( ) } not supported| ).

        endcase.

      catch lcx_error into data(error).

        mo_response->set_status( code   = 404
                                 reason = error->get_text( ) ).

    endtry.

  endmethod.


  method map_form_fields_to_where_cls.

    data: selopt_tab type standard table of ddshselopt.

    data(lo_structdescr) = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( i_table_name ) ).

    loop at lo_structdescr->components assigning field-symbol(<component>).

      assign mt_form_fields[ name = to_lower( <component>-name ) ] to field-symbol(<form_field>).
      if sy-subrc <> 0.
        continue.
      endif.

      insert value #( shlpfield = <form_field>-name
                      sign      = 'I'
                      option    = 'EQ'
                      low       = <form_field>-value )
             into table selopt_tab.

    endloop.

    if lines( selopt_tab ) > 0.

      call function 'F4_CONV_SELOPT_TO_WHERECLAUSE'
        importing
          where_clause = e_where_clause
        tables
          selopt_tab   = selopt_tab.

    endif.

  endmethod.


  method post.

  endmethod.


  method string_to_integer.

    if i_string cn '0123456789'.
      lcx_error=>raise( |{ i_string } contains invalid characters| ).
    endif.

    r_integer = i_string.

  endmethod.


  method write_count_to_response.

    new lcl_count_writer( i_count     = i_count
                          io_request  = mo_request
                          io_response = mo_response )->execute( ).

  endmethod.


  method write_table_to_repsonse.

    new lcl_table_writer( it_table    = i_data
                          io_request  = mo_request
                          io_response = mo_response )->execute( ).

  endmethod.


  method write_struct_to_repsonse.

    new lcl_struct_writer( is_struct   = is_struct
                           io_request  = mo_request
                           io_response = mo_response )->execute( ).

  endmethod.

endclass.
