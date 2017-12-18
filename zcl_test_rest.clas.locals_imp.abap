*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

class lcl_abstract_writer definition abstract.

  public section.
    methods:
      constructor
        importing
          io_request  type ref to if_http_request
          io_response type ref to if_http_response,

      execute.

  protected section.
    data: mo_request  type ref to if_http_request,
          mo_response type ref to if_http_response.

    methods:
      transform abstract
        changing
          co_writer type ref to cl_sxml_string_writer .

endclass.

class lcl_count_writer definition inheriting from lcl_abstract_writer.

  public section.
    methods:
      constructor
        importing
          i_count     type i
          io_request  type ref to if_http_request
          io_response type ref to if_http_response.

  protected section.
    methods:
      transform redefinition.

  private section.
    data: m_count type i.

endclass.

class lcl_table_writer definition inheriting from lcl_abstract_writer.

  public section.
    methods:
      constructor
        importing
          it_table    type any
          io_request  type ref to if_http_request
          io_response type ref to if_http_response.

  protected section.
    methods:
      transform redefinition.

  private section.
    data: mr_table     type ref to data.

endclass.

class lcl_struct_writer definition inheriting from lcl_abstract_writer.

  public section.
    methods:
      constructor
        importing
          is_struct   type any
          io_request  type ref to if_http_request
          io_response type ref to if_http_response.

  protected section.
    methods:
      transform redefinition.

  private section.
    data: mr_struct type ref to data.

endclass.

class lcx_error implementation.

  method constructor.

    super->constructor( textid = textid previous = previous ).
    m_msg = msg.
    m_text = text.

  endmethod.

  method raise_syst.

    raise exception type lcx_error
      exporting
        msg = value symsg( msgty = sy-msgty
                           msgid = sy-msgid
                           msgno = sy-msgno
                           msgv1 = sy-msgv1
                           msgv2 = sy-msgv2
                           msgv3 = sy-msgv3
                           msgv4 = sy-msgv4 ).

  endmethod.

  method raise.

    raise exception type lcx_error
      exporting
        text = i_text.

  endmethod.

  method raise_exception.

    raise exception type lcx_error
      exporting
        previous = io_exception.

  endmethod.

  method get_text.

    result = cond #( when m_msg is not initial then get_message_text(  )
                     when m_text is not initial then m_text
                     else super->get_text( )  ).

  endmethod.

  method get_message_text.

    message id m_msg-msgid
            type m_msg-msgty
            number m_msg-msgno
            with m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
            into r_text.

  endmethod.

endclass.

class lcl_abstract_writer implementation.

  method constructor.

    mo_request = io_request.
    mo_response = io_response.

  endmethod.

  method execute.

    data: writer type ref to cl_sxml_string_writer.

    data(content_type) = mo_request->get_content_type( ).

    case content_type.
      when 'application/xml'.

        writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_xml10 ).

      when 'application/json'.

        writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).

      when others.

        mo_response->set_status( code   = 404
                                 reason = |Content-Type { content_type } not supported| ).

    endcase.

    transform( changing co_writer = writer ).

    mo_response->set_data( writer->get_output( ) ).
    mo_response->set_content_type( content_type ).
    mo_response->set_status( code   = 200
                             reason = |ok| ).

  endmethod.

endclass.

class lcl_count_writer implementation.

  method constructor.

    super->constructor( io_request  = io_request
                        io_response = io_response ).
    m_count = i_count.

  endmethod.

  method transform.

    call transformation id source count = m_count
                           result xml co_writer.

  endmethod.

endclass.

class lcl_table_writer implementation.

  method constructor.

    super->constructor( io_request  = io_request
                        io_response = io_response ).
    mr_table = ref #( it_table ).

  endmethod.

  method transform.

    field-symbols: <table> type any table.

    assign mr_table->* to <table>.
    assert sy-subrc = 0.

    call transformation id source table = <table>
                           result xml co_writer.

  endmethod.

endclass.

class lcl_struct_writer implementation.

  method constructor.

    super->constructor( io_request  = io_request
                        io_response = io_response ).
    mr_struct = ref #( is_struct ).

  endmethod.

  method transform.

    field-symbols: <data> type any.

    assign mr_struct->* to <data>.
    assert sy-subrc = 0.

    call transformation id source item = <data>
                           result xml co_writer.

  endmethod.

endclass.
