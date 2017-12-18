*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

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
