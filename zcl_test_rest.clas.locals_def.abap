*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section


class lcx_error definition final
                inheriting from cx_static_check.

  public section.
    class-methods:
      raise_syst
        raising
          lcx_error,

      raise
        importing
          i_text type string
        raising
          lcx_error,

      raise_exception
        importing
          io_exception type ref to cx_root
        raising
          lcx_error.

    methods:
      constructor
        importing
          textid   like textid optional
          previous like previous optional
          msg      type symsg optional
          text     type csequence optional,

      get_text redefinition.

  private section.

    methods:
      get_message_text
        returning
          value(r_text) type string.

    data: m_msg  type symsg,
          m_text type string.

endclass.
