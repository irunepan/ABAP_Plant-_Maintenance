class ZCL_PM_UTILIDADES definition
  public
  final
  create public .

public section.

  class-methods CONVERT_MSG_TO_UI5
    importing
      !IT_RETURN type BAPIRET2_T
    returning
      value(RT_LOG) type ZIF_PM_DATA=>TT_LOG .
  class-methods CONVERT_CUNIT_OUT
    importing
      !IV_UNIT type MEINS
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RV_UNIT) type MEINS .
  class-methods CONVERT_CUNIT_IN
    importing
      !IV_UNIT type MEINS
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RV_UNIT) type MEINS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PM_UTILIDADES IMPLEMENTATION.


  METHOD convert_cunit_in.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = iv_unit
        language       = iv_langu
      IMPORTING
        output         = rv_unit
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

  ENDMETHOD.


  METHOD convert_cunit_out.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = iv_unit
        language       = iv_langu
      IMPORTING
        output         = rv_unit
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

  ENDMETHOD.


  METHOD convert_msg_to_ui5.
    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      APPEND INITIAL LINE TO rt_log ASSIGNING FIELD-SYMBOL(<fs_log>).
      <fs_log> = CORRESPONDING #( <fs_return> ).
      CASE <fs_return>-type.
        WHEN zif_pm_data=>cs_msg_type-error.
          <fs_log>-ui5_type = zif_pm_data=>cs_ui5_msg_type-error.
        WHEN zif_pm_data=>cs_msg_type-warning.
          <fs_log>-ui5_type = zif_pm_data=>cs_ui5_msg_type-warning.
        WHEN zif_pm_data=>cs_msg_type-success.
          <fs_log>-ui5_type = zif_pm_data=>cs_ui5_msg_type-success.
        WHEN zif_pm_data=>cs_msg_type-info.
          <fs_log>-ui5_type = zif_pm_data=>cs_ui5_msg_type-info.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
