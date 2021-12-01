class ZCL_PM_CONSTANTS definition
  public
  final
  create public .

public section.

  class-methods OBTENER_CONSTANTES_EN_RANGES
    importing
      !IV_PATRON type ZPM_E_CONSTANT
      !IV_OPTION type ANY default 'EQ'
      !IV_SIGN type ANY default 'I'
    changing
      !CT_RANGES type STANDARD TABLE .
  class-methods OBTENER_CONSTANTES
    importing
      !IV_CONSTANTE type ZPM_E_CONSTANT
    returning
      value(RV_VALOR) type ZPM_E_CONS_VALUE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PM_CONSTANTS IMPLEMENTATION.


  METHOD OBTENER_CONSTANTES.

    DATA ld_valor TYPE zdso_e_cons_value.

    SELECT SINGLE value FROM zpm_t_constants INTO ld_valor
           WHERE constant = iv_constante.
    IF sy-subrc = 0.
      rv_valor = ld_valor.
    ENDIF.

  ENDMETHOD.


  METHOD OBTENER_CONSTANTES_EN_RANGES.


* Uso: Rellena la tabla tipo ranges T_RANGES con los valores en la tabla ZZT_CA0401
* que cumplan ID = iv_id y CONSTANTE = iv_patron, donde iv_patron puede ser una expresión
* del tipo 'BUKRS%', por ejemplo.


* Uso: Rellena la tabla tipo ranges T_RANGES con los valores en la tabla ZZT_CA0401
* que cumplan ID = iv_id y CONSTANTE = iv_patron, donde iv_patron puede ser una expresión
* del tipo 'BUKRS%', por ejemplo.
    DATA it_valores        TYPE TABLE OF zdso_e_cons_value.
    DATA d_valor           TYPE zdso_e_cons_value.
    DATA: dref             TYPE REF TO data.
    FIELD-SYMBOLS: <wa>    TYPE any,
                   <campo> TYPE any.

* Obtener los valores de la tabla de constantes
    SELECT value FROM zpm_t_constants INTO TABLE it_valores
           WHERE constant LIKE iv_patron.

    IF sy-subrc = 0.
*   Crear una Work area del mismo tipo que una linea del ranges pasado por parámetro
      CREATE DATA dref LIKE LINE OF ct_ranges.
      ASSIGN dref->* TO <wa>.

*   Asignamos y llenamos campo Sign
      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <wa> TO <campo>.
      IF sy-subrc = 0.
        <campo> = iv_sign.
      ENDIF.

*   Asignamos y llenamos campo Option
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <wa> TO <campo>.
      IF sy-subrc = 0.
        <campo> = iv_option.
      ENDIF.

*   Asignamos campo Low
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <wa> TO <campo>.
      IF sy-subrc = 0.

*     Asignar todos los valores y llenar la tabla ranges.
        LOOP AT it_valores INTO d_valor.

          <campo> = d_valor.
          APPEND <wa> TO ct_ranges.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
