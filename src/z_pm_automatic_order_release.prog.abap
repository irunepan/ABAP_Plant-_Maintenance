*&---------------------------------------------------------------------*
*& Report Z_PM_AUTOMATIC_ORDER_RELEASE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_pm_automatic_order_release.

SELECTION-SCREEN BEGIN OF BLOCK sbl_001.
PARAMETERS: sp_test TYPE abap_bool AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK sbl_001.

INITIALIZATION.

  DATA: lv_date_notification         TYPE datum,
        lv_time_notification         TYPE uzeit,
        lv_order_selection_date_to   TYPE begda,
        lv_order_selection_date_from TYPE begda,
        lt_order_class               TYPE RANGE OF aufart,
        lr_status                    TYPE RANGE OF j_status,
        lr_erdat                     TYPE RANGE OF erdat,
        lr_order                     TYPE RANGE OF aufnr,
        lr_objnr                     TYPE RANGE OF objnr,
        lt_return                    TYPE bapiret2_t.

  DATA(lo_order) = NEW zcl_pm_order( ).

END-OF-SELECTION.

* Se buscan las ordenes creadas en el ultimo año
  lv_order_selection_date_to = sy-datum.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lv_order_selection_date_to
      days      = 0
      months    = 0
      signum    = '-'
      years     = 1
    IMPORTING
      calc_date = lv_order_selection_date_from.

  lr_erdat = VALUE #( ( sign = 'I' option = 'BT' low = lv_order_selection_date_from high = lv_order_selection_date_to ) ).
  lr_status = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>ms_back_work_status-noti ) ).

  DATA(lv_hour) = CONV thour(  zcl_pm_constants=>obtener_constantes( 'JOB_ORDER_RELEASE_HOUR' ) ).
  DATA(lv_release_pernr) = zcl_pm_constants=>obtener_constantes( 'JOB_ORDER_RELEASE_PERNR' ).
  DATA(lv_enc_order_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).

* Tabla con los interlocutores
  DATA(lt_partners) = VALUE zcl_pm_order=>tt_order_partner( ( employee_code = |{ CONV pernr_d( lv_release_pernr ) ALPHA = IN }| parvw = lv_enc_order_parvw ) ).

* Se calcula la hora hasta la cual todas las ordenes tienen que estar cerradas
  lv_hour = lv_hour * -1.
  CALL FUNCTION 'CATT_ADD_TO_TIME'
    EXPORTING
      idate = sy-datum
      itime = sy-uzeit
      stdaz = lv_hour
    IMPORTING
      edate = lv_date_notification
      etime = lv_time_notification.

* Excluir las ordenes de preventivo
  lt_order_class = VALUE #( ( sign = 'E' option = 'EQ' low = zcl_pm_order=>ms_back_order_type-preventive ) ).

  DATA(lt_order_id) = lo_order->search_orders( it_order_status = lr_status
                                               it_order_class = lt_order_class
                                               it_date_creation = lr_erdat ).

  lr_order = VALUE #( FOR <b> IN lt_order_id ( sign = 'I' option = 'EQ' low = <b> ) ).

  DATA(lt_order_notifications) = lo_order->get_order_notifications( it_order = lr_order ).

* Se borran las que no son finales
  DELETE lt_order_notifications WHERE is_final IS INITIAL.

  CHECK lt_order_notifications IS NOT INITIAL.

* Se buscan los datos de cabecera de las ordenes con notificacion final
  lr_order = VALUE #( FOR <a> IN lt_order_notifications ( sign = 'I' option = 'EQ' low = <a>-order ) ).
  SORT lr_order BY low DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lr_order.
  DATA(lt_order_header) = lo_order->get_order_header( it_order = lr_order ).

* Se buscan los interlocutores de las ordenes
  lr_objnr = VALUE #( FOR <c> IN lt_order_header ( sign = 'I' option = 'EQ' low = <c>-objnr ) ).
  DATA(lt_order_partner) = lo_order->get_order_partners( it_objnr = lr_objnr ).

* Se borran los que no sean encargados de produccion
  DELETE lt_order_partner WHERE parvw NE lv_enc_order_parvw.

* Se agrupan las ordenes que tienen encargado de produccion
  lr_objnr = VALUE #( FOR <d> IN lt_order_partner ( sign = 'I' option = 'EQ' low = <d>-objnr ) ).

* Se borran las ordenes con encargado de produccion
  IF lr_objnr IS NOT INITIAL.
    DELETE lt_order_header WHERE objnr NOT IN lr_objnr.
  ENDIF.

* En este rango estan las ordenes sin encargado de produccion
  lr_order = VALUE #( FOR <c> IN lt_order_header ( sign = 'I' option = 'EQ' low = <c>-order ) ).

* Se borran las notificaciones de las ordenes que tienen encargado de produccion
  IF lr_order IS NOT INITIAL.
    DELETE lt_order_notifications WHERE order NOT IN lr_order.
  ENDIF.

  LOOP AT lt_order_notifications ASSIGNING FIELD-SYMBOL(<ls_order_notification>).

    WRITE |Procesamiento orden: { <ls_order_notification>-order }|.
    WRITE |Fecha notificacion final: { <ls_order_notification>-creation_date }|.
    WRITE |Hora notificacion final: { <ls_order_notification>-creation_hour }|.
    WRITE /.

    READ TABLE lt_order_header INTO DATA(ls_order_header) WITH KEY order = <ls_order_notification>-order.

*   Si el dia que se creo la notificacion final es antes de la parametrizada se libera
    IF <ls_order_notification>-creation_date LE lv_date_notification.
      WRITE |Orden a liberar|.
      WRITE /.

      IF sp_test IS INITIAL.
        lt_return = lo_order->add_partner_to_order( is_order_header = ls_order_header
                                                    it_partners = lt_partners ).
      ENDIF.

*   Si la fecha es la misma se compara las horas
    ELSEIF <ls_order_notification>-creation_date EQ lv_date_notification AND
           <ls_order_notification>-creation_hour LE lv_time_notification.
      WRITE |Orden a liberar|.
      WRITE /.

      IF sp_test IS INITIAL.
        lt_return = lo_order->add_partner_to_order( is_order_header = ls_order_header
                                                    it_partners = lt_partners ).
      ENDIF.

    ELSE.
      WRITE |Orden no procesada|.
      WRITE /.
    ENDIF.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      WRITE <ls_return>-message.
      WRITE /.
    ENDLOOP.

    CLEAR lt_return[].
    WRITE /.

  ENDLOOP.
