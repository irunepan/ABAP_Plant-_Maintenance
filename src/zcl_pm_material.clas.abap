CLASS zcl_pm_material DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_material_movement_header,
        header_text TYPE string,
      END OF ts_material_movement_header .
    TYPES:
      BEGIN OF ts_material_header,
        material      TYPE matnr,
        material_desc TYPE matxt,
        unit          TYPE meins,
      END OF ts_material_header .
    TYPES:
      tt_material_header TYPE STANDARD TABLE OF ts_material_header WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_material_movement_item,
        material  TYPE matnr,
        center    TYPE werks_d,
        storage   TYPE lgort_d,
        move_type TYPE bwart,
        order     TYPE aufnr,
        quantity  TYPE menge_d,
        unit      TYPE meins,
        user      TYPE wempf,
        kostl     TYPE kostl,
      END OF ts_material_movement_item .
    TYPES:
      tt_material_movement_item TYPE STANDARD TABLE OF ts_material_movement_item WITH DEFAULT KEY .

    METHODS get_material_header
      IMPORTING
        !it_material              TYPE STANDARD TABLE
        !iv_complete_data         TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_material_hedaer) TYPE tt_material_header .
    METHODS complete_header_data .
    METHODS constructor
      IMPORTING
        !iv_langu TYPE langu DEFAULT sy-langu .
    METHODS create_material_movement
      IMPORTING
        !is_material_movement_header TYPE ts_material_movement_header
        !it_material_movement_item   TYPE tt_material_movement_item
      EXPORTING
        !ev_materialdocument         TYPE mblnr
        !ev_matdocumentyear          TYPE mjahr
        !et_return                   TYPE bapiret2_t .
    METHODS cancel_material_movement
      IMPORTING
        !iv_doc    TYPE mblnr
        !iv_year   TYPE mjahr
      EXPORTING
        !et_return TYPE bapiret2_t .
protected section.

  data MT_MATERIAL_HEADER type TT_MATERIAL_HEADER .
  data MV_LANGU type LANGU .
private section.
ENDCLASS.



CLASS ZCL_PM_MATERIAL IMPLEMENTATION.


  METHOD cancel_material_movement.

    DATA ls_headret TYPE bapi2017_gm_head_ret.

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument = iv_doc
        matdocumentyear  = iv_year
      IMPORTING
        goodsmvt_headret = ls_headret
      TABLES
        return           = et_return.

    READ TABLE et_return TRANSPORTING NO FIELDS WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.



  ENDMETHOD.


  METHOD complete_header_data.

    DATA: lr_material TYPE RANGE OF matnr.

    lr_material = VALUE #( FOR <a> IN mt_material_header ( sign = 'I' option = 'EQ' low = <a>-material ) ).

    SELECT matnr AS material, maktx AS material_desc
      FROM makt
      INTO TABLE @DATA(lt_makt)
      WHERE matnr IN @lr_material AND
            spras EQ @mv_langu.

    LOOP AT mt_material_header ASSIGNING FIELD-SYMBOL(<ls_material_header>).
      READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt>) WITH KEY material = <ls_material_header>-material.
      IF sy-subrc IS INITIAL.
        <ls_material_header>-material_desc = <ls_makt>-material_desc.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    mv_langu = iv_langu.

  ENDMETHOD.


  METHOD create_material_movement.

    DATA: ls_bapi_header TYPE bapi2017_gm_head_01,
          lt_bapi_item   TYPE TABLE OF bapi2017_gm_item_create.

    ls_bapi_header-pstng_date   = sy-datum.
    ls_bapi_header-doc_date = sy-datum.
    ls_bapi_header-header_txt = is_material_movement_header-header_text.

    LOOP AT it_material_movement_item ASSIGNING FIELD-SYMBOL(<ls_mat_movement_item>).
      APPEND INITIAL LINE TO lt_bapi_item ASSIGNING FIELD-SYMBOL(<ls_bapi_item>).
      <ls_bapi_item> = CORRESPONDING #( <ls_mat_movement_item> MAPPING plant = center orderid = order stge_loc = storage gr_rcpt = user costcenter = kostl ).
      <ls_bapi_item>-entry_qnt      = <ls_mat_movement_item>-quantity.
      <ls_bapi_item>-entry_uom      = <ls_mat_movement_item>-unit.
      <ls_bapi_item>-entry_uom_iso  = <ls_mat_movement_item>-unit.
    ENDLOOP.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_bapi_header
        goodsmvt_code    = '03'
      IMPORTING
        materialdocument = ev_materialdocument
        matdocumentyear  = ev_matdocumentyear
      TABLES
        goodsmvt_item    = lt_bapi_item
        return           = et_return.

    IF ev_materialdocument IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'    .

    ENDIF.
  ENDMETHOD.


  METHOD get_material_header.

    DATA: lr_material TYPE RANGE OF matnr.

    lr_material = CORRESPONDING #( it_material ).

    SELECT matnr AS material, meins AS unit
      INTO TABLE @DATA(lt_mara)
      FROM mara
      WHERE matnr IN @lr_material.

    CHECK lt_mara IS NOT INITIAL.

    mt_material_header = CORRESPONDING #( lt_mara ).

    IF iv_complete_data IS NOT INITIAL.
      complete_header_data( ).
    ENDIF.

    rt_material_hedaer = CORRESPONDING #( mt_material_header ).

  ENDMETHOD.
ENDCLASS.
