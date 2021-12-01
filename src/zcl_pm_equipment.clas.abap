CLASS zcl_pm_equipment DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_equipment_id TYPE STANDARD TABLE OF equnr WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_equipment_header,
        equipment      TYPE equnr,
        equipment_desc TYPE ktx01,
        tec_location   TYPE tplnr,
      END OF ts_equipment_header .
    TYPES:
      tt_equipment_header TYPE STANDARD TABLE OF ts_equipment_header WITH DEFAULT KEY .

    METHODS search_equipment
      IMPORTING
        !it_equipment       TYPE STANDARD TABLE OPTIONAL
        !it_tec_location    TYPE STANDARD TABLE OPTIONAL
        !it_center          TYPE STANDARD TABLE OPTIONAL
      RETURNING
        VALUE(rt_equipment) TYPE tt_equipment_id .
    METHODS get_header_data
      IMPORTING
        !it_equipment              TYPE STANDARD TABLE
        !iv_langu                  TYPE langu DEFAULT sy-langu
      RETURNING
        VALUE(rt_equipment_header) TYPE tt_equipment_header .
protected section.

  data MT_EQUIPMENT_HEADER type TT_EQUIPMENT_HEADER .
private section.
ENDCLASS.



CLASS ZCL_PM_EQUIPMENT IMPLEMENTATION.


  METHOD get_header_data.

    DATA: lr_equipment TYPE RANGE OF equnr.

*   Hay que especificar algun equipo
    CHECK it_equipment IS NOT INITIAL.

    lr_equipment = CORRESPONDING #( it_equipment ).

    SELECT a~equnr AS equipment
*       ,a~tplnr as tec_location                                                    "Santiago 20.10.21
       ,( CASE WHEN a~hequi <> ' ' THEN a~hequi ELSE a~tplnr END ) as tec_location  "Santiago 20.10.21
       ,b~eqktx AS equipment_desc
      FROM v_equi AS a INNER JOIN eqkt AS b ON a~equnr = b~equnr
      INTO TABLE @DATA(lt_equi)
      WHERE a~equnr IN @lr_equipment AND
            b~spras EQ @iv_langu.

    mt_equipment_header = CORRESPONDING #( lt_equi ).

    rt_equipment_header = mt_equipment_header.

  ENDMETHOD.


  METHOD search_equipment.

    DATA: lr_equnr TYPE RANGE OF equnr,
          lr_tplnr TYPE RANGE OF tplnr,
          lr_werks TYPE RANGE OF werks_d.

    lr_equnr  = CORRESPONDING #( it_equipment ).
    lr_tplnr = CORRESPONDING #( it_tec_location ).
    lr_werks = CORRESPONDING #( it_center ).

    SELECT equnr
     FROM v_equi
     INTO TABLE @DATA(lt_equi)
       WHERE equnr IN @lr_equnr AND
             tplnr IN @lr_tplnr AND
             swerk IN @lr_werks.


    rt_equipment = VALUE #( FOR <a> IN lt_equi ( <a>-equnr ) ) .


  ENDMETHOD.
ENDCLASS.
