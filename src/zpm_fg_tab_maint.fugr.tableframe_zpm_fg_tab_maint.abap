*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPM_FG_TAB_MAINT
*   generation date: 19.03.2021 at 11:45:58
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPM_FG_TAB_MAINT   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
