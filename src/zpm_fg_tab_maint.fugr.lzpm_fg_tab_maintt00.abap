*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 19.03.2021 at 11:45:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPM_T_CONSTANTS.................................*
DATA:  BEGIN OF STATUS_ZPM_T_CONSTANTS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPM_T_CONSTANTS               .
CONTROLS: TCTRL_ZPM_T_CONSTANTS
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZPM_T_STATUS....................................*
DATA:  BEGIN OF STATUS_ZPM_T_STATUS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPM_T_STATUS                  .
CONTROLS: TCTRL_ZPM_T_STATUS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPM_T_CONSTANTS               .
TABLES: *ZPM_T_STATUS                  .
TABLES: ZPM_T_CONSTANTS                .
TABLES: ZPM_T_STATUS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
