&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mg               ORACLE
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**
**   Programa: DVV0090
**   Objetivo: Extra??o de dados do EMS
**      Autor: Paulo Roberto Barth NewTechs
**       Data: 04/06/2013
**
*******************************************************************************/
&SCOPED-DEFINE base emsfnd
    DEF BUFFER impressora             FOR {&base}.impressora.
    DEF BUFFER tip_imprsor            FOR {&base}.tip_imprsor.
    DEF BUFFER configur_layout_impres FOR {&base}.configur_layout_impres.
    DEF BUFFER configur_tip_imprsor   FOR {&base}.configur_tip_imprsor.
    DEF BUFFER servid_exec_imprsor    FOR {&base}.servid_exec_imprsor.
    DEF BUFFER prog_dtsul             FOR {&base}.prog_dtsul.
  /*  DEF BUFFER empresa                FOR {&base}.empresa. */
    DEF BUFFER usuar_mestre           FOR {&base}.usuar_mestre.
    DEF BUFFER layout_impres_padr     FOR {&base}.layout_impres_padr.
    DEF BUFFER ped_exec               FOR {&base}.ped_exec.
    DEF BUFFER imprsor_usuar          FOR {&base}.imprsor_usuar.
    DEF BUFFER layout_impres          FOR {&base}.layout_impres.
    DEF BUFFER procedimento           FOR {&base}.procedimento.
    DEF BUFFER modul_dtsul            FOR {&base}.modul_dtsul.

{include/i-prgvrs.i dvv0199 2.06.00.002}
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-PAR
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Include Com as Vari veis Globais */
{utp/ut-glob.i}
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino                     as integer
    field arquivo                     as char
    field usuario                     as char
    field data-exec                   as date
    field hora-exec                   as integer
    FIELD dt-ctbl-ini                 AS DATE
    FIELD dt-ctbl-fim                 AS DATE
    FIELD c-cta-ctbl-ini              AS CHAR
    FIELD c-cta-ctbl-fim              AS CHAR
    FIELD l-val-neg                   AS LOG
    FIELD l-realizacao-dvv-me         AS LOG
    FIELD l-realizacao-dvv-mi         AS LOG
    FIELD l-provisao-dvv-me           AS LOG
    FIELD l-provisao-dvv-mi           AS LOG
	FIELD l-provisao-dvv-rem          AS LOG
    FIELD l-previa-provisoes          AS LOG // Barth.alter.001
    FIELD l-estorno                   AS LOG
    FIELD c-estab-ini                 AS CHAR
    FIELD c-estab-fim                 AS CHAR
    FIELD c-unid-negoc-ini            AS CHAR
    FIELD c-unid-negoc-fim            AS CHAR
    FIELD l-atualiza-bi               AS LOGICAL
    FIELD l-imprime-bi                AS LOGICAL
    FIELD l-remessa-venda-estoq       AS LOGICAL
    FIELD l-despesa-nao-rast          AS LOGICAL
    FIELD l-fech-previo               AS LOGICAL
    FIELD i-mo-codigo                 AS INT
    FIELD de-cotacao                  AS DEC.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
DEF VAR c-periodo AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cotacao pedido-compr estabelec item-fornec

/* Definitions for FRAME f-pg-par                                       */
&Scoped-define FIELDS-IN-QUERY-f-pg-par cotacao.mo-codigo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-f-pg-par cotacao.mo-codigo 
&Scoped-define ENABLED-TABLES-IN-QUERY-f-pg-par cotacao
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-f-pg-par cotacao
&Scoped-define QUERY-STRING-f-pg-par FOR EACH cotacao SHARE-LOCK
&Scoped-define OPEN-QUERY-f-pg-par OPEN QUERY f-pg-par FOR EACH cotacao SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-par cotacao
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-par cotacao


/* Definitions for FRAME f-pg-sel                                       */
&Scoped-define QUERY-STRING-f-pg-sel FOR EACH pedido-compr SHARE-LOCK, ~
      EACH estabelec WHERE TRUE /* Join to pedido-compr incomplete */ SHARE-LOCK, ~
      EACH item-fornec WHERE TRUE /* Join to pedido-compr incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-f-pg-sel OPEN QUERY f-pg-sel FOR EACH pedido-compr SHARE-LOCK, ~
      EACH estabelec WHERE TRUE /* Join to pedido-compr incomplete */ SHARE-LOCK, ~
      EACH item-fornec WHERE TRUE /* Join to pedido-compr incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-sel pedido-compr estabelec item-fornec
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-sel pedido-compr
&Scoped-define SECOND-TABLE-IN-QUERY-f-pg-sel estabelec
&Scoped-define THIRD-TABLE-IN-QUERY-f-pg-sel item-fornec


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-config-impr ~
bt-arquivo c-arquivo rs-execucao 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cotacao.mo-codigo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE c_descricao_moeda AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .88.

DEFINE VARIABLE fi-cotacao AS DECIMAL FORMAT "->>,>>9.99999999":U INITIAL 0 
     LABEL "Cota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE l-despesa-nao-rast AS LOGICAL INITIAL no 
     LABEL "Rateio Despesas NÆo Rastre veis" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE l-estorno AS LOGICAL INITIAL no 
     LABEL "Detalha Estorno" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-provisao-dvv-me AS LOGICAL INITIAL no 
     LABEL "ProvisÆo DVV ME (DEX)" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE l-provisao-dvv-mi AS LOGICAL INITIAL no 
     LABEL "ProvisÆo DVV MI" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE l-previa-provisoes AS LOGICAL INITIAL no // Barth.002
     LABEL "Pr‚via das Provisäes" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE l-realizacao-dvv-me AS LOGICAL INITIAL no 
     LABEL "Realiza‡Æo DVV ME (DEX)" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE l-realizacao-dvv-mi AS LOGICAL INITIAL no 
     LABEL "Realiza‡Æo DVV MI" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE l-remessa-venda-estoq AS LOGICAL INITIAL no 
     LABEL "Rateio Remessas x Venda Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .83 NO-UNDO.

DEFINE VARIABLE tg-val-neg AS LOGICAL INITIAL yes 
     LABEL "Apresentar valor CR negativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .83 NO-UNDO.

DEFINE VARIABLE c-cta-ctbl-fim AS CHARACTER FORMAT "99999999":U INITIAL "99999999" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-cta-ctbl-ini AS CHARACTER FORMAT "99999999":U INITIAL "00000000" 
     LABEL "Conta Cont bil" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-unid-negoc-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-unid-negoc-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Unid. Negoc" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE VARIABLE dt-ctbl-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt-ctbl-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/21 
     LABEL "Data Contabiliza‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE tg-bi AS LOGICAL INITIAL no 
     LABEL "Atualizar BI" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE tg-bi-imp AS LOGICAL INITIAL no 
     LABEL "Imprimir BI" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE tg-fech-previo AS LOGICAL INITIAL no 
     LABEL "Fechamento Pr‚vio" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 12.25
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 12
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 12
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f-pg-par FOR 
      cotacao SCROLLING.

DEFINE QUERY f-pg-sel FOR 
      pedido-compr, 
      estabelec, 
      item-fornec SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 15.33 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 15.33 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 15.33 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 14.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-1 AT ROW 15.08 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15.54
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     tg-val-neg AT ROW 1.75 COL 4
     l-realizacao-dvv-me AT ROW 2.75 COL 4 WIDGET-ID 2
     l-realizacao-dvv-mi AT ROW 3.75 COL 4 WIDGET-ID 4
     l-provisao-dvv-me AT ROW 4.75 COL 4 WIDGET-ID 6
     l-provisao-dvv-mi AT ROW 5.75 COL 4 WIDGET-ID 8
     l-previa-provisoes AT ROW 6.75 COL 4 WIDGET-ID 26
     l-estorno AT ROW 7.63 COL 4 WIDGET-ID 10
     l-remessa-venda-estoq AT ROW 8.54 COL 4 WIDGET-ID 14
     l-despesa-nao-rast AT ROW 9.46 COL 4 WIDGET-ID 12
     cotacao.mo-codigo AT ROW 10.63 COL 8.86 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 3.86 BY .88
     c_descricao_moeda AT ROW 10.63 COL 13.43 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     fi-cotacao AT ROW 11.83 COL 8.86 COLON-ALIGNED WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.04
         SIZE 77 BY 11.96
         FONT 1.

DEFINE FRAME f-pg-sel
     dt-ctbl-ini AT ROW 2 COL 15 COLON-ALIGNED
     dt-ctbl-fim AT ROW 2 COL 45 COLON-ALIGNED NO-LABEL
     c-cta-ctbl-ini AT ROW 3 COL 15 COLON-ALIGNED
     c-cta-ctbl-fim AT ROW 3 COL 45 COLON-ALIGNED NO-LABEL
     c-estab-ini AT ROW 4 COL 15 COLON-ALIGNED WIDGET-ID 2
     c-estab-fim AT ROW 4 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-unid-negoc-ini AT ROW 5 COL 15 COLON-ALIGNED WIDGET-ID 14
     c-unid-negoc-fim AT ROW 5 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     tg-bi AT ROW 6.5 COL 17 WIDGET-ID 6
     tg-bi-imp AT ROW 7.25 COL 17 WIDGET-ID 20
     tg-fech-previo AT ROW 8 COL 17 WIDGET-ID 22
     IMAGE-27 AT ROW 2 COL 38
     IMAGE-28 AT ROW 2 COL 44
     IMAGE-29 AT ROW 3 COL 44
     IMAGE-30 AT ROW 3 COL 38
     IMAGE-31 AT ROW 4 COL 44 WIDGET-ID 8
     IMAGE-32 AT ROW 4 COL 38 WIDGET-ID 10
     IMAGE-33 AT ROW 5 COL 44 WIDGET-ID 16
     IMAGE-34 AT ROW 5 COL 38 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.83
         SIZE 76.86 BY 11.71
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Relat¢rio de DVV"
         HEIGHT             = 15.54
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN c_descricao_moeda IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cotacao.mo-codigo IN FRAME f-pg-par
   1                                                                    */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR IMAGE IMAGE-27 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-28 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-29 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-30 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-31 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-32 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-33 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-34 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-par
/* Query rebuild information for FRAME f-pg-par
     _TblList          = "mg.cotacao"
     _Query            is OPENED
*/  /* FRAME f-pg-par */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _TblList          = "mg.pedido-compr,mg.estabelec WHERE mg.pedido-compr ...,mg.item-fornec WHERE mg.pedido-compr ..."
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Relat¢rio de DVV */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Relat¢rio de DVV */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo C-Win
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr C-Win
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
    run pi-executar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp C-Win
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME cotacao.mo-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cotacao.mo-codigo C-Win
ON F5 OF cotacao.mo-codigo IN FRAME f-pg-par /* Moeda */
DO:
    //assign l-implanta = yes.
    {include/zoom.i &prog-zoom=adzoom/z01ad178.w
                    &tabela=cotacao
                    &atributo=mo-codigo}
                    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cotacao.mo-codigo C-Win
ON LEAVE OF cotacao.mo-codigo IN FRAME f-pg-par /* Moeda */
DO:


      {include/leave.i &tabela=moeda 
                     &where="moeda.mo-codigo = input frame {&frame-name} cotacao.mo-codigo"  
                     &atributo-ref=descricao
                     &variavel-ref=c_descricao_moeda}
                     
          ASSIGN c-periodo =
          SUBSTRING(dt-ctbl-ini:SCREEN-VALUE IN FRAME f-pg-sel,7,4) +
          SUBSTRING(dt-ctbl-ini:SCREEN-VALUE IN FRAME f-pg-sel,4,2).

          FIND FIRST cotacao NO-LOCK
          WHERE cotacao.mo-codigo = INT(cotacao.mo-codigo:SCREEN-VALUE IN FRAME f-pg-par)
            AND cotacao.ano-periodo = c-periodo NO-ERROR.
          IF AVAIL cotacao THEN
              ASSIGN fi-cotacao:SCREEN-VALUE IN FRAME f-pg-par = STRING(cotacao.cota-media).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cotacao.mo-codigo C-Win
ON MOUSE-SELECT-DBLCLICK OF cotacao.mo-codigo IN FRAME f-pg-par /* Moeda */
DO:
  apply "F5" to self.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-fech-previo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-fech-previo C-Win
ON VALUE-CHANGED OF tg-fech-previo IN FRAME f-pg-sel
DO:
    /* Mantém o comportamento de habilitar/desabilitar a flag l-previa-provisoes */
    IF tg-fech-previo:CHECKED IN FRAME f-pg-sel THEN
        ASSIGN l-previa-provisoes:SENSITIVE IN FRAME f-pg-par = YES
               l-previa-provisoes:CHECKED   IN FRAME f-pg-par = YES.
    ELSE
        ASSIGN l-previa-provisoes:SENSITIVE IN FRAME f-pg-par = NO
               l-previa-provisoes:CHECKED   IN FRAME f-pg-par = NO.

    /* NOVO: se Atualiza BI + Fechamento Prévio = YES, forçar Imprimir BI = YES */
    IF tg-fech-previo:SCREEN-VALUE IN FRAME f-pg-sel = "YES"
       AND tg-bi:SCREEN-VALUE       IN FRAME f-pg-sel = "YES" THEN
        ASSIGN tg-bi-imp:SCREEN-VALUE IN FRAME f-pg-sel = "YES".
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tg-bi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-bi C-Win
ON VALUE-CHANGED OF tg-bi IN FRAME f-pg-sel /* Atualizar BI */
DO:
    RUN pi-esconde.

    IF tg-bi:SCREEN-VALUE IN FRAME f-pg-sel = "YES" THEN DO:
        /* ATENÇÃO: não travamos mais o tg-fech-previo aqui. */
        ASSIGN 
           tg-fech-previo:SENSITIVE               IN FRAME f-pg-sel = YES
           tg-val-neg:SCREEN-VALUE                IN FRAME f-pg-par = "YES"
           tg-val-neg:SENSITIVE                   IN FRAME f-pg-par = NO
           l-realizacao-dvv-me:SCREEN-VALUE       IN FRAME f-pg-par = "YES"
           l-realizacao-dvv-me:SENSITIVE          IN FRAME f-pg-par = NO
           l-realizacao-dvv-mi:SCREEN-VALUE       IN FRAME f-pg-par = "YES"
           l-realizacao-dvv-mi:SENSITIVE          IN FRAME f-pg-par = NO
           l-provisao-dvv-me:SCREEN-VALUE         IN FRAME f-pg-par = "YES"
           l-provisao-dvv-me:SENSITIVE            IN FRAME f-pg-par = NO
           l-provisao-dvv-mi:SCREEN-VALUE         IN FRAME f-pg-par = "YES"
           l-provisao-dvv-mi:SENSITIVE            IN FRAME f-pg-par = NO
           /* l-previa-provisoes agora é controlada pelo tg-fech-previo */
           l-estorno:SCREEN-VALUE                 IN FRAME f-pg-par = "YES"
           l-estorno:SENSITIVE                    IN FRAME f-pg-par = NO
           l-remessa-venda-estoq:SCREEN-VALUE     IN FRAME f-pg-par = "NO"
           l-remessa-venda-estoq:SENSITIVE        IN FRAME f-pg-par = NO
           l-despesa-nao-rast:SCREEN-VALUE        IN FRAME f-pg-par = "NO"
           l-despesa-nao-rast:SENSITIVE           IN FRAME f-pg-par = NO
           c-cta-ctbl-ini:SCREEN-VALUE            IN FRAME f-pg-sel = "00000000"
           c-cta-ctbl-ini:SENSITIVE               IN FRAME f-pg-sel = NO
           c-cta-ctbl-fim:SCREEN-VALUE            IN FRAME f-pg-sel = "99999999"
           c-cta-ctbl-fim:SENSITIVE               IN FRAME f-pg-sel = NO
           c-estab-ini:SCREEN-VALUE               IN FRAME f-pg-sel = ""
           c-estab-ini:SENSITIVE                  IN FRAME f-pg-sel = NO
           c-estab-fim:SCREEN-VALUE               IN FRAME f-pg-sel = "ZZZZZ"
           c-estab-fim:SENSITIVE                  IN FRAME f-pg-sel = NO
           c-unid-negoc-ini:SCREEN-VALUE          IN FRAME f-pg-sel = ""
           c-unid-negoc-ini:SENSITIVE             IN FRAME f-pg-sel = NO
           c-unid-negoc-fim:SCREEN-VALUE          IN FRAME f-pg-sel = "ZZZ"
           c-unid-negoc-fim:SENSITIVE             IN FRAME f-pg-sel = NO.
    END.
    ELSE DO:
        ASSIGN 
           tg-fech-previo:SENSITIVE               IN FRAME f-pg-sel = YES
           tg-val-neg:SENSITIVE                   IN FRAME f-pg-par = YES
           l-realizacao-dvv-me:SENSITIVE          IN FRAME f-pg-par = YES
           l-realizacao-dvv-mi:SENSITIVE          IN FRAME f-pg-par = YES
           l-provisao-dvv-me:SENSITIVE            IN FRAME f-pg-par = YES
           l-provisao-dvv-mi:SENSITIVE            IN FRAME f-pg-par = YES
           l-previa-provisoes:SENSITIVE           IN FRAME f-pg-par = YES
           l-estorno:SENSITIVE                    IN FRAME f-pg-par = YES
           l-remessa-venda-estoq:SENSITIVE        IN FRAME f-pg-par = YES
           l-despesa-nao-rast:SENSITIVE           IN FRAME f-pg-par = YES
           c-cta-ctbl-ini:SENSITIVE               IN FRAME f-pg-sel = YES
           c-cta-ctbl-fim:SENSITIVE               IN FRAME f-pg-sel = YES
           c-estab-ini:SENSITIVE                  IN FRAME f-pg-sel = YES
           c-estab-fim:SENSITIVE                  IN FRAME f-pg-sel = YES
           c-unid-negoc-ini:SENSITIVE             IN FRAME f-pg-sel = YES
           c-unid-negoc-fim:SENSITIVE             IN FRAME f-pg-sel = YES.
    END.

    /* NOVO: se Atualiza BI + Fechamento Prévio = YES, forçar Imprimir BI = YES */
    IF tg-bi:SCREEN-VALUE         IN FRAME f-pg-sel = "YES"
       AND tg-fech-previo:SCREEN-VALUE IN FRAME f-pg-sel = "YES" THEN
        ASSIGN tg-bi-imp:SCREEN-VALUE IN FRAME f-pg-sel = "YES".

    /* Mantém sua lógica pré-existente que depende do tg-fech-previo */
    APPLY "VALUE-CHANGED" TO tg-fech-previo IN FRAME f-pg-sel.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-bi-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-bi-imp C-Win
ON VALUE-CHANGED OF tg-bi-imp IN FRAME f-pg-sel /* Imprimir BI */
DO:
  RUN pi-esconde.

  IF tg-bi-imp:SCREEN-VALUE IN FRAME f-pg-sel = "YES" THEN DO:
      /* Não travamos mais o Fechamento Prévio aqui */
      ASSIGN 
         tg-fech-previo:SENSITIVE               IN FRAME f-pg-sel = YES
         tg-val-neg:SCREEN-VALUE                IN FRAME f-pg-par = "YES"
         tg-val-neg:SENSITIVE                   IN FRAME f-pg-par = NO
         l-realizacao-dvv-me:SCREEN-VALUE       IN FRAME f-pg-par = "YES"
         l-realizacao-dvv-me:SENSITIVE          IN FRAME f-pg-par = NO
         l-realizacao-dvv-mi:SCREEN-VALUE       IN FRAME f-pg-par = "YES"
         l-realizacao-dvv-mi:SENSITIVE          IN FRAME f-pg-par = NO
         l-provisao-dvv-me:SCREEN-VALUE         IN FRAME f-pg-par = "YES"
         l-provisao-dvv-me:SENSITIVE            IN FRAME f-pg-par = NO
         l-provisao-dvv-mi:SCREEN-VALUE         IN FRAME f-pg-par = "YES"
         l-provisao-dvv-mi:SENSITIVE            IN FRAME f-pg-par = NO
         l-estorno:SCREEN-VALUE                 IN FRAME f-pg-par = "YES"
         l-estorno:SENSITIVE                    IN FRAME f-pg-par = NO
         l-remessa-venda-estoq:SCREEN-VALUE     IN FRAME f-pg-par = "NO"
         l-remessa-venda-estoq:SENSITIVE        IN FRAME f-pg-par = NO
         l-despesa-nao-rast:SCREEN-VALUE        IN FRAME f-pg-par = "NO"
         l-despesa-nao-rast:SENSITIVE           IN FRAME f-pg-par = NO.
  END.
  ELSE DO:
      ASSIGN 
         tg-fech-previo:SENSITIVE               IN FRAME f-pg-sel = YES
         tg-val-neg:SENSITIVE                   IN FRAME f-pg-par = YES
         l-realizacao-dvv-me:SENSITIVE          IN FRAME f-pg-par = YES
         l-realizacao-dvv-mi:SENSITIVE          IN FRAME f-pg-par = YES
         l-provisao-dvv-me:SENSITIVE            IN FRAME f-pg-par = YES
         l-provisao-dvv-mi:SENSITIVE            IN FRAME f-pg-par = YES
         l-estorno:SENSITIVE                    IN FRAME f-pg-par = YES
         l-remessa-venda-estoq:SENSITIVE        IN FRAME f-pg-par = YES
         l-despesa-nao-rast:SENSITIVE           IN FRAME f-pg-par = YES.
  END.

  /* Caso o usuário tenha (ou venha a) marcar os dois, garantimos a impressão */
  IF tg-bi:SCREEN-VALUE          IN FRAME f-pg-sel = "YES"
     AND tg-fech-previo:SCREEN-VALUE IN FRAME f-pg-sel = "YES" THEN
      ASSIGN tg-bi-imp:SCREEN-VALUE IN FRAME f-pg-sel = "YES".

  APPLY "VALUE-CHANGED" TO tg-fech-previo IN FRAME f-pg-sel.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "dvv0199" "2.06.00.002"}

/* inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
  
    {include/i-rpmbl.i}

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY dt-ctbl-ini dt-ctbl-fim c-cta-ctbl-ini c-cta-ctbl-fim c-estab-ini 
          c-estab-fim c-unid-negoc-ini c-unid-negoc-fim tg-bi tg-bi-imp 
          tg-fech-previo 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE dt-ctbl-ini dt-ctbl-fim c-cta-ctbl-ini c-cta-ctbl-fim c-estab-ini 
         c-estab-fim c-unid-negoc-ini c-unid-negoc-fim tg-bi tg-bi-imp 
         tg-fech-previo 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}

  {&OPEN-QUERY-f-pg-par}
  GET FIRST f-pg-par.
  DISPLAY tg-val-neg l-realizacao-dvv-me l-realizacao-dvv-mi l-provisao-dvv-me 
          l-provisao-dvv-mi l-estorno l-remessa-venda-estoq l-despesa-nao-rast 
          c_descricao_moeda fi-cotacao l-previa-provisoes
      WITH FRAME f-pg-par IN WINDOW C-Win.
  IF AVAILABLE cotacao THEN 
    DISPLAY cotacao.mo-codigo 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE tg-val-neg l-realizacao-dvv-me l-realizacao-dvv-mi l-provisao-dvv-me 
         l-provisao-dvv-mi l-estorno l-remessa-venda-estoq l-despesa-nao-rast 
         cotacao.mo-codigo fi-cotacao 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}

  APPLY "VALUE-CHANGED" TO tg-fech-previo IN FRAME f-pg-sel.
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-esconde C-Win 
PROCEDURE pi-esconde :
/*------------------------------------------------------------------------------
  Purpose: Ajustar visuais sem travar a combinação de flags.
------------------------------------------------------------------------------*/

/* Sempre permitir que os dois toggles sejam checados juntos */
ASSIGN
    tg-bi:SENSITIVE       IN FRAME f-pg-sel = YES
    tg-bi-imp:SENSITIVE   IN FRAME f-pg-sel = YES
    tg-fech-previo:SENSITIVE IN FRAME f-pg-sel = YES.

/* NUNCA mais desmarcar um por causa do outro. 
   Mantemos apenas comportamentos visuais mínimos que você já usa.
   (Se você ainda quiser ocultar/mostrar algo, faça aqui, mas sem forçar NO) */

/* Regra de ouro: se Atualiza BI + Fechamento Prévio = YES, Imprimir BI = YES */
IF tg-bi:SCREEN-VALUE         IN FRAME f-pg-sel = "YES"
AND tg-fech-previo:SCREEN-VALUE IN FRAME f-pg-sel = "YES" THEN
    ASSIGN tg-bi-imp:SCREEN-VALUE IN FRAME f-pg-sel = "YES".

END PROCEDURE.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do  on error undo, return error
    on stop  undo, return error:     
    
    {include/i-rpexa.i}

    if  input frame f-pg-imp rs-destino = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    // Aviso no Fechamento Pr‚vio com Pr‚via das Provisäes, caso a rotina DVV5100 nÆo tenha gerado as pr‚vias recentemente.
    IF INPUT FRAME f-pg-par l-previa-provisoes = YES THEN DO:
        FIND LAST dvv_log_provisao
            WHERE dvv_log_provisao.origem_movto BEGINS "P"
            USE-INDEX id_data NO-LOCK NO-ERROR.
        IF  NOT AVAIL dvv_log_provisao THEN DO:
            run utp/ut-msgs.p (input "show", 
                               input 27100, 
                               input "Confirma ?~~NÆo foi executado ainda o DVV5100 para gerar a 'Pr‚via das Provisäes'." + CHR(13)
                                   + "Confirma execu‡Æo assim mesmo ?").
            IF  RETURN-VALUE <> "YES" THEN
                RETURN NO-APPLY.
        END.
        ELSE IF  dvv_log_provisao.dt_provisao < TODAY THEN DO:
            run utp/ut-msgs.p (input "show", 
                               input 27100, 
                               input "Confirma ?~~A op‡Æo 'Pr‚via das Provisäes' est  marcada, mas a £ltima pr‚via foi gerada pelo DVV5100 em " + STRING(dvv_log_provisao.dt_provisao,"99/99/9999") + "." + CHR(13)
                                   + "Confirma execu‡Æo assim mesmo ?").
            IF  RETURN-VALUE <> "YES" THEN
                RETURN NO-APPLY.
        END.
    END.

    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    CREATE tt-param.
    ASSIGN tt-param.usuario               = c-seg-usuario
           tt-param.destino               = input frame f-pg-imp rs-destino
           tt-param.data-exec             = today
           tt-param.hora-exec             = time
           tt-param.dt-ctbl-ini           = input frame f-pg-sel dt-ctbl-ini
           tt-param.dt-ctbl-fim           = input frame f-pg-sel dt-ctbl-fim
           tt-param.c-cta-ctbl-ini        = input frame f-pg-sel c-cta-ctbl-ini
           tt-param.c-cta-ctbl-fim        = input frame f-pg-sel c-cta-ctbl-fim
           tt-param.l-val-neg             = input frame f-pg-par tg-val-neg
           tt-param.l-realizacao-dvv-me   = INPUT FRAME f-pg-par l-realizacao-dvv-me
           tt-param.l-realizacao-dvv-mi   = INPUT FRAME f-pg-par l-realizacao-dvv-mi
           tt-param.l-provisao-dvv-me     = INPUT FRAME f-pg-par l-provisao-dvv-me
           tt-param.l-provisao-dvv-mi     = INPUT FRAME f-pg-par l-provisao-dvv-mi
           tt-param.l-previa-provisoes    = INPUT FRAME f-pg-par l-previa-provisoes
           tt-param.l-remessa-venda-estoq = INPUT FRAME f-pg-par l-remessa-venda-estoq
           tt-param.l-despesa-nao-rast    = INPUT FRAME f-pg-par l-despesa-nao-rast
           tt-param.l-estorno             = INPUT FRAME f-pg-par l-estorno
           tt-param.c-estab-ini           = INPUT FRAME f-pg-sel c-estab-ini
           tt-param.c-estab-fim           = INPUT FRAME f-pg-sel c-estab-fim
           tt-param.c-unid-negoc-ini      = INPUT FRAME f-pg-sel c-unid-negoc-ini 
           tt-param.c-unid-negoc-fim      = INPUT FRAME f-pg-sel c-unid-negoc-fim 
           tt-param.l-atualiza-bi         = INPUT FRAME f-pg-sel tg-bi
           tt-param.l-imprime-bi          = INPUT FRAME f-pg-sel tg-bi-imp
           tt-param.l-fech-previo         = INPUT FRAME f-pg-sel tg-fech-previo
           tt-param.i-mo-codigo           = INPUT FRAME f-pg-par cotacao.mo-codigo
           tt-param.de-cotacao            = INPUT FRAME f-pg-par fi-cotacao.
	
	IF tt-param.l-atualiza-bi = TRUE
	AND tt-param.l-fech-previo = TRUE THEN DO:
		ASSIGN tt-param.l-imprime-bi = TRUE.

		/* (Opcional) Sincronizar o estado visual na tela, caso o usuário volte da execução */
		IF VALID-HANDLE(tg-bi-imp:HANDLE) THEN
			ASSIGN tg-bi-imp:SCREEN-VALUE IN FRAME f-pg-sel = "YES".
	END.

    if  tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

    /*RUN tms/connectTms.p.RETIRADO POIS NÇO SERµ MAIS CONECTADO AO TMS*/

    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.

    {include/i-rprun.i dvv/dvv0199rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

    /*RUN tms/disconnectTms.p.*/
    
    /*{include/i-rptrm.i}*/
    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "pedido-compr"}
  {src/adm/template/snd-list.i "estabelec"}
  {src/adm/template/snd-list.i "item-fornec"}
  {src/adm/template/snd-list.i "cotacao"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

