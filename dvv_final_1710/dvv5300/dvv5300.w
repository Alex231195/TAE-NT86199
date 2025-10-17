&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i DVV5300 2.06.00.001}
{esp/Moedas.i} //Include com o valor do c¢digo das moedas

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i DVV5300 MEX}
&ENDIF


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)"
    label "Empresa"
    column-label "Empresa"
    no-undo.


DEF TEMP-TABLE tt-dvv_log_provisao NO-UNDO LIKE dvv_log_provisao
    FIELD marca          AS CHAR
    FIELD desc_SITUACAO LIKE pto-contr.descricao
    FIELD des_tit_ctbl AS CHAR
    FIELD val_lancto_euro AS DEC
    FIELD r_rowid        AS ROWID.

def temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

DEF TEMP-TABLE tt_erro_import NO-UNDO
    FIELD cod_empresa         LIKE estabelecimento.cod_empresa
    FIELD cd-erro             AS INT  FORMAT ">>>>,>>9"
    FIELD mensagem            AS CHAR FORMAT "x(150)"
    FIELD ajuda               AS CHAR FORMAT "x(150)".

DEF TEMP-TABLE tt_movto_import NO-UNDO
    FIELD cod_empresa      LIKE lote_ctbl.cod_empresa 
    FIELD cod_estab        LIKE item_lancto_ctbl.cod_estab
    FIELD dat_lancto_ctbl  LIKE lote_ctbl.dat_lote_ctbl
    FIELD dat_convert_val  LIKE lote_ctbl.dat_lote_ctbl
    FIELD des_lote_ctbl    LIKE lote_ctbl.des_lote_ctbl   
    FIELD ind_natur        LIKE item_lancto_ctbl.ind_natur_lancto_ctbl
    FIELD cod_un           LIKE item_lancto_ctbl.cod_unid_negoc
    FIELD cod_plano_cta    LIKE item_lancto_ctbl.cod_plano_cta_ctbl
    FIELD cod_cta_db       LIKE item_lancto_ctbl.cod_cta_ctbl      
    FIELD cod_plano_ccu    LIKE item_lancto_ctbl.cod_plano_ccusto 
    FIELD cod_ccu          LIKE item_lancto_ctbl.cod_ccusto       
    FIELD cod_cta_cr       LIKE item_lancto_ctbl.cod_cta_ctbl      
    FIELD cod_ie           LIKE item_lancto_ctbl.cod_indic_econ
    FIELD val_lancto       LIKE item_lancto_ctbl.val_lancto_ctbl
    FIELD des_hist         LIKE item_lancto_ctbl.des_histor_lancto_ctbl 
    FIELD log_estorno      AS   LOG 
    FIELD num_lote_ger     LIKE lancto_ctbl.num_lote_ctbl.

DEF TEMP-TABLE tt-dvv_log_prov_desp NO-UNDO /*LIKE dvv_log_prov_desp*/
    FIELD num_id_provisao LIKE dvv_log_prov_desp.num_id_provisao
    FIELD cod_estabel     LIKE dvv_log_prov_desp.cod_estabel    
    FIELD nr_proc_exp     LIKE dvv_log_prov_desp.nr_proc_exp    
    FIELD cod_despesa     LIKE dvv_log_prov_desp.cod_despesa    
    FIELD vl_despesa      LIKE dvv_log_prov_desp.vl_despesa     

    FIELD r_rowid         AS ROWID
    FIELD reg-seg         AS CHAR
    FIELD dt_emissao      AS DATE
    FIELD fm_codigo       AS CHAR
    FIELD cod_fornec      AS INT
    FIELD desc_fornec     AS CHAR
    FIELD DESC_despesa    AS CHAR
    FIELD novo            AS LOG  FORMAT "Sim/N∆o"
    FIELD provisiona      AS CHAR
    FIELD cod-cliente     AS INT
    FIELD nome_cliente    AS CHAR.

DEF TEMP-TABLE tt-dvv_log_prov_desp_item  LIKE tt-dvv_log_prov_desp
    FIELD it_codigo       AS CHAR
    FIELD grupo_item      AS CHAR
    FIELD grupo-classe    AS CHAR
    FIELD cod_incoterm    AS CHAR 
    FIELD val_desp_rat    AS DEC
    FIELD tipo_venda      AS CHAR
    FIELD val_desp_euro_it LIKE item_lancto_ctbl.val_lancto_ctbl.


DEF TEMP-TABLE tt-valores NO-UNDO
    FIELD cod-estabel LIKE processo-exp.cod-estabel
    FIELD nr-proc-exp LIKE processo-exp.nr-proc-exp
    FIELD cod-itiner  LIKE processo-exp.cod-itiner
    FIELD sigla-moeda LIKE moeda.sigla
    FIELD vl-tot-liq     AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD vl-tot-desp    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD vl-incoterm    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD peso-liq-tot   AS DECIMAL FORMAT ">>>,>>>,>>9.9999"
    FIELD peso-bruto-tot AS DECIMAL FORMAT ">>>,>>>,>>9.9999"
    FIELD volume-tot     AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD desp-desc      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    INDEX pk_valor IS UNIQUE PRIMARY
        cod-estabel
        nr-proc-exp
    ASCENDING.


DEF VAR i-seq AS INT NO-UNDO.
DEF VAR i-pto-contr AS INT NO-UNDO.
DEF VAR dt-cotacao AS DATE NO-UNDO.
DEF VAR d-val-provisao AS DEC NO-UNDO.
DEF VAR i-cod-estabel  AS CHAR NO-UNDO.
DEF VAR h-bocx185      AS HANDLE NO-UNDO.

{include/boerrtab.i}

DEF VAR p-dt-contab AS DATE NO-UNDO.
DEF VAR p-dt-contab-estorno AS DATE NO-UNDO.

DEF VAR p-data-real AS DATE NO-UNDO.
DEF VAR de-valor-conv AS DEC NO-UNDO.

DEF BUFFER bdvv_log_prov_desp FOR dvv_log_prov_desp.
DEF BUFFER b1dvv_log_prov_desp FOR dvv_log_prov_desp.
DEF BUFFER b-emit-fornec FOR emitente.
DEF BUFFER b_dvv_log_provis_estorno FOR dvv_log_provisao.

/**
def temp-table tt-erro-aux no-undo
    FIELD cod_estabel   LIKE tt-dvv_log_provisao.cod_estabel  
    FIELD nr_proc_exp   LIKE tt-dvv_log_provisao.nr_proc_exp  
    FIELD tip_movimento LIKE tt-dvv_log_provisao.tip_movimento
    field i-sequen      as int FORMAT ">>9"            
    field cd-erro       as int FORMAT ">>>>9"
    field mensagem      as char format "x(255)".
**/
/* Local Variable Definitions ---                                       */

 
DEF BUFFER bdvv_log_provisao     FOR dvv_log_provisao.
DEF BUFFER bdvv_log_provisao_exec  FOR dvv_log_provisao_exec.
DEF NEW GLOBAL SHARED VAR gl-atualiza-filtro AS LOGICAL NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-processo-exp AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren LIKE usuar_mestre.cod_usuario NO-UNDO.

DEF VAR i-width AS INTEGER NO-UNDO.
DEF VAR i-heigth AS INTEGER NO-UNDO.

DEF VAR p-cod-estabel-ini AS CHARACTER NO-UNDO.
DEF VAR p-cod-estabel-fim AS CHARACTER NO-UNDO.
DEF VAR p-dt-provisao-ini   AS DATE NO-UNDO.   
DEF VAR p-dt-provisao-fim   AS DATE NO-UNDO.   
DEF VAR p-dt-lancto-ini     AS DATE NO-UNDO.
DEF VAR p-dt-lancto-fim     AS DATE NO-UNDO.
DEF VAR p-cta-ctbl-ini    AS CHARACTER NO-UNDO.
DEF VAR p-cta-ctbl-fim    AS CHARACTER NO-UNDO.
DEF VAR p-lg-dex            AS LOG NO-UNDO.    
DEF VAR p-lg-dvvme          AS LOG NO-UNDO.    
DEF VAR p-lg-dvvmi          AS LOG NO-UNDO.    
DEF VAR p-lg-pdex            AS LOG NO-UNDO.    
DEF VAR p-lg-pdvvme          AS LOG NO-UNDO.    
DEF VAR p-lg-pdvvmi          AS LOG NO-UNDO.    
DEF VAR p-ind-situacao      AS INT NO-UNDO.    
DEF VAR c-todos-nenhum   AS CHARACTER NO-UNDO.
DEF VAR i-transbordo AS INTEGER NO-UNDO .
DEFINE VARIABLE p-lg-dvvrem       AS LOGICAL   NO-UNDO.  
DEFINE VARIABLE p-lg-pdvvrem      AS LOGICAL   NO-UNDO.  

def var h as handle.
DEF VAR h-acomp AS HANDLE NO-UNDO.

PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
    DEF INPUT  PARAM prg_name                          AS CHARACTER.
    DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-dvv_log_provisao

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-dvv_log_provisao.marca tt-dvv_log_provisao.cod_estabel tt-dvv_log_provisao.origem_movto tt-dvv_log_provisao.dt_provisao tt-dvv_log_provisao.dt_lancto_provis tt-dvv_log_provisao.dt_lancto_estorno tt-dvv_log_provisao.vl_provisao tt-dvv_log_provisao.val_lancto_euro tt-dvv_log_provisao.DESC_situacao tt-dvv_log_provisao.cod_cta_ctbl_cr tt-dvv_log_provisao.cod_cta_ctbl_db tt-dvv_log_provisao.des_tit_ctbl tt-dvv_log_provisao.num_id_provisao tt-dvv_log_provisao.cod_ccusto tt-dvv_log_provisao.cod_un   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-dvv_log_provisao
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-dvv_log_provisao.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-dvv_log_provisao
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-dvv_log_provisao


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-filtro bt-atualiza BROWSE-2 ~
bt-todos bt-completa bt-exporta bt-despesa bt-log 
&Scoped-Define DISPLAYED-OBJECTS de-total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "Imprimir"       ACCELERATOR "CTRL-I"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Menu"         
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-atualiza 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "&Atualiza" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-completa 
     LABEL "Provisiona" 
     SIZE 10.86 BY 1.

DEFINE BUTTON bt-despesa 
     LABEL "Detalhe" 
     SIZE 10.86 BY 1.

DEFINE BUTTON bt-exporta 
     LABEL "Exporta" 
     SIZE 10.86 BY 1.

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/im-param.bmp":U
     LABEL "&Filtro" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-log 
     LABEL "Log" 
     SIZE 10.86 BY 1.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 10.86 BY 1.

DEFINE VARIABLE de-total AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-dvv_log_provisao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-dvv_log_provisao.marca           COLUMN-LABEL "Sel" WIDTH 2      
        tt-dvv_log_provisao.cod_estabel     COLUMN-LABEL "Estabel" FORMAT "x(05)"
        tt-dvv_log_provisao.origem_movto   WIDTH 8  column-label "Origem" FORMAT "x(08)"
        tt-dvv_log_provisao.dt_provisao      column-label "Data Provis∆o"          FORMAT "99/99/9999"
        tt-dvv_log_provisao.dt_lancto_provis column-label "Data Lanto.Ctbl"        FORMAT "99/99/9999"
        tt-dvv_log_provisao.dt_lancto_estorno column-label "Data Estorno"          FORMAT "99/99/9999"
        tt-dvv_log_provisao.vl_provisao     column-label "Vl.Provis∆o"   FORMAT ">>,>>>,>>>,>>9.999"
        tt-dvv_log_provisao.val_lancto_euro column-label "Vl.Provis.Euro"   FORMAT ">>,>>>,>>>,>>9.999"
        tt-dvv_log_provisao.DESC_situacao   column-label "Situaá∆o" FORMAT "x(12)"
        tt-dvv_log_provisao.cod_cta_ctbl_cr column-label "Conta CrÇdito" FORMAT "x(8)"
        tt-dvv_log_provisao.cod_cta_ctbl_db column-label "Conta DÇbito"  FORMAT "x(8)"
        tt-dvv_log_provisao.des_tit_ctbl    COLUMN-LABEL "Desc Conta Db" FORMAT "x(40)"
        tt-dvv_log_provisao.num_id_provisao column-label "NumProvis∆o" FORMAT ">>>>>>>>9"
        tt-dvv_log_provisao.cod_ccusto      COLUMN-LABEL "C.Custo"         
        tt-dvv_log_provisao.cod_un          COLUMN-LABEL "Cod.UN"        FORMAT "X(03)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86.72 BY 14.5 ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-filtro AT ROW 1.25 COL 1.72
     bt-atualiza AT ROW 1.25 COL 6
     BROWSE-2 AT ROW 2.63 COL 2.29
     bt-todos AT ROW 17.25 COL 2.72
     bt-completa AT ROW 17.25 COL 14.14
     bt-exporta AT ROW 17.25 COL 25.43 WIDGET-ID 4
     bt-despesa AT ROW 17.25 COL 36.72 WIDGET-ID 2
     bt-log AT ROW 17.25 COL 48
     de-total AT ROW 17.25 COL 70.72 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 18.21
         WIDTH              = 90
         MAX-HEIGHT         = 27.54
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 27.54
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME Size-to-Fit L-To-R                                        */
/* BROWSE-TAB BROWSE-2 bt-atualiza f-cad */
ASSIGN 
       FRAME f-cad:SCROLLABLE       = FALSE.

ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME f-cad = TRUE.

/* SETTINGS FOR FILL-IN de-total IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-dvv_log_provisao.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-MAXIMIZED OF w-livre /* Template Livre <Insira complemento> */
DO:                                                                          
    ASSIGN w-livre:WIDTH  = 190.
    ASSIGN w-livre:HEIGHT  = 27.42.
    ASSIGN FRAME f-cad:WIDTH = 190.
    ASSIGN FRAME f-cad:HEIGHT = 27.26.
    ASSIGN bt-log:ROW IN FRAME f-cad = 26.25
           bt-todos:ROW IN FRAME f-cad = 26.25
           bt-completa:ROW IN FRAME f-cad = 26.25
           bt-despesa:ROW IN FRAME f-cad = 26.25
           bt-exporta:ROW IN FRAME f-cad = 26.25
           de-total:ROW IN FRAME f-cad = 26.25.
    ASSIGN browse-2:WIDTH IN FRAME f-cad = 186.72.
    ASSIGN browse-2:HEIGHT IN FRAME f-cad = 23.50.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-RESTORED OF w-livre /* Template Livre <Insira complemento> */
DO:
    ASSIGN browse-2:HEIGHT IN FRAME f-cad = 14.50.
    ASSIGN browse-2:WIDTH IN FRAME f-cad = 86.72.
    ASSIGN bt-log:ROW IN FRAME f-cad = 17.25
           bt-todos:ROW IN FRAME f-cad = 17.25
           bt-completa:ROW IN FRAME f-cad = 17.25
           bt-despesa:ROW IN FRAME f-cad = 17.25
           bt-exporta:ROW IN FRAME f-cad = 17.25
           de-total:ROW IN FRAME f-cad = 17.25.

    ASSIGN FRAME f-cad:HEIGHT = 17.26.
    ASSIGN FRAME f-cad:WIDTH = 89.73.
    
    ASSIGN w-livre:WIDTH  = 90.

    ASSIGN w-livre:HEIGHT  = 17.42.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME f-cad
DO:
    IF AVAIL tt-dvv_log_provisao THEN DO:
        IF tt-dvv_log_provisao.marca:SCREEN-VALUE IN BROWSE browse-2 = "" THEN DO:
            ASSIGN tt-dvv_log_provisao.marca:SCREEN-VALUE IN BROWSE browse-2 = "*"
                   tt-dvv_log_provisao.marca = "*" .

        END.
        ELSE DO:
            ASSIGN tt-dvv_log_provisao.marca:SCREEN-VALUE IN BROWSE browse-2 = ""
                   tt-dvv_log_provisao.marca = "".

        END.

        RUN pi-calcula-total.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON START-SEARCH OF BROWSE-2 IN FRAME f-cad
DO:
  DEF VAR coluna AS CHAR. /* Guarda o nome real da coluna clicada no browse */
  DEF VAR cWhere AS CHAR INIT "FOR EACH tt-dvv_log_provisao NO-LOCK BY tt-dvv_log_provisao.num_id_provisao INDEXED-REPOSITION.".
  DEF VAR hQuery AS HANDLE.
/*****************************
 /* Se a coluna clicada n∆o foi clicada anteriormente faáa */
  IF SELF :CURRENT-COLUMN :SORT-ASCENDING = ? THEN DO:
     SELF :CLEAR-SORT-ARROWS(). /* Limpa as setas das outras colunas */
     SELF :CURRENT-COLUMN :SORT-ASCENDING = FALSE. /* Ordena a coluna ascendentemente */
  END.
  /* Sen∆o ordena a coluna ao inverso do que j† est† ordenada */
  ELSE SELF :CURRENT-COLUMN :SORT-ASCENDING = NOT SELF :CURRENT-COLUMN :SORT-ASCENDING.

  /* Armazena a coluna recem selecionada */
  coluna = SELF :CURRENT-COLUMN :NAME.

  /* Verifica se a ordenaá∆o est† ascendente e altera a vari†vel cWhere da Query */
  IF SELF :CURRENT-COLUMN :SORT-ASCENDING = FALSE THEN DO:
/* Retira as ordenaá‰es que existiam e coloca somente para a coluna clicada - CRESCENTE */
     cWhere = REPLACE(cWhere,SUBSTRING(cWhere,INDEX(cWhere,"BY"),INDEX(cWhere,"indexed") - INDEX(cWhere,"BY")),STRING("BY " + coluna + " ")).
     c-ult-ordem = "A".
     c-ult-coluna = coluna.
  END.
  ELSE IF SELF :CURRENT-COLUMN :SORT-ASCENDING = TRUE THEN DO:
     /* Retira as ordenaá‰es que existiam e coloca somente para a coluna clicada - DECRESCENTE */
     cWhere = REPLACE(cWhere,SUBSTRING(cWhere,INDEX(cWhere,"BY"),INDEX(cWhere,"indexed") - INDEX(cWhere,"BY")),STRING("BY " + coluna + " DESC ")).
     c-ult-ordem = "D".
     c-ult-coluna = coluna.
  END.
  ELSE DO:
      /* Se n∆o houver coluna clicada limpa as £ltimas clicadas e retorna. */
      SELF :CLEAR-SORT-ARROWS().
      c-ult-ordem = "".
      c-ult-coluna = "".
      RETURN.
  END.
*****************/

  /* Executa a nova query para o Browse */
  ASSIGN hQuery = SELF :QUERY :HANDLE.
  hQuery:QUERY-PREPARE(cWhere).
  hQuery:QUERY-OPEN().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-atualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiza w-livre
ON CHOOSE OF bt-atualiza IN FRAME f-cad /* Atualiza */
DO:

    EMPTY TEMP-TABLE tt-dvv_log_provisao.

    ASSIGN c-todos-nenhum  = "N".
    /*ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 0.*/
    run pi-gerar-dados.
    open query {&browse-name} for each tt-dvv_log_provisao.
    /*ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 3600000.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-completa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-completa w-livre
ON CHOOSE OF bt-completa IN FRAME f-cad /* Provisiona */
DO:

    /*EMPTY TEMP-TABLE tt-erro-aux.*/

    RUN dvv/dvv5300-dt.w (INPUT-OUTPUT p-dt-contab,
                          INPUT-OUTPUT p-dt-contab-estorno).

    IF p-dt-contab = ? THEN do:
        MESSAGE "INFORMAR A DATA DO LANÄAMENTO" VIEW-AS ALERT-BOX.
        LEAVE.
    END.
    IF p-dt-contab-estorno = ? THEN do:
        MESSAGE "INFORMAR A DATA DO ESTORNO" VIEW-AS ALERT-BOX.
        LEAVE.
    END.

    FOR EACH tt-dvv_log_provisao WHERE
             tt-dvv_log_provisao.marca = "*" 
        BY tt-dvv_log_provisao.origem 
        /*BY tt-dvv_log_provisao.cod_estabel*/
        BY tt-dvv_log_provisao.dt_provisao
        BY tt-dvv_log_provisao.cod_cta_ctbl_db
        BY tt-dvv_log_provisao.cod_un:

        IF tt-dvv_log_provisao.situacao <> 1 THEN NEXT.

        IF tt-dvv_log_provisao.origem BEGINS "P" OR tt-dvv_log_provisao.origem BEGINS "EP" THEN NEXT. // A prÇvia da provis∆o n∆o deve ser efetivamente lanáada na contabilidade, Ç apenas informativa.

        EMPTY TEMP-TABLE tt_movto_import.
        EMPTY TEMP-TABLE tt_erro_import.

        FIND estabelec WHERE
             estabelec.cod-estabel = tt-dvv_log_provisao.cod_estabel NO-LOCK NO-ERROR.


        FOR EACH b_dvv_log_provis_estorno WHERE
                 b_dvv_log_provis_estorno.cod_estabel       = tt-dvv_log_provisao.cod_estabel AND
                 b_dvv_log_provis_estorno.dt_lancto_provis  < p-dt-contab                     AND
                 /*b_dvv_log_provis_estorno.cod_cta_ctbl_db = tt-dvv_log_provisao.cod_cta_ctbl_db AND*/
                 b_dvv_log_provis_estorno.origem            = tt-dvv_log_provisao.origem  AND
                 b_dvv_log_provis_estorno.situacao          = 2 NO-LOCK:

                RUN pi-gera-estorno.

        END.


        EMPTY TEMP-TABLE tt_movto_import.
        CREATE tt_movto_import.
        ASSIGN tt_movto_import.cod_empresa     = IF estabelec.ep-codigo = "2" THEN "02" ELSE "01" /*estabelec.ep-codigo*/
               tt_movto_import.cod_estab       = estabelec.cod-estabel /*tt-dvv_log_provisao.cod_estabel*/
               tt_movto_import.dat_lancto_ctbl = p-dt-contab /*TODAY - tt-dvv_log_provisao.dt_provisao*/
               tt_movto_import.des_lote_ctbl   = "Lancamento Provisao " + tt-dvv_log_provisao.origem 
               tt_movto_import.ind_natur       = ""
               tt_movto_import.cod_un          = tt-dvv_log_provisao.cod_un
               tt_movto_import.cod_plano_cta   = "NITROC"    
               tt_movto_import.cod_cta_db      = tt-dvv_log_provisao.cod_cta_ctbl_db
               tt_movto_import.cod_plano_ccu   = ""
               tt_movto_import.cod_ccu         = ""
               tt_movto_import.cod_cta_cr      = tt-dvv_log_provisao.cod_cta_ctbl_cr
               tt_movto_import.cod_ie          = IF estabelec.cod-estabel = "002" THEN cEuroVenda ELSE cRealVenda
               tt_movto_import.val_lancto      = tt-dvv_log_provisao.vl_provisao
               tt_movto_import.des_hist        = "Lancamento Provisao  " + tt-dvv_log_provisao.origem
               tt_movto_import.log_estorno     = NO
               tt_movto_import.num_lote_ger    = 0. 

        RELEASE tt_movto_import.

        RUN esp/esfgl0001.p (INPUT-OUTPUT TABLE tt_movto_import,
                             OUTPUT TABLE tt_erro_import).

        FIND FIRST tt_erro_import NO-ERROR.

        IF NOT CAN-FIND (FIRST tt_erro_import) THEN DO:
            FIND FIRST tt_movto_import NO-ERROR.
            IF STRING(tt_movto_import.num_lote_ger) <> "0" THEN DO:
                FIND dvv_log_provisao WHERE
                     ROWID(dvv_log_provisao) = tt-dvv_log_provisao.r_rowid EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL dvv_log_provisao THEN
                    ASSIGN dvv_log_provisao.situacao = 2
                           dvv_log_provisao.num_lote_ctbl    = tt_movto_import.num_lote_ger
                           dvv_log_provisao.dt_lancto_provis = p-dt-contab.

                FIND CURRENT dvv_log_provisao NO-LOCK NO-ERROR.
                ASSIGN i-seq = 1.
                FIND LAST bdvv_log_provisao_exec WHERE
                          bdvv_log_provisao_exec.num_id_provisao = tt-dvv_log_provisao.num_id_provisao NO-LOCK NO-ERROR.
                IF AVAIL bdvv_log_provisao_exec THEN
                    ASSIGN i-seq = bdvv_log_provisao_exec.sequencia + 1.

                CREATE dvv_log_provisao_exec.
                ASSIGN dvv_log_provisao_exec.num_id_provisao = tt-dvv_log_provisao.num_id_provisao
                       dvv_log_provisao_exec.sequencia       = i-seq
                       dvv_log_provisao_exec.dt_atualiz      = TODAY
                       dvv_log_provisao_exec.cod_usuario     = v_cod_usuar_corren
                       dvv_log_provisao_exec.hr_atualiz      = replace(STRING(TIME,"HH:MM:SS"),":","")
                       dvv_log_provisao_exec.mensagem        = "Lote Gerado com sucesso. Numero lote: " + STRING(tt_movto_import.num_lote_ger).
                       dvv_log_provisao_exec.status_execucao = "Sucesso".
            END.
        END.
        ELSE DO:
            
            FIND FIRST tt_erro_import NO-ERROR.

            ASSIGN i-seq = 1.
            FIND LAST bdvv_log_provisao_exec WHERE
                      bdvv_log_provisao_exec.num_id_provisao = tt-dvv_log_provisao.num_id_provisao NO-LOCK NO-ERROR.
            IF AVAIL bdvv_log_provisao_exec THEN
                ASSIGN i-seq = bdvv_log_provisao_exec.sequencia + 1.
                
            CREATE dvv_log_provisao_exec.
            ASSIGN dvv_log_provisao_exec.num_id_provisao = tt-dvv_log_provisao.num_id_provisao
                   dvv_log_provisao_exec.sequencia       = i-seq
                   dvv_log_provisao_exec.dt_atualiz      = TODAY
                   dvv_log_provisao_exec.cod_usuario     = v_cod_usuar_corren
                   dvv_log_provisao_exec.hr_atualiz      = replace(STRING(TIME,"HH:MM:SS"),":","")
                   dvv_log_provisao_exec.mensagem        = string(tt_erro_import.cd-erro) + " - " + tt_erro_import.mensagem + " - " + tt_erro_import.ajuda
                   dvv_log_provisao_exec.status_execucao = "Erro".
            /*******
            FOR EACH tt_erro_import:
                MESSAGE tt_erro_import.cod_empresa SKIP
                        tt_erro_import.cd-erro     SKIP
                        tt_erro_import.mensagem    SKIP
                        tt_erro_import.ajuda       VIEW-AS ALERT-BOX.

            END.
            *********/
        END.
        RELEASE dvv_log_provisao_exec.



/***
        CREATE tt-erro-aux.
        ASSIGN tt-erro-aux.cod_estabel   = tt-dvv_log_provisao.cod_estabel
               tt-erro-aux.nr_proc_exp   = tt-dvv_log_provisao.nr_proc_exp
               tt-erro-aux.tip_movimento = tt-dvv_log_provisao.tip_movimento
               tt-erro-aux.i-sequen      = 1
               tt-erro-aux.cd-erro       = 17006
               tt-erro-aux.mensagem      = "Integraá∆o realizada com sucesso.".
**/

          
    /*RUN pi-imprime-erros.*/
    END.

    APPLY "CHOOSE" TO bt-atualiza IN FRAME f-cad.

    /*
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 0.
    
    FIND processo-exp
        WHERE processo-exp.cod-estabel = "001"
          AND processo-exp.nr-proc-exp = tt-monitor.nr-proc-exp NO-LOCK NO-ERROR.
    IF AVAIL processo-exp THEN DO:
        ASSIGN gr-processo-exp = ROWID(processo-exp).
        RUN EXP/ex0200.w.
    END.
    ELSE DO:
        MESSAGE "Processo de Exportaá∆o n∆o encontrado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 60000.
    */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-despesa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-despesa w-livre
ON CHOOSE OF bt-despesa IN FRAME f-cad /* Detalhe */
DO:

    FIND dvv_log_provisao WHERE
         rowid(dvv_log_provisao) = tt-dvv_log_provisao.r_rowid NO-LOCK NO-ERROR.
    IF AVAIL dvv_log_provisao THEN DO:
        RUN dvv/dvv5300-DESP.w (INPUT rowid(dvv_log_provisao)).
        APPLY "choose" TO bt-atualiza IN FRAME f-cad.

    END.



/*    
    DEF VAR c-arq AS CHAR NO-UNDO.

    FIND es_tracking WHERE
         rowid(es_tracking) = tt-es_tracking.r_rowid NO-LOCK NO-ERROR.
    IF AVAIL es_tracking THEN DO:

        ASSIGN c-arq = session:temp-directory + string(es_tracking.cod_estabel) + string(es_tracking.nr_proc_exp) + trim(es_tracking.tip_movimento) + ".xml".

        OUTPUT TO value(c-arq) NO-CONVERT.
        PUT UNFORMAT es_tracking.xml.
        OUTPUT CLOSE.

        DOS SILENT START value(c-arq).

        DOS SILENT DELETE value (c-arq).

    END.

    /*
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 0.
    
    FIND processo-exp
        WHERE processo-exp.cod-estabel = "001"
          AND processo-exp.nr-proc-exp = tt-monitor.nr-proc-exp NO-LOCK NO-ERROR.
    IF AVAIL processo-exp THEN DO:
        ASSIGN gr-processo-exp = ROWID(processo-exp).
        RUN EXP/ex0190.w.
    END.
    ELSE DO:
        MESSAGE "Processo de Exportaá∆o n∆o encontrado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.  
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 60000.
    */
*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exporta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exporta w-livre
ON CHOOSE OF bt-exporta IN FRAME f-cad /* Exporta */
DO:
    EMPTY TEMP-TABLE tt-dvv_log_prov_desp.
    EMPTY TEMP-TABLE tt-dvv_log_prov_desp_item.


    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Exportando Provis∆o").

    FOR EACH tt-dvv_log_provisao WHERE
             tt-dvv_log_provisao.marca = "*" :

        FIND dvv_log_provisao WHERE
             rowid(dvv_log_provisao) = tt-dvv_log_provisao.r_rowid NO-LOCK NO-ERROR.

        FOR EACH dvv_log_prov_desp WHERE
                 dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao NO-LOCK:

            run pi-acompanhar in h-acomp (string(dvv_log_prov_desp.num_id_provisao) + " - " + dvv_log_prov_desp.nr_proc_exp).

            CREATE tt-dvv_log_prov_desp.
            BUFFER-COPY dvv_log_prov_desp TO tt-dvv_log_prov_desp.
            ASSIGN tt-dvv_log_prov_desp.r_rowid = ROWID(dvv_log_prov_desp).

            {dvv/dvv5300-desp.i}

        END.

    END.

    {dvv/dvv5300-desp.i2}

    OUTPUT TO value(session:temp-directory + "dvv5300.csv") CONVERT TARGET "iso8859-1" .

    PUT UNFORMATTED 
        "NumIdProvisao" ";"
        "Origem" ";"
        "Provisiona"    ";"
        "Dt.Emiss∆o"    ";"
        "Novo"          ";"
        "Processo/NF"   ";"
        "Item"          ";"  
        "Grupo"         ";"  
        "Familia"       ";"
        "Class.Item"    ";"
        "Despesa"       ";"
        "Descriá∆o"     ";"
        "Fornec"        ";"
        "Desc.Fornec"   ";"
        "Vl.Despesa"    ";"
        "Vl.Desp.Euro"  ";"
        "Reg/Seg"       ";"
        "Incoterm"      ";"
        "Tipo Venda"    SKIP.


    FOR EACH tt-dvv_log_prov_desp_item:
        FIND dvv_log_provisao WHERE
             dvv_log_provisao.num_id_provisao = tt-dvv_log_prov_desp_item.num_id_provisao NO-LOCK NO-ERROR.

        PUT UNFORMATTED 
            tt-dvv_log_prov_desp_item.num_id_provisao ";"
            dvv_log_provisao.origem_movto ";"
            tt-dvv_log_prov_desp_item.provisiona   ";"        
            tt-dvv_log_prov_desp_item.dt_emissao   ";" 
            IF tt-dvv_log_prov_desp_item.novo THEN "Sim" ELSE "N∆o"        ";" 
            tt-dvv_log_prov_desp_item.nr_proc_exp  ";" 
            tt-dvv_log_prov_desp_item.it_codigo    ";" 
            tt-dvv_log_prov_desp_item.grupo_item   ";"
            tt-dvv_log_prov_desp_item.fm_codigo    ";" 
            tt-dvv_log_prov_desp_item.grupo-classe ";"
            tt-dvv_log_prov_desp_item.cod_despesa  ";" 
            tt-dvv_log_prov_desp_item.DESC_despesa ";" 
            tt-dvv_log_prov_desp_item.cod_fornec   ";" 
            tt-dvv_log_prov_desp_item.DESC_fornec  ";" 
            tt-dvv_log_prov_desp_item.val_desp_rat  ";" 
            tt-dvv_log_prov_desp_item.val_desp_euro_it ";"
            tt-dvv_log_prov_desp_item.reg-seg      ";" 
            tt-dvv_log_prov_desp_item.cod_incoterm ";"
            tt-dvv_log_prov_desp_item.tipo_venda   SKIP. 

    END.
    OUTPUT CLOSE.

    run pi-finalizar in h-acomp.

    OS-COMMAND no-wait VALUE (session:TEMP-DIRECTORY + "dvv5300.csv") .


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-livre
ON CHOOSE OF bt-filtro IN FRAME f-cad /* Filtro */
DO:
    /*ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 0.*/
    ASSIGN gl-atualiza-filtro = NO.
	RUN dvv/dvv5300-filtro.w (OUTPUT p-cod-estabel-ini,
							  OUTPUT p-cod-estabel-fim,
							  OUTPUT p-dt-provisao-ini,
							  OUTPUT p-dt-provisao-fim,
							  OUTPUT p-dt-lancto-ini,
							  OUTPUT p-dt-lancto-fim,
							  OUTPUT p-cta-ctbl-ini,
							  OUTPUT p-cta-ctbl-fim,
							  OUTPUT p-lg-dex,
							  OUTPUT p-lg-dvvme,
							  OUTPUT p-lg-dvvmi,
							  OUTPUT p-lg-pdex,
							  OUTPUT p-lg-pdvvme,
							  OUTPUT p-lg-pdvvmi,
							  OUTPUT p-ind-situacao).
    IF gl-atualiza-filtro THEN DO:
        run pi-gerar-dados.
        open query {&browse-name} for each tt-dvv_log_provisao.
    END.
    /*ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 60000.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-livre
ON CHOOSE OF bt-log IN FRAME f-cad /* Log */
DO:

    FIND dvv_log_provisao WHERE
         rowid(dvv_log_provisao) = tt-dvv_log_provisao.r_rowid NO-LOCK NO-ERROR.
    IF AVAIL dvv_log_provisao THEN DO:
        RUN dvv/dvv5300-log.w (INPUT rowid(dvv_log_provisao)).
    END.

    /*
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 0.
    
    FIND processo-exp
        WHERE processo-exp.cod-estabel = "001"
          AND processo-exp.nr-proc-exp = tt-monitor.nr-proc-exp NO-LOCK NO-ERROR.
    IF AVAIL processo-exp THEN DO:
        ASSIGN gr-processo-exp = ROWID(processo-exp).
        RUN EXP/ex0190.w.
    END.
    ELSE DO:
        MESSAGE "Processo de Exportaá∆o n∆o encontrado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.  
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 60000.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:
    
    if c-todos-nenhum = "N" then do:
        assign c-todos-nenhum = "T".   

        FOR EACH tt-dvv_log_provisao:
            ASSIGN tt-dvv_log_provisao.marca = "*".
            assign bt-todos:label in frame f-cad = "Nenhum".
        END.
    END.
    ELSE DO:
        assign c-todos-nenhum = "N".

        FOR EACH tt-dvv_log_provisao:
            ASSIGN tt-dvv_log_provisao.marca = "".
            assign bt-todos:label in frame f-cad = "Todos".
        END.
    END.

    OPEN QUERY browse-2
        FOR each tt-dvv_log_provisao NO-LOCK .
    
    RUN pi-calcula-total.
    /*
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 0.
    
    FIND processo-exp
        WHERE processo-exp.cod-estabel = "001"
          AND processo-exp.nr-proc-exp = tt-monitor.nr-proc-exp NO-LOCK NO-ERROR.
    IF AVAIL processo-exp THEN DO:
        ASSIGN gr-processo-exp = ROWID(processo-exp).
        /*SESSION:SET-WAIT-STATE("general"):U.*/
        {&WINDOW-NAME}:sensitive = no.
        run exp/ex3001.w persistent set h.
        run initializeInterface in h.
        wait-for close of h.
        /*session:set-wait-state(""):U.*/
        {&WINDOW-NAME}:sensitive = YES.
    END.
    ELSE DO:
        MESSAGE "Processo de Exportaá∆o n∆o encontrado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    
    ASSIGN chCtrlFrame:PSTIMER:INTERVAL = 60000.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Menu */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
VIEW  w-livre.

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-filtro:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY de-total 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button bt-filtro bt-atualiza BROWSE-2 bt-todos bt-completa 
         bt-exporta bt-despesa bt-log 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
ASSIGN p-cod-estabel-ini  = ""
       p-cod-estabel-fim  = "ZZZZZ"
       p-dt-provisao-ini  = 01/01/2017
       p-dt-provisao-fim  = 12/31/2017
       p-cta-ctbl-ini     = ""
       p-cta-ctbl-fim     = "ZZZZZZZZ"
       p-lg-dex           = YES
       p-lg-dvvme         = YES
       p-lg-dvvmi         = YES
       p-lg-pdex           = YES
       p-lg-pdvvme         = YES
       p-lg-pdvvmi         = YES
       p-lg-dvvrem         = NO    /* NOVO */
       p-lg-pdvvrem        = NO    /* NOVO */
       p-ind-situacao      = 1.
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "DVV5300" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-total w-livre 
PROCEDURE pi-calcula-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN de-total = 0.
    FOR EACH tt-dvv_log_provisao WHERE
             tt-dvv_log_provisao.marca = "*" :

        ASSIGN de-total = de-total + tt-dvv_log_provisao.vl_provisao.

    END.

    ASSIGN de-total:SCREEN-VALUE IN FRAME {&FRAME-NAME}= STRING(de-total).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-es-tracking-log w-livre 
PROCEDURE pi-cria-es-tracking-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER p-cod-erro AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEF INPUT PARAMETER p-mensagem AS CHARACTER NO-UNDO.

DEF VAR i-seq AS INTEGER FORMAT  ">>>9" NO-UNDO.

ASSIGN i-seq = 1.
/***
FIND LAST bes_tracking_log WHERE
          bes_tracking_log.cod_estabel   = tt-es_tracking.cod_estabel   AND
          bes_tracking_log.nr_proc_exp   = tt-es_tracking.nr_proc_exp   AND
          bes_tracking_log.tip_movimento = tt-es_tracking.tip_movimento AND
          bes_tracking_log.EVENT_code    = tt-es_tracking.EVENT_code    AND     
          bes_tracking_log.cod_pto_contr = tt-es_tracking.cod_pto_contr AND
          bes_tracking_log.sequencia     = tt-es_tracking.sequencia     NO-LOCK NO-ERROR.

IF AVAIL bes_tracking_log THEN
    ASSIGN i-seq = bes_tracking_log.sequen_log + 1.

CREATE es_tracking_log.
ASSIGN es_tracking_log.cod_estabel   = tt-es_tracking.cod_estabel  
       es_tracking_log.nr_proc_exp   = tt-es_tracking.nr_proc_exp  
       es_tracking_log.tip_movimento = tt-es_tracking.tip_movimento
       es_tracking_log.EVENT_code    = tt-es_tracking.EVENT_code
       es_tracking_log.cod_pto_contr = tt-es_tracking.cod_pto_contr
       
       es_tracking_log.sequen_log        = i-seq
       es_tracking_log.cod_mensagem      = p-cod-erro
       es_tracking_log.desc_mensagem     = p-mensagem
       es_tracking_log.cod_usuar_atualiz = v_cod_usuar_corren
       es_tracking_log.dat_atualiz       = TODAY
       es_tracking_log.hor_atualiz       = REPLACE(STRING(TIME,"HH:MM"), ":","").

**/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-estorno w-livre 
PROCEDURE pi-gera-estorno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-seq AS INT NO-UNDO.
    DEF BUFFER bdvv_log_provisao_exec FOR dvv_log_provisao_exec.

    FIND estabelec WHERE
         estabelec.cod-estabel = b_dvv_log_provis_estorno.cod_estabel NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt_movto_import.

    CREATE tt_movto_import.                                                              
    ASSIGN tt_movto_import.cod_empresa     = IF estabelec.ep-codigo = "2" THEN "02" ELSE string(int(estabelec.ep-codigo),"99")
           tt_movto_import.cod_estab       = b_dvv_log_provis_estorno.cod_estabel             
           tt_movto_import.dat_lancto_ctbl = p-dt-contab-estorno /*tt-dvv_log_provisao.dt_provisao*/
           tt_movto_import.dat_convert_val = b_dvv_log_provis_estorno.dt_lancto_provis /*tt-dvv_log_provisao.dt_lancto_provis*/
           tt_movto_import.des_lote_ctbl   = "Estorno Lancamento Provisao DVV"
           tt_movto_import.ind_natur       = ""
           tt_movto_import.cod_un          = b_dvv_log_provis_estorno.cod_un
           tt_movto_import.cod_plano_cta   = "NITROC"    
           tt_movto_import.cod_cta_db      = b_dvv_log_provis_estorno.cod_cta_ctbl_db
           tt_movto_import.cod_plano_ccu   = ""
           tt_movto_import.cod_ccu         = ""
           tt_movto_import.cod_cta_cr      = b_dvv_log_provis_estorno.cod_cta_ctbl_cr
           tt_movto_import.cod_ie          = IF estabelec.cod-estabel = "002" THEN cEuroVenda ELSE cRealVenda
           tt_movto_import.val_lancto      = b_dvv_log_provis_estorno.vl_provisao
           tt_movto_import.des_hist        = "Estorno Lancamento Provisao DVV"
           tt_movto_import.log_estorno     = YES
           tt_movto_import.num_lote_ger    = 0. 

    RELEASE tt_movto_import.

    RUN esp/esfgl0001.p (INPUT-OUTPUT TABLE tt_movto_import,
                         OUTPUT TABLE tt_erro_import).

    IF NOT CAN-FIND (FIRST tt_erro_import) THEN DO:
        FIND FIRST tt_movto_import NO-ERROR.
        FIND CURRENT b_dvv_log_provis_estorno EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN b_dvv_log_provis_estorno.situacao = 4
               /*b_dvv_log_provis_estorno.num_lote_ctbl = tt_movto_import.num_lote_ger*/
               b_dvv_log_provis_estorno.num_lote_ctbl_estorno = tt_movto_import.num_lote_ger
               b_dvv_log_provis_estorno.dt_lancto_estorno     = p-dt-contab-estorno.
        FIND CURRENT b_dvv_log_provis_estorno NO-LOCK NO-ERROR.
        ASSIGN i-seq = 1.
        FIND LAST bdvv_log_provisao_exec WHERE
                  bdvv_log_provisao_exec.num_id_provisao = b_dvv_log_provis_estorno.num_id_provisao NO-LOCK NO-ERROR.
        IF AVAIL bdvv_log_provisao_exec THEN
            ASSIGN i-seq = bdvv_log_provisao_exec.sequencia + 1.

        CREATE dvv_log_provisao_exec.
        ASSIGN dvv_log_provisao_exec.num_id_provisao = b_dvv_log_provis_estorno.num_id_provisao
               dvv_log_provisao_exec.sequencia       = i-seq
               dvv_log_provisao_exec.dt_atualiz      = TODAY
               dvv_log_provisao_exec.cod_usuario     = v_cod_usuar_corren
               dvv_log_provisao_exec.hr_atualiz      = replace(STRING(TIME,"HH:MM:SS"),":","")
               dvv_log_provisao_exec.mensagem        = "Lote Gerado com sucesso. Numero lote: " + STRING(tt_movto_import.num_lote_ger).
               dvv_log_provisao_exec.status_execucao = "Sucesso".


    END.
    ELSE DO:

        FIND FIRST tt_erro_import NO-ERROR.

        ASSIGN i-seq = 1.
        FIND LAST bdvv_log_provisao_exec WHERE
                  bdvv_log_provisao_exec.num_id_provisao = b_dvv_log_provis_estorno.num_id_provisao NO-LOCK NO-ERROR.
        IF AVAIL bdvv_log_provisao_exec THEN
            ASSIGN i-seq = bdvv_log_provisao_exec.sequencia + 1.

        CREATE dvv_log_provisao_exec.
        ASSIGN dvv_log_provisao_exec.num_id_provisao = b_dvv_log_provis_estorno.num_id_provisao
               dvv_log_provisao_exec.sequencia       = i-seq
               dvv_log_provisao_exec.dt_atualiz      = TODAY
               dvv_log_provisao_exec.cod_usuario     = v_cod_usuar_corren
               dvv_log_provisao_exec.hr_atualiz      = replace(STRING(TIME,"HH:MM:SS"),":","")
               dvv_log_provisao_exec.mensagem        = string(tt_erro_import.cd-erro) + " - " + tt_erro_import.mensagem + " - " + tt_erro_import.ajuda
               dvv_log_provisao_exec.status_execucao = "Erro".

        /************
        FOR EACH tt_erro_import:
            MESSAGE tt_erro_import.cod_empresa SKIP
                    tt_erro_import.cd-erro     SKIP
                    tt_erro_import.mensagem    SKIP
                    tt_erro_import.ajuda       VIEW-AS ALERT-BOX.

        END.
        ********/
    END.
    RELEASE dvv_log_provisao_exec.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gerar-dados w-livre 
PROCEDURE pi-gerar-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp ("Monitor Provis∆o").

EMPTY TEMP-TABLE tt-dvv_log_provisao.

IF p-ind-situacao = 5 THEN DO:

    FOR EACH estabelec WHERE
             estabelec.cod-estabel >= p-cod-estabel-ini AND
             estabelec.cod-estabel <= p-cod-estabel-fim NO-LOCK:
             
        FOR EACH dvv_log_provisao WHERE
                 dvv_log_provisao.cod_estabel = estabelec.cod-estabel AND
                 dvv_log_provisao.dt_provisao >= p-dt-provisao-ini   AND
                 dvv_log_provisao.dt_provisao <= p-dt-provisao-fim   and
                 (dvv_log_provisao.dt_lancto_provis = ? OR
                 (dvv_log_provisao.dt_lancto_provis >= p-dt-lancto-ini   AND
                 dvv_log_provisao.dt_lancto_provis  <= p-dt-lancto-fim))  and
                 dvv_log_provisao.cod_cta_ctbl_db  >= p-cta-ctbl-ini   and
                 dvv_log_provisao.cod_cta_ctbl_db  <= p-cta-ctbl-fim    NO-LOCK :

            IF NOT p-lg-dex   AND dvv_log_provisao.origem = "DEX" THEN NEXT.
            IF NOT p-lg-dvvme AND dvv_log_provisao.origem = "DVV ME" THEN NEXT.
            IF NOT p-lg-dvvmi AND dvv_log_provisao.origem = "DVV MI" THEN NEXT.
			IF NOT p-lg-dvvrem  AND dvv_log_provisao.origem = "DVV REM" THEN NEXT.  
            IF NOT p-lg-pdex   AND (dvv_log_provisao.origem = "PDEX"    OR dvv_log_provisao.origem = "EPDEX") THEN NEXT.
            IF NOT p-lg-pdvvme AND (dvv_log_provisao.origem = "PDVV ME" OR dvv_log_provisao.origem = "EPDVV ME") THEN NEXT.
            IF NOT p-lg-pdvvmi AND (dvv_log_provisao.origem = "PDVV MI" OR dvv_log_provisao.origem = "EPDVV MI") THEN NEXT.
			IF NOT p-lg-pdvvrem  AND (dvv_log_provisao.origem = "PDVV REM" OR dvv_log_provisao.origem = "EPDVV REM") THEN NEXT.  /* NOVO */.

            CREATE tt-dvv_log_provisao.
            BUFFER-COPY dvv_log_provisao TO tt-dvv_log_provisao.
            ASSIGN tt-dvv_log_provisao.r_rowid = ROWID(dvv_log_provisao).
            IF dvv_log_provisao.situacao = 1 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Pendente".
            IF dvv_log_provisao.situacao = 2 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Provisionado".
            IF dvv_log_provisao.situacao = 3 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Cancelado".
            IF dvv_log_provisao.situacao = 4 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Estornado".
    
            /* Acessa PLANO DE CONTAS */
            FIND LAST plano_cta_unid_organ no-lock where 
                      plano_cta_unid_organ.cod_unid_organ          = v_cod_empres_usuar
                  AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl  = "Prim†rio" 
                  AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
                  AND plano_cta_unid_organ.dat_fim_valid          >= TODAY NO-ERROR.

            FIND FIRST cta_ctbl WHERE
                       cta_ctbl.cod_plano_cta_ctbl  = plano_cta_unid_organ.cod_plano_cta_ctbl AND
                       cta_ctbl.cod_cta_ctbl        = dvv_log_provisao.cod_cta_ctbl_db NO-LOCK NO-ERROR.
            ASSIGN tt-dvv_log_provisao.des_tit_ctbl = cta_ctbl.des_tit_ctbl.

            ASSIGN tt-dvv_log_provisao.val_lancto_euro = 0.
            IF dvv_log_provisao.cod_estabel = "002" THEN DO:
                ASSIGN p-data-real = dvv_log_provisao.dt_provisao.
                IF dvv_log_provisao.situacao = 2 THEN
                    ASSIGN p-data-real = dvv_log_provisao.dt_lancto_provis.
                IF dvv_log_provisao.situacao = 4 THEN
                    ASSIGN p-data-real = dvv_log_provisao.dt_lancto_estorno.

                run cdp/cd0812.p (input  iRealVenda, /*moeda realizado*/
                                  input  iEuroVenda, /*0*/
                                  input  dvv_log_provisao.vl_provisao, /*valor realizado*/
                                  input  p-data-real,
                                  output de-valor-conv).
                if  de-valor-conv <> ? then
                    ASSIGN tt-dvv_log_provisao.val_lancto_euro = de-valor-conv.
            END.
        END.
    END.
END.
ELSE DO:

    FOR EACH estabelec WHERE
             estabelec.cod-estabel >= p-cod-estabel-ini AND
             estabelec.cod-estabel <= p-cod-estabel-fim NO-LOCK:

        FOR EACH dvv_log_provisao WHERE
                 dvv_log_provisao.cod_estabel      = estabelec.cod-estabel AND
                 dvv_log_provisao.dt_provisao >= p-dt-provisao-ini   AND
                 dvv_log_provisao.dt_provisao <= p-dt-provisao-fim   and
                 (dvv_log_provisao.dt_lancto_provis = ? OR 
                 (dvv_log_provisao.dt_lancto_provis >= p-dt-lancto-ini    AND
                 dvv_log_provisao.dt_lancto_provis  <= p-dt-lancto-fim))    and
                 dvv_log_provisao.cod_cta_ctbl_db  >= p-cta-ctbl-ini   and
                 dvv_log_provisao.cod_cta_ctbl_db  <= p-cta-ctbl-fim   AND
                 dvv_log_provisao.situacao         = p-ind-situacao NO-LOCK :

            IF NOT p-lg-dex   AND dvv_log_provisao.origem = "DEX" THEN NEXT.
            IF NOT p-lg-dvvme AND dvv_log_provisao.origem = "DVV ME" THEN NEXT.
            IF NOT p-lg-dvvmi AND dvv_log_provisao.origem = "DVV MI" THEN NEXT.
			IF NOT p-lg-dvvrem  AND dvv_log_provisao.origem = "DVV-REM" THEN NEXT.
            IF NOT p-lg-pdex   AND (dvv_log_provisao.origem = "PDEX"    OR dvv_log_provisao.origem = "EPDEX") THEN NEXT.
            IF NOT p-lg-pdvvme AND (dvv_log_provisao.origem = "PDVV ME" OR dvv_log_provisao.origem = "EPDVV ME") THEN NEXT.
            IF NOT p-lg-pdvvmi AND (dvv_log_provisao.origem = "PDVV MI" OR dvv_log_provisao.origem = "EPDVV MI") THEN NEXT.
			IF NOT p-lg-pdvvrem  AND (dvv_log_provisao.origem = "PDVV-REM" OR dvv_log_provisao.origem = "EPDVV-REM") THEN NEXT.  /* NOVO */

            CREATE tt-dvv_log_provisao.
            BUFFER-COPY dvv_log_provisao TO tt-dvv_log_provisao.
            ASSIGN tt-dvv_log_provisao.r_rowid = ROWID(dvv_log_provisao).
            IF dvv_log_provisao.situacao = 1 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Pendente".
            IF dvv_log_provisao.situacao = 2 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Provisionado".
            IF dvv_log_provisao.situacao = 3 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Cancelado".
            IF dvv_log_provisao.situacao = 4 THEN
                ASSIGN tt-dvv_log_provisao.DESC_situacao = "Estornado".

            /* Acessa PLANO DE CONTAS */
            FIND LAST plano_cta_unid_organ no-lock where 
                      plano_cta_unid_organ.cod_unid_organ          = v_cod_empres_usuar
                  AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl  = "Prim†rio" 
                  AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
                  AND plano_cta_unid_organ.dat_fim_valid          >= TODAY NO-ERROR.

            FIND FIRST cta_ctbl WHERE
                       cta_ctbl.cod_plano_cta_ctbl  = plano_cta_unid_organ.cod_plano_cta_ctbl AND
                       cta_ctbl.cod_cta_ctbl        = dvv_log_provisao.cod_cta_ctbl_db NO-LOCK NO-ERROR.
            ASSIGN tt-dvv_log_provisao.des_tit_ctbl = cta_ctbl.des_tit_ctbl.
            /*dever† converter a moeda para a moeda da previs∆o com a data da realizaá∆o*/

            ASSIGN tt-dvv_log_provisao.val_lancto_euro = 0.
            IF dvv_log_provisao.cod_estabel = "002" THEN DO:
                ASSIGN p-data-real = dvv_log_provisao.dt_provisao.
                IF dvv_log_provisao.situacao = 2 THEN
                    ASSIGN p-data-real = dvv_log_provisao.dt_lancto_provis.
                IF dvv_log_provisao.situacao = 4 THEN
                    ASSIGN p-data-real = dvv_log_provisao.dt_lancto_estorno.

                run cdp/cd0812.p (input  iRealVenda, /*moeda realizado*/
                                  input  iEuroVenda, /*0*/
                                  input  dvv_log_provisao.vl_provisao, /*valor realizado*/
                                  input  p-data-real,
                                  output de-valor-conv).
                if  de-valor-conv <> ? then
                    ASSIGN tt-dvv_log_provisao.val_lancto_euro = de-valor-conv.
            END.
        END.
    END.
END.



RUN pi-finalizar IN h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-erros w-livre 
PROCEDURE pi-imprime-erros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/************
    DEF VAR cEditor AS CHAR NO-UNDO.
    DEF VAR cArquivo AS CHAR NO-UNDO.
    DEF VAR vLog    AS LOGICAL  NO-UNDO.
    
    /*
    /* definicao do arquivo de saida nao de impressora e sim de arquivo texto */
    
    find first param-global no-lock.
    find first mguni.empresa no-lock.
    
    assign c-programa     = "ESEX5003RP.P"
           c-versao       = "2.06"
           c-revisao      = "01.000"
           c-empresa      = param-global.grupo
           c-titulo-relat = "Get Arquivos INTTRA"
           c-sistema      = "EXP".
    */
    
    ASSIGN cArquivo = (session:temp-directory + "exes5004" + ".txt":U).
    
    OUTPUT TO VALUE (cArquivo) NO-CONVERT.
    
    PUT "Estab"         AT 01
        "Nr Processo"   AT 07
        "Tip Movimento" AT 20
        "Sequ"          AT 41
        "Erro"          AT 46
        "Mensagem"      AT 52 SKIP
        "-----"                AT 01
        "------------"         at 07
        "--------------------" AT 20
        "----"                 AT 41
        "-----"                AT 46
        "----------------------------------------------------------------------------------------------------------------------------" AT 52 SKIP.
    
    FOR EACH tt-erro-aux:
        PUT tt-erro-aux.cod_estabel   AT 01
            tt-erro-aux.nr_proc_exp   AT 07
            tt-erro-aux.tip_movimento AT 20
            tt-erro-aux.i-sequen      AT 42
            tt-erro-aux.cd-erro       AT 46
            tt-erro-aux.mensagem      AT 52 SKIP.
    
    END.
    
    OUTPUT CLOSE.
    
    DOS SILENT START value(cArquivo).

    /************/
    ASSIGN  cEditor = OS-GETENV("windir") + "~\notepad.exe"
            vLog    = YES.
    RUN winexec (INPUT cEditor + CHR(32) + cArquivo, INPUT 1).
    /************/
**********/    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-dvv_log_provisao"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

