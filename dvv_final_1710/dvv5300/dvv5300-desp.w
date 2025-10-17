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
{include/i-prgvrs.i DVV5300-log 2.06.00.001}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i dvv5300-desp MEX}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF INPUT PARAMETER p-r-dvv_log_provisao AS ROWID NO-UNDO.

DEF BUFFER bdvv_log_prov_desp FOR dvv_log_prov_desp.
DEF BUFFER b1dvv_log_prov_desp FOR dvv_log_prov_desp.
DEF BUFFER b-emit-fornec FOR emitente.

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
    FIELD novo            AS LOG
    FIELD provisiona      AS CHAR
    FIELD cod-cliente     AS INT
    FIELD nome_cliente    AS CHAR.

DEF TEMP-TABLE tt-dvv_log_prov_desp_item  LIKE tt-dvv_log_prov_desp
    FIELD it_codigo       AS CHAR
    FIELD grupo_item      AS CHAR
    FIELD grupo-classe    AS CHAR
    FIELD val_desp_rat    AS DEC
    FIELD cod_incoterm    AS CHAR
    FIELD tipo_venda AS CHAR
    FIELD val_desp_euro_it AS DEC.

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


/* Local Variable Definitions ---                                       */

DEF VAR i-width AS INTEGER NO-UNDO.
DEF VAR i-heigth AS INTEGER NO-UNDO.

DEF VAR h-bocx185 AS HANDLE NO-UNDO.

def var h as handle.
DEF VAR h-acomp AS HANDLE NO-UNDO.
DEF VAR de-valor-prov AS DEC NO-UNDO.
DEF VAR p-data-real AS DATE NO-UNDO.
DEF VAR de-valor-conv AS DEC NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt-dvv_log_prov_desp_item

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-dvv_log_prov_desp_item.provisiona tt-dvv_log_prov_desp_item.dt_emissao tt-dvv_log_prov_desp_item.novo tt-dvv_log_prov_desp_item.nr_proc_exp tt-dvv_log_prov_desp_item.cod-cliente tt-dvv_log_prov_desp_item.nome_cliente tt-dvv_log_prov_desp_item.it_codigo tt-dvv_log_prov_desp_item.grupo_item tt-dvv_log_prov_desp_item.fm_codigo tt-dvv_log_prov_desp_item.grupo-classe tt-dvv_log_prov_desp_item.cod_despesa tt-dvv_log_prov_desp_item.DESC_despesa tt-dvv_log_prov_desp_item.cod_fornec tt-dvv_log_prov_desp_item.DESC_fornec tt-dvv_log_prov_desp_item.val_desp_rat /*tt-dvv_log_prov_desp_item.vl_despesa*/ tt-dvv_log_prov_desp_item.val_desp_euro_it tt-dvv_log_prov_desp_item.reg-seg tt-dvv_log_prov_desp_item.cod_incoterm tt-dvv_log_prov_desp_item.tipo_venda   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-dvv_log_prov_desp_item
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-dvv_log_prov_desp_item.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-dvv_log_prov_desp_item
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-dvv_log_prov_desp_item


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button BROWSE-2 

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
DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-dvv_log_prov_desp_item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-dvv_log_prov_desp_item.provisiona    COLUMN-LABEL "Provisiona" FORMAT "x(03)":U          
        tt-dvv_log_prov_desp_item.dt_emissao    COLUMN-LABEL "Reconhec.Rec." FORMAT "99/99/9999"
        tt-dvv_log_prov_desp_item.novo          COLUMN-LABEL "Novo" FORMAT "Sim/NÆo"
        tt-dvv_log_prov_desp_item.nr_proc_exp   column-label "Processo/NF"
        tt-dvv_log_prov_desp_item.cod-cliente   COLUMN-LABEL "Cliente"
        tt-dvv_log_prov_desp_item.nome_cliente  COLUMN-LABEL "Nome Cliente" FORMAT "x(40)"
        tt-dvv_log_prov_desp_item.it_codigo     COLUMN-LABEL "Item" FORMAT "x(16)"
        tt-dvv_log_prov_desp_item.grupo_item    COLUMN-LABEL "Grupo" FORMAT "x(30)"
        tt-dvv_log_prov_desp_item.fm_codigo     COLUMN-LABEL "Familia" FORMAT "x(20)"
        tt-dvv_log_prov_desp_item.grupo-classe  COLUMN-LABEL "Grupo Familia" FORMAT "x(30)"
        tt-dvv_log_prov_desp_item.cod_despesa   column-label "Despesa"    
        tt-dvv_log_prov_desp_item.DESC_despesa  COLUMN-LABEL "Descri‡Æo"  FORMAT "x(30)"
        tt-dvv_log_prov_desp_item.cod_fornec    COLUMN-LABEL "Fornec"
        tt-dvv_log_prov_desp_item.DESC_fornec   COLUMN-LABEL "Desc.Fornec" FORMAT "X(30)"
        tt-dvv_log_prov_desp_item.val_desp_rat /*tt-dvv_log_prov_desp_item.vl_despesa*/    COLUMN-LABEL "Vl.Despesa"
        tt-dvv_log_prov_desp_item.val_desp_euro_it COLUMN-LABEL "Vl.Desp.Euro"
        tt-dvv_log_prov_desp_item.reg-seg       COLUMN-LABEL "Reg/Seg" FORMAT "x(30)"
        tt-dvv_log_prov_desp_item.cod_incoterm  COLUMN-LABEL "Incoterm" FORMAT "x(4)"
        tt-dvv_log_prov_desp_item.tipo_venda    COLUMN-LABEL "Tipo Venda" FORMAT "x(10)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86.72 BY 14.5 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BROWSE-2 AT ROW 2.63 COL 2.29
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
         HEIGHT             = 17.46
         WIDTH              = 90
         MAX-HEIGHT         = 19.38
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 19.38
         VIRTUAL-WIDTH      = 90
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
/* BROWSE-TAB BROWSE-2 rt-button f-cad */
ASSIGN 
       FRAME f-cad:SCROLLABLE       = FALSE.

ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME f-cad = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-dvv_log_prov_desp_item.
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
  /*APPLY "CHOOSE" TO bt-provisionar IN FRAME f-cad.*/
    FIND dvv_log_provisao WHERE
         dvv_log_provisao.num_id_provisao = tt-dvv_log_prov_desp_item.num_id_provisao NO-LOCK NO-ERROR.

    IF dvv_log_provisao.origem_movto = "DEX" THEN DO:    
        FIND dex_movto
            WHERE dex_movto.cod_estabel  = "002" /*tt-dvv_log_prov_desp_item.cod_estabel*/
              AND dex_movto.Nr_PROCESSO  = tt-dvv_log_prov_desp_item.nr_proc_exp                         
              AND dex_movto.TP_DeSPESA   = tt-dvv_log_prov_desp_item.cod_despesa EXCLUSIVE-LOCK NO-ERROR.

        /*IF substring(dex_movto.CHAR_2,1,1) <> "" AND dex_movto.CHAR_2 <> ? THEN*/
        ASSIGN overlay(dex_movto.CHAR_2,1,1) = IF tt-dvv_log_prov_desp_item.provisiona = "Sim" THEN "0" ELSE "1".

    END.

    IF dvv_log_provisao.origem_movto = "DVV ME" THEN DO:
        
        FIND FIRST dvv_movto
          WHERE /*dvv_movto.cod_estabel  = processo-exp.cod-estabel
            AND*/ dvv_movto.NUM_PROCESSO = tt-dvv_log_prov_desp_item.nr_proc_exp
            AND dvv_movto.TIPO_DESPESA   = tt-dvv_log_prov_desp_item.cod_despesa EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL dvv_movto THEN
          ASSIGN OVERLAY(dvv_movto.CHAR_2,1,1) = IF tt-dvv_log_prov_desp_item.provisiona = "Sim" THEN "0" ELSE "1".
        FIND CURRENT dvv_movto NO-LOCK NO-ERROR.
    END.

    IF dvv_log_provisao.origem_movto = "DVV MI" THEN DO:
        
        FIND FIRST dvv_movto_mi WHERE         
                   dvv_movto_mi.cod_estabel  = tt-dvv_log_prov_desp_item.cod_estabel AND
                   dvv_movto_mi.serie        = "6" AND
                   dvv_movto_mi.nr-nota-fis  = tt-dvv_log_prov_desp_item.nr_proc_exp AND
                   dvv_movto_mi.tipo_despesa = tt-dvv_log_prov_desp_item.cod_despesa EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL dvv_movto_mi THEN
          ASSIGN OVERLAY(dvv_movto_mi.CHAR_3,1,1) = IF tt-dvv_log_prov_desp_item.provisiona = "Sim" THEN "0" ELSE "1".
        FIND CURRENT dvv_movto_mi NO-LOCK NO-ERROR.
    END.
	
	/* --- NOVO: tratar DVV-REM (Remessa/Venda Consignada) --- */
    IF dvv_log_provisao.origem_movto = "DVV REM" THEN DO:
        /* 
           Atenção: estamos reutilizando a mesma origem de DVV ME (tabela dvv_movto)
           e o mesmo flag CHAR_2 posição 1. Se sua base usar outra tabela/campo
           para remessa, troque aqui por ela.
        */
        FIND FIRST dvv_movto
          WHERE /* opcional: dvv_movto.cod_estabel = tt-dvv_log_prov_desp_item.cod_estabel AND */
                dvv_movto.NUM_PROCESSO = tt-dvv_log_prov_desp_item.nr_proc_exp
            AND dvv_movto.TIPO_DESPESA = tt-dvv_log_prov_desp_item.cod_despesa
        EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL dvv_movto THEN
            ASSIGN OVERLAY(dvv_movto.CHAR_2,1,1) = IF tt-dvv_log_prov_desp_item.provisiona = "Sim" THEN "0" ELSE "1".
        FIND CURRENT dvv_movto NO-LOCK NO-ERROR.
    END.

    /* --- NOVO: prévias PDVV-REM não escrevem na origem (comportamento igual às demais prévias) --- */
    IF dvv_log_provisao.origem_movto = "PDVV-REM" 
       OR dvv_log_provisao.origem_movto = "EPDVV-REM" THEN DO:
        /* nenhuma escrita em tabela origem; apenas atualiza dvv_log_prov_desp abaixo */
    END.

    FIND dvv_log_prov_desp WHERE 
         ROWID(dvv_log_prov_desp) = tt-dvv_log_prov_desp_item.r_rowid EXCLUSIVE-LOCK NO-ERROR.

    /*marca se provisiona ou nÆo*/
    ASSIGN overlay(dvv_log_prov_desp.CHAR_1,1,1) = IF tt-dvv_log_prov_desp_item.provisiona = "Sim" THEN "0" ELSE "1".

    FIND CURRENT dvv_log_prov_desp NO-LOCK NO-ERROR.
    /*RUN local-open-query IN THIS-PROCEDURE.*/

    ASSIGN de-valor-prov = 0.
    FOR EACH b1dvv_log_prov_desp WHERE
             b1dvv_log_prov_desp.num_id_provisao = tt-dvv_log_prov_desp_item.num_id_provisao NO-LOCK:

        IF SUBSTRING(b1dvv_log_prov_desp.CHAR_1,1,1) = "0" THEN NEXT. /*nÆo provisiona*/

        ASSIGN de-valor-prov = de-valor-prov + b1dvv_log_prov_desp.vl_despesa.

    END.

    FIND CURRENT dvv_log_provisao EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN dvv_log_provisao.vl_provisao = de-valor-prov.
    FIND CURRENT dvv_log_provisao NO-LOCK NO-ERROR.

    RUN pi-gerar-dados.
    open query {&browse-name} for each tt-dvv_log_prov_desp_item.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON ROW-DISPLAY OF BROWSE-2 IN FRAME f-cad
DO:                                 
  IF tt-dvv_log_prov_desp_item.provisiona = "NÆo" THEN DO:
      ASSIGN tt-dvv_log_prov_desp_item.provisiona:BGCOLOR IN BROWSE browse-2 = 12 
             tt-dvv_log_prov_desp_item.dt_emissao:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.novo:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.nr_proc_exp:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.cod-cliente:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.nome_cliente:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.it_codigo:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.grupo_item:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.fm_codigo:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.grupo-classe:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.cod_despesa:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.DESC_despesa:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.cod_fornec:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.DESC_fornec:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.val_desp_rat:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.val_desp_euro_it:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.reg-seg:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.cod_incoterm:BGCOLOR IN BROWSE browse-2 = 12
             tt-dvv_log_prov_desp_item.tipo_venda:BGCOLOR IN BROWSE browse-2 = 12.

  END.





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
       RUN set-position IN h_p-exihel ( 1.13 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             BROWSE-2:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  ENABLE rt-button BROWSE-2 
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
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "DVV5300-DESP" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN pi-gerar-dados.
  open query {&browse-name} for each tt-dvv_log_prov_desp_item.

  /* Code placed here will execute AFTER standard behavior.    */

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

    DEF VAR i-pto-contr AS INT NO-UNDO.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    
    RUN pi-inicializar IN h-acomp ("Detalhe DVV").
    RUN pi-acompanhar IN h-acomp ("Despesas...").
    
    FIND dvv_log_provisao WHERE
         rowid(dvv_log_provisao) = p-r-dvv_log_provisao NO-LOCK NO-ERROR.
    
    /*
    IF dvv_log_provisao.situacao <> 1 THEN
        DISABLE bt-provisionar WITH FRAME {&FRAME-NAME}.
    */

    EMPTY TEMP-TABLE tt-dvv_log_prov_desp.
    EMPTY TEMP-TABLE tt-dvv_log_prov_desp_item.

    FOR EACH dvv_log_prov_desp WHERE
             dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao NO-LOCK:

        PROCESS EVENTS.

        CREATE tt-dvv_log_prov_desp.
        BUFFER-COPY dvv_log_prov_desp TO tt-dvv_log_prov_desp.
        ASSIGN tt-dvv_log_prov_desp.r_rowid = ROWID(dvv_log_prov_desp).

        {dvv/dvv5300-desp.i}

    END.
        
    RUN pi-acompanhar IN h-acomp ("Rateio...").
    
    RUN pi-gerar-rateio.

    RUN pi-finalizar IN h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gerar-rateio w-livre 
PROCEDURE pi-gerar-rateio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i-cod-estabel AS CHAR NO-UNDO.

{dvv/dvv5300-desp.i2}


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
  {src/adm/template/snd-list.i "tt-dvv_log_prov_desp_item"}

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

