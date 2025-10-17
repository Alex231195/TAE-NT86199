&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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

/* Parameters Definitions ---                                           */
{utp/ut-glob.i}
DEF NEW GLOBAL SHARED VAR gl-atualiza-filtro AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
/*DEFINE INPUT PARAM c-usuario AS char   NO-UNDO.*/
DEF OUTPUT PARAMETER p-cod-estabel-ini AS CHARACTER NO-UNDO.
DEF OUTPUT PARAMETER p-cod-estabel-fim AS CHARACTER NO-UNDO.
DEF OUTPUT PARAMETER p-dt-provisao-ini   AS DATE NO-UNDO.
DEF OUTPUT PARAMETER p-dt-provisao-fim   AS DATE NO-UNDO.
DEF OUTPUT PARAMETER p-dt-lancto-ini  AS DATE NO-UNDO.
DEF OUTPUT PARAMETER p-dt-lancto-fim  AS DATE NO-UNDO.
DEF OUTPUT PARAMETER p-cta-ctbl-ini    AS CHARACTER NO-UNDO.
DEF OUTPUT PARAMETER p-cta-ctbl-fim    AS CHARACTER NO-UNDO.
DEF OUTPUT PARAMETER p-lg-dex            AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-lg-dvvme          AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-lg-dvvmi          AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-lg-pdex            AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-lg-pdvvme          AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-lg-pdvvmi          AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-ind-situacao      AS INT NO-UNDO.
DEF OUTPUT PARAMETER p-lg-dvvrem         AS LOG NO-UNDO.
DEF OUTPUT PARAMETER p-lg-pdvvrem        AS LOG NO-UNDO.


DEFINE VARIABLE    wh-pesquisa  AS HANDLE  NO-UNDO.
DEFINE VARIABLE    l-implanta   AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS c-cod-estabel-ini c-cod-estabel-fim ~
dt-provisao-ini dt-provisao-fim dt-lancto-ini dt-lancto-fim c-cta-ctbl-ini ~
c-cta-ctbl-fim lg-dex lg-dvvme lg-dvvmI lg-dvvrem lg-pdex lg-pdvvme lg-pdvvmI lg-pdvvrem rs-ind-situacao bt-ok bt-cancela ~
rt-buttom IMAGE-3 IMAGE-5 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 ~
IMAGE-12 

&Scoped-Define DISPLAYED-OBJECTS c-cod-estabel-ini c-cod-estabel-fim ~
dt-provisao-ini dt-provisao-fim dt-lancto-ini dt-lancto-fim c-cta-ctbl-ini ~
c-cta-ctbl-fim lg-dex lg-dvvme lg-dvvmI lg-dvvrem lg-pdex lg-pdvvme lg-pdvvmI lg-pdvvrem rs-ind-situacao  

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabel" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE c-cta-ctbl-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-cta-ctbl-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cta.Ctbl.Db" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE dt-lancto-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt-lancto-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Lancto.Ctbl" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.
DEFINE VARIABLE dt-provisao-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE dt-provisao-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.ProvisÆo" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image/im-las.bmp":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-11
     FILENAME "image/im-fir.bmp":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-12
     FILENAME "image/im-las.bmp":U
     SIZE 3 BY 1.
DEFINE IMAGE IMAGE-3
     FILENAME "image/im-fir.bmp":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "image/im-las.bmp":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-7
     FILENAME "image/im-fir.bmp":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-8
     FILENAME "image/im-las.bmp":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-9
     FILENAME "image/im-fir.bmp":U
     SIZE 3 BY 1.

DEFINE VARIABLE rs-ind-situacao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pendente", 1,
"Provisionado", 2,
"Cancelado", 3,
"Estornado", 4,
"Todos", 5
     SIZE 63.86 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 76.86 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE lg-dex AS LOGICAL INITIAL no 
     LABEL "DEX" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE lg-dvvme AS LOGICAL INITIAL no 
     LABEL "DVV ME" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE lg-dvvmi AS LOGICAL INITIAL no 
     LABEL "DVV MI" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE lg-pdex AS LOGICAL INITIAL no 
     LABEL "PDEX" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE lg-pdvvme AS LOGICAL INITIAL no 
     LABEL "PDVV ME" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE lg-pdvvmi AS LOGICAL INITIAL no 
     LABEL "PDVV MI" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.
	 
DEFINE VARIABLE lg-dvvrem AS LOGICAL INITIAL no 
     LABEL "DVV-REM"
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE lg-pdvvrem AS LOGICAL INITIAL no 
     LABEL "PDVV-REM"
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.	 

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod-estabel-ini AT ROW 1.5 COL 13.14 COLON-ALIGNED WIDGET-ID 14
     c-cod-estabel-fim AT ROW 1.5 COL 39.72 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     dt-provisao-ini AT ROW 2.5 COL 13.14 COLON-ALIGNED
     dt-provisao-fim AT ROW 2.5 COL 39.72 COLON-ALIGNED NO-LABEL
     dt-lancto-ini AT ROW 3.54 COL 13.14 COLON-ALIGNED WIDGET-ID 44
     dt-lancto-fim AT ROW 3.54 COL 39.86 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     c-cta-ctbl-ini AT ROW 4.67 COL 13 COLON-ALIGNED WIDGET-ID 30
     c-cta-ctbl-fim AT ROW 4.63 COL 39.72 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     lg-dex AT ROW 5.88 COL 15 WIDGET-ID 36
     lg-dvvme AT ROW 5.88 COL 24.72 WIDGET-ID 38
     lg-dvvmI AT ROW 5.88 COL 37 WIDGET-ID 40
     lg-pdex AT ROW 6.88 COL 15 WIDGET-ID 36
     lg-pdvvme AT ROW 6.88 COL 24.72 WIDGET-ID 38
     lg-pdvvmI AT ROW 6.88 COL 37 WIDGET-ID 40
	 lg-dvvrem  AT ROW 5.88 COL 49.28
     lg-pdvvrem AT ROW 6.88 COL 49.28
     rs-ind-situacao AT ROW 8 COL 15.14 NO-LABEL WIDGET-ID 20
     bt-ok AT ROW 9.83 COL 2.14
     bt-cancela AT ROW 9.83 COL 13.14
     "Situa‡Æo:" VIEW-AS TEXT
          SIZE 9.14 BY .67 AT ROW 8.08 COL 5.72 WIDGET-ID 26
     "Tipo Movto:" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 5.88 COL 3.29 WIDGET-ID 42
     "Pr‚vias:" VIEW-AS TEXT
         SIZE 11 BY .67 AT ROW 6.88 COL 3.29 WIDGET-ID 42
	 	 

     rt-buttom AT ROW 9.58 COL 1.14
     IMAGE-3 AT ROW 2.54 COL 34.72
     IMAGE-5 AT ROW 2.54 COL 38.14
     IMAGE-7 AT ROW 1.54 COL 34.72 WIDGET-ID 16
     IMAGE-8 AT ROW 1.54 COL 38.14 WIDGET-ID 18
     IMAGE-9 AT ROW 4.67 COL 34.72 WIDGET-ID 32
     IMAGE-10 AT ROW 4.67 COL 38.14 WIDGET-ID 34
     IMAGE-11 AT ROW 3.54 COL 34.72 WIDGET-ID 46
     IMAGE-12 AT ROW 3.54 COL 38.14 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 78.72 BY 10.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Parƒmetros Monitor DEX/DVV"
         HEIGHT             = 10.17
         WIDTH              = 77.72
         MAX-HEIGHT         = 19.13
         MAX-WIDTH          = 88.72
         VIRTUAL-HEIGHT     = 19.13
         VIRTUAL-WIDTH      = 88.72
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Parƒmetros Monitor DEX/DVV */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Parƒmetros Monitor DEX/DVV */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main W-Win
ON ENTRY OF FRAME F-Main
DO:
    ASSIGN c-cod-estabel-ini = "001"
           c-cod-estabel-fim = "ZZZ"
           dt-provisao-ini   = TODAY
           dt-provisao-fim   = TODAY
           dt-lancto-ini     = TODAY
           dt-lancto-fim     = TODAY
           c-cta-ctbl-ini    = ""
           c-cta-ctbl-fim    = "ZZZZZZZZ"
           lg-dex            = YES
           lg-dvvme          = YES
           lg-dvvmi          = YES
           lg-pdex           = NO
           lg-pdvvme         = NO
           lg-pdvvmi         = NO
           rs-ind-situacao   = 1
		   lg-dvvrem         = NO   
		   lg-pdvvrem        = NO  .

    DISP c-cod-estabel-ini 
         c-cod-estabel-fim 
         dt-provisao-ini   
         dt-provisao-fim   
         dt-lancto-ini 
         dt-lancto-fim 
         c-cta-ctbl-ini    
         c-cta-ctbl-fim    
         lg-dex            
         lg-dvvme          
         lg-dvvmi          
         lg-pdex            
         lg-pdvvme          
         lg-pdvvmi          
         rs-ind-situacao
		 lg-dvvrem          /* NOVO */
         lg-pdvvrem         /* NOVO */
        WITH FRAME f-main.





/**************
    FIND es_param_monitor
        WHERE es_param_monitor.cod_usuar = c-usuario EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL es_param_monitor THEN DO:
        ASSIGN    
        c-nr-proc-ini           =   es_param_monitor.nr_proc_exp_ini       
        c-nr-proc-fim           =   es_param_monitor.nr_proc_exp_fim      
        l-venda-direta          =   es_param_monitor.LOG_venda_direta     
        l-venda-fc              =   es_param_monitor.LOG_venda_fc         
        l-venda-parcial-us      =   es_param_monitor.log_venda_parcial_us 
        l-venda-parcial-eu      =   es_param_monitor.log_venda_parcial_eu 
        l-remessa-us            =   es_param_monitor.log_remessa_us       
        l-remessa-eu            =   es_param_monitor.log_remessa_eu       
        l-africa                =   es_param_monitor.log_africa           
        l-africa-sul            =   es_param_monitor.log_africa_sul       
        l-amr                   =   es_param_monitor.log_amr              
        l-amr-2                 =   es_param_monitor.log_amr2             
        l-amr-3                 =   es_param_monitor.log_amr3             
        l-asia                  =   es_param_monitor.log_asia             
        l-europa                =   es_param_monitor.log_europa           
        l-middle-east           =   es_param_monitor.log_middle_east      
        l-usca                  =   es_param_monitor.log_usca             
        i-dias-status           =   es_param_monitor.dias_status          
        l-stand-by              =   es_param_monitor.log_stand_by         
        l-preparacao            =   es_param_monitor.log_preparacao       
        l-saida-nitro           =   es_param_monitor.log_saida_nitro      
        l-desembaracado         =   es_param_monitor.log_desembaracado    
        l-produto-aguas         =   es_param_monitor.log_produto_aguas    
        l-apos-fronteira        =   es_param_monitor.log_pos_fronteira              
        l-transbordo            =   es_param_monitor.log_transbordo                 
        l-porto-destino         =   es_param_monitor.log_porto_destino              
        l-entregue              =   es_param_monitor.log_entregue_cliente           
        l-stand-by-parcial      =   es_param_monitor.log_stand_by_parcial           
        l-envio                 =   es_param_monitor.log_instrucoes_parcial         
        l-faturado              =   es_param_monitor.log_faturado                   
        l-entregue-parcial      =   es_param_monitor.log_entregue_cliente_parcial   
        l-maritimo              =   es_param_monitor.log_maritimo                   
        l-rodoviario            =   es_param_monitor.log_rodoviario
        d-dt-implant-ini        =   es_param_monitor.dt_implant_ini
        d-dt-implant-fim        =   es_param_monitor.dt_implant_fim
        l-amostra               =   IF SUBSTRING(es_param_monitor.char_1,1,10) <> "" AND SUBSTRING(es_param_monitor.char_1,1,10) <> ? THEN LOGICAL(SUBSTRING(es_param_monitor.char_1,1,10)) ELSE YES
        l-venda-parcial-ita     =   IF SUBSTRING(es_param_monitor.char_1,11,10) <> "" AND SUBSTRING(es_param_monitor.char_1,11,10) <> ? THEN LOGICAL(SUBSTRING(es_param_monitor.char_1,11,10)) ELSE YES 
        l-remessa-ita           =   IF SUBSTRING(es_param_monitor.char_1,21,10) <> "" AND SUBSTRING(es_param_monitor.char_1,21,10) <> ? THEN LOGICAL(SUBSTRING(es_param_monitor.char_1,21,10)) ELSE YES .
        l-venda-parcial-fra     =   IF SUBSTRING(es_param_monitor.char_1,31,10) <> "" AND SUBSTRING(es_param_monitor.char_1,31,10) <> ? THEN LOGICAL(SUBSTRING(es_param_monitor.char_1,31,10)) ELSE YES .
        l-remessa-fra           =   IF SUBSTRING(es_param_monitor.char_1,41,10) <> "" AND SUBSTRING(es_param_monitor.char_1,41,10) <> ? THEN LOGICAL(SUBSTRING(es_param_monitor.char_1,41,10)) ELSE YES .
            
            .

        DISP 
            c-nr-proc-ini        
            c-nr-proc-fim        
            l-venda-direta       
            l-venda-fc           
            l-venda-parcial-us    
            l-venda-parcial-eu    
            l-remessa-us         
            l-remessa-eu         
            l-africa             
            l-africa-sul         
            l-amr                
            l-amr-2              
            l-amr-3              
            l-asia               
            l-europa             
            l-middle-east        
            l-usca               
            i-dias-status        
            l-stand-by           
            l-preparacao         
            l-saida-nitro        
            l-desembaracado      
            l-produto-aguas      
            l-apos-fronteira     
            l-transbordo         
            l-porto-destino      
            l-entregue           
            l-stand-by-parcial   
            l-envio              
            l-faturado           
            l-entregue-parcial   
            l-maritimo           
            l-rodoviario     
            d-dt-implant-ini
            d-dt-implant-fim
            l-amostra
            l-venda-parcial-ita
            l-remessa-ita      
            l-venda-parcial-fra
            l-remessa-fra
            WITH FRAME f-main.

    END.
************/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela W-Win
ON CHOOSE OF bt-cancela IN FRAME F-Main /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok W-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    ASSIGN INPUT FRAME f-main 
        c-cod-estabel-ini
        c-cod-estabel-fim
        dt-provisao-ini
        dt-provisao-fim
        dt-lancto-ini
        dt-lancto-fim
        c-cta-ctbl-ini
        c-cta-ctbl-fim
        lg-dex
        lg-dvvme
        lg-dvvmi
        lg-pdex
        lg-pdvvme
        lg-pdvvmi
		lg-dvvrem      /* NOVO */
        lg-pdvvrem     /* NOVO */
        rs-ind-situacao.

    /*
    IF rs-ind-situacao = 1 THEN
        ASSIGN p-ind-situacao = "Pendente".

    IF rs-ind-situacao = 2 THEN
        ASSIGN p-ind-situacao = "Provisionado".

    IF rs-ind-situacao = 3 THEN
        ASSIGN p-ind-situacao = "Cancelado".

    IF rs-ind-situacao = 4 THEN
        ASSIGN p-ind-situacao = "Todos".
    */

    ASSIGN p-cod-estabel-ini = c-cod-estabel-ini
           p-cod-estabel-fim = c-cod-estabel-fim
           p-dt-provisao-ini = dt-provisao-ini  
           p-dt-provisao-fim = dt-provisao-fim  
           p-dt-lancto-ini   = dt-lancto-ini
           p-dt-lancto-fim   = dt-lancto-fim
           p-cta-ctbl-ini    = c-cta-ctbl-ini   
           p-cta-ctbl-fim    = c-cta-ctbl-fim   
           p-ind-situacao    = rs-ind-situacao
           p-lg-dex          = lg-dex           
           p-lg-dvvme        = lg-dvvme         
           p-lg-dvvmi        = lg-dvvmi
           p-lg-pdex         = lg-pdex           
           p-lg-pdvvme       = lg-pdvvme         
           p-lg-pdvvmi       = lg-pdvvmi
		   p-lg-dvvrem       = lg-dvvrem     /* NOVO */
		   p-lg-pdvvrem      = lg-pdvvrem    /* NOVO */
        .



   /*********
    FIND es_param_monitor
        WHERE es_param_monitor.cod_usuar = c-usuario EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL es_param_monitor THEN DO:
        CREATE es_param_monitor.
        ASSIGN es_param_monitor.cod_usuar = c-usuario.
    END.

    ASSIGN 
        es_param_monitor.nr_proc_exp_ini        = c-nr-proc-ini                                
        es_param_monitor.nr_proc_exp_fim        = c-nr-proc-fim                            
        es_param_monitor.LOG_venda_direta       = l-venda-direta
        es_param_monitor.LOG_venda_fc           = l-venda-fc          
        es_param_monitor.log_venda_parcial_us   = l-venda-parcial-us                        
        es_param_monitor.log_venda_parcial_eu   = l-venda-parcial-eu                        
        es_param_monitor.log_remessa_us         = l-remessa-us                        
        es_param_monitor.log_remessa_eu         = l-remessa-eu                        
        es_param_monitor.log_africa             = l-africa                        
        es_param_monitor.log_africa_sul         = l-africa-sul                        
        es_param_monitor.log_amr                = l-amr                        
        es_param_monitor.log_amr2               = l-amr-2                        
        es_param_monitor.log_amr3               = l-amr-3                        
        es_param_monitor.log_asia               = l-asia                        
        es_param_monitor.log_europa             = l-europa                        
        es_param_monitor.log_middle_east        = l-middle-east                        
        es_param_monitor.log_usca               = l-usca                        
        es_param_monitor.dias_status            = i-dias-status                        
        es_param_monitor.log_stand_by           = l-stand-by                        
        es_param_monitor.log_preparacao         = l-preparacao                        
        es_param_monitor.log_saida_nitro        = l-saida-nitro                        
        es_param_monitor.log_desembaracado      = l-desembaracado                        
        es_param_monitor.log_produto_aguas      = l-produto-aguas                        
        es_param_monitor.log_pos_fronteira      = l-apos-fronteira                        
        es_param_monitor.log_transbordo         = l-transbordo                        
        es_param_monitor.log_porto_destino      = l-porto-destino                        
        es_param_monitor.log_entregue_cliente   = l-entregue
        es_param_monitor.log_stand_by_parcial   = l-stand-by-parcial                        
        es_param_monitor.log_instrucoes_parcial = l-envio
        es_param_monitor.log_faturado           = l-faturado                      
        es_param_monitor.log_entregue_cliente_parcial = l-entregue-parcial             
        es_param_monitor.log_maritimo           = l-maritimo                        
        es_param_monitor.log_rodoviario         = l-rodoviario
        es_param_monitor.dt_implant_ini         = d-dt-implant-ini
        es_param_monitor.dt_implant_fim         = d-dt-implant-fim
        overlay(es_param_monitor.char_1,1,10)   = STRING(l-amostra)
        overlay(es_param_monitor.char_1,11,10)  = STRING(l-venda-parcial-ita)
        overlay(es_param_monitor.char_1,21,10)  = STRING(l-remessa-ita)
        OVERLAY(es_param_monitor.CHAR_1,31,10)  = STRING(l-venda-parcial-fra)
        OVERLAY(es_param_monitor.CHAR_1,41,10)  = STRING(l-remessa-fra).
   */

    ASSIGN gl-atualiza-filtro = YES.

    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY c-cod-estabel-ini c-cod-estabel-fim dt-provisao-ini dt-provisao-fim 
        dt-lancto-ini dt-lancto-fim c-cta-ctbl-ini c-cta-ctbl-fim lg-dex 
        lg-dvvme lg-dvvmI lg-dvvrem  /* NOVO */
        lg-pdex lg-pdvvme lg-pdvvmI lg-pdvvrem  /* NOVO */
        rs-ind-situacao 
    WITH FRAME F-Main IN WINDOW W-Win.

ENABLE c-cod-estabel-ini c-cod-estabel-fim dt-provisao-ini dt-provisao-fim 
       dt-lancto-ini dt-lancto-fim c-cta-ctbl-ini c-cta-ctbl-fim lg-dex 
       lg-dvvme lg-dvvmI lg-dvvrem   /* NOVO */
       lg-pdex lg-pdvvme lg-pdvvmI lg-pdvvrem   /* NOVO */
       rs-ind-situacao bt-ok bt-cancela rt-buttom IMAGE-3 
       IMAGE-5 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 
    WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

