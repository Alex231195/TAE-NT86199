{dvv/dvv0199.i}

DEF TEMP-TABLE tt-rel-item-dif LIKE tt-rel-item 
    FIELD r-rowid AS ROWID.

DEFINE INPUT  PARAMETER p-provisao AS LOGICAL     NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-rel-item.
DEFINE INPUT PARAMETER TABLE FOR tt-rel-item-dif.
DEFINE INPUT PARAMETER d-data-ini AS DATE NO-UNDO.
DEFINE INPUT PARAMETER d-data-fim AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-fech-previo AS LOGICAL NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE   NO-UNDO.
DEFINE VARIABLE i-row   AS INTEGER  NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Gravando DVV Cont�bil / Pr�via").

/* -------------------------------------------------------------------
   Limpeza pr�via no per�odo:
   - Mant�m a regra original de pular registros "RT"
   - Agora limpa tamb�m a tabela de PR�VIA quando aplic�vel
   ------------------------------------------------------------------- */
IF NOT p-fech-previo THEN DO:
  FOR EACH mgnitro.dvv_contabil
      WHERE mgnitro.dvv_contabil.dta-ctbz >= d-data-ini
        AND mgnitro.dvv_contabil.dta-ctbz <= d-data-fim:
    IF SUBSTRING(mgnitro.dvv_contabil.movto,1,2) = "RT" THEN NEXT.
    DELETE mgnitro.dvv_contabil.
  END.
END.

/* Se FECHAMENTO PR�VIO = NO -> n�o mexe na dvv_contabil_previa */
IF p-fech-previo THEN DO:
  FOR EACH mgnitro.dvv_contabil_previa
      WHERE mgnitro.dvv_contabil_previa.dta-ctbz >= d-data-ini
        AND mgnitro.dvv_contabil_previa.dta-ctbz <= d-data-fim:
    IF SUBSTRING(mgnitro.dvv_contabil_previa.movto,1,2) = "RT" THEN NEXT.
    DELETE mgnitro.dvv_contabil_previa.
  END.
END.

/* -------------------------------------------------------------------
   Grava��o principal a partir de tt-rel-item
   - Previsto (movto inicia com "P")  -> dvv_contabil_previa
   - Demais ("Realizado", "Estornado", etc.) -> dvv_contabil
   - Pula "RT" como no original
   ------------------------------------------------------------------- */
FOR EACH tt-rel-item:
  IF SUBSTRING(tt-rel-item.movto,1,2) = "RT" THEN NEXT.

  /* FECHAMENTO PR�VIO = YES => s� PR�VIA (movto come�ando com P) */
  IF p-fech-previo THEN DO:
    IF CAPS(SUBSTRING(tt-rel-item.movto,1,1)) <> "P" THEN NEXT. /* seguran�a */
    CREATE mgnitro.dvv_contabil_previa.
    {dvv/dvv0199-bi.i1 tt-rel-item dvv_contabil_previa}
    RELEASE mgnitro.dvv_contabil_previa.
  END.
  ELSE DO:
    /* FECHAMENTO PR�VIO = NO => s� REALIZADO (movto n�o P) */
    IF CAPS(SUBSTRING(tt-rel-item.movto,1,1)) = "P" THEN NEXT.  /* seguran�a */
    CREATE mgnitro.dvv_contabil.
    {dvv/dvv0199-bi.i1 tt-rel-item dvv_contabil}
    RELEASE mgnitro.dvv_contabil.
  END.
END.

/* -------------------------------------------------------------------
   Grava��o das diferen�as (quando N�O for provis�o)
   Mant�m a l�gica do original, incluindo o pulo de "RT",
   e aplica o mesmo roteamento por tipo de movimento.
   ------------------------------------------------------------------- */
IF NOT p-provisao THEN DO:

    FOR EACH tt-rel-item-dif:
	  IF SUBSTRING(tt-rel-item-dif.movto,1,2) = "RT" THEN NEXT.

	  IF p-fech-previo THEN DO:
		IF CAPS(SUBSTRING(tt-rel-item-dif.movto,1,1)) <> "P" THEN NEXT.
		CREATE mgnitro.dvv_contabil_previa.
		{dvv/dvv0199-bi.i1 tt-rel-item-dif dvv_contabil_previa}
		RELEASE mgnitro.dvv_contabil_previa.
	  END.
	  ELSE DO:
		IF CAPS(SUBSTRING(tt-rel-item-dif.movto,1,1)) = "P" THEN NEXT.
		CREATE mgnitro.dvv_contabil.
		{dvv/dvv0199-bi.i1 tt-rel-item-dif dvv_contabil}
		RELEASE mgnitro.dvv_contabil.
	  END.
	END.

END.

RUN pi-finalizar IN h-acomp.
