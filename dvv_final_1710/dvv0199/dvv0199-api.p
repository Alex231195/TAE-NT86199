DEF BUFFER b-processo-exp FOR processo-exp.

PROCEDURE pi-retorna-tip-oper-exp:
    DEF INPUT PARAMETER p-cod-estabel AS CHAR NO-UNDO.
    DEF INPUT PARAMETER p-nr-proc-exp AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-tip-oper-exp AS CHAR NO-UNDO.

    IF index(p-nr-proc-exp,"P") > 0 THEN
        ASSIGN p-cod-estabel = "002".

    FIND processo-exp
        WHERE processo-exp.cod-estabel = p-cod-estabel
          AND processo-exp.nr-proc-exp = p-nr-proc-exp NO-LOCK NO-ERROR.
    IF NOT AVAIL processo-exp AND p-cod-estabel = "002" THEN DO:
        ASSIGN p-cod-estabel = "001".
        FIND processo-exp
            WHERE processo-exp.cod-estabel = p-cod-estabel
              AND processo-exp.nr-proc-exp = p-nr-proc-exp NO-LOCK NO-ERROR.
    END.
    IF AVAIL processo-exp THEN DO:
       
        FIND FIRST proc-ped-venda OF processo-exp NO-LOCK NO-ERROR.

        FIND ped-venda 
            WHERE ped-venda.nr-pedcli  = proc-ped-venda.nr-pedcli
              AND ped-venda.nome-abrev = proc-ped-venda.nome-abrev NO-LOCK NO-ERROR.

        FIND ped-venda 
            WHERE ped-venda.nr-pedcli  = proc-ped-venda.nr-pedcli
              AND ped-venda.nome-abrev = proc-ped-venda.nome-abrev NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda THEN DO:
            ASSIGN p-tip-oper-exp = "Pedido nao relacionado".
            RETURN.
        END.

        FIND es_natur_oper NO-LOCK 
            WHERE es_natur_oper.nat_operacao = ped-venda.nat-operacao NO-ERROR.
        IF AVAIL es_natur_oper THEN DO:
        
            CASE es_natur_oper.tip_oper_exp:
                WHEN 1 OR WHEN 4 THEN DO: 
                    ASSIGN p-tip-oper-exp = "Normal".
                END.
                WHEN 2 THEN DO: 
                    ASSIGN p-tip-oper-exp = "Remessa para Estoque".
                END.
                WHEN 3 THEN 
                    ASSIGN p-tip-oper-exp = "Venda Estoque Exterior".
                WHEN 5 THEN 
                    ASSIGN p-tip-oper-exp = "Venda Estoque Consig".
                WHEN 6 THEN 
                    ASSIGN p-tip-oper-exp = "Remessa Estoque Consig".
                WHEN 7 THEN 
                    ASSIGN p-tip-oper-exp = "Retorno Estoque Consig".
                    
            END CASE.
        END.
    END.

END.

PROCEDURE pi-retorna-regiao-segmento:
    DEF INPUT PARAMETER p-cod-estabel AS CHAR NO-UNDO.
    DEF INPUT PARAMETER p-nr-proc-exp AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-regiao     AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-segmento   AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-tip-oper-exp AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-depos AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-nome-ab-reg AS CHAR NO-UNDO.

    DEF VAR i-cod-emitente LIKE emitente.cod-emitente NO-UNDO.
    DEF VAR c-cod-depos AS CHAR NO-UNDO.
    DEF VAR c-reg-comerc AS CHAR NO-UNDO.

    IF index(p-nr-proc-exp,"P") > 0 THEN
        ASSIGN p-cod-estabel = "002".

    FIND processo-exp
        WHERE processo-exp.cod-estabel = p-cod-estabel
          AND processo-exp.nr-proc-exp = p-nr-proc-exp NO-LOCK NO-ERROR.
    IF NOT AVAIL processo-exp AND p-cod-estabel = "002" THEN DO:
        ASSIGN p-cod-estabel = "001".
        FIND processo-exp
            WHERE processo-exp.cod-estabel = p-cod-estabel
              AND processo-exp.nr-proc-exp = p-nr-proc-exp NO-LOCK NO-ERROR.
    END.
    
    IF AVAIL processo-exp THEN DO:
        ASSIGN i-cod-emitente = processo-exp.cod-emitente.

        
        for first es_param_estab no-lock
            where es_param_estab.cod_estabel = processo-exp.cod-estabel
              and es_param_Estab.cod_estabel_trd ne "".        
        
            FIND b-processo-exp NO-LOCK
                WHERE b-processo-exp.cod-estabel = es_param_Estab.cod_estabel_trd
                  AND b-processo-exp.nr-proc-exp = processo-exp.nr-proc-exp NO-ERROR.
            IF AVAIL b-processo-exp THEN
                ASSIGN i-cod-emitente = b-processo-exp.cod-emitente.
        END.

        FIND FIRST proc-ped-venda OF processo-exp NO-LOCK NO-ERROR.

        FIND es_itinerario
            WHERE es_itinerario.cod_itiner = processo-exp.cod-itiner NO-LOCK NO-ERROR.
        IF AVAIL es_itinerario THEN DO:
            ASSIGN p-depos = es_itinerario.cod_depos.
        END.

        FIND ped-venda 
            WHERE ped-venda.nr-pedcli  = proc-ped-venda.nr-pedcli
              AND ped-venda.nome-abrev = proc-ped-venda.nome-abrev NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda THEN DO:
            ASSIGN p-tip-oper-exp = "Pedido nao relacionado".
            RETURN.
        END.

        FIND es_natur_oper NO-LOCK 
            WHERE es_natur_oper.nat_operacao = ped-venda.nat-operacao NO-ERROR.
        IF AVAIL es_natur_oper THEN DO:
            
            if es_natur_oper.tip_oper_exp = 2 or es_natur_oper.tip_oper_exp = 6 THEN DO:

                IF p-depos = "DUS" THEN
                   ASSIGN p-depos = "DAT".

                FIND FIRST es_cliente_Exterior
                     WHERE es_cliente_Exterior.cod_estabel   = ped-venda.cod-estabel
                       AND es_cliente_Exterior.cod_depos     = p-depos NO-LOCK NO-ERROR.
                
				IF AVAIL es_cliente_Exterior THEN
				ASSIGN /*c-cons-final = trim(substring(es_cliente_exterior.CHAR_1,1,100))*/
                       c-reg-comerc = es_cliente_exterior.nome_ab_reg.
					   
            END.
            ELSE 
                ASSIGN c-reg-comerc = "".

            CASE es_natur_oper.tip_oper_exp:
                WHEN 1 OR WHEN 4 THEN DO: 
                    ASSIGN p-tip-oper-exp = "Normal"
                           p-depos        = "".
                END.
                WHEN 2 THEN DO: 
                    ASSIGN p-tip-oper-exp = "Remessa para Estoque".
                END.
                WHEN 3 THEN 
                    ASSIGN p-tip-oper-exp = "Venda Estoque Exterior".
                WHEN 5 THEN 
                    ASSIGN p-tip-oper-exp = "Venda Estoque Consig".
                WHEN 6 THEN 
                    ASSIGN p-tip-oper-exp = "Remessa Estoque Consig".
                WHEN 7 THEN 
                    ASSIGN p-tip-oper-exp = "Retorno Estoque Consig".
                    
            END CASE.

        END.

    END.
 
 
    FOR FIRST emitente FIELDS(cod-rep cod-canal-venda) NO-LOCK
        WHERE emitente.cod-emitente = i-cod-emitente:        
        
        FOR FIRST repres FIELDS(nome-ab-reg) NO-LOCK
            WHERE repres.cod-rep = emitente.cod-rep:
    
            IF c-reg-comerc = "" THEN DO:
    
                FOR FIRST regiao FIELDS(nome-regiao) NO-LOCK
                    WHERE regiao.nome-ab-reg = repres.nome-ab-reg:
    
                    ASSIGN p-regiao      = regiao.nome-regiao
                           p-nome-ab-reg = regiao.nome-ab-reg .
                END.
            END.
            ELSE DO: 
    
                FOR FIRST regiao FIELDS(nome-regiao) NO-LOCK
                    WHERE regiao.nome-ab-reg = c-reg-comerc:
    
                    ASSIGN p-regiao      = regiao.nome-regiao
                           p-nome-ab-reg = regiao.nome-ab-reg .
                END.
            END.
        END.

        FIND FIRST canal-venda NO-LOCK
             WHERE canal-venda.cod-canal-venda = emitente.cod-canal-venda NO-ERROR.
        IF AVAIL canal-venda THEN
             ASSIGN p-segmento = STRING(emitente.cod-canal-venda) + canal-venda.descricao.

    END.
    
END.

PROCEDURE pi-retorna-item:
    DEF INPUT PARAMETER p-it-codigo LIKE ITEM.it-codigo NO-UNDO.
    DEF OUTPUT PARAMETER p-familia AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-classe-item AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER p-grupo-classe AS CHAR NO-UNDO.
    /*
    **
    ** Tratamento item dvv0199
    **
    */                        
    FIND ITEM
        WHERE ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
        FIND fam-comerc WHERE
             fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.
    IF AVAIL fam-comerc THEN
        ASSIGN p-familia = fam-comerc.descricao.

    FIND FIRST es_item_class_item WHERE
               es_item_class_item.it_codigo = item.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL es_item_class_item THEN
        FIND FIRST es_Class_item WHERE
                   es_class_item.cod_class_item = es_item_class_item.cod_class_item NO-LOCK NO-ERROR.
    IF AVAIL es_class_item THEN
        ASSIGN p-classe-item   = es_class_item.CLASS_item
               p-grupo-classe = substrin(es_class_item.CHAR_1,1,20).

END.
