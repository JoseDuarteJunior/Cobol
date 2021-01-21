      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:18/01/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:14 PAG:146
      * Tectonics: cobc
      * Objetivo: MANUTENCAO DE ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER14.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPRO ASSIGN TO DISK
                  ORGANIZATION     INDEXED
                  ACCESS MODE      DYNAMIC
                  RECORD KEY       CODIGO
                  FILE STATUS      FS.
           SELECT RELATO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQPRO LABEL RECORD STANDARD
           VALUE OF FILE-ID "ARQPRO".
       01  REG-ARQPRO.
           02 FD-CODIGO.
               03 CODIGO       PIC 9(04)    VALUE ZEROS.
               03 DV           PIC 9(01)    VALUE ZEROS.
           02 FD-NOME-PRODUTO  PIC X(30)    VALUE SPACES.
           02 FD-CODIGO-FORNE  PIC 9(03)    VALUE ZEROS.
           02 FD-VALOR-UNIT    PIC 9(06)V99 VALUE ZEROS.
           02 FD-QUANTIDA-EST  PIC 9(04)    VALUE ZEROS.
           02 FD-VALOR-TOT     PIC 9(08)V99 VALUE ZEROS.
       FD  RELATO LABEL RECORD OMITTED LINAGE 54 TOP 2 BOTTOM 5.
       01  REG-RELATO PIC X(132).
       WORKING-STORAGE SECTION.
       77  FS                  PIC X(02)    VALUE SPACES.
       77  CONFIRMA            PIC X(01)    VALUE SPACES.
       77  FUNCAO              PIC 9(01)    VALUE ZEROS.
       77  NOME-FUNCAO         PIC X(02)    VALUE SPACES.
       77  DV-CALC             PIC S99V     VALUE ZEROS.
       77  REGISTRO-INICIAL    PIC 9(02)    VALUE ZEROS.
       77  REGISTRO-FINAL      PIC 9(02)    VALUE ZEROS.
       77  QUANTID-REGISTROS   PIC 9(02)    VALUE ZEROS.
       77  CONTADOR-PAGINA     PIC 9(03)    VALUE ZEROS.
       01  CAB01.
           02 FILLER           PIC X(02)    VALUE SPACES.
           02 FILLER           PIC X(02)    VALUE "EM".
           02 FILLER           PIC X(01)    VALUE SPACES.
           02 DATA-CAB01.
               03 ANO-CAB01    PIC 99/      VALUE ZEROS.
               03 MES-CAB01    PIC 99/      VALUE ZEROS.
               03 DIA-CAB01    PIC 99      VALUE ZEROS.
           02 FILLER           PIC X(02)    VALUE SPACES.
           02 FILLER           PIC X(43)
           VALUE "CONTROLE DE PRODUTOS DA EMPRESA ZOOM   PAG.".
           02 PAG-CAB01        PIC ZZ9      VALUE ZEROS.
           02 FILLER           PIC X(71)    VALUE SPACES.
       01  CAB02.
           02 FILLER           PIC X(02)    VALUE SPACES.
           02 FILLER           PIC X(06)    VALUE "CODIGO".
           02 FILLER           PIC X(03)    VALUE SPACES.
           02 FILLER           PIC X(20)    VALUE "NOME DO PRODUTO".
           02 FILLER           PIC X(15)    VALUE SPACES.
           02 FILLER           PIC X(10)    VALUE "COD. FORNC".
           02 FILLER           PIC X(05)    VALUE SPACES.
           02 FILLER           PIC X(11)    VALUE "VALOR UNIT.".
           02 FILLER           PIC X(02)    VALUE SPACES.
           02 FILLER           PIC X(09)    VALUE "QTDE ESTQ".
           02 FILLER           PIC X(04)    VALUE SPACES.
           02 FILLER           PIC X(08)    VALUE "QTDE MIN".
           02 FILLER           PIC X(25)    VALUE SPACES.
       01  DETALHE.
           02 FILLER           PIC X(02)    VALUE SPACES.
           02 CODIGO-DET       PIC 9999     VALUE ZEROS.
           02 DV-DET           PIC -9       VALUE ZEROS.
           02 FILLER           PIC X(03)    VALUE SPACES.
           02 NOME-DET         PIC X(30)    VALUE SPACES.
           02 FILLER           PIC X(05)    VALUE SPACES.
           02 FORNC-DET        PIC 999      VALUE ZEROS.
           02 FILLER           PIC X(12)    VALUE SPACES.
           02 VALOR-UNIT-DET   PIC ZZZ.ZZ9,99 VALUE ZEROS.
           02 FILLER           PIC X(03)    VALUE SPACES.
           02 QTE-EST-DET      PIC ZZZ9     VALUE SPACES.
           02 FILLER           PIC X(09)    VALUE SPACES.
           02 QTE-MIN-DET      PIC ZZZ9     VALUE ZEROS.
           02 FILLER           PIC X(29)    VALUE SPACES.
       01  TOTAL.
           02 FILLER           PIC X(06)    VALUE SPACES.
           02 FILLER           PIC X(31)     
           VALUE "TOTAL DE PRODUTOS IMPRESSOS:  ".
           02 TOTAL-DET        PIC ZZ9      VALUE ZEROS.
           02 FILLER           PIC X(92)    VALUE SPACES.
       01 WS-DATA.
           02 DIA              PIC 9(02)    VALUE ZEROS.
           02 MES              PIC 9(02)    VALUE ZEROS.
           02 ANO              PIC 9(02)    VALUE ZEROS.
       01  WS-MENSAGENS.
           02 LIMPA-MENSAGEM    PIC X(50) VALUE SPACES.
           02 MSG-ERRO-1        PIC X(22) VALUE "OPCAO INVALIDA".
           02 MSG-ERRO-2        PIC X(22) VALUE "ESTE CODIGO JA EXISTE".
           02 MSG-ERRO-3        PIC X(17) VALUE "CODIGO NAO EXISTE".
           02 MSG-ERRO-4        PIC X(14) VALUE "CAMPO INVALIDO".
       01  TMP-VALORES.
           02 QTE-ESTOQUE         PIC ZZZZ.
           02 VALOR-UNIT-MASK     PIC ZZZZZZZ,99.
           02 VALOR-TOT-MASK      PIC ZZZZZZZ,99.
       SCREEN SECTION.
       01  TELA-1.
            02 BLANK SCREEN.
            02 LINE 01 COLUMN 01 VALUE "DATA:".
            02 LINE 01 COLUMN 17 VALUE "CIA PRODUTOS DETERIORADOS S/A".
            02 LINE 12 COLUMN 04 VALUE "MENSAGEM:".
       01  MENU-1.
           02 LINE 03 COLUMN 04 VALUE "FUNCAO ESCOLHIDA:      < >".
           02 LINE 05 COLUMN 08 VALUE "<1>     INCLUSAO".
           02 LINE 06 COLUMN 08 VALUE "<2>     ALTERACO".
           02 LINE 07 COLUMN 08 VALUE "<3>     EXCLUSAO".
           02 LINE 08 COLUMN 08 VALUE "<4>     CONSULTA".
           02 LINE 09 COLUMN 08 VALUE "<5>     LISTAGEM".
           02 LINE 10 COLUMN 08 VALUE "<6>     FIM".
       01  MENU-2.
           02 LINE 03 COLUMN 01
           VALUE "ESCOLHA A FUNCAO:    (IN - AL - EX - CO - FF)".
           02 LINE 05 COLUMN 05 VALUE "CODIGO..............>".
           02 LINE 06 COLUMN 05 VALUE "NOME................>".
           02 LINE 07 COLUMN 05 VALUE "QUANTIDADE ESTOQUE..>".
           02 LINE 08 COLUMN 05 VALUE "VALOR UNITARIO......>".
           02 LINE 09 COLUMN 05 VALUE "VALOR TOTAL.........>".
       01  TELA-LISTAGEM.
           02 LINE 03 COLUMN 04 VALUE "FUNCAO: LISTAGEM".
           02 LINE 05 COLUMN 02
           VALUE "CODIGO DO REGISTRO INICIAL:_______".
           02 LINE 06 COLUMN 02
           VALUE "CODIGO DO REGISTRO FINAL:_________".
           02 LINE 08 COLUMN 02
           VALUE "Contador de registros impressos:__".
       01  LIMPA-TELA.
            02 LINE 05 COLUMN 29 VALUE "     ".
            02 LINE 06 COLUMN 29 VALUE "                              ".
            02 LINE 07 COLUMN 29 VALUE "    ".
            02 LINE 08 COLUMN 29 VALUE "          ".
            02 LINE 09 COLUMN 29 VALUE "           ".
            02 LINE 10 COLUMN 4 VALUE "                   ".
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARQPRO.
           IF FS NOT = "00"
               IF FS = "35"
                   CLOSE ARQPRO OPEN OUTPUT ARQPRO CLOSE ARQPRO
                   GO TO INICIO
               ELSE
                   DISPLAY FS "STATUS DO ARQUIVO"
                   STOP RUN
           END-IF.
       TELA-INICIAL.
           DISPLAY TELA-1.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY DIA AT 0107 '/' MES '/' ANO.
           MOVE ANO TO ANO-CAB01.
           MOVE MES TO MES-CAB01.
           MOVE DIA TO DIA-CAB01.
       ESCOLHA.
           DISPLAY MENU-1.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           ACCEPT FUNCAO AT 0328.
           IF FUNCAO = 0 OR > 6
               DISPLAY MSG-ERRO-1 AT 1214
               GO TO ESCOLHA.
           GO TO INCLUSAO ALTERACAO EXCLUSAO CONSULTA LISTAGEM FIM
           DEPENDING ON FUNCAO.
       INCLUSAO.
           MOVE "IN" TO NOME-FUNCAO.
           DISPLAY LIMPA-TELA.
           DISPLAY MENU-2.
           DISPLAY NOME-FUNCAO AT 0319.
       INCLUIR.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           ACCEPT CODIGO AT 0529.
           IF CODIGO = ZEROS
               DISPLAY MSG-ERRO-4 AT 1214
               ACCEPT CONFIRMA AT 1228
               GO TO INCLUIR.
           READ ARQPRO INVALID KEY GO TO ROT-NOME.
           DISPLAY MSG-ERRO-2 AT 1214
           ACCEPT CONFIRMA AT 1228
           GO TO INCLUIR.
       ROT-NOME.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           ACCEPT FD-NOME-PRODUTO AT 0629.
           IF FD-NOME-PRODUTO = SPACES
               DISPLAY MSG-ERRO-4 AT 1214
               ACCEPT CONFIRMA AT 1228
               GO TO ROT-NOME.
       ROT-QUANTIDADE-EST.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           ACCEPT FD-QUANTIDA-EST AT 0729.
           IF FD-QUANTIDA-EST <= 0
               DISPLAY MSG-ERRO-4 AT 1214
               ACCEPT CONFIRMA AT 1228
               GO TO ROT-QUANTIDADE-EST.
       ROT-VALOR-UNITARIO.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           ACCEPT FD-VALOR-UNIT AT 0829.
           IF FD-VALOR-UNIT <=0
               DISPLAY MSG-ERRO-4 AT 1214
               ACCEPT CONFIRMA AT 1228
               GO TO ROT-VALOR-UNITARIO.
       ROT-VALOR-TOTAL.
           COMPUTE FD-VALOR-TOT = FD-VALOR-UNIT * FD-QUANTIDA-EST.
           MOVE FD-VALOR-TOT TO VALOR-TOT-MASK.
           DISPLAY LIMPA-MENSAGEM AT 0926.
           DISPLAY VALOR-TOT-MASK AT 0926.
       ROT-GRAVA.
           COMPUTE DV-CALC = CODIGO / 10.
           COMPUTE DV-CALC = DV-CALC * 10.
           COMPUTE DV = CODIGO - DV-CALC.
           COMPUTE FD-CODIGO-FORNE = 999 - CODIGO.
           WRITE REG-ARQPRO INVALID KEY
           DISPLAY "ERRO DE GRAVACAO" FS AT 1214
           STOP RUN.
       ROT-RETORNO.
           DISPLAY "CONTINUAR INCLUSAO ? S-SIM" AT 1214.
           ACCEPT CONFIRMA AT 1240.
           IF CONFIRMA = "S" OR "s"
               DISPLAY LIMPA-TELA
               GO TO INCLUIR.
           GO TO TELA-INICIAL.
       ALTERACAO.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           MOVE "AL" TO NOME-FUNCAO.
           DISPLAY LIMPA-TELA.
           DISPLAY MENU-2.
           DISPLAY NOME-FUNCAO AT 0319.
       ALTERAR.
           ACCEPT CODIGO AT 0529.
           IF CODIGO = ZEROS
               DISPLAY MSG-ERRO-3 AT 1214
               ACCEPT CONFIRMA AT 1233
               GO TO ALTERAR.
           READ ARQPRO INVALID KEY
           DISPLAY MSG-ERRO-3 AT 1214
           ACCEPT CONFIRMA AT 1233
           DISPLAY LIMPA-MENSAGEM AT 1214
           GO TO ALTERAR.
           PERFORM MOSTRA.
       ALTERA.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           DISPLAY "INFORME O CAMPO QUE VAI ALTERAR < >" AT 1214.
           ACCEPT FUNCAO AT 1247.
           IF FUNCAO =0 OR >3
               GO TO ALTERA.
           GO TO CPO-NOME CPO-QUANTIDADE-EST CPO-VALOR-UNIT
           DEPENDING FUNCAO.
       CPO-NOME.
           PERFORM ROT-NOME.
           GO TO ROT-REGRAVA.
       CPO-QUANTIDADE-EST.
           PERFORM ROT-QUANTIDADE-EST.
           PERFORM ROT-VALOR-TOTAL.
           GO TO ROT-REGRAVA.
       CPO-VALOR-UNIT.
           PERFORM ROT-VALOR-UNITARIO.
           PERFORM ROT-VALOR-TOTAL.
           GO TO ROT-REGRAVA.
       ROT-REGRAVA.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           DISPLAY "CONTINUAR ALTERANDO MESMO REGISTRO ? S-SIM" AT 1214.
           ACCEPT CONFIRMA AT 1259.
           IF CONFIRMA = "S" OR "s"
               GO TO ALTERA.
           REWRITE REG-ARQPRO INVALID KEY
           DISPLAY "ERRO DE GRAVACAO" FS AT 1214 STOP RUN.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           DISPLAY "CONTINUAR ALTERANDO ? S-SIM" AT 1214.
           ACCEPT CONFIRMA AT 1245.
           IF CONFIRMA = "S" OR "s"
               DISPLAY LIMPA-MENSAGEM AT 1214
               DISPLAY LIMPA-TELA
               GO TO ALTERACAO.
           GO TO TELA-INICIAL.
       MOSTRA.
           MOVE FD-QUANTIDA-EST TO QTE-ESTOQUE.
           MOVE FD-VALOR-UNIT TO VALOR-UNIT-MASK.
           MOVE FD-VALOR-TOT TO VALOR-TOT-MASK.
           DISPLAY FD-NOME-PRODUTO AT 0629.
           DISPLAY QTE-ESTOQUE AT 0729.
           DISPLAY VALOR-UNIT-MASK AT 0829.
           DISPLAY VALOR-TOT-MASK AT 0929.
       EXCLUSAO.
           MOVE "EX" TO NOME-FUNCAO.
           DISPLAY LIMPA-TELA.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           DISPLAY MENU-2.
           DISPLAY NOME-FUNCAO AT 0319.
       EXCLUIR.
           PERFORM ALTERAR.
           DISPLAY "CONFIRMA EXCLUSAO ? S-SIM" AT 1214.
           ACCEPT CONFIRMA AT 1240.
           IF CONFIRMA = "S" OR "s"
               NEXT SENTENCE
           ELSE
               GO TO EXCLUSAO.
           DELETE ARQPRO INVALID KEY
           DISPLAY "ERRO DE EXCLUSAO" FS AT 1214
           STOP RUN.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           DISPLAY "CONTINUAR EXCLUSAO ? S-SIM" AT 1214.
           ACCEPT CONFIRMA AT 1242.
           IF CONFIRMA = "S" OR "s"
               GO TO EXCLUSAO.
           GO TO TELA-INICIAL.
       CONSULTA.
           MOVE "CO" TO  NOME-FUNCAO.
           DISPLAY LIMPA-TELA.
           DISPLAY LIMPA-MENSAGEM AT 1214.
           DISPLAY MENU-2.
           DISPLAY NOME-FUNCAO AT 0319.
       CONSULTAR.
           PERFORM ALTERAR.
           DISPLAY "CONTINUAR CONSULTA ? S-SIM" AT 1214.
           ACCEPT CONFIRMA AT 1241.
           IF CONFIRMA = "S" OR "s"
               GO TO CONSULTA.
           GO TO TELA-INICIAL.
       CABECALHO.
           MOVE SPACES TO REG-RELATO.
           ADD 1 TO CONTADOR-PAGINA.
           MOVE CONTADOR-PAGINA TO PAG-CAB01.
           WRITE REG-RELATO FROM CAB01 BEFORE ADVANCING 2 LINES.
           WRITE REG-RELATO FROM CAB02 BEFORE ADVANCING 2 LINES.
       LISTAGEM.
           PERFORM TELA-INICIAL.
           DISPLAY TELA-LISTAGEM.
           ACCEPT REGISTRO-INICIAL AT 0536.
           ACCEPT REGISTRO-FINAL AT  0636.
           MOVE REGISTRO-INICIAL TO CODIGO.
           START ARQPRO KEY EQUAL CODIGO.
           OPEN OUTPUT RELATO.
           PERFORM CABECALHO.
       LER.
           READ ARQPRO AT END GO TO FIM-RELATO.
           ADD 1 TO QUANTID-REGISTROS.
           MOVE QUANTID-REGISTROS TO TOTAL-DET.
           MOVE CODIGO          TO CODIGO-DET.
           MOVE DV              TO DV-DET.
           MOVE FD-NOME-PRODUTO TO NOME-DET.
           MOVE FD-CODIGO-FORNE TO FORNC-DET.
           MOVE FD-VALOR-UNIT   TO VALOR-UNIT-DET.
           MOVE FD-QUANTIDA-EST TO QTE-EST-DET.
           MOVE FD-VALOR-TOT    TO QTE-MIN-DET.
           WRITE REG-RELATO FROM DETALHE BEFORE ADVANCING 2 LINES
               AT EOP PERFORM CABECALHO.
           IF CODIGO = REGISTRO-FINAL
               GO TO FIM-RELATO.
           GO TO LER.
       FIM-RELATO.
           MOVE SPACES TO REG-RELATO.
           WRITE REG-RELATO BEFORE ADVANCING 2 LINES.
           WRITE REG-RELATO FROM TOTAL BEFORE ADVANCING 1 LINE.
           CLOSE RELATO.
           CLOSE ARQPRO.
           DISPLAY QUANTID-REGISTROS AT 0836.
           MOVE 0 TO QUANTID-REGISTROS.
           DISPLAY "RELATORIO GERADO COM SUCESSO" AT 1214.
           ACCEPT CONFIRMA AT 1329.
           GO TO INICIO.
       FIM.
           DISPLAY "FIM DO PROCESSAMENTO " AT 1214.
           ACCEPT CONFIRMA AT 1245.
           CLOSE ARQPRO.
           STOP RUN.
           END PROGRAM EXER14.
