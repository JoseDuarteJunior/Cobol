      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:30/11/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:7 PAG:91
      * Tectonics: cobc
      * Objetivo: CRIACAO DE ARQUIVO SEQUENCIAL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER7.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CRIA02 ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CRIA02
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CRIA02".
       01  REG-CRIA02.
           02 FD-CODIGO-PRODUTO      PIC 9(04)       VALUE ZEROS.
           02 FD-NOME-PRODUTO        PIC X(30)       VALUE SPACES.
           02 FD-QUANTIDADE-ESTOQUE  PIC 9(04)       VALUE ZEROS.
           02 FD-CUSTO-UNITARIO      PIC 9(05)V99    VALUE ZEROS.
           02 FD-CUSTO-TOTAL         PIC 9(06)V99    VALUE ZEROS.
       WORKING-STORAGE SECTION.
       01  DATA-SISTEMA.
           02 DIA  PIC 9(02) VALUE ZEROS.
           02 MES  PIC 9(02) VALUE ZEROS.
           02 ANO  PIC 9(02) VALUE ZEROS.
       01  MASCARAS.
           02 M-CODIGO-PRODUTO     PIC ZZZ9         VALUE ZEROS.
           02 M-QUANTIDADE-ESTOQUE PIC Z.ZZ9        VALUE ZEROS.
           02 M-CUSTO-UNITARIO     PIC ZZ.ZZ9,99    VALUE ZEROS.
           02 M-CUSTO-TOTAL        PIC ZZZ.ZZ9,99   VALUE ZEROS.
       01  CUSTO-TOTAL-M           PIC ZZZ.ZZ9,99   VALUE ZEROS.
       77  CUSTO-TOTAL-25-M        PIC ZZZ.ZZ9,99   VALUE ZEROS.
       77  CUSTO-TOTAL-25          PIC 9(08)        VALUE ZEROS.
       77  LINHACOLUNA             PIC 9(04)        VALUE 0502.
       77  CONTADOR                PIC 9(02)        VALUE 1.
       77  MAIOR-CUSTO-TOTAL       PIC 9(06)V99     VALUE ZEROS.
       77  CONFIRMA    PIC X(01) VALUE SPACES.
       77  WS-STATUS   PIC X(02) VALUE SPACES.
       77  WS-LIMPA    PIC X(50) VALUE SPACES.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "DATA:".
           02 LINE 01 COLUMN 16
           VALUE "CONTROLE DE ALMOXARIFADO".
           02 LINE 03 COLUMN 02
           VALUE "CODIGO".
           02 LINE 03 COLUMN 11
           VALUE "NOME".
           02 LINE 03 COLUMN 20
           VALUE "ESTOQUE".
           02 LINE 03 COLUMN 29
           VALUE "V.UNITARIO".
           02 LINE 03 COLUMN 43
           VALUE "V.TOTAL".
           02 LINE 18 COLUMN 02
           VALUE "Mensagem:".
       01  MSG-ERRO-1.
           02 LINE 08 COLUMN 12
           VALUE "O CODIGO NAO PODE SER 0 !".
       01  MSG-ERRO-2.
           02 LINE 08 COLUMN 12
           VALUE "NOME NAO PODER FICAR EM BRANCO".
       01  MSG-ERRO-3.
           02 LINE 08 COLUMN 12
           VALUE "ESTOQUE ACIMA OU IGUAL A 10".
       01  MSG-ERRO-4.
           02 LINE 08 COLUMN 12
           VALUE "CUSTO UNITARIO DIFERENTE DE ZERO".
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY LIMPA-TELA.
           OPEN INPUT CRIA02.
           IF WS-STATUS = "00"
               NEXT SENTENCE
           ELSE
               DISPLAY "Erro de abertura" AT 2029
               STOP RUN.
           ACCEPT DATA-SISTEMA FROM DATE.
       LIMPA-DADOS.
           DISPLAY WS-LIMPA AT 0502.
           DISPLAY WS-LIMPA AT 0702.
           DISPLAY WS-LIMPA AT 0902.
           DISPLAY WS-LIMPA AT 1102.
           DISPLAY WS-LIMPA AT 1302.
           DISPLAY WS-LIMPA AT 1502.
           DISPLAY WS-LIMPA AT 1702.
           DISPLAY WS-LIMPA AT 1812.
       VIDEO.
           DISPLAY TELA-BASE.
           DISPLAY DIA AT 0107'/' MES '/' ANO.
       ABERTURA.
           READ CRIA02 INTO REG-CRIA02 AT END GO TO FECHAMENTO.
           MOVE FD-CODIGO-PRODUTO TO M-CODIGO-PRODUTO.
           MOVE FD-QUANTIDADE-ESTOQUE TO M-QUANTIDADE-ESTOQUE.
           MOVE FD-CUSTO-UNITARIO TO M-CUSTO-UNITARIO.
           MOVE FD-CUSTO-TOTAL TO M-CUSTO-TOTAL.
           IF FD-CUSTO-TOTAL > MAIOR-CUSTO-TOTAL
               MOVE FD-CUSTO-TOTAL TO MAIOR-CUSTO-TOTAL.
           COMPUTE CUSTO-TOTAL-25 = CUSTO-TOTAL-25 + FD-CUSTO-TOTAL.
           DISPLAY M-CODIGO-PRODUTO AT LINHACOLUNA.
           ADD 9 TO LINHACOLUNA.
           DISPLAY FD-NOME-PRODUTO AT LINHACOLUNA.
           ADD 9 TO LINHACOLUNA.
           DISPLAY M-QUANTIDADE-ESTOQUE AT LINHACOLUNA.
           ADD 9 TO LINHACOLUNA.
           DISPLAY M-CUSTO-UNITARIO AT LINHACOLUNA.
           ADD 10 TO LINHACOLUNA.
           DISPLAY M-CUSTO-TOTAL AT LINHACOLUNA.
           SUBTRACT 37 FROM LINHACOLUNA.
           ADD 200 TO LINHACOLUNA.
           ADD 1 TO CONTADOR.
           IF CONTADOR = 7
               GO TO CONTINUAR
           ELSE
               GO  ABERTURA
           END-IF.
       CONTINUAR.
           MOVE 1 TO CONTADOR.
           MOVE 0502 TO  LINHACOLUNA.
           DISPLAY "MOSTRAR RESTANTE DOS REGISTROS ? S-SIM" AT 1812.
           ACCEPT CONFIRMA AT 1851.
           IF CONFIRMA = 'S'
               PERFORM LIMPA-DADOS
               GO TO ABERTURA
           ELSE
               GO TO FECHAMENTO
           END-IF.
       FECHAMENTO.
           COMPUTE CUSTO-TOTAL-25 = (CUSTO-TOTAL-25 * 0,25).
           MOVE CUSTO-TOTAL-25 TO CUSTO-TOTAL-25-M.
           DISPLAY "25% DO CUSTO TOTAL E':" AT 1812 CUSTO-TOTAL-25-M.
           MOVE MAIOR-CUSTO-TOTAL TO CUSTO-TOTAL-25-M.
           DISPLAY "MAIOR CUSTO TOTAL FOI DE:" AT 1912 CUSTO-TOTAL-25-M.
           ACCEPT CONFIRMA AT 1948.
       FIM.
           CLOSE CRIA02.
           STOP RUN.
       END PROGRAM EXER7.
