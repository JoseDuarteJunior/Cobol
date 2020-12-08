      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:08/12/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:7 PAG:110
      * Tectonics: cobc
      * Objetivo: EMISSAO DE RELATORIO EM ARQUIVO SEQUENCIAL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER9.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQMAT ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
           SELECT RELATO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQMAT LABEL RECORD STANDARD
       VALUE OF FILE-ID IS "ARQMAT".
       01  REG-ARQMAT.
           02  FD-CODIGO        PIC 9(05)    VALUE ZEROS.
           02  FD-NOME          PIC X(15)    VALUE SPACES.
           02  FD-QTDE-ESTOQUE  PIC 9(07)    VALUE ZEROS.
           02  FD-PRECO-UNIT    PIC 9(05)V99 VALUE ZEROS.
           02  FD-PRECO-TOT     PIC 9(06)V99 VALUE ZEROS.
       FD RELATO LABEL RECORD OMITTED
       LINAGE 58 TOP 2 BOTTOM 6.
       01  REG-RELATO PIC X(132).
       WORKING-STORAGE SECTION. 
       77  WS-STATUS              PIC 9(02)     VALUE ZEROS.
       77  CONFIRMA               PIC X(01)     VALUE SPACES.
       77  CT-PAG                 PIC 9(02)     VALUE ZEROS.
       77  QUANTIDADE-PRODUTO     PIC 9(07)     VALUE ZEROS.
       77  PRECO-MEDIO            PIC 9(06)V99  VALUE ZEROS.
       77  PRECO-TOTAL            PIC 9(07)V99  VALUE ZEROS.
       77  TOTAL-REGISTROS        PIC 9(03)     VALUE ZEROS.
       01  DATA-SISTEMA.
           02 ANO           PIC 9(02) VALUE ZEROS.
           02 MES           PIC 9(02) VALUE ZEROS.
           02 DIA           PIC 9(02) VALUE ZEROS.
       01  CAB01.
           02 FILLER    PIC X(04)    VALUE SPACES.
           02 FILLER    PIC X(06)    VALUE "DATA:".
           02 DATA-CAB01.
               03 DIA-CAB01   PIC 99/ VALUE ZEROS.
               03 MES-CAB01   PIC 99/ VALUE ZEROS.
               03 ANO-CAB01   PIC 99  VALUE ZEROS.
           02 FILLER    PIC X(07)     VALUE SPACES.
           02 FILLER    PIC X(32)
           VALUE "RELATORIO DE MATERIAL EM ESTOQUE".
           02 FILLER     PIC X(05)   VALUE SPACES.
           02 FILLER     PIC X(04)   VALUE "PAG.".
           02 PAG-CAB01  PIC ZZ9     VALUE ZEROS.
           02 FILLER     PIC X(05)   VALUE SPACES.
       01  CAB02.
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(08) VALUE "CODIGO".
           02 FILLER  PIC X(15) VALUE "NOME".
           02 FILLER  PIC X(14) VALUE "QTDE ESTOQUE".
           02 FILLER  PIC X(17) VALUE "CUSTO UNITARIO".
           02 FILLER  PIC X(11) VALUE "CUSTO TOTAL".
       01  CAB03.
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(22) VALUE "QUANTIDADE DE PRODUTO".
           02 FILLER  PIC X(06) VALUE SPACES.
           02 FILLER  PIC X(11) VALUE "PRECO MEDIO".
           02 FILLER  PIC X(12) VALUE SPACES.
           02 FILLER  PIC X(11) VALUE "PRECO TOTAL".
       01  ULTIMA-PAGINA.
           02 FILLER                 PIC X(08)        VALUE SPACES.
           02 QTDE-PRODUTO           PIC Z.ZZZ.ZZ9    VALUE ZEROS.
           02 FILLER                 PIC X(15)        VALUE SPACES.
           02 PRECO-MEDIO-M          PIC ZZZ.ZZ9,99   VALUE ZEROS.
           02 FILLER                 PIC X(12)        VALUE SPACES.
           02 PRECO-TOTAL-M          PIC Z.ZZZ.ZZ9,99 VALUE ZEROS.
       01  DETALHE.
           02 FILLER            PIC X(04)    VALUE SPACES.
           02 CODIGO-DET        PIC 9(05)    VALUE ZEROS.
           02 FILLER            PIC X(03)    VALUE SPACES.
           02 NOME-DET          PIC X(15)    VALUE SPACES.
           02 FILLER            PIC X(01)    VALUE SPACES.
           02 QTDE-ESTOQUE-DET  PIC 9(07)    VALUE ZEROS.
           02 FILLER            PIC X(07)    VALUE SPACES.
           02 PRECO-UNIT-DET    PIC 9(05)V99 VALUE ZEROS.
           02 FILLER            PIC X(09)    VALUE SPACES.
           02 PRECO-TOT-DET     PIC 9(06)V99 VALUE ZEROS.
       SCREEN SECTION.
       01  TELA.
           02 BLANK SCREEN.
           02 LINE 10 COLUMN 20 VALUE "I M P R I M I N D O . . ."
           REVERSE-VIDEO BLINK.
       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARQMAT.
           IF WS-STATUS NOT = "00"
               IF WS-STATUS NOT = "30"
                   DISPLAY "ARQUIVO NAO SE ENCONTRA NO DISCO" AT 2020
                   STOP RUN
                ELSE
                DISPLAY "ARQUIVO DANIFICADO" AT 2020
                DISPLAY "CODIGO STATUS = " WS-STATUS AT 2120
                STOP RUN
           ELSE
               NEXT SENTENCE
           END-IF.
           OPEN OUTPUT RELATO.
           ACCEPT DATA-SISTEMA FROM DATE.
           ACCEPT CONFIRMA AT 3130.
           MOVE DIA TO DIA-CAB01.
           MOVE MES TO MES-CAB01.
           MOVE ANO TO ANO-CAB01.
           DISPLAY TELA.
       CABECALHO.
           MOVE SPACES TO REG-RELATO.
           ADD 1 TO CT-PAG.
           MOVE CT-PAG TO PAG-CAB01.
           WRITE REG-RELATO FROM CAB01 BEFORE ADVANCING 3 LINES.
           WRITE REG-RELATO FROM CAB02 BEFORE ADVANCING 2 LINES.
       LER.
           READ ARQMAT AT END GO TO FIM.
           MOVE FD-CODIGO TO CODIGO-DET.
           MOVE FD-NOME TO NOME-DET.
           MOVE FD-QTDE-ESTOQUE TO QTDE-ESTOQUE-DET.
           MOVE FD-PRECO-UNIT TO PRECO-UNIT-DET.
           MOVE FD-PRECO-TOT TO PRECO-TOT-DET.
           COMPUTE QUANTIDADE-PRODUTO  =
           QUANTIDADE-PRODUTO + FD-QTDE-ESTOQUE.
           COMPUTE PRECO-TOTAL =
           PRECO-TOTAL + FD-PRECO-TOT.
           ADD 1 TO TOTAL-REGISTROS.
           WRITE REG-RELATO FROM DETALHE BEFORE ADVANCING 2 LINES
           AT EOP PERFORM CABECALHO.
           GO TO LER.
       CABECALHO-FINAL.
           ADD 1 TO CT-PAG.
           MOVE CT-PAG TO PAG-CAB01.
           WRITE REG-RELATO FROM CAB01 BEFORE ADVANCING 3 LINES.
           WRITE REG-RELATO FROM CAB03 BEFORE ADVANCING 2 LINES.
           WRITE REG-RELATO FROM ULTIMA-PAGINA.  
       FIM.
           MOVE QUANTIDADE-PRODUTO TO QTDE-PRODUTO.
           MOVE PRECO-TOTAL TO PRECO-TOTAL-M.
           COMPUTE PRECO-MEDIO = PRECO-TOTAL / TOTAL-REGISTROS.
           MOVE PRECO-MEDIO TO PRECO-MEDIO-M.
           MOVE SPACES TO REG-RELATO.
           WRITE REG-RELATO BEFORE PAGE.
           PERFORM CABECALHO-FINAL. 
           CLOSE RELATO ARQMAT.
           STOP RUN.
           END PROGRAM EXER9.

