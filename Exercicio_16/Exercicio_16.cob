      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:15/02/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:16 PAG:170
      * Tectonics: cobc
      * Objetivo: RELATORIOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER16.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CEP01 ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
           SELECT SORCEP ASSIGN TO DISK
           SORT STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CEP01 LABEL RECORD STANDARD
           VALUE OF FILE-ID "CEP01".
       01  REG-CEP01.
           02 FD-CEP                 PIC 9(05)      VALUE ZEROS.
           02 FD-ENDERECO            PIC X(30)      VALUE SPACES.
           02 FD-CIDADE              PIC X(20)      VALUE SPACES.
           02 FD-ESTADO              PIC X(02)      VALUE SPACES.
       SD  SORCEP VALUE OF FILE-ID IS "SORCEP".
       01  REG-SORCEP.
           02 SOR-CEP                PIC 9(05).
           02 SOR-ENDERECO           PIC X(30).
           02 SOR-CIDADE             PIC X(20).
           02 SOR-ESTADO             PIC X(02).
       WORKING-STORAGE SECTION.
           77  CONFIRMA          PIC X(01)     VALUE SPACES.
           77  WS-STATUS         PIC 9(02)     VALUE ZEROS.
           77  WS-TEMP           PIC 9(05)     VALUE ZEROS.
           77  LINHA             PIC 9(06)     VALUE 4.
           77  REPETIDO          PIC 9(02)     VALUE ZEROS.
           77  CEP-TEMP          PIC 9(05)     VALUE ZEROS.
       SCREEN SECTION.
       01  TELA-1.
            02 BLANK SCREEN.
            02 LINE 01 COLUMN 27 VALUE "RELATORIO DE CEPS CONSULTADOS".
            02 LINE 02 COLUMN 01
       VALUE "-------------------------------------------------".
           02 LINE 02 COLUMN 50 VALUE "-------------------------------".
           
           02 LINE 03 COLUMN 02 VALUE "CEP         |".
           02 LINE 03 COLUMN 16 VALUE "ENDERECO    |".
           02 LINE 03 COLUMN 29 VALUE "CIDADE      |".
           02 LINE 03 COLUMN 43 VALUE "ESTADO      |".
           02 LINE 03 COLUMN 57 VALUE "REPETIDO".
       PROCEDURE DIVISION.
       INICIO.
           SORT SORCEP ASCENDING KEY SOR-CEP
           INPUT PROCEDURE ENTRADA
           OUTPUT PROCEDURE SAIDA.
           STOP RUN.
       ENTRADA SECTION.
       INI-ENTRADA.
           OPEN INPUT CEP01.
           DISPLAY TELA-1.
           IF WS-STATUS = "00" NEXT SENTENCE
               ELSE
                   DISPLAY "ERRO DE LEITURA" AT 2020
           CLOSE CEP01 STOP RUN.
       LER-ENTRADA.
           READ CEP01 AT END CLOSE CEP01 GO TO FIM-ENTRADA.
           IF FD-CEP = WS-TEMP
               GO TO LER-ENTRADA
           ELSE
               MOVE FD-CEP TO WS-TEMP
               MOVE REG-CEP01 TO REG-SORCEP.
           RELEASE REG-SORCEP.
           IF WS-STATUS = "00" NEXT SENTENCE
           ELSE DISPLAY "ERRO NO RELEASE" AT 2020
               CLOSE CEP01 STOP RUN.
           GO TO LER-ENTRADA.
       FIM-ENTRADA.
           EXIT.
       SAIDA SECTION.
       LER-SAIDA.
           IF LINHA >=10
               ACCEPT CONFIRMA AT 2020
               DISPLAY TELA-1
               MOVE 4 TO LINHA
           END-IF.
           RETURN SORCEP AT END GO TO FIM-SAIDA.
           IF SOR-CEP(4:2)=00
               IF SOR-CEP = CEP-TEMP
                   ADD 1 TO REPETIDO
                   COMPUTE LINHA = LINHA - 1
                   DISPLAY REPETIDO AT LINE LINHA COLUMN 57
                   ADD 1 TO LINHA
                   SUBTRACT 1 FROM REPETIDO
                   GO TO LER-SAIDA
               ELSE 
                   DISPLAY SOR-CEP AT LINE LINHA COLUMN 02
                   DISPLAY SOR-ENDERECO AT LINE LINHA COLUMN 16
                   DISPLAY SOR-CIDADE   AT LINE LINHA COLUMN 29
                   DISPLAY SOR-ESTADO   AT LINE LINHA COLUMN 43
                   DISPLAY REPETIDO     AT LINE LINHA COLUMN 57
               END-IF
               MOVE SOR-CEP TO CEP-TEMP
               MOVE 0 TO REPETIDO
               ADD 1 TO LINHA
           ELSE
               GO TO LER-SAIDA
           END-IF.
           GO TO LER-SAIDA.
       FIM-SAIDA.
           DISPLAY "PROGRAMA ENCERRADO" AT 2020.
           ACCEPT CONFIRMA AT 2450.
       EXIT-SAIDA.
           EXIT.
