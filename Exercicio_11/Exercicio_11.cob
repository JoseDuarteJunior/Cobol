
      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:08/12/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:7 PAG:125
      * Tectonics: cobc
      * Objetivo: MANIPULACAO DE TABELA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER11.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  DATA-SISTEMA.
           02 ANO           PIC 9(02) VALUE ZEROS.
           02 MES           PIC 9(02) VALUE ZEROS.
           02 DIA           PIC 9(02) VALUE ZEROS.
       77  SIGLA-ESTADO     PIC X(02) VALUE SPACES.
       77  CONFIRMA         PIC X(01) VALUE SPACES.
       01  TABELA-SIGLA.
           02 FILLER    PIC X(54) VALUE
           "ACALAMAPBACEDFESFNGOMAMGMSMTPAPBPEPIPRRJRNRORRRSSCSESP".
       01 SIGLA REDEFINES TABELA-SIGLA.
           02 TABELA-SIGLA-R      PIC 99 OCCURS 27 TIMES INDEXED BY IND.
       01  TABELA-ESTADO.
           02 FILLER    PIC X(19) VALUE  "ACRE".
           02 FILLER    PIC X(19) VALUE  "ALAGOAS".
           02 FILLER    PIC X(19) VALUE  "AMAZONAS".
           02 FILLER    PIC X(19) VALUE  "AMAPA".
           02 FILLER    PIC X(19) VALUE  "BAHIA".
           02 FILLER    PIC X(19) VALUE  "CEARA".
           02 FILLER    PIC X(19) VALUE  "DISTRITO FEDERAL".
           02 FILLER    PIC X(19) VALUE  "ESPIRITO SANTO".
           02 FILLER    PIC X(19) VALUE  "FERNANDO DE NORONHA".
           02 FILLER    PIC X(19) VALUE  "GOIAS".
           02 FILLER    PIC X(19) VALUE  "MARANHAO".
           02 FILLER    PIC X(19) VALUE  "MINAS GERAIS".
           02 FILLER    PIC X(19) VALUE  "MATO GROSSO DO SUL".
           02 FILLER    PIC X(19) VALUE  "MATO GROSSO".
           02 FILLER    PIC X(19) VALUE  "PARA".
           02 FILLER    PIC X(19) VALUE  "PARAIBA".
           02 FILLER    PIC X(19) VALUE  "PERNAMBUCO".
           02 FILLER    PIC X(19) VALUE  "PIAUI".
           02 FILLER    PIC X(19) VALUE  "PARANA".
           02 FILLER    PIC X(19) VALUE  "RIO DE JANEIRO".
           02 FILLER    PIC X(19) VALUE  "RIO GRANDE DO NORTE".
           02 FILLER    PIC X(19) VALUE  "RONDONIA".
           02 FILLER    PIC X(19) VALUE  "RORAIMA".
           02 FILLER    PIC X(19) VALUE  "RIO GRANDE DO SUL".
           02 FILLER    PIC X(19) VALUE  "SANTA CATARINA".
           02 FILLER    PIC X(19) VALUE  "SERGIPE".
           02 FILLER    PIC X(19) VALUE  "SAO PAULO".
       01  ESTADOS REDEFINES TABELA-ESTADO.
           02 TABELA-ESTADO-R     PIC X(19) OCCURS 27 TIMES.
       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 03 VALUE "DATA-DO-DIA:".
           02 LINE 04 COLUMN 15 VALUE "T__A__B__E__L__A".
           02 LINE 06 COLUMN 03 VALUE "DIGITE A SIGLA:".
           02 LINE 08 COLUMN 03 VALUE "ESTADO/TERRITORIO: ___________".
           02 LINE 10 COLUMN 03 VALUE "Continua? (S/N)  < >".
           02 LINE 12 COLUMN 03 VALUE "MENSAGEM: ____________________".
       PROCEDURE DIVISION.
       INICIO.
           SET IND TO 1.
           DISPLAY TELA.
           ACCEPT DATA-SISTEMA FROM DATE.
           DISPLAY DIA AT 0219 '/' MES '/' ANO.
           ACCEPT SIGLA-ESTADO AT 0620.
           PERFORM PROCURA-ESTADO UNTIL 
               TABELA-SIGLA-R(IND)=SIGLA-ESTADO OR IND >26.
           IF IND > 26
               DISPLAY "SIGLA INVALIDA" AT 1213
               ACCEPT CONFIRMA AT 1229
               GO TO INICIO
           END-IF.
           IF TABELA-SIGLA-R(IND) = SIGLA-ESTADO
               DISPLAY TABELA-ESTADO-R(IND) AT 0822
               ACCEPT CONFIRMA AT 1021
               IF CONFIRMA= "S"
                   GO TO INICIO
               ELSE
                   GO TO ENCERRA-PROGRAMA
           END-IF.  
       PROCURA-ESTADO.
           ADD 1 TO IND.
       ENCERRA-PROGRAMA.
           END PROGRAM EXER11.
