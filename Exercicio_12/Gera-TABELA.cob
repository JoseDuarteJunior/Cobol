
      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:14/01/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:12 PAG:127
      * Tectonics: cobc
      * Objetivo: ARQUIVO DE APOIO II EXERC 12
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER122.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TABELA ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  TABELA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "TABELA".
       01  REG-TABELA.
           02 FD-CODIGO              PIC 9(03)       VALUE ZEROS.
           02 FD-MODELO              PIC X(15)       VALUE SPACES.
           02 FD-VALOR               PIC 9(7)V99     VALUE ZEROS.
       WORKING-STORAGE SECTION.
       77  CONFIRMA PIC X(01) VALUE SPACES.
       77  WS-STATUS PIC X(02) VALUE SPACES.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "PROGRAMA QUE GERA E POPULA TABELA".
           02 LINE 03 COLUMN 02 VALUE "CODIGO:".
           02 LINE 04 COLUMN 02 VALUE "MODELO:".
           02 LINE 05 COLUMN 02 VALUE "VALOR:".
       PROCEDURE DIVISION.
       INICIO.
           OPEN OUTPUT TABELA.
       GRAVA-DADOS.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT FD-CODIGO AT 0317.
           ACCEPT FD-MODELO AT   0417.
           ACCEPT FD-VALOR AT 0517.
           ACCEPT CONFIRMA AT 0617.
           WRITE REG-TABELA.
           DISPLAY "PARA DE INSERIR ? S-SIM" AT 0802.
           ACCEPT CONFIRMA AT 0917.
           IF CONFIRMA = "S"
               CLOSE TABELA
               DISPLAY "ARQUIVO GERADO E GRAVADO SAINDO" AT 1002
               ACCEPT CONFIRMA AT 1017
               STOP RUN
           ELSE
               GO TO GRAVA-DADOS.
       END PROGRAM EXER122.
