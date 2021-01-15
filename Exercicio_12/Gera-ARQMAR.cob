      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:14/01/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:12 PAG:127
      * Tectonics: cobc
      * Objetivo: ARQUIVO DE APOIO EXERC 12
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER121.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQMAR ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQMAR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "ARQMAR".
       01  REG-ARQMAR.
           02 FD-MATRICULA           PIC 9(06)       VALUE ZEROS.
           02 FD-NOME                PIC X(30)       VALUE SPACES.
           02 FD-CIDADE              PIC X(20)       VALUE ZEROS.
           02 FD-ESTADO              PIC XX          VALUE ZEROS.
           02 FD-CODIGO              PIC 99          VALUE ZEROS.
       WORKING-STORAGE SECTION.
       77  CONFIRMA PIC X(01) VALUE SPACES.
       77  WS-STATUS PIC X(02) VALUE SPACES.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "PROGRAMA QUE GERA E POPULA ARQMAR".
           02 LINE 03 COLUMN 02 VALUE "MATRICULA:".
           02 LINE 04 COLUMN 02 VALUE "NOME:".
           02 LINE 05 COLUMN 02 VALUE "CIDADE:".
           02 LINE 06 COLUMN 02 VALUE "ESTADO:".
           02 LINE 07 COLUMN 02 VALUE "CODIGO:".
       PROCEDURE DIVISION.
       INICIO.
           OPEN OUTPUT ARQMAR.
       GRAVA-DADOS.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT FD-MATRICULA AT 0317.
           ACCEPT FD-NOME AT   0417.
           ACCEPT FD-CIDADE AT 0517.
           ACCEPT FD-ESTADO AT 0617.
           ACCEPT FD-CODIGO AT 0717.
           ACCEPT CONFIRMA AT 0917.
           WRITE REG-ARQMAR.
           DISPLAY "PARA DE INSERIR ? S-SIM" AT 0902.
           ACCEPT CONFIRMA AT 0917.
           IF CONFIRMA = "S"
               CLOSE ARQMAR
               DISPLAY "ARQUIVO GERADO E GRAVADO SAINDO" AT 1002
               ACCEPT CONFIRMA AT 1017
               STOP RUN
           ELSE
               GO TO GRAVA-DADOS.
       END PROGRAM EXER121.
