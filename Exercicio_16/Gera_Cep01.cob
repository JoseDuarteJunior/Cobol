
      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:15/02/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:16 PAG:170
      * Tectonics: cobc
      * Objetivo: ARQUIVO DE APOIO EXER 16
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER161.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CEP01 ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CEP01
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CEP01".
       01  REG-CEP01.
           02 FD-CEP                 PIC 9(05)       VALUE ZEROS.
           02 FD-ENDERECO            PIC X(30)       VALUE SPACES.
           02 FD-CIDADE              PIC X(20)       VALUE SPACES.
           02 FD-ESTADO              PIC X(02)       VALUE SPACES.
       WORKING-STORAGE SECTION.
           77  CONFIRMA PIC X(01) VALUE SPACES.
           77  WS-STATUS PIC X(02) VALUE SPACES.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "PROGRAMA QUE GERA E POPULA CEP01".
           02 LINE 03 COLUMN 02 VALUE "CEP".
           02 LINE 04 COLUMN 02 VALUE "ENDERECO:".
           02 LINE 05 COLUMN 02 VALUE "CIDADE:".
           02 LINE 06 COLUMN 02 VALUE "ESTADO:".
       PROCEDURE DIVISION.
       INICIO.
           OPEN OUTPUT CEP01.
       GRAVA-DADOS.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT FD-CEP       AT 0320.
           ACCEPT FD-ENDERECO  AT 0420.
           ACCEPT FD-CIDADE    AT 0520.
           ACCEPT FD-ESTADO    AT 0620.
           ACCEPT CONFIRMA AT 0917.
           WRITE REG-CEP01.
           DISPLAY "PARA DE INSERIR ? S-SIM" AT 1002.
           ACCEPT CONFIRMA AT 1026.
           IF CONFIRMA = "S"
               CLOSE CEP01
               DISPLAY "ARQUIVO GERADO E GRAVADO SAINDO" AT 1102
               ACCEPT CONFIRMA AT 1134
               STOP RUN
           ELSE
               GO TO GRAVA-DADOS.
       END PROGRAM EXER161.
