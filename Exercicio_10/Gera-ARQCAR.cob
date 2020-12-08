      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:07/12/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:9 PAG:110
      * Tectonics: cobc
      * Objetivo: ARQUIVO DE APOIO EXERC 9
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER101.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCAR ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQCAR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "ARQCAR".
       01  REG-ARQCAR.
           02 FD-CODIGO              PIC 9(03)       VALUE ZEROS.
           02 FD-MARCA               PIC X(15)       VALUE SPACES.
           02 FD-COR                 PIC X(10)       VALUE ZEROS.
           02 FD-ANO                 PIC 9(04)       VALUE ZEROS.
           02 FD-CHAPA               PIC X(06)       VALUE ZEROS.
           02 FD-MODELO              PIC X(10)       VALUE SPACES.
       WORKING-STORAGE SECTION.
       77  CONFIRMA PIC X(01) VALUE SPACES.
       77  WS-STATUS PIC X(02) VALUE SPACES.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "PROGRAMA QUE GERA E POPULA ARQCAR".
           02 LINE 03 COLUMN 02 VALUE "CODIGO:".
           02 LINE 04 COLUMN 02 VALUE "MARCA:".
           02 LINE 05 COLUMN 02 VALUE "COR:".
           02 LINE 06 COLUMN 02 VALUE "ANO:".
           02 LINE 07 COLUMN 02 VALUE "CHAPA:".
           02 LINE 08 COLUMN 02 VALUE "MODELO:".
       PROCEDURE DIVISION.
       INICIO.
           OPEN OUTPUT ARQCAR.
       GRAVA-DADOS.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT FD-CODIGO AT 0317.
           ACCEPT FD-MARCA AT   0417.
           ACCEPT FD-COR AT 0517.
           ACCEPT FD-ANO AT 0617.
           ACCEPT FD-CHAPA AT 0717.
           ACCEPT FD-MODELO AT 0817.
           ACCEPT CONFIRMA AT 0917.
           WRITE REG-ARQCAR.
           DISPLAY "PARA DE INSERIR ? S-SIM" AT 0902.
           ACCEPT CONFIRMA AT 0917.
           IF CONFIRMA = "S"
               CLOSE ARQCAR
               DISPLAY "ARQUIVO GERADO E GRAVADO SAINDO" AT 1002
               ACCEPT CONFIRMA AT 1017
               STOP RUN
           ELSE
               GO TO GRAVA-DADOS.
       END PROGRAM EXER101.
