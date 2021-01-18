      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:18/01/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:13 PAG:144
      * Tectonics: cobc
      * Objetivo: MANUTENCAO DE ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER13.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQMAT ASSIGN TO DISK
           ORGANIZATION     INDEXED
           ACCESS MODE      DYNAMIC
           RECORD KEY       FD-CODIGO
           FILE STATUS      FS.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQMAT LABEL RECORD STANDARD
           VALUE OF FILE-ID "ARQMAT".
       01  REG-ARQMAT.
           02 FD-CODIGO.
               03 CODIGO        PIC 9(05).
           02 FD-NOME           PIC X(30).
           02 FD-QTDE-ESTOQUE   PIC S9(04).
           02 FD-VALOR-UNIT     PIC S9(07)V99.
           02 FD-VALOR-TOT      PIC S9(08)V99.
       WORKING-STORAGE SECTION.
       77  FS                   PIC X(02) VALUE SPACES.
       77  CONFIRMA             PIC X(01) VALUE SPACES.
       77  FUNCAO               PIC X(02) VALUE SPACES.
       77  CAMPO                PIC 9(01) VALUE ZEROS.
       01  TMP-VALORES.
           02 QTE-ESTOQUE       PIC ZZZZ.
           02 VALOR-UNITARIO    PIC Z9,99.
           02 VALOR-TOTAL       PIC Z9,99.
       01  WS-DATA.
           02 ANO               PIC 9(02) VALUE ZEROS.
           02 MES               PIC 9(02) VALUE ZEROS.
           02 DIA               PIC 9(02) VALUE ZEROS.
       01  WS-MENSAGENS.
           02 LIMPA-MENSAGEM    PIC X(50) VALUE SPACES.
           02 MSG-ERRO-1        PIC X(22) VALUE "CODIGO NAO PODE SER 0".
           02 MSG-ERRO-2        PIC X(22) VALUE "ESTE CODIGO JA EXISTE".
           02 MSG-ERRO-3        PIC X(17) VALUE "CODIGO NAO EXISTE".
       SCREEN SECTION.
       01  TELA1.
            02 BLANK SCREEN.
            02 LINE 01 COLUMN 01 VALUE "DATA:".
            02 LINE 01 COLUMN 30 VALUE "CONTROLE DE ESTOQUE".
            02 LINE 03 COLUMN 01 VALUE "ESCOLHA A FUNCAO: ".
            02 LINE 03 COLUMN 22 VALUE "(IN - AL - EX - CO - FF)".
            02 LINE 11 COLUMN 01 VALUE "MENSAGEM:".
            02 LINE 05 COLUMN 05 VALUE "CODIGO..................>".
            02 LINE 06 COLUMN 05 VALUE "NOME....................>".
            02 LINE 07 COLUMN 05 VALUE "QUANTIDADE ESTOQUE......>".
            02 LINE 08 COLUMN 05 VALUE "VALOR UNITARIO..........>".
            02 LINE 09 COLUMN 05 VALUE "VALOR TOTAL.............>".
       01  LIMPA-TELA.
            02 LINE 05 COLUMN 35 VALUE "     ".
            02 LINE 06 COLUMN 35 VALUE "                              ".
            02 LINE 07 COLUMN 35 VALUE "    ".
            02 LINE 08 COLUMN 35 VALUE "          ".
            02 LINE 09 COLUMN 35 VALUE "           ".
            02 LINE 11 COLUMN 35 VALUE "                              ".
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARQMAT.
           IF FS NOT = "00"
               IF FS = "35"
                   CLOSE ARQMAT OPEN OUTPUT ARQMAT CLOSE ARQMAT
                   GO TO INICIO
               ELSE
                   DISPLAY FS "STATUS DO ARQUIVO"
                   STOP RUN
           END-IF.
       TELA-1.
           DISPLAY TELA1.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY DIA AT 0107 '/' MES '/' ANO.
           ACCEPT FUNCAO AT 0319.
           EVALUATE FUNCAO
               WHEN "IN"
                  GO TO INCLUSAO
               WHEN "AL"
                  GO TO ALTERACAO
               WHEN "EX"
                  GO TO EXCLUSAO
               WHEN "CO"
                  GO TO CONSULTA
               WHEN "FF"
                  GO TO FIM 
               WHEN OTHER
                  DISPLAY "OPCAO INVALIDA" AT 1110
                  ACCEPT CONFIRMA AT 1124
                  GO TO TELA-1
           END-EVALUATE.
       INCLUSAO.
           ACCEPT CODIGO AT 0535.
           IF CODIGO = ZEROS
               DISPLAY MSG-ERRO-1 AT 1110
               ACCEPT CONFIRMA AT 1133
               DISPLAY LIMPA-MENSAGEM AT 1110
               GO TO INCLUSAO.
           READ ARQMAT INVALID KEY GO TO ROT-NOME.
           DISPLAY MSG-ERRO-2 AT 1110.
           ACCEPT CONFIRMA AT 1133.
           DISPLAY LIMPA-MENSAGEM AT 1110.
           GO TO INCLUSAO.
       ROT-NOME.
           ACCEPT FD-NOME AT 0635.
           IF FD-NOME = SPACES 
               DISPLAY MSG-ERRO-1 AT 1110
               ACCEPT CONFIRMA AT 1133
               DISPLAY LIMPA-MENSAGEM AT 1110
               GO TO ROT-NOME
           END-IF.
       ROT-QUANTIDADE-ESTOQUE.
           ACCEPT FD-QTDE-ESTOQUE AT 0735.
           IF FD-QTDE-ESTOQUE <= 0 
               DISPLAY MSG-ERRO-1 AT 1110
               ACCEPT CONFIRMA AT 1133
               DISPLAY LIMPA-MENSAGEM AT 1110
               GO TO ROT-QUANTIDADE-ESTOQUE
           END-IF.
       ROT-VALOR-UNITARIO.
           ACCEPT FD-VALOR-UNIT AT 0835.
           IF FD-VALOR-UNIT <=0
               DISPLAY MSG-ERRO-1 AT 1110
               ACCEPT CONFIRMA AT 1133
               DISPLAY LIMPA-MENSAGEM AT 1110
               GO TO ROT-VALOR-UNITARIO
           END-IF.
       ROT-VALOR-TOTAL.
           COMPUTE FD-VALOR-TOT = FD-QTDE-ESTOQUE * FD-VALOR-UNIT.
           DISPLAY FD-VALOR-TOT AT 0935.
       ROT-GRAVA-REGISTRO.
           WRITE REG-ARQMAT INVALID KEY
           DISPLAY "ERRO DE GRAVAÇÃO"
           STOP RUN.
       ROT-RETORNO.
           DISPLAY "CONTINUAR INCLUSAO ? S-SIM" AT 1110.
           ACCEPT CONFIRMA AT 1137.
           IF CONFIRMA = "S" OR "s"
               DISPLAY LIMPA-MENSAGEM AT 1110
               DISPLAY LIMPA-TELA
               GO TO INCLUSAO.
           GO TO TELA-1.
       ALTERACAO.
           ACCEPT CODIGO AT 0535.
           IF CODIGO = ZEROS
               DISPLAY MSG-ERRO-1 AT 1110
               ACCEPT CONFIRMA AT 1133
               DISPLAY LIMPA-MENSAGEM AT 1110
               GO TO ALTERACAO.
           READ ARQMAT INVALID KEY
           DISPLAY MSG-ERRO-3 AT 1110 GO TO ALTERACAO.
           PERFORM MOSTRA.
       ALTERA.
           DISPLAY LIMPA-MENSAGEM AT 1110.
           DISPLAY "DIGITE NUMERO DO CAMPO QUE QUER ALTERAR" AT 1110.
           ACCEPT CAMPO AT 1151.
           DISPLAY LIMPA-MENSAGEM AT 1110
           IF CAMPO =0 OR > 3 
               GO TO ALTERA.
           GO TO CPO-NOME CPO-ESTOQUE CPO-VALOR-UNIT
             DEPENDING ON CAMPO.
       CPO-NOME.
           PERFORM ROT-NOME.
           GO TO ROT-REGRAVA.
       CPO-ESTOQUE.
           PERFORM ROT-QUANTIDADE-ESTOQUE.
           PERFORM ROT-VALOR-TOTAL.
           GO TO ROT-REGRAVA.
       CPO-VALOR-UNIT.
           PERFORM ROT-VALOR-UNITARIO.
           PERFORM ROT-VALOR-TOTAL.
           GO TO ROT-REGRAVA.
       ROT-REGRAVA.
           DISPLAY LIMPA-MENSAGEM AT 1110.
           DISPLAY "CONTINUAR ALTERANDO ESTE REGISTRO ? S-SIM" AT 1110.
           ACCEPT CONFIRMA AT 1151.
           IF CONFIRMA = "S" OR "s"
               GO TO ALTERA.
           REWRITE REG-ARQMAT INVALID KEY
           DISPLAY "ERRO DE GRAVACAO" FS AT 1110 STOP RUN.
           DISPLAY LIMPA-MENSAGEM AT 1110.
           DISPLAY "CONTINUAR ALTERANDO ? S-SIM" AT 1110.
           ACCEPT CONFIRMA AT 1138.
           IF CONFIRMA = "S" OR "s"
               DISPLAY LIMPA-MENSAGEM AT 1110
               DISPLAY LIMPA-TELA
               GO TO ALTERACAO.
           GO TO TELA-1.
       MOSTRA.
           MOVE     FD-QTDE-ESTOQUE TO QTE-ESTOQUE.
           MOVE     FD-VALOR-UNIT   TO VALOR-UNITARIO.
           MOVE     FD-VALOR-TOT    TO VALOR-TOTAL.
           DISPLAY  FD-NOME          AT 0635.
           DISPLAY  QTE-ESTOQUE      AT 0735.
           DISPLAY  VALOR-UNITARIO   AT 0835.
           DISPLAY  VALOR-TOTAL      AT 0935.
       EXCLUSAO.
           PERFORM ALTERACAO.
           DISPLAY "APAGAR ESTE REGISTRO ? S-SIM" AT 1110.
           ACCEPT CONFIRMA AT 1139.
           DISPLAY LIMPA-MENSAGEM AT 1110.
           IF CONFIRMA = "S" OR "s"
               NEXT SENTENCE
           ELSE
              DISPLAY LIMPA-TELA GO TO EXCLUSAO.
           DELETE ARQMAT INVALID KEY
           DISPLAY "ERRO DE EXCLUSAO" FS AT 1110
           STOP RUN.
           DISPLAY LIMPA-MENSAGEM AT 1110.
           DISPLAY "CONTINUAR EXCLUSAO? S-SIM" AT 1110.
           ACCEPT CONFIRMA AT 1136.
           IF CONFIRMA = "S" OR "s"
               DISPLAY LIMPA-TELA GO TO EXCLUSAO.
           GO TO TELA-1.
       CONSULTA.
           PERFORM ALTERACAO.
           DISPLAY "CONTINUAR CONSULTA ? S-SIM" AT 1110.
           ACCEPT CONFIRMA AT 1137.
           IF CONFIRMA = "S" OR "s"
               DISPLAY LIMPA-TELA GO TO CONSULTA.
           GO TO TELA-1.
       FIM.    
           DISPLAY "FIM DO PROCESSAMENTO " AT 1110.
           CLOSE ARQMAT.
           STOP RUN.     
           END PROGRAM EXER13.
           
           
       
