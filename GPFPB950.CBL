      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB950.
       AUTHOR.                     ULISSES & MORAES - TI.
       DATE-WRITTEN.               02/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES & MORAES - TI                          *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES & MORAES - TI                          *
      * DATA.........: 02/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A PARTIR DA TABELA LOG DE CONTA PARTICIPANTE,  *
      *                MONTAR SYSOUT DE ESTATISTICA DE CONVENIO.      *
      *****************************************************************
      *
      *****************************************************************
       ENVIRONMENT                 DIVISION.
      *****************************************************************
       CONFIGURATION               SECTION.
      *****************************************************************
       SPECIAL-NAMES.              DECIMAL-POINT   IS    COMMA.
      *****************************************************************
       INPUT-OUTPUT                SECTION.
      *****************************************************************
       FILE-CONTROL.
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-COUNT            PIC    S9(009) COMP VALUE +0.
      *
       01      WS-VLR-SUM          PIC    S9(13)V9(2) USAGE COMP-3.
       01      WS-VLR-MAX          PIC    S9(13)V9(2) USAGE COMP-3.
       01      WS-VLR-MIN          PIC    S9(13)V9(2) USAGE COMP-3.
       01      WS-VLR-AVG          PIC    S9(13)V9(2) USAGE COMP-3.
      *
       01      WS-EDI-QTD          PIC     Z.ZZZ.ZZZ.ZZ9.
       01      WS-EDI-VLR          PIC     ZZ.ZZZ.ZZ9,99.
      *
      *****************************************************************
      *        VARIAVEIS PARA TRATAMENTO DE ABEND                     *
      *****************************************************************
      *
       01      WS-ACESSO-ARQ       PIC     X(013) VALUE SPACES.
       01      WS-DDNAME-ARQ       PIC     X(008) VALUE SPACES.
       01      WS-FS-ARQ           PIC     9(002) VALUE ZEROS.
      *
       01      WS-ACESSO-DB2       PIC     X(006) VALUE SPACES.
       01      WS-TABELA-DB2       PIC     X(008) VALUE SPACES.
       01      WS-SQLCOD-DB2       PIC     +++9.
      *
       01      WS-PTO-ERRO         PIC     9(003) VALUE ZEROS.
      *
      *****************************************************************
      *        AREA DE TRATAMENTO DB2                                 *
      *****************************************************************
      *
           EXEC    SQL
                   INCLUDE SQLCA
           END-EXEC.
      /
           EXEC    SQL
                   INCLUDE TCSTBS61
           END-EXEC.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           MOVE    +0              TO      WS-COUNT
                                           WS-VLR-SUM
                                           WS-VLR-MAX
                                           WS-VLR-MIN
                                           WS-VLR-AVG.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           PERFORM 1100-00-SCOUNT-TCSTBS61.

           PERFORM 1200-00-SELSUM-TCSTBS61.

           PERFORM 1300-00-SELMAX-TCSTBS61.

           PERFORM 1400-00-SELMIN-TCSTBS61.

           PERFORM 1500-00-SELAVG-TCSTBS61.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-SCOUNT-TCSTBS61     SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   SELECT  COUNT(*)

                   INTO   :WS-COUNT

                   FROM    TCS.TCSTBS61_SMRZOGRCS

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100 AND -305
                   MOVE 'COUNT'    TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  001       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100 OR -305
                   MOVE +0         TO      WS-COUNT
           END-IF.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-SELSUM-TCSTBS61     SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   SELECT  SUM(VR_CONTRIBUICAO)

                   INTO   :WS-VLR-SUM

                   FROM    TCS.TCSTBS61_SMRZOGRCS

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100 AND -305
                   MOVE 'SELSUM'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  002       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100 OR -305
                   MOVE +0         TO      WS-VLR-SUM
           END-IF.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1300-00-SELMAX-TCSTBS61     SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   SELECT  MAX(VR_CONTRIBUICAO)

                   INTO   :WS-VLR-MAX

                   FROM    TCS.TCSTBS61_SMRZOGRCS

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100 AND -305
                   MOVE 'SELMAX'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  003       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100 OR -305
                   MOVE +0         TO      WS-VLR-MAX
           END-IF.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-SELMIN-TCSTBS61     SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   SELECT  MIN(VR_CONTRIBUICAO)

                   INTO   :WS-VLR-MIN

                   FROM    TCS.TCSTBS61_SMRZOGRCS

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100 AND -305
                   MOVE 'SELMIN'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  004       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100 OR -305
                   MOVE +0         TO      WS-VLR-MIN
           END-IF.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-SELAVG-TCSTBS61     SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   SELECT  AVG(VR_CONTRIBUICAO)

                   INTO   :WS-VLR-AVG

                   FROM    TCS.TCSTBS61_SMRZOGRCS

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100 AND -305
                   MOVE 'SELAVG'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  005       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100 OR -305
                   MOVE +0         TO      WS-VLR-AVG
           END-IF.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB950 ************ DB2 *'.
           MOVE    WS-COUNT        TO      WS-EDI-QTD.
           DISPLAY '* QTDE. ENTIDADES.....- COUNT = ' WS-EDI-QTD ' *'.
           DISPLAY '******************* GFFPB950 ******************'.
           MOVE    WS-VLR-SUM      TO      WS-EDI-VLR.
           DISPLAY '* VALOR TOTAL CONTRIB.- SUM   = ' WS-EDI-VLR ' *'.
           DISPLAY '******************* GPFPB950 ******************'.
           MOVE    WS-VLR-MAX      TO      WS-EDI-VLR.
           DISPLAY '* MAIOR VALOR CONTRIB.- MAX   = ' WS-EDI-VLR ' *'.
           DISPLAY '******************* GPFPB950 ******************'.
           MOVE    WS-VLR-MIN      TO      WS-EDI-VLR.
           DISPLAY '* MENOR VALOR CONTRIB.- MIN   = ' WS-EDI-VLR ' *'.
           DISPLAY '******************* GPFPB950 ******************'.
           MOVE    WS-VLR-AVG      TO      WS-EDI-VLR.
           DISPLAY '* MEDIA VALOR CONTRIB.- AVG   = ' WS-EDI-VLR ' *'.
           DISPLAY '******************* GPFPB950 ******************'.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0998-00-ABEND-DB2           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           MOVE    SQLCODE         TO      WS-SQLCOD-DB2.

           DISPLAY '******************* GPFPB950 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB950 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*   PROBLEMAS NO ' WS-ACESSO-DB2 ' DA TABELA '
           WS-TABELA-DB2 '    *'.
           DISPLAY '*                                             *'.
           DISPLAY '*                SQLCODE = ' WS-SQLCOD-DB2
           '               *'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PONTO COM ERRO...: ' WS-PTO-ERRO
           '            *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB950 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB950 ******************'.

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0998-00-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB950 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB950 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '* PROBLEMAS ' WS-ACESSO-ARQ ' DO ARQUIVO '
           WS-DDNAME-ARQ ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '*             FILE STATUS....: ' WS-FS-ARQ
           '             *'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PONTO COM ERRO...: ' WS-PTO-ERRO
           '            *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB950 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB950 ******************'.

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB950                  *
      *****************************************************************
