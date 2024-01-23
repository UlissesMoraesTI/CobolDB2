      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB954.
       AUTHOR.                     ULISSES & MORAES - TI.
       DATE-WRITTEN.               09/03/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES & MORAES - TI                          *
      * LINGUAGEM....: COBOL/BATCH/DB2                                *
      * PROGRAMADOR..: ULISSES & MORAES - TI                          *
      * DATA.........: 09/09/2014                                     *
      *****************************************************************
      * OBJETIVO.....: EXPURGO ENTIDADES VENCIDAS                     *
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
      * OUTPUT.: CADMANC - CADASTRO DE MANUTENCOES      - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADMANC  ASSIGN  TO  UT-S-CADMANC
                   FILE     STATUS  IS  WS-FS-CADMANC.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
      * OUTPUT.: CADMANC - CADASTRO DE MANUTENCOES      - LRECL = 100 *
      *****************************************************************
      *
       FD  CADMANC
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADMANC         PIC     X(100).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADMANC       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-TCSTBS61     PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADMANC      PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
      *****************************************************************
      *        TRATA CONVERSAO DE DATAS                               *
      *****************************************************************
      *
       01      WS-DAT-INI          PIC     X(010) VALUE '99.99.9999'.
       01      WS-DAT-FIM          PIC     X(010) VALUE '99.99.9999'.
      *
       01      WS-DAT-AMD          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-AMD.
         03    WS-ANO-AMD          PIC     9(004).
         03    WS-MES-AMD          PIC     9(002).
         03    WS-DIA-AMD          PIC     9(002).
      *
       01      WS-DAT-DB2          PIC     X(010) VALUE '99.99.9999'.
       01      FILLER              REDEFINES      WS-DAT-DB2.
         03    WS-DIA-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MES-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-ANO-DB2          PIC     9(004).
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
      * OUTPUT.: CADMANC - CADASTRO DE MANUTENCOES      - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBI951.
      *
      *****************************************************************
      *            SAGBB006 - CRITICA DE DATAS                        *
      *****************************************************************
      *
       01          WS-COBBB006     PIC     X(008) VALUE 'COBBB006'.
      *
           COPY    SAGWS006.
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
      *            AREA DE TRATAMENTO DE CURSORES DB2                 *
      *****************************************************************
      *
           EXEC    SQL      DECLARE     C-TCSTBS61  CURSOR  FOR

                   SELECT   CO_ENTDE_SNDCL_S01,
                            CO_TIPO_ENTDE_S01 ,
                            DT_VENCIMENTO     ,
                            QT_GRCS           ,
                            VR_CONTRIBUICAO

                   FROM     TCS.TCSTBS61_SMRZOGRCS

                   WHERE    DT_VENCIMENTO >= :WS-DAT-INI
                   AND      DT_VENCIMENTO <= :WS-DAT-FIM

                   ORDER BY CO_ENTDE_SNDCL_S01,
                            CO_TIPO_ENTDE_S01 ,
                            DT_VENCIMENTO

                   FOR      FETCH       ONLY

           END-EXEC.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
      *
       01          LKG-PARM.
         03        LKG-TAM         PIC    S9(004) COMP.
         03        LKG-DAT-INI     PIC     9(008).
         03        LKG-DAT-FIM     PIC     9(008).
      *
      *****************************************************************
       PROCEDURE    DIVISION    USING    LKG-PARM.
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
           PERFORM 0150-00-VALIDA-PARM.

           OPEN    OUTPUT  CADMANC.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FS-CADMANC.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0150-00-VALIDA-PARM         SECTION.
      *****************************************************************
      *
           IF      LKG-TAM     NOT EQUAL   16
                   PERFORM         0997-00-ABEND-PARM
           END-IF.

           IF      LKG-DAT-INI     NOT     NUMERIC OR
                   LKG-DAT-INI     EQUAL   ZEROS
                   PERFORM         0997-00-ABEND-PARM
           END-IF.

           MOVE    LKG-DAT-INI     TO      WS-DAT-AMD.
           PERFORM 0160-00-CALL-COBBB006.

           IF      LKG-DAT-FIM NOT     NUMERIC OR
                   LKG-DAT-FIM EQUAL   ZEROS
                   PERFORM     0997-00-ABEND-PARM
           END-IF.

           MOVE    LKG-DAT-FIM     TO      WS-DAT-AMD.
           PERFORM 0160-00-CALL-COBBB006.
      *
       0150-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0160-00-CALL-COBBB006       SECTION.
      *****************************************************************
      *
           MOVE   'C'              TO      WRD-CODOPE.

           MOVE    WS-DIA-AMD      TO      WRD-DATA01-DD.
           MOVE    WS-MES-AMD      TO      WRD-DATA01-MM.
           MOVE    WS-ANO-AMD      TO      WRD-DATA01-AA.

           MOVE    00              TO      WRD-CODRET.

           CALL    WS-COBBB006     USING   WRD-GRUPO

           END-CALL.

           IF      WRD-CODRET  NOT EQUAL   00
                   PERFORM     0997-00-ABEND-PARM
           END-IF.
      *
       0160-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0200-00-TESTA-FS-CADMANC    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADMANC NOT EQUAL 00
                   MOVE 'CADMANC'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADMANC
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           PERFORM 1100-00-OPEN-C-TCSTBS61.

           PERFORM 1200-00-FETCH-C-TCSTBS61.

           IF      SQLCODE         EQUAL   +100
                   DISPLAY
                   '******************* GPFPB954 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '* NAO EXISTEM ENTIDADES P/ DELECAO NESTA DATA *'

                   DISPLAY
                   '*                                             *'
           END-IF.

           PERFORM 1400-00-TRATA-C-TCSTBS61
             UNTIL SQLCODE EQUAL +100.

           PERFORM 1300-00-CLOSE-C-TCSTBS61.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-OPEN-C-TCSTBS61     SECTION.
      *****************************************************************
      *
           MOVE    LKG-DAT-INI     TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DB2.
           MOVE    WS-MES-AMD      TO      WS-MES-DB2.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DB2.

           MOVE    WS-DAT-DB2      TO      WS-DAT-INI.

           MOVE    LKG-DAT-FIM     TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DB2.
           MOVE    WS-MES-AMD      TO      WS-MES-DB2.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DB2.

           MOVE    WS-DAT-DB2      TO      WS-DAT-FIM.

           EXEC    SQL
                   OPEN    C-TCSTBS61
           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE ' OPEN '   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  002       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-FETCH-C-TCSTBS61    SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   FETCH   C-TCSTBS61

                   INTO   :CO-ENTDE-SNDCL-S01,
                          :CO-TIPO-ENTDE-S01 ,
                          :DT-VENCIMENTO     ,
                          :QT-GRCS           ,
                          :VR-CONTRIBUICAO

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100
                   MOVE 'FETCH '   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  003       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +000
                   ADD 001         TO      WS-LID-TCSTBS61
           END-IF.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1300-00-CLOSE-C-TCSTBS61    SECTION.
      *****************************************************************
      *
           EXEC    SQL
                   CLOSE   C-TCSTBS61
           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'CLOSE '   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  004       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-TRATA-C-TCSTBS61    SECTION.
      *****************************************************************
      *
           PERFORM 1500-00-GRAVACAO-CADMANC.

           PERFORM 1200-00-FETCH-C-TCSTBS61.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-GRAVACAO-CADMANC    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-MANC.

           MOVE    CO-ENTDE-SNDCL-S01
                                   TO      MANC-CO-ENTID.
           MOVE    CO-TIPO-ENTDE-S01
                                   TO      MANC-TP-ENTID.

           MOVE    DT-VENCIMENTO   TO      WS-DAT-DB2.

           MOVE    WS-DIA-DB2      TO      WS-DIA-AMD.
           MOVE    WS-MES-DB2      TO      WS-MES-AMD.
           MOVE    WS-ANO-DB2      TO      WS-ANO-AMD.

           MOVE    WS-DAT-AMD      TO      MANC-DT-VENCI.

           MOVE    QT-GRCS         TO      MANC-QT-GUIAS.
           MOVE    VR-CONTRIBUICAO TO      MANC-VL-CONTR.

           MOVE   'E'              TO      MANC-TP-MANUT.

           WRITE   REG-CADMANC     FROM    REG-MANC

           END-WRITE.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    009             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FS-CADMANC.

           ADD     001             TO      WS-GRV-CADMANC.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADMANC.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    009             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FS-CADMANC.

           PERFORM 3100-00-MONTA-ESTATISTICA.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3100-00-MONTA-ESTATISTICA   SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB954 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB954 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-TCSTBS61 TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- TCSTBS61: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADMANC  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADMANC.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB954 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0997-00-ABEND-PARM          SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '*** GPFPB954 *** - ABEND PARM'
           DISPLAY '*'
           DISPLAY '* LKG-DAT-INI = ' LKG-DAT-INI
           DISPLAY '* LKG-DAT-FIM = ' LKG-DAT-FIM
           DISPLAY '*'
           DISPLAY '*** GPFPB954 *** - ABEND PARM'

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0997-00-EXIT.
           EXIT.
      *
      *****************************************************************
       0998-00-ABEND-DB2           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           MOVE    SQLCODE         TO      WS-SQLCOD-DB2.

           DISPLAY '******************* GPFPB954 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB954 ******************'.
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
           DISPLAY '******************* GPFPB954 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB954 ******************'.

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

           DISPLAY '******************* GPFPB954 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB954 ******************'.
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
           DISPLAY '******************* GPFPB954 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB954 ******************'.

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB954                  *
      *****************************************************************
