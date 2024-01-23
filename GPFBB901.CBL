      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFBB901.
       AUTHOR.                     ULISSES & MORAES - TI.
       DATE-WRITTEN.               26/01/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES & MORAES - TI                          *
      * LINGUAGEM....: COBOL/BATCH/DB2                                *
      * PROGRAMADOR..: ULISSES & MORAES - TI                          *
      * DATA.........: 26/01/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A  PARTIR  DA  DATA/HORA/TIMESTAMP DO DB2      *
      *                FORMATAR A LINKAGE.                            *
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
       01      WS-DAT-DB2          PIC     X(010) VALUE '99.99.9999'.
       01      FILLER              REDEFINES      WS-DAT-DB2.
         03    WS-DIA-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MES-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-ANO-DB2          PIC     9(004).
      *
       01      WS-HMS-DB2          PIC     X(008) VALUE '99:99:99'.
       01      FILLER              REDEFINES      WS-HMS-DB2.
         03    WS-HOR-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MIN-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-SEG-DB2          PIC     9(002).
      *
       01      WS-TIMESTAMP        PIC     X(026) VALUE ZEROS.
      *
       01      WS-DAT-AMD          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-AMD.
         03    WS-ANO-AMD          PIC     9(004).
         03    WS-MES-AMD          PIC     9(002).
         03    WS-DIA-AMD          PIC     9(002).
      *
       01      WS-DAT-DMA          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-DMA.
         03    WS-DIA-DMA          PIC     9(002).
         03    WS-MES-DMA          PIC     9(002).
         03    WS-ANO-DMA          PIC     9(004).
      *
      *****************************************************************
      *        AREA DE TRATAMENTO DB2                                 *
      *****************************************************************
      *
           EXEC    SQL
                   INCLUDE SQLCA
           END-EXEC.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
      *
           COPY    COBI901.
      *
      *****************************************************************
       PROCEDURE    DIVISION    USING    W901-REG.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS.

           PERFORM 3000-00-PROCED-FINAIS.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           MOVE    00              TO      W901-COD-RET.

           MOVE    SPACES          TO      W901-TIM-DB2.
           MOVE    SPACES          TO      W901-DAT-DB2.
           MOVE    SPACES          TO      W901-DAT-EDI.

           MOVE    ZEROS           TO      W901-DAT-AMD.
           MOVE    ZEROS           TO      W901-DAT-DMA.

           MOVE    SPACES          TO      W901-HOR-DB2.
           MOVE    SPACES          TO      W901-HOR-EDI.

           EXEC    SQL
                   SET :WS-DAT-DB2 = CURRENT DATE
           END-EXEC.

           EXEC    SQL
                   SET :WS-HMS-DB2 = CURRENT TIME
           END-EXEC.

           EXEC    SQL
                   SET :WS-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           MOVE    WS-TIMESTAMP    TO      W901-TIM-DB2.

           MOVE    WS-DAT-DB2      TO      W901-DAT-DB2.

           MOVE    WS-DAT-DB2      TO      W901-DAT-EDI.

           INSPECT W901-DAT-EDI    REPLACING ALL '.' BY '/'.

           MOVE    WS-DIA-DB2      TO      WS-DIA-AMD.
           MOVE    WS-MES-DB2      TO      WS-MES-AMD.
           MOVE    WS-ANO-DB2      TO      WS-ANO-AMD.

           MOVE    WS-DAT-AMD      TO      W901-DAT-AMD.

           MOVE    WS-DIA-DB2      TO      WS-DIA-DMA.
           MOVE    WS-MES-DB2      TO      WS-MES-DMA.
           MOVE    WS-ANO-DB2      TO      WS-ANO-DMA.

           MOVE    WS-DAT-DMA      TO      W901-DAT-DMA.

           MOVE    WS-HMS-DB2      TO      W901-HOR-DB2.

           MOVE    WS-HMS-DB2      TO      W901-HOR-EDI.

           INSPECT W901-HOR-EDI    REPLACING ALL '.' BY ':'.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           GOBACK.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFBB901                  *
      *****************************************************************
