       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPPRJ2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSUR  ASSIGN ASSUR
             FILE STATUS  WS-ASR.
           SELECT LST  ASSIGN LST
             FILE STATUS  WS-LST.
       DATA DIVISION.
       FILE SECTION.
       FD  ASSUR RECORDING F.
       01  EASR.
           05 MATRICUL       PIC 9(6).
           05 NOM-CLIENT     PIC X(20).
           05 ASUR-ADRS      PIC X(18).
           05 ASUR-CP        PIC 9(5).
           05 ASUR-VLL       PIC X(12).
           05 ASUR-TP-VHCL   PIC X.
           05 ASUR-PRM-BS    PIC 9(4)V99.
           05 ASUR-BM        PIC X.
           05 ASUR-TAUX      PIC 99.
           05                PIC X(9).
       FD  LST RECORDING F.
       01  ELST              PIC X(80).
       WORKING-STORAGE SECTION.
       77  WS-ASR         PIC XX.
       77  WS-LST         PIC XX.
       77  SOUSPROG       PIC X(8) VALUE 'SPDATE'.
       77  DAT            PIC X(30).
       77  CODEDG         PIC X VALUE 'D'.
       77  CODECL         PIC X VALUE 'L'.
       77  DISP-TAUX      PIC Z9.
       77  DISP-PRM-BS    PIC ZB999V,99.
       01  TRTM-PRIM      PIC 9(4)V99  COMP-3.
       77  DISP-PRIM      PIC ZB999V,99.
       77  PCT-TAUX       PIC 9V99.


       PROCEDURE DIVISION.
           OPEN INPUT ASSUR
           OPEN OUTPUT LST
           PERFORM TEST-STAT-ASR
           PERFORM TEST-STAT-LST
           CALL SOUSPROG USING DAT CODEDG CODECL
           READ ASSUR
           PERFORM UNTIL WS-ASR = '10'
             MOVE ASUR-TAUX TO DISP-TAUX
             MOVE ASUR-PRM-BS TO DISP-PRM-BS
             MOVE ALL SPACE TO ELST
             STRING 'QUITTANCE DE PRIME            ' DAT
                    DELIMITED BY SIZE INTO ELST
             WRITE ELST
             MOVE ALL SPACE TO ELST
             STRING '                    ***  ' NOM-CLIENT '  ***'
                    DELIMITED BY SIZE INTO ELST
             WRITE ELST
             MOVE ALL SPACE TO ELST

             STRING '                    ***  ' ASUR-ADRS '    ***'
                    DELIMITED BY SIZE INTO ELST
             WRITE ELST
             MOVE ALL SPACE TO ELST
             STRING '                    ***  ' ASUR-CP '/' ASUR-VLL
                    '    ***'
                    DELIMITED BY SIZE INTO ELST
             WRITE ELST
             MOVE ALL SPACE TO ELST
             WRITE ELST
             STRING 'PRIME DE BASE       ' DISP-PRM-BS
                     DELIMITED BY SIZE INTO ELST
             WRITE ELST
             MOVE ALL SPACE TO ELST
             EVALUATE ASUR-BM
               WHEN 'B'
                    DIVIDE ASUR-TAUX BY 100 GIVING PCT-TAUX
                    MULTIPLY ASUR-PRM-BS BY PCT-TAUX GIVING TRTM-PRIM
                    MOVE TRTM-PRIM TO DISP-PRIM
                    MOVE ALL SPACE TO ELST
                    STRING 'DEGREVEMENT        -' DISP-PRIM
                           '     BONUS   ' DISP-TAUX '%'
                           DELIMITED BY SIZE INTO ELST
                    WRITE ELST
                    MOVE ALL SPACE TO ELST
                    STRING '                   ---------'
                           DELIMITED BY SIZE INTO ELST
                    WRITE ELST
                    MOVE ALL SPACE TO ELST
                    SUBTRACT TRTM-PRIM FROM ASUR-PRM-BS GIVING TRTM-PRIM
                    MOVE TRTM-PRIM TO DISP-PRIM
                    STRING 'TOTAL A PAYER       '   DISP-PRIM
                           DELIMITED BY SIZE INTO ELST
                    WRITE ELST
                    MOVE ALL SPACE TO ELST
               WHEN 'M'
                    DIVIDE ASUR-TAUX BY 100 GIVING PCT-TAUX
                    MULTIPLY ASUR-PRM-BS BY PCT-TAUX GIVING TRTM-PRIM
                    MOVE TRTM-PRIM TO DISP-PRIM
                    STRING 'MAJORATION         +' DISP-PRIM
                           '     MALUS   ' DISP-TAUX '%'
                           DELIMITED BY SIZE INTO ELST
                    WRITE ELST
                    MOVE ALL SPACE TO ELST
                    STRING '                   ---------'
                           DELIMITED BY SIZE INTO ELST
                    WRITE ELST
                    MOVE ALL SPACE TO ELST
                    ADD TRTM-PRIM TO ASUR-PRM-BS GIVING TRTM-PRIM
                    MOVE TRTM-PRIM TO DISP-PRIM
                    STRING 'TOTAL A PAYER       ' DISP-PRIM
                           DELIMITED BY SIZE INTO ELST
                    WRITE ELST
                    MOVE ALL SPACE TO ELST
               WHEN OTHER
                    DISPLAY "ERREUR CODE BONUS MALUS INCORRECT"
             END-EVALUATE
             MOVE ALL SPACE TO ELST
             WRITE ELST
             READ ASSUR
           END-PERFORM
           CLOSE LST
           CLOSE ASSUR
           PERFORM TEST-STAT-LST
           PERFORM TEST-STAT-ASR
           GOBACK.
       TEST-STAT-ASR.
           IF WS-ASR NOT = '00'
             DISPLAY 'ERREUR FICHIER ASSURE ' WS-ASR
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
       TEST-STAT-LST.
           IF WS-LST NOT = '00'
             DISPLAY 'ERREUR FICHIER LISTE ' WS-LST
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
