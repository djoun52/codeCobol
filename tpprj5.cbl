       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPPRJ3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MVT  ASSIGN MVT
             FILE STATUS  WS-MVT.
           SELECT RESP  ASSIGN RESP
             FILE STATUS  WS-RESP.
           SELECT STAT  ASSIGN STAT
             FILE STATUS  WS-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  MVT RECORDING F.
       01  EMVT.
           05 MATRICUL       PIC 9(6).
           05 CDE-MVT        PIC 9.
           05 CODE1.
             10 SGN          PIC X.
             10 TAUX         PIC X(2).
             10              PIC X(70).
           05 CODE2 REDEFINES CODE1.
             10 NM-PRNM      PIC X(20).
             10              PIC X(53).
           05 CODE3 REDEFINES CODE1.
             10 ADSS         PIC X(18).
             10 CP           PIC 9(5).
             10 VLL          PIC X(12).
             10              PIC X(38).
           05 CODE4 REDEFINES CODE1.
             10 TPE          PIC X.
             10 PRM          PIC 9(4)V99.
             10              PIC X(66).
           05 CODE5 REDEFINES CODE1.
             10 NM           PIC X(12).
             10 PRNM         PIC X(10).
             10 TEL-FIX      PIC 9(10).
             10 TEL-PTB      PIC 9(10).
             10 SLR          PIC S9(5)V99 COMP-3.
             10 QLFCT        PIC X(19).
             10 DT-EMB.     
               15 JJ         PIC 99.
               15 MM         PIC 99.
               15 AA         PIC 9999.
       FD  RESP RECORDING F.
       01  ERESP              PIC X(80).  
       FD  STAT RECORDING F.
       01  ESTAT             PIC X(80).  
       WORKING-STORAGE SECTION.
       77  WS-MVT         PIC XX.
       77  WS-RESP        PIC XX.
       77  WS-STAT        PIC XX.
       77  DISP-PRM       PIC 9(4)V,99.
       77  ERR            PIC 9.
       77  LGN-DEB1       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB2       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB3       PIC X(80) VALUE ALL SPACE.
       77  INTERLIGNE     PIC X(80) VALUE ALL '-'.
       77  LGN-DISP       PIC X(80) VALUE ALL SPACE. 
       77  SOUSPROG1      PIC X(8) VALUE 'SPDATE'.
       77  SOUSPROG2      PIC X(8) VALUE 'SHOURS'.
       77  DAT            PIC X(30).
       77  CODEDG         PIC X VALUE 'D'.
       77  CODECL         PIC X VALUE 'L'.
       01  TIM            PIC X(8).
       01  CPT-L          PIC 999  VALUE 00 COMP-3.
       01  NB-SUCC        PIC 999  VALUE 00 COMP-3.
       01  NB-ERR         PIC 999  VALUE 00 COMP-3.
       01  CPT-E1         PIC 999  VALUE 00 COMP-3.
       01  CPT-E2         PIC 999  VALUE 00 COMP-3.
       01  CPT-E3         PIC 999  VALUE 00 COMP-3.
       01  CPT-E4         PIC 999  VALUE 00 COMP-3.
       01  CPT-E5         PIC 999  VALUE 00 COMP-3.
       01  CPT-E5-RESV    PIC 999  VALUE 00 COMP-3.
       01  CPT-EMI        PIC 999  VALUE 00 COMP-3.
       01  CPT-ECI        PIC 999  VALUE 00.
       01  DIS-RES        PIC ZZ9.
       01  DIS-SLR        PIC ZZBZZ9V,99.
       01  TAB-PRM.
           05 PRM-OK  OCCURS 10 INDEXED BY IND-1 PIC X.
       PROCEDURE DIVISION.
           OPEN INPUT MVT
           OPEN OUTPUT RESP
           OPEN OUTPUT STAT
           PERFORM TEST-STAT-MVT
           PERFORM TEST-STAT-STAT
           PERFORM CREA-DAT-TIME
           MOVE '6' TO PRM-OK(1)
           MOVE '2' TO PRM-OK(2)
           MOVE '7' TO PRM-OK(3)
           PERFORM TRT-RESP
           PERFORM CREA-TABL-STAT
           CLOSE RESP
           CLOSE STAT
           CLOSE MVT
           PERFORM TEST-STAT-RESP
           PERFORM TEST-STAT-STAT
           PERFORM TEST-STAT-MVT
           GOBACK
           .
       CREA-DAT-TIME.
           CALL SOUSPROG1 USING DAT CODEDG CODECL
           CALL SOUSPROG2 USING TIM 
           MOVE 'API3' TO LGN-DEB1(1:4) LGN-DEB3(1:4)
           MOVE 'LISTE DE CONTROLE DU FICHIER MVT' TO LGN-DEB1(14:32)
           MOVE 'STATISTIQUE SUR CONTROLE DU FICHIER MOUVEMENT' TO 
                LGN-DEB3(8:45)
           MOVE DAT TO LGN-DEB1(51:30) LGN-DEB3(51:30)
           MOVE TIM TO LGN-DEB2(73:8)
           .

       TRT-RESP.
           READ MVT
           STRING LGN-DEB1 DELIMITED BY SIZE INTO ERESP
           PERFORM ECRIRE-RESP
           STRING LGN-DEB2 DELIMITED BY SIZE INTO ERESP
           PERFORM ECRIRE-RESP
           PERFORM ECRIRE-RESP
           PERFORM UNTIL WS-MVT = '10'
             ADD 1 TO CPT-L
             MOVE 0 TO ERR
             IF MATRICUL IS NOT NUMERIC
               ADD 1 TO CPT-EMI
               STRING EMVT(1:43) ' 1. MATRICULE NON NUMERIQUE'
                   DELIMITED BY SIZE INTO ERESP
               PERFORM ECRIRE-RESP
             END-IF
             EVALUATE CDE-MVT
              WHEN '1'
                IF SGN NOT = '+' AND SGN NOT = '-' THEN
                   ADD 1 TO CPT-E1
                   STRING EMVT(1:43) ' 3. SIGNE DIFFERENT DE + OU -'
                   DELIMITED BY SIZE INTO ERESP
                   PERFORM ECRIRE-RESP
                 END-IF
                IF TAUX IS NOT NUMERIC THEN
                  MOVE 1 TO ERR
                  ADD 1 TO CPT-E1
                  STRING EMVT(1:43) ' 4. TAUX NON NUMERIQUE'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                END-IF
                IF ERR = 0
                  ADD 1 TO NB-SUCC
                  STRING EMVT(1:43) ' MOUVEMENT CORRECT'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                END-IF
              WHEN '2'
                IF NM-PRNM = SPACE
                  ADD 1 TO CPT-E2
                  STRING EMVT(1:43) ' 5. LE NOM NEST PAS RENSEIGNE'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                ELSE
                  ADD 1 TO NB-SUCC
                  STRING EMVT(1:43) ' MOUVEMENT CORRECT'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                END-IF
              WHEN '3'
                IF ADSS = SPACE AND CP IS NOT NUMERIC
                     AND VLL = SPACE
                  MOVE 1 TO ERR
                  ADD 1 TO CPT-E3
                  STRING EMVT(1:43) ' 6. MINIMUM SAISI :'
                           ' ADRESSE-CP-VILLE'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                ELSE
                  IF CP IS NOT NUMERIC AND CP NOT = SPACE
                    MOVE 1 TO ERR
                    ADD 1 TO CPT-E3
                    STRING EMVT(1:43) ' 7. CODE POSTAL INVALIDE'
                         DELIMITED BY SIZE INTO ERESP
                    PERFORM ECRIRE-RESP
                  ELSE
                    ADD 1 TO NB-SUCC
                    STRING EMVT(1:43) ' MOUVEMENT CORRECT'
                         DELIMITED BY SIZE INTO ERESP
                    PERFORM ECRIRE-RESP
                  END-IF
                END-IF
              WHEN '4'
                SET IND-1 TO 1
                SEARCH PRM-OK 
                  AT END
                    MOVE 1 TO ERR
                    ADD 1 TO CPT-E4
                    STRING EMVT(1:43) ' 8. TYPE DE PRIME INVALIDE'
                         DELIMITED BY SIZE INTO ERESP
                    PERFORM ECRIRE-RESP
                  WHEN PRM-OK(IND-1) = TPE 
                    MOVE 0 TO ERR
                END-SEARCH
                IF PRM IS NOT NUMERIC
                  MOVE 1 TO ERR
                  ADD 1 TO CPT-E4
                  STRING EMVT(1:43) ' 9. PRIME NON NUMERIQUE'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                END-IF
                IF ERR NOT = 1 THEN
                  ADD 1 TO NB-SUCC
                  STRING EMVT(1:43) ' MOUVEMENT CORRECT'
                         DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP
                END-IF
              WHEN '5'
                MOVE CPT-E5 TO CPT-E5-RESV
                MOVE SLR TO DIS-SLR
                DISPLAY  MATRICUL ' ' NM ' ' DIS-SLR
                IF TEL-FIX(1:1) NOT = 0 OR TEL-FIX(2:1) > 5 OR
                     TEL-FIX(2:1) < 1
                  ADD 1 TO CPT-E5
                  STRING EMVT(1:43) ' 19. TELEPHONE FIXE INVALIDE'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP                
                END-IF
                 IF TEL-PTB(1:2) NOT = 06 AND TEL-PTB(1:2) NOT = 07
                  ADD 1 TO CPT-E5
                  STRING EMVT(1:43) ' 20. TELEPHONE PORTABLE INVALIDE'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP                
                END-IF
                IF SLR IS NOT NUMERIC
                  ADD 1 TO CPT-E5
                  STRING EMVT(1:43) ' 21. SALAIRE NON NUMRIQUE'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP 
                END-IF
                IF DT-EMB IS NOT NUMERIC
                  ADD 1 TO CPT-E5
                  STRING EMVT(1:43) ' 22. DATE NON NUMERIQUE'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP 
                END-IF
                IF JJ < 01 OR JJ >  31
                  ADD 1 TO CPT-E5
                  STRING EMVT(1:43) ' 23. JOUR EMBAUCHE INVALIDE'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP 
                END-IF
                IF MM < 01 OR MM > 12
                  ADD 1 TO CPT-E5
                  STRING EMVT(1:43) ' 24. MOIS EMBAUCHE INVALIDE'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP 
                END-IF
                IF CPT-E5-RESV = CPT-E5
                  ADD 1 TO NB-SUCC 
                  STRING EMVT(1:43) ' MOUVEMENT CORRECT'
                          DELIMITED BY SIZE INTO ERESP
                  PERFORM ECRIRE-RESP    
                END-IF
              WHEN OTHER
                ADD 1 TO CPT-ECI
                STRING EMVT(1:43) ' 2. CODE MOUVEMENT INCONNU'
                         DELIMITED BY SIZE INTO ERESP
               PERFORM ECRIRE-RESP
             END-EVALUATE
             STRING INTERLIGNE DELIMITED BY SIZE INTO ERESP
             PERFORM ECRIRE-RESP
             READ MVT
           END-PERFORM
           .        
       CREA-TABL-STAT.
           STRING LGN-DEB3 DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           STRING LGN-DEB2 DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           PERFORM ECRIRE-STAT
           MOVE CPT-L TO DIS-RES.
           STRING '10. NOMBRE D''ENREGISTREMENTS LUS     ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           SUBTRACT NB-SUCC FROM CPT-L GIVING NB-ERR
           MOVE NB-ERR TO DIS-RES.
           STRING '11. NOMBRE D''ENREGISTREMENTS ERRONES ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-E1 TO DIS-RES.
           STRING '    12. NOMBRE D''ERREUR DE TYPE 1    ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-E2 TO DIS-RES.
           STRING '    13. NOMBRE D''ERREUR DE TYPE 2    ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-E3 TO DIS-RES.
           STRING '    14. NOMBRE D''ERREUR DE TYPE 3    ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-E4 TO DIS-RES.
           STRING '    15. NOMBRE D''ERREUR DE TYPE 4    ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-E5 TO DIS-RES.
           STRING '    15. NOMBRE D''ERREUR DE TYPE 5    ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-EMI TO DIS-RES.
           STRING '    16. NOMBRE D''ERREUR MAT INVALIDE ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           MOVE CPT-ECI TO DIS-RES.
           STRING '    17. NOMBRE DE CODES INVALIDES    ' DIS-RES
                  DELIMITED BY SIZE INTO ESTAT
           PERFORM ECRIRE-STAT
           .
       TEST-STAT-MVT.
           IF WS-MVT NOT = '00'
             DISPLAY 'ERREUR FICHIER MOUVEMENT ' WS-MVT
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
       TEST-STAT-RESP.
           IF WS-RESP NOT = '00'
             DISPLAY 'ERREUR FICHIER ANNOMALIE ' WS-RESP
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
       TEST-STAT-STAT.
           IF WS-STAT NOT = '00'
             DISPLAY 'ERREUR FICHIER STAT ' WS-STAT
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .   
       ECRIRE-RESP.
           WRITE ERESP
           MOVE ALL SPACE TO ERESP
           .
       ECRIRE-STAT.
           WRITE ESTAT
           MOVE ALL SPACE TO ESTAT
           .
     
      