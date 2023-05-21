       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPPRJ7.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DAT.
          05 AA          PIC X(4).
          05 MM          PIC XX.
          05 JJ          PIC XX.
       01 AA9            PIC 9(4).
       01 MM9            PIC 99.
       01 JJ9            PIC 99.
       01 DIFFYEAR       PIC 9(4).
       01 RESULDIV       PIC 9(4).
       01 MOD            PIC 99.
       01 WS-STATE-CHECK   PIC 9 VALUE 0.
       01  MESSAGES.
         05 PIC X(13)  VALUE 'DATE VALIDE  '.
         05 PIC X(13)  VALUE 'MOIS INVALIDE'.
         05 PIC X(13)  VALUE 'JOUR INVALIDE'.
       01 REDEFINES MESSAGES.
         05  OCCURS 3.
           10 MESS  PIC X(13).
       01  IND-MESS PIC 9.
       77 CHECK-ERR    PIC X VALUE 'F'.
       77 FIN-BOUCLE   PIC X VALUE 'F'.
       PROCEDURE DIVISION.
           ACCEPT DAT
           PERFORM UNTIL FIN-BOUCLE = 'T'
             MOVE AA TO AA9
             MOVE MM TO MM9
             MOVE JJ TO JJ9
             MOVE 'F' TO CHECK-ERR
             MOVE 1 TO IND-MESS 
             PERFORM CHECK-END
             IF FIN-BOUCLE = 'F'
               DISPLAY AA '/' MM '/' JJ
               IF CHECK-ERR = 'F' 
                 PERFORM CHECK-MONTH
                 IF CHECK-ERR = 'F' 
                   PERFORM CHECK-DAY
                 END-IF
               END-IF
               ACCEPT DAT
               DISPLAY MESS(IND-MESS)
               DISPLAY '---------------'
             END-IF
           END-PERFORM
           GOBACK
           .
       CHECK-END.
           IF AA = '0000' AND MM = '00'
              DISPLAY 'FIN PROG'
              MOVE 'T' TO FIN-BOUCLE
           END-IF
           .
       CHECK-MONTH.
           IF MM9 > 12 OR MM9 < 01
              MOVE 2 TO IND-MESS 
              MOVE 'T' TO CHECK-ERR
           END-IF
           .

       CHECK-DAY.
           IF JJ9 < 01 OR > 31
             MOVE 3 TO IND-MESS 
             MOVE 'T' TO CHECK-ERR
           END-IF
           IF MM9 = 02 AND JJ9 > 29 AND CHECK-ERR = 'F' 
             MOVE 3 TO IND-MESS
             MOVE 'T' TO CHECK-ERR
           END-IF
           IF MM9 = 02 AND JJ9 = 29 AND CHECK-ERR = 'F' 
             PERFORM CHECK-LY
           END-IF
           IF WS-STATE-CHECK = 0 AND CHECK-ERR = 'F' 
             EVALUATE MM9
               WHEN 04
               WHEN 06
               WHEN 09
               WHEN 11
                 IF JJ9 > 30 
                   MOVE 3 TO IND-MESS
                   MOVE 'T' TO CHECK-ERR
                 END-IF
               WHEN 02
                 IF JJ9 > 29
                   MOVE 3 TO IND-MESS
                   MOVE 'T' TO CHECK-ERR
                 END-IF
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF
           .
      * VERIFICATION ANNEE BISSEXTILE
       CHECK-LY.
           IF AA9 < 2000
              SUBTRACT AA9 FROM 2000 GIVING DIFFYEAR
           ELSE 
              SUBTRACT 2000 FROM AA9 GIVING DIFFYEAR
           END-IF
           IF DIFFYEAR < 4 AND NOT = 0
             MOVE 3 TO IND-MESS
             MOVE 'T' TO CHECK-ERR
           ELSE 
             DIVIDE DIFFYEAR BY 4 GIVING RESULDIV REMAINDER MOD 
             IF MOD NOT = 0
               MOVE 3 TO IND-MESS
               MOVE 'T' TO CHECK-ERR
             END-IF
           END-IF

           .