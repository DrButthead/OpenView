	PROGRAM VAXDRIV
C********************************************************************
C  THIS IS A SAMPLE PROGRAM WHICH READS AND DECOMPRESSES VOYAGER
C  IMAGES AND WRITES THEM OUT IN PDS LABELLED FORMAT.  IT ALSO
C  MODIFIES THE PDS LABELS TO REFLECT THE CONVERSION FROM VARIABLE
C  TO FIXED RECORD FORMAT.  IT USES THE SUBROUTINES IN DECOMP.FOR
C  TO PERFORM THE DECOMPRESSION.  TWO VERSIONS OF THE DRIVER EXIST, 
C  ONE WHICH RUNS ON THE IBM PC USING MICROSOFT FORTRAN, VERSION 4.XX,
C  AND ONE WHICH RUNS UNDER VAX/VMS FORTRAN.  THE TWO VERSIONS ARE 
C  IDENTICAL EXCEPT FOR THE FILE OPEN STATEMENTS AND VARIABLE
C  LENGTH RECORD I/O (READ STATEMENTS).
C
C_HIST
C  JUL88 PC AND VAX VERSIONS BY MIKE MARTIN 1988/07/30, WITH 
C  ASSISTANCE FROM ROGER BOWEN, WHO CODED THE FIRST PC VERSIONS
C  OF THESE ROUTINES.
C
C  INPUTS   - INPUT FILE TO BE DECOMPRESSED.
C
C  OUTPUTS  - OUTPUT FILE CONTAINING DECOMPRESSED IMAGE.
C
C  TO COMPILE AND LINK UNDER MICROSOFT FORTRAN USE THE COMMAND:
C
C    FL /FPi PCDRIV.FOR DECOMP.FOR
C
C  TO COMPILE AND LINK USING VAX/VMS FORTRAN USE THE COMMANDS:
C
C    FOR  VAXDRIV,DECOMP
C    LINK VAXDRIV,DECOMP  
C_END
C_VARS
	CHARACTER  NAME*80, INAME*80, LABSTRING*80, OUTSTRING*2508,
     1		   IBUF(2048), OBUF(2508),TEMPSTRING*80
        CHARACTER CR,LF,BLANK
        INTEGER*2 TOTAL_BYTES,LINE,I,J,NLEN
	INTEGER*4 HIST(512),HISTIN(209)
        INTEGER*4 LEN,NS
	EQUIVALENCE (IBUF,LABSTRING,HISTIN), (OBUF,OUTSTRING)
C********************************************************************
C
C INITIALIZE SOME CONSTANTS
C
C********************************************************************
        CR    = CHAR(13)
        LF    = CHAR(10)
        BLANK = CHAR(32)
        NS    = 836
C********************************************************************
C
C GET INPUT AND OUTPUT FILE NAMES AND OPEN THE FILES
C
C********************************************************************
	WRITE (*,1000)
1000	FORMAT(' ENTER NAME OF FILE TO BE DECOMPRESSED: ')
1020	FORMAT(A)
	READ  (*,1020) INAME
        WRITE (*,1010)
1010	FORMAT(' ENTER NAME OF UNCOMPRESSED OUTPUT FILE:')
        READ  (*,1020) NAME
	OPEN  (10,FILE=INAME,FORM='FORMATTED',STATUS='OLD',READONLY)
	OPEN  (11,FILE=NAME, FORM='UNFORMATTED',STATUS='NEW',
     1            RECORDTYPE='FIXED',RECORDSIZE=209)
C********************************************************************
C
C READ AND PROCESS THE COMPRESSED FILE LABELS.  
C 
C ALL THE LABELS ARE CONCATINATED INTO AN ARRAY, TO ALLOW THE 50-ODD 
C LABEL LINES TO BE WRITTEN OUT AS 3-FIXED-LENGTH RECORDS ON THE VAX.
C
C********************************************************************
        TOTAL_BYTES = 0
  101   FORMAT(Q,A)
  100   READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
C********************************************************************
C
C EDIT THE PDS LABELS WHICH HAVE TO BE CHANGED.
C
C********************************************************************
C CHANGE THE LENGTH FIELD OF THE SFDU LABEL
C********************************************************************
        I = INDEX(LABSTRING,'NJPL1I00PDS1')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:12) // '00673816' // 
     1                 LABSTRING(21:NLEN)
          OUTSTRING = TEMPSTRING(1:NLEN) // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C CHANGE THE RECORD TYPE FROM VARIABLE TO FIXED
C********************************************************************
        I = INDEX(LABSTRING,'RECORD_TYPE')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:35) // 'FIXED_LENGTH'
          NLEN = NLEN-3
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // TEMPSTRING(1:NLEN) 
     1                // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C CHANGE THE FILE RECORD COUNT TO REFLECT THE FIXED STRUCTURE
C********************************************************************
        I = INDEX(LABSTRING,'FILE_RECORDS')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:35) // '806'
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // TEMPSTRING(1:NLEN) 
     1                // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C CHANGE THE COUNT OF LABEL RECORDS TO 3
C********************************************************************
        I = INDEX(LABSTRING,'LABEL_RECORDS')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:35) // '3'
          NLEN = NLEN -1
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // TEMPSTRING(1:NLEN) 
     1                // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C CHANGE THE LOCATION POINTER OF THE HISTOGRAM TO RECORD 4
C********************************************************************
        I = INDEX(LABSTRING,'^IMAGE_HISTOGRAM')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:35) // '4'
          NLEN = NLEN -1
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // TEMPSTRING(1:NLEN) 
     1                // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C DELETE THE ENCODING HISTOGRAM LOCATION POINTER
C********************************************************************
        I = INDEX(LABSTRING,'^ENCODING_HISTOGRAM')
        IF (I .EQ. 1) GOTO 100
C********************************************************************
C CHANGE THE LOCATION POINTER OF THE ENGINEERING TABLE TO RECORD 6
C********************************************************************
        I = INDEX(LABSTRING,'^ENGINEERING_TABLE')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:35) // '6'
          NLEN = NLEN -1
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // TEMPSTRING(1:NLEN) 
     1                // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C CHANGE THE LOCATION POINTER OF THE IMAGE TO RECORD 7
C********************************************************************
        I = INDEX(LABSTRING,'^IMAGE')
        IF (I .EQ. 1) THEN 
          TEMPSTRING = LABSTRING(1:35) // '7'
          NLEN = NLEN -1
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // TEMPSTRING(1:NLEN) 
     1                // CR // LF
          TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
          GOTO 100
        ENDIF
C********************************************************************
C DELETE THE ENCODING HISTOGRAM OBJECT DEFINITION
C********************************************************************
        I = INDEX(LABSTRING,
     1            'OBJECT                           = ENCODING_')
        IF (I .EQ. 1) THEN 
          READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
          READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
          READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
          READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
          GOTO 100
        ENDIF
C********************************************************************
C DELETE THE ENCODING TYPE KEYWORD IN THE IMAGE OBJECT DEFINITION
C********************************************************************
        I = INDEX(LABSTRING,' ENCODING')
        IF (I .EQ. 1) GOTO 100
C********************************************************************
C IF WE GET HERE JUST WRITE OUT THE LABEL
C********************************************************************
          OUTSTRING = OUTSTRING(1:TOTAL_BYTES) // LABSTRING(1:NLEN) 
     1                // CR // LF
        TOTAL_BYTES = TOTAL_BYTES + NLEN + 2
        I= INDEX(LABSTRING,'END') 
        IF (I .EQ. 1 .AND. NLEN .EQ. 3) GOTO 300
        GOTO 100
C********************************************************************
C PAD OUT LABELS TO MULTIPLE OF 836
C********************************************************************
300     DO 310 I=TOTAL_BYTES+1,2508
310     OBUF(I) =  BLANK
C********************************************************************
C NOW WRITE OUT THE LABEL RECORDS IN 3-WRITES, FILLING OUT THE THIRD
C RECORD TO 836 BYTES WITH BLANKS.
C********************************************************************
        WRITE(11) (OBUF(I), I=   1,  836)
        WRITE(11) (OBUF(I), I= 837, 1672)
        WRITE(11) (OBUF(I), I=1673, 2508)
C********************************************************************
C
C READ AND WRITE THE IMAGE HISTOGRAM AS TWO RECORDS, FILLING OUT THE
C SECOND RECORD TO 836 BYTES WITH BLANKS.
C
C********************************************************************
        DO 320 J=1,2
        READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
        IF (NLEN .EQ. 836) WRITE(11) (IBUF(I), I=1, NLEN)
320     CONTINUE
        DO 330 I=NLEN+1,836
330     IBUF(I) =  BLANK
        WRITE(11) (IBUF(I), I=1, 836)
C********************************************************************
C
C READ THE ENCODING HISTOGRAM, AND LOAD THE HIST ARRAY FOR USE BY
C THE DECOMPRESSION SUBROUTINES.
C
C********************************************************************
        READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
        DO 340 I=1,209
340       HIST(I) = HISTIN(I)
        READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
        DO 350 I=1,209
350       HIST(I+209) = HISTIN(I)
        READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
        DO 360 I=1,93
360       HIST(I+418) = HISTIN(I)
C********************************************************************
C
C READ AND WRITE THE ENGINEERING SUMMARY AS ONE RECORD, FILLING OUT 
C THE RECORD TO 836 BYTES WITH BLANKS.
C
C********************************************************************
        READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
        DO 370 I=NLEN+1,836
370     IBUF(I) = BLANK
        WRITE(11) (IBUF(I), I=1, 836)
C********************************************************************
C
C INITIALIZE THE DECOMPRESSION.
C
C********************************************************************
	WRITE(*,*) 'INITIALIZING DECOMPRESSION ROUTINE...'
	CALL DECMPINIT(HIST)
C********************************************************************
C
C PERFORM THE DECOMPRESSION.
C
C********************************************************************
	WRITE(*,*) 'DECOMPRESSING DATA...'
        LINE=0
400	READ(10,101,END=500) NLEN, LABSTRING(1:NLEN)
		LINE = LINE + 1
                LEN = NLEN
                CALL DECOMPRESS(IBUF, OBUF, LEN, NS)
		WRITE(11) (OBUF(I), I=1, NS)
                J = MOD(LINE,100)
                IF (J .EQ. 0) WRITE (*,'(I5,A6)') LINE,' LINES'
                IF (LINE .EQ. 800) GOTO 500
                GO TO 400
C********************************************************************
C
C DONE.  CLOSE FILES AND GET OUT OF HERE.
C
C********************************************************************
500	CLOSE(10)
	CLOSE(11)
	END
