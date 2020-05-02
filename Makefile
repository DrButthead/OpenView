CC=gcc
CC_FLAGS=-m32
RM=rm -f

SRCS_CDCOMP=archive/software/cdcomp_patched.c
TARG_CDCOMP=cdcomp

SRCS_DETEST=archive/software/detest.c archive/software/decomp.c
TARG_DETEST=detest

all: cdcomp detest

cdcomp: $(SRCS_CDCOMP)
	$(CC) $(CC_FLAGS) -o $(TARG_CDCOMP) $(SRCS_CDCOMP)

detest: $(SRCS_DETEST)
	$(CC) $(CC_FLAGS) -o $(TARG_DETEST) $(SRCS_DETEST)

clean:
	$(RM) $(TARG_CDCOMP)
	$(RM) $(TARG_DETEST)
