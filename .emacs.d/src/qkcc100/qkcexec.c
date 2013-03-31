/*
 *    qkcexec.c
 *
 *    Copyright (C) 1992 by K.Sato
 *    All rights reserved.
 */

#include "qkc.h"

#ifdef MSDOS

#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <string.h>

#define CMDLINE_BUF_SIZE 256

int execpascalver = 1;

extern int convmode;
extern int inputcode;
extern int newlineconvmode;
extern int forcenewlineconv;
extern int outtostdout;
extern int infromstdin;
extern int _7bitmode;
extern int removectrlz;
extern int usetempfile;
extern int quietmode;
extern int emulationmode;
extern unsigned char esc_k[];
extern unsigned char esc_a[];

/* generate option string */
void genoptionstr(s)
char *s;
{
    char *p;

    strcpy(s, "/C ");
    if (convmode == MODE_SJIS)
        p = "/S ";
    else if (convmode == MODE_EUC)
        p = "/E ";
    else
        if (esc_k[0] == '$')
            if (esc_k[1] == 'B')
                if (esc_a[1] == 'J')
                    p = "/J ";
                else
                    p = "/BJ ";
            else
                p = "/OJ ";
        else
            p = "/NJ ";
    strcat(s, p);
    switch (inputcode) {
    case INPUT_SJIS:
        p = "/IS ";
        break;
    case INPUT_EUC:
        p = "/IE ";
        break;
    case INPUT_JIS:
        p = "/IJ ";
        break;
    default:
        p = "";
    }
    strcat(s, p);
    switch (newlineconvmode) {
    case NEWLINE_CRLF:
        p = "/M ";
        break;
    case NEWLINE_LF:
        p = "/U ";
        break;
    case NEWLINE_CR:
        p = "/MA ";
        break;
    default:
        p = "";
    }
    strcat(s, p);
    if (forcenewlineconv)
        strcat(s, "/L ");
    if (outtostdout)
        strcat(s, "/O ");
    if (infromstdin)
        strcat(s, "/I ");
    if (_7bitmode)
        strcat(s, "/7 ");
    if (removectrlz)
        strcat(s, "/Z ");
    if (usetempfile)
        strcat(s, "/T ");
    if (quietmode)
        strcat(s, "/QQ ");
}

/* execute 'qkc.exe' */
void execqkcexe(argc, argv)
int argc;
char **argv;
{
    char *p, line[CMDLINE_BUF_SIZE];

    if (emulationmode || !execpascalver)
        return;
    genoptionstr(line);
    if (!infromstdin)
        while (argc-- > 0) {
            p = *argv++;
            if (p[0] == '-' && p[1] == '-')
                p++;
            if (*p == '/' || *p == '-')
                strcat(line, ".\\");
            strcat(line, p);
            strcat(line, " ");
        }
    if (execlp("QKC.EXE", "QKC.EXE", line, NULL) == -1)
        fputs("qkc: Cannot execute \'qkc.exe\'\n", stderr);
    exit(1);
}

#endif
