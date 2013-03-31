/*
 *    qkcopt.c
 *
 *    Copyright (C) 1992 by K.Sato
 *    All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "qkc.h"
#ifdef UNIX
#  include <unistd.h>
#endif

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
extern int rotencoding;
extern int emulationmode;
extern unsigned char esc_k[];
extern unsigned char esc_a[];
#ifdef MSDOS
extern int execpascalver;
#endif

extern void onlinehelp();
extern void allocerror();
extern char *strdup2();

/* check if s is option or not */
int isoption(s)
char *s;
{
    if (emulationmode)
        return *s == '-';
    else
        return *s++ == '-' && *s != '-';
}

/* invalid option */
void invalidoption(c)
int c;
{
    fprintf(stderr, "qkc: Invalid command line option: \'%c\'\n", c);
    exit(1);
}

/* set escape sequence */
void setescseq(d, s)
unsigned char *d, *s;
{
    *d++ = *s++;
    *d = *s;
}

/* if argv[1] = '-nkf', qkc emulates nkf */
void checkemulationmode(s)
char *s;
{
    if (s != NULL && !strcmp(s, "-nkf")) {
        emulationmode = outtostdout = quietmode = _7bitmode = 1;
        convmode = MODE_JIS;
        setescseq(esc_k, NEWJIS2_K);
        setescseq(esc_a, NEWJIS2_A);
    }
}

/* check option (nkf emulation mode) */
void checkoption_nkf(opt)
char *opt;
{
    static int skip = 1;

    if (skip) {
        skip = 0;
        return;
    }
    opt++;
    while (*opt)
        switch (*opt++) {
        case 'j':
        case 'n':
            convmode = MODE_JIS;
            break;
        case 'e':
        case 'a':
            convmode = MODE_EUC;
            break;
        case 's':
        case 'x':
            convmode = MODE_SJIS;
            break;
        case 't':
            convmode = MODE_COPY;
            break;
        case 'i':
            if (*opt)
                esc_k[1] = *opt++;
            break;
        case 'o':
            if (*opt)
                esc_a[1] = *opt++;
            break;
        case 'B':
            if (*opt == 'B') {
                opt++;
                esc_k[1] = esc_a[1] = 'B';
            }
            else if (*opt == 'J') {
                opt++;
                esc_k[1] = 'B';
                esc_a[1] = 'J';
            }
            break;
        case 'J':
            esc_k[1] = 'B';
            esc_a[1] = 'J';
            break;
        case 'r':
            rotencoding = 1;
            break;
        case 'v':
            onlinehelp();
            exit(0);
        }
}

/* check option */
void checkoption(opt)
char *opt;
{
    if (emulationmode) {
        checkoption_nkf(opt);
        return;
    }
    if (*++opt == '\0') {
        infromstdin = 1;
        return;
    }
    while (*opt)
        switch (*opt++) {
        case 's':
            convmode = MODE_SJIS;
            break;
        case 'e':
            convmode = MODE_EUC;
            break;
        case 'j':
            convmode = MODE_JIS;
            setescseq(esc_k, NEWJIS_K);
            setescseq(esc_a, NEWJIS_A);
            break;
        case 'b':
            if (*opt++ == 'j') {
                convmode = MODE_JIS;
                setescseq(esc_k, NEWJIS2_K);
                setescseq(esc_a, NEWJIS2_A);
            }
            else
                invalidoption('b');
            break;
        case 'o':
            if (*opt++ == 'j') {
                convmode = MODE_JIS;
                setescseq(esc_k, OLDJIS_K);
                setescseq(esc_a, OLDJIS_A);
            }
            else
                invalidoption('o');
            break;
        case 'n':
            if (*opt == 'j') {
                opt++;
                convmode = MODE_JIS;
                setescseq(esc_k, NECJIS_K);
                setescseq(esc_a, NECJIS_A);
            }
            else
                newlineconvmode = NEWLINE_NO;
            break;
        case 'O':
            if (*opt == '-') {
                opt++;
                outtostdout = 0;
            }
            else
                outtostdout = 1;
            break;
        case 'I':
            if (*opt == '-') {
                opt++;
                infromstdin = 0;
            }
            else
                infromstdin = 1;
            break;
        case 'm':
            if (*opt == 'a') {
                opt++;
                newlineconvmode = NEWLINE_CR;
            }
            else
                newlineconvmode = NEWLINE_CRLF;
            break;
        case 'u':
            newlineconvmode = NEWLINE_LF;
            break;
        case 'l':
            if (*opt == '-') {
                opt++;
                forcenewlineconv = 0;
            }
            else
                forcenewlineconv = 1;
            break;
        case '8':
            _7bitmode = 0;
            break;
        case '7':
            _7bitmode = 1;
            break;
        case 'z':
            if (*opt == '-') {
                opt++;
                removectrlz = 0;
            }
            else
                removectrlz = 1;
            break;
        case 'q':
            if (*opt == '-') {
                opt++;
                quietmode = 0;
            }
            else
                quietmode = 1;
            break;
        case 'i':
            switch (*opt++) {
            case 's':
                inputcode = INPUT_SJIS;
                break;
            case 'e':
                inputcode = INPUT_EUC;
                break;
            case 'j':
                inputcode = INPUT_JIS;
                break;
            case 'a':
                inputcode = INPUT_AUTO;
                break;
            default:
                invalidoption('i');
            }
            break;
        case 'h':
            convmode = MODE_HELP;
            break;
        case 'H':
            convmode = MODE_DHELP;
            break;
        case 't':
            if (*opt == '-') {
                opt++;
                usetempfile = 0;
            }
            else
                usetempfile = 1;
            break;
        case 'c':
            break;
#ifdef MSDOS
        case 'P':
            execpascalver = !execpascalver;
            break;
#endif
        default:
            invalidoption(*--opt);
        }
}

/* check environment option */
void checkenvoption()
{
    char *org, *s, *t, bak;

    if (emulationmode)
        return;
    if ((org = getenv("QKC")) == NULL)
        return;
    if ((s = strdup2(org)) == NULL)
        allocerror();
    while (1) {
        while (*s == ' ' || *s == '\t')
            s++;
        if (*s == '\0')
            break;
        t = s;
        while (*t != ' ' && *t != '\t' && *t != '\0')
            t++;
        bak = *t;
        *t = '\0';
        if (isoption(s))
            checkoption(s);
        if (bak == '\0')
            break;
        s = ++t;
    }
    free(s);
}
