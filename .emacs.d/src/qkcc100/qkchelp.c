/*
 *    qkchelp.c
 *
 *    Copyright (C) 1992 by K.Sato
 *    All rights reserved.
 */

#include <stdio.h>
#include "qkc.h"

#define COLUMN1 4
#define COLUMN2 36
#define LINE_LEN 100

#define NAME "QKC - Quick KANJI code Converter  C Version 1.0\n"
#define RIGHT "Copyright (C) 1992 by K.Sato\n\n"
#define EMU_MODE "nkf emulation mode\n\n"
#define USAGE "Usage: qkc [options] [filenames]\n"
#define USAGE_NKF "Usage: qkc -nkf [options] [filenames]\n"
#define ARE "Options are:\n"

extern int convmode;
extern int emulationmode;
static char *helpmsg[] = {
    " -s   to Shift-JIS",
    " -e   to EUC",
    " -j   to JIS",
    " -7   7 bit mode",
    " -i?  Input KANJI code",
    " -O   Output to stdout",
    " -I   Input from stdin",
    " -m,u Type of newline code",
    " -h   Show this help",
    " ",
    " qkc -H for detailed help",
    NULL
};
static char *detailedhelpmsg[] = {
    ">-s    to Shift-JIS",
    " -m    Convert newline to CR LF",
    ">-e    to EUC",
    " -u    Convert newline to LF",
    ">-j    to JIS",
    " -ma   Convert newline to CR",
    " -bj   to JIS",
    ">-oj   to Old-JIS",
    " -is   Input KANJI code is Shift-JIS",
    ">-nj   to NEC-JIS",
    " -ie   Input KANJI code is EUC",
    "> ",
    " -ij   Input KANJI code is JIS",
    " -O    Output to stdout",
    ">-I    Input from stdin",
    " -7    7 bit mode",
    "> ",
    " -z    Remove ^Z",
    ">-h,H  Show help",
    " -q    Quiet mode",
    NULL
};
static char *nkfhelpmsg[] = {
    " -j,n  JIS",
    " -s,x  Shift-JIS",
    " -e,a  EUC",
    " -t    No operation",
    " -i?   Escape sequence (2 byte char)",
    " -o?   Escape sequence (1 byte char)",
    " -BJ,J = -iBoJ",
    " -BB   = -iBoB",
    " -r    rot13/47 encoding",
    " -v    Show help",
    NULL
};

/* print online help */
void onlinehelp()
{
    int i;
    char **p, *s, *t, line[LINE_LEN];

    fputs(NAME, stderr);
    fputs(RIGHT, stderr);
    if (emulationmode) {
        fputs(EMU_MODE, stderr);
        fputs(USAGE_NKF, stderr);
    }
    else
        fputs(USAGE, stderr);
    fputs(ARE, stderr);
    t = line;
    for (i = 0; i < COLUMN1; i++)
        *t++ = ' ';
    if (emulationmode)
        p = nkfhelpmsg;
    else
        p = (convmode == MODE_DHELP) ? detailedhelpmsg : helpmsg;
    while (*p != NULL) {
        s = *p + 1;
        if (**p++ != ' ') {
            t = line + COLUMN1;
            while (*s != '\0')
                *t++ = *s++;
        }
        else {
            if (t != line + COLUMN1)
                while (t < line + COLUMN2)
                    *t++ = ' ';
            while (*s != '\0')
                *t++ = *s++;
            if (t != line + COLUMN1) {
                *t++ = '\n';
                *t = '\0';
                fputs(line, stderr);
            }
            else
                fputc('\n', stderr);
            t = line + COLUMN1;
        }
    }
}
