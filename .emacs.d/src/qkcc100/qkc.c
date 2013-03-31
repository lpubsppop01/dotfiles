/*
 *    QKC - Quick KANJI code Converter  C Version 1.0
 *
 *    Copyright (C) 1992 by K.Sato
 *    All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "qkc.h"
#ifdef MSDOS
#  include <io.h>
#endif
#ifdef UNIX
#  include <unistd.h>
#endif

int convmode = MODE_DEFAULT;
int inputcode = INPUT_AUTO;
int newlineconvmode = NEWLINE_NO;
int forcenewlineconv = 0;
int outtostdout = 0;
int infromstdin = 0;
int _7bitmode = 0;
int removectrlz = 0;
int usetempfile = 0;
int quietmode = 0;
int rotencoding = 0;
int emulationmode = 0;
unsigned char esc_k[2] = {'$', 'B'};
unsigned char esc_a[2] = {'(', 'J'};

int infd, outfd;
int ineof;
unsigned char *inbuf, *outbuf, *newlinebuf;
unsigned char *inbufptr, *outbufptr;
unsigned char *inbufend, *outbufend;
int detectedcode, newlineconv, crflag;

extern char binarytbl[];

extern void checkemulationmode();
extern int isoption();
extern void checkoption();
extern void checkenvoption();
extern void onlinehelp();
extern void execqkcexe();
extern void copy();
extern void sjistoeuc();
extern void euctosjis();
extern void sjistojis();
extern void jistosjis();
extern void euctojis();
extern void jistoeuc();
extern void sjistosjis();
extern void euctoeuc();
extern void jistojis();

/* call perror then exit(1) */
void runerror()
{
    perror("qkc");
    exit(1);
}

/* memory allocation error */
void allocerror()
{
    fputs("qkc: Memory allocation error\n", stderr);
    exit(1);
}

/* no files found */
void filenotfound()
{
    fputs("qkc: File not found or permission denied\n", stderr);
    exit(1);
}

/* out of memory */
void outofmemory()
{
    fputs("qkc: Out of memory\n", stderr);
    exit(1);
}

/* write error */
void writeerror()
{
    fputs("qkc: Write error\n", stderr);
    exit(1);
}

/* if no files specified, input from stdin */
void fromstdin(c)
int c;
{
    if (c == 0)
        infromstdin = 1;
    if (infromstdin)
        outtostdout = quietmode = 1;
}

/* if stdout is redirected to a file, output to stdout */
void tostdout()
{
    if (!emulationmode)
        outtostdout = !isatty(fileno(stdout));
}

/* get memory for input and output buffer */
void getmemory()
{
    if ((inbuf = (unsigned char *) malloc(IO_BUF_SIZE)) == NULL)
        outofmemory();
    if ((outbuf = (unsigned char *) malloc(IO_BUF_SIZE)) == NULL)
        outofmemory();
    outbufptr = outbuf;
    outbufend = outbuf + IO_BUF_SIZE;
    if (newlineconvmode != NEWLINE_NO)
        if ((newlinebuf = (unsigned char *) malloc(IO_BUF_SIZE)) == NULL)
            outofmemory();
}

/* duplicate string */
char *strdup2(s)
char *s;
{
    char *p;

    p = (char *) malloc(strlen(s) + 1);
    if (p)
        strcpy(p, s);
    return p;
}

/* split path into directory and filename */
void pathsplit(path, dir, name)
char *path;
char **dir, **name;
{
    char *s, *p;

    if ((p = strdup2(path)) == NULL)
        allocerror();
    s = p;
    while (*p)
        p++;
    while (--p >= s)
#ifdef MSDOS_HUMAN
        if (*p == '\\' || *p == ':' || *p == '/')
#else
        if (*p == '/')
#endif
            break;
    if ((*name = strdup2(++p)) == NULL)
        allocerror();
    *p = '\0';
    *dir = s;
}

/* make temporary filename (length(base) <= 3, length(ext) <= 4) */
char *maketemp(base, ext, dir)
char *base, *ext, *dir;
{
    int i;
    char *tempname;
    static int done = 0;

    if (!done) {
        srand((unsigned int) time(NULL));
        done = 1;
    }
    if ((tempname = (char *) malloc(strlen(dir) + 13)) == NULL)
        allocerror();
    for (i = 0; i < MKTEMP_MAX; i++) {
        sprintf(tempname, "%s%s%05ld%s",
                dir, base, (long) rand() % 100000, ext);
        if (access(tempname, 00))
            return tempname;
    }
    free(tempname);
    return NULL;
}

/* close input file */
void closeinfile()
{
    if (!infromstdin && close(infd))
        runerror();
}

/* open temporary file */
char *opentempfile(dir)
char *dir;
{
    char *tempname;

    if ((tempname = maketemp("qkc", TMP_EXT, dir)) == NULL) {
        closeinfile();
        fputs("qkc: Cannot open temporary file\n", stderr);
        exit(1);
    }
    if ((outfd = open(tempname, O_RDWR | O_CREAT | O_BINARY, 0600)) == -1) {
        free(tempname);
        return NULL;
    }
    return tempname;
}

/* convert newline code to CR LF */
unsigned int convertnewline1(buf, size)
unsigned char *buf;
unsigned int size;
{
    register unsigned char *s, *t, c;
    register unsigned int i;

    if (size == 0)
        return 0;
    s = buf;
    t = newlinebuf;
    if (crflag && *s == '\n') {
        crflag = 0;
        s++;
        size--;
    }
    for (i = 0; i < size; i++)
        switch (c = *s++) {
        case '\n':
            *t++ = '\r';
            *t++ = '\n';
            break;
        case '\r':
            *t++ = '\r';
            *t++ = '\n';
            if (i < size - 1 && *s == '\n') {
                s++;
                i++;
            }
            else
                crflag = 1;
            break;
        default:
            *t++ = c;
        }
    return t - newlinebuf;
}

/* convert newline code to LF or CR */
unsigned int convertnewline2(buf, size, code)
unsigned char *buf;
unsigned int size;
unsigned char code;
{
    register unsigned char *s, *t, c;
    register unsigned int i;

    if (size == 0)
        return 0;
    s = buf;
    t = newlinebuf;
    if (crflag && *s == '\n') {
        crflag = 0;
        s++;
        size--;
    }
    for (i = 0; i < size; i++) {
        c = *s++;
        switch (c) {
        case '\n':
            *t++ = code;
            break;
        case '\r':
            *t++ = code;
            if (i < size - 1 && *s == '\n') {
                s++;
                i++;
            }
            else
                crflag = 1;
            break;
        default:
            *t++ = c;
        }
    }
    return t - newlinebuf;
}

/* read into input buffer from input file */
int readbuf()
{
    unsigned int size;
    int result;
    unsigned char *end;

    if (ineof)
        return 1;
    inbufptr = inbufend = inbuf;
    end = inbuf + IO_BUF_SIZE;
    while (inbufend < end) {
        size = end - inbufend;
        result = read(infd, inbufend, size);
        if (result == -1) {
            fputs("qkc: Read error\n", stderr);
            exit(1);
        }
        else if (result == 0) {
            ineof = 1;
            return inbufend == inbuf;
        }
        inbufend += result;
    }
    return 0;
}

/* write from output buffer to output file */
void writebuf()
{
    unsigned int size, sizetemp;
    unsigned char *buf;

    size = outbufptr - outbuf;
    if (newlineconv == NEWLINE_CRLF) {
        if (size > IO_BUF_SIZE / 2) {
            sizetemp = convertnewline1(outbuf, IO_BUF_SIZE / 2);
            if (write(outfd, newlinebuf, sizetemp) < sizetemp)
                if (!outtostdout || !isatty(outfd))
                    writeerror();
            buf = outbuf + IO_BUF_SIZE / 2;
            size -= IO_BUF_SIZE / 2;
        }
        else
            buf = outbuf;
        size = convertnewline1(buf, size);
        buf = newlinebuf;
    }
    else if (newlineconv != NEWLINE_NO) {
        size = convertnewline2(outbuf, size,
                               newlineconv == NEWLINE_LF ? '\n' : '\r');
        buf = newlinebuf;
    }
    else
        buf = outbuf;
    if (write(outfd, buf, size) < size)
        if (!outtostdout || !isatty(outfd))
            writeerror();
    outbufptr = outbuf;
}

/* truncate ctrl-Z */
void truncatectrlz()
{
    long pos;
    unsigned char buf;

#ifdef UNIX
    if (outtostdout)
        return;
#endif
    if ((pos = lseek(outfd, 0L, 2)) == -1 || pos == 0)
        return;
    if ((pos = lseek(outfd, -1L, 1)) == -1
        || read(outfd, &buf, sizeof buf) == -1)
        runerror();
    if (buf == CTRLZ)
#ifdef MSDOS
        if (chsize(outfd, pos))
#else
        if (ftruncate(outfd, pos))
#endif
            runerror();
    if (lseek(outfd, 0L, 2) == -1)
        runerror();
}

/* close output file */
void closeoutfile()
{
    writebuf();
    if (removectrlz)
        truncatectrlz();
    if (!outtostdout && close(outfd))
        runerror();
}

/* check if buf's code is binary */
int checkbinary(buf, size)
register unsigned char *buf;
register unsigned int size;
{
    register int i, count;

    if (size > CHK_BIN_SIZE)
        size = CHK_BIN_SIZE;
    count = 0;
    for (i = 0; i < size; i++) {
        if (*buf < 0x20 && binarytbl[*buf])
            count++;
        buf++;
    }
    return count << 5 > size;
}

/* detect newline code */
int detectnewlinecode(buf, size)
register unsigned char *buf;
register unsigned int size;
{
    register unsigned char *end;

    end = buf + size;
    while (buf < end)
        switch (*buf) {
        case '\n':
            return NEWLINE_LF;
        case '\r':
            if (++buf == end)
                return NEWLINE_NO;
            else if (*buf == '\n')
                return NEWLINE_CRLF;
            else
                return NEWLINE_CR;
        default:
            buf++;
        }
    return NEWLINE_NO;
}

/* scan forward if Shift-JIS code or JIS code exists */
int checkforward(p, size)
unsigned char *p;
unsigned int size;
{
    unsigned char c;

    if (size > CHK_KANA_SIZE)
        size = CHK_KANA_SIZE;
    if (size == 0)
        return INPUT_EUC;
    c = *p++;
    while (1) {
        if (c == ESC) {
            if (--size == 0)
                break;
            if ((c = *p++) == '$') {
                if (--size == 0)
                    break;
                if ((c = *p++) == 'B' || c == '@')
                    return INPUT_JIS;
                else
                    continue;
            }
            else if (c == 'K')
                return INPUT_JIS;
            else
                continue;
        }
        else if (c >= 0x81) {
            if (c == 0x8e) {
                if (--size == 0)
                    break;
                p++;
            }
            else if (c <= 0x9f) {
                if (--size == 0)
                    break;
                c = *p++;
                if (iskanji2nd(c))
                    return INPUT_SJIS;
                else
                    continue;
            }
            else if (c >= 0xa1 && c <= 0xdf) {
                if (--size == 0)
                    break;
                c = *p++;
                if (iskana(c))
                    continue;
                else if (iseuc(c))
                    return INPUT_EUC;
                else
                    continue;
            }
            else if (c != 0xa0)
                return INPUT_EUC;
        }
        if (--size == 0)
            break;
        c = *p++;
    }
    return INPUT_EUC;
}

/* detect kanji code */
int detectcode(buf, size)
unsigned char *buf;
unsigned int size;
{
    register unsigned char *p, c;
    register unsigned int count;
    int unknownstat;

    count = size;
    if (count == 0)
        return INPUT_ASCII;
    p = buf;
    unknownstat = 0;
    c = *p++;
    while (1) {
        if (c == ESC) {
            if (--count == 0)
                break;
            if ((c = *p++) == '$') {
                if (--count == 0)
                    break;
                if ((c = *p++) == 'B' || c == '@')
                    return INPUT_JIS;
                else
                    continue;
            }
            else if (c == 'K')
                return INPUT_JIS;
            else
                continue;
        }
        else if (c >= 0x81) {
            if (c == 0x8e) {
                if (--count == 0)
                    break;
                c = *p++;
                if (iskana(c))
                    unknownstat |= 2;
                else if (iskanji2nd(c))
                    return INPUT_SJIS;
                else
                    continue;
            }
            else if (c <= 0x9f) {
                if (--count == 0)
                    break;
                c = *p++;
                if (iskanji2nd(c))
                    return INPUT_SJIS;
                else
                    continue;
            }
            else if (c >= 0xa1 && c <= 0xdf || c == 0xfd || c == 0xfe) {
                if (--count == 0)
                    break;
                c = *p++;
                if (iseuc(c))
                    if (iskana(c))
                        return checkforward(p, count - 1);
                    else
                        return INPUT_EUC;
                else
                    continue;
            }
            else if (c >= 0xe0 && c <= 0xfc) {
                if (--count == 0)
                    break;
                c = *p++;
                if (iskanji2nd(c))
                    if (iseuc(c))
                        unknownstat |= 1;
                    else
                        return INPUT_SJIS;
                else
                    if (iseuc(c))
                        return INPUT_EUC;
                    else
                        continue;
            }
        }
        if (--count == 0)
            break;
        c = *p++;
    }
    switch (unknownstat) {
    case 1:
    case 3:
        if (!outtostdout)
            return INPUT_UNKNOWN;
    case 2:
        return INPUT_SJIS;
    default:
        return INPUT_ASCII;
    }
}

/* check if input file needs conversion */
int checkfile()
{
    int binary, result, same;
    unsigned int bufsize;

    ineof = 0;
    readbuf();
    bufsize = inbufend - inbuf;
    binary = 0;
    if (inputcode == INPUT_AUTO) {
        binary = checkbinary(inbuf, bufsize);
        if (binary && !outtostdout)
            return CHECK_BINARY;
    }
    if (!forcenewlineconv)
        if (!binary && newlineconvmode != NEWLINE_NO) {
            result = detectnewlinecode(inbuf, bufsize);
            if (result == NEWLINE_NO || result == newlineconvmode)
                newlineconv = NEWLINE_NO;
            else
                newlineconv = newlineconvmode;
        }
        else
            newlineconv = NEWLINE_NO;
    else
        newlineconv = newlineconvmode;
    if (binary)
        detectedcode = INPUT_BINARY;
    else if (inputcode == INPUT_AUTO)
        detectedcode = detectcode(inbuf, bufsize);
    else
        detectedcode = inputcode;
    same = detectedcode == INPUT_SJIS && convmode == MODE_SJIS
           || detectedcode == INPUT_EUC && convmode == MODE_EUC
           || detectedcode == INPUT_JIS && convmode == MODE_JIS
           || convmode == MODE_COPY;
    if ((detectedcode == INPUT_ASCII || detectedcode == INPUT_UNKNOWN || same)
        && !outtostdout && newlineconv == NEWLINE_NO)
        if (inputcode == INPUT_AUTO)
            switch (detectedcode) {
            case INPUT_SJIS:
                return CHECK_SJIS;
            case INPUT_EUC:
                return CHECK_EUC;
            case INPUT_JIS:
                return CHECK_JIS;
            case INPUT_ASCII:
                return CHECK_ASCII;
            case INPUT_UNKNOWN:
                return CHECK_UNKNOWN;
            default:
                return CHECK_NOTHING;
            }
        else if (same && convmode != MODE_JIS)
            return CHECK_NOTHING;
    return CHECK_OK;
}

/* convert input file's kanji code, then output */
void conv()
{
    crflag = 0;
    if (convmode == MODE_COPY || detectedcode == INPUT_BINARY
        || detectedcode == INPUT_ASCII || detectedcode == INPUT_UNKNOWN)
        copy();
    else if (convmode == MODE_SJIS)
        switch (detectedcode) {
        case INPUT_SJIS:
            sjistosjis();
            break;
        case INPUT_EUC:
            euctosjis();
            break;
        case INPUT_JIS:
            jistosjis();
            break;
        default:
            copy();
        }
    else if (convmode == MODE_EUC)
        switch (detectedcode) {
        case INPUT_SJIS:
            sjistoeuc();
            break;
        case INPUT_EUC:
            euctoeuc();
            break;
        case INPUT_JIS:
            jistoeuc();
            break;
        default:
            copy();
        }
    else if (convmode == MODE_JIS)
        switch (detectedcode) {
        case INPUT_SJIS:
            sjistojis();
            break;
        case INPUT_EUC:
            euctojis();
            break;
        case INPUT_JIS:
            jistojis();
            break;
        default:
            copy();
        }
    else
        copy();
}

/* conversion information string */
char *convinfostr(buf)
char *buf;
{
    switch (detectedcode) {
    case INPUT_BINARY:
        strcpy(buf, "(Binary)");
        break;
    case INPUT_ASCII:
        strcpy(buf, "(Ascii)");
        break;
    case INPUT_UNKNOWN:
        strcpy(buf, "(Unknown)");
        break;
    case INPUT_SJIS:
        strcpy(buf, "(Shift-JIS");
        if (convmode == MODE_EUC)
            strcat(buf, " to EUC)");
        else if (convmode == MODE_JIS)
            strcat(buf, " to JIS)");
        else
            strcat(buf, ")");
        break;
    case INPUT_EUC:
        strcpy(buf, "(EUC");
        if (convmode == MODE_SJIS)
            strcat(buf, " to Shift-JIS)");
        else if (convmode == MODE_JIS)
            strcat(buf, " to JIS)");
        else
            strcat(buf, ")");
        break;
    case INPUT_JIS:
        strcpy(buf, "(JIS");
        if (convmode == MODE_SJIS)
            strcat(buf, " to Shift-JIS)");
        else if (convmode == MODE_EUC)
            strcat(buf, " to EUC)");
        else
            strcat(buf, ")");
        break;
    default:
        *buf = '\0';
    }
    return buf;
}

/* file opening, check, code detection, conversion, etc. */
int convert(filename)
char *filename;
{
    char *dir, *name, *tempname, *msg, info[19];
    int width, result;
    struct stat statbuf;
#ifdef MSDOS_HUMAN
    static int done_i = 0, done_o = 0;
#endif

    if (filename && filename[0] == '-' && filename[1] == '-')
        filename++;
    if (!infromstdin) {
        msg = NULL;
        if ((infd = open(filename, O_RDONLY | O_BINARY)) == -1)
            return 0;
        width = (((int) strlen(filename) - 1) / 12 + 1) * 12;
        if (stat(filename, &statbuf))
            runerror();
#if defined LSI_C || defined MS_C
        if (isatty(infd))
#else
        if ((statbuf.st_mode & S_IFMT) != S_IFREG)
#endif
            msg = "Is not a file";
        else if (!outtostdout && access(filename, 02))
            msg = "File is read only";
        else if (statbuf.st_size == 0)
            msg = "File is empty";
        if (msg) {
            if (!quietmode)
                fprintf(stderr, "  Skipped: %-*s  %s\n",
                        width, filename, msg);
            closeinfile();
            return 1;
        }
    }
    else {
        infd = fileno(stdin);
#ifdef MSDOS_HUMAN
        if (!done_i) {
#  ifdef LSI_C
            fsetbin(stdin);
#  elif defined HUMAN
            fmode(stdin, 1);
#  else
            if (setmode(infd, O_BINARY) == -1) {
                fputs("qkc: Binary mode not available\n", stderr);
                exit(1);
            }
#  endif
            done_i = 1;
        }
#endif
    }
    result = checkfile();
    if (result != CHECK_OK) {
        switch (result) {
        case CHECK_BINARY:
            msg = "May be binary file";
            break;
        case CHECK_SJIS:
            msg = "Shift-JIS code already exists";
            break;
        case CHECK_EUC:
            msg = "EUC already exists";
            break;
        case CHECK_JIS:
            msg = "JIS code already exists";
            break;
        case CHECK_ASCII:
            msg = "No KANJI code exists";
            break;
        case CHECK_UNKNOWN:
            msg = "Cannot identify KANJI code";
            break;
        case CHECK_NOTHING:
            msg = "Nothing to do";
        }
        if (!quietmode)
            fprintf(stderr, "  Skipped: %-*s  %s\n", width, filename, msg);
        closeinfile();
        return 1;
    }
    if (!outtostdout) {
        pathsplit(filename, &dir, &name);
        tempname = opentempfile(dir);
        free(dir);
        free(name);
        if (tempname == NULL) {
            if (!quietmode)
                fprintf(stderr, "  Skipped: %-*s  Cannot open temorary file\n",
                        width, filename);
            closeinfile();
            return 1;
        }
    }
    else {
        outfd = fileno(stdout);
#ifdef MSDOS_HUMAN
        if (!done_o) {
#  ifdef LSI_C
            fsetbin(stdout);
#  elif defined HUMAN
            fmode(stdout, 1);
#  else
            if (setmode(outfd, O_BINARY) == -1) {
                fputs("qkc: Binary mode not available\n", stderr);
                exit(1);
            }
#  endif
            done_o = 1;
        }
#endif
    }
    conv();
    closeinfile();
    closeoutfile();
    if (!outtostdout) {
        if (chmod(tempname, statbuf.st_mode & 0777))
            runerror();
        if (unlink(filename))
            runerror();
        if (rename(tempname, filename))
            runerror();
        free(tempname);
    }
    if (!quietmode)
        fprintf(stderr, "Converted: %-*s  %s\n",
                width, filename, convinfostr(info));
    return 1;
}

/* function main */
int CALLTYPE main(argc, argv)
int argc;
char **argv;
{
    int filefound;

    checkemulationmode(*(argv + 1));
    tostdout();
    checkenvoption();
    while (--argc > 0)
        if (isoption(*++argv))
            checkoption(*argv);
        else
            break;
    fromstdin(argc);
    if (convmode == MODE_HELP || convmode == MODE_DHELP)
        onlinehelp();
    else {
#ifdef MSDOS
        execqkcexe(argc, argv);
#endif
        getmemory();
        if (!infromstdin) {
            filefound = 0;
            while (argc-- > 0)
                filefound |= convert(*argv++);
            if (!filefound)
                filenotfound();
        }
        else
            convert(NULL);
    }
    return 0;
}
