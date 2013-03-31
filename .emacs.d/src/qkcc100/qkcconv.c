/*
 *    qkcconv.c
 *
 *    Copyright (C) 1992 by K.Sato
 *    All rights reserved.
 */

#include <stdio.h>
#include "qkc.h"

unsigned char kanji2nd;

extern int _7bitmode, rotencoding;
extern unsigned char esc_k[], esc_a[];
extern unsigned char *inbufptr, *outbufptr;
extern unsigned char *inbufend, *outbufend;
extern unsigned char rot13tbl[], rot47tbl[];

extern int readbuf();
extern void writebuf();

#define get() (inbufptr >= inbufend && readbuf() ? EOF : *inbufptr++)
#define put(c) (outbufptr >= outbufend ?\
               (writebuf(), *outbufptr++ = (c)) : (*outbufptr++ = (c)))

/* function version of get() */
int getf()
{
    return get();
}

/* function version of put() */
void putf(c)
int c;
{
    put(c);
}

/* convert JIS code to Shift-JIS code */
unsigned char jistojms(c1, c2)
register unsigned char c1, c2;
{
    if (c1 & 1) {
        c1 = (c1 >> 1) + 0x71;
        c2 += 0x1f;
        if (c2 >= 0x7f)
            c2++;
    }
    else {
        c1 = (c1 >> 1) + 0x70;
        c2 += 0x7e;
    }
    if (c1 > 0x9f)
        c1 += 0x40;
    kanji2nd = c2;
    return c1;
}

/* convert Shift-JIS code to JIS code */
unsigned char jmstojis(c1, c2)
register unsigned char c1, c2;
{
    c1 -= (c1 <= 0x9f) ? 0x70 : 0xb0;
    c1 <<= 1;
    if (c2 < 0x9f) {
        c2 -= (c2 < 0x7f) ? 0x1f : 0x20;
        c1--;
    }
    else
        c2 -= 0x7e;
    kanji2nd = c2;
    return c1;
}

/* output escape sequence for 1 byte character */
void set1byte()
{
    putf(ESC);
    putf(esc_a[0]);
    if (esc_a[1])
        putf(esc_a[1]);
}

/* output escape sequence for 2 byte character */
void set2byte()
{
    putf(ESC);
    putf(esc_k[0]);
    if (esc_k[1])
        putf(esc_k[1]);
}

/* at the end of JIS file, output escape sequence for 1 byte character */
void jisend(s, k)
char s, k;
{
    if (k)
        putf(SI);
    else if (s)
        set1byte();
}

/* copy */
void copy()
{
    register int c;

    while ((c = get()) != EOF)
        put(c);
}

/* Shift-JIS to EUC */
void sjistoeuc()
{
    register int c, c2;
    register char rot;

    rot = rotencoding;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c < 0x80) {
            if (rot)
                c = rot13tbl[c];
            putf(c);
        }
        else if (iskanji1st(c)) {
            if ((c2 = get()) == EOF) {
                putf(c);
                return;
            }
            if (iskanji2nd(c2)) {
                c = jmstojis(c, c2);
                if (rot) {
                    c = rot47tbl[c];
                    kanji2nd = rot47tbl[kanji2nd];
                }
                put(c | 0x80);
                put(kanji2nd | 0x80);
            }
            else {
                putf(c);
                putf(c2);
            }
        }
        else if (iskana(c)) {
            putf(0x8e);
            putf(c);
        }
        else
            putf(c);
        if ((c = get()) == EOF)
            return;
    }
}

/* EUC to Shift-JIS */
void euctosjis()
{
    register int c, c2;
    register char rot;

    rot = rotencoding;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c < 0x80) {
            if (rot)
                c = rot13tbl[c];
            put(c);
        }
        else if (iseuc(c)) {
            if ((c2 = get()) == EOF) {
                putf(c);
                return;
            }
            if (iseuc(c2)) {
                if (rot) {
                    c = rot47tbl[c & 0x7f];
                    c2 = rot47tbl[c2 & 0x7f];
                }
                c = jistojms(c & 0x7f, c2 & 0x7f);
                put(c);
                put(kanji2nd);
            }
            else {
                putf(c);
                putf(c2);
            }
        }
        else if (c == 0x8e) {
            if ((c = getf()) == EOF)
                return;
            putf(c);
        }
        else if (c == 0x8f) {
            if ((c = getf()) == EOF)
                return;
            putf(c);
            if ((c = getf()) == EOF)
                return;
            putf(c);
        }
        else
            putf(c);
        if ((c = get()) == EOF)
            return;
    }
}

/* Shift-JIS to JIS */
void sjistojis()
{
    register int c, c2;
    register char rot, state, kana;
    char _7bit;

    rot = rotencoding;
    _7bit = _7bitmode;
    state = kana = 0;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c < 0x80) {
            if (state) {
                set1byte();
                state = 0;
            }
            else if (kana) {
                putf(SI);
                kana = 0;
            }
            if (rot)
                c = rot13tbl[c];
            put(c);
        }
        else if (iskanji1st(c)) {
            if ((c2 = get()) == EOF) {
                jisend(state, kana);
                putf(c);
                return;
            }
            if (!state) {
                if (kana) {
                    putf(SI);
                    kana = 0;
                }
                set2byte();
                state = 1;
            }
            if (iskanji2nd(c2)) {
                c = jmstojis(c, c2);
                if (rot) {
                    c = rot47tbl[c];
                    kanji2nd = rot47tbl[kanji2nd];
                }
                put(c);
                put(kanji2nd);
            }
            else {
                putf(c);
                putf(c2);
            }
        }
        else if (iskana(c)) {
            if (state) {
                set1byte();
                state = 0;
            }
            if (_7bit) {
                if (!kana) {
                    putf(SO);
                    kana = 1;
                }
                c &= 0x7f;
            }
            putf(c);
        }
        else {
            if (state) {
                set1byte();
                state = 0;
            }
            else if (kana) {
                putf(SI);
                kana = 0;
            }
            putf(c);
        }
        if ((c = get()) == EOF) {
            jisend(state, kana);
            return;
        }
    }
}

/* JIS to Shift-JIS */
void jistosjis()
{
    register int c, c2;
    register char rot, state, kana;
    char _7bit;

    rot = rotencoding;
    _7bit = _7bitmode;
    state = kana = 0;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c == ESC) {
            if ((c = getf()) == EOF) {
                putf(ESC);
                return;
            }
            switch (c) {
            case '$':
                if ((c2 = getf()) == EOF) {
                    putf(ESC);
                    putf(c);
                    return;
                }
                if (c2 == 'B' || c2 == '@')
                    state = 1;
                else {
                    putf(ESC);
                    putf(c);
                    c = c2;
                    continue;
                }
                break;
            case '(':
                if ((c2 = getf()) == EOF) {
                    putf(ESC);
                    putf(c);
                    return;
                }
                if (c2 == 'J' || c2 == 'B' || c2 == 'H')
                    state = 0;
                else {
                    putf(ESC);
                    putf(c);
                    c = c2;
                    continue;
                }
                break;
            case 'K':
                state = 1;
                break;
            case 'H':
                state = 0;
                break;
            default:
                putf(ESC);
                continue;
            }
        }
        else if (c <= 0x20 || c == 0x7f) {
            if (_7bit && (c == SO || c == SI))
                kana = c == SO;
            else
                putf(c);
        }
        else if (state) {
            if ((c2 = get()) == EOF) {
                putf(c);
                return;
            }
            if (c2 <= 0x20) {
                putf(c);
                c = c2;
                continue;
            }
            if (c < 0x80 && isjis(c2)) {
                if (rot) {
                    c = rot47tbl[c];
                    c2 = rot47tbl[c2];
                }
                c = jistojms(c, c2);
                put(c);
                put(kanji2nd);
            }
            else {
                putf(c);
                putf(c2);
            }
        }
        else {
            if (_7bit && kana)
                c |= 0x80;
            else if (rot)
                c = rot13tbl[c];
            put(c);
        }
        if ((c = get()) == EOF)
            return;
    }
}

/* EUC to JIS */
void euctojis()
{
    register int c, c2;
    register char rot, state, kana;
    char _7bit;

    rot = rotencoding;
    _7bit = _7bitmode;
    state = kana = 0;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c < 0x80) {
            if (state) {
                set1byte();
                state = 0;
            }
            else if (kana) {
                putf(SI);
                kana = 0;
            }
            if (rot)
                c = rot13tbl[c];
            put(c);
        }
        else if (iseuc(c)) {
            if ((c2 = get()) == EOF) {
                jisend(state, kana);
                putf(c);
                return;
            }
            if (!state) {
                if (kana) {
                    putf(SI);
                    kana = 0;
                }
                set2byte();
                state = 1;
            }
            if (iseuc(c2)) {
                if (rot) {
                    c = rot47tbl[c & 0x7f];
                    c2 = rot47tbl[c2 & 0x7f];
                }
                put(c & 0x7f);
                put(c2 & 0x7f);
            }
            else {
                putf(c);
                putf(c2);
            }
        }
        else if (c == 0x8e) {
            if ((c = getf()) == EOF) {
                jisend(state, kana);
                return;
            }
            if (state) {
                set1byte();
                state = 0;
            }
            if (_7bit) {
                if (!kana) {
                    putf(SO);
                    kana = 1;
                }
                c &= 0x7f;
            }
            putf(c);
        }
        else if (c == 0x8f) {
            if ((c = getf()) == EOF) {
                jisend(state, kana);
                return;
            }
            if ((c2 = getf()) == EOF) {
                jisend(state, kana);
                putf(c);
                return;
            }
            if (!state) {
                if (kana) {
                    putf(SI);
                    kana = 0;
                }
                set2byte();
                state = 1;
            }
            putf(c);
            putf(c2);
        }
        else {
            if (state) {
                set1byte();
                state = 0;
            }
            else if (kana) {
                putf(SI);
                kana = 0;
            }
            putf(c);
        }
        if ((c = get()) == EOF) {
            jisend(state, kana);
            return;
        }
    }
}

/* JIS to EUC */
void jistoeuc()
{
    register int c, c2;
    register char rot, state, kana;
    char _7bit;

    rot = rotencoding;
    _7bit = _7bitmode;
    state = kana = 0;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c == ESC) {
            if ((c = getf()) == EOF) {
                putf(ESC);
                return;
            }
            switch (c) {
            case '$':
                if ((c2 = getf()) == EOF) {
                    putf(ESC);
                    putf(c);
                    return;
                }
                if (c2 == 'B' || c2 == '@')
                    state = 1;
                else {
                    putf(ESC);
                    putf(c);
                    c = c2;
                    continue;
                }
                break;
            case '(':
                if ((c2 = getf()) == EOF) {
                    putf(ESC);
                    putf(c);
                    return;
                }
                if (c2 == 'J' || c2 == 'B' || c2 == 'H')
                    state = 0;
                else {
                    putf(ESC);
                    putf(c);
                    c = c2;
                    continue;
                }
                break;
            case 'K':
                state = 1;
                break;
            case 'H':
                state = 0;
                break;
            default:
                putf(ESC);
                continue;
            }
        }
        else if (c <= 0x20 || c == 0x7f) {
            if (_7bit && (c == SO || c == SI))
                kana = c == SO;
            else
                putf(c);
        }
        else if (state) {
            if ((c2 = get()) == EOF) {
                putf(c);
                return;
            }
            if (c2 <= 0x20 || c2 == 0x7f) {
                putf(c);
                c = c2;
                continue;
            }
            if (c < 0x80 && isjis(c2) && rot) {
                c = rot47tbl[c];
                c2 = rot47tbl[c2];
            }
            put(c | 0x80);
            put(c2 | 0x80);
        }
        else {
            if (_7bit && kana)
                c |= 0x80;
            else if (rot)
                c = rot13tbl[c];
            if (iskana(c))
                putf(0x8e);
            put(c);
        }
        if ((c = get()) == EOF)
            return;
    }
}

/* Shift-JIS to Shift-JIS */
void sjistosjis()
{
    register int c, c2;

    if (!rotencoding) {
        copy();
        return;
    }
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (iskanji1st(c)) {
            if ((c2 = get()) == EOF) {
                putf(c);
                return;
            }
            if (iskanji2nd(c2)) {
                c = jmstojis(c, c2);
                c = jistojms(rot47tbl[c], rot47tbl[kanji2nd]);
                put(c);
                put(kanji2nd);
            }
            else {
                putf(c);
                putf(c2);
            }
        }
        else {
            c = rot13tbl[c];
            put(c);
        }
        if ((c = get()) == EOF)
            return;
    }
}

/* EUC to EUC */
void euctoeuc()
{
    register int c, c2;

    if (!rotencoding) {
        copy();
        return;
    }
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c < 0x80) {
            c = rot13tbl[c];
            put(c);
        }
        else if (iseuc(c)) {
            if ((c2 = get()) == EOF) {
                putf(c);
                return;
            }
            if (iseuc(c2)) {
                c = rot47tbl[c & 0x7f] | 0x80;
                c2 = rot47tbl[c2 & 0x7f] | 0x80;
            }
            put(c);
            put(c2);
        }
        else if (c == 0x8e || c == 0x8f) {
            putf(c);
            c2 = c;
            if ((c = getf()) == EOF)
                return;
            putf(c);
            if ((c = getf()) == EOF)
                return;
            if (c2 == 0x8e)
                continue;
            putf(c);
        }
        else
            putf(c);
        if ((c = get()) == EOF)
            return;
    }
}

/* JIS to JIS */
void jistojis()
{
    register int c, c2;
    register char i_state, o_state;
    char rot, _7bit, i_kana, o_kana;

    rot = rotencoding;
    _7bit = _7bitmode;
    i_state = o_state = i_kana = o_kana = 0;
    if ((c = getf()) == EOF)
        return;
    while (1) {
        if (c == ESC) {
            if ((c = getf()) == EOF) {
                jisend(o_state, o_kana);
                putf(ESC);
                return;
            }
            switch (c) {
            case '$':
                if ((c2 = getf()) == EOF) {
                    jisend(o_state, o_kana);
                    putf(ESC);
                    putf(c);
                    return;
                }
                if (c2 == 'B' || c2 == '@')
                    i_state = 1;
                else {
                    if (o_state) {
                        set1byte();
                        o_state = 0;
                    }
                    putf(ESC);
                    putf(c);
                    c = c2;
                    continue;
                }
                break;
            case '(':
                if ((c2 = getf()) == EOF) {
                    jisend(o_state, o_kana);
                    putf(ESC);
                    putf(c);
                    return;
                }
                if (c2 == 'J' || c2 == 'B' || c2 == 'H')
                    i_state = 0;
                else {
                    if (o_state) {
                        set1byte();
                        o_state = 0;
                    }
                    putf(ESC);
                    putf(c);
                    c = c2;
                    continue;
                }
                break;
            case 'K':
                i_state = 1;
                break;
            case 'H':
                i_state = 0;
                break;
            default:
                if (o_state) {
                    set1byte();
                    o_state = 0;
                }
                putf(ESC);
                continue;
            }
        }
        else if (c <= 0x20 || c == 0x7f)
            switch (c) {
            case SO:
                i_kana = 1;
                break;
            case SI:
                i_kana = 0;
                break;
            default:
                if (o_state) {
                    set1byte();
                    o_state = 0;
                }
                else if (o_kana) {
                    putf(SI);
                    o_kana = 0;
                }
                putf(c);
            }
        else if (i_state) {
            if ((c2 = get()) == EOF) {
                jisend(o_state, o_kana);
                putf(c);
                return;
            }
            if (c2 <= 0x20 || c2 == 0x7f) {
                if (o_state) {
                    set1byte();
                    o_state = 0;
                }
                putf(c);
                c = c2;
                continue;
            }
            if (!o_state) {
                if (o_kana) {
                    putf(SI);
                    o_kana = 0;
                }
                set2byte();
                o_state = 1;
            }
            if (c < 0x80 && isjis(c2) && rot) {
                c = rot47tbl[c];
                c2 = rot47tbl[c2];
            }
            put(c);
            put(c2);
        }
        else {
            if (i_kana)
                c |= 0x80;
            if (o_state) {
                set1byte();
                o_state = 0;
            }
            if (iskana(c) && _7bit) {
                if (!o_kana) {
                    putf(SO);
                    o_kana = 1;
                }
                c &= 0x7f;
            }
            else {
                if (o_kana) {
                    putf(SI);
                    o_kana = 0;
                }
                if (rot)
                    c = rot13tbl[c];
            }
            put(c);
        }
        if ((c = get()) == EOF)
            return;
    }
}
