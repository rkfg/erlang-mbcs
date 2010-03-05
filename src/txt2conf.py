#!/usr/bin/env python
# coding:utf-8

import sys, os, re
import glob


def do(filename):
    lines = [x.strip() for x in open(filename) if x.startswith('0x')]
    undefined = [x.split()[0] for x in lines if x.upper().find('UNDEFINED') >= 0]
    leadbytes = [(x.split()[0], x.split()[1]) for x in lines if x.upper().find('LEAD BYTE') >= 0]
    mapping = [(x.split()[0], x.split()[1]) for x in lines if x.upper().find('LEAD BYTE') < 0 and x.upper().find('UNDEFINED') < 0]
    text = '''\
[
{undefined, "%s"},
{leadbytes, "%s"},
{mapping,   [%s]}
].
    ''' % (''.join('\\x%s' % x[2:] for x in undefined),
            ''.join('\\x%s' % x[2:] for x in leadbytes),
            ','.join('\n             {16#%s, 16#%s}' % (x[2:], y[2:]) for x,y in mapping).strip())
    newname = os.path.splitext(filename)[0]+'.CONF'
    open(newname, 'wb').write(text)


if __name__ == '__main__':
    for filename in glob.glob('*.TXT'):
        print filename
        do(filename)
