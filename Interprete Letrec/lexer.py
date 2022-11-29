import ply.lex as lex


tokens = [
    'NUMBER', 'MINUS', 'LPAREN', 'RPAREN', 'COMMA', 'ZERO', 'ID', 'ASSIGN', 'NEWLINE', 'SPACE', 'TAB', 'COMMENT'
]

reserved = {
    'if':'IF',
    'then': 'THEN',
    'else':'ELSE',
    'let':'LET',
    'in':'IN',
    'proc':'PROC',
    'letrec':'LETREC'
}

tokens += list(reserved.values())

t_MINUS = r'-'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_ASSIGN = r'='


def t_COMMENT(t):
    r';.*'
    if coloring:
        return t
    else:
        pass

def t_SPACE(t):
    '[ ]'
    if coloring: return t

def t_TAB(t):
    r'\t'
    if coloring: return t

def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += len(t.value)
    if coloring: return t

def t_ZERO(t):
    'zero\?'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_NUMBER(t):
    r'[+-]?\d+(?:\.\d+)?'
    try:
        t.value = int(t.value)
    except ValueError:
        t.value = float(t.value)
    return t

def t_error(t):
    if coloring:
        t.lexer.skip(1)
        return t
    else:
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)


toks = []
coloring = True

def set_coloring(c):
    global coloring
    coloring = c


lexer = lex.lex()

def init_lexer(input):
    lexer.input(input)

    while True:
        token = lexer.token()
        if not token : return
        toks.append((token.type, token.value))