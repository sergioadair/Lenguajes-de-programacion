import ply.lex as lex

tokens = [
    'NUMBER', 'MINUS', 'LPAREN', 'RPAREN', 'COMMA', 'ZERO', 'ID', 'ASSIGN', 'NEWLINE'
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

t_ignore = ' \t'
t_MINUS = r'-'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_ASSIGN = r'='


def t_COMMENT(t):
    r';.*'
    pass

def t_NEWLINE(t):
     r'\n+'
     t.lexer.lineno += len(t.value)
     #return t

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
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def init_lexer(input):
    lexer = lex.lex()
    lexer.input(input)

    while True:
        token = lexer.token()
        if not token : return
        #print(token)