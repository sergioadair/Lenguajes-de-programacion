import ply.yacc as yacc
from lexer import tokens


def p_expression_number(p):
    '''
    expression : NUMBER
    '''
    p[0] = ('const-exp', p[1])

def p_expression_minus(p):
    '''
    expression : MINUS LPAREN expression COMMA expression RPAREN
    '''
    p[0] = ('diff-exp', p[3], p[5])

def p_expression_zero(p):
    '''
    expression : ZERO LPAREN expression RPAREN
    '''
    p[0] = ('zero?-exp', p[3])

def p_expression_if(p):
    '''
    expression : IF expression THEN expression ELSE expression
    '''
    p[0] = ('if-exp', p[2], p[4], p[6])

def p_expression_id(p):
    '''
    expression : ID
    '''
    p[0] = ('var-exp', p[1])

def p_expression_let(p):
    '''
    expression : LET ID ASSIGN expression IN expression
    '''
    p[0] = ('let-exp', p[2], p[4], p[6])

def p_expression_proc(p):
    '''
    expression : PROC LPAREN ID RPAREN expression
    '''
    p[0] = ('proc-exp', p[3], p[5])

def p_expression_call(p):
    '''
    expression : LPAREN expression expression RPAREN
    '''
    p[0] = ('call-exp', p[2], p[3])

def p_expression_letrec(p):
    '''
    expression : LETREC ID LPAREN ID RPAREN ASSIGN expression IN expression
    '''
    p[0] = ('letrec-exp', p[2], p[4], p[7], p[9])

def p_error(p):
    if p:
        print(f'Syntax error at {p.value!r}')
    else:
        print('Syntax error at EOF')
    return


def init_parser(input):
    result = None
    if input:
        parser = yacc.yacc()
        result = parser.parse(input)
    #print(result)
    return result
