import ply.yacc as yacc
import ply.lex as lex

tokens = (
	'VAR',
	'IMPLY',
	'OR',
	'AND',
	'NOT',
	'LPAREN',
	'RPAREN'
)

t_VAR = r'[A-Z][0-9A-Z\']*'
t_IMPLY = r'->'
t_OR = r'\|'
t_AND = r'&'
t_NOT = r'!'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_ignore  = ' \t\n'

def t_error(t):
	 print("Illegal character '%s'" % t.value[0])

lexer = lex.lex()


precedence = (
	('right', 'IMPLY'),
	('left', 'OR'),
	('left', 'AND'),
	('right', 'NOT')
)

names = {}

def p_top_group(p):
	'expression : LPAREN expression RPAREN'
	p[0] = p[2]

def p_top_full(p):
	'''expression : expression IMPLY expression
	| expression OR expression
	| expression AND expression'''
	p[0] = '(' + p[2] + ',' + p[1] + ',' + p[3] + ')'

def p_top_var(p):
	'expression : VAR'
	p[0] = p[1]

def p_top_not(p):
	'expression : NOT expression'
	p[0] = '(' + p[1] + p[2] + ')'

def p_error(p):
	print("Syntax error at '%s'" % p.value)	

yaccer = yacc.yacc()

def build(data):
	return yaccer.parse(data)