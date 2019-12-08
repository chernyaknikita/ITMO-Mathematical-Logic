import ply.yacc as yacc
import ply.lex as lex
import treebuilding

tokens = (
	'VAR',
	'IMPLY',
	'OR',
	'AND',
	'NOT',
	'LPAREN',
	'RPAREN',
	'COMMA'
)

t_VAR = r'[A-Z][0-9A-Z\']*'
t_IMPLY = r'->'
t_OR = r'\|'
t_AND = r'&'
t_NOT = r'!'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_ignore  = ' \t\n'

def t_error(t):
	 print("Illegal character '%s'" % t.value[0])

lexer1 = lex.lex()

axioms = [
'A->B->A',
'(A->B)->(A->B->C)->(A->C)',
'A->B->A&B',
'A&B->A',
'A&B->B',
'A->A|B',
'B->A|B',
'(A->C)->(B->C)->(A|B->C)',
'(A->B)->(A->!B)->!A',
'!!A->A',
'(A&B->A)->B->(A&B->A)'
]




names = {}

#tree representation

def p_axiom_1(p):
	'axiom : LPAREN IMPLY COMMA expression COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN RPAREN'
	if p[4] == p[11]:
		print('Ax. sch. 1')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_2(p):
	'axiom : LPAREN IMPLY COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN COMMA LPAREN IMPLY COMMA LPAREN IMPLY COMMA expression COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN RPAREN COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN RPAREN RPAREN'
	if p[7] == p[18] == p[32] and p[9] == p[23] and p[25] == p[34]:
		print('Ax. sch. 2')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_3(p):
	'axiom : LPAREN IMPLY COMMA expression COMMA LPAREN IMPLY COMMA expression COMMA LPAREN AND COMMA expression COMMA expression RPAREN RPAREN RPAREN'
	if p[4] == p[14] and p[9] == p[16]:
		print('Ax. sch. 3')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_4_5(p):
	'axiom : LPAREN IMPLY COMMA LPAREN AND COMMA expression COMMA expression RPAREN COMMA expression RPAREN'
	if p[7] == p[12]:
		print('Ax. sch. 4')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
	elif p[9] == p[12]:
		print('Ax. sch. 5')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_6_7(p):
	'axiom : LPAREN IMPLY COMMA expression COMMA LPAREN OR COMMA expression COMMA expression RPAREN RPAREN'
	if p[4] == p[9]:
		print('Ax. sch. 6')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
	elif p[4] == p[11]:
		print('Ax. sch. 7')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_8(p):
	'axiom : LPAREN IMPLY COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN COMMA LPAREN IMPLY COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN COMMA LPAREN IMPLY COMMA LPAREN OR COMMA expression COMMA expression RPAREN COMMA expression RPAREN RPAREN RPAREN'
	if p[7] == p[29] and p[18] == p[31] and p[9] == p[20] == p[34]:
		print('Ax. sch. 8')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_9(p):
	'axiom : LPAREN IMPLY COMMA LPAREN IMPLY COMMA expression COMMA expression RPAREN COMMA LPAREN IMPLY COMMA LPAREN IMPLY COMMA expression COMMA LPAREN NOT expression RPAREN RPAREN COMMA LPAREN NOT expression RPAREN RPAREN RPAREN'
	if p[7] == p[18] == p[28] and p[9] == p[22]:
		print('Ax. sch. 9')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_axiom_10(p):
	'axiom : LPAREN IMPLY COMMA LPAREN NOT LPAREN NOT expression RPAREN RPAREN COMMA expression RPAREN'
	if p[8] == p[12]:
		print('Ax. sch. 10')
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]

def p_expression_axiom(p):
	'expression : axiom'
	p[0] = p[1]

def p_expression_var(p):
	'expression : VAR'
	p[0] = p[1]

#direct representation

# def p_axiom_1(p):
# 	'axiom : expression IMPLY expression IMPLY expression'
# 	if p[1] == p[5]:
# 		print('Ax. sch. 1')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_2(p):
# 	'axiom : LPAREN expression IMPLY expression RPAREN IMPLY LPAREN expression IMPLY expression IMPLY expression RPAREN IMPLY LPAREN expression IMPLY expression RPAREN'
# 	if p[2] == p[8] == p[16] and p[4] == p[10] and p[12] == p[18]:
# 		print('Ax. sch. 2')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_3(p):
# 	'axiom : expression IMPLY expression IMPLY expression AND expression'
# 	if p[1] == p[5] and p[3] == p[7]:
# 		print('Ax. sch. 3')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_4_5(p):
# 	'axiom : expression AND expression IMPLY expression'
# 	if p[1] == p[5]:
# 		print('Ax. sch. 4')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]
# 	elif p[3] == p[5]:
# 		print('Ax. sch. 5')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_6_7(p):
# 	'axiom : expression IMPLY expression OR expression'
# 	if p[1] == p[3]:
# 		print('Ax. sch. 6')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]
# 	elif p[1] == p[5]:
# 		print('Ax. sch. 7')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_8(p):
# 	'axiom : LPAREN expression IMPLY expression RPAREN IMPLY LPAREN expression IMPLY expression RPAREN IMPLY LPAREN expression OR expression IMPLY expression RPAREN'
# 	if p[2] == p[14] and p[8] == p[16] and p[4] == p[10] == p[18]:
# 		print('Ax. sch. 8')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_9(p):
# 	'axiom : LPAREN expression IMPLY expression RPAREN IMPLY LPAREN expression IMPLY NOT expression RPAREN IMPLY NOT expression'
# 	if p[2] == p[8] == p[15] and p[4] == p[11]:
# 		print('Ax. sch. 9')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_axiom_10(p):
# 	'axiom : NOT NOT expression IMPLY expression'
# 	if p[3] == p[5]:
# 		print('Ax. sch. 10')
# 		p[0] = ''
# 		for i in range(1, len(p)):
# 			p[0] += p[i]

# def p_expression_axiom(p):
# 	'expression : LPAREN axiom RPAREN'
# 	p[0] = p[1] + p[2] + p[3]

# def p_expression_var(p):
# 	'expression : VAR'
# 	p[0] = p[1]

def p_error(p):
	print("Syntax error at '%s'" % p)

yacc.yacc()
for i in axioms:
# 	# x = treebuilding.build(i)
# 	# print(x)
# 	# lexer1.input(x)
# 	# k = 1
# 	# s = []
# 	# for j in lexer1:
# 	# 	if j.type == 'VAR':
# 	# 		s.append((j.value, k))
# 	# 	k += 1
# 	# s.sort()
# 	# for i in s:
# 	# 	print(i[0], ' : ', i[1])
# 	# print('-' * 50)
	# yacc.parse(i)
	yacc.parse(treebuilding.build(i))