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
	'COMMA',
	'SCOLON',
	'INF'
)

t_VAR = r'[A-Z][0-9A-Z\']*'
t_IMPLY = r'->'
t_OR = r'\|'
t_AND = r'&'
t_NOT = r'!'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_SCOLON = r';'
t_INF = r'\|-'
t_ignore  = ' \t\n'

def t_error(t):
	 print("Illegal character '%s'" % t.value[0])

lexer1 = lex.lex()

inputList = [
'A->B',
'C',
'C->!C|A',
'(A->C)->(B->C)->(A|B->C)',
'(A->B)->!!!B'
'(A&B->A)->((A->C)->(B->C)->(A|B->C))->(A&B->A)',
'!B',
'A',
'!A'
'B'
]

task = 'A->B, A, !C |- !A'

hlist = []

flag = False

names = {}

def makeTrue():
	global flag
	flag = True

#tree representation
def p_expression_inf(p):
	'expression : LPAREN INF SCOLON expression SCOLON expression RPAREN'
	p[0] = ''
	for i in range(1, len(p)):
		p[0] += p[i]

def p_expression_comma(p):
	'expression : LPAREN COMMA SCOLON expression SCOLON expression RPAREN'
	if p[4][1] != ',':
		hlist.append(p[4])
	hlist.append(p[6])
	p[0] = ''
	for i in range(1, len(p)):
		p[0] += p[i]

def p_expression_binary(p):
	'''expression : LPAREN IMPLY SCOLON expression SCOLON expression RPAREN
	| LPAREN OR SCOLON expression SCOLON expression RPAREN
	| LPAREN AND SCOLON expression SCOLON expression RPAREN'''
	p[0] = ''
	for i in range(1, len(p)):
		p[0] += p[i]

def p_expression_unary(p):
	'expression : LPAREN NOT expression RPAREN'
	p[0] = ''
	for i in range(1, len(p)):
		p[0] += p[i]

def p_expression_var(p):
	'expression : VAR'
	p[0] = p[1]

def p_expression_axiom(p):
	'expression : axiom'
	p[0] = p[1]

def p_axiom_1(p):
	'axiom : LPAREN IMPLY SCOLON expression SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN RPAREN'
	if p[4] == p[11]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 1]')
			makeTrue()

def p_axiom_2(p):
	'axiom : LPAREN IMPLY SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN SCOLON LPAREN IMPLY SCOLON LPAREN IMPLY SCOLON expression SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN RPAREN SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN RPAREN RPAREN'
	if p[7] == p[18] == p[32] and p[9] == p[23] and p[25] == p[34]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 2]')
			makeTrue()

def p_axiom_3(p):
	'axiom : LPAREN IMPLY SCOLON expression SCOLON LPAREN IMPLY SCOLON expression SCOLON LPAREN AND SCOLON expression SCOLON expression RPAREN RPAREN RPAREN'
	if p[4] == p[14] and p[9] == p[16]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 3]')
			makeTrue()

def p_axiom_4_5(p):
	'axiom : LPAREN IMPLY SCOLON LPAREN AND SCOLON expression SCOLON expression RPAREN SCOLON expression RPAREN'
	if p[7] == p[12]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 4]')
			makeTrue()
	elif p[9] == p[12]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 5]')
			makeTrue()

def p_axiom_6_7(p):
	'axiom : LPAREN IMPLY SCOLON expression SCOLON LPAREN OR SCOLON expression SCOLON expression RPAREN RPAREN'
	if p[4] == p[9]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 6]')
			makeTrue()
	elif p[4] == p[11]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 7]')
			makeTrue()

def p_axiom_8(p):
	'axiom : LPAREN IMPLY SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN SCOLON LPAREN IMPLY SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN SCOLON LPAREN IMPLY SCOLON LPAREN OR SCOLON expression SCOLON expression RPAREN SCOLON expression RPAREN RPAREN RPAREN'
	if p[7] == p[29] and p[18] == p[31] and p[9] == p[20] == p[34]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 8]')
			makeTrue()

def p_axiom_9(p):
	'axiom : LPAREN IMPLY SCOLON LPAREN IMPLY SCOLON expression SCOLON expression RPAREN SCOLON LPAREN IMPLY SCOLON LPAREN IMPLY SCOLON expression SCOLON LPAREN NOT expression RPAREN RPAREN SCOLON LPAREN NOT expression RPAREN RPAREN RPAREN'
	if p[7] == p[18] == p[28] and p[9] == p[22]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 9]')
			makeTrue()

def p_axiom_10(p):
	'axiom : LPAREN IMPLY SCOLON LPAREN NOT LPAREN NOT expression RPAREN RPAREN SCOLON expression RPAREN'
	if p[8] == p[12]:
		p[0] = ''
		for i in range(1, len(p)):
			p[0] += p[i]
		if p[0] == k:
			print('[' + str(n + 1) + '. Ax. sch. 10]')
			makeTrue()

def p_error(p):
	print("Syntax error at '%s'" % p)

yacc.yacc()
yacc.parse(treebuilding.build(task))
inputList = [treebuilding.build(i) for i in inputList]
for k in inputList:
	flag = False
	n = inputList.index(k)
	if k in hlist:
		flag = True
		print('[' + str(n + 1) + '. Hypothesis ' + str(hlist.index(k) + 1) + ']')
	if flag:
		continue
	yacc.parse(k)
	if flag:
		continue
	for p in range(n):
		for q in range(p + 1, n):
			if inputList[q] == '(->;' + inputList[p] + ';' + k + ')': 
				flag = True
				print('[' + str(n + 1) + '. M.P. ' + str(q + 1) + ', ' + str(p + 1) + ']')
				break
			elif inputList[p] == '(->;' + inputList[q] + ';' + k + ')':
				flag = True
				print('[' + str(n + 1) + '. M.P. ' + str(p + 1) + ', ' + str(q + 1) + ']')
				break
		else:
			continue
		break
	if flag:
		continue
	inputList.remove(k)
output = []
def inferenceBuilding(s):
	flag = False
	n = inputList.index(s)
	if s in hlist:
		output.append(('Hypothesis ' + str(hlist.index(s) + 1), s))
	yacc.parse(s)
	if flag:
		continue
	for p in range(n):
		for q in range(p + 1, n):
			if inputList[q] == '(->;' + inputList[p] + ';' + s + ')': 
				flag = True
				print('[' + str(n + 1) + '. M.P. ' + str(q + 1) + ', ' + str(p + 1) + ']')
				break
			elif inputList[p] == '(->;' + inputList[q] + ';' + s + ')':
				flag = True
				print('[' + str(n + 1) + '. M.P. ' + str(p + 1) + ', ' + str(q + 1) + ']')
				break
		else:
			continue
		break
	if flag:
		continue