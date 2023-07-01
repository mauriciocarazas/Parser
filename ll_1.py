import ply.yacc as yacc

# Definición de los tokens (omitidos en esta versión sin lexer)
# Definimos los tokens
tokens = [
    # Palabras reservadas
    'CLASS', 'DEF', 'IF', 'ELSE', 'WHILE', 'FOR', 'IN', 'RETURN',
    'BREAK', 'CONTINUE', 'NEW', 'TRUE', 'FALSE',
    # Identificadores y literales
    'ID', 'INTEGER', 'STRING',
    # Operadores y puntuación
   'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULO',
    'LESS_THAN', 'LESS_THAN_EQUAL', 'GREATER_THAN',
    'GREATER_THAN_EQUAL', 'EQUALS', 'NOT_EQUALS', 'ASSIGN',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LBRACE', 'RBRACE',
    'COMMA', 'COLON', 'DOT', 'SEMICOLON', 'ARROW',
    'AND', 'OR', 'NOT'
]

# Expresiones regulares para cada token
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MODULO = r'%'
t_LESS_THAN = r'<'
t_LESS_THAN_EQUAL = r'<='
t_GREATER_THAN = r'>'
t_GREATER_THAN_EQUAL = r'>='
t_EQUALS = r'=='
t_NOT_EQUALS = r'!='
t_ASSIGN = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'{'
t_RBRACE = r'}'
t_COMMA = r','
t_COLON = r':'
t_DOT = r'\.'
t_SEMICOLON = r';'
t_ARROW = r'->'
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'

# Ignoramos espacios en blanco y comentarios
t_ignore = ' \t\r'
def t_COMMENT(t):
    r'\#.*'
    pass

# Palabras reservadas
reserved = {
    'False': 'FALSE',
    'None': 'NONE',
    'True': 'TRUE',
    'and': 'AND',
    'as': 'AS',
    'assert': 'ASSERT',
    'async': 'ASYNC',
    'await': 'AWAIT',
    'break': 'BREAK',
    'class': 'CLASS',
    'continue': 'CONTINUE',
    'def': 'DEF',
    'del': 'DEL',
    'elif': 'ELIF',
    'else': 'ELSE',
    'except': 'EXCEPT',
    'finally': 'FINALLY',
    'for': 'FOR',
    'from': 'FROM',
    'global': 'GLOBAL',
    'if': 'IF',
    'import': 'IMPORT',
    'in': 'IN',
    'is': 'IS',
    'lambda': 'LAMBDA',
    'nonlocal': 'NONLOCAL',
    'not': 'NOT',
    'or': 'OR',
    'pass': 'PASS',
    'raise': 'RAISE',
    'return': 'RETURN',
    'try': 'TRY',
    'while': 'WHILE',
    'with': 'WITH',
    'yield': 'YIELD'
}

# Identificadores y literales
def t_ID(t):
    r'[a-z|A-Z][a-z|A-Z|0-9|_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'"(?:[^"\\]|\\.)*"'
    t.value = t.value[1:-1]
    return t

# Conjunto de siguientes para cada NT (símbolo no terminal)
# Aquí debes completar los conjuntos de siguientes correspondientes a cada NT de acuerdo a la gramática
# Conjuntos de primeros
primeros = {
    'Program': {'def', 'ε'},
    'DefList': {'def', 'ε'},
    'Def': {'def'},
    'TypedVar': {'ID'},
    'Type': {'int', 'str', '['},
    'TypedVarList': {'ID', 'ε'},
    'TypedVarListTail': {',', 'ε'},
    'Return': {'->', 'ε'},
    'Block': {'NEWLINE'},
    'StatementList': {'SimpleStatement', 'if', 'while', 'for', 'ε'},
    'Statement': {'SimpleStatement', 'if', 'while', 'for'},
    'ElifList': {'elif', 'ε'},
    'Elif': {'elif'},
    'Else': {'else', 'ε'},
    'SimpleStatement': {'Expr', 'pass', 'return'},
    'SSTail': {'=', 'ε'},
    'ReturnExpr': {'Expr', 'ε'},
    'Expr': {'orExpr'},
    'ExprPrime': {'if', 'ε'},
    'orExpr': {'andExpr'},
    'orExprPrime': {'or', 'ε'},
    'andExpr': {'notExpr'},
    'andExprPrime': {'and', 'ε'},
    'notExpr': {'CompExpr'},
    'notExprPrime': {'not', 'ε'},
    'CompExpr': {'IntExpr'},
    'CompExprPrime': {'CompOp', 'ε'},
    'IntExpr': {'Term', '-'},
    'IntExprPrime': {'+', '-', 'ε'},
    'Term': {'Factor', '-'},
    'TermPrime': {'*', '//', '%', 'ε'},
    'Factor': {'-', 'ID', 'None', 'True', 'False', 'INTEGER', 'STRING', '[', '('},
    'Name': {'ID'},
    'NameTail': {'ε', '(', '['},
    'Literal': {'None', 'True', 'False', 'INTEGER', 'STRING'},
    'List': {'['},
    'ExprList': {'Expr', 'ε'},
    'ExprListTail': {',', 'ε'},
    'CompOp': {'==', '!=', '<', '>', '<=', '>=', 'is'}
}

# Conjuntos de siguientes
siguientes = {
    'Program': {'$'},
    'DefList': {'SimpleStatement', 'if', 'while', 'for', 'Elif', 'else', 'NEWLINE', 'DEDENT'},
    'Def': {'def', 'SimpleStatement', 'if', 'while', 'for', 'Elif', 'else', 'NEWLINE', 'DEDENT'},
    'TypedVar': {'ID'},
    'Type': {':'},
    'TypedVarList': {')'},
    'TypedVarListTail': {')'},
    'Return': {':'},
    'Block': {'NEWLINE', 'DEDENT'},
    'StatementList': {'NEWLINE', 'DEDENT'},
    'Statement': {'NEWLINE', 'DEDENT'},
    'ElifList': {'else', 'NEWLINE', 'DEDENT'},
    'Elif': {'elif', 'else', 'NEWLINE', 'DEDENT'},
    'Else': {'else', 'NEWLINE', 'DEDENT'},
    'SimpleStatement': {'NEWLINE'},
    'SSTail': {'=', 'NEWLINE'},
    'ReturnExpr': {'NEWLINE'},
    'Expr': {':', 'NEWLINE'},
}

#NO TIENE PUNTOS!
gramatica = {
    'Program': {'$'},
    'DefList': {'Def', 'ε'},
    'Def': {'def ID ( TypedVarList ) Return : Block'},
    'TypedVar': {'ID : Type'},
    'Type': {'int', 'str', '[ Type ]'},
    'TypedVarList': {'ε', 'TypedVar TypedVarListTail'},
    'TypedVarListTail': {', TypedVar TypedVarListTail', 'ε'},
    'Return': {'ε', '-> Type'},
    'Block': {'NEWLINE INDENT Statement StatementList DEDENT'},
    'StatementList': {'Statement StatementList', 'ε'},
    'Statement': {'SimpleStatement NEWLINE', 'if Expr : Block ElifList Else', 'while Expr : Block', 'for ID in Expr : Block'},
    'ElifList': {'Elif ElifList', 'ε'},
    'Elif': {'elif Expr : Block'},
    'Else': {'ε', 'else : Block'},
    'SimpleStatement': {'Expr SSTail', 'pass', 'return ReturnExpr'},
    'SSTail': {'ε', '= Expr'},
    'ReturnExpr': {'Expr', 'ε'},
    'Expr': {'orExpr ExprPrime'},
    'ExprPrime': {'if andExpr else andExpr ExprPrime', 'ε'},
    'orExpr': {'andExpr orExprPrime'},
    'orExprPrime': {'or andExpr orExprPrime', 'ε'},
    'andExpr': {'notExpr andExprPrime'},
    'andExprPrime': {'and notExpr andExprPrime', 'ε'},
    'notExpr': {'CompExpr notExprPrime'},
    'notExprPrime': {'not CompExpr notExprPrime', 'ε'},
    'CompExpr': {'IntExpr CompExprPrime'},
    'CompExprPrime': {'CompOp IntExpr CompExprPrime', 'ε'},
    'IntExpr': {'Term IntExprPrime'},
    'IntExprPrime': {'+ Term IntExprPrime', '- Term IntExprPrime', 'ε'},
    'Term': {'Factor TermPrime'},
    'TermPrime': {'* Factor TermPrime', '// Factor TermPrime', '% Factor TermPrime', 'ε'},
    'Factor': {'- Factor', 'Name', 'Literal', 'List', '( Expr )'},
    'Name': {'ID NameTail'},
    'NameTail': {'ε', '( ExprList )', 'List'},
    'Literal': {'None', 'True', 'False', 'INTEGER', 'STRING'},
    'List': {'[ ExprList ]'},
    'ExprList': {'ε', 'Expr ExprListTail'},
    'ExprListTail': {'ε', ', Expr ExprListTail'},
    'CompOp': {'==', '!=', '<', '>', '<=', '>=', 'is'}
}

# Reglas de producción...
def extract_grammar_rules(grammar):
    rules = {}
    for non_terminal, productions in grammar.items():
        rules[non_terminal] = []
        for production in productions:
            symbols = production.split()
            rules[non_terminal].append(symbols)
    return rules

# Reglas de ERRORES...recuperación de errores
def p_empty(p):
    'empty :'
    pass

def p_error(p):
    # Manejo de errores sintácticos
    error_msg = f"Error de sintaxis en el token: {p.value}"
    p[0] = [error_msg]  # Agrega el error a la lista

def gettoken():
    # Llama a la función gettoken() del scanner para obtener el siguiente token
    token = scanner.gettoken()
    if token:
        return token.type
    else:
        return '$'  # Fin de entrada   

def parse(input_code):
   # parser = yacc.yacc()
    result = parser.parse(input_code)
    return result

# Prueba del parser con una cadena de entrada
data = '''
def my_func(x: int) -> int:
    if x > 0:
        return x
    elif x < 0:
        return -x
    else:
        return 0
'''
tokens_errors = []  # Lista para almacenar errores léxicos
scanner.initialize(data)  # Inicializa el scanner con el código de entrada
result = parser.parse()  # Parser sin argumentos
result = parse(data)

if isinstance(result, list):
    # Se encontraron errores sintácticos
    print("El código de entrada contiene errores sintácticos:")
    for error in result:
        print(error)
elif tokens_errors:
    # Se encontraron errores léxicos
    print("El código de entrada contiene errores léxicos:")
    for error in tokens_errors:
        print(error)
else:
    print("El código de entrada pertenece al lenguaje")