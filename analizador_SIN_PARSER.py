import ply.lex as lex
import ply.yacc as yacc

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


# Define las reglas de la gramática
def p_program(p):
    '''Program : DefList StatementList'''
    # Código para manejar la regla del programa

def p_def_list(p):
    '''DefList : Def DefList
               |'''
    # Código para manejar la regla de la lista de definiciones

def p_def(p):
    '''Def : DEF ID LPAREN TypedVarList RPAREN Return COLON Block'''
    # Código para manejar la regla de definición

def p_typed_var(p):
    '''TypedVar : ID COLON Type'''
    # Código para manejar la regla de variable tipada

def p_type(p):
    '''Type : INT
            | STR
            | LBRACKET Type RBRACKET'''
    # Código para manejar la regla de tipo



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

# Manejo de errores
def t_error(t):
    print("Carácter ilegal '%s'" % t.value[0])
    t.lexer.skip(1)

# Construimos el lexer
lexer = lex.lex()

# Archivo de entrada
filename = 'code.txt'

# Construye el parser
parser = yacc.yacc()


# Abrir el archivo y leer su contenido
with open(filename, 'r') as file:
    data = file.read()
parsed_data = parser.parse(data)
# Pasamos el contenido del archivo al lexer
lexer.input(data)

# Imprimimos los tokens encontrados
while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)

