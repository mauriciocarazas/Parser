import ply.lex as lex
import ply.yacc as yacc

# Definición de los tokens (omitidos en esta versión sin lexer)
# Definimos los tokens
tokens = [
    # Indicar el inicio y el final de un bloque en Python
    'INDENT',
    'DEDENT',
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
    'AND', 'OR', 'NOT',

    'INT',
    'STR',
    'NEWLINE',
    'ELIF',
    'PASS',
    'NONE',
    'IS'
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

# Manejo de errores léxicos
def t_error(t):
    print(f"Error léxico: Carácter inesperado '{t.value[0]}' en la línea {t.lineno}")
    t.lexer.skip(1)
    
# Manejo de saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Expresiones regulares para INDENT y DEDENT
def t_INDENT(t):
    r'\n[ \t]+'
    t.lexer.level = len(t.value) - 1  # Nivel de indentación
    if t.lexer.level > t.lexer.indent_stack[-1]:
        t.type = 'INDENT'
        t.lexer.indent_stack.append(t.lexer.level)
        return t

def t_DEDENT(t):
    r'\n[ \t]+'
    t.lexer.level = len(t.value) - 1  # Nivel de indentación
    if t.lexer.level < t.lexer.indent_stack[-1]:
        while t.lexer.level < t.lexer.indent_stack[-1]:
            t.lexer.indent_stack.pop()
            t.type = 'DEDENT'
            return t
        t.lexer.skip(1)  # Ignorar líneas vacías


# Construir el lexer
lexer = lex.lex()

lexer.indent_stack = [0]  # Pila de niveles de indentación

# Integración con scanner
def gettoken():
    token = lexer.token()
    if token:
        return token.type
    else:
        return None

# Definición de las reglas gramaticales
def p_Program(p):
    '''Program : DefList'''
    p[0] = True

def p_DefList(p):
    '''DefList : Def DefList
               | empty'''
    p[0] = True

def p_Def(p):
    '''Def : DEF ID LPAREN TypedVarList RPAREN Return COLON Block'''
    p[0] = True

def p_TypedVar(p):
    '''TypedVar : ID COLON Type'''
    p[0] = True

def p_Type(p):
    '''Type : INT
            | STR
            | LBRACKET Type RBRACKET'''
    p[0] = True

def p_TypedVarList(p):
    '''TypedVarList : empty
                    | TypedVar TypedVarListTail'''
    p[0] = True

def p_TypedVarListTail(p):
    '''TypedVarListTail : COMMA TypedVar TypedVarListTail
                        | empty'''
    p[0] = True

def p_Return(p):
    '''Return : ARROW Type
              | empty'''
    p[0] = True

def p_Block(p):
    '''Block : NEWLINE INDENT Statement StatementList DEDENT'''
    p[0] = True

def p_StatementList(p):
    '''StatementList : Statement StatementList
                     | empty'''
    p[0] = True

def p_Statement(p):
    '''Statement : SimpleStatement NEWLINE
                 | IF Expr COLON Block ElifList Else
                 | WHILE Expr COLON Block
                 | FOR ID IN Expr COLON Block'''
    p[0] = True

def p_ElifList(p):
    '''ElifList : Elif ElifList
                | empty'''
    p[0] = True

def p_Elif(p):
    '''Elif : ELIF Expr COLON Block'''
    p[0] = True

def p_Else(p):
    '''Else : ELSE COLON Block
            | empty'''
    p[0] = True

def p_SimpleStatement(p):
    '''SimpleStatement : Expr SSTail
                       | PASS
                       | RETURN ReturnExpr'''
    p[0] = True

def p_SSTail(p):
    '''SSTail : ASSIGN Expr
              | empty'''
    p[0] = True

def p_ReturnExpr(p):
    '''ReturnExpr : Expr
                  | empty'''
    p[0] = True

def p_Expr(p):
    '''Expr : OrExpr'''
    p[0] = True

def p_OrExpr(p):
    '''OrExpr : AndExpr OrExprPrime'''
    p[0] = True

def p_OrExprPrime(p):
    '''OrExprPrime : OR AndExpr OrExprPrime
                   | empty'''
    p[0] = True

def p_AndExpr(p):
    '''AndExpr : NotExpr AndExprPrime'''
    p[0] = True

def p_AndExprPrime(p):
    '''AndExprPrime : AND NotExpr AndExprPrime
                    | empty'''
    p[0] = True

def p_NotExpr(p):
    '''NotExpr : CompExpr NotExprPrime'''
    p[0] = True

def p_NotExprPrime(p):
    '''NotExprPrime : NOT CompExpr NotExprPrime
                    | empty'''
    p[0] = True

def p_CompExpr(p):
    '''CompExpr : IntExpr CompExprPrime'''
    p[0] = True

def p_CompExprPrime(p):
    '''CompExprPrime : CompOp IntExpr CompExprPrime
                     | empty'''
    p[0] = True

def p_IntExpr(p):
    '''IntExpr : Term IntExprPrime'''
    p[0] = True

def p_IntExprPrime(p):
    '''IntExprPrime : PLUS Term IntExprPrime
                    | MINUS Term IntExprPrime
                    | empty'''
    p[0] = True

def p_Term(p):
    '''Term : Factor TermPrime'''
    p[0] = True

def p_TermPrime(p):
    '''TermPrime : TIMES Factor TermPrime
                 | DIVIDE Factor TermPrime
                 | MODULO Factor TermPrime
                 | empty'''
    p[0] = True

def p_Factor(p):
    '''Factor : MINUS Factor
              | Name
              | Literal
              | List
              | LPAREN Expr RPAREN'''
    p[0] = True

def p_Name(p):
    '''Name : ID NameTail'''
    p[0] = True

def p_NameTail(p):
    '''NameTail : LPAREN ExprList RPAREN
                | List
                | empty'''
    p[0] = True

def p_Literal(p):
    '''Literal : NONE
               | TRUE
               | FALSE
               | INTEGER
               | STRING'''
    p[0] = True

def p_List(p):
    '''List : LBRACKET ExprList RBRACKET'''
    p[0] = True

def p_ExprList(p):
    '''ExprList : empty
                | Expr ExprListTail'''
    p[0] = True

def p_ExprListTail(p):
    '''ExprListTail : empty
                    | COMMA Expr ExprListTail'''
    p[0] = True

def p_CompOp(p):
    '''CompOp : EQUALS
              | NOT_EQUALS
              | LESS_THAN
              | GREATER_THAN
              | LESS_THAN_EQUAL
              | GREATER_THAN_EQUAL
              | IS'''
    p[0] = True

def p_empty(p):
    '''empty :'''
    p[0] = True


# Manejo de errores sintácticos
def p_error(p):
    global errors
    if p:
        errors.append(f"Error de sintaxis: Token inesperado '{p.value}' en la línea {p.lineno}")
    else:
        errors.append("Error de sintaxis: Fin inesperado del archivo")

# Construir el parser
parser = yacc.yacc()

# Función para analizar el código de entrada
def analyze(code):
    global errors
    errors = []
    lexer.input(code)
    parser.parse(lexer=lexer, tracking=True)
    if errors:
        return errors
    else:
        return True


# Prueba del parser con una cadena de entrada
data = '''
x = 10 + 5 * (3 - 2)
print("El resultado es:", x)
'''

# Análisis del código de entrada
result = analyze(data)
    
# Impresión de los errores o éxito del análisis
if result == True:
    print("Análisis exitoso. El código de entrada pertenece al lenguaje.")
else:
    for error in result:
        print(error)
