const DEBUG_MODE = off

from std/tables import TableRef, newTable, toTable, contains, `[]`, `[]=`, pairs
from std/strutils import indent, replace, toUpper
from std/os import changeFileExt, commandLineParams, fileExists, splitFile
from std/terminal import ForegroundColor, Style, styledWrite

when DEBUG_MODE:
  from std/json import `%`, pretty

#####################
#    Type System    #
#####################

type RslTypeKind = enum
  RSL_TYPE_KIND_IDENTIFIER
  RSL_TYPE_KIND_POINTER
  RSL_TYPE_KIND_CONSTANT
  RSL_TYPE_KIND_STRUCTURE
  RSL_TYPE_KIND_ARRAY

type RslType = ref object
  case kind: RslTypeKind:
  of RSL_TYPE_KIND_IDENTIFIER:
    identifierOf: string
  of RSL_TYPE_KIND_POINTER:
    pointerOf: RslType
  of RSL_TYPE_KIND_CONSTANT:
    constantOf: RslType
  of RSL_TYPE_KIND_STRUCTURE:
    structureKind: string
    structureOf: RslType
  of RSL_TYPE_KIND_ARRAY:
    arrayOf: RslType

proc `$`(this: RslType): string =
  case this.kind:
  of RSL_TYPE_KIND_IDENTIFIER:
    result = this.identifierOf
  of RSL_TYPE_KIND_POINTER:
    result = $this.pointerOf & "*"
  of RSL_TYPE_KIND_CONSTANT:
    result = "const " & $this.constantOf
  of RSL_TYPE_KIND_STRUCTURE:
    result = this.structureKind & " " & $this.structureOf
  of RSL_TYPE_KIND_ARRAY:
    result = $this.arrayOf

proc toVariable(this: RslType, identifier: string): string =
  case this.kind:
  of RSL_TYPE_KIND_IDENTIFIER:
    result = this.identifierOf & " " & identifier
  of RSL_TYPE_KIND_POINTER:
    result = $this.pointerOf & "*" & " " & identifier
  of RSL_TYPE_KIND_CONSTANT:
    result = "const " & $this.constantOf & " " & identifier
  of RSL_TYPE_KIND_STRUCTURE:
    result = this.structureKind & " " & $this.structureOf & " " & identifier
  of RSL_TYPE_KIND_ARRAY:
    result = $this.arrayOf & " " & identifier & "[]"

proc identifier(this: RslType): string =
  case this.kind:
  of RSL_TYPE_KIND_IDENTIFIER:
    result = this.identifierOf
  of RSL_TYPE_KIND_POINTER:
    result = this.pointerOf.identifier()
  of RSL_TYPE_KIND_CONSTANT:
    result = this.constantOf.identifier()
  of RSL_TYPE_KIND_STRUCTURE:
    result = this.structureOf.identifier()
  of RSL_TYPE_KIND_ARRAY:
    result = this.arrayOf.identifier()

###############
#    State    #
###############

type State = ref object
  sourceName: string
  
  packageIdentifier: string

proc newState(sourceName: string): State =
  result = State()

  result.sourceName = sourceName
  
  result.packageIdentifier = sourceName.splitFile().name

###############
#    Lexer    #
###############

type TokenKind = enum
  TOKEN_KIND_ADD
  TOKEN_KIND_SUB
  TOKEN_KIND_MUL
  TOKEN_KIND_DIV
  TOKEN_KIND_MOD
  
  TOKEN_KIND_LOGICAL_NOT
  TOKEN_KIND_LOGICAL_AND
  TOKEN_KIND_LOGICAL_XOR
  TOKEN_KIND_LOGICAL_OR

  TOKEN_KIND_BITWISE_NOT
  TOKEN_KIND_BITWISE_AND
  TOKEN_KIND_BITWISE_XOR
  TOKEN_KIND_BITWISE_OR

  TOKEN_KIND_SHL
  TOKEN_KIND_SHR
  TOKEN_KIND_USHL
  TOKEN_KIND_USHR

  TOKEN_KIND_ASSIGN

  TOKEN_KIND_EQUAL
  TOKEN_KIND_NOT_EQUAL
  TOKEN_KIND_GREATER
  TOKEN_KIND_GREATER_EQUAL
  TOKEN_KIND_LESSER
  TOKEN_KIND_LESSER_EQUAL

  TOKEN_KIND_IDENTIFIER

  TOKEN_KIND_INTEGER
  TOKEN_KIND_FLOAT
  TOKEN_KIND_STRING
  TOKEN_KIND_CHARACTER
  TOKEN_KIND_BOOLEAN

  TOKEN_KIND_LPAREN
  TOKEN_KIND_RPAREN
  TOKEN_KIND_LBRACK
  TOKEN_KIND_RBRACK
  TOKEN_KIND_LBRACE
  TOKEN_KIND_RBRACE

  TOKEN_KIND_DOT
  TOKEN_KIND_COLON
  TOKEN_KIND_SEMI
  TOKEN_KIND_COMMA

  TOKEN_KIND_PACKAGE
  TOKEN_KIND_INCLUDE
  TOKEN_KIND_DEFINE
  TOKEN_KIND_RAW
  
  TOKEN_KIND_PTR
  TOKEN_KIND_ADDR

  TOKEN_KIND_VAL
  TOKEN_KIND_VAR

  TOKEN_KIND_PROC
  
  TOKEN_KIND_STRUCT
  TOKEN_KIND_UNION
  
  TOKEN_KIND_IF
  TOKEN_KIND_ELIF
  TOKEN_KIND_ELSE

  TOKEN_KIND_CASE
  TOKEN_KIND_OF

  TOKEN_KIND_WHILE
  TOKEN_KIND_FOR

  TOKEN_KIND_RETURN
  TOKEN_KIND_BREAK
  TOKEN_KIND_CONTINUE

const UNARY_OPERATOR_TOKEN_KINDS = {TOKEN_KIND_ADD, TOKEN_KIND_SUB, TOKEN_KIND_LOGICAL_NOT, TOKEN_KIND_BITWISE_NOT, TOKEN_KIND_PTR, TOKEN_KIND_ADDR}
const BINARY_OPERATOR_TOKEN_KINDS = {TOKEN_KIND_ADD..TOKEN_KIND_LESSER_EQUAL} - {TOKEN_KIND_LOGICAL_NOT, TOKEN_KIND_BITWISE_NOT}
const CONSTANT_LITERAL_TOKEN_KINDS = {TOKEN_KIND_INTEGER..TOKEN_KIND_BOOLEAN}

type TokenLocation = tuple[line: int, column: int]

type Token = tuple[kind: TokenKind, lexeme: string, location: TokenLocation]

const RESERVED_WORDS = toTable({
  "true":     TOKEN_KIND_BOOLEAN,
  "false":    TOKEN_KIND_BOOLEAN,

  "not":      TOKEN_KIND_LOGICAL_NOT,
  "and":      TOKEN_KIND_LOGICAL_AND,
  "xor":      TOKEN_KIND_LOGICAL_XOR,
  "or":       TOKEN_KIND_LOGICAL_OR,
  
  "package":  TOKEN_KIND_PACKAGE,
  "include":  TOKEN_KIND_INCLUDE,
  "define":   TOKEN_KIND_DEFINE,
  "raw":      TOKEN_KIND_RAW,

  "val":      TOKEN_KIND_VAL,
  "var":      TOKEN_KIND_VAR,
  "ptr":      TOKEN_KIND_PTR,
  "addr":     TOKEN_KIND_ADDR,

  "proc":     TOKEN_KIND_PROC,
  "struct":   TOKEN_KIND_STRUCT,
  "struct":   TOKEN_KIND_UNION,

  "if":       TOKEN_KIND_IF,
  "elif":     TOKEN_KIND_ELIF,
  "else":     TOKEN_KIND_ELSE,

  "case":     TOKEN_KIND_CASE,
  "of":       TOKEN_KIND_OF,

  "while":    TOKEN_KIND_WHILE,
  "for":      TOKEN_KIND_FOR,

  "return":   TOKEN_KIND_RETURN,
  "break":    TOKEN_KIND_BREAK,
  "continue": TOKEN_KIND_CONTINUE,
})

type Lexer = ref object
  state: State
  
  index: int
  start: int
  
  line: int
  column: int
  
  input: string
  output: seq[Token]

proc newLexer(sourceName: string, input: string): Lexer =  
  result = Lexer()
  
  result.state = newState(sourceName)

  result.index = 0
  result.start = 0
  
  result.line = 1
  result.column = 1
  
  result.input = input
  result.output = newSeq[Token]()

template eos(this: Lexer, l: int = 0): bool = this.index + l > high this.input

template reset(this: Lexer): void = this.start = this.index

template read(this: Lexer, l: int = 0): char = this.input[this.index + l]

proc warn(this: Lexer, message: string): void =
  stdout.styledWrite(fgRed, "[LEXER, ERROR ON LINE ", $this.line, " AND COLUMN ", $this.column, ")]: ", message, "\n")

template nextLine(this: Lexer): void =
  this.index.inc()
  this.line.inc()
  this.column = 1
  
template nextColumn(this: Lexer): void =
  this.index.inc()
  this.column.inc()

template lexeme(this: Lexer, soffset: int = 0, eoffset: int = -1): string =
  this.input[this.start + soffset..this.index + eoffset]

proc addToken(this: Lexer, kind: TokenKind, lexeme: string = this.lexeme, xoffset: int = -lexeme.len(), yoffset: int = 0): void =
  this.output.add((kind, lexeme, (this.line + yoffset, this.column + xoffset)))

proc tokenize(this: Lexer): void =
  while not this.eos():
    this.reset()
    
    case this.read():
    of '\n':
      this.nextLine()
    of {'\t', '\r', ' '}:
      this.nextColumn()
    of '#':
      while not this.eos() and this.read() != '\n':
        this.nextColumn()
      this.nextLine()
    of '+':
      this.nextColumn()
      this.addToken(TOKEN_KIND_ADD)
    of '-':
      this.nextColumn()
      this.addToken(TOKEN_KIND_SUB)
    of '*':
      this.nextColumn()
      this.addToken(TOKEN_KIND_MUL)
    of '/':
      this.nextColumn()
      this.addToken(TOKEN_KIND_DIV)
    of '%':
      this.nextColumn()
      this.addToken(TOKEN_KIND_MOD)
    of '~':
      this.nextColumn()
      this.addToken(TOKEN_KIND_BITWISE_NOT)
    of '&':
      this.nextColumn()
      this.addToken(TOKEN_KIND_BITWISE_AND)
    of '^':
      this.nextColumn()
      this.addToken(TOKEN_KIND_BITWISE_XOR)
    of '|':
      this.nextColumn()
      this.addToken(TOKEN_KIND_BITWISE_OR)
    of '!':
      this.nextColumn()
      case this.read():
      of '=':
        this.nextColumn()
        this.addToken(TOKEN_KIND_NOT_EQUAL)
      else:
        this.warn("Unexpected character after '!' expected '=' for 'not equal' operator")
    of '=':
      this.nextColumn()
      case this.read():
      of '=':
        this.nextColumn()
        this.addToken(TOKEN_KIND_EQUAL)
      else:
        this.addToken(TOKEN_KIND_ASSIGN)
    of '>':
      this.nextColumn()
      case this.read()
      of '=':
        this.nextColumn()
        this.addToken(TOKEN_KIND_GREATER_EQUAL)
      of '>':
        this.nextColumn()
        case this.read():
        of '>':
          this.addToken(TOKEN_KIND_SHR)
        else:
          this.addToken(TOKEN_KIND_USHR)
      else:
        this.addToken(TOKEN_KIND_GREATER)
    of '<':
      this.nextColumn()
      case this.read()
      of '=':
        this.nextColumn()
        this.addToken(TOKEN_KIND_LESSER_EQUAL)
      of '<':
        this.nextColumn()
        case this.read():
        of '<':
          this.addToken(TOKEN_KIND_SHL)
        else:
          this.addToken(TOKEN_KIND_USHL)
      else:
        this.addToken(TOKEN_KIND_LESSER)
    of '(':
      this.nextColumn()
      this.addToken(TOKEN_KIND_LPAREN)
    of ')':
      this.nextColumn()
      this.addToken(TOKEN_KIND_RPAREN)
    of '[':
      this.nextColumn()
      this.addToken(TOKEN_KIND_LBRACK)
    of ']':
      this.nextColumn()
      this.addToken(TOKEN_KIND_RBRACK)
    of '{':
      this.nextColumn()
      this.addToken(TOKEN_KIND_LBRACE)
    of '}':
      this.nextColumn()
      this.addToken(TOKEN_KIND_RBRACE)
    of '.':
      this.nextColumn()
      this.addToken(TOKEN_KIND_DOT)
    of ':':
      this.nextColumn()
      this.addToken(TOKEN_KIND_COLON)
    of ';':
      this.nextColumn()
      this.addToken(TOKEN_KIND_SEMI)
    of ',':
      this.nextColumn()
      this.addToken(TOKEN_KIND_COMMA)
    of '\'':
      this.nextColumn()

      while not this.eos() and this.read() != '\'':
        this.nextColumn()

      this.nextColumn()

      this.addToken(TOKEN_KIND_CHARACTER)
    of '\"':
      this.nextColumn()

      while not this.eos() and this.read() != '\"':
        this.nextColumn()

      this.nextColumn()

      this.addToken(TOKEN_KIND_STRING)
    of {'0'..'9'}:
      while not this.eos() and this.read() in {'0'..'9'}:
        this.nextColumn()

      if this.read() == '.':
        while not this.eos() and this.read() in {'0'..'9'}:
          this.nextColumn()
  
        this.addToken(TOKEN_KIND_FLOAT)
      else:
        this.addToken(TOKEN_KIND_INTEGER)
    of {'a'..'z', 'A'..'Z', '_'}:
      while not this.eos() and this.read() in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
        this.nextColumn()

      if this.lexeme() in RESERVED_WORDS:
        this.addToken(RESERVED_WORDS[this.lexeme])
      else:
        this.addToken(TOKEN_KIND_IDENTIFIER)
    else:
      this.warn("Unexpected character, received '" & $this.read() & "'")

##############################
#    Abstract Syntax Tree    #
##############################

type NodeKind = enum
  NODE_KIND_PROGRAM

  NODE_KIND_BLOCK_STMT
  
  NODE_KIND_PACKAGE_STMT
  NODE_KIND_INCLUDE_STMT
  NODE_KIND_DEFINE_STMT
  NODE_KIND_RAW_STMT
  
  NODE_KIND_VARIABLE_DEF_STMT

  NODE_KIND_PROCEDURE_DEF_STMT

  NODE_KIND_STRUCTURE_DEF_STMT
  
  NODE_KIND_IF_STMT
  NODE_KIND_SWITCH_STMT
  NODE_KIND_WHILE_STMT
  
  NODE_KIND_RETURN_STMT
  NODE_KIND_BREAK_STMT
  NODE_KIND_CONTINUE_STMT  
  
  NODE_KIND_EXPR_STMT

  NODE_KIND_GROUPING_EXPR
  NODE_KIND_BINARY_EXPR
  NODE_KIND_UNARY_EXPR
  
  NODE_KIND_LITERAL_EXPR
  
  NODE_KIND_REFERENCE_EXPR
  NODE_KIND_FIELD_ACCESS_EXPR
  NODE_KIND_ARRAY_ACCESS_EXPR
  NODE_KIND_PROC_CALL_EXPR
  NODE_KIND_STRUCTURE_INIT_EXPR

type Node = ref object
  location: TokenLocation

  rslType: RslType

  case kind: NodeKind:
  of NODE_KIND_PROGRAM:
    programBody: seq[Node]
  of NODE_KIND_BLOCK_STMT:
    blockBody: seq[Node]
  of NODE_KIND_PACKAGE_STMT:
    packageIdentifier: string
  of NODE_KIND_INCLUDE_STMT:
    includeIdentifier: string
  of NODE_KIND_DEFINE_STMT:
    defineIdentifier: string
    defineKind: RslType
  of NODE_KIND_RAW_STMT:
    rawIsGlobal: bool
    rawBody: string
  of NODE_KIND_VARIABLE_DEF_STMT:
    variableIsConstant: bool
    variableIsGlobal: bool
    variableIdentifier: string
    variableKind: RslType
    variableValue: Node
  of NODE_KIND_PROCEDURE_DEF_STMT:
    procedureIdentifier: string
    procedureParameters: seq[tuple[identifier: string, kind: RslType]]
    procedureReturnKind: RslType
    procedureBody: Node
  of NODE_KIND_STRUCTURE_DEF_STMT:
    structureIsGlobal: bool
    structureKind: string
    structureIdentifier: string
    structureFields: seq[tuple[identifier: string, kind: RslType]]
  of NODE_KIND_IF_STMT:
    ifCondition: Node
    ifBody: Node
    elifs: seq[tuple[condition: Node, body: Node]]
    elseBody: Node
  of NODE_KIND_SWITCH_STMT:
    switchIdentifier: string
    switchCases: seq[tuple[matches: Node, body: Node]]
  of NODE_KIND_WHILE_STMT:
    whileCondition: Node
    whileBody: Node
  of NODE_KIND_RETURN_STMT:
    returnBody: Node
  of NODE_KIND_BREAK_STMT:
    breakIdentifier: string
  of NODE_KIND_CONTINUE_STMT:
    continueIdentifier: string
  of NODE_KIND_EXPR_STMT:
    stmtExpr: Node
  of NODE_KIND_GROUPING_EXPR:
    groupingExpr: Node
  of NODE_KIND_BINARY_EXPR:
    bopOperand1: Node
    bopOperator: TokenKind
    bopOperand2: Node
  of NODE_KIND_UNARY_EXPR:
    uopOperand1: Node
    uopOperator: TokenKind
  of NODE_KIND_LITERAL_EXPR:
    literalKind: TokenKind
    literalValue: string
  of NODE_KIND_REFERENCE_EXPR:
    referenceIdentifier: string
  of NODE_KIND_FIELD_ACCESS_EXPR:
    fieldIdentifier: string
    fieldParent: string
  of NODE_KIND_ARRAY_ACCESS_EXPR:
    arrayIdentifier: string
    arrayIndex: Node
  of NODE_KIND_PROC_CALL_EXPR:
    callIdentifier: string
    callArguments: seq[Node]
  of NODE_KIND_STRUCTURE_INIT_EXPR:
    initArguments: seq[Node]

################
#    Parser    #
################

const OPERATORS = toTable({
  TOKEN_KIND_LOGICAL_NOT:   (bop: (precedence: 0x00, isRightAssociative: false), uop: (precedence: 0x0C)),
  TOKEN_KIND_BITWISE_NOT:   (bop: (precedence: 0x00, isRightAssociative: false), uop: (precedence: 0x0C)),
  TOKEN_KIND_PTR:           (bop: (precedence: 0x00, isRightAssociative: false), uop: (precedence: 0x0C)),
  TOKEN_KIND_ADDR:          (bop: (precedence: 0x00, isRightAssociative: false), uop: (precedence: 0x0C)),

  TOKEN_KIND_MUL:           (bop: (precedence: 0x0B, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_DIV:           (bop: (precedence: 0x0B, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_MOD:           (bop: (precedence: 0x0B, isRightAssociative: false), uop: (precedence: 0x00)),

  TOKEN_KIND_ADD:           (bop: (precedence: 0x0A, isRightAssociative: false), uop: (precedence: 0x0C)),
  TOKEN_KIND_SUB:           (bop: (precedence: 0x0A, isRightAssociative: false), uop: (precedence: 0x0C)),

  TOKEN_KIND_GREATER:       (bop: (precedence: 0x09, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_GREATER_EQUAL: (bop: (precedence: 0x09, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_LESSER:        (bop: (precedence: 0x09, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_LESSER_EQUAL:  (bop: (precedence: 0x09, isRightAssociative: false), uop: (precedence: 0x00)),

  TOKEN_KIND_EQUAL:         (bop: (precedence: 0x08, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_NOT_EQUAL:     (bop: (precedence: 0x08, isRightAssociative: false), uop: (precedence: 0x00)),

  TOKEN_KIND_SHL:           (bop: (precedence: 0x07, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_SHR:           (bop: (precedence: 0x07, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_USHL:          (bop: (precedence: 0x07, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_USHR:          (bop: (precedence: 0x07, isRightAssociative: false), uop: (precedence: 0x00)),

  TOKEN_KIND_BITWISE_AND:   (bop: (precedence: 0x06, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_BITWISE_XOR:   (bop: (precedence: 0x05, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_BITWISE_OR:    (bop: (precedence: 0x04, isRightAssociative: false), uop: (precedence: 0x00)),

  TOKEN_KIND_LOGICAL_AND:   (bop: (precedence: 0x03, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_LOGICAL_XOR:   (bop: (precedence: 0x02, isRightAssociative: false), uop: (precedence: 0x00)),
  TOKEN_KIND_LOGICAL_OR:    (bop: (precedence: 0x01, isRightAssociative: false), uop: (precedence: 0x00)),

  TOKEN_KIND_ASSIGN:        (bop: (precedence: 0x00, isRightAssociative: true),  uop: (precedence: 0x00)),
})

type Parser = ref object
  state: State
  
  index: int

  input: seq[Token]
  output: Node

proc newParser(state: State, input: seq[Token]): Parser =
  result = Parser()

  result.state = state

  result.index = 0

  result.input = input
  result.output = Node(kind: NODE_KIND_PROGRAM, programBody: newSeq[Node]())

template eos(this: Parser, l: int = 0): bool = this.index + l > high this.input

template read(this: Parser, l: int = 0): Token = this.input[this.index + l]

proc warn(this: Parser, message: string): void =
  stdout.styledWrite(fgRed, "[PARSER, ERROR ON LINE ", $this.read().location.line, " AND COLUMN ", $this.read().location.column, ")]: ", message, "\n")

template next(this: Parser, l: int = 1): void = this.index.inc(l)

template matches(this: Parser, kinds: set[TokenKind], l: int = 0): bool = this.read(l).kind in kinds

template expects(this: Parser, kinds: set[TokenKind], onError: untyped): void =
  if this.matches(kinds):
    this.next()
  else:
    onError

const EXPR_FIRSTS = UNARY_OPERATOR_TOKEN_KINDS + CONSTANT_LITERAL_TOKEN_KINDS + {TOKEN_KIND_LPAREN, TOKEN_KIND_IDENTIFIER}

proc parseQualifiedName(this: Parser): string =
  result = ""

  this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier at the start of a fully qualified name")

  result &= this.read(-1).lexeme

  while not this.eos() and this.matches({TOKEN_KIND_DOT}):
    this.expects({TOKEN_KIND_DOT}): this.warn("Expected a dot ('.') between components of a fully qualified name")
    
    result &= "."

    this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier after dot ('.') in a fully qualified name")

    result &= this.read(-1).lexeme

proc parseRslType(this: Parser): RslType =
  case this.read().kind:
  of TOKEN_KIND_IDENTIFIER:
    result = RslType(kind: RSL_TYPE_KIND_IDENTIFIER)

    result.identifierOf = this.parseQualifiedName()
  of TOKEN_KIND_PTR:
    result = RslType(kind: RSL_TYPE_KIND_POINTER)

    this.expects({TOKEN_KIND_PTR}): this.warn("Expected a 'ptr' key word for pointer type. This may be an internal error")

    result.pointerOf = this.parseRslType()
  of TOKEN_KIND_VAL:
    result = RslType(kind: RSL_TYPE_KIND_CONSTANT)

    this.expects({TOKEN_KIND_VAL}): this.warn("Expected a 'val' key word for constant type. This may be an internal error")

    result.constantOf = this.parseRslType()
  of TOKEN_KIND_LBRACK:
    result = RslType(kind: RSL_TYPE_KIND_ARRAY)

    this.expects({TOKEN_KIND_LBRACK}): this.warn("Expected an opening bracket ('[') for array type. This may be an internal error")

    result.arrayOf = this.parseRslType()

    this.expects({TOKEN_KIND_RBRACK}): this.warn("Expected a closing bracket (']') for array type")
  else:
    this.warn("Invalid RSL type signiture")

proc parseExpr(this: Parser, prec: int = 0, primary: bool = false): Node =
  if primary:
    case this.read().kind:
    of UNARY_OPERATOR_TOKEN_KINDS:
      result = Node(kind: NODE_KIND_UNARY_EXPR, location: this.read().location)

      this.expects(UNARY_OPERATOR_TOKEN_KINDS): this.warn("Expected an unary operator. This may be an internal error")

      result.uopOperator = this.read(-1).kind

      result.uopOperand1 = this.parseExpr(prec=OPERATORS[result.uopOperator].uop.precedence, primary=true)
    of CONSTANT_LITERAL_TOKEN_KINDS:
      result = Node(kind: NODE_KIND_LITERAL_EXPR, location: this.read().location)

      this.expects(CONSTANT_LITERAL_TOKEN_KINDS): this.warn("Expected a literal value. This may be an internal error")

      result.literalKind = this.read(-1).kind
      result.literalValue = this.read(-1).lexeme
    of TOKEN_KIND_LPAREN:
      result = Node(kind: NODE_KIND_GROUPING_EXPR, location: this.read().location)

      this.expects({TOKEN_KIND_LPAREN}): this.warn("Expected an opening bracket ('('). This may be an internal error")
      
      result.groupingExpr = this.parseExpr()
      
      this.expects({TOKEN_KIND_RPAREN}): this.warn("Expected a closing bracket (')')")
    of TOKEN_KIND_IDENTIFIER:
      if this.eos(1):
        result = Node(kind: NODE_KIND_REFERENCE_EXPR, location: this.read().location)

        this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for variable reference. This may be an internal error")

        result.referenceIdentifier = this.read(-1).lexeme
      else:
        case this.read(1).kind:
        of TOKEN_KIND_DOT:
          result = Node(kind: NODE_KIND_FIELD_ACCESS_EXPR)

          this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for field access. This may be an internal error")

          result.fieldParent = this.read(-1).lexeme

          this.expects({TOKEN_KIND_DOT}): this.warn("Expected an dot ('.'). This may be an internal error")

          this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for field access")

          result.fieldIdentifier = this.read(-1).lexeme
        of TOKEN_KIND_LBRACK:
          result = Node(kind: NODE_KIND_ARRAY_ACCESS_EXPR, location: this.read().location)

          this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for array access. This may be an internal error")
          
          result.arrayIdentifier = this.read(-1).lexeme
        
          this.expects({TOKEN_KIND_LBRACK}): this.warn("Expected an opening bracket ('['). This may be an internal error")

          result.arrayIndex = this.parseExpr()

          this.expects({TOKEN_KIND_RBRACK}): this.warn("Expected a closing bracket (']') for array access call")
        of TOKEN_KIND_LPAREN:
          result = Node(kind: NODE_KIND_PROC_CALL_EXPR, location: this.read().location, callArguments: newSeq[Node]())

          this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for procedure call. This may be an internal error")

          result.callIdentifier = this.read(-1).lexeme

          this.expects({TOKEN_KIND_LPAREN}): this.warn("Expected an opening bracket ('('). This may be an internal error")

          while not this.eos() and not this.matches({TOKEN_KIND_RPAREN}):
            result.callArguments.add(this.parseExpr())

            if not this.matches({TOKEN_KIND_RPAREN}):
              this.expects({TOKEN_KIND_COMMA}): this.warn("Expected a comma separating procedure arguments")

          this.expects({TOKEN_KIND_RPAREN}): this.warn("Expected a closing bracket (')') for procedure call")
        else:
          result = Node(kind: NODE_KIND_REFERENCE_EXPR, location: this.read().location)

          this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for variable reference. This may be an internal error")

          result.referenceIdentifier = this.read(-1).lexeme
    else:
      this.warn("Unexpected token expected a literal value, opening bracket, unary operator or identifier")
  else:
    result = this.parseExpr(primary=true)

    while not this.eos() and this.read().kind in BINARY_OPERATOR_TOKEN_KINDS and OPERATORS[this.read().kind].bop.precedence >= prec:
      result = Node(kind: NODE_KIND_BINARY_EXPR, location: this.read().location, bopOperand1: result)

      this.expects(BINARY_OPERATOR_TOKEN_KINDS): this.warn("Expected a binary operator. This may be an internal error")

      result.bopOperator = this.read(-1).kind

      result.bopOperand2 = this.parseExpr(prec=
        if OPERATORS[result.bopOperator].bop.isRightAssociative:
          OPERATORS[result.bopOperator].bop.precedence + 0
        else:
          OPERATORS[result.bopOperator].bop.precedence + 1
      )

proc parseStmt(this: Parser, local: bool = true): Node =
  if local:
    case this.read().kind:
    of TOKEN_KIND_LBRACE:
      result = Node(kind: NODE_KIND_BLOCK_STMT, location: this.read().location, blockBody: newSeq[Node]())

      this.expects({TOKEN_KIND_LBRACE}): this.warn("Expected an opening bracket ('{') for block statement. This may be an internal error")
      
      while not this.eos() and not this.matches({TOKEN_KIND_RBRACE}):
        result.blockBody.add(this.parseStmt())
    
      this.expects({TOKEN_KIND_RBRACE}): this.warn("Expected a closing bracket ('}') for block statement")
    of TOKEN_KIND_RAW:
      result = Node(kind: NODE_KIND_RAW_STMT, location: this.read().location)

      result.rawIsGlobal = false

      this.expects({TOKEN_KIND_RAW}): this.warn("Expected a 'raw' key word. This may be an internal error")
      this.expects({TOKEN_KIND_STRING}): this.warn("Expected a string for raw definition")

      result.rawBody = this.read(-1).lexeme

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (':') after raw definition")
    of {TOKEN_KIND_VAR, TOKEN_KIND_VAL}:
      result = Node(kind: NODE_KIND_VARIABLE_DEF_STMT, location: this.read().location)

      result.variableIsGlobal = false

      this.expects({TOKEN_KIND_VAR, TOKEN_KIND_VAL}): this.warn("Expected a 'var' or 'val' key word. This may be an internal error")

      result.variableIsConstant = this.read(-1).kind == TOKEN_KIND_VAL

      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifer for variable definition")

      result.variableIdentifier = this.read(-1).lexeme

      this.expects({TOKEN_KIND_COLON}): this.warn("Expected a colon (':') between variable identifier and type")
      
      result.variableKind = this.parseRslType()
      
      if result.variableIsConstant or this.matches({TOKEN_KIND_ASSIGN}):
          this.expects({TOKEN_KIND_ASSIGN}): this.warn("Expected an assign ('=') between variable identifier and value. This may be an internal error")
          
          if this.matches({TOKEN_KIND_LBRACE}):
            result.variableValue = Node(kind: NODE_KIND_STRUCTURE_INIT_EXPR, location: this.read().location, initArguments: newSeq[Node]())

            this.expects({TOKEN_KIND_LBRACE}): this.warn("Expected an opening bracket ('{') for structure initialization. This may be an internal error")

            while not this.eos() and not this.matches({TOKEN_KIND_RBRACE}):
              result.variableValue.initArguments.add(this.parseExpr())

              if not this.matches({TOKEN_KIND_RBRACE}):
                this.expects({TOKEN_KIND_COMMA}): this.warn("Expected a comma (',') between structure arguments")

            this.expects({TOKEN_KIND_RBRACE}): this.warn("Expected an closing bracket ('}') after structure arguments")
          else:      
            result.variableValue = this.parseExpr()

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (':') after variable definition")
    of {TOKEN_KIND_STRUCT, TOKEN_KIND_UNION}:
      result = Node(kind: NODE_KIND_STRUCTURE_DEF_STMT, location: this.read().location, structureFields: newSeq[tuple[identifier: string, kind: RslType]]())

      result.structureIsGlobal = false

      this.expects({TOKEN_KIND_STRUCT, TOKEN_KIND_UNION}): this.warn("Expected a 'struct' or 'union' key word. This may be an internal error")
      
      result.structureKind = this.read(-1).lexeme

      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for a structure definition")

      result.structureIdentifier = this.read(-1).lexeme

      this.expects({TOKEN_KIND_ASSIGN}): this.warn("Expected an assign ('=') between structure identifier and body")

      this.expects({TOKEN_KIND_LBRACE}): this.warn("Expected a opening bracket ('{') for structure body")
      
      while not this.eos() and not this.matches({TOKEN_KIND_RBRACE}):
        this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for structure field")

        let identifier = this.read(-1).lexeme

        this.expects({TOKEN_KIND_COLON}): this.warn("Expected a colon (':') between structure field identifier and type")
        
        var kind = this.parseRslType()
        
        if kind.identifier() == result.structureIdentifier:
          kind = RslType(kind: RSL_TYPE_KIND_STRUCTURE, structureKind: result.structureKind, structureOf: kind)

        result.structureFields.add((identifier, kind))
      
        if not this.matches({TOKEN_KIND_RBRACE}):
          this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') between structure fields")

      this.expects({TOKEN_KIND_RBRACE}): this.warn("Expected a closing bracket ('}') after structure body")
    of TOKEN_KIND_IF:
      result = Node(kind: NODE_KIND_IF_STMT, location: this.read().location, elifs: newSeq[tuple[condition: Node, body: Node]]())
    
      this.expects({TOKEN_KIND_IF}): this.warn("Expected an 'if' key word. This may be an internal error")

      result.ifCondition = this.parseExpr()

      result.ifBody = this.parseStmt()

      while this.matches({TOKEN_KIND_ELIF}):
        this.expects({TOKEN_KIND_ELIF}): this.warn("Expected an 'elif' key word. This may be an internal error")

        let condition = this.parseExpr()

        let body = this.parseStmt()

        result.elifs.add((condition, body))

      if this.matches({TOKEN_KIND_ELSE}):
        this.expects({TOKEN_KIND_ELSE}): this.warn("Expected an 'else' key word. This may be an internal error")
        
        result.elseBody = this.parseStmt()
    of TOKEN_KIND_CASE:
      result = Node(kind: NODE_KIND_SWITCH_STMT, location: this.read().location, switchCases: newSeq[tuple[matches: Node, body: Node]]())
    
      this.expects({TOKEN_KIND_CASE}): this.warn("Expected a 'case' key word. This may be an internal error")
      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an variable identifier for case statement")

      result.switchIdentifier = this.read(-1).lexeme

      while not this.eos() and this.matches({TOKEN_KIND_OF}):
        this.expects({TOKEN_KIND_OF}): this.warn("Expected an 'of' key word in case statement. This may be an internal error")
        this.expects(UNARY_OPERATOR_TOKEN_KINDS + CONSTANT_LITERAL_TOKEN_KINDS): this.warn("Expected a constant for case")

        let matches = this.parseExpr(primary=true)

        let body = this.parseStmt()

        result.switchCases.add((matches, body))
    of TOKEN_KIND_WHILE:
      result = Node(kind: NODE_KIND_WHILE_STMT, location: this.read().location)
    
      this.expects({TOKEN_KIND_WHILE}): this.warn("Expected a 'while' key word. This may be an internal error")

      result.whileCondition = this.parseExpr()
      result.whileBody = this.parseStmt()
    of TOKEN_KIND_RETURN:
      result = Node(kind: NODE_KIND_RETURN_STMT, location: this.read().location)
    
      this.expects({TOKEN_KIND_RETURN}): this.warn("Expected a 'return' key word. This may be an internal error")

      result.returnBody = Node(kind: NODE_KIND_EXPR_STMT, location: this.read().location, stmtExpr: this.parseExpr())
      
      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') after return statement")
    of TOKEN_KIND_BREAK:
      result = Node(kind: NODE_KIND_BREAK_STMT, location: this.read().location)
      
      this.expects({TOKEN_KIND_BREAK}): this.warn("Expected a 'break' key word. This may be an internal error")

      if not this.matches({TOKEN_KIND_SEMI}):
        this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier or nothing in break statement")
        
        result.continueIdentifier = this.read(-1).lexeme
    
      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') after break statement")
    of TOKEN_KIND_CONTINUE:
      result = Node(kind: NODE_KIND_CONTINUE_STMT, location: this.read().location)
      
      this.expects({TOKEN_KIND_CONTINUE}): this.warn("Expected a 'continue' key word. This may be an internal error")

      if not this.matches({TOKEN_KIND_SEMI}):
        this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier or nothing in continue statement")
        
        result.continueIdentifier = this.read(-1).lexeme
    
      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') after continue statement")
    of TOKEN_KIND_IDENTIFIER:
      result = Node(kind: NODE_KIND_EXPR_STMT, location: this.read().location)

      result.stmtExpr = this.parseExpr()

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') after expression statement")
    else:
      this.warn("Unexpected token expected a 'block', 'if', 'case', 'while', 'return', 'break', 'continue' statement or variable or constant definition")
  else:
    case this.read().kind:
    of TOKEN_KIND_LBRACE:
      result = Node(kind: NODE_KIND_BLOCK_STMT, location: this.read().location, blockBody: newSeq[Node]())

      this.expects({TOKEN_KIND_LBRACE}): this.warn("Expected an opening bracket ('{') for block statement. This may be an internal error")
      
      while not this.eos() and not this.matches({TOKEN_KIND_RBRACE}):
        result.blockBody.add(this.parseStmt(local=false))

      this.expects({TOKEN_KIND_RBRACE}): this.warn("Expected a closing bracket ('}') for block statement")
    of TOKEN_KIND_PACKAGE:
      result = Node(kind: NODE_KIND_PACKAGE_STMT, location: this.read().location)

      this.expects({TOKEN_KIND_PACKAGE}): this.warn("Expected a 'package' key word. This may be an internal error")

      result.packageIdentifier = this.parseQualifiedName()

      this.state.packageIdentifier = result.packageIdentifier

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected an semicolon (';') after package statement")
    of TOKEN_KIND_INCLUDE:
      result = Node(kind: NODE_KIND_INCLUDE_STMT, location: this.read().location)

      this.expects({TOKEN_KIND_INCLUDE}): this.warn("Expected an 'include' key word. This may be an internal error")
      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier in include statement")

      result.includeIdentifier = this.read(-1).lexeme

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected an semicolon (';') after include statement")
    of TOKEN_KIND_DEFINE:
      result = Node(kind: NODE_KIND_DEFINE_STMT, location: this.read().location)

      this.expects({TOKEN_KIND_DEFINE}): this.warn("Expected an 'define' key word. This may be an internal error")
      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an name identifier in define statement")

      result.defineIdentifier = this.read(-1).lexeme

      result.defineKind = this.parseRslType()

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected an semicolon (';') after define statement")  
    of TOKEN_KIND_RAW:
      result = Node(kind: NODE_KIND_RAW_STMT, location: this.read().location)
      
      result.rawIsGlobal = true
      
      this.expects({TOKEN_KIND_RAW}): this.warn("Expected a 'raw' key word. This may be an internal error")
      this.expects({TOKEN_KIND_STRING}): this.warn("Expected a string for raw definition")

      result.rawBody = this.read(-1).lexeme

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (':') after raw definition")
    of {TOKEN_KIND_VAR, TOKEN_KIND_VAL}:
      result = Node(kind: NODE_KIND_VARIABLE_DEF_STMT, location: this.read().location)

      result.variableIsGlobal = true

      this.expects({TOKEN_KIND_VAR, TOKEN_KIND_VAL}): this.warn("Expected a 'var' or 'val' key word. This may be an internal error")

      result.variableIsConstant = this.read(-1).kind == TOKEN_KIND_VAL

      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifer for variable definition")

      result.variableIdentifier = this.read(-1).lexeme

      this.expects({TOKEN_KIND_COLON}): this.warn("Expected a colon (':') between variable identifier and type")
      
      result.variableKind = this.parseRslType()
      
      if result.variableIsConstant or this.matches({TOKEN_KIND_ASSIGN}):
          this.expects({TOKEN_KIND_ASSIGN}): this.warn("Expected an assign ('=') between variable identifier and value. This may be an internal error")
          
          if this.matches({TOKEN_KIND_LBRACE}):
            result.variableValue = Node(kind: NODE_KIND_STRUCTURE_INIT_EXPR, location: this.read().location, initArguments: newSeq[Node]())

            this.expects({TOKEN_KIND_LBRACE}): this.warn("Expected an opening bracket ('{') for structure initialization. This may be an internal error")

            while not this.eos() and not this.matches({TOKEN_KIND_RBRACE}):
              result.variableValue.initArguments.add(this.parseExpr())

              if not this.matches({TOKEN_KIND_RBRACE}):
                this.expects({TOKEN_KIND_COMMA}): this.warn("Expected a comma (',') between structure arguments")

            this.expects({TOKEN_KIND_RBRACE}): this.warn("Expected an closing bracket ('}') after structure arguments")
          else:      
            result.variableValue = this.parseExpr()

      this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (':') after variable definition")
    of TOKEN_KIND_PROC:
      result = Node(kind: NODE_KIND_PROCEDURE_DEF_STMT, location: this.read().location, procedureParameters: newSeq[tuple[identifier: string, kind: RslType]]())

      this.expects({TOKEN_KIND_PROC}): this.warn("Expected a 'proc' key word. This may be an internal error")
      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for a procedure definition")

      result.procedureIdentifier = this.read(-1).lexeme

      this.expects({TOKEN_KIND_LPAREN}): this.warn("Expected opening bracket ('(') before procedure parameters")

      while not this.eos() and not this.matches({TOKEN_KIND_RPAREN}):
        this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier in procedure parameters")
    
        let identifier = this.read(-1).lexeme

        this.expects({TOKEN_KIND_COLON}): this.warn("Expected a colon (':') between procedure parameter identifier and type")

        let kind = this.parseRslType()

        result.procedureParameters.add((identifier: identifier, kind: kind))
        
        if not this.matches({TOKEN_KIND_RPAREN}):
          this.expects({TOKEN_KIND_COMMA}): this.warn("Expected a comma (',') between procedure parameters")

      this.expects({TOKEN_KIND_RPAREN}): this.warn("Expected a closing bracket (')') after procedure parameters")

      this.expects({TOKEN_KIND_COLON}): this.warn("Expected a colon (':') between procedure parameters and return type")

      result.procedureReturnKind = this.parseRslType()

      this.expects({TOKEN_KIND_ASSIGN}): this.warn("Expected an assign ('=') between procedure return type and body")

      if this.matches(EXPR_FIRSTS):
        result.procedureBody = Node(kind: NODE_KIND_RETURN_STMT, location: this.read().location)

        result.procedureBody.returnBody = Node(kind: NODE_KIND_EXPR_STMT, location: this.read().location, stmtExpr: this.parseExpr())

        this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') after single statement procedure")
      else:
        result.procedureBody = this.parseStmt()
    of {TOKEN_KIND_STRUCT, TOKEN_KIND_UNION}:
      result = Node(kind: NODE_KIND_STRUCTURE_DEF_STMT, location: this.read().location, structureFields: newSeq[tuple[identifier: string, kind: RslType]]())

      result.structureIsGlobal = true
      
      this.expects({TOKEN_KIND_STRUCT, TOKEN_KIND_UNION}): this.warn("Expected a 'struct' or 'union' key word. This may be an internal error")
      
      result.structureKind = this.read(-1).lexeme

      this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for a structure definition")

      result.structureIdentifier = this.read(-1).lexeme

      this.expects({TOKEN_KIND_ASSIGN}): this.warn("Expected an assign ('=') between structure identifier and body")

      this.expects({TOKEN_KIND_LBRACE}): this.warn("Expected a opening bracket ('{') for structure body")
      
      while not this.eos() and not this.matches({TOKEN_KIND_RBRACE}):
        this.expects({TOKEN_KIND_IDENTIFIER}): this.warn("Expected an identifier for structure field")

        let identifier = this.read(-1).lexeme

        this.expects({TOKEN_KIND_COLON}): this.warn("Expected a colon (':') between structure field identifier and type")
        
        var kind = this.parseRslType()
        
        if kind.identifier() == result.structureIdentifier:
          kind = RslType(kind: RSL_TYPE_KIND_STRUCTURE, structureKind: result.structureKind, structureOf: kind)

        result.structureFields.add((identifier, kind))
      
        if not this.matches({TOKEN_KIND_RBRACE}):
          this.expects({TOKEN_KIND_SEMI}): this.warn("Expected a semicolon (';') between structure fields")

      this.expects({TOKEN_KIND_RBRACE}): this.warn("Expected a closing bracket ('}') after structure body")
    else:
      this.warn("Unexpected token expected a 'block', 'include', 'define' statement or a procedure, struct, constant or variable definition")

proc parse(this: Parser): void =
  while not this.eos():
    this.output.programBody.add(this.parseStmt(local=false))

####################
#    Transpiler    #
####################

const STAGE_NONE =              0x00
const STAGE_PREPROCESSING =     0x02
const STAGE_PROCESSING =        0x04
const STAGE_POSTPROCESSING =    0x08
const STAGE_ALL =               0xFF

type Transpiler = ref object
  state: State
  
  stage: int
  indentation: int
  
  input: Node
  output: array[2, string]

proc newTranspiler(state: State, input: Node): Transpiler =
  result = Transpiler()

  result.state = state

  result.stage = STAGE_PREPROCESSING
  result.indentation = 0

  result.input = input
  result.output = ["", ""]

const C_OPERATOR_LEXEMES = toTable({
  TOKEN_KIND_PTR:           "*",
  TOKEN_KIND_ADDR:          "&",

  TOKEN_KIND_MUL:           "*",
  TOKEN_KIND_DIV:           "/",
  TOKEN_KIND_MOD:           "%",

  TOKEN_KIND_ADD:           "+",
  TOKEN_KIND_SUB:           "-",

  TOKEN_KIND_GREATER:       ">",
  TOKEN_KIND_GREATER_EQUAL: ">=",
  TOKEN_KIND_LESSER:        "<",
  TOKEN_KIND_LESSER_EQUAL:  "<=",

  TOKEN_KIND_EQUAL:         "==",
  TOKEN_KIND_NOT_EQUAL:     "!=",

  TOKEN_KIND_SHL:           "<<",
  TOKEN_KIND_SHR:           ">>",
  TOKEN_KIND_USHL:          "<<",
  TOKEN_KIND_USHR:          ">>>",

  TOKEN_KIND_BITWISE_NOT:   "~",
  TOKEN_KIND_BITWISE_AND:   "&",
  TOKEN_KIND_BITWISE_XOR:   "^",
  TOKEN_KIND_BITWISE_OR:    "|",

  TOKEN_KIND_LOGICAL_NOT:   "!",
  TOKEN_KIND_LOGICAL_AND:   "&&",
  TOKEN_KIND_LOGICAL_XOR:   "THIS SHOULD NOT BE SEEN IN CODE",
  TOKEN_KIND_LOGICAL_OR:    "||",

  TOKEN_KIND_ASSIGN:        "=",
})

template indent(this: Transpiler): void = this.indentation.inc()
template dedent(this: Transpiler): void = this.indentation.dec()

template during(this: Transpiler, stages: int, body: untyped): void =
  if (stages and this.stage) > 1:
    body

proc process(this: Transpiler, node: Node): string =
  result = ""

  case node.kind:
  of NODE_KIND_BLOCK_STMT:
    this.during(STAGE_PREPROCESSING or STAGE_PROCESSING):
      result &= "{\n"

      this.indent()

      for child in node.blockBody:
        result &= this.process(child).indent(this.indentation, "\t") & "\n"

      this.dedent()
      
      result &= "}\n"
  of NODE_KIND_INCLUDE_STMT:
    this.during(STAGE_PREPROCESSING):
      result &= "#include \"" & node.includeIdentifier & ".h\"\n"
  of NODE_KIND_DEFINE_STMT:
    this.during(STAGE_PREPROCESSING):
      result &= "#define " & node.defineIdentifier & " " & $node.defineKind & "\n"
  of NODE_KIND_RAW_STMT:
    this.during(STAGE_PREPROCESSING):
      result &= node.rawBody[1..node.rawBody.len()-2] & "\n"
    
    this.during(STAGE_PROCESSING):
      if not node.rawIsGlobal:
        result &= node.rawBody[1..node.rawBody.len()-2] & "\n"
  of NODE_KIND_VARIABLE_DEF_STMT:
    this.during(STAGE_PREPROCESSING):
      if node.variableIsConstant:
        result &= "const "

      result &= $node.variableKind.toVariable(node.variableIdentifier)
      
      if node.variableValue != nil:
        result &= " = " & this.process(node.variableValue)
      
      result &= ";\n"

    this.during(STAGE_PROCESSING):
      if not node.variableIsGlobal:
        if node.variableIsConstant:
          result &= "const "

        result &= $node.variableKind.toVariable(node.variableIdentifier)
        
        if node.variableValue != nil:
          result &= " = " & this.process(node.variableValue)
        
        result &= ";\n"
  of NODE_KIND_PROCEDURE_DEF_STMT:
    this.during(STAGE_PREPROCESSING or STAGE_PROCESSING):
      result &= $node.procedureReturnKind & " " & node.procedureIdentifier & "("
      
      for i, param in node.procedureParameters:
        result &= param.kind.toVariable(param.identifier)
      
        if i < high node.procedureParameters:
          result &= ", "

    this.during(STAGE_PREPROCESSING):
      result &= ");\n"
    
    this.during(STAGE_PROCESSING):
      result &= ") {\n"
      
      this.indent()
    
      if node.procedureBody.kind == NODE_KIND_BLOCK_STMT:
        for child in node.procedureBody.blockBody:
          result &= this.process(child).indent(this.indentation, "\t") & "\n"
      else:
        result &= this.process(node.procedureBody).indent(this.indentation, "\t") & "\n"
      
      this.dedent()
      
      result &= ("}\n")
  of NODE_KIND_STRUCTURE_DEF_STMT:
    this.during(STAGE_PREPROCESSING):
      result &= "typedef " & node.structureKind & " " & node.structureIdentifier & " {\n"

      this.indent()

      for field in node.structureFields:
        result &= (field.kind.toVariable(field.identifier) & ";").indent(this.indentation, "\t") & "\n"
        
      this.dedent()

      result &= "} " & node.structureIdentifier & ";\n"

    this.during(STAGE_PROCESSING):
      if not node.structureIsGlobal:
        result &= "typedef " & node.structureKind & " " & node.structureIdentifier & " {\n"

        this.indent()

        for field in node.structureFields:
          result &= (field.kind.toVariable(field.identifier) & ";").indent(this.indentation, "\t") & "\n"
        
        this.dedent()

        result &= "} " & node.structureIdentifier & ";\n"
  of NODE_KIND_IF_STMT:
    result &= "if ("
      
    if node.ifCondition.kind == NODE_KIND_GROUPING_EXPR:
      result &= this.process(node.ifCondition.groupingExpr)
    else:
      result &= this.process(node.ifCondition)
    
    result &= ") {\n"

    this.indent()

    if node.ifBody.kind == NODE_KIND_BLOCK_STMT:
      for child in node.ifBody.blockBody:
        result &= this.process(child).indent(this.indentation, "\t") & "\n"
    else:
      result &= this.process(node.ifBody).indent(this.indentation, "\t") & "\n"
    
    this.dedent()

    result &= "}"

    for child in node.elifs:
      result &= " else if ("
      
      if child.condition.kind == NODE_KIND_GROUPING_EXPR:
        result &= this.process(child.condition.groupingExpr)
      else:
        result &= this.process(child.condition)
      
      result &= ") {\n"
      
      this.indent()
      
      if child.body.kind == NODE_KIND_BLOCK_STMT:
        for khild in child.body.blockBody:
          result &= this.process(khild).indent(this.indentation, "\t") & "\n"
      else:
        result &= this.process(child.body).indent(this.indentation, "\t") & "\n"

      this.dedent()

      result &= "}"

    if node.elseBody != nil:
      result &= " else {\n"
      
      this.indent()

      if node.elseBody.kind == NODE_KIND_BLOCK_STMT:
        for child in node.elseBody.blockBody:
          result &= this.process(child).indent(this.indentation, "\t") & "\n"
      else:
        result &= this.process(node.elseBody).indent(this.indentation, "\t") & "\n"

      this.dedent()

      result &= "}"
  of NODE_KIND_SWITCH_STMT:
    result &= "switch (" & node.switchIdentifier & ") {\n"

    this.indent()

    for match in node.switchCases:
      result &= "case " & this.process(match.matches) & ":\n"

      if match.body.kind == NODE_KIND_BLOCK_STMT:
        for child in match.body.blockBody:
          result &= this.process(child).indent(this.indentation, "\t") & "\n"
      else:
        result &= this.process(match.body).indent(this.indentation, "\t") & "\n"

      result &= ("break;\n").indent(this.indentation, "\t")

    this.dedent()

    result &= "}"
  of NODE_KIND_WHILE_STMT:
    result &= "while ("
    
    if node.whileCondition.kind == NODE_KIND_GROUPING_EXPR:
      result &= this.process(node.whileCondition.groupingExpr)
    else:
      result &= this.process(node.whileCondition)
    
    result &= ") {\n"

    this.indent()
    
    if node.whileBody.kind == NODE_KIND_BLOCK_STMT:
      for child in node.whileBody.blockBody:
        result &= this.process(child).indent(this.indentation, "\t") & "\n"
    else:
      result &= this.process(node.whileBody).indent(this.indentation, "\t") & "\n"

    this.dedent()

    result &= "}"
  of NODE_KIND_RETURN_STMT:
    result &= "return " & this.process(node.returnBody)
  of NODE_KIND_BREAK_STMT:
    result &= "break " & node.breakIdentifier & ";"
  of NODE_KIND_CONTINUE_STMT:
    result &= "continue " & node.continueIdentifier & ";"
  of NODE_KIND_EXPR_STMT:
    result &= this.process(node.stmtExpr) & ";"
  of NODE_KIND_GROUPING_EXPR:
    result = "(" & this.process(node.groupingExpr) & ")"
  of NODE_KIND_BINARY_EXPR:
    case node.bopOperator:
    of TOKEN_KIND_LOGICAL_XOR:
      result &= "!" & this.process(node.bopOperand1) & " != !" & this.process(node.bopOperand2)
    else:
      result &= this.process(node.bopOperand1) & " " & C_OPERATOR_LEXEMES[node.bopOperator] & " " & this.process(node.bopOperand2)
  of NODE_KIND_UNARY_EXPR:
    result &= C_OPERATOR_LEXEMES[node.uopOperator] & this.process(node.uopOperand1)
  of NODE_KIND_LITERAL_EXPR:
    if node.literalKind == TOKEN_KIND_BOOLEAN:
      if node.literalValue == "false":
        result &= "0"
      else:
        result &= "1"
    else: 
      result &= node.literalValue
  of NODE_KIND_REFERENCE_EXPR:
    result &= node.referenceIdentifier
  of NODE_KIND_FIELD_ACCESS_EXPR:
    result &= node.fieldParent & "." & node.fieldIdentifier
  of NODE_KIND_ARRAY_ACCESS_EXPR:
    result &= node.arrayIdentifier & "[" & this.process(node.arrayIndex) & "]"
  of NODE_KIND_PROC_CALL_EXPR:
    result &= node.callIdentifier & "("
    
    for i, arg in node.callArguments:
      result &= this.process(arg)
    
      if i < high node.callArguments:
        result &= ", "

    result &= ")"
  of NODE_KIND_STRUCTURE_INIT_EXPR:
    result &= "{"

    for i, arg in node.initArguments:
      result &= this.process(arg)

      if i < high node.initArguments:
        result &= ", "

    result &= "}"
  else: discard

proc transpile(this: Transpiler): void =
  this.output[1] &= "#ifndef " & (this.state.packageIdentifier & "_H").replace(".", "_").toUpper() & "\n"
  this.output[1] &= "#define " & (this.state.packageIdentifier & "_H").replace(".", "_").toUpper() & "\n"
  
  this.stage = STAGE_PREPROCESSING

  for child in this.input.programBody:
    this.output[1] &= this.process(child)

  this.output[1] &= "#endif\n"

  this.output[0] &= "#include \"" & this.state.sourceName.changeFileExt(".h") & "\"\n"

  this.stage = STAGE_PROCESSING
  
  for child in this.input.programBody:
    this.output[0] &= this.process(child)

###############
#    Tests    #
###############

when DEBUG_MODE:
  var errors = false

  for kind in BINARY_OPERATOR_TOKEN_KINDS:
    if kind notin OPERATORS:
      echo "No operator defined for binary " & $kind
  
      errors = true

  for kind in UNARY_OPERATOR_TOKEN_KINDS:
    if kind notin OPERATORS:
      echo "No operator defined for unary " & $kind
  
      errors = true

  for kind in BINARY_OPERATOR_TOKEN_KINDS:
    if kind notin C_OPERATOR_LEXEMES:
      echo "No C operator defined for binary " & $kind
  
      errors = true

  for kind in UNARY_OPERATOR_TOKEN_KINDS:
    if kind notin C_OPERATOR_LEXEMES:
      echo "No C operator defined for unary " & $kind
  
      errors = true

  if errors: quit(QUIT_FAILURE)

#####################
#    Entry Point    #
#####################

when isMainModule:
  let options = commandLineParams()

  if (high options) < 0:
    echo "Too few args"
    quit(QUIT_FAILURE)

  if not fileExists(options[0]):
    echo "Too few args"
    quit(QUIT_FAILURE)

  let lexer = newLexer(options[0], readFile(options[0]))

  lexer.tokenize()

  when DEBUG_MODE:
    for token in lexer.output:
      echo token

  let parser = newParser(lexer.state, lexer.output)

  parser.parse()

  when DEBUG_MODE:
    echo (%parser.output).pretty()

  let transpiler = newTranspiler(parser.state, parser.output)

  transpiler.transpile()

  when DEBUG_MODE:
    echo transpiler.output[0]
    echo transpiler.output[1]

  writeFile(transpiler.state.sourceName.changeFileExt(".c"), transpiler.output[0])
  writeFile(transpiler.state.sourceName.changeFileExt(".h"), transpiler.output[1])
