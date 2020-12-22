grammar MiniJava;

program             :   main=mainClass ';' (others+=classDeclaration);

Identifier          :   [a-zA-Z_][0-9a-zA-Z_]*;

mainClass           :   'class' className=Identifier '{' 'public' 'static' 'void' 'main' '(' 'String' '[' ']' argName=Identifier ')' '{' body=statement '}' '}';
classDeclaration    :   'class' className=Identifier ( 'extends' superClass=Identifier )? '{' vars+=varDeclaration* methods+=methodDeclaration* '}';
methodDeclaration   :   'public' returnType=type methodName=Identifier '(' paramList=parameterList? ')' '{' vars+=varDeclaration* stmts+=statement* '}';

type                :    'int' # IntType
                    |    'int''['']' # ArrayType
                    |    'boolean' # BoolType
                    |    Identifier # ClassType;

statement           :    '{' statement* '}' # Block
                    |    'if' '(' cond=expression ')' thenBlock=statement 'else' elseBlock=statement # If
                    |    'while' '(' cond=expression ')' block=statement # While
                    |    'System.out.println' '(' toPrint=expression ')' ';' # Println
                    |    Identifier '=' value=expression ';' # Assign
                    |    Identifier '[' index=expression ']' '=' arrValue=expression ';' # ArrayAssign
                    |    'return' value=expression ';' #Return;

expression          :    lenExp=expression '.length' # Length
                    |    arrExp=expression '[' indexExp=expression ']' # Index
                    |    obj=expression '.' fn=Identifier '(' ( args+=expression ( ',' args+=expression )* )? ')' # Call
                    |    left=expression op=Relation right=expression # BinOP
                    |    'this' # This
                    |    'new' 'int' '[' arrSize=expression ']' # NewArray
                    |    'new' newType=Identifier '(' ')' # New
                    |    '!' exp=expression # Negate
                    |    '(' expression ')' # List
                    |    IntegerLiteral # Int
                    |    Decimal # Decimal
                    |    Boolean # Bool
                    |    Identifier # Identifer;

parameter           :   paramType=type paramName=Identifier;
varDeclaration      :   varType=type varName=Identifier ';';
parameterList       :   params+=parameter (',' params+=parameter)*;

Boolean             :   'true' | 'false';
Relation            :   '**' | '*' | '/' | '+' | '-' | '>' | '<' | '=' | '&&' | '||';
IntegerLiteral      :   '0' | [1-9][0-9]*;
Decimal             :   IntegerLiteral? '.' [0-9]*;
WhiteSpace          :   [ \r\t\n]+ -> skip;
MULTILINE_COMMENT   :   '/*' .*? '*/' -> skip;
LINE_COMMENT        :   '//' .*? '\n' -> skip;

