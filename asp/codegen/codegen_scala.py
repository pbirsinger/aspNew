from ast import *
from ast_tools import *
import scala_ast
import time

BOOLOP_SYMBOLS = {
    And:        'and',
    Or:         'or'
}

BINOP_SYMBOLS = {
    Add:        '+',
    Sub:        '-',
    Mult:       '*',
    Div:        '/',
    FloorDiv:   '//',
    Mod:        '%',
    LShift:     '<<',
    RShift:     '>>',
    BitOr:      '|',
    BitAnd:     '&',
    BitXor:     '^'
}

CMPOP_SYMBOLS = {
    Eq:         '==',
    Gt:         '>',
    GtE:        '>=',
    In:         'in',
    Is:         'is',
    IsNot:      'is not',
    Lt:         '<',
    LtE:        '<=',
    NotEq:      '!=',
    NotIn:      'not in'
}

UNARYOP_SYMBOLS = {
    Invert:     '~',
    Not:        'not',
    UAdd:       '+',
    USub:       '-'
}

TYPES = {
    'int' : 'Int',
    'float': 'Float',
    'double': 'Double',
    'string': 'String', 
    'boolean': 'Boolean',
    'null': 'Unit'
    }

"""
POSSIBLE TYPES:
int
float
double
string
(array, type) i.e. (array, int)
(tuple, type, type [,type..]) i.e. (tuple, int, int)
boolean
specific class name
null
"""

ALL_SYMBOLS = {}
ALL_SYMBOLS.update(BOOLOP_SYMBOLS)
ALL_SYMBOLS.update(BINOP_SYMBOLS)
ALL_SYMBOLS.update(CMPOP_SYMBOLS)
ALL_SYMBOLS.update(UNARYOP_SYMBOLS)


def to_source(node):
    generator = SourceGenerator()
    generator.visit(node)
    return ''.join(generator.result)

class SourceGenerator(NodeVisitor):
    def __init__(self, func_types):
        self.result = []
        self.new_lines = 0
        self.indentation =0
        self.indent_with=' ' * 4
        self.stored_vals = {}
        self.current_func = ''
        self.prev_func = ''
        self.vars = {}       
        self.types = {}
        self.subl_count = 0
        self.set_func_types(func_types)

    
    def to_source(self, node):
        self.result = []
        self.visit(node)
        return ''.join(self.result)      
     
    def add_func_type(self, type):
        self.types.append(type)
             
    def already_def(self, var):
        if self.current_func in self.vars.keys():
            if var in self.vars[self.current_func]:
                return True
            else: 
                return False
    
    def store_var(self,var):
        if self.current_func in self.vars.keys():
            self.vars[self.current_func].append(var)
        else: self.vars[self.current_func] = [var]            
    
    def write(self,x):
        if self.new_lines:
            if self.result:
                self.result.append('\n' * self.new_lines)
            self.result.append(self.indent_with * self.indentation)

            self.new_lines = 0
        self.result.append(x)
        
    def newline(self, node=None, extra=0):
        if (self.new_lines == -1):
            self.new_lines = 0
        elif (self.new_lines == -2):
            self.new_lines =-2
        else:
           self.new_lines = max(self.new_lines, 1 + extra)

    def body(self, statements):
        self.new_line = True
        self.indentation += 1
        for stmt in statements:
            self.visit(stmt)
        self.indentation -= 1
        
    def visit_Number(self, node):
        self.write(repr(node.num))

    def visit_String(self, node):
        self.write('"')
        self.write(node.text)
        self.write('"')
    
    def evaluate_special_name(self,node):
        if node.name == "False":
            self.write("false")
        elif node.name == "True":
            self.write("true")
        else:
            return False
        return True

    def visit_Name(self, node):
        if not self.evaluate_special_name(node):
            self.write(node.name)

    def visit_Expression(self, node):
        self.newline(node) #may cause problems in newline()
        self.generic_visit(node)

    def visit_BinOp(self, node):
        if type(node.op) == ast.Pow:
            self.write('math.pow(')
            self.visit(node.left)
            self.write(', ')
            self.visit(node.right)
            self.write(')')
        elif node.op == '*' and (isinstance(node.left, scala_ast.List) or isinstance(node.right, scala_ast.List) ):
            if isinstance(node.left, scala_ast.List):
                self.write("scala_lib.copy_n(")
                self.visit(node.left)
                self.write(",")
                self.visit(node.right)
                self.write(")")
                """
                node.left.elements = node.left.elements * node.right.num
                self.visit(node.left)
                """
            else:
                self.write("scala_lib.copy_n(")
                self.visit(node.right)
                self.write(",")
                self.visit(node.left)
                self.visit(")")
        else:
            self.write('(')
            self.visit(node.left)
            self.write(' ' + node.op + ' ')
            self.visit(node.right)
            self.write(')')
    
    def visit_BoolOp(self,node):
        self.newline(node)
        self.write('(')
        op = BOOLOP_SYMBOLS[type(node.op)]      
        self.visit(node.values[0])
        for i in range(1, len(node.values)): 
            if op == 'and':
                self.write(' && ')
            elif op == 'or':
                self.write(' || ')
            else:
                raise Error("Unsupported BoolOp type")
            self.visit(node.values[i])
        self.write(')')   
    
    def visit_UnaryOp(self, node):
        self.write('(')
        op = UNARYOP_SYMBOLS[type(node.op)] 
        self.write(op)  
        if op == 'not':
            self.write(' ')
        self.visit(node.operand)
        self.write(')')

    def visit_Subscript(self, node):        
        if node.context == 'load':
            if isinstance(node.index, ast.Slice):
                self.visit(node.value)
                self.write('.slice(')
                self.visit(node.index.lower)
                self.write(', ')
                self.visit(node.index.upper)
                self.write(')')
            else:
                self.visit(node.value)
                self.write('(')
                self.visit(node.index)
                self.write(')')
        else: 
            self.visit(node.value)
            self.write('(')
            if isinstance(node.index, ast.Slice):
                raise Exception("Slice assign not supported")
            self.visit(node.index)
            self.write(')')
            #will finish this in assign
        
    def visit_Print(self, node):
        self.newline(node)
        if node.dest:
            self.write('System.err.')
        self.write('println(')
        plus = False
        for t in node.text: 
            if plus: self.write('+" " + ')  
            self.visit(t)
            plus = True
        self.write(')')

    def visit_List(self,node):
        elements = node.elements
        #self.write('scala.collection.mutable.MutableList(')
        self.write('Array(')
        size = len(elements)
        for i in range(size):
            elem = elements[i]
            self.visit(elem)
            if i != size-1:
                self.write(', ')   
        self.write(')')         

    def visit_Attribute(self,node):
        #self.newline(node)
        self.visit(node.value)
        self.write('.' + node.attr)   

    def evaluate_custom_func(self,node):
        if node.func.name == 'range':
            if len(node.args) == 1:
                self.write('Range(0,')
                self.visit(node.args[0])
                self.write(')')
            else:
                self.write('Range(')
                self.visit(node.args[0])
                self.write(",")
                self.visit(node.args[1])
                self.write(")")
        elif node.func.name == 'len':
            self.visit(node.args[0])
            self.write('.length')
        elif node.func.name == 'int':
            self.write('scala_lib.convert_to_int(')
            self.visit(node.args[0])
            self.write(')')
        elif node.func.name == 'str':
            self.visit(node.args[0])
            self.write('.toString')
        elif node.func.name == 'float':
            self.visit(node.args[0])
            self.write('.asInstanceOf[Double]')
        elif node.func.name == 'read_avro_file':
            self.write('(new JAvroInter("res.avro", "args.avro")).readModel(')
            self.visit(node.args[0])
            self.write(')')  
        elif node.func.name == 'zip':
            #only supports zipping together two lists 
            self.write("scala_lib.zip(")
            self.visit(node.args[0])
            self.write(",")
            self.visit(node.args[1])
            self.write(")")
        elif node.func.name == "PriorityQueue":
            #assume priority queue of type double ?
            #otherwise would need to make of type any and then how would i get element of right type out ?
            self.write("(new PriorityQueue[Double]().reverse)")
            #self.write("(new PriorityQueue[Any]().reverse)")
        else:
            return False
        return True

    def evaluate_func(self,node):
        if not self.evaluate_custom_func(node):
            self.visit(node.func)
            self.write('(')
            comma = False
            for a in node.args:
                if comma: self.write(', ')
                self.visit(a)
                comma = True
            self.write(')')         

    def evaluate_custom_attr_func(self,node):
        if node.func.attr == 'append':
            """
            self.visit(node.func.value)            
            self.write(' += (')
            self.visit(node.args[0])
            self.write(')') 
            """
            self.visit(node.func.value)            
            self.write(' = ')
            self.visit(node.func.value)
            self.write('++')
            self.visit(node.args[0]) 
        elif isinstance(node.func.value,scala_ast.Name) and node.func.value.name == "PriorityQueue" and node.func.attr == 'put':
            self.visit(node.args[0])
            self.write(".enqueue(")
            self.visit(node.args[1])
            self.write(")")
        elif isinstance(node.func.value,scala_ast.Name) and node.func.value.name == "PriorityQueue" and node.func.attr == 'get':
            self.visit(node.args[0])
            self.write(".dequeue(")
            self.write(")")       
        elif node.func.attr == "qsize":
            self.visit(node.func.value)
            self.write(".size")
        elif node.func.attr == "sort":
            self.write("scala.util.Sorting.quickSort(")
            self.visit(node.func.value)
            self.write(")")
        else:
            return False
        return True

    def evaluate_attr_func(self,node):
        if not self.evaluate_custom_attr_func(node):
            self.visit(node.func)
            self.write('(')
            comma = False
            for a in node.args:
                if comma: self.write(', ')
                self.visit(a)
                comma = True
            self.write(')')                 
        
    def visit_Call(self,node):
        self.newline(node)             
        if isinstance(node.func,scala_ast.Name):
            self.evaluate_func(node)   
        elif isinstance(node.func,scala_ast.Attribute):
            self.evaluate_attr_func(node)
        
    def visit_Function(self,node):
        self.newline(node)
        self.visit(node.declaration)
        self.write('{ ')        
        self.body(node.body)
        self.current_func = self.prev_func
        self.write("\n}")
    
    def visit_FunctionDeclaration(self,node):
        self.write('def '+node.name+'( ')    
        self.prev_func = self.current_func    
        self.current_func = node.name
        arg_types = self.types[node.name][0]
        ret_type = self.types[node.name][1]
        
        self.visit_Arguments(node.args, arg_types)
        self.write('): %s =' %(ret_type))
        
    def visit_Arguments(self,node, types=None):   
        comma = False     
        for i in range(len(node.args)):
            if comma:self.write(', ')
            arg = node.args[i]
            self.visit(arg)
            if types:
                self.write(': %s' %types[i])
            else:
                self.write(': Any')
            comma = True
    
    def visit_ReturnStatement(self, node):
        self.newline(node)
        self.write('return ')
        self.new_lines = -2
        self.visit(node.retval)
        self.new_lines = 0
        self.new_lines =0
        
    def visit_Compare(self,node):
        self.newline(node,-1)
        self.write('(')
        self.visit(node.left)
        self.write(' %s ' %(node.op))
        self.visit(node.right)
        self.write(')')
    
    def visit_AugAssign(self,node):
        self.newline(node)
        self.visit(node.target)
        self.write(' ' + node.op +'= ')
        self.visit(node.value)
                   
    def visit_Assign(self,node):
        try:
            if node.lvalue.name == 'TYPE_DECS':
                self.visit(node.rvalue)
                return 0
        except: pass      
        self.newline(node)       
        self.stored_vals["lvalue"] = node.lvalue
        if not isinstance(node.lvalue, scala_ast.Subscript) and not isinstance(node.lvalue, scala_ast.Attribute)\
            and not self.already_def(node.lvalue.name):
            self.write('var ')
            self.store_var(node.lvalue.name)
        self.visit(node.lvalue)
        self.write(' = ')       
        self.new_lines = -1
        self.visit(node.rvalue)
        self.new_lines = 0   
    
    def visit_IfConv(self,node): 
        self.newline(node)
        if node.inner_if:
            self.write('else if (')
        else:
            self.write('if(')
        self.visit(node.test)
        self.write(') {')
        self.body(node.body)
        self.newline(node)
        self.write('}')
        
        if node.orelse:
            if not isinstance(node.orelse[0], scala_ast.IfConv):
                self.newline(node)
                self.write('else { ')
                self.body(node.orelse)
                self.newline(node)
                self.write('}')
            else:
                self.visit_IfConv(node.orelse[0])
  
    def visit_For(self,node):
        self.newline(node)
        if isinstance(node.iter, scala_ast.Call):
            self.newline(node)
            self.write('for (')
            self.visit(node.target)
            self.write( ' <- ')
            self.visit(node.iter)
            self.write(') {')
            self.body(node.body)
            self.newline(node)
            self.write('}')
        else:    
            self.write( 'for (i <- Range(0, ')
            self.visit(node.iter)
            self.write('.size)) {')
            self.newline(node)
            self.write('var ')
            self.visit(node.target)
            self.write(' = ')

            self.visit(node.iter)
            self.write('(i)')
            self.body(node.body)
            self.newline(node)
            self.write('}')

        # self.newline(node)
        # current_time = str(int(time.time()*100))
        # self.write('var count' +current_time + ' = 0')
        # self.visit(new Assign("count"+str(int(time.time()*100)), 
        # self.newline(node)
        # self.write('while ( count' +current_time+ " < ")
        # self.visit(node.iter)
        # self.write(".size) {")
        # self.body(node.body)
        # self.newline(node)
        # self.write("count"+current_time+ " += 1")
        # self.newline(node)
        # self.write('}')

    
    def visit_ListComp(self,node):
        self.visit(node.comprehension)
        self.visit(node.elt)
        self.write( " )")

    def visit_Comprehension(self,node):
        #does not support multiple target
        self.newline(node)
        self.visit(node.iter)
        self.write(".map( ")
        self.visit(node.target)
        self.write(" => ")

    def visit_While(self, node):
        self.newline(node)
        self.write('while (')
        #self.new_lines = -1
        self.visit(node.test)
        self.write(') {')
        self.newline(node)
        self.body(node.body)
        self.newline(node)
        self.write('}')
    
    def convert_types(self,input_type):
        if len(input_type) == 2 and input_type[0] == 'array':
            #return 'org.apache.avro.generic.GenericData.Array[%s]' % (convert_types(input_type[1]))
            return 'Array[%s]' %(self.convert_types(input_type[1]))
        elif len(input_type) == 2 and input_type[0] == 'list':
            return 'List[%s]' %(self.convert_types(input_type[1]))
        elif len(input_type) == 3 and input_type[0] == 'tuple':
            str = '('
            for x in input_type[1:]:
                str += self.convert_types(x) +','
            return str[0:-1] + ')'
        
        elif input_type in TYPES:
            return TYPES[input_type]
        else:
            print 'WARNING POTENTIAL SCALA TYPE MISMATCH OF:', input_type
            return input_type
        
    def set_func_types(self,types):
        source = []
        for func in types:
            name = func[0]
            #convert types somewhere?
            scala_arg_types, scala_ret_type = [],[]
            for arg in func[1]:
                scala_arg_types.append(self.convert_types(arg))
            scala_ret_type = self.convert_types(func[2])
            self.types[name] = [scala_arg_types, scala_ret_type] 

    
    
    