#!/usr/bin/env python
import sys
from pyparsing import Literal, Word, Optional, ZeroOrMore, Empty, Group, Combine, Forward, alphas, countedArray

# XQuery Grammar: http://www.w3.org/TR/xquery/

Separator = Literal(";").suppress()

String = Word(alphas+"0123456789.-&;")
StringLiteral = (Literal('"') + String + Literal('"')) | Literal("'") + String + Literal("'")

VersionDecl = Literal("xquery") + Literal("version") + StringLiteral + Optional(Literal("encoding") + StringLiteral) + Separator

# QName
NameStartChar   = Word(alphas + "_")
NameChar        = Word(alphas + "_-.0123456789")
Name            = NameStartChar + ZeroOrMore(NameChar)
NCName          = Combine(Name)
LocalPart       = NCName.setResultsName("localpart")
Prefix          = NCName.setResultsName("prefix")
UnprefixedName  = LocalPart
PrefixedName    = Prefix + Literal(":") + LocalPart
QName           = Combine(PrefixedName | UnprefixedName)
VarName         = QName
VarRef          = Literal("$") + VarName

# Types and occurrence indicators
AnyKindTest           = Literal("node") + Literal("(") + Literal(")")
TextTest              = Literal("text") + Literal("(") + Literal(")")
CommentTest           = Literal("comment") + Literal("(" ")")
PITest                = Literal("processing-instruction") + Literal("(") + Optional(NCName | StringLiteral) + Literal(")")
TypeName              = QName
AttributeName         = QName
AttribNameOrWildcard  = AttributeName | Literal("*")
ElementName           = QName
ElementNameOrWildcard = ElementName | Literal("*")
AttributeDeclaration  = AttributeName
SchemaAttributeTest   = Literal("schema-attribute") + Literal("(") + AttributeDeclaration + Literal(")")
AttributeTest         = Literal("attribute") + Literal("(") + Optional(AttribNameOrWildcard + Optional(Literal(",").suppress() + TypeName)) + Literal(")")
ElementDeclaration    = ElementName
SchemaElementTest     = Literal("schema-element") + Literal("(") + ElementDeclaration + Literal(")")
ElementTest           = Literal("element") + Literal("(") + Optional(ElementNameOrWildcard + Optional(Literal(",").suppress() + TypeName + Optional(Literal("?")))) + Literal(")")
DocumentTest          = Literal("document-node") + Literal("(") + Optional(ElementTest | SchemaElementTest) + Literal(")")
KindTest              = DocumentTest | ElementTest | AttributeTest | SchemaElementTest | SchemaAttributeTest | PITest | CommentTest | TextTest | AnyKindTest
AtomicType            = QName
OccurrenceIndicator   = (Literal("?") | Literal("*") | Literal("+")).setResultsName("occurrence")
ItemType              = (KindTest | (Literal("item") + Literal("(") + Literal(")")) | AtomicType).setResultsName("type")
SequenceType          = (Literal("empty-sequence") + Literal("(") + Literal(")")) | (ItemType + Optional(OccurrenceIndicator))

# Parameters
TypeDeclaration = Literal("as") + SequenceType
Param           = Group(Literal("$").suppress() + QName.setResultsName("name") + Optional(TypeDeclaration))
ParamList       = Param + ZeroOrMore(Literal(",").suppress() + Param)

# Expressions
ExprSingle      = Forward()
Expr            = Forward()
ValidationMode  = Literal("lax") | Literal("strict")
NodeComp        = Literal("is") | Literal("<<") | Literal(">>")
ValueComp       = Literal("eq") | Literal("ne") | Literal("lt") | Literal("le") | Literal("gt") | Literal("ge")
GeneralComp     = Literal("=") | Literal("!=") | Literal("<") | Literal("<=") | Literal(">") | Literal(">=")
ValidateExpr    = Literal("validate") + Optional(ValidationMode) + Literal("{") + Expr + Literal("}")

# TODO: this is yao next step.. impl these and follow the chain
StepExpr            = FilterExpr | AxisStep
RelativePathExpr    = StepExpr + ZeroOrMore((Literal("/") | Literal("//")) + StepExpr)
PathExpr            = (Literal("/") + Optional(RelativePathExpr)) | (Literal("//") + RelativePathExpr) | RelativePathExpr
#PragmaContents     = (Char* - (Char* '#)' Char*)) # TODO: wtf
#S                  = (#x20 | #x9 | #xD | #xA)+ # TODO: wtf
PragmaContents      = Word(alphas) # TODO: just a terrible guess
S                   = Word(alphas) # TODO: just a terrible guess
Pragma              = Literal("(#") + Optional(S) + QName + Optional(S + PragmaContents) + Literal("#)")
ExtensionExpr       = Pragma + Literal("{") + Optional(Expr) + Literal("}")
ValueExpr           = ValidateExpr | PathExpr | ExtensionExpr
UnaryExpr           = ZeroOrMore(Literal("-") | Literal("+")) + ValueExpr
CastExpr            = UnaryExpr + Optional(Literal("cast") + Literal("as") + SingleType)
CastableExpr        = CastExpr + Optional(Literal("castable") + Literal("as") + SingleType)
TreatExpr           = CastableExpr + Optional(Literal("treat") + Literal("as") + SequenceType)
InstanceofExpr      = TreatExpr + Optional(Literal("instance") + Literal("of") + SequenceType)
IntersectExceptExpr = InstanceofExpr + ZeroOrMore((Literal("intersect") | Literal("except")) + InstanceofExpr)
UnionExpr           = IntersectExceptExpr + ZeroOrMore(("union") | Literal("|") + IntersectExceptExpr)
MultiplicativeExpr  = UnionExpr + ZeroOrMore((Literal("*") | Literal("div") | Literal("idiv") | Literal("mod")) + UnionExpr)
AdditiveExpr        = MultiplicativeExpr + ZeroOrMore((("+") | Literal("-")) + MultiplicativeExpr)
RangeExpr           = AdditiveExpr + Optional(Literal("to") + AdditiveExpr)
ComparisonExpr      = RangeExpr + Optional((ValueComp | GeneralComp | NodeComp) + RangeExpr)
AndExpr             = ComparisonExpr + ZeroOrMore(Literal("and") + ComparisonExpr)
OrExpr              = AndExpr + ZeroOrMore(Literal("or") + AndExpr)

QuantifiedExpr  = Literal("some") | Literal("every") + Literal("$") + VarName + Optional(TypeDeclaration) + Literal("in") + ExprSingle + ZeroOrMore(Literal(",") + Literal("$") + VarName + Optional(TypeDeclaration) + Literal("in") + ExprSingle) + Literal("satisfies") + ExprSingle
OrderModifier   = Optional(Literal("ascending") | Literal("descending")) + Optional(Literal("empty") + (Literal("greatest") | Literal("least"))) + Optional(Literal("collation") + URILiteral)
OrderSpec       = ExprSingle + OrderModifier
OrderSpecList   = OrderSpec + ZeroOrMore(Literal(",") + OrderSpec)
OrderByClause   = (("order") + Literal("by") | Literal("stable") + Literal("order") + Literal("by")) + OrderSpecList
WhereClause     = Literal("where") + ExprSingle
LetClause       = Literal("let") + Literal("$") + VarName + Optional(TypeDeclaration) + Literal(":=") + ExprSingle + ZeroOrMore(Literal(",") + Literal("$") + VarName + Optional(TypeDeclaration) + Literal(":=") + ExprSingle)
PositionalVar   = Literal("at") + Literal("$") + VarName
ForClause       = Literal("for") + Literal("$") + VarName + Optional(TypeDeclaration) + Optional(PositionalVar) + Literal("in") + ExprSingle + ZeroOrMore(Literal(",") + Literal("$") + VarName + Optional(TypeDeclaration) + Optional(PositionalVar) + Literal("in") + ExprSingle)
FLWORExpr       = OneOrMore(ForClause | LetClause) + Optional(WhereClause) + Optional(OrderByClause) + Literal("return") + ExprSingle

IfExpr          = Literal("if") + Literal("(") + Expr + Literal(")") + Literal("then") + ExprSingle + Literal("else") + ExprSingle
CaseClause      = Literal("case") + Optional(Literal("$") + VarName + Literal("as")) + SequenceType + Literal("return") + ExprSingle
TypeswitchExpr  = Literal("typeswitch") + Literal("(") + Expr + Literal(")") + OneOrMore(CaseClause) + Literal("default") + Optional(Literal("$") + VarName) + Literal("return") + ExprSingle
ExprSingle    <<= (FLWORExpr | QuantifiedExpr | TypeswitchExpr | IfExpr | OrExpr)
Expr          <<= (ExprSingle + ZeroOrMore(Literal(",") + ExprSingle))
QueryBody       = Expr
EnclosedExpr    = Literal("{").suppress() + Expr + Literal("}").suppress()

# Prolog: Imports
URILiteral    = StringLiteral # TODO: StringLiteral will need to include slashes etc
SchemaPrefix  = (Literal("namespace") + NCName + Literal("=")) | (Literal("default") + Literal("element") + Literal("namespace"))
SchemaImport  = Literal("import") + Literal("schema") + Optional(SchemaPrefix) + URILiteral + Optional("at" + URILiteral + ZeroOrMore(Literal(",") + URILiteral))
ModuleImport  = Literal("import") + Literal("module") + Optional(Literal("namespace") + NCName + Literal("=")) + URILiteral + Optional(Literal("at") + URILiteral + ZeroOrMore(Literal(",") + URILiteral))
Import        = SchemaImport | ModuleImport

# Prolog: Declarations
NamespaceDecl         = Literal("declare") + Literal("namespace") + NCName + Literal("=") + URILiteral
BoundarySpaceDecl     = Literal("declare") + Literal("boundary-space") + (Literal("preserve") | Literal("strip"))
DefaultNamespaceDecl  = Literal("declare") + Literal("default") + (Literal("element") | Literal("function")) + Literal("namespace") + URILiteral
OptionDecl            = Literal("declare") + Literal("option") + QName + StringLiteral
OrderingModeDecl      = Literal("declare") + Literal("ordering") + (Literal("ordered") | Literal("unordered"))
EmptyOrderDecl        = Literal("declare") + Literal("default") + Literal("order") + Literal("empty") + (Literal("greatest") | Literal("least"))
PreserveMode          = Literal("preserve") | Literal("no-preserve")
InheritMode           = Literal("inherit") | Literal("no-inherit")
CopyNamespacesDecl    = Literal("declare") + Literal("copy-namespaces") + PreserveMode + Literal(",") + InheritMode
DefaultCollationDecl  = Literal("declare") + Literal("default") + Literal("collation") + URILiteral
BaseURIDecl           = Literal("declare") + Literal("base-uri") + URILiteral

VarDecl           = Literal("declare") + Literal("variable") + Literal("$") + QName + Optional(TypeDeclaration) ((Literal(":=") + ExprSingle) | Literal("external"))
ConstructionDecl  = Literal("declare") + Literal("construction") + (Literal("strip") | Literal("preserve"))
FunctionDecl      = Group(Literal("declare") + Literal("function") + QName.setResultsName("name") + Literal("(") + Optional(ParamList).setResultsName("params") + Literal(")") + Optional(Literal("as").suppress() + SequenceType).setResultsName("rtype") + (EnclosedExpr | "external").setResultsName("body"))

Setter = BoundarySpaceDecl | DefaultCollationDecl | BaseURIDecl | ConstructionDecl | OrderingModeDecl | EmptyOrderDecl | CopyNamespacesDecl
Prolog = ZeroOrMore((DefaultNamespaceDecl | Setter | NamespaceDecl | Import) + Separator) + ZeroOrMore((VarDecl | FunctionDecl | OptionDecl) + Separator)

#QueryBody = Word(alphas) # TODO
ModuleDecl = Word(alphas) # TODO

LibraryModule   = ModuleDecl + Prolog
MainModule      = Prolog + QueryBody
Module          = Optional(VersionDecl) + (LibraryModule | MainModule)
Grammar         = Module

#grammar = (
#  VersionDecl +
#  (FunctionDecl + ZeroOrMore(Separator + FunctionDecl)).setResultsName("functions")
#)

# TODO: add all of the other types!
types = {
  "xs:string": "String",
  "xs:boolean": "Boolean"
}

def separated_to_camelcase(value, separator):
  def camelcase(): 
    yield str.lower
    while True:
      yield str.capitalize
  c = camelcase()
  return "".join(c.next()(x) if x else separator for x in value.split(separator))

def hyphen_to_camelcase(value):
  return separated_to_camelcase(value, "-")

def scala_object_name(name):
  return name.title()

def scala_func_name(name):
  return hyphen_to_camelcase(name)

def scala_func_params(params):
  s = ""
  for param in params:
    s += param.name.localpart + ": " + scala_type(param.type, param.occurrence) + ", "
  # Remove trailing comma
  if len(s) > 0:
      if s[-2:] == ", ":
          s = s[:-2]
  return s

def scala_type(xquery_type, occurrence):
  t = types[xquery_type]
  if occurrence == "?":
    t = "Option["+t+"]"
  elif occurrence == "*":
    t = "Option[Array["+t+"]]" # TODO: would be nice if this didn't have to be surrounded by Option, but maybe this is a style improvement that just has be done manually
  return t

def scala_function(function):
  return """  def {func_name}({func_params}): {func_rtype} = {{ "hello world" }}\n""".format(
          func_name=scala_func_name(function.name.localpart),
          func_params=scala_func_params(function.params),
          func_rtype=scala_type(function.rtype.type, function.rtype.occurrence)
         )

def scala_functions(functions):
  s = ""
  for function in functions:
    s += scala_function(function)
  return s

def scala_object(obj, functions):
  return """object {object_name} {{\n{functions}\n}}""".format(
          object_name=obj,
          functions=scala_functions(functions)
         )

with open(sys.argv[1], "r") as f:
  xquery_input = f.read()
  xquery_parsed = grammar.parseString(xquery_input)

  # Group functions by xquery namespace, placing them into scala objects
  objects = {}
  for function in xquery_parsed.functions:
    object_name = scala_object_name(function.name.prefix)
    if object_name in objects:
      objects[object_name].append(function)
    else:
      objects[object_name] = [function]

  for obj, functions in objects.iteritems():
    print scala_object(obj, functions)

