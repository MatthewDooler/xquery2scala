#!/usr/bin/env python
import sys
from pyparsing import Literal, Word, Optional, ZeroOrMore, Empty, Group, Combine, alphas, countedArray

# XQuery Grammar: http://www.w3.org/TR/xquery/

NameStartChar = Word(alphas + "_")
NameChar = Word(alphas + "_-.0123456789")
Name = NameStartChar + ZeroOrMore(NameChar)
NCName = Combine(Name)

LocalPart = NCName.setResultsName("localpart")
Prefix = NCName.setResultsName("prefix")
UnprefixedName = LocalPart
PrefixedName = Prefix + Literal(":") + LocalPart
QName = Combine(PrefixedName | UnprefixedName)

AnyKindTest = Literal("node") + Literal("(") + Literal(")")
TextTest = Literal("text") + Literal("(") + Literal(")")
CommentTest = Literal("comment") + Literal("(" ")")
#PITest = Literal("processing-instruction") + Literal("(") + Optional(NCName | StringLiteral) + Literal(")")
TypeName = QName
AttributeName = QName
AttribNameOrWildcard = AttributeName | Literal("*")
ElementName = QName
ElementNameOrWildcard = ElementName | Literal("*")
AttributeDeclaration = AttributeName
SchemaAttributeTest = Literal("schema-attribute") + Literal("(") + AttributeDeclaration + Literal(")")
AttributeTest = Literal("attribute") + Literal("(") + Optional(AttribNameOrWildcard + Optional(Literal(",").suppress() + TypeName)) + Literal(")")
ElementDeclaration = ElementName
SchemaElementTest = Literal("schema-element") + Literal("(") + ElementDeclaration + Literal(")")
ElementTest = Literal("element") + Literal("(") + Optional(ElementNameOrWildcard + Optional(Literal(",").suppress() + TypeName + Optional(Literal("?")))) + Literal(")")
DocumentTest = Literal("document-node") + Literal("(") + Optional(ElementTest | SchemaElementTest) + Literal(")")
KindTest = DocumentTest | ElementTest | AttributeTest | SchemaElementTest | SchemaAttributeTest | CommentTest | TextTest | AnyKindTest
# TODO: support processing instructions (PITest)

AtomicType = QName
OccurrenceIndicator = (Literal("?") | Literal("*") | Literal("+")).setResultsName("occurrence")
ItemType = (KindTest | (Literal("item") + Literal("(") + Literal(")")) | AtomicType).setResultsName("type")
SequenceType = (Literal("empty-sequence") + Literal("(") + Literal(")")) | (ItemType + Optional(OccurrenceIndicator))

TypeDeclaration = Literal("as") + SequenceType
Param          = Group(Literal("$").suppress() + QName.setResultsName("name") + Optional(TypeDeclaration))
ParamList      = Param + ZeroOrMore(Literal(",").suppress() + Param)

Expr = Word(alphas) # TODO: parse expressions
EnclosedExpr = Literal("{").suppress() + Expr + Literal("}").suppress()

FunctionDecl = Group(Literal("declare") + Literal("function") + QName.setResultsName("name") + Literal("(") + Optional(ParamList).setResultsName("params") + Literal(")") + Optional(Literal("as").suppress() + SequenceType).setResultsName("rtype") + (EnclosedExpr | "external").setResultsName("body"))


grammar = (FunctionDecl + ZeroOrMore(Literal(";").suppress() + FunctionDecl)).setResultsName("functions")

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

