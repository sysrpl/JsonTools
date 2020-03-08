unit ParserTest;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  JsonTools in '../jsontools.pas';

type

  TParserTest= class(TTestCase)
  private
    Root: TJsonNode;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // test object
    procedure TestEmptyObject;
    procedure TestString1;
    procedure TestString2;
    procedure TestEmptyString;
    procedure TestInteger;
    procedure TestIntegerNegative;
    procedure TestFloat;
    procedure TestFloatExponent;
    procedure TestNull;
    procedure TestBooleanTrue;
    procedure TestBooleanFalse;
    procedure TestArrayValue;
    procedure TestWhiteSpace;
    // test array
    procedure TestEmptyArray;
    procedure TestArraySingleValue;
    procedure TestArrayMultipleValues;
    // test nested structures
    procedure TestNested;
    // test syntax errors
    procedure TestStringWithoutObject;
    // test methods
    procedure TestFind;
    procedure TestLoadFromFile;
  end;

implementation

procedure TParserTest.SetUp;
begin
  Root := TJsonNode.Create;
end;

procedure TParserTest.TearDown;
begin
  Root.Free;
end;

procedure TParserTest.TestEmptyObject;
const JsonText = '{}';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(0, Root.Count);
end;

procedure TParserTest.TestString1;
const JsonText = '{ "key": "value" }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('"value"', Root.Child(0).Value);
  AssertTrue(nkString = Root.Child(0).Kind);
  AssertEquals('value', Root.Child(0).AsString);
end;

// Verify bug fix: https://github.com/sysrpl/JsonTools/issues/11
procedure TParserTest.TestString2;
const JsonText = '{"test": "one \"\"two\"\" three"}';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('test', Root.Child(0).Name);
  AssertEquals('"one \"\"two\"\" three"', Root.Child(0).Value);
  AssertTrue(nkString = Root.Child(0).Kind);
  AssertEquals('one ""two"" three', Root.Child(0).AsString);
end;

procedure TParserTest.TestEmptyString;
const JsonText = '{ "key": "" }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('""', Root.Child(0).Value);
  AssertTrue(nkString = Root.Child(0).Kind);
  AssertEquals('', Root.Child(0).AsString);
end;

procedure TParserTest.TestInteger;
const JsonText = '{ "key": 42 }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('42', Root.Child(0).Value);
  AssertTrue(nkNumber = Root.Child(0).Kind);
  AssertEquals(42, Root.Child(0).AsNumber);
end;

procedure TParserTest.TestIntegerNegative;
const JsonText = '{ "key": -42 }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('-42', Root.Child(0).Value);
  AssertTrue(nkNumber = Root.Child(0).Kind);
  AssertEquals(-42, Root.Child(0).AsNumber);
end;

procedure TParserTest.TestFloat;
const JsonText = '{ "key": 3.14 }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('3.14', Root.Child(0).Value);
  AssertTrue(nkNumber = Root.Child(0).Kind);
  AssertEquals(3.14, Root.Child(0).AsNumber);
end;

procedure TParserTest.TestFloatExponent;
const JsonText = '{ "key": 314e-2 }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('314e-2', Root.Child(0).Value);
  AssertTrue(nkNumber = Root.Child(0).Kind);
  AssertEquals(3.14, Root.Child(0).AsNumber);
end;

procedure TParserTest.TestNull;
const JsonText = '{ "key": null }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('null', Root.Child(0).Value);
  AssertTrue(nkNull = Root.Child(0).Kind);
end;

procedure TParserTest.TestBooleanTrue;
const JsonText = '{ "key": true }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('true', Root.Child(0).Value);
  AssertTrue(nkBool = Root.Child(0).Kind);
  AssertEquals(true, Root.Child(0).AsBoolean);
end;

procedure TParserTest.TestBooleanFalse;
const JsonText = '{ "key": false }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertEquals('false', Root.Child(0).Value);
  AssertTrue(nkBool = Root.Child(0).Kind);
  AssertEquals(false, Root.Child(0).AsBoolean);
end;

procedure TParserTest.TestArrayValue;
const JsonText = '{ "key": [] }';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('key', Root.Child(0).Name);
  AssertTrue(nkArray = Root.Child(0).Kind);
end;

procedure TParserTest.TestWhiteSpace;
const JsonText =
  ' { '#10 +
  '    "object_or_array"  : "object",'#10 +
  '  "empty" : false   ,   '#10 +
  '    "parse_time_nanoseconds" :   19608  ,'#10 +
  '     "validate"  : true,'#10 +
  '   "size": 1'#10 +
  '}';
begin
  Root.Value := JsonText;
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(5, Root.Count);
  AssertEquals('object_or_array', Root.Child(0).Name);
  AssertTrue(nkString = Root.Child(0).Kind);
  AssertEquals('object', Root.Child(0).AsString);

  AssertEquals('empty', Root.Child(1).Name);
  AssertTrue(nkBool = Root.Child(1).Kind);
  AssertFalse(Root.Child(1).AsBoolean);

  AssertEquals('parse_time_nanoseconds', Root.Child(2).Name);
  AssertTrue(nkNumber = Root.Child(2).Kind);
  AssertEquals(19608, Root.Child(2).AsNumber);

  AssertEquals('validate', Root.Child(3).Name);
  AssertTrue(nkBool = Root.Child(3).Kind);
  AssertTrue(Root.Child(3).AsBoolean);

  AssertEquals('size', Root.Child(4).Name);
  AssertTrue(nkNumber = Root.Child(4).Kind);
  AssertEquals(1, Root.Child(4).AsNumber);
end;

procedure TParserTest.TestEmptyArray;
const JsonText = '[]';
begin
  Root.Value := JsonText;
  AssertTrue(nkArray = Root.Kind);
  AssertEquals(0, Root.Count);
end;

procedure TParserTest.TestArraySingleValue;
const JsonText = '[ "value" ]';
begin
  Root.Value := JsonText;
  AssertTrue(nkArray = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertEquals('"value"', Root.Child(0).Value);
  AssertTrue(nkString = Root.Child(0).Kind);
  AssertEquals('value', Root.Child(0).AsString);
end;

procedure TParserTest.TestArrayMultipleValues;
const JsonText = '[ "value", 42, 3.14, true, null, {} ]';
begin
  Root.Value := JsonText;
  AssertTrue(nkArray = Root.Kind);
  AssertEquals(6, Root.Count);
  AssertTrue(nkString = Root.Child(0).Kind);
  AssertEquals('value', Root.Child(0).AsString);
  AssertTrue(nkNumber = Root.Child(1).Kind);
  AssertEquals(42, Root.Child(1).AsNumber);
  AssertTrue(nkNumber = Root.Child(2).Kind);
  AssertEquals(3.14, Root.Child(2).AsNumber);
  AssertTrue(nkBool = Root.Child(3).Kind);
  AssertEquals(true, Root.Child(3).AsBoolean);
  AssertTrue(nkNull = Root.Child(4).Kind);
  AssertTrue(nkObject = Root.Child(5).Kind);
end;

procedure TParserTest.TestNested;
const JsonText = '{ "arr": [ "value", { "arr2": [ 42 ] } ] }';
var Arr1, Obj1, Arr2: TJsonNode;
begin
  Root.Value := JsonText;
  // top level object
  AssertTrue(nkObject = Root.Kind);
  AssertEquals(1, Root.Count);
  AssertTrue(nkArray = Root.Child(0).Kind);
  // nested array
  Arr1 := Root.Child(0).AsArray;
  AssertEquals(2, Arr1.Count);
  AssertEquals('value', Arr1.Child(0).AsString);
  // nested object
  Obj1 := Arr1.Child(1).AsObject;
  AssertEquals(1, Obj1.Count);
  AssertEquals('arr2', Obj1.Child(0).Name);
  AssertTrue(nkArray = Obj1.Child(0).Kind);
  // nested array
  Arr2 := Obj1.Child(0).AsArray;
  AssertEquals(1, Arr2.Count);
  AssertEquals(42, Arr2.Child(0).AsNumber);
end;

procedure TParserTest.TestStringWithoutObject;
const JsonText = '"value"';
begin
  try
    Root.Value := JsonText;
  except
    on E: EJsonException do
    Exit;
  end;
  Fail('No EJsonException raised');
end;

procedure TParserTest.TestFind;
const JsonText = '[ "john", 32, [ { "pi": 314e-2 } ] ]';
begin
  Root.Value := JsonText;
  AssertEquals('314e-2', Root.Find('/2/0/pi').Value);
  AssertEquals('314e-2', Root.Find('2/0/pi').Value);
end;

procedure TParserTest.TestLoadFromFile;
begin
  Root.LoadFromFile('orders.json');
  AssertEquals(199.95, Root.Find('price').AsNumber);
  AssertEquals('Alice Brown', Root.Find('billTo/name').AsString);
end;

initialization
  RegisterTest(TParserTest);
end.

