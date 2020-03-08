unit GeneratorTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  JsonTools in '../jsontools.pas';

type

  TGeneratorTest= class(TTestCase)
  private
    Root: TJsonNode;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // test object
    procedure TestEmptyObject;
    procedure TestString;
    procedure TestEmptyString;
    procedure TestInteger;
    procedure TestIntegerNegative;
    procedure TestFloat;
    procedure TestFloatExponent;
    procedure TestNull;
    procedure TestBooleanTrue;
    procedure TestBooleanFalse;
    procedure TestArrayValue;
    procedure TestMultipleValues;
    // test array
    procedure TestEmptyArray;
    procedure TestArraySingleValue;
    procedure TestArrayMultipleValues;
    // test nested structures
    procedure TestNested;
    // test methods
    procedure TestSaveToFile;
    procedure TestGetAsArray;
    procedure TestGetAsObject;
    procedure TestGetAsNull;
    procedure TestGetAsBoolean;
    procedure TestGetAsString;
    procedure TestGetAsNumber;
  end;

implementation

procedure TGeneratorTest.SetUp;
begin
  Root := TJsonNode.Create;
end;

procedure TGeneratorTest.TearDown;
begin
  Root.Free;
end;

procedure TGeneratorTest.TestEmptyObject;
const JsonText = '{}';
begin
  Root.AsObject;
  AssertEquals(JsonText, Root.AsJson);
end;

procedure TGeneratorTest.TestString;
const JsonText  = '{"key":"value"}';
      ValueText = '{'#10#9'"key": "value"'#10'}';
begin
  Root.AsObject;
  Root.Add('key', 'value');
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestEmptyString;
const JsonText  = '{"key":""}';
      ValueText = '{'#10#9'"key": ""'#10'}';
begin
  Root.AsObject;
  Root.Add('key', '');
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestInteger;
const JsonText  = '{"key":42}';
      ValueText = '{'#10#9'"key": 42'#10'}';
begin
  Root.AsObject;
  Root.Add('key', 42);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestIntegerNegative;
const JsonText  = '{"key":-42}';
      ValueText = '{'#10#9'"key": -42'#10'}';
begin
  Root.AsObject;
  Root.Add('key', -42);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestFloat;
const JsonText  = '{"key":3.14}';
      ValueText = '{'#10#9'"key": 3.14'#10'}';
begin
  Root.AsObject;
  Root.Add('key', 3.14);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestFloatExponent;
const JsonText  = '{"key":3.14}';
      ValueText = '{'#10#9'"key": 3.14'#10'}';
begin
  Root.AsObject;
  Root.Add('key', 314e-2);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestNull;
const JsonText  = '{"key":null}';
      ValueText = '{'#10#9'"key": null'#10'}';
begin
  Root.AsObject;
  Root.Add('key', nkNull);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestBooleanTrue;
const JsonText  = '{"key":true}';
      ValueText = '{'#10#9'"key": true'#10'}';
begin
  Root.AsObject;
  Root.Add('key', true);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestBooleanFalse;
const JsonText  = '{"key":false}';
      ValueText = '{'#10#9'"key": false'#10'}';
begin
  Root.AsObject;
  Root.Add('key', false);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestArrayValue;
const JsonText  = '{"key":[]}';
      ValueText = '{'#10#9'"key": [ ]'#10'}';
begin
  Root.AsObject;
  Root.Add('key', nkArray);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestMultipleValues;
const JsonText = '{"key1":"value","key2":3.14,"key3":true,"key4":[],"key5":{},"key6":null}';
      ValueText =
        '{'#10 +
        #9'"key1": "value",'#10 +
        #9'"key2": 3.14,'#10 +
        #9'"key3": true,'#10 +
        #9'"key4": [ ],'#10 +
        #9'"key5": { },'#10 +
        #9'"key6": null'#10 +
        '}';
begin
  Root.AsObject;
  Root.Add('key1', 'value');
  Root.Add('key2', 3.14);
  Root.Add('key3', true);
  Root.Add('key4', nkArray);
  Root.Add('key5', nkObject);
  Root.Add('key6', nkNull);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestEmptyArray;
const JsonText  = '[]';
      ValueText = '[ ]';
begin
  Root.AsArray;
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestArraySingleValue;
const JsonText  = '["value"]';
      ValueText = '['#10#9'"value"'#10']';
begin
  Root.AsArray;
  Root.Add('', 'value');
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestArrayMultipleValues;
const JsonText = '["value",42,3.14,true,null,{}]';
      ValueText =
        '['#10 +
        #9'"value",'#10 +
        #9'42,'#10 +
        #9'3.14,'#10 +
        #9'true,'#10 +
        #9'null,'#10 +
        #9'{ }'#10 +
        ']';
begin
  Root.AsArray;
  Root.Add('', 'value');
  Root.Add('', 42);
  Root.Add('', 3.14);
  Root.Add('', true);
  Root.Add('', nkNull);
  Root.Add('', nkObject);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

procedure TGeneratorTest.TestNested;
const JsonText = '{"arr":["value",{"arr2":[42]}]}';
      ValueText =
        '{'#10 +
        #9'"arr": ['#10 +
        #9#9'"value",'#10 +
        #9#9'{'#10 +
        #9#9#9'"arr2": ['#10 +
        #9#9#9#9'42'#10 +
        #9#9#9']'#10 +
        #9#9'}'#10 +
        #9']'#10 +
        '}';
var Arr1, Obj1, Arr2: TJsonNode;
begin
  Arr1 := Root.Add('arr', nkArray);
  Arr1.Add('', 'value');
  Obj1 := Arr1.Add('', nkObject);
  Arr2 := Obj1.Add('arr2', nkArray);
  Arr2.Add('', 42);
  AssertEquals(JsonText, Root.AsJson);
  AssertEquals(ValueText, Root.Value);
end;

function ReadTextFile(Filename: String): String;
var F: TextFile;
    S: String;
begin
  Result := '';
  AssignFile(F, Filename);
  Reset(F);
  while not Eof(F) do
  begin
    ReadLn(F, S);
    Result := Result + S + #10;
  end;
  CloseFile(F);
end;

procedure TGeneratorTest.TestSaveToFile;
const JsonText =
  '{'#10 +
  #9'"key": "value"'#10 +
  '}'#10;
      Filename = 'tmp.json';
var FileContent: String;
begin
  Root.Add('key', 'value');
  Root.SaveToFile(Filename);
  FileContent := ReadTextFile(Filename);
  AssertEquals(JsonText, FileContent);
  DeleteFile(Filename);
end;

procedure TGeneratorTest.TestGetAsArray;
var Node, ReturnValue: TJsonNode;
begin
  // Array
  Node := Root.Add('key1', nkArray);
  Node.Add('', 'value');
  AssertTrue(nkArray = Node.Kind);
  ReturnValue := Node.AsArray; // call GetAsArray
  AssertTrue(nkArray = ReturnValue.Kind);
  AssertEquals(1, ReturnValue.Count);
  AssertEquals('value', ReturnValue.Child(0).AsString);
  // String
  Node := Root.Add('key2', 'value');
  AssertTrue(nkString = Node.Kind);
  ReturnValue := Node.AsArray; // call GetAsArray
  AssertTrue(nkArray = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
   // Object
  Node := Root.Add('key3', nkObject);
  Node.Add('subkey', 'value');
  AssertTrue(nkObject = Node.Kind);
  ReturnValue := Node.AsArray; // call GetAsArray
  AssertTrue(nkArray = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
end;

procedure TGeneratorTest.TestGetAsObject;
var Node, ReturnValue: TJsonNode;
begin
  // Object
  Node := Root.Add('key1', nkObject);
  Node.Add('subkey1', 'value1');
  AssertTrue(nkObject = Node.Kind);
  ReturnValue := Node.AsObject; // call GetAsObject
  AssertTrue(nkObject = ReturnValue.Kind);
  AssertEquals(1, ReturnValue.Count);
  AssertEquals('subkey1', ReturnValue.Child(0).Name);
  AssertEquals('value1', ReturnValue.Child(0).AsString);
  // Array
  Node := Root.Add('key2', nkArray);
  Node.Add('', 'value2');
  AssertTrue(nkArray = Node.Kind);
  ReturnValue := Node.AsObject; // call GetAsObject
  AssertTrue(nkObject = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
  // String
  Node := Root.Add('key3', 'value3');
  AssertTrue(nkString = Node.Kind);
  ReturnValue := Node.AsObject; // call GetAsObject
  AssertTrue(nkObject = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
end;

procedure TGeneratorTest.TestGetAsNull;
var Node, ReturnValue: TJsonNode;
begin
  // Null
  Node := Root.Add('key1', nkNull);
  AssertTrue(nkNull = Node.Kind);
  ReturnValue := Node.AsNull; // call GetAsNull
  AssertTrue(nkNull = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
  // Object
  Node := Root.Add('key2', nkObject);
  Node.Add('subkey2', 'value2');
  AssertTrue(nkObject = Node.Kind);
  ReturnValue := Node.AsNull; // call GetAsNull
  AssertTrue(nkNull = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
  // Array
  Node := Root.Add('key3', nkArray);
  Node.Add('', 'value3');
  AssertTrue(nkArray = Node.Kind);
  ReturnValue := Node.AsNull; // call GetAsNull
  AssertTrue(nkNull = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
  // String
  Node := Root.Add('key4', 'value4');
  AssertTrue(nkString = Node.Kind);
  ReturnValue := Node.AsNull; // call GetAsNull
  AssertTrue(nkNull = ReturnValue.Kind);
  AssertEquals(0, ReturnValue.Count);
end;

procedure TGeneratorTest.TestGetAsBoolean;
var Node: TJsonNode;
begin
  // Boolean
  Node := Root.Add('key1', true);
  AssertTrue(nkBool = Node.Kind);
  AssertTrue(Node.AsBoolean); // call GetAsBoolean
  // Object
  Node := Root.Add('key2', nkObject);
  Node.Add('subkey2', 'value2');
  AssertTrue(nkObject = Node.Kind);
  AssertFalse(Node.AsBoolean); // call GetAsBoolean
  AssertTrue(nkBool = Node.Kind);
  // String
  Node := Root.Add('key3', 'true');
  AssertTrue(nkString = Node.Kind);
  AssertFalse(Node.AsBoolean); // call GetAsBoolean
  AssertTrue(nkBool = Node.Kind);
end;

procedure TGeneratorTest.TestGetAsString;
var Node: TJsonNode;
begin
  // String
  Node := Root.Add('key1', 'value1');
  AssertTrue(nkString = Node.Kind);
  AssertEquals('value1', Node.AsString); // call GetAsString
  // Boolean
  Node := Root.Add('key2', true);
  AssertTrue(nkBool = Node.Kind);
  AssertEquals('', Node.AsString); // call GetAsString
  AssertTrue(nkString = Node.Kind);
  // Object
  Node := Root.Add('key3', nkObject);
  Node.Add('subkey3', 'value3');
  AssertTrue(nkObject = Node.Kind);
  AssertEquals('', Node.AsString); // call GetAsString
  AssertTrue(nkString = Node.Kind);
end;

procedure TGeneratorTest.TestGetAsNumber;
var Node: TJsonNode;
begin
  // Number
  Node := Root.Add('key1', 42);
  AssertTrue(nkNumber = Node.Kind);
  AssertEquals(42, Node.AsNumber); // call GetAsNumber
  // String
  Node := Root.Add('key2', '42');
  AssertTrue(nkString = Node.Kind);
  AssertEquals(0, Node.AsNumber); // call GetAsNumber
  AssertTrue(nkNumber = Node.Kind);
  // Object
  Node := Root.Add('key3', nkObject);
  Node.Add('subkey3', 'value3');
  AssertTrue(nkObject = Node.Kind);
  AssertEquals(0, Node.AsNumber); // call GetAsNumber
  AssertTrue(nkNumber = Node.Kind);
end;

initialization
  RegisterTest(TGeneratorTest);
end.

