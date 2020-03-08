unit EncoderTest;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  JsonTools in '../jsontools.pas';

type

  TEncoderTest = class(TTestCase)
  published
    procedure TestJsonValidate;
    procedure TestJsonNumberValidate;
    procedure TestJsonStringValidate;
    procedure TestJsonStringEncode;
    procedure TestJsonStringDecode;
  end;

implementation

procedure TEncoderTest.TestJsonValidate;
begin
  AssertTrue(JsonValidate('[]'));
  AssertTrue(JsonValidate('{}'));
  AssertTrue(JsonValidate('{ "key": "value" }'));
  AssertFalse(JsonValidate(''));
  AssertFalse(JsonValidate('"key": "value"'));
end;

procedure TEncoderTest.TestJsonNumberValidate;
begin
  AssertTrue(JsonNumberValidate('42'));
  AssertTrue(JsonNumberValidate('0'));
  AssertTrue(JsonNumberValidate('-42'));
  AssertTrue(JsonNumberValidate('3.14'));
  AssertTrue(JsonNumberValidate('314e-2'));
  AssertFalse(JsonNumberValidate(''));
  AssertFalse(JsonNumberValidate(' 42'));
  AssertFalse(JsonNumberValidate('42 '));
  AssertFalse(JsonNumberValidate('"42"'));
  AssertFalse(JsonNumberValidate('false'));
end;

procedure TEncoderTest.TestJsonStringValidate;
begin
  AssertTrue(JsonStringValidate('""'));
  AssertTrue(JsonStringValidate('" "'));
  AssertTrue(JsonStringValidate('"text"'));
  AssertFalse(JsonStringValidate(''));
  AssertFalse(JsonStringValidate('42'));
  AssertFalse(JsonStringValidate('text'));
  AssertFalse(JsonStringValidate('"text'));
end;

procedure TEncoderTest.TestJsonStringEncode;
begin
  AssertEquals('""', JsonStringEncode(''));
  AssertEquals('"\u0001\u0002\u0003\u0004\u0005"', JsonStringEncode(#1#2#3#4#5));
  AssertEquals('"\u0006\u0007\b\t\n"', JsonStringEncode(#6#7#8#9#10));
  AssertEquals('"\v\f\r\u000E\u000F"', JsonStringEncode(#11#12#13#14#15));
  AssertEquals('"\u0010\u0011\u0012\u0013\u0014"', JsonStringEncode(#16#17#18#19#20));
  AssertEquals('"\u0015\u0016\u0017\u0018\u0019"', JsonStringEncode(#21#22#23#24#25));
  AssertEquals('"\u001A\u001B\u001C\u001D\u001E"', JsonStringEncode(#26#27#28#29#30));
  AssertEquals('"\u001F !\"#"', JsonStringEncode(#31#32#33#34#35));
  AssertEquals('"text\"with''special\\characters"', JsonStringEncode('text"with''special\characters'));
end;

procedure TEncoderTest.TestJsonStringDecode;
begin
  AssertEquals('', JsonStringDecode('""'));
  AssertEquals(#1#2#3#4#5, JsonStringDecode('"\u0001\u0002\u0003\u0004\u0005"'));
  AssertEquals(#6#7#8#9#10, JsonStringDecode('"\u0006\u0007\b\t\n"'));
  AssertEquals(#11#12#13#14#15, JsonStringDecode('"\v\f\r\u000E\u000F"'));
  AssertEquals(#16#17#18#19#20, JsonStringDecode('"\u0010\u0011\u0012\u0013\u0014"'));
  AssertEquals(#21#22#23#24#25, JsonStringDecode('"\u0015\u0016\u0017\u0018\u0019"'));
  AssertEquals(#26#27#28#29#30, JsonStringDecode('"\u001A\u001B\u001C\u001D\u001E"'));
  AssertEquals(#31#32#33#34#35, JsonStringDecode('"\u001F !\"#"'));
  AssertEquals('text"with''special\characters', JsonStringDecode('"text\"with''special\\characters"'));
  AssertEquals('one ""two"" three', JsonStringDecode('"one \"\"two\"\" three"'));
end;

initialization
  RegisterTest(TEncoderTest);
end.

