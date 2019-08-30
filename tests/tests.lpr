(********************************************************)
(*                                                      *)
(*  Json Tools Pascal Unit Test                         *)
(*                                                      *)
(*  http://www.getlazarus.org/json                      *)
(*  Released under the GPLv3 August 2019                *)
(*                                                      *)
(********************************************************)
program Tests;

{$mode delphi}

uses
  JsonTools in '../jsontools.pas';

type
  TTest = function(out Msg: string): Boolean;

function Test1(out Msg: string): Boolean;
var
  N: TJsonNode;
begin
  Msg := 'Test: Cannot add non object or array roots';
  N := TJsonNode.Create;
  try
    N.Value := '"employee"';
    Result := False;
  except
    Result := True;
  end;
  N.Free;
end;

function Test2(out Msg: string): Boolean;
var
  N: TJsonNode;
begin
  Msg := 'Test: Parse simple object';
  N := TJsonNode.Create;
  try
    N.Value := '{ "name": "john" }';
    Result := (N.Kind = nkObject) and (N.Count = 1) and (N.Child(0).Name = 'name')
      and (N.Child(0).Value = '"john"');
  except
    Result := False;
  end;
  N.Free;
end;

function Test3(out Msg: string): Boolean;
var
  N: TJsonNode;
begin
  Msg := 'Test: Parse array';
  N := TJsonNode.Create;
  try
    N.Value := '[ "john", 32, [ { "pi": 314e-2 } ] ]';
    Result := (N.Kind = nkArray) and (N.Count = 3)and (N.Child(1).Value = '32')
      and (N.Child(2).Child(0).Child(0).Value = '314e-2');
  except
    Result := False;
  end;
  N.Free;
end;

function Test4(out Msg: string): Boolean;
var
  N: TJsonNode;
begin
  Msg := 'Test: Find item';
  N := TJsonNode.Create;
  try
    N.Value := '[ "john", 32, [ { "pi": 314e-2 } ] ]';
    Result := N.Find('/2/0/pi').Value = '314e-2';
    Result := Result and (N.Find('2/0/pi').Value = '314e-2');
  except
    Result := False;
  end;
  N.Free;
end;

function Test5(out Msg: string): Boolean;
var
  N: TJsonNode;
begin
  Msg := 'Test: Parse various kinds';
  N := TJsonNode.Create;
  try
    N.Value :=
     ' { '#10+
     '    "object_or_array"  : "object",'#10+
     '  "empty" : false   ,   '#10+
     '    "parse_time_nanoseconds" :   19608  ,'#10+
     '     "validate"  : true,'#10+
     '   "size": 1'#10+
      '}';
    Result := (N.Find('object_or_array').Value = '"object"')
      and (N.Child('empty').Value = 'false')
      and (N.Child('parse_time_nanoseconds').AsNumber = 19608)
      and (N.Child('validate').AsBoolean)
      and (N.Find('size').AsNumber = 1)
      and (N.Child('validate').AsNumber = 0)
      and (N.Find('size').AsBoolean = False);
  except
    Result := False;
  end;
  N.Free;
end;

function Test6(out Msg: string): Boolean;
var
  N: TJsonNode;
begin
  Msg := 'Test: Dynamic creation';
  N := TJsonNode.Create;
  try
    N.Add('name', 'john');
    N := N.Add('address', nkObject);
    N.Add('street', '123 Skippy Lane');
    N.Add('city', 'Fairfield');
    N.Add('city', 'Los Angeles');
    N.Add('state', 'CA');
    N.Add('zip', nkNull);
    Result := (N.Root.Count = 2)
      and (N.Find('/address/city').Value = '"Los Angeles"')
      and (N.Find('/address').Count = 4)
      and (N.Child(3).Value = 'null');
  except
    Result := False;
  end;
  N.Root.Free;
end;

function Test7(out Msg: string): Boolean;
var
  N: TJsonNode;
  S: string;
begin
  Msg := 'Test: Dynamic creation';
  N := TJsonNode.Create;
  try
N.Value := '{ "name"   : "Alice Brown",'+
'  "sku"    : "54321",'+
'  "valued"  : true,'+
'  "dates"  : [1, true, "true", [[[]]]],'+
'  "price"  : 199.95,'+
'  "shipTo" : { "name" : "Bob Brown",'+
'               "address" : "456 Oak Lane",'+
'               "city" : "Pretendville",'+
'               "state" : "HI",'+
'               "zip"   : "98999" },'+
'  "billTo" : { "name" : "Alice Brown",'+
'               "address" : "456 Oak > Lane",'+
'               "city" : "Pretendville",'+
'               "state" : "HI",'+
'               "zip"   : "98999",'+
'"notes": null}' +
'}';
    S := N.AsJson;
    N.Parse(S);
    Result := True;
  except
    Result := False;
  end;
  N.Root.Free;
end;

procedure Check(Test: TTest; var Passed, Failed: Integer);
var
  S: string;
begin
  if Test(S) then
  begin
    Inc(Passed);
    WriteLn(S, ' - PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn(S, ' - FAIL');
  end;
end;

procedure RunTests;
var
  Passed, Failed: Integer;
begin
  Passed := 0;
  Failed := 0;
  Check(Test1, Passed, Failed);
  Check(Test2, Passed, Failed);
  Check(Test3, Passed, Failed);
  Check(Test4, Passed, Failed);
  Check(Test5, Passed, Failed);
  Check(Test6, Passed, Failed);
  Check(Test7, Passed, Failed);
  if Failed > 0 then
    WriteLn(Failed, ' tests FAILED')
  else
    WriteLn('All tests PASSED');
end;

begin
  RunTests;
end.

