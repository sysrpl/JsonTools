(********************************************************)
(*                                                      *)
(*  Json Tools Pascal Unit Test                         *)
(*                                                      *)
(*  http://www.getlazarus.org/json                      *)
(*  Released under the GPLv3 August 2019                *)
(*                                                      *)
(********************************************************)
program Tests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, ParserTest, GeneratorTest, EncoderTest;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

