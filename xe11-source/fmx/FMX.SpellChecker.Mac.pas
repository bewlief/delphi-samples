{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.SpellChecker.Mac;

interface

{$SCOPEDENUMS ON}

uses
  Macapi.AppKit, Macapi.Foundation, FMX.SpellChecker;

type
  TMacSpellCheckerService = class(TInterfacedObject, IFMXSpellCheckerService)
  public class var
    FMaxUsedPrefferedLanguagesCount: Integer;
    class constructor Create;
    class procedure SetMaxUsedPrefferedLanguagesCount(const Value: Integer); static;
  private
    FChecker: NSSpellChecker;
    function GetChecker: NSSpellChecker;
    function GuessesForWordRange(const AWord: string; const ALanguage: NSString = nil): TArray<string>;
  public
    destructor Destroy; override;
    { IFMXSpellCheckerService }
    function CheckSpelling(AWord: string): TArray<string>;
    function GuessCompletions(AWord: string): TArray<string>;
  public
    property Checker: NSSpellChecker read GetChecker;

    /// <summary>
    ///   The maximum allowed number of languages used. We check specified word across all preferable languages by user.
    ///   The list of preferred languages are provided by macOS and can include 10+ languages. To avoid checking all
    ///   languages, we check no more than this number of languages.
    /// </summary>
    /// <remarks>
    ///   0 - Let the system choose the language itself automatically. This can significantly reduce the number
    ///   of incorrect words found.
    /// </remarks>
    class property MaxUsedPrefferedLanguagesCount: Integer read FMaxUsedPrefferedLanguagesCount
      write SetMaxUsedPrefferedLanguagesCount;
  end;

implementation

uses
  System.SysUtils, System.Math, System.Generics.Collections, Macapi.CocoaTypes, Macapi.Helpers, Macapi.ObjectiveC, FMX.Platform, FMX.Types;

procedure RegisterSpellCheckerService;
begin
  TPlatformServices.Current.AddPlatformService(IFMXSpellCheckerService, TMacSpellCheckerService.Create);
end;

procedure UnregisterSpellCheckerService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXSpellCheckerService);
end;

function NSArrayToStringArray(const AArray: NSArray): TArray<string>;
var
  Count: Integer;
begin
  Count := AArray.count;
  SetLength(Result, Count);
  for var I := 0 to Count - 1 do
    Result[I] := NSStrToStr(TNSString.Wrap(AArray.objectAtIndex(I)));
end;

{ TMacSpellCheckerService }

function TMacSpellCheckerService.CheckSpelling(AWord: string): TArray<string>;
var
  WordRange: NSRange;
  PrefferedLanguages: NSArray;
  Language: NSString;
  Ignored: NSInteger;
  Guesses: TArray<string>;
  Word: NSString;
begin
  Assert(Checker <> nil, 'Native bridge was not created');
  Result := nil;

  Word := StrToNSStr(AWord);
  if MaxUsedPrefferedLanguagesCount = 0 then
  begin
    WordRange := Checker.checkSpellingOfString(Word, 0);
    if WordRange.location <> NSNotFound then
      Result := GuessesForWordRange(AWord);
  end
  else
  begin
    PrefferedLanguages := Checker.userPreferredLanguages;
    for var I := 0 to Min(Integer(PrefferedLanguages.count) - 1, MaxUsedPrefferedLanguagesCount - 1) do
    begin
      Language := TNSString.Wrap(PrefferedLanguages.objectAtIndex(I));
      WordRange := Checker.checkSpellingOfString(Word, 0, Language, False, 0, @Ignored);
      if WordRange.location <> NSNotFound then
      begin
        Guesses := GuessesForWordRange(AWord, Language);
        Result := TArray.Concat<string>([Result, Guesses]);
      end;
    end;
  end;
end;

function TMacSpellCheckerService.GetChecker: NSSpellChecker;
begin
  if FChecker = nil then
  begin
    FChecker := TNSSpellChecker.Wrap(TNSSpellChecker.OCClass.sharedSpellChecker);
    FChecker.setAutomaticallyIdentifiesLanguages(True);
  end;
  Result := FChecker;
end;

class constructor TMacSpellCheckerService.Create;
begin
  MaxUsedPrefferedLanguagesCount := 2;
end;

destructor TMacSpellCheckerService.Destroy;
begin
  if FChecker <> nil then
  begin
    FChecker.release;
    FChecker := nil;
  end;
  inherited;
end;

function TMacSpellCheckerService.GuessCompletions(AWord: string): TArray<string>;
var
  Words: NSArray;
  Range: NSRange;
begin
  Assert(Checker <> nil, 'Native bridge was not created');

  Result := nil;
  Range.location := 0;
  Range.length := AWord.Length;
  Words := Checker.completionsForPartialWordRange(Range, StrToNSStr(AWord), nil, 0);
  Result := NSArrayToStringArray(Words);
end;

function TMacSpellCheckerService.GuessesForWordRange(const AWord: string; const ALanguage: NSString): TArray<string>;
var
  WordRange: NSRange;
  Guesses: NSArray;
begin
  WordRange.location := 0;
  WordRange.length := AWord.Length;
  Guesses := Checker.guessesForWordRange(WordRange, StrToNSStr(AWord), ALanguage, 0);
  Result := NSArrayToStringArray(Guesses);
end;

class procedure TMacSpellCheckerService.SetMaxUsedPrefferedLanguagesCount(const Value: Integer);
begin
  FMaxUsedPrefferedLanguagesCount := Max(0, Value);
end;

initialization
  RegisterSpellCheckerService;
end.
