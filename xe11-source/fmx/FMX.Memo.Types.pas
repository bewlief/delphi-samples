{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Memo.Types;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, FMX.Text;


type
  TInsertOption = (Selected, MoveCaret, CanUndo, UndoPairedWithPrev, Typed);
  TInsertOptions = set of TInsertOption;

  TDeleteOption = (MoveCaret, CanUndo, Selected);
  TDeleteOptions = set of TDeleteOption;

  TActionType = (Delete, Insert);

  TSelectionPointType = (Left, Right);

  // Type alias for backward compatibility
  TCaretPosition = FMX.Text.TCaretPosition;

  ///<summary>Information about fragment of the text that was inserted</summary>
  TFragmentInserted = record
    ///<summary>Position in text where text was inserted</summary>
    StartPos: Integer;
    ///<summary>Length of text that was inserted</summary>
    FragmentLength: Integer;
    ///<summary>Defines that change was made right after the previous and was made in the similar way
    ///(e.g. text editing (delete and insert) via keyabord)</summary>
    PairedWithPrev: Boolean;
    ///<summary>Was text inserted via typing from keyboard or not</summary>
    Typed: Boolean;
  public
    ///<summary>Create new information about inserted text with defined values</summary>
    class function Create(const StartPos, FragmentLength: Integer;
      const PairedWithPrev, Typed: Boolean): TFragmentInserted; static; inline;
  end;

  ///<summary>Information about fragment of the text that was removed</summary>
  TFragmentDeleted = record
    ///<summary>Position in text from which text was deleted</summary>
    StartPos: Integer;
    ///<summary>Fragmen of text that was deleted</summary>
    Fragment: string;
    ///<summary>Was removed text select or not</summary>
    Selected: Boolean;
    ///<summary>Was caret moved after text was removed or not</summary>
    CaretMoved: Boolean;
  public
    ///<summary>Create new information about removed text with defined values</summary>
    class function Create(const StartPos: Integer; const Fragment: string;
      const Selected, CaretMoved: Boolean): TFragmentDeleted; static; inline;
  end;

implementation

{ TFragmentInserted }

class function TFragmentInserted.Create(const StartPos, FragmentLength: Integer;
  const PairedWithPrev, Typed: Boolean): TFragmentInserted;
begin
  Result.StartPos := StartPos;
  Result.FragmentLength := FragmentLength;
  Result.PairedWithPrev := PairedWithPrev;
  Result.Typed := Typed;
end;

{ TFragmentDeleted }

class function TFragmentDeleted.Create(const StartPos: Integer; const Fragment: string;
  const Selected, CaretMoved: Boolean): TFragmentDeleted;
begin
  Result.StartPos := StartPos;
  Result.Fragment := Fragment;
  Result.Selected := Selected;
  Result.CaretMoved := CaretMoved;
end;

end.
