{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$IF Defined(SJLJ_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS) or Defined(ZCX_BASED_EXCEPTIONS)}

{$IFDEF SJLJ_BASED_EXCEPTIONS}
type
  PSJLJ_Unwind_FunctionContext = ^TSJLJ_Unwind_FunctionContext;
  TSJLJ_Unwind_FunctionContext = record
  type
    JBufType = (_R7 = 0, _PC, _SP);
  var
    prev : PSJLJ_Unwind_FunctionContext;
    resumeLocation: IntPtr;
    resumeParameters: array[0..3] of IntPtr;
    personality: Pointer;
    lsda: Pointer;
    jbuf: array[JBufType] of NativeInt; // 0 = r7, 1 = pc, 2 = sp
  end;
{$ENDIF SJLJ_BASED_EXCEPTIONS}

function _DelphiPersonalityRoutine(Version: Integer; actions: _Unwind_Action; exceptionClass: UInt64;
  var exceptionObject: _Unwind_Exception; context: PUnwind_Context): _Unwind_Reason_Code; cdecl;

{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
  function GetFramePtr: Pointer;
  begin
{$IF Defined(CPUX64)}
    Result := Pointer(_Unwind_GetGR (context, 6));  // UNW_X86_64_RBP = 6,
{$ELSEIF Defined(CPUARM32)}
    Result := Pointer(_Unwind_GetGR (context, 7));  // UNW_ARM_R7 = 7,
{$ELSEIF Defined(CPUARM64)}
    Result := Pointer(_Unwind_GetGR (context, 29));  // UNW_ARM64_FP  = 29,
{$ELSE}
  {$MESSAGE ERROR 'Unknown platform'}
{$ENDIF}
  end;
{$ENDIF}

const
  CPPExceptionPersonalityMask = UInt64($FFFFFFFFFFFFFF00); // get_vendor_and_language
var
  DelphiException, CPPException: Boolean;
  Results: TScanResults;
  RaiseFrame: PRaiseFrame;
  CPPExceptionPtr: PCXA_EXCEPTION;
  CPPExceptionObj: UIntPtr;
  E: TObject;
  ExceptionRecord: TExceptionRecord;
type
  PObject = ^TObject;
begin
  if (Version <> 1) or (@exceptionObject = nil) or (@context = nil) then
    Exit(_URC_FATAL_PHASE1_ERROR);
  DelphiException := exceptionClass = DelphiExceptionPersonality;
  if not DelphiException then
  begin
    CPPException := (CPPExceptionPersonalityMask and exceptionClass) = CPPExceptionPersonality;
    if not CPPException then
      CPPException := exceptionClass = UInt64($474E5543432B2B00); // iOS6 uses 'GNUCC++\0'
  end
  else
    CPPException := False;
  if Actions and _UA_SEARCH_PHASE <> 0 then
  begin
{$IFDEF MACOS}
    {
      In some cases we may be debugging code, when RTL is not fully
      initialized yet (for example constructors of static and global C++ objects)
      In such case it is not possible to convert C++ exception to Delphi exception
      and phase 2 of stack unwinding will crash accessing null pointer. To prevent
      the crash, the ability to convert C++ exceptions is checked on phase 1 (search phase)
      of stack unwinding. If conversion is not possible then __lldb_fcall_except_handler
      function is invoked and lldb internal breakpoint is hit.
    }
    if CPPException and not Assigned(ExceptObjProc) then
      lldb_invoke_except_handler('C++ exception');
{$ENDIF MACOS}
    Results := ScanEHTable(actions, DelphiException, exceptionObject, Context);
    if (Results.FReason = _URC_HANDLER_FOUND) and DelphiException then
    begin
{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
      RaiseFrame := PRaiseFrame(RaiseListPtr);
{$ELSE}
      RaiseFrame := GetRaiseFrame(exceptionObject);
{$ENDIF}
      RaiseFrame.HandlerSwitchValue := Integer(Results.FTypeIndex);
      RaiseFrame.ActionRecord := Results.FActionRecord;
      RaiseFrame.LanguageSpecificData := Results.FLanguageSpecificData;
      RaiseFrame.CatchTemp := Pointer(Results.FLandingPad);
      RaiseFrame.AdjustedPtr := Results.FAdjustedPtr;
{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
      RaiseFrame.ExceptionFramePtr := GetFramePtr;
{$ENDIF}
    end;
    Exit(Results.FReason);
  end;
  if Actions and _UA_CLEANUP_PHASE <> 0 then
  begin
    // Phase 2 search:
    //  Did we find a catching handler in phase 1?
    if Actions and _UA_HANDLER_FRAME <> 0 then
    begin
      if DelphiException then
      begin
        // Exception is DelphiException.
        // Yes, reload the results from the cache.
{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
        RaiseFrame := PRaiseFrame(RaiseListPtr);
{$ELSE}
        RaiseFrame := GetRaiseFrame(exceptionObject);
{$ENDIF}
        Results.FTypeIndex := RaiseFrame.HandlerSwitchValue;
        Results.FActionRecord := RaiseFrame.ActionRecord;
        Results.FLanguageSpecificData := RaiseFrame.LanguageSpecificData;
        Results.FLandingPad := UIntPtr(RaiseFrame.CatchTemp);
        Results.FAdjustedPtr := RaiseFrame.AdjustedPtr;
        SetRegisters(exceptionObject, context, Results);
{$IFDEF SJLJ_BASED_EXCEPTIONS}
        _DbgExcNotify(excCatch, nil, nil, nil,
          Pointer(PSJLJ_Unwind_FunctionContext(context).jbuf[_PC] and (not 1)) ); // Remove thumb bit from PC address.
{$ELSE  SJLJ_BASED_EXCEPTIONS}
        _DbgExcNotify(excCatch, nil, nil, nil, RaiseFrame.CatchTemp);
{$ENDIF SJLJ_BASED_EXCEPTIONS}
{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
        RaiseFrame.ExceptionFramePtr := GetFramePtr;
{$ENDIF}
      end
      else if CPPException then
      begin
        CPPExceptionPtr := PCXA_EXCEPTION(PByte(@exceptionObject) + SizeOf(_Unwind_Exception) - SizeOf(TCXA_EXCEPTION));
        CPPExceptionObj := UIntPtr(PByte(@exceptionObject) + SizeOf(_Unwind_Exception));

        if isDelphiType(CPPExceptionPtr.exceptionType) then
          E := TObject(Pointer(CPPExceptionObj))
        else
        begin
          if Assigned(ExceptObjProc) then
          begin
            E := ExceptObjProc(exceptionClass);
            if Assigned(CPPExceptionPtr.exceptionDestructor) then
              CPPExceptionPtr.exceptionDestructor(CPPExceptionObj);
          end
          else
            E := nil;
        end;

        if E <> nil then
        begin
          if RaiseExceptObjProc <> nil then
          begin
            ExceptionRecord.ExceptObject := E;
            TRaiseExceptObjProc(RaiseExceptObjProc)(@ExceptionRecord);
          end;
          RaiseFrame := AllocateRaiseFrame;
          RaiseFrame.ExceptObject := E;
          RaiseFrame.ExceptAddr := nil; // ToDo: fill exception address.
          RaiseFrame.UnwindException.exception_class := DelphiExceptionPersonality;
          RaiseFrame.UnwindException.private_1 := UIntPtr(E);
          RaiseFrame.UnwindException.private_2 := 0;
{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
          RaiseFrame.ExceptionFramePtr := GetFramePtr;
{$ENDIF}
          LinkRaiseFrame(RaiseFrame);

          Results := ScanEHTable(actions, DelphiException, exceptionObject, Context);
          if (Results.FReason <> _URC_HANDLER_FOUND) then
            Halt;
          SetRegisters(RaiseFrame.UnwindException, context, Results);
        end
        else
        begin
          // when the non-Delphi exception is not able to convert to Delphi Eception.
          Results := ScanEHTable(Actions, DelphiException, exceptionObject, Context);
          if Results.FReason <> _URC_HANDLER_FOUND then
            Halt;
          SetRegisters(exceptionObject, context, Results);
        end;
      end
      else
      begin
        // not Delphi nor C++ exception

      end;
      Exit(_URC_INSTALL_CONTEXT);
    end;
    Results := ScanEHTable(Actions, DelphiException, exceptionObject, Context);
    if Results.FReason = _URC_HANDLER_FOUND then
    begin
      SetRegisters(exceptionObject, context, Results);
{$IF Defined(ZCX_BASED_EXCEPTIONS) or Defined(ZCX_BASED_EXCEPTIONS_MACOS)}
      if DelphiException then
        PRaiseFrame(RaiseListPtr).ExceptionFramePtr := GetFramePtr;
{$ENDIF}
      Exit(_URC_INSTALL_CONTEXT);
    end;
    Exit(Results.FReason);
  end;
  Result := _URC_FATAL_PHASE1_ERROR;
end;

{$ENDIF SJLJ_BASED_EXCEPTIONS or ZCX_BASED_EXCEPTIONS_MACOS or ZCX_BASED_EXCEPTIONS}

