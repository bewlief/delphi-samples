{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$IFDEF ZCX_BASED_EXCEPTIONS}
{$IF Defined(CPUARM32)}

type
  TUnwindState = record
    data: NativeUInt;
    next: NativeUInt;
    bytes_left: UInt8;
    words_left: UInt8;
  end;

const
  R_IP = 12;
  R_SP = 13;
  R_LR = 14;
  R_PC = 15;

type
  TPersonalityRoutine = function (status: _Unwind_State; var exceptionObject: _Unwind_Exception;
    var context) : _Unwind_Reason_Code; cdecl;

  TExceptionIndexTable = record
    offset,
    content : UInt32;
  end;
{$POINTERMATH ON}
  PExceptionIndexTable = ^TExceptionIndexTable;
{$POINTERMATH OFF}
  TUnwindFindExidxPtr = function(pc: NativeUInt; var pcount: integer) : PExceptionIndexTable;

var
  UnwindFindExidxPtrFunc : TUnwindFindExidxPtr;

function selfrel_offset31(p: NativeUInt): NativeUInt;
const
  b31 = NativeUInt(1) shl 31;
begin
  Result := PCardinal(p)^;
  Result := Result xor (((Result shl 1) xor Result) and b31);
  Result := Result + p;
end;

function search_EIT_table(table : PExceptionIndexTable; nrec: integer; ReturnAddress: NativeUInt): PExceptionIndexTable;
var
  nextFn,
  thisFn: NativeUInt;
  n, left, right: Integer;
begin
  Result := nil;
  if nrec = 0 then Exit;

  left := 0;
  right := nrec - 1;

  while true do
  begin
    n := (left + right) div 2;
    thisFn := selfrel_offset31(NativeUInt(@((table+n)^.offset)));
    if n <> (nrec - 1) then
      nextFn := selfrel_offset31(NativeUInt(@((table+(n+1))^.offset))) - 1
    else
      nextFn := NativeUInt(-1);

    if ReturnAddress < thisFn then
    begin
      if n = left then Exit;
      right := n - 1;
    end
    else if ReturnAddress  <= nextFn then
       Exit( (table+n) )
    else
      left := n + 1;
  end;
end;

function get_eit_entry(var exception_object: _Unwind_Exception; ReturnAddress: NativeUInt) : _Unwind_Reason_Code;
const
  EXIDX_CANTUNWIND = 1;
var
  eitp: PExceptionIndexTable;
  nrec: Integer;
{$IFDEF CPUARM32}
  prID: Byte;
{$ENDIF}
begin
  ReturnAddress := ReturnAddress - 2;

  exception_object.private_2 := 0;
  Result := _URC_FAILURE;
{$IF Defined(CPUARM32) and Defined(Android)}
  eitp := dl_unwind_find_exidx(ReturnAddress, nrec);
{$ELSE !CPUARM32}
  Error(rePlatformNotImplemented);
  eitp := nil;
  nrec := 0;
{$ENDIF CPUARM32}
  if eitp = nil then Exit;

  eitp := search_EIT_table (eitp, nrec, ReturnAddress);

  exception_object.fnstart := selfrel_offset31(NativeUInt(@(eitp.offset)));
  if eitp.content = EXIDX_CANTUNWIND then Exit(_URC_END_OF_STACK);

  if (eitp.content and $80000000) <> 0 then
    // it is immediate data.
    exception_object.ehtp := NativeUInt(@(eitp.content))
  else
    // it is self-relative.
    exception_object.ehtp := selfrel_offset31(NativeUInt(@(eitp.content)));

// TODO -cELBRUS_LLVM_NON_ARM: get_eit_entry
{$IFDEF CPUARM32}
  prID := Byte(PCardinal(exception_object.ehtp)^ shr 24);
  case prID of
    $80: exception_object.private_2 := NativeUInt(@__aeabi_unwind_cpp_pr0);
    $81: exception_object.private_2 := NativeUInt(@__aeabi_unwind_cpp_pr1);
    $82: exception_object.private_2 := NativeUInt(@__aeabi_unwind_cpp_pr2);
    $83..$FF:
      Exit;
  else
    exception_object.private_2 := selfrel_offset31(exception_object.ehtp);
  end;
{$ENDIF CPUARM32}
  Result := _URC_NO_REASON;
end;

procedure UnwindPhase2(var exception_object: _Unwind_Exception; var Register: TReg);
var
  pr_result: _Unwind_Reason_Code;
  PersonalityRoutine : TPersonalityRoutine;
begin
  repeat
// TODO -cELBRUS_LLVM_NON_ARM: UnwindPhase2
{$IFDEF CPUARM32}
    if get_eit_entry(exception_object, Register.GPReg[R_PC]) <> _URC_NO_REASON then
      _Halt0;
{$ENDIF CPUARM32}

{$IFDEF CPUARM32}
    exception_object.reserved[0] := Register.GPReg[R_PC];
{$ENDIF CPUARM32}

    PersonalityRoutine := TPersonalityRoutine(exception_object.private_2);
    pr_result := PersonalityRoutine(_US_UNWIND_FRAME_STARTING, exception_object, Register);
  until (pr_result <> _URC_CONTINUE_UNWIND);

  if pr_result <> _URC_INSTALL_CONTEXT then
    _Halt0;

  _RestoreCoreRegs (Register);
end;

// TODO -cELBRUS_LLVM_NON_ARM: _BorUnwind_RaiseException
function _BorUnwind_RaiseException(var exception_object: _Unwind_Exception; var Register: TReg ): _Unwind_Reason_Code; cdecl;
var
  SavedReg: TReg;
  pr_result: _Unwind_Reason_Code;
  PersonalityRoutine : TPersonalityRoutine;
begin
  Result := _URC_FAILURE;
{$IFDEF CPUARM32}
  Register.GPreg[R_PC] := Register.GPReg[R_LR];
{$ENDIF CPUARM32}
  SavedReg := Register;
  repeat
{$IFDEF CPUARM32}
    if get_eit_entry(exception_object, SavedReg.GPReg[R_PC]) <> _URC_NO_REASON then
      Exit;
{$ENDIF CPUARM32}
    PersonalityRoutine := TPersonalityRoutine(exception_object.private_2);
    pr_result := PersonalityRoutine(_US_VIRTUAL_UNWIND_FRAME, exception_object, SavedReg);
  until (pr_result <> _URC_CONTINUE_UNWIND);

  if pr_result <> _URC_HANDLER_FOUND then
    Exit;

  UnwindPhase2 (exception_object, Register);
end;
exports _BorUnwind_RaiseException;

function _BorUnwind_RaiseException2(var exception_object: _Unwind_Exception; var Register: TReg ): _Unwind_Reason_Code; cdecl;
var
  SavedReg: TReg;
  pr_result: _Unwind_Reason_Code;
  PersonalityRoutine : TPersonalityRoutine;
begin
  NotifyReRaise(GetRaiseFrame(PUnwind_Exception(Register.GPReg[0])^).ExceptObject, Pointer(Register.GPReg[R_PC]));

  Result := _URC_FAILURE;
  SavedReg := Register;
  repeat
{$IFDEF CPUARM32}
    if get_eit_entry(exception_object, SavedReg.GPReg[R_PC]) <> _URC_NO_REASON then
      Exit;
{$ENDIF CPUARM32}
    PersonalityRoutine := TPersonalityRoutine(exception_object.private_2);
    pr_result := PersonalityRoutine(_US_VIRTUAL_UNWIND_FRAME, exception_object, SavedReg);
  until (pr_result <> _URC_CONTINUE_UNWIND);

  if pr_result <> _URC_HANDLER_FOUND then
    Exit;

  UnwindPhase2 (exception_object, Register);
end;
exports _BorUnwind_RaiseException2;

// TODO -cELBRUS_LLVM_NON_ARM: _BorUnwind_Resume
function _BorUnwind_Resume(var exception_object: _Unwind_Exception; var Register: TReg): _Unwind_Reason_Code; cdecl;
var
  pr_result: _Unwind_Reason_Code;
  PersonalityRoutine : TPersonalityRoutine;
begin
{$IFDEF CPUARM32}
  Register.GPReg[R_PC] := exception_object.reserved[0];
{$ENDIF CPUARM32}

//  if (UCB_FORCED_STOP_FN (ucbp))
//      unwind_phase2_forced (ucbp, entry_vrs, 1);

  PersonalityRoutine := TPersonalityRoutine(exception_object.private_2);
  pr_result := PersonalityRoutine(_US_UNWIND_FRAME_RESUME, exception_object, Register);

  if pr_result = _URC_INSTALL_CONTEXT then
    _RestoreCoreRegs (Register)
  else if pr_result = _URC_CONTINUE_UNWIND then
    UnwindPhase2 (exception_object, Register);

  _Halt0;
  Result := _URC_FAILURE;
end;
exports _BorUnwind_Resume;

function UnwindExecute(context: PUnwind_Context; var uws: TUnwindState): _Unwind_Reason_Code;

  function NextCode: byte;
  begin
    if uws.bytes_left = 0 then
    begin
      if uws.words_left = 0 then
        Exit($B0); // op_Finish
      uws.data := PCardinal(uws.next)^;
      Inc(uws.next, sizeof(Cardinal));
      Dec(uws.words_left);
      uws.bytes_left := 4;
    end;

    Result := (uws.data shr 24) and $FF;
    uws.data := uws.data shl 8;
    Dec(uws.bytes_left);
  end;

// It should be rerite to use ReadULEB128
  function ReadULEB128FromUnwindState: UIntPtr;
  var
    Shift: UIntPtr;
    B: Byte;
  begin
    Result := 0;
    Shift := 0;
    repeat
      B := NextCode;
      Result := Result or UIntPtr(B and $7F) shl Shift;
      Inc(Shift, 7);
    until B and $80 = 0;
  end;

var
  op: word;
  reg: UInt32;
  IPIsNotDefined: Boolean;
begin
  Result := _URC_FAILURE;
  IPIsNotDefined := True;
  while True do
  begin
    op := NextCode;

    case byte(op) of
      $00..$3F: // vsp = vsp + (xxxxxx << 2) + 4
        begin
          _Unwind_VRS_Get(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
          reg := reg + (op and $3F) shl 2 + 4;
          _Unwind_VRS_Set(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
        end;
      $40..$7F: // vsp = vsp - (xxxxxx << 2) - 4
        begin
          _Unwind_VRS_Get(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
          reg := reg - (op and $3F) shl 2 - 4;
          _Unwind_VRS_Set(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
        end;
      $80..$8F: // Pop up to 12 integer registers
        begin
          op := (op shl 8) or NextCode;
          if op = $8000 then Exit;
          if _Unwind_VRS_Pop(context, _UVRSC_CORE, (op and $0FFF) shl 4, _UVRSD_UINT32) <> _UVRSR_OK then Exit;
          if (op and $800) = $800 then
            IPIsNotDefined := False; // op contains PC/R15 register
        end;
      $90..$9C,$9E: // Set vsp = r[nnnn]
        begin
          op := op and $0F;
          _Unwind_VRS_Get(context, _UVRSC_CORE, op, _UVRSD_UINT32, reg);
          _Unwind_VRS_Set(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
        end;
      $9D: // Reserved as prefix for ARM register to register moves
        Exit;
      $9F: // Reserved as prefix for Intel Wireless MMX register to register moves
        Exit;
      $A0..$A7: // Pop r4-r[4+nnn]
        begin
          op := op and $07;
          if _Unwind_VRS_Pop(context, _UVRSC_CORE, ($0FF0 shr (7 - op)) and $0FF0, _UVRSD_UINT32) <> _UVRSR_OK then
            Exit;
        end;
      $A8..$AF: // Pop r4-r[4+nnn], r14
        begin
          op := op and $07;
          // $4000 is R14
          if _Unwind_VRS_Pop(context, _UVRSC_CORE, (($0FF0 shr (7 - op)) and $0FF0) or $4000, _UVRSD_UINT32) <> _UVRSR_OK then
            Exit;
        end;
      $B0: // Finish
        begin
          if IPIsNotDefined then
          begin
            reg := 0;
            _Unwind_VRS_Get (context, _UVRSC_CORE, UNWIND_LINK_REG, _UVRSD_UINT32, reg);
            _Unwind_VRS_Set (context, _UVRSC_CORE, UNWIND_IP_REG, _UVRSD_UINT32, reg);
          end;
          Break;
        end;
      $B1: // Pop integer registers under mask {r3, r2, r1, r0}
        begin
          op := NextCode;
          if (op = 0) or (op >= $10) then Exit;
          if _Unwind_VRS_Pop(context, _UVRSC_CORE, op and $0F, _UVRSD_UINT32) <> _UVRSR_OK then
            Exit;
        end;
      $B2: // vsp = vsp + 0x204+ (uleb128 << 2)
        begin
          _Unwind_VRS_Get(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
          reg := reg + $204 + (ReadULEB128FromUnwindState shl 2);
          _Unwind_VRS_Set(context, _UVRSC_CORE, UNWIND_STACK_REG, _UVRSD_UINT32, reg);
        end;
      $B3: // Pop VFP double-precision registers D[ssss]-D[ssss+cccc]
        begin
          op := NextCode;
          if _Unwind_VRS_Pop(context, _UVRSC_VFP, ((op and $F0) shl 12) or ((op and $0F) + 1), _UVRSD_VFPX) <> _UVRSR_OK then
            Exit;
        end;
      $B4..$B7: // Spare
        Exit;
      $B8..$BF: // Pop VFP double-precision registers D[8]-D[8+nnn] s
        begin
          if _Unwind_VRS_Pop(context, _UVRSC_VFP, $40000 or ((op and $07) + 1), _UVRSD_VFPX) <> _UVRSR_OK then
            Exit;
        end;
      $C0..$C5: // Intel Wireless MMX pop wR[10]-wR[10+nnn]
        begin
          if _Unwind_VRS_Pop(context, _UVRSC_VFP, $A0000 or ((op and $07) + 1), _UVRSD_UINT32) <> _UVRSR_OK then
            Exit;
        end;
      $C6:
        begin
          op := NextCode;
          if _Unwind_VRS_Pop(context, _UVRSC_WMMXD, ((op and $F0) shl 12) or ((op and $0F) + 1), _UVRSD_UINT64) <> _UVRSR_OK then
            Exit;
        end;
      $C7: // Intel Wireless MMX pop wCGR registers under mask {
        begin
          op := NextCode;
          if (op = 0) or (op >= $10) then Exit;
          if _Unwind_VRS_Pop(context, _UVRSC_WMMXC, op and $0F, _UVRSD_UINT32) <> _UVRSR_OK then
            Exit;
        end;
      $C8: // Pop VFP double precision registers D[16+ssss]-D[16+ssss+cccc]
        begin
          op := NextCode;
          if _Unwind_VRS_Pop(context, _UVRSC_VFP, (((op and $F0) + (16 * $10)) shl 12) or ((op and $0F) + 1), _UVRSD_DOUBLE) <> _UVRSR_OK then
            Exit;
        end;
      $C9: // Pop VFP double precision registers D[ssss]-D[ssss+cccc] s
        begin
          op := NextCode;
          if _Unwind_VRS_Pop(context, _UVRSC_VFP, ((op and $F0) shl 12) or ((op and $0F) + 1), _UVRSD_DOUBLE) <> _UVRSR_OK then
            Exit;
        end;
      $CA..$CF: // Spare
        Exit;
      $D0..$D7: // Pop VFP double-precision registers D[8]-D[8+nnn]
        begin
          if _Unwind_VRS_Pop(context, _UVRSC_VFP, $80000 or ((op and $07) + 1), _UVRSD_DOUBLE) <> _UVRSR_OK then
            Exit;
        end;
      $D8..$FF: // Spare
        Exit;
    end;
  end;
  Result := _URC_NO_REASON;
end;

function ContinueUnwinding(var ex: _Unwind_Exception; context: PUnwind_Context): _Unwind_Reason_Code;
var
  p : NativeUInt;
  data: Cardinal;
  uws: TUnwindState;
begin
  p := ex.ehtp + sizeof(UInt32); // Skip over the personality routine address.

  data := PCardinal(p)^;

  uws.data := data shl 8;
  uws.next := NativeUInt(p + sizeof(NativeUInt));
  uws.bytes_left := 3;
  uws.words_left := (data shr 24) and $FF;

  Result := UnwindExecute(context, uws);
  if Result = _URC_NO_REASON then
    Result := _URC_CONTINUE_UNWIND
end;

function _DelphiPersonalityRoutine(status: _Unwind_State; var exceptionObject: _Unwind_Exception;
  context: PUnwind_Context) : _Unwind_Reason_Code; cdecl;

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
begin
  if (@exceptionObject = nil) or (@context = nil) then
    Exit(_URC_FATAL_PHASE1_ERROR);
  if status = _US_UNWIND_FRAME_RESUME then
    Exit(ContinueUnwinding(exceptionObject, context))
  else if (status <> _US_VIRTUAL_UNWIND_FRAME) and (status <> _US_UNWIND_FRAME_STARTING) then
    Exit(_URC_FAILURE);

  DelphiException := exceptionObject.exception_class = DelphiExceptionPersonality;
  CPPException := (CPPExceptionPersonalityMask and exceptionObject.exception_class) = CPPExceptionPersonality;

  _Unwind_SetGR (context, UNWIND_POINTER_REG, NativeUInt(@exceptionObject));

  if status = _US_VIRTUAL_UNWIND_FRAME then
  begin
    Results := ScanEHTable(_UA_SEARCH_PHASE, DelphiException, exceptionObject, Context);
    if Results.FReason = _URC_HANDLER_FOUND then
    begin
      if DelphiException then
      begin
        RaiseFrame := PRaiseFrame(RaiseListPtr);
        RaiseFrame.HandlerSwitchValue := Integer(Results.FTypeIndex);
        RaiseFrame.ActionRecord := Results.FActionRecord;
        RaiseFrame.LanguageSpecificData := Results.FLanguageSpecificData;
        RaiseFrame.CatchTemp := Pointer(Results.FLandingPad);
        RaiseFrame.AdjustedPtr := Results.FAdjustedPtr;
        RaiseFrame.ExceptionFramePtr := GetFramePtr;
      end;
      exceptionObject.barrier_cache_sp := _Unwind_GetGR(Context, UNWIND_STACK_REG);
      exceptionObject.barrier_cache_bitpattern[0] := NativeUInt(Results.FAdjustedPtr);
      exceptionObject.barrier_cache_bitpattern[1] := Results.FTypeIndex;
      exceptionObject.barrier_cache_bitpattern[3] := Results.FLandingPad;
      Exit(_URC_HANDLER_FOUND);
    end;
    Exit(ContinueUnwinding(exceptionObject, context));
  end;
  if status = _US_UNWIND_FRAME_STARTING then
  begin
    // Phase 2 search:
    //  Did we find a catching handler in phase 1?
    if exceptionObject.barrier_cache_sp = _Unwind_GetGR(context, UNWIND_STACK_REG) then
    begin
      // Yes, phase 1 said we have a catching handler here.
      // Did we cache the results of the scan?
      if DelphiException then
      begin
        // Yes, reload the results from the cache.
        RaiseFrame := PRaiseFrame(RaiseListPtr);
        Results.FTypeIndex := RaiseFrame.HandlerSwitchValue;
        Results.FActionRecord := RaiseFrame.ActionRecord;
        Results.FLanguageSpecificData := RaiseFrame.LanguageSpecificData;
        Results.FLandingPad := UIntPtr(RaiseFrame.CatchTemp);
        Results.FAdjustedPtr := RaiseFrame.AdjustedPtr;
        SetRegisters(exceptionObject, context, Results);
        _DbgExcNotify(excCatch, nil, nil, nil, RaiseFrame.CatchTemp);
        RaiseFrame.ExceptionFramePtr := GetFramePtr;
      end
      else if CPPException then
      begin
        CPPExceptionPtr := PCXA_EXCEPTION(PByte(@exceptionObject) + SizeOf(_Unwind_Exception) - SizeOf(TCXA_EXCEPTION));
        CPPExceptionObj := UIntPtr(PByte(@exceptionObject) + SizeOf(_Unwind_Exception));

        if isDelphiType(CPPExceptionPtr.exceptionType) then
        begin
          E := TObject(Pointer(CPPExceptionObj))
        end
        else
        begin
          if Assigned(ExceptObjProc) then
          begin
            E := ExceptObjProc(exceptionObject.exception_class);
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
          RaiseFrame.UnwindException.private_1 := 0;
          RaiseFrame.UnwindException.private_2 := 0;
          RaiseFrame.ExceptionFramePtr := GetFramePtr;
          LinkRaiseFrame(RaiseFrame);

          Results := ScanEHTable(_UA_SEARCH_PHASE, DelphiException, exceptionObject, Context);
          if (Results.FReason <> _URC_HANDLER_FOUND) then
            Halt;
          SetRegisters(RaiseFrame.UnwindException, context, Results);
        end
        else
        begin
          // when the non-Delphi exception is not able to convert to Delphi Eception.
          Results := ScanEHTable(_UA_CLEANUP_PHASE or _UA_HANDLER_FRAME, DelphiException, exceptionObject, Context);
          if Results.FReason <> _URC_HANDLER_FOUND then
            Halt;
          SetRegisters(exceptionObject, context, Results);
        end;
      end else
      begin
        Results := ScanEHTable(_UA_CLEANUP_PHASE or _UA_HANDLER_FRAME, DelphiException, exceptionObject, Context);
        if Results.FReason <> _URC_HANDLER_FOUND then
          Halt;
        SetRegisters(exceptionObject, context, Results);
      end;
      Exit(_URC_INSTALL_CONTEXT);
    end;

    Results := ScanEHTable(_UA_CLEANUP_PHASE, DelphiException, exceptionObject, Context);
    if Results.FReason = _URC_HANDLER_FOUND then
    begin
      SetRegisters(exceptionObject, context, Results);
      if DelphiException then
        PRaiseFrame(RaiseListPtr).ExceptionFramePtr := GetFramePtr;
      Exit(_URC_INSTALL_CONTEXT);
    end;

    Exit(ContinueUnwinding(exceptionObject, context));
  end;
  Result := _URC_FATAL_PHASE1_ERROR;
end;

function _UnwindCppFrame(var exceptionObject: _Unwind_Exception; context: PUnwind_Context): _Unwind_Reason_Code;
var
  p : NativeUInt;
  data: Cardinal;
  uws: TUnwindState;
begin
  p := exceptionObject.ehtp + sizeof(Uint32);

  data := PCardinal(p)^;

  uws.data := data shl 8;
  uws.next := NativeUInt(p + sizeof(NativeUInt));
  uws.bytes_left := 3;
  uws.words_left := (data shr 24) and $FF;

  Result := UnwindExecute(context, uws);
end;
exports _UnwindCppFrame;

function UnwindCppPersonalityCommon(status: _Unwind_State; var exceptionObject: _Unwind_Exception;
  context: PUnwind_Context; PersonalityType: Integer): _Unwind_Reason_Code; cdecl;

   function ContinueUnwinding2: _Unwind_Reason_Code;
  var
    p : NativeUInt;
    uws: TUnwindState;
  begin
    p := exceptionObject.ehtp;
    case PersonalityType of
      0:
        begin
          // p: 80 op1 op2 op3
          uws.data := PCardinal(p)^ shl 8;
          uws.next := 0;
          uws.bytes_left := 3;
          uws.words_left := 0;
        end;
      1, 2:
        begin
          // p: 81 len op1 op2
          uws.data := PCardinal(p)^ shl 16;
          uws.next := p + sizeof(Cardinal);
          uws.bytes_left := 2;
          uws.words_left := (PCardinal(p)^ shr 16) and $FF;
        end;
    end;

    Result := UnwindExecute(context, uws);
    if Result = _URC_NO_REASON then
      Result := _URC_CONTINUE_UNWIND
  end;

begin
  if (@exceptionObject = nil) or (@context = nil) then
    Exit(_URC_FATAL_PHASE1_ERROR);

  if status = _US_UNWIND_FRAME_RESUME then
    Exit(ContinueUnwinding2)
  else if (status <> _US_VIRTUAL_UNWIND_FRAME) and (status <> _US_UNWIND_FRAME_STARTING) then
    Exit(_URC_FAILURE);

  _Unwind_SetGR (context, UNWIND_POINTER_REG, NativeUInt(@exceptionObject));

  if status = _US_VIRTUAL_UNWIND_FRAME then
    Exit(ContinueUnwinding2);
  if status = _US_UNWIND_FRAME_STARTING then
    Exit(ContinueUnwinding2);
  Result := _URC_FATAL_PHASE1_ERROR;
end;
exports UnwindCppPersonalityCommon;


procedure _Unwind_DeleteException(exceptionObject: PUnwind_Exception);
var
  ExceptionCleanupFunc: _Unwind_Exception_Cleanup_Fn;
begin
  if exceptionObject <> nil then
  begin
    ExceptionCleanupFunc := exceptionObject.exception_cleanup;
    if @ExceptionCleanupFunc <> nil then
      ExceptionCleanupFunc(_URC_FOREIGN_EXCEPTION_CAUGHT, exceptionObject);
  end;
end;
exports _Unwind_DeleteException;

{$ELSE}
{$MESSAGE ERROR 'Unknown Platform'}
{$ENDIF}

{$ENDIF ZCX_BASED_EXCEPTIONS}
