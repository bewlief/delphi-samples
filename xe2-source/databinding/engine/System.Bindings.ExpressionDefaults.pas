{*******************************************************}
{                                                       }
{             Delphi LiveBindings Framework             }
{                                                       }
{ Copyright(c) 2011 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit System.Bindings.ExpressionDefaults;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections, System.Math, System.Bindings.Consts,

  System.Bindings.EvalProtocol, System.Bindings.EvalSys, System.Bindings.Evaluator, System.Bindings.ObjEval,
  System.Bindings.NotifierContracts, System.Bindings.Expression, System.Bindings.Manager;

type
  /// <summary>Class that implements the default behaviour for binding expressions.</summary>
  TBindingExpressionDefault = class(TBindingExpression,
    IScope, IScopeEx, IScopeEnumerator, IScopeSymbols,
    ICompiledBinding, ICompiledBindingWrappers)
  private
    FBinding: ICompiledBinding;
    FRootScope: IScope;
    FCompiled: Boolean;
    FManager: TBindingManager;
    FScopeSymbols: IScopeSymbols;
    FCompiledBinding: ICompiledBinding;

    function GetRootScopeEx: IScopeEx; inline;
    function GetRootScopeEnumerator: IScopeEnumerator; inline;
    function GetBindingWrappers: ICompiledBindingWrappers; inline;
  protected
    function GetCompiled: Boolean; override;

    /// <summary>Creates a scope based on the passed associations between Delphi
    /// objects and expression objects.</summary>
    /// <param name="Assocs">Indicates the associations between Delphi objects
    /// and expression objects that are going to be added in the resultin scope.</param>
    /// <returns>A scope containing the associations passed as input.</returns>
    function CreateScope(const Assocs: TBindingExpression.TAssociations): IScope;
    procedure Compile; overload; override;
    function GetIsManaged: Boolean; override;

    { IInterface }

    /// <summary>Denotes the scope of the expression that contains all the
    /// wrappers for the rest of the symbols in the expression.</summary>
    property RootScope: IScope read FRootScope implements IScope;
    /// <summary>Delegates the implementation for suplimentary scope functionality.</summary>
    property RootScopeEx: IScopeEx read GetRootScopeEx implements IScopeEx;
    /// <summary>Delegates the enumerator of the expression scope and makes the
    /// expression enumerable in terms of wrappers.</summary>
    property RootScopeEnumerator: IScopeEnumerator read GetRootScopeEnumerator implements IScopeEnumerator;
    /// <summary>Delegates the implementation of IScopeSymbols to an internal object.</summary>
    property ScopeSymbols: IScopeSymbols read FScopeSymbols implements IScopeSymbols;
    /// <summary>The compiled binding through which evaluation of the expression can occur.</summary>
    property Binding: ICompiledBinding read FCompiledBinding implements ICompiledBinding;
    /// <summary>The wrappers used by the compiled binding in order to evaluate the expression.</summary>
    property BindingWrappers: ICompiledBindingWrappers read GetBindingWrappers implements ICompiledBindingWrappers;
  public
    /// <summary>Assigning a manager doesn't mean that the expression is added
    /// in the manager's list. Instances of this class should not be created
    /// using directly this constructor, but using a binding manager instead.</summary>
    constructor Create(Manager: TBindingManager = nil);
    destructor Destroy; override;

    /// <summary>The binding manager owning the expression.</summary>
    property Manager: TBindingManager read FManager;
    function Evaluate: IValue; override;
    procedure EvaluateOutputs; override;
    procedure Clear; override;
  end;

implementation

uses
  System.TypInfo, System.Bindings.Factories, System.Bindings.Graph;

{ TBindingExpressionDefault }

procedure TBindingExpressionDefault.Compile;
var
  LScope: IScope;
  LParentScope: IScope;
begin
  try
    // Create nested scopes as needed
    LParentScope := TNestedScope.Create(BasicOperators, BasicConstants);
    for LScope in FScopes do
      LParentScope := TNestedScope.Create(LParentScope,
      LScope);
    if Associations.Count > 0 then
      LParentScope := TNestedScope.Create(LParentScope,
        CreateScope(Associations));
    FRootScope := LParentScope;

    // compile the binding and store it
    FBinding := System.Bindings.Evaluator.Compile(Source, FRootScope);
    FCompiled := True;
  except on E: Exception do
    begin
      if Assigned(OnEvalErrorEvent) then
        OnEvalErrorEvent(E);
      raise;
    end;
  end;
end;

constructor TBindingExpressionDefault.Create(Manager: TBindingManager);
begin
//  if not Assigned(Manager) then
//    raise EBindingExpressionError.Create(sManagerNotFound);

  // the manager must be assigned before the creation because the inherited
  // constructor determines whether the outputs must notify or not based
  // on the IsManaged property of the expression
  FManager := Manager;

  inherited Create;

  FScopeSymbols := TScopeSymbols.Create;
end;

function TBindingExpressionDefault.CreateScope(const Assocs: TBindingExpression.TAssociations): IScope;
var
  LScope: TDictionaryScope;
  LAssocPair: TPair<TObject, String>;
begin
  // add the mappings to a local and specific scope for the expression
  LScope := TDictionaryScope.Create;
  Result := LScope;

  for LAssocPair in Assocs do
  begin
                                                                                               
    //  and possibly remove them on TObjectWrapper.Destroy? (with ref count)
    LScope.Map.Add(LAssocPair.Value, WrapObject(LAssocPair.Key));
  end;
end;

destructor TBindingExpressionDefault.Destroy;
begin
  // remove the expression from its owner manager; if it fails in the constructor,
  // it won't signal an error
  if Assigned(FManager) then
    // in destroy so use extract instead of remove
    FManager.Extract(Self);

  inherited;
end;

function TBindingExpressionDefault.Evaluate: IValue;
begin
  if not FCompiled then
    raise EBindingExpressionError.Create(sUncompiledExpression);

  try
    Result := FBinding.Evaluate(FRootScope, nil, nil);
    OutputValue := Result.GetValue;
    SetOutputs(
      function: IValue
      begin
        Result := TValueWrapper.Create(OutputValue);
      end
    );
  except on E: Exception do
    begin
      if Assigned(OnEvalErrorEvent) then
        OnEvalErrorEvent(E);
      raise;
    end;
  end;
end;

procedure TBindingExpressionDefault.EvaluateOutputs;
begin
  if not FCompiled then
    raise EBindingExpressionError.Create(sUncompiledExpression);

  SetOutputs(
    function: IValue
    begin
      Result := FBinding.Evaluate(FRootScope, nil, nil);
    end
  );
end;

function TBindingExpressionDefault.GetBindingWrappers: ICompiledBindingWrappers;
begin
  Supports(FBinding, ICompiledBindingWrappers, Result);
end;

function TBindingExpressionDefault.GetCompiled: Boolean;
begin
  Result := FCompiled;
end;

function TBindingExpressionDefault.GetIsManaged: Boolean;
begin
  Result := FManager <> nil;
end;

function TBindingExpressionDefault.GetRootScopeEnumerator: IScopeEnumerator;
begin
  Supports(FRootScope, IScopeEnumerator, Result);
end;

function TBindingExpressionDefault.GetRootScopeEx: IScopeEx;
begin
  Supports(FRootScope, IScopeEx, Result);
end;

procedure TBindingExpressionDefault.Clear;
begin
  inherited;

  FRootScope := nil;
  FBinding := nil;
  FCompiled := False;
end;

end.
