{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Design.Items;

interface

uses
  SysUtils, Types, Classes, Variants, System.UITypes, TypInfo, FMX.Types, 
  DesignIntf, FmxDesignWindows, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListBox, 
  FMX.Layouts, FMX.TreeView, FMX.StdCtrls, FMX.Objects, FMX.SearchBox, FMX.Controls.Presentation;

type
   // Not every item can accept similar item
   // For Example: TListBoxItem doesn't accept TListBox Item
   //              TMenuItem can accept TMenuItem
   TItemClassDesc = record
     ItemClass: TFmxObjectClass;
     CanContainSimilarItem: Boolean; // Can accept ItemClass Items
     ShowOnlyInMenu: Boolean;
     constructor Create(const AItemClass: TFmxObjectClass; const ACanContaineSimilarItem: Boolean = False;
                        const AShowOnlyInMenu: Boolean = False);
   end;

  TDesignItemsForm = class(TFmxDesignWindow, IFreeNotification)
    ItemsTree: TTreeView;
    ItemsClasses: TComboBox;
    btnAdd: TButton;
    ControlsLayout: TLayout;
    btnAddChild: TButton;
    btnDelete: TButton;
    Layout2: TLayout;
    btnUp: TButton;
    btnDown: TButton;
    Path1: TPath;
    Path2: TPath;
    procedure btnAddClick(Sender: TObject);
    procedure btnAddChildClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure ItemsTreeChange(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure ItemsTreeDragChange(SourceItem, DestItem: TTreeViewItem; var Allow: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FItemsDescription: array of TItemClassDesc;
    FContainer: IItemsContainer;
    FDeletedItem: TTreeViewItem;
    procedure UpdateTree;
    procedure UpdateStates;
    function AcceptsChildItem(const AItem: TObject): Boolean;
    procedure RealignItems;
  protected
    procedure FreeNotification(AObject: TObject); override;
  public
    destructor Destroy; override;
    procedure SetItemClasses(const AContainer: IItemsContainer; const AItemDescriptions: array of TItemClassDesc);
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
  end;

var
  DesignItemsForm: TDesignItemsForm;

implementation

uses
  Math, FmxDsnConst;

{$R *.fmx}

type
  TCloseWatcher = class
    procedure DesignerClosed(Sender: TObject; var Action: TCloseAction);
  end;

var
  Watcher: TCloseWatcher;

procedure TCloseWatcher.DesignerClosed(Sender: TObject; var Action: TCloseAction);
begin
  if Sender = DesignItemsForm then
  begin
    Action := TCloseAction.caFree;
    DesignItemsForm := nil;
  end;
end;

procedure TDesignItemsForm.SetItemClasses(const AContainer: IItemsContainer; const AItemDescriptions: array of TItemClassDesc);
var
  I: Integer;
  Item: TListBoxItem;
  ItemIndex: Integer;
begin
  if Length(AItemDescriptions) = 0 then Exit;

  ItemsClasses.Clear;

  SetLength(FItemsDescription, Length(AItemDescriptions));
  ItemIndex := 0;
  for I := 0 to High(AItemDescriptions) do
  begin
    if AItemDescriptions[I].ShowOnlyInMenu then
      Continue;
    Item := TListBoxItem.Create(nil);
    Item.Parent := ItemsClasses;
    Item.Text := AItemDescriptions[I].ItemClass.ClassName;
    Item.TextAlign := TTextAlign.Center;

    FItemsDescription[ItemIndex] := AItemDescriptions[I];
    Inc(ItemIndex);
  end;
  SetLength(FItemsDescription, ItemIndex);
  ItemsClasses.ItemIndex := 0;

  FContainer := AContainer;
  FContainer.GetObject.AddFreeNotify(Self);

  UpdateTree;
end;

function TDesignItemsForm.AcceptsChildItem(const AItem: TObject): Boolean;

  function FindItemDescription(AItemClass: TClass): Integer;
  var
    I: Integer;
    Founded: Boolean;
  begin
    I := 0;
    Founded := False;
    while (I < Length(FItemsDescription)) and not Founded do
      if FItemsDescription[I].ItemClass = AItemClass then
        Founded := True
      else
        Inc(I);
    if Founded then
      Result := I
    else
      Result := -1;
  end;

var
  Index: Integer;
begin
  Result := True;
  if Assigned(AItem) then
  begin
    Index := FindItemDescription(AItem.ClassType);
    Result := (Index <> -1) and FItemsDescription[Index].CanContainSimilarItem;
  end;
end;

procedure TDesignItemsForm.btnAddChildClick(Sender: TObject);
var
  Node: TTreeViewItem;
  Item: TFmxObject;
  SelectedItem: TTreeViewItem;
  ItemParent: TFmxObject;
begin
  Node := TTreeViewItem.Create(Self);
  SelectedItem:= ItemsTree.Selected;
  if ItemsTree.Selected <> nil then
    Node.Parent := ItemsTree.Selected
  else
    Node.Parent := ItemsTree;

  if Node.ParentItem <> nil then
    ItemParent := TFmxObject(Node.ParentItem.TagObject)
  else
    ItemParent := FContainer.GetObject;

  Item := TfmxObject(Designer.CreateChild(FItemsDescription[ItemsClasses.ItemIndex].ItemClass, ItemParent));

  if GetPropInfo(Item, 'text') <> nil then
    Node.Text := GetStrProp(Item, 'text');
  if Node.Text = '' then
    Node.Text := Item.Name;
  if Node.Text = '' then
    Node.Text := SUnnamed;
  Node.TagObject := Item;

//  ItemsTree.Selected := Node;
  ItemsTree.Selected:= SelectedItem;
  ItemsTree.Selected.IsExpanded:= True;

  UpdateStates;
end;

procedure TDesignItemsForm.btnAddClick(Sender: TObject);
var
  Node: TTreeViewItem;
  Item: TFmxObject;
  ItemParent: TFmxObject;
begin
  Node := TTreeViewItem.Create(Self);
  if ItemsTree.Selected <> nil then
    Node.Parent := ItemsTree.Selected.Parent
  else
    Node.Parent := ItemsTree;

  if Node.ParentItem <> nil then
    ItemParent := TFmxObject(Node.ParentItem.TagObject)
  else
    ItemParent := TFmxObject(FContainer.GetObject);

  Item := TFmxObject(Designer.CreateChild(FItemsDescription[ItemsClasses.ItemIndex].ItemClass, ItemParent));

  if GetPropInfo(Item, 'text') <> nil then
    Node.Text := GetStrProp(Item, 'text');
  if Node.Text = '' then
    Node.Text := Item.Name;
  if Node.Text = '' then
    Node.Text := SUnnamed;
  Node.TagObject := Item;

  ItemsTree.Selected := Node;

  UpdateStates;
end;

procedure TDesignItemsForm.btnDeleteClick(Sender: TObject);
var
  Idx: integer;
begin
  if ItemsTree.Selected <> nil then
  begin
    Idx := ItemsTree.Selected.GlobalIndex;

    Designer.DeleteSelection(False);

    if ItemsTree.GlobalCount > 0 then
      if Idx > 0 then
        ItemsTree.Selected := ItemsTree.ItemByGlobalIndex(Idx - 1)
      else
        ItemsTree.Selected := ItemsTree.ItemByGlobalIndex(0);

    UpdateStates;
  end;
end;

procedure TDesignItemsForm.btnDownClick(Sender: TObject);
var
  OldIndex: Integer;
  Modified: Boolean;
begin
  { down }
  Modified := False;
  if ItemsTree.Selected <> nil then
  begin
    ItemsTree.Selected.Index := ItemsTree.Selected.Index + 1;
    if ItemsTree.Selected.TagObject <> nil then
    begin
      OldIndex := TFmxObject(ItemsTree.Selected.TagObject).Index;
      TFmxObject(ItemsTree.Selected.TagObject).Index := TFmxObject(ItemsTree.Selected.TagObject).Index + 1;
      Modified := OldIndex <> TFmxObject(ItemsTree.Selected.TagObject).Index;
    end;
    RealignItems;
  end;
  UpdateStates;
  if Modified then
    Designer.Modified;
end;

procedure TDesignItemsForm.btnUpClick(Sender: TObject);
var
  OldIndex: Integer;
  Modified: Boolean;
begin
  { up }
  Modified := False;
  if ItemsTree.Selected <> nil then
  begin
    ItemsTree.Selected.Index := ItemsTree.Selected.Index - 1;
    if ItemsTree.Selected.TagObject <> nil then
    begin
      OldIndex := TFmxObject(ItemsTree.Selected.TagObject).Index;
      TFmxObject(ItemsTree.Selected.TagObject).Index := TFmxObject(ItemsTree.Selected.TagObject).Index - 1;
      Modified := OldIndex <> TFmxObject(ItemsTree.Selected.TagObject).Index;
    end;
    RealignItems;
  end;
  UpdateStates;
  if Modified then
    Designer.Modified;
end;

procedure TDesignItemsForm.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  inherited;
  if Assigned(FContainer) then
  begin
    FContainer.GetObject.RemoveFreeNotify(Self);
    FContainer := nil;
  end;
  Close;
end;

destructor TDesignItemsForm.Destroy;
begin
  if Assigned(FContainer) then
  begin
    FContainer.GetObject.RemoveFreeNotify(Self);
    FContainer := nil;
  end;
  inherited;
end;

procedure TDesignItemsForm.FormCreate(Sender: TObject);
const
  ButtonMargin: Integer = 8;
var
  RequiredWidth: Integer;
begin
  // Layout buttons for localization
  RequiredWidth := Round(Canvas.TextWidth(btnAdd.Text)) + ButtonMargin * 2;
  RequiredWidth := Math.Max(RequiredWidth,
    Round(Canvas.TextWidth(btnAddChild.Text)) + ButtonMargin * 2);

  // Finally examine the UpDownDelete buttons as a group
  RequiredWidth := Math.Max(RequiredWidth,
    Round(btnUp.Width) + ButtonMargin + Round(btnDown.Width) + ButtonMargin +
    Round(Canvas.TextWidth(btnDelete.Text)) + ButtonMargin * 2);

  // Make the changes
  ControlsLayout.Width := RequiredWidth + 2 * ButtonMargin;

  // Setup the watcher
  if not Assigned(Watcher) then
    Watcher := TCloseWatcher.Create;
  OnClose := Watcher.DesignerClosed;
end;

procedure TDesignItemsForm.FreeNotification(AObject: TObject);
begin
  inherited;
  if Assigned(FContainer) and (AObject = FContainer.GetObject) then
  begin
    FContainer := nil;
    Close;
  end;
end;

procedure TDesignItemsForm.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
var
  I: Integer;
begin
  inherited;
  { check for deletion }
  for I := 0 to ItemsTree.GlobalCount - 1 do
    if ItemsTree.ItemByGlobalIndex(I).TagObject = Item then
    begin
      FDeletedItem := ItemsTree.ItemByGlobalIndex(I);
      FDeletedItem.Free;
      Exit;
    end;
end;

procedure TDesignItemsForm.ItemsModified(const Designer: IDesigner);
var
  I, J: Integer;
  Sel: TDesignerSelections;
  Obj: TPersistent;
  Node: TTreeViewItem;
begin
  inherited;
  { check selection for object in tree }
  Sel := TDesignerSelections.Create;
  Designer.GetSelections(Sel);
  for I := 0 to IDesignerSelections(Sel).Count - 1 do
  begin
    Obj := IDesignerSelections(Sel).Items[i];
    for J := 0 to ItemsTree.GlobalCount - 1 do
    begin
      Node := ItemsTree.ItemByGlobalIndex(J);
      if Node.TagObject = Obj then
      begin
        { change text }
        if GetPropInfo(Obj, 'text') <> nil then
          Node.Text := GetStrProp(Obj, 'text');
        if Node.Text = '' then
          if Obj is TComponent then
            Node.Text := TComponent(Obj).Name
          else
            Node.Text := SUnnamed;
        Break;
      end;
    end;
  end;
end;

procedure TDesignItemsForm.ItemsTreeChange(Sender: TObject);
begin
  if (ItemsTree.Selected <> nil) and (ItemsTree.Selected.TagObject <> nil) then
    Designer.SelectComponent(TFmxObject(ItemsTree.Selected.TagObject))
  else
    Designer.SelectComponent(nil);
  UpdateStates;
end;

procedure TDesignItemsForm.ItemsTreeDragChange(SourceItem,
  DestItem: TTreeViewItem; var Allow: Boolean);
var
  i: integer;
  Modified: Boolean;
begin
  Modified := False;
  Allow := btnAddChild.Visible;
  if Allow then
  begin
    if Assigned(DestItem) and not (SourceItem.IsChild(TFmxObject(DestItem)))
        and AcceptsChildItem(DestItem.TagObject) then
    begin
      TFmxObject(SourceItem.TagObject).Parent := TFmxObject(DestItem.TagObject);
      Modified := True;
    end
    else
    begin
      if DestItem = nil then
        if not (TFmxObject(SourceItem).Parent is TScrollContent) then
        begin
          TFmxObject(SourceItem.TagObject).Parent:= FContainer.GetObject;
          Modified := True;
        end
        else
        begin
          for i := 1 to ItemsTree.Count - SourceItem.Index - 1  do
          begin
            TFmxObject(SourceItem).Index:= TFmxObject(SourceItem).Index + 1;
            TFmxObject(SourceItem.TagObject).Index:= TFmxObject(SourceItem.TagObject).Index  + 1;
            Modified := True;
          end;
        end
      else
        //changing the order of the THeaderItems/ TListBoxItem/ TTabItem; dragged item will be moved before/after DestItem
        if not AcceptsChildItem(DestItem.TagObject) then
        begin
          if DestItem.Index > SourceItem.Index then
            for i := 1 to DestItem.Index - SourceItem.Index  do
            begin
              TFmxObject(SourceItem).Index:= TFmxObject(SourceItem).Index + 1;
              TFmxObject(SourceItem.TagObject).Index:= TFmxObject(SourceItem.TagObject).Index  + 1;
              Modified := True;
            end
          else
            for i := 1 to SourceItem.Index - DestItem.Index  do
            begin
              TFmxObject(SourceItem).Index:= TFmxObject(SourceItem).Index - 1;
              TFmxObject(SourceItem.TagObject).Index:= TFmxObject(SourceItem.TagObject).Index  - 1;
              Modified := True;
            end
        end;
    end;
  end;
  UpdateStates;
  RealignItems;
  if Modified then
    Designer.Modified;
end;

procedure TDesignItemsForm.RealignItems;
var
  AlignRoot: IAlignRoot;
begin
  if FContainer.GetObject.GetInterface(IAlignRoot, IInterface(AlignRoot)) then
  begin
    AlignRoot.Realign;
    AlignRoot := nil;
  end;
  Designer.SelectComponent(TFmxObject(ItemsTree.Selected.TagObject));
end;

procedure TDesignItemsForm.UpdateStates;
  function AreAllItemsStored: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to ItemsTree.GlobalCount - 1 do
      if not TFmxObject(ItemsTree.ItemByGlobalIndex(I).TagObject).Stored then
        Exit(False);
  end;
var
  Editable: Boolean;
begin
  Editable := AreAllItemsStored;
  btnUp.Enabled := Editable and (ItemsTree.Selected <> nil) and (ItemsTree.Selected.Index > 0);
  btnDown.Enabled := Editable and (ItemsTree.Selected <> nil) and (ItemsTree.Selected.Parent <> nil) and (ItemsTree.Selected.Index < ItemsTree.Selected.Parent.ChildrenCount - 1);
  btnDelete.Enabled := Editable and (Designer.GetAncestorDesigner = nil) and (ItemsTree.Selected <> nil);
  btnAddChild.Enabled := Editable and (ItemsTree.Selected <> nil);
  btnAdd.Enabled := Editable;
end;

procedure TDesignItemsForm.UpdateTree;

 procedure UpdateItem(AItem: IItemsContainer; AParentNode: TFmxObject);
  var
    Node: TTreeViewItem;
    ChildContainer: IItemsContainer;
    IsItem: Boolean;
    i, j: integer;
  begin
    for i := 0 to AItem.GetItemsCount - 1 do
    begin
      IsItem := false;
      for j := 0 to High(FItemsDescription) do
        if (AItem.GetItem(i) is FItemsDescription[j].ItemClass) and (AItem.GetItem(i).Owner <> nil) then
        begin
          IsItem := true;
          Break;
        end;
      if not IsItem then Continue;

      Node := TTreeViewItem.Create(nil);
      Node.Parent := AParentNode;
      if GetPropInfo(AItem.GetItem(i), 'text') <> nil then
        Node.Text := GetStrProp(AItem.GetItem(i), 'text');
      if Node.Text = '' then
        Node.Text := AItem.GetItem(i).Name;
      if Node.Text = '' then
        Node.Text := SUnnamed;
      Node.TagObject := AItem.GetItem(i);
      if Supports(AItem.GetItem(i), IItemsContainer, ChildContainer) then
        UpdateItem(ChildContainer, Node);
    end;
  end;

begin
  if FContainer = nil then Exit;
  ItemsTree.Clear;
  UpdateItem(FContainer, ItemsTree);
  UpdateStates;
end;

{ TItemClassDesc }

constructor TItemClassDesc.Create(const AItemClass: TFmxObjectClass;
  const ACanContaineSimilarItem: Boolean;
  const AShowOnlyInMenu: Boolean);
begin
  Self.ItemClass := AItemClass;
  Self.CanContainSimilarItem := ACanContaineSimilarItem;
  Self.ShowOnlyInMenu := AShowOnlyInMenu;
end;

initialization

finalization

  if Assigned(DesignItemsForm) then
    DesignItemsForm.Free;

  if Assigned(Watcher) then
    Watcher.Free;

end.
