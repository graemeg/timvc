unit lcl_controllers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  ,Classes
  ,StdCtrls
  ,ExtCtrls
  ,ComCtrls
  ,EditBtn
  ,ColorBox
  ,Spin
  ,Grids
  ,Variants
  ,tiObject
  ,widget_controllers
  ,mvc_base
  ;

type

  {: Edit controller. }
  TEditController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TEdit; reintroduce;
    constructor Create(AModel: TtiObject; AView: TEdit; const AModelAttrib: string); reintroduce;
  end;

  {: Memo controller. }
  TMemoController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TMemo; reintroduce;
    constructor Create(AModel: TtiObject; AView: TMemo; const AModelAttrib: string); reintroduce;
  end;

  {: Label controller. }
  TLabelController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
  public
    function    View: TLabel; reintroduce;
    constructor Create(AModel: TtiObject; AView: TLabel; const AModelAttrib: string); reintroduce;
  end;

  {: Checkbox controller. }
  TCheckBoxController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TCheckBox; reintroduce;
    constructor Create(AModel: TtiObject; AView: TCheckBox; const AModelAttrib: string); reintroduce;
  end;

  {: DateTimePicker controller. }
  TDateTimePickerController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TDateEdit; reintroduce;
    constructor Create(AModel: TtiObject; AView: TDateEdit; const AModelAttrib: string); reintroduce;
  end;

  {: Listbox controller. }
  TListBoxController = class(TBaseListController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   BuildList; override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TListBox; reintroduce;
    constructor Create(AModel: TtiObject; AView: TListBox; const AModelAttrib: string); reintroduce;
  end;

  {: ComboBox controller. }
  TComboBoxController = class(TBaseListController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   BuildList; override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TComboBox; reintroduce;
    constructor Create(AModel: TtiObject; AView: TComboBox; const AModelAttrib: string); reintroduce;
  end;

  {: ListBox controller which mantches the itemindex to a property of the subject which can
  be treated as ordinal. Good for enum/integer properties.}
  TListBoxIndexController = class(TBaseListController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TListBox; reintroduce;
    constructor Create(AModel: TtiObject; AView: TListBox; const AModelAttrib: string); reintroduce;
  end;

  {: ComboBox controller which mantches the itemindex to a property of the subject. }
  TComboBoxIndexController = class(TBaseListController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TComboBox; reintroduce;
    constructor Create(AModel: TtiObject; AView: TComboBox; const AModelAttrib: string); reintroduce;
  end;

  TSpinEditController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TSpinEdit; reintroduce;
    constructor Create(AModel: TtiObject; AView: TSpinEdit; const AModelAttrib: string); reintroduce;
  end;

  {: ListItem observer. }
  TListViewItemController = class(TMVCController)

  end;

  {: TListView controller. }
  TListViewController = class(TBaseListController)
  private
    function GetSelectedItem: TtiObject;
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   PrepareView;
    procedure   BuildList; override;
    procedure   BuildItem(AObject: TtiObject);
    procedure   UpdateItem(AObject: TtiObject);
    procedure   RemoveItemControllers;
    procedure   HandleListViewOnSelect;
  public
    property    SelectedItem: TtiObject read GetSelectedItem;
    function    View: TListView; reintroduce;
    function    Model: TtiObjectList; reintroduce;
    procedure   Update(ASubject: TtiObject); override;
    constructor Create(AModel: TtiObjectList; AView: TListView; const AModelAttrib: string); reintroduce;
  end;

  {: TStringGrid controller. }
  TGridController = class(TBaseListController)
  private
    FSelectedList: TtiObjectList;
    function GetSelectedItem: TtiObject;

    function SelectedList: TtiObjectList;
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   PrepareView;
    procedure   DoSizeColumns; virtual;
    procedure   BuildList; override;
    procedure   BuildItem(AObject: TtiObject);
    procedure   UpdateItem(AObject: TtiObject);
    procedure   DoCreateView; override;
    function    GetSelItems: TtiObjectList; virtual;
  public
    property    SelectedItem: TtiObject read GetSelectedItem;
    function    SelectedCount: Integer; virtual;
    property    SelectedItems: TtiObjectList read GetSelItems;
    function    View: TStringGrid; reintroduce;
    function    Model: TtiObjectList; reintroduce;
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation); override;
    constructor Create(AModel: TtiObjectList; AView: TStringGrid; const AModelAttrib: string); reintroduce;
    destructor  Destroy; override;
  end;

  {: Read only Combo box controller. }
  TReadOnlyComboBoxController = class(TComboBoxController)
  protected
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   ModelToView; override;
  end;

  {: TColorBox controller. }
  TColorBoxController = class(TBaseListController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TColorBox; reintroduce;
    constructor Create(AModel: TtiObject; AView: TColorBox; const AModelAttrib: string); reintroduce;
  end;

  {: Maps an ordinal property of the model object with the ItemIndex property of the TRadioGroup
  component in the LCL. }
  TRadioGroupController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    function    View: TRadioGroup; reintroduce;
    constructor Create(AModel: TtiObject; AView: TRadioGroup; const AModelAttrib: string); reintroduce;
  end;

const
    ON_LiST_SELECTION = 'OnListSelection';

  procedure RegisterControllers;

implementation

uses
  tiRTTI
  ,TypInfo
  ,Forms
  ,Controls
  ,Graphics
  ,Dialogs
  ;

procedure RegisterControllers;
begin
  gCtrlFactory.RegisterMapping(TEdit, TEditController);
  gCtrlFactory.RegisterMapping(TMemo, TMemoController);
  gCtrlFactory.RegisterMapping(TLabel, TLabelController);
  gCtrlFactory.RegisterMapping(TCheckBox, TCheckBoxController);
  gCtrlFactory.RegisterMapping(TSpinEdit, TSpinEditController);
  gCtrlFactory.RegisterMapping(TDateEdit, TDateTimePickerController);
  gCtrlFactory.RegisterMapping(TColorBox, TColorBoxController);
  gCtrlFactory.RegisterMapping(TRadioGroup, TRadioGroupController);
end;

{ TEditController }

constructor TEditController.Create(AModel: TtiObject; AView: TEdit;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  FViewAttrib := 'Text';

  Assert(tiIsPublishedProp(Model, FModelAttrib), ClassName + '.Create: ' +
    'The attribute ' + FModelAttrib + ' is not a published property');
end;

procedure TEditController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TEditController.ModelToView;
begin
  View.Text := tiGetProperty(Model, FModelAttrib);
end;

procedure TEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if not Active then
    begin
      View.Text := '';
      View.OnChange := nil;
    end
  else
    begin
      ModelToView;
      View.OnChange := @self.HandleChangeEvent;
    end;
end;

function TEditController.View: TEdit;
begin
  result := inherited View as TEdit;
end;

procedure TEditController.ViewToModel;
begin
  if Active then
    tiSetProperty(Model, FModelAttrib, View.Text);
end;

{ TMemoController }

constructor TMemoController.Create(AModel: TtiObject; AView: TMemo;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  FViewAttrib := 'Text';
end;

procedure TMemoController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TMemoController.ModelToView;
begin
  View.Text := tiGetProperty(Model, FModelAttrib);
end;

procedure TMemoController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
      View.OnChange := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnChange := nil;
      View.Text := '';
    end;
end;

function TMemoController.View: TMemo;
begin
  result := inherited View as TMemo;
end;

procedure TMemoController.ViewToModel;
begin
  tiSetProperty(Model, FModelAttrib, View.Text);
end;

{ TLabelController }

constructor TLabelController.Create(AModel: TtiObject; AView: TLabel;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  FViewAttrib := 'Caption';
end;

procedure TLabelController.ModelToView;
begin
  View.Caption := tiGetProperty(Model, FModelAttrib);
end;

procedure TLabelController.SetActive(const AValue: Boolean);
var
  lString: string;
begin
  inherited;
  if not Active then
    View.Caption := ''
  else
    begin
      lString := tiGetProperty(Model, FModelAttrib);
      View.Caption := lString;
    end;
end;

function TLabelController.View: TLabel;
begin
  result := inherited View as TLabel;
end;

procedure TLabelController.ViewToModel;
begin
  // Read only.  Do nothing.
end;

{ TCheckBoxController }

constructor TCheckBoxController.Create(AModel: TtiObject; AView: TCheckBox;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  FViewAttrib := 'Checked';
end;

procedure TCheckBoxController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TCheckBoxController.ModelToView;
begin
  View.Checked := tiGetProperty(Model, FModelAttrib);
end;

procedure TCheckBoxController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
      View.OnClick := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnClick := nil;
      View.Checked := False;
    end;
end;

function TCheckBoxController.View: TCheckBox;
begin
  result := inherited View as TCheckBox;
end;

procedure TCheckBoxController.ViewToModel;
begin
  tiSetProperty(Model, FModelAttrib, View.Checked);
end;

{ TDateTimePickerController }

constructor TDateTimePickerController.Create(AModel: TtiObject;
  AView: TDateEdit; const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TDateTimePickerController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TDateTimePickerController.ModelToView;
begin
  View.Date := tiGetProperty(Model, FModelAttrib);
end;

procedure TDateTimePickerController.SetActive(const AValue: Boolean);
begin
  inherited;
  View.Enabled := Active;
  if Active then
    begin
      View.OnChange := @Self.HandleChangeEvent;
      ModelToView;
    end
  else
    begin
      View.OnChange := nil;
    end;
end;

function TDateTimePickerController.View: TDateEdit;
begin
  result := inherited View as TDateEdit;
end;

procedure TDateTimePickerController.ViewToModel;
begin
  tiSetProperty(Model, FModelAttrib, View.Date);
end;

{ TListBoxController }

procedure TListBoxController.BuildList;
var
  lCounter: Integer;
  lObj: TtiObject;
  lVal: string;
begin

  Assert(ListData <> nil, ClassName + '.BuildList: No List data assigned.');

  View.Clear;
  View.Items.BeginUpdate;
  try
    for lCounter := 0 to ListData.Count - 1 do
      begin
        lObj := ListData.Items[lCounter];
        lVal := tiGetProperty(lObj, ListDisplayProp);
        View.Items.Add(lVal);
      end;

  finally
    View.Items.EndUpdate;
  end;
end;

constructor TListBoxController.Create(AModel: TtiObject; AView: TListBox;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TListBoxController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
  if Assigned(OnSelection) then
    OnSelection(Sender);
end;

procedure TListBoxController.ModelToView;
var
  lIdx: Integer;
  lObj: TtiObject;
  lVal: Variant;
begin

  // If FListData is not nil, the controller is bound to an external object list for its items.
  // If it IS nil, then we are using items that already in the list manually.

  if FListData <> nil then
    begin
        lVal := tiGetProperty(Model, FModelAttrib);
        if VarIsNull(lVal) then
          exit;
        // Find the object in the ListData list with the ListValueName matching
        // the Model's ModelAttrib field value.
        if LowerCase(ListValueProp) = 'oid' then
          lObj := ListData.Find(lVal)
        else
          lObj := ListData.FindByProps([ListValueProp], [lval]);

        if lObj <> nil then
          View.ItemIndex := lObj.Index;
    end
  else
    begin
      lVal := Model.PropValue[FModelAttrib];
      lIdx := View.Items.IndexOf(lVal);
      View.ItemIndex := lIdx;
    end;

  //lVal := tiGetProperty(Model, FModelAttrib);
  //// Find the object in the ListData list with the ListValueName matching
  //// the Model's ModelAttrib field value.
  //if LowerCase(ListValueProp) = 'oid' then
  //  lObj := ListData.Find(lVal)
  //else
  //  lObj := ListData.FindByProps([ListValueProp], [lval]);
  //
  //if lObj <> nil then
  //  View.ItemIndex := lObj.Index;

end;

procedure TListBoxController.SetActive(const AValue: Boolean);
begin
  inherited;
  if not Active then
    begin
      View.OnClick := nil;
      View.ItemIndex := -1
    end
  else
    begin
      BuildList;
      ModelToView;
      View.OnClick := @self.HandleChangeEvent;
    end;
end;

function TListBoxController.View: TListBox;
begin
  result := inherited View as TListBox;
end;

procedure TListBoxController.ViewToModel;
var
  lVal: Variant;
begin

  if FListData <> nil then
    begin
      if View.ItemIndex >= 0 then
        begin
          if LowerCase(ListValueProp) = 'oid' then
            lVal := ListData.Items[View.ItemIndex].OID.AsString
          else
            lVal := tiGetProperty(ListData.Items[View.ItemIndex], ListValueProp);

          tiSetProperty(Model, FModelAttrib, lVal);
        end;
    end
  else
    begin
      if View.ItemIndex >= 0 then
        tiSetProperty(Model, FModelAttrib, View.Items[View.ItemIndex]);
    end;

end;

{ TComboBoxController }

procedure TComboBoxController.BuildList;
var
  lCounter: Integer;
  lObj: TtiObject;
  lVal: string;
begin

  Assert(ListData <> nil, ClassName + '.BuildList: No List data assigned.');

  View.Clear;
  View.Items.BeginUpdate;
  try
    for lCounter := 0 to ListData.Count - 1 do
      begin
        lObj := ListData.Items[lCounter];
        lVal := tiGetProperty(lObj, ListDisplayProp);
        View.Items.Add(lVal);
      end;

  finally
    View.Items.EndUpdate;
  end;
end;

constructor TComboBoxController.Create(AModel: TtiObject; AView: TComboBox;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TComboBoxController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
  if Assigned(OnSelection) then
    OnSelection(Sender);
end;

procedure TComboBoxController.ModelToView;
var
  lIdx: Integer;
  lObj: TtiObject;
  lVal: Variant;
begin

  // If FListData is not nil, the controller is bound to an external object list for its items.
  // If it IS nil, then we are using items that already in the list manually.

  if FListData <> nil then
    begin
        lVal := tiGetProperty(Model, FModelAttrib);
        if VarIsNull(lVal) then
          exit;
        // Find the object in the ListData list with the ListValueName matching
        // the Model's ModelAttrib field value.
        if LowerCase(ListValueProp) = 'oid' then
          lObj := ListData.Find(lVal)
        else
          lObj := ListData.FindByProps([ListValueProp], [lval]);

        if lObj <> nil then
          View.ItemIndex := lObj.Index;
    end
  else
    begin
      lVal := Model.PropValue[FModelAttrib];
      lIdx := View.Items.IndexOf(lVal);
      View.ItemIndex := lIdx;
    end;

end;

procedure TComboBoxController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      if FListData <> nil then
        BuildList;
      ModelToView;
      View.OnChange := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnChange := nil;
      View.ItemIndex := -1;
    end;
end;

function TComboBoxController.View: TComboBox;
begin
  result := inherited View as TComboBox;
end;

procedure TComboBoxController.ViewToModel;
var
  lVal: Variant;
begin

  if FListData <> nil then
    begin
      if View.ItemIndex >= 0 then
        begin
          if LowerCase(ListValueProp) = 'oid' then
            lVal := ListData.Items[View.ItemIndex].OID.AsString
          else
            lVal := tiGetProperty(ListData.Items[View.ItemIndex], ListValueProp);

          tiSetProperty(Model, FModelAttrib, lVal);
        end;
    end
  else
    begin
      tiSetProperty(Model, FModelAttrib, View.Text);
    end;

end;

{ TListBoxIndexController }

constructor TListBoxIndexController.Create(AModel: TtiObject; AView: TListBox;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TListBoxIndexController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
  if Assigned(OnSelection) then
    OnSelection(Sender);
end;

procedure TListBoxIndexController.ModelToView;
var
  lInteger: Integer;
begin
  lInteger := TypInfo.GetOrdProp(Model, FModelAttrib);
  View.ItemIndex := lInteger;

end;

procedure TListBoxIndexController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      View.OnClick := @self.HandleChangeEvent;
      ModelToView;
    end
  else
    begin
      View.OnClick := nil;
      View.ItemIndex := -1;
    end;
end;

function TListBoxIndexController.View: TListBox;
begin
  result := inherited View as TListBox;
end;

procedure TListBoxIndexController.ViewToModel;
begin
  if View.ItemIndex >= 0 then
    begin
      tiSetProperty(Model, FModelAttrib, View.ItemIndex);
    end;
end;

{ TComboBoxIndexController }

constructor TComboBoxIndexController.Create(AModel: TtiObject;
  AView: TComboBox; const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TComboBoxIndexController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
  if Assigned(OnSelection) then
    OnSelection(Sender);
end;

procedure TComboBoxIndexController.ModelToView;
var
  lInteger: Integer;
begin
  lInteger := TypInfo.GetOrdProp(Model, FModelAttrib);
  View.ItemIndex := lInteger;
end;

procedure TComboBoxIndexController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
      View.OnChange := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnChange := nil;
      View.ItemIndex := -1;
    end;
end;

function TComboBoxIndexController.View: TComboBox;
begin
  result := inherited View as TComboBox;
end;

procedure TComboBoxIndexController.ViewToModel;
begin
  tiSetProperty(Model, FModelAttrib, View.ItemIndex);
end;

{ TSpinEditController }

constructor TSpinEditController.Create(AModel: TtiObject; AView: TSpinEdit;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TSpinEditController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TSpinEditController.ModelToView;
begin
  View.Value := tiGetProperty(Model, FModelAttrib);
end;

procedure TSpinEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
      View.OnChange := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnChange := nil;
      View.Value := -1;
    end;
end;

function TSpinEditController.View: TSpinEdit;
begin
  result := inherited View as TSpinEdit;
end;

procedure TSpinEditController.ViewToModel;
begin
  if View.Text <>'' then
    tiSetProperty(Model, FModelAttrib, View.Value);
end;

{ TListViewController }

procedure TListViewController.BuildItem(AObject: TtiObject);
var
  lItem: TListItem;
  lCounter: Integer;
  lCol: TWidgetColumn;
  lClassName: string;
begin
  lClassName := AObject.ClassName;
  lCol := Columns.Items[0];

  lItem := View.Items.Add;
  if lCol.RendererIndex >= 0 then
    lItem.Caption := Renderers.Items[lCol.RendererIndex].RenderValue(AObject, lCol.ModelAttrib)
  else
    lItem.Caption := tiGetProperty(AObject, lCol.ModelAttrib);

  for lCounter := 1 to Columns.Count - 1 do
    begin
      lCol := Columns.Items[lCounter];
      if lCol.RendererIndex >= 0 then
        lItem.SubItems.Add(Renderers.Items[lCol.RendererIndex].RenderValue(AObject, lCol.ModelAttrib))
      else
        lItem.SubItems.Add(tiGetProperty(AObject, lCol.ModelAttrib));
    end;

  AObject.AttachObserver(self);

end;

procedure TListViewController.BuildList;
var
  lItem: TListItem;
  lCounter: Integer;
begin
  if Model = nil then
    exit;

  View.Items.BeginUpdate;
  try
    PrepareView;
    for lCounter := 0 to Model.Count - 1 do
      begin
        BuildItem(Model.Items[lCounter]);
      end;
  finally
    View.Items.EndUpdate;
  end;
end;

constructor TListViewController.Create(AModel: TtiObjectList; AView: TListView;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  Assert(AModel is TtiObjectList, ClassName + '.Create: Model must be of type ' +
    'TtiObjectList');
end;

function TListViewController.GetSelectedItem: TtiObject;
begin
  if View.Selected = nil then
    begin
      result := nil;
      exit;
    end
  else
    begin
      Result := Model.Items[View.Selected.Index];
    end;
end;

procedure TListViewController.HandleListViewOnSelect;
begin

end;

function TListViewController.Model: TtiObjectList;
begin
  result := inherited Model as TtiObjectList;
end;

procedure TListViewController.ModelToView;
begin

end;

procedure TListViewController.PrepareView;
var
  lCounter: Integer;
  lCol: TListColumn;
  lExistCol: TWidgetColumn;
begin
  View.Items.Clear;
  View.ReadOnly := True;
  View.RowSelect := True;
  View.ViewStyle := vsReport;
  View.Columns.Clear;

  for lCounter := 0 to Columns.Count - 1 do
    begin
      lExistCol := Columns.Items[lCounter];
      lCol := View.Columns.Add;
      lCol.Caption := lExistCol.ColCaption;
      lCol.Alignment := TAlignment(lExistCol.Align);
      lCol.Width := lExistCol.Width;
      lCol.AutoSize := lExistCol.AutoSize;
    end;

end;

procedure TListViewController.RemoveItemControllers;
var
  lCtr: Integer;
begin
  for lCtr := 0 to Model.Count - 1 do
    begin
      Model.Items[lCtr].DetachObserver(self);
    end;
end;

procedure TListViewController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      BuildList;
    end
  else
    begin
      View.Items.BeginUpdate;
      try
        DetachFromItems;
        View.Items.Clear;
      finally
        View.Items.EndUpdate;
      end;
    end;
end;

procedure TListViewController.Update(ASubject: TtiObject);
begin
  inherited;
  if not Active then
    exit;
  if ASubject is TtiObjectList then
    begin
      BuildList;
    end
  else
    begin
      UpdateItem(ASubject);
    end;
end;

procedure TListViewController.UpdateItem(AObject: TtiObject);
var
  lCol: TWidgetColumn;
  lItem: TListItem;
  lCounter: Integer;
begin
  lItem := View.Items[AObject.Index];

  lCol := Columns.Items[0];

  if lCol.RendererIndex >= 0 then
    lItem.Caption := Renderers.Items[lCol.RendererIndex].RenderValue(AObject, lCol.ModelAttrib)
  else
    lItem.Caption := tiGetProperty(AObject, lCol.ModelAttrib);

  for lCounter := 1 to Columns.Count - 1 do
    begin
      lCol := Columns.Items[lCounter];
      if lCol.RendererIndex >= 0 then
        lItem.SubItems[lCounter-1] := Renderers.Items[lCol.RendererIndex].RenderValue(AObject, lCol.ModelAttrib)
      else
        lItem.SubItems[lCounter-1] := tiGetProperty(AObject, lCol.ModelAttrib);
    end;

end;

function TListViewController.View: TListView;
begin
  Result := inherited View as TListView;
end;

procedure TListViewController.ViewToModel;
begin

end;

{ TGridController }

procedure TGridController.BuildItem(AObject: TtiObject);
var
  lItem: TListItem;
  lCounter: Integer;
  lCol: TWidgetColumn;
  lVal: string;
begin

  for lCounter := 0 to Columns.Count - 1 do
    begin
      lCol := Columns.Items[lCounter];
      if lCol.RendererIndex >= 0 then
        lVal := Renderers.Items[lCol.RendererIndex].RenderValue(AObject, lCol.ModelAttrib)
      else
        lVal := tiGetProperty(AObject, lCol.ModelAttrib);

      View.Cells[lCounter, AObject.Index + 1] := lVal;
    end;

  AObject.AttachObserver(self);
end;

procedure TGridController.BuildList;
var
  lCounter: Integer;
begin
  View.RowCount := Model.Count + 1;

  Screen.Cursor := crHourGlass;
  try
    for lCounter := 0 to Model.Count - 1 do
      begin
        BuildItem(Model.Items[lCounter]);
      end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

constructor TGridController.Create(AModel: TtiObjectList; AView: TStringGrid;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  FSelectedList := TtiObjectList.Create;
  FSelectedList.OwnsObjects := False;
end;

destructor TGridController.Destroy;
begin
  FSelectedList.Free;
  inherited Destroy;
end;

procedure TGridController.DoCreateView;
begin
  PrepareView;
end;

procedure TGridController.DoSizeColumns;
var
  lCounter: Integer;
  lCol: TListColumn;
  lExistCol: TWidgetColumn;
  lTotalCol: Integer;
  lNonContentWidth: Integer;
  lLastColumnWidth: Integer;
begin
  lTotalCol := 0;
  //View.RowCount := Model.Count + 1;
  lNonContentWidth := 2 + 2 + (Columns.Count -1);

  for lCounter := 0 to Columns.Count - 1 do
    begin
      lExistCol := Columns.Items[lCounter];

      if lCounter = Columns.Count - 1 then
        begin
          if View.Width > (lTotalCol + lNonContentWidth) then
            begin
              lLastColumnWidth := View.Width - (lTotalCol + lNonContentWidth);
              if lLastColumnWidth > 10 then
                View.ColWidths[lCounter] := lLastColumnWidth -20;
            end;
        end
      else
        View.ColWidths[lCounter] := lExistCol.Width;
        lTotalCol := lTotalCol + View.ColWidths[lCounter];
    end;

end;

function TGridController.GetSelectedItem: TtiObject;
begin
  result := nil;

  if View.Row >= 1 then
    begin
      Result := Model.Items[View.Row -1];
    end;
end;

function TGridController.GetSelItems: TtiObjectList;
begin

end;

function TGridController.Model: TtiObjectList;
begin
  result := inherited Model as TtiObjectList;
end;

procedure TGridController.ModelToView;
begin

end;

procedure TGridController.PrepareView;
var
  lCounter: Integer;
  lCol: TListColumn;
  lExistCol: TWidgetColumn;
  lTotalCol: Integer;
  lNonContentWidth: Integer;
  lLastColumnWidth: Integer;
begin

  for lCounter := 0 to Columns.Count - 1 do
    begin
      lExistCol := Columns.Items[lCounter];
      View.Cols[lCounter].Text := lExistCol.ColCaption;
    end;

  View.DefaultRowHeight := 20;
  View.ColCount := Columns.Count;
  View.FixedRows := 1;
  View.Options := View.Options + [goRowSelect];
  View.Options := View.Options - [goRangeSelect];
  DoSizeColumns;

end;

function TGridController.SelectedCount: Integer;
begin
  Result := FSelectedList.Count;
end;

function TGridController.SelectedList: TtiObjectList;
begin
  result := FSelectedList;
end;

procedure TGridController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      BuildList;
    end
  else
    begin
      View.RowCount := 2;
    end;
end;

procedure TGridController.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin
if (ASubject is TtiObjectList) and (AOperation = noChanged) then
    begin
      BuildList;
    end
  else if (AOperation = noChanged) then

    begin
      UpdateItem(ASubject);
    end;
end;

procedure TGridController.UpdateItem(AObject: TtiObject);
var
  lCol: TWidgetColumn;
  lCounter: Integer;
begin

  for lCounter := 0 to Columns.Count - 1 do
    begin
      lCol := Columns.Items[lCounter];
      if lCol.RendererIndex >= 0 then
        View.Cells[lCounter, AObject.Index +1] := Renderers.Items[lCol.RendererIndex].RenderValue(AObject, lCol.ModelAttrib)
      else
        View.Cells[lCounter, AObject.Index +1] := tiGetProperty(AObject, lCol.ModelAttrib);
    end;

end;

function TGridController.View: TStringGrid;
begin
  Result := inherited View as TStringGrid;
end;

procedure TGridController.ViewToModel;
begin

end;

{ TReadOnlyComboBoxController }

procedure TReadOnlyComboBoxController.ModelToView;
begin

end;

procedure TReadOnlyComboBoxController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
    end
  else
    begin
      View.ItemIndex := -1;
    end;
end;

procedure TReadOnlyComboBoxController.ViewToModel;
begin

end;

{ TColorBoxController }

constructor TColorBoxController.Create(AModel: TtiObject; AView: TColorBox;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TColorBoxController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TColorBoxController.ModelToView;
var
  lColor: TColor;
begin
  lColor := tiGetProperty(Model, FModelAttrib);
  View.Selected := lColor;

end;

procedure TColorBoxController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
      View.OnChange := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnChange := nil;
      View.Selected := clBlack;
    end;
end;

function TColorBoxController.View: TColorBox;
begin
  result := inherited View as TColorBox;
end;

procedure TColorBoxController.ViewToModel;
begin
  tiSetProperty(Model, FModelAttrib, View.Selected);
end;

{ TRadioGroupController }

constructor TRadioGroupController.Create(AModel: TtiObject; AView: TRadioGroup;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
end;

procedure TRadioGroupController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TRadioGroupController.ModelToView;
var
  lInt: Integer;
begin
  lInt := TypInfo.GetOrdProp(Model, FModelAttrib);
  View.ItemIndex := lInt;

end;

procedure TRadioGroupController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      ModelToView;
      View.OnClick := @self.HandleChangeEvent;
    end
  else
    begin
      View.OnClick := nil;
      View.ItemIndex := -1;
    end;
end;

function TRadioGroupController.View: TRadioGroup;
begin
  result := inherited View as TRadioGroup;
end;

procedure TRadioGroupController.ViewToModel;
begin
  TypInfo.SetOrdProp(Model, FModelAttrib, View.ItemIndex);
end;

end.

