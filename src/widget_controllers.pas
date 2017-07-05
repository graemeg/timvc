unit widget_controllers;

{$IFDEF fpc}
  {$mode objfpc}{$H+}
{$ENDIF}


interface
uses
  SysUtils
  ,Classes
  ,tiObject
  ,tiRTTI
  ,mvc_base
  ;

type

  TWidgetColumn = class;
  TWidgetControllerClass = class of TWidgetController;

  TColumnAlign = (caLeft, caRight, caCenter);

  {: Base widget controller. }
  TWidgetController = class(TMVCController)
  private
    FListDisplayProp: string;
    FListValueProp: string;
    procedure SetListData(const Value: TtiObjectList);
    procedure SetModelAttrib(const Value: string);
    procedure SetViewAttrib(const Value: string);
    procedure SetListDisplayProp(const Value: string);
    procedure SetListValueProp(const Value: string);
  protected
    FListData: TtiObjectList;
    FViewAttrib: string;
    FModelAttrib: string;
    procedure   ModelToView; virtual; abstract;
    procedure   ViewToModel; virtual; abstract;
    procedure   SetActive(const AValue: Boolean); override;
  public
    procedure   Update(ASubject: TtiObject); override;
    function    Model: TtiObject; reintroduce;
    property    ListData: TtiObjectList read FListData write SetListData;
    property    ListDisplayProp: string read FListDisplayProp write SetListDisplayProp;
    property    ListValueProp: string read FListValueProp write SetListValueProp;
    constructor Create(AModel: TtiObject; AView: TObject; const AModelAttrib: string); reintroduce; virtual;
  published
    property    ModelAttrib: string read FModelAttrib write SetModelAttrib;
    property    ViewAttrib: string read FViewAttrib write SetViewAttrib;
  end;

  TWidgetControllerList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TWidgetController; reintroduce;
    procedure   SetItems(i: integer; const AValue: TWidgetController); reintroduce;
  public
    property    Items[i:integer] : TWidgetController read GetItems write SetItems;
    procedure   Add(AObject : TWidgetController); reintroduce;
  end;

  {: Custom renderers for column values in lists. }
  TColumnRenderer = class(TtiObject)
  protected
    FCol: TWidgetColumn;
  public
    function    RenderValue(AObject: TtiObject; const AModelAttrib: string): string; virtual; abstract;
  end;

  TColumnRendererList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TColumnRenderer; reintroduce;
    procedure   SetItems(i: integer; const AValue: TColumnRenderer); reintroduce;
  public
    property    Items[i:integer] : TColumnRenderer read GetItems write SetItems;
    procedure   Add(AObject : TColumnRenderer); reintroduce;
  end;

  {: Column definition for list based widget controllers. }
  TWidgetColumn = class(TtiObject)
  private
    FAlign: TColumnAlign;
    FModelAttrib: string;
    FRenderer: TColumnRenderer;
    FAutoSize: Boolean;
    FWidth: Integer;
    FColCaption: string;
    FRendererIndex: Integer;
    procedure SetAlign(const Value: TColumnAlign);
    procedure SetModelAttrib(const Value: string);
    procedure SetRenderer(const Value: TColumnRenderer);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetColCaption(const Value: string);
    procedure SetRendererIndex(const Value: Integer);
  public
    destructor  Destroy; override;
    constructor Create; override;
  published
    property    ColCaption: string read FColCaption write SetColCaption;
    property    ModelAttrib: string read FModelAttrib write SetModelAttrib;
    property    Align: TColumnAlign read FAlign write SetAlign;
    property    Renderer: TColumnRenderer read FRenderer write SetRenderer;
    property    RendererIndex: Integer read FRendererIndex write SetRendererIndex;
    property    Width: Integer read FWidth write SetWidth;
    property    AutoSize: Boolean read FAutoSize write SetAutoSize;
  end;

  {: List of TWidgetColumn objects. }
  TWidgetColumnList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TWidgetColumn; reintroduce;
    procedure   SetItems(i: integer; const AValue: TWidgetColumn); reintroduce;
  public
    property    Items[i:integer] : TWidgetColumn read GetItems write SetItems;
    procedure   Add(AObject : TWidgetColumn); reintroduce;
  end;

  {: Base constroller for lists. }
  TBaseListController = class(TWidgetController)
  private
    FColumns: TWidgetColumnList;
    FOnSelection: TNotifyEvent;
    FRenderers: TColumnRendererList;
    procedure SetColumns(const Value: TWidgetColumnList);
    procedure SetOnSelection(const AValue: TNotifyEvent);
  protected
    property    Renderers: TColumnRendererList read FRenderers;
    procedure   BuildList; virtual;
    procedure   DetachFromItems;
  public
    property    OnSelection: TNotifyEvent read FOnSelection write SetOnSelection;
    function    AddRenderer(ARenderer: TColumnRenderer): TColumnRenderer; virtual;
    function    AddNewCol(const AModelAttrib: string; const ACaption: string = ''): TWidgetColumn;
    constructor Create(AModel: TtiObject; AView: TObject; const AModelAttrib: string); override;
    destructor  Destroy; override;
    property    Columns: TWidgetColumnList read FColumns write SetColumns;
  end;

  {: Registration of a object class with a  widget  controller. }
  TControllerWidgetMap = class(TtiObject)
  private
    FControllerClass: TWidgetControllerClass;
    FControllerClassName: string;
    FGUIObjectClass: TClass;
    FGUIObjectClassName: string;
    procedure SetGUIObjectClass(const Value: TClass);
    procedure SetGUIObjectClassName(const Value: string);
    procedure SetControllerClass(const Value: TWidgetControllerClass);
    procedure SetControllerClassName(const Value: string);
  published
    property    GUIObjectClass: TClass read FGUIObjectClass write SetGUIObjectClass;
    property    GUIObjectClassName: string read FGUIObjectClassName write SetGUIObjectClassName;
    property    ControllerClass: TWidgetControllerClass read FControllerClass write SetControllerClass;
    property    ControllerClassName: string read FControllerClassName write SetControllerClassName;
  end;

  TControllerWidgetMapList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TControllerWidgetMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TControllerWidgetMap); reintroduce;
  public
    property    Items[i:integer] : TControllerWidgetMap read GetItems write SetItems;
    procedure   Add(AObject : TControllerWidgetMap); reintroduce;
  end;

  TControllerWidgetManager = class(TtiObject)
  private
    FMappings: TControllerWidgetMapList;
  public
    procedure   RegisterMapping(const AWidgetClass: TClass;
      AControllerClass: TWidgetControllerClass);
    function    New(AControl: TObject; AModel: TtiObject;
      const AModelAttrib: string): TWidgetController;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TFormatFloatRenderer = class(TColumnRenderer)
  private
    FFormatStr: string;
    procedure SetFormatStr(const Value: string);
  public
    function RenderValue(AObject: TtiObject; const AModelAttrib: string): string; override;
    property    FormatStr: string read FFormatStr write SetFormatStr;
  end;

  TRegionalDateRenderer = class(TColumnRenderer)
  public
    function RenderValue(AObject: TtiObject;
      const AModelAttrib: string): string; override;
  end;

  TRegionalDateTimeRenderer = class(TColumnRenderer)
  public
    function RenderValue(AObject: TtiObject;
      const AModelAttrib: string): string; override;
  end;

  TRegionalTimeRenderer = class(TColumnRenderer)
  public
    function RenderValue(AObject: TtiObject;
      const AModelAttrib: string): string; override;
  end;

  TBooleanRenderer = class(TColumnRenderer)
  public
    function RenderValue(AObject: TtiObject;
      const AModelAttrib: string): string; override;
  end;

  // -----------------------------------------------------------------
  //  Singletons
  // -----------------------------------------------------------------

  function gCtrlFactory: TControllerWidgetManager;




implementation
var
  mControllerMgr: TControllerWidgetManager;

function gCtrlFactory: TControllerWidgetManager;
begin
  if mControllerMgr = nil then
    mControllerMgr := TControllerWidgetManager.Create;
  result := mControllerMgr;
end;

{ TVCLControllerList }

procedure TWidgetControllerList.Add(AObject: TWidgetController);
begin
  inherited Add(AObject);
end;

function TWidgetControllerList.GetItems(i: integer): TWidgetController;
begin
  result:= inherited GetItems(i) as TWidgetController;
end;

procedure TWidgetControllerList.SetItems(i: integer; const AValue: TWidgetController);
begin
  inherited SetItems(i, AValue);
end;

{ TWidgetController }

constructor TWidgetController.Create(AModel: TtiObject; AView: TObject;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView);
  FModelAttrib := AModelAttrib;
end;

function TWidgetController.Model: TtiObject;
begin
  result := inherited Model as TtiObject;
end;

procedure TWidgetController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    Model.AttachObserver(self)
  else
    Model.DetachObserver(self);
end;

procedure TWidgetController.SetListData(const Value: TtiObjectList);
begin
  FListData := Value;
end;

procedure TWidgetController.SetListDisplayProp(const Value: string);
begin
  FListDisplayProp := Value;
end;

procedure TWidgetController.SetListValueProp(const Value: string);
begin
  FListValueProp := Value;
end;

procedure TWidgetController.SetModelAttrib(const Value: string);
begin
  FModelAttrib := Value;
end;

procedure TWidgetController.SetViewAttrib(const Value: string);
begin
  FViewAttrib := Value;
end;

procedure TWidgetController.Update(ASubject: TtiObject);
begin
  inherited;
  if Active then
    ModelToView;
end;

{ TWidgetColumn }

constructor TWidgetColumn.Create;
begin
  inherited;
  FRendererIndex := -1;
end;

destructor TWidgetColumn.Destroy;
begin
  if FRenderer <> nil then
    FRenderer.Free;
  inherited;
end;

procedure TWidgetColumn.SetAlign(const Value: TColumnAlign);
begin
  FAlign := Value;
end;

procedure TWidgetColumn.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
end;

procedure TWidgetColumn.SetColCaption(const Value: string);
begin
  FColCaption := Value;
end;

procedure TWidgetColumn.SetModelAttrib(const Value: string);
begin
  FModelAttrib := Value;
end;

procedure TWidgetColumn.SetRenderer(const Value: TColumnRenderer);
begin
  FRenderer := Value;
end;

procedure TWidgetColumn.SetRendererIndex(const Value: Integer);
begin
  FRendererIndex := Value;
end;

procedure TWidgetColumn.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

{ TWidgetColumnList }

procedure TWidgetColumnList.Add(AObject: TWidgetColumn);
begin
  inherited Add(AObject);
end;

function TWidgetColumnList.GetItems(i: integer): TWidgetColumn;
begin
  result:= inherited GetItems(i) as TWidgetColumn;
end;

procedure TWidgetColumnList.SetItems(i: integer; const AValue: TWidgetColumn);
begin
  inherited SetItems(i, AValue);
end;

{ TBaseListController }

function TBaseListController.AddNewCol(
  const AModelAttrib: string; const ACaption: string = ''): TWidgetColumn;
var
  lCol: TWidgetColumn;
begin
  lCol := FColumns.FindByProps(['ModelAttrib'], [AModelAttrib]) as TWidgetColumn;
  if lCol = nil then
    begin
      lCol := TWidgetColumn.Create;
      lCol.ModelAttrib := AModelAttrib;
      FColumns.Add(lCol);
    end;

  result := lCol;

end;

function TBaseListController.AddRenderer(
  ARenderer: TColumnRenderer): TColumnRenderer;
begin
  ARenderer.Owner := Self;
  FRenderers.Add(ARenderer);
end;

procedure TBaseListController.BuildList;
begin
  // do nothing, allow to fall through
end;

constructor TBaseListController.Create(AModel: TtiObject; AView: TObject;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  FColumns := TWidgetColumnList.Create;
  FRenderers := TColumnRendererList.Create;
end;

destructor TBaseListController.Destroy;
begin
  FColumns.Free;
  FRenderers.Free;
  inherited;
end;

procedure TBaseListController.DetachFromItems;
var
  lCounter: Integer;
  lList: TtiObjectList;
begin
  lList := TtiObjectList(Model);
  for lCounter := 0 to lList.Count - 1 do
    begin
      lList.Items[lCounter].DetachObserver(self);
    end;
end;

procedure TBaseListController.SetColumns(const Value: TWidgetColumnList);
begin
  FColumns := Value;
end;

procedure TBaseListController.SetOnSelection(const AValue: TNotifyEvent);
begin
  FOnSelection:=AValue;
end;

{ TControllerWidgetMap }

procedure TControllerWidgetMap.SetGUIObjectClass(const Value: TClass);
begin
  FGUIObjectClass := Value;
end;

procedure TControllerWidgetMap.SetGUIObjectClassName(const Value: string);
begin
  FGUIObjectClassName := Value;
end;

procedure TControllerWidgetMap.SetControllerClass(const Value: TWidgetControllerClass);
begin
  FControllerClass := Value;
end;

procedure TControllerWidgetMap.SetControllerClassName(const Value: string);
begin
  FControllerClassName := Value;
end;

{ TControllerWidgetMapList }

procedure TControllerWidgetMapList.Add(AObject: TControllerWidgetMap);
begin
  inherited Add(AObject);
end;

function TControllerWidgetMapList.GetItems(i: integer): TControllerWidgetMap;
begin
  result:= inherited GetItems(i) as TControllerWidgetMap;
end;

procedure TControllerWidgetMapList.SetItems(i: integer;
  const AValue: TControllerWidgetMap);
begin
  inherited SetItems(i, AValue);
end;

{ TControllerWidgetManager }

constructor TControllerWidgetManager.Create;
begin
  inherited;
  FMappings := TControllerWidgetMapList.Create;
end;

function TControllerWidgetManager.New(AControl: TObject;
  AModel: TtiObject; const AModelAttrib: string): TWidgetController;
var
  lReg: TControllerWidgetMap;
begin
  lReg := TControllerWidgetMap(FMappings.FindByProps(['GUIObjectClassName'],
    [AControl.ClassName]));
  Assert(lReg <> nil, ClassName + '.CreateController: No registration found');
  Result := lReg.FControllerClass.Create(AModel, AControl, AModelAttrib);

end;

destructor TControllerWidgetManager.Destroy;
begin
  FMappings.Free;
  inherited;
end;

procedure TControllerWidgetManager.RegisterMapping(const AWidgetClass: TClass;
  AControllerClass: TWidgetControllerClass);
var
  lReg: TControllerWidgetMap;
begin
  lReg := TControllerWidgetMap(FMappings.FindByProps(['GUIObjectClassName'],
    [AWidgetClass.ClassName]));
  if lReg = nil then
    begin
      lReg := TControllerWidgetMap.Create;
      lReg.GUIObjectClass := AWidgetClass;
      lReg.GUIObjectClassName := AWidgetClass.ClassName;
      lReg.ControllerClass := AControllerClass;
      lReg.ControllerClassName := AControllerClass.ClassName;
      FMappings.Add(lReg);
    end;
end;

{ TFormatFloatRenderer }

function TFormatFloatRenderer.RenderValue(AObject: TtiObject; const AModelAttrib: string): string;
var
  lFloatVal: Extended;
begin
  lFloatVal := tiGetProperty(AObject, AModelAttrib);
  Result := FormatFloat(FFormatStr, lFloatVal);
end;

procedure TFormatFloatRenderer.SetFormatStr(const Value: string);
begin
  FFormatStr := Value;
end;

{ TColumnRendererList }

procedure TColumnRendererList.Add(AObject: TColumnRenderer);
begin
  inherited Add(AObject);
end;

function TColumnRendererList.GetItems(i: integer): TColumnRenderer;
begin
  result:= inherited GetItems(i) as TColumnRenderer;
end;

procedure TColumnRendererList.SetItems(i: integer;
  const AValue: TColumnRenderer);
begin
  inherited SetItems(i, AValue);
end;

{ TRegionalDateRenderer }

function TRegionalDateRenderer.RenderValue(AObject: TtiObject;
  const AModelAttrib: string): string;
var
  lValue: TDateTime;
begin
  lValue := tiGetProperty(AObject, AModelAttrib);
  result := DateToStr(lValue);
end;

{ TRegionalDateTimeRenderer }

function TRegionalDateTimeRenderer.RenderValue(AObject: TtiObject;
  const AModelAttrib: string): string;
var
  lDate: TDateTime;
begin
  lDate := tiGetProperty(AObject, AModelAttrib);
  result := DateTimeToStr(lDate);
end;

{ TRegionalTimeRenderer }

function TRegionalTimeRenderer.RenderValue(AObject: TtiObject;
  const AModelAttrib: string): string;
var
  lTime: TDateTime;
begin
  lTime := tiGetProperty(AObject, AModelAttrib);
  Result := TimeToStr(lTime);
end;

{ TBooleanRenderer }

function TBooleanRenderer.RenderValue(AObject: TtiObject;
  const AModelAttrib: string): string;
var
  lType: TtiTypeKind;
  lBool: Boolean;
begin
  lType := AObject.PropType(AModelAttrib);
  lBool := AObject.PropValue[AModelAttrib];
  result := BoolToStr(lBool, true);
end;

initialization

finalization
  if mControllerMgr <> nil then
    mControllerMgr.Free;
end.
