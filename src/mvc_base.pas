unit mvc_base;

interface
uses
  SysUtils
  ,Classes
  ,tiObject
  ;

type

  // -----------------------------------------------------------------
  //  Class Of
  // -----------------------------------------------------------------

  TMVCCommandClass = class of TMVCCommand;
  TMVCControllerClass = class of TMVCController;

  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  TListenerState = (lsActive, lsInactive, lsDeleted);

  // -----------------------------------------------------------------
  //  Forward declarations
  // -----------------------------------------------------------------

  TMVCController = class;
  TMVCControllerList = class;
  TMVCCommand = class;
  TMVCCommandList = class;
  TMVCEvent = class;

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Base Controller. }
  TMVCController = class(TtiObject)
  private
    FGroupName: string;
    FName: string;
    FInitialized: Boolean;
    function GetParentController: TMVCController;
    procedure SetGroupName(const AValue: string);
    procedure SetParentController(const Value: TMVCController);
    procedure SetName(const Value: string);
  protected
    FActive: Boolean;
    FCommands: TMVCCommandList;
    FParentController: TMVCController;
    FControllers: TMVCControllerList;
    FModel: TObject;
    FView: TObject;
    procedure   DoCreateModel; virtual;
    procedure   DoCreateView; virtual;
    procedure   DoBeforeInit; virtual;
    procedure   DoCreateCommands; virtual;
    procedure   DoCreateMediators; virtual;
    procedure   DoTearDownMediators; virtual;
    procedure   DoAfterInit; virtual;
    procedure   SetActive(const AValue: Boolean); virtual;
  public
    property    ParentController: TMVCController read GetParentController write SetParentController;
    property    Controllers: TMVCControllerList read FControllers;
    property    Commands: TMVCCommandList read FCommands;
    property    Active: Boolean read FActive write SetActive;
    property    Initialized: Boolean read FInitialized;
    function    Model: TObject; virtual;
    function    View: TObject; virtual;
    procedure   Init; virtual;
    procedure   ChangeModel(ANewModel: TObject); virtual;
    function    AddController(AController: TMVCController): TMVCController; overload;
    function    AddCommand(ACommand: TMVCCommand): TMVCCommand;
    constructor Create; overload; override;
    constructor Create(AModel: TObject; AView: TObject); reintroduce; overload; virtual;
    destructor  Destroy; override;
  published
    property    Name: string read FName write SetName;
    property    GroupName: string read FGroupName write SetGroupName;
  end;

  {: List of TMVCController objects. }
  TMVCControllerList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TMVCController; reintroduce;
    procedure   SetItems(i: integer; const AValue: TMVCController); reintroduce;
  public
    property    Items[i:integer] : TMVCController read GetItems write SetItems;
    procedure   Add(AObject : TMVCController); reintroduce;
    procedure   ChangeAllActive(const AMakeActive: Boolean);
    procedure   ChangeGroupActive(const AGroupName: string; const AMakeActive: Boolean);
    procedure   DeleteByGroup(const AGroupName: string);
    function    FindByModel(AModel: TObject): TMVCController;
    function    FindByName(const AName: string): TMVCController;
    procedure   InitializeAll;
  end;

  {: TMVCCommand. }
  TMVCCommand = class(TtiObject)
  private
    FController: TMVCController;
    FEnabled: Boolean;
    FCurrentEvent: TMVCEvent;
    FID: string;
    FGroup: string;
    procedure SetEnabled(const Value: Boolean);
    function GetCurrentEvent: TMVCEvent;
    procedure SetID(const Value: string);
    procedure SetGroup(const Value: string);
  protected
    procedure   DoExecute; virtual; abstract;
    procedure   DoAddListeners; virtual;
    procedure   DoRemoveListeners; virtual;
    property    CurrentEvent: TMVCEvent read GetCurrentEvent;
  public
    function    Controller: TMVCController; virtual;
    procedure   Execute; virtual;
    procedure   Update(ASubject: TtiObject); override;
    constructor Create(AController: TMVCController); reintroduce; virtual;
    destructor  Destroy; override;
  published
    property    Enabled: Boolean read FEnabled write SetEnabled;
    property    ID: string read FID write SetID;
    property    Group: string read FGroup write SetGroup;
  end;

  {: List of TMVCCommand objects. }
  TMVCCommandList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TMVCCommand; reintroduce;
    procedure   SetItems(i: integer; const AValue: TMVCCommand); reintroduce;
  public
    property    Items[i:integer] : TMVCCommand read GetItems write SetItems;
    procedure   Add(AObject : TMVCCommand); reintroduce;
    procedure   ChangeAllActive(const AMakeActive: Boolean);
    function    ByName(const AName: string): TMVCCommand;
    procedure   ExecuteGroup(const AGroupName: string); virtual;
  end;

  {: Command that can listen for TNotifyEvent and respond. }
  TCmdNotifyEvent = class(TMVCCommand)
  private
    FSender: TObject;
    procedure SetSender(const Value: TObject);
  protected
    procedure   HandleNotifyEvent(Sender: TObject); virtual;
  public
    property    Sender: TObject read FSender write SetSender;
  end;

  {: Basic event object. }
  TMVCEvent = class(TtiObject)
  private
    FName: string;
    FSource: TObject;
    FHandled: Boolean;
    procedure SetName(const Value: string);
    procedure SetSource(const Value: TObject);
    procedure SetHandled(const Value: Boolean);
  public
    constructor Create; overload; override;
    constructor Create(ASource: TObject); reintroduce; overload; virtual;
    property    Name: string read FName write SetName;
    property    Source: TObject read FSource write SetSource;
    property    Handled: Boolean read FHandled write SetHandled;
  end;

  {: List of TMVCEvent objects. }
  TMVCEventList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TMVCEvent; reintroduce;
    procedure   SetItems(i: integer; const AValue: TMVCEvent); reintroduce;
  public
    property    Items[i:integer] : TMVCEvent read GetItems write SetItems;
    procedure   Add(AObject : TMVCEvent); reintroduce;
  end;

  {: Registration object for listening for MVC events. }
  TMVCListenerReg = class(TtiObject)
  private
    FTarget: TtiObject;
    FEventName: string;
    FSource: TObject;
    FListenerState: TListenerState;
    procedure SetEventName(const Value: string);
    procedure SetTarget(const Value: TtiObject);
    procedure SetSource(const Value: TObject);
    procedure SetListenerState(const Value: TListenerState);
  published
    {: The object which will emit the event.  The observed object. }
    property    Source: TObject read FSource write SetSource;
    {: The object which will listen for events.  The "the target of event". }
    property    Target: TtiObject read FTarget write SetTarget;
    property    EventName: string read FEventName write SetEventName;
    property    ListenerState: TListenerState read FListenerState write SetListenerState;
  end;

  {: List of TMVCListernReg objects. }
  TMVCListenerRegList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TMVCListenerReg; reintroduce;
    procedure   SetItems(i: integer; const AValue: TMVCListenerReg); reintroduce;
  public
    property    Items[i:integer] : TMVCListenerReg read GetItems write SetItems;
    procedure   Add(AObject : TMVCListenerReg); reintroduce;
    function    FindByTargetAndSource(ATarget: TObject; ASource: TObject; const AEventName: string): TMVCListenerReg;
    procedure   ClearDeleted;
  end;

  {: Registers TMVCCommand objects against a TMVCController.  These are loaded by the controller. }
  TCommandReg = class(TtiObject)
  private
    FCommandClass: TMVCCommandClass;
    FCommandClassName: string;
    FControllerClass: TMVCControllerClass;
    FControllerClassName: string;
    procedure SetCommandClass(const Value: TMVCCommandClass);
    procedure SetCommandClassName(const Value: string);
    procedure SetControllerClass(const Value: TMVCControllerClass);
    procedure SetControllerClassName(const Value: string);
  published
    property    CommandClassName: string read FCommandClassName write SetCommandClassName;
    property    CommandClass: TMVCCommandClass read FCommandClass write SetCommandClass;
    property    ControllerClass: TMVCControllerClass read FControllerClass write SetControllerClass;
    property    ControllerClassName: string read FControllerClassName write SetControllerClassName;
  end;

  {: List of TCommandReg objects. }
  TCommandRegList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TCommandReg; reintroduce;
    procedure   SetItems(i: integer; const AValue: TCommandReg); reintroduce;
  public
    property    Items[i:integer] : TCommandReg read GetItems write SetItems;
    procedure   Add(AObject : TCommandReg); reintroduce;
  end;

  {: Global singleton object acts as a central repository for commands registered
  to controllers. }
  TCommandManager = class(TtiObject)
  private
    FRegs: TCommandRegList;
  public
    procedure   RegisterCommand(const ACmdClass: TMVCCommandClass;
      const ACtrlClass: TMVCControllerClass);
    procedure   CreateCommands(AController: TMVCController);
    constructor Create; override;
    destructor  Destroy; override;
  end;

  {: Event listener registration. }
  TMVCEventListener = class(TtiObject)
  published
  end;


  {: Manages MVC events subscription and dispatch. }
  TMVCEventManager = class(TtiObject)
  private
    FListeners: TMVCListenerRegList;
    FTerminated: boolean;
    procedure SetTerminated(const AValue: boolean);
  protected
    procedure   NotifyListeners(AEvent: TMVCEvent); virtual;
  public
    property    Terminated: boolean read FTerminated write SetTerminated;
    procedure   AddListener(AListener: TtiObject; ASource: TObject; const
      AEventName: string); virtual;
    procedure   RemoveListenersByTarget(ATarget: TtiObject); virtual;
    procedure   RemoveListenersBySource(ASource: TtiObject); virtual;
    procedure   DispatchEvent(AEvent: TMVCEvent; const ADestroyAfter: Boolean = True); virtual;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  // -----------------------------------------------------------------
  //  Singletons
  // -----------------------------------------------------------------

  function gEventManager: TMVCEventManager;
  function gCmdManager: TCommandManager;

  // -----------------------------------------------------------------
  //  Globals
  // -----------------------------------------------------------------


  function gCreateQuickEvent(const AName: string; ASource: TObject): TMVCEvent;

implementation

var
  mManager: TMVCEventManager;
  mCmdMgr: TCommandManager;

function gEventManager: TMVCEventManager;
begin
  if mManager = nil then
    mManager := TMVCEventManager.Create;
  result := mManager;
end;

function gCmdManager: TCommandManager;
begin
  if mCmdMgr = nil then
    mCmdMgr := TCommandManager.Create;
  Result := mCmdMgr;
end;

function gCreateQuickEvent(const AName: string; ASource: TObject): TMVCEvent;
begin
  result := TMVCEvent.Create;
  result.Name := AName;
  result.Source := ASource;
end;

{ TMVCControllerList }

procedure TMVCControllerList.Add(AObject: TMVCController);
begin
  inherited Add(AObject);
end;

procedure TMVCControllerList.ChangeAllActive(const AMakeActive: Boolean);
var
  lCounter: Integer;
begin
  for lCounter := 0 to Count - 1 do
    begin
      Items[lCounter].Active := AMakeActive;
    end;
end;

procedure TMVCControllerList.ChangeGroupActive(const AGroupName: string; const AMakeActive: Boolean);
var
  lCounter: Integer;
  lCtrl: TMVCController;
  lName: string;
begin
  lName := LowerCase(AGroupName);

  for lCounter := 0 to Count - 1 do
    begin
      lCtrl := Items[lCounter];
      if LowerCase(lCtrl.GroupName) = lName then
        lCtrl.Active := AMakeActive;
    end;
end;

procedure TMVCControllerList.DeleteByGroup(const AGroupName: string);
var
  lCounter: Integer;
  lCtrl: TMVCController;
  lName: string;
begin
  lName := LowerCase(AGroupName);

  for lCounter := Count - 1 downto 0 do
    begin
      lCtrl := Items[lCounter];
      if LowerCase(lCtrl.GroupName) = lName then
        begin
          lCtrl.Active := False;
          Extract(lCtrl);
          lCtrl.Free;
        end;
    end;

end;

function TMVCControllerList.FindByModel(AModel: TObject): TMVCController;
var
  lCounter: Integer;
  lCtrl: TMVCController;
begin
  Result := nil;

  for lCounter := 0 to Count - 1 do
    begin
      lCtrl := Items[lCounter];
      if lCtrl.Model = AModel then
        begin
          Result := lCtrl;
          Exit;
        end;
    end;

end;

function TMVCControllerList.FindByName(const AName: string): TMVCController;
begin
  Result :=
    TMVCController(FindByProps(['Name'], [AName], False));
end;

function TMVCControllerList.GetItems(i: integer): TMVCController;
begin
  result:= inherited GetItems(i) as TMVCController;
end;

procedure TMVCControllerList.InitializeAll;
var
  lCounter: Integer;
begin
  for lCounter := 0 to Count - 1 do
    begin
      Items[lCounter].Init;
    end;
end;

procedure TMVCControllerList.SetItems(i: integer; const AValue: TMVCController);
begin
  inherited SetItems(i, AValue);
end;

{ TMVCCommand }

function TMVCCommand.Controller: TMVCController;
begin
  Result := FController;
end;

constructor TMVCCommand.Create(AController: TMVCController);
begin

  FController := AController;
  DoAddListeners;
  inherited Create;
end;

destructor TMVCCommand.Destroy;
begin
  DoRemoveListeners;
  inherited;
end;

procedure TMVCCommand.DoAddListeners;
begin
  // dummy method
end;

procedure TMVCCommand.DoRemoveListeners;
begin
  // dummy method
end;

procedure TMVCCommand.Execute;
begin
  if FEnabled then
    DoExecute;
end;

function TMVCCommand.GetCurrentEvent: TMVCEvent;
begin
  Result := FCurrentEvent;
end;

procedure TMVCCommand.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TMVCCommand.SetGroup(const Value: string);
begin
  FGroup := Value;
end;

procedure TMVCCommand.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TMVCCommand.Update(ASubject: TtiObject);
begin
  if ASubject is TMVCEvent then
    begin
      FCurrentEvent := TMVCEvent(ASubject);
      Execute;
    end;

end;

{ TMVCCommandList }

procedure TMVCCommandList.Add(AObject: TMVCCommand);
begin
  inherited Add(AObject);
end;

function TMVCCommandList.ByName(const AName: string): TMVCCommand;
begin
  Result := TMVCCommand(FindByProps(['ID'], [AName]));
end;

procedure TMVCCommandList.ChangeAllActive(const AMakeActive: Boolean);
var
  lCounter: Integer;
begin
  for lCounter := 0 to Count - 1 do
    begin
      Items[lCounter].Enabled := AMakeActive;
    end;
end;

procedure TMVCCommandList.ExecuteGroup(const AGroupName: string);
var
  lCtr: Integer;
  lCmd: TMVCCommand;
  lGroup: string;
begin
  lGroup := LowerCase(AGroupName);

  for lCtr := 0 to Count - 1 do
    begin
      lCmd := Items[lCtr];
      if LowerCase(lCmd.Group) = lGroup then
        lCmd.Execute;
    end;
end;

function TMVCCommandList.GetItems(i: integer): TMVCCommand;
begin
  result:= inherited GetItems(i) as TMVCCommand;
end;

procedure TMVCCommandList.SetItems(i: integer; const AValue: TMVCCommand);
begin
  inherited SetItems(i, AValue);
end;

{ TMVCEvent }

constructor TMVCEvent.Create;
begin
  inherited;
end;

constructor TMVCEvent.Create(ASource: TObject);
begin
  inherited Create;
  FSource := ASource;
end;

procedure TMVCEvent.SetHandled(const Value: Boolean);
begin
  FHandled := Value;
end;

procedure TMVCEvent.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TMVCEvent.SetSource(const Value: TObject);
begin
  FSource := Value;
end;

{ TMVCController }

constructor TMVCController.Create;
begin
  inherited;
  FName := ClassName;
  FCommands := TMVCCommandList.Create;
  FControllers := TMVCControllerList.Create;
  FControllers.Owner := Self;
end;

function  TMVCController.AddCommand(ACommand: TMVCCommand): TMVCCommand;
begin
  if Commands.IndexOf(ACommand) < 0 then
    begin
      ACommand.Enabled := Self.Active;
      Commands.Add(ACommand);
    end;
end;

function TMVCController.AddController(AController: TMVCController): TMVCController;
begin
  if Controllers.IndexOf(AController) < 0 then
    begin
      AController.Owner := Self;
      if not AController.Initialized then
        AController.Init;
      Controllers.Add(AController);
    end;
end;

procedure TMVCController.ChangeModel(ANewModel: TObject);
begin
  Active := false;
  FModel := ANewModel;
  if FModel <> nil then
    Active := true;
end;

constructor TMVCController.Create(AModel, AView: TObject);
begin
  Create;
  FView := AView;
  FModel := AModel;
end;

destructor TMVCController.Destroy;
begin
  DoTearDownMediators;
  FCommands.Free;
  FControllers.Free;
  inherited;
end;

procedure TMVCController.DoAfterInit;
begin
  // dummy method.
end;

procedure TMVCController.DoBeforeInit;
begin
  // dummy method.
end;

procedure TMVCController.DoCreateCommands;
begin
  // dummy method
end;

procedure TMVCController.DoCreateMediators;
begin
  // dummy method
end;

procedure TMVCController.DoCreateModel;
begin
  // dummy method.
end;

procedure TMVCController.DoCreateView;
begin
  // dummy method.
end;

procedure TMVCController.DoTearDownMediators;
begin
  // null method.
end;

function TMVCController.GetParentController: TMVCController;
begin
  Result := FParentController;
end;

procedure TMVCController.Init;
begin
  if FInitialized then
    exit;

  DoBeforeInit;
  DoCreateModel;
  DoCreateView;
  DoCreateCommands;
  DoCreateMediators;
  DoAfterInit;
  // Initialize any subcontrollers
  FControllers.InitializeAll;
  FInitialized := True;
end;

function TMVCController.Model: TObject;
begin
  Result := FModel;
end;

procedure TMVCController.SetActive(const AValue: Boolean);
begin
  if AValue <> FActive then
    begin
      FActive := AValue;
      FControllers.ChangeAllActive(AValue);
      FCommands.ChangeAllActive(AValue);
    end;
end;

procedure TMVCController.SetGroupName(const AValue: string);
begin
  if FGroupName=AValue then exit;
  FGroupName:=AValue;
end;

procedure TMVCController.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TMVCController.SetParentController(const Value: TMVCController);
begin
  FParentController := Value;
end;

function TMVCController.View: TObject;
begin
  Result := FView;
end;

{ TMVCEventList }

procedure TMVCEventList.Add(AObject: TMVCEvent);
begin
  inherited Add(AObject);
end;

function TMVCEventList.GetItems(i: integer): TMVCEvent;
begin
  result:= inherited GetItems(i) as TMVCEvent;
end;

procedure TMVCEventList.SetItems(i: integer; const AValue: TMVCEvent);
begin
  inherited SetItems(i, AValue);
end;

{ TMVCListenerReg }

procedure TMVCListenerReg.SetEventName(const Value: string);
begin
  FEventName := Value;
end;

procedure TMVCListenerReg.SetListenerState(const Value: TListenerState);
begin
  FListenerState := Value;
end;

procedure TMVCListenerReg.SetSource(const Value: TObject);
begin
  FSource := Value;
end;

procedure TMVCListenerReg.SetTarget(const Value: TtiObject);
begin
  FTarget := Value;
end;

{ TMVCListenerRegList }

procedure TMVCListenerRegList.Add(AObject: TMVCListenerReg);
begin
  inherited Add(AObject);
end;

procedure TMVCListenerRegList.ClearDeleted;
var
  lCounter: Integer;
  lReg: TMVCListenerReg;
begin
  for lCounter := Pred(Count) downto 0 do
    begin
      lReg := Items[lCounter];
      if lReg.ListenerState = lsDeleted then
        begin
          Extract(lReg);
          lReg.Free;
        end;
    end;
end;

function TMVCListenerRegList.FindByTargetAndSource(ATarget: TObject;
  ASource: TObject; const AEventName: string): TMVCListenerReg;
var
  lCounter: Integer;
  lReg: TMVCListenerReg;
begin
  result := nil;

  for lCounter := 0 to Count - 1 do
    begin
      lReg := Items[lCounter];
      if (lReg.EventName = AEventName) and (lReg.Target = ATarget) and (lReg.Source = ASource) then
        begin
          result := lReg;
          exit;
        end;

    end;
end;

function TMVCListenerRegList.GetItems(i: integer): TMVCListenerReg;
begin
  result:= inherited GetItems(i) as TMVCListenerReg;
end;

procedure TMVCListenerRegList.SetItems(i: integer;
  const AValue: TMVCListenerReg);
begin
  inherited SetItems(i, AValue);
end;

{ TMVCEventManager }

procedure TMVCEventManager.AddListener(AListener: TtiObject; ASource: TObject;
  const AEventName: string);
var
  lReg: TMVCListenerReg;
begin
  lReg := FListeners.FindByTargetAndSource(AListener, ASource, AEventName);  //TMVCListenerReg(FListeners.FindByProps(['EventName'], [AEventName], false));
  if lReg = nil then
    begin
      lReg := TMVCListenerReg.Create;
      lReg.Source := ASource;
      lReg.Target := AListener;
      lReg.EventName := AEventName;
      FListeners.Add(lReg);
    end;
end;

constructor TMVCEventManager.Create;
begin
  inherited;
  FListeners := TMVCListenerRegList.Create;
end;

destructor TMVCEventManager.Destroy;
begin
  FListeners.Free;
  inherited;
end;

procedure TMVCEventManager.DispatchEvent(AEvent: TMVCEvent; const ADestroyAfter: Boolean = True);
begin
  NotifyListeners(AEvent);
  if ADestroyAfter then
    AEvent.Free;
end;

procedure TMVCEventManager.NotifyListeners(AEvent: TMVCEvent);
var
  lCounter: Integer;
  lReg: TMVCListenerReg;
begin
  for lCounter := 0 to FListeners.Count -1 do
    begin
      if Terminated then
        begin
          FListeners.Clear;
          exit;
        end;
      lReg := FListeners.Items[lCounter];
      if lReg.ListenerState = lsActive then
        begin
          {: EventName must match and either the source match or the source be null, meaning to accept all. }
          if (lReg.EventName = AEvent.Name) and ((AEvent.Source = lReg.Source) or (lReg.Source = nil)) then
            begin
              lReg.Target.Update(AEvent, noChanged);
              // if handled and no further execution is desired then exit;
              if AEvent.Handled then
                Break;
            end;
        end;
    end;

 FListeners.ClearDeleted;

end;

procedure TMVCEventManager.RemoveListenersBySource(ASource: TtiObject);
var
  lCounter: Integer;
  lReg: TMVCListenerReg;
begin
  for lCounter := Pred(FListeners.Count) downto 0 do
    begin
      lReg := FListeners.Items[lCounter];
      if lReg.Source = ASource then
        begin
          FListeners.Extract(lReg);
          lReg.Free;
        end;
    end;
end;

procedure TMVCEventManager.RemoveListenersByTarget(ATarget: TtiObject);
var
  lCounter: Integer;
  lReg: TMVCListenerReg;
begin
  for lCounter := Pred(FListeners.Count) downto 0 do
    begin
      lReg := FListeners.Items[lCounter];
      if lReg.Target = ATarget then
        begin
          FListeners.Extract(lReg);
          lReg.Free;
        end;
    end;
end;

procedure TMVCEventManager.SetTerminated(const AValue: boolean);
begin
  if FTerminated=AValue then exit;
  FTerminated:=AValue;
end;

{ TCommandReg }

procedure TCommandReg.SetCommandClass(const Value: TMVCCommandClass);
begin
  FCommandClass := Value;
end;

procedure TCommandReg.SetCommandClassName(const Value: string);
begin
  FCommandClassName := Value;
end;

procedure TCommandReg.SetControllerClass(const Value: TMVCControllerClass);
begin
  FControllerClass := Value;
end;

procedure TCommandReg.SetControllerClassName(const Value: string);
begin
  FControllerClassName := Value;
end;

{ TCommandRegList }

procedure TCommandRegList.Add(AObject: TCommandReg);
begin
  inherited Add(AObject);
end;

function TCommandRegList.GetItems(i: integer): TCommandReg;
begin
  result:= inherited GetItems(i) as TCommandReg;
end;

procedure TCommandRegList.SetItems(i: integer; const AValue: TCommandReg);
begin
  inherited SetItems(i, AValue);
end;

{ TCommandManager }

constructor TCommandManager.Create;
begin
  inherited;
  FRegs := TCommandRegList.Create;
end;

procedure TCommandManager.CreateCommands(AController: TMVCController);
var
  lCounter: Integer;
  lReg: TCommandReg;
begin
  for lCounter := 0 to FRegs.Count - 1 do
    begin
      lReg := FRegs.Items[lCounter];
      if lReg.ControllerClass = AController.ClassType then
        AController.AddCommand(lReg.CommandClass.Create(AController));
    end;
end;

destructor TCommandManager.Destroy;
begin
  FRegs.Free;
  inherited;
end;

procedure TCommandManager.RegisterCommand(const ACmdClass: TMVCCommandClass;
  const ACtrlClass: TMVCControllerClass);
var
  lReg: TCommandReg;
begin
  lReg := TCommandReg(FRegs.FindByProps(['CommandClassName', 'ControllerClassName'],
    [ACmdClass.ClassName, ACtrlClass.ClassName]));
  if lReg = nil then
    begin
      lReg := TCommandReg.Create;
      lReg.CommandClassName := ACmdClass.ClassName;
      lReg.CommandClass := ACmdClass;
      lReg.ControllerClass := ACtrlClass;
      lReg.ControllerClassName := ACtrlClass.ClassName;
      FRegs.Add(lReg);
    end;
end;

procedure TCmdNotifyEvent.SetSender(const Value: TObject);
begin
  FSender := Value;
end;

procedure TCmdNotifyEvent.HandleNotifyEvent(Sender: TObject);
begin
  inherited;
  if Enabled then
    begin
      self.Sender := Sender;
      Execute;
    end;
end;


initialization

finalization
  if mManager <> nil then
    mManager.Free;

  if mCmdMgr <> nil then
    mCmdMgr.Free;
end.
