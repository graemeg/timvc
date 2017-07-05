unit mvc_criteria;

interface
uses
  SysUtils
  ,Classes
  ,tiObject
  ,tiCriteria
  ,tiFilteredObjectList
  ,tiRTTI
  ;

type
  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  {: Filtering kind. }
  TFilterKind = (fkInput, fkObjectList, fkStringList);

  {: Operator to use. }
  TOperatorKind = (okEqual, okGreat, okLess, okGreatOrEqual, okLessOrEqual,
    okNoEqual, okLike, okNotExists);

  {: Boolean handling. }
  TBooleanType = (btBoolean, btTF, btTrueFalse, btOneZero);

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Holds a reference to an object's property that are available for filtering. }
  TObjectProp = class(TtiObject)
  private
    FDisplayName: string;
    FPropType: TtiTypeKind;
    FModelAttrib: string;
    FObjectClassName: string;
    procedure SetDisplayName(const Value: string);
    procedure SetModelAttrib(const Value: string);
    procedure SetPropType(const Value: TtiTypeKind);
    procedure SetObjectClassName(const Value: string);
  published
    property    ObjectClassName: string read FObjectClassName write SetObjectClassName;
    property    ModelAttrib: string read FModelAttrib write SetModelAttrib;
    property    DisplayName: string read FDisplayName write SetDisplayName;
    property    PropType: TtiTypeKind read FPropType write SetPropType;
  end;

  {: List if TObjectProp objects. }
  TObjectPropList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TObjectProp; reintroduce;
    procedure   SetItems(i: integer; const AValue: TObjectProp); reintroduce;
  public
    property    Items[i:integer] : TObjectProp read GetItems write SetItems;
    procedure   Add(AObject : TObjectProp); reintroduce;
  end;

  {: Holds a filtering value. }
  TFilterValue = class(TtiObject)
  private
    FModelAttrib: string;
    FOperation: TOperatorKind;
    FValue: Variant;
    FDisplayName: string;
    FPropType: TtiTypeKind;
    procedure SetModelAttrib(const Value: string);
    procedure SetOperation(const Value: TOperatorKind);
    procedure SetValue(const Value: Variant);
    procedure SetDisplayName(const Value: string);
    function GetOpString: string;
    procedure SetPropType(const Value: TtiTypeKind);
  published
    property    OpString: string read GetOpString;
    property    Value: Variant read FValue write SetValue;
    property    ModelAttrib: string read FModelAttrib write SetModelAttrib;
    property    Operation: TOperatorKind read FOperation write SetOperation;
    property    DisplayName: string read FDisplayName write SetDisplayName;
    property    PropType: TtiTypeKind read FPropType write SetPropType;
  end;

  {: List of TFilterType objects. }
  TFilterValueList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TFilterValue; reintroduce;
    procedure   SetItems(i: integer; const AValue: TFilterValue); reintroduce;
  public
    property    Items[i:integer] : TFilterValue read GetItems write SetItems;
    procedure   Add(AObject : TFilterValue); reintroduce; overload;
    procedure   Add(const AModelAttrib: string;
      const OpType: TOperatorKind; const ADisplayName: string;
      AValue: Variant; const APropType: TtiTypeKind); overload;
    function    FindByDisplayName(const AName: string): TFilterValue;
    function    FindByAttribName(const AName: string): TFilterValue;
  end;

  {: Object Filter object. }
  TObjectFilter = class(TtiObject)
  private
    FPropsList: TObjectPropList;
    FFilters: TFilterValueList;
    FBooleanType: TBooleanType;
    procedure SetBooleanType(const Value: TBooleanType);
    procedure   AddBooleanFilter(AObjectList: TtiFilteredObjectList;
      const AValue: Boolean; const AModelAttrib: string);
  public
    property    PropList: TObjectPropList read FPropsList;
    property    Filters: TFilterValueList read FFilters;
    property    BooleanType: TBooleanType read FBooleanType write SetBooleanType;
    procedure   AddProp(const AModelAttrib: string; const ADisplayName: string;
      const APropType: TtiTypeKind = tiTKString);
    procedure   AddFilterValue(const AModelAttrib: string;
      const OpType: TOperatorKind; const ADisplayName: string;
      AValue: Variant; const APropType: TtiTypeKind);
    procedure   ApplyFilter(AObjectList: TtiFilteredObjectList);
    function    AsString: string;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TObjectFilterList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TObjectFilter; reintroduce;
    procedure   SetItems(i: integer; const AValue: TObjectFilter); reintroduce;
  public
    property    Items[i:integer] : TObjectFilter read GetItems write SetItems;
    procedure   Add(AObject : TObjectFilter); reintroduce;
  end;

implementation

{ TObjectProp }

procedure TObjectProp.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TObjectProp.SetModelAttrib(const Value: string);
begin
  FModelAttrib := Value;
end;

procedure TObjectProp.SetObjectClassName(const Value: string);
begin
  FObjectClassName := Value;
end;

procedure TObjectProp.SetPropType(const Value: TtiTypeKind);
begin
  FPropType := Value;
end;

{ TObjectPropList }

procedure TObjectPropList.Add(AObject: TObjectProp);
begin
  inherited Add(AObject);
end;

function TObjectPropList.GetItems(i: integer): TObjectProp;
begin
  result:= inherited GetItems(i) as TObjectProp;
end;

procedure TObjectPropList.SetItems(i: integer; const AValue: TObjectProp);
begin
  inherited SetItems(i, AValue);
end;

{ TFilterValue }

function TFilterValue.GetOpString: string;
begin
  case Operation of
    okEqual: Result := 'Equal'; //GLang.GetLang(cEqualTo);
    okGreat: result := 'Greater Than'; //GLang.GetLang(cGreatThan);
    okLess: result := 'Less Than'; //GLang.GetLang(cLessThan);
    okGreatOrEqual: result := 'Greater or Equal'; //GLang.GetLang(cGreatOrEqual);
    okLessOrEqual: result := 'Less or Equal'; //GLang.GetLang(cLessOrEqual);
    okNoEqual: result := 'Not Equal'; //GLang.GetLang(cNotEqual);
    okLike: result := 'LIKE'; //GLang.GetLang(cLike);
  end;
end;

procedure TFilterValue.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TFilterValue.SetModelAttrib(const Value: string);
begin
  FModelAttrib := Value;
end;

procedure TFilterValue.SetOperation(const Value: TOperatorKind);
begin
  FOperation := Value;
end;

procedure TFilterValue.SetPropType(const Value: TtiTypeKind);
begin
  FPropType := Value;
end;

procedure TFilterValue.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TFilterValueList }

procedure TFilterValueList.Add(AObject: TFilterValue);
begin
  inherited Add(AObject);
end;

procedure TFilterValueList.Add(const AModelAttrib: string;
  const OpType: TOperatorKind; const ADisplayName: string; AValue: Variant;
  const APropType: TtiTypeKind);
var
  lValue: TFilterValue;
begin
  lValue := FindByAttribName(AModelAttrib);
  if lValue <> nil then
    exit;

  lValue := TFilterValue.Create;
  lValue.ModelAttrib := AModelAttrib;
  lValue.Operation := OpType;
  lValue.DisplayName := ADisplayName;
  lValue.PropType := APropType;
  lValue.Value := AValue;
  Add(lValue);
end;

function TFilterValueList.FindByAttribName(const AName: string): TFilterValue;
begin
  Result := TFilterValue(FindByProps(['ModelAttrib'], [AName], False));
end;

function TFilterValueList.FindByDisplayName(const AName: string): TFilterValue;
begin
  Result := TFilterValue(FindByProps(['DisplayName'], [AName], False));
end;

function TFilterValueList.GetItems(i: integer): TFilterValue;
begin
  result:= inherited GetItems(i) as TFilterValue;
end;

procedure TFilterValueList.SetItems(i: integer; const AValue: TFilterValue);
begin
  inherited SetItems(i, AValue);
end;

{ TObjectFilter }

procedure TObjectFilter.AddBooleanFilter(AObjectList: TtiFilteredObjectList;
  const AValue: Boolean; const AModelAttrib: string);
begin

end;

procedure TObjectFilter.AddFilterValue(const AModelAttrib: string;
  const OpType: TOperatorKind; const ADisplayName: string; AValue: Variant;
  const APropType: TtiTypeKind);
var
  lVal: TFilterValue;
begin
  lVal := TFilterValue(FFilters.FindByProps(['ModelAttrib', 'Operation'],
    [AModelAttrib, OpType], False));
  if lVal = nil then
    begin
      lVal := TFilterValue.Create;
      lVal.ModelAttrib := AModelAttrib;
      lVal.Operation := OpType;
      lVal.DisplayName := ADisplayName;
      lVal.Value := AValue;
      lVal.PropType := APropType;
      FFilters.Add(lVal);
    end;
end;

procedure TObjectFilter.AddProp(const AModelAttrib, ADisplayName: string;
  const APropType: TtiTypeKind = tiTKString);
var
  lProp: TObjectProp;
begin
  lProp := FPropsList.FindByProps(['ModelAttrib','PropType'],
    [AModelAttrib, APropType], false) as TObjectProp;
  if lProp = nil then
    begin
      lProp := TObjectProp.Create;
      lProp.DisplayName := ADisplayName;
      lProp.PropType := APropType;
      lProp.ModelAttrib := AModelAttrib;
      FPropsList.Add(lProp);
    end;
end;

procedure TObjectFilter.ApplyFilter(AObjectList: TtiFilteredObjectList);
var
  lCounter: Integer;
  lFilter: TFilterValue;
  lType: TtiTypeKind;
  lValue: Variant;
  lDateVal: TDateTime;
begin
  AObjectList.Criteria.SelectionCriterias.Clear;
  for lCounter := 0 to FFilters.Count - 1 do
    begin
      lFilter := FFilters.Items[lCounter];
      if lFilter.Operation <> okNotExists then
        begin
          case lFilter.PropType of
            tiTKInteger: lValue := Integer(lFilter.Value);
            tiTKFloat: lValue := StrToFloat(lFilter.Value);
            tiTKString: lValue := lFilter.Value;
            tiTKDateTime:
              begin
                lDateVal := lFilter.Value;
                lValue := FormatDateTime('mm/dd/yyyy', lDateVal);
              end;
            tiTKBoolean:
              begin
                case BooleanType of
                  btBoolean: lValue := StrToBool(lFilter.Value);
                  btTF:
                    begin
                      if lFilter.Value = True then
                        lValue := 'T'
                      else
                        lValue := 'F';
                    end;
                  btTrueFalse:
                    begin
                      if lFilter.Value = True then
                        lValue := 'true'
                      else
                        lValue := 'false';
                    end;
                  btOneZero:
                    begin
                      if lFilter.Value = True then
                        lValue := Integer(1)
                      else
                        lValue := Integer(0);
                    end;
                end;
              end;
            tiTKBinary: ;
          end;
        end;

      case lFilter.Operation of
        okEqual: AObjectList.Criteria.AddEqualTo(lFilter.ModelAttrib, lValue);
        okGreat: AObjectList.Criteria.AddGreaterThan(lFilter.ModelAttrib, lValue);
        okLess: AObjectList.Criteria.AddLessThan(lFilter.ModelAttrib, lValue);
        okGreatOrEqual: AObjectList.Criteria.AddGreaterOrEqualThan(lFilter.ModelAttrib, lValue);
        okLessOrEqual: AObjectList.Criteria.AddLessOrEqualThan(lfilter.ModelAttrib, lValue);
        okNoEqual: AObjectList.Criteria.AddNotEqualTo(lFilter.ModelAttrib, lValue);
        okLike: AObjectList.Criteria.AddLike(lFilter.ModelAttrib, lValue);
        okNotExists: AObjectList.Criteria.AddNotExists(lFilter.Value);
      end;
    end;
end;

function TObjectFilter.AsString: string;
var
  lCounter: Integer;
  lFilter: TFilterValue;
begin
  { TODO : Finish this up.  Don't need it yet.}
  for lCounter := 0 to FFilters.Count - 1 do
    begin
      lFilter := FFilters.Items[lCounter];
    end;
end;

constructor TObjectFilter.Create;
begin
  inherited;
  FFilters := TFilterValueList.Create;
  FPropsList := TObjectPropList.Create;
end;

destructor TObjectFilter.Destroy;
begin
  FFilters.Free;
  FPropsList.Free;
  inherited;
end;

procedure TObjectFilter.SetBooleanType(const Value: TBooleanType);
begin
  FBooleanType := Value;
end;

{ TObjectFilterList }

procedure TObjectFilterList.Add(AObject: TObjectFilter);
begin
  inherited Add(AObject);
end;

function TObjectFilterList.GetItems(i: integer): TObjectFilter;
begin
  result:= inherited GetItems(i) as TObjectFilter;
end;

procedure TObjectFilterList.SetItems(i: integer; const AValue: TObjectFilter);
begin
  inherited SetItems(i, AValue);
end;

end.
