unit tms_controllers;

interface
uses
  SysUtils
  ,Classes
  ,StdCtrls
  ,Grids
  ,Variants
  ,tiObject
  ,tiRTTI
  ,widget_controllers
  ,vcl_controllers
  ,mvc_base
  ,BaseGrid
  ,AdvGrid
  ,AdvEdit
  ;

type

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: TMSAdvStringGrid controller. }
  TTMSGridController = class(TVCLGridController)
  protected
    FLastWidth: Integer;
    FLastHeight: Integer;
    procedure   HandleOnGetAlignment(Sender: TObject; ARow, ACol: Integer;
      var HAlign: TAlignment; var VAlign: TVAlignment);
    procedure   HandleResize(Sender: TObject);
    procedure   SetActive(const AValue: Boolean); override;
  public
    function    View: TAdvStringGrid; reintroduce;
    function    GetSelItems: TtiObjectList; override;
  end;

  {: TMS Edit controller. }
  TTMSEditController = class(TWidgetController)
  protected
    procedure   ModelToView; override;
    procedure   ViewToModel; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleChangeEvent(Sender: TObject);
  public
    constructor Create(AModel: TtiObject; AView: TAdvEdit; const AModelAttrib: string); reintroduce;
    function    View: TAdvEdit; reintroduce;
  end;

implementation

{ TTMSGridController }

function TTMSGridController.GetSelItems: TtiObjectList;
begin

end;

procedure TTMSGridController.HandleOnGetAlignment(Sender: TObject; ARow,
  ACol: Integer; var HAlign: TAlignment; var VAlign: TVAlignment);
var
  lCol: TWidgetColumn;
begin
  if ACol <= (Columns.Count-1) then
    begin
      lCol := Columns.Items[ACol];
      case lCol.Align of
        caLeft: HAlign := taLeftJustify;
        caRight: HAlign := taRightJustify;
        caCenter: HAlign := taCenter;
      end;
    end;
end;

procedure TTMSGridController.HandleResize(Sender: TObject);
begin

//  View.AutoSizeCol(Columns.Count -1);
  if (FLastWidth = View.Width) and (FLastHeight = view.Height) then
    exit;
  View.BeginUpdate;
  try
    DoSizeColumns;
    View.Invalidate;
  finally
    View.EndUpdate;
  end;
  FLastWidth := View.Width;
  FLastHeight := View.Height;
end;

procedure TTMSGridController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    begin
      View.OnGetAlignment := self.HandleOnGetAlignment;
      View.OnResize := self.HandleResize;
    end
  else
    begin
      View.OnResize := nil;
      View.OnGetAlignment := nil;
    end;
end;

function TTMSGridController.View: TAdvStringGrid;
begin
  Result := inherited View as TAdvStringGrid;
end;

{ TTMSEditController }

constructor TTMSEditController.Create(AModel: TtiObject; AView: TAdvEdit;
  const AModelAttrib: string);
begin
  inherited Create(AModel, AView, AModelAttrib);
  Assert(tiIsPublishedProp(Model, FModelAttrib), ClassName + '.Create: ' +
    'The attribute ' + FModelAttrib + ' is not a published property');
end;

procedure TTMSEditController.HandleChangeEvent(Sender: TObject);
begin
  ViewToModel;
end;

procedure TTMSEditController.ModelToView;
begin
  View.Text := tiGetProperty(Model, FModelAttrib);
end;

procedure TTMSEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if not Active then
    begin
      View.OnChange := nil;
      View.Text := '';
    end
  else
    begin
      ModelToView;
      View.OnChange := self.HandleChangeEvent;
    end;
end;

function TTMSEditController.View: TAdvEdit;
begin
  Result := inherited View as TAdvEdit;
end;

procedure TTMSEditController.ViewToModel;
begin
  if View.EditType <> etString then
    if View.Text = '' then
      Exit;

    tiSetProperty(Model, FModelAttrib, View.Text);
end;

initialization
  gCtrlFactory.RegisterMapping(TAdvEdit, TTMSEditController);
end.
