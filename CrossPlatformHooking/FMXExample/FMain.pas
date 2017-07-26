unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.ScrollBox,
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    ToolBar: TToolBar;
    LabelInstances: TLabel;
    ButtonUpdate: TButton;
    GridInstances: TStringGrid;
    ColumnClass: TStringColumn;
    ColumnCount: TStringColumn;
    procedure GridInstancesResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
  private
    { Private declarations }
    procedure ResizeColumns;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  InstanceHashMap,
  InstanceTracker;

procedure TFormMain.ButtonUpdateClick(Sender: TObject);
var
  InstanceCounts: TArray<TInstanceInfo>;
  I, Count: Integer;
begin
  { Retrieve an array of live instances and sort it by class name. }
  InstanceCounts := GetInstanceCounts;
  TArray.Sort<TInstanceInfo>(InstanceCounts, TComparer<TInstanceInfo>.Construct(
    function(const ALeft, ARight: TInstanceInfo): Integer
    begin
      Result := CompareText(ALeft.Clazz.ClassName, ARight.Clazz.ClassName);
    end));

  { Populate the grid with the instances }
  GridInstances.BeginUpdate;
  try
    GridInstances.RowCount := Length(InstanceCounts);
    Count := 0;
    for I := 0 to Length(InstanceCounts) - 1 do
    begin
      if (InstanceCounts[I].Count > 0) then
      begin
        GridInstances.Cells[0, Count] := InstanceCounts[I].Clazz.ClassName;
        GridInstances.Cells[1, Count] := InstanceCounts[I].Count.ToString;
        Inc(Count);
      end;
    end;
    GridInstances.RowCount := Count;
  finally
    GridInstances.EndUpdate;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ResizeColumns;
end;

procedure TFormMain.GridInstancesResize(Sender: TObject);
begin
  ResizeColumns;
end;

procedure TFormMain.ResizeColumns;
begin
  ColumnClass.Width := Trunc(GridInstances.Width - ColumnCount.Width - 30);
end;

end.
