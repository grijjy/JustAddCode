unit FOwnership;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FBase,
  UBase,
  USampleClasses;

type
  TFrameOwnership = class(TFrameBase)
    ButtonCreateObjectTree: TButton;
    ButtonDeleteFirstBranch: TButton;
    ButtonFreeObjectTree: TButton;
    procedure ButtonCreateObjectTreeClick(Sender: TObject);
    procedure ButtonDeleteFirstBranchClick(Sender: TObject);
    procedure ButtonFreeObjectTreeClick(Sender: TObject);
  private
    { Private declarations }
    FTree: TSampleNode;
    FBranch1: TSampleNode;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameOwnership.ButtonCreateObjectTreeClick(Sender: TObject);
var
  Branch2, SubBranch: TSampleNode;
  I: Integer;
begin
  ButtonCreateObjectTree.Enabled := False;
  ButtonDeleteFirstBranch.Enabled := True;
  ButtonFreeObjectTree.Enabled := True;

  { Create the root without a parent. This is the only object we need to free
    at some point. }
  FTree := TSampleNode.Create(nil);

  { Create two branches and add them to the root }
  FBranch1 := TSampleNode.Create(FTree);
  Branch2 := TSampleNode.Create(FTree);

  { Create a reference cycle between these two branches. Since the Link property
    is implemented using a [weak] reference, this will NOT lead to a memory
    leak. }
  FBranch1.Link := Branch2;
  Branch2.Link := FBranch1;

  { Add 5 sub-branches to FBranch1. }
  for I := 0 to 4 do
  begin
    SubBranch := TSampleNode.Create(FBranch1);

    { Create a reference cycle between the sub-branch and its owner }
    SubBranch.Link := FBranch1;
  end;

  { We should have a total of 1 + 2 + 5 = 8 live objects now. }
  Log('----------------------------------------------------');
  Log('Created Object Tree with two branches, where the first branch has ' +
    'five sub-branches for a total of 1 + 2 + 5 = 8 objects.');
  Log(' * TSampleNode.InstanceCount = %d', [TSampleNode.InstanceCount]);
  Log('');
end;

procedure TFrameOwnership.ButtonDeleteFirstBranchClick(Sender: TObject);
begin
  { Delete the first branch. We also need to set its reference to nil on ARC
    platforms, otherwise the branch will stay alive and not be released. }
  FBranch1.Delete;
  FBranch1 := nil;

  Log('Freed first branch with its 5 sub-branches. ' +
    'So only 2 instances should remain (the root and the second branch).');
  Log(' * TSampleNode.InstanceCount = %d', [TSampleNode.InstanceCount]);
  Log('');

  ButtonDeleteFirstBranch.Enabled := False;
end;

procedure TFrameOwnership.ButtonFreeObjectTreeClick(Sender: TObject);
begin
  ButtonCreateObjectTree.Enabled := True;
  ButtonDeleteFirstBranch.Enabled := False;
  ButtonFreeObjectTree.Enabled := False;

  { Free tree and all its branches and sub-branches. On ARC platforms, we
    need to set FTree to nil to release any references. }
  FTree.Free;
  FTree := nil;

  { There is another subtle but important difference between ARC and non-ARC
    platforms:
    * On ARC platforms, the FBranch1 field may keep a whole branch of the tree
      alive, so we need to set it to nil to free it and its sub-branches.
    * On non-ARC platforms, freeing the FTree also freed all its branches and
      sub-branches, so the FBranch1 field may be an invalid reference now (a
      dangling pointer). So we should also set it to nil. }
  FBranch1 := nil;

  Log('Freed entire Object Tree with all branches and sub-branches.');
  Log(' * TSampleNode.InstanceCount = %d', [TSampleNode.InstanceCount]);
  Log('');
end;

end.
