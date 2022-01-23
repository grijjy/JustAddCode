unit FluentInterface.Standard;

interface

type
  TStringBuilder = class
  private
    FData: String;
  public
    function Append(const AValue: String): TStringBuilder;

    property Data: String read FData;
  end;

type
  THtmlStringBuilder = class(TStringBuilder)
  public
    function BoldOn: THtmlStringBuilder;
    function BoldOff: THtmlStringBuilder;
  end;

procedure TestFluentInterfaceStandard;

implementation

uses
  Utils;

procedure TestFluentInterfaceStandard;
begin
  StartSection;
  WriteLn('A standard fluent string builder implementation.');
  Write('Building an HTML string: ');

  var Builder := THtmlStringBuilder.Create;
  try
    { This does not compile, because Append returns a TStringBuilder, which does
      not have a method called BoldOn.
    Builder.Append('This is ').BoldOn.Append('bold').BoldOff.Append('!'); }

    { So you have to *not* use a fluent interface, or only partially, as in: }
    Builder.Append('This is ');
    Builder.BoldOn.Append('bold');
    Builder.BoldOff.Append('!');

    WriteLn(Builder.Data);
  finally
    Builder.Free;
  end;
end;

{ TStringBuilder }

function TStringBuilder.Append(const AValue: String): TStringBuilder;
begin
  FData := FData + AValue;
  Result := Self;
end;

{ THtmlStringBuilder }

function THtmlStringBuilder.BoldOff: THtmlStringBuilder;
begin
  Append('</b>');
  Result := Self;
end;

function THtmlStringBuilder.BoldOn: THtmlStringBuilder;
begin
  Append('<b>');
  Result := Self;
end;

end.
