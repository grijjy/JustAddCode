unit FluentInterface.CRGP;

interface

type
  TStringBuilder<T: class> = class
  private
    FData: String;
  public
    class constructor Create;
  public
    function Append(const AValue: String): T;

    property Data: String read FData;
  end;

type
  THtmlStringBuilder = class(TStringBuilder<THtmlStringBuilder>)
  public
    function BoldOn: THtmlStringBuilder;
    function BoldOff: THtmlStringBuilder;
  end;

procedure TestFluentInterfaceCRGP;

implementation

uses
  Utils;

procedure TestFluentInterfaceCRGP;
begin
  StartSection;
  WriteLn('A fluent string builder implementation using CRGP.');
  Write('Building an HTML string: ');

  var Builder := THtmlStringBuilder.Create;
  try
    Builder.Append('This is ').BoldOn.Append('bold').BoldOff.Append('!');
    WriteLn(Builder.Data);
  finally
    Builder.Free;
  end;
end;

{ TStringBuilder<T> }

function TStringBuilder<T>.Append(const AValue: String): T;
begin
  FData := FData + AValue;
  Result := T(Self);
end;

class constructor TStringBuilder<T>.Create;
begin
  Assert(T.InheritsFrom(TStringBuilder<T>));
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
