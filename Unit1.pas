unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg, Vcl.ComCtrls,
  externrun_free;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Button3: TButton;
    Button4: TButton;
    ExternRunFree1: TExternRunFree;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ExternRunFree1StatusMessages(Sender: TObject;
      var statInformations: TStatMsg);
    procedure ExternRunFree1ConsoleError(Sender: TObject; var errString: string;
      var errCharArray: array of AnsiChar);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExternRunFree1ConsoleOutput(Sender: TObject; var outString: string;
      var outCharArray: array of AnsiChar; var totalBytesRead,
      totalBytesAvailable: Cardinal);
    procedure ExternRunFree1ConsoleInput(Sender: TObject; var inString: string;
      var inCharArray: array of AnsiChar; var totalBytesWrite,
      totalBytesAvailable: Cardinal);
    procedure ExternRunFree1ProcessTermination(Sender: TObject;
      var processTerminate, abortProcess: Boolean; waitUntilResult: string);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1                 :TForm1;

  envStringList         :TStringList;

implementation

{$R *.dfm}

{
 creationFlags[0] := CREATE_DEFAULT_ERROR_MODE;
 creationFlags[1] := CREATE_NEW_CONSOLE;
 creationFlags[2] := CREATE_NEW_PROCESS_GROUP;
 creationFlags[3] := NORMAL_PRIORITY_CLASS;

 windowState[0] := SW_SHOW;
 windowState[1] := SW_SHOWMAXIMIZED;
 windowState[2] := SW_SHOWMINIMIZED;

 windowFlags[0] := STARTF_USEFILLATTRIBUTE;
 windowFlags[1] := STARTF_USEPOSITION Or STARTF_USESIZE;
 windowFlags[2] := STARTF_USESHOWWINDOW;
 windowFlags[3] := STARTF_USESTDHANDLES;

 fillAttrib[0] := FOREGROUND_BLUE;
 fillAttrib[1] := FOREGROUND_GREEN;
 fillAttrib[2] := FOREGROUND_RED;
 fillAttrib[3] := FOREGROUND_INTENSITY;
 fillAttrib[4] := BACKGROUND_BLUE;
 fillAttrib[5] := BACKGROUND_GREEN;
 fillAttrib[6] := BACKGROUND_RED;
 fillAttrib[7] := BACKGROUND_INTENSITY;
}




procedure TForm1.Button1Click(Sender: TObject);
begin
 ExternRunFree1.CreationFlags := CREATE_DEFAULT_ERROR_MODE + CREATE_NEW_CONSOLE +
                                 CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS;

 ExternRunFree1.UseWindowFlags := STARTF_USEFILLATTRIBUTE + STARTF_USEPOSITION Or
                                  STARTF_USESIZE + STARTF_USESHOWWINDOW +
                                  STARTF_USESTDHANDLES;

 ExternRunFree1.WindowState := SW_SHOW;

 ExternRunFree1.RedirectOutput := True;
 ExternRunFree1.RedirectInput := True;

 ExternRunFree1.InheritHandles := True;

 ExternRunFree1.AppName := 'C:\Windows\system32\cmd.exe';

 ExternRunFree1.StartNewProcess;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 ExternRunFree1.WriteToConsole(Edit1.Text);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 ExternRunFree1.AbortProcessNow;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 envStringList.Add('Path="M:\Programme\ffmpeg\bin"');
 envStringList.Add('ALLUSERSPROFILE=C:\ProgramData');
 envStringList.Add('TEST="This is a little environment test!"');

 ExternRunFree1.Environment := envStringList;
end;

procedure TForm1.ExternRunFree1ConsoleError(Sender: TObject; var errString: string;
  var errCharArray: array of AnsiChar);
begin
 Memo2.Lines.Add('ConsoleError: ' + errString);
end;

procedure TForm1.ExternRunFree1ConsoleInput(Sender: TObject; var inString: string;
  var inCharArray: array of AnsiChar; var totalBytesWrite,
  totalBytesAvailable: Cardinal);

Var i          :Integer;
    writeStr   :AnsiString;

begin
 writeStr := '';

 For i := 0 To Length(inCharArray) Do
  Begin
   writeStr := writeStr + inCharArray[i];
  End;

 Memo2.Lines.Add('CommandStr: ' + UTF8ToAnsi(writeStr));
end;

procedure TForm1.ExternRunFree1ConsoleOutput(Sender: TObject; var outString: string;
  var outCharArray: array of AnsiChar; var totalBytesRead,
  totalBytesAvailable: Cardinal);
begin
 Memo1.Lines.Add(outString);
end;

procedure TForm1.ExternRunFree1ProcessTermination(Sender: TObject;
  var processTerminate, abortProcess: Boolean; waitUntilResult: string);
begin
 If processTerminate = True Then
  Begin
   Memo2.Lines.Add('');
   Memo2.Lines.Add('Programm wurde beendet.');
  End;

 If abortProcess = True Then
  Begin
   Memo2.Lines.Add('');
   Memo2.Lines.Add('Programm wurde abgebrochen.');
  End;

 If waitUntilResult <> '' Then
  Begin
   Memo2.Lines.Add('');
   Memo2.Lines.Add(waitUntilResult);
  End;
end;

procedure TForm1.ExternRunFree1StatusMessages(Sender: TObject;
  var statInformations: TStatMsg);
begin
 Memo2.Lines.Add('###########################################################');
 Memo2.Lines.Add('Process: ' + statInformations.procNumber.ToString);
 Memo2.Lines.Add('Thread: ' + statInformations.threadNumber.ToString);
 Memo2.Lines.Add('ProcessID: ' + statInformations.procID.ToString);
 Memo2.Lines.Add('ThreadID: ' + statInformations.threadID.ToString);
 Memo2.Lines.Add('Read Input Pipe: ' + statInformations.readInputPipe.ToString);
 Memo2.Lines.Add('Write Input Pipe: ' + statInformations.writeInputPipe.ToString);
 Memo2.Lines.Add('Read Output Pipe: ' + statInformations.readOutputPipe.ToString);
 Memo2.Lines.Add('Write Output Pipe: ' + statInformations.writeOutputPipe.ToString);
 Memo2.Lines.Add('dwFlags: ' + statInformations.startupInfo_dwFlags.ToString);
 Memo2.Lines.Add('###########################################################');
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 envStringList.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 envStringList := TStringList.Create;
end;

end.
