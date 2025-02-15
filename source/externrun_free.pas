unit externrun_free;

Interface

Uses Winapi.Windows, Winapi.UserEnv, System.Classes, System.Variants,
     System.SysUtils, System.StrUtils, System.UITypes, Vcl.Dialogs, Vcl.Controls,
     Vcl.Graphics, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

Type
  /// <summary>
  ///   <para>
  ///     Return information about the current started process.
  ///   </para>
  ///   <para>
  ///     statusInformations contains:
  ///   </para>
  ///   <para>
  ///     procNumber, threadNumber, procID, threadID, readInputPipe, writeInputPipe,
  ///     readOutputPipe, writeOutputPipe, startupInfo_dwFlags, applicationName,
  ///     commandLineProgram, currentDirectory
  ///   </para>
  /// </summary>
  TStatMsg = Class(TObject)
    Public
     /// <summary>
     ///   Contains the process number.
     /// </summary>
     procNumber                 :Integer;
     /// <summary>
     ///   Contains the thread number.
     /// </summary>
     threadNumber               :Integer;
     /// <summary>
     ///   Contains the process ID.
     /// </summary>
     procID                     :Cardinal;
     /// <summary>
     ///   Contains the thread ID.
     /// </summary>
     threadID                   :Cardinal;
     /// <summary>
     ///   Contains the input read pipe handle.
     /// </summary>
     readInputPipe              :Cardinal;
     /// <summary>
     ///   Contains the input write pipe handle.
     /// </summary>
     writeInputPipe             :Cardinal;
     /// <summary>
     ///   Contains the output read pipe handle.
     /// </summary>
     readOutputPipe             :Cardinal;
     /// <summary>
     ///   Contains the output write pipe handle.
     /// </summary>
     writeOutputPipe            :Cardinal;
     /// <summary>
     ///   Contains the "dwFlags" of the StartupInfo.
     /// </summary>
     startupInfo_dwFlags        :Cardinal;
     /// <summary>
     ///   Contains the application name.
     /// </summary>
     applicationName            :String;
     /// <summary>
     ///   Contains the command line program.
     /// </summary>
     commandLineProg            :String;
     /// <summary>
     ///   Contains the current directory.
     /// </summary>
     currentDirectory           :String;
  End;

Type
  /// <summary>
  ///    This is the "OnConsoleOutput" event.
  /// </summary>
  /// <param name="outputStr">
  ///    Contains the output from the current process.
  /// </param>
  /// <param name="outputCharArray">
  ///    Contains the output from the current process as an array of Char.
  /// </param>
  /// <param name="totalBytesReaded">
  ///    Return the total readed bytes.
  /// </param>
  /// <param name="totalBytesAvailable">
  ///    Return the total available bytes.
  /// </param>
  TConsoleOutput = Procedure(Sender :TObject; Var outString :String;
                             Var outCharArray :Array Of AnsiChar;
                             Var totalBytesRead :Cardinal;
                             Var totalBytesAvailable :Cardinal) Of Object;

  /// <summary>
  ///   This is the "OnConsoleInput" event.
  /// </summary>
  /// <param name="inputStr">
  ///   Return the string that is send to the process.
  /// </param>
  /// <param name="inputCharArray">
  ///   Return a array of char that is send to the process.
  /// </param>
  /// <param name="totalBytesWritten">
  ///   Return the total bytes that are send to the process.
  /// </param>
  /// <param name="totalBytesAvailable">
  ///   Return the total available bytes that are send to the process.
  /// </param>

  TConsoleInput = Procedure(Sender :TObject; Var inString :String;
                            Var inCharArray :Array Of AnsiChar;
                            Var totalBytesWrite :Cardinal;
                            Var totalBytesAvailable :Cardinal) Of Object;

  /// <summary>
  ///   This is the "OnConsoleError" event.
  /// </summary>
  /// <param name="errorStr">
  ///   Return the error string.
  /// </param>
  /// <param name="errorCharArray">
  ///   Return the error as an array of char.
  /// </param>
  TConsoleError = Procedure(Sender :TObject; Var errString :String;
                            Var errCharArray :Array Of AnsiChar) Of Object;

  /// <summary>
  ///   This is the "OnStatusOutput" event.
  /// </summary>
  TStatusOutput = Procedure(Sender :TObject; Var statInformations :TStatMsg) Of Object;

  /// <summary>
  ///   This is the "OnProcessTermination" event.
  /// </summary>
  /// <param name="processTerminate">
  ///   Is set if the current process is terminated.
  /// </param>
  /// <param name="abortProcess">
  ///   Is set if the current process is aborted (e.g. through a user
  ///   action).
  /// </param>
  /// <remarks>
  ///   If "processTerminate" is "True" then is "abortProcess" set to
  ///   "False" and vice versa. <br /><br />This has a logical reason
  ///   because a process is terminated or aborted. A process never will be
  ///   terminated and aborted at the same time!
  /// </remarks>
  TProcessTermination = Procedure(Sender :TObject; Var processTerminate :Boolean;
                                  Var abortProcess :Boolean;
                                  waitUntilResult :String = '') Of Object;

Type
  /// <summary>
  ///   The "ExternRun" component helps a developer to integrate the
  ///   possibility to start external programs / apps at an easy way.
  /// </summary>
  TExternRunFree = Class(TComponent)
    Private
     FInternTimer               :TTimer;

     FTimerInterval             :Cardinal;

     Procedure InternTimerEvent(Sender :TObject);
    Protected
     FStatusMessage             :TStatMsg;

     FOnConsoleOutput           :TConsoleOutput;
     FOnConsoleInput            :TConsoleInput;
     FOnConsoleError            :TConsoleError;
     FOnStatusMessages          :TStatusOutput;
     FOnProcessTermination      :TProcessTermination;

     FEnvironment               :TStringList;

     FOutputControl             :TMemo;

     FAppName                   :String;
     FCommandLineProg           :String;
     FWriteOutputToFile         :String;
     FCurrDirectory             :String;

     FCopyrightInfo             :String;

     FAppNameP                  :PChar;
     FCommandLineProgP          :PChar;
     FCurrDirectoryP            :PChar;

     FCreationFlags             :Cardinal;
     FWindowState               :Cardinal;
     FWindowFlags               :Cardinal;
     FFillAttribs               :Cardinal;
     FWindowSettings            :TRect;

     FInheritHandles            :Boolean;
     FInputRedirect    :Boolean;
     FOutputRedirect   :Boolean;
     FErrorRedirect    :Boolean;
     FErrorToOutput    :Boolean;

     FWaitUntilProcessExits     :Boolean;

     Procedure Notification(AComponent :TComponent; Operation :TOperation); Override;

     Procedure CreateStatMessage; Virtual;

    Procedure SetAppName(Const Value :String);
     Procedure SetCommandLineProg(Value :String);
     Procedure SetInheritHandles(Const Value :Boolean);
     Procedure SetCurrDirectory(Const Value :String);
     Procedure SetTimerInterval(Const Value :Cardinal);
     Procedure SetWaitUntilProcessExits(Const Value :Boolean);
     Procedure SetWriteOutputToFile(Const Value :String);
     Procedure SetOutpuControl(Const Value :TMemo);

     Procedure ConsoleOutput(Var outputStr :String;
                             Var outputCharArray :Array Of AnsiChar;
                             Var totalBytesReaded :Cardinal;
                             Var totalBytesAvailable :Cardinal);

     Procedure ConsoleInput(Var inputStr :String;
                            Var inputCharArray :Array Of AnsiChar;
                            Var totalBytesWritten :Cardinal;
                            Var totalBytesAvailable :Cardinal);

     Procedure ConsoleError(Var errorStr :String;
                            Var errorCharArray :Array Of AnsiChar);

     Procedure StatusOutput(Var statusInformation :TStatMsg);

     Procedure ProcessTermination(Var processTerminate :Boolean;
                                  Var abortProcess :Boolean;
                                  waitUntilResult :String = '');

     Procedure SetErrorMessage;

     Procedure ExecMainProc;
    Public
     Property CreationFlags :Cardinal Read FCreationFlags Write FCreationFlags;
     /// <summary>
     ///   Enabled or disabled the redirection of the input pipe and output
     ///   pipe of the new process.
     /// </summary>
     /// <remarks>
     ///   This property set automatically the "EnableHandles" property in
     ///   "WindowFlags".
     /// </remarks>
     Property InheritHandles :Boolean Read FInheritHandles Write FInheritHandles;
     /// <summary>
     ///   Enables / Disables the redirection of the process input.
     /// </summary>
     Property RedirectInput :Boolean Read FInputRedirect Write FInputRedirect;
     /// <summary>
     ///   Enables / Disables the redirection of the process output.
     /// </summary>
     Property RedirectOutput :Boolean Read FOutputRedirect Write FOutputRedirect;
     /// <summary>
     ///   Enables / Disables the redirection of the process error.
     /// </summary>
     Property RedirectError :Boolean Read FErrorRedirect Write FErrorRedirect;
     /// <summary>
     ///   Enables / Disables the redirection of the process error to output.
     /// </summary>
     /// <remarks>
     ///   This is very useful if a command line tool send the error messages
     ///   through output instead the error pipe.
     /// </remarks>
     Property RedirectErrorToOutput :Boolean Read FErrorToOutput Write FErrorToOutput;

     Property WindowState :Cardinal Read FWindowState Write FWindowState;
     Property UseWindowFlags :Cardinal Read FWindowFlags Write FWindowFlags;
     Property ConsoleColor :Cardinal Read FFillAttribs Write FFillAttribs;
     Property WindowPosAndSize :TRect Read FWindowSettings Write FWindowSettings;
     /// <summary>
     ///   The "StatusMessage" property is only visible in the source code
     ///   editor because the property return detailed informations about the
     ///   currently running or currently stopped process.
     /// </summary>
     Property StatusMessage :TStatMsg Read FStatusMessage Write FStatusMessage;
     /// <summary>
     ///   The "Environment" property is only visible in the source code
     ///   editor. <br /><br />With this property it is possible to set an
     ///   environment if a program start a command line session. <br /><br />
     ///   The strings have the same values as in a command line session which
     ///   start in Windows.
     /// </summary>
     Property Environment :TStringList Read FEnvironment Write FEnvironment;
     /// <summary>
     ///   Enabled or disabled the waiting of the termination of the new
     ///   process. <br /><br />Very IMPORTANT NOTICE: the process MUST(!)
     ///   terminate itself! <br /><br />Otherwise the application is in an
     ///   endless loop and never exits. Only a manually termination in the
     ///   task manager is possibility!
     /// </summary>
     /// <remarks>
     ///   The best option: never set it to "True".
     /// </remarks>
     Property WaitUntilProcessExits :Boolean Read FWaitUntilProcessExits Write SetWaitUntilProcessExits;
     /// <summary>
     ///   Terminated the currently running process.
     /// </summary>
     Procedure AbortProcessNow;
    Published
     Constructor Create(AOwner :TComponent); Override;

     /// <summary>
     ///   Created and run a new process with the given properties (like
     ///   "AppName" and so on).
     /// </summary>
     Procedure StartNewProcess;
     /// <summary>
     ///   <para>
     ///     Send a string to a created console.
     ///   </para>
     ///   <para>
     ///     This is perfect if a hidden command line session run and an
     ///     application let execute commands that are used in a command line
     ///     normally.
     ///   </para>
     /// </summary>
     /// <param name="sendCommand">
     ///   This string contains the content to send.
     /// </param>
     Procedure WriteToConsole(sendCommand :String);

     Property About :String Read FCopyrightInfo;

     /// <summary>
     ///   This property contains the application name (optional included full
     ///   file path) of the process be start.
     /// </summary>
     Property AppName :String Read FAppName Write SetAppName;
     /// <summary>
     ///   This property contains the command line program name (optional
     ///   included full file path) to start. <br />
     /// </summary>
     Property CommandLineProg :String Read FCommandLineProg Write SetCommandLineProg;
     /// <summary>
     ///   This property contains the directory that the new process use at the
     ///   current directory.
     /// </summary>
     Property CurrDirectory :String Read FCurrDirectory Write SetCurrDirectory;
     /// <summary>
     ///   <para>
     ///     This property set the output of the current created process to a
     ///     TMemo control.
     ///   </para>
     ///   <para>
     ///     If a TMemo is assigned the redirected output displayed
     ///     automatically in the TMemo control.
     ///   </para>
     /// </summary>
     Property OutputControl :TMemo Read FOutputControl Write SetOutpuControl;
     /// <summary>
     ///   This writes the redirect output from the created process in a file. <br /><br />
     ///   Setting a valid filename (with optional path) activate the function
     ///   automatically.
     /// </summary>
     /// <remarks>
     ///   This is perfect to log an output of a command line session. <br />
     /// </remarks>
     Property WriteOutputToFile :String Read FWriteOutputToFile Write SetWriteOutputToFile;

     /// <summary>
     ///    This is the "OnConsoleOutput" event which is fired if process send output through pipe.
     /// </summary>
     /// <remarks>
     /// <para>
     ///   outputStr: Contains the output from the current process.
     /// </para>
     /// <para>
     ///   outputCharArray: Contains the output from the current process as an array of Char.
     /// </para>
     /// <para>
     ///   totalBytesReaded: Return the total readed bytes.
     /// </para>
     /// <para>
     ///   totalBytesAvailable: Return the total available bytes.
     /// </para>
     /// </remarks>
     Property OnConsoleOutput :TConsoleOutput Read FOnConsoleOutput Write FOnConsoleOutput;
     Property OnConsoleInput :TConsoleInput Read FOnConsoleInput Write FOnConsoleInput;
     Property OnConsoleError :TConsoleError Read FOnConsoleError Write FOnConsoleError;

     /// <summary>
     ///   StatusMessages
     /// </summary>
     Property OnStatusMessages :TStatusOutput Read FOnStatusMessages Write FOnStatusMessages;
     Property OnProcessTermination :TProcessTermination Read FOnProcessTermination Write FOnProcessTermination;
  End;

Procedure Register;

Const
  LOGON_WITH_PROFILE = $00000001;
  LOGON32_LOGON_NEW_CREDENTIALS = $00000009;

Function CreateProcessWithLogon(lpUsername :LPCWSTR; lpDomain :LPCWSTR;
                                 lpPassword :LPCWSTR; dwLogonFlags :DWORD;
                                 lpApplicationName :LPCWSTR; lpCommandLine :LPWSTR;
                                 dwCreationFlags :DWORD; lpEnvironment :LPVOID;
                                 lpCurrentDirectory :LPCWSTR; lpStartupInfo :TStartupInfoW;
                                 Var lpProcessInformation :TProcessInformation) :BOOL; stdcall;
{$EXTERNALSYM CreateProcessWithLogon}

Implementation

Function CreateProcessWithLogon; External Advapi32 Name 'CreateProcessWithLogonW';

Var
 FStartupInfo                        :TStartupInfo;
 FProcessInfo                        :TProcessInformation;
 FSecurityAttribs                    :TSecurityAttributes;

 FInputReadPipe                      :THandle;
 FInputWritePipe                     :THandle;
 FOutputReadPipe                     :THandle;
 FOutputWritePipe                    :THandle;
 FErrorReadPipe                      :THandle;
 FErrorWritePipe                     :THandle;

 logFileHandle                       :THandle;

 waitForProcessResult                :Cardinal;

 numberOfBytesToWrite                :Cardinal;

 readedoutBytes,readederrBytes       :Cardinal;
 totaloutBytesAvailable              :Cardinal;
 totalerrBytesAvailable              :Cardinal;

 terminateContinueFlag               :Byte;

 commonResult                        :BOOL;

 resultReadFile,resultWriteFile      :Boolean;
 terminateStat,abortStat             :Boolean;

 CRLF                                :String;

Constructor TExternRunFree.Create(AOwner :TComponent);
Begin
 Inherited Create(AOwner);

 CreateStatMessage;

 FInternTimer := TTimer.Create(Self);

 FInternTimer.OnTimer := InternTimerEvent;

 FInternTimer.Enabled := False;
 FInternTimer.Interval := 0;

 FillMemory(@FStartupInfo,SizeOf(FStartupInfo),0);
 FillMemory(@FProcessInfo,SizeOf(FProcessInfo),0);
 FillMemory(@FSecurityAttribs,SizeOf(FSecurityAttribs),0);

 FStartupInfo.cb := SizeOf(FStartupInfo);
 FStartupInfo.lpReserved := NIL;
 FStartupInfo.lpTitle := NIL;

 FSecurityAttribs.nLength := SizeOf(FSecurityAttribs);
 FSecurityAttribs.bInheritHandle := True;
 FSecurityAttribs.lpSecurityDescriptor := NIL;

 FInheritHandles := False;

 numberOfBytesToWrite := 0;

                // '© by neXt generation Software in 2021 - 2025.';

 FCopyrightInfo := Chr(169) + Chr(32) + Chr(98) + Chr(121) + Chr(32) + Chr(110) + Chr(101) + Chr(88) +
                   Chr(116) + Chr(32) + Chr(103) + Chr(101) + Chr(110) + Chr(101) + Chr(114) + Chr(97) +
                   Chr(116) + Chr(105) + Chr(111) + Chr(110) + Chr(32) + Chr(83) + Chr(111) + Chr(102) +
                   Chr(116) + Chr(119) + Chr(97) + Chr(114) + Chr(101) + Chr(32) + Chr(105) + Chr(110) +
                   Chr(32) + Chr(50) + Chr(48) + Chr(50) + Chr(49) + Chr(32) + Chr(45) + Chr(32) + Chr(50) +
                   Chr(48) + Chr(50) + Chr(53) + Chr(46);

 FWriteOutputToFile := '';

 terminateContinueFlag := 0;

 readedoutBytes := 0;
 readederrBytes := 0;
 totaloutBytesAvailable := 0;
 totalerrBytesAvailable := 0;

 CRLF := Chr(13) + Chr(10);
End;


Procedure TExternRunFree.InternTimerEvent(Sender: TObject);
begin
 ExecMainProc;
end;

Procedure TExternRunFree.StartNewProcess;

Var
 envBlock                            :PChar;

begin
 If (FAppName = '') And (FCommandLineProg = '') Then
  Begin    // '"AppName" and "CommandLineProg" properties are empty!'
   MessageDlg(Chr(34) + Chr(65) + Chr(112) + Chr(112) + Chr(78) + Chr(97) + Chr(109) + Chr(101) +
              Chr(34) + Chr(32) + Chr(97) + Chr(110) + Chr(100) + Chr(32) + Chr(34) + Chr(67) + Chr(111) +
              Chr(109) + Chr(109) + Chr(97) + Chr(110) + Chr(100) + Chr(76) + Chr(105) + Chr(110) + Chr(101) +
              Chr(80) + Chr(114) + Chr(111) + Chr(103) + Chr(34) + Chr(32) + Chr(112) + Chr(114) + Chr(111) +
              Chr(112) + Chr(101) + Chr(114) + Chr(116) + Chr(105) + Chr(101) + Chr(115) + Chr(32) + Chr(97) +
              Chr(114) + Chr(101) + Chr(32) + Chr(101) + Chr(109) + Chr(112) + Chr(116) + Chr(121) + Chr(33)
              ,mtError,[mbOK],0);
   Halt;
  End;

 If Length(FAppName) > 0 Then
  Begin
   FAppNameP := PChar(FAppName);

   If Length(FCommandLineProg) > 0 Then
    Begin
     FCommandLineProg := ' ' + FCommandLineProg;
     FCommandLineProgP := PChar(FCommandLineProg);
    End;
  End;

 If (Length(FAppName) = 0) And (Length(FCommandLineProg) > 0) Then
  Begin
   FAppNameP := NIL;
   FCommandLineProgP := PChar(FCommandLineProg);
  End;

 If Length(FCommandLineProg) > 0 Then
  Begin
   FCommandLineProgP := PChar(FCommandLineProg);
  End;

 If Length(FCurrDirectory) > 0 Then
  Begin
   FCurrDirectoryP := PChar(FCurrDirectory);
  End
 Else
  Begin
   FCurrDirectoryP := NIL;
  End;

 If (Length(FAppName) = 0) And (Length(FCurrDirectory) = 0) Then
  Begin
   FCommandLineProgP := PChar(FCommandLineProg);
   FAppNameP := NIL;
  End;

 FStartupInfo.wShowWindow := 0;
 FStartupInfo.dwFlags := 0;
 FStartupInfo.dwFillAttribute := 0;

 If (FEnvironment = NIL) Or (FEnvironment.Count > 0) Then
  Begin
   FCreationFlags := FCreationFlags Or CREATE_UNICODE_ENVIRONMENT;
  End;

 FStartupInfo.wShowWindow := FWindowState;
 FStartupInfo.dwFlags := FWindowFlags;
 FStartupInfo.dwFillAttribute := FFillAttribs;

 If FWindowFlags And (STARTF_USEPOSITION Or STARTF_USESIZE) = (STARTF_USEPOSITION Or STARTF_USESIZE) Then
  Begin
   FStartupInfo.dwX := FWindowSettings.Top;
   FStartupInfo.dwY := FWindowSettings.Left;
   FStartupInfo.dwXSize := FWindowSettings.Width;
   FStartupInfo.dwYSize := FWindowSettings.Height;
  End;

 If FInheritHandles = True Then
  Begin
   If FOutputRedirect = True Then
    Begin
     If CreatePipe(FOutputReadPipe,FOutputWritePipe,@FSecurityAttribs,2048) = False Then
      Begin
       FStatusMessage.procNumber := FProcessInfo.hProcess;
       FStatusMessage.threadNumber := FProcessInfo.hThread;
       FStatusMessage.procID := FProcessInfo.dwProcessId;
       FStatusMessage.threadID := FProcessInfo.dwThreadId;
       FStatusMessage.startupInfo_dwFlags := FStartupInfo.dwFlags;
       FStatusMessage.applicationName := '';
       FStatusMessage.commandLineProg := FOutputReadPipe.ToString;
       FStatusMessage.currentDirectory := FOutputWritePipe.ToString;

       StatusOutput(FStatusMessage);

       Exit;
      End;

     FStartupInfo.hStdOutput := FOutputWritePipe;
    End;

   If FInputRedirect = True Then
    Begin
     If CreatePipe(FInputReadPipe,FInputWritePipe,@FSecurityAttribs,2048) = False Then
      Begin
       FStatusMessage.procNumber := FProcessInfo.hProcess;
       FStatusMessage.threadNumber := FProcessInfo.hThread;
       FStatusMessage.procID := FProcessInfo.dwProcessId;
       FStatusMessage.threadID := FProcessInfo.dwThreadId;
       FStatusMessage.startupInfo_dwFlags := FStartupInfo.dwFlags;
       FStatusMessage.applicationName := '';
       FStatusMessage.commandLineProg := FOutputReadPipe.ToString;
       FStatusMessage.currentDirectory := FOutputWritePipe.ToString;

       StatusOutput(FStatusMessage);

       Exit;
      End;

     FStartupInfo.hStdInput := FInputReadPipe;
    End;

   If FErrorRedirect = True Then
    Begin
     If CreatePipe(FErrorReadPipe,FErrorWritePipe,@FSecurityAttribs,2048) = False Then
      Begin
       FStatusMessage.procNumber := FProcessInfo.hProcess;
       FStatusMessage.threadNumber := FProcessInfo.hThread;
       FStatusMessage.procID := FProcessInfo.dwProcessId;
       FStatusMessage.threadID := FProcessInfo.dwThreadId;
       FStatusMessage.startupInfo_dwFlags := FStartupInfo.dwFlags;
       FStatusMessage.applicationName := '';
       FStatusMessage.commandLineProg := FOutputReadPipe.ToString;
       FStatusMessage.currentDirectory := FOutputWritePipe.ToString;

       StatusOutput(FStatusMessage);

       Exit;
      End;

     FStartupInfo.hStdError := FErrorWritePipe;
    End;
  End;

 envBlock := NIL;

 If FEnvironment <> NIL Then
  Begin
   envBlock := PChar(ReplaceStr(FEnvironment.Text,CRLF,Chr(0)));
  End;

 // ############################################################################
 // "ActivateDefaultCreate" => CreateProcess
 // ############################################################################
 {$REGION 'CreateProcess'}
 commonResult := CreateProcess(FAppNameP,             // ApplicationName
                               FCommandLineProgP,     // CommandLine
                               NIL,                   // ProcessAttributes
                               NIL,                   // ThreadAttributes
                               FInheritHandles,       // InheritHandles
                               FCreationFlags,        // CreationFlags
                               envBlock,              // Environment
                               FCurrDirectoryP,       // CurrentDirectory
                               FStartupInfo,          // StartupInfo
                               FProcessInfo);         // ProcessInformation
 {$ENDREGION}
 // ############################################################################

 SetErrorMessage;

 If FWriteOutputToFile <> '' Then
  Begin
   logFileHandle := FileCreate(FWriteOutputToFile);
  End;

 {$IFDEF VER310}
 FTimerInterval := 50;
 {$ENDIF}

 {$IFDEF VER320}
 FTimerInterval := 100;
 {$ENDIF}

 {$IFDEF VER330}
 FTimerInterval := 100;
 {$ENDIF}

 {$IFDEF VER340}
 FTimerInterval := 100;
 {$ENDIF}

 FInternTimer.Interval := FTimerInterval;
 FInternTimer.Enabled := True;
end;

Procedure TExternRunFree.CreateStatMessage;
begin
 FStatusMessage := TStatMsg.Create;
end;

Procedure TExternRunFree.SetAppName(Const Value :String);
begin
 FAppName := Value;
end;

Procedure TExternRunFree.SetCommandLineProg(Value :String);
begin
 FCommandLineProg := Value;
end;

Procedure TExternRunFree.SetCurrDirectory(Const Value :String);
begin
 FCurrDirectory := Value;
end;

Procedure TExternRunFree.SetInheritHandles(Const Value :Boolean);
begin
 FInheritHandles := Value;

 If FInheritHandles = True Then
  Begin
   FWaitUntilProcessExits := False;
  End;
end;

Procedure TExternRunFree.SetTimerInterval(Const Value :Cardinal);
begin
 FTimerInterval := Value;
end;

Procedure TExternRunFree.SetWaitUntilProcessExits(Const Value :Boolean);
begin
 FWaitUntilProcessExits := Value;

 If FWaitUntilProcessExits = True Then
  Begin
   FInheritHandles := False;
  End;
end;

Procedure TExternRunFree.SetWriteOutputToFile(Const Value :String);
begin
 FWriteOutputToFile := Value;

 If FWriteOutputToFile <> '' Then
  Begin
   If (csDesigning In ComponentState = True) Or (csDesigning In ComponentState = False) Then
    Begin
     If FileExists(FWriteOutputToFile) = True Then
      Begin
       If GetLastError = ERROR_ALREADY_EXISTS Then
        Begin
         MessageDlg(SysErrorMessage(GetLastError),mtWarning,[mbYes],0);
        End;
      End;
    End;
  End;
end;

Procedure TExternRunFree.Notification(AComponent :TComponent; Operation :TOperation);
begin
 Inherited Notification(AComponent,Operation);

 If (Operation = opRemove) And (AComponent = FOutputControl) Then
  Begin
   FOutputControl := NIL;
  End;
end;

Procedure TExternRunFree.SetOutpuControl(Const Value :TMemo);
begin
 Value.FreeNotification(Self);

 FOutputControl := Value;
end;

Procedure TExternRunFree.ConsoleOutput(Var outputStr :String;
                                   Var outputCharArray :Array Of AnsiChar;
                                   Var totalBytesReaded: Cardinal;
                                   Var totalBytesAvailable :Cardinal);
begin
 If Assigned(FOnConsoleOutput) = True Then
  Begin
   FOnConsoleOutput(Self,outputStr,outputCharArray,totalBytesReaded,totalBytesAvailable);
  End;
end;

Procedure TExternRunFree.ConsoleInput(Var inputStr :String;
                                  Var inputCharArray :Array of AnsiChar;
                                  Var totalBytesWritten: Cardinal;
                                  Var totalBytesAvailable :Cardinal);
begin
 If Assigned(FOnConsoleInput) = True Then
  Begin
   FOnConsoleInput(Self,inputStr,inputCharArray,totalBytesWritten,totalBytesAvailable);
  End;
end;

Procedure TExternRunFree.ConsoleError(Var errorStr :String;
                                  Var errorCharArray :Array of AnsiChar);
begin
 If Assigned(FOnConsoleError) = True Then
  Begin
   FOnConsoleError(Self,errorStr,errorCharArray);
  End;
end;

Procedure TExternRunFree.StatusOutput(Var statusInformation :TStatMsg);
begin
 If Assigned(FOnStatusMessages) = True Then
  Begin
   FOnStatusMessages(Self,statusInformation);
  End;
end;

Procedure TExternRunFree.ProcessTermination(Var processTerminate :Boolean;
                                        Var abortProcess :Boolean;
                                        waitUntilResult :String = '');
begin
 If Assigned(FOnProcessTermination) = True Then
  Begin
   FOnProcessTermination(Self,processTerminate,abortProcess,waitUntilResult);
  End;

 terminateContinueFlag := 1;
end;

Procedure TExternRunFree.AbortProcessNow;
begin
 If TerminateProcess(FProcessInfo.hProcess,255) = True Then
  Begin
   terminateStat := False;
   abortStat := True;

   ProcessTermination(terminateStat,abortStat);
  End;
end;

Procedure TExternRunFree.WriteToConsole(sendCommand :String);

Var inBuffer         :Array[0..2048] Of AnsiChar;

    tmpAnsiString    :AnsiString;

    writtenBytes     :Cardinal;
    i,z              :Integer;

begin
 If Length(sendCommand) = 0 Then
  Begin
   Exit;
  End;

 tmpAnsiString := AnsiToUTF8(sendCommand);
 z := 1;

 For i := 0 To 2048 Do
  Begin
   inBuffer[i] := Chr(0);
  End;

 For i := 0 To Length(tmpAnsiString) Do
  Begin
   inBuffer[i] := tmpAnsiString[z];
   z := z + 1;
  End;

 inBuffer[Length(tmpAnsiString)] := Chr(13);
 inBuffer[Length(tmpAnsiString) + 1] := Chr(10);

 numberOfBytesToWrite := Length(tmpAnsiString) + 2;

 resultWriteFile := WriteFile(FInputWritePipe,inBuffer,numberOfBytesToWrite,writtenBytes,NIL);

 If (resultWriteFile = True) And (writtenBytes > 0) Then
  Begin
   ConsoleInput(sendCommand,inBuffer,writtenBytes,numberOfBytesToWrite);
  End;
end;

Procedure TExternRunFree.ExecMainProc;

Var outBuffer,errBuffer                               :Array[0..65535] Of AnsiChar;

    conOutput,conError                                :String;

    i                                                 :Integer;

Begin
 If FWaitUntilProcessExits = False Then
  Begin
   If FInheritHandles = True Then
    Begin
     readedoutBytes := 0;
     readederrBytes := 0;
     conOutput := '';
     conError := '';

     If FOutputRedirect = True Then
      Begin
       PeekNamedPipe(FOutputReadPipe,NIL,0,NIL,@totaloutBytesAvailable,NIL);

       If totaloutBytesAvailable > 0 Then
        Begin
         For i := 0 To totaloutBytesAvailable Do
          Begin
           outBuffer[i] := Chr(0);
          End;

         resultReadFile := ReadFile(FOutputReadPipe,outBuffer,totaloutBytesAvailable,
                                    readedoutBytes,NIL);

         If (logFileHandle <> INVALID_HANDLE_VALUE) And (FWriteOutputToFile <> '') Then
          Begin
           FileWrite(logFileHandle,outBuffer,readedoutBytes);
          End;

         For i := 0 To readedoutBytes Do
          Begin
           conOutput := conOutput + Utf8ToAnsi(outBuffer[i]);
          End;

         If FOutputControl <> NIL Then
          Begin
           FOutputControl.Lines.Add(conOutput);
          End
         Else
          Begin
           ConsoleOutput(conOutput,outBuffer,readedoutBytes,totaloutBytesAvailable);
          End;
        End;
      End;

     If FErrorRedirect = True Then
      Begin
       PeekNamedPipe(FErrorReadPipe,NIL,0,NIL,@totalerrBytesAvailable,NIL);

       If totalerrBytesAvailable > 0 Then
        Begin
         For i := 0 To totalerrBytesAvailable Do
          Begin
           errBuffer[i] := Chr(0);
          End;

         resultReadFile := ReadFile(FErrorReadPipe,errBuffer,totalerrBytesAvailable,
                                    readederrBytes,NIL);

         If (logFileHandle <> INVALID_HANDLE_VALUE) And (FWriteOutputToFile <> '') Then
          Begin
           FileWrite(logFileHandle,errBuffer,readederrBytes);
          End;

         For i := 0 To readederrBytes Do
          Begin
           conError := conError + Utf8ToAnsi(errBuffer[i]);
          End;

         If FErrorToOutput = True Then
          Begin
           If FOutputControl = NIL Then
            Begin
             ConsoleOutput(conError,errBuffer,readederrBytes,totalerrBytesAvailable);
            End
           Else
            Begin
             FOutputControl.Lines.Add(conError);
            End;
          End
         Else
          Begin
           ConsoleError(conError,errBuffer);
          End;
        End;
      End;
    End;
  End;

 If FWaitUntilProcessExits = True Then
  Begin
   waitForProcessResult := WaitForSingleObject(FProcessInfo.hProcess,INFINITE);

   FInternTimer.Enabled := False;

   If waitForProcessResult = WAIT_ABANDONED Then
    Begin
     terminateStat := True;
     abortStat := False;

     ProcessTermination(terminateStat,abortStat,'WAIT_ABANDONED');
    End;

   If waitForProcessResult = WAIT_OBJECT_0 Then
    Begin
     terminateStat := True;
     abortStat := False;

     ProcessTermination(terminateStat,abortStat,'Process exit normal.');
    End;

   If waitForProcessResult = WAIT_TIMEOUT Then
    Begin
     terminateStat := False;
     abortStat := False;

     ProcessTermination(terminateStat,abortStat,'Process has timeout, but not exited.');
    End;

   If waitForProcessResult = WAIT_FAILED Then
    Begin
     terminateStat := True;
     abortStat := False;

     ProcessTermination(terminateStat,abortStat,'Error ' + GetLastError.ToString +
                        ': ' + SysErrorMessage(GetLastError));
    End;

   Exit;
  End;

 If WaitForSingleObject(FProcessInfo.hProcess,0) = WAIT_OBJECT_0 Then
  Begin
   If FInputRedirect = True Then
    Begin
     CloseHandle(FInputReadPipe);
     CloseHandle(FInputWritePipe);
    End;

   If FOutputRedirect = True Then
    Begin
     PeekNamedPipe(FOutputReadPipe,NIL,0,NIL,@totaloutBytesAvailable,NIL);

     If totaloutBytesAvailable > 0 Then
      Begin
       For i := 0 To totaloutBytesAvailable Do
        Begin
         outBuffer[i] := Chr(0);
        End;

       resultReadFile := ReadFile(FOutputReadPipe,outBuffer,totaloutBytesAvailable,
                                readedoutBytes,NIL);

       If (logFileHandle <> INVALID_HANDLE_VALUE) And (FWriteOutputToFile <> '') Then
        Begin
         FileWrite(logFileHandle,outBuffer,readedoutBytes);
        End;

       For i := 0 To readedoutBytes Do
        Begin
         conOutput := conOutput + Utf8ToAnsi(outBuffer[i]);
        End;

       If FOutputControl <> NIL Then
        Begin
         FOutputControl.Lines.Add(conOutput);
        End
       Else
        Begin
         ConsoleOutput(conOutput,outBuffer,readedoutBytes,totaloutBytesAvailable);
        End;
      End;

     CloseHandle(FOutputReadPipe);
     CloseHandle(FOutputWritePipe);
    End;

   If FErrorRedirect = True Then
    Begin
     PeekNamedPipe(FErrorReadPipe,NIL,0,NIL,@totalerrBytesAvailable,NIL);

     If totalerrBytesAvailable > 0 Then
      Begin
       For i := 0 To totalerrBytesAvailable Do
        Begin
         errBuffer[i] := Chr(0);
        End;

       resultReadFile := ReadFile(FErrorReadPipe,errBuffer,totalerrBytesAvailable,
                                  readederrBytes,NIL);

       If (logFileHandle <> INVALID_HANDLE_VALUE) And (FWriteOutputToFile <> '') Then
        Begin
         FileWrite(logFileHandle,errBuffer,readederrBytes);
        End;

       For i := 0 To readederrBytes Do
        Begin
         conError := conError + Utf8ToAnsi(errBuffer[i]);
        End;

       If FErrorToOutput = True Then
        Begin
         If FOutputControl = NIL Then
          Begin
           ConsoleOutput(conError,errBuffer,readederrBytes,totalerrBytesAvailable);
          End
         Else
          Begin
           FOutputControl.Lines.Add(conError);
          End;
        End
       Else
        Begin
         ConsoleError(conError,errBuffer);
        End;
      End;

     CloseHandle(FErrorReadPipe);
     CloseHandle(FErrorWritePipe);
    End;

   If logFileHandle <> INVALID_HANDLE_VALUE Then
    Begin
     FileClose(logFileHandle);
    End;

   CloseHandle(FProcessInfo.hProcess);
   CloseHandle(FProcessInfo.hThread);

   terminateStat := True;
   abortStat := False;

   ProcessTermination(terminateStat,abortStat);

   While terminateContinueFlag = 0 Do
    Begin
     // Wait for OnProcessTermination Event
    End;

   FInternTimer.Enabled := False;

   Exit;
  End;
End;

Procedure TExternRunFree.SetErrorMessage;

Var lastError                               :Cardinal;
    errorMessage,commandLine,messageText    :String;

begin
 lastError := GetLastError;
 errorMessage := SysErrorMessage(lastError);

 If lastError <> 0 Then
  Begin        // 'Error: '
   messageText := Chr(69) + Chr(114) + Chr(114) + Chr(111) + Chr(114) + Chr(58) + Chr(32)
                  + lastError.ToString + CRLF + errorMessage;

   If lastError = 2 Then
    Begin
     If FAppName <> '' Then
      Begin
       commandLine := FAppName;
      End;

     If FCommandLineProg <> '' Then
      Begin
       commandLine := FAppName + ' ' + FCommandLineProg;
      End;

     If FCurrDirectory <> '' Then
      Begin
       commandLine := FCurrDirectory;
      End;
                                             // 'Command: '
     messageText := messageText + CRLF + CRLF + Chr(67) + Chr(111) + Chr(109) + Chr(109) + Chr(97) +
                                  Chr(110) + Chr(100) + Chr(58) + Chr(32) + CRLF + commandLine;
    End;

   MessageDlg(messageText,mtError,[mbOK],0);
   Exit;
  End;

 FStatusMessage.procNumber := FProcessInfo.hProcess;
 FStatusMessage.threadNumber := FProcessInfo.hThread;
 FStatusMessage.procID := FProcessInfo.dwProcessId;
 FStatusMessage.threadID := FProcessInfo.dwThreadId;
 FStatusMessage.readInputPipe := FInputReadPipe;
 FStatusMessage.writeInputPipe := FInputWritePipe;
 FStatusMessage.readOutputPipe := FOutputReadPipe;
 FStatusMessage.writeOutputPipe := FOutputWritePipe;
 FStatusMessage.startupInfo_dwFlags := FStartupInfo.dwFlags;

 StatusOutput(FStatusMessage);
end;

Procedure Register;
Begin
 RegisterComponents('ExternRun Free version',[TExternRunFree]);
End;

end.
