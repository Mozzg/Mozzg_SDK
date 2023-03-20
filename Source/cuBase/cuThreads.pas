unit cuThreads;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils, WinAPI.Windows,
  cuConsts,
  uCustomExceptions;

const
  THREAD_EVENT_ERROR_ID = -1;
  THREAD_EVENT_TIMEOUT_ID = 0;
  THREAD_EVENT_PAUSING_ID = 1;
  THREAD_EVENT_PAUSED_ID = 2;
  THREAD_EVENT_UNPAUSING_ID = 3;
  THREAD_EVENT_UNPAUSED_ID = 4;
  THREAD_EVENT_TERMINATING_ID = 5;

  DEFAULT_MAX_THREAD_EVENT_ID = 5;

type
  TCustomEventThread = class(TThread)
  protected
    fPausingEvent: TEvent;
    fPausedEvent: TEvent;
    fUnpausingEvent: TEvent;
    fUnpausedEvent: TEvent;
    fTerminatingEvent: TEvent;
    // +++ Подумать, может стоит сделать просто динамический массив и динамически его расширять при добавлении эвента
    fEventsArray: array of TEvent;
    fOnPaused: TNotifyEvent;
    fOnUnpaused: TNotifyEvent;

    procedure DoPaused; virtual;
    procedure DoUnpaused; virtual;

    procedure ChangePausingToPaused;
    procedure ChangeUnpausingToUnpaused;
    function CheckAndWaitIfPaused: Boolean;
    // Be very careful when using this method, it may cause deadlock.
    function WaitForThreadEvents(const aEvents: array of Integer; const aTimeout: DWORD): Integer;

    class function GetMaxEventID: Integer; virtual;
    procedure InitEventsArray; virtual;
    procedure TerminatedSet; override;
  public
    constructor Create(aCreateSuspended: Boolean);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure AddCustomEvent(aEventIndex: Integer; aEvent: TEvent); virtual;

    procedure PauseThread;
    procedure PauseThreadAndWait;
    procedure UnpauseThread;
    procedure UnpauseThreadAndWait;
    function IsPaused: Boolean;
    function IsRunning: Boolean;

    property MaxEventID: Integer read GetMaxEventID;
    property OnPaused: TNotifyEvent read fOnPaused write fOnPaused;
    property OnUnpaused: TNotifyEvent read fOnUnpaused write fOnUnpaused;
  end;

implementation

// +++ потом убрать
uses
  Vcl.Forms,
  Unit1;

{ TCustomEventThread }

constructor TCustomEventThread.Create(aCreateSuspended: Boolean);
begin
  fPausingEvent := TEvent.Create(nil, True, False, EmptyStr);
  fPausedEvent := TEvent.Create(nil, True, False, EmptyStr);
  fUnpausingEvent := TEvent.Create(nil, True, False, EmptyStr);
  fUnpausedEvent := TEvent.Create(nil, True, True, EmptyStr);
  fTerminatingEvent := TEvent.Create(nil, True, False, EmptyStr);

  inherited Create(aCreateSuspended);
end;

destructor TCustomEventThread.Destroy;
begin
  inherited Destroy;
  SetLength(fEventsArray, 0);
  fPausingEvent.Free;
  fPausedEvent.Free;
  fUnpausingEvent.Free;
  fUnpausedEvent.Free;
  fTerminatingEvent.Free;
end;

procedure TCustomEventThread.AfterConstruction;
begin
  InitEventsArray;
  inherited AfterConstruction;
end;

procedure TCustomEventThread.DoPaused;
begin
  if Assigned(fOnPaused) then fOnPaused(Self);
end;

procedure TCustomEventThread.DoUnpaused;
begin
  if Assigned(fOnUnpaused) then fOnUnpaused(Self);
end;

procedure TCustomEventThread.ChangePausingToPaused;
begin
  fUnpausedEvent.ResetEvent;
  fPausedEvent.SetEvent;
end;

procedure TCustomEventThread.ChangeUnpausingToUnpaused;
begin
  fPausedEvent.ResetEvent;
  fUnpausedEvent.SetEvent;
end;

function TCustomEventThread.CheckAndWaitIfPaused: Boolean;
var
  lWaitResult: Integer;
begin
  //Log(Format('Thread%d Enter WaitIfPaused', [TThread.CurrentThread.ThreadID]));
  lWaitResult := WaitForThreadEvents([THREAD_EVENT_PAUSING_ID, THREAD_EVENT_TERMINATING_ID], 0);

  //Log(Format('Thread%d delay after WaitForThreadEvents', [TThread.CurrentThread.ThreadID]));
  //Sleep(1000);

  case lWaitResult of
    THREAD_EVENT_TERMINATING_ID:
      Exit(False);
    THREAD_EVENT_TIMEOUT_ID:
      Exit(True);
    THREAD_EVENT_PAUSING_ID:
    begin
      // +++ подумать, может сделать эвент Unpausing и смотреть его. А в Unpause делать ожидания полного запуска.
      // +++ ещё сделать функции в потоке типа IsPaused, IsRunning, где можно проверять оба эвента Pausing и Paused
      //Log(Format('Thread%d before ChangePausingToPaused', [TThread.CurrentThread.ThreadID]));
      ChangePausingToPaused;
      DoPaused;
      lWaitResult := WaitForThreadEvents([THREAD_EVENT_UNPAUSING_ID, THREAD_EVENT_TERMINATING_ID], INFINITE);
      //Log(Format('Thread%d after ChangePausingToPaused', [TThread.CurrentThread.ThreadID]));
      //Sleep(1000);
      if lWaitResult = THREAD_EVENT_TERMINATING_ID then
        Exit(False);
      if lWaitResult <> THREAD_EVENT_UNPAUSING_ID then
        raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR, [ClassName]);
      //Log(Format('Thread%d before ChangeUnpausingToUnpaused', [TThread.CurrentThread.ThreadID]));
      ChangeUnpausingToUnpaused;
      DoUnpaused;
    end;
  else
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR, [ClassName]);
  end;

  Result := True;
  Log(Format('Thread%d Exit WaitIfPaused', [TThread.CurrentThread.ThreadID]));
end;

function TCustomEventThread.WaitForThreadEvents(const aEvents: array of Integer; const aTimeout: DWORD): Integer;
var
  lHandles: array of THandle;
  lEventCount: DWORD;
  lMaxEventIndex, i: Integer;
  lWaitResult: Cardinal;
  lMsg: TMsg;
begin
  lEventCount := Length(aEvents);
  if lEventCount = 0 then
    Exit(THREAD_EVENT_ERROR_ID);

  lMaxEventIndex := GetMaxEventID;
  SetLength(lHandles, lEventCount);
  for i := Low(lHandles) to High(lHandles) do
  begin
    if (aEvents[i] < 0) or (aEvents[i] > lMaxEventIndex) then
      raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_INVALID_EVENT_ID, [aEvents[i], ClassName]);
    if not Assigned(fEventsArray[aEvents[i]]) then
      raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_ID_NOT_ASSIGNED, [aEvents[i], ClassName]);

    lHandles[i] := fEventsArray[aEvents[i]].Handle;
  end;

  //Log(Format('Thread%d WaitForMultipleObjects', [TThread.CurrentThread.ThreadID]));
  (*if CurrentThread.ThreadID = MainThreadID then
  begin
    repeat
      lWaitResult := MsgWaitForMultipleObjects(lEventCount, lHandles[0], False, aTimeout, QS_ALLINPUT);
      if lWaitResult = (WAIT_OBJECT_0 + lEventCount) then
      begin
        Application.ProcessMessages;
        //PeekMessage(lMsg, 0, 0, 0, PM_NOREMOVE);
        {if PeekMessage(lMsg, 0, 0, 0, PM_NOREMOVE) then
          if PeekMessage(lMsg, 0, 0, 0, PM_REMOVE) then
          begin
            TranslateMessage(lMsg);
            DispatchMessage(lMsg);
          end;   }
      end;
      //lWaitResult := WaitForMultipleObjects(lEventCount, @lHandles[0], False, aTimeout);
    until lWaitResult < (WAIT_OBJECT_0 + lEventCount);
  end
  else      *)
    lWaitResult := WaitForMultipleObjects(lEventCount, @lHandles[0], False, aTimeout);

  if (lWaitResult = WAIT_FAILED) or ((lWaitResult >= WAIT_ABANDONED_0) and (lWaitResult < (WAIT_ABANDONED_0 + lEventCount))) then
    Exit(THREAD_EVENT_ERROR_ID);
  if lWaitResult = WAIT_TIMEOUT then
    Exit(THREAD_EVENT_TIMEOUT_ID);
  Result := aEvents[lWaitResult - WAIT_OBJECT_0];
end;

class function TCustomEventThread.GetMaxEventID: Integer;
begin
  Result := DEFAULT_MAX_THREAD_EVENT_ID;
end;

procedure TCustomEventThread.InitEventsArray;
begin
  if GetMaxEventID < 0 then
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_INVALID_MAX_EVENT_ID, [ClassName, GetMaxEventID]);
  SetLength(fEventsArray, GetMaxEventID + 1);
  AddCustomEvent(THREAD_EVENT_PAUSING_ID, fPausingEvent);
  AddCustomEvent(THREAD_EVENT_PAUSED_ID, fPausedEvent);
  AddCustomEvent(THREAD_EVENT_UNPAUSING_ID, fUnpausingEvent);
  AddCustomEvent(THREAD_EVENT_UNPAUSED_ID, fUnpausedEvent);
  AddCustomEvent(THREAD_EVENT_TERMINATING_ID, fTerminatingEvent);
end;

procedure TCustomEventThread.TerminatedSet;
begin
  inherited TerminatedSet;
  fTerminatingEvent.SetEvent;
end;

procedure TCustomEventThread.AddCustomEvent(aEventIndex: Integer; aEvent: TEvent);
begin
  if (aEventIndex < 0) or (aEventIndex > GetMaxEventID) then
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_INVALID_EVENT_ID, [aEventIndex, ClassName]);
  fEventsArray[aEventIndex] := aEvent;
end;

procedure TCustomEventThread.PauseThread;
begin
  fUnpausingEvent.ResetEvent;
  fPausingEvent.SetEvent;
end;

procedure TCustomEventThread.PauseThreadAndWait;
begin
  PauseThread;
  if WaitForThreadEvents([THREAD_EVENT_PAUSED_ID], INFINITE) = THREAD_EVENT_ERROR_ID then
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR, [ClassName]);
end;

procedure TCustomEventThread.UnpauseThread;
begin
  fPausingEvent.ResetEvent;
  fUnpausingEvent.SetEvent;
end;

procedure TCustomEventThread.UnpauseThreadAndWait;
begin
  UnpauseThread;
  if WaitForThreadEvents([THREAD_EVENT_UNPAUSED_ID], INFINITE) = THREAD_EVENT_ERROR_ID then
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR, [ClassName]);
end;

function TCustomEventThread.IsPaused: Boolean;
var
  lWaitResult: Integer;
begin
  lWaitResult := WaitForThreadEvents([THREAD_EVENT_PAUSING_ID, THREAD_EVENT_PAUSED_ID], 0);

  if lWaitResult = THREAD_EVENT_ERROR_ID then
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR, [ClassName]);
  if (lWaitResult = THREAD_EVENT_PAUSING_ID) or (lWaitResult = THREAD_EVENT_PAUSED_ID) then
    Exit(True);
  Result := False;
end;

function TCustomEventThread.IsRunning: Boolean;
var
  lWaitResult: Integer;
begin
  lWaitResult := WaitForThreadEvents([THREAD_EVENT_PAUSING_ID, THREAD_EVENT_UNPAUSED_ID], 0);

  // +++ продумать и проверить правильность условий ожидания и переключения статусов
  if lWaitResult = THREAD_EVENT_ERROR_ID then
    raise ECustomEventThreadException.CreateFmt(EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR, [ClassName]);
  if (lWaitResult = THREAD_EVENT_PAUSING_ID) or (lWaitResult = THREAD_EVENT_TIMEOUT_ID) then
    Exit(False);
  Result := True;
end;

end.
