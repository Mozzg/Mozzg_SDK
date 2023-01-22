unit cuSystem;

interface

uses
  Winapi.Windows, System.SysUtils, System.Math, Winapi.MMSystem,
  cuConsts;

type
  // Custom High resolution timer
  THighResTimer = class(TObject)
  strict private
    fTicksPerSecond: Int64;
    fInverseTicksPerMilisecond: Double;
    fInverseTicksPerMicrosecond: Double;
    fInverseTicksPerNanosecond: Double;
  private
    class var fPrecisionTimerInitialized: Boolean;
    class procedure InitPrecisionTimer;
  protected
    fStartTicks: Int64;
    fCurrentTicks: Int64;
    procedure InternalUpdateCurrentTicks;
    // Elapsed
    function GetElapsedTicks: Int64;
    function GetElapsedSeconds: Double;
    function GetElapsedMiliseconds: Int64;
    function GetElapsedMicroseconds: Int64;
    function GetElapsedNanoseconds: Int64;
    // Current
    function GetCurrentTicks: Int64;
    function GetCurrentSeconds: Double;
  public
    constructor Create;
    // Start ticks becomes 0, changing elapsed properties from system start
    procedure ResetToSystemStart;
    // Returns elapsed ticks
    function Restart: Int64;

    // Precision sleep. Parameter is how many 100 nanosecond intervals to sleep.
    class procedure SleepPrecise(const a100NanosecIntervalsCount: Int64);

    // Elapsed
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedSeconds: Double read GetElapsedSeconds;
    property ElapsedMiliseconds: Int64 read GetElapsedMiliseconds;
    property ElapsedMicroseconds: Int64 read GetElapsedMicroseconds;
    property ElapsedNanoseconds: Int64 read GetElapsedNanoseconds;
    // Current
    property CurrentTicks: Int64 read GetCurrentTicks;
    property CurrentSeconds: Double read GetCurrentSeconds;
  end;

implementation

var
  ulTimeDeviceCaps: TTimeCaps;
  ulTimePrecisionChanged: Boolean;
  ulWaitableTimerHandle: Winapi.Windows.THandle;

{ THighResTimer }

constructor THighResTimer.Create;
begin
  inherited Create;
  if (not QueryPerformanceFrequency(fTicksPerSecond)) or (not QueryPerformanceCounter(fCurrentTicks)) then
    raise Exception.CreateFmt(EXCEPTION_MESSAGE_TIMER_NOT_AVAILABLE, [Self.ClassName]);
  fStartTicks := fCurrentTicks;
  fInverseTicksPerMilisecond := 1000 / fTicksPerSecond;
  fInverseTicksPerMicrosecond := 1000000 / fTicksPerSecond;
  fInverseTicksPerNanosecond := 1000000000 / fTicksPerSecond;
end;

class procedure THighResTimer.InitPrecisionTimer;
begin
  ulWaitableTimerHandle := CreateWaitableTimer(nil, False, nil);
  if ulWaitableTimerHandle = 0 then
    Exit;
  if timeGetDevCaps(@ulTimeDeviceCaps, SizeOf(TTimeCaps)) = MMSYSERR_NOERROR then
    if ulTimeDeviceCaps.wPeriodMin <> PRECISION_TIMER_MINIMUM_RESOLUTION then
      if timeBeginPeriod(PRECISION_TIMER_MINIMUM_RESOLUTION) = MMSYSERR_NOERROR then
        ulTimePrecisionChanged := True;
  THighResTimer.fPrecisionTimerInitialized := True;
end;

procedure THighResTimer.InternalUpdateCurrentTicks;
begin
  QueryPerformanceCounter(fCurrentTicks);
end;

function THighResTimer.GetElapsedTicks: Int64;
begin
  InternalUpdateCurrentTicks;
  Result := fCurrentTicks - fStartTicks;
end;

function THighResTimer.GetElapsedSeconds: Double;
begin
  Result := GetElapsedTicks / fTicksPerSecond;
end;

function THighResTimer.GetElapsedMiliseconds: Int64;
begin
  Result := Floor(GetElapsedTicks * fInverseTicksPerMilisecond);
end;

function THighResTimer.GetElapsedMicroseconds: Int64;
begin
  Result := Floor(GetElapsedTicks * fInverseTicksPerMicrosecond);
end;

function THighResTimer.GetElapsedNanoseconds: Int64;
begin
  Result := Floor(GetElapsedTicks * fInverseTicksPerNanosecond);
end;

function THighResTimer.GetCurrentTicks: Int64;
begin
  InternalUpdateCurrentTicks;
  Result := fCurrentTicks;
end;

function THighResTimer.GetCurrentSeconds: Double;
begin
  Result := GetCurrentTicks / fTicksPerSecond;
end;

procedure THighResTimer.ResetToSystemStart;
begin
  fStartTicks := 0;
end;

function THighResTimer.Restart: Int64;
begin
  InternalUpdateCurrentTicks;
  Result := fCurrentTicks - fStartTicks;
  fStartTicks := fCurrentTicks;
end;

class procedure THighResTimer.SleepPrecise(const a100NanosecIntervalsCount: Int64);
 var
  lWaitInterval100ns: Int64;
begin
  if not THighResTimer.fPrecisionTimerInitialized then
    raise Exception.Create(EXCEPTION_MESSAGE_TIMER_NOT_INITIALIZED);
  lWaitInterval100ns := a100NanosecIntervalsCount * -1;
  if SetWaitableTimer(ulWaitableTimerHandle, lWaitInterval100ns, 0, nil, nil, False) then
    if WaitForSingleObject(ulWaitableTimerHandle, INFINITE) = WAIT_FAILED then
      raise Exception.Create(EXCEPTION_MESSAGE_TIMER_WAIT_FAILED);
end;

initialization
  // Initializing variables for precision sleep
  ulWaitableTimerHandle := 0;
  ulTimePrecisionChanged := False;
  // Initializing precision timer
  THighResTimer.fPrecisionTimerInitialized := False;
  THighResTimer.InitPrecisionTimer;

finalization
  // Restoring and cleaning
  if ulTimePrecisionChanged then
    timeEndPeriod(PRECISION_TIMER_MINIMUM_RESOLUTION);
  if ulWaitableTimerHandle <> 0 then
    CloseHandle(ulWaitableTimerHandle);
end.
