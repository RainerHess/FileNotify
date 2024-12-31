unit FileNotify;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils;

{ TODO -oRH  -cErweiterung : Wenn ein ganzer Ordner in den Mülleimer verschoben wird muss noch getestet werden bzw. eine Lösung finden wenn man wissen
                             möchte welche Dateien waren in dem Verzeichnis enthalten. Derzeit hat man nur die Info des gelöschten Verzeichnisses}

(*
  Ursprungs-Vorlagen: https://www.delphipraxis.net/70188-ordner-ueberwachen-2.html aus Delphi-Praxis Version 1.2
                      von JUAN CARLOS MOLINOS MESA und die alte WatchFileSystem.pas von 2006

  Geändert: 28.-30.11.2024, 02.12.2024, 14-17/19.12.2024

  Noch etwas zu dem Thema:
  https://github.com/Wosi/DirectoryWatcher
  https://github.com/bero/DirMonitorDemo. Habe ich erst am 14.12.2024 gefunden
  https://stackoverflow.com/questions/71160299/where-can-i-find-system-io-to-use-filesystemwatcher-on-delphi-11

  Version 1.4: Anpassungen, Texte etc. 16.12.2024
  Version 1.3: Änderungen/Anpassungen für Delphi 12: 28.11.2024
               Generelle Anpassungen, Datentypen, Puffer, Infos etc.: 29.11.2024

  Konstanten für die Funktion ReadDirectoryChangesW => Parameter: dwNotifyFilter

  ********************************************* NOTIFY-FILTER-LISTE *********************************************
  *                                                                                                             *
  * Value:                          Meaning:                                                                    *
  * FILE_NOTIFY_CHANGE_FILE_NAME    Any file name change in the watched directory or subtree causes a change    *
  *                                 notification wait operation to return. Changes include renaming, creating,  *
  *                                 or deleting a file.                                                         *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_DIR_NAME     Any directory-name change in the watched directory or subtree causes a      *
  *                                 change notification wait operation to return. Changes include creating or   *
  *                                 deleting a directory.                                                       *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_ATTRIBUTES   Any attribute change in the watched directory or subtree causes a change    *
  *                                 notification wait operation to return.                                      *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_SIZE         Any file-size change in the watched directory or subtree causes a change    *
  *                                 notification wait operation to return. The operating system detects a       *
  *                                 change in file size only when the file is written to the disk. For          *
  *                                 operating systems that use extensive caching, detection occurs only         *
  *                                 when the cache is sufficiently flushed.                                     *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_LAST_WRITE   Any change to the last write-time of files in the watched directory or      *
  *                                 subtree causes a change notification wait operation to return. The          *
  *                                 operating system detects a change to the last write-time only when the      *
  *                                 file is written to the disk. For operating systems that use extensive       *
  *                                 caching, detection occurs only when the cache is sufficiently flushed.      *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_LAST_ACCESS  Any change to the last access time of files in the watched directory or     *
  *                                 subtree causes a change notification wait operation to return.              *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_CREATION     Any change to the creation time of files in the watched directory or        *
  *                                 subtree causes a change notification wait operation to return.              *
  *                                                                                                             *
  * FILE_NOTIFY_CHANGE_SECURITY     Any security-descriptor change in the watched directory or  subtree         *
  *                                 causes a change notification wait operation to return.                      *
  *                                                                                                             *
  ***************************************************************************************************************

  Konstanten für den Typ TFileNotifyInformation => Variable: NotifyAction (DWORD)

  ******************************* AKTIONS-LISTE **********************************
  *                                                                              *
  * Value:                        Meaning:                                       *
  * FILE_ACTION_ADDED             The file was added to the directory.           *
  * FILE_ACTION_REMOVED           The file was removed from the directory.       *
  * FILE_ACTION_MODIFIED          The file was modified. This can be a change    *
  *                               in the time stamp or attributes.               *
  * FILE_ACTION_RENAMED_OLD_NAME  The file was renamed and this is the old name. *
  * FILE_ACTION_RENAMED_NEW_NAME  The file was renamed and this is the new name. *
  *                                                                              *
  ********************************************************************************
*)

type
  TFileNotifyThread = class;   // Forward-Deklaration um die Klasse in TFileNotify nutzen zu können, weil sie dort noch nicht bekannt ist

  // Infos: https://learn.microsoft.com/de-de/windows/win32/api/winnt/ns-winnt-file_notify_information
  // =================================================================================================
  // FileNameLength:  Die Größe des Dateinamenteils des Datensatzes in Bytes. Beachten Sie, dass dieser Wert das beendende NULL-Zeichen nicht enthält.
  //
  // FileName[1]:    Ein Feld mit variabler Länge, das den Dateinamen relativ zum Verzeichnishandle enthält. Der Dateiname hat das
  //                 Unicode-Zeichenformat und ist nicht null-beendet. Wenn sowohl ein kurzer als auch ein langer Name für die Datei
  //                 vorhanden ist, gibt die Funktion einen dieser Namen zurück, aber es ist nicht angegeben welcher Name.

  PFileNotifyInformation = ^TFileNotifyInformation;      // FILE_NOTIFY_INFORMATION Struktur (Struktur für Puffer)
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;                              // Offset vom nächsten Record
    NotifyAction:    DWORD;                              // Aktion (Siehe Aktions-Liste)
    FileNameLength:  DWORD;                              // Länge das Dateinamens
    FileName:        array [0..1] of WideChar;           // Umwandlung mit WideCharToString (zu beachten ist das man den WideChar-Buffer als Zeiger (Pointer) übergeben muss)
  //FileName:        array [0..MAX_PATH] of WideChar;    // Geändert zum Test. 16.12.2024
  end;

  TFilters = (fnFILE_NAME, fnDIR_NAME, fnATTRIBUTES, fnSIZE, fnLAST_WRITE, fnLAST_ACCESS, fnCREATION, fnSECURITY);
  TFilter  = set of TFilters;

  TNotifyAction = (faADDED, faREMOVED, faMODIFIED, faRENAMED_OLD_NAME, faRENAMED_NEW_NAME);
  TNotifyFilter = set of TNotifyAction;

  TEventChange = procedure (Sender: TObject; NotifyAction: TNotifyAction; NotifyFileName: string) of object;

  TFileNotify = class(TComponent)
  private
    { Private declarations }
    procedure DeactivateAndCloseHandles;
  protected
    { Protected declarations }
    FWatchDir: string;
    ProcessFileNotify: TFileNotifyThread;
    FEventChange: TEventChange;
    FActive: Boolean;
    procedure FActiveDirMonitor(Value: boolean);
    procedure Loaded; override;
  public
    { Public declarations }
    FCompletionPort: Cardinal;                        // Handle vom Completion-Port
    FOverlapped: TOverlapped;                         // Overlapped-Struktur
    FPOverlapped: POverlapped;                        // Overlapped-Struktur (Zeiger)
    FBytesWrite: DWORD;                               // Anzahl der Bytes die in den Buffer geschrieben wurden
    FNotificationBuffer: array[0..65534] of Byte;     // Buffer muss kleiner 64 KB sein (wäre maximal 65535). Von 0 aus gerechnet 65534. Siehe Microsoft-Dokumentation.
    FHandle: THandle;                                 // Handle vom Completion-Port
    FWatchSubtree: Boolean;                           // Unterverzeichnisse durchsuchen
    FNotifyFilter: DWORD;                             // Welche Filter wurden gesetzt
    FActionFilterFlag: TNotifyFilter;                 // Über welche Aktionen will man Informiert werden
    FNotifyFilterFlag: TFilter;                       // Auf Informationen der Aktionen filtern
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyEvent;
  published
    { Published declarations }
    property WatchDirectory: string read FWatchDir write FWatchDir;
    property WatchSubtree: boolean read FWatchSubtree write FWatchSubtree default FALSE;
    property FilterAction: TNotifyFilter read FActionFilterFlag write FActionFilterFlag default [faADDED, faREMOVED, faMODIFIED, faRENAMED_OLD_NAME, faRENAMED_NEW_NAME]; // Default ist nur Optik im Designer (Fettschrift)
    property FilterNotification: TFilter read FNotifyFilterFlag write FNotifyFilterFlag default [fnFILE_NAME, fnDIR_NAME, fnSIZE, fnLAST_WRITE];
    property OnChange: TEventChange read FEventChange write FEventChange;
    property Active: boolean read FActive write FActiveDirMonitor default FALSE;
  end;

  { -- TFileNotifyThread -- }

  TFileNotifyThread = class(TThread)
  private
    { Private declarations }
  protected
    FNotifyRef: TComponent;               // Sicht auf unsere TFileNotify aus der Hauptanwendung
    procedure Execute; override;
    procedure SendEvent;
  public
    constructor Create(NotifyRef: TComponent); overload;
  end;

procedure Register;

implementation

{$R FileNotify.dcr}

{ --- TFileNotify --- }

constructor TFileNotify.Create(Owner: TComponent);
begin
  inherited;
  FPOverlapped := @FOverlapped;
  FCompletionPort := 0;
  FWatchDir := 'C:\';
  FNotifyFilterFlag := [fnFILE_NAME, fnDIR_NAME, fnSIZE, fnLAST_WRITE];                             // Standard-Einstellungen wie beim TRxFolderMonitor aus der rxLib - 30.11.2024
  FActionFilterFlag := [faADDED, faREMOVED, faMODIFIED, faRENAMED_OLD_NAME, faRENAMED_NEW_NAME];
  ProcessFileNotify := TFileNotifyThread.Create(Self);
  ProcessFileNotify.SetFreeOnTerminate(TRUE);
end;

destructor TFileNotify.Destroy;
begin
  ProcessFileNotify.Terminate;                  // Thread zur Terminierung auffordern.
  if not FActive then ProcessFileNotify.Free;   // Wird automatisch freigegeben wenn der FileNotifyThread aktiv war. Setzt man es ohne if versucht .Free es nochmals und es dauert ca. 12-14. Sekunden bis das Programm komplett beendet
  DeactivateAndCloseHandles;
  inherited;
end;

procedure TFileNotify.DeactivateAndCloseHandles;
begin
  if(FActive) then begin
    // https://learn.microsoft.com/de-de/windows/win32/api/ioapiset/nf-ioapiset-postqueuedcompletionstatus
    if not PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil) then
      RaiseLastOSError;

    CloseHandle(FCompletionPort); FCompletionPort := 0;
    CloseHandle(FHandle);         FHandle := 0;
  end;
  FActive := FALSE;
end;

procedure TFileNotify.Loaded;
begin
  inherited;
  if FActive then FActiveDirMonitor(TRUE);
end;

procedure TFileNotify.NotifyEvent;
var
  FileOpNotification: PFileNotifyInformation;
  Offset:             DWORD;
  FileActions, len:   integer;
  NotifyDateiname:    string;                     // eg. unicodestring: https://docwiki.embarcadero.com/RADStudio/Athens/de/String-Typen_(Delphi)
  NotifyAction:       TNotifyAction;
begin
  FileOpNotification := @FNotificationBuffer;

  repeat
    Offset := FileOpNotification^.NextEntryOffset;
    FileActions := FileOpNotification^.NotifyAction;
                                                                                // https://docwiki.embarcadero.com/Libraries/Athens/de/System.WideChar
    len := FileOpNotification^.FileNameLength div 2;                            // FileOpNotification^.FileNameLength ist doppelt so groß. WideChar-Werte sind auf Word-Größe (16 Bit) ausgerichtete Zeichen
    NotifyDateiname := WideCharLenToString(@FileOpNotification^.FileName, len); // WideCharLenToString konvertiert SourceLen WideString-Zeichen aus dem in Source angegebenen Puffer in einen UnicodeString.

    if(DebugHook <> 0) then OutputDebugString(LPCWSTR('TFileNotify.NotifyEvent: "' + NotifyDateiname + '" FileNameLength: ' + len.ToString + '/' + Length(NotifyDateiname).ToString));

    PByte(FileOpNotification) := PByte(FileOpNotification) + Offset;

    case FileActions of
      FILE_ACTION_ADDED:            NotifyAction := faADDED;
      FILE_ACTION_REMOVED:          NotifyAction := faREMOVED;
      FILE_ACTION_MODIFIED:         NotifyAction := faMODIFIED;
      FILE_ACTION_RENAMED_OLD_NAME: NotifyAction := faRENAMED_OLD_NAME;
      FILE_ACTION_RENAMED_NEW_NAME: NotifyAction := faRENAMED_NEW_NAME;
      else                          NotifyAction := TNotifyAction(-1);
    end;

    if NotifyAction in FActionFilterFlag then begin
      if(Assigned(FEventChange)) and (Length(NotifyDateiname) > 0) then FEventChange(Self, NotifyAction, NotifyDateiname);
    end;
  until Offset = 0;
end;

procedure TFileNotify.FActiveDirMonitor(Value: boolean);
var
  ret:  boolean;
begin
  if not (csDesigning in Self.ComponentState) then begin
    // Ausführungsmodus. Wurde TRUE übergeben aber nicht aktiv, aktivieren...
    if((Value) and (not FActive)) then begin
      FActive := TRUE;

      FNotifyFilter := 0;
      if fnFILE_NAME   in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
      if fnDIR_NAME    in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_DIR_NAME;
      if fnATTRIBUTES  in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
      if fnSIZE        in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_SIZE;
      if fnLAST_WRITE  in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_WRITE;
      if fnLAST_ACCESS in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_ACCESS;
      if fnCREATION    in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_CREATION;
      if fnSECURITY    in FNotifyFilterFlag then FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_SECURITY;

      // https://learn.microsoft.com/de-de/windows/win32/api/fileapi/nf-fileapi-createfilew
      // https://learn.microsoft.com/de-de/windows/win32/secauthz/generic-access-rights
      FHandle := CreateFile(
                   LPCWSTR(FWatchDir),                                           // Zeiger auf Dateiname
                   GENERIC_READ or GENERIC_WRITE,                                // Zugriffsmethode (Lesen/Schreiben)
                   FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,     // Handle-Modi
                   nil,
                   OPEN_EXISTING,                                                // Wie soll erstellt werden
                   FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,           // Datei-Attribute
                   0);

      if DWORD(FHandle) = INVALID_HANDLE_VALUE then begin
        RaiseLastOSError;          // old: raise EInvalidOperation.Create('Directory not found'); // 'No se ha encontrado el directorio');
        exit;
      end;

      // https://learn.microsoft.com/de-de/windows/win32/api/ioapiset/nf-ioapiset-createiocompletionport
      // Input/Output - Completion-Port mit dem Verzeichnis-Handle verbinden
      FCompletionPort := CreateIoCompletionPort(FHandle, 0, ULONG_PTR(Self), 0);  // Geändert in ULONG_PTR 28.11.2024 => war: ...Longint(pointer(self)), 0);

      if Pointer(FCompletionPort) = nil then begin
        RaiseLastOSError;          // old: raise Exception.Create('Error in "CreateIoCompletionPort"');
        exit;
      end;

      ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
      // https://learn.microsoft.com/de-de/windows/win32/api/winbase/nf-winbase-readdirectorychangesw
      //
      // Ausschnit aus der Doku:
      // ReadDirectoryChangesW schlägt mit ERROR_INVALID_PARAMETER fehl, wenn die Pufferlänge größer als 64 KB ist und die Anwendung ein Verzeichnis
      // über das Netzwerk überwacht. Dies ist auf eine Paketgrößeseinschränkung mit den zugrunde liegenden Dateifreigabeprotokollen zurückzuführen.
      //
      // https://www.delphipraxis.net/173627-verzeichnis-ueberwachen-readdirectorychangesw.html
      ret := ReadDirectoryChanges(
               FHandle,                      // Handle von einem Verzeichnis
               @FNotificationBuffer,         // Zeiger auf Buffer
               SizeOf(FNotificationBuffer),  // Größe des Buffers
               FWatchSubtree,                // Unterverzeichnisse überwachen
               FNotifyFilter,                // Notify-Filter
               @FBytesWrite,                 // Bytes in Buffer geschrieben
               @FOverlapped,                 // Zeiger auf Overlapped Struktur
               nil);                         // Completion-Routine

      if not ret then begin
        RaiseLastOSError;          // old: raise Exception.Create('Error in "ReadDirectoryChanges"');
        exit;
      end;

      if(DebugHook <> 0) then  OutputDebugString(LPCWSTR('TFileNotify.FActiveDirMonitor - FActive: Start ('+ FActive.ToString + ')'));
      // https://stackoverflow.com/questions/1418333/tthread-resume-is-deprecated-in-delphi-2010-what-should-be-used-in-place
      ProcessFileNotify.Suspended := FALSE;    // Änderung: 28.11.2024 => War: ProcessFileNotify.Resume (Thread ausführen - ist veraltet).
    end
    else begin
      // wurde FALSE übergeben aber noch aktiv, stoppen...
      if((not Value) and (FActive)) then begin
        DeactivateAndCloseHandles;
        if(DebugHook <> 0) then OutputDebugString(LPCWSTR('TFileNotify.FActiveDirMonitor - FActive: Stop ('+ FActive.ToString + ')'));
        ProcessFileNotify.Suspended := TRUE;   // Änderung: 28.11.2024 => War: ProcessFileNotify.Suspend;
      end;
    end;
  end
  else
    FActive := Value; // Zeichenweise (modo diseño)
end;

{ --- TFileNotifyThread --- }

constructor TFileNotifyThread.Create(NotifyRef: TComponent);
begin
  inherited Create(TRUE);    // Aus der Doku: Wenn CreateSuspended den Wert FALSE hat, wird Execute sofort aufgerufen. Andernfalls erfolgt der Aufruf erst nach einem Aufruf von Resume.
  FNotifyRef := NotifyRef;   // Sicht auf TFileNotify aus der Hauptanwendung
  Priority   := tpLowest;    // tpHigher; // tpTimeCritical;       { TODO -oRH -cErweiterung : Noch prüfen, welche Priorität sinnvoll ist, evtl. auch im Designer auswählbar machen. }
end;

procedure TFileNotifyThread.SendEvent;
var
  Notify: TFileNotify;
begin
  Notify := FNotifyRef as TFileNotify;
  Notify.NotifyEvent;
end;

procedure TFileNotifyThread.Execute;
var
  state:    NativeUInt;
  Notify:   TFileNotify;
  numBytes: DWORD;
begin
  Notify := TFileNotify(FNotifyRef);

  // Solange nach Veränderungen suchen bis Thread beendet wird
  while not Terminated do begin
    if(DebugHook <> 0) then OutputDebugString('TFileNotifyThread.Execute');

    // Auf System-Mitteilung warten (warten bis was verändert wurde)
    // https://learn.microsoft.com/de-de/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus
    GetQueuedCompletionStatus(Notify.FCompletionPort, numBytes, state, Notify.FPOverlapped, INFINITE);

    // Wenn etwas verändert wurde Notify.FPOverlapped <> nil, besorgen wir uns mit ReadDirectoryChanges die Informationen
    if (Notify.FPOverlapped <> nil) then begin
      if not Terminated then begin
        Synchronize(SendEvent);                // Veränderungen auslesen (Informationen abarbeiten)

        ZeroMemory(@Notify.FNotificationBuffer, SizeOf(Notify.FNotificationBuffer));

        // https://learn.microsoft.com/de-de/windows/win32/api/winbase/nf-winbase-readdirectorychangesw
        ReadDirectoryChanges(
          Notify.FHandle,                      // Handle von einem Verzeichnis
          @Notify.FNotificationBuffer,         // Zeiger auf Buffer
          SizeOf(Notify.FNotificationBuffer),  // Größe des Buffers
          Notify.FWatchSubtree,                // Unterverzeichnisse überwachen
          Notify.FNotifyFilter,                // Notify-Filter
          @Notify.FBytesWrite,                 // Bytes in Buffer geschrieben
          @Notify.FOverlapped,                 // Zeiger auf Overlapped Struktur
          nil);

         // OutputDebugString(pchar('TFileNotify.FNotificationBuffer: ' + SizeOf(Notify.FNotificationBuffer).ToString));
      end;
    end;
  end;
end;

{ --- Klasse registrieren  --- }

procedure Register;
begin
  RegisterComponents('System', [TFileNotify]);
end;

end.
