unit WatchFileSystem;

interface

uses
  Classes, Windows, SysUtils;

const
  FILE_LIST_DIRECTORY   = $0001; // Nicht in Delphi vorhanden also müssen wir die Konstante deklarieren

{ Typ der Informationen über die Veränderungen enthält }
type
  PInfoCallBack = ^TInfoCallBack;
  TInfoCallBack = record
    FAction      : Integer; // Aktion (Siehe Aktions-Liste)
    FDrive       : string;  // Laufwerk
    FOldFileName : string;  // Alter Dateiname ( Gefüllt bei der Aktion Rename )
    FNewFileName : string;  // Neuer Dateiname ( Standart bei den Aktionen ADD, REMOVE, MODIDIFY )
  end;  

{ Das ist unsere Call-Back-Funktion an die wir unsere Veränderungen schicken }
  TWatchFileSystemCallBack = procedure (pInfo: TInfoCallBack);

{ ?????? ??????????? ???????? ???????
  ????????:
  pName    - ??? ????? ??? ???????????
  pFilter  - ?????????? ???????? FILE_NOTIFY_XXX
  pSubTree - ?????????? ?? ??? ???????? ???????? ?????
  pInfoCallback - ????? callback ?????????, ?????????? ??? ????????? ? ???????? ???????}
procedure StartWatch(pName: string; pFilter: cardinal; pSubTree: boolean; pInfoCallback: TWatchFileSystemCallback);
procedure StopWatch;

{ FileNotify-Typ auf den der Notify-Puffer zeigt (Struktur für Puffer) }
type
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset : DWORD; // Offset vom nächsten Record
    Action          : DWORD; // Aktion (Siehe Aktions-Liste)
    FileNameLength  : DWORD; // Länge das Dateinamens
    FileName        : array [0..MAX_PATH - 1] of WideChar; // Dateiname (Buffer WideChar)
  end;                                                     // Umwandlung mit ->SetString<-
                                                           // oder WideCharToString
                                                           // (zu beachten ist das man den
                                                           // WideChar-Buffer als Pointer
                                                           // übergeben muss)

{ Das ist unsere Fehler-Klasse mit der wir unsere Fehler erzeugen }
{z.B. raise WFSError.Create('Fehler!'); }
type
  WFSError = class(Exception); // Fehler-Behandlungs-Klasse

{ Unsere eigene Klasse der Thread }
type
  TWatchFileSystem = class(TThread)
  private
    FName           : string;                  // Dateiname (Verzeichnis/Datei)
    FFilter         : Cardinal;                // Notify-Filter
    FSubTree        : boolean;                 // Unterverzeichnisse überwachen
    FInfoCallBack   : TWatchFileSystemCallBack;
    FWatchHandle    : THandle;                 // Handle des Laufwerks/Ordners
    FWatchBuf       : array[0..4096] of Byte;  // Notify-Buffer
    FOverLapp       : TOverlapped;             // Overlapped-Struktur
    FPOverLapp      : POverlapped;             // Overlapped-Struktur
    FBytesWritte    : DWORD;                   // Anzahl der Bytes die in den Buffer geschrieben wurden sind
    FCompletionPort : THandle;                 // Handle vom Completion-Port
    FNumBytes       : Cardinal;                // Bytes gelesen
    FOldFileName    : string;                  // Temporär-Variable zum zwischenspeichern des alten Datei-Namen

    function CreateDirHandle(aDir: string): THandle;
    procedure WatchEvent;
    procedure HandleEvent;
    { Private-Deklarationen } 
  protected
    procedure Execute; override;
  public 
    constructor Create(pName: string; pFilter: cardinal; pSubTree: boolean; pInfoCallBack: TWatchFileSystemCallBack); 
    destructor Destroy; override;
  end;

var
  WFS : TWatchFileSystem;

implementation

procedure StartWatch(pName: string; pFilter: cardinal; pSubTree: boolean; pInfoCallback: TWatchFileSystemCallback);
begin
  WFS := TWatchFileSystem.Create(pName, pFilter, pSubTree, pInfoCallback);
end;

procedure StopWatch;
var
  Temp : TWatchFileSystem;
begin
  if Assigned(WFS) then
  begin
   PostQueuedCompletionStatus(WFS.FCompletionPort, 0, 0, nil);
   Temp := WFS;
   WFS:=nil;
   Temp.Terminate;
  end;
end;

function TWatchFileSystem.CreateDirHandle(aDir: string): THandle;
begin 
  Result:=CreateFile(
    PChar(aDir),                                         // Zeiger auf Dateiname 
    FILE_LIST_DIRECTORY,                                 // Zugriffsmethode (Lesen/Schreiben)
    FILE_SHARE_READ+FILE_SHARE_DELETE+FILE_SHARE_WRITE,  // Handle-Modi 
    nil, 
    OPEN_EXISTING,                                       // Wie soll erstellt werden
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,  // Datei-Attribute
    0 
  ); 
end;

procedure TWatchFileSystem.WatchEvent;
var
  CompletionKey: Cardinal;
begin 
  { Input/Output - Completion-Port mit dem Verzeichnis-Handle verbinden }
  FCompletionPort:=CreateIoCompletionPort(FWatchHandle, 0, Longint(pointer(self)), 0); 

  { Verzeichnis auf Veränderungen prüfen }
  if not ReadDirectoryChanges(FWatchHandle, @FWatchBuf, SizeOf(FWatchBuf), FSubTree, 
    FFilter, @FBytesWritte,  @FOverLapp, nil) then 
  begin 
  { Letzte Fehlermeldung ausgeben } 
    raise WFSError.Create(SysErrorMessage(GetLastError));
    Terminate; // Thread beenden 
  end else 
  begin 
    while not Terminated do // Solange nach Veränderungen suchen bis Thread beendet wird 
    begin 
    { Auf System-Mitteilung warten (warten bis was verändert wurde) } 
      GetQueuedCompletionStatus(FCompletionPort, FNumBytes, CompletionKey, FPOverLapp, INFINITE); 
    { Wenn etwas verändert wurde dann ist der CompletionKey <> 0 und wir hohlen 
       uns mit ReadDirectoryChanges Informationen darüber } 
      if CompletionKey<>0 then 
      begin 
        Synchronize(HandleEvent); // Veränderungen auslesen (Informationen abarbeite) 

        ZeroMemory(@FWatchBuf, SizeOf(FWatchBuf)); 
        FBytesWritte:=0; 

        ReadDirectoryChanges( 
        FWatchHandle,         // Handle von einem Verzeichnis
        @FWatchBuf,           // Zeiger auf Buffer 
        SizeOf(FWatchBuf),    // Größe des Buffers 
        FSubTree,             // Unterverzeichnisse überwachen 
        FFilter,              // Notify-Filter
        @FBytesWritte,        // Bytes in Buffer geschrieben 
        @FOverLapp,           // Zeiger auf Overlapped Struktur 
        nil); 
      end else // if CompletionKey 
        Terminate; 

    end; // while not Terminated 
  end; // if not ReadDirectoryChanges 
end;

procedure TWatchFileSystem.HandleEvent;
var
  FileNotifyInfo : PFileNotifyInformation; 
  InfoCallBack   : TInfoCallBack; 
  Offset         : Longint; 
begin 
  Pointer(FileNotifyInfo) := @FWatchBuf[0]; // FileOpNotification mit Daten aus dem Buffer füllen 
  repeat 
    Offset:=FileNotifyInfo^.NextEntryOffset; // Offset vom nächsten Record 

  { Informationen im CallBackInfo-Record speichern } 
    InfoCallBack.FAction:=FileNotifyInfo^.Action; 
    InfoCallBack.FDrive:=FName; // Laufwerksbuchstabe 

    SetString( 
      InfoCallBack.FNewFileName,         // Zeiger auf String der mit Daten gefüllt werden soll 
      FileNotifyInfo^.FileName,       // Übergabe von Buffer (in dem Fall -> Array[0..0] of WideChar) 
      FileNotifyInfo^.FileNameLength  // Länge das Buffers (in dem Fall der Datei-Name) 
    ); 
    InfoCallBack.FNewFileName:=Trim(InfoCallBack.FNewFileName); // Alle am Anfang und am Ende vorhandenen Steuerzeichen entfernen 

    case FileNotifyInfo^.Action of 
      4: FOldFileName:=Trim(WideCharToString(@(FileNotifyInfo^.FileName[0]))); 
      5: InfoCallBack.FOldFileName:=FOldFileName; 
    end; 

    FInfoCallBack(InfoCallBack); // CallBack-Funktion aufrufen 

    PChar(FileNotifyInfo):=PChar(FileNotifyInfo)+Offset; // Auf nächsten Record zeigen 
  until Offset=0;
end;

constructor TWatchFileSystem.Create(pName: string; pFilter: cardinal; pSubTree: boolean; pInfoCallBack: TWatchFileSystemCallBack);
begin 
  inherited Create(True); 

  FName:=IncludeTrailingBackslash(pName); // immer Backslash hintendran hängen!
  FFilter:=pFilter;
  FSubTree:=pSubTree;

  FOldFileName:=EmptyStr;

  ZeroMemory(@FOverLapp, SizeOf(TOverLapped));
  FPOverLapp:=@FOverLapp;
  FInfoCallBack:=pInfoCallBack;

  FreeOnTerminate:=True;
  Resume; // Thread ausführen
end;

destructor TWatchFileSystem.Destroy;
begin 
  inherited Destroy; 

  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil); 
  CloseHandle(FWatchHandle); 
  FWatchHandle:=0; 
  CloseHandle(FCompletionPort);
  FCompletionPort:=0; 
end;

procedure TWatchFileSystem.Execute; 
begin 
{ Handle vom Verzeichnis bekommen } 
  FWatchHandle:=CreateDirHandle(FName); 
{ Funktion aufrufen die die Veränderungen Aufspürt und abarbeitet } 
  WatchEvent; // Ausschau halten 
end;

{
Konstanten für die Funktion ReadDirectoryChangesW 
(Parameter: dwNotifyFilter) 

***************************** NOTIFY-FILTER-LISTE ****************************** 
*                                                                              * 
* Value:                          Meaning:                                     * 
* FILE_NOTIFY_CHANGE_FILE_NAME    Any file name change in the watched          * 
*                                 directory or subtree causes a change         * 
*                                 notification wait operation to return.       * 
*                                 Changes include renaming, creating,          * 
*                                 or deleting a file.                          * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_DIR_NAME     Any directory-name change in the watched     * 
*                                 directory or subtree causes a change         * 
*                                 notification wait operation to return.       * 
*                                 Changes include creating or deleting         * 
*                                 a directory.                                 * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_ATTRIBUTES   Any attribute change in the watched          * 
*                                 directory or subtree causes a change         * 
*                                 notification wait operation to return.       * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_SIZE         Any file-size change in the watched          * 
*                                 directory or subtree causes a change         * 
*                                 notification wait operation to return.       * 
*                                 The operating system detects a change in     * 
*                                 file size only when the file is written to   * 
*                                 the disk. For operating systems that use     * 
*                                 extensive caching, detection occurs only     * 
*                                 when the cache is sufficiently flushed.      * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_LAST_WRITE   Any change to the last write-time of files   * 
*                                 in the watched directory or subtree causes   * 
*                                 a change notification wait operation to      * 
*                                 return. The operating system detects a       * 
*                                 change to the last write-time only when the  * 
*                                 file is written to the disk.                 * 
*                                 For operating systems that use extensive     * 
*                                 caching, detection occurs only when the      * 
*                                 cache is sufficiently flushed.               * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_LAST_ACCESS  Any change to the last access time of files  * 
*                                 in the watched directory or subtree causes   * 
*                                 a change notification wait operation         * 
*                                 to return.                                   * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_CREATION     Any change to the creation time of files in  * 
*                                 the watched directory or subtree causes a    * 
*                                 change notification wait operation           * 
*                                 to return.                                   * 
*                                                                              * 
* FILE_NOTIFY_CHANGE_SECURITY     Any security-descriptor change in the        * 
*                                 watched directory or subtree causes a        * 
*                                 change notification wait operation           * 
*                                 to return.                                   * 
*                                                                              * 
******************************************************************************** 

Und noch ein paar Konstanten für den Typ TFileNotifyInformation 
(Variable: Action (DWORD) ) 

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
}

end.
