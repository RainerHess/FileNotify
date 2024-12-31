unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, FileUtil, ToolEdit, Db, RxMemDS, Grids, DBGrids, RXDBCtrl;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    DirectoryEdit1: TDirectoryEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    DataSource: TDataSource;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData: TRxMemoryData;
    RxMemoryDataAktion: TIntegerField;
    RxMemoryDataZeitstempel: TDateTimeField;
    RxMemoryDataAktionsname: TStringField;
    RxMemoryDataAktionstext: TStringField;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  WatchFileSystem;

{ CallBack-Funktion für WatchFileSystem }
procedure MyInfoCallBack(pInfo: TInfoCallBack);
const
  Action: array[1..3] of String = ('%d: %s Hinzugefügt -> %s', '%d: %s Gelöscht    -> %s', '%d: %s Modifiziert -> %s');
  ActionDB: array[1..3] of String = ('Hinzugefügt', 'Gelöscht', 'Modifiziert');
var
  timestr:       string;
begin
   timestr := DateTimeToStr(now) + ' ';

  case pInfo.FAction of
    FILE_ACTION_ADDED, FILE_ACTION_REMOVED, FILE_ACTION_MODIFIED:
      begin
        Form1.Memo1.Lines.Add(Format(Action[pInfo.Faction], [pInfo.Faction, timestr, pInfo.FDrive+pInfo.FNewFileName]));

        with Form1.RxMemoryData do begin
          Append;
          FieldByName('Aktion').AsInteger := pInfo.Faction;
          FieldByName('Zeitstempel').AsDateTime := now;
          FieldByName('Aktionsname').AsString := ActionDB[pInfo.Faction];
          FieldByName('Aktionstext').AsString := pInfo.FDrive+pInfo.FNewFileName;
          Post;
        end;
      end;

    //FILE_ACTION_RENAMED_OLD_NAME,
    FILE_ACTION_RENAMED_NEW_NAME:
      begin
        Form1.Memo1.Lines.Add(Format('%d: %s Umbenannt   -> %s in %s', [pInfo.Faction, timestr, pInfo.FDrive+pInfo.FOldFileName,pInfo.FDrive+pInfo.FNewFileName]));

        with Form1.RxMemoryData do begin
          Append;
          FieldByName('Aktion').AsInteger := pInfo.Faction;
          FieldByName('Zeitstempel').AsDateTime := now;
          FieldByName('Aktionsname').AsString := 'Umbenannt';
          FieldByName('Aktionstext').AsString := pInfo.FDrive+pInfo.FOldFileName + ' in ' + pInfo.FDrive+pInfo.FNewFileName;
          Post;
        end;
      end;
  else
    if(pInfo.FAction < 4) then
      Form1.Memo1.Lines.Add(Format(Action[pInfo.Faction], [pInfo.FDrive+pInfo.FNewFileName]));
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  StopWatch;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  watch = 'C:\unzipped'; //'\\M46348\f\dh';
begin
  DirectoryEdit1.InitialDir := watch;
  DirectoryEdit1.Text := watch;
end;

procedure TForm1.DirectoryEdit1Change(Sender: TObject);
begin
  StopWatch;
  if(DirExists(DirectoryEdit1.Text)) then begin
    StartWatch(DirectoryEdit1.Text, FILE_NOTIFY_CHANGE_FILE_NAME + FILE_NOTIFY_CHANGE_DIR_NAME + FILE_NOTIFY_CHANGE_SIZE, CheckBox1.Checked, @MyInfoCallBack);
    Memo1.Lines.Add('Verzeichnis: ' + DirectoryEdit1.Text);
  end
  else
    Memo1.Lines.Add('Verzeichnis: "' + DirectoryEdit1.Text + '" existiert nicht! Keine Überwachung...');
end;

end.
