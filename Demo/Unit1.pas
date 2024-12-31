unit Unit1;

interface

uses
  Winapi.Windows, Vcl.Forms, Vcl.Dialogs, Vcl.Mask, Vcl.StdCtrls, Vcl.Controls, System.Classes, System.SysUtils, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, FireDAC.Stan.StorageJSON, Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, Vcl.ExtCtrls, Vcl.FileCtrl, FileNotify;

type
  TForm1 = class(TForm)
    EditEvents: TEdit;
    Label1: TLabel;
    ButtonClear: TButton;
    CheckBoxActive: TCheckBox;
    Label2: TLabel;
    Memo1: TMemo;
    ButtonSave: TButton;
    CheckBoxRekursiv: TCheckBox;
    DBGrid1: TDBGrid;
    FDMemTable: TFDMemTable;
    FDMemTableAktion: TIntegerField;
    FDMemTableZeitstempel: TDateTimeField;
    FDMemTableAktionsname: TStringField;
    FDMemTableAktionstext: TStringField;
    DataSource: TDataSource;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDMemTableEventNummer: TIntegerField;
    VirtualImageList: TVirtualImageList;
    ImageCollection: TImageCollection;
    DirectoryEdit1: TButtonedEdit;
    FileOpenDialog1: TFileOpenDialog;
    FDMemTablePruefsumme: TStringField;
    FileNotify1: TFileNotify;
    FDMemTableCounter: TIntegerField;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure CheckBoxActiveClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckBoxRekursivClick(Sender: TObject);
    procedure DirectoryEdit1RightButtonClick(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure FileNotify1Change(Sender: TObject; NotifyAction: TNotifyAction; NotifyFileName: string);
  private
    { Private declarations }
  public
    { Public declarations }
    events_counter: integer;
  end;

var
  Form1: TForm1;

implementation

uses
  System.Hash, crc32;

{$R *.dfm}

procedure TForm1.FileNotify1Change(Sender: TObject; NotifyAction: TNotifyAction; NotifyFileName: string);
var
  aktion, datei:  string;
  counter:        integer;
begin
  inc(events_counter);
  EditEvents.Text := events_counter.ToString;
  aktion := '';

  case NotifyAction of
    faADDED:            aktion := 'Hinzugefügt';
    faREMOVED:          aktion := 'Gelöscht';
    faMODIFIED:         aktion := 'Modifiziert';
    faRENAMED_OLD_NAME: aktion := 'Umbenannt (alt)';
    faRENAMED_NEW_NAME: aktion := 'Umbenannt (neu)';
  end;

  Memo1.Lines.Append(FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now) + format(' %16s -> ', [aktion]) +  NotifyFileName);

  with FDMemTable do begin
    Append;
    FieldByName('Aktion').AsInteger := integer(TNotifyAction(NotifyAction));
    FieldByName('Zeitstempel').AsDateTime := Now;
    FieldByName('Aktionsname').AsString := aktion;
    FieldByName('Aktionstext').AsString := NotifyFileName;
    FieldByName('EventNummer').AsString := EditEvents.Text;

    counter := 2; // 5;    // Anzahl der Versuche wenn man keinen Dateizugriff bekommt.

    case NotifyAction of
      // Prüfsumme ermitteln wenn Datei nicht gelöscht wurde.
      faADDED, faMODIFIED, faRENAMED_NEW_NAME:
        begin
          datei := IncludeTrailingPathDelimiter(FileNotify1.WatchDirectory) + FieldByName('Aktionstext').AsString;

          while boolean(counter) do begin
            try
              // Datei existiert? Prüfsumme kann ermittelt werden, wenn nicht evtl. ein Verzeichnis?
              if FileExists(datei) then
                FieldByName('Pruefsumme').AsString := CalcCRC32(datei)
                // FieldByName('Pruefsumme').AsString := THashMD5.GetHashStringFromFile(datei)
                // FieldByName('Pruefsumme').AsString := THashSHA1.GetHashStringFromFile(datei)
              else
                FieldByName('Pruefsumme').AsString := 'Directory';
              break;
            except
              Memo1.Lines.Append('Zugriffsfehler: ' + datei + ' (' + counter.ToString + ')');
              FieldByName('Pruefsumme').AsString := 'No Access';
              break;
            end;

            if(counter > 0) then begin
              if(DebugHook <> 0) then OutputDebugString(pchar('Counter: ' + counter.ToString));
              sleep(350);                 // 350ms warten bis zum nächsten Versuch
              dec(counter);
            end;
          end;
        end;

      faREMOVED:          FieldByName('Pruefsumme').AsString := 'Deleted';
      faRENAMED_OLD_NAME: FieldByName('Pruefsumme').AsString := 'Oldname';
    end;

    FieldByName('Counter').AsInteger := counter;
    Post;

    DBGrid1.Refresh;
    EditEvents.Refresh;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  events_counter := 0;
  FileNotify1.WatchDirectory := 'C:\Temp';
  DirectoryEdit1.Text := FileNotify1.WatchDirectory;
//  CheckBoxActive.Checked := TRUE;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  EditEvents.Text := '';
  events_counter := 0;
  Memo1.Clear;
  FDMemTable.EmptyView;
end;

procedure TForm1.DirectoryEdit1Change(Sender: TObject);
begin
  CheckBoxActive.Checked := FALSE;
end;

procedure TForm1.DirectoryEdit1RightButtonClick(Sender: TObject);
//var
//  path: string;
begin
  CheckBoxActive.Checked := FALSE;

  // Alter Dialog:
  //  if SelectDirectory(path, [sdAllowCreate, sdPerformCreate, sdPrompt], 1000) then
  //    DirectoryEdit1.Text := path;

  // Test 2:
  //  path := DirectoryEdit1.Text;
  //  if SelectDirectory('Verzeichnis wählen', '', path, [sdNewUI, sdNewFolder]) then begin
  //    DirectoryEdit1.Text := path;

  FileOpenDialog1.DefaultFolder := DirectoryEdit1.Text;

  if FileOpenDialog1.Execute then begin
    DirectoryEdit1.Text := FileOpenDialog1.FileName;

    if(System.SysUtils.DirectoryExists(DirectoryEdit1.Text)) then begin
      CheckBoxActive.Checked := TRUE;
      Memo1.Lines.Add('Verzeichnis: ' + DirectoryEdit1.Text);
    end;
    // else
    //   Memo1.Lines.Add('Verzeichnis: "' + DirectoryEdit1.Text + '" existiert nicht! Keine Überwachung...');
  end;
end;

procedure TForm1.ButtonSaveClick(Sender: TObject);
begin
  Memo1.Lines.SaveToFile(ChangeFileExt(Application.ExeName, '.log'));
  FDMemTable.SaveToFile(ChangeFileExt(Application.ExeName, '.json'), sfJSON);
end;

procedure TForm1.CheckBoxActiveClick(Sender: TObject);
begin
  with FileNotify1 do begin
    Active := FALSE;
    WatchDirectory := DirectoryEdit1.Text;
    Active := CheckBoxActive.Checked;

    if Active then
    begin
      Caption := 'Monitoring ' + WatchDirectory;
      Memo1.Lines.Add(DateTimeToStr(Now) + ' - Monitoring "' + WatchDirectory + '" started');
    end
    else begin
      Caption := 'Monitoring stopped';
      Memo1.Lines.Add(DateTimeToStr(Now) + ' - Monitoring "' + WatchDirectory + '" stopped');
    end;
  end;
end;

procedure TForm1.CheckBoxRekursivClick(Sender: TObject);
begin
  with FileNotify1 do begin
    Active := FALSE;
    WatchSubtree := CheckBoxRekursiv.Checked;
    Active := CheckBoxActive.Checked;
  end;
end;

end.
