{*******************************************************}
{                                                       }
{       Borland Delphi Component                        }
{       Archiver  (version 1.1)                         }
{                                                       }
{       Copyright (C) 1997-99 DragonSoft                }
{                                                       }
{*******************************************************}

unit Archiver;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

{ Everything about error handling }
type
  TErrorAction=(eaRetry, eaException, eaAbort, eaIgnore);
  TErrorEvent=procedure(Sender: TObject;
                        const EClass: ExceptClass;
                        const ErrorCode: Integer;
                        var Action: TErrorAction) of Object;
const ecOK             =  0;        { No error }
      ecAlreadyWorking = -1;        { Archiver is already working }
      ecBrokenArchive  = -2;        { Specified file is not an archive or it is broken }
      ecVolumeNotFound = -3;        { Next volume not found }
      ecPropNotSet     = -4;        { Required property not set }
      ecChangeOnRunning= -5;        { Property cannot be changed on running archiver }
      ecInvalidValue   = -6;        { Invalid property value }
      ecNotWorking     = -7;        { Archiver is not working }
      ecOtherException = -8;        { Another exception }

type
  TArchiverStatus=(astReady,astWorking,astWaiting,astBusy);
  TFileName=String[255];

  EArchiver = class(Exception)
  end;

  TQueryRewriteEvent = procedure(Sender: TObject; var CanRewrite: Boolean) of object;

  TArchiver = class(TComponent)
  private
    { Private declarations }
    DoStop,
    IsRunning:   Boolean;

    { Status for recovering operations }
    recBuff1, recBuff2: Pointer;
    recBuff1Size, recBuff2Size: Word;
    recFIOpen, recFOOpen: Boolean;
    FI, FO: File;

    { I/O routines which do my own error handling }
    procedure Reset(var F: File; const RecordSize: Integer);
    procedure Rewrite(var F: File; const RecordSize: Integer);
    procedure BlockRead(var F: File; var X; const Size: Integer);
    procedure BlockWrite(var F: File; const X; const Size: Integer);
  protected
    { Protected declarations }
    FStatus:     TArchiverStatus;
    FDiskNum,
    FFileNum:    Integer;
    FFilePosition,
    FFilePackedPos,
    FPosition,
    FCurrFileSize,
    FTotalSize:  Int64;
    FFileName:   String;

    FFileList:   TStrings;
    FDestName:   String;
    FVolumeSize: Integer;

    FOnNextVolume: TNotifyEvent;
    FOnProgress: TNotifyEvent;
    FOnError: TErrorEvent;
    FOnQueryRewrite: TQueryRewriteEvent;

    function  GetFileList: TStrings;
    procedure SetFileList(const Value: TStrings);
    procedure SetVolumeSize(const Value: Integer);
    procedure SetDestName(const Value: String);
  public
    { Public declarations }
    property Status: TArchiverStatus read FStatus;
    property TotalSize: Int64 read FTotalSize;
    property DiskNum: Integer read FDiskNum;
    property FileNum: Integer read FFileNum;
    property FilePosition: Int64 read FFilePosition;
    property FilePackedPos: Int64 read FFilePackedPos;
    property Position: Int64 read FPosition;
    property FileName: String read FFileName;
    property CurrFileSize: Int64 read FCurrFileSize;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   CountTotalSize;
    procedure   StartPacking;
    procedure   StartUnpacking(const ArchiveName: String);
    procedure   StopWorking;
    procedure   Recover;     { Recover from exception }

    procedure   AboutBox;

    class function ErrorMessage(const ErrorCode: Integer): String;
  published
    { Published declarations }
    property FileList: TStrings read GetFileList write SetFileList;
    property VolumeSize: Integer read FVolumeSize write SetVolumeSize;
    property DestName: String read FDestName write SetDestName;

    property OnNextVolume: TNotifyEvent read FOnNextVolume write FOnNextVolume;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnQueryRewrite: TQueryRewriteEvent read FOnQueryRewrite write FOnQueryRewrite;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

  TArchiveHeader=Record
                      Magic: Word;
                      VerMaj,
                      VerMin: Byte;
                      Files,
                      OriTotalSize: Longint;
                      ArchiveID: Longint;
                End;
  TVolumeHeader=Record
                      Magic: Word;
                      ArchiveID: Longint;
                      DiskNum: Longint;
                End;
  TVolumeFooter=Record
                      Magic: Word;
                      Last: Boolean;
                      VolumeSize: Longint;
                End;
  TFileHeader=Record
                    Magic:    Word;
                    FileName: TFileName;
                    OriSize:  Longint;
              End;
  TBlockHeader=Record
                     BlockSize,
                     OriSize: Word;
               End;

const MIN_BLOCK_SIZE=512;
      MIN_VOLUME_SIZE=SizeOf(TArchiveHeader)+SizeOf(TVolumeHeader)+SizeOf(TBlockHeader)+
                      SizeOf(TVolumeFooter)+SizeOf(TFileHeader)+2*MIN_BLOCK_SIZE;

procedure Register;

implementation
